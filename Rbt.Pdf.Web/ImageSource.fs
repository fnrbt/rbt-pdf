module Rbt.Pdf.Web.ImageSource

open Fable.Core
open Fable.Core.JsInterop
open Rbt.Pdf
open Rbt.Renderer.CanvasApi

// Keyed by the original stream byte-array identity.
let private jpegCache : obj = createWeakMap ()

/// Synchronous cache lookup, with the managed decoder as fallback.
let decodeJpeg (data: byte[]) : (int * int * uint32[]) option =
    match weakMapTryGet<int * int * uint32[]> jpegCache data with
    | Some decoded -> Some decoded
    | None -> Rbt.Pdf.Render.ImageDecode.decode data

[<Emit("createImageBitmap($0, {colorSpaceConversion: 'none', imageOrientation: 'none'})")>]
let private createJpegBitmap (blob: Browser.Types.Blob) : JS.Promise<ImageBitmap> = jsNative

let private decodeJpegAsync (rawData: byte[], width: int, height: int) : JS.Promise<unit> =
    // Start in a promise callback so synchronous Blob/bitmap/canvas failures also fall back.
    promiseReturn ()
    |> promiseBind (fun () -> createJpegBitmap (createBlob rawData "image/jpeg"))
    |> promiseBind (fun bmp ->
        try
            if bmp.width = width && bmp.height = height then
                let canvas = createOffscreenCanvas width height
                let ctx = getOffscreenContext2DReadFrequent canvas
                ctxDrawImageBitmap ctx bmp 0.0 0.0
                let pixels = Rbt.Renderer.CanvasImage.pixelsFromCanvas canvas
                weakMapSet<int * int * uint32[]> jpegCache rawData (width, height, pixels)
            promiseReturn ()
        finally
            bmp.close())
    |> promiseCatch (fun _ -> promiseReturn ())

// Visit each form on the current path rather than globally: a form without resources
// inherits its caller's resources, which may differ between invocations.
let private collectDCTStreams (doc: PdfDocument) (resObj: PdfObj) : (byte[] * int * int) list =
    let results = ResizeArray<byte[] * int * int>()
    let seen : obj = createWeakMap ()
    let active : obj = createWeakMap ()
    let rec walkResources (res: PdfObj) =
        match Pdf.dictGet doc "XObject" res |> Option.map (Pdf.deref doc) with
        | Some (PdfDict entries) ->
            for kv in entries do
                let xobj = Pdf.deref doc kv.Value
                match xobj with
                | PdfStream (_, raw) ->
                    match Pdf.dictGet doc "Subtype" xobj |> Option.map (Pdf.deref doc) with
                    | Some (PdfName "Image") ->
                        if not (weakMapHas seen raw) && not (weakMapHas jpegCache raw) then
                            match Rbt.Pdf.Render.PdfImages.tryBrowserJpeg doc res xobj with
                            | Some (bytes, w, h) ->
                                weakMapSet<bool> seen bytes true
                                results.Add(bytes, w, h)
                            | None -> ()
                    | Some (PdfName "Form") ->
                        if weakMapTryGet<bool> active raw <> Some true then
                            weakMapSet<bool> active raw true
                            try
                                let formRes =
                                    Pdf.dictGet doc "Resources" xobj
                                    |> Option.map (Pdf.deref doc)
                                    |> Option.defaultValue res
                                walkResources formRes
                            finally
                                weakMapSet<bool> active raw false
                    | _ -> ()
                | _ -> ()
        | _ -> ()
    walkResources (Pdf.deref doc resObj)
    results |> Seq.toList

/// Predecode eligible page and form JPEGs; rejected or failed decodes stay managed.
let prepare (doc: PdfDocument) (pageNum: int) : JS.Promise<unit> =
    match Pdf.getPage doc pageNum with
    | None -> promiseReturn ()
    | Some pageRef ->
        let resources = Pdf.getPageInheritable doc pageRef "Resources" |> Option.defaultValue PdfNull
        let streams = collectDCTStreams doc resources
        if streams.IsEmpty then promiseReturn ()
        else
            streams
            |> List.map decodeJpegAsync
            |> Array.ofList
            |> promiseAll
            |> promiseMap (fun _ -> ())
