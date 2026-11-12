module Rbt.Pdf.Web.ImageSource

open Fable.Core
open Fable.Core.JsInterop
open Rbt.Pdf
open Rbt.Renderer.CanvasApi

// ── JPEG decode cache ────────────────────────────────────
// Keyed by raw byte[] identity (JS WeakMap — same array object from PdfStream each time).

let private jpegCache : obj = createWeakMap ()

/// Synchronous JPEG cache lookup. Returns cached (w, h, premultiplied BGRA uint32[]) or None.
/// Intended as the RenderOptions.DecodeJpeg function for web builds.
let decodeJpeg (data: byte[]) : (int * int * uint32[]) option =
    match weakMapTryGet<int * int * uint32[]> jpegCache data with
    | Some decoded -> Some decoded
    | None -> Rbt.Pdf.Render.ImageDecode.decode data


/// Decode a JPEG blob via the browser, returning (w, h, premultiplied BGRA uint32[]).
let private decodeJpegAsync (rawData: byte[]) : JS.Promise<unit> =
    let blob = createBlob rawData "image/jpeg"
    createImageBitmap blob
    |> promiseBind (fun bmp ->
        let w = bmp.width
        let h = bmp.height
        let oc = createOffscreenCanvas w h
        let ctx = getOffscreenContext2DReadFrequent oc
        ctxDrawImageBitmap ctx bmp 0.0 0.0
        bmp.close()
        let pixels = Rbt.Renderer.CanvasImage.pixelsFromCanvas oc
        weakMapSet<int * int * uint32[]> jpegCache rawData (w, h, pixels)
        promiseReturn ()
    )
    |> promiseCatch (fun _err ->
        // The synchronous managed decoder remains available when the browser rejects a JPEG.
        promiseReturn ()
    )

// ── Prepare pre-pass ─────────────────────────────────────
// Walk a page's XObject resources and pre-decode any DCTDecode images.

let private getFilterName (doc: PdfDocument) (obj: PdfObj) : string =
    match Pdf.dictGet doc "Filter" obj with
    | Some (PdfName f) -> f
    | Some (PdfArray (f :: _)) ->
        match Pdf.deref doc f with
        | PdfName n -> n
        | _ -> ""
    | _ -> ""

let private isDCT (doc: PdfDocument) (obj: PdfObj) : bool =
    let f = getFilterName doc obj
    f = "DCTDecode" || f = "DCT"
// PdfImages.decode uses the managed component decoder whenever ColorSpace is
// declared. The browser's BGRA fallback is reached only when it is absent
// (except image masks, whose stencil color space is supplied by the renderer).
let private needsBrowserJpeg (doc: PdfDocument) (obj: PdfObj) =
    Pdf.dictGet doc "ColorSpace" obj |> Option.isNone
    && (match Pdf.dictGet doc "ImageMask" obj |> Option.map (Pdf.deref doc) with
        | Some (PdfBool true) -> false
        | _ -> true)

let private collectDCTStreams (doc: PdfDocument) (resObj: PdfObj) : byte[] list =
    let results = ResizeArray<byte[]>()
    let visited = System.Collections.Generic.HashSet<int>()

    let rec walkResources (res: PdfObj) =
        match Pdf.dictGet doc "XObject" res with
        | Some xobjDictRef ->
            match Pdf.deref doc xobjDictRef with
            | PdfDict entries ->
                for kv in entries do
                    let xobj = Pdf.deref doc kv.Value
                    // Track visited by object number to avoid infinite recursion
                    let objNum =
                        match kv.Value with
                        | PdfIndirect (n, _) -> Some n
                        | _ -> None
                    let alreadyVisited =
                        match objNum with
                        | Some n -> not (visited.Add(n))
                        | None -> false
                    if not alreadyVisited then
                        match Pdf.dictGet doc "Subtype" xobj with
                        | Some (PdfName "Image") when isDCT doc xobj && needsBrowserJpeg doc xobj ->
                            // Get the raw stream bytes (JPEG data)
                            match Pdf.deref doc kv.Value with
                            | PdfStream (_, raw) ->
                                if not (weakMapHas jpegCache raw) then
                                    results.Add(raw)
                            | _ -> ()
                        | Some (PdfName "Form") ->
                            // Recurse into Form XObject resources
                            match Pdf.dictGet doc "Resources" xobj with
                            | Some formRes -> walkResources (Pdf.deref doc formRes)
                            | None -> ()
                        | _ -> ()
            | _ -> ()
        | None -> ()

    walkResources (Pdf.deref doc resObj)
    results |> Seq.toList

/// Async pre-pass: browser-decode only DCT images using PdfImages' no-ColorSpace fallback.
let prepare (doc: PdfDocument) (pageNum: int) : JS.Promise<unit> =
    match Pdf.getPage doc pageNum with
    | None -> promiseReturn ()
    | Some pageRef ->
        let resObj =
            match Pdf.getPageInheritable doc pageRef "Resources" with
            | Some r -> r
            | None -> PdfNull
        let rawStreams = collectDCTStreams doc resObj
        if rawStreams.IsEmpty then
            promiseReturn ()
        else
            rawStreams
            |> List.map decodeJpegAsync
            |> Array.ofList
            |> promiseAll
            |> promiseMap (fun _ -> ())

