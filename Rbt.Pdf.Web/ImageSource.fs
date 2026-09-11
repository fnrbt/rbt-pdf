module Rbt.Pdf.Web.ImageSource

open Fable.Core
open Fable.Core.JsInterop
open Rbt.Renderer
open Rbt.Pdf
open Rbt.Pdf.Web.WebApi

// ── JPEG decode cache ────────────────────────────────────
// Keyed by raw byte[] identity (JS WeakMap — same array object from PdfStream each time).

let private jpegCache : obj = createWeakMap ()

/// Synchronous JPEG cache lookup. Returns cached (w, h, premultiplied BGRA uint32[]) or None.
/// Intended as the RenderOptions.DecodeJpeg function for web builds.
let decodeJpeg (data: byte[]) : (int * int * uint32[]) option =
    weakMapTryGet<int * int * uint32[]> jpegCache data

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
        let imgData = ctx.getImageData(0.0, 0.0, float w, float h)
        let rgba : uint32[] = emitJsExpr imgData.data "new Uint32Array($0.buffer)" 
        let pixels = Array.zeroCreate<uint32> (w * h)
        for i in 0 .. rgba.Length - 1 do
            let v = rgba.[i]
            let r = v &&& 0xFFu
            let g = (v >>> 8) &&& 0xFFu
            let b = (v >>> 16) &&& 0xFFu
            let a = v >>> 24
            if a = 0u then
                pixels.[i] <- 0u
            elif a = 255u then
                pixels.[i] <- b ||| (g <<< 8) ||| (r <<< 16) ||| (a <<< 24)
            else
                let af = float a / 255.0
                let rp = uint32 (float r * af)
                let gp = uint32 (float g * af)
                let bp = uint32 (float b * af)
                pixels.[i] <- bp ||| (gp <<< 8) ||| (rp <<< 16) ||| (a <<< 24)
        weakMapSet<int * int * uint32[]> jpegCache rawData (w, h, pixels)
        promiseReturn ()
    )
    |> promiseCatch (fun _err ->
        // Browser can't decode this JPEG (12-bit, CMYK, corrupt, etc.) — skip silently.
        // renderPage will get None from decodeJpeg and skip the image.
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

let private lastFilterIsDCT (doc: PdfDocument) (obj: PdfObj) : bool =
    match Pdf.dictGet doc "Filter" obj with
    | Some (PdfName f) -> f = "DCTDecode" || f = "DCT"
    | Some (PdfArray filters) ->
        match filters |> List.tryLast with
        | Some f ->
            match Pdf.deref doc f with
            | PdfName n -> n = "DCTDecode" || n = "DCT"
            | _ -> false
        | None -> false
    | _ -> false

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
                        | Some (PdfName "Image") when isDCT doc xobj ->
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

/// Async pre-pass: decode every DCTDecode XObject reachable from the page into the JPEG cache.
let prepare (doc: PdfDocument) (pageNum: int) : JS.Promise<unit> =
    match Pdf.getPage doc pageNum with
    | None -> promiseReturn ()
    | Some pageRef ->
        let pageObj = Pdf.deref doc pageRef
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

// ── Framebuffer → OffscreenCanvas cache ──────────────────
// Keyed by fb.Px (uint32[]) identity via JS WeakMap.

let private fbCache : obj = createWeakMap ()

/// Convert a Framebuffer to an OffscreenCanvas, cached by pixel array identity.
/// Un-premultiplies BGRA pixels to RGBA for ImageData, then putImageData.
let canvasFor (fb: Framebuffer) : OffscreenCanvas =
    let px =
        match Framebuffer.tryGetArray fb with
        | Some arr -> arr
        | None -> failwith "canvasFor: non-array-backed Framebuffer"
    match weakMapTryGet<OffscreenCanvas> fbCache px with
    | Some oc -> oc
    | None ->
        let w = fb.W
        let h = fb.H
        let oc = createOffscreenCanvas w h
        let ctx = getOffscreenContext2D oc
        let imageData = createImageDataEmpty w h
        // Uint32Array view over ImageData.data for fast 32-bit writes
        let dst : uint32[] = emitJsExpr imageData.data "new Uint32Array($0.buffer)"
        for i in 0 .. px.Length - 1 do
            let v = px.[i]
            let a = v >>> 24
            if a = 0u then
                dst.[i] <- 0u
            elif a = 255u then
                // Opaque: BGRA→RGBA swap (B↔R, keep G and A)
                // Input LE: B|(G<<8)|(R<<16)|(A<<24)  Output LE: R|(G<<8)|(B<<16)|(A<<24)
                dst.[i] <- (v &&& 0xFF00FF00u) ||| ((v &&& 0xFFu) <<< 16) ||| ((v >>> 16) &&& 0xFFu)
            else
                // Un-premultiply + BGRA→RGBA
                let ia = 255.0 / float a
                let b = v &&& 0xFFu
                let g = (v >>> 8) &&& 0xFFu
                let r = (v >>> 16) &&& 0xFFu
                let ru = min 255u (uint32 (float r * ia))
                let gu = min 255u (uint32 (float g * ia))
                let bu = min 255u (uint32 (float b * ia))
                // RGBA LE: R|(G<<8)|(B<<16)|(A<<24)
                dst.[i] <- ru ||| (gu <<< 8) ||| (bu <<< 16) ||| (a <<< 24)
        ctx.putImageData(imageData, 0.0, 0.0)
        weakMapSet<OffscreenCanvas> fbCache px oc
        oc
