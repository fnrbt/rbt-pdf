module Rbt.Pdf.Web.WorkerRuntime

open Fable.Core
open Fable.Core.JsInterop
open Rbt.Pdf
open Rbt.Pdf.Render.Render
open Rbt.Renderer
open Rbt.Renderer.CanvasApi

// Worker messages are plain JS objects, numeric arrays and Uint8Array buffers. Never send
// PdfDocument, RenderOptions, FontData or F# records across the structured-clone boundary.
[<Emit("fetch($0).then(function(r){ if(!r.ok) throw new Error('fetch '+$0+': '+r.status); return r.arrayBuffer(); }).then(function(b){ return new Uint8Array(b); })")>]
let private fetchBytes (url: string) : JS.Promise<byte[]> = jsNative

let private cmapNames =
    [| "90ms-RKSJ-H"; "90ms-RKSJ-V"; "EUC-H"; "EUC-V"; "UniJIS-UCS2-H"; "UniJIS-UCS2-V"
       "UniJIS-UTF16-H"; "UniJIS-UTF16-V"; "Adobe-Japan1-UCS2"
       "GBK-EUC-H"; "GBK-EUC-V"; "GB-EUC-H"; "GB-EUC-V"; "UniGB-UCS2-H"; "UniGB-UCS2-V"
       "UniGB-UTF16-H"; "UniGB-UTF16-V"; "Adobe-GB1-UCS2"
       "B5pc-H"; "B5pc-V"; "ETen-B5-H"; "ETen-B5-V"; "UniCNS-UCS2-H"; "UniCNS-UCS2-V"
       "UniCNS-UTF16-H"; "UniCNS-UTF16-V"; "Adobe-CNS1-UCS2"
       "KSC-EUC-H"; "KSC-EUC-V"; "KSCms-UHC-H"; "KSCms-UHC-V"; "UniKS-UCS2-H"; "UniKS-UCS2-V"
       "UniKS-UTF16-H"; "UniKS-UTF16-V"; "Adobe-Korea1-UCS2" |]

let mutable private document : PdfDocument option = None
let mutable private options : RenderOptions option = None
let mutable private assetsBase = ""
let mutable private cjkFont : Text.FontData option = None
let mutable private cjkPromise : JS.Promise<unit> option = None
let mutable private fallbackRequested = false

let private renderOptions font cmaps =
    { FallbackFont = font
      CjkFont = lazy (fallbackRequested <- true; cjkFont)
      CMaps = cmaps
      DecodeJpeg = ImageSource.decodeJpeg
      Rasterize = Rbt.Renderer.CanvasBackend.rasterize }

let private send (message: obj) (transfers: obj[]) : unit =
    emitJsExpr (message, transfers) "self.postMessage($0, $1)"

let private error (id: int) (err: obj) =
    let message : string = emitJsExpr err "String($0 && ($0.stack || $0.message || $0))"
    let response : obj = emitJsExpr (id, message) "({kind:'error', id:$0, message:$1})"
    send response [||]

let private loadAssets (baseUrl: string) : JS.Promise<RenderOptions> =
    let key = baseUrl.TrimEnd('/')
    let urls = Array.append [|key + "/ucd.dat"; key + "/NotoSans-Regular.ttf"|] (cmapNames |> Array.map (fun n -> key + "/cmaps/" + n))
    promiseAll (Array.map fetchBytes urls)
    |> promiseMap (fun data ->
        Rbt.Unicode.Ucd.loadData data.[0]
        let font = Text.loadFont data.[1]
        let cmaps = cmapNames |> Array.mapi (fun i name -> name, data.[i + 2]) |> Map.ofArray
        renderOptions font cmaps)

let private needsCjk (boxes: CharBox[]) : bool =
    // Only a missing CJK glyph triggers this large download. Latin and missing
    // symbols never fetch the 16 MB fallback font.
    boxes |> Array.exists (fun box ->
        emitJsExpr box.Char "/[\\u1100-\\u11ff\\u2e80-\\u9fff\\ua960-\\ua97f\\uac00-\\ud7ff\\uf900-\\ufaff\\uff00-\\uffef]|[\\ud840-\\ud87f][\\udc00-\\udfff]/.test($0)")

let private loadCjk () : JS.Promise<unit> =
    match cjkPromise with
    | Some p -> p
    | None ->
        let p =
            fetchBytes (assetsBase + "/NotoSansCJKsc-Regular.otf")
            |> promiseMap (fun bytes ->
                cjkFont <- Some (Text.loadFont bytes)
                // A Lazy(None) from the first pass cannot be reused after loading.
                options <- options |> Option.map (fun old -> { old with CjkFont = lazy cjkFont }))
        cjkPromise <- Some p
        p

let private render (id: int) (pageNum: int) (scale: float) : JS.Promise<unit> =
    match document, options with
    | Some doc, Some opts ->
        ImageSource.prepare doc pageNum
        |> promiseBind (fun () ->
            fallbackRequested <- false
            // A Lazy(None) from a previous Latin page must not hide later CJK misses.
            let latest = options |> Option.defaultValue opts
            let current =
                if cjkFont.IsNone then { latest with CjkFont = lazy (fallbackRequested <- true; None) }
                else latest
            let first = renderPage current doc pageNum scale
            let finalResult =
                if fallbackRequested then
                    match first with
                    | Some (_, _, _, boxes) when needsCjk boxes && cjkFont.IsNone ->
                        loadCjk () |> promiseMap (fun () ->
                            match options with
                            | Some updated -> renderPage updated doc pageNum scale
                            | None -> None)
                    | _ -> promiseReturn first
                else promiseReturn first
            finalResult |> promiseMap (fun rendered ->
                match rendered with
                | None -> failwith (sprintf "PDF page %d is unavailable" pageNum)
                | Some (commands, width, height, boxes) ->
                    let w, h = int (ceil width), int (ceil height)
                    let canvas = createOffscreenCanvas w h
                    Rbt.Renderer.CanvasBackend.render (getOffscreenContext2D canvas) commands
                    let bitmap : ImageBitmap = emitJsExpr canvas "$0.transferToImageBitmap()"
                    try
                        let text : obj[] = boxes |> Array.map (fun b -> emitJsExpr (b.Char, b.X, b.Y, b.W, b.H) "[$0,$1,$2,$3,$4]")
                        let response : obj = emitJsExpr (id, pageNum, w, h, bitmap, text) "({kind:'rendered', id:$0, page:$1, width:$2, height:$3, bitmap:$4, boxes:$5})"
                        send response [| box bitmap |]
                    with _ ->
                        bitmap.close()
                        reraise ())
        )
    | _ -> failwith "PDF worker has no open document"

let private openDocument (id: int) (bytes: byte[]) (baseUrl: string) : JS.Promise<unit> =
    assetsBase <- baseUrl.TrimEnd('/')
    loadAssets assetsBase
    |> promiseMap (fun opts ->
        let doc = Pdf.openData bytes
        let count = Pdf.getPageCount doc
        let sizes : float[][] =
            [| for n in 1 .. count do
                let w, h =
                    match Pdf.getPage doc n with
                    | Some pageRef ->
                        match Rbt.Pdf.Render.PageGeometry.tryGet doc (Pdf.deref doc pageRef) with
                        | Some viewport -> viewport.Width, viewport.Height
                        | None -> 612.0, 792.0
                    | None -> 612.0, 792.0
                yield [|w; h|] |]
        options <- Some opts
        document <- Some doc
        let response : obj = emitJsExpr (id, sizes) "({kind:'opened', id:$0, sizes:$1})"
        send response [||])

let private receive (evt: obj) =
    let message : obj = emitJsExpr evt "$0.data"
    let id : int = emitJsExpr message "$0.id"
    let kind : string = emitJsExpr message "$0.kind"
    let work =
        try
            match kind with
            | "open" ->
                let bytes : byte[] = emitJsExpr message "$0.bytes"
                let baseUrl : string = emitJsExpr message "$0.assetsBase"
                openDocument id bytes baseUrl
            | "render" ->
                let page : int = emitJsExpr message "$0.page"
                let scale : float = emitJsExpr message "$0.scale"
                render id page scale
            | _ -> failwith ("Unknown PDF worker message: " + kind)
        with ex ->
            error id (box ex)
            promiseReturn ()
    work |> promiseCatch (fun ex -> error id ex; promiseReturn ()) |> ignore

// This module is imported only as an ES module worker entry, never by the UI.
do emitJsExpr receive "self.onmessage = $0"
