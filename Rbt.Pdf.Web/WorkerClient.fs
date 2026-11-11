module Rbt.Pdf.Web.WorkerClient

open Fable.Core
open Fable.Core.JsInterop
open Rbt.Renderer.CanvasApi

// Worker messages are plain objects and primitive arrays only. PdfDocument and F#
// record/class instances stay within the worker, not the structured clone protocol.
/// Reuse wire tuples without loading renderer records or copying every glyph on the UI thread.
type TextBox = string * float * float * float * float
type RenderedPage = { Bitmap: ImageBitmap; Width: int; Height: int; Boxes: TextBox[] }

type Client(bytes: byte[], assetsBase: string) =
    // Construct relative to THIS compiled module. The same relative URL works for
    // .generated/WorkerRuntime.js under Vite and dist/WorkerRuntime.js in @rbt/pdf;
    // bundlers can also discover the static new URL(..., import.meta.url) expression.
    let assetUrl : string = emitJsExpr assetsBase "new URL($0, document.baseURI).href"
    let worker : obj = emitJsExpr () "new Worker(new URL('./WorkerRuntime.js', import.meta.url), {type:'module'})"
    let requests = System.Collections.Generic.Dictionary<int, obj -> unit>()
    let mutable nextId = 0
    let mutable disposed = false

    let rejectAll (message: string) =
        let response : obj = emitJsExpr message "({kind:'error', message:$0})"
        for callback in requests.Values |> Seq.toArray do callback response
        requests.Clear()

    do
        let onMessage (evt: obj) =
            let response : obj = emitJsExpr evt "$0.data"
            let id : int = emitJsExpr response "$0.id"
            match requests.TryGetValue id with
            | true, callback ->
                requests.Remove id |> ignore
                callback response
            | _ ->
                // A canceled or disposed request still owns a transferred bitmap.
                let kind : string = emitJsExpr response "$0.kind"
                if kind = "rendered" then
                    let bitmap : ImageBitmap = emitJsExpr response "$0.bitmap"
                    bitmap.close()
        let onError (evt: obj) =
            let message : string = emitJsExpr evt "String($0.message || 'PDF worker failed')"
            rejectAll message
            emitJsExpr worker "$0.terminate()"
            disposed <- true
        emitJsExpr (worker, onMessage) "$0.onmessage = $1"
        emitJsExpr (worker, onError) "$0.onerror = $1"
        emitJsExpr (worker, onError) "$0.onmessageerror = $1"

    let request (makeMessage: int -> obj) (transfer: obj[]) : JS.Promise<obj> =
        if disposed then
            let err : obj = emitJsExpr () "new Error('PDF worker disposed')"
            emitJsExpr err "Promise.reject($0)"
        else
            nextId <- nextId + 1
            let id = nextId
            // Use an uncurried delegate: the browser Promise executor receives two arguments.
            let executor = System.Action<obj, obj>(fun resolve reject ->
                requests.[id] <- (fun response ->
                    let kind : string = emitJsExpr response "$0.kind"
                    if kind = "error" then
                        let message : string = emitJsExpr response "$0.message"
                        emitJsExpr (reject, message) "$0(new Error($1))"
                    else emitJsExpr (resolve, response) "$0($1)")
                try
                    let message = makeMessage id
                    emitJsExpr (worker, message, transfer) "$0.postMessage($1, $2)"
                with ex ->
                    requests.Remove id |> ignore
                    emitJsExpr (reject, ex.Message) "$0(new Error($1))")
            emitJsExpr executor "new Promise($0)"

    member _.Open() : JS.Promise<float[][]> =
        // Transfer a COPY. The caller retains ownership of the supplied PDF bytes.
        let copy : byte[] = emitJsExpr bytes "new Uint8Array($0)"
        let buffer : obj = emitJsExpr copy "$0.buffer"
        request (fun id -> emitJsExpr (id, copy, assetUrl) "({kind:'open', id:$0, bytes:$1, assetsBase:$2})") [|buffer|]
        |> promiseMap (fun response -> emitJsExpr response "$0.sizes")

    member _.Render(page: int, scale: float) : JS.Promise<RenderedPage> =
        request (fun id -> emitJsExpr (id, page, scale) "({kind:'render', id:$0, page:$1, scale:$2})") [||]
        |> promiseMap (fun response ->
            let bitmap : ImageBitmap = emitJsExpr response "$0.bitmap"
            let width : int = emitJsExpr response "$0.width"
            let height : int = emitJsExpr response "$0.height"
            let boxes : TextBox[] = emitJsExpr response "$0.boxes"
            { Bitmap = bitmap; Width = width; Height = height; Boxes = boxes })

    member _.Dispose() =
        if not disposed then
            disposed <- true
            emitJsExpr worker "$0.terminate()"
            rejectAll "PDF worker disposed"
