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
    // Own a stable snapshot: callers may change or release their buffer after Open.
    // Each worker receives a transferred copy, while this one survives restarts.
    let mutable retainedBytes : byte[] = emitJsExpr bytes "new Uint8Array($0)"
    let mutable worker : obj = null
    let requests = System.Collections.Generic.Dictionary<int, obj -> unit>()
    let mutable nextId = 0
    let mutable disposed = false
    let mutable openPromise : JS.Promise<float[][]> option = None
    let mutable renderEpoch = 0
    let mutable renderRequestId = 0

    let rejectPromise (message: string) : JS.Promise<'T> =
        let err : obj = emitJsExpr message "new Error($0)"
        emitJsExpr err "Promise.reject($0)"

    let closeUnclaimedBitmap (response: obj) =
        let kind : string = emitJsExpr response "$0.kind"
        if kind = "rendered" then
            let bitmap : ImageBitmap = emitJsExpr response "$0.bitmap"
            bitmap.close()

    let rejectAll (message: string) =
        let response : obj = emitJsExpr message "({kind:'error', message:$0})"
        let callbacks = requests.Values |> Seq.toArray
        requests.Clear()
        renderRequestId <- 0
        for callback in callbacks do callback response

    let startWorker () =
        let current : obj = emitJsExpr () "new Worker(new URL('./WorkerRuntime.js', import.meta.url), {type:'module'})"
        worker <- current
        let onMessage (evt: obj) =
            let response : obj = emitJsExpr evt "$0.data"
            if obj.ReferenceEquals(worker, current) then
                let id : int = emitJsExpr response "$0.id"
                match requests.TryGetValue id with
                | true, callback ->
                    requests.Remove id |> ignore
                    callback response
                | _ -> closeUnclaimedBitmap response
            else closeUnclaimedBitmap response
        let onError (evt: obj) =
            if obj.ReferenceEquals(worker, current) then
                let message : string = emitJsExpr evt "String($0.message || 'PDF worker failed')"
                disposed <- true
                retainedBytes <- null
                emitJsExpr current "$0.terminate()"
                worker <- null
                openPromise <- None
                rejectAll message
        emitJsExpr (current, onMessage) "$0.onmessage = $1"
        emitJsExpr (current, onError) "$0.onerror = $1"
        emitJsExpr (current, onError) "$0.onmessageerror = $1"
        current

    let request (current: obj) (isRender: bool) (makeMessage: int -> obj) (transfer: obj[]) : JS.Promise<obj> =
        if disposed || not (obj.ReferenceEquals(worker, current)) then
            rejectPromise "PDF worker disposed"
        else
            nextId <- nextId + 1
            let id = nextId
            // Use an uncurried delegate: the browser Promise executor receives two arguments.
            let executor = System.Action<obj, obj>(fun resolve reject ->
                requests.[id] <- (fun response ->
                    if isRender && renderRequestId = id then renderRequestId <- 0
                    let kind : string = emitJsExpr response "$0.kind"
                    if kind = "error" then
                        let message : string = emitJsExpr response "$0.message"
                        emitJsExpr (reject, message) "$0(new Error($1))"
                    else emitJsExpr (resolve, response) "$0($1)")
                try
                    let message = makeMessage id
                    if isRender then renderRequestId <- id
                    emitJsExpr (current, message, transfer) "$0.postMessage($1, $2)"
                with ex ->
                    requests.Remove id |> ignore
                    if isRender && renderRequestId = id then renderRequestId <- 0
                    emitJsExpr (reject, ex.Message) "$0(new Error($1))")
            emitJsExpr executor "new Promise($0)"

    let openDocument () : JS.Promise<float[][]> =
        if disposed then rejectPromise "PDF worker disposed"
        else
            match openPromise with
            | Some p -> p
            | None ->
                try
                    let current = if isNull worker then startWorker () else worker
                    // Transfer a copy of our private snapshot. The worker's copy is
                    // detached from this thread; reopening must use the same bytes.
                    let copy : byte[] = emitJsExpr retainedBytes "new Uint8Array($0)"
                    let buffer : obj = emitJsExpr copy "$0.buffer"
                    let p =
                        request current false
                            (fun id -> emitJsExpr (id, copy, assetUrl) "({kind:'open', id:$0, bytes:$1, assetsBase:$2})")
                            [|buffer|]
                        |> promiseMap (fun response -> emitJsExpr response "$0.sizes")
                    openPromise <- Some p
                    p
                with ex -> rejectPromise ex.Message

    member _.Open() : JS.Promise<float[][]> = openDocument ()

    member _.Render(page: int, scale: float) : JS.Promise<RenderedPage> =
        let epoch = renderEpoch
        openDocument ()
        |> promiseBind (fun _ ->
            if disposed then rejectPromise "PDF worker disposed"
            elif epoch <> renderEpoch then rejectPromise "PDF render interrupted"
            else
                request worker true
                    (fun id -> emitJsExpr (id, page, scale) "({kind:'render', id:$0, page:$1, scale:$2})")
                    [||])
        |> promiseMap (fun response ->
            let bitmap : ImageBitmap = emitJsExpr response "$0.bitmap"
            let width : int = emitJsExpr response "$0.width"
            let height : int = emitJsExpr response "$0.height"
            let boxes : TextBox[] = emitJsExpr response "$0.boxes"
            { Bitmap = bitmap; Width = width; Height = height; Boxes = boxes })

    /// Interrupt the currently requested render. A pending reopen needs no
    /// termination: its render has not been posted and is invalidated by epoch.
    member _.CancelRender() =
        if not disposed then
            renderEpoch <- renderEpoch + 1
            if renderRequestId <> 0 then
                emitJsExpr worker "$0.terminate()"
                worker <- null
                openPromise <- None
                rejectAll "PDF render interrupted"

    member _.Dispose() =
        if not disposed then
            disposed <- true
            renderEpoch <- renderEpoch + 1
            if not (isNull worker) then emitJsExpr worker "$0.terminate()"
            worker <- null
            openPromise <- None
            retainedBytes <- null
            rejectAll "PDF worker disposed"
