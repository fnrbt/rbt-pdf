module Rbt.Pdf.Web.App

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Rbt.Renderer.CanvasApi

// Open and render always happen in a dedicated module worker. Only DOM updates and
// the selectable text overlay run on the UI thread.
type PdfViewer =
    abstract setZoom: float -> unit
    abstract zoom: float
    abstract pageCount: int
    abstract scrollToPage: int -> unit
    abstract dispose: unit -> unit

type private Session = { Worker: WorkerClient.Client; mutable View: PageView.PageView option }

let private sessions : obj = createWeakMap ()

let private disposeSession (session: Session) =
    match session.View with
    | Some view -> view.Dispose()
    | None -> session.Worker.Dispose()
    session.View <- None

/// Open a PDF inside `container`. `bytes` is not detached or mutated.
/// `assetsBase` is the URL prefix for the fonts, CMaps and ucd.dat.
/// The worker entry URL is resolved relative to the installed @rbt/pdf module.
let openPdf (container: HTMLElement, bytes: byte[], assetsBase: string) : JS.Promise<PdfViewer> =
    promiseReturn ()
    |> promiseBind (fun () ->
        match weakMapTryGet<Session> sessions container with
        | Some old -> disposeSession old
        | None -> ()
        let session = { Worker = WorkerClient.Client(bytes, assetsBase); View = None }
        weakMapSet sessions container session
        session.Worker.Open()
        |> promiseMap (fun sizes ->
            if not (weakMapHas sessions container) || not (obj.ReferenceEquals(weakMapGet<Session> sessions container, session)) then
                session.Worker.Dispose()
                failwith "PDF open was superseded by another document"
            let view = PageView.PageView(container, sizes, session.Worker)
            session.View <- Some view
            { new PdfViewer with
                member _.setZoom(z) = view.SetZoom(z)
                member _.zoom = view.Zoom
                member _.pageCount = view.PageCount
                member _.scrollToPage(n) = view.ScrollToPage(n)
                member _.dispose() =
                    disposeSession session
                    if weakMapHas sessions container && obj.ReferenceEquals(weakMapGet<Session> sessions container, session) then
                        emitJsExpr (sessions, container) "$0.delete($1)" } )
        |> promiseCatch (fun err ->
            disposeSession session
            if weakMapHas sessions container && obj.ReferenceEquals(weakMapGet<Session> sessions container, session) then
                emitJsExpr (sessions, container) "$0.delete($1)"
            emitJsExpr err "Promise.reject($0)"))
