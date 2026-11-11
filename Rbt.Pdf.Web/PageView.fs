module Rbt.Pdf.Web.PageView

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Rbt.Renderer.CanvasApi

[<Emit("window.devicePixelRatio || 1")>]
let private devicePixelRatio () : float = jsNative

[<Emit("requestAnimationFrame($0)")>]
let private requestAnimationFrame (cb: unit -> unit) : int = jsNative

[<Emit("$0.style[$1] = $2")>]
let private setS (el: obj) (prop: string) (v: string) : unit = jsNative

type private PageInfo = {
    W: float
    H: float
    Div: HTMLElement
    mutable Canvas: HTMLCanvasElement option
    mutable TextDiv: HTMLElement option
    mutable Gen: int
    mutable Version: int
}

type PageView(container: HTMLElement, sizes: float[][], worker: WorkerClient.Client) =
    let pageCount = sizes.Length
    let mutable zoom = 1.0
    let mutable generation = 0
    let mutable disposed = false
    let mutable renderPending = false
    let pendingPages = System.Collections.Generic.HashSet<int>()
    let visiblePages = System.Collections.Generic.HashSet<int>()
    let inFlight = System.Collections.Generic.HashSet<int>()
    let mutable observer : obj = null

    let pages =
        container.innerHTML <- ""
        [| for i in 0 .. pageCount - 1 do
            let w, h = sizes.[i].[0], sizes.[i].[1]
            let div : HTMLElement = downcast Browser.Dom.document.createElement "div"
            div.className <- "page"
            setS div "width" (sprintf "%.0fpx" w)
            setS div "height" (sprintf "%.0fpx" h)
            setS div "position" "relative"
            setS div "overflow" "hidden"
            setS div "flex" "none"
            setS div "margin" "0 auto 8px"
            div.setAttribute("data-page", string (i + 1))
            container.appendChild(div) |> ignore
            yield { W = w; H = h; Div = div; Canvas = None; TextDiv = None; Gen = -1; Version = 0 } |]

    let evictPage (idx: int) =
        let info = pages.[idx]
        info.Version <- info.Version + 1
        match info.Canvas with
        | Some c -> c.width <- 0; c.height <- 0
        | None -> ()
        match info.TextDiv with
        | Some d -> d.innerHTML <- ""
        | None -> ()
        info.Gen <- -1

    let installPage (info: PageInfo) (result: WorkerClient.RenderedPage) (scale: float) (dpr: float) =
        // Ownership of result.Bitmap ends here. Close even if DOM/text construction fails.
        try
            let canvas =
                match info.Canvas with
                | Some c -> c
                | None ->
                    let c : HTMLCanvasElement = downcast Browser.Dom.document.createElement "canvas"
                    setS c "position" "absolute"
                    setS c "left" "0"
                    setS c "top" "0"
                    setS c "width" "100%"
                    setS c "height" "100%"
                    info.Div.appendChild(c) |> ignore
                    info.Canvas <- Some c
                    c
            canvas.width <- result.Width
            canvas.height <- result.Height
            let ctx = canvas.getContext_2d()
            ctxDrawImageBitmap ctx result.Bitmap 0.0 0.0
            let textDiv =
                match info.TextDiv with
                | Some d -> d
                | None ->
                    let d : HTMLElement = downcast Browser.Dom.document.createElement "div"
                    d.className <- "text-layer"
                    setS d "position" "absolute"
                    setS d "left" "0"
                    setS d "top" "0"
                    setS d "width" "100%"
                    setS d "height" "100%"
                    setS d "overflow" "hidden"
                    info.Div.appendChild(d) |> ignore
                    info.TextDiv <- Some d
                    d
            TextLayer.build textDiv result.Boxes scale dpr
        finally result.Bitmap.close()

    let rec scheduleRender () =
        if not disposed && not renderPending then
            renderPending <- true
            requestAnimationFrame (fun () ->
                renderPending <- false
                if not disposed then
                    match pendingPages |> Seq.tryHead with
                    | Some pageNum ->
                        pendingPages.Remove(pageNum) |> ignore
                        if visiblePages.Contains(pageNum) && inFlight.Add(pageNum) then
                            let info = pages.[pageNum - 1]
                            let gen = generation
                            let version = info.Version
                            let dpr = devicePixelRatio ()
                            let scale = zoom * dpr
                            let mutable retry = false
                            worker.Render(pageNum, scale)
                            |> promiseMap (fun rendered ->
                                if not disposed && gen = generation && version = info.Version && visiblePages.Contains(pageNum) then
                                    try
                                        installPage info rendered scale dpr
                                        info.Gen <- gen
                                    with ex ->
                                        evictPage (pageNum - 1)
                                        emitJsExpr (pageNum, ex) "console.error('PDF page', $0, 'install failed:', $1)"
                                else
                                    retry <- not disposed && visiblePages.Contains(pageNum) && (gen <> generation || version <> info.Version)
                                    rendered.Bitmap.close())
                            |> promiseCatch (fun err ->
                                if not disposed then
                                    retry <- visiblePages.Contains(pageNum) && (gen <> generation || version <> info.Version)
                                    emitJsExpr (pageNum, err) "console.error('PDF page', $0, 'render failed:', $1)"
                                promiseReturn ())
                            |> promiseMap (fun () ->
                                inFlight.Remove(pageNum) |> ignore
                                if retry && not disposed && visiblePages.Contains(pageNum) && info.Gen <> generation then
                                    pendingPages.Add(pageNum) |> ignore
                                    scheduleRender ())
                            |> ignore
                        if pendingPages.Count > 0 then scheduleRender ()
                    | None -> ()) |> ignore

    let observerCallback (entries: obj[]) =
        if not disposed then
            for entry in entries do
                let isIntersecting : bool = emitJsExpr entry "$0.isIntersecting"
                let target : HTMLElement = emitJsExpr entry "$0.target"
                let pageStr : string = emitJsExpr target "$0.getAttribute('data-page')"
                match System.Int32.TryParse(pageStr) with
                | true, pageNum when pageNum > 0 && pageNum <= pageCount ->
                    let idx = pageNum - 1
                    if isIntersecting then
                        visiblePages.Add(pageNum) |> ignore
                        if pages.[idx].Gen <> generation then
                            pendingPages.Add(pageNum) |> ignore
                            scheduleRender ()
                    else
                        visiblePages.Remove(pageNum) |> ignore
                        pendingPages.Remove(pageNum) |> ignore
                        evictPage idx
                | _ -> ()

    let setupObserver () =
        if not (isNull observer) then
            emitJsExpr observer "$0.disconnect()"
        // Pre-render a viewport height before and after the visible region.
        let obs = emitJsExpr observerCallback "new IntersectionObserver($0, { root: null, rootMargin: '100% 0px', threshold: 0 })"
        observer <- obs
        for info in pages do emitJsExpr (obs, info.Div) "$0.observe($1)"

    do setupObserver ()

    member _.SetZoom(z: float) =
        if not disposed && z > 0.0 && z <> zoom then
            zoom <- z
            generation <- generation + 1
            pendingPages.Clear()
            visiblePages.Clear()
            for i in 0 .. pageCount - 1 do
                let info = pages.[i]
                evictPage i
                setS info.Div "width" (sprintf "%.0fpx" (info.W * zoom))
                setS info.Div "height" (sprintf "%.0fpx" (info.H * zoom))
            // Fresh observer callbacks identify the pages to re-render at this zoom.
            setupObserver ()

    member _.Zoom = zoom
    member _.PageCount = pageCount

    member _.ScrollToPage(pageNum: int) =
        let idx = pageNum - 1
        if not disposed && idx >= 0 && idx < pageCount then
            emitJsExpr pages.[idx].Div "$0.scrollIntoView({ behavior: 'smooth', block: 'start' })"

    member _.Dispose() =
        if not disposed then
            disposed <- true
            generation <- generation + 1
            if not (isNull observer) then
                emitJsExpr observer "$0.disconnect()"
                observer <- null
            pendingPages.Clear()
            visiblePages.Clear()
            worker.Dispose()
            container.innerHTML <- ""
