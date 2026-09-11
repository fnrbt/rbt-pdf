module Rbt.Pdf.Web.PageView

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Rbt.Pdf
open Rbt.Pdf.Render.Render
open Rbt.Pdf.Web.WebApi

// ── Helpers ──────────────────────────────────────────────

[<Emit("window.devicePixelRatio || 1")>]
let private devicePixelRatio () : float = jsNative

[<Emit("requestAnimationFrame($0)")>]
let private requestAnimationFrame (cb: unit -> unit) : int = jsNative

[<Emit("$0.style[$1] = $2")>]
let private setS (el: obj) (prop: string) (v: string) : unit = jsNative

// ── Page info ────────────────────────────────────────────

type private PageInfo = {
    /// Unscaled PDF page width (points)
    W: float
    /// Unscaled PDF page height (points)
    H: float
    Div: HTMLElement
    mutable Canvas: HTMLCanvasElement option
    mutable TextDiv: HTMLElement option
    /// Generation at which this page was last rendered
    mutable Gen: int
}

// ── PageView ─────────────────────────────────────────────

type PageView(container: HTMLElement, doc: PdfDocument, opts: RenderOptions) =

    let pageCount = Pdf.getPageCount doc
    let mutable zoom = 1.0
    let mutable generation = 0
    let mutable renderPending = false
    let pendingPages = System.Collections.Generic.HashSet<int>()
    let inFlight = System.Collections.Generic.HashSet<int>()
    let mutable observer : obj = null

    // Pre-compute page sizes and create placeholder divs
    let pages : PageInfo[] =
        container.innerHTML <- ""
        [| for n in 1 .. pageCount do
            let (w, h) =
                match Pdf.getPage doc n with
                | Some pageRef ->
                    let pageObj = Pdf.deref doc pageRef
                    match PageOps.getMediaBox doc pageObj with
                    | Some (x0, y0, x1, y1) -> (x1 - x0, y1 - y0)
                    | None -> (612.0, 792.0)
                | None -> (612.0, 792.0)
            let div : HTMLElement = downcast Browser.Dom.document.createElement "div"
            div.className <- "page"
            setS div "width" (sprintf "%.0fpx" w)
            setS div "height" (sprintf "%.0fpx" h)
            setS div "position" "relative"
            setS div "overflow" "hidden"
            // Block-level box centred by auto margins. `flex: none` keeps a flex
            // host container from shrinking the page to fit the viewport, which
            // would crush every page to (viewport / pageCount) px tall.
            setS div "flex" "none"
            setS div "margin" "0 auto 8px"
            div.setAttribute("data-page", string n)
            container.appendChild(div) |> ignore
            yield { W = w; H = h; Div = div; Canvas = None; TextDiv = None; Gen = -1 } |]

    let renderOnePage (pageNum: int) (gen: int) =
        let idx = pageNum - 1
        if idx < 0 || idx >= pageCount || not (inFlight.Add(pageNum)) then ()
        else
            let info = pages.[idx]
            let currentDpr = devicePixelRatio ()
            let scale = zoom * currentDpr

            ImageSource.prepare doc pageNum
            |> promiseBind (fun () ->
                if gen <> generation then promiseReturn ()
                else
                    match renderPage opts doc pageNum scale with
                    | None -> promiseReturn ()
                    | Some (cmds, w, h, boxes) ->
                        if gen <> generation then promiseReturn ()
                        else
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
                            canvas.width <- int (ceil w)
                            canvas.height <- int (ceil h)
                            let ctx = canvas.getContext_2d()
                            CanvasBackend.render ctx cmds

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
                            TextLayer.build textDiv boxes scale currentDpr
                            info.Gen <- gen
                            promiseReturn ()
            )
            |> promiseCatch (fun err ->
                emitJsExpr (pageNum, err) "console.error('PDF page', $0, 'render failed:', $1)"
                promiseReturn ()
            )
            |> promiseMap (fun () -> inFlight.Remove(pageNum) |> ignore)
            |> ignore

    let rec scheduleRender () =
        if not renderPending then
            renderPending <- true
            requestAnimationFrame (fun () ->
                renderPending <- false
                let gen = generation
                // Render only ONE page per frame to avoid blocking the main thread.
                // Re-schedule if more pages are pending.
                match pendingPages |> Seq.tryHead with
                | Some p ->
                    pendingPages.Remove(p) |> ignore
                    renderOnePage p gen
                    if pendingPages.Count > 0 then scheduleRender ()
                | None -> ()
            ) |> ignore

    let evictPage (idx: int) =
        let info = pages.[idx]
        match info.Canvas with
        | Some c -> c.width <- 0; c.height <- 0
        | None -> ()
        match info.TextDiv with
        | Some d -> d.innerHTML <- ""
        | None -> ()
        info.Gen <- -1

    let observerCallback (entries: obj[]) =
        for entry in entries do
            let isIntersecting : bool = emitJsExpr entry "$0.isIntersecting"
            let target : HTMLElement = emitJsExpr entry "$0.target"
            let pageStr : string = emitJsExpr target "$0.getAttribute('data-page')"
            match System.Int32.TryParse(pageStr) with
            | true, pageNum ->
                let idx = pageNum - 1
                if isIntersecting then
                    if idx >= 0 && idx < pageCount && pages.[idx].Gen <> generation then
                        pendingPages.Add(pageNum) |> ignore
                        scheduleRender ()
                else
                    if idx >= 0 && idx < pageCount && pages.[idx].Gen >= 0 then
                        evictPage idx
            | _ -> ()

    let setupObserver () =
        if not (isNull observer) then
            emitJsExpr observer "$0.disconnect()"
        // rootMargin '100% 0px' pre-renders ±1 viewport height
        let obs = emitJsExpr observerCallback "new IntersectionObserver($0, { root: null, rootMargin: '100% 0px', threshold: 0 })"
        observer <- obs
        for info in pages do
            emitJsExpr (obs, info.Div) "$0.observe($1)"

    do setupObserver ()

    member _.SetZoom(z: float) =
        if z <> zoom then
            zoom <- z
            generation <- generation + 1
            for info in pages do
                setS info.Div "width" (sprintf "%.0fpx" (info.W * zoom))
                setS info.Div "height" (sprintf "%.0fpx" (info.H * zoom))
            // Re-observe — fires callbacks for visible pages
            setupObserver ()

    member _.Zoom = zoom
    member _.PageCount = pageCount

    member _.ScrollToPage(pageNum: int) =
        let idx = pageNum - 1
        if idx >= 0 && idx < pageCount then
            emitJsExpr pages.[idx].Div "$0.scrollIntoView({ behavior: 'smooth', block: 'start' })"

    member _.Dispose() =
        if not (isNull observer) then
            emitJsExpr observer "$0.disconnect()"
            observer <- null
        container.innerHTML <- ""
