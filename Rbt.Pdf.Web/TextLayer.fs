module Rbt.Pdf.Web.TextLayer

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Rbt.Pdf.Web.WorkerClient
open Rbt.Renderer.CanvasApi

/// Maximum spans per page (matches pdf.js MAX_TEXT_DIVS_TO_RENDER).
let [<Literal>] private MaxSpans = 100_000

// ── Style helper ─────────────────────────────────────────

[<Emit("$0.style[$1] = $2")>]
let private setS (el: obj) (prop: string) (v: string) : unit = jsNative

// ── Run grouping ─────────────────────────────────────────

/// A run of contiguous characters on the same baseline.
type private TextRun = {
    Text: string
    X: float; Y: float
    W: float; H: float
}

/// Group wire text boxes into runs without importing the PDF renderer on the UI thread.
/// Same Y/H within 0.5 px, X-contiguous within 0.5*H.
let private groupIntoRuns (boxes: TextBox[]) : TextRun[] =
    if boxes.Length = 0 then [||]
    else
        let runs = ResizeArray<TextRun>()
        let buf = System.Text.StringBuilder()
        let firstChar, firstX, firstY, firstW, firstH = boxes.[0]
        let mutable runX = firstX
        let mutable runY = firstY
        let mutable runH = firstH
        let mutable runEndX = firstX + firstW
        buf.Append(firstChar) |> ignore

        for i in 1 .. boxes.Length - 1 do
            let ch,x,y,w,h = boxes.[i]
            let sameY = abs (y - runY) < 0.5
            let sameH = abs (h - runH) < 0.5
            let contiguous = abs (x - runEndX) < 0.5 * runH
            if sameY && sameH && contiguous then
                buf.Append(ch) |> ignore
                runEndX <- x + w
            else
                runs.Add({ Text = buf.ToString(); X = runX; Y = runY; W = runEndX - runX; H = runH })
                buf.Clear().Append(ch) |> ignore
                runX <- x; runY <- y; runH <- h
                runEndX <- x + w

        runs.Add({ Text = buf.ToString(); X = runX; Y = runY; W = runEndX - runX; H = runH })
        runs.ToArray()

/// Shared OffscreenCanvas for measureText.
let private measureCtx : CanvasRenderingContext2D =
    let oc = createOffscreenCanvas 1 1
    getOffscreenContext2D oc

/// Build text-layer spans inside `container`, positioned to overlay the canvas.
/// `scale` = zoom * dpr (matches the scale passed to renderPage).
/// `dpr`   = window.devicePixelRatio (CSS px = scaled px / dpr).
let build (container: HTMLElement) (boxes: TextBox[]) (scale: float) (dpr: float) : unit =
    container.innerHTML <- ""
    let runs = groupIntoRuns boxes
    let total = min runs.Length MaxSpans

    for i in 0 .. total - 1 do
        let run = runs.[i]
        if run.Text.Trim().Length > 0 then
            let span = Browser.Dom.document.createElement "span"
            let fontSize = run.H / dpr
            setS span "position" "absolute"
            setS span "left" (sprintf "%.2fpx" (run.X / dpr))
            setS span "top" (sprintf "%.2fpx" (run.Y / dpr))
            setS span "fontSize" (sprintf "%.2fpx" fontSize)
            setS span "fontFamily" "sans-serif"
            setS span "lineHeight" "1"
            setS span "whiteSpace" "pre"
            span.textContent <- run.Text

            // Fit span width to the PDF character box width via scaleX.
            measureCtx.font <- sprintf "%.2fpx sans-serif" fontSize
            let measured = measureCtx.measureText(run.Text)
            let measuredW = measured.width
            let targetW = run.W / dpr
            if measuredW > 0.001 then
                let sx = targetW / measuredW
                setS span "transform" (sprintf "scaleX(%.4f)" sx)
                setS span "transformOrigin" "left top"

            container.appendChild(span) |> ignore
