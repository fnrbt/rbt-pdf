module Rbt.Pdf.Web.CanvasBackend

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Rbt.Renderer
open Rbt.Pdf.Web.WebApi

// ── Helpers ──────────────────────────────────────────────

let private css (c: Color) =
    sprintf "rgba(%d,%d,%d,%g)" c.R c.G c.B (float c.A / 255.0)

let private fillRuleStr = function
    | EvenOdd -> "evenodd"
    | NonZero -> "nonzero"

let private applyStroke (ctx: CanvasRenderingContext2D) (st: StrokeStyle) =
    // PDF: width 0 = thinnest device line. Canvas draws nothing at 0,
    // so clamp to 1 device pixel (cf. pdf.js getSinglePixelWidth).
    let det : float = emitJsExpr ctx "(function(t){return Math.abs(t.a*t.d - t.b*t.c)})(($0).getTransform())"
    let scale = max 1e-6 (sqrt det)
    ctx.lineWidth <- max (float st.Width) (1.0 / scale)
    ctx.lineCap <-
        match st.Cap with
        | Butt -> "butt" | Round -> "round" | Square -> "square"
    ctx.lineJoin <-
        match st.Join with
        | Miter -> "miter" | Bevel -> "bevel" | RoundJoin -> "round"
    ctx.miterLimit <- st.MiterLimit
    ctxSetLineDash ctx (st.DashArray |> List.map float |> Array.ofList)
    ctxSetLineDashOffset ctx (float st.DashOffset)
    match st.Paint with
    | Solid c -> ctx.strokeStyle <- !^(css c)
    | _ -> ()

// ── Main interpreter ─────────────────────────────────────

let rec private execute (ctx: CanvasRenderingContext2D) (cmd: Cmd) =
    match cmd with
    | Group (m, opacity, cmds) ->
        ctx.save()
        ctx.transform(m.M11, m.M12, m.M21, m.M22, m.DX, m.DY)
        if opacity < 1.0 then
            ctx.globalAlpha <- ctx.globalAlpha * opacity
        for c in cmds do execute ctx c
        ctx.restore()

    | Clip (segs, fr, cmds) ->
        ctx.save()
        ctxClipPath ctx (toPath2D segs) (fillRuleStr fr)
        for c in cmds do execute ctx c
        ctx.restore()

    | FillPath (segs, paint, fr) ->
        match paint with
        | Solid c ->
            ctx.fillStyle <- !^(css c)
            ctxFillPath ctx (toPath2D segs) (fillRuleStr fr)
        | _ ->
            // Gradient/tiling paints: Render.fs never emits them for PDF yet.
            ()

    | StrokePath (segs, st) ->
        applyStroke ctx st
        ctxStrokePath ctx (toPath2D segs)

    | DrawGlyphs (origin, runs, paint) ->
        match paint with
        | Solid c ->
            ctx.fillStyle <- !^(css c)
            for run in runs do
                let upm = Text.unitsPerEm run.Font
                if upm > 0.0 then
                    let s = run.FontSize / upm
                    for g in run.Glyphs do
                        match GlyphCache.get run.Font g.GlyphId with
                        | Some path ->
                            ctx.save()
                            ctx.translate(float origin.X + g.X, float origin.Y + g.Y)
                            ctx.scale(s, s)
                            ctxFillPath ctx path "nonzero"
                            ctx.restore()
                        | None -> ()
        | _ -> ()

    | DrawImage (fb, srcR, dstR, smooth) ->
        let bmp = ImageSource.canvasFor fb
        ctx.save()
        ctx.imageSmoothingEnabled <- smooth
        ctx.translate(float dstR.Pos.X, float dstR.Pos.Y + float dstR.Size.Y)
        ctx.scale(float dstR.Size.X, -(float dstR.Size.Y))
        ctxDrawImage9 ctx bmp
            (float srcR.Pos.X) (float srcR.Pos.Y) (float srcR.Size.X) (float srcR.Size.Y)
            0.0 0.0 1.0 1.0
        ctx.restore()

    | FillRect (r, c) ->
        ctx.fillStyle <- !^(css c)
        ctx.fillRect(float r.Pos.X, float r.Pos.Y, float r.Size.X, float r.Size.Y)

    | Clear c ->
        ctx.fillStyle <- !^(css c)
        // Fill the entire canvas (use a large rect; the caller should setTransform first)
        emitJsExpr ctx "$0.fillRect(0, 0, $0.canvas.width, $0.canvas.height)"

    | Line (p0, p1, c, w) ->
        ctx.strokeStyle <- !^(css c)
        ctx.lineWidth <- float w
        ctx.beginPath()
        ctx.moveTo(float p0.X, float p0.Y)
        ctx.lineTo(float p1.X, float p1.Y)
        ctx.stroke()

    | other ->
        let tag : string = emitJsExpr other "($0.tag !== undefined ? $0.tag : 'unknown')"
        warnOnce (sprintf "CanvasBackend: unsupported Cmd variant %s" tag)

/// Reset transform to identity and execute all commands.
let render (ctx: CanvasRenderingContext2D) (cmds: Cmd list) =
    ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
    for c in cmds do execute ctx c
