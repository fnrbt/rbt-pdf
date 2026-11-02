module Rbt.Pdf.Render.Render

open System
open Rbt.Pdf
open Rbt.Pdf.ContentStream
open Rbt.Renderer
open Rbt.Pdf.Render.Fonts

// ── Graphics State ───────────────────────────────────────

let inline private px' (v: float) : float<px> = LanguagePrimitives.FloatWithMeasure v

type GraphicsState = {
    CTM: Matrix3x2
    FillColor: Color
    StrokeColor: Color
    LineWidth: float
    LineCap: LineCap
    LineJoin: LineJoin
    MiterLimit: float
    DashArray: float<px> list
    DashOffset: float<px>
    FillAlpha: float
    StrokeAlpha: float
    FontName: string
    FontSize: float
    CharSpacing: float
    WordSpacing: float
    HorizontalScaling: float
    TextLeading: float
    TextRise: float
    TextRenderMode: int
    ColorSpace: string
    StrokeColorSpace: string
    BlendMode: string
}

let private defaultState = {
    CTM = Matrix3x2.identity
    FillColor = Color.black
    StrokeColor = Color.black
    LineWidth = 1.0
    LineCap = Butt
    LineJoin = Miter
    MiterLimit = 10.0
    DashArray = []
    DashOffset = 0.0<px>
    FillAlpha = 1.0
    StrokeAlpha = 1.0
    FontName = ""
    FontSize = 12.0
    CharSpacing = 0.0
    WordSpacing = 0.0
    HorizontalScaling = 100.0
    TextLeading = 0.0
    TextRise = 0.0
    TextRenderMode = 0
    ColorSpace = "DeviceGray"
    StrokeColorSpace = "DeviceGray"
    BlendMode = "Normal"
}

// ── Color Conversion ─────────────────────────────────────

let private clamp lo hi v = max lo (min hi v)

let private pdfColorToColor (cs: string) (components: float list) (alpha: float) : Color =
    let a = byte (clamp 0.0 255.0 (alpha * 255.0))
    match cs, components with
    | "DeviceGray", [g] ->
        let v = byte (clamp 0.0 255.0 (g * 255.0))
        Color.rgba v v v a
    | "DeviceRGB", [r; g; b] ->
        Color.rgba (byte (clamp 0.0 255.0 (r * 255.0))) (byte (clamp 0.0 255.0 (g * 255.0))) (byte (clamp 0.0 255.0 (b * 255.0))) a
    | "DeviceCMYK", [c; m; y; k] ->
        let r = (1.0 - c) * (1.0 - k)
        let g = (1.0 - m) * (1.0 - k)
        let b = (1.0 - y) * (1.0 - k)
        Color.rgba (byte (clamp 0.0 255.0 (r * 255.0))) (byte (clamp 0.0 255.0 (g * 255.0))) (byte (clamp 0.0 255.0 (b * 255.0))) a
    | _ ->
        match components with
        | [g] -> let v = byte (clamp 0.0 255.0 (g * 255.0)) in Color.rgba v v v a
        | [r; g; b] -> Color.rgba (byte (clamp 0.0 255.0 (r * 255.0))) (byte (clamp 0.0 255.0 (g * 255.0))) (byte (clamp 0.0 255.0 (b * 255.0))) a
        | _ -> Color.rgba 0uy 0uy 0uy a

// ── Path Building ────────────────────────────────────────

type private PathBuilder() =
    let segs = ResizeArray<PathSeg>()
    let mutable curX = 0.0
    let mutable curY = 0.0

    member _.CurrentPoint = (curX, curY)
    member _.Segments = segs |> Seq.toList
    member _.Clear() = segs.Clear(); curX <- 0.0; curY <- 0.0
    member _.IsEmpty = segs.Count = 0

    member _.MoveTo(x, y) =
        segs.Add(MoveTo(vec2 x y))
        curX <- x; curY <- y

    member _.LineTo(x, y) =
        segs.Add(LineTo(vec2 x y))
        curX <- x; curY <- y

    member _.CubicTo(x1, y1, x2, y2, x3, y3) =
        segs.Add(CubicTo(vec2 x1 y1, vec2 x2 y2, vec2 x3 y3))
        curX <- x3; curY <- y3

    member _.Close() = segs.Add(Close)

    member this.Rect(x, y, w, h) =
        this.MoveTo(x, y)
        this.LineTo(x + w, y)
        this.LineTo(x + w, y + h)
        this.LineTo(x, y + h)
        this.Close()

// ── Stroke Style Builder ─────────────────────────────────

let private makeStroke (gs: GraphicsState) : StrokeStyle =
    let color = { gs.StrokeColor with A = byte (clamp 0.0 255.0 (gs.StrokeAlpha * 255.0)) }
    { Width = px' gs.LineWidth
      Cap = gs.LineCap
      Join = gs.LineJoin
      Paint = Solid color
      DashArray = gs.DashArray
      DashOffset = gs.DashOffset
      MiterLimit = gs.MiterLimit }

let private fillColor (gs: GraphicsState) : Color =
    { gs.FillColor with A = byte (clamp 0.0 255.0 (gs.FillAlpha * 255.0)) }

// ── Character Position Map ───────────────────────────────

type CharBox = {
    Char: string
    X: float; Y: float
    W: float; H: float
}

type RenderOptions =
    { FallbackFont: Text.FontData
      /// JPEG (DCTDecode) raw stream bytes -> (w, h, premultiplied BGRA uint32[]). None = skip image.
      DecodeJpeg: byte[] -> (int * int * uint32[]) option }

#if !FABLE_COMPILER
module RenderOptions =
    /// Where the native options look for the fallback font (NotoSans-Regular.ttf), in order: next to the binary,
    /// in the current directory, in this repository's assets/. An application ships the font next to its binary;
    /// __SOURCE_DIRECTORY__ is only meaningful on a dev machine, and a package build maps it to a placeholder path.
    let fallbackFontCandidates () =
        [ System.IO.Path.Combine(System.AppContext.BaseDirectory, "NotoSans-Regular.ttf")
          System.IO.Path.Combine(System.Environment.CurrentDirectory, "NotoSans-Regular.ttf")
          System.IO.Path.GetFullPath(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../assets/NotoSans-Regular.ttf")) ]

    // Loaded on first use, once.
    let private nativeOpts =
        lazy
            let candidates = fallbackFontCandidates ()
            let fontPath =
                match candidates |> List.tryFind System.IO.File.Exists with
                | Some path -> path
                | None ->
                    raise (System.IO.FileNotFoundException(
                        "Rbt.Pdf.Render: fallback font NotoSans-Regular.ttf not found; looked in: " + String.concat ", " candidates,
                        "NotoSans-Regular.ttf"))
            { FallbackFont = Text.loadFontFile fontPath
              DecodeJpeg = ImageDecode.decode }

    /// Options for .NET: the fallback font from fallbackFontCandidates, JPEG decoding by ImageDecode.decode.
    let native () = nativeOpts.Value
#endif

// ── Main Interpreter ─────────────────────────────────────

let renderPage (opts: RenderOptions) (doc: PdfDocument) (pageNum: int) (scale: float) : (Cmd list * float * float * CharBox[]) option =
    match Pdf.getPage doc pageNum with
    | None -> None
    | Some pageRef ->
        let pageObj = Pdf.deref doc pageRef
        match PageOps.getMediaBox doc pageObj with
        | None -> None
        | Some (x0, y0, x1, y1) ->
            let pageW = (x1 - x0) * scale
            let pageH = (y1 - y0) * scale

            let mutable currentResources =
                match Pdf.dictGet doc "Resources" pageObj with
                | Some r -> r
                | None ->
                    match Pdf.getPageInheritable doc pageObj "Resources" with
                    | Some r -> r
                    | None -> PdfDict Map.empty

            let contentData = PageOps.getPageContents doc pageObj
            if contentData.Length = 0 then Some ([], pageW, pageH, [||])
            else

            let tokens = tokenize contentData

            let rawPageH = y1 - y0

            // Interpreter state
            let stateStack = ResizeArray<GraphicsState>()
            let mutable gs = defaultState
            let path = PathBuilder()
            let cmds = ResizeArray<Cmd>()
            let textCmds = ResizeArray<Cmd>()
            let charBoxes = ResizeArray<CharBox>()

            // Text state
            let mutable textMatrix = Matrix3x2.identity
            let mutable lineMatrix = Matrix3x2.identity
            let mutable inText = false

            // Clipping state
            let mutable pendingClip: (PathSeg list * FillRule) option = None

            // Operand stack
            let operands = ResizeArray<Token>()

            let popNum () =
                if operands.Count > 0 then
                    let t = operands.[operands.Count - 1]
                    operands.RemoveAt(operands.Count - 1)
                    match t with TNumber n -> n | _ -> 0.0
                else 0.0

            let popString () =
                if operands.Count > 0 then
                    let t = operands.[operands.Count - 1]
                    operands.RemoveAt(operands.Count - 1)
                    match t with TString s -> s | THexString s -> s | _ -> [||]
                else [||]

            let popName () =
                if operands.Count > 0 then
                    let t = operands.[operands.Count - 1]
                    operands.RemoveAt(operands.Count - 1)
                    match t with TName n -> n | TOperator n -> n | _ -> ""
                else ""

            let popAllOperands () =
                let result = operands |> Seq.toList
                operands.Clear()
                result

            let popArray () =
                let result = operands |> Seq.toList
                operands.Clear()
                result

            let getFontInfo () = Fonts.resolve doc currentResources gs.FontName

            let decodeTextBytes (stringBytes: byte[]) : string =
                let fi = getFontInfo ()
                match fi.ToUnicodeCMap with
                | Some cmap ->
                    match Fonts.decodeWithToUnicode cmap stringBytes with
                    | Some s -> s
                    | None -> Fonts.decodeByEncoding fi.Encoding stringBytes
                | None -> Fonts.decodeByEncoding fi.Encoding stringBytes

            let fallbackFont = opts.FallbackFont

            let getRendererFont () =
                let fi = getFontInfo ()
                match fi.EmbeddedFont with
                | Some f -> f
                | None -> fallbackFont

            let emitTextRun (stringBytes: byte[]) =
                let fi = getFontInfo ()
                let text = decodeTextBytes stringBytes
                if text.Length > 0 || stringBytes.Length > 0 then
                    let f = getRendererFont ()
                    let tmScale = sqrt(textMatrix.M21 * textMatrix.M21 + textMatrix.M22 * textMatrix.M22)
                    let ctmScale = sqrt(gs.CTM.M21 * gs.CTM.M21 + gs.CTM.M22 * gs.CTM.M22)
                    let fontSize = abs gs.FontSize * tmScale * ctmScale
                    let th = gs.HorizontalScaling / 100.0

                    let runs, tw =
                        match fi.EmbeddedFont, fi.ByteToGlyph with
                        | Some embFont, Some glyphMap ->
                            let scale = fontSize / Text.unitsPerEm embFont
                            let tcDevice = gs.CharSpacing * tmScale * ctmScale
                            let twDevice = gs.WordSpacing * tmScale * ctmScale
                            let mutable x = 0.0
                            let glyphs : Text.PositionedGlyph[] =
                                [| for b in stringBytes do
                                    let code = int b
                                    let gid = if code < glyphMap.Length then glyphMap.[code] else 0
                                    let baseAdv =
                                        let gw = if gid >= 0 && gid < embFont.ShFont.GlyphWidths.Length then embFont.ShFont.GlyphWidths.[gid] else 0
                                        if gw > 0 then float gw * scale
                                        else Fonts.charWidth fi code / 1000.0 * fontSize
                                    let totalAdv = (baseAdv + tcDevice + (if b = 0x20uy then twDevice else 0.0)) * th
                                    yield { GlyphId = gid; X = x; Y = 0.0; XAdv = totalAdv }
                                    x <- x + totalAdv |]
                            let totalW = glyphs |> Array.sumBy (fun g -> g.XAdv)
                            let run : Text.GlyphRun = { Font = embFont; FontSize = fontSize; Glyphs = glyphs; Direction = Rbt.Shaper.Shaper.LeftToRight }
                            [run], totalW
                        | _ ->
                            if text.Length > 0 then
                                let runs = Text.layout f fontSize text
                                let baseTw = Text.measureWidth f fontSize text
                                let charCount = float text.Length
                                let spaceCount = text |> Seq.filter ((=) ' ') |> Seq.length |> float
                                let tw = (baseTw + (charCount * gs.CharSpacing + spaceCount * gs.WordSpacing) * tmScale * ctmScale) * th
                                runs, tw
                            else [], 0.0

                    let combined = Matrix3x2.multiply textMatrix gs.CTM
                    let screenX = combined.DX
                    let screenY = rawPageH - combined.DY

                    if runs.Length > 0 && gs.TextRenderMode <> 3 then
                        let origin = vec2 screenX screenY

                        let fColor = fillColor gs
                        let sColor = { gs.StrokeColor with A = byte (clamp 0.0 255.0 (gs.StrokeAlpha * 255.0)) }

                        match gs.TextRenderMode with
                        | 1 -> textCmds.Add(DrawGlyphs(origin, runs, Solid sColor))
                        | 2 ->
                            textCmds.Add(DrawGlyphs(origin, runs, Solid fColor))
                            textCmds.Add(DrawGlyphs(origin, runs, Solid sColor))
                        | _ -> textCmds.Add(DrawGlyphs(origin, runs, Solid fColor))

                    // Emit CharBoxes for text selection
                    if runs.Length > 0 then
                        let asc = fontSize * 0.8
                        let desc = fontSize * 0.2
                        let lineH = asc + desc
                        match fi.EmbeddedFont, fi.ByteToGlyph with
                        | Some _, Some _ ->
                            // Glyph-by-glyph path: use decoded text characters
                            let glyphs = runs.[0].Glyphs
                            let mutable ci = 0
                            for g in glyphs do
                                let ch = if ci < text.Length then string text.[ci] else ""
                                ci <- ci + 1
                                charBoxes.Add({
                                    Char = ch
                                    X = screenX + g.X; Y = screenY - asc
                                    W = g.XAdv; H = lineH
                                })
                        | _ ->
                            // Fallback: distribute width across characters
                            if text.Length > 0 then
                                let charW = tw / float text.Length
                                let mutable cx = 0.0
                                for c in text do
                                    charBoxes.Add({
                                        Char = string c
                                        X = screenX + cx; Y = screenY - asc
                                        W = charW; H = lineH
                                    })
                                    cx <- cx + charW

                    // Text matrix advance in text-space units using PDF formula
                    let mutable tsWidth = 0.0
                    for b in stringBytes do
                        let w0 = Fonts.charWidth fi (int b)
                        let ws = if b = 0x20uy then gs.WordSpacing else 0.0
                        tsWidth <- tsWidth + (w0 / 1000.0 * gs.FontSize + gs.CharSpacing + ws) * th
                    textMatrix <- Matrix3x2.multiply (Matrix3x2.translate tsWidth 0.0) textMatrix

            let emitCompositeTextRun (stringBytes: byte[]) =
                let fi = getFontInfo ()
                if stringBytes.Length >= 2 then
                    let f = getRendererFont ()
                    let tmScale = sqrt(textMatrix.M21 * textMatrix.M21 + textMatrix.M22 * textMatrix.M22)
                    let ctmScale = sqrt(gs.CTM.M21 * gs.CTM.M21 + gs.CTM.M22 * gs.CTM.M22)
                    let fontSize = abs gs.FontSize * tmScale * ctmScale
                    let th = gs.HorizontalScaling / 100.0

                    match fi.EmbeddedFont with
                    | Some embFont ->
                        let scale = fontSize / Text.unitsPerEm embFont
                        let tcDevice = gs.CharSpacing * tmScale * ctmScale
                        let mutable x = 0.0
                        let glyphs = ResizeArray<Text.PositionedGlyph>()
                        let mutable i = 0
                        while i + 1 < stringBytes.Length do
                            let hi = int stringBytes.[i]
                            let lo = int stringBytes.[i + 1]
                            let cid = (hi <<< 8) ||| lo
                            let gid =
                                match fi.CIDToGIDMap with
                                | Some map -> if cid < map.Length then map.[cid] else cid
                                | None -> cid
                            let w0 = Fonts.charWidth fi cid
                            let baseAdv = w0 / 1000.0 * fontSize
                            let totalAdv = (baseAdv + tcDevice) * th
                            glyphs.Add({ GlyphId = gid; X = x; Y = 0.0; XAdv = totalAdv })
                            x <- x + totalAdv
                            i <- i + 2

                        let combined = Matrix3x2.multiply textMatrix gs.CTM
                        let screenX = combined.DX
                        let screenY = rawPageH - combined.DY

                        if glyphs.Count > 0 && gs.TextRenderMode <> 3 then
                            let origin = vec2 screenX screenY
                            let run : Text.GlyphRun = { Font = embFont; FontSize = fontSize; Glyphs = glyphs.ToArray(); Direction = Rbt.Shaper.Shaper.LeftToRight }
                            let runs = [run]

                            let fColor = fillColor gs
                            let sColor = { gs.StrokeColor with A = byte (clamp 0.0 255.0 (gs.StrokeAlpha * 255.0)) }

                            match gs.TextRenderMode with
                            | 1 -> textCmds.Add(DrawGlyphs(origin, runs, Solid sColor))
                            | 2 ->
                                textCmds.Add(DrawGlyphs(origin, runs, Solid fColor))
                                textCmds.Add(DrawGlyphs(origin, runs, Solid sColor))
                            | _ -> textCmds.Add(DrawGlyphs(origin, runs, Solid fColor))

                        // Emit CharBoxes for composite text
                        if glyphs.Count > 0 then
                            let text = decodeTextBytes stringBytes
                            let asc = fontSize * 0.8
                            let desc = fontSize * 0.2
                            let lineH = asc + desc
                            let glyphArr = glyphs.ToArray()
                            let mutable ci = 0
                            for g in glyphArr do
                                let ch = if ci < text.Length then string text.[ci] else ""
                                ci <- ci + 1
                                charBoxes.Add({
                                    Char = ch
                                    X = screenX + g.X; Y = screenY - asc
                                    W = g.XAdv; H = lineH
                                })

                        // Text matrix advance
                        let mutable tsWidth = 0.0
                        let mutable j = 0
                        while j + 1 < stringBytes.Length do
                            let cid = (int stringBytes.[j] <<< 8) ||| int stringBytes.[j + 1]
                            let w0 = Fonts.charWidth fi cid
                            tsWidth <- tsWidth + (w0 / 1000.0 * gs.FontSize + gs.CharSpacing) * th
                            j <- j + 2
                        textMatrix <- Matrix3x2.multiply (Matrix3x2.translate tsWidth 0.0) textMatrix
                    | None ->
                        // Fallback: decode with ToUnicode and use shaper
                        let text = decodeTextBytes stringBytes
                        if text.Length > 0 then
                            let runs = Text.layout f fontSize text
                            let tw = Text.measureWidth f fontSize text
                            let combined = Matrix3x2.multiply textMatrix gs.CTM
                            let screenX = combined.DX
                            let screenY = rawPageH - combined.DY
                            if runs.Length > 0 && gs.TextRenderMode <> 3 then
                                let origin = vec2 screenX screenY
                                textCmds.Add(DrawGlyphs(origin, runs, Solid (fillColor gs)))
                            // Emit CharBoxes for fallback composite text
                            if text.Length > 0 then
                                let asc = fontSize * 0.8
                                let lineH = fontSize
                                let charW = tw / float text.Length
                                let mutable cx = 0.0
                                for c in text do
                                    charBoxes.Add({
                                        Char = string c
                                        X = screenX + cx; Y = screenY - asc
                                        W = charW; H = lineH
                                    })
                                    cx <- cx + charW
                            let advance = if tmScale > 0.001 then tw / tmScale else tw
                            textMatrix <- Matrix3x2.multiply (Matrix3x2.translate advance 0.0) textMatrix

            let emitText (stringBytes: byte[]) =
                let fi = getFontInfo ()
                if fi.IsComposite then emitCompositeTextRun stringBytes
                else emitTextRun stringBytes

            let resolveColorSpace (csName: string) =
                match csName with
                | "DeviceGray" | "DeviceRGB" | "DeviceCMYK" | "G" | "RGB" | "CMYK" -> csName
                | _ ->
                    match Pdf.dictGet doc "ColorSpace" currentResources with
                    | Some csDict ->
                        match Pdf.dictGet doc csName csDict with
                        | Some csObj ->
                            let resolved = Pdf.deref doc csObj
                            match resolved with
                            | PdfArray items when items.Length >= 2 ->
                                let typeName = match Pdf.deref doc items.[0] with PdfName n -> n | _ -> ""
                                match typeName with
                                | "Separation" ->
                                    if items.Length >= 3 then
                                        match Pdf.deref doc items.[2] with
                                        | PdfName alt -> "Separation:" + alt
                                        | _ -> "DeviceGray"
                                    else "DeviceGray"
                                | "ICCBased" ->
                                    if items.Length >= 2 then
                                        match Pdf.dictGet doc "N" items.[1] with
                                        | Some (PdfInt 1L) -> "DeviceGray"
                                        | Some (PdfInt 3L) -> "DeviceRGB"
                                        | Some (PdfInt 4L) -> "DeviceCMYK"
                                        | _ -> "DeviceRGB"
                                    else "DeviceRGB"
                                | "CalGray" -> "DeviceGray"
                                | "CalRGB" | "Lab" -> "DeviceRGB"
                                | _ -> "DeviceGray"
                            | PdfName n -> n
                            | _ -> "DeviceGray"
                        | None -> csName
                    | None -> csName

            let emitPath (fill: bool) (stroke: bool) (fillRule: FillRule) =
                let segs = path.Segments
                if segs.Length > 0 then
                    if fill then
                        cmds.Add(FillPath(segs, Solid (fillColor gs), fillRule))
                    if stroke then
                        cmds.Add(StrokePath(segs, makeStroke gs))
                path.Clear()

            let clipStack = ResizeArray<(int * PathSeg list * FillRule) option>()

            let streamStack = Collections.Generic.Stack<Token[] * int * PdfObj>()
            let mutable tokenArr = tokens |> Array.ofList
            let mutable tokenCount = tokenArr.Length
            let mutable i = 0

            let startClipIfPending () =
                match pendingClip with
                | Some (clipSegs, clipRule) ->
                    pendingClip <- None
                    clipStack.Add(Some (cmds.Count, clipSegs, clipRule))
                | None -> ()

            let mutable loopRunning = true
            while loopRunning do
                if i >= tokenCount then
                    if streamStack.Count > 0 then
                        let (prevArr, prevI, prevRes) = streamStack.Pop()
                        if stateStack.Count > 0 then
                            gs <- stateStack.[stateStack.Count - 1]
                            stateStack.RemoveAt(stateStack.Count - 1)
                        let mutable poppingClips2 = true
                        while poppingClips2 && clipStack.Count > 0 do
                            match clipStack.[clipStack.Count - 1] with
                            | Some (startIdx, segs, rule) ->
                                let clippedCmds = cmds |> Seq.skip startIdx |> Seq.toList
                                cmds.RemoveRange(startIdx, cmds.Count - startIdx)
                                cmds.Add(Clip(segs, rule, clippedCmds))
                                clipStack.RemoveAt(clipStack.Count - 1)
                            | None ->
                                clipStack.RemoveAt(clipStack.Count - 1)
                                poppingClips2 <- false
                        tokenArr <- prevArr
                        tokenCount <- prevArr.Length
                        i <- prevI
                        currentResources <- prevRes
                    else
                        loopRunning <- false
                else

                let tok = tokenArr.[i]
                i <- i + 1

                match tok with
                | TNumber _ | TString _ | THexString _ | TName _ | TBool _ | TInlineImageData _ ->
                    operands.Add(tok)

                | TArrayStart ->
                    operands.Add(TArrayStart)

                | TArrayEnd ->
                    operands.Add(TArrayEnd)

                | TOperator op ->
                    match op with
                    // ── Graphics State ──
                    | "q" ->
                        stateStack.Add(gs)
                        clipStack.Add(None)
                    | "Q" ->
                        if stateStack.Count > 0 then
                            gs <- stateStack.[stateStack.Count - 1]
                            stateStack.RemoveAt(stateStack.Count - 1)
                        let mutable poppingClips = true
                        while poppingClips && clipStack.Count > 0 do
                            match clipStack.[clipStack.Count - 1] with
                            | Some (startIdx, segs, rule) ->
                                let clippedCmds = cmds |> Seq.skip startIdx |> Seq.toList
                                cmds.RemoveRange(startIdx, cmds.Count - startIdx)
                                cmds.Add(Clip(segs, rule, clippedCmds))
                                clipStack.RemoveAt(clipStack.Count - 1)
                            | None ->
                                clipStack.RemoveAt(clipStack.Count - 1)
                                poppingClips <- false

                    | "cm" ->
                        let f = popNum ()
                        let e = popNum ()
                        let d = popNum ()
                        let c = popNum ()
                        let b = popNum ()
                        let a = popNum ()
                        let m = { M11 = a; M12 = b; M21 = c; M22 = d; DX = e; DY = f }
                        gs <- { gs with CTM = Matrix3x2.multiply m gs.CTM }

                    | "w" -> gs <- { gs with LineWidth = popNum () }
                    | "J" ->
                        let cap = int (popNum ())
                        gs <- { gs with LineCap = match cap with 1 -> Round | 2 -> Square | _ -> Butt }
                    | "j" ->
                        let join = int (popNum ())
                        gs <- { gs with LineJoin = match join with 1 -> RoundJoin | 2 -> Bevel | _ -> Miter }
                    | "M" -> gs <- { gs with MiterLimit = popNum () }
                    | "d" ->
                        let phase = popNum ()
                        let arr = popAllOperands ()
                        let dashValues =
                            arr |> List.choose (function TNumber n -> Some (px' n) | _ -> None)
                        gs <- { gs with DashArray = dashValues; DashOffset = px' phase }

                    | "i" -> popNum () |> ignore
                    | "gs" ->
                        let gsName = popName ()
                        match Pdf.dictGet doc "ExtGState" currentResources with
                        | Some extGStateDict ->
                            match Pdf.dictGet doc gsName extGStateDict with
                            | Some gsObj ->
                                match Pdf.dictGet doc "ca" gsObj with
                                | Some (PdfReal a) -> gs <- { gs with FillAlpha = a }
                                | Some (PdfInt a) -> gs <- { gs with FillAlpha = float a }
                                | _ -> ()
                                match Pdf.dictGet doc "CA" gsObj with
                                | Some (PdfReal a) -> gs <- { gs with StrokeAlpha = a }
                                | Some (PdfInt a) -> gs <- { gs with StrokeAlpha = float a }
                                | _ -> ()
                                match Pdf.dictGet doc "LW" gsObj with
                                | Some (PdfReal w) -> gs <- { gs with LineWidth = w }
                                | Some (PdfInt w) -> gs <- { gs with LineWidth = float w }
                                | _ -> ()
                                match Pdf.dictGet doc "LC" gsObj with
                                | Some (PdfInt c) -> gs <- { gs with LineCap = match int c with 1 -> Round | 2 -> Square | _ -> Butt }
                                | _ -> ()
                                match Pdf.dictGet doc "LJ" gsObj with
                                | Some (PdfInt j) -> gs <- { gs with LineJoin = match int j with 1 -> RoundJoin | 2 -> Bevel | _ -> Miter }
                                | _ -> ()
                                match Pdf.dictGet doc "BM" gsObj with
                                | Some (PdfName bm) -> gs <- { gs with BlendMode = bm }
                                | Some (PdfArray (first :: _)) ->
                                    match Pdf.deref doc first with
                                    | PdfName bm -> gs <- { gs with BlendMode = bm }
                                    | _ -> ()
                                | _ -> ()
                            | None -> ()
                        | None -> ()

                    // ── Color ──
                    | "g" -> let gray = popNum () in gs <- { gs with FillColor = pdfColorToColor "DeviceGray" [gray] gs.FillAlpha; ColorSpace = "DeviceGray" }
                    | "G" -> let gray = popNum () in gs <- { gs with StrokeColor = pdfColorToColor "DeviceGray" [gray] gs.StrokeAlpha; StrokeColorSpace = "DeviceGray" }
                    | "rg" ->
                        let b = popNum () in let g = popNum () in let r = popNum ()
                        gs <- { gs with FillColor = pdfColorToColor "DeviceRGB" [r; g; b] gs.FillAlpha; ColorSpace = "DeviceRGB" }
                    | "RG" ->
                        let b = popNum () in let g = popNum () in let r = popNum ()
                        gs <- { gs with StrokeColor = pdfColorToColor "DeviceRGB" [r; g; b] gs.StrokeAlpha; StrokeColorSpace = "DeviceRGB" }
                    | "k" ->
                        let k = popNum () in let y = popNum () in let m = popNum () in let c = popNum ()
                        gs <- { gs with FillColor = pdfColorToColor "DeviceCMYK" [c; m; y; k] gs.FillAlpha; ColorSpace = "DeviceCMYK" }
                    | "K" ->
                        let k = popNum () in let y = popNum () in let m = popNum () in let c = popNum ()
                        gs <- { gs with StrokeColor = pdfColorToColor "DeviceCMYK" [c; m; y; k] gs.StrokeAlpha; StrokeColorSpace = "DeviceCMYK" }
                    | "cs" ->
                        let csName = popName ()
                        gs <- { gs with ColorSpace = resolveColorSpace csName }
                    | "CS" ->
                        let csName = popName ()
                        gs <- { gs with StrokeColorSpace = resolveColorSpace csName }
                    | "sc" | "scn" ->
                        let ops = popAllOperands ()
                        let nums = ops |> List.choose (function TNumber n -> Some n | _ -> None)
                        let color =
                            if gs.ColorSpace.StartsWith("Separation:") then
                                let alt = gs.ColorSpace.Substring(11)
                                let tint = match nums with [t] -> t | _ -> 0.0
                                match alt with
                                | "DeviceCMYK" -> pdfColorToColor "DeviceCMYK" [tint; tint; tint; tint] gs.FillAlpha
                                | "DeviceRGB" -> pdfColorToColor "DeviceRGB" [1.0-tint; 1.0-tint; 1.0-tint] gs.FillAlpha
                                | _ -> pdfColorToColor "DeviceGray" [1.0-tint] gs.FillAlpha
                            else pdfColorToColor gs.ColorSpace nums gs.FillAlpha
                        gs <- { gs with FillColor = color }
                    | "SC" | "SCN" ->
                        let ops = popAllOperands ()
                        let nums = ops |> List.choose (function TNumber n -> Some n | _ -> None)
                        let color =
                            if gs.StrokeColorSpace.StartsWith("Separation:") then
                                let alt = gs.StrokeColorSpace.Substring(11)
                                let tint = match nums with [t] -> t | _ -> 0.0
                                match alt with
                                | "DeviceCMYK" -> pdfColorToColor "DeviceCMYK" [tint; tint; tint; tint] gs.StrokeAlpha
                                | "DeviceRGB" -> pdfColorToColor "DeviceRGB" [1.0-tint; 1.0-tint; 1.0-tint] gs.StrokeAlpha
                                | _ -> pdfColorToColor "DeviceGray" [1.0-tint] gs.StrokeAlpha
                            else pdfColorToColor gs.StrokeColorSpace nums gs.StrokeAlpha
                        gs <- { gs with StrokeColor = color }

                    // ── Path Construction ──
                    | "m" -> let y = popNum () in let x = popNum () in path.MoveTo(x, y)
                    | "l" -> let y = popNum () in let x = popNum () in path.LineTo(x, y)
                    | "c" ->
                        let y3 = popNum () in let x3 = popNum ()
                        let y2 = popNum () in let x2 = popNum ()
                        let y1 = popNum () in let x1 = popNum ()
                        path.CubicTo(x1, y1, x2, y2, x3, y3)
                    | "v" ->
                        let y3 = popNum () in let x3 = popNum ()
                        let y2 = popNum () in let x2 = popNum ()
                        let (cx, cy) = path.CurrentPoint
                        path.CubicTo(cx, cy, x2, y2, x3, y3)
                    | "y" ->
                        let y3 = popNum () in let x3 = popNum ()
                        let y1 = popNum () in let x1 = popNum ()
                        path.CubicTo(x1, y1, x3, y3, x3, y3)
                    | "h" -> path.Close()
                    | "re" ->
                        let h = popNum () in let w = popNum ()
                        let y = popNum () in let x = popNum ()
                        path.Rect(x, y, w, h)

                    // ── Path Painting ──
                    | "S" -> emitPath false true NonZero; operands.Clear(); startClipIfPending ()
                    | "s" -> path.Close(); emitPath false true NonZero; operands.Clear(); startClipIfPending ()
                    | "f" | "F" -> emitPath true false NonZero; operands.Clear(); startClipIfPending ()
                    | "f*" -> emitPath true false EvenOdd; operands.Clear(); startClipIfPending ()
                    | "B" -> emitPath true true NonZero; operands.Clear(); startClipIfPending ()
                    | "B*" -> emitPath true true EvenOdd; operands.Clear(); startClipIfPending ()
                    | "b" -> path.Close(); emitPath true true NonZero; operands.Clear(); startClipIfPending ()
                    | "b*" -> path.Close(); emitPath true true EvenOdd; operands.Clear(); startClipIfPending ()
                    | "n" -> path.Clear(); operands.Clear(); startClipIfPending ()

                    // ── Clipping ──
                    | "W" ->
                        let segs = path.Segments
                        if segs.Length > 0 then pendingClip <- Some (segs, NonZero)
                    | "W*" ->
                        let segs = path.Segments
                        if segs.Length > 0 then pendingClip <- Some (segs, EvenOdd)

                    // ── Text ──
                    | "BT" ->
                        inText <- true
                        textMatrix <- Matrix3x2.identity
                        lineMatrix <- Matrix3x2.identity
                    | "ET" -> inText <- false

                    | "Tf" ->
                        let size = popNum ()
                        let name = popName ()
                        gs <- { gs with FontName = name; FontSize = size }

                    | "Tc" -> gs <- { gs with CharSpacing = popNum () }
                    | "Tw" -> gs <- { gs with WordSpacing = popNum () }
                    | "Tz" -> gs <- { gs with HorizontalScaling = popNum () }
                    | "TL" -> gs <- { gs with TextLeading = popNum () }
                    | "Tr" -> gs <- { gs with TextRenderMode = int (popNum ()) }
                    | "Ts" -> gs <- { gs with TextRise = popNum () }

                    | "Td" ->
                        let ty = popNum ()
                        let tx = popNum ()
                        let m = Matrix3x2.translate tx ty
                        textMatrix <- Matrix3x2.multiply m lineMatrix
                        lineMatrix <- textMatrix

                    | "TD" ->
                        let ty = popNum ()
                        let tx = popNum ()
                        gs <- { gs with TextLeading = -ty }
                        let m = Matrix3x2.translate tx ty
                        textMatrix <- Matrix3x2.multiply m lineMatrix
                        lineMatrix <- textMatrix

                    | "Tm" ->
                        let f = popNum () in let e = popNum ()
                        let d = popNum () in let c = popNum ()
                        let b = popNum () in let a = popNum ()
                        textMatrix <- { M11 = a; M12 = b; M21 = c; M22 = d; DX = e; DY = f }
                        lineMatrix <- textMatrix

                    | "T*" ->
                        let m = Matrix3x2.translate 0.0 (-gs.TextLeading)
                        textMatrix <- Matrix3x2.multiply m lineMatrix
                        lineMatrix <- textMatrix

                    | "Tj" ->
                        let s = popString ()
                        emitText s

                    | "'" ->
                        let m = Matrix3x2.translate 0.0 (-gs.TextLeading)
                        textMatrix <- Matrix3x2.multiply m lineMatrix
                        lineMatrix <- textMatrix
                        let s = popString ()
                        emitText s

                    | "\"" ->
                        let s = popString ()
                        let ac = popNum ()
                        let aw = popNum ()
                        gs <- { gs with WordSpacing = aw; CharSpacing = ac }
                        let m = Matrix3x2.translate 0.0 (-gs.TextLeading)
                        textMatrix <- Matrix3x2.multiply m lineMatrix
                        lineMatrix <- textMatrix
                        emitText s

                    | "TJ" ->
                        let arr = popAllOperands ()
                        for item in arr do
                            match item with
                            | TString s -> emitText s
                            | THexString s -> emitText s
                            | TNumber n ->
                                let shift = -n / 1000.0 * gs.FontSize * (gs.HorizontalScaling / 100.0)
                                textMatrix <- Matrix3x2.multiply (Matrix3x2.translate shift 0.0) textMatrix
                            | _ -> ()

                    // ── XObject (Images and Forms) ──
                    | "Do" ->
                        let name = popName ()
                        match Pdf.dictGet doc "XObject" currentResources with
                        | Some xobjDict ->
                            match Pdf.dictGet doc name xobjDict with
                            | Some xobj ->
                                let subtype =
                                    match Pdf.dictGet doc "Subtype" xobj with
                                    | Some (PdfName s) -> s
                                    | _ -> ""
                                match subtype with
                                | "Image" ->
                                    let imgW =
                                        match Pdf.dictGet doc "Width" xobj with
                                        | Some (PdfInt n) -> int n | Some (PdfReal n) -> int n | _ -> 1
                                    let imgH =
                                        match Pdf.dictGet doc "Height" xobj with
                                        | Some (PdfInt n) -> int n | Some (PdfReal n) -> int n | _ -> 1
                                    let bpc =
                                        match Pdf.dictGet doc "BitsPerComponent" xobj with
                                        | Some (PdfInt n) -> int n | _ -> 8
                                    let imgColorSpaceObj = Pdf.dictGet doc "ColorSpace" xobj
                                    let imgColorSpace, palette =
                                        match imgColorSpaceObj with
                                        | Some (PdfName cs) -> cs, None
                                        | Some csObj ->
                                            let resolved = Pdf.deref doc csObj
                                            match resolved with
                                            | PdfArray items when items.Length >= 1 ->
                                                let typeName = match Pdf.deref doc items.[0] with PdfName n -> n | _ -> ""
                                                match typeName with
                                                | "Indexed" when items.Length >= 4 ->
                                                    let baseCS =
                                                        match Pdf.deref doc items.[1] with
                                                        | PdfName n -> n
                                                        | PdfArray bItems when bItems.Length >= 1 ->
                                                            let bType = match Pdf.deref doc bItems.[0] with PdfName n -> n | _ -> ""
                                                            match bType with
                                                            | "ICCBased" when bItems.Length >= 2 ->
                                                                match Pdf.dictGet doc "N" bItems.[1] with
                                                                | Some (PdfInt 1L) -> "DeviceGray"
                                                                | Some (PdfInt 3L) -> "DeviceRGB"
                                                                | Some (PdfInt 4L) -> "DeviceCMYK"
                                                                | _ -> "DeviceRGB"
                                                            | _ -> "DeviceRGB"
                                                        | _ -> "DeviceRGB"
                                                    let paletteBytes =
                                                        match Pdf.deref doc items.[3] with
                                                        | PdfString s -> s
                                                        | _ -> try Pdf.getStreamData doc items.[3] with _ -> [||]
                                                    "Indexed", Some (baseCS, paletteBytes)
                                                | "ICCBased" when items.Length >= 2 ->
                                                    let cs =
                                                        match Pdf.dictGet doc "N" items.[1] with
                                                        | Some (PdfInt 1L) -> "DeviceGray"
                                                        | Some (PdfInt 3L) -> "DeviceRGB"
                                                        | Some (PdfInt 4L) -> "DeviceCMYK"
                                                        | _ -> "DeviceRGB"
                                                    cs, None
                                                | "CalGray" -> "DeviceGray", None
                                                | "CalRGB" | "Lab" -> "DeviceRGB", None
                                                | _ -> "DeviceRGB", None
                                            | _ -> "DeviceRGB", None
                                        | _ -> "DeviceRGB", None

                                    let isImageMask =
                                        match Pdf.dictGet doc "ImageMask" xobj with
                                        | Some (PdfBool true) -> true | _ -> false

                                    // Check for JPEG filter
                                    let filterName =
                                        match Pdf.dictGet doc "Filter" xobj with
                                        | Some (PdfName f) -> f
                                        | Some (PdfArray (f :: _)) ->
                                            match Pdf.deref doc f with PdfName n -> n | _ -> ""
                                        | _ -> ""

                                    try
                                        let imgData = Pdf.getStreamData doc xobj

                                        // Check for SMask
                                        let smaskAlpha =
                                            match Pdf.dictGet doc "SMask" xobj with
                                            | Some smaskObj ->
                                                try
                                                    let smW = match Pdf.dictGet doc "Width" smaskObj with Some (PdfInt n) -> int n | Some (PdfReal n) -> int n | _ -> imgW
                                                    let smH = match Pdf.dictGet doc "Height" smaskObj with Some (PdfInt n) -> int n | Some (PdfReal n) -> int n | _ -> imgH
                                                    let smData = Pdf.getStreamData doc smaskObj
                                                    if smData.Length >= smW * smH && smW = imgW && smH = imgH then Some smData
                                                    else None
                                                with _ -> None
                                            | None -> None

                                        // Check for color key mask
                                        let colorKeyMask =
                                            match Pdf.dictGet doc "Mask" xobj with
                                            | Some (PdfArray items) ->
                                                let nums = items |> List.choose (fun o -> match Pdf.deref doc o with PdfInt n -> Some (int n) | PdfReal f -> Some (int f) | _ -> None)
                                                if nums.Length >= 2 && nums.Length % 2 = 0 then Some (nums |> Array.ofList)
                                                else None
                                            | _ -> None

                                        if isImageMask then
                                            // 1-bit stencil: each bit=1 paints current fill color
                                            let fc = fillColor gs
                                            let pixels = Array.create (imgW * imgH) 0u
                                            let rowBytes = (imgW + 7) / 8
                                            for y in 0..imgH - 1 do
                                                for x in 0..imgW - 1 do
                                                    let byteIdx = y * rowBytes + x / 8
                                                    let bitIdx = 7 - (x % 8)
                                                    if byteIdx < imgData.Length then
                                                        let bit = (imgData.[byteIdx] >>> bitIdx) &&& 1uy
                                                        // Decode array: default [0 1] means bit=0 → paint
                                                        let paint =
                                                            match Pdf.dictGet doc "Decode" xobj with
                                                            | Some (PdfArray [a; _]) ->
                                                                match Pdf.deref doc a with
                                                                | PdfInt 1L | PdfReal 1.0 -> bit = 1uy
                                                                | _ -> bit = 0uy
                                                            | _ -> bit = 0uy
                                                        if paint then
                                                            pixels.[y * imgW + x] <- Pixel.pack fc.B fc.G fc.R fc.A

                                            let fb = Framebuffer.ofArray pixels imgW imgH
                                            let srcR = rect 0.0 0.0 (float imgW) (float imgH)
                                            let dstR = rect 0.0 0.0 1.0 1.0
                                            cmds.Add(Group(gs.CTM, 1.0, [DrawImage(fb, srcR, dstR, true)]))

                                        elif filterName = "DCTDecode" then
                                            // JPEG — use external decoder on raw stream bytes
                                            let rawData =
                                                match Pdf.deref doc xobj with
                                                | PdfStream (_, raw) -> raw
                                                | _ -> imgData
                                            match opts.DecodeJpeg rawData with
                                            | Some (dw, dh, dpixels) ->
                                                let fb = Framebuffer.ofArray dpixels dw dh
                                                let srcR = rect 0.0 0.0 (float dw) (float dh)
                                                let dstR = rect 0.0 0.0 1.0 1.0
                                                cmds.Add(Group(gs.CTM, 1.0, [DrawImage(fb, srcR, dstR, true)]))
                                            | None -> ()

                                        else

                                        let pixels = Array.create (imgW * imgH) 0u
                                        match palette with
                                        | Some (baseCS, paletteBytes) ->
                                            // Indexed color space: pixel byte is palette index
                                            let entrySize = match baseCS with "DeviceGray" -> 1 | "DeviceCMYK" -> 4 | _ -> 3
                                            for y in 0..imgH - 1 do
                                                for x in 0..imgW - 1 do
                                                    let idx = y * imgW + x
                                                    if idx < imgData.Length then
                                                        let palIdx = int imgData.[idx]
                                                        let off = palIdx * entrySize
                                                        let r, g, b =
                                                            if off + entrySize - 1 < paletteBytes.Length then
                                                                match baseCS with
                                                                | "DeviceGray" -> let v = paletteBytes.[off] in (v, v, v)
                                                                | "DeviceCMYK" ->
                                                                    let c = float paletteBytes.[off] / 255.0
                                                                    let m = float paletteBytes.[off+1] / 255.0
                                                                    let yy = float paletteBytes.[off+2] / 255.0
                                                                    let k = float paletteBytes.[off+3] / 255.0
                                                                    (byte ((1.0-c)*(1.0-k)*255.0), byte ((1.0-m)*(1.0-k)*255.0), byte ((1.0-yy)*(1.0-k)*255.0))
                                                                | _ -> (paletteBytes.[off], paletteBytes.[off+1], paletteBytes.[off+2])
                                                            else (255uy, 255uy, 255uy)
                                                        pixels.[idx] <- Pixel.pack b g r 255uy
                                        | None ->
                                            let channelsPerPixel =
                                                match imgColorSpace with
                                                | "DeviceGray" -> 1
                                                | "DeviceRGB" -> 3
                                                | "DeviceCMYK" -> 4
                                                | _ -> 3

                                            for y in 0..imgH - 1 do
                                                for x in 0..imgW - 1 do
                                                    let idx = (y * imgW + x) * channelsPerPixel
                                                    let r, g, b =
                                                        if idx + channelsPerPixel - 1 < imgData.Length then
                                                            match imgColorSpace with
                                                            | "DeviceGray" ->
                                                                let v = imgData.[idx]
                                                                (v, v, v)
                                                            | "DeviceCMYK" ->
                                                                let c = float imgData.[idx] / 255.0
                                                                let m = float imgData.[idx + 1] / 255.0
                                                                let yy = float imgData.[idx + 2] / 255.0
                                                                let k = float imgData.[idx + 3] / 255.0
                                                                (byte ((1.0 - c) * (1.0 - k) * 255.0),
                                                                 byte ((1.0 - m) * (1.0 - k) * 255.0),
                                                                 byte ((1.0 - yy) * (1.0 - k) * 255.0))
                                                            | _ ->
                                                                (imgData.[idx], imgData.[idx + 1], imgData.[idx + 2])
                                                        else (255uy, 255uy, 255uy)
                                                    pixels.[y * imgW + x] <- Pixel.pack b g r 255uy

                                        // Apply SMask alpha
                                        match smaskAlpha with
                                        | Some smData ->
                                            for i in 0..imgW * imgH - 1 do
                                                let px = pixels.[i]
                                                let a = Pixel.a px
                                                let sa = smData.[i]
                                                let newA = byte (int a * int sa / 255)
                                                // Re-premultiply with new alpha
                                                let r = byte (int (Pixel.r px) * int sa / 255)
                                                let g = byte (int (Pixel.g px) * int sa / 255)
                                                let b = byte (int (Pixel.b px) * int sa / 255)
                                                pixels.[i] <- Pixel.pack b g r newA
                                        | None -> ()

                                        // Apply color key mask
                                        match colorKeyMask with
                                        | Some ranges ->
                                            let channelsPerPixel = match imgColorSpace with "DeviceGray" -> 1 | "DeviceCMYK" -> 4 | _ -> 3
                                            let numRanges = ranges.Length / 2
                                            if numRanges = channelsPerPixel then
                                                for i in 0..imgW * imgH - 1 do
                                                    let srcIdx = i * channelsPerPixel
                                                    if srcIdx + channelsPerPixel - 1 < imgData.Length then
                                                        let mutable masked = true
                                                        for c in 0..channelsPerPixel - 1 do
                                                            let v = int imgData.[srcIdx + c]
                                                            if v < ranges.[c * 2] || v > ranges.[c * 2 + 1] then masked <- false
                                                        if masked then pixels.[i] <- 0u
                                        | None -> ()

                                        let fb = Framebuffer.ofArray pixels imgW imgH
                                        let srcR = rect 0.0 0.0 (float imgW) (float imgH)
                                        let dstR = rect 0.0 0.0 1.0 1.0
                                        cmds.Add(Group(gs.CTM, 1.0, [DrawImage(fb, srcR, dstR, true)]))
                                    with _ -> ()

                                | "Form" ->
                                    try
                                        let fl = function PdfInt n -> float n | PdfReal r -> float r | _ -> 0.0
                                        let formMatrix =
                                            match Pdf.dictGet doc "Matrix" xobj with
                                            | Some (PdfArray [a; b; c; d; e; f]) ->
                                                { M11 = fl a; M12 = fl b; M21 = fl c; M22 = fl d; DX = fl e; DY = fl f }
                                            | _ -> Matrix3x2.identity
                                        let formResources =
                                            match Pdf.dictGet doc "Resources" xobj with
                                            | Some r -> r
                                            | None -> currentResources
                                        let formData = Pdf.getStreamData doc xobj
                                        let formTokens = tokenize formData |> Array.ofList
                                        if formTokens.Length > 0 then
                                            streamStack.Push(tokenArr, i, currentResources)
                                            stateStack.Add(gs)
                                            clipStack.Add(None)
                                            gs <- { gs with CTM = Matrix3x2.multiply formMatrix gs.CTM }
                                            currentResources <- formResources
                                            tokenArr <- formTokens
                                            tokenCount <- formTokens.Length
                                            i <- 0
                                    with _ -> ()
                                | _ -> ()
                            | None -> ()
                        | None -> ()

                    // ── Inline Image (BI/ID/EI) ──
                    | "BI" ->
                        // Collect dictionary key/value pairs until TInlineImageData
                        let iiDict = Collections.Generic.Dictionary<string, Token>()
                        let mutable iiKey = ""
                        let mutable iiData: byte[] option = None
                        let mutable iiDone = false
                        while i < tokenCount && not iiDone do
                            match tokenArr.[i] with
                            | TInlineImageData d -> iiData <- Some d; i <- i + 1
                            | TOperator "EI" -> i <- i + 1; iiDone <- true
                            | TName k -> iiKey <- k; i <- i + 1
                            | v ->
                                if iiKey.Length > 0 then iiDict.[iiKey] <- v; iiKey <- ""
                                i <- i + 1
                        // Skip EI if not yet consumed
                        if not iiDone && i < tokenCount then
                            match tokenArr.[i] with TOperator "EI" -> i <- i + 1 | _ -> ()

                        match iiData with
                        | Some rawData ->
                            // Normalize abbreviated keys
                            let getII (key: string) (abbrev: string) =
                                match iiDict.TryGetValue(key) with
                                | true, v -> Some v
                                | _ -> match iiDict.TryGetValue(abbrev) with true, v -> Some v | _ -> None
                            let iiW = match getII "Width" "W" with Some (TNumber n) -> int n | _ -> 0
                            let iiH = match getII "Height" "H" with Some (TNumber n) -> int n | _ -> 0
                            let iiBpc = match getII "BitsPerComponent" "BPC" with Some (TNumber n) -> int n | _ -> 8
                            let iiCS =
                                match getII "ColorSpace" "CS" with
                                | Some (TName cs) ->
                                    match cs with
                                    | "G" -> "DeviceGray" | "RGB" -> "DeviceRGB" | "CMYK" -> "DeviceCMYK"
                                    | _ -> resolveColorSpace cs
                                | _ -> "DeviceRGB"
                            let iiFilter =
                                match getII "Filter" "F" with
                                | Some (TName f) ->
                                    match f with
                                    | "Fl" | "FlateDecode" -> "FlateDecode"
                                    | "DCT" | "DCTDecode" -> "DCTDecode"
                                    | "AHx" | "ASCIIHexDecode" -> "ASCIIHexDecode"
                                    | "A85" | "ASCII85Decode" -> "ASCII85Decode"
                                    | _ -> f
                                | _ -> ""

                            if iiW > 0 && iiH > 0 then
                                try
                                    if iiFilter = "DCTDecode" then
                                        match opts.DecodeJpeg rawData with
                                        | Some (dw, dh, dpixels) ->
                                            let fb = Framebuffer.ofArray dpixels dw dh
                                            let srcR = rect 0.0 0.0 (float dw) (float dh)
                                            let dstR = rect 0.0 0.0 1.0 1.0
                                            cmds.Add(Group(gs.CTM, 1.0, [DrawImage(fb, srcR, dstR, true)]))
                                        | None -> ()
                                    else
                                        let imgData =
                                            if iiFilter.Length > 0 && iiFilter <> "DCTDecode" then
                                                try Rbt.Pdf.Streams.decompress iiFilter rawData with _ -> rawData
                                            else rawData

                                        let pixels = Array.create (iiW * iiH) 0u
                                        let channelsPerPixel = match iiCS with "DeviceGray" -> 1 | "DeviceCMYK" -> 4 | _ -> 3
                                        for y in 0..iiH - 1 do
                                            for x in 0..iiW - 1 do
                                                let idx = (y * iiW + x) * channelsPerPixel
                                                let r, g, b =
                                                    if idx + channelsPerPixel - 1 < imgData.Length then
                                                        match iiCS with
                                                        | "DeviceGray" -> let v = imgData.[idx] in (v, v, v)
                                                        | "DeviceCMYK" ->
                                                            let c = float imgData.[idx] / 255.0
                                                            let m = float imgData.[idx+1] / 255.0
                                                            let yy = float imgData.[idx+2] / 255.0
                                                            let k = float imgData.[idx+3] / 255.0
                                                            (byte ((1.0-c)*(1.0-k)*255.0), byte ((1.0-m)*(1.0-k)*255.0), byte ((1.0-yy)*(1.0-k)*255.0))
                                                        | _ -> (imgData.[idx], imgData.[idx+1], imgData.[idx+2])
                                                    else (255uy, 255uy, 255uy)
                                                pixels.[y * iiW + x] <- Pixel.pack b g r 255uy
                                        let fb = Framebuffer.ofArray pixels iiW iiH
                                        let srcR = rect 0.0 0.0 (float iiW) (float iiH)
                                        let dstR = rect 0.0 0.0 1.0 1.0
                                        cmds.Add(Group(gs.CTM, 1.0, [DrawImage(fb, srcR, dstR, true)]))
                                with _ -> ()
                        | None -> ()

                    | _ ->
                        operands.Clear()

            // Close remaining clip scopes
            while clipStack.Count > 0 do
                match clipStack.[clipStack.Count - 1] with
                | Some (startIdx, segs, rule) ->
                    let clippedCmds = cmds |> Seq.skip startIdx |> Seq.toList
                    cmds.RemoveRange(startIdx, cmds.Count - startIdx)
                    cmds.Add(Clip(segs, rule, clippedCmds))
                | None -> ()
                clipStack.RemoveAt(clipStack.Count - 1)

            let pathCmdList = cmds |> Seq.toList
            let textCmdList = textCmds |> Seq.toList

            let flipMatrix = { M11 = 1.0; M12 = 0.0; M21 = 0.0; M22 = -1.0; DX = 0.0; DY = rawPageH }
            let scaledCmds =
                if scale <> 1.0 then
                    let scaleMatrix = Matrix3x2.scale scale scale
                    let flipScaled = Matrix3x2.multiply flipMatrix scaleMatrix
                    let flippedPaths = [Group(flipScaled, 1.0, pathCmdList)]
                    let scaledText = [Group(scaleMatrix, 1.0, textCmdList)]
                    flippedPaths @ scaledText
                else
                    [Group(flipMatrix, 1.0, pathCmdList)] @ textCmdList

            // Scale charboxes (text coords are already in screen space, just need scaling)
            let scaledCharBoxes =
                charBoxes.ToArray()
                |> Array.map (fun cb ->
                    { cb with X = cb.X * scale; Y = cb.Y * scale
                              W = cb.W * scale; H = cb.H * scale })

            Some (scaledCmds, pageW, pageH, scaledCharBoxes)
