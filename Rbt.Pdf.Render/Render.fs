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
    FillK: byte
    StrokeK: byte
    FillSource: (PdfColor.Space * float[]) option
    StrokeSource: (PdfColor.Space * float[]) option
    LineWidth: float
    LineCap: LineCap
    LineJoin: LineJoin
    MiterLimit: float
    DashArray: float<px> list
    DashOffset: float<px>
    FillAlpha: float
    StrokeAlpha: float
    AlphaIsShape: bool
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
    SoftMask: Cmd list option
    FillPattern: string option
    StrokePattern: string option
}

let private defaultState = {
    CTM = Matrix3x2.identity
    FillColor = Color.black
    StrokeColor = Color.black
    FillK = 0uy
    StrokeK = 0uy
    FillSource = PdfColor.device 1 |> Option.map (fun space -> space, [|0.0|])
    StrokeSource = PdfColor.device 1 |> Option.map (fun space -> space, [|0.0|])
    LineWidth = 1.0
    LineCap = Butt
    LineJoin = Miter
    MiterLimit = 10.0
    DashArray = []
    DashOffset = 0.0<px>
    FillAlpha = 1.0
    StrokeAlpha = 1.0
    AlphaIsShape = false
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
    SoftMask = None
    FillPattern = None
    StrokePattern = None
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

type private PathBuilder(getTransform: unit -> Matrix3x2) =
    let segs = ResizeArray<PathSeg>()
    let mutable current = vec2 0.0 0.0
    let mutable start = current

    let point x y = Matrix3x2.transformVec2 (getTransform ()) (vec2 x y)

    member _.Segments = segs |> Seq.toList
    member _.Clear() = segs.Clear(); current <- vec2 0.0 0.0; start <- current

    member _.MoveTo(x, y) =
        current <- point x y
        start <- current
        segs.Add(MoveTo current)

    member _.LineTo(x, y) =
        current <- point x y
        segs.Add(LineTo current)

    member _.CubicTo(x1, y1, x2, y2, x3, y3) =
        let endpoint = point x3 y3
        segs.Add(CubicTo(point x1 y1, point x2 y2, endpoint))
        current <- endpoint

    member _.CubicFromCurrent(x2, y2, x3, y3) =
        let endpoint = point x3 y3
        segs.Add(CubicTo(current, point x2 y2, endpoint))
        current <- endpoint

    member _.Close() =
        segs.Add(Close)
        current <- start

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
type private ClipKind =
    | PathClip of PathSeg list * FillRule
    | TextClip of PathSeg list list


type RenderOptions =
    { FallbackFont: Text.FontData
      /// Loaded only when a missing Unicode glyph needs the CJK fallback.
      CjkFont: Lazy<Text.FontData option>
      /// Predefined PDF CMaps, keyed by their exact resource names.
      CMaps: Map<string, byte[]>
      /// JPEG (DCTDecode) raw stream bytes -> (w, h, premultiplied BGRA uint32[]). None = skip image.
      DecodeJpeg: byte[] -> (int * int * uint32[]) option
      /// Render a transparent pattern cell using the destination backend.
      Rasterize: int -> int -> Cmd list -> Framebuffer
      /// Render a subtractive four-component cell, preserving CMY and K planes.
      Rasterize4: int -> int -> Cmd list -> Framebuffer * Framebuffer }

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
            let assetPath relative =
                [ System.IO.Path.Combine(System.IO.Path.GetDirectoryName fontPath, relative)
                  System.IO.Path.Combine(System.AppContext.BaseDirectory, "assets", relative)
                  System.IO.Path.GetFullPath(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../resources", relative)) ]
                |> List.tryFind System.IO.File.Exists
            let cmaps =
                [ System.IO.Path.Combine(System.IO.Path.GetDirectoryName fontPath, "cmaps")
                  System.IO.Path.Combine(System.AppContext.BaseDirectory, "assets", "cmaps")
                  System.IO.Path.GetFullPath(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../resources/cmaps")) ]
                |> List.filter System.IO.Directory.Exists
                |> List.collect (fun directory -> System.IO.Directory.GetFiles(directory) |> Array.toList)
                |> List.rev
                |> List.fold (fun maps path -> Map.add (System.IO.Path.GetFileName path) (System.IO.File.ReadAllBytes path) maps) Map.empty
            { FallbackFont = Text.loadFontFile fontPath
              CjkFont = lazy (assetPath "NotoSansCJKsc-Regular.otf" |> Option.map Text.loadFontFile)
              CMaps = cmaps
              DecodeJpeg = ImageDecode.decode
              Rasterize = fun width height commands ->
                  let backend = Raster.create ()
                  backend.Init width height
                  backend.Submit (Clear (Color.rgba 0uy 0uy 0uy 0uy) :: commands)
                  backend.Finish ()
              Rasterize4 = fun width height commands ->
                  Raster.rasterizeComponents width height Matrix3x2.identity 0 true commands }

    /// Options for .NET: the fallback font from fallbackFontCandidates, JPEG decoding by ImageDecode.decode.
    let native () = nativeOpts.Value
#endif

// ── Main Interpreter ─────────────────────────────────────

let private groupWorkingSpace doc resources group =
    let rec isIcc depth value =
        if depth > 12 then false else
        match Pdf.deref doc value with
        | PdfArray (head :: _) -> Pdf.deref doc head = PdfName "ICCBased"
        | PdfName name ->
            Pdf.dictGet doc "ColorSpace" resources
            |> Option.bind (Pdf.dictGet doc name)
            |> Option.exists (isIcc (depth + 1))
        | _ -> false
    Pdf.dictGet doc "CS" group
    |> Option.map (fun colorSpace ->
        let space = PdfColor.resolve doc resources colorSpace
                    |> Option.defaultWith (fun () -> invalidOp "Unsupported PDF transparency-group color space")
        if isIcc 0 colorSpace && not (PdfColor.isIcc space) then
            invalidOp "Unsupported PDF transparency-group ICC profile"
        PdfColor.tryWorkingSpace space
        |> Option.defaultWith (fun () -> invalidOp "Unsupported PDF transparency-group blending transform"))

let private groupConversion (parent: PdfColor.WorkingSpace) (child: PdfColor.WorkingSpace) : ColorSpaceConversion =
    let convert (source: PdfColor.WorkingSpace) (destination: PdfColor.WorkingSpace) r g b =
        let struct(x,y,z) = source.ToPcs r g b
        destination.FromPcs x y z
    { ToGroup = convert parent child; ToParent = convert child parent }
let private groupConversion4 (parent: PdfColor.WorkingSpace) (child: PdfColor.WorkingSpace) : ColorSpaceConversion4 =
    let components (space: PdfColor.WorkingSpace) = if space.FourChannel.IsSome then 4 else 3
    let convert (source: PdfColor.WorkingSpace) (destination: PdfColor.WorkingSpace) r g b k =
        let struct(x,y,z) =
            match source.FourChannel with
            | Some (forward, _) -> forward r g b k
            | None -> source.ToPcs r g b
        match destination.FourChannel with
        | Some (_, inverse) -> inverse x y z
        | None ->
            let struct(r,g,b) = destination.FromPcs x y z
            struct(r,g,b,0.0)
    { ParentComponents = components parent; GroupComponents = components child
      ToGroup = convert parent child; ToParent = convert child parent
      Complement = child.FourChannel.IsSome }

let private managedGroup (parent: PdfColor.WorkingSpace) (child: PdfColor.WorkingSpace option)
                         isolated knockout opacity mode mask children =
    match child with
    | Some target when not (PdfColor.sameWorkingSpace parent target) ->
        if parent.FourChannel.IsSome || target.FourChannel.IsSome then
            ColorManagedGroup4(groupConversion4 parent target, isolated, knockout, opacity, mode, mask, children)
        else ColorManagedGroup(groupConversion parent target, isolated, knockout, opacity, mode, mask, children)
    | _ -> TransparencyGroup(isolated, knockout, opacity, mode, mask, children)

let rec private renderContent (opts: RenderOptions) (doc: PdfDocument) (resources: PdfObj) (contentData: byte[]) (initialState: GraphicsState) (initialWorking: PdfColor.WorkingSpace option) (viewport: PageGeometry.Viewport) (scale: float) (depth: int) : Cmd list * CharBox[] =
            if depth > 32 then invalidOp "PDF content recursion limit exceeded"
            let mutable currentResources = resources
            let mutable defaultCTM = initialState.CTM
            let mutable defaultAlphaIsShape = initialState.AlphaIsShape

            let tokens = tokenize contentData

            // Interpreter state
            let stateStack = ResizeArray<GraphicsState>()
            let mutable gs = initialState
            let mutable working = initialWorking
            let isFourChannel () = working |> Option.exists (fun space -> space.FourChannel.IsSome)
            let toTarget space values =
                match working with
                | Some target -> PdfColor.toWorkingRgb target space values
                | None -> PdfColor.toRgb space values
            let toImageTarget space values =
                match working with
                | Some target when target.FourChannel.IsSome -> PdfColor.toWorkingCmyk target space values
                | _ -> let struct(r,g,b) = toTarget space values in struct(r,g,b,0uy)
            let sourceColor (space, values) alpha =
                let struct(r,g,b,k) = toImageTarget space values
                Color.rgba r g b (byte (clamp 0.0 255.0 (alpha * 255.0))), k
            let refreshColors state =
                let fill, fillK = state.FillSource |> Option.map (fun source -> sourceColor source state.FillAlpha)
                                  |> Option.defaultValue (state.FillColor,state.FillK)
                let stroke, strokeK = state.StrokeSource |> Option.map (fun source -> sourceColor source state.StrokeAlpha)
                                      |> Option.defaultValue (state.StrokeColor,state.StrokeK)
                { state with FillColor = fill; FillK = fillK; StrokeColor = stroke; StrokeK = strokeK }
            gs <- refreshColors gs
            let path = PathBuilder(fun () -> gs.CTM)
            let cmds = ResizeArray<Cmd>()
            let blendMode name =
                match name with
                | "Normal" | "Compatible" -> Normal
                | "Multiply" -> Multiply | "Screen" -> Screen | "Overlay" -> Overlay
                | "Darken" -> Darken | "Lighten" -> Lighten
                | "ColorDodge" -> ColorDodge | "ColorBurn" -> ColorBurn
                | "HardLight" -> HardLight | "SoftLight" -> SoftLight
                | "Difference" -> Difference | "Exclusion" -> Exclusion
                | "Hue" -> Hue | "Saturation" -> Saturation
                | "Color" -> ColorMode | "Luminosity" -> Luminosity
                | _ -> invalidOp ("Unsupported PDF blend mode: " + name)
            // Explicit shape is lazy: most objects need only their color pass.
            // The graphics-state alpha and soft mask affect coverage only with AIS.
            let addPaint alpha intrinsicShape useSoftMask command =
                let mask = if useSoftMask then gs.SoftMask else None
                let masked = match mask with Some commands -> Mask(commands, [command]) | None -> command
                let mode = blendMode gs.BlendMode
                let color = if mode = Normal then masked else Blend(mode, [masked])
                if gs.AlphaIsShape && (alpha <> 1.0 || mask.IsSome) then
                    let shape = lazy (
                        let intrinsic =
                            match intrinsicShape with
                            | Some (shape: Lazy<Cmd list>) -> shape.Value
                            | None -> [Cmd.shapeOf command]
                        let withAlpha = if alpha = 1.0 then intrinsic else [Group(Matrix3x2.identity, alpha, intrinsic)]
                        match mask with Some commands -> [Mask(commands, withAlpha)] | None -> withAlpha)
                    cmds.Add(WithShape(shape, [color]))
                else
                    match intrinsicShape with
                    | Some (shape: Lazy<Cmd list>) -> cmds.Add(WithShape(shape, [color]))
                    | None when mask.IsSome -> cmds.Add(WithShape(lazy [Cmd.shapeOf command], [color]))
                    | None -> cmds.Add color
            let charBoxes = ResizeArray<CharBox>()
            // Text state
            let mutable textMatrix = Matrix3x2.identity
            let mutable lineMatrix = Matrix3x2.identity
            let mutable inText = false
            let textClip = ResizeArray<PathSeg list>()
            let mutable hasTextClip = false
            let fontCache = Collections.Generic.Dictionary<PdfObj, FontInfo>()

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

            let transformPath matrix segments =
                let p = Matrix3x2.transformVec2 matrix
                segments |> List.map (function
                    | MoveTo a -> MoveTo(p a)
                    | LineTo a -> LineTo(p a)
                    | QuadTo (a, b) -> QuadTo(p a, p b)
                    | CubicTo (a, b, c) -> CubicTo(p a, p b, p c)
                    | other -> other)

            let patternCommands name color k source alpha bounds =
                let number value =
                    match Pdf.deref doc value with PdfInt n -> float n | PdfReal n -> n | _ -> 0.0
                let get key obj = Pdf.dictGet doc key obj |> Option.map (Pdf.deref doc)
                match get "Pattern" currentResources |> Option.bind (get name) with
                | None -> [], lazy []
                | Some pattern ->
                    let matrix =
                        match get "Matrix" pattern with
                        | Some (PdfArray [a; b; c; d; e; f]) ->
                            { M11 = number a; M12 = number b; M21 = number c; M22 = number d; DX = number e; DY = number f }
                        | _ -> Matrix3x2.identity
                    // Pattern matrices use the stream's default user space, not
                    // the CTM at the painting operator.
                    let patternToPage = Matrix3x2.multiply matrix defaultCTM
                    match get "PatternType" pattern with
                    | Some (PdfInt 2L) ->
                        match get "Shading" pattern with
                        | Some shading ->
                            Shading.tryRenderObject toImageTarget (isFourChannel ()) doc currentResources shading patternToPage 1.0 bounds scale
                            |> Option.map (fun command -> [Group(Matrix3x2.identity, alpha, [command])], lazy [Cmd.shapeOf command])
                            |> Option.defaultValue ([], lazy [])
                        | None -> [], lazy []
                    | Some (PdfInt 1L) ->
                        match get "BBox" pattern, get "XStep" pattern, get "YStep" pattern with
                        | Some (PdfArray [a; b; c; d]), Some xs, Some ys ->
                            let x0, y0, x1, y1 = number a, number b, number c, number d
                            let stepX, stepY = abs (number xs), abs (number ys)
                            let det = patternToPage.M11 * patternToPage.M22 - patternToPage.M12 * patternToPage.M21
                            if stepX = 0.0 || stepY = 0.0 || x1 <= x0 || y1 <= y0 || abs det < 1e-10 then [], lazy []
                            else
                                let bx0, by0, bx1, by1 = bounds
                                let firstX, lastX = int (ceil (-x1 / stepX)), int (floor ((stepX - x0) / stepX))
                                let firstY, lastY = int (ceil (-y1 / stepY)), int (floor ((stepY - y0) / stepY))
                                let tileResources = get "Resources" pattern |> Option.defaultValue currentResources
                                let tileState =
                                    { gs with CTM = Matrix3x2.identity; FillPattern = None; StrokePattern = None
                                              FillColor = { color with A = 255uy }; StrokeColor = { color with A = 255uy }
                                              FillK = k; StrokeK = k
                                              FillSource = source; StrokeSource = source; FillAlpha = 1.0; StrokeAlpha = 1.0
                                              BlendMode = "Normal"; SoftMask = None; AlphaIsShape = defaultAlphaIsShape }
                                let tileViewport =
                                    { viewport with Bounds = (x0, y0, x1, y1); Width = x1 - x0; Height = y1 - y0; Transform = Matrix3x2.identity }
                                let tileScale = scale * max (sqrt (patternToPage.M11 ** 2.0 + patternToPage.M12 ** 2.0)) (sqrt (patternToPage.M21 ** 2.0 + patternToPage.M22 ** 2.0))
                                let tileCommands, _ = renderContent opts doc tileResources (Pdf.getStreamData doc pattern) tileState working tileViewport tileScale (depth + 1)
                                let tileBounds = [MoveTo(vec2 x0 y0); LineTo(vec2 x1 y0); LineTo(vec2 x1 y1); LineTo(vec2 x0 y1); Close]
                                let cell = [Clip(tileBounds, NonZero, tileCommands)]
                                // Render a single period, including cells that overlap it.
                                // Repeating a surface avoids source-over seams between
                                // adjacent antialiased vector cells at fractional zoom.
                                let requestedW = max 1.0 (ceil (stepX * tileScale))
                                let requestedH = max 1.0 (ceil (stepY * tileScale))
                                let reduction = min 1.0 (sqrt (4000000.0 / (requestedW * requestedH)))
                                let width, height = max 1 (int (requestedW * reduction)), max 1 (int (requestedH * reduction))
                                let sx, sy = float width / stepX, float height / stepY
                                let toPixels = { M11 = sx; M12 = 0.0; M21 = 0.0; M22 = -sy; DX = 0.0; DY = float height }
                                let cells =
                                    [ for iy in firstY .. lastY do
                                        for ix in firstX .. lastX do
                                            yield Group(Matrix3x2.translate (float ix * stepX) (float iy * stepY), 1.0, cell) ]
                                let images =
                                    let commands = [Group(toPixels, 1.0, cells)]
                                    if isFourChannel () then
                                        let main, k = opts.Rasterize4 width height commands
                                        Some (main, k)
                                    else None
                                let image =
                                    match images with
                                    | Some (main, _) -> main
                                    | None -> opts.Rasterize width height [Group(toPixels, 1.0, cells)]
                                let fromPixels =
                                    { M11 = 1.0 / sx; M12 = 0.0; M21 = 0.0; M22 = -1.0 / sy
                                      DX = 0.5 / sx; DY = stepY - 0.5 / sy }
                                let placement = Matrix3x2.multiply fromPixels patternToPage
                                let area = [MoveTo(vec2 bx0 by0); LineTo(vec2 bx1 by0); LineTo(vec2 bx1 by1); LineTo(vec2 bx0 by1); Close]
                                let shape = lazy (
                                    let shapeImage = opts.Rasterize width height [Cmd.shapeOf (Group(toPixels, 1.0, cells))]
                                    [FillPath(area, TilingPaint(shapeImage, placement, Repeat, Repeat), NonZero)])
                                let main = Group(Matrix3x2.identity, alpha, [FillPath(area, TilingPaint(image, placement, Repeat, Repeat), NonZero)])
                                let command =
                                    match images with
                                    | Some (_, k) ->
                                        Components4(main, Group(Matrix3x2.identity, alpha, [FillPath(area, TilingPaint(k, placement, Repeat, Repeat), NonZero)]))
                                    | None -> main
                                [command], shape
                        | _ -> [], lazy []
                    | _ -> [], lazy []

            let fourthColor value (original: Color) = Color.rgba value value value original.A
            let paintPath (segments: PathSeg list) fill stroke rule =
                let bounds () =
                    let points =
                        segments |> List.collect (function
                            | MoveTo p | LineTo p -> [p]
                            | QuadTo (a, b) -> [a; b]
                            | CubicTo (a, b, c) -> [a; b; c]
                            | _ -> [])
                    let x0, y0, x1, y1 = viewport.Bounds
                    if List.isEmpty points then viewport.Bounds
                    else
                        let margin = if stroke then abs gs.LineWidth * max gs.MiterLimit 1.0 * max (abs gs.CTM.M11 + abs gs.CTM.M21) (abs gs.CTM.M12 + abs gs.CTM.M22) else 0.0
                        max x0 ((points |> List.minBy (fun p -> p.X) |> fun p -> float p.X) - margin),
                        max y0 ((points |> List.minBy (fun p -> p.Y) |> fun p -> float p.Y) - margin),
                        min x1 ((points |> List.maxBy (fun p -> p.X) |> fun p -> float p.X) + margin),
                        min y1 ((points |> List.maxBy (fun p -> p.Y) |> fun p -> float p.Y) + margin)
                if fill then
                    match gs.FillPattern with
                    | Some name ->
                        let color, shape = patternCommands name gs.FillColor gs.FillK gs.FillSource gs.FillAlpha (bounds ())
                        addPaint gs.FillAlpha (Some (lazy [Clip(segments, rule, shape.Value)])) true (Clip(segments, rule, color))
                    | None ->
                        let color = fillColor gs
                        let main = FillPath(segments, Solid color, rule)
                        let command =
                            if isFourChannel () then
                                Components4(main, FillPath(segments, Solid (fourthColor gs.FillK color), rule))
                            else main
                        addPaint gs.FillAlpha None true command
                if stroke && abs (gs.CTM.M11 * gs.CTM.M22 - gs.CTM.M12 * gs.CTM.M21) > 1e-12 then
                    let style = makeStroke gs
                    let strokeCommand paint =
                        if gs.CTM = Matrix3x2.identity then StrokePath(segments, { style with Paint = paint })
                        else Group(gs.CTM, 1.0, [StrokePath(transformPath (Matrix3x2.inverse gs.CTM) segments, { style with Paint = paint })])
                    match gs.StrokePattern with
                    | Some name ->
                        let color, shape = patternCommands name gs.StrokeColor gs.StrokeK gs.StrokeSource gs.StrokeAlpha (bounds ())
                        let geometry = strokeCommand (Solid Color.white)
                        addPaint gs.StrokeAlpha (Some (lazy [Mask([geometry], shape.Value)])) true (Mask([geometry], color))
                    | None ->
                        let main = strokeCommand style.Paint
                        let command =
                            if isFourChannel () then
                                let alpha = byte (clamp 0.0 255.0 (gs.StrokeAlpha * 255.0))
                                let k = Color.rgba gs.StrokeK gs.StrokeK gs.StrokeK alpha
                                Components4(main, strokeCommand (Solid k))
                            else main
                        addPaint gs.StrokeAlpha None true command

            let getFontInfo () =
                let key = Pdf.dictGet doc "Font" currentResources |> Option.bind (Pdf.dictGet doc gs.FontName)
                match key with
                | Some obj ->
                    match fontCache.TryGetValue obj with
                    | true, info -> info
                    | _ ->
                        let info = Fonts.resolve opts.CMaps doc currentResources gs.FontName
                        fontCache.[obj] <- info
                        info
                | None -> Fonts.resolve opts.CMaps doc currentResources gs.FontName

            let emitText (stringBytes: byte[]) =
                let fi = getFontInfo ()
                let codes = Fonts.sourceCodes fi stringBytes
                let count = codes.Length
                if count > 0 then
                    let labels = codes |> Array.map (fun (code, bytes) -> Fonts.label fi code bytes)
                    let font = fi.EmbeddedFont |> Option.defaultValue opts.FallbackFont
                    let fontSize = abs gs.FontSize
                    let sign = if gs.FontSize < 0.0 then -1.0 else 1.0
                    let horizontal = gs.HorizontalScaling / 100.0
                    // Glyph outlines use y-down coordinates. The local transform flips
                    // them into text space, applies rise and horizontal scaling, then
                    // composes the complete text matrix and graphics CTM.
                    let localToText =
                        { M11 = sign * horizontal; M12 = 0.0
                          M21 = 0.0; M22 = -sign; DX = 0.0; DY = gs.TextRise }
                    let localToPage =
                        Matrix3x2.multiply (Matrix3x2.multiply localToText textMatrix) gs.CTM
                    let glyphs = ResizeArray<Text.FontData * Text.PositionedGlyph>()
                    let mutable advance = 0.0
                    let mutable advanceY = 0.0
                    for i in 0 .. count - 1 do
                        let sourceCode, bytes = codes.[i]
                        let code = if fi.IsComposite then Fonts.cid fi sourceCode bytes.Length else sourceCode
                        let wordSpacing = if bytes.Length = 1 && sourceCode = 32 then gs.WordSpacing else 0.0
                        let proc =
                            fi.Type3CharProcs
                            |> Option.bind (fun procs ->
                                Pdf.dictGet doc (Fonts.glyphName fi code) procs
                                |> Option.orElseWith (fun () -> Pdf.dictGet doc ".notdef" procs))
                            |> Option.map (Pdf.getStreamData doc)
                        let width, widthY =
                            if fi.Type3Font.IsSome then
                                let wx, wy =
                                    if fi.HasExplicitWidths then Fonts.charWidth fi code, 0.0
                                    else
                                        match proc with
                                        | Some data ->
                                            match tokenize data with
                                            | TNumber wx :: TNumber wy :: TOperator "d0" :: _
                                            | TNumber wx :: TNumber wy :: TNumber _ :: TNumber _ :: TNumber _ :: TNumber _ :: TOperator "d1" :: _ -> wx, wy
                                            | _ -> 0.0, 0.0
                                        | None -> 0.0, 0.0
                                (wx * fi.Type3Matrix.M11 + wy * fi.Type3Matrix.M21) * gs.FontSize,
                                (wx * fi.Type3Matrix.M12 + wy * fi.Type3Matrix.M22) * gs.FontSize
                            else Fonts.charWidth fi code / 1000.0 * gs.FontSize, 0.0
                        let verticalAdvance, originX, originY =
                            if fi.IsVertical then
                                let w1y, v1x, v1y = Fonts.verticalMetrics fi code
                                w1y * gs.FontSize / 1000.0 - gs.CharSpacing - wordSpacing,
                                v1x * gs.FontSize / 1000.0, v1y * gs.FontSize / 1000.0
                            else 0.0, 0.0, 0.0
                        let step = if fi.IsVertical then 0.0 else width + gs.CharSpacing + wordSpacing
                        let x = (advance - originX) * sign
                        let y = -(advanceY - originY) * sign
                        match fi.Type3Font, proc with
                        | Some _, Some data ->
                            let glyphText = { M11 = gs.FontSize * horizontal; M12 = 0.0; M21 = 0.0; M22 = gs.FontSize; DX = advance * horizontal; DY = gs.TextRise + advanceY }
                            let glyphCTM = Matrix3x2.multiply (Matrix3x2.multiply (Matrix3x2.multiply fi.Type3Matrix glyphText) textMatrix) gs.CTM
                            let glyphResources = fi.Type3Resources |> Option.defaultValue currentResources
                            let glyphCmds, _ = renderContent opts doc glyphResources data { gs with CTM = glyphCTM } working viewport scale (depth + 1)
                            cmds.AddRange glyphCmds
                        | _ -> ()
                        match fi.Type3Font, fi.EmbeddedFont, fi.ByteToGlyph, fi.IsComposite with
                        | Some _, _, _, _ -> ()
                        | None, Some _, Some map, false when not fi.UnicodeGlyphs ->
                            let gid = if code < map.Length then map.[code] else 0
                            glyphs.Add(font, { GlyphId = gid; X = x; Y = y; XAdv = step * sign })
                        | None, Some _, _, true ->
                            let gid =
                                match fi.CIDToGIDMap with
                                | Some map -> if code >= 0 && code < map.Length then map.[code] else 0
                                | None -> code
                            glyphs.Add(font, { GlyphId = gid; X = x; Y = y; XAdv = step * sign })
                        | _ ->
                            // Shape only the Unicode represented by this PDF code;
                            // positioning between codes always comes from PDF widths.
                            let runs = Text.layout font fontSize labels.[i]
                            let missing = List.isEmpty runs || (runs |> List.exists (fun run -> run.Glyphs |> Array.exists (fun glyph -> glyph.GlyphId = 0)))
                            let selectedFont, selectedRuns =
                                if fi.EmbeddedFont.IsNone && missing then
                                    match opts.CjkFont.Value with
                                    | Some cjk -> cjk, Text.layout cjk fontSize labels.[i]
                                    | None -> font, runs
                                else font, runs
                            for run in selectedRuns do
                                for glyph in run.Glyphs do
                                    glyphs.Add(selectedFont, { glyph with X = x + glyph.X; Y = y + glyph.Y })

                        let x2 = x + (if fi.IsVertical then width else step) * sign
                        let top = if fi.IsVertical then -advanceY * sign else y - 0.8 * fontSize
                        let bottom = if fi.IsVertical then -(advanceY + verticalAdvance) * sign else y + 0.2 * fontSize
                        let p0 = Matrix3x2.transformVec2 localToPage (vec2 x top)
                        let p1 = Matrix3x2.transformVec2 localToPage (vec2 x2 top)
                        let p2 = Matrix3x2.transformVec2 localToPage (vec2 x bottom)
                        let p3 = Matrix3x2.transformVec2 localToPage (vec2 x2 bottom)
                        let minX = float (min (min p0.X p1.X) (min p2.X p3.X))
                        let maxX = float (max (max p0.X p1.X) (max p2.X p3.X))
                        let minY = float (min (min p0.Y p1.Y) (min p2.Y p3.Y))
                        let maxY = float (max (max p0.Y p1.Y) (max p2.Y p3.Y))
                        charBoxes.Add({ Char = labels.[i]; X = minX; Y = minY; W = maxX - minX; H = maxY - minY })
                        advance <- advance + step
                        advanceY <- advanceY + (if fi.IsVertical then verticalAdvance else widthY)

                    if glyphs.Count > 0 then
                        let mode = gs.TextRenderMode
                        let fill = mode = 0 || mode = 2 || mode = 4 || mode = 6
                        let stroke = mode = 1 || mode = 2 || mode = 5 || mode = 6
                        let clipping = mode >= 4 && mode <= 7
                        if clipping then hasTextClip <- true
                        if mode = 0 && gs.FillPattern.IsNone then
                            // Keep ordinary text on the backends' cached glyph path.
                            let runs = ResizeArray<Text.GlyphRun>()
                            let mutable first = 0
                            while first < glyphs.Count do
                                let glyphFont = fst glyphs.[first]
                                let mutable finish = first + 1
                                while finish < glyphs.Count && (fst glyphs.[finish]).Id = glyphFont.Id do
                                    finish <- finish + 1
                                runs.Add { Font = glyphFont; FontSize = fontSize
                                           Glyphs = Array.init (finish - first) (fun i -> snd glyphs.[first + i])
                                           Direction = Rbt.Shaper.Shaper.LeftToRight }
                                first <- finish
                            let runs = List.ofSeq runs
                            let color = fillColor gs
                            let main = Group(localToPage, 1.0, [DrawGlyphs(vec2 0.0 0.0, runs, Solid color)])
                            let command =
                                if isFourChannel () then
                                    let fourth = Group(localToPage, 1.0, [DrawGlyphs(vec2 0.0 0.0, runs, Solid (fourthColor gs.FillK color))])
                                    Components4(main, fourth)
                                else main
                            addPaint gs.FillAlpha None true command
                        elif fill || stroke || clipping then
                            for glyphFont, glyph in glyphs do
                                let size = fontSize / Text.unitsPerEm glyphFont
                                let segments =
                                    Text.glyphOutline glyphFont glyph.GlyphId
                                    |> List.map (Text.translatePathSeg glyph.X glyph.Y size)
                                    |> transformPath localToPage
                                if clipping then textClip.Add segments
                                if fill || stroke then paintPath segments fill stroke NonZero
                    textMatrix <- Matrix3x2.multiply (Matrix3x2.translate (advance * horizontal) advanceY) textMatrix

            let resolveColorSpace (csName: string) =
                match csName with
                | "DeviceGray" | "DeviceRGB" | "DeviceCMYK" | "Pattern" -> csName
                | _ ->
                    match Pdf.dictGet doc "ColorSpace" currentResources with
                    | Some csDict ->
                        match Pdf.dictGet doc csName csDict with
                        | Some csObj ->
                            let resolved = Pdf.deref doc csObj
                            match resolved with
                            | PdfArray items when not items.IsEmpty ->
                                let typeName = match Pdf.deref doc items.[0] with PdfName n -> n | _ -> ""
                                match typeName with
                                | "Pattern" ->
                                    match items with
                                    | [_; PdfName baseSpace] -> "Pattern:" + baseSpace
                                    | _ -> "Pattern"
                                | _ -> csName
                            | PdfName n -> n
                            | _ -> "DeviceGray"
                        | None -> csName
                    | None -> csName

            let emitPath (fill: bool) (stroke: bool) (fillRule: FillRule) =
                let segs = path.Segments
                if not segs.IsEmpty then paintPath segs fill stroke fillRule
                path.Clear()

            let clipStack = ResizeArray<(int * ClipKind) option>()
            let wrapClip kind children =
                match kind with
                | PathClip (segs, rule) -> Clip(segs, rule, children)
                | TextClip paths ->
                    let masks = paths |> List.map (fun segs -> FillPath(segs, Solid Color.white, NonZero))
                    Mask(masks, children)

            let streamStack = Collections.Generic.Stack<Token[] * int * PdfObj * Matrix3x2 * bool * PdfColor.WorkingSpace option * (int * bool * bool * PdfColor.WorkingSpace option) option>()
            let mutable tokenArr = tokens |> Array.ofList
            let mutable tokenCount = tokenArr.Length
            let mutable i = 0

            let startClipIfPending () =
                match pendingClip with
                | Some (clipSegs, clipRule) ->
                    pendingClip <- None
                    clipStack.Add(Some (cmds.Count, PathClip(clipSegs, clipRule)))
                | None -> ()

            let paintImage image =
                match PdfImages.decode opts.DecodeJpeg toImageTarget (isFourChannel ()) doc currentResources
                    image { (fillColor gs) with A = 255uy } gs.FillK 0 with
                | Some (w, h, pixels, geometry, softImageMask, geometricMask, fourth) ->
                    let interpolate = Pdf.dictGet doc "Interpolate" image |> Option.map (Pdf.deref doc) = Some (PdfBool true)
                    let ctm, alpha = gs.CTM, gs.FillAlpha
                    let main = Group(ctm, alpha, [BitmapPaint.command pixels w h interpolate])
                    let command =
                        match fourth with
                        | Some k -> Components4(main, Group(ctm, alpha, [BitmapPaint.command k w h interpolate]))
                        | None -> main
                    let intrinsic =
                        if softImageMask && not gs.AlphaIsShape then
                            Some (lazy (
                                match geometry with
                                | Some shapePixels -> [Cmd.shapeOf (Group(ctm, 1.0, [BitmapPaint.command shapePixels w h interpolate]))]
                                | None ->
                                    let square = [MoveTo(vec2 0.0 0.0); LineTo(vec2 1.0 0.0); LineTo(vec2 1.0 1.0); LineTo(vec2 0.0 1.0); Close]
                                    [Group(ctm, 1.0, [FillPath(square, Solid Color.white, NonZero)])]))
                        else None
                    addPaint alpha intrinsic (not (softImageMask || geometricMask)) command
                | None -> ()

            let mutable loopRunning = true
            while loopRunning do
                if i >= tokenCount then
                    if streamStack.Count > 0 then
                        let (prevArr, prevI, prevRes, prevDefaultCTM, prevDefaultAIS, prevWorking, groupStart) = streamStack.Pop()
                        working <- prevWorking
                        if stateStack.Count > 0 then
                            gs <- stateStack.[stateStack.Count - 1]
                            stateStack.RemoveAt(stateStack.Count - 1)
                        let mutable poppingClips2 = true
                        while poppingClips2 && clipStack.Count > 0 do
                            match clipStack.[clipStack.Count - 1] with
                            | Some (startIdx, kind) ->
                                let clippedCmds = cmds |> Seq.skip startIdx |> Seq.toList
                                cmds.RemoveRange(startIdx, cmds.Count - startIdx)
                                cmds.Add(wrapClip kind clippedCmds)
                                clipStack.RemoveAt(clipStack.Count - 1)
                            | None ->
                                clipStack.RemoveAt(clipStack.Count - 1)
                                poppingClips2 <- false
                        match groupStart with
                        | Some (start, isolated, knockout, childWorking) ->
                            let children = cmds |> Seq.skip start |> Seq.toList
                            cmds.RemoveRange(start, cmds.Count - start)
                            let group =
                                managedGroup (prevWorking |> Option.defaultValue PdfColor.displaySpace) childWorking
                                    isolated knockout gs.FillAlpha (blendMode gs.BlendMode) gs.SoftMask children
                            if gs.AlphaIsShape && (gs.FillAlpha <> 1.0 || gs.SoftMask.IsSome) then
                                let alpha, mask = gs.FillAlpha, gs.SoftMask
                                let shape = lazy (
                                    let intrinsic = [Cmd.shapeOf group]
                                    let withAlpha = if alpha = 1.0 then intrinsic else [Group(Matrix3x2.identity, alpha, intrinsic)]
                                    match mask with Some commands -> [Mask(commands, withAlpha)] | None -> withAlpha)
                                cmds.Add(WithShape(shape, [group]))
                            else cmds.Add group
                        | None -> ()
                        tokenArr <- prevArr
                        tokenCount <- prevArr.Length
                        i <- prevI
                        currentResources <- prevRes
                        defaultCTM <- prevDefaultCTM
                        defaultAlphaIsShape <- prevDefaultAIS
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
                            | Some (startIdx, kind) ->
                                let clippedCmds = cmds |> Seq.skip startIdx |> Seq.toList
                                cmds.RemoveRange(startIdx, cmds.Count - startIdx)
                                cmds.Add(wrapClip kind clippedCmds)
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
                                match Pdf.dictGet doc "AIS" gsObj |> Option.map (Pdf.deref doc) with
                                | Some (PdfBool value) -> gs <- { gs with AlphaIsShape = value }
                                | _ -> ()
                                let supportedBlend name =
                                    match name with
                                    | "Normal" | "Compatible" | "Multiply" | "Screen" | "Overlay"
                                    | "Darken" | "Lighten" | "ColorDodge" | "ColorBurn" | "HardLight"
                                    | "SoftLight" | "Difference" | "Exclusion" | "Hue" | "Saturation"
                                    | "Color" | "Luminosity" -> true
                                    | _ -> false
                                match Pdf.dictGet doc "BM" gsObj |> Option.map (Pdf.deref doc) with
                                | Some (PdfName bm) -> gs <- { gs with BlendMode = if supportedBlend bm then bm else "Normal" }
                                | Some (PdfArray choices) ->
                                    let selected = choices |> List.tryPick (fun choice ->
                                        match Pdf.deref doc choice with PdfName bm when supportedBlend bm -> Some bm | _ -> None)
                                    gs <- { gs with BlendMode = Option.defaultValue "Normal" selected }
                                | _ -> ()
                                match Pdf.dictGet doc "SMask" gsObj |> Option.map (Pdf.deref doc) with
                                | Some (PdfName "None") -> gs <- { gs with SoftMask = None }
                                | Some mask ->
                                    let transfer =
                                        match Pdf.dictGet doc "TR" mask |> Option.map (Pdf.deref doc) with
                                        | None | Some (PdfName "Identity") -> None
                                        | Some fn -> Some (Shading.tryEvaluateFunction doc fn |> Option.defaultWith (fun () -> invalidOp "Unsupported PDF soft-mask transfer function"))
                                    let group = Pdf.dictGet doc "G" mask |> Option.defaultWith (fun () -> invalidOp "PDF soft mask has no group")
                                    let number value = match Pdf.deref doc value with PdfInt n -> float n | PdfReal n -> n | _ -> 0.0
                                    let matrix =
                                        match Pdf.dictGet doc "Matrix" group |> Option.map (Pdf.deref doc) with
                                        | Some (PdfArray [a;b;c;d;e;f]) ->
                                            { M11 = number a; M12 = number b; M21 = number c; M22 = number d; DX = number e; DY = number f }
                                        | _ -> Matrix3x2.identity
                                    let maskCTM = Matrix3x2.multiply matrix gs.CTM
                                    let maskResources = Pdf.dictGet doc "Resources" group |> Option.defaultValue currentResources
                                    let maskState = { gs with CTM = maskCTM; SoftMask = None; BlendMode = "Normal"; FillAlpha = 1.0; StrokeAlpha = 1.0 }
                                    let groupDict = Pdf.dictGet doc "Group" group
                                    let maskWorking =
                                        let declaredGray =
                                            groupDict
                                            |> Option.bind (Pdf.dictGet doc "CS")
                                            |> Option.bind (PdfColor.resolve doc maskResources)
                                            |> Option.exists PdfColor.isDeviceGray
                                        if declaredGray then Some PdfColor.deviceGraySpace
                                        else groupDict |> Option.bind (groupWorkingSpace doc maskResources) |> Option.orElse working
                                    let children, _ = renderContent opts doc maskResources (Pdf.getStreamData doc group) maskState maskWorking viewport scale (depth + 1)
                                    let knockout = groupDict |> Option.bind (Pdf.dictGet doc "K") |> Option.map (Pdf.deref doc) = Some (PdfBool true)
                                    let isolated = groupDict |> Option.bind (Pdf.dictGet doc "I") |> Option.map (Pdf.deref doc) = Some (PdfBool true)
                                    // The mask surface holds group-space pixels, not parent-space pixels.
                                    let children = [TransparencyGroup(isolated, knockout, 1.0, Normal, None, children)]
                                    let bounded =
                                        match Pdf.dictGet doc "BBox" group |> Option.map (Pdf.deref doc) with
                                        | Some (PdfArray [a;b;c;d]) ->
                                            let p x y = Matrix3x2.transformVec2 maskCTM (vec2 x y)
                                            let x0,y0,x1,y1 = number a,number b,number c,number d
                                            [Clip([MoveTo(p x0 y0); LineTo(p x1 y0); LineTo(p x1 y1); LineTo(p x0 y1); Close], NonZero, children)]
                                        | _ -> children
                                    let luminosity =
                                        match Pdf.dictGet doc "S" mask |> Option.map (Pdf.deref doc) with
                                        | Some (PdfName "Alpha") -> false
                                        | Some (PdfName "Luminosity") -> true
                                        | _ -> invalidOp "Unsupported PDF soft-mask subtype"
                                    let maskCommands =
                                        if not luminosity && transfer.IsNone then bounded
                                        else
                                            let input =
                                                if not luminosity then bounded
                                                else
                                                    let colorSpace = groupDict |> Option.bind (Pdf.dictGet doc "CS") |> Option.defaultValue (PdfName "DeviceRGB")
                                                    let space = PdfColor.resolve doc maskResources colorSpace |> Option.defaultWith (fun () -> invalidOp "Unsupported PDF soft-mask backdrop color space")
                                                    let count = PdfColor.components space
                                                    let limits = PdfColor.ranges space
                                                    let values =
                                                        match Pdf.dictGet doc "BC" mask |> Option.map (Pdf.deref doc) with
                                                        | None -> Array.zeroCreate count
                                                        | Some (PdfArray values) when values.Length = count -> values |> List.map number |> List.toArray
                                                        | _ -> invalidOp "Invalid PDF soft-mask backdrop color"
                                                    let encodedComponent i =
                                                        let value = max limits.[2*i] (min limits.[2*i+1] values.[i])
                                                        byte (int (clamp 0.0 1.0 value * 255.0 + 0.5))
                                                    match maskWorking with
                                                    | Some target when target.FourChannel.IsSome ->
                                                        let struct(c,m,y,k) = PdfColor.toWorkingCmyk target space values
                                                        let x0,y0,x1,y1 = viewport.Bounds
                                                        let whole = [MoveTo(vec2 x0 y0); LineTo(vec2 x1 y0); LineTo(vec2 x1 y1); LineTo(vec2 x0 y1); Close]
                                                        let primary = FillPath(whole, Solid(Color.rgb c m y), NonZero)
                                                        let fourth = FillPath(whole, Solid(Color.rgb k k k), NonZero)
                                                        Components4(primary, fourth) :: bounded
                                                    | _ ->
                                                        let r = encodedComponent 0
                                                        let g, b = if count = 1 then r, r else encodedComponent 1, encodedComponent 2
                                                        let backdrop = Pixel.fromColor { R = r; G = g; B = b; A = 255uy }
                                                        // BC is the group backdrop even outside its BBox.
                                                        let seed _ _ (pixels: uint32[]) =
                                                            Array.fill pixels 0 pixels.Length backdrop
                                                            pixels
                                                        Filtered([Custom seed], []) :: bounded
                                            match maskWorking with
                                            | Some target when luminosity && target.FourChannel.IsSome ->
                                                let forward, _ = target.FourChannel.Value
                                                let x0,y0,x1,y1 = viewport.Bounds
                                                let requested = if scale > 0.0 then scale else 1.0
                                                let resolution = requested * min 1.0 (sqrt (4000000.0 / ((x1-x0)*(y1-y0)*requested*requested)))
                                                let width = max 1 (int (ceil ((x1-x0)*resolution)))
                                                let height = max 1 (int (ceil ((y1-y0)*resolution)))
                                                let sx,sy = float width/(x1-x0), float height/(y1-y0)
                                                let toPixels =
                                                    { M11 = sx; M12 = 0.0; M21 = 0.0; M22 = -sy
                                                      DX = -x0*sx; DY = y1*sy }
                                                let primary, fourth = opts.Rasterize4 width height [Group(toPixels, 1.0, input)]
                                                let pixels = Framebuffer.tryGetArray primary |> Option.get
                                                let black = Framebuffer.tryGetArray fourth |> Option.get
                                                for index in 0 .. pixels.Length-1 do
                                                    let color = pixels.[index]
                                                    let alpha = Pixel.a color
                                                    let lightness =
                                                        if alpha = 0uy then 0.0
                                                        else
                                                            let unpremul channel =
                                                                min 1.0 (float channel / float alpha)
                                                            let struct(_, y, _) =
                                                                forward (unpremul (Pixel.r color)) (unpremul (Pixel.g color))
                                                                    (unpremul (Pixel.b color)) (unpremul (Pixel.r black.[index]))
                                                            clamp 0.0 1.0 y
                                                    let value = match transfer with Some fn -> fn lightness | None -> lightness
                                                    let maskAlpha = byte (clamp 0.0 255.0 (value * 255.0))
                                                    pixels.[index] <- Pixel.pack maskAlpha maskAlpha maskAlpha maskAlpha
                                                let toPage = { M11 = x1-x0; M12 = 0.0; M21 = 0.0; M22 = y1-y0; DX = x0; DY = y0 }
                                                [Group(toPage, 1.0, [BitmapPaint.command pixels width height true])]
                                            | _ ->
                                                let convert _ _ (pixels: uint32[]) =
                                                    pixels |> Array.map (fun pixel ->
                                                        let c = Pixel.toColor pixel
                                                        let value =
                                                            if not luminosity then float c.A / 255.0
                                                            else
                                                                let r, g, b = float c.R / 255.0, float c.G / 255.0, float c.B / 255.0
                                                                match maskWorking with
                                                                | Some target when not target.IsDisplay && not target.IsGray ->
                                                                    let struct(_, y, _) = target.ToPcs r g b
                                                                    clamp 0.0 1.0 y
                                                                | _ -> 0.3 * r + 0.59 * g + 0.11 * b
                                                        let mapped = match transfer with Some fn -> fn value | None -> value
                                                        let a = byte (clamp 0.0 255.0 (mapped * 255.0))
                                                        Pixel.fromColor { R = a; G = a; B = a; A = a })
                                                [Filtered([Custom convert], input)]
                                    gs <- { gs with SoftMask = Some maskCommands }
                                | None -> ()
                            | None -> ()
                        | None -> ()

                    // ── Color ──
                    | "g" | "G" | "rg" | "RG" | "k" | "K" as op ->
                        let count, name =
                            match op.ToLowerInvariant() with
                            | "g" -> 1, "DeviceGray"
                            | "rg" -> 3, "DeviceRGB"
                            | _ -> 4, "DeviceCMYK"
                        let values = Array.init count (fun _ -> popNum ()) |> Array.rev
                        let source = PdfColor.device count |> Option.map (fun space -> space, values)
                        let color, k = source |> Option.map (fun s -> sourceColor s 1.0) |> Option.defaultValue (Color.black,0uy)
                        if op = op.ToLowerInvariant() then
                            gs <- { gs with FillColor = color; FillK = k; FillSource = source; ColorSpace = name; FillPattern = None }
                        else
                            gs <- { gs with StrokeColor = color; StrokeK = k; StrokeSource = source; StrokeColorSpace = name; StrokePattern = None }
                    | "cs" ->
                        let csName = popName ()
                        gs <- { gs with ColorSpace = resolveColorSpace csName; FillPattern = None }
                    | "CS" ->
                        let csName = popName ()
                        gs <- { gs with StrokeColorSpace = resolveColorSpace csName; StrokePattern = None }
                    | "sc" | "scn" ->
                        let ops = popAllOperands ()
                        let nums = ops |> List.choose (function TNumber n -> Some n | _ -> None)
                        let space = if gs.ColorSpace.StartsWith("Pattern:") then gs.ColorSpace.Substring(8) else gs.ColorSpace
                        let source = PdfColor.resolve doc currentResources (PdfName space) |> Option.map (fun s -> s, List.toArray nums)
                        let color, k = source |> Option.map (fun s -> sourceColor s gs.FillAlpha) |> Option.defaultWith (fun () ->
                            if working.IsSome && not nums.IsEmpty then invalidOp "Unsupported PDF paint color space for transparency blending"
                            pdfColorToColor space nums gs.FillAlpha, 0uy)
                        let pattern = if gs.ColorSpace.StartsWith("Pattern") then ops |> List.tryPick (function TName n -> Some n | _ -> None) else None
                        gs <- { gs with FillColor = color; FillK = k; FillSource = source; FillPattern = pattern }
                    | "SC" | "SCN" ->
                        let ops = popAllOperands ()
                        let nums = ops |> List.choose (function TNumber n -> Some n | _ -> None)
                        let space = if gs.StrokeColorSpace.StartsWith("Pattern:") then gs.StrokeColorSpace.Substring(8) else gs.StrokeColorSpace
                        let source = PdfColor.resolve doc currentResources (PdfName space) |> Option.map (fun s -> s, List.toArray nums)
                        let color, k = source |> Option.map (fun s -> sourceColor s gs.StrokeAlpha) |> Option.defaultWith (fun () ->
                            if working.IsSome && not nums.IsEmpty then invalidOp "Unsupported PDF paint color space for transparency blending"
                            pdfColorToColor space nums gs.StrokeAlpha, 0uy)
                        let pattern = if gs.StrokeColorSpace.StartsWith("Pattern") then ops |> List.tryPick (function TName n -> Some n | _ -> None) else None
                        gs <- { gs with StrokeColor = color; StrokeK = k; StrokeSource = source; StrokePattern = pattern }

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
                        path.CubicFromCurrent(x2, y2, x3, y3)
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
                        textClip.Clear()
                        hasTextClip <- false
                    | "ET" ->
                        inText <- false
                        if hasTextClip then
                            clipStack.Add(Some (cmds.Count, TextClip (textClip |> Seq.toList)))
                        textClip.Clear()
                        hasTextClip <- false

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
                                let shift = -n / 1000.0 * gs.FontSize
                                let dx, dy = if (getFontInfo ()).IsVertical then 0.0, shift else shift * (gs.HorizontalScaling / 100.0), 0.0
                                textMatrix <- Matrix3x2.multiply (Matrix3x2.translate dx dy) textMatrix
                            | _ -> ()

                    | "sh" ->
                        let name = popName ()
                        match Shading.tryRender toImageTarget (isFourChannel ()) doc currentResources name gs.CTM 1.0 viewport.Bounds scale with
                        | Some command ->
                            let color = if gs.FillAlpha = 1.0 then command else Group(Matrix3x2.identity, gs.FillAlpha, [command])
                            addPaint gs.FillAlpha None true color
                        | None -> ()

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
                                | "Image" -> paintImage xobj

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
                                            let childWorking, groupStart =
                                                match Pdf.dictGet doc "Group" xobj with
                                                | Some group when Pdf.dictGet doc "S" group |> Option.map (Pdf.deref doc) = Some (PdfName "Transparency") ->
                                                    let child = groupWorkingSpace doc formResources group
                                                    let knockout = Pdf.dictGet doc "K" group |> Option.map (Pdf.deref doc) = Some (PdfBool true)
                                                    let isolated = Pdf.dictGet doc "I" group |> Option.map (Pdf.deref doc) = Some (PdfBool true)
                                                    child |> Option.orElse working, Some (cmds.Count, isolated, knockout, child)
                                                | _ -> working, None
                                            streamStack.Push(tokenArr, i, currentResources, defaultCTM, defaultAlphaIsShape, working, groupStart)
                                            stateStack.Add(gs)
                                            clipStack.Add(None)
                                            gs <- { gs with CTM = Matrix3x2.multiply formMatrix gs.CTM }
                                            working <- childWorking
                                            if groupStart.IsSome then
                                                gs <- { gs with FillAlpha = 1.0; StrokeAlpha = 1.0; BlendMode = "Normal"; SoftMask = None }
                                            gs <- refreshColors gs
                                            defaultCTM <- gs.CTM
                                            defaultAlphaIsShape <- gs.AlphaIsShape
                                            // Form content is always clipped to its BBox,
                                            // transformed by the form matrix and caller CTM.
                                            match Pdf.dictGet doc "BBox" xobj |> Option.map (Pdf.deref doc) with
                                            | Some (PdfArray [a; b; c; d]) ->
                                                let x0, y0, x1, y1 = fl (Pdf.deref doc a), fl (Pdf.deref doc b), fl (Pdf.deref doc c), fl (Pdf.deref doc d)
                                                let p x y = Matrix3x2.transformVec2 gs.CTM (vec2 x y)
                                                let bounds = [ MoveTo(p x0 y0); LineTo(p x1 y0); LineTo(p x1 y1); LineTo(p x0 y1); Close ]
                                                clipStack.Add(Some (cmds.Count, PathClip(bounds, NonZero)))
                                            | _ -> ()
                                            currentResources <- formResources
                                            tokenArr <- formTokens
                                            tokenCount <- formTokens.Length
                                            i <- 0
                                    with _ -> reraise ()
                                | _ -> ()
                            | None -> ()
                        | None -> ()

                    // ── Inline Image (BI/ID/EI) ──
                    | "BI" ->
                        let mutable header = Map.empty
                        let mutable rawData = None
                        let rec readValue () =
                            if i >= tokenCount then PdfNull else
                            let token = tokenArr.[i]
                            i <- i + 1
                            match token with
                            | TNumber n -> PdfReal n
                            | TName n -> PdfName n
                            | TBool b -> PdfBool b
                            | TString b | THexString b -> PdfString b
                            | TArrayStart ->
                                let values = ResizeArray<PdfObj>()
                                while i < tokenCount && tokenArr.[i] <> TArrayEnd do
                                    values.Add(readValue ())
                                if i < tokenCount then i <- i + 1
                                PdfArray (values |> Seq.toList)
                            | TOperator "<<" ->
                                let mutable entries = Map.empty
                                while i < tokenCount && tokenArr.[i] <> TOperator ">>" do
                                    match tokenArr.[i] with
                                    | TName key ->
                                        i <- i + 1
                                        entries <- Map.add key (readValue ()) entries
                                    | _ -> i <- i + 1
                                if i < tokenCount then i <- i + 1
                                PdfDict entries
                            | _ -> PdfNull
                        let rec expandColorSpace = function
                            | PdfName "G" -> PdfName "DeviceGray"
                            | PdfName "RGB" -> PdfName "DeviceRGB"
                            | PdfName "CMYK" -> PdfName "DeviceCMYK"
                            | PdfName "I" -> PdfName "Indexed"
                            | PdfArray values -> PdfArray (List.map expandColorSpace values)
                            | value -> value
                        let mutable doneImage = false
                        while i < tokenCount && not doneImage do
                            match tokenArr.[i] with
                            | TInlineImageData bytes -> rawData <- Some bytes; i <- i + 1
                            | TOperator "EI" -> i <- i + 1; doneImage <- true
                            | TName key ->
                                i <- i + 1
                                let key =
                                    match key with
                                    | "W" -> "Width" | "H" -> "Height" | "BPC" -> "BitsPerComponent"
                                    | "CS" -> "ColorSpace" | "F" -> "Filter" | "D" -> "Decode"
                                    | "DP" -> "DecodeParms" | "IM" -> "ImageMask" | "I" -> "Interpolate"
                                    | _ -> key
                                let value = readValue ()
                                header <- Map.add key (if key = "ColorSpace" then expandColorSpace value else value) header
                            | _ -> i <- i + 1
                        match rawData with
                        | Some bytes ->
                            let image = PdfStream(header, bytes)
                            paintImage image
                        | None -> ()

                    | _ ->
                        operands.Clear()

            // Close remaining clip scopes
            while clipStack.Count > 0 do
                match clipStack.[clipStack.Count - 1] with
                | Some (startIdx, kind) ->
                    let clippedCmds = cmds |> Seq.skip startIdx |> Seq.toList
                    cmds.RemoveRange(startIdx, cmds.Count - startIdx)
                    cmds.Add(wrapClip kind clippedCmds)
                | None -> ()
                clipStack.RemoveAt(clipStack.Count - 1)

            cmds |> Seq.toList, charBoxes.ToArray()

let private annotationNumber (doc: PdfDocument) value =
    match Pdf.deref doc value with
    | PdfInt n -> Some (float n)
    | PdfReal n when not (Double.IsNaN n || Double.IsInfinity n) -> Some n
    | _ -> None

let private annotationBox (doc: PdfDocument) value =
    match Pdf.deref doc value with
    | PdfArray [a; b; c; d] ->
        match annotationNumber doc a, annotationNumber doc b, annotationNumber doc c, annotationNumber doc d with
        | Some x0, Some y0, Some x1, Some y1 when x0 <> x1 && y0 <> y1 ->
            Some (min x0 x1, min y0 y1, max x0 x1, max y0 y1)
        | _ -> None
    | _ -> None

let private appearanceMatrix (doc: PdfDocument) appearance =
    match Pdf.dictGet doc "Matrix" appearance |> Option.map (Pdf.deref doc) with
    | Some (PdfArray [a; b; c; d; e; f]) ->
        match List.map (annotationNumber doc) [a; b; c; d; e; f] with
        | [Some a; Some b; Some c; Some d; Some e; Some f] ->
            { M11 = a; M12 = b; M21 = c; M22 = d; DX = e; DY = f }
        | _ -> Matrix3x2.identity
    | _ -> Matrix3x2.identity

// A missing widget appearance is a Form, not page content: the normal Form
// placement below supplies its BBox, resources, rotation and Rect clipping.
let private widgetAppearance (opts: RenderOptions) (doc: PdfDocument) acro annotation (x0, y0, x1, y1) =
    let get key node = Pdf.dictGet doc key node |> Option.map (Pdf.deref doc)
    let rec inherited key visited depth node =
        let identity =
            match node with
            | PdfIndirect reference -> Some reference
            | _ -> None
        if depth >= 64 || (identity |> Option.exists (fun reference -> Set.contains reference visited)) then None
        else
            match get key node with
            | Some value -> Some value
            | None ->
                let visited = identity |> Option.map (fun reference -> Set.add reference visited) |> Option.defaultValue visited
                Pdf.dictGet doc "Parent" node |> Option.bind (inherited key visited (depth + 1))
    let field key = inherited key Set.empty 0 annotation
    let integer = function Some (PdfInt n) -> n | _ -> 0L
    let stringValue = function
        | Some (PdfString bytes) -> Fonts.decodePdfString bytes
        | Some (PdfName name) -> name
        | _ -> ""
    let number = annotationNumber doc
    let color value =
        match value with
        | Some (PdfArray components) ->
            match List.map (Pdf.deref doc >> number) components with
            | [Some a] -> Some (sprintf "%s g" (a.ToString("0.###", Globalization.CultureInfo.InvariantCulture)))
            | [Some a; Some b; Some c] ->
                Some (String.concat " " ([a;b;c] |> List.map (fun n -> n.ToString("0.###", Globalization.CultureInfo.InvariantCulture))) + " rg")
            | [Some a; Some b; Some c; Some d] ->
                Some (String.concat " " ([a;b;c;d] |> List.map (fun n -> n.ToString("0.###", Globalization.CultureInfo.InvariantCulture))) + " k")
            | _ -> None
        | _ -> None
    let fmt (n: float) = n.ToString("0.###", Globalization.CultureInfo.InvariantCulture)
    let emit (builder: System.Text.StringBuilder) (parts: string list) =
        builder.Append(String.concat " " parts).Append('\n') |> ignore
    let mk = get "MK" annotation
    let width, height = x1 - x0, y1 - y0
    let rotation = mk |> Option.bind (get "R") |> Option.map (fun value -> integer (Some value)) |> Option.defaultValue 0L
    let rotation = ((rotation % 360L) + 360L) % 360L
    let w, h = if rotation = 90L || rotation = 270L then height, width else width, height
    let matrix =
        match rotation with
        | 90L -> [0.; 1.; -1.; 0.; width; 0.]
        | 180L -> [-1.; 0.; 0.; -1.; width; height]
        | 270L -> [0.; -1.; 1.; 0.; 0.; height]
        | _ -> [1.; 0.; 0.; 1.; 0.; 0.]
    let background = mk |> Option.bind (get "BG") |> color
    let borderColor = mk |> Option.bind (get "BC") |> color
    let border =
        get "BS" annotation |> Option.bind (get "W") |> Option.bind number
        |> Option.orElseWith (fun () ->
            match get "Border" annotation with
            | Some (PdfArray (_ :: _ :: lineWidth :: _)) -> number (Pdf.deref doc lineWidth)
            | _ -> None)
        |> Option.defaultValue 1.0
    let style = get "BS" annotation |> Option.bind (get "S")
    let kind = field "FT"
    let ff = integer (field "Ff")
    let radio = ff &&& (1L <<< 15) <> 0L
    let push = ff &&& (1L <<< 16) <> 0L
    let caption = mk |> Option.bind (get "CA") |> Option.map (fun value -> stringValue (Some value))
    let roundBorder = radio && caption.IsNone
    let stroke =
        borderColor
        |> Option.map (fun source -> source.Replace(" rg", " RG").Replace(" g", " G").Replace(" k", " K"))
    let paint = System.Text.StringBuilder()
    background |> Option.iter (fun paintColor ->
        emit paint [paintColor; "0 0"; fmt w; fmt h; "re f"])
    if border > 0.0 && not roundBorder then
        stroke |> Option.iter (fun strokeColor ->
            let inset = border / 2.0
            emit paint [strokeColor; fmt border; "w"]
            match style with
            | Some (PdfName "U") -> emit paint [fmt inset; fmt inset; "m"; fmt (w-inset); fmt inset; "l S"]
            | _ when w > border && h > border ->
                emit paint [fmt inset; fmt inset; fmt (w-border); fmt (h-border); "re S"]
            | _ -> ())
    let resources = acro |> Option.bind (get "DR") |> Option.defaultValue (PdfDict Map.empty)
    let normal = get "AP" annotation |> Option.bind (get "N")
    let stateNames =
        match normal with Some (PdfDict states) -> states |> Map.toList |> List.map fst |> List.filter ((<>) "Off") | _ -> []
    let state = get "AS" annotation |> Option.bind (function PdfName name -> Some name | _ -> None)
    let value = field "V" |> Option.map (fun value -> stringValue (Some value)) |> Option.defaultValue ""
    let parent = Pdf.dictGet doc "Parent" annotation
    let kidIndex =
        match parent |> Option.bind (get "Kids") with
        | Some (PdfArray kids) -> kids |> List.tryFindIndex ((=) annotation)
        | _ -> None
    let options =
        match field "Opt" with
        | Some (PdfArray items) -> Some items
        | _ -> None
    // With /Opt, AP on-state names (and V) are Kids indices, not export strings.
    let indexedOnState =
        match options, kidIndex with
        | Some items, Some index when index < items.Length -> Some (string index)
        | _ -> None
    let onState =
        indexedOnState |> Option.orElseWith (fun () ->
            match state with
            | Some name when name <> "Off" -> Some name
            | _ ->
                match stateNames with
                | [name] -> Some name
                | _ when radio && (parent |> Option.bind (get "Kids") |> Option.exists (function PdfArray (_ :: _ :: _) -> true | _ -> false)) -> None
                | _ when value <> "" && value <> "Off" -> Some value
                | _ -> Some "Yes")
        |> Option.defaultValue ""
    let radiosInUnison = ff &&& (1L <<< 25) <> 0L
    let uniqueRadio =
        if not radio || radiosInUnison then true
        else
            match parent |> Option.bind (get "Kids") with
            | Some (PdfArray kids) ->
                let active = kids |> List.tryFind (fun kid -> get "AS" kid = Some (PdfName value))
                let firstMatching =
                    kids |> List.tryFind (fun kid ->
                        match get "AP" kid |> Option.bind (get "N") with
                        | Some (PdfDict states) -> states.ContainsKey value
                        | _ -> false)
                match active |> Option.orElse firstMatching with
                | Some chosen -> chosen = annotation
                | None -> true
            | _ -> true
    let selected =
        (value <> "" && value <> "Off" && value = onState && uniqueRadio)
        || (radio && radiosInUnison
            && (match options, kidIndex, Int32.TryParse value with
                | Some items, Some index, (true, chosen) when index < items.Length && chosen >= 0 && chosen < items.Length ->
                    stringValue (Some (Pdf.deref doc items.[index])) = stringValue (Some (Pdf.deref doc items.[chosen]))
                | _ -> false))
    let parseDefaultAppearance () =
        let da =
            field "DA" |> Option.orElseWith (fun () -> acro |> Option.bind (get "DA"))
            |> Option.bind (function PdfString bytes -> Some bytes | _ -> None)
            |> Option.defaultValue [||]
        let tokens = tokenize da |> List.toArray
        let mutable fontName = ""
        let mutable fontSize = 0.0
        let mutable ink = "0 g"
        for i in 0 .. tokens.Length - 1 do
            match tokens.[i] with
            | TOperator "Tf" when i >= 2 ->
                match tokens.[i-2], tokens.[i-1] with
                | TName name, TNumber size -> fontName <- name; fontSize <- size
                | _ -> ()
            | TOperator "g" when i >= 1 ->
                match tokens.[i-1] with TNumber a -> ink <- fmt a + " g" | _ -> ()
            | TOperator "rg" when i >= 3 ->
                match tokens.[i-3], tokens.[i-2], tokens.[i-1] with
                | TNumber a, TNumber b, TNumber c -> ink <- String.concat " " [fmt a; fmt b; fmt c; "rg"]
                | _ -> ()
            | TOperator "k" when i >= 4 ->
                match tokens.[i-4], tokens.[i-3], tokens.[i-2], tokens.[i-1] with
                | TNumber a, TNumber b, TNumber c, TNumber d -> ink <- String.concat " " [fmt a; fmt b; fmt c; fmt d; "k"]
                | _ -> ()
            | _ -> ()
        fontName, fontSize, ink
    (match kind with
     | Some (PdfName "Tx") ->
        let fontName, specifiedSize, daColor = parseDefaultAppearance ()
        let raw = stringValue (field "V") |> fun text -> text.Replace("\r\n", "\n").Replace('\r', '\n')
        let raw = if ff &&& (1L <<< 13) <> 0L then String('*', raw.Length) else raw
        let multiline = ff &&& (1L <<< 12) <> 0L
        let comb = ff &&& (1L <<< 24) <> 0L && not multiline && ff &&& (1L <<< 13) = 0L
        let maxLen = field "MaxLen" |> integer |> int
        let raw = if maxLen > 0 && raw.Length > maxLen then raw.Substring(0, maxLen) else raw
        let raw = if multiline then raw else raw.Replace('\n', ' ')
        let font =
            Pdf.dictGet doc "Font" resources |> Option.bind (Pdf.dictGet doc fontName)
        if fontName <> "" && font.IsSome then
            let info = Fonts.resolve opts.CMaps doc resources fontName
            let encode = Fonts.textEncoder info
            // No universal replacement byte exists for a PDF font's private encoding.
            let bytes text = encode text |> Option.defaultValue [||]
            let measure text =
                Fonts.sourceCodes info (bytes text)
                |> Array.sumBy (fun (code, data) ->
                    float (Fonts.charWidth info (if info.IsComposite then Fonts.cid info code data.Length else code)) / 1000.0)
            let margin = max 2.0 (border + 1.0)
            let available = max 0.0 (w - 2.0 * margin)
            let lines = ResizeArray<string>()
            if multiline then
                for paragraph in raw.Split('\n') do
                    let line = System.Text.StringBuilder()
                    let mutable lineWidth = 0.0
                    let mutable i = 0
                    while i < paragraph.Length do
                        let glyphLength =
                            if Char.IsHighSurrogate paragraph.[i] && i + 1 < paragraph.Length
                               && Char.IsLowSurrogate paragraph.[i+1] then 2 else 1
                        let glyph = paragraph.Substring(i, glyphLength)
                        let advance = measure glyph
                        if line.Length > 0 && (lineWidth + advance) * (if specifiedSize > 0.0 then specifiedSize else 12.0) > available then
                            lines.Add(line.ToString())
                            line.Clear() |> ignore
                            lineWidth <- 0.0
                        line.Append(glyph) |> ignore
                        lineWidth <- lineWidth + advance
                        i <- i + glyphLength
                    lines.Add(line.ToString())
            else lines.Add raw
            let maxWidth =
                if comb && maxLen > 0 then raw |> Seq.map (string >> measure) |> Seq.fold max 0.0
                else lines |> Seq.map measure |> Seq.fold max 0.0
            let size =
                if specifiedSize > 0.0 then specifiedSize
                else
                    let fitWidth =
                        if maxWidth <= 0.0 then 12.0
                        elif comb && maxLen > 0 then max 0.0 (w / float maxLen - 2.0) / maxWidth
                        else available / maxWidth
                    min 12.0 (min fitWidth (max 1.0 ((h - 2.0 * margin) / (if multiline then float lines.Count * 1.2 else 1.0))))
                    |> max 1.0
            let lineHeight = size * 1.2
            let align = field "Q" |> integer
            let quote (data: byte[]) =
                "(" + (data |> Array.map (fun b -> "\\" + Convert.ToString(int b, 8).PadLeft(3, '0')) |> String.concat "") + ")"
            if available > 0.0 && h > 2.0 * margin
               && (lines |> Seq.forall (fun line -> (encode line).IsSome)) then
                emit paint ["q"; fmt margin; fmt margin; fmt available; fmt (h-2.0*margin); "re W n"]
                emit paint ["BT"; daColor; "/" + fontName; fmt size; "Tf"]
                if comb && maxLen > 0 then
                    let cell = w / float maxLen
                    let offset = if align = 1L then float (maxLen - raw.Length) / 2.0
                                 elif align = 2L then float (maxLen - raw.Length) else 0.0
                    for i in 0 .. min (raw.Length - 1) (maxLen - 1) do
                        let ch = string raw.[i]
                        let x = (float i + offset) * cell + (cell - measure ch * size) / 2.0
                        emit paint ["1 0 0 1"; fmt x; fmt ((h-size)/2.0); "Tm"; quote (bytes ch); "Tj"]
                else
                    for i in 0 .. lines.Count - 1 do
                        let line = lines.[i]
                        let lineWidth = measure line * size
                        let x = if align = 1L then (w-lineWidth)/2.0 elif align = 2L then w-margin-lineWidth else margin
                        let y = if multiline then h-margin-size-float i * lineHeight else (h-size)/2.0
                        emit paint ["1 0 0 1"; fmt x; fmt y; "Tm"; quote (bytes line); "Tj"]
                emit paint ["ET Q"]
            Some resources
        else None
     | Some (PdfName "Btn") when not push ->
        let size = min w h
        if roundBorder then
            let radius = size * 0.40
            let cx, cy = w / 2.0, h / 2.0
            let circle r =
                let k = 0.5522847498 * r
                emit paint [fmt (cx+r); fmt cy; "m"; fmt (cx+r); fmt (cy+k); fmt (cx+k); fmt (cy+r); fmt cx; fmt (cy+r); "c"]
                emit paint [fmt (cx-k); fmt (cy+r); fmt (cx-r); fmt (cy+k); fmt (cx-r); fmt cy; "c"]
                emit paint [fmt (cx-r); fmt (cy-k); fmt (cx-k); fmt (cy-r); fmt cx; fmt (cy-r); "c"]
                emit paint [fmt (cx+k); fmt (cy-r); fmt (cx+r); fmt (cy-k); fmt (cx+r); fmt cy; "c h"]
            if border > 0.0 then
                stroke |> Option.iter (fun strokeColor ->
                    emit paint [strokeColor; fmt border; "w"]
                    circle radius
                    emit paint ["S"])
            if selected then
                let _, _, ink = parseDefaultAppearance ()
                emit paint [ink]
                circle (radius * 0.55)
                emit paint ["f"]
        elif selected then
            match caption with
            | Some text when text <> "" ->
                let fontName, fontSize, ink = parseDefaultAppearance ()
                let font =
                    Pdf.dictGet doc "Font" resources |> Option.bind (Pdf.dictGet doc fontName)
                let baseFont = font |> Option.bind (get "BaseFont")
                let markerInk = ink.Replace(" rg", " RG").Replace(" g", " G").Replace(" k", " K")
                let thickness = max 1.0 (size * 0.09)
                match baseFont, text with
                // Annex D.6: ZapfDingbats 3/4 are ticks, 5..8 are crosses.
                | Some (PdfName "ZapfDingbats"), ("5" | "6" | "7" | "8") ->
                    emit paint [markerInk; fmt thickness; "w"]
                    emit paint [fmt (w*0.22); fmt (h*0.22); "m"; fmt (w*0.78); fmt (h*0.78); "l S"]
                    emit paint [fmt (w*0.22); fmt (h*0.78); "m"; fmt (w*0.78); fmt (h*0.22); "l S"]
                | Some (PdfName "ZapfDingbats"), ("3" | "4") ->
                    emit paint [markerInk; fmt thickness; "w"]
                    emit paint [fmt (w*0.18); fmt (h*0.5); "m"; fmt (w*0.42); fmt (h*0.24); "l"; fmt (w*0.82); fmt (h*0.77); "l S"]
                | _ ->
                    font |> Option.iter (fun _ ->
                        let info = Fonts.resolve opts.CMaps doc resources fontName
                        (Fonts.textEncoder info) text
                        |> Option.iter (fun source ->
                            let advance =
                                Fonts.sourceCodes info source
                                |> Array.sumBy (fun (code, bytes) ->
                                    Fonts.charWidth info (if info.IsComposite then Fonts.cid info code bytes.Length else code) / 1000.0)
                            let size =
                                if fontSize > 0.0 then fontSize
                                elif advance > 0.0 then min (h*0.7) (w*0.8/advance)
                                else h*0.7
                            let quote =
                                "(" + (source |> Array.map (fun b -> "\\" + Convert.ToString(int b, 8).PadLeft(3, '0')) |> String.concat "") + ")"
                            emit paint ["BT"; ink; "/" + fontName; fmt size; "Tf"]
                            emit paint ["1 0 0 1"; fmt ((w-advance*size)/2.0); fmt ((h-size)/2.0); "Tm"; quote; "Tj ET"]))
            | Some _ -> ()
            | None ->
                let _, _, ink = parseDefaultAppearance ()
                let markerInk = ink.Replace(" rg", " RG").Replace(" g", " G").Replace(" k", " K")
                emit paint [markerInk; fmt (max 1.0 (size * 0.09)); "w"]
                emit paint [fmt (w*0.18); fmt (h*0.5); "m"; fmt (w*0.42); fmt (h*0.24); "l"; fmt (w*0.82); fmt (h*0.77); "l S"]
        Some resources
     | _ -> None)
    |> Option.map (fun formResources ->
        let data = latin1Bytes (paint.ToString())
        let dictionary =
            Map.ofList ["BBox", PdfArray [PdfInt 0L; PdfInt 0L; PdfReal w; PdfReal h]
                        "Matrix", PdfArray (List.map PdfReal matrix)
                        "Resources", formResources]
        PdfStream (dictionary, data))

let private appearanceInvocation = "/Appearance Do"B

let private renderAnnotations opts doc page resources working viewport scale =
    let commands = ResizeArray<Cmd>()
    let boxes = ResizeArray<CharBox>()
    match Pdf.dictGet doc "Annots" page |> Option.map (Pdf.deref doc) with
    | Some (PdfArray annotations) ->
        for annotation in annotations do
            let flags =
                match Pdf.dictGet doc "F" annotation |> Option.map (Pdf.deref doc) with
                | Some (PdfInt n) -> n
                | _ -> 0L
            // Invisible, Hidden and NoView annotations do not appear on screen.
            if flags &&& 35L = 0L then
                let rect = Pdf.dictGet doc "Rect" annotation |> Option.bind (annotationBox doc)
                let suppliedAppearance =
                    Pdf.dictGet doc "AP" annotation
                    |> Option.bind (Pdf.dictGet doc "N")
                    |> Option.bind (fun normal ->
                        match Pdf.deref doc normal with
                        | PdfStream _ -> Some normal
                        | PdfDict states ->
                            match Pdf.dictGet doc "AS" annotation |> Option.map (Pdf.deref doc) with
                            | Some (PdfName state) -> Map.tryFind state states
                            | _ when states.Count = 1 -> states |> Map.toSeq |> Seq.head |> snd |> Some
                            | _ -> None
                        | _ -> None)
                    |> Option.filter (fun value ->
                        match Pdf.deref doc value with
                        | PdfStream _ -> Pdf.dictGet doc "BBox" value |> Option.bind (annotationBox doc) |> Option.isSome
                        | _ -> false)
                let appearance =
                    match suppliedAppearance, rect, Pdf.dictGet doc "Subtype" annotation |> Option.map (Pdf.deref doc) with
                    | Some existing, _, _ -> Some existing
                    | None, Some rect, Some (PdfName "Widget") ->
                        widgetAppearance opts doc (Pdf.dictGet doc "AcroForm" doc.Catalog) annotation rect
                    | _ -> None
                match appearance |> Option.map (Pdf.deref doc), rect with
                | Some (PdfStream (dictionary, data) as appearance), Some (x0, y0, x1, y1) ->
                    match Pdf.dictGet doc "BBox" appearance |> Option.bind (annotationBox doc) with
                    | Some (bx0, by0, bx1, by1) ->
                        let matrix = appearanceMatrix doc appearance
                        let corners =
                            [| vec2 bx0 by0; vec2 bx1 by0; vec2 bx0 by1; vec2 bx1 by1 |]
                            |> Array.map (Matrix3x2.transformVec2 matrix)
                        let minX = corners |> Array.minBy (fun p -> p.X) |> fun p -> float p.X
                        let minY = corners |> Array.minBy (fun p -> p.Y) |> fun p -> float p.Y
                        let maxX = corners |> Array.maxBy (fun p -> p.X) |> fun p -> float p.X
                        let maxY = corners |> Array.maxBy (fun p -> p.Y) |> fun p -> float p.Y
                        if maxX > minX && maxY > minY then
                            let placement =
                                Matrix3x2.multiply (Matrix3x2.translate (-minX) (-minY))
                                    (Matrix3x2.multiply (Matrix3x2.scale ((x1 - x0) / (maxX - minX)) ((y1 - y0) / (maxY - minY)))
                                        (Matrix3x2.translate x0 y0))
                            let appearanceResources =
                                Pdf.dictGet doc "Resources" appearance |> Option.defaultValue resources
                            // Reuse Form BBox clipping, Matrix and transparency semantics.
                            let form =
                                PdfStream (dictionary |> Map.add "Subtype" (PdfName "Form")
                                                      |> Map.add "Resources" appearanceResources, data)
                            let invocationResources =
                                PdfDict (Map.ofList ["XObject", PdfDict (Map.ofList ["Appearance", form])])
                            let painted, chars =
                                renderContent opts doc invocationResources appearanceInvocation
                                    { defaultState with CTM = placement } working viewport scale 0
                            let rect =
                                [ MoveTo(vec2 x0 y0); LineTo(vec2 x1 y0)
                                  LineTo(vec2 x1 y1); LineTo(vec2 x0 y1); Close ]
                            commands.Add(Clip(rect, NonZero, painted))
                            boxes.AddRange chars
                    | _ -> ()
                | _ -> ()
    | _ -> ()
    commands |> Seq.toList, boxes.ToArray()

let renderPage (opts: RenderOptions) (doc: PdfDocument) (pageNum: int) (scale: float) : (Cmd list * float * float * CharBox[]) option =
    match Pdf.getPage doc pageNum with
    | None -> None
    | Some pageRef ->
        let pageObj = Pdf.deref doc pageRef
        match PageGeometry.tryGet doc pageObj with
        | None -> None
        | Some viewport ->
            let pageW = viewport.Width * scale
            let pageH = viewport.Height * scale
            let pageTransform = Matrix3x2.multiply viewport.Transform (Matrix3x2.scale scale scale)
            let resources = Pdf.getPageInheritable doc pageObj "Resources" |> Option.defaultValue (PdfDict Map.empty)
            let contentData = PageOps.getPageContents doc pageObj
            let pageGroupDict =
                Pdf.getPageInheritable doc pageObj "Group"
                |> Option.filter (fun group -> Pdf.dictGet doc "S" group |> Option.map (Pdf.deref doc) = Some (PdfName "Transparency"))
            let pageWorking = pageGroupDict |> Option.bind (groupWorkingSpace doc resources)
            let pageCommands, pageChars =
                if contentData.Length = 0 then [], [||]
                else renderContent opts doc resources contentData defaultState pageWorking viewport scale 0
            let annotationCommands, annotationChars =
                renderAnnotations opts doc pageObj resources pageWorking viewport scale
            let x0, y0, x1, y1 = viewport.Bounds
            let pageClip =
                [ MoveTo(vec2 x0 y0); LineTo(vec2 x1 y0)
                  LineTo(vec2 x1 y1); LineTo(vec2 x0 y1); Close ]
            let pageKnockout =
                pageGroupDict |> Option.bind (Pdf.dictGet doc "K") |> Option.map (Pdf.deref doc) = Some (PdfBool true)
            // A page is isolated from the caller's backdrop.
            let pageGroup =
                managedGroup PdfColor.displaySpace pageWorking true pageKnockout 1.0 Normal None
                    (pageCommands @ annotationCommands)
            let scaledCmds = [Group(pageTransform, 1.0, [Clip(pageClip, NonZero, [pageGroup])])]
            let scaledCharBoxes =
                Array.append pageChars annotationChars |> Array.map (fun cb ->
                    let p0 = Matrix3x2.transformVec2 pageTransform (vec2 cb.X cb.Y)
                    let p1 = Matrix3x2.transformVec2 pageTransform (vec2 (cb.X + cb.W) (cb.Y + cb.H))
                    { cb with X = float (min p0.X p1.X); Y = float (min p0.Y p1.Y)
                              W = float (abs (p1.X - p0.X)); H = float (abs (p1.Y - p0.Y)) })
            Some (scaledCmds, pageW, pageH, scaledCharBoxes)
