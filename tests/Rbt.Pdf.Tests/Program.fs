/// Parser tolerance and page-copy tests on small hand-built PDFs.
/// Run: dotnet run --project tests/Rbt.Pdf.Tests
module Rbt.Pdf.Tests.Program

open System
open System.Text
open Rbt.Pdf
open Rbt.Pdf.Types
open Rbt.Renderer
open Rbt.Pdf.Render.Render

let latin1 (s: string) = Encoding.Latin1.GetBytes s
let text (b: byte[]) = Encoding.Latin1.GetString b

/// A minimal PDF from (object number, body) pairs: header, objects, an xref
/// table, trailer and startxref, with correct offsets unless `layout` says
/// otherwise.
type Layout =
    { Prefix: string                 // bytes before %PDF (junk)
      OffsetsIncludePrefix: bool     // xref offsets counted from file start
      TrailerOnXRefLine: bool        // "trailer <<...>>" on one line
      OmitXRef: bool                 // no xref, trailer or startxref at all
      GarbleOffsets: bool            // every xref offset wrong
      StartXRefBogus: bool }         // startxref points into an object

let normal =
    { Prefix = ""; OffsetsIncludePrefix = false; TrailerOnXRefLine = false
      OmitXRef = false; GarbleOffsets = false; StartXRefBogus = false }

let build (layout: Layout) (objects: (int * string) list) (rootNum: int) =
    let sb = StringBuilder()
    sb.Append("%PDF-1.4\n%\xe2\xe3\xcf\xd3\n") |> ignore
    let offsets = Collections.Generic.Dictionary<int, int>()
    for num, body in objects do
        offsets.[num] <- sb.Length
        sb.Append($"{num} 0 obj\n{body}\nendobj\n") |> ignore
    let size = (objects |> List.map fst |> List.max) + 1
    if not layout.OmitXRef then
        let xrefPos = sb.Length
        sb.Append($"xref\n0 {size}\n0000000000 65535 f\r\n") |> ignore
        for n in 1 .. size - 1 do
            match offsets.TryGetValue n with
            | true, off ->
                let off =
                    if layout.GarbleOffsets then off + 3
                    elif layout.OffsetsIncludePrefix then off + layout.Prefix.Length
                    else off
                sb.Append(off.ToString("D10")).Append(" 00000 n\r\n") |> ignore
            | _ -> sb.Append("0000000000 00000 f\r\n") |> ignore
        let trailer = $"<</Size {size} /Root {rootNum} 0 R>>"
        if layout.TrailerOnXRefLine then sb.Append($"trailer {trailer}\n\n") |> ignore
        else sb.Append($"trailer\n{trailer}\n") |> ignore
        let startxref =
            if layout.StartXRefBogus then offsets.[rootNum] + 4
            elif layout.OffsetsIncludePrefix then xrefPos + layout.Prefix.Length
            else xrefPos
        sb.Append($"startxref\n{startxref}\n%%%%EOF\n") |> ignore
    Array.append (latin1 layout.Prefix) (latin1 (sb.ToString()))

/// One page with inline content "BT (Hello) Tj ET".
let onePage =
    [ 1, "<</Type /Catalog /Pages 2 0 R>>"
      2, "<</Type /Pages /Kids [3 0 R] /Count 1>>"
      3, "<</Type /Page /Parent 2 0 R /MediaBox [0 0 300 400] /Contents 4 0 R>>"
      4, "<</Length 18>>\nstream\nBT (Hello) Tj ET\n\nendstream" ]

let mutable failures = 0
let check name cond =
    if cond then printfn "ok   %s" name
    else
        failures <- failures + 1
        printfn "FAIL %s" name

let test name (f: unit -> unit) =
    try f ()
    with ex ->
        failures <- failures + 1
        printfn "FAIL %s: %s" name (ex.ToString())

let pageContent (doc: PdfDocument) (n: int) =
    let page = Pdf.getPageRefs doc |> List.item (n - 1)
    PageOps.getPageContents doc page |> text

let assertHello name (bytes: byte[]) =
    let doc = Pdf.openData bytes
    check $"{name}: one page" (Pdf.getPageRefs doc |> List.length = 1)
    check $"{name}: content" ((pageContent doc 1).Contains "BT (Hello) Tj ET")
    // And it survives a round trip through merge.
    let merged = Operations.merge [ doc ] |> Operations.toBytes |> Pdf.openData
    check $"{name}: merged content" ((pageContent merged 1).Contains "BT (Hello) Tj ET")

let renderFixture pageEntries parentEntries resources extraObjects content scale =
    let objects =
        [ yield 1, "<</Type /Catalog /Pages 2 0 R>>"
          yield 2, $"<</Type /Pages /Kids [3 0 R] /Count 1 {parentEntries}>>"
          yield 3, $"<</Type /Page /Parent 2 0 R {pageEntries} /Resources <<{resources}>> /Contents 4 0 R>>"
          yield 4, $"<</Length {String.length content}>>\nstream\n{content}\nendstream"
          yield! extraObjects ]
    let doc = Pdf.openData (build normal objects 1)
    renderPage (RenderOptions.native ()) doc 1 scale |> Option.get

let renderContent content =
    let cmds, _, _, _ = renderFixture "/MediaBox [0 0 300 400]" "" "" [] content 1.0
    cmds

let rasterize (cmds, width, height, _) =
    let backend = Raster.create ()
    backend.Init (int (ceil width)) (int (ceil height))
    backend.Submit (Clear Color.white :: cmds)
    backend.Finish () |> Framebuffer.tryGetArray |> Option.get

let near expected actual = abs (expected - actual) < 0.0001

let testFont = "/Font <</F1 <</Type /Font /Subtype /Type1 /BaseFont /Helvetica /FirstChar 65 /LastChar 66 /Widths [500 250]>>>>"

// Observe page-space geometry and painting order, independent of grouping.
let rec drawingEvents matrix clipped commands =
    [ for cmd in commands do
        match cmd with
        | Group (m, _, children) ->
            yield! drawingEvents (Matrix3x2.multiply m matrix) clipped children
        | Clip (_, _, children) -> yield! drawingEvents matrix true children
        | TransparencyGroup (_, _, _, _, _, children)
        | ColorManagedGroup (_, _, _, _, _, _, children) ->
            yield! drawingEvents matrix clipped children
        | WithShape (_, children) -> yield! drawingEvents matrix clipped children
        | FillPath (segments, _, _) | StrokePath (segments, _) ->
            yield "path", clipped, [ for seg in segments do
                                        match seg with
                                        | MoveTo p | LineTo p -> yield Matrix3x2.transformVec2 matrix p
                                        | CubicTo (a, b, c) ->
                                            yield Matrix3x2.transformVec2 matrix a
                                            yield Matrix3x2.transformVec2 matrix b
                                            yield Matrix3x2.transformVec2 matrix c
                                        | _ -> () ]
        | DrawGlyphs (origin, _, _) ->
            yield "text", clipped, [Matrix3x2.transformVec2 matrix origin]
        | _ -> () ]

let renderPixels content =
    let backend = Raster.create ()
    backend.Init 300 400
    backend.Submit (Clear Color.white :: renderContent content)
    backend.Finish () |> Framebuffer.tryGetArray |> Option.get

[<EntryPoint>]
let main _ =
    test "baseline" (fun () -> assertHello "baseline" (build normal onePage 1))

    test "JPX odd-phase multilevel tiles reconstruct pixels" (fun () ->
        // Generated from the formula below using FFmpeg jpeg2000, dwt53,
        // 32x24 tiles in a 64x48 image (six decomposition levels).
        use stream = Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("jpx-tiled.base64")
        use reader = new IO.StreamReader(stream)
        let data = Convert.FromBase64String(reader.ReadToEnd())
        match Rbt.Pdf.Render.JpxDecode.decode data with
        | Some (64, 48, 3, pixels) ->
            let mutable matches = true
            for y in 0 .. 47 do
                for x in 0 .. 63 do
                    let i = (y * 64 + x) * 3
                    matches <- matches
                        && int pixels.[i] = (x * 4 + y * 3) % 256
                        && int pixels.[i+1] = (y * 5 + (x / 8 % 2) * 100) % 256
                        && int pixels.[i+2] = ((x / 8 + y / 8) % 2) * 255
            check "all tiled JPX RGB samples match source pixels" matches
        | _ -> check "tiled JPX dimensions and components" false)

    test "JPX tiled irreversible color transform reconstructs pixels" (fun () ->
        // OpenJPEG: same 64x48 source formula, 32x24 tiles, four resolution
        // levels, irreversible 9/7 and MCT. Expected pixels decoded by FFmpeg.
        use stream = Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("jpx-tiled-mct.base64")
        use reader = new IO.StreamReader(stream)
        let data = Convert.FromBase64String(reader.ReadToEnd())
        match Rbt.Pdf.Render.JpxDecode.decode data with
        | Some (64, 48, 3, pixels) ->
            let digest = Security.Cryptography.SHA256.HashData pixels |> Convert.ToHexStringLower
            check "tiled irreversible MCT pixels match independent decoder"
                (digest = "a9070589b0c09b9a70cc23f04447295ddd863f2bef6509994f898a73c96fe7e9")
        | _ -> check "irreversible MCT JPX dimensions and components" false)

    test "closed stroke joins differ from coincident open endpoints" (fun () ->
        let closed = renderPixels "8 w 10 10 20 20 re S"
        let openPath = renderPixels "8 w 10 10 m 30 10 l 30 30 l 10 30 l 10 10 l S"
        check "explicit Close joins the starting corner" (closed.[393*300+6] = 0xff000000u)
        check "coincident open endpoints retain butt caps" (openPath.[393*300+6] = 0xffffffffu)
        check "closed miter stays within stroke bounds" (closed.[395*300+5] = 0xffffffffu))

    test "native hairlines preserve device width without clamping positive strokes" (fun () ->
        let stroke width x0 x1 =
            StrokePath ([MoveTo (vec2 x0 58.0); LineTo (vec2 x1 58.0)],
                { Width = width; Cap = Butt; Join = Miter; Paint = Solid Color.black
                  DashArray = []; DashOffset = 0.0<px>; MiterLimit = 10.0 })
        let backend = Raster.create ()
        backend.Init 64 40
        backend.Submit [Clear Color.white; AntiAlias (4,
            [Group (Matrix3x2.scale 0.35 0.35, 1.0,
                [stroke 0.8<px> 20.0 70.0; stroke 0.0<px> 100.0 150.0])])]
        let pixels = backend.Finish () |> Framebuffer.tryGetArray |> Option.get
        let coverage x = [19..21] |> List.sumBy (fun y -> 255 - int (Pixel.r pixels.[y*64+x]))
        let thin, hairline = coverage 15, coverage 42
        check "zero-width stroke covers one device pixel under a shrinking transform" (hairline >= 240 && hairline <= 270)
        check "positive fractional width retains less coverage than a hairline" (thin > 0 && thin < hairline))

    test "nested transparency groups retain their working or knockout backdrop" (fun () ->
        let square = [MoveTo (vec2 0.0 0.0); LineTo (vec2 16.0 0.0); LineTo (vec2 16.0 16.0); LineTo (vec2 0.0 16.0); Close]
        let paint (r,g,b) = FillPath (square, Solid (Color.rgb (byte r) (byte g) (byte b)), NonZero)
        let draw command =
            let backend = Raster.create ()
            backend.Init 16 16
            backend.Submit [Clear Color.white; command]
            let pixels = backend.Finish () |> Framebuffer.tryGetArray |> Option.get
            pixels.[8*16+8]
        let assertRgb label (r,g,b) pixel =
            check label (abs (int (Pixel.r pixel)-r) <= 2 && abs (int (Pixel.g pixel)-g) <= 2
                         && abs (int (Pixel.b pixel)-b) <= 2 && Pixel.a pixel = 255uy)
        for isolated, knockout, expected in
            [true,false,(180,140,10); false,false,(180,140,10)
             true,true,(20,180,30); false,true,(235,75,225)] do
            for childIsolated in [false;true] do
                let nested = TransparencyGroup (childIsolated, false, 1.0, Difference, None, [paint (20,180,30)])
                let wrapped = AntiAlias (4, [SaveRestore [Group (Matrix3x2.identity, 1.0, [Clip (square, NonZero, [nested])])]])
                draw (TransparencyGroup (isolated, knockout, 1.0, Normal, None, [paint (200,40,20); wrapped]))
                |> assertRgb $"nested boundary mode: isolated={isolated}, knockout={knockout}, child isolated={childIsolated}" expected
        let internalBlend = TransparencyGroup (false, false, 1.0, Normal, None, [Blend (Difference, [paint (200,180,10)])])
        draw (TransparencyGroup (true, false, 1.0, Normal, None, [paint (10,20,200); internalBlend]))
        |> assertRgb "nested internal blend sees the parent's painted backdrop" (190,160,190)
        let halfMask = [FillPath (square, Solid (Color.rgba 255uy 255uy 255uy 128uy), NonZero)]
        let masked = TransparencyGroup (false, false, 0.5, Difference, Some halfMask, [paint (200,180,10)])
        draw (TransparencyGroup (false, false, 1.0, Normal, None, [paint (10,20,200); masked]))
        |> assertRgb "nested opacity and mask each apply once" (55,55,198))

    test "packed blending preserves opaque alpha and rounds by 255" (fun () ->
        let destination = Pixel.pack 21uy 90uy 201uy 255uy
        let mutable exact = true
        for alpha in 0 .. 255 do
            let r, g, b = alpha, alpha/2, alpha/3
            let pixel = Pixel.blend (Pixel.pack (byte b) (byte g) (byte r) (byte alpha)) destination
            let expected source backdrop = source + (backdrop * (255-alpha) + 127)/255
            exact <- exact && Pixel.a pixel = 255uy
                     && int (Pixel.r pixel) = expected r 201
                     && int (Pixel.g pixel) = expected g 90
                     && int (Pixel.b pixel) = expected b 21
        check "all source alpha values match scalar source-over" exact)

    test "premultiplication preserves constant color through source-over" (fun () ->
        let mutable exact = true
        for channel in 0 .. 255 do
            let color = Color.rgb (byte channel) (byte (255-channel)) (byte ((79*channel)%256))
            let destination = Pixel.fromColorPremul color
            for alpha in 0 .. 255 do
                let source = Pixel.fromColorPremul { color with A = byte alpha }
                exact <- exact && Pixel.blend source destination = destination
        check "every component and alpha preserve unchanged source-over color" exact)

    test "managed groups convert contributions before parent compositing" (fun () ->
        let square = [MoveTo (vec2 0.0 0.0); LineTo (vec2 16.0 0.0); LineTo (vec2 16.0 16.0); LineTo (vec2 0.0 16.0); Close]
        let paint color = FillPath (square, Solid color, NonZero)
        let conversion =
            { ToGroup = fun r g b -> struct(r*r, g*g, b*b)
              ToParent = fun r g b -> struct(sqrt r, sqrt g, sqrt b) }
        let children = [paint (Color.rgb 255uy 0uy 0uy); paint (Color.rgba 0uy 0uy 255uy 128uy)]
        let draw command =
            let backend = Raster.create ()
            backend.Init 16 16
            backend.Submit [Clear (Color.rgb 0uy 255uy 0uy); command]
            let pixels = backend.Finish () |> Framebuffer.tryGetArray |> Option.get
            pixels.[8*16+8]
        let assertRgb label (r,g,b) pixel =
            check label (abs (int (Pixel.r pixel)-r) <= 2 && abs (int (Pixel.g pixel)-g) <= 2
                         && abs (int (Pixel.b pixel)-b) <= 2 && Pixel.a pixel = 255uy)
        for isolated in [false;true] do
            for knockout in [false;true] do
                let expected = if knockout then (0,127,128) else (180,0,181)
                draw (ColorManagedGroup(conversion, isolated, knockout, 1.0, Normal, None, children))
                |> assertRgb $"managed isolation={isolated}, knockout={knockout}" expected
        draw (ColorManagedGroup(conversion, true, false, 0.5, Normal, None, children))
        |> assertRgb "group opacity composites encoded parent components" (90,128,90)
        draw (ColorManagedGroup(conversion, true, false, 0.5, Multiply, None, children))
        |> assertRgb "boundary Multiply uses parent space" (0,128,0)
        let mask = [paint (Color.rgba 255uy 255uy 255uy 128uy)]
        draw (ColorManagedGroup(conversion, true, false, 0.5, Normal, Some mask, children))
        |> assertRgb "managed mask and opacity apply once" (45,191,45))

    test "DeviceGray soft masks retain alpha and luminosity semantics" (fun () ->
        for subtype, paint in ["Alpha", "/Half gs 1 g"; "Luminosity", "0.5 g"] do
            let content = paint + " 0 0 100 100 re f"
            let mask =
                $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /CS /DeviceGray /I true>> "
                + $"/Resources <</ExtGState <</Half <</ca 0.5>>>>>> /Length {content.Length}>>\nstream\n{content}\nendstream"
            let resources = $"/ExtGState <</M <</SMask <</S /{subtype} /G 5 0 R>>>>>>"
            let pixels = renderFixture "/MediaBox [0 0 100 100]" "" resources [5,mask]
                             "1 0 0 rg /M gs 0 0 100 100 re f" 1.0 |> rasterize
            let pixel = pixels.[50*100+50]
            check $"{subtype} DeviceGray mask composites half red over opaque white"
                (Pixel.a pixel = 255uy && Pixel.r pixel = 255uy
                 && abs (int (Pixel.g pixel)-128) <= 2 && abs (int (Pixel.b pixel)-128) <= 2))

    test "PDF AIS defaults to opacity, inherits into groups, and is saved by q/Q" (fun () ->
        let content = "0 0 1 rg 0 0 100 100 re f /Half gs 1 0 0 rg 0 0 100 100 re f"
        let form = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true /K true>> /Resources <</ExtGState <</Half <</ca 0.5>>>>>> /Length {content.Length}>>\nstream\n{content}\nendstream"
        let resources = "/XObject <</F 5 0 R>> /ExtGState <</Shape <</AIS 7 0 R>> /Opacity <</AIS false>>>>"
        let sample prefix =
            renderFixture "/MediaBox [0 0 100 100]" "" resources [5,form; 7,"true"]
                (prefix + " /F Do") 1.0 |> rasterize |> fun pixels -> pixels.[50*100+50]
        let defaultPixel = sample ""
        let shapePixel = sample "/Shape gs"
        let restoredPixel = sample "q /Shape gs Q"
        let explicitFalse = sample "/Shape gs /Opacity gs"
        check "default alpha is opacity: knocked-out blue does not survive" (Pixel.r defaultPixel > 245uy && Pixel.g defaultPixel > 110uy && Pixel.b defaultPixel > 110uy)
        check "indirect AIS true preserves blue underneath partial shape" (Pixel.r shapePixel < 200uy && Pixel.g shapePixel < 20uy && Pixel.b shapePixel > 100uy)
        check "Q restores default AIS" (restoredPixel = defaultPixel)
        check "explicit AIS false overrides inherited true" (explicitFalse = defaultPixel))

    test "PDF AIS applies ca and CA to their own fill and stroke coverage, including zero" (fun () ->
        let content = "0 0 1 rg 0 0 100 100 re f /Alphas gs 1 0 0 rg 1 0 0 RG 0 0 100 100 re f 10 w 15 15 70 70 re S"
        let form = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true /K true>> /Resources <</ExtGState <</Alphas <</ca 0 /CA 0.5>>>>>> /Length {content.Length}>>\nstream\n{content}\nendstream"
        let resources = "/XObject <</F 5 0 R>> /ExtGState <</Shape <</AIS true>>>>"
        let paint prefix =
            renderFixture "/MediaBox [0 0 100 100]" "" resources [5,form] (prefix + " /F Do") 1.0 |> rasterize
        let defaultPixels = paint ""
        let shapedPixels = paint "/Shape gs"
        let center (p: uint32[]) = p.[50*100+50]
        let stroke (p: uint32[]) = p.[50*100+15]
        check "zero ca still knocks out prior color by default" (Pixel.r (center defaultPixels) > 245uy)
        check "zero ca preserves earlier opaque shape when AIS true" (Pixel.r (center shapedPixels) < 20uy && Pixel.b (center shapedPixels) > 245uy)
        check "CA changes stroke knockout without changing its painted opacity"
            (int (Pixel.r (stroke defaultPixels)) - int (Pixel.r (stroke shapedPixels)) > 40))

    test "PDF soft mask affects shape only under AIS, without double applying color opacity" (fun () ->
        let content = "0 0 1 rg 0 0 100 100 re f /Masked gs 1 0 0 rg 0 0 100 100 re f"
        let form = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true /K true>> /Resources <</ExtGState <</Masked <</SMask <</S /Alpha /G 6 0 R>>>>>>>> /Length {content.Length}>>\nstream\n{content}\nendstream"
        let maskContent = "/Half gs 1 g 0 0 100 100 re f"
        let mask = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true>> /Resources <</ExtGState <</Half <</ca 0.5>>>>>> /Length {maskContent.Length}>>\nstream\n{maskContent}\nendstream"
        let resources = "/XObject <</F 5 0 R>> /ExtGState <</Shape <</AIS true>>>>"
        let sample prefix =
            renderFixture "/MediaBox [0 0 100 100]" "" resources [5,form; 6,mask]
                (prefix + " /F Do") 1.0 |> rasterize |> fun pixels -> pixels.[50*100+50]
        let opacity, shape = sample "", sample "/Shape gs"
        check "opacity mask knocks out the full red geometry" (Pixel.r opacity > 245uy && Pixel.g opacity > 110uy)
        check "AIS makes the mask partial shape but color still has half opacity"
            (Pixel.r shape < 200uy && Pixel.g shape < 20uy && Pixel.b shape > 100uy))

    test "nested Form shape does not promote internal Form opacity into caller AIS" (fun () ->
        let innerContent = "/Inner gs 0 1 0 rg 0 0 100 100 re f"
        let inner = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true>> /Resources <</ExtGState <</Inner <</ca 0.5 /AIS false>>>>>> /Length {innerContent.Length}>>\nstream\n{innerContent}\nendstream"
        let outerContent = "0 0 1 rg 0 0 100 100 re f /F Do"
        let outer = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true /K true>> /Resources <</XObject <</F 6 0 R>>>> /Length {outerContent.Length}>>\nstream\n{outerContent}\nendstream"
        let resources = "/XObject <</Outer 5 0 R>> /ExtGState <</Shape <</AIS true>>>>"
        let pixels =
            renderFixture "/MediaBox [0 0 100 100]" "" resources [5,outer;6,inner]
                "/Shape gs /Outer Do" 1.0 |> rasterize
        let p = pixels.[50*100+50]
        check "inner half-opacity green retains full geometric knockout shape"
            (Pixel.r p > 110uy && Pixel.g p > 245uy && Pixel.b p > 110uy))

    test "zero-opacity nested Form still has intrinsic shape at an AIS boundary" (fun () ->
        let innerContent = "/Invisible gs 0 1 0 rg 0 0 100 100 re f"
        let inner = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true>> /Resources <</ExtGState <</Invisible <</ca 0 /AIS false>>>>>> /Length {innerContent.Length}>>\nstream\n{innerContent}\nendstream"
        let outerContent = "1 0 0 rg 0 0 100 100 re f /Half gs /Nested Do"
        let outer = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true /K true>> /Resources <</XObject <</Nested 6 0 R>> /ExtGState <</Half <</ca 0.5>>>>>> /Length {outerContent.Length}>>\nstream\n{outerContent}\nendstream"
        let resources = "/XObject <</Outer 5 0 R>> /ExtGState <</Shape <</AIS true>>>>"
        let sample prefix =
            renderFixture "/MediaBox [0 0 100 100]" "" resources [5,outer;6,inner]
                (prefix + "/Outer Do") 1.0 |> rasterize |> fun pixels -> pixels.[50*100+50]
        let opacity, shape = sample "", sample "/Shape gs "
        check "default group boundary removes earlier red despite zero painted opacity"
            (Pixel.r opacity > 245uy && Pixel.g opacity > 245uy)
        check "AIS group boundary knocks out only half the previous red"
            (Pixel.r shape > 245uy && Pixel.g shape > 110uy && Pixel.g shape < 190uy && Pixel.b shape > 110uy))

    test "PDF image SMask is AIS-dependent; image masks override the graphics-state mask" (fun () ->
        let rgb = "\xff\x00\x00\xff\x00\x00"
        let image suffix =
            $"<</Type /XObject /Subtype /Image /Width 2 /Height 1 /BitsPerComponent 8 /ColorSpace /DeviceRGB {suffix} /Length 6>>\nstream\n{rgb}\nendstream"
        let soft = "<</Type /XObject /Subtype /Image /Width 2 /Height 1 /BitsPerComponent 8 /ColorSpace /DeviceGray /Length 2>>\nstream\n\x80\x80\nendstream"
        let hard = "<</Type /XObject /Subtype /Image /Width 2 /Height 1 /ImageMask true /BitsPerComponent 1 /Length 1>>\nstream\n\x40\nendstream"
        let zero = "0 0 0 0 re f"
        let maskGroup = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true>> /Length {zero.Length}>>\nstream\n{zero}\nendstream"
        let content = "0 0 1 rg 0 0 100 100 re f /Zero gs q 100 0 0 100 0 0 cm /Im Do Q"
        let form = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true /K true>> /Resources <</XObject <</Im 7 0 R>> /ExtGState <</Zero <</SMask <</S /Alpha /G 10 0 R>>>>>>>> /Length {content.Length}>>\nstream\n{content}\nendstream"
        let resources = "/XObject <</F 5 0 R>> /ExtGState <</Shape <</AIS true>>>>"
        let sample suffix shape =
            renderFixture "/MediaBox [0 0 100 100]" "" resources
                [5,form;7,image suffix;8,soft;9,hard;10,maskGroup]
                ((if shape then "/Shape gs " else "") + "/F Do") 1.0 |> rasterize
        let plain = sample "" false
        let imageSoftOpacity = sample "/SMask 8 0 R" false
        let imageSoftShape = sample "/SMask 8 0 R" true
        let explicit = sample "/Mask 9 0 R" false
        let both = sample "/Mask 9 0 R /SMask 8 0 R" false
        let nonJpxMaskInData = sample "/SMaskInData 1" false
        let left (p: uint32[]) = p.[50*100+25]
        let right (p: uint32[]) = p.[50*100+75]
        check "ordinary image obeys zero graphics-state mask" (Pixel.g (left plain) > 245uy)
        check "SMaskInData is ignored outside JPX and cannot suppress the graphics-state mask"
            (Pixel.g (left nonJpxMaskInData) > 245uy)
        check "image SMask overrides graphics-state mask" (Pixel.r (left imageSoftOpacity) > 245uy && Pixel.g (left imageSoftOpacity) < 200uy)
        check "image SMask changes knockout coverage with AIS" (Pixel.r (left imageSoftShape) < 200uy && Pixel.g (left imageSoftShape) < 20uy)
        check "explicit Mask is geometric and overrides graphics-state SMask"
            (Pixel.r (left explicit) > 245uy && Pixel.g (left explicit) < 20uy &&
             Pixel.r (right explicit) < 20uy && Pixel.b (right explicit) > 245uy)
        check "SMask overrides image Mask, including the Mask's transparent sample"
            (Pixel.r (right both) > 245uy && Pixel.g (right both) < 200uy))

    test "JPX embedded soft mask has precedence and retains AIS shape semantics" (fun () ->
        use stream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("ais-rgba-jpx.base64")
        use reader = new System.IO.StreamReader(stream)
        let bytes = Convert.FromBase64String(reader.ReadToEnd())
        let encoded = text bytes
        let image = $"<</Type /XObject /Subtype /Image /Width 32 /Height 8 /ColorSpace /DeviceRGB /Filter /JPXDecode /SMaskInData 1 /SMask 6 0 R /Mask [0 0 0 0 255 255] /Length {bytes.Length}>>\nstream\n{encoded}\nendstream"
        let zeroImage = "<</Type /XObject /Subtype /Image /Width 1 /Height 1 /ColorSpace /DeviceGray /BitsPerComponent 8 /Length 1>>\nstream\n\x00\nendstream"
        let zeroGroup = "<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true>> /Resources <<>> /Length 0>>\nstream\n\nendstream"
        let content = "1 0 0 rg 0 0 100 100 re f /Alpha gs q 100 0 0 100 0 0 cm /Im Do Q"
        let form = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true /K true>> /Resources <</XObject <</Im 5 0 R>> /ExtGState <</Alpha <</ca 0.5 /SMask <</S /Alpha /G 8 0 R>>>>>>>> /Length {content.Length}>>\nstream\n{content}\nendstream"
        let resources = "/XObject <</F 7 0 R>> /ExtGState <</Shape <</AIS true>>>>"
        let sample prefix =
            renderFixture "/MediaBox [0 0 100 100]" "" resources [5,image;6,zeroImage;7,form;8,zeroGroup]
                (prefix + "/F Do") 1.0 |> rasterize
        let opacity, shape = sample "", sample "/Shape gs "
        let rgbNear (r,g,b) pixel =
            abs(int(Pixel.r pixel)-r) <= 2 && abs(int(Pixel.g pixel)-g) <= 2 && abs(int(Pixel.b pixel)-b) <= 2
        check "zero embedded opacity still knocks out earlier geometry" (rgbNear (255,255,255) opacity.[5025])
        check "embedded alpha overrides image and graphics-state masks" (rgbNear (191,191,255) opacity.[5075])
        check "zero embedded shape retains earlier geometry" (rgbNear (255,0,0) shape.[5025])
        check "embedded shape and ca multiply once" (rgbNear (191,0,64) shape.[5075]))

    test "stencil intrinsic shape does not override the graphics-state soft mask" (fun () ->
        let stencil = "<</Type /XObject /Subtype /Image /Width 2 /Height 1 /ImageMask true /BitsPerComponent 1 /Length 1>>\nstream\n\x40\nendstream"
        let zeroGroup = "<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true>> /Resources <<>> /Length 0>>\nstream\n\nendstream"
        let resources = "/XObject <</Im 5 0 R>> /ExtGState <</Zero <</SMask <</S /Alpha /G 6 0 R>>>>>>"
        let pixels = renderFixture "/MediaBox [0 0 100 100]" "" resources [5,stencil;6,zeroGroup]
                         "0 0 1 rg 0 0 100 100 re f /Zero gs 1 0 0 rg q 100 0 0 100 0 0 cm /Im Do Q" 1.0 |> rasterize
        check "graphics-state zero soft mask suppresses stencil ink"
            (Pixel.r pixels.[5025] = 0uy && Pixel.b pixels.[5025] = 255uy))

    test "inline stencil geometric shape remains distinct from zero paint alpha" (fun () ->
        let content = "0 0 1 rg 0 0 100 100 re f /Zero gs 1 0 0 rg q 100 0 0 100 0 0 cm BI /W 2 /H 1 /IM true /BPC 1 ID \x40 EI Q"
        let form = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true /K true>> /Resources <</ExtGState <</Zero <</ca 0>>>>>> /Length {content.Length}>>\nstream\n{content}\nendstream"
        let resources = "/XObject <</F 5 0 R>> /ExtGState <</Shape <</AIS true>>>>"
        let sample prefix =
            renderFixture "/MediaBox [0 0 100 100]" "" resources [5,form] (prefix + "/F Do") 1.0 |> rasterize
        let defaultPixels, shapedPixels = sample "", sample "/Shape gs "
        let left (p: uint32[]) = p.[50*100+25]
        let right (p: uint32[]) = p.[50*100+75]
        check "zero-alpha stencil knocks out only its inked sample by default"
            (Pixel.r (left defaultPixels) > 245uy && Pixel.r (right defaultPixels) < 20uy)
        check "AIS true zero-alpha stencil cannot knock out prior ink"
            (Pixel.r (left shapedPixels) < 20uy && Pixel.b (left shapedPixels) > 245uy))

    test "pattern cell opacity does not become its intrinsic shape under caller AIS" (fun () ->
        let tileContent = "/Inner gs 1 0 0 rg 0 0 5 10 re f"
        let tile = $"<</Type /Pattern /PatternType 1 /PaintType 1 /TilingType 1 /BBox [0 0 10 10] /XStep 10 /YStep 10 /Resources <</ExtGState <</Inner <</ca 0.5 /AIS false>>>>>> /Length {tileContent.Length}>>\nstream\n{tileContent}\nendstream"
        let formContent = "0 0 1 rg 0 0 100 100 re f /Pattern cs /P scn 0 0 100 100 re f"
        let form = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true /K true>> /Resources <</Pattern <</P 6 0 R>>>> /Length {formContent.Length}>>\nstream\n{formContent}\nendstream"
        let resources = "/XObject <</F 5 0 R>> /ExtGState <</Shape <</AIS true>>>>"
        let pixels =
            renderFixture "/MediaBox [0 0 100 100]" "" resources [5,form;6,tile]
                "/Shape gs /F Do" 1.0 |> rasterize
        let ink, gap = pixels.[50*100+22], pixels.[50*100+27]
        check "half-transparent tile ink knocks out opaque blue with full cell geometry"
            (Pixel.r ink > 245uy && Pixel.g ink > 110uy && Pixel.b ink > 110uy)
        check "unpainted half of tile does not knock out underlying blue"
            (Pixel.r gap < 20uy && Pixel.b gap > 245uy))

    test "shading ca changes geometric coverage only when AIS true" (fun () ->
        let shading = "<</ShadingType 2 /ColorSpace /DeviceRGB /Coords [0 50 100 50] /Function <</FunctionType 2 /Domain [0 1] /C0 [1 0 0] /C1 [1 0 0] /N 1>> /Extend [true true]>>"
        let content = "0 0 1 rg 0 0 100 100 re f /Half gs /Red sh"
        let form = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true /K true>> /Resources <</Shading <</Red 6 0 R>> /ExtGState <</Half <</ca 0.5>>>>>> /Length {content.Length}>>\nstream\n{content}\nendstream"
        let resources = "/XObject <</F 5 0 R>> /ExtGState <</Shape <</AIS true>>>>"
        let sample state =
            renderFixture "/MediaBox [0 0 100 100]" "" resources [5,form;6,shading]
                (state + "/F Do") 1.0 |> rasterize |> fun pixels -> pixels.[50*100+50]
        let opacity, shape = sample "", sample "/Shape gs "
        check "default shading alpha knocks out opaque underpaint" (Pixel.r opacity > 245uy && Pixel.g opacity > 110uy)
        check "AIS shading alpha retains underpaint without duplicating the shader"
            (Pixel.r shape < 200uy && Pixel.g shape < 20uy && Pixel.b shape > 110uy))

    test "PDF AIS text fill uses glyph geometry rather than opacity as knockout shape" (fun () ->
        let content = "0 0 1 rg 0 0 100 100 re f /Half gs 1 0 0 rg BT /F1 80 Tf 1 0 0 1 10 10 Tm (A) Tj ET"
        let form = $"<</Type /XObject /Subtype /Form /BBox [0 0 100 100] /Group <</S /Transparency /I true /K true>> /Resources <<{testFont} /ExtGState <</Half <</ca 0.5>>>>>> /Length {content.Length}>>\nstream\n{content}\nendstream"
        let resources = "/XObject <</F 5 0 R>> /ExtGState <</Shape <</AIS true>>>>"
        let sample prefix =
            renderFixture "/MediaBox [0 0 100 100]" "" resources [5,form]
                (prefix + "/F Do") 1.0 |> rasterize
        let opacity, shape = sample "", sample "/Shape gs "
        let mutable glyphPixels = 0
        for i in 0 .. opacity.Length-1 do
            let a, b = opacity.[i], shape.[i]
            if Pixel.r a > 240uy && Pixel.g a > 80uy && Pixel.g a < 180uy &&
               Pixel.r b < 200uy && Pixel.g b < 30uy && Pixel.b b > 80uy then
                glyphPixels <- glyphPixels + 1
        check "cached glyph paint retains half opacity but changes knockout coverage" (glyphPixels > 20))

    test "ICC luminosity masks composite in group space before PCS and transfer" (fun () ->
        // Adobe RGB (1998), D50 matrix primaries and a 563/256 gamma TRC.
        let profile = Convert.FromBase64String("AAABVgAAAAACIAAAc2NuclJHQiBYWVogAAAAAAAAAAAAAAAAYWNzcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAPbWAAEAAAAA0y0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHd3RwdAAAANgAAAAUclhZWgAAAOwAAAAUZ1hZWgAAAQAAAAAUYlhZWgAAARQAAAAUclRSQwAAASgAAAAOZ1RSQwAAATgAAAAOYlRSQwAAAUgAAAAOWFlaIAAAAAAAAPbWAAEAAAAA0y1YWVogAAAAAAAAnBkAAE+mAAAE/VhZWiAAAAAAAAA0iwAAoCsAAA+XWFlaIAAAAAAAACY0AAAQLwAAvq5jdXJ2AAAAAAAAAAECMwAAY3VydgAAAAAAAAABAjMAAGN1cnYAAAAAAAAAAQIz") |> text
        let profileObject = $"<</N 3 /Length {profile.Length}>>\nstream\n{profile}\nendstream"
        let gray = "/Managed cs 0.5 0.5 0.5 sc 0 0 100 100 re f"
        let halfBlack = "/Managed cs 0 0 0 sc /Half gs 0 0 100 100 re f"
        let multiplyBlue = "/Managed cs 0 0 1 sc /Multiply gs 0 0 100 100 re f"
        let inverse = "/TR <</FunctionType 2 /Domain [0 1] /C0 [1] /C1 [0] /N 1>>"
        // Expected gray on the white page after painting black through the mask.
        // Adobe RGB 0.5 has Y ~= 0.218; its blue and red primaries have
        // Y ~= 0.063 and 0.311, respectively.
        let cases =
            [ "encoded half gray", "Luminosity", "true", "false", "/BC [1 1 1]", "", gray, 199, 0
              "half black over white", "Luminosity", "true", "false", "/BC [1 1 1]", "", halfBlack, 199, 0
              "isolated Multiply", "Luminosity", "true", "false", "/BC [1 0 0]", "", multiplyBlue, 239, 176
              "nonisolated Multiply", "Luminosity", "false", "false", "/BC [1 0 0]", "", multiplyBlue, 255, 176
              "inverse transfer", "Luminosity", "true", "false", "/BC [1 1 1]", inverse, gray, 56, 255
              "default black backdrop", "Luminosity", "true", "false", "", "", gray, 199, 255
              "isolated knockout", "Luminosity", "true", "true", "/BC [1 1 1]", "", gray + " " + halfBlack, 199, 0
              "isolated non-knockout", "Luminosity", "true", "false", "/BC [1 1 1]", "", gray + " " + halfBlack, 243, 0
              "alpha inverse outside BBox", "Alpha", "false", "false", "", inverse, halfBlack, 128, 0 ]
        for label, subtype, isolated, knockout, bc, tr, content, inside, outside in cases do
            let groupResources = "/ColorSpace <</Managed [/ICCBased 6 0 R]>> /ExtGState <</Half <</ca 0.5>> /Multiply <</BM /Multiply>>>>"
            let group =
                $"<</Type /XObject /Subtype /Form /BBox [0 0 50 100] /Group <</S /Transparency /CS [/ICCBased 6 0 R] /I {isolated} /K {knockout}>> /Resources <<{groupResources}>> /Length {content.Length}>>\nstream\n{content}\nendstream"
            let resources = $"/ExtGState <</M <</SMask <</S /{subtype} /G 5 0 R {bc} {tr}>>>>>>"
            let pixels =
                renderFixture "/MediaBox [0 0 100 100]" "" resources [5,group; 6,profileObject]
                    "0 g /M gs 0 0 100 100 re f" 1.0 |> rasterize
            for location, x, expected in ["inside", 25, inside; "outside", 75, outside] do
                let p = pixels.[50*100+x]
                let close c = abs (int c - expected) <= 3
                check $"{label}: {location}" (Pixel.a p = 255uy && close (Pixel.r p) && close (Pixel.g p) && close (Pixel.b p)))

    test "PDF DCT samples use Decode rather than standalone Adobe polarity" (fun () ->
        // One 8x8 MCU with constant CMYK samples (0,128,128,0), unit
        // quantization and DC-only blocks. APP14 declares transform zero.
        let jpeg = Convert.FromBase64String("/9j/7gAOQWRvYmUAZAAAAAAA/9sAQwABAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEB/8AAFAgACAAIBAERAAIRAAMRAAQRAP/EACcAAAIAAAAAAAAAAAAAAAAAAAALEAEAAAAAAAAAAAAAAAAAAAAA/9oADgQBAAIAAwAEAAA/AF/4Bf8Av//Z")
        let unmarked = Array.append jpeg.[0..1] jpeg.[18..]
        for label, data, decode, black in
            [ "Adobe default", jpeg, "", false
              "Adobe identity", jpeg, "/Decode [0 1 0 1 0 1 0 1]", false
              "Adobe reversed", jpeg, "/Decode [1 0 1 0 1 0 1 0]", true
              "unmarked default", unmarked, "", false ] do
            let payload = text data
            let image = $"<</Type /XObject /Subtype /Image /Width 8 /Height 8 /BitsPerComponent 8 /ColorSpace /DeviceCMYK /Filter /DCTDecode {decode} /Length {data.Length}>>\nstream\n{payload}\nendstream"
            let pixels =
                renderFixture "/MediaBox [0 0 20 20]" "" "/XObject <</Im 5 0 R>>" [5,image]
                    "q 20 0 0 20 0 0 cm /Im Do Q" 1.0 |> rasterize
            let pixel = pixels.[10*20+10]
            check $"{label} preserves CMYK sample polarity"
                (if black then Pixel.r pixel = 0uy && Pixel.g pixel = 0uy && Pixel.b pixel = 0uy
                 else Pixel.r pixel > 220uy && Pixel.g pixel > 80uy && Pixel.g pixel < 180uy
                      && Pixel.b pixel > 80uy && Pixel.b pixel < 180uy))

    test "truncated Flate content retains its actual decoded prefix" (fun () ->
        // issue22044's truncated stream; Python zlib independently recovers
        // 2371 bytes, ending in an incomplete text-positioning instruction.
        let encoded = Convert.FromBase64String("eJztyrEJgDAQhtHeKf5SKxNQtA5csIhEjhtBLNx/AOcQvle/EpprVt4Vj9akbUmKW+NhrXVV76fcLu+T4pXFUPh8Pp/P5/P5")
        let line = "BT /F1 18 Tf 50 740 Td (HELLO FROM REPRO) Tj ET\n"
        let expected = (String.replicate 54 line).Substring(0,2371)
        let decoded = Streams.decompress "FlateDecode" encoded
        check "all bytes before truncation survive without compressed garbage" (text decoded = expected)
        let objects =
            [ 1, "<</Type /Catalog /Pages 2 0 R>>"
              2, "<</Type /Pages /Kids [3 0 R] /Count 1>>"
              3, "<</Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources <</Font <</F1 5 0 R>>>> /Contents 6 0 R>>"
              4, $"<</Filter /FlateDecode /Length {encoded.Length}>>\nstream\n{text encoded}\nendstream"
              5, "<</Type /Font /Subtype /Type1 /BaseFont /Helvetica>>"
              6, "[4 0 R]" ]
        let doc = Pdf.openData (build normal objects 1)
        let _,_,_,boxes = renderPage (RenderOptions.native ()) doc 1 1.0 |> Option.get
        let selected = boxes |> Array.map (fun box -> box.Char) |> String.concat ""
        check "complete text shows remain renderable before the damaged suffix"
            (selected = String.replicate (expected.Length / line.Length) "HELLO FROM REPRO")
        let refused =
            try Streams.decompress "FlateDecode" [|0uy;0uy|] |> ignore; false
            with _ -> true
        check "undecodable Flate bytes are not returned as content" refused)

    for resource, finalHeight in ["vertical-cid.pdf", 26.0; "vertical-cid-default.pdf", 20.0] do
        test ("vertical CID metrics " + resource) (fun () ->
            use stream = Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
            use bytes = new IO.MemoryStream()
            stream.CopyTo bytes
            let doc = Pdf.openData (bytes.ToArray())
            match renderPage (RenderOptions.native ()) doc 1 1.0 with
            | Some (_, _, _, boxes) ->
                let expected = [|92.0,150.0,16.0,18.0; 90.0,168.0,20.0,22.0; 88.0,190.0,24.0,24.0; 94.0,214.0,12.0,finalHeight|]
                check "vertical mixed-width CMap selects all four characters" (boxes.Length = expected.Length)
                if boxes.Length = expected.Length then
                    for i in 0 .. expected.Length - 1 do
                        let x,y,w,h = expected.[i]
                        let b = boxes.[i]
                        check $"vertical selection origin and advance {resource} glyph {i}"
                            (near x b.X && near y b.Y && near w b.W && near h b.H)
            | None -> check "vertical CID page renders" false)

    test "named PostScript usecmap inheritance" (fun () ->
        let parent = "/WMode 1 def\n2 begincodespacerange\n<00> <7f>\n<8100> <81ff>\nendcodespacerange\n2 begincidrange\n<41> <41> 2\n<8140> <8142> 4\nendcidrange"
        let child = "/Parent usecmap\n1 begincidchar\n<41> 3\nendcidchar"
        let cycleA = "/CycleB usecmap\n1 begincidchar\n<41> 9\nendcidchar"
        let cycleB = "/CycleA usecmap\n/WMode 1 def"
        let cmaps = ["Parent", latin1 parent; "Child", latin1 child; "CycleA", latin1 cycleA; "CycleB", latin1 cycleB] |> Map.ofList
        let doc = Pdf.openData (build normal onePage 1)
        let resources name =
            let font =
                PdfDict (Map.ofList [
                    "Type", PdfName "Font"; "Subtype", PdfName "Type0"; "BaseFont", PdfName "Fixture"
                    "Encoding", PdfName name
                    "DescendantFonts", PdfArray [PdfDict (Map.ofList ["Subtype", PdfName "CIDFontType2"])] ])
            PdfDict (Map.ofList ["Font", PdfDict (Map.ofList ["F1", font])])
        let font = Rbt.Pdf.Render.Fonts.resolve cmaps doc (resources "Child") "F1"
        let codes = Rbt.Pdf.Render.Fonts.sourceCodes font [|0x41uy; 0x81uy; 0x40uy; 0x81uy; 0x41uy|]
        check "inherited CMap writing mode" font.IsVertical
        check "inherited mixed CMap code widths" ((codes |> Array.map (fun (_, b) -> b.Length)) = [|1;2;2|])
        check "child CMap overrides inherited CID"
            ((codes |> Array.map (fun (code,b) -> Rbt.Pdf.Render.Fonts.cid font code b.Length)) = [|3;4;5|])
        let cyclic = Rbt.Pdf.Render.Fonts.resolve cmaps doc (resources "CycleA") "F1"
        check "cyclic usecmap retains reachable mappings"
            (cyclic.IsVertical && Rbt.Pdf.Render.Fonts.cid cyclic 0x41 1 = 9))

    test "CMYK ICC source PCS retains range and wide gamut" (fun () ->
        // ICC lut16 u1.15 XYZ: the fourth input selects X=0 or X=1.5.
        let offset, header, entries, channels = 144, 52, 2, 4
        let size = offset + header + 2*(channels*entries + 16*3 + 3*entries)
        let bytes = Array.zeroCreate<byte> size
        let u16 p n = bytes.[p] <- byte (n >>> 8); bytes.[p+1] <- byte n
        let u32 p n =
            bytes.[p] <- byte (n >>> 24); bytes.[p+1] <- byte (n >>> 16)
            bytes.[p+2] <- byte (n >>> 8); bytes.[p+3] <- byte n
        let sigAt p (s: string) = for i in 0..3 do bytes.[p+i] <- byte s.[i]
        u32 0 size; sigAt 12 "scnr"; sigAt 16 "CMYK"; sigAt 20 "XYZ "; sigAt 36 "acsp"
        u32 128 1; sigAt 132 "A2B0"; u32 136 offset; u32 140 (size-offset)
        sigAt offset "mft2"
        bytes.[offset+8] <- 4uy; bytes.[offset+9] <- 3uy; bytes.[offset+10] <- 2uy
        for i in 0..2 do u32 (offset+12+16*i) 65536
        u16 (offset+48) 2; u16 (offset+50) 2
        let mutable pos = offset+header
        let sample n = u16 pos n; pos <- pos+2
        for _ in 1..4 do sample 0; sample 65535
        for corner in 0..15 do
            sample (if corner &&& 1 = 1 then 49152 else 0)
            sample 8192
            sample 4096
        for _ in 1..3 do sample 0; sample 65535
        let doc = Pdf.openData (build normal onePage 1)
        let range = [0.0;1.0;0.0;1.0;0.0;1.0;0.25;0.75] |> List.map PdfReal |> PdfArray
        let stream = PdfStream (Map.ofList ["N", PdfInt 4L; "Range", range], bytes)
        let spec = PdfArray [PdfName "ICCBased"; stream]
        let space = Rbt.Pdf.Render.PdfColor.resolve doc PdfNull spec |> Option.get
        check "LUT CMYK is not a blending space" (Rbt.Pdf.Render.PdfColor.tryWorkingSpace space |> Option.isNone)
        let target: Rbt.Pdf.Render.PdfColor.WorkingSpace =
            { IsDisplay = false; IsGray = false
              ToPcs = fun r g b -> struct(r,g,b)
              FromPcs = fun x y z -> struct(x/2.0,y,z)
              ProfileSource = None }
        let convert k = Rbt.Pdf.Render.PdfColor.toWorkingRgb target space [|0.0;0.0;0.0;k|]
        // Range clips K without rescaling: X=1.5*K, then target R=X/2.
        let struct(r0,g0,b0) = convert 0.0
        let struct(r1,g1,b1) = convert 1.0
        check "ICC Range clips low fourth input" (r0 = 48uy && g0 = 64uy && b0 = 32uy)
        check "ICC Range clips high fourth input without display clipping" (r1 = 143uy && g1 = 64uy && b1 = 32uy))

    let loadLutFixture name =
        use stream = Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(name)
        use reader = new IO.StreamReader(stream)
        Convert.FromBase64String(reader.ReadToEnd())

    test "explicit RGB LUT8 XYZ and LUT16 Lab inverse admit PDF blending spaces" (fun () ->
        let fixture name colorName =
            let fixtureDoc = Pdf.openData (loadLutFixture name)
            let page = Pdf.getPage fixtureDoc 1 |> Option.get
            let resources = Pdf.dictGet fixtureDoc "Resources" page |> Option.get
            let spaces = Pdf.dictGet fixtureDoc "ColorSpace" resources |> Option.get
            let space = Pdf.dictGet fixtureDoc colorName spaces |> Option.get
            match Pdf.deref fixtureDoc space with
            | PdfArray [PdfName "ICCBased"; stream] -> Pdf.getStreamData fixtureDoc stream
            | _ -> failwith "LUT fixture has no ICC profile"
        let doc = Pdf.openData (build normal onePage 1)
        let space bytes =
            Rbt.Pdf.Render.PdfColor.resolve doc PdfNull
                (PdfArray [PdfName "ICCBased"; PdfStream (Map.ofList ["N", PdfInt 3L], bytes)])
            |> Option.get
        let xyzBytes = fixture "icc-rgb-lut8-xyz-page.base64" "Working"
        let labBytes = fixture "icc-rgb-lut16-lab-forms.base64" "Lab"
        let xyzSpace, labSpace = space xyzBytes, space labBytes
        let xyz = Rbt.Pdf.Render.PdfColor.tryWorkingSpace xyzSpace |> Option.get
        let lab = Rbt.Pdf.Render.PdfColor.tryWorkingSpace labSpace |> Option.get
        // Independent fixture definition: A2B swaps R/G and halves XYZ;
        // B2A accepts physical D50 XYZ (not a normalized or display RGB triplet).
        let struct(x,y,z) = xyz.ToPcs 0.75 0.25 0.5
        check "RGB lut8 forward XYZ component order" (abs(x-0.125)<0.004 && abs(y-0.375)<0.004 && abs(z-0.25)<0.004)
        let struct(r,g,b) = xyz.FromPcs 0.125 0.375 0.25
        check "RGB lut8 B2A XYZ physical encoding" (abs(r-0.5625)<0.015 && abs(g-0.25)<0.015 && abs(b-0.5)<0.015)
        let struct(l,a,c) = lab.FromPcs 0.9642 1.0 0.8249
        check "RGB lut16 B2A Lab D50 white encoding"
            (abs(l-0.8)<0.006 && abs(a-0.502)<0.008 && abs(c-0.502)<0.008)
        let missing = Array.copy xyzBytes
        let struct(sr,sg,sb) =
            Rbt.Pdf.Render.PdfColor.toWorkingRgb xyz xyzSpace [|0.75;0.25;0.5|]
        check "same ICC source stays encoded despite non-reciprocal B2A"
            (sr=191uy && sg=64uy && sb=128uy)
        let struct(ll,aa,bb) =
            Rbt.Pdf.Render.PdfColor.toWorkingRgb lab labSpace [|0.5;0.5;0.5|]
        check "same Lab source avoids lossy B2A L contraction"
            (ll=128uy && aa=128uy && bb=128uy)
        // Hide B2A0 in the tag table, leaving the valid forward A2B0 intact.
        let marker = latin1 "B2A0"
        let index =
            [132 .. missing.Length-4]
            |> List.find (fun i -> missing.[i]=marker.[0] && missing.[i+1]=marker.[1]
                                   && missing.[i+2]=marker.[2] && missing.[i+3]=marker.[3])
        missing.[index] <- byte 'X'
        check "RGB LUT without explicit inverse is unsupported as group"
            (Rbt.Pdf.Render.PdfColor.tryWorkingSpace (space missing) |> Option.isNone)
        let malformed = Array.copy xyzBytes
        let offset =
            [0..3] |> List.fold (fun value n -> value*256 + int malformed.[index+4+n]) 0
        malformed.[offset] <- byte 'X'
        check "unsupported inverse preserves valid forward RGB source"
            (Rbt.Pdf.Render.PdfColor.tryWorkingSpace (space malformed) |> Option.isNone
             && (match Rbt.Pdf.Render.PdfColor.toWorkingRgb Rbt.Pdf.Render.PdfColor.displaySpace
                           (space malformed) [|1.0;0.0;0.0|] with
                 | struct(_,green,_) -> green > 245uy)))

    test "RGB LUT page and Form groups composite before display and boundary effects" (fun () ->
        let render name =
            loadLutFixture name
            |> Pdf.openData
            |> fun doc -> renderPage (RenderOptions.native ()) doc 1 1.0 |> Option.get |> rasterize
        let pixel (pixels: uint32[]) x y = pixels.[y*200+x]
        let checkColor label (r,g,b) p =
            check label (abs(int(Pixel.r p)-r)<=5 && abs(int(Pixel.g p)-g)<=5
                         && abs(int(Pixel.b p)-b)<=5)
        let page = render "icc-rgb-lut8-xyz-page.base64"
        // LittleCMS relative-colorimetric oracle, RGB [128,0,128] -> [0,186,148].
        for x,y in [50,50;150,50;50,150;150,150] do
            checkColor $"RGB LUT page encoded blend ({x},{y})" (0,186,148) (pixel page x y)
        let forms = render "icc-rgb-lut16-lab-forms.base64"
        // LittleCMS: Lab LUT RGB [191,128,128] -> sRGB [186,184,183].
        checkColor "RGB LUT isolated Form" (186,184,183) (pixel forms 50 50)
        checkColor "RGB LUT non-isolated Form sees green backdrop" (128,255,127) (pixel forms 150 50)
        checkColor "RGB LUT boundary half-opacity in sRGB" (93,220,92) (pixel forms 50 150)
        // XYZ blue -> Lab [0,.5,0]; half over Lab [.5,.5,.5] gives
        // [.25,.5,.25], LittleCMS sRGB [0,64,159]. At the boundary
        // Multiply over green with 0.5 opacity * 0.5 Alpha mask -> [0,207,0].
        checkColor "RGB LUT nested XYZ to Lab, boundary Multiply and mask" (0,207,0) (pixel forms 150 150))

    test "ICC range bounds and image Decode preserve component values" (fun () ->
        let profile = Convert.FromBase64String("AAAAvAAAAAACIAAAc2NuckdSQVlYWVogAAAAAAAAAAAAAAAAYWNzcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAPbWAAEAAAAA0y0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACd3RwdAAAAJwAAAAUa1RSQwAAALAAAAAMWFlaIAAAAAAAAPbWAAEAAAAA0y1jdXJ2AAAAAAAAAAA=") |> text
        let profileObject = $"<</N 1 /Range [0.2 0.8] /Length {profile.Length}>>\nstream\n{profile}\nendstream"
        let image extra = $"<</Type /XObject /Subtype /Image /Width 1 /Height 1 /BitsPerComponent 8 /ColorSpace [/ICCBased 5 0 R] {extra} /Length 1>>\nstream\n\000\nendstream"
        let resources = "/ColorSpace <</Managed [/ICCBased 5 0 R]>> /XObject <</Low 6 0 R /High 7 0 R>>"
        let pixels =
            renderFixture "/MediaBox [0 0 300 400]" "" resources [5,profileObject;6,image "";7,image "/Decode [0.8 0.2]"]
                "q 20 0 0 20 10 20 cm /Low Do Q /Managed cs 0.2 sc 40 20 20 20 re f 0.8 sc 70 20 20 20 re f q 20 0 0 20 100 20 cm /High Do Q" 1.0 |> rasterize
        for x, expected in [20,124;50,124;80,231;110,231] do
            let p = pixels.[370*300+x]
            check $"ICC bounded or decoded sample {x}"
                (abs(int(Pixel.r p)-expected)<=1 && abs(int(Pixel.g p)-expected)<=1 && abs(int(Pixel.b p)-expected)<=1))

    test "calibrated color spaces use transfer curves and XYZ" (fun () ->
        let resources = "/ColorSpace <</G [/CalGray <</WhitePoint [0.9642 1 0.8249] /Gamma 2>>] /L [/Lab <</WhitePoint [0.9642 1 0.8249]>>] /R [/CalRGB <</WhitePoint [0.9642 1 0.8249] /Matrix [0.3851 0.7169 0.0971 0.4361 0.2225 0.0139 0.1431 0.0606 0.7141]>>]>>"
        let pixels = renderFixture "/MediaBox [0 0 300 400]" "" resources []
                        "/G cs 0.5 sc 10 20 20 20 re f /L cs 50 0 0 sc 40 20 20 20 re f /R cs 1 0 0 sc 70 20 20 20 re f" 1.0 |> rasterize
        for x,r,g,b in [20,137,137,137;50,119,119,119;80,0,255,0] do
            let p = pixels.[370*300+x]
            check $"calibrated color {x}"
                (abs(int(Pixel.r p)-r)<=1 && abs(int(Pixel.g p)-g)<=1 && abs(int(Pixel.b p)-b)<=1))

    test "renderer transforms path coordinates" (fun () ->
        let events = renderContent "2 0 0 3 50 60 cm 10 20 m 20 30 l S" |> drawingEvents Matrix3x2.identity false
        match events with
        | [_, _, [a; b]] ->
            check "transformed path start" (a = vec2 70.0 280.0)
            check "transformed path endpoint" (b = vec2 90.0 250.0)
        | _ -> check "transformed path emitted" false)

    test "renderer close restores subpath start" (fun () ->
        let events = renderContent "10 20 m 30 40 l h 50 60 70 80 v S" |> drawingEvents Matrix3x2.identity false
        match events with
        | [_, _, [_; _; cp; _; _]] ->
            check "v control point after h" (cp = vec2 10.0 380.0)
        | _ -> check "closed curve emitted" false)

    test "renderer transformed fill and clip pixels" (fun () ->
        let pixels = renderPixels "q 2 0 0 3 50 60 cm 0 0 10 10 re W n 1 0 0 rg 0 0 20 20 re f Q 0 0 1 rg 0 0 5 5 re f"
        check "transformed fill inside transformed clip" (pixels.[320 * 300 + 60] = Pixel.pack 0uy 0uy 255uy 255uy)
        check "transformed clip excludes rest of fill" (pixels.[320 * 300 + 80] = Pixel.fromColorPremul Color.white)
        check "Q restores transform and clipping" (pixels.[397 * 300 + 2] = Pixel.pack 255uy 0uy 0uy 255uy))

    test "renderer text can be covered and clipped" (fun () ->
        let text = "BT /F1 24 Tf 20 40 Td (Visible) Tj ET "
        let white = Pixel.fromColorPremul Color.white
        let visible = renderPixels text
        check "control text paints visible pixels" (visible |> Array.exists ((<>) white))
        let covered = renderPixels (text + "1 g 0 0 300 400 re f")
        check "later opaque fill covers text" (covered |> Array.forall ((=) white))
        let clipped = renderPixels ("q 0 0 1 1 re W n " + text + "Q")
        check "path clip excludes text" (clipped |> Array.forall ((=) white)))

    test "renderer crop origin and inherited rotations" (fun () ->
        // Rectangles are observed in screen space, including scale and crop origin.
        for rotation, expectedW, expectedH, expectedX, expectedY in
            [ 0, 400.0, 200.0, 0.0, 200.0
              90, 200.0, 400.0, 0.0, 0.0
              180, 400.0, 200.0, 400.0, 0.0
              270, 200.0, 400.0, 200.0, 400.0
              -90, 200.0, 400.0, 200.0, 400.0 ] do
            let cmds, w, h, _ =
                renderFixture "/MediaBox [100 200 400 600]"
                    $"/CropBox [120 230 320 330] /Rotate {rotation}" "" []
                    "120 230 20 10 re f" 2.0
            check $"rotation {rotation}: cropped size" (w = expectedW && h = expectedH)
            let _, _, points = drawingEvents Matrix3x2.identity false cmds |> List.exactlyOne
            check $"rotation {rotation}: cropped origin" (points.Head = vec2 expectedX expectedY)
        let empty, w, h, _ =
            renderFixture "/MediaBox [100 200 400 600]" "/CropBox [120 230 320 330] /Rotate 90" "" [] "" 1.0
        check "empty page retains rotated cropped dimensions" (w = 100.0 && h = 200.0 && List.isEmpty empty)
        let pixels =
            renderFixture "/MediaBox [100 200 340 360]" "" "" [] "1 0 0 rg 120 220 65 45 re f" 1.0 |> rasterize
        check "nonzero MediaBox paints visible content" (pixels.[130 * 240 + 30] = Pixel.pack 0uy 0uy 255uy 255uy))

    test "renderer text transforms and PDF advances" (fun () ->
        let _, _, _, boxes =
            renderFixture "/MediaBox [0 0 300 400]" "" testFont []
                "BT /F1 20 Tf 2 Tc 50 Tz 10 Ts 0 1 -1 0 100 100 Tm (AB) Tj (A) Tj ET" 1.0
        check "selection bounds follow glyph transform"
            (boxes.Length = 3 && near 74.0 boxes.[0].X && near 294.0 boxes.[0].Y
             && near 20.0 boxes.[0].W && near 6.0 boxes.[0].H)
        check "successive shows retain PDF advances"
            (near 290.5 boxes.[1].Y && near 284.5 boxes.[2].Y)
        let _, _, _, mirroredBoxes =
            renderFixture "/MediaBox [0 0 300 400]" "" testFont []
                "BT /F1 -20 Tf 1 0 0 1 100 100 Tm (A) Tj (A) Tj ET" 1.0
        check "negative font size mirrors selection and advances backward"
            (mirroredBoxes.Length = 2 && near 90.0 mirroredBoxes.[0].X && near 80.0 mirroredBoxes.[1].X
             && mirroredBoxes |> Array.forall (fun box -> box.W > 0.0 && box.H > 0.0)))

    test "renderer TJ and word spacing" (fun () ->
        let font = "/Font <</F1 <</Type /Font /Subtype /Type1 /BaseFont /Helvetica>>>>"
        let _, _, _, boxes =
            renderFixture "/MediaBox [0 0 300 400]" "" font []
                "BT /F1 10 Tf 2 Tc 3 Tw 50 Tz 1 0 0 1 20 40 Tm [(A ) 200 (B)] TJ ET" 1.0
        // Helvetica A = 667; space = 278; TJ 200 shifts by -1 at this Tz.
        check "word spacing applies only to space and TJ scales once"
            (boxes.Length = 3 && near 24.335 boxes.[1].X
             && near 27.225 boxes.[2].X))

    test "renderer clips Form XObjects to transformed BBox" (fun () ->
        let content = "1 0 0 rg -10 -10 40 40 re f"
        let form = $"<</Type /XObject /Subtype /Form /BBox [0 0 20 20] /Matrix [2 0 0 2 20 30] /Length {content.Length}>>\nstream\n{content}\nendstream"
        let pixels =
            renderFixture "/MediaBox [0 0 300 400]" "" "/XObject <</Fm 5 0 R>>" [5, form]
                "/Fm Do 0 0 1 rg 0 0 5 5 re f" 1.0 |> rasterize
        check "form paints inside transformed BBox" (pixels.[360 * 300 + 30] = Pixel.pack 0uy 0uy 255uy 255uy)
        check "form BBox excludes overflowing content" (pixels.[360 * 300 + 10] = Pixel.fromColorPremul Color.white)
        check "form clip does not leak into caller" (pixels.[397 * 300 + 2] = Pixel.pack 255uy 0uy 0uy 255uy))

    test "glyph-indexed TrueType subsets and CID map bounds" (fun () ->
        let font = IO.File.ReadAllBytes(IO.Path.Combine(__SOURCE_DIRECTORY__, "../../assets/NotoSans-Regular.ttf"))
        let parsed = Rbt.Shaper.Shaper.parseFont font
        let gid = Rbt.Shaper.OpenType.Types.CmapLookup.lookup parsed.Cmap (int 'A')
        let subset = Array.copy font
        let count = (int subset.[4] <<< 8) ||| int subset.[5]
        let cmapOffset =
            [0 .. count-1]
            |> List.map (fun i -> 12 + i*16)
            |> List.find (fun offset -> Encoding.ASCII.GetString(subset, offset, 4) = "cmap")
        Array.blit subset (12 + (count-1)*16) subset cmapOffset 16
        subset.[4] <- byte ((count-1) >>> 8)
        subset.[5] <- byte (count-1)
        let fontData = text subset
        let rec glyphIds commands =
            [ for command in commands do
                match command with
                | Group (_, _, children) | Clip (_, _, children)
                | TransparencyGroup (_, _, _, _, _, children)
                | ColorManagedGroup (_, _, _, _, _, _, children) -> yield! glyphIds children
                | DrawGlyphs (_, runs, _) ->
                    for run in runs do
                        for glyph in run.Glyphs do yield glyph.GlyphId
                | _ -> () ]
        for mapping, payload, expected in ["/Identity", "", gid; "8 0 R", "", 0; "8 0 R", "\000\000", 0] do
            let objects =
                [ 5, "<</Type /Font /Subtype /Type0 /BaseFont /Subset /Encoding /Identity-H /DescendantFonts [6 0 R]>>"
                  6, $"<</Type /Font /Subtype /CIDFontType2 /BaseFont /Subset /CIDSystemInfo <</Registry (Adobe) /Ordering (Identity) /Supplement 0>> /FontDescriptor 7 0 R /CIDToGIDMap {mapping} /DW 500>>"
                  7, "<</Type /FontDescriptor /FontName /Subset /Flags 32 /FontFile2 9 0 R>>"
                  8, $"<</Length {payload.Length}>>\nstream\n{payload}\nendstream"
                  9, $"<</Length {fontData.Length}>>\nstream\n{fontData}\nendstream" ]
            let code = gid.ToString("X4")
            let commands, _, _, _ =
                renderFixture "/MediaBox [0 0 100 100]" "" "/Font <</F1 5 0 R>>" objects
                    $"BT /F1 40 Tf 1 0 0 1 20 40 Tm <{code}> Tj ET" 1.0
            check $"CID glyph selection with {mapping} and {payload.Length}-byte map" (glyphIds commands = [expected]))

    test "PDF page and nested Form blend independently of caller background" (fun () ->
        let formContent = "0 0 1 rg 0 0 80 30 re f"
        let form = $"<</Type /XObject /Subtype /Form /BBox [0 0 80 30] /Resources <<>> /Group <</S /Transparency /CS /DeviceRGB /I false>> /Length {formContent.Length}>>\nstream\n{formContent}\nendstream"
        let commands, _, _, _ =
            renderFixture "/MediaBox [0 0 80 60]" ""
                "/ExtGState <</D <</BM /Difference>>>> /XObject <</Fm 5 0 R>>" [5, form]
                "1 0 0 rg 0 0 40 60 re f q /D gs 0 0 1 rg 0 30 80 30 re f Q q /D gs /Fm Do Q" 1.0
        let draw background =
            let backend = Raster.create ()
            backend.Init 100 100
            backend.Submit (Clear background :: commands)
            backend.Finish () |> Framebuffer.tryGetArray |> Option.get
        for background in [Color.white; Color.rgb 0uy 255uy 0uy] do
            let pixels = draw background
            for y in [15; 45] do
                check $"Difference uses painted page backdrop at y={y}" (pixels.[y*100+20] = Pixel.pack 255uy 0uy 255uy 255uy)
                check $"Difference ignores external page backdrop at y={y}" (pixels.[y*100+60] = Pixel.pack 255uy 0uy 0uy 255uy)
            check "page clipping preserves surrounding backdrop" (pixels.[90*100+90] = Pixel.fromColorPremul background))

    test "PDF page group knockout" (fun () ->
        let pixels =
            renderFixture "/MediaBox [0 0 100 100] /Group <</S /Transparency /CS /DeviceRGB /K true>>"
                "" "/ExtGState <</Half <</ca 0.5>>>>" []
                "/Half gs 1 0 0 rg 0 0 70 100 re f 0 0 1 rg 30 0 70 100 re f" 1.0 |> rasterize
        let overlap = pixels.[50*100+50]
        check "page knockout removes earlier translucent member"
            (abs (int (Pixel.r overlap) - 127) <= 1 && abs (int (Pixel.g overlap) - 127) <= 1 && Pixel.b overlap = 255uy))

    test "simple-font advances outside explicit widths" (fun () ->
        for descriptor, expected in ["/MissingWidth 0", 110.0; "/MissingWidth 250", 115.0; "", 110.0] do
            let resources =
                $"/Font <</F1 <</Type /Font /Subtype /Type1 /BaseFont /Helvetica /FirstChar 65 /LastChar 66 /Widths [500 250] /FontDescriptor <</Type /FontDescriptor /FontName /Helvetica /Flags 32 {descriptor}>>>>>>"
            let _, _, _, boxes =
                renderFixture "/MediaBox [0 0 300 200]" "" resources []
                    "BT /F1 20 Tf 1 0 0 1 100 100 Tm (A B) Tj ET" 1.0
            let b = boxes |> Array.find (fun box -> box.Char = "B")
            check $"out-of-range space uses descriptor width ({descriptor})" (near expected b.X))

    test "indirect widths preserve PDF 2.0 simple-font advances" (fun () ->
        // The PDF Association's Simple PDF 2.0 example puts /Widths in 8 0 R,
        // starts the width array at code 33, and assigns code 32 /MissingWidth 0.
        let widths = Array.zeroCreate<int> 94
        for code, width in [ 'H', 722; 'e', 556; 'l', 222; 'o', 556
                             'W', 944; 'r', 333; 'd', 556 ] do
            widths.[int code - 33] <- width
        let widthArray = widths |> Array.map string |> String.concat " "
        let font = "<</Type /Font /Subtype /Type1 /BaseFont /Helvetica /FirstChar 33 /LastChar 126 /Widths 8 0 R /FontDescriptor 9 0 R>>"
        let descriptor = "<</Type /FontDescriptor /FontName /Helvetica /Flags 32 /MissingWidth 0>>"
        let _, _, _, boxes =
            renderFixture "/MediaBox [0 0 612 396]" "" "/Font <</F1 7 0 R>>"
                [7, font; 8, $"[{widthArray}]"; 9, descriptor]
                "BT /F1 24 Tf 100 100 Td (Hello World) Tj ET" 1.0
        check "indirect widths place World immediately after Hello"
            (boxes.Length = 11 && near 154.672 boxes.[6].X
             && near 217.336 (boxes.[10].X + boxes.[10].W)))

    test "sampled PDF transfer functions" (fun () ->
        let resolve dict data =
            let body = $"<<{dict} /Length {String.length data}>>\nstream\n{data}\nendstream"
            let objects = [1, "<</Type /Catalog /Pages 2 0 R>>"; 2, "<</Type /Pages /Kids [3 0 R] /Count 1>>"; 3, "<</Type /Page /Parent 2 0 R /MediaBox [0 0 10 10] /F 5 0 R>>"; 5, body]
            let doc = Pdf.openData (build normal objects 1)
            let obj = Pdf.dictGet doc "F" (Pdf.getPageRefs doc |> List.head) |> Option.get
            Rbt.Pdf.Render.Shading.tryEvaluateFunction doc obj
        for bits, data in [1, [0x40]; 2, [0x30]; 4, [0x0f]; 8, [0;255]; 12, [0;15;255]; 16, [0;0;255;255]; 24, [0;0;0;255;255;255]; 32, [0;0;0;0;255;255;255;255]] do
            let payload = data |> List.map char |> Array.ofList |> String
            let f = resolve $"/FunctionType 0 /Domain [0 1] /Range [0 1] /Size [2] /BitsPerSample {bits}" payload |> Option.get
            check $"sampled {bits}-bit interpolation and clipping" (near 0.0 (f -1.0) && near 0.5 (f 0.5) && near 1.0 (f 2.0))
        let f = resolve "/FunctionType 0 /Domain [0 1] /Range [0.2 0.8] /Size [2] /BitsPerSample 8 /Encode [1 0] /Decode [0 1]" (String [|char 0; char 255|]) |> Option.get
        check "sampled reversed Encode and range clipping" (near 0.8 (f 0.0) && near 0.5 (f 0.5) && near 0.2 (f 1.0))
        let cubicDict = "/FunctionType 0 /Domain [0 3] /Range [0 1] /Size [4] /BitsPerSample 8 /Order 3"
        let cubic bytes = resolve cubicDict (bytes |> List.map char |> Array.ofList |> String) |> Option.get
        let crest = cubic [0;255;255;0]
        let trough = cubic [255;0;0;255]
        // At the middle of two unit-height knots, Hermite tangents are
        // +1/2 and -1/2: the unbounded value is 9/8 (or -1/8 inverted).
        check "cubic overshoot clipped after interpolation"
            (near 1.0 (crest 1.5) && near 0.0 (trough 1.5))
        let crestUnclipped =
            resolve "/FunctionType 0 /Domain [0 3] /Range [0 2] /Size [4] /BitsPerSample 8 /Order 3 /Decode [0 1]"
                (String [|char 0; char 255; char 255; char 0|]) |> Option.get
        check "cubic interpolates before output clipping" (near 1.125 (crestUnclipped 1.5))
        let edge = cubic [0;255;0;0]
        // First secant tangent = 1 and next centered tangent = 0,
        // hence the first half-segment is 1/2 + 1/8 = 5/8.
        check "cubic endpoint tangents and exact knots"
            (near 0.0 (edge -1.0) && near 0.625 (edge 0.5)
             && near 1.0 (edge 1.0) && near 0.0 (edge 3.0)
             && near 0.0 (edge 4.0))
        let rightEdge = cubic [0;0;255;0]
        check "cubic right endpoint secant and exact knot"
            (near 0.625 (rightEdge 2.5) && near 0.0 (rightEdge 3.0))
        let reversedCubic =
            resolve (cubicDict + " /Encode [3 0]") (String [|char 0; char 255; char 0; char 0|]) |> Option.get
        check "cubic reversed Encode and clipped input"
            (near 0.625 (reversedCubic 2.5) && near 0.0 (reversedCubic -1.0)
             && near 0.0 (reversedCubic 4.0))
        let decodedCubic =
            resolve "/FunctionType 0 /Domain [0 3] /Range [0.25 0.75] /Size [4] /BitsPerSample 8 /Order 3 /Decode [0.2 0.8]"
                (String [|char 0; char 255; char 255; char 0|]) |> Option.get
        check "cubic Decode followed by Range clipping"
            (near 0.25 (decodedCubic 0.0) && near 0.75 (decodedCubic 1.5))
        for size, bytes, expected in
            [1, [255], 1.0; 2, [0;255], 0.5; 3, [0;255;0], 0.5] do
            let short =
                resolve $"/FunctionType 0 /Domain [0 1] /Range [0 1] /Size [{size}] /BitsPerSample 8 /Order 3"
                    (bytes |> List.map char |> Array.ofList |> String) |> Option.get
            check $"cubic Size {size} uses constant or linear interpolation"
                (near expected (short (if size = 3 then 0.25 else 0.5)))
        check "sampled unsupported interpolation orders rejected"
            ([0;2;4;-1] |> List.forall (fun order ->
                (resolve $"/FunctionType 0 /Domain [0 1] /Range [0 1] /Size [2] /BitsPerSample 8 /Order {order}"
                    (String [|char 0; char 255|])).IsNone))
        let decoded = resolve "/FunctionType 0 /Domain [0 1] /Range [0 1] /Size [2] /BitsPerSample 8 /Decode [1 0]" (String [|char 0; char 255|]) |> Option.get
        check "sampled reversed Decode" (near 1.0 (decoded 0.0) && near 0.25 (decoded 0.75) && near 0.0 (decoded 1.0))
        check "odd Encode rejected" ((resolve "/FunctionType 0 /Domain [0 1] /Range [0 1] /Size [2] /BitsPerSample 8 /Encode [0 1 2]" (String [|char 0; char 255|])).IsNone)
        check "odd Decode rejected" ((resolve "/FunctionType 0 /Domain [0 1] /Range [0 1] /Size [2] /BitsPerSample 8 /Decode [0 1 2]" (String [|char 0; char 255|])).IsNone)
        let sampled = Rbt.Pdf.Render.PdfSampled.create
        check "sampled evaluator rejects odd Encode without indexing past end"
            ((sampled [|0.0;1.0|] [|0.0;1.0|] [|2|] 8 1 [|0.0;1.0;2.0|] [|0.0;1.0|] [|0uy;255uy|]).IsNone)
        check "sampled evaluator rejects odd Decode without indexing past end"
            ((sampled [|0.0;1.0|] [|0.0;1.0|] [|2|] 8 1 [|0.0;1.0|] [|0.0;1.0;2.0|] [|0uy;255uy|]).IsNone)
        let paired =
            sampled [|0.0;3.0|] [|0.0;2.0;-1.0;1.0|] [|4|] 8 3
                [|0.0;3.0|] [|0.0;1.0;0.0;1.0|]
                [|0uy;255uy;255uy;0uy;255uy;0uy;0uy;255uy|] |> Option.get
        let pairedResult = [|0.0;0.0|]
        check "cubic interleaved output components decode independently"
            (paired [|1.5|] pairedResult && near 1.125 pairedResult.[0] && near -0.125 pairedResult.[1])
        let mixedSamples = [|0uy;255uy;255uy;0uy;255uy;0uy;0uy;255uy|]
        let mixed = sampled [|0.0;3.0;0.0;1.0|] [|0.0;1.0|] [|4;2|] 8 3
                        [|0.0;3.0;0.0;1.0|] [|0.0;1.0|] mixedSamples |> Option.get
        let mixedResult = [|0.0|]
        // At x=3/2 the rows are 9/8 and -1/8; linear y=1/4 yields 13/16.
        check "cubic tensor with short dimension and first-dimension-fast samples"
            (mixed [|1.5;0.25|] mixedResult && near 0.8125 mixedResult.[0])
        let grid = Array.zeroCreate<byte> 16
        for y in 2 .. 3 do
            for x in 1 .. 2 do
                grid.[y * 4 + x] <- 255uy
        let tensor = sampled [|0.0;3.0;0.0;3.0|] [|0.0;2.0|] [|4;4|] 8 3
                        [|0.0;3.0;0.0;3.0|] [|0.0;1.0|] grid |> Option.get
        let tensorResult = [|0.0|]
        // Row x=3/2 gives 9/8, and y=3/2 between 0 and 1 gives 1/2.
        check "cubic tensor interpolates in both dimensions"
            (tensor [|1.5;1.5|] tensorResult && near 0.5625 tensorResult.[0])
        check "truncated samples rejected" ((resolve "/FunctionType 0 /Domain [0 1] /Range [0 1] /Size [2] /BitsPerSample 8" (String [|char 0|])).IsNone))

    test "calculator PDF transfer functions" (fun () ->
        let resolve program =
            let body = $"<</FunctionType 4 /Domain [0 1] /Range [0 1] /Length {String.length program}>>\nstream\n{program}\nendstream"
            let objects = [1, "<</Type /Catalog /Pages 2 0 R>>"; 2, "<</Type /Pages /Kids [3 0 R] /Count 1>>"; 3, "<</Type /Page /Parent 2 0 R /MediaBox [0 0 10 10] /F 5 0 R>>"; 5, body]
            let doc = Pdf.openData (build normal objects 1)
            let obj = Pdf.dictGet doc "F" (Pdf.getPageRefs doc |> List.head) |> Option.get
            Rbt.Pdf.Render.Shading.tryEvaluateFunction doc obj
        let f = resolve "{ dup .5 lt { 2 mul } { pop 3 1 bitshift 6 eq { .75 } { 0 } ifelse } ifelse }" |> Option.get
        check "calculator nested branches and integer bit operations" (near 0.5 (f 0.25) && near 0.75 (f 0.75))
        let fails program =
            match resolve program with
            | None -> true
            | Some f -> try f 0.5 |> ignore; false with _ -> true
        check "calculator rejects bad operand types" (fails "{ true 1 add }")
        let ints = resolve "{ pop 7 3 idiv 2 eq { .4 } { 0 } ifelse }" |> Option.get
        check "calculator integer division and equality" (near 0.4 (ints 0.5))
        let rounded = resolve "{ pop -.5 round 1 add }" |> Option.get
        check "calculator round negative half toward positive infinity" (near 1.0 (rounded 0.5))
        check "calculator rounding preserves real type" (fails "{ pop 3.5 floor 1 bitshift }")
        check "calculator rejects real operands to integer bitshift" (fails "{ pop 3.0 1 bitshift }")
        check "calculator rejects division by zero" (fails "{ 0 div }")
        check "calculator rejects unknown operators" (fails "{ pop bogus }")
        check "calculator bounds stack" (fails ("{ " + String.replicate 1025 "1 " + "}")))

    test "two-dimensional function shading" (fun () ->
        let program = "{ 0 }"
        let fn = $"<</FunctionType 4 /Domain [0 1 0 1] /Range [0 1 0 1 0 1] /Length {program.Length}>>\nstream\n{program}\nendstream"
        let shading = "<</ShadingType 1 /ColorSpace /DeviceRGB /Domain [0 1 0 1] /Matrix [80 0 0 80 10 10] /BBox [10 10 90 90] /Function 6 0 R>>"
        let pixels = renderFixture "/MediaBox [0 0 100 100]" "" "/Shading <</S 5 0 R>>" [5, shading; 6, fn] "/S sh" 1.0 |> rasterize
        let p = pixels.[69 * 100 + 30]
        check "2D function evaluates transformed x and y" (abs (int (Pixel.r p) - 65) < 5 && abs (int (Pixel.g p) - 65) < 5 && Pixel.b p = 0uy)
        check "2D shading respects BBox" (pixels.[95 * 100 + 5] = Pixel.fromColorPremul Color.white)
        let data = [|0;0;0; 255;0;0; 0;255;0; 255;255;0|] |> Array.map char |> String
        let sampled = $"<</FunctionType 0 /Domain [0 1 0 1] /Range [0 1 0 1 0 1] /Size [2 2] /BitsPerSample 8 /Length {data.Length}>>\nstream\n{data}\nendstream"
        let samplePixels = renderFixture "/MediaBox [0 0 100 100]" "" "/Shading <</S 5 0 R>>" [5, shading; 6, sampled] "/S sh" 1.0 |> rasterize
        let q = samplePixels.[29 * 100 + 30]
        check "sampled 2D first dimension varies fastest" (abs (int (Pixel.r q) - 65) < 5 && abs (int (Pixel.g q) - 193) < 5 && Pixel.b q = 0uy))

    test "renderer axial shading domain and extension" (fun () ->
        let resources = "/Shading <</Grad 5 0 R>>"
        let shading = "<</ShadingType 2 /ColorSpace /DeviceRGB /Coords [10 0 90 0] /BBox [0 0 100 20] /Function <</FunctionType 2 /Domain [0 1] /C0 [1 0 0] /C1 [0 0 1] /N 2>>>>"
        let pixels =
            renderFixture "/MediaBox [0 0 300 400]" "" resources [5, shading]
                "1 0 0 1 20 40 cm /Grad sh" 1.0 |> rasterize
        let mid = pixels.[350 * 300 + 70]
        check "nonlinear gradient uses function exponent" (abs (int (Pixel.r mid) - 191) <= 4 && abs (int (Pixel.b mid) - 64) <= 4)
        check "default Extend leaves both sides unpainted"
            (pixels.[350 * 300 + 25] = Pixel.fromColorPremul Color.white && pixels.[350 * 300 + 115] = Pixel.fromColorPremul Color.white)
        check "shading BBox clips perpendicular axis" (pixels.[330 * 300 + 70] = Pixel.fromColorPremul Color.white)
        let extended = shading.Replace("/BBox", "/Extend [true false] /BBox")
        let extendedPixels =
            renderFixture "/MediaBox [0 0 300 400]" "" resources [5, extended]
                "1 0 0 1 20 40 cm /Grad sh" 1.0 |> rasterize
        check "asymmetric Extend pads only start"
            (extendedPixels.[350 * 300 + 25] = Pixel.pack 0uy 0uy 255uy 255uy
             && extendedPixels.[350 * 300 + 115] = Pixel.fromColorPremul Color.white))

    test "renderer tiling pattern anchoring and tint" (fun () ->
        let tile = "0 0 4 10 re f"
        let pattern = $"<</Type /Pattern /PatternType 1 /PaintType 2 /TilingType 1 /BBox [0 0 10 10] /XStep 10 /YStep 10 /Resources <<>> /Length {tile.Length}>>\nstream\n{tile}\nendstream"
        let resources = "/ColorSpace <</P [/Pattern /DeviceRGB]>> /Pattern <</Tiles 5 0 R>>"
        let pixels =
            renderFixture "/MediaBox [0 0 300 400]" "" resources [5, pattern]
                "/P cs 1 0 0 /Tiles scn q 1 0 0 1 3 0 cm 0 20 47 20 re f Q 0 0 1 rg 60 20 10 20 re f" 1.0 |> rasterize
        check "uncolored pattern uses selected red tint" (pixels.[370 * 300 + 12] = Pixel.pack 0uy 0uy 255uy 255uy)
        check "pattern remains anchored across later cm" (pixels.[370 * 300 + 16] = Pixel.fromColorPremul Color.white)
        check "device color operator leaves pattern space" (pixels.[370 * 300 + 65] = Pixel.pack 255uy 0uy 0uy 255uy))

    test "renderer patterned stroke uses stroke shape" (fun () ->
        let tile = "1 0 0 rg 0 0 10 10 re f"
        let pattern = $"<</Type /Pattern /PatternType 1 /PaintType 1 /TilingType 1 /BBox [0 0 10 10] /XStep 10 /YStep 10 /Resources <<>> /Length {tile.Length}>>\nstream\n{tile}\nendstream"
        let pixels =
            renderFixture "/MediaBox [0 0 300 400]" "" "/Pattern <</P 5 0 R>>" [5, pattern]
                "/Pattern CS /P SCN 10 w 20 30 m 80 30 l S" 1.0 |> rasterize
        check "pattern paints stroke interior" (pixels.[370 * 300 + 50] = Pixel.pack 0uy 0uy 255uy 255uy)
        check "pattern excludes stroke exterior" (pixels.[360 * 300 + 50] = Pixel.fromColorPremul Color.white)
        let fractional =
            renderFixture "/MediaBox [0 0 300 400]" "" "/Pattern <</P 5 0 R>>" [5, pattern]
                "/Pattern CS /P SCN 10 w 20 30 m 80 30 l S" 1.25 |> rasterize
        check "fractional zoom leaves no pattern cell seams"
            ([31 .. 93] |> List.forall (fun x -> fractional.[462 * 375 + x] = Pixel.pack 0uy 0uy 255uy 255uy)))

    test "renderer radial shading preserves raster placement" (fun () ->
        let shading = "<</ShadingType 3 /ColorSpace /DeviceRGB /Coords [50 30 0 50 30 20] /Function <</FunctionType 2 /Domain [0 1] /C0 [1 0 0] /C1 [0 0 1] /N 1>>>>"
        let pixels =
            renderFixture "/MediaBox [0 0 100 100]" "" "/Shading <</R 5 0 R>>" [5, shading] "/R sh" 1.0 |> rasterize
        let centre = pixels.[70 * 100 + 50]
        let middle = pixels.[70 * 100 + 60]
        check "radial centre is red at lower page location" (Pixel.r centre > 230uy && Pixel.b centre < 25uy)
        check "radial interpolation reaches midpoint" (abs (int (Pixel.r middle) - 128) < 20 && abs (int (Pixel.b middle) - 128) < 20)
        check "radial default extension leaves exterior white" (pixels.[30 * 100 + 50] = Pixel.fromColorPremul Color.white))

    test "renderer affine image orientation" (fun () ->
        let bytes = "\xff\x00\x00\x00\xff\x00\x00\x00\xff\xff\xff\x00"
        let image = $"<</Type /XObject /Subtype /Image /Width 2 /Height 2 /BitsPerComponent 8 /ColorSpace /DeviceRGB /Length 12>>\nstream\n{bytes}\nendstream"
        let pixels =
            renderFixture "/MediaBox [0 0 300 400]" "" "/XObject <</Im 5 0 R>>" [5, image]
                "q 0 80 -80 0 140 30 cm /Im Do Q" 1.0 |> rasterize
        for x, y, r, g, b in [80,50,255,0,0; 80,65,255,0,0; 80,75,0,255,0; 80,90,0,255,0; 120,50,0,0,255; 120,90,255,255,0] do
            let p = pixels.[(400-y)*300+x]
            check $"rotated image sample {x},{y}"
                (abs (int (Pixel.r p)-r) <= 5 && abs (int (Pixel.g p)-g) <= 5 && abs (int (Pixel.b p)-b) <= 5))

    test "renderer soft mask preserves premultiplied colors" (fun () ->
        let image = "<</Type /XObject /Subtype /Image /Width 1 /Height 1 /BitsPerComponent 8 /ColorSpace /DeviceRGB /SMask 6 0 R /Length 3>>\nstream\n\xff\x00\x00\nendstream"
        let mask = "<</Type /XObject /Subtype /Image /Width 1 /Height 1 /BitsPerComponent 8 /ColorSpace /DeviceGray /Length 1>>\nstream\n\x80\nendstream"
        let pixels =
            renderFixture "/MediaBox [0 0 300 400]" "" "/XObject <</Im 5 0 R>>" [5, image; 6, mask]
                "0 0 1 rg 0 0 100 100 re f q 100 0 0 100 0 0 cm /Im Do Q" 1.0 |> rasterize
        let p = pixels.[350*300+50]
        check "half red over blue is purple without double premultiplication"
            (abs (int (Pixel.r p)-128) <= 2 && Pixel.g p = 0uy && abs (int (Pixel.b p)-127) <= 2))

    test "inline image binary whitespace and dictionaries" (fun () ->
        let pixels =
            renderFixture "/MediaBox [0 0 300 400]" "" "" []
                "q 60 0 0 60 20 20 cm BI /W 1 /H 1 /BPC 8 /CS /RGB /DP << /Predictor 1 >> ID \xff\x00\x0a EI Q" 1.0 |> rasterize
        let p = pixels.[350*300+50]
        check "last whitespace-valued sample survives EI boundary"
            (Pixel.r p = 255uy && Pixel.g p = 0uy && Pixel.b p = 10uy))

    test "text stroke modes and accumulated clipping" (fun () ->
        let render mode suffix =
            renderFixture "/MediaBox [0 0 300 400]" "" testFont []
                $"1 0 0 rg 0 0 1 RG 2 w q BT /F1 60 Tf {mode} Tr 1 0 0 1 20 40 Tm (A) Tj 1 0 0 1 100 40 Tm (B) Tj ET {suffix} Q" 1.0 |> rasterize
        let count predicate x0 x1 (pixels: uint32[]) =
            seq { for y in 290 .. 365 do for x in x0 .. x1 do if predicate pixels.[y*300+x] then yield 1 } |> Seq.sum
        let red p = Pixel.r p > 180uy && Pixel.g p < 50uy && Pixel.b p < 50uy
        let blue p = Pixel.b p > 180uy && Pixel.g p < 50uy && Pixel.r p < 50uy
        let green p = Pixel.g p > 180uy && Pixel.r p < 50uy && Pixel.b p < 50uy
        let outlined = render 1 ""
        check "stroke-only glyphs use stroke rather than fill color" (count blue 10 150 outlined > 100 && count red 10 150 outlined = 0)
        let both = render 2 ""
        check "fill-and-stroke glyphs preserve both paints" (count blue 10 150 both > 100 && count red 10 150 both > 100)
        check "invisible text paints nothing" (render 3 "" |> Array.forall ((=) (Pixel.fromColorPremul Color.white)))
        let clipped = render 7 "0 1 0 rg 0 0 300 400 re f"
        check "ET unions separate text shows before clipping" (count green 15 65 clipped > 150 && count green 95 145 clipped > 150)
        check "text clip excludes inter-glyph gap" (clipped.[350*300+80] = Pixel.fromColorPremul Color.white))

    test "Type3 charprocs use FontMatrix and ignore Tr" (fun () ->
        let glyph = "500 0 d0 0 0 1 rg 0 0 400 700 re f"
        let font = "<</Type /Font /Subtype /Type3 /FontBBox [0 0 500 700] /FontMatrix [0.002 0 0 0.001 0 0] /FirstChar 65 /LastChar 65 /Widths [500] /Encoding <</Differences [65 /A]>> /CharProcs <</A 6 0 R>> /Resources <<>>>>"
        let procedure = $"<</Length {glyph.Length}>>\nstream\n{glyph}\nendstream"
        let pixels =
            renderFixture "/MediaBox [0 0 300 400]" "" "/Font <</F1 5 0 R>>" [5, font; 6, procedure]
                "BT /F1 20 Tf 3 Tr 1 0 0 1 20 40 Tm (AA) Tj ET" 1.0 |> rasterize
        let blue = Pixel.pack 255uy 0uy 0uy 255uy
        check "Type3 paints both glyphs despite invisible Tr" (pixels.[350*300+25] = blue && pixels.[350*300+45] = blue)
        check "Type3 advance includes nonstandard FontMatrix" (pixels.[350*300+38] = Pixel.fromColorPremul Color.white))

    test "documents with matching BaseFont names keep distinct outlines" (fun () ->
        let original = IO.File.ReadAllBytes(IO.Path.Combine(__SOURCE_DIRECTORY__, "../../assets/NotoSans-Regular.ttf"))
        let smaller = Array.copy original
        let u16 offset = (int original.[offset] <<< 8) ||| int original.[offset+1]
        let u32 offset = (u16 offset <<< 16) ||| u16 (offset+2)
        let head =
            [0 .. u16 4 - 1]
            |> List.map (fun i -> 12 + i*16)
            |> List.find (fun offset -> Encoding.ASCII.GetString(original, offset, 4) = "head")
            |> fun offset -> u32 (offset+8)
        let upm = 2 * u16 (head+18)
        smaller.[head+18] <- byte (upm >>> 8)
        smaller.[head+19] <- byte upm
        let height bytes =
            let font = "<</Type /Font /Subtype /TrueType /BaseFont /SharedName /FirstChar 65 /LastChar 65 /Widths [600] /FontDescriptor 6 0 R>>"
            let descriptor = "<</Type /FontDescriptor /FontName /SharedName /Flags 32 /FontFile2 7 0 R>>"
            let stream = $"<</Length {Array.length bytes}>>\nstream\n{text bytes}\nendstream"
            let pixels =
                renderFixture "/MediaBox [0 0 300 400]" "" "/Font <</F1 5 0 R>>" [5,font; 6,descriptor; 7,stream]
                    "BT /F1 60 Tf 20 40 Td (A) Tj ET" 1.0 |> rasterize
            let rows = pixels |> Array.mapi (fun i p -> if Pixel.r p < 128uy then Some (i/300) else None) |> Array.choose id
            if Array.isEmpty rows then 0 else Array.max rows - Array.min rows + 1
        let full, half = height original, height smaller
        check "later document uses its own embedded font units"
            (full > 30 && abs (2*half-full) <= 3))

    test "junk prefix" (fun () ->
        // NV Energy bills: 8 bytes before %PDF, offsets relative to the header.
        assertHello "junk prefix" (build { normal with Prefix = "[BEGIN]\n" } onePage 1))

    test "junk prefix, file-relative offsets" (fun () ->
        assertHello "junk prefix, file-relative offsets"
            (build { normal with Prefix = "[BEGIN]\n"; OffsetsIncludePrefix = true } onePage 1))

    test "header beyond 1 KB" (fun () ->
        let bytes = build { normal with Prefix = String('x', 1100) } onePage 1
        let refused = try Pdf.openData bytes |> ignore; false with ex -> ex.Message.Contains "Not a PDF"
        check "header beyond 1 KB is refused" refused)

    test "trailer on the xref line" (fun () ->
        // Henderson water bills: "trailer <</Size 57 /Root 56 0 R>>" on one line.
        assertHello "trailer on the xref line" (build { normal with TrailerOnXRefLine = true } onePage 1))

    test "no xref" (fun () ->
        assertHello "no xref (reconstructed)" (build { normal with OmitXRef = true } onePage 1))

    test "garbled offsets" (fun () ->
        assertHello "garbled offsets (repaired)" (build { normal with GarbleOffsets = true } onePage 1))

    test "bogus startxref" (fun () ->
        assertHello "bogus startxref (reconstructed)" (build { normal with StartXRefBogus = true } onePage 1))

    test "garbled xref section" (fun () ->
        let bytes = build normal onePage 1 |> text
        let broken = bytes.Replace("0000000000 65535 f\r\n", "0000000000 65535 f\r\nthis is not an xref entry\r\n")
        assertHello "garbled xref section (reconstructed)" (latin1 broken))

    test "inherited page attributes" (fun () ->
        // MediaBox, Resources and Rotate live on the Pages node, not the page.
        let objects =
            [ 1, "<</Type /Catalog /Pages 2 0 R>>"
              2, "<</Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 200 100] /Rotate 90 /Resources <</Font <</F1 5 0 R>>>>>>"
              3, "<</Type /Page /Parent 2 0 R /Contents 4 0 R>>"
              4, "<</Length 18>>\nstream\nBT (Hello) Tj ET\nendstream"
              5, "<</Type /Font /Subtype /Type1 /BaseFont /Helvetica>>" ]
        let doc = Pdf.openData (build normal objects 1)
        let check' label (copy: PdfDocument) =
            let copy = copy |> Operations.toBytes |> Pdf.openData
            let page = Pdf.getPageRefs copy |> List.head
            let d = match Pdf.deref copy page with PdfDict d -> d | _ -> Map.empty
            check $"{label}: MediaBox on the page" (Map.tryFind "MediaBox" d = Some (PdfArray [ PdfInt 0L; PdfInt 0L; PdfInt 200L; PdfInt 100L ]))
            check $"{label}: Rotate on the page" (Map.tryFind "Rotate" d = Some (PdfInt 90L))
            let font =
                Map.tryFind "Resources" d
                |> Option.bind (fun r -> Pdf.dictGet copy "Font" r)
                |> Option.bind (fun f -> Pdf.dictGet copy "F1" f)
                |> Option.map (Pdf.deref copy)
            check $"{label}: Resources font copied" (match font with Some (PdfDict f) -> Map.tryFind "BaseFont" f = Some (PdfName "Helvetica") | _ -> false)
        check' "merge" (Operations.merge [ doc ])
        check' "extractPages" (Operations.extractPages doc [ 1 ]))

    test "annotation cycle and long chains" (fun () ->
        // An annotation's /P points back at its page, and a 20,000-link chain
        // of objects hangs off the page: neither may loop or overflow the stack.
        let chain = 20_000
        let objects =
            [ yield 1, "<</Type /Catalog /Pages 2 0 R>>"
              yield 2, "<</Type /Pages /Kids [3 0 R 6 0 R] /Count 2>>"
              yield 3, "<</Type /Page /Parent 2 0 R /MediaBox [0 0 300 400] /Contents 4 0 R /Annots [5 0 R] /Chain 10 0 R>>"
              yield 4, "<</Length 18>>\nstream\nBT (Hello) Tj ET\nendstream"
              yield 5, "<</Type /Annot /Subtype /Link /Rect [0 0 10 10] /P 3 0 R /Dest [6 0 R /Fit]>>"
              yield 6, "<</Type /Page /Parent 2 0 R /MediaBox [0 0 300 400]>>"
              for i in 0 .. chain - 1 do
                  let next = if i = chain - 1 then "null" else $"{11 + i} 0 R"
                  yield 10 + i, $"<</Next {next} /Up 3 0 R>>" ]
        let doc = Pdf.openData (build normal objects 1)
        // Page 1 only: the link to page 2 must not drag page 2 in.
        let copy = Operations.extractPages doc [ 1 ] |> Operations.toBytes |> Pdf.openData
        let pages = Pdf.getPageRefs copy
        check "cycle: one page" (pages.Length = 1)
        let page = pages.Head
        let annot = Pdf.dictGet copy "Annots" page |> Option.map (Pdf.deref copy)
        match annot with
        | Some (PdfArray [ a ]) ->
            check "cycle: annotation /P is the copied page" (Pdf.dictGet copy "P" a = Some page)
            check "cycle: link to an uncopied page is null" (Pdf.dictGet copy "Dest" a = Some (PdfArray [ PdfNull; PdfName "Fit" ]))
        | _ -> check "cycle: annotation copied" false
        let rec chainLength n (node: PdfObj option) =
            match node |> Option.map (Pdf.deref copy) with
            | Some (PdfDict d) -> chainLength (n + 1) (Map.tryFind "Next" d)
            | _ -> n
        check "cycle: whole chain copied" (chainLength 0 (Pdf.dictGet copy "Chain" page) = chain))

    test "stream trailing newlines" (fun () ->
        // Stream data that itself ends in CR/LF bytes, with an indirect /Length
        // (the parser must not trim the data's own newlines) and with a direct one.
        let data = "x\x9c\n\r\n"  // arbitrary binary ending in LF CR LF
        let objects =
            [ 1, "<</Type /Catalog /Pages 2 0 R>>"
              2, "<</Type /Pages /Kids [3 0 R] /Count 1>>"
              3, "<</Type /Page /Parent 2 0 R /MediaBox [0 0 300 400] /Resources <</XObject <</A 4 0 R /B 6 0 R>>>>>>"
              4, $"<</Length 5 0 R>>\nstream\n{data}\nendstream"
              5, string data.Length
              6, $"<</Length {data.Length}>>\nstream\n{data}\nendstream" ]
        let raw (doc: PdfDocument) name =
            let page = Pdf.getPageRefs doc |> List.head
            Pdf.getPageInheritable doc page "Resources"
            |> Option.bind (Pdf.dictGet doc "XObject")
            |> Option.bind (Pdf.dictGet doc name)
            |> Option.map (Pdf.deref doc)
            |> function Some (PdfStream (_, d)) -> text d | _ -> "<none>"
        let doc = Pdf.openData (build normal objects 1)
        check "stream: indirect /Length keeps trailing newlines" (raw doc "A" = data)
        check "stream: direct /Length keeps trailing newlines" (raw doc "B" = data)
        let copy = Operations.merge [ doc ] |> Operations.toBytes |> Pdf.openData
        check "stream: indirect /Length survives merge" (raw copy "A" = data)
        check "stream: direct /Length survives merge" (raw copy "B" = data))

    test "stream with a wrong /Length" (fun () ->
        let objects =
            [ 1, "<</Type /Catalog /Pages 2 0 R>>"
              2, "<</Type /Pages /Kids [3 0 R] /Count 1>>"
              3, "<</Type /Page /Parent 2 0 R /MediaBox [0 0 300 400] /Contents 4 0 R>>"
              4, "<</Length 99>>\nstream\nBT (Hello) Tj ET\nendstream" ]
        assertHello "wrong /Length" (build normal objects 1))

    if failures > 0 then
        printfn "%d failure(s)" failures
        1
    else
        printfn "all passed"
        0
