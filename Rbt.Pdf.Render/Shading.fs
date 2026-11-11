module Rbt.Pdf.Render.Shading

open System
open Rbt.Pdf
open Rbt.Renderer

let private clamp lo hi x = max lo (min hi x)
let private finite x = not (Double.IsNaN x || Double.IsInfinity x)

let private number doc obj =
    match Pdf.deref doc obj with
    | PdfInt n -> Some (float n)
    | PdfReal n when finite n -> Some n
    | _ -> None

let private sequence xs =
    let rec loop acc = function
        | [] -> Some (List.rev acc)
        | Some x :: rest -> loop (x :: acc) rest
        | None :: _ -> None
    loop [] xs

let private numbers doc obj =
    match Pdf.deref doc obj with
    | PdfArray xs -> xs |> List.map (number doc) |> sequence |> Option.map List.toArray
    | _ -> None

let private field doc key obj = Pdf.dictGet doc key obj
let private numberField doc key obj = field doc key obj |> Option.bind (number doc)
let private arrayField doc key obj = field doc key obj |> Option.bind (numbers doc)

exception private FunctionEvaluationError

type private Function =
    { Eval: float -> float[]
      EvalN: float[] -> float[]
      Inputs: int
      Outputs: int
      Breaks: float list }

let rec private parseFunction doc depth obj : Function option =
    if depth > 16 then None else
    let validPairs (a: float[]) =
        a.Length > 0 && a.Length % 2 = 0 &&
        ([0 .. 2 .. a.Length - 2] |> List.forall (fun i -> a.[i] <= a.[i+1]))
    let domain = arrayField doc "Domain" obj
    let range = match field doc "Range" obj with None -> Some None | Some x -> numbers doc x |> Option.map Some
    match domain, range with
    | Some d, Some r when validPairs d && (r |> Option.forall validPairs) ->
        let inputs = d.Length / 2
        let wrap outputs (eval: float[] -> float[] -> bool) breaks =
            let clipped = Array.zeroCreate<float> inputs
            let values = Array.zeroCreate<float> outputs
            let single = Array.zeroCreate<float> 1
            let run (xs: float[]) =
                if xs.Length <> inputs then raise FunctionEvaluationError
                for i in 0 .. inputs-1 do
                    clipped.[i] <- clamp d.[2*i] d.[2*i+1] xs.[i]
                if not (eval clipped values) || not (Array.forall finite values) then raise FunctionEvaluationError
                match r with
                | Some limits ->
                    for i in 0 .. outputs-1 do values.[i] <- clamp limits.[2*i] limits.[2*i+1] values.[i]
                | None -> ()
                values
            { Eval = (fun x -> single.[0] <- x; run single)
              EvalN = run; Inputs = inputs; Outputs = outputs; Breaks = breaks }
        match numberField doc "FunctionType" obj with
        | Some 0.0 ->
            match r, arrayField doc "Size" obj, numberField doc "BitsPerSample" obj with
            | Some limits, Some sizes, Some bits when
                sizes.Length = inputs && Array.forall (fun n -> n >= 1.0 && n <= float Int32.MaxValue && n = floor n) sizes &&
                bits = floor bits && bits >= 0.0 && bits <= 32.0 ->
                let size = sizes |> Array.map int
                let encode = match field doc "Encode" obj with None -> Some (Array.init (inputs*2) (fun i -> if i % 2 = 0 then 0.0 else float (size.[i/2]-1))) | Some x -> numbers doc x
                let decode = match field doc "Decode" obj with None -> Some limits | Some x -> numbers doc x
                let order = match field doc "Order" obj with None -> Some 1.0 | Some x -> number doc x
                match encode, decode, order with
                | Some enc, Some dec, Some ord when enc.Length = inputs*2 && dec.Length = limits.Length && ord = floor ord && ord >= 0.0 && ord <= 3.0 ->
                    let data = try Some (Pdf.getStreamData doc obj) with _ -> None
                    data |> Option.bind (PdfSampled.create d limits size (int bits) (int ord) enc dec)
                    |> Option.map (fun eval -> wrap (limits.Length/2) eval [])
                | _ -> None
            | _ -> None
        | Some 4.0 ->
            match r with
            | Some limits ->
                let data = try Some (Pdf.getStreamData doc obj) with _ -> None
                data |> Option.bind (fun bytes -> PdfCalculator.parse bytes (limits.Length/2))
                |> Option.map (fun eval -> wrap (limits.Length/2) eval [])
            | None -> None
        | Some 2.0 when inputs = 1 && d.[0] < d.[1] ->
            let c0 = match field doc "C0" obj with None -> Some [|0.0|] | Some x -> numbers doc x
            let c1 = match field doc "C1" obj with None -> Some [|1.0|] | Some x -> numbers doc x
            match numberField doc "N" obj, c0, c1 with
            | Some n, Some c0, Some c1 when c0.Length > 0 && c0.Length = c1.Length && n > 0.0 && (d.[0] >= 0.0 || n = floor n) && (r |> Option.forall (fun a -> a.Length = c0.Length * 2)) ->
                let eval (x: float[]) (result: float[]) =
                    let t = Math.Pow(x.[0], n)
                    for i in 0 .. result.Length-1 do result.[i] <- c0.[i] + t * (c1.[i] - c0.[i])
                    true
                Some (wrap c0.Length eval [])
            | _ -> None
        | Some 3.0 when inputs = 1 && d.[0] < d.[1] ->
            match field doc "Functions" obj |> Option.map (Pdf.deref doc), arrayField doc "Bounds" obj, arrayField doc "Encode" obj with
            | Some (PdfArray fs), Some bounds, Some encode when not fs.IsEmpty && bounds.Length = fs.Length - 1 && encode.Length = fs.Length * 2 ->
                let edges = Array.concat [ [|d.[0]|]; bounds; [|d.[1]|] ]
                if not ([0 .. edges.Length - 2] |> List.forall (fun i -> edges.[i] < edges.[i+1])) then None else
                match fs |> List.map (parseFunction doc (depth + 1)) |> sequence with
                | None -> None
                | Some parsed ->
                    let funcs = List.toArray parsed
                    let eval (xs: float[]) (result: float[]) =
                        let x = xs.[0]
                        let mutable i = 0
                        while i < bounds.Length && x >= bounds.[i] do i <- i + 1
                        let t = (x - edges.[i]) / (edges.[i+1] - edges.[i])
                        let values = funcs.[i].Eval (encode.[2*i] + t * (encode.[2*i+1] - encode.[2*i]))
                        Array.Copy(values, result, result.Length)
                        true
                    let nested =
                        [ for i in 0 .. funcs.Length - 1 do
                            let a, b = encode.[2*i], encode.[2*i+1]
                            if a <> b then
                                for v in funcs.[i].Breaks do
                                    let t = (v - a) / (b - a)
                                    if t > 0.0 && t < 1.0 then yield edges.[i] + t * (edges.[i+1] - edges.[i]) ]
                    let outputs = funcs.[0].Outputs
                    if (funcs |> Array.exists (fun f -> f.Inputs <> 1 || f.Outputs <> outputs)) || (r |> Option.exists (fun a -> a.Length <> outputs * 2)) then None
                    else Some (wrap outputs eval (Array.toList bounds @ nested))
            | _ -> None
        | _ -> None
    | _ -> None

let private parseFunctions doc obj =
    match Pdf.deref doc obj with
    | PdfArray fs when not fs.IsEmpty ->
        match fs |> List.map (parseFunction doc 0) |> sequence with
        | Some (first :: _ as funcs) when funcs |> List.forall (fun f -> f.Inputs = first.Inputs && f.Outputs = 1) ->
            let funcs = List.toArray funcs
            let result = Array.zeroCreate<float> funcs.Length
            let single = Array.zeroCreate<float> 1
            let eval xs =
                for i in 0 .. funcs.Length-1 do result.[i] <- (funcs.[i].EvalN xs).[0]
                result
            Some { Eval = (fun x -> single.[0] <- x; eval single)
                   EvalN = eval; Inputs = first.Inputs; Outputs = funcs.Length
                   Breaks = funcs |> Array.toList |> List.collect (fun f -> f.Breaks) }
        | _ -> None
    | _ -> parseFunction doc 0 obj
/// Resolve a PDF function with one input and one output, for soft-mask transfer.
let tryEvaluateFunction (doc: PdfDocument) (obj: PdfObj) : (float -> float) option =
    parseFunctions doc obj
    |> Option.bind (fun f ->
        if f.Inputs <> 1 || f.Outputs <> 1 then None
        else
            let gate = System.Object()
            Some (fun x ->
#if FABLE_COMPILER
                (f.Eval x).[0]
#else
                System.Threading.Monitor.Enter gate
                try (f.Eval x).[0]
                finally System.Threading.Monitor.Exit gate
#endif
            ))

// Intersect a convex polygon with a*x+b*y+c >= 0, in shading space.
let private clip (a: float) (b: float) (c: float) (polygon: (float * float) list) =
    let side (x, y) = a*x + b*y + c
    let intersect (x0, y0) (x1, y1) s0 s1 =
        let t = s0 / (s0 - s1)
        (x0 + t * (x1-x0), y0 + t * (y1-y0))
    match polygon with
    | [] -> []
    | _ ->
        let mutable prev = List.last polygon
        let mutable sp = side prev
        [ for current in polygon do
            let sc = side current
            if sc >= 0.0 then
                if sp < 0.0 then yield intersect prev current sp sc
                yield current
            elif sp >= 0.0 then yield intersect prev current sp sc
            prev <- current
            sp <- sc ]

type private MeshVertex = { X: float; Y: float; C: float[] }

// Track the actual mesh bit count separately: the shared reader prefetches and
// pads past EOF, and its individual reads are limited to 24 bits.
type private BitReader(data: byte[]) =
    let reader = Rbt.Image.Codecs.MsbBitReader(data)
    let mutable pos = 0
    member _.Remaining = data.Length * 8 - pos
    member _.Align() =
        reader.AlignToByte()
        pos <- (pos + 7) &&& ~~~7
    member _.Read(n: int) =
        pos <- pos + n
        if n <= 24 then float (reader.Read n)
        else float (reader.Read 16) * 65536.0 + float (reader.Read (n-16))

let private packedColor toTarget space alpha (v: float[]) =
    let struct(r,g,b) = toTarget space v
    let a = byte (clamp 0.0 255.0 (alpha * 255.0 + 0.5))
    let premul c = byte ((int c * int a + 127) / 255)
    Pixel.pack (premul b) (premul g) (premul r) a

// Rasterization is in page space, so the result also handles sheared shading
// matrices. The transparent image is bounded to the visible page and BBox.
let private rasterBounds (ctm: Matrix3x2) pageBounds bbox scale =
    let xmin, ymin, xmax, ymax = pageBounds
    let det = ctm.M11*ctm.M22 - ctm.M12*ctm.M21
    if abs det < 1e-12 || xmin >= xmax || ymin >= ymax then None else
    let inv = Matrix3x2.inverse ctm
    let mutable x0 = xmin
    let mutable y0 = ymin
    let mutable x1 = xmax
    let mutable y1 = ymax
    match bbox with
    | Some (b: float[]) ->
        let points = [| for x in [b.[0]; b.[2]] do for y in [b.[1]; b.[3]] do yield Matrix3x2.transformVec2 ctm (vec2 x y) |]
        x0 <- max x0 (points |> Array.minBy (fun p -> p.X) |> fun p -> float p.X)
        y0 <- max y0 (points |> Array.minBy (fun p -> p.Y) |> fun p -> float p.Y)
        x1 <- min x1 (points |> Array.maxBy (fun p -> p.X) |> fun p -> float p.X)
        y1 <- min y1 (points |> Array.maxBy (fun p -> p.Y) |> fun p -> float p.Y)
    | None -> ()
    if x0 >= x1 || y0 >= y1 then None else
    let s = if finite scale && scale > 0.0 then scale else 1.0
    let s = min s (sqrt (4000000.0 / ((x1-x0)*(y1-y0))))
    let w = max 1 (int (ceil ((x1-x0)*s)))
    let h = max 1 (int (ceil ((y1-y0)*s)))
    Some (inv, x0, y0, x1, y1, w, h)

let private rasterCommand x0 y0 x1 y1 w h pixels =
    let m = { M11 = x1-x0; M12 = 0.0; M21 = 0.0; M22 = y1-y0; DX = x0; DY = y0 }
    Group (m, 1.0, [BitmapPaint.command pixels w h true])

let private inBBox bbox x y =
    match bbox with
    | None -> true
    | Some (b: float[]) -> x >= b.[0] && x <= b.[2] && y >= b.[1] && y <= b.[3]

let private functionShading toTarget ctm alpha pageBounds scale bbox space (domain: float[]) (matrix: Matrix3x2) (func: Function) =
    match rasterBounds ctm pageBounds bbox scale with
    | None -> Some (Group (Matrix3x2.identity, 1.0, []))
    | Some (inv, x0, y0, x1, y1, w, h) ->
        let inverseMatrix = Matrix3x2.inverse matrix
        let pixels = Array.zeroCreate<uint32> (w*h)
        let input = Array.zeroCreate<float> 2
        for py in 0 .. h-1 do
            let y = y1 - (float py + 0.5) * (y1-y0) / float h
            for px in 0 .. w-1 do
                let x = x0 + (float px + 0.5) * (x1-x0) / float w
                let p = Matrix3x2.transformVec2 inv (vec2 x y)
                if inBBox bbox (float p.X) (float p.Y) then
                    let q = Matrix3x2.transformVec2 inverseMatrix p
                    let u, v = float q.X, float q.Y
                    if u >= domain.[0] && u <= domain.[1] && v >= domain.[2] && v <= domain.[3] then
                        input.[0] <- u
                        input.[1] <- v
                        pixels.[py*w+px] <- packedColor toTarget space alpha (func.EvalN input)
        Some (rasterCommand x0 y0 x1 y1 w h pixels)

let private radial toTarget ctm alpha pageBounds scale bbox space (coords: float[]) (domain: float[]) (extend0, extend1) (func: Function) =
    let components = PdfColor.components space
    match rasterBounds ctm pageBounds bbox scale with
    | None -> Some (Group (Matrix3x2.identity, 1.0, []))
    | Some (inv, x0, y0, x1, y1, w, h) ->
        let pixels = Array.zeroCreate<uint32> (w*h)
        let cx, cy, r = coords.[0], coords.[1], coords.[2]
        let dx, dy, dr = coords.[3]-cx, coords.[4]-cy, coords.[5]-r
        let a = dx*dx + dy*dy - dr*dr
        for py in 0 .. h-1 do
            let y = y1 - (float py + 0.5) * (y1-y0) / float h
            for px in 0 .. w-1 do
                let x = x0 + (float px + 0.5) * (x1-x0) / float w
                let p = Matrix3x2.transformVec2 inv (vec2 x y)
                let x, y = float p.X, float p.Y
                if inBBox bbox x y then
                    let qx, qy = x-cx, y-cy
                    let b = -2.0 * (qx*dx + qy*dy + r*dr)
                    let c = qx*qx + qy*qy - r*r
                    // Later circles paint over earlier circles where the cone overlaps.
                    let t0, t1 =
                        if abs a < 1e-12 then
                            (if abs b >= 1e-12 then -c/b else Double.NegativeInfinity), Double.NegativeInfinity
                        else
                            let disc = b*b - 4.0*a*c
                            if disc < 0.0 then Double.NegativeInfinity, Double.NegativeInfinity
                            else
                                let d = sqrt disc
                                (-b-d)/(2.0*a), (-b+d)/(2.0*a)
                    let mutable best = Double.NegativeInfinity
                    if finite t0 && r+t0*dr >= -1e-9 && (t0 >= 0.0 || extend0) && (t0 <= 1.0 || extend1) then best <- t0
                    if finite t1 && r+t1*dr >= -1e-9 && (t1 >= 0.0 || extend0) && (t1 <= 1.0 || extend1) && t1 > best then best <- t1
                    if best <> Double.NegativeInfinity then
                        let t = clamp 0.0 1.0 best
                        let values = func.Eval (domain.[0] + t*(domain.[1]-domain.[0]))
                        if values.Length = components then pixels.[py*w+px] <- packedColor toTarget space alpha values
        Some (rasterCommand x0 y0 x1 y1 w h pixels)

let private mesh toTarget doc sh ctm alpha pageBounds scale bbox space (func: Function option) kind =
    let components = PdfColor.components space
    let integer key = numberField doc key sh |> Option.bind (fun x -> if x = floor x then Some (int x) else None)
    let data = try Some (Pdf.getStreamData doc sh) with _ -> None
    let nc = if func.IsSome then 1 else components
    match integer "BitsPerCoordinate", integer "BitsPerComponent", arrayField doc "Decode" sh, data with
    | Some bc, Some bv, Some decode, Some data when
        List.contains bc [1;2;4;8;12;16;24;32] && List.contains bv [1;2;4;8;12;16] && decode.Length = 4 + 2*nc ->
        let flagBits = if kind = 5 then Some 0 else integer "BitsPerFlag"
        let row = if kind = 5 then integer "VerticesPerRow" else Some 0
        match flagBits, row, rasterBounds ctm pageBounds bbox scale with
        | Some bf, Some row, Some (inv, x0, y0, x1, y1, w, h) when (kind = 5 && row >= 2) || (kind <> 5 && List.contains bf [2;4;8]) ->
            let pixels = Array.zeroCreate<uint32> (w*h)
            let reader = BitReader(data)
            let sample bits lo hi = lo + reader.Read(bits) * (hi-lo) / (Math.Pow(2.0, float bits)-1.0)
            let point () = sample bc decode.[0] decode.[1], sample bc decode.[2] decode.[3]
            let colors () = Array.init nc (fun i -> sample bv decode.[4+2*i] decode.[5+2*i])
            let vertex () = let x,y = point () in { X=x; Y=y; C=colors () }
            let transformed (v: MeshVertex) =
                let p = Matrix3x2.transformVec2 ctm (vec2 v.X v.Y)
                (float p.X-x0)*float w/(x1-x0), (y1-float p.Y)*float h/(y1-y0)
            let mutable triangleCount = 0
            let triangle (v0: MeshVertex) (v1: MeshVertex) (v2: MeshVertex) =
                triangleCount <- triangleCount + 1
                let ax,ay = transformed v0
                let bx,by = transformed v1
                let cx,cy = transformed v2
                let area = (bx-ax)*(cy-ay)-(by-ay)*(cx-ax)
                if abs area > 1e-12 then
                    let left = max 0 (int (floor (min ax (min bx cx))))
                    let right = min (w-1) (int (ceil (max ax (max bx cx))))
                    let top = max 0 (int (floor (min ay (min by cy))))
                    let bottom = min (h-1) (int (ceil (max ay (max by cy))))
                    for py in top .. bottom do
                        for px in left .. right do
                            let x,y = float px+0.5, float py+0.5
                            let b = ((x-ax)*(cy-ay)-(y-ay)*(cx-ax))/area
                            let c = ((bx-ax)*(y-ay)-(by-ay)*(x-ax))/area
                            let a = 1.0-b-c
                            if a >= -1e-8 && b >= -1e-8 && c >= -1e-8 then
                                let p = Matrix3x2.transformVec2 inv (vec2 (x0+x*(x1-x0)/float w) (y1-y*(y1-y0)/float h))
                                if inBBox bbox (float p.X) (float p.Y) then
                                    let values = Array.init nc (fun i -> a*v0.C.[i]+b*v1.C.[i]+c*v2.C.[i])
                                    let values = match func with Some f -> f.Eval values.[0] | None -> values
                                    if values.Length = components then pixels.[py*w+px] <- packedColor toTarget space alpha values
            let mutable ok = true
            if kind = 4 || kind = 5 then
                let vertices = ResizeArray<MeshVertex>()
                let recordBits = bf + 2*bc + nc*bv
                let mutable previous : (MeshVertex * MeshVertex * MeshVertex) option = None
                while reader.Remaining >= recordBits && ok do
                    let flag = if kind = 4 then int (reader.Read bf) else 0
                    let v = vertex ()
                    reader.Align()
                    if kind = 5 then vertices.Add v
                    elif flag = 0 then
                        let alignedBits = ((recordBits + 7) / 8) * 8
                        if reader.Remaining < alignedBits + recordBits then ok <- false else
                        reader.Read bf |> ignore
                        let v1 = vertex ()
                        reader.Align()
                        reader.Read bf |> ignore
                        let v2 = vertex ()
                        reader.Align()
                        triangle v v1 v2
                        previous <- Some (v, v1, v2)
                    elif flag = 1 || flag = 2 then
                        match previous with
                        | Some (a,b,c) ->
                            let first = if flag = 1 then b else a
                            triangle first c v
                            previous <- Some (first, c, v)
                        | None -> ok <- false
                    else ok <- false
                if kind = 5 then
                    let rows = vertices.Count / row
                    for j in 0 .. rows-2 do
                        for i in 0 .. row-2 do
                            let a,b,c,d = vertices.[j*row+i], vertices.[j*row+i+1], vertices.[(j+1)*row+i], vertices.[(j+1)*row+i+1]
                            triangle a b c
                            triangle b d c
            else
                let mutable previousPoints : (float*float)[] = [||]
                let mutable previousColors : float[][] = [||]
                let totalPoints = if kind = 6 then 12 else 16
                let mutable run = true
                while run && ok && reader.Remaining >= bf do
                    let flag = int (reader.Read bf)
                    let count = if flag = 0 then totalPoints else totalPoints-4
                    let colorCount = if flag = 0 then 4 else 2
                    if flag > 3 || (flag <> 0 && previousPoints.Length = 0) || reader.Remaining < count*2*bc + colorCount*nc*bv then
                        ok <- false
                        run <- false
                    else
                        let points = Array.zeroCreate<float*float> totalPoints
                        let cs = Array.zeroCreate<float[]> 4
                        let start = if flag = 0 then 0 else 4
                        if flag <> 0 then
                            for i in 0 .. 3 do points.[i] <- previousPoints.[(flag*3+i)%12]
                            cs.[0] <- previousColors.[flag]
                            cs.[1] <- previousColors.[(flag+1)%4]
                        for i in start .. totalPoints-1 do points.[i] <- point ()
                        for i in (if flag = 0 then 0 else 2) .. 3 do cs.[i] <- colors ()
                        reader.Align()
                        previousPoints <- points
                        previousColors <- cs
                        let bez a b c d t =
                            let s = 1.0-t
                            s*s*s*a + 3.0*s*s*t*b + 3.0*s*t*t*c + t*t*t*d
                        let coord axis u v =
                            let p i = if axis = 0 then fst points.[i] else snd points.[i]
                            if kind = 6 then
                                let top = bez (p 0) (p 1) (p 2) (p 3) u
                                let bottom = bez (p 9) (p 8) (p 7) (p 6) u
                                let left = bez (p 0) (p 11) (p 10) (p 9) v
                                let right = bez (p 3) (p 4) (p 5) (p 6) v
                                (1.0-v)*top+v*bottom+(1.0-u)*left+u*right -
                                    ((1.0-u)*(1.0-v)*p 0+u*(1.0-v)*p 3+u*v*p 6+(1.0-u)*v*p 9)
                            else
                                let a = bez (p 0) (p 1) (p 2) (p 3) u
                                let b = bez (p 11) (p 12) (p 13) (p 4) u
                                let c = bez (p 10) (p 15) (p 14) (p 5) u
                                let d = bez (p 9) (p 8) (p 7) (p 6) u
                                bez a b c d v
                        let at u v =
                            { X=coord 0 u v; Y=coord 1 u v
                              C=Array.init nc (fun i -> (1.0-u)*(1.0-v)*cs.[0].[i]+u*(1.0-v)*cs.[1].[i]+u*v*cs.[2].[i]+(1.0-u)*v*cs.[3].[i]) }
                        // A shared regular parameter grid makes adjacent triangles watertight.
                        let n = 16
                        let grid = Array.init (n+1) (fun j -> Array.init (n+1) (fun i -> at (float i/float n) (float j/float n)))
                        for j in 0 .. n-1 do
                            for i in 0 .. n-1 do
                                triangle grid.[j].[i] grid.[j].[i+1] grid.[j+1].[i]
                                triangle grid.[j].[i+1] grid.[j+1].[i+1] grid.[j+1].[i]
            if ok && triangleCount > 0 then Some (rasterCommand x0 y0 x1 y1 w h pixels) else None
        | _, _, None -> Some (Group (Matrix3x2.identity, 1.0, []))
        | _ -> None
    | _ -> None

/// Render a shading object in PDF page coordinates.
let private renderObject (toTarget: PdfColor.Space -> float[] -> struct(byte*byte*byte)) (doc: PdfDocument) (resources: PdfObj) (shading: PdfObj) (ctm: Matrix3x2) (alpha: float) (pageBounds: float * float * float * float) (scale: float) : Cmd option =
    let shading = Some shading
    match shading with
    | None -> None
    | Some sh ->
        let colorSpace = field doc "ColorSpace" sh |> Option.bind (PdfColor.resolve doc resources)
        let kind = numberField doc "ShadingType" sh
        let domain = match field doc "Domain" sh with None -> Some (if kind = Some 1.0 then [|0.0; 1.0; 0.0; 1.0|] else [|0.0; 1.0|]) | Some x -> numbers doc x
        let bbox = match field doc "BBox" sh with None -> Some None | Some x -> numbers doc x |> Option.map Some
        let extend =
            match field doc "Extend" sh with
            | None -> Some (false, false)
            | Some x ->
                match Pdf.deref doc x with
                | PdfArray [a; b] ->
                    match Pdf.deref doc a, Pdf.deref doc b with PdfBool a, PdfBool b -> Some (a, b) | _ -> None
                | _ -> None
        let func = field doc "Function" sh |> Option.bind (parseFunctions doc)
        match kind, colorSpace, arrayField doc "Coords" sh, domain, bbox, extend, func with
        | Some 1.0, Some space, _, Some domain, Some bbox, _, Some func
            when domain.Length = 4 && domain.[0] < domain.[1] && domain.[2] < domain.[3] &&
                 func.Inputs = 2 && func.Outputs = PdfColor.components space &&
                 (bbox |> Option.forall (fun b -> b.Length = 4 && b.[0] <= b.[2] && b.[1] <= b.[3])) ->
            let matrix = match field doc "Matrix" sh with None -> Some [|1.0;0.0;0.0;1.0;0.0;0.0|] | Some x -> numbers doc x
            match matrix with
            | Some m when m.Length = 6 && abs (m.[0]*m.[3]-m.[1]*m.[2]) >= 1e-12 ->
                let m = { M11=m.[0]; M12=m.[1]; M21=m.[2]; M22=m.[3]; DX=m.[4]; DY=m.[5] }
                functionShading toTarget ctm alpha pageBounds scale bbox space domain m func
            | _ -> None
        | Some 3.0, Some space, Some coords, Some domain, Some bbox, Some ext, Some func
            when coords.Length = 6 && coords.[2] >= 0.0 && coords.[5] >= 0.0 &&
                 domain.Length = 2 && domain.[0] < domain.[1] &&
                 (bbox |> Option.forall (fun b -> b.Length = 4 && b.[0] <= b.[2] && b.[1] <= b.[3])) &&
                 func.Inputs = 1 && func.Outputs = PdfColor.components space ->
            radial toTarget ctm alpha pageBounds scale bbox space coords domain ext func
        | Some kind, Some space, _, _, Some bbox, _, func
            when kind >= 4.0 && kind <= 7.0 && kind = floor kind &&
                 (bbox |> Option.forall (fun b -> b.Length = 4 && b.[0] <= b.[2] && b.[1] <= b.[3])) &&
                 (field doc "Function" sh |> Option.isNone || (func |> Option.exists (fun f -> f.Inputs = 1 && f.Outputs = PdfColor.components space))) ->
            mesh toTarget doc sh ctm alpha pageBounds scale bbox space func (int kind)
        | Some 2.0, Some space, Some coords, Some domain, Some bbox, Some (extend0, extend1), Some func
            when coords.Length = 4 && domain.Length = 2 && domain.[0] < domain.[1] && func.Inputs = 1 && func.Outputs = PdfColor.components space && (bbox |> Option.forall (fun b -> b.Length = 4 && b.[0] <= b.[2] && b.[1] <= b.[3])) ->
            let x0, y0, x1, y1 = coords.[0], coords.[1], coords.[2], coords.[3]
            let dx, dy = x1-x0, y1-y0
            let len2 = dx*dx + dy*dy
            let det = ctm.M11*ctm.M22 - ctm.M12*ctm.M21
            let xmin, ymin, xmax, ymax = pageBounds
            let sample = func.Eval domain.[0]
            if len2 < 1e-20 || abs det < 1e-10 || sample.Length <> PdfColor.components space || xmin >= xmax || ymin >= ymax then None else
            let inv = Matrix3x2.inverse ctm
            let local x y =
                let p = Matrix3x2.transformVec2 inv (vec2 x y)
                float p.X, float p.Y
            let mutable poly = [local xmin ymin; local xmax ymin; local xmax ymax; local xmin ymax]
            match bbox with
            | Some b ->
                poly <- poly |> clip 1.0 0.0 (-b.[0]) |> clip (-1.0) 0.0 b.[2] |> clip 0.0 1.0 (-b.[1]) |> clip 0.0 (-1.0) b.[3]
            | None -> ()
            if not extend0 then poly <- clip dx dy (-dx*x0-dy*y0) poly
            if not extend1 then poly <- clip (-dx) (-dy) (dx*x1+dy*y1) poly
            if poly.Length < 3 then Some (Group (Matrix3x2.identity, 1.0, [])) else
            let color (values: float[]) =
                let struct(r,g,b) = toTarget space values
                Color.rgba r g b (byte (clamp 0.0 255.0 (alpha * 255.0 + 0.5)))
            let span = domain.[1] - domain.[0]
            let boundaries =
                func.Breaks |> List.filter (fun x -> x > domain.[0] && x < domain.[1]) |> List.distinct |> List.sort
            let edges = domain.[0] :: boundaries @ [domain.[1]] |> List.toArray
            let stops = ResizeArray<GradientStop>()
            let add x c =
                stops.Add { Offset = clamp 0.0 1.0 ((x-domain.[0])/span); StopColor = c }
            let deviation (a: Color) (m: Color) (b: Color) =
                let diff (x: byte) (y: byte) (z: byte) = abs (float y - (float x + float z) * 0.5)
                max (diff a.R m.R b.R) (max (diff a.G m.G b.G) (diff a.B m.B b.B))
            let rec segment depth a ca b cb =
                let mid = (a+b)*0.5
                let cm = color (func.Eval mid)
                if depth < 10 && deviation ca cm cb > 1.0 then
                    segment (depth+1) a ca mid cm
                    segment (depth+1) mid cm b cb
                else add b cb
            for i in 0 .. edges.Length - 2 do
                let a, b = edges.[i], edges.[i+1]
                let count = max 1 (int (ceil (128.0 * (b-a) / span)))
                let mutable prevX = a
                let mutable prevColor = color (func.Eval a)
                add a prevColor
                for j in 1 .. count do
                    let x = a + (b-a) * float j / float count
                    // Preserve both sides of a stitching discontinuity at the same offset.
                    let evalX = if j = count && i < edges.Length - 2 then b - (b-a)*1e-10 else x
                    let nextColor = color (func.Eval evalX)
                    segment 0 prevX prevColor x nextColor
                    prevX <- x
                    prevColor <- nextColor
            // A transformed axial parameter is a covector, not the transformed axis.
            // Construct a page-space gradient whose projection equals the local one,
            // including under nonuniform scale and shear.
            let gx = inv.M11*dx + inv.M12*dy
            let gy = inv.M21*dx + inv.M22*dy
            let factor = len2 / (gx*gx + gy*gy)
            let p0 = Matrix3x2.transformVec2 ctm (vec2 x0 y0)
            let p1 = vec2 (float p0.X + gx*factor) (float p0.Y + gy*factor)
            let points = poly |> List.map (fun (x,y) -> Matrix3x2.transformVec2 ctm (vec2 x y))
            let path = MoveTo (List.head points) :: ((List.tail points |> List.map LineTo) @ [Close])
            Some (FillPath (path, LinearGradient (p0, p1, List.ofSeq stops, Pad), NonZero))
        | _ -> None

/// Render a shading, rejecting any function execution error.
let tryRenderObject (toTarget: PdfColor.Space -> float[] -> struct(byte*byte*byte)) (doc: PdfDocument) (resources: PdfObj) (shading: PdfObj) (ctm: Matrix3x2) (alpha: float) (pageBounds: float * float * float * float) (scale: float) : Cmd option =
    try renderObject toTarget doc resources shading ctm alpha pageBounds scale
    with FunctionEvaluationError -> None

/// Resolve a named shading from the current resource dictionary.
let tryRender (toTarget: PdfColor.Space -> float[] -> struct(byte*byte*byte)) (doc: PdfDocument) (resources: PdfObj) (name: string) (ctm: Matrix3x2) (alpha: float) (pageBounds: float * float * float * float) (scale: float) : Cmd option =
    field doc "Shading" resources
    |> Option.bind (field doc name)
    |> Option.bind (fun sh -> tryRenderObject toTarget doc resources sh ctm alpha pageBounds scale)
