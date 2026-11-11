module Rbt.Pdf.Render.PdfColor

open System
open Rbt.Pdf
open Rbt.Renderer
open Rbt.Image.Codecs

/// PDF color space with its native component count and conversion to sRGB.
type Space =
    private
    | Gray | Rgb | Cmyk
    | CalGray of struct(float * float * float) * float
    | CalRgb of struct(float * float * float) * float[] * float[]
    | Lab of struct(float * float * float) * float[]
    | Icc of Icc.Profile * float[]
    | Indexed of int * Space * float[][]
let isIndexed = function Indexed _ -> true | _ -> false
let isIcc = function Icc _ -> true | _ -> false
let isDeviceGray = function Gray -> true | _ -> false
let device = function 1 -> Some Gray | 3 -> Some Rgb | 4 -> Some Cmyk | _ -> None

let components = function Gray | CalGray _ -> 1 | Rgb | CalRgb _ | Lab _ -> 3 | Cmyk -> 4 | Icc (p,_) -> Icc.components p | Indexed _ -> 1
let private clamp v = max 0.0 (min 1.0 v)
let private channel v = byte (int (clamp v * 255.0 + 0.5))
let private number = function PdfInt n -> Some (float n) | PdfReal n when not (Double.IsNaN n || Double.IsInfinity n) -> Some n | _ -> None

let private bounds = function
    | Icc (_, ranges) | Lab (_,ranges) -> ranges
    | Indexed _ -> [|0.0; 1.0|]
    | space -> Array.init (2*components space) (fun i -> if i%2 = 0 then 0.0 else 1.0)

/// Default ranges for samples, before the image's optional Decode array is applied.
let ranges space = Array.copy (bounds space)
let private bounded (limits: float[]) (values: float[]) i =
    max limits.[2*i] (min limits.[2*i+1] values.[i])
let private iccComponent (limits: float[]) (values: float[]) i = clamp (bounded limits values i)

let rec toRgb space (values: float[]) : struct(byte * byte * byte) =
    if isNull values || values.Length < components space then invalidArg "values" "PDF color component count mismatch"
    let struct(r,g,b) =
        match space with
        | Gray -> let g = clamp values.[0] in struct(g,g,g)
        | Rgb -> struct(clamp values.[0], clamp values.[1], clamp values.[2])
        | Cmyk ->
            let k = 1.0-clamp values.[3]
            struct((1.0-clamp values.[0])*k, (1.0-clamp values.[1])*k, (1.0-clamp values.[2])*k)
        | CalGray (white,gamma) ->
            let gray = Math.Pow(clamp values.[0],gamma)
            let struct(wx,wy,wz) = white
            let struct(x,y,z) = Icc.adaptXYZToD50 white (gray*wx) (gray*wy) (gray*wz)
            Icc.xyzD50ToRgb x y z
        | CalRgb (white,gamma,matrix) ->
            let a = Math.Pow(clamp values.[0],gamma.[0])
            let b = Math.Pow(clamp values.[1],gamma.[1])
            let c = Math.Pow(clamp values.[2],gamma.[2])
            let struct(x,y,z) =
                Icc.adaptXYZToD50 white
                    (a*matrix.[0]+b*matrix.[3]+c*matrix.[6])
                    (a*matrix.[1]+b*matrix.[4]+c*matrix.[7])
                    (a*matrix.[2]+b*matrix.[5]+c*matrix.[8])
            Icc.xyzD50ToRgb x y z
        | Lab (white,limits) ->
            let struct(x,y,z) =
                Icc.labToXyz white (bounded limits values 0) (bounded limits values 1) (bounded limits values 2)
            let struct(x,y,z) = Icc.adaptXYZToD50 white x y z
            Icc.xyzD50ToRgb x y z
        | Icc (profile, limits) ->
            Icc.toRgbComponents profile (iccComponent limits values 0)
                (if Icc.components profile > 1 then iccComponent limits values 1 else 0.0)
                (if Icc.components profile > 2 then iccComponent limits values 2 else 0.0)
                (if Icc.components profile > 3 then iccComponent limits values 3 else 0.0)
        | Indexed (hi, baseSpace, palette) ->
            let index = max 0 (min hi (int (values.[0]+0.5)))
            let struct(r,g,b) = toRgb baseSpace palette.[index]
            struct(float r / 255.0, float g / 255.0, float b / 255.0)
    struct(channel r,channel g,channel b)

let toColor space (values: float[]) alpha =
    let struct(r,g,b) = toRgb space values
    Color.rgba r g b (channel alpha)

/// A three-component encoded blending space with an unclipped D50 PCS.
type WorkingSpace =
    { IsDisplay: bool
      IsGray: bool
      ToPcs: float -> float -> float -> struct(float * float * float)
      FromPcs: float -> float -> float -> struct(float * float * float) }

let displaySpace =
    { IsDisplay = true
      IsGray = false
      ToPcs = Icc.rgbToXyzD50
      FromPcs = Icc.xyzD50ToRgb }

// DeviceGray is represented by three identical channels in renderer surfaces.
let deviceGraySpace =
    let toPcs gray _ _ = Icc.rgbToXyzD50 gray gray gray
    let fromPcs x y z =
        let struct(r,g,b) = Icc.xyzD50ToRgb x y z
        let gray = 0.30*r + 0.59*g + 0.11*b
        struct(gray,gray,gray)
    { IsDisplay = false; IsGray = true; ToPcs = toPcs; FromPcs = fromPcs }

let tryWorkingSpace = function
    | Rgb -> Some displaySpace
    | Icc (profile, _) ->
        match Icc.tryToPcs profile, Icc.tryFromPcs profile with
        | Some forward, Some inverse -> Some { IsDisplay = false; IsGray = false; ToPcs = forward; FromPcs = inverse }
        | _ -> None
    | _ -> None

let rec private toPcs space (values: float[]) =
    match space with
    | Gray -> let g = clamp values.[0] in Icc.rgbToXyzD50 g g g
    | Rgb -> Icc.rgbToXyzD50 values.[0] values.[1] values.[2]
    | Cmyk ->
        let k = 1.0 - clamp values.[3]
        Icc.rgbToXyzD50 ((1.0-clamp values.[0])*k) ((1.0-clamp values.[1])*k) ((1.0-clamp values.[2])*k)
    | CalGray (white,gamma) ->
        let gray = Math.Pow(clamp values.[0], gamma)
        let struct(wx,wy,wz) = white
        Icc.adaptXYZToD50 white (gray*wx) (gray*wy) (gray*wz)
    | CalRgb (white,gamma,matrix) ->
        let a = Math.Pow(clamp values.[0], gamma.[0])
        let b = Math.Pow(clamp values.[1], gamma.[1])
        let c = Math.Pow(clamp values.[2], gamma.[2])
        Icc.adaptXYZToD50 white
            (a*matrix.[0]+b*matrix.[3]+c*matrix.[6])
            (a*matrix.[1]+b*matrix.[4]+c*matrix.[7])
            (a*matrix.[2]+b*matrix.[5]+c*matrix.[8])
    | Lab (white,limits) ->
        let struct(x,y,z) = Icc.labToXyz white (bounded limits values 0) (bounded limits values 1) (bounded limits values 2)
        Icc.adaptXYZToD50 white x y z
    | Icc (profile,limits) ->
        match Icc.tryToPcs profile with
        | Some forward when Icc.components profile = 3 ->
            forward (iccComponent limits values 0) (iccComponent limits values 1) (iccComponent limits values 2)
        | Some forward when Icc.components profile = 1 -> forward (iccComponent limits values 0) 0.0 0.0
        | _ -> invalidOp "Unsupported source ICC PCS transform for transparency blending"
    | Indexed (hi,baseSpace,palette) ->
        let index = max 0 (min hi (int (values.[0]+0.5)))
        toPcs baseSpace palette.[index]

/// Convert before quantization, without routing an out-of-gamut source through sRGB.
let toWorkingRgb (target: WorkingSpace) space (values: float[]) =
    if isNull values || values.Length < components space then invalidArg "values" "PDF color component count mismatch"
    if target.IsDisplay then toRgb space values
    elif target.IsGray then
        let struct(r,g,b) = toRgb space values
        let gray = channel (0.30 * float r / 255.0 + 0.59 * float g / 255.0 + 0.11 * float b / 255.0)
        struct(gray,gray,gray)
    else
        let struct(x,y,z) = toPcs space values
        let struct(r,g,b) = target.FromPcs x y z
        struct(channel r, channel g, channel b)

/// Resolve PDF resource names, ICC stream Alternate and Indexed lookup tables.
/// Unsupported ICC data uses its declared Alternate, or the PDF default Device
/// space for /N when Alternate is absent; it is not treated as an ICC transform.
let resolve (doc: PdfDocument) (resources: PdfObj) (colorSpace: PdfObj) : Space option =
    let numbers obj =
        match Pdf.deref doc obj with
        | PdfArray xs ->
            let values = xs |> List.map (Pdf.deref doc >> number)
            if values |> List.forall Option.isSome then Some (values |> List.choose id |> List.toArray) else None
        | _ -> None
    let fieldNumbers key obj = Pdf.dictGet doc key obj |> Option.bind numbers
    let whitePoint obj =
        fieldNumbers "WhitePoint" obj |> Option.bind (function
            | [|x;y;z|] when x > 0.0 && y > 0.0 && z > 0.0 -> Some (struct(x,y,z))
            | _ -> None)
    let rec parse depth obj =
        if depth > 12 then None else
        match Pdf.deref doc obj with
        | PdfName "DeviceGray" -> Some Gray
        | PdfName "DeviceRGB" -> Some Rgb
        | PdfName "DeviceCMYK" -> Some Cmyk
        | PdfName name ->
            Pdf.dictGet doc "ColorSpace" resources
            |> Option.bind (Pdf.dictGet doc name)
            |> Option.bind (parse (depth+1))
        | PdfArray (head :: rest) ->
            match Pdf.deref doc head, rest with
            | PdfName ("Indexed" | "I"), baseSpace :: high :: lookup :: _ ->
                match parse (depth+1) baseSpace, number (Pdf.deref doc high) with
                | Some baseSpace, Some hi when hi >= 0.0 && hi <= 255.0 && hi = floor hi ->
                    let data =
                        match Pdf.deref doc lookup with
                        | PdfString bytes -> Some bytes
                        | PdfStream _ -> try Some (Pdf.getStreamData doc lookup) with _ -> None
                        | _ -> None
                    data |> Option.bind (fun bytes ->
                        let n = components baseSpace
                        if bytes.Length < (int hi+1)*n then None
                        else
                            let limits = bounds baseSpace
                            let palette = Array.init (int hi+1) (fun index ->
                                Array.init n (fun i ->
                                    let t = float bytes.[index*n+i]/255.0
                                    limits.[2*i] + t*(limits.[2*i+1]-limits.[2*i])))
                            Some (Indexed(int hi,baseSpace,palette)))
                | _ -> None
            | PdfName "ICCBased", profile :: _ ->
                let n = Pdf.dictGet doc "N" profile |> Option.bind (Pdf.deref doc >> number)
                let explicitAlternate = Pdf.dictGet doc "Alternate" profile
                let alternate = explicitAlternate |> Option.bind (parse (depth+1))
                let fallback () =
                    match n, explicitAlternate with
                    | Some 1.0, None -> Some Gray
                    | Some 3.0, None -> Some Rgb
                    | Some 4.0, None -> Some Cmyk
                    | Some count, Some _ -> alternate |> Option.filter (fun s ->
                        float (components s) = count &&
                        (match s with Indexed _ | Icc _ -> false | _ -> true))
                    | _ -> None
                match n with
                | Some count when count = 1.0 || count = 3.0 || count = 4.0 ->
                    let limits =
                        match Pdf.dictGet doc "Range" profile |> Option.map (Pdf.deref doc) with
                        | Some (PdfArray xs) when xs.Length = int count * 2 ->
                            let parsed = xs |> List.map (Pdf.deref doc >> number)
                            if parsed |> List.forall Option.isSome then
                                let values = parsed |> List.choose id |> List.toArray
                                if [0 .. int count-1] |> List.forall (fun i -> values.[2*i] <= values.[2*i+1]) then Some values else None
                            else None
                        | None -> Some (Array.init (int count*2) (fun i -> if i%2 = 0 then 0.0 else 1.0))
                        | _ -> None
                    match limits, Pdf.deref doc profile with
                    | Some limits, PdfStream _ ->
                        let parsed = try Pdf.getStreamData doc profile |> Icc.tryParse with _ -> None
                        match parsed with
                        | Some icc when Icc.components icc = int count -> Some (Icc(icc,limits))
                        | _ -> fallback ()
                    | _ -> fallback ()
                | _ -> None
            | PdfName "CalGray", parameters :: _ ->
                match whitePoint parameters with
                | None -> None
                | Some white ->
                    let gamma = match Pdf.dictGet doc "Gamma" parameters with
                                | None -> Some 1.0
                                | Some value -> number (Pdf.deref doc value)
                    gamma |> Option.bind (fun g -> if g > 0.0 then Some (CalGray(white,g)) else None)
            | PdfName "CalRGB", parameters :: _ ->
                match whitePoint parameters with
                | None -> None
                | Some white ->
                    let gamma = match Pdf.dictGet doc "Gamma" parameters with
                                | None -> Some [|1.0;1.0;1.0|]
                                | Some value -> numbers value
                    let matrix = match Pdf.dictGet doc "Matrix" parameters with
                                 | None -> Some [|1.0;0.0;0.0;0.0;1.0;0.0;0.0;0.0;1.0|]
                                 | Some value -> numbers value
                    match gamma,matrix with
                    | Some gamma, Some matrix when gamma.Length = 3 && Array.forall ((<) 0.0) gamma && matrix.Length = 9 ->
                        Some (CalRgb(white,gamma,matrix))
                    | _ -> None
            | PdfName "Lab", parameters :: _ ->
                match whitePoint parameters with
                | None -> None
                | Some white ->
                    let ab = match Pdf.dictGet doc "Range" parameters with
                             | None -> Some [|-100.0;100.0;-100.0;100.0|]
                             | Some value -> numbers value
                    match ab with
                    | Some [|amin;amax;bmin;bmax|] when amin <= amax && bmin <= bmax ->
                        Some (Lab(white,[|0.0;100.0;amin;amax;bmin;bmax|]))
                    | _ -> None
            | _ -> None
        | _ -> None
    parse 0 colorSpace
