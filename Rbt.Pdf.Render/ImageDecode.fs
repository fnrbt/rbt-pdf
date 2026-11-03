/// JPEG (DCTDecode) decoding for PDF images, with Rbt.Image (managed code, .NET and Fable).
module Rbt.Pdf.Render.ImageDecode

open Rbt.Image
open Rbt.Image.Codecs
open Rbt.Image.Formats

let inline private bgra (r: int) (g: int) (b: int) : uint32 =
    0xFF000000u ||| (uint32 r <<< 16) ||| (uint32 g <<< 8) ||| uint32 b

/// Index of the SOI marker (FF D8); some producers put bytes before it.
let private soi (data: byte[]) =
    let mutable i = 0
    let mutable found = -1
    while found < 0 && i + 1 < data.Length && i < 1024 do
        if data.[i] = 0xFFuy && data.[i + 1] = 0xD8uy then found <- i
        i <- i + 1
    max 0 found

/// Decode a DCTDecode stream to (width, height, premultiplied BGRA pixels); None when the
/// stream cannot be decoded. Grey and RGB/YCbCr as coded. CMYK has no colour profile here,
/// as in browsers: an Adobe-marked stream stores inverted ink (Photoshop), r = c * k / 255;
/// a stream without the Adobe marker stores plain ink values. EXIF orientation is ignored
/// (PDF places the image with its own matrix).
let decode (imageBytes: byte[]) : (int * int * uint32[]) option =
    if isNull imageBytes || imageBytes.Length < 4 then None
    else
    let start = soi imageBytes
    let ctx = DecodeContext({ DecodeOptions.Default with ApplyOrientation = false })
    match Codec.run (fun () -> Jpeg.decodeStream imageBytes start (imageBytes.Length - start) ctx None) with
    | Error _ -> None
    | Ok j ->
        let n = j.Width * j.Height
        let d = j.Data
        let px = Array.zeroCreate<uint32> n
        match j.Components with
        | 1 ->
            for i in 0 .. n - 1 do
                let v = int d.[i]
                px.[i] <- bgra v v v
        | 3 ->
            for i in 0 .. n - 1 do
                px.[i] <- bgra (int d.[3 * i]) (int d.[3 * i + 1]) (int d.[3 * i + 2])
        | _ ->
            let inverted = j.AdobeTransform >= 0
            for i in 0 .. n - 1 do
                let c = int d.[4 * i]
                let m = int d.[4 * i + 1]
                let y = int d.[4 * i + 2]
                let k = int d.[4 * i + 3]
                px.[i] <- if inverted then Color.cmykInverted c m y k else Color.cmyk c m y k
        Some (j.Width, j.Height, px)
