module Rbt.Pdf.Render.PdfImages

open System
open Rbt.Pdf
open Rbt.Renderer

let private number = function PdfInt n -> float n | PdfReal n -> n | _ -> 0.0
let private integer d k def = match Pdf.dictGet d k def |> Option.map (Pdf.deref d) with Some (PdfInt n) -> int n | Some (PdfReal n) -> int n | _ -> 0
let private clamp x = max 0.0 (min 1.0 x)
let private count = PdfColor.components

let private colorSpace doc resources obj = PdfColor.resolve doc resources obj

let private filters doc obj =
    match Pdf.dictGet doc "Filter" obj |> Option.map (Pdf.deref doc) with
    | Some (PdfName f) -> [f]
    | Some (PdfArray fs) -> fs |> List.choose (fun f -> match Pdf.deref doc f with PdfName n -> Some n | _ -> None)
    | _ -> []
let private filterParameters doc obj =
    match Pdf.dictGet doc "DecodeParms" obj |> Option.map (Pdf.deref doc) with
    | Some (PdfArray values) -> values |> List.tryLast |> Option.map (Pdf.deref doc) |> Option.defaultValue PdfNull
    | Some value -> value
    | None -> PdfNull

let private standardFilter = function
    | "FlateDecode" | "Fl" | "LZWDecode" | "LZW" | "ASCIIHexDecode" | "AHx" | "ASCII85Decode" | "A85" | "RunLengthDecode" | "RL" -> true
    | _ -> false

let private runLength (data: byte[]) =
    let result = ResizeArray<byte>()
    let mutable i = 0
    let mutable doneData = false
    while i < data.Length && not doneData do
        let n = int data.[i]
        i <- i + 1
        if n = 128 then doneData <- true
        elif n < 128 then
            let amount = min (n + 1) (data.Length - i)
            for j in 0 .. amount - 1 do result.Add(data.[i+j])
            i <- i + amount
        elif i < data.Length then
            for _ in 1 .. 257 - n do result.Add(data.[i])
            i <- i + 1
    result.ToArray()

let private dataFor doc obj fs =
    match Pdf.deref doc obj with
    | PdfStream (header, raw) ->
        if List.isEmpty fs then raw
        else
            let decoded = fs |> List.fold (fun data f ->
                if f = "RunLengthDecode" || f = "RL" then runLength data
                else Streams.decompress f data) raw
            Streams.decodeStream (Map.remove "Filter" header) decoded
    | _ -> [||]

let private decodeArray doc obj n defaults =
    match Pdf.dictGet doc "Decode" obj |> Option.map (Pdf.deref doc) with
    | Some (PdfArray xs) when xs.Length >= n * 2 -> xs |> List.map (Pdf.deref doc >> number) |> List.toArray
    | _ -> defaults

let private pack r g b a =
    let mul c = byte ((int c * int a + 127) / 255)
    Pixel.pack (mul b) (mul g) (mul r) a

/// Decode an image XObject or an equivalent inline image stream into premultiplied pixels.
let rec decode (decodeJpeg: byte[] -> (int * int * uint32[]) option) (toTarget: PdfColor.Space -> float[] -> struct(byte*byte*byte)) (doc: PdfDocument) (resources: PdfObj) (obj: PdfObj) (fill: Color) depth : (int * int * uint32[]) option =
    if depth > 8 then None else
    let w, h = integer doc "Width" obj, integer doc "Height" obj
    let stencil = match Pdf.dictGet doc "ImageMask" obj |> Option.map (Pdf.deref doc) with Some (PdfBool true) -> true | _ -> false
    let space = if stencil then PdfColor.device 1 else Pdf.dictGet doc "ColorSpace" obj |> Option.bind (colorSpace doc resources)
    let fs = filters doc obj
    let last = List.tryLast fs |> Option.defaultValue ""
    let jpeg = last = "DCTDecode" || last = "DCT"
    let fax = last = "CCITTFaxDecode" || last = "CCF"
    let jbig2 = last = "JBIG2Decode"
    let jpx = last = "JPXDecode"
    let special = jpeg || fax || jbig2 || jpx
    let prefix = if special then fs |> List.take (fs.Length - 1) else fs
    let bits = if stencil || fax || jbig2 then 1 else let b = integer doc "BitsPerComponent" obj in if b = 0 then 8 else b
    if w <= 0 || h <= 0 || int64 w * int64 h > 100000000L then None
    elif not (List.forall standardFilter prefix) then
        invalidOp ("Unsupported PDF image filter: " + (prefix |> List.find (standardFilter >> not)))
    else
    let parms = filterParameters doc obj
    let intParam key fallback = match Pdf.dictGet doc key parms |> Option.map (Pdf.deref doc) with Some (PdfInt n) -> int n | Some (PdfReal n) -> int n | _ -> fallback
    let boolParam key fallback = match Pdf.dictGet doc key parms |> Option.map (Pdf.deref doc) with Some (PdfBool b) -> b | _ -> fallback
    let decodeFax () =
        let columns = intParam "Columns" 1728
        let rows = intParam "Rows" h
        let rows = if rows = 0 then h else rows
        if columns <> w || rows <> h then None else
        CcittDecode.decode (dataFor doc obj prefix) {
            CcittDecode.K = intParam "K" 0; Columns = columns; Rows = rows
            EndOfLine = boolParam "EndOfLine" false
            EncodedByteAlign = boolParam "EncodedByteAlign" false
            EndOfBlock = boolParam "EndOfBlock" true
            BlackIs1 = boolParam "BlackIs1" false }
    let decodeJbig2 () =
        let globals = Pdf.dictGet doc "JBIG2Globals" parms |> Option.map (Pdf.getStreamData doc)
        Jbig2Decode.decode (dataFor doc obj prefix) globals w h
    let data =
        if fax then decodeFax () |> Option.defaultWith (fun () -> invalidOp "Unable to decode CCITTFax image")
        elif jbig2 then decodeJbig2 () |> Option.defaultWith (fun () -> invalidOp "Unable to decode JBIG2 image")
        elif not special then dataFor doc obj fs
        else [||]
    let decoded =
        if jpx then
            match JpxDecode.decodeWithColorSpace (dataFor doc obj prefix) with
            | Some (dw, dh, components, bytes, containerCS) when dw = w && dh = h && components > 0 && int64 bytes.Length >= int64 w * int64 h * int64 components ->
                let smask = match Pdf.dictGet doc "SMaskInData" obj |> Option.map (Pdf.deref doc) with Some (PdfInt n) -> int n | _ -> 0
                let hasAlpha = smask = 1 || smask = 2
                let colorCount = components - (if hasAlpha then 1 else 0)
                let cs = match space with Some s -> Some s | None when Pdf.dictGet doc "ColorSpace" obj |> Option.isNone -> PdfColor.device colorCount | None -> None
                match cs with
                | Some cs when count cs = colorCount ->
                    let pixels = Array.zeroCreate<uint32> (w*h)
                    let values = Array.zeroCreate<float> colorCount
                    let defaults = if PdfColor.isIndexed cs then [|0.0; 255.0|] else PdfColor.ranges cs
                    let decodeRanges = decodeArray doc obj colorCount defaults
                    let key = match Pdf.dictGet doc "Mask" obj |> Option.map (Pdf.deref doc) with Some (PdfArray a) when a.Length = 2*colorCount -> Some (a |> List.map (Pdf.deref doc >> number >> int) |> List.toArray) | _ -> None
                    let matte =
                        match smask, (Pdf.dictGet doc "Matte" obj |> Option.map (Pdf.deref doc)) with
                        | 2, Some (PdfArray entries) when entries.Length = colorCount ->
                            Some (entries |> List.map (Pdf.deref doc >> number) |> List.toArray)
                        | _ -> None
                    for i in 0 .. w*h-1 do
                        let mutable masked = key.IsSome
                        for c in 0 .. colorCount-1 do
                            let sample = int bytes.[i*components+c]
                            match key with Some ranges when sample < ranges.[2*c] || sample > ranges.[2*c+1] -> masked <- false | _ -> ()
                            values.[c] <- decodeRanges.[2*c] + float sample / 255.0 * (decodeRanges.[2*c+1]-decodeRanges.[2*c])
                        let alpha = if hasAlpha then bytes.[i*components+colorCount] else 255uy
                        if not masked && alpha <> 0uy then
                            match matte with
                            | Some background ->
                                let a = float alpha / 255.0
                                for c in 0 .. colorCount-1 do
                                    values.[c] <- clamp ((values.[c] - (1.0 - a) * background.[c]) / a)
                            | None -> ()
                            let struct(r,g,b) =
                                if space.IsNone && containerCS = Some 18 && colorCount = 3 then
                                    let y, cb, cr = values.[0], values.[1] - 0.5, values.[2] - 0.5
                                    let rgbValues = [| clamp (y + 1.402 * cr); clamp (y - 0.344136 * cb - 0.714136 * cr); clamp (y + 1.772 * cb) |]
                                    toTarget cs rgbValues
                                else toTarget cs values
                            pixels.[i] <-
                                if smask = 2 && matte.IsNone then
                                    Pixel.pack (min b alpha) (min g alpha) (min r alpha) alpha
                                else pack r g b alpha
                    Some (pixels, None)
                | _ -> invalidOp "Unsupported JPEG 2000 image color space or component count"
            | _ -> invalidOp "Unable to decode JPEG 2000 image"
        elif jpeg then
            let bytes = dataFor doc obj prefix
            let key = match Pdf.dictGet doc "Mask" obj |> Option.map (Pdf.deref doc) with Some (PdfArray a) -> Some (a |> List.map (Pdf.deref doc >> number >> int) |> List.toArray) | _ -> None
            let parms = filterParameters doc obj
            let transform =
                match Pdf.dictGet doc "ColorTransform" parms |> Option.map (Pdf.deref doc) with
                | Some (PdfInt n) -> Some (int n)
                | Some (PdfReal n) -> Some (int n)
                | _ -> None
            match space with
            | Some cs ->
                let n = count cs
                match ImageDecode.decodeComponents bytes transform with
                | Some image when image.Width = w && image.Height = h && image.Components = n && image.Data.Length >= w*h*n ->
                    let defaults = if PdfColor.isIndexed cs then [|0.0; 255.0|] else PdfColor.ranges cs
                    let ranges = decodeArray doc obj n defaults
                    let pixels = Array.zeroCreate<uint32> (w*h)
                    let values = Array.zeroCreate<float> n
                    for i in 0 .. w*h-1 do
                        let mutable masked = match key with Some k when k.Length = 2*n -> true | _ -> false
                        for c in 0 .. n-1 do
                            let v = int image.Data.[i*n+c]
                            match key with Some k when k.Length = 2*n && (v < k.[2*c] || v > k.[2*c+1]) -> masked <- false | _ -> ()
                            // PDF's Decode array maps the DCT output samples, regardless of
                            // Adobe JPEG polarity conventions used for standalone images.
                            let sample = float v / 255.0
                            values.[c] <- ranges.[2*c] + sample * (ranges.[2*c+1] - ranges.[2*c])
                        if not masked then
                            let struct(r,g,b) = toTarget cs values
                            pixels.[i] <- pack r g b 255uy
                    Some (pixels, None)
                | _ -> None
            | None when Pdf.dictGet doc "ColorSpace" obj |> Option.isSome -> None
            | _ ->
                decodeJpeg bytes |> Option.bind (fun (dw, dh, pixels) ->
                    if dw <> w || dh <> h || pixels.Length < w*h then None
                    else
                        let cs = Option.get (PdfColor.device 3)
                        let values = Array.zeroCreate<float> 3
                        let converted = Array.zeroCreate<uint32> (w*h)
                        for i in 0 .. w*h-1 do
                            let p = pixels.[i]
                            let a = Pixel.a p
                            if a <> 0uy then
                                values.[0] <- float (Pixel.r p) / float a
                                values.[1] <- float (Pixel.g p) / float a
                                values.[2] <- float (Pixel.b p) / float a
                                let struct(r,g,b) = toTarget cs values
                                converted.[i] <- pack r g b a
                        Some (converted, None))
        else
            match space with
            | None -> None
            | Some cs when bits = 1 || bits = 2 || bits = 4 || bits = 8 || bits = 16 ->
                let n = count cs
                let rowBytes = int ((int64 w * int64 n * int64 bits + 7L) / 8L)
                if int64 data.Length < int64 rowBytes * int64 h then None else
                let maxSample = (1 <<< bits) - 1
                let defaults = if PdfColor.isIndexed cs then [|0.0; float maxSample|] else PdfColor.ranges cs
                let ranges = decodeArray doc obj n defaults
                let key = match Pdf.dictGet doc "Mask" obj |> Option.map (Pdf.deref doc) with Some (PdfArray a) when a.Length = n*2 -> Some (a |> List.map (Pdf.deref doc >> number >> int) |> List.toArray) | _ -> None
                let pixels = Array.zeroCreate<uint32> (w*h)
                let samples = Array.zeroCreate<int> n
                let values = Array.zeroCreate<float> n
                for y in 0 .. h-1 do
                    for x in 0 .. w-1 do
                        for c in 0 .. n-1 do
                            let bit = (x*n+c)*bits
                            let pos = y*rowBytes + bit/8
                            let v = if bits = 16 then (int data.[pos] <<< 8) ||| int data.[pos+1] else (int data.[pos] >>> (8-bits-bit%8)) &&& maxSample
                            samples.[c] <- v
                            values.[c] <- ranges.[2*c] + float v / float maxSample * (ranges.[2*c+1] - ranges.[2*c])
                        let mutable masked = key.IsSome
                        match key with
                        | Some k -> for c in 0 .. n-1 do if samples.[c] < k.[2*c] || samples.[c] > k.[2*c+1] then masked <- false
                        | None -> ()
                        if not masked then
                            if stencil then
                                if values.[0] < 0.5 then pixels.[y*w+x] <- pack fill.R fill.G fill.B fill.A
                            else
                                let struct(r,g,b) = toTarget cs values
                                pixels.[y*w+x] <- pack r g b 255uy
                Some (pixels, Some samples)
            | _ -> None
    match decoded with
    | None -> None
    | Some (pixels, _) ->
        let maskObj = Pdf.dictGet doc "SMask" obj |> Option.bind (fun m -> match Pdf.deref doc m with PdfStream _ -> Some m | _ -> None)
        let maskObj = match maskObj with Some _ -> maskObj | None -> Pdf.dictGet doc "Mask" obj |> Option.bind (fun m -> match Pdf.deref doc m with PdfStream _ -> Some m | _ -> None)
        let mutable maskDecoded = true
        match maskObj with
        | Some mask ->
            match decode decodeJpeg PdfColor.toRgb doc resources mask (Color.rgba 255uy 255uy 255uy 255uy) (depth+1) with
            | Some (mw,mh,mp) ->
                let isStencil = match Pdf.dictGet doc "ImageMask" mask |> Option.map (Pdf.deref doc) with Some (PdfBool true) -> true | _ -> false
                let matte =
                    match space, (Pdf.dictGet doc "Matte" mask |> Option.map (Pdf.deref doc)) with
                    | Some cs, Some (PdfArray values) when values.Length >= count cs ->
                        let struct(r,g,b) = toTarget cs (values |> List.map (Pdf.deref doc >> number) |> List.toArray)
                        Some (r,g,b)
                    | _ -> None
                for y in 0 .. h-1 do
                    for x in 0 .. w-1 do
                        let i = y*w+x
                        let m = mp.[(y*mh/h)*mw + x*mw/w]
                        let a = if isStencil then int (Pixel.a m) else int (Pixel.r m)
                        let p = pixels.[i]
                        let scale v = byte ((int v * a + 127)/255)
                        let r,g,b =
                            match matte with
                            | Some (mr,mg,mb) ->
                                let unblend v mv =
                                    let alpha = (int (Pixel.a p) * a + 127) / 255
                                    byte (min alpha (max 0 (int v - ((int mv * (255-a) * int (Pixel.a p) + 32512) / 65025))))
                                unblend (Pixel.r p) mr, unblend (Pixel.g p) mg, unblend (Pixel.b p) mb
                            | None -> scale (Pixel.r p), scale (Pixel.g p), scale (Pixel.b p)
                        pixels.[i] <- Pixel.pack b g r (scale (Pixel.a p))
            | None -> maskDecoded <- false
        | None -> ()
        if maskDecoded then Some (w,h,pixels) else None
