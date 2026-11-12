module Rbt.Pdf.Render.PdfImages

open System
open Rbt.Pdf
open Rbt.Renderer
open Rbt.Image
open Rbt.Image.Codecs
open Rbt.Image.Formats

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

/// Browser pixels are safe only when the JPEG's color interpretation agrees
/// with the PDF image dictionary. The no-ColorSpace RGB preview is intentional.
let tryBrowserJpeg (doc: PdfDocument) (resources: PdfObj) (obj: PdfObj) : (byte[] * int * int) option =
    let field key = Pdf.dictGet doc key obj
    let present key = field key |> Option.isSome
    let dimension key =
        match field key |> Option.map (Pdf.deref doc) with
        | Some (PdfInt value) when value > 0L && value <= int64 Int32.MaxValue -> Some (int value)
        | _ -> None
    let components, defaultName =
        match field "ColorSpace" |> Option.map (Pdf.deref doc) with
        | Some (PdfName "DeviceRGB") | None -> 3, "DefaultRGB"
        | Some (PdfName "DeviceGray") -> 1, "DefaultGray"
        | _ -> 0, ""
    let identityDecode =
        match field "Decode" |> Option.map (Pdf.deref doc) with
        | None -> true
        | Some (PdfArray values) when values.Length = components * 2 ->
            values |> List.mapi (fun i v ->
                match Pdf.deref doc v with
                | PdfInt n -> float n = float (i % 2)
                | PdfReal n -> n = float (i % 2)
                | _ -> false) |> List.forall id
        | _ -> false
    let defaultUnchanged =
        Pdf.dictGet doc "ColorSpace" resources
        |> Option.bind (Pdf.dictGet doc defaultName)
        |> Option.isNone
    match Pdf.deref doc obj, dimension "Width", dimension "Height" with
    | PdfStream (_, bytes), Some w, Some h
        when components <> 0 && int64 w * int64 h <= 100000000L &&
             (match field "Filter" |> Option.map (Pdf.deref doc) with
              | Some (PdfName ("DCTDecode" | "DCT")) -> true
              | Some (PdfArray [f]) ->
                  match Pdf.deref doc f with PdfName ("DCTDecode" | "DCT") -> true | _ -> false
              | _ -> false) &&
             defaultUnchanged &&
             (field "BitsPerComponent" |> Option.map (Pdf.deref doc) = Some (PdfInt 8L)) &&
             identityDecode &&
             not (List.exists present ["Mask"; "SMask"; "ImageMask"; "Matte"; "SMaskInData"]) &&
             (match field "DecodeParms" |> Option.map (Pdf.deref doc) with
              | None | Some PdfNull -> true
              | Some (PdfArray [p]) -> Pdf.deref doc p = PdfNull
              | _ -> false) ->
        match Codec.run (fun () -> Jpeg.inspect bytes) with
        | Ok header when header.Width = w && header.Height = h &&
                         header.Components = components &&
                         not header.HasExif && not header.HasIccProfile &&
                         header.Orientation = 1 &&
                         (if components = 3 then header.ColorTransform = 1 &&
                                                  (header.AdobeTransform = -1 || header.AdobeTransform = 1)
                          else header.ColorTransform = 0 && header.AdobeTransform = -1) ->
            Some (bytes, w, h)
        | _ -> None
    | _ -> None

/// The optional pixels retain geometric coverage before a soft image mask.
/// A soft image mask contributes to shape only when the caller's AIS is true.
/// The final optional plane stores premultiplied K as grayscale with matching
/// alpha; PDF JPEG component samples reach it before any RGB conversion.
let rec decode (decodeJpeg: byte[] -> (int * int * uint32[]) option) (toTarget: PdfColor.Space -> float[] -> struct(byte*byte*byte*byte)) (fourChannel: bool) (displayTarget: bool) (doc: PdfDocument) (resources: PdfObj) (obj: PdfObj) (fill: Color) (fillK: byte) depth : (int * int * uint32[] * uint32[] option * bool * bool * uint32[] option) option =
    if depth > 8 then None else
    let w, h = integer doc "Width" obj, integer doc "Height" obj
    let fourthPixels = if fourChannel then Some (lazy (Array.zeroCreate<uint32> (w*h))) else None
    let store (pixels: uint32[]) i r g b k a =
        pixels.[i] <- pack r g b a
        match fourthPixels with
        | Some fourth -> fourth.Value.[i] <- pack k k k a
        | None -> ()
    let color (pixels: uint32[]) i cs values alpha =
        let struct(r,g,b,k) = toTarget cs values
        store pixels i r g b k alpha
    let stencil = match Pdf.dictGet doc "ImageMask" obj |> Option.map (Pdf.deref doc) with Some (PdfBool true) -> true | _ -> false
    let space = if stencil then PdfColor.device 1 else Pdf.dictGet doc "ColorSpace" obj |> Option.bind (colorSpace doc resources)
    let fs = filters doc obj
    let last = List.tryLast fs |> Option.defaultValue ""
    let jpeg = last = "DCTDecode" || last = "DCT"
    let fax = last = "CCITTFaxDecode" || last = "CCF"
    let jbig2 = last = "JBIG2Decode"
    let jpx = last = "JPXDecode"
    let embeddedMaskMode = if jpx then integer doc "SMaskInData" obj else 0
    let embeddedSoftMask = embeddedMaskMode = 1 || embeddedMaskMode = 2
    let explicitSoftMask =
        if embeddedSoftMask then None
        else Pdf.dictGet doc "SMask" obj |> Option.bind (fun m -> match Pdf.deref doc m with PdfStream _ -> Some m | _ -> None)
    let imageMask =
        if stencil || embeddedSoftMask || explicitSoftMask.IsSome then None
        else Pdf.dictGet doc "Mask" obj |> Option.map (Pdf.deref doc)
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
                let smask = embeddedMaskMode
                let hasAlpha = embeddedSoftMask
                let colorCount = components - (if hasAlpha then 1 else 0)
                let cs = match space with Some s -> Some s | None when Pdf.dictGet doc "ColorSpace" obj |> Option.isNone -> PdfColor.device colorCount | None -> None
                match cs with
                | Some cs when count cs = colorCount ->
                    let pixels = Array.zeroCreate<uint32> (w*h)
                    let values = Array.zeroCreate<float> colorCount
                    let defaults = if PdfColor.isIndexed cs then [|0.0; 255.0|] else PdfColor.ranges cs
                    let decodeRanges = decodeArray doc obj colorCount defaults
                    let key = match imageMask with Some (PdfArray a) when a.Length = 2*colorCount -> Some (a |> List.map (Pdf.deref doc >> number >> int) |> List.toArray) | _ -> None
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
                            let struct(r,g,b,k) =
                                if space.IsNone && containerCS = Some 18 && colorCount = 3 then
                                    let y, cb, cr = values.[0], values.[1] - 0.5, values.[2] - 0.5
                                    let rgbValues = [| clamp (y + 1.402 * cr); clamp (y - 0.344136 * cb - 0.714136 * cr); clamp (y + 1.772 * cb) |]
                                    toTarget cs rgbValues
                                else toTarget cs values
                            if smask = 2 && matte.IsNone then
                                pixels.[i] <- Pixel.pack (min b alpha) (min g alpha) (min r alpha) alpha
                                match fourthPixels with
                                | Some fourth ->
                                    let value = min k alpha
                                    fourth.Value.[i] <- Pixel.pack value value value alpha
                                | None -> ()
                            else store pixels i r g b k alpha
                    Some pixels
                | _ -> invalidOp "Unsupported JPEG 2000 image color space or component count"
            | _ -> invalidOp "Unable to decode JPEG 2000 image"
        elif jpeg then
            let bytes = dataFor doc obj prefix
            let key = match imageMask with Some (PdfArray a) -> Some (a |> List.map (Pdf.deref doc >> number >> int) |> List.toArray) | _ -> None
            let parms = filterParameters doc obj
            let transform =
                match Pdf.dictGet doc "ColorTransform" parms |> Option.map (Pdf.deref doc) with
                | Some (PdfInt n) -> Some (int n)
                | Some (PdfReal n) -> Some (int n)
                | _ -> None
            let browserPixels =
                match tryBrowserJpeg doc resources obj with
                | Some (_, width, height) ->
                    let gray = space |> Option.exists PdfColor.isDeviceGray
                    let result = try decodeJpeg bytes with _ -> None
                    match result with
                    | Some (dw, dh, pixels) when dw = width && dh = height &&
                                                 not (isNull pixels) && pixels.Length = w*h &&
                                                 Array.forall (fun pixel ->
                                                     Pixel.a pixel = 255uy &&
                                                     (not gray || (Pixel.r pixel = Pixel.g pixel && Pixel.g pixel = Pixel.b pixel))) pixels ->
                        if displayTarget && not fourChannel then Some pixels
                        else
                            let cs = space |> Option.defaultWith (fun () -> Option.get (PdfColor.device 3))
                            let n = count cs
                            let values = Array.zeroCreate<float> n
                            let converted = Array.zeroCreate<uint32> (w*h)
                            for i in 0 .. w*h-1 do
                                let p = pixels.[i]
                                values.[0] <- float (Pixel.r p) / 255.0
                                if n = 3 then
                                    values.[1] <- float (Pixel.g p) / 255.0
                                    values.[2] <- float (Pixel.b p) / 255.0
                                color converted i cs values 255uy
                            Some converted
                    | _ -> None
                | None -> None
            if browserPixels.IsSome then browserPixels else
            (match space with
            | Some cs ->
                let n = count cs
                match ImageDecode.decodeComponents bytes transform with
                | Some image when image.Width = w && image.Height = h && image.Components = n && image.Data.Length >= w*h*n ->
                    let defaults = if PdfColor.isIndexed cs then [|0.0; 255.0|] else PdfColor.ranges cs
                    let ranges = decodeArray doc obj n defaults
                    let pixels = Array.zeroCreate<uint32> (w*h)
                    let values = Array.zeroCreate<float> n
                    // JPEG components are bytes, so each decoded component has
                    // only 256 possible values. Preserve the exact Decode-array
                    // arithmetic while moving it out of the per-pixel loop.
                    let decodedSamples =
                        Array.init (n * 256) (fun index ->
                            let c, v = index / 256, index % 256
                            let sample = float v / 255.0
                            ranges.[2*c] + sample * (ranges.[2*c+1] - ranges.[2*c]))
                    for i in 0 .. w*h-1 do
                        let mutable masked = match key with Some k when k.Length = 2*n -> true | _ -> false
                        for c in 0 .. n-1 do
                            let v = int image.Data.[i*n+c]
                            match key with Some k when k.Length = 2*n && (v < k.[2*c] || v > k.[2*c+1]) -> masked <- false | _ -> ()
                            values.[c] <- decodedSamples.[c*256+v]
                        if not masked then color pixels i cs values 255uy
                    Some pixels
                | _ -> None
            | None when Pdf.dictGet doc "ColorSpace" obj |> Option.isSome -> None
            | _ ->
                ImageDecode.decode bytes |> Option.bind (fun (dw, dh, pixels) ->
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
                                color converted i cs values a
                        Some converted)
            )
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
                let key = match imageMask with Some (PdfArray a) when a.Length = n*2 -> Some (a |> List.map (Pdf.deref doc >> number >> int) |> List.toArray) | _ -> None
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
                                if values.[0] < 0.5 then store pixels (y*w+x) fill.R fill.G fill.B fillK fill.A
                            else color pixels (y*w+x) cs values 255uy
                Some pixels
            | _ -> None
    let explicitMask = imageMask |> Option.bind (function PdfStream _ as mask -> Some mask | _ -> None)
    let maskOverridesState =
        explicitMask.IsSome || (match imageMask with Some (PdfArray _) -> true | _ -> false)
    let hasGeometricMask = stencil || maskOverridesState
    match decoded with
    | None -> None
    | Some pixels ->
        let geometricPixels =
            if explicitSoftMask.IsSome && hasGeometricMask then Some pixels else None
        let colorPixels = if explicitSoftMask.IsSome && hasGeometricMask then Array.copy pixels else pixels
        let maskObj = explicitSoftMask |> Option.orElse explicitMask
        let mutable maskDecoded = true
        match maskObj with
        | Some mask ->
            match decode decodeJpeg (fun cs values ->
                    let struct(r,g,b) = PdfColor.toRgb cs values
                    struct(r,g,b,0uy)) false true doc resources mask (Color.rgba 255uy 255uy 255uy 255uy) 0uy (depth+1) with
            | Some (mw,mh,mp,_,_,_,_) ->
                let isStencil = match Pdf.dictGet doc "ImageMask" mask |> Option.map (Pdf.deref doc) with Some (PdfBool true) -> true | _ -> false
                let matte =
                    match space, (Pdf.dictGet doc "Matte" mask |> Option.map (Pdf.deref doc)) with
                    | Some cs, Some (PdfArray values) when values.Length >= count cs ->
                        Some (toTarget cs (values |> List.map (Pdf.deref doc >> number) |> List.toArray))
                    | _ -> None
                for y in 0 .. h-1 do
                    for x in 0 .. w-1 do
                        let i = y*w+x
                        let m = mp.[(y*mh/h)*mw + x*mw/w]
                        let a = if isStencil then int (Pixel.a m) else int (Pixel.r m)
                        let p = colorPixels.[i]
                        let scale v = byte ((int v * a + 127)/255)
                        let unblend v mv =
                            let alpha = (int (Pixel.a p) * a + 127) / 255
                            byte (min alpha (max 0 (int v - ((int mv * (255-a) * int (Pixel.a p) + 32512) / 65025))))
                        let r,g,b,k =
                            match matte with
                            | Some (struct(mr,mg,mb,mk)) ->
                                let fourth = match fourthPixels with Some fourth -> Pixel.r fourth.Value.[i] | None -> 0uy
                                unblend (Pixel.r p) mr, unblend (Pixel.g p) mg, unblend (Pixel.b p) mb,
                                unblend fourth mk
                            | None ->
                                let fourth = match fourthPixels with Some fourth -> Pixel.r fourth.Value.[i] | None -> 0uy
                                scale (Pixel.r p), scale (Pixel.g p), scale (Pixel.b p), scale fourth
                        let alpha = scale (Pixel.a p)
                        colorPixels.[i] <- Pixel.pack b g r alpha
                        match fourthPixels with
                        | Some fourth -> fourth.Value.[i] <- Pixel.pack k k k alpha
                        | None -> ()
            | None -> maskDecoded <- false
        | None -> ()
        if maskDecoded then
            Some (w,h,colorPixels,geometricPixels,explicitSoftMask.IsSome || embeddedSoftMask,maskOverridesState,
                  fourthPixels |> Option.map (fun pixels -> pixels.Value))
        else None
