// Copyright 2012 Mozilla Foundation
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// JPEG 2000 reconstruction based on PDF.js v3.11.174, src/core/jpx.js:
// https://github.com/mozilla/pdf.js/blob/v3.11.174/src/core/jpx.js
module Rbt.Pdf.Render.JpxDecode

open System
open Rbt.Pdf.Render.JpxPackets

let private invalid message = invalidOp ("JPX: " + message)

// DWT coefficients are level shifted to a signed interval. Map that interval to
// the full eight-bit sample range; this also handles signed SIZ components.
let private sample precision value =
    if precision < 1 || precision > 16 then invalid "unsupported component precision"
    let range = float ((1 <<< precision) - 1)
    let shifted = value + float (1 <<< (precision - 1))
    let scaled = shifted * 255.0 / range
    if Double.IsNaN scaled || Double.IsInfinity scaled then invalid "invalid coefficient"
    byte (int (floor (max 0.0 (min 255.0 scaled) + 0.5)))

type private Plane = { Width: int; Height: int; X0: int; Y0: int; Items: float32[] }

// Compose tile-component planes onto the image grid. MCT is a codestream
// transform, not PDF colour-space conversion, and does not touch alpha.
let private writeTile (image: Image) (tile: Tile) (planes: Plane[]) (output: byte[]) =
    let n = image.Components.Length
    let imageWidth = image.X1 - image.X0
    let mct = tile.CodingStyle.MultipleComponentTransform
    if mct && n < 3 then invalid "multiple component transform needs three components"
    if mct then
        let first = image.Components.[0]
        for c = 1 to 2 do
            let comp = image.Components.[c]
            if comp.XRsiz <> first.XRsiz || comp.YRsiz <> first.YRsiz || comp.IsSigned <> first.IsSigned then
                invalid "multiple component transform with incompatible components"
    if mct && (image.Components.[0].Precision <> image.Components.[1].Precision || image.Components.[0].Precision <> image.Components.[2].Precision) then
        invalid "multiple component transform with unequal precision"
    let get c x y =
        let comp = image.Components.[c]
        let plane = planes.[c]
        let cx = max 0 (min (plane.Width - 1) (x / comp.XRsiz - plane.X0))
        let cy = max 0 (min (plane.Height - 1) (y / comp.YRsiz - plane.Y0))
        float plane.Items.[cy * plane.Width + cx]
    for y = tile.Y0 to tile.Y1 - 1 do
        for x = tile.X0 to tile.X1 - 1 do
            let pos = ((y - image.Y0) * imageWidth + x - image.X0) * n
            if mct then
                let y0, y1, y2 = get 0 x y, get 1 x y, get 2 x y
                let r, g, b =
                    if tile.Components.[0].CodingStyle.Reversible then
                        let g = y0 - floor ((y1 + y2) / 4.0)
                        g + y2, g, g + y1
                    else
                        y0 + 1.402 * y2, y0 - 0.34413 * y1 - 0.71414 * y2, y0 + 1.772 * y1
                output.[pos] <- sample image.Components.[0].Precision r
                output.[pos + 1] <- sample image.Components.[1].Precision g
                output.[pos + 2] <- sample image.Components.[2].Precision b
                for c = 3 to n - 1 do
                    output.[pos + c] <- sample image.Components.[c].Precision (get c x y)
            else
                for c = 0 to n - 1 do
                    output.[pos + c] <- sample image.Components.[c].Precision (get c x y)

let private copyCoefficients (coefficients: float32[]) levelWidth originX originY (subband: Subband)
                             delta mb reversible segmentation reset =
    let width = subband.X1 - subband.X0
    if width < 0 || mb < 0 || mb > 31 then invalid "invalid subband parameters"
    let right = (if subband.Kind.[0] = 'H' then 1 else 0) ^^^ (originX &&& 1)
    let bottom = (if subband.Kind.[1] = 'H' then 1 else 0) ^^^ (originY &&& 1)
    let interleave = subband.Kind <> "LL"
    for block in subband.Codeblocks do
        let bw, bh = block.X1 - block.X0, block.Y1 - block.Y0
        if bw < 0 || bh < 0 || block.X0 < subband.X0 || block.Y0 < subband.Y0 ||
           block.X1 > subband.X1 || block.Y1 > subband.Y1 then invalid "invalid codeblock bounds"
        if bw > 0 && bh > 0 && block.Segments.Count > 0 then
            let mutable length = 0
            let mutable passes = 0
            for segment in block.Segments do
                if segment.Start < 0 || segment.Length < 0 ||
                   segment.Start > segment.Data.Length - segment.Length ||
                   segment.Length > Int32.MaxValue - length ||
                   segment.CodingPasses < 0 || segment.CodingPasses > 100 - passes then
                    invalid "invalid codeblock segment"
                length <- length + segment.Length
                passes <- passes + segment.CodingPasses
            if length = 0 || length > 64000000 || passes = 0 then invalid "invalid codeblock data length"
            let encoded = Array.zeroCreate<byte> length
            let mutable at = 0
            for segment in block.Segments do
                Array.blit segment.Data segment.Start encoded at segment.Length
                at <- at + segment.Length
            let model = Rbt.Pdf.Render.BitModel(bw, bh, subband.Kind, block.ZeroBitPlanes, mb)
            model.SetDecoder(Rbt.Image.Codecs.ArithmeticDecoder.Decoder(encoded, 0, length))
            let mutable pass = 2
            for _ = 1 to passes do
                match pass with
                | 0 -> model.RunSignificancePropagationPass()
                | 1 -> model.RunMagnitudeRefinementPass()
                | _ ->
                    model.RunCleanupPass()
                    if segmentation then model.CheckSegmentationSymbol()
                if reset then model.Reset()
                pass <- (pass + 1) % 3
            let signs = model.CoefficientsSign
            let magnitudes = model.CoefficientsMagnitude
            let decoded = model.BitsDecoded
            let mutable source = 0
            for y = 0 to bh - 1 do
                let row = block.Y0 - subband.Y0 + y
                for x = 0 to bw - 1 do
                    let column = block.X0 - subband.X0 + x
                    let index =
                        if interleave then (2 * row + bottom) * levelWidth + 2 * column + right
                        else row * width + column
                    if index < 0 || index >= coefficients.Length then invalid "coefficient outside resolution"
                    let magnitude = magnitudes.[source]
                    if magnitude <> 0u then
                        let nb = int decoded.[source]
                        let value = (float magnitude + (if reversible then 0.0 else 0.5)) * delta
                        let value = if signs.[source] = 0uy then value else -value
                        let value = if reversible && nb >= mb then value else value * (2.0 ** float (mb - nb))
                        coefficients.[index] <- float32 value
                    source <- source + 1

let private transformComponent (comp: Component) (tileComponent: TileComponent) =
    let style, quant = tileComponent.CodingStyle, tileComponent.Quantization
    let resolutions = tileComponent.Resolutions
    if resolutions.Length <> style.DecompositionLevels + 1 || quant.Steps.Length = 0 then
        invalid "missing resolution or quantization parameters"
    let mutable stepIndex = 0
    let levels =
        resolutions |> Array.mapi (fun i resolution ->
            let w, h = resolution.X1 - resolution.X0, resolution.Y1 - resolution.Y0
            if w < 0 || h < 0 || int64 w * int64 h > 64000000L then invalid "invalid resolution dimensions"
            let coefficients = Array.zeroCreate<float32> (w * h)
            for subband in resolution.Subbands do
                let step =
                    if quant.ScalarExpounded then
                        if stepIndex >= quant.Steps.Length then invalid "missing quantization step"
                        let value = quant.Steps.[stepIndex]
                        stepIndex <- stepIndex + 1
                        value
                    else quant.Steps.[0]
                let epsilon = step.Epsilon + (if not quant.ScalarExpounded && i > 0 then 1 - i else 0)
                let gain =
                    match subband.Kind with
                    | "LL" -> 0
                    | "LH" | "HL" -> 1
                    | "HH" -> 2
                    | _ -> invalid "invalid subband"
                let delta =
                    if style.Reversible then 1.0
                    else (2.0 ** float (comp.Precision + gain - epsilon)) * (1.0 + float step.Mu / 2048.0)
                let mb = quant.GuardBits + epsilon - 1
                copyCoefficients coefficients w resolution.X0 resolution.Y0 subband delta mb style.Reversible style.SegmentationSymbolUsed style.ResetContextProbabilities
            ({ width = w; height = h; x0 = resolution.X0; y0 = resolution.Y0; items = coefficients }: JpxWavelets.Level))
    let result = JpxWavelets.reconstruct style.Reversible levels
    if result.width <> tileComponent.X1 - tileComponent.X0 || result.height <> tileComponent.Y1 - tileComponent.Y0 then
        invalid "invalid reconstructed tile component"
    { Width = result.width; Height = result.height; X0 = tileComponent.X0; Y0 = tileComponent.Y0; Items = result.items }

/// Decode a JP2 wrapper or raw JPEG 2000 codestream into interleaved 8-bit
/// component samples and return its enumerated JP2 colour space, if present.
/// PDF Decode arrays and soft-mask interpretation remain the caller's responsibility.
let decodeWithColorSpace (data: byte[]) : (int * int * int * byte[] * int option) option =
    if isNull data then None
    else
        try
            let image = JpxPackets.parse data
            let width, height, count = image.X1 - image.X0, image.Y1 - image.Y0, image.Components.Length
            if width <= 0 || height <= 0 || count <= 0 || count > 16 ||
               int64 width * int64 height * int64 count > 64000000L || image.Tiles.Length = 0 then invalid "image dimensions exceed limit"
            let output = Array.zeroCreate<byte> (width * height * count)
            for comp in image.Components do
                if comp.Precision < 1 || comp.Precision > 16 || comp.XRsiz <= 0 || comp.YRsiz <= 0 then
                    invalid "unsupported component parameters"
            for tile in image.Tiles do
                if tile.X0 < image.X0 || tile.Y0 < image.Y0 || tile.X1 > image.X1 || tile.Y1 > image.Y1 ||
                   tile.X0 >= tile.X1 || tile.Y0 >= tile.Y1 || tile.Components.Length <> count then
                    invalid "invalid tile"
                let planes = tile.Components |> Array.mapi (fun i c -> transformComponent image.Components.[i] c)
                writeTile image tile planes output
            Some (width, height, count, output, image.ColorSpace)
        with _ -> None

/// Decode without returning JP2 colour-space metadata.
let decode (data: byte[]) : (int * int * int * byte[]) option =
    decodeWithColorSpace data |> Option.map (fun (w, h, n, samples, _) -> w, h, n, samples)
