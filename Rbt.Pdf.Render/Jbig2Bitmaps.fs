(* Copyright 2012 Mozilla Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Generic bitmap decoding adapted from PDF.js v4.10.38 src/core/jbig2.js.
 *)
module Rbt.Pdf.Render.Jbig2Bitmaps

open System
open System.Collections.Generic
open Rbt.Image.Codecs

/// Arithmetic decoder and the independently adaptive statistics for each procedure.
type Context(data: byte[], start: int, endExclusive: int, ?statistics: Dictionary<string, byte[]>) =
    do
        if isNull data || start < 0 || endExclusive < start || endExclusive > data.Length then
            invalidArg "data" "Invalid JBIG2 arithmetic data range"
    let decoder = lazy (ArithmeticDecoder.Decoder(data, start, endExclusive))
    let tables = defaultArg statistics (Dictionary<string, byte[]>())
    member _.Data = data
    member _.Start = start
    member _.EndExclusive = endExclusive
    member _.Statistics = tables
    member _.Decoder = decoder.Value
    member _.GetContexts(name: string) =
        match tables.TryGetValue(name) with
        | true, table -> table
        | _ ->
            let table = Array.zeroCreate<byte> 65536
            tables.Add(name, table)
            table

let private codingTemplates =
    [| [| (-1,-2); (0,-2); (1,-2); (-2,-1); (-1,-1); (0,-1); (1,-1); (2,-1); (-4,0); (-3,0); (-2,0); (-1,0) |]
       [| (-1,-2); (0,-2); (1,-2); (2,-2); (-2,-1); (-1,-1); (0,-1); (1,-1); (2,-1); (-3,0); (-2,0); (-1,0) |]
       [| (-1,-2); (0,-2); (1,-2); (-2,-1); (-1,-1); (0,-1); (1,-1); (-2,0); (-1,0) |]
       [| (-3,-1); (-2,-1); (-1,-1); (0,-1); (1,-1); (-4,0); (-3,0); (-2,0); (-1,0) |] |]

let private genericLtpContexts = [| 0x9b25; 0x0795; 0x00e5; 0x0195 |]

let private checkDimensions width height =
    if width < 0 || height < 0 || int64 width * int64 height > int64 Int32.MaxValue then
        invalidArg "width" "Invalid JBIG2 bitmap dimensions"

let private checkCausal (x, y) =
    if y > 0 || (y = 0 && x >= 0) then
        invalidArg "at" "JBIG2 coding template refers to an undecoded pixel"

let private checkBitmap (bitmap: byte[][]) width height =
    if isNull bitmap || bitmap.Length <> height then
        invalidArg "bitmap" "Invalid JBIG2 bitmap height"
    for row in bitmap do
        if isNull row || row.Length <> width then
            invalidArg "bitmap" "Invalid JBIG2 bitmap width"
        for pixel in row do
            if pixel > 1uy then invalidArg "bitmap" "Invalid JBIG2 bitmap pixel"

// Calculate legal source coordinates once, using wide arithmetic so even
// arbitrary public-API offsets cannot overflow in the pixel loops.
let private axisWindow sourceLength targetLength (delta: int64) =
    let first = max 0L (-delta)
    let last = min (int64 targetLength) (int64 sourceLength - delta)
    if first >= last then 0, 0, 0
    else int first, int last, int delta

type private PixelWindow =
    { X0: int; X1: int; Dx: int
      Y0: int; Y1: int; Dy: int }

let private pixelWindow sourceWidth sourceHeight width height dx dy =
    let x0, x1, dx = axisWindow sourceWidth width dx
    let y0, y1, dy = axisWindow sourceHeight height dy
    { X0 = x0; X1 = x1; Dx = dx; Y0 = y0; Y1 = y1; Dy = dy }

let private windowPixel (bitmap: byte[][]) (window: PixelWindow) x y =
    if x >= window.X0 && x < window.X1 && y >= window.Y0 && y < window.Y1 then
        int bitmap.[y + window.Dy].[x + window.Dx]
    else 0

/// Decode a generic region. Bitmap pixels are bytes: zero is white, one is black.
let decodeBitmap (mmr: bool) (width: int) (height: int) (templateIndex: int)
                 (prediction: bool) (skip: byte[][] option) (at: (int * int)[]) (context: Context) : byte[][] =
    checkDimensions width height
    if mmr then
        if width = 0 || height = 0 then Array.init height (fun _ -> Array.zeroCreate width)
        else
            let data = context.Data.[context.Start .. context.EndExclusive - 1]
            match CcittDecode.decodeMmr data width height with
            | None -> invalidOp "Invalid JBIG2 MMR bitmap"
            | Some packed ->
                let stride = int ((int64 width + 7L) / 8L)
                if int64 packed.Length <> int64 stride * int64 height then
                    invalidOp "Invalid JBIG2 MMR bitmap length"
                Array.init height (fun y ->
                    Array.init width (fun x ->
                        if (int packed.[y * stride + x / 8] &&& (0x80 >>> (x % 8))) = 0 then 1uy else 0uy))
    else
        if templateIndex < 0 || templateIndex >= codingTemplates.Length then invalidArg "templateIndex" "Invalid JBIG2 generic template"
        let expected = if templateIndex = 0 then 4 else 1
        if isNull at || at.Length <> expected then invalidArg "at" "Invalid JBIG2 adaptive template"
        Array.iter checkCausal at
        match skip with
        | Some bitmap -> checkBitmap bitmap width height
        | None -> ()
        // Adaptive pixels occupy fixed context positions, regardless of coordinates.
        let fixedPixels = codingTemplates.[templateIndex]
        let template =
            match templateIndex with
            | 0 ->
                Array.concat [| [| at.[3] |]; fixedPixels.[0..2]; [| at.[2]; at.[1] |]
                                fixedPixels.[3..7]; [| at.[0] |]; fixedPixels.[8..11] |]
            | 1 -> Array.concat [| fixedPixels.[0..8]; [| at.[0] |]; fixedPixels.[9..11] |]
            | 2 -> Array.concat [| fixedPixels.[0..6]; [| at.[0] |]; fixedPixels.[7..8] |]
            | _ -> Array.concat [| fixedPixels.[0..4]; [| at.[0] |]; fixedPixels.[5..8] |]
        let windows =
            template |> Array.map (fun (dx, dy) -> pixelWindow width height width height (int64 dx) (int64 dy))
        let rows: byte[][] = Array.zeroCreate windows.Length
        let bitmap = Array.init height (fun _ -> Array.zeroCreate<byte> width)
        let decoder = context.Decoder
        let contexts = context.GetContexts("GB")
        let mutable ltp = 0
        for y in 0 .. height - 1 do
            if prediction then ltp <- ltp ^^^ decoder.ReadBit(contexts, genericLtpContexts.[templateIndex])
            if ltp <> 0 then
                if y > 0 then Array.Copy(bitmap.[y-1], bitmap.[y], width)
            else
                for i = 0 to windows.Length - 1 do
                    let w = windows.[i]
                    rows.[i] <- if y >= w.Y0 && y < w.Y1 then bitmap.[y + w.Dy] else null
                for x in 0 .. width - 1 do
                    let skipped = match skip with Some s -> s.[y].[x] <> 0uy | None -> false
                    if not skipped then
                        let mutable label = 0
                        for i = 0 to windows.Length - 1 do
                            let w = windows.[i]
                            let row = rows.[i]
                            label <- label <<< 1
                            if not (isNull row) && x >= w.X0 && x < w.X1 then
                                label <- label ||| int row.[x + w.Dx]
                        bitmap.[y].[x] <- byte (decoder.ReadBit(contexts, label))
        bitmap

/// Decode a generic refinement region relative to a black-is-one reference bitmap.
let decodeRefinement (width: int) (height: int) (templateIndex: int) (referenceBitmap: byte[][])
                     (offsetX: int) (offsetY: int) (prediction: bool) (at: (int * int)[])
                     (context: Context) : byte[][] =
    checkDimensions width height
    if templateIndex < 0 || templateIndex > 1 then invalidArg "templateIndex" "Invalid JBIG2 refinement template"
    if isNull at || at.Length <> (if templateIndex = 0 then 2 else 0) then
        invalidArg "at" "Invalid JBIG2 refinement adaptive template"
    if templateIndex = 0 then checkCausal at.[0]
    if isNull referenceBitmap then invalidArg "referenceBitmap" "Missing JBIG2 reference bitmap"
    let refHeight = referenceBitmap.Length
    let refWidth = if refHeight = 0 then 0 elif isNull referenceBitmap.[0] then invalidArg "referenceBitmap" "Invalid reference row" else referenceBitmap.[0].Length
    checkBitmap referenceBitmap refWidth refHeight
    let referenceTemplate, codingTemplate =
        if templateIndex = 0 then
            Array.append [| at.[1] |] [| (0,-1); (1,-1); (-1,0); (0,0); (1,0); (-1,1); (0,1); (1,1) |],
            [| at.[0]; (0,-1); (1,-1); (-1,0) |]
        else
            [| (0,-1); (-1,0); (0,0); (1,0); (0,1); (1,1) |],
            [| (-1,-1); (0,-1); (1,-1); (-1,0) |]
    let referenceWindows =
        referenceTemplate |> Array.map (fun (dx, dy) ->
            pixelWindow refWidth refHeight width height (int64 dx - int64 offsetX) (int64 dy - int64 offsetY))
    let codingWindows =
        codingTemplate |> Array.map (fun (dx, dy) -> pixelWindow width height width height (int64 dx) (int64 dy))
    let predictionWindows =
        [| for dy in -1 .. 1 do
               for dx in -1 .. 1 do
                   yield pixelWindow refWidth refHeight width height (int64 dx - int64 offsetX) (int64 dy - int64 offsetY) |]
    let bitmap = Array.init height (fun _ -> Array.zeroCreate<byte> width)
    let decoder = context.Decoder
    let contexts = context.GetContexts("GR")
    let mutable ltp = 0
    for y in 0 .. height - 1 do
        if prediction then
            let ltpContext = if templateIndex = 0 then 0x100 else 0x40
            ltp <- ltp ^^^ decoder.ReadBit(contexts, ltpContext)
        for x in 0 .. width - 1 do
            let mutable implicitPixel = -1
            if ltp <> 0 then
                let value = windowPixel referenceBitmap predictionWindows.[0] x y
                let mutable same = true
                for w in predictionWindows do
                    if windowPixel referenceBitmap w x y <> value then same <- false
                if same then implicitPixel <- value
            if implicitPixel >= 0 then bitmap.[y].[x] <- byte implicitPixel
            else
                let mutable label = 0
                for w in referenceWindows do
                    label <- (label <<< 1) ||| windowPixel referenceBitmap w x y
                for w in codingWindows do
                    label <- (label <<< 1) ||| windowPixel bitmap w x y
                bitmap.[y].[x] <- byte (decoder.ReadBit(contexts, label))
    bitmap
