// Copyright 2012 Mozilla Foundation
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy at http://www.apache.org/licenses/LICENSE-2.0
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and limitations.
// Segment interpretation based on pdf.js src/core/jbig2.js.
module Rbt.Pdf.Render.Jbig2Decode

open System
open System.Collections.Generic
open Rbt.Pdf.Render.Jbig2Segments

type private Page = { Pixels: byte[]; Width: int; Height: int; Stride: int; Operator: int; Override: bool; Association: uint32 }

let private createPage width height association (r: Reader) =
    let pageWidth = r.Size()
    let pageHeight = r.UInt32()
    r.UInt32() |> ignore
    r.UInt32() |> ignore
    let flags = r.Byte()
    let striping = r.UInt16()
    if pageWidth <= 0 || (pageHeight <> UInt32.MaxValue && pageHeight = 0u) then
        invalidOp "Invalid JBIG2 page dimensions"
    if pageHeight = UInt32.MaxValue && (striping &&& 0x8000) = 0 then invalidOp "Invalid unknown JBIG2 page height"
    // The PDF image dimensions can differ from the embedded page dimensions.
    // Compose only their intersection, using the PDF row stride for output.
    let pageWidth = min pageWidth width
    let pageHeight = if pageHeight = UInt32.MaxValue then height else int (min pageHeight (uint32 height))
    let stride = (width + 7) / 8
    let pixels = Array.zeroCreate<byte> (stride * height)
    if flags &&& 4 <> 0 then
        for y = 0 to pageHeight - 1 do
            let row = y * stride
            for x = 0 to pageWidth / 8 - 1 do pixels.[row + x] <- 0xffuy
            if pageWidth % 8 <> 0 then
                pixels.[row + pageWidth / 8] <- byte (0xff <<< (8 - pageWidth % 8))
    { Pixels = pixels; Width = pageWidth; Height = pageHeight; Stride = stride
      Operator = (flags >>> 3) &&& 3; Override = (flags &&& 64) <> 0; Association = association }

// Page bitmap uses the JBIG2 convention 1=black until final output conversion.
let private compose (page: Page) (region: Region) (bitmap: byte[][]) =
    if bitmap.Length <> region.Height then invalidOp "Invalid JBIG2 bitmap height"
    let op = if page.Override then region.Operator else page.Operator
    if op > 4 then invalidOp "Invalid JBIG2 combination operator"
    let clippedWidth = if region.X >= page.Width then 0 else min region.Width (page.Width - region.X)
    let clippedHeight = if region.Y >= page.Height then 0 else min region.Height (page.Height - region.Y)
    for y = 0 to region.Height - 1 do
        let row = bitmap.[y]
        if row.Length <> region.Width then invalidOp "Invalid JBIG2 bitmap width"
        if y < clippedHeight then
            let rowStart = (region.Y + y) * page.Stride
            for x = 0 to clippedWidth - 1 do
                let px = region.X + x
                let index = rowStart + (px >>> 3)
                let mask = 0x80 >>> (px &&& 7)
                let old = (int page.Pixels.[index] &&& mask) <> 0
                let bit = row.[x] <> 0uy
                let value =
                    match op with
                    | 0 -> old || bit
                    | 1 -> old && bit
                    | 2 -> old <> bit
                    | 3 -> old = bit
                    | _ -> bit
                page.Pixels.[index] <- byte (if value then int page.Pixels.[index] ||| mask else int page.Pixels.[index] &&& (~~~mask))

let private referencedSymbols (symbols: Dictionary<uint32, byte[][][]>) (references: uint32[]) =
    let result = ResizeArray<byte[][]>()
    for id in references do
        match symbols.TryGetValue(id) with
        | true, items ->
            if items.Length > 1000000 - result.Count then invalidOp "Too many JBIG2 input symbols"
            result.AddRange(items)
        | _ -> ()
    result.ToArray()

let private decodeCore (data: byte[]) (globals: byte[] option) width height =
    if isNull data || width <= 0 || height <= 0 || width > 268435456 - 7 ||
       int64 width * int64 height > 268435456L then invalidOp "Invalid JBIG2 image dimensions"
    let segments =
        match globals with
        | Some g -> Array.append (readSegments g) (readSegments data)
        | None -> readSegments data
    let seen = HashSet<uint32>()
    let tables = Dictionary<uint32, Jbig2Huffman.HuffmanTable>()
    let symbols = Dictionary<uint32, byte[][][]>()
    let bitmaps = Dictionary<uint32, byte[][]>()
    let retained = Dictionary<uint32, int * int * Dictionary<string, byte[]>>()
    let mutable page: Page option = None
    for segment in segments do
        if not (seen.Add(segment.Number)) then invalidOp "Duplicate JBIG2 segment number"
        for reference in segment.Referred do
            if not (seen.Contains(reference)) || reference = segment.Number then invalidOp "Unknown JBIG2 segment reference"
        let r = Reader(segment.Data, segment.Start, segment.EndExclusive)
        match segment.Kind with
        | 48 ->
            if page.IsSome || segment.Page = 0u then invalidOp "Invalid JBIG2 page information"
            page <- Some (createPage width height segment.Page r)
        | 36 | 38 | 39 ->
            let region = readRegion r
            let flags = r.Byte()
            let mmr = flags &&& 1 <> 0
            let template = (flags >>> 1) &&& 3
            let prediction = flags &&& 8 <> 0
            if flags &&& 0xf0 <> 0 then invalidOp "Invalid JBIG2 generic region flags"
            let at = if mmr then [||] else readAdaptive r (if template = 0 then 4 else 1)
            let context = Jbig2Bitmaps.Context(segment.Data, r.Position, segment.EndExclusive)
            let bitmap = Jbig2Bitmaps.decodeBitmap mmr region.Width region.Height template prediction None at context
            if segment.Kind = 36 then bitmaps.Add(segment.Number, bitmap)
            else
                match page with
                | Some p when p.Association = segment.Page -> compose p region bitmap
                | _ -> invalidOp "Invalid JBIG2 generic region page"
        | 40 | 42 | 43 ->
            let region = readRegion r
            let flags = r.Byte()
            if flags &&& 0xfc <> 0 then invalidOp "Invalid JBIG2 refinement region flags"
            let template = flags &&& 1
            let prediction = flags &&& 2 <> 0
            let at = if template = 0 then readAdaptive r 2 else [||]
            let reference =
                if segment.Referred.Length = 0 then
                    match page with
                    | Some p when p.Association = segment.Page ->
                        Array.init region.Height (fun y ->
                            Array.init region.Width (fun x ->
                                let px = int64 region.X + int64 x
                                let py = int64 region.Y + int64 y
                                if px >= int64 p.Width || py >= int64 p.Height then 0uy
                                else byte ((int p.Pixels.[int py * p.Stride + int px / 8] >>> (7 - int px % 8)) &&& 1)))
                    | _ -> invalidOp "Missing JBIG2 refinement page"
                else
                    match segment.Referred |> Array.tryPick (fun id -> match bitmaps.TryGetValue(id) with true, bitmap -> Some (id, bitmap) | _ -> None) with
                    | Some (id, bitmap) ->
                        bitmaps.Remove(id) |> ignore
                        bitmap
                    | None -> invalidOp "Missing JBIG2 refinement reference"
            let context = Jbig2Bitmaps.Context(segment.Data, r.Position, segment.EndExclusive)
            let bitmap = Jbig2Bitmaps.decodeRefinement region.Width region.Height template reference 0 0 prediction at context
            if segment.Kind = 40 then bitmaps.Add(segment.Number, bitmap)
            else
                match page with
                | Some p when p.Association = segment.Page -> compose p region bitmap
                | _ -> invalidOp "Invalid JBIG2 refinement region page"
        | 0 ->
            let flags = r.UInt16()
            let huffman = flags &&& 1 <> 0
            let refinement = flags &&& 2 <> 0
            let dh, dw = (flags >>> 2) &&& 3, (flags >>> 4) &&& 3
            let bitmapCustom, aggregateCustom = flags &&& 64 <> 0, flags &&& 128 <> 0
            let template = (flags >>> 10) &&& 3
            let refinementTemplate = (flags >>> 12) &&& 1
            if flags &&& 0xe000 <> 0 then invalidOp "Invalid JBIG2 dictionary flags"
            let at = if huffman then [||] else readAdaptive r (if template = 0 then 4 else 1)
            let refinementAt = if refinement && refinementTemplate = 0 then readAdaptive r 2 else [||]
            let exported, added = r.Size(), r.Size()
            for id in segment.Referred do
                if not (symbols.ContainsKey(id) || tables.ContainsKey(id)) then invalidOp "Invalid JBIG2 dictionary reference"
            let inputSymbols = referencedSymbols symbols segment.Referred
            let custom =
                segment.Referred
                |> Array.choose (fun id -> match tables.TryGetValue(id) with true, value -> Some value | _ -> None)
            let statistics = Dictionary<string, byte[]>()
            if flags &&& 256 <> 0 then
                let previous =
                    segment.Referred
                    |> Array.tryPick (fun id -> match retained.TryGetValue(id) with true, value -> Some value | _ -> None)
                match previous with
                | Some (oldTemplate, oldRefinementTemplate, saved) when oldTemplate = template && oldRefinementTemplate = refinementTemplate ->
                    for item in saved do statistics.Add(item.Key, Array.copy item.Value)
                | _ -> invalidOp "Missing compatible retained JBIG2 dictionary context"
            let context = Jbig2Bitmaps.Context(segment.Data, r.Position, segment.EndExclusive, statistics = statistics)
            let input = if huffman then Some (Jbig2Huffman.Reader(segment.Data, r.Position, segment.EndExclusive)) else None
            let read =
                if huffman then
                    let selected = Jbig2Huffman.getSymbolDictionaryHuffmanTables dh dw bitmapCustom aggregateCustom custom
                    let reader = input.Value
                    fun name ->
                        match name with
                        | "DH" -> selected.TableDeltaHeight.Decode(reader)
                        | "DW" -> selected.TableDeltaWidth.Decode(reader)
                        | "BMSIZE" -> selected.TableBitmapSize.Decode(reader)
                        | "AI" -> selected.TableAggregateInstances.Decode(reader)
                        | "EX" -> (Jbig2Huffman.getStandardTable 1).Decode(reader)
                        | _ -> invalidOp "Unknown JBIG2 Huffman procedure"
                else fun _ -> invalidOp "Unexpected JBIG2 Huffman procedure"
            let decoder: Jbig2Symbols.Decoder = { Context = context; HuffmanInput = input; ReadHuffman = read }
            let options: Jbig2Symbols.DictionaryOptions =
                { Huffman = huffman; Refinement = refinement; Symbols = inputSymbols
                  NumberOfNewSymbols = added; NumberOfExportedSymbols = exported
                  Template = template; AdaptiveTemplate = at
                  RefinementTemplate = refinementTemplate; RefinementAdaptiveTemplate = refinementAt }
            symbols.Add(segment.Number, Jbig2Symbols.decodeSymbolDictionary decoder options)
            if flags &&& 512 <> 0 then
                let saved = Dictionary<string, byte[]>()
                for name in [| "GB"; "GR" |] do
                    match context.Statistics.TryGetValue(name) with
                    | true, values -> saved.Add(name, Array.copy values)
                    | _ -> ()
                retained.Add(segment.Number, (template, refinementTemplate, saved))
        | 4 | 6 | 7 ->
            let region = readRegion r
            let flags = r.UInt16()
            let huffman = flags &&& 1 <> 0
            let refinement = flags &&& 2 <> 0
            let logStrip = (flags >>> 2) &&& 3
            let corner = (flags >>> 4) &&& 3
            let transposed = flags &&& 64 <> 0
            let operator = (flags >>> 7) &&& 3
            let defaultPixel = (flags >>> 9) &&& 1
            let offsetBits = (flags >>> 10) &&& 31
            let dsOffset = if offsetBits >= 16 then offsetBits - 32 else offsetBits
            let refinementTemplate = (flags >>> 15) &&& 1
            let hf = if huffman then r.UInt16() else 0
            let at = if refinement && refinementTemplate = 0 then readAdaptive r 2 else [||]
            let instances = r.Size()
            for id in segment.Referred do
                if not (symbols.ContainsKey(id) || tables.ContainsKey(id)) then invalidOp "Invalid JBIG2 text reference"
            let inputSymbols = referencedSymbols symbols segment.Referred
            let custom =
                segment.Referred
                |> Array.choose (fun id -> match tables.TryGetValue(id) with true, value -> Some value | _ -> None)
            let mutable symbolLength = 0
            let mutable n = inputSymbols.Length - 1
            while n > 0 do
                symbolLength <- symbolLength + 1
                n <- n >>> 1
            let context = Jbig2Bitmaps.Context(segment.Data, r.Position, segment.EndExclusive)
            let input = if huffman then Some (Jbig2Huffman.Reader(segment.Data, r.Position, segment.EndExclusive)) else None
            let read =
                if huffman then
                    let reader = input.Value
                    let selected = Jbig2Huffman.getTextRegionHuffmanTables (hf &&& 3) ((hf >>> 2) &&& 3) ((hf >>> 4) &&& 3) custom inputSymbols.Length reader
                    let refinementTables =
                        if refinement then
                            Some (Jbig2Huffman.getTextRegionRefinementHuffmanTables
                                    (hf &&& 3) ((hf >>> 2) &&& 3) ((hf >>> 4) &&& 3)
                                    ((hf >>> 6) &&& 3) ((hf >>> 8) &&& 3)
                                    ((hf >>> 10) &&& 3) ((hf >>> 12) &&& 3)
                                    (hf &&& 0x4000 <> 0) custom)
                        else None
                    fun name ->
                        match name with
                        | "DT" -> selected.TableDeltaT.Decode(reader)
                        | "FS" -> selected.TableFirstS.Decode(reader)
                        | "DS" -> selected.TableDeltaS.Decode(reader)
                        | "ID" -> selected.SymbolIdTable.Decode(reader)
                        | "RDW" -> refinementTables.Value.TableDeltaWidth.Decode(reader)
                        | "RDH" -> refinementTables.Value.TableDeltaHeight.Decode(reader)
                        | "RDX" -> refinementTables.Value.TableDeltaX.Decode(reader)
                        | "RDY" -> refinementTables.Value.TableDeltaY.Decode(reader)
                        | "RSIZE" -> refinementTables.Value.TableBitmapSize.Decode(reader)
                        | _ -> invalidOp "Unknown JBIG2 Huffman procedure"
                else fun _ -> invalidOp "Unexpected JBIG2 Huffman procedure"
            let decoder: Jbig2Symbols.Decoder = { Context = context; HuffmanInput = input; ReadHuffman = read }
            let options: Jbig2Symbols.TextRegionOptions =
                { Huffman = huffman; Refinement = refinement; Width = region.Width; Height = region.Height
                  DefaultPixel = defaultPixel; NumberOfInstances = instances; LogStripSize = logStrip
                  Symbols = inputSymbols; SymbolCodeLength = symbolLength; Transposed = transposed
                  DsOffset = dsOffset; ReferenceCorner = corner; CombinationOperator = operator
                  RefinementTemplate = refinementTemplate; RefinementAdaptiveTemplate = at }
            let bitmap = Jbig2Symbols.decodeTextRegion decoder options
            if segment.Kind = 4 then bitmaps.Add(segment.Number, bitmap)
            else
                match page with
                | Some p when p.Association = segment.Page -> compose p region bitmap
                | _ -> invalidOp "Invalid JBIG2 text region page"
        | 49 | 50 | 51 | 52 | 62 -> ()
        | 53 -> tables.Add(segment.Number, Jbig2Huffman.decodeTablesSegment segment.Data segment.Start segment.EndExclusive)
        | _ -> invalidOp "Unsupported JBIG2 segment type"
    match page with
    | None -> invalidOp "JBIG2 page information is missing"
    | Some p ->
        // PDF DeviceGray has the opposite polarity to a JBIG2 bitmap.
        p.Pixels |> Array.map (fun b -> b ^^^ 0xffuy)

/// Decode the raw sequential JBIG2 stream and optional global segments into
/// MSB-first, row-padded PDF DeviceGray samples (zero is black).
let decode (data: byte[]) (globals: byte[] option) (width: int) (height: int) : byte[] option =
    try Some (decodeCore data globals width height)
    // Fable erases CLR exception subclasses. Keep the optional decode boundary
    // identical on both runtimes; callers report None as a failed image filter.
    with _ -> None
