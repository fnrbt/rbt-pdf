// Copyright 2012 Mozilla Foundation
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// http://www.apache.org/licenses/LICENSE-2.0
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// Adapted from PDF.js v4.10.38, src/core/jbig2.js.
// Huffman refinement adapted from Apache PDFBox JBIG2 TextRegion.java and
// SymbolDictionary.java, Copyright The Apache Software Foundation, Apache-2.0.
module Rbt.Pdf.Render.Jbig2Symbols

open System

/// Dictionary tables: DH, DW, BMSIZE, AI, EX; text tables: DT, FS, DS, ID,
/// RDW, RDH, RDX, RDY, RSIZE. None denotes an out-of-band code.
type Decoder = {
    Context: Jbig2Bitmaps.Context
    HuffmanInput: Jbig2Huffman.Reader option
    ReadHuffman: string -> int option
}

type DictionaryOptions = {
    Huffman: bool
    Refinement: bool
    Symbols: byte[][][]
    NumberOfNewSymbols: int
    NumberOfExportedSymbols: int
    Template: int
    AdaptiveTemplate: (int * int)[]
    RefinementTemplate: int
    RefinementAdaptiveTemplate: (int * int)[]
}

type TextRegionOptions = {
    Huffman: bool
    Refinement: bool
    Width: int
    Height: int
    DefaultPixel: int
    NumberOfInstances: int
    LogStripSize: int
    Symbols: byte[][][]
    SymbolCodeLength: int
    Transposed: bool
    DsOffset: int
    ReferenceCorner: int
    CombinationOperator: int
    RefinementTemplate: int
    RefinementAdaptiveTemplate: (int * int)[]
}

let private fail message = invalidOp ("JBIG2: " + message)
let private maxPixels = 100000000L
let private maxCount = 1000000

let private dimensions width height =
    if width <= 0 || height <= 0 || width > maxCount || height > maxCount || int64 width * int64 height > maxPixels then
        fail "invalid or excessive bitmap dimensions"

let private add a b =
    let n = int64 a + int64 b
    if n < int64 Int32.MinValue || n > int64 Int32.MaxValue then fail "integer overflow"
    int n

let private integer (context: Jbig2Bitmaps.Context) name =
    let contexts = context.GetContexts(name)
    let mutable prev = 1
    let readBits count =
        let mutable value = 0L
        for _ in 1 .. count do
            let bit = context.Decoder.ReadBit(contexts, prev)
            prev <- if prev < 256 then (prev <<< 1) ||| bit else (((prev <<< 1) ||| bit) &&& 511) ||| 256
            value <- (value <<< 1) ||| int64 bit
        value
    let sign = readBits 1
    let value =
        if readBits 1 = 0L then readBits 2
        elif readBits 1 = 0L then readBits 4 + 4L
        elif readBits 1 = 0L then readBits 6 + 20L
        elif readBits 1 = 0L then readBits 8 + 84L
        elif readBits 1 = 0L then readBits 12 + 340L
        else readBits 32 + 4436L
    if sign <> 0L && value = 0L then None
    else
        let n = if sign = 0L then value else -value
        if n < int64 Int32.MinValue || n > int64 Int32.MaxValue then fail "arithmetic integer overflow"
        Some (int n)

let private iaid (context: Jbig2Bitmaps.Context) codeLength =
    // The standard IAID context is indexed by the complete prefix.
    if codeLength < 0 || codeLength > 16 then fail "unsupported symbol ID code length"
    let contexts = context.GetContexts("IAID")
    let mutable prev = 1
    for _ in 1 .. codeLength do
        prev <- (prev <<< 1) ||| context.Decoder.ReadBit(contexts, prev)
    prev - (1 <<< codeLength)

let private required = function Some value -> value | None -> fail "unexpected out-of-band value"
let private value decoder huffman huffName arithmeticName =
    if huffman then decoder.ReadHuffman huffName else integer decoder.Context arithmeticName
let private reader decoder =
    match decoder.HuffmanInput with Some input -> input | None -> fail "missing Huffman input"

let private refineBitmap decoder huffman size width height template bitmap dx dy at =
    if not huffman then
        Jbig2Bitmaps.decodeRefinement width height template bitmap dx dy false at decoder.Context
    else
        let input = reader decoder
        input.ByteAlign()
        let start = input.Position
        if size <= 0 || size > input.End - start then fail "invalid refinement bitmap size"
        let context = Jbig2Bitmaps.Context(decoder.Context.Data, start, start + size, statistics = decoder.Context.Statistics)
        let result = Jbig2Bitmaps.decodeRefinement width height template bitmap dx dy false at context
        input.Position <- start + size
        result
let private symbolLength count =
    if count < 0 || count > maxCount then fail "invalid symbol count"
    let mutable length = 0
    let mutable n = count - 1
    while n > 0 do
        length <- length + 1
        n <- n >>> 1
    length
let private symbolSize (symbol: byte[][]) =
    if isNull symbol || symbol.Length = 0 || isNull symbol.[0] || symbol.[0].Length = 0 then fail "invalid symbol bitmap"
    let width = symbol.[0].Length
    dimensions width symbol.Length
    if symbol |> Array.exists (fun row -> isNull row || row.Length <> width) then fail "invalid symbol rows"
    width, symbol.Length

let decodeTextRegion (decoder: Decoder) (options: TextRegionOptions) =
    // Each Huffman refinement instance contains an independently bounded QM stream.
    dimensions options.Width options.Height
    if isNull options.Symbols then fail "missing symbols"
    if options.NumberOfInstances < 0 || options.NumberOfInstances > maxCount then fail "invalid symbol instance count"
    if options.LogStripSize < 0 || options.LogStripSize > 3 then fail "invalid strip size"
    if options.DefaultPixel <> 0 && options.DefaultPixel <> 1 then fail "invalid default pixel"
    if options.ReferenceCorner < 0 || options.ReferenceCorner > 3 then fail "invalid reference corner"
    if options.CombinationOperator < 0 || options.CombinationOperator > 3 then fail "invalid text combination operator"
    let bitmap = Array.init options.Height (fun _ -> Array.create options.Width (byte options.DefaultPixel))
    let stripSize = 1 <<< options.LogStripSize
    let read h a = value decoder options.Huffman h a
    let mutable stripT = -(int64 (required (read "DT" "IADT")))
    let mutable firstS = 0L
    let mutable count = 0
    while count < options.NumberOfInstances do
        stripT <- stripT + int64 (required (read "DT" "IADT"))
        firstS <- firstS + int64 (required (read "FS" "IAFS"))
        let mutable currentS = firstS
        let mutable inStrip = true
        while inStrip do
            if count >= options.NumberOfInstances then fail "too many symbol instances in strip"
            let currentT =
                if stripSize = 1 then 0
                elif options.Huffman then (reader decoder).ReadBits(options.LogStripSize)
                else required (integer decoder.Context "IAIT")
            if currentT < 0 || currentT >= stripSize then fail "invalid symbol strip offset"
            let t = int64 stripSize * stripT + int64 currentT
            let symbolId =
                if options.Huffman then required (decoder.ReadHuffman "ID")
                else iaid decoder.Context options.SymbolCodeLength
            if symbolId < 0 || symbolId >= options.Symbols.Length then fail "invalid symbol ID"
            let mutable symbol = options.Symbols.[symbolId]
            let initialWidth, initialHeight = symbolSize symbol
            let mutable symbolWidth = initialWidth
            let mutable symbolHeight = initialHeight
            let refine =
                options.Refinement &&
                (if options.Huffman then (reader decoder).ReadBit() <> 0
                 else required (integer decoder.Context "IARI") <> 0)
            if refine then
                let dw = required (read "RDW" "IARDW")
                let dh = required (read "RDH" "IARDH")
                let dx = required (read "RDX" "IARDX")
                let dy = required (read "RDY" "IARDY")
                let size = if options.Huffman then required (decoder.ReadHuffman "RSIZE") else 0
                symbolWidth <- add symbolWidth dw
                symbolHeight <- add symbolHeight dh
                dimensions symbolWidth symbolHeight
                symbol <- refineBitmap decoder options.Huffman size symbolWidth symbolHeight options.RefinementTemplate symbol (add (dw >>> 1) dx) (add (dh >>> 1) dy) options.RefinementAdaptiveTemplate
            let mutable increment = 0
            if not options.Transposed then
                if options.ReferenceCorner > 1 then currentS <- currentS + int64 (symbolWidth - 1)
                else increment <- symbolWidth - 1
            elif options.ReferenceCorner &&& 1 = 0 then currentS <- currentS + int64 (symbolHeight - 1)
            else increment <- symbolHeight - 1
            let offsetT = t - (if options.ReferenceCorner &&& 1 <> 0 then 0L else int64 (symbolHeight - 1))
            let offsetS = currentS - (if options.ReferenceCorner &&& 2 <> 0 then int64 (symbolWidth - 1) else 0L)
            for sy in 0 .. symbolHeight - 1 do
                let y = (if options.Transposed then offsetS else offsetT) + int64 sy
                if y >= 0L && y < int64 options.Height then
                    let row = bitmap.[int y]
                    let source = symbol.[sy]
                    let x0 = if options.Transposed then offsetT else offsetS
                    let left = int (max 0L (min (int64 symbolWidth) (-x0)))
                    let right = int (max 0L (min (int64 symbolWidth) (int64 options.Width - x0)))
                    for sx in left .. right - 1 do
                        let x = int (x0 + int64 sx)
                        row.[x] <-
                            match options.CombinationOperator with
                            | 0 -> row.[x] ||| source.[sx]
                            | 1 -> row.[x] &&& source.[sx]
                            | 2 -> row.[x] ^^^ source.[sx]
                            | _ -> 1uy ^^^ row.[x] ^^^ source.[sx]
            count <- count + 1
            match read "DS" "IADS" with
            | None -> inStrip <- false
            | Some delta -> currentS <- currentS + int64 increment + int64 delta + int64 options.DsOffset
    bitmap

let decodeSymbolDictionary (decoder: Decoder) (options: DictionaryOptions) =
    if isNull options.Symbols || options.NumberOfNewSymbols < 0 || options.NumberOfNewSymbols > maxCount || options.Symbols.Length > maxCount - options.NumberOfNewSymbols then fail "invalid symbol count"
    let total = options.Symbols.Length + options.NumberOfNewSymbols
    if options.NumberOfExportedSymbols < 0 || options.NumberOfExportedSymbols > total then fail "invalid exported symbol count"
    let codeLength = max (if options.Huffman then 1 else 0) (symbolLength total)
    let symbols = ResizeArray<byte[][]>(options.NumberOfNewSymbols)
    let mutable height = 0
    let mutable allocatedPixels = 0L
    while symbols.Count < options.NumberOfNewSymbols do
        let firstSymbol = symbols.Count
        height <- add height (required (value decoder options.Huffman "DH" "IADH"))
        let mutable width = 0
        let mutable totalWidth = 0
        let widths = ResizeArray<int>()
        let mutable doneClass = false
        while not doneClass do
            match value decoder options.Huffman "DW" "IADW" with
            | None -> doneClass <- true
            | Some delta ->
                if symbols.Count + widths.Count >= options.NumberOfNewSymbols then fail "too many dictionary symbols"
                width <- add width delta
                dimensions width height
                allocatedPixels <- allocatedPixels + int64 width * int64 height
                if allocatedPixels > maxPixels then fail "excessive symbol bitmap allocation"
                totalWidth <- add totalWidth width
                if options.Huffman && not options.Refinement then widths.Add(width)
                elif options.Refinement then
                    let instances = required (value decoder options.Huffman "AI" "IAAI")
                    if instances <= 0 || instances > maxCount then fail "invalid refinement instance count"
                    let available = Array.append options.Symbols (symbols.ToArray())
                    let bitmap =
                        if instances > 1 then
                            let aggregateDecoder =
                                if not options.Huffman then decoder
                                else
                                    let input = reader decoder
                                    let standard n = (Jbig2Huffman.getStandardTable n).Decode(input)
                                    { decoder with
                                        ReadHuffman = fun name ->
                                            match name with
                                            | "DT" -> standard 11
                                            | "FS" -> standard 6
                                            | "DS" -> standard 8
                                            | "ID" -> Some (input.ReadBits(codeLength))
                                            | "RDW" | "RDH" | "RDX" | "RDY" -> standard 14
                                            | "RSIZE" -> standard 1
                                            | _ -> fail "invalid aggregate Huffman table" }
                            decodeTextRegion aggregateDecoder {
                                Huffman = options.Huffman; Refinement = true; Width = width; Height = height
                                DefaultPixel = 0; NumberOfInstances = instances; LogStripSize = 0
                                Symbols = available; SymbolCodeLength = codeLength; Transposed = false
                                DsOffset = 0; ReferenceCorner = 1; CombinationOperator = 0
                                RefinementTemplate = options.RefinementTemplate
                                RefinementAdaptiveTemplate = options.RefinementAdaptiveTemplate }
                        else
                            let input = if options.Huffman then Some (reader decoder) else None
                            let id = match input with Some r -> r.ReadBits(codeLength) | None -> iaid decoder.Context codeLength
                            let readOffset name =
                                match input with
                                | Some r -> required ((Jbig2Huffman.getStandardTable 15).Decode(r))
                                | None -> required (integer decoder.Context name)
                            let dx = readOffset "IARDX"
                            let dy = readOffset "IARDY"
                            if id < 0 || id >= available.Length then fail "invalid refinement symbol ID"
                            let size =
                                match input with
                                | Some r -> required ((Jbig2Huffman.getStandardTable 1).Decode(r))
                                | None -> 0
                            refineBitmap decoder options.Huffman size width height options.RefinementTemplate available.[id] dx dy options.RefinementAdaptiveTemplate
                    symbols.Add(bitmap)
                else
                    symbols.Add(Jbig2Bitmaps.decodeBitmap false width height options.Template false None options.AdaptiveTemplate decoder.Context)
        if options.Huffman && not options.Refinement then
            if widths.Count = 0 then fail "empty symbol height class"
            dimensions totalWidth height
            let size = required (decoder.ReadHuffman "BMSIZE")
            if size < 0 then fail "invalid collective bitmap size"
            let input = reader decoder
            input.ByteAlign()
            let collective =
                if size = 0 then
                    Array.init height (fun _ ->
                        let row = Array.zeroCreate<byte> totalWidth
                        let mutable bits = 0
                        for x in 0 .. totalWidth - 1 do
                            if x % 8 = 0 then bits <- input.ReadByte()
                            row.[x] <- byte ((bits >>> (7 - x % 8)) &&& 1)
                        row)
                else
                    let start = input.Position
                    if size > decoder.Context.EndExclusive - start then fail "collective bitmap exceeds segment"
                    let context = Jbig2Bitmaps.Context(decoder.Context.Data, start, start + size)
                    let bitmap = Jbig2Bitmaps.decodeBitmap true totalWidth height 0 false None [||] context
                    input.Position <- start + size
                    bitmap
            let mutable x = 0
            for w in widths do
                symbols.Add(Array.init height (fun y -> collective.[y].[x .. x + w - 1]))
                x <- x + w
        elif symbols.Count = firstSymbol then fail "empty symbol height class"
    let exported = ResizeArray<byte[][]>(options.NumberOfExportedSymbols)
    let mutable index = 0
    let mutable flag = false
    let mutable zeroRuns = 0
    while index < total do
        let run = required (value decoder options.Huffman "EX" "IAEX")
        if run < 0 || run > total - index then fail "invalid exported symbol run"
        if run = 0 then zeroRuns <- zeroRuns + 1 else zeroRuns <- 0
        if zeroRuns > 1 then fail "empty exported symbol runs"
        if flag then
            for i in index .. index + run - 1 do
                exported.Add(if i < options.Symbols.Length then options.Symbols.[i] else symbols.[i - options.Symbols.Length])
        index <- index + run
        flag <- not flag
    if exported.Count <> options.NumberOfExportedSymbols then fail "exported symbol count mismatch"
    exported.ToArray()
