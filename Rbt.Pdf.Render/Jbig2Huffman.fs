// Copyright 2012 Mozilla Foundation
// Copyright 2026 Rbt contributors
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

/// JBIG2 Annex B Huffman decoding, ported from PDF.js v4.10.38 src/core/jbig2.js.
/// Unlike JavaScript bitwise arithmetic, range offsets are unsigned and checked
/// before conversion to signed JBIG2 integers. Truncation and invalid codes throw.
module Rbt.Pdf.Render.Jbig2Huffman

let private malformed message = invalidOp ("JBIG2 Huffman: " + message)

/// Bounded, MSB-first reader. Position is the next byte to fetch, including when
/// a partially consumed byte is buffered. Setting Position discards buffered bits.
type Reader(data: byte[], start: int, endExclusive: int) =
    do
        if isNull data || start < 0 || endExclusive < start || endExclusive > data.Length then
            invalidArg "data" "Invalid JBIG2 reader bounds."
    let mutable position = start
    let mutable shift = -1
    let mutable currentByte = 0

    member _.Start = start
    member _.End = endExclusive
    member _.IsByteAligned = shift < 0
    member _.Position
        with get () = position
        and set value =
            if value < start || value > endExclusive then
                malformed "byte position outside reader bounds"
            position <- value
            shift <- -1

    member _.ReadBit() =
        if shift < 0 then
            if position >= endExclusive then malformed "end of data while reading bit"
            currentByte <- int data.[position]
            position <- position + 1
            shift <- 7
        let bit = (currentByte >>> shift) &&& 1
        shift <- shift - 1
        bit

    member this.ReadBitsUInt32(count: int) =
        if count < 0 || count > 32 then invalidArg "count" "Bit count must be between 0 and 32."
        let mutable value = 0u
        for _ = 1 to count do
            value <- (value <<< 1) ||| uint32 (this.ReadBit())
        value

    member this.ReadBits(count: int) =
        if count < 0 || count > 31 then invalidArg "count" "Signed bit count must be between 0 and 31."
        int (this.ReadBitsUInt32(count))

    member _.ByteAlign() = shift <- -1

    /// Byte-oriented consumers must explicitly align before switching from bits.
    member _.Next() =
        if shift >= 0 then malformed "byte read without alignment"
        if position >= endExclusive then -1
        else
            let value = int data.[position]
            position <- position + 1
            value

    member this.ReadByte() =
        let value = this.Next()
        if value < 0 then malformed "end of data while reading byte"
        value

    member _.SkipBytes(count: int) =
        if shift >= 0 then malformed "byte skip without alignment"
        if count < 0 || count > endExclusive - position then malformed "byte skip outside reader bounds"
        position <- position + count

/// PrefixLength zero omits a line. Canonical tables support the complete custom
/// prefix-length range (0..255); explicit PrefixCode values support 0..32 bits.
type HuffmanLine =
    { RangeLow: int64
      PrefixLength: int
      RangeLength: int
      PrefixCode: uint32
      IsLowerRange: bool
      IsOob: bool }

type private HuffmanNode(line: HuffmanLine option) =
    member val Zero = -1 with get, set
    member val One = -1 with get, set
    member _.Line = line

/// Immutable decoding tree. Canonical prefix assignment follows Annex B.3 and
/// rejects oversubscribed or colliding codes, but permits incomplete code trees.
type HuffmanTable(lines: HuffmanLine[], prefixCodesDone: bool) =
    let nodes =
        if isNull lines then nullArg "lines"
        let tree = ResizeArray<HuffmanNode>()
        tree.Add(HuffmanNode(None))
        let mutable maximum = 0
        for line in lines do
            if line.PrefixLength < 0 || line.PrefixLength > 255 then
                malformed "invalid prefix length"
            if line.RangeLength < 0 || line.RangeLength > 32 then
                malformed "invalid range length"
            // These bounds include the special lower endpoint MIN_INT32 - 1.
            if line.RangeLow < -2147483649L || line.RangeLow > 2147483647L then
                malformed "range endpoint outside JBIG2 integer bounds"
            if line.IsOob && (line.RangeLength <> 0 || line.IsLowerRange) then
                malformed "invalid out-of-band line"
            maximum <- max maximum line.PrefixLength
        let bits = Array.zeroCreate<bool> maximum
        let insert (line: HuffmanLine) =
            let mutable index = 0
            for bitIndex = 0 to line.PrefixLength - 1 do
                let parent = tree.[index]
                if parent.Line.IsSome then malformed "Huffman prefix collision"
                let bit = bits.[bitIndex]
                let child = if bit then parent.One else parent.Zero
                let last = bitIndex = line.PrefixLength - 1
                if last then
                    if child >= 0 then malformed "duplicate or colliding Huffman code"
                    let next = tree.Count
                    tree.Add(HuffmanNode(Some line))
                    if bit then parent.One <- next else parent.Zero <- next
                elif child >= 0 then
                    index <- child
                else
                    let next = tree.Count
                    tree.Add(HuffmanNode(None))
                    if bit then parent.One <- next else parent.Zero <- next
                    index <- next
        if prefixCodesDone then
            for line in lines do
                if line.PrefixLength > 32 then malformed "explicit Huffman prefix exceeds 32 bits"
                if line.PrefixLength > 0 then
                    if line.PrefixLength < 32 && (line.PrefixCode >>> line.PrefixLength) <> 0u then
                        malformed "Huffman prefix does not fit its length"
                    for bit = 0 to line.PrefixLength - 1 do
                        bits.[bit] <- ((line.PrefixCode >>> (line.PrefixLength - bit - 1)) &&& 1u) <> 0u
                    insert line
        else
            // A bit vector avoids overflow for legal custom prefixes longer than
            // a machine word. Appending zero bits implements firstCode <<= 1.
            let mutable exhausted = false
            for length = 1 to maximum do
                for line in lines do
                    if line.PrefixLength = length then
                        if exhausted then malformed "oversubscribed Huffman table"
                        insert line
                        let mutable bit = length - 1
                        while bit >= 0 && bits.[bit] do
                            bits.[bit] <- false
                            bit <- bit - 1
                        if bit < 0 then exhausted <- true
                        else bits.[bit] <- true
        tree.ToArray()

    /// None is the out-of-band marker, never a truncated or invalid integer.
    member _.Decode(reader: Reader) : int option =
        let mutable index = 0
        while nodes.[index].Line.IsNone do
            let node = nodes.[index]
            index <- if reader.ReadBit() = 0 then node.Zero else node.One
            if index < 0 then malformed "invalid Huffman data"
        let line = nodes.[index].Line.Value
        if line.IsOob then None
        else
            let offset = int64 (reader.ReadBitsUInt32(line.RangeLength))
            let value = if line.IsLowerRange then line.RangeLow - offset else line.RangeLow + offset
            if value < -2147483648L || value > 2147483647L then
                malformed "decoded integer overflow"
            Some (int value)

let private normal low prefixLength rangeLength prefixCode =
    { RangeLow = int64 low
      PrefixLength = prefixLength
      RangeLength = rangeLength
      PrefixCode = uint32 prefixCode
      IsLowerRange = false
      IsOob = false }

let private lower low prefixLength prefixCode =
    { normal low prefixLength 32 prefixCode with IsLowerRange = true }

let private oob prefixLength prefixCode =
    { normal 0 prefixLength 0 prefixCode with IsOob = true }

let private standardTables = Array.create<HuffmanTable option> 15 None

/// Standard tables B.1 through B.15, with the published prefix assignments.
let getStandardTable number =
    if number < 1 || number > 15 then malformed "standard table number must be between 1 and 15"
    match standardTables.[number - 1] with
    | Some table -> table
    | None ->
        let lines =
            match number with
            | 1 ->
                [| normal 0 1 4 0x0; normal 16 2 8 0x2
                   normal 272 3 16 0x6; normal 65808 3 32 0x7 |]
            | 2 ->
                [| normal 0 1 0 0x0; normal 1 2 0 0x2; normal 2 3 0 0x6
                   normal 3 4 3 0xe; normal 11 5 6 0x1e; normal 75 6 32 0x3e
                   oob 6 0x3f |]
            | 3 ->
                [| normal -256 8 8 0xfe; normal 0 1 0 0x0; normal 1 2 0 0x2
                   normal 2 3 0 0x6; normal 3 4 3 0xe; normal 11 5 6 0x1e
                   lower -257 8 0xff; normal 75 7 32 0x7e; oob 6 0x3e |]
            | 4 ->
                [| normal 1 1 0 0x0; normal 2 2 0 0x2; normal 3 3 0 0x6
                   normal 4 4 3 0xe; normal 12 5 6 0x1e; normal 76 5 32 0x1f |]
            | 5 ->
                [| normal -255 7 8 0x7e; normal 1 1 0 0x0; normal 2 2 0 0x2
                   normal 3 3 0 0x6; normal 4 4 3 0xe; normal 12 5 6 0x1e
                   lower -256 7 0x7f; normal 76 6 32 0x3e |]
            | 6 ->
                [| normal -2048 5 10 0x1c; normal -1024 4 9 0x8; normal -512 4 8 0x9
                   normal -256 4 7 0xa; normal -128 5 6 0x1d; normal -64 5 5 0x1e
                   normal -32 4 5 0xb; normal 0 2 7 0x0; normal 128 3 7 0x2
                   normal 256 3 8 0x3; normal 512 4 9 0xc; normal 1024 4 10 0xd
                   lower -2049 6 0x3e; normal 2048 6 32 0x3f |]
            | 7 ->
                [| normal -1024 4 9 0x8; normal -512 3 8 0x0; normal -256 4 7 0x9
                   normal -128 5 6 0x1a; normal -64 5 5 0x1b; normal -32 4 5 0xa
                   normal 0 4 5 0xb; normal 32 5 5 0x1c; normal 64 5 6 0x1d
                   normal 128 4 7 0xc; normal 256 3 8 0x1; normal 512 3 9 0x2
                   normal 1024 3 10 0x3; lower -1025 5 0x1e; normal 2048 5 32 0x1f |]
            | 8 ->
                [| normal -15 8 3 0xfc; normal -7 9 1 0x1fc; normal -5 8 1 0xfd
                   normal -3 9 0 0x1fd; normal -2 7 0 0x7c; normal -1 4 0 0xa
                   normal 0 2 1 0x0; normal 2 5 0 0x1a; normal 3 6 0 0x3a
                   normal 4 3 4 0x4; normal 20 6 1 0x3b; normal 22 4 4 0xb
                   normal 38 4 5 0xc; normal 70 5 6 0x1b; normal 134 5 7 0x1c
                   normal 262 6 7 0x3c; normal 390 7 8 0x7d; normal 646 6 10 0x3d
                   lower -16 9 0x1fe; normal 1670 9 32 0x1ff; oob 2 0x1 |]
            | 9 ->
                [| normal -31 8 4 0xfc; normal -15 9 2 0x1fc; normal -11 8 2 0xfd
                   normal -7 9 1 0x1fd; normal -5 7 1 0x7c; normal -3 4 1 0xa
                   normal -1 3 1 0x2; normal 1 3 1 0x3; normal 3 5 1 0x1a
                   normal 5 6 1 0x3a; normal 7 3 5 0x4; normal 39 6 2 0x3b
                   normal 43 4 5 0xb; normal 75 4 6 0xc; normal 139 5 7 0x1b
                   normal 267 5 8 0x1c; normal 523 6 8 0x3c; normal 779 7 9 0x7d
                   normal 1291 6 11 0x3d; lower -32 9 0x1fe; normal 3339 9 32 0x1ff
                   oob 2 0x0 |]
            | 10 ->
                [| normal -21 7 4 0x7a; normal -5 8 0 0xfc; normal -4 7 0 0x7b
                   normal -3 5 0 0x18; normal -2 2 2 0x0; normal 2 5 0 0x19
                   normal 3 6 0 0x36; normal 4 7 0 0x7c; normal 5 8 0 0xfd
                   normal 6 2 6 0x1; normal 70 5 5 0x1a; normal 102 6 5 0x37
                   normal 134 6 6 0x38; normal 198 6 7 0x39; normal 326 6 8 0x3a
                   normal 582 6 9 0x3b; normal 1094 6 10 0x3c; normal 2118 7 11 0x7d
                   lower -22 8 0xfe; normal 4166 8 32 0xff; oob 2 0x2 |]
            | 11 ->
                [| normal 1 1 0 0x0; normal 2 2 1 0x2; normal 4 4 0 0xc
                   normal 5 4 1 0xd; normal 7 5 1 0x1c; normal 9 5 2 0x1d
                   normal 13 6 2 0x3c; normal 17 7 2 0x7a; normal 21 7 3 0x7b
                   normal 29 7 4 0x7c; normal 45 7 5 0x7d; normal 77 7 6 0x7e
                   normal 141 7 32 0x7f |]
            | 12 ->
                [| normal 1 1 0 0x0; normal 2 2 0 0x2; normal 3 3 1 0x6
                   normal 5 5 0 0x1c; normal 6 5 1 0x1d; normal 8 6 1 0x3c
                   normal 10 7 0 0x7a; normal 11 7 1 0x7b; normal 13 7 2 0x7c
                   normal 17 7 3 0x7d; normal 25 7 4 0x7e; normal 41 8 5 0xfe
                   normal 73 8 32 0xff |]
            | 13 ->
                [| normal 1 1 0 0x0; normal 2 3 0 0x4; normal 3 4 0 0xc
                   normal 4 5 0 0x1c; normal 5 4 1 0xd; normal 7 3 3 0x5
                   normal 15 6 1 0x3a; normal 17 6 2 0x3b; normal 21 6 3 0x3c
                   normal 29 6 4 0x3d; normal 45 6 5 0x3e; normal 77 7 6 0x7e
                   normal 141 7 32 0x7f |]
            | 14 ->
                [| normal -2 3 0 0x4; normal -1 3 0 0x5; normal 0 1 0 0x0
                   normal 1 3 0 0x6; normal 2 3 0 0x7 |]
            | 15 ->
                [| normal -24 7 4 0x7c; normal -8 6 2 0x3c; normal -4 5 1 0x1c
                   normal -2 4 0 0xc; normal -1 3 0 0x4; normal 0 1 0 0x0
                   normal 1 3 0 0x5; normal 2 4 0 0xd; normal 3 5 1 0x1d
                   normal 5 6 2 0x3d; normal 9 7 4 0x7d; lower -25 7 0x7e
                   normal 25 7 32 0x7f |]
            | _ -> malformed "unknown standard Huffman table"
        let table = HuffmanTable(lines, true)
        standardTables.[number - 1] <- Some table
        table

/// Decode a type-53 Tables segment, excluding its segment header (Annex B.2).
let decodeTablesSegment (data: byte[]) start endExclusive =
    let reader = Reader(data, start, endExclusive)
    let flags = reader.ReadByte()
    if flags &&& 0x80 <> 0 then malformed "reserved custom table flag is set"
    let readSigned () =
        let unsigned = int64 (reader.ReadBitsUInt32(32))
        if unsigned >= 2147483648L then unsigned - 4294967296L else unsigned
    let lowest = readSigned ()
    let highest = readSigned ()
    if lowest >= highest then malformed "invalid custom table range"
    let prefixSize = ((flags >>> 1) &&& 7) + 1
    let rangeSize = ((flags >>> 4) &&& 7) + 1
    let lines = ResizeArray<HuffmanLine>()
    let mutable current = lowest
    while current < highest do
        let prefixLength = reader.ReadBits(prefixSize)
        let rangeLength = reader.ReadBits(rangeSize)
        if rangeLength > 32 then malformed "custom table range length exceeds 32 bits"
        lines.Add({ normal 0 prefixLength rangeLength 0 with RangeLow = current })
        current <- current + (1L <<< rangeLength)
    let lowerPrefix = reader.ReadBits(prefixSize)
    lines.Add({ lower 0 lowerPrefix 0 with RangeLow = lowest - 1L })
    let upperPrefix = reader.ReadBits(prefixSize)
    lines.Add({ normal 0 upperPrefix 32 0 with RangeLow = highest })
    if flags &&& 1 <> 0 then lines.Add(oob (reader.ReadBits(prefixSize)) 0)
    reader.ByteAlign()
    HuffmanTable(lines.ToArray(), false)

/// Decode the run-length-coded symbol-ID table at the start of Huffman text data
/// (7.4.3.1.7). Padding bits are discarded, leaving the reader byte aligned.
let decodeSymbolIdTable numberOfSymbols (reader: Reader) =
    if numberOfSymbols < 0 then malformed "negative symbol count"
    let runCodes = Array.init 35 (fun symbol -> normal symbol (reader.ReadBits(4)) 0 0)
    let runTable = HuffmanTable(runCodes, false)
    let codes = ResizeArray<HuffmanLine>()
    let mutable symbol = 0
    let mutable previousLength = 0
    while symbol < numberOfSymbols do
        let length =
            match runTable.Decode(reader) with
            | Some value -> value
            | None -> malformed "out-of-band symbol-ID run code"
        let repeatedLength, count =
            match length with
            | 32 ->
                if symbol = 0 then malformed "no previous code length in symbol-ID table"
                previousLength, reader.ReadBits(2) + 3
            | 33 -> 0, reader.ReadBits(3) + 3
            | 34 -> 0, reader.ReadBits(7) + 11
            | value when value >= 0 && value < 32 -> value, 1
            | _ -> malformed "invalid symbol-ID code length"
        if count > numberOfSymbols - symbol then malformed "symbol-ID repeat exceeds symbol count"
        for _ = 1 to count do
            codes.Add(normal symbol repeatedLength 0 0)
            symbol <- symbol + 1
        previousLength <- repeatedLength
    reader.ByteAlign()
    HuffmanTable(codes.ToArray(), false)

/// Find the nth Tables segment in reference order; non-table references are skipped.
/// The caller owns the segment-number type and lookup storage.
let getCustomHuffmanTable index (referredTo: 'SegmentId[]) (tryFind: 'SegmentId -> HuffmanTable option) =
    if index < 0 then malformed "negative custom table index"
    let mutable remaining = index
    let mutable position = 0
    let mutable result = None
    while position < referredTo.Length && result.IsNone do
        match tryFind referredTo.[position] with
        | Some table when remaining = 0 -> result <- Some table
        | Some _ -> remaining <- remaining - 1
        | None -> ()
        position <- position + 1
    match result with
    | Some table -> table
    | None -> malformed "cannot find referenced custom Huffman table"

let private customTable (tables: HuffmanTable[]) index =
    if isNull tables || index < 0 || index >= tables.Length then
        malformed "cannot find selected custom Huffman table"
    tables.[index]

type SymbolDictionaryHuffmanTables =
    { TableDeltaHeight: HuffmanTable
      TableDeltaWidth: HuffmanTable
      TableBitmapSize: HuffmanTable
      TableAggregateInstances: HuffmanTable }

/// Select dictionary tables (7.4.2.1.6). customTables contains only Tables
/// segments, in referred-to order, not one element per segment reference.
let getSymbolDictionaryHuffmanTables dh dw bitmapCustom aggregateCustom (customTables: HuffmanTable[]) =
    let mutable customIndex = 0
    let takeCustom () =
        let table = customTable customTables customIndex
        customIndex <- customIndex + 1
        table
    let select selector first =
        match selector with
        | 0 | 1 -> getStandardTable (first + selector)
        | 3 -> takeCustom ()
        | _ -> malformed "invalid symbol dictionary Huffman selector"
    let height = select dh 4
    let width = select dw 2
    let bitmap = if bitmapCustom then takeCustom () else getStandardTable 1
    let aggregate = if aggregateCustom then takeCustom () else getStandardTable 1
    { TableDeltaHeight = height
      TableDeltaWidth = width
      TableBitmapSize = bitmap
      TableAggregateInstances = aggregate }

type TextRegionHuffmanTables =
    { SymbolIdTable: HuffmanTable
      TableFirstS: HuffmanTable
      TableDeltaS: HuffmanTable
      TableDeltaT: HuffmanTable }

/// Select base text tables (7.4.3.1.6) and consume the symbol-ID table.
/// Refinement regions additionally select getTextRegionRefinementHuffmanTables.
let getTextRegionHuffmanTables fs ds dt (customTables: HuffmanTable[]) numberOfSymbols (reader: Reader) =
    let symbols = decodeSymbolIdTable numberOfSymbols reader
    let mutable customIndex = 0
    let select selector first lastSelector =
        if selector = 3 then
            let table = customTable customTables customIndex
            customIndex <- customIndex + 1
            table
        elif selector >= 0 && selector <= lastSelector then
            getStandardTable (first + selector)
        else malformed "invalid text region Huffman selector"
    let firstS = select fs 6 1
    let deltaS = select ds 8 2
    let deltaT = select dt 11 2
    { SymbolIdTable = symbols
      TableFirstS = firstS
      TableDeltaS = deltaS
      TableDeltaT = deltaT }

/// Refinement tables supplementing TextRegionHuffmanTables.
type TextRegionRefinementHuffmanTables =
    { TableDeltaWidth: HuffmanTable
      TableDeltaHeight: HuffmanTable
      TableDeltaX: HuffmanTable
      TableDeltaY: HuffmanTable
      TableBitmapSize: HuffmanTable }

// Refinement table mapping follows Apache JBig2 ImageIO Plugin TextRegion.java:
// https://github.com/apache/pdfbox-jbig2/blob/2d62947a279423dfdadcd4ca2d894c55b4e665d9/src/main/java/org/apache/pdfbox/jbig2/segments/TextRegion.java
// Copyright 2017 The Apache Software Foundation.
// Based on source code originally developed in the JBig2 project.
// Copyright (C) 1995-2017 levigo holding GmbH.
// Licensed to the Apache Software Foundation (ASF) under contributor license
// agreements; distributed under the Apache License, Version 2.0 stated above.

/// Select RDW, RDH, RDX, RDY and RSIZE (7.4.3.1.6), without consuming data.
/// Delta selectors 0/1 select B.14/B.15, 2 is reserved, and 3 selects a custom
/// table. RSIZE false selects B.1; true selects the next custom table.
/// Pass the same reference-ordered customTables array as for the base tables;
/// fs/ds/dt account for the custom entries already assigned to those tables.
let getTextRegionRefinementHuffmanTables fs ds dt rdw rdh rdx rdy rsize
    (customTables: HuffmanTable[]) : TextRegionRefinementHuffmanTables =
    let mutable customIndex = 0
    let skipBase selector lastSelector =
        if selector = 3 then customIndex <- customIndex + 1
        elif selector < 0 || selector > lastSelector then
            malformed "invalid text region Huffman selector"
    skipBase fs 1
    skipBase ds 2
    skipBase dt 2
    let takeCustom () =
        let table = customTable customTables customIndex
        customIndex <- customIndex + 1
        table
    let select selector =
        match selector with
        | 0 | 1 -> getStandardTable (14 + selector)
        | 3 -> takeCustom ()
        | _ -> malformed "invalid text refinement Huffman selector"
    let width = select rdw
    let height = select rdh
    let deltaX = select rdx
    let deltaY = select rdy
    let bitmapSize = if rsize then takeCustom () else getStandardTable 1
    { TableDeltaWidth = width
      TableDeltaHeight = height
      TableDeltaX = deltaX
      TableDeltaY = deltaY
      TableBitmapSize = bitmapSize }
