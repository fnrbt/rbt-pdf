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
// Port of Mozilla PDF.js v3.11.174 src/core/jpx.js, packet parsing and
// initializeTile (https://github.com/mozilla/pdf.js/blob/v3.11.174/src/core/jpx.js).
// Unlike the original recovery path, malformed and unsupported streams fail.
module Rbt.Pdf.Render.JpxPackets

open System
open System.Collections.Generic
open Rbt.Pdf.Render

type QuantizationStep = { Epsilon: int; Mu: int }
type Quantization =
    { NoQuantization: bool
      ScalarExpounded: bool
      GuardBits: int
      Steps: QuantizationStep[] }
type CodingStyle =
    { DecompositionLevels: int
      CodeblockWidthExponent: int
      CodeblockHeightExponent: int
      ProgressionOrder: int
      LayersCount: int
      MultipleComponentTransform: bool
      Reversible: bool
      ResetContextProbabilities: bool
      SegmentationSymbolUsed: bool
      SopMarkerUsed: bool
      EphMarkerUsed: bool
      Precincts: (int * int)[] }
type Segment = { Data: byte[]; Start: int; Length: int; CodingPasses: int }
type Codeblock =
    { X0: int; Y0: int; X1: int; Y1: int
      mutable ZeroBitPlanes: int
      Segments: ResizeArray<Segment> }
type Subband =
    { Kind: string; X0: int; Y0: int; X1: int; Y1: int
      Codeblocks: Codeblock[] }
type Resolution =
    { Level: int; X0: int; Y0: int; X1: int; Y1: int
      Subbands: Subband[] }
type TileComponent =
    { X0: int; Y0: int; X1: int; Y1: int
      CodingStyle: CodingStyle
      Quantization: Quantization
      Resolutions: Resolution[] }
type Tile =
    { X0: int; Y0: int; X1: int; Y1: int
      CodingStyle: CodingStyle
      Components: TileComponent[] }
type Component =
    { Precision: int; IsSigned: bool; XRsiz: int; YRsiz: int
      X0: int; Y0: int; X1: int; Y1: int }
type Image =
    { X0: int; Y0: int; X1: int; Y1: int
      ColorSpace: int option
      Components: Component[]
      Tiles: Tile[] }

let private malformed message = invalidOp ("JPX: " + message)
let private require condition message = if not condition then malformed message
let private ceilDiv a b = a / b + (if a % b = 0 then 0 else 1)
let private ceilDiv64 (a: int64) (b: int64) =
    a / b + (if a % b > 0L then 1L else 0L)
let private maxSamples = 64000000L
let private maxStructures = 1000000
let private checkArea width height =
    require (width >= 0 && height >= 0 && int64 width * int64 height <= maxSamples) "Image allocation limit exceeded"

type private Reader(data: byte[], start: int, finish: int) =
    let mutable position = start
    member _.Position
        with get () = position
        and set value =
            require (value >= start && value <= finish) "Read outside stream bounds"
            position <- value
    member _.End = finish
    member _.Remaining = finish - position
    member _.Byte() =
        require (position < finish) "Truncated marker segment"
        let value = int data[position]
        position <- position + 1
        value
    member this.U16() =
        let high = this.Byte()
        (high <<< 8) ||| this.Byte()
    member this.U32() =
        let high = this.U16()
        (uint32 high <<< 16) ||| uint32 (this.U16())

let private coordinate (reader: Reader) =
    let value = reader.U32()
    require (value <= 1073741823u) "Unsupported image coordinate"
    int value

let private readCodingStyle (reader: Reader) =
    let flags = reader.Byte()
    require (flags &&& 0xF8 = 0) "Unsupported COD flags"
    let progression = reader.Byte()
    require (progression <= 4) "Unsupported progression order"
    let layers = reader.U16()
    require (layers > 0) "Zero quality layers"
    let mct = reader.Byte()
    require (mct <= 1) "Unsupported multiple component transform"
    let levels = reader.Byte()
    require (levels <= 30) "Unsupported decomposition count"
    let xb, yb = reader.Byte(), reader.Byte()
    require (xb <= 8 && yb <= 8 && xb + yb <= 8) "Invalid codeblock dimensions"
    let style = reader.Byte()
    require (style &&& 0xDD = 0) "Unsupported codeblock coding style"
    let transform = reader.Byte()
    require (transform <= 1) "Unsupported wavelet transform"
    let precincts =
        if flags &&& 1 = 0 then Array.create (levels + 1) (15, 15)
        else
            Array.init (levels + 1) (fun r ->
                let value = reader.Byte()
                let px, py = value &&& 15, value >>> 4
                require (r = 0 || (px > 0 && py > 0)) "Invalid high-resolution precinct dimensions"
                px, py)
    require (reader.Remaining = 0) "Invalid COD marker length"
    { DecompositionLevels = levels; CodeblockWidthExponent = xb + 2
      CodeblockHeightExponent = yb + 2; ProgressionOrder = progression
      LayersCount = layers; MultipleComponentTransform = mct = 1
      Reversible = transform = 1; ResetContextProbabilities = style &&& 2 <> 0
      SegmentationSymbolUsed = style &&& 32 <> 0
      SopMarkerUsed = flags &&& 2 <> 0; EphMarkerUsed = flags &&& 4 <> 0
      Precincts = precincts }

let private readQuantization (reader: Reader) =
    let flags = reader.Byte()
    let style = flags &&& 31
    require (style <= 2) "Unsupported quantization style"
    let size = if style = 0 then 1 else 2
    require (reader.Remaining > 0 && reader.Remaining % size = 0) "Invalid quantization marker length"
    let steps = Array.init (reader.Remaining / size) (fun _ ->
        let first = reader.Byte()
        if style = 0 then
            require (first &&& 7 = 0) "Invalid reversible quantization exponent"
            { Epsilon = first >>> 3; Mu = 0 }
        else
            { Epsilon = first >>> 3; Mu = ((first &&& 7) <<< 8) ||| reader.Byte() })
    require (style <> 1 || steps.Length = 1) "Invalid scalar-derived quantization"
    { NoQuantization = style = 0; ScalarExpounded = style <> 1
      GuardBits = flags >>> 5; Steps = steps }

type private BlockState =
    { Output: Codeblock; Column: int; Row: int
      mutable Included: bool; mutable LengthBits: int }
type private PrecinctState =
    { mutable MinColumn: int; mutable MinRow: int
      mutable MaxColumn: int; mutable MaxRow: int
      Blocks: ResizeArray<BlockState>
      mutable Inclusion: InclusionTree option
      mutable ZeroPlanes: TagTree option }
type private BandState =
    { Output: Subband; Precincts: Dictionary<int, PrecinctState> }
type private ResolutionState =
    { Output: Resolution; Bands: BandState[]
      PrecinctWidth: int; PrecinctHeight: int
      FirstColumn: int; FirstRow: int; Columns: int; Rows: int }
[<Struct>]
type private PacketAddress =
    { Component: int; Resolution: int; Precinct: int; X: int64; Y: int64 }
type private TileState =
    { X0: int; Y0: int; X1: int; Y1: int
      mutable Style: CodingStyle option
      mutable Quantization: Quantization option
      Quantizations: Dictionary<int, Quantization>
      mutable Output: Tile option
      mutable Resolutions: ResolutionState[][]
      mutable Packets: IEnumerator<PacketAddress * int> option
      mutable NextPart: int
      mutable PartsCount: int }

let private buildTile (components: Component[]) (tile: TileState) (structureCount: int ref) =
    let style = tile.Style |> Option.defaultWith (fun () -> malformed "Missing COD marker")
    let defaultQ = tile.Quantization |> Option.defaultWith (fun () -> malformed "Missing QCD marker")
    let reserve count =
        require (count >= 0 && count <= maxStructures - structureCount.Value) "Packet geometry allocation limit exceeded"
        structureCount.Value <- structureCount.Value + count
    let addresses = ResizeArray<PacketAddress>()
    let states = Array.zeroCreate<ResolutionState[]> components.Length
    let tileComponents = Array.init components.Length (fun c ->
        let comp = components[c]
        let x0, y0 = ceilDiv tile.X0 comp.XRsiz, ceilDiv tile.Y0 comp.YRsiz
        let x1, y1 = ceilDiv tile.X1 comp.XRsiz, ceilDiv tile.Y1 comp.YRsiz
        require (x1 > x0 && y1 > y0) "Zero-area tile components are unsupported"
        let quantization =
            match tile.Quantizations.TryGetValue c with
            | true, q -> q
            | _ -> defaultQ
        require (not quantization.ScalarExpounded || quantization.Steps.Length >= 1 + 3 * style.DecompositionLevels) "Insufficient quantization subbands"
        require (not style.Reversible || quantization.NoQuantization) "Quantization with reversible wavelet"
        let resolutions = Array.init (style.DecompositionLevels + 1) (fun r ->
            let scale = 1L <<< (style.DecompositionLevels - r)
            let rx0, ry0 = int (ceilDiv64 (int64 x0) scale), int (ceilDiv64 (int64 y0) scale)
            let rx1, ry1 = int (ceilDiv64 (int64 x1) scale), int (ceilDiv64 (int64 y1) scale)
            let px, py = style.Precincts[r]
            let pw, ph = 1 <<< px, 1 <<< py
            let firstColumn, firstRow = rx0 / pw, ry0 / ph
            let columns = if rx1 > rx0 then ceilDiv rx1 pw - firstColumn else 0
            let rows = if ry1 > ry0 then ceilDiv ry1 ph - firstRow else 0
            require (int64 columns * int64 rows <= int64 maxStructures) "Too many precincts"
            reserve (columns * rows)
            let bx = min style.CodeblockWidthExponent (px - (if r = 0 then 0 else 1))
            let by = min style.CodeblockHeightExponent (py - (if r = 0 then 0 else 1))
            let bw, bh = 1 <<< bx, 1 <<< by
            let spw, sph = if r = 0 then pw, ph else pw / 2, ph / 2
            let makeBand kind highX highY =
                let bandScale = if r = 0 then scale else scale * 2L
                let offsetX = if highX then scale else 0L
                let offsetY = if highY then scale else 0L
                let sx0 = int (ceilDiv64 (int64 x0 - offsetX) bandScale)
                let sy0 = int (ceilDiv64 (int64 y0 - offsetY) bandScale)
                let sx1 = int (ceilDiv64 (int64 x1 - offsetX) bandScale)
                let sy1 = int (ceilDiv64 (int64 y1 - offsetY) bandScale)
                let precincts = Dictionary<int, PrecinctState>()
                let blocks = ResizeArray<Codeblock>()
                if sx1 > sx0 && sy1 > sy0 then
                    let cbx0, cby0 = sx0 / bw, sy0 / bh
                    let cbx1, cby1 = ceilDiv sx1 bw, ceilDiv sy1 bh
                    require (int64 (cbx1 - cbx0) * int64 (cby1 - cby0) <= int64 maxStructures) "Too many codeblocks"
                    reserve ((cbx1 - cbx0) * (cby1 - cby0))
                    for y = cby0 to cby1 - 1 do
                        for x = cbx0 to cbx1 - 1 do
                            let ax, ay = max sx0 (x * bw), max sy0 (y * bh)
                            // Precincts are anchored at the resolution grid, not at the
                            // clipped subband origin (important for nonzero image origins).
                            let pi, pj = ax / spw - firstColumn, ay / sph - firstRow
                            require (pi >= 0 && pi < columns && pj >= 0 && pj < rows) "Invalid codeblock precinct mapping"
                            let number = pi + pj * columns
                            let precinct =
                                match precincts.TryGetValue number with
                                | true, value -> value
                                | _ ->
                                    let value =
                                        { MinColumn = x; MinRow = y; MaxColumn = x; MaxRow = y
                                          Blocks = ResizeArray(); Inclusion = None; ZeroPlanes = None }
                                    precincts.Add(number, value)
                                    value
                            precinct.MinColumn <- min precinct.MinColumn x
                            precinct.MinRow <- min precinct.MinRow y
                            precinct.MaxColumn <- max precinct.MaxColumn x
                            precinct.MaxRow <- max precinct.MaxRow y
                            let block =
                                { X0 = ax; Y0 = ay; X1 = min sx1 ((x + 1) * bw); Y1 = min sy1 ((y + 1) * bh)
                                  ZeroBitPlanes = 0; Segments = ResizeArray() }
                            blocks.Add block
                            precinct.Blocks.Add { Output = block; Column = x; Row = y; Included = false; LengthBits = 3 }
                { Output = { Kind = kind; X0 = sx0; Y0 = sy0; X1 = sx1; Y1 = sy1; Codeblocks = blocks.ToArray() }
                  Precincts = precincts }
            let bands =
                if r = 0 then [| makeBand "LL" false false |]
                else [| makeBand "HL" true false; makeBand "LH" false true; makeBand "HH" true true |]
            for row = 0 to rows - 1 do
                for column = 0 to columns - 1 do
                    addresses.Add
                        { Component = c; Resolution = r; Precinct = column + row * columns
                          X = max (int64 tile.X0) (int64 (firstColumn + column) * int64 pw * scale * int64 comp.XRsiz)
                          Y = max (int64 tile.Y0) (int64 (firstRow + row) * int64 ph * scale * int64 comp.YRsiz) }
            { Output = { Level = r; X0 = rx0; Y0 = ry0; X1 = rx1; Y1 = ry1; Subbands = Array.map (fun (b: BandState) -> b.Output) bands }
              Bands = bands; PrecinctWidth = pw; PrecinctHeight = ph
              FirstColumn = firstColumn; FirstRow = firstRow; Columns = columns; Rows = rows })
        states[c] <- resolutions
        { X0 = x0; Y0 = y0; X1 = x1; Y1 = y1; CodingStyle = style
          Quantization = quantization; Resolutions = Array.map (fun (r: ResolutionState) -> r.Output) resolutions })
    require (int64 addresses.Count * int64 style.LayersCount <= 16000000L) "Too many packets"
    let ordered = addresses.ToArray()
    let compareAddress (a: PacketAddress) (b: PacketAddress) =
        let mutable result = 0
        let cmp x y = if result = 0 then result <- compare x y
        let cmp64 (x: int64) (y: int64) = if result = 0 then result <- compare x y
        match style.ProgressionOrder with
        | 0 | 1 -> cmp a.Resolution b.Resolution; cmp a.Component b.Component; cmp a.Precinct b.Precinct
        | 2 -> cmp a.Resolution b.Resolution; cmp64 a.Y b.Y; cmp64 a.X b.X; cmp a.Component b.Component
        | 3 -> cmp64 a.Y b.Y; cmp64 a.X b.X; cmp a.Component b.Component; cmp a.Resolution b.Resolution
        | _ -> cmp a.Component b.Component; cmp64 a.Y b.Y; cmp64 a.X b.X; cmp a.Resolution b.Resolution
        result
    Array.sortInPlaceWith compareAddress ordered
    let packets = seq {
        match style.ProgressionOrder with
        | 0 ->
            for layer = 0 to style.LayersCount - 1 do
                for address in ordered do yield address, layer
        | 1 ->
            let mutable first = 0
            while first < ordered.Length do
                let mutable finish = first + 1
                while finish < ordered.Length && ordered[finish].Resolution = ordered[first].Resolution do
                    finish <- finish + 1
                for layer = 0 to style.LayersCount - 1 do
                    for index = first to finish - 1 do yield ordered[index], layer
                first <- finish
        | _ ->
            for address in ordered do
                for layer = 0 to style.LayersCount - 1 do yield address, layer }
    tile.Resolutions <- states
    tile.Packets <- Some (packets.GetEnumerator())
    tile.Output <- Some { X0 = tile.X0; Y0 = tile.Y0; X1 = tile.X1; Y1 = tile.Y1; CodingStyle = style; Components = tileComponents }

type private PacketReader(data: byte[], start: int, finish: int) =
    let mutable position = start
    let mutable current = 0
    let mutable remaining = 0
    let mutable stuffed = false
    member _.Position = position
    member _.ReadBits(count: int) =
        require (count >= 0 && count <= 30) "Unsupported packet length bit count"
        let mutable value = 0
        for _ = 1 to count do
            if remaining = 0 then
                require (position < finish) "Truncated packet header"
                current <- int data[position]
                position <- position + 1
                if stuffed then require (current &&& 128 = 0) "Invalid packet header bit stuffing"
                remaining <- if stuffed then 7 else 8
                stuffed <- current = 255
            remaining <- remaining - 1
            value <- (value <<< 1) ||| ((current >>> remaining) &&& 1)
        value
    member _.Align() =
        remaining <- 0
        if stuffed then
            require (position < finish && data[position] &&& 128uy = 0uy) "Missing packet header stuffed byte"
            position <- position + 1
            stuffed <- false
    member _.Marker(code: int, required: bool) =
        let found = position + 1 < finish && data[position] = 255uy && int data[position + 1] = code
        require (found || not required) "Missing packet header marker"
        if found then position <- position + 2
        found
    member _.Skip(count: int) =
        require (count >= 0 && count <= finish - position) "Truncated packet body"
        position <- position + count
    member _.U16() =
        require (finish - position >= 2) "Truncated packet marker"
        let value = (int data[position] <<< 8) ||| int data[position + 1]
        position <- position + 2
        value

let private codingPasses (reader: PacketReader) =
    if reader.ReadBits 1 = 0 then 1
    elif reader.ReadBits 1 = 0 then 2
    else
        let n = reader.ReadBits 2
        if n < 3 then n + 3
        else
            let n = reader.ReadBits 5
            if n < 31 then n + 6 else reader.ReadBits 7 + 37

let private parsePackets data start finish (tile: TileState) (structureCount: int ref) =
    let reader = PacketReader(data, start, finish)
    let style = tile.Style.Value
    let packets = tile.Packets.Value
    let pending = ResizeArray<Codeblock * int * int>()
    while reader.Position < finish do
        if style.SopMarkerUsed && reader.Marker(0x91, false) then
            require (reader.U16() = 4) "Invalid SOP marker length"
            reader.U16() |> ignore
        require (packets.MoveNext()) "More packet data than declared packets"
        let address, layer = packets.Current
        pending.Clear()
        if reader.ReadBits 1 <> 0 then
            let resolution = tile.Resolutions[address.Component][address.Resolution]
            for band in resolution.Bands do
                match band.Precincts.TryGetValue address.Precinct with
                | false, _ -> ()
                | true, precinct ->
                    for block in precinct.Blocks do
                        let column, row = block.Column - precinct.MinColumn, block.Row - precinct.MinRow
                        let mutable included = false
                        let mutable firstInclusion = false
                        if block.Included then included <- reader.ReadBits 1 <> 0
                        else
                            if precinct.Inclusion.IsNone then
                                let width, height = precinct.MaxColumn - precinct.MinColumn + 1, precinct.MaxRow - precinct.MinRow + 1
                                precinct.Inclusion <- Some (InclusionTree(width, height, layer))
                                precinct.ZeroPlanes <- Some (TagTree(width, height))
                                for _ = 1 to layer do
                                    require (reader.ReadBits 1 = 0) "Invalid initial inclusion tag tree"
                            let tree = precinct.Inclusion.Value
                            if tree.Reset(column, row, layer) then
                                let mutable complete = false
                                while not complete do
                                    if reader.ReadBits 1 <> 0 then
                                        if not (tree.NextLevel()) then
                                            complete <- true
                                            included <- true
                                            firstInclusion <- true
                                            block.Included <- true
                                    else
                                        tree.IncrementValue layer
                                        complete <- true
                        if included then
                            if firstInclusion then
                                let tree = precinct.ZeroPlanes.Value
                                tree.Reset(column, row)
                                let mutable complete = false
                                let mutable increments = 0
                                while not complete do
                                    if reader.ReadBits 1 <> 0 then complete <- not (tree.NextLevel())
                                    else
                                        increments <- increments + 1
                                        require (increments <= 255) "Excessive zero bit planes"
                                        tree.IncrementValue()
                                block.Output.ZeroBitPlanes <- tree.Value |> Option.defaultWith (fun () -> malformed "Incomplete zero bit plane tag tree")
                            let passes = codingPasses reader
                            while reader.ReadBits 1 <> 0 do
                                block.LengthBits <- block.LengthBits + 1
                                require (block.LengthBits <= 30) "Excessive codeblock length exponent"
                            let mutable logarithm, n = 0, passes
                            while n > 1 do
                                logarithm <- logarithm + 1
                                n <- n >>> 1
                            let length = reader.ReadBits (logarithm + block.LengthBits)
                            pending.Add(block.Output, passes, length)
        reader.Align()
        if style.EphMarkerUsed then reader.Marker(0x92, true) |> ignore
        for block, passes, length in pending do
            let offset = reader.Position
            reader.Skip length
            require (structureCount.Value < maxStructures) "Codeblock segment allocation limit exceeded"
            structureCount.Value <- structureCount.Value + 1
            block.Segments.Add { Data = data; Start = offset; Length = length; CodingPasses = passes }
    require (reader.Position = finish) "Invalid tile-part packet boundary"

let private parseCodestream (data: byte[]) start finish colorSpace =
    let reader = Reader(data, start, finish)
    require (reader.U16() = 0xFF4F) "Missing SOC marker"
    let mutable imageBounds = None
    let mutable components: Component[] = [||]
    let mutable tiles: TileState[] = [||]
    let mutable mainStyle: CodingStyle option = None
    let mutable mainQuantization: Quantization option = None
    let mainQuantizations = Dictionary<int, Quantization>()
    let mutable currentTile: TileState option = None
    let mutable tileEnd = 0
    let mutable mainHeader = true
    let mutable ended = false
    let structures = ref 0
    while not ended do
        require (reader.Remaining >= 2) "Missing EOC marker"
        let markerStart = reader.Position
        let marker = reader.U16()
        match marker with
        | 0xFFD9 ->
            require currentTile.IsNone "EOC inside tile header"
            ended <- true
        | 0xFF93 ->
            let tile = currentTile |> Option.defaultWith (fun () -> malformed "SOD without SOT")
            if tile.Output.IsNone then buildTile components tile structures
            require (reader.Position <= tileEnd) "Tile-part header exceeds tile-part length"
            parsePackets data reader.Position tileEnd tile structures
            reader.Position <- tileEnd
            tile.NextPart <- tile.NextPart + 1
            currentTile <- None
        | _ ->
            require (marker >= 0xFF00) "Invalid codestream marker"
            let lengthStart = reader.Position
            let length = reader.U16()
            require (length >= 2 && length <= finish - lengthStart) "Invalid marker segment length"
            let segmentEnd = lengthStart + length
            if currentTile.IsSome then require (segmentEnd <= tileEnd) "Marker crosses tile-part boundary"
            let segment = Reader(data, reader.Position, segmentEnd)
            match marker with
            | 0xFF51 ->
                require (mainHeader && imageBounds.IsNone && markerStart = start + 2) "Unexpected SIZ marker"
                let capabilities = segment.U16()
                require (capabilities <= 2) "Unsupported JPEG2000 capabilities"
                let x1, y1 = coordinate segment, coordinate segment
                let x0, y0 = coordinate segment, coordinate segment
                let tileWidth, tileHeight = coordinate segment, coordinate segment
                let tileX, tileY = coordinate segment, coordinate segment
                let count = segment.U16()
                require (count > 0 && count <= 16384 && segment.Remaining = 3 * count) "Invalid SIZ component count"
                require (x1 > x0 && y1 > y0 && tileWidth > 0 && tileHeight > 0) "Invalid image or tile dimensions"
                require (tileX <= x0 && tileY <= y0 && int64 tileX + int64 tileWidth > int64 x0 && int64 tileY + int64 tileHeight > int64 y0) "Invalid tile origin"
                checkArea (x1 - x0) (y1 - y0)
                require (int64 (x1 - x0) * int64 (y1 - y0) * int64 count <= maxSamples) "Component sample allocation limit exceeded"
                components <- Array.init count (fun _ ->
                    let ssiz = segment.Byte()
                    let xr, yr = segment.Byte(), segment.Byte()
                    let precision = (ssiz &&& 127) + 1
                    require (precision <= 31 && xr > 0 && yr > 0) "Unsupported component precision or sampling"
                    { Precision = precision; IsSigned = ssiz &&& 128 <> 0; XRsiz = xr; YRsiz = yr
                      X0 = ceilDiv x0 xr; Y0 = ceilDiv y0 yr; X1 = ceilDiv x1 xr; Y1 = ceilDiv y1 yr })
                let columns, rows = ceilDiv (x1 - tileX) tileWidth, ceilDiv (y1 - tileY) tileHeight
                require (int64 columns * int64 rows <= 65535L && int64 columns * int64 rows * int64 count <= int64 maxStructures) "Too many tiles"
                tiles <- Array.init (columns * rows) (fun index ->
                    let left = int64 tileX + int64 (index % columns) * int64 tileWidth
                    let top = int64 tileY + int64 (index / columns) * int64 tileHeight
                    { X0 = int (max (int64 x0) left); Y0 = int (max (int64 y0) top)
                      X1 = int (min (int64 x1) (left + int64 tileWidth)); Y1 = int (min (int64 y1) (top + int64 tileHeight))
                      Style = None; Quantization = None; Quantizations = Dictionary()
                      Output = None; Resolutions = [||]; Packets = None; NextPart = 0; PartsCount = 0 })
                imageBounds <- Some (x0, y0, x1, y1)
            | 0xFF52 ->
                require imageBounds.IsSome "COD before SIZ"
                let style = readCodingStyle segment
                if mainHeader then mainStyle <- Some style
                else
                    let tile = currentTile |> Option.defaultWith (fun () -> malformed "COD outside tile header")
                    require (tile.NextPart = 0) "Coding style changes after first tile-part are unsupported"
                    tile.Style <- Some style
            | 0xFF5C ->
                require imageBounds.IsSome "QCD before SIZ"
                let quantization = readQuantization segment
                if mainHeader then mainQuantization <- Some quantization
                else
                    let tile = currentTile |> Option.defaultWith (fun () -> malformed "QCD outside tile header")
                    require (tile.NextPart = 0) "Quantization changes after first tile-part are unsupported"
                    tile.Quantization <- Some quantization
                    tile.Quantizations.Clear()
            | 0xFF5D ->
                require imageBounds.IsSome "QCC before SIZ"
                let comp = if components.Length < 257 then segment.Byte() else segment.U16()
                require (comp < components.Length) "Invalid QCC component index"
                let quantization = readQuantization segment
                if mainHeader then mainQuantizations[comp] <- quantization
                else
                    let tile = currentTile |> Option.defaultWith (fun () -> malformed "QCC outside tile header")
                    require (tile.NextPart = 0) "Quantization changes after first tile-part are unsupported"
                    tile.Quantizations[comp] <- quantization
            | 0xFF90 ->
                require (imageBounds.IsSome && currentTile.IsNone && length = 10) "Invalid SOT marker"
                let index = segment.U16()
                let partLength = segment.U32()
                let part, parts = segment.Byte(), segment.Byte()
                require (index < tiles.Length) "Invalid tile index"
                let tile = tiles[index]
                require (part = tile.NextPart && (parts = 0 || part < parts)) "Invalid tile-part sequence"
                require (tile.PartsCount = 0 || parts = 0 || tile.PartsCount = parts) "Inconsistent tile-part count"
                if parts <> 0 then tile.PartsCount <- parts
                if part = 0 then
                    tile.Style <- mainStyle
                    tile.Quantization <- mainQuantization
                    for KeyValue(comp, quantization) in mainQuantizations do tile.Quantizations[comp] <- quantization
                if partLength = 0u then
                    // A zero Psot is permitted only for the last tile-part; EOC
                    // terminates its body and is not part of its packet stream.
                    require (finish >= 2 && data[finish - 2] = 255uy && data[finish - 1] = 217uy) "Missing terminal EOC for zero-length tile-part"
                    tileEnd <- finish - 2
                else
                    require (partLength >= 14u && uint64 partLength <= uint64 (finish - markerStart)) "Invalid tile-part length"
                    tileEnd <- markerStart + int partLength
                require (segmentEnd <= tileEnd) "Invalid tile-part header length"
                currentTile <- Some tile
                mainHeader <- false
            | 0xFF53 -> malformed "Component coding style (COC) is unsupported"
            | 0xFF55 ->
                require (mainHeader && imageBounds.IsSome) "TLM outside main header"
                segment.Byte() |> ignore // Ztlm
                let flags = segment.Byte()
                let tileBytes = (flags >>> 4) &&& 3
                let lengthBytes = if flags &&& 64 = 0 then 2 else 4
                require (flags &&& 143 = 0 && tileBytes <= 2) "Invalid TLM parameters"
                require (segment.Remaining % (tileBytes + lengthBytes) = 0) "Truncated tile-length index"
                while segment.Remaining > 0 do
                    let tileIndex =
                        if tileBytes = 0 then 0
                        elif tileBytes = 1 then segment.Byte()
                        else segment.U16()
                    require (tileIndex < tiles.Length) "Invalid TLM tile index"
                    let partLength = if lengthBytes = 2 then uint32 (segment.U16()) else segment.U32()
                    require (partLength >= 14u) "Invalid indexed tile-part length"
            | 0xFF57 ->
                require (mainHeader && imageBounds.IsSome) "PLM outside main header"
                segment.Byte() |> ignore // Zplm
                while segment.Remaining > 0 do
                    let count = segment.U32()
                    require (uint64 count <= uint64 segment.Remaining) "Truncated packet-length index"
                    segment.Position <- segment.Position + int count
            | 0xFF58 ->
                require currentTile.IsSome "PLT outside tile header"
                require (segment.Remaining >= 1) "Truncated packet length marker"
            | 0xFF64 ->
                require imageBounds.IsSome "COM before SIZ"
                require (segment.Remaining >= 2) "Truncated comment marker"
                let registration = segment.U16()
                require (registration <= 1) "Unsupported comment registration"
            | _ -> malformed (sprintf "Unsupported codestream marker 0x%04X" marker)
            reader.Position <- segmentEnd
    require (reader.Position = finish) "Trailing bytes after EOC"
    let x0, y0, x1, y1 = imageBounds |> Option.defaultWith (fun () -> malformed "Missing SIZ marker")
    let outputTiles =
        tiles |> Array.map (fun (tile: TileState) ->
            let output = tile.Output |> Option.defaultWith (fun () -> malformed "Missing tile data")
            require (tile.PartsCount = 0 || tile.PartsCount = tile.NextPart) "Missing tile-parts"
            let packets = tile.Packets.Value
            let incomplete = packets.MoveNext()
            packets.Dispose()
            require (not incomplete) "Missing packets at end of codestream"
            output)
    { X0 = x0; Y0 = y0; X1 = x1; Y1 = y1; ColorSpace = colorSpace; Components = components; Tiles = outputTiles }

/// Parse a raw JPEG2000 codestream or JP2 file. Encoded segments reference the
/// input array without copying; the caller must not mutate it during decoding.
/// COC, ROI, progression changes, packed headers and unsupported coding modes
/// fail explicitly. Unknown ancillary JP2 boxes are skipped within checked bounds.
/// Identity channel definitions and a final straight/premultiplied opacity
/// channel preserve codestream order; PDF image handling interprets the alpha.
let parse (data: byte[]) : Image =
    require (not (isNull data) && data.Length >= 2) "Empty JPEG2000 stream"
    if data[0] = 255uy && data[1] = 79uy then parseCodestream data 0 data.Length None
    else
        let mutable codestream = None
        let mutable colorSpace = None
        let mutable signature = false
        let mutable channelDefinitions: (int * int * int)[] option = None
        let rec boxes start finish depth =
            require (depth <= 4) "Excessive JP2 box nesting"
            let reader = Reader(data, start, finish)
            while reader.Remaining > 0 do
                let boxStart = reader.Position
                require (reader.Remaining >= 8) "Truncated JP2 box header"
                let length = reader.U32()
                let kind = reader.U32()
                let fullLength =
                    if length = 0u then int64 (finish - boxStart)
                    elif length = 1u then
                        require (reader.Remaining >= 8) "Truncated extended JP2 box length"
                        let high, low = reader.U32(), reader.U32()
                        require (high = 0u) "JP2 box exceeds supported stream size"
                        int64 low
                    else int64 length
                require (fullLength >= int64 (reader.Position - boxStart) && fullLength <= int64 (finish - boxStart)) "Invalid JP2 box length"
                let boxEnd = boxStart + int fullLength
                let payload = Reader(data, reader.Position, boxEnd)
                match kind with
                | 0x6A502020u ->
                    require (depth = 0 && boxStart = 0 && fullLength = 12L && payload.U32() = 0x0D0A870Au) "Invalid JP2 signature"
                    signature <- true
                | 0x6A703268u -> boxes reader.Position boxEnd (depth + 1)
                | 0x636F6C72u ->
                    require (payload.Remaining >= 3) "Truncated JP2 colour specification"
                    let methodCode = payload.Byte()
                    let precedence = payload.Byte()
                    payload.Byte() |> ignore
                    if methodCode = 1 then
                        require (payload.Remaining = 4) "Invalid enumerated JP2 colour specification"
                        let space = payload.U32()
                        require (space <= uint32 Int32.MaxValue) "Invalid enumerated colour space"
                        if colorSpace.IsNone || precedence = 0 then colorSpace <- Some (int space)
                    elif methodCode = 2 then
                        // PDF colour spaces govern ICC interpretation; retain packet samples.
                        require (payload.Remaining >= 128) "Truncated JP2 ICC profile"
                    else malformed "Unsupported JP2 colour specification method"
                | 0x6A703263u ->
                    require (depth = 0 && codestream.IsNone) "Multiple or nested JP2 codestreams are unsupported"
                    codestream <- Some (reader.Position, boxEnd)
                | 0x70636C72u | 0x636D6170u -> malformed "JP2 palette/component mapping is unsupported"
                | 0x63646566u ->
                    require (depth = 1 && channelDefinitions.IsNone) "Invalid or duplicate JP2 channel definitions"
                    let count = payload.U16()
                    require (count > 0 && count <= 16384 && payload.Remaining = 6 * count) "Invalid JP2 channel definition length"
                    let seen = HashSet<int>()
                    channelDefinitions <- Some (Array.init count (fun _ ->
                        let channel = payload.U16()
                        let channelType = payload.U16()
                        let association = payload.U16()
                        require (seen.Add channel) "Duplicate JP2 channel definition"
                        require
                            ((channelType = 0 && association = channel + 1)
                             || ((channelType = 1 || channelType = 2) && association = 0))
                            "Unsupported JP2 channel reordering or association"
                        channel, channelType, association))
                | _ -> ()
                reader.Position <- boxEnd
        boxes 0 data.Length 0
        require signature "Missing JP2 signature"
        let start, finish = codestream |> Option.defaultWith (fun () -> malformed "Missing JP2 codestream box")
        let image = parseCodestream data start finish colorSpace
        match channelDefinitions with
        | None -> ()
        | Some definitions ->
            for channel, channelType, _ in definitions do
                require (channel < image.Components.Length) "JP2 channel index exceeds component count"
                require (channelType = 0 || channel = image.Components.Length - 1) "Only a final JP2 opacity channel is supported"
        image
