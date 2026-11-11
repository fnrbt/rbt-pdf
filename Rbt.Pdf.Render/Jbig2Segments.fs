// Copyright 2012 Mozilla Foundation
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy at http://www.apache.org/licenses/LICENSE-2.0
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and limitations.
// Segment syntax based on pdf.js src/core/jbig2.js and ISO/IEC 14492 section 7.
module Rbt.Pdf.Render.Jbig2Segments

open System

type Reader(data: byte[], start: int, finish: int) =
    let mutable position = start
    do if isNull data || start < 0 || finish < start || finish > data.Length then invalidArg "data" "Invalid segment range"
    member _.Position = position
    member _.Remaining = finish - position
    member _.Byte() =
        if position >= finish then invalidOp "Truncated JBIG2 segment"
        let v = int data.[position]
        position <- position + 1
        v
    member this.Int8() =
        let v = this.Byte()
        if v >= 128 then v - 256 else v
    member this.UInt16() = (this.Byte() <<< 8) ||| this.Byte()
    member this.UInt32() =
        (uint32 (this.Byte()) <<< 24) ||| (uint32 (this.Byte()) <<< 16) |||
        (uint32 (this.Byte()) <<< 8) ||| uint32 (this.Byte())
    member this.Int32() = int32 (this.UInt32()) |> int
    member this.Size() =
        let value = this.UInt32()
        if value > uint32 Int32.MaxValue then invalidOp "JBIG2 value exceeds implementation bounds"
        int value
    member _.Skip(count: int) =
        if count < 0 || count > finish - position then invalidOp "Truncated JBIG2 segment"
        position <- position + count

type Segment = {
    Number: uint32
    Kind: int
    Referred: uint32[]
    Page: uint32
    Data: byte[]
    Start: int
    EndExclusive: int
}

type Region = { Width: int; Height: int; X: int; Y: int; Operator: int }

let readRegion (r: Reader) =
    let w = r.Size()
    let h = r.Size()
    let x = r.Size()
    let y = r.Size()
    let op = r.Byte() &&& 7
    if w <= 0 || h <= 0 || int64 w * int64 h > 268435456L || op > 4 then
        invalidOp "Invalid JBIG2 region"
    { Width = w; Height = h; X = x; Y = y; Operator = op }

let readAdaptive (r: Reader) n = Array.init n (fun _ -> r.Int8(), r.Int8())

/// Parse the sequential segment organization used in PDF embedded streams.
let readSegments (data: byte[]) =
    if isNull data then invalidArg "data" "Missing JBIG2 data"
    let r = Reader(data, 0, data.Length)
    let segments = ResizeArray<Segment>()
    while r.Remaining > 0 do
        let number = r.UInt32()
        let flags = r.Byte()
        let kind = flags &&& 63
        let referredFlags = r.Byte()
        let countCode = referredFlags >>> 5
        let count =
            if countCode = 7 then
                let countValue = (uint32 (referredFlags &&& 31) <<< 24) ||| (uint32 (r.Byte()) <<< 16) ||| (uint32 (r.Byte()) <<< 8) ||| uint32 (r.Byte())
                if countValue > 1000000u then invalidOp "Too many JBIG2 segment references"
                let count = int countValue
                r.Skip((count + 1 + 7) / 8)
                count
            elif countCode >= 5 then invalidOp "Invalid JBIG2 reference count"
            else countCode
        let refSize = if number <= 256u then 1 elif number <= 65536u then 2 else 4
        if int64 count * int64 refSize > int64 r.Remaining then invalidOp "Truncated JBIG2 references"
        let references = Array.init count (fun _ -> if refSize = 1 then uint32 (r.Byte()) elif refSize = 2 then uint32 (r.UInt16()) else r.UInt32())
        let page = if (flags &&& 64) <> 0 then r.UInt32() else uint32 (r.Byte())
        let length = r.UInt32()
        let start = r.Position
        let actualLength =
            if length <> UInt32.MaxValue then
                if length > uint32 r.Remaining then invalidOp "Truncated JBIG2 segment body"
                int length
            else
                if kind <> 38 || r.Remaining < 18 then invalidOp "Invalid unknown JBIG2 segment length"
                let info = Reader(data, start, data.Length)
                let region = readRegion info
                let mmr = (info.Byte() &&& 1) <> 0
                let b0, b1 = if mmr then 0, 0 else 255, 172
                let height = uint32 region.Height
                let pattern = [| byte b0; byte b1; byte (height >>> 24); byte (height >>> 16); byte (height >>> 8); byte height |]
                let mutable i = info.Position
                let mutable found = -1
                while found < 0 && i <= data.Length - pattern.Length do
                    let mutable j = 0
                    while j < pattern.Length && data.[i + j] = pattern.[j] do j <- j + 1
                    if j = pattern.Length then found <- i + pattern.Length else i <- i + 1
                if found < 0 then invalidOp "Unknown length JBIG2 segment has no terminator"
                found - start
        r.Skip(actualLength)
        segments.Add({ Number = number; Kind = kind; Referred = references; Page = page; Data = data; Start = start; EndExclusive = start + actualLength })
    segments.ToArray()
