// Copyright 2012 Mozilla Foundation
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Port of pdf.js v3.11.174 src/core/jpx.js, Section D (BitModel), lines 1736-2117.
namespace Rbt.Pdf.Render

open System
open Rbt.Image.Codecs.ArithmeticDecoder

type BitModel(width: int, height: int, subband: string, zeroBitPlanes: int, mb: int) =
    static let llLhLabels =
        [| 0uy;5uy;8uy;0uy;3uy;7uy;8uy;0uy;4uy;7uy;8uy;0uy;0uy;0uy;0uy;0uy;1uy;6uy;8uy;0uy;3uy;7uy;8uy;0uy;4uy;7uy;8uy;0uy;0uy;0uy;0uy;0uy;2uy;6uy;8uy;0uy;3uy;7uy;8uy;0uy;4uy;7uy;8uy;0uy;0uy;0uy;0uy;0uy;2uy;6uy;8uy;0uy;3uy;7uy;8uy;0uy;4uy;7uy;8uy;0uy;0uy;0uy;0uy;0uy;2uy;6uy;8uy;0uy;3uy;7uy;8uy;0uy;4uy;7uy;8uy |]
    static let hlLabels =
        [| 0uy;3uy;4uy;0uy;5uy;7uy;7uy;0uy;8uy;8uy;8uy;0uy;0uy;0uy;0uy;0uy;1uy;3uy;4uy;0uy;6uy;7uy;7uy;0uy;8uy;8uy;8uy;0uy;0uy;0uy;0uy;0uy;2uy;3uy;4uy;0uy;6uy;7uy;7uy;0uy;8uy;8uy;8uy;0uy;0uy;0uy;0uy;0uy;2uy;3uy;4uy;0uy;6uy;7uy;7uy;0uy;8uy;8uy;8uy;0uy;0uy;0uy;0uy;0uy;2uy;3uy;4uy;0uy;6uy;7uy;7uy;0uy;8uy;8uy;8uy |]
    static let hhLabels =
        [| 0uy;1uy;2uy;0uy;1uy;2uy;2uy;0uy;2uy;2uy;2uy;0uy;0uy;0uy;0uy;0uy;3uy;4uy;5uy;0uy;4uy;5uy;5uy;0uy;5uy;5uy;5uy;0uy;0uy;0uy;0uy;0uy;6uy;7uy;7uy;0uy;7uy;7uy;7uy;0uy;7uy;7uy;7uy;0uy;0uy;0uy;0uy;0uy;8uy;8uy;8uy;0uy;8uy;8uy;8uy;0uy;8uy;8uy;8uy;0uy;0uy;0uy;0uy;0uy;8uy;8uy;8uy;0uy;8uy;8uy;8uy;0uy;8uy;8uy;8uy |]

    do
        if width < 0 || height < 0 || (width <> 0 && height > Int32.MaxValue / width) then
            invalidArg "width" "Invalid JPEG 2000 codeblock dimensions"
        if zeroBitPlanes < 0 || zeroBitPlanes > 255 then invalidArg "zeroBitPlanes" "Must fit in a byte"
        if mb < 0 then invalidArg "mb" "Must be non-negative"

    let coefficientCount = width * height
    let neighborsSignificance = Array.zeroCreate<byte> coefficientCount
    let coefficientsSign = Array.zeroCreate<byte> coefficientCount
    let coefficientsMagnitude = Array.zeroCreate<uint32> coefficientCount
    let processingFlags = Array.zeroCreate<byte> coefficientCount
    let bitsDecoded = Array.create coefficientCount (byte zeroBitPlanes)
    let contexts = Array.zeroCreate<byte> 19
    let labels = if subband = "HH" then hhLabels elif subband = "HL" then hlLabels else llLhLabels
    let mutable decoder: Decoder option = None

    let getDecoder () =
        match decoder with
        | Some d -> d
        | None -> invalidOp "BitModel decoder has not been set"

    let setNeighborsSignificance (row: int) (column: int) (index: int) =
        if row > 0 then
            let i = index - width
            if column > 0 then neighborsSignificance.[i - 1] <- neighborsSignificance.[i - 1] + 0x10uy
            if column + 1 < width then neighborsSignificance.[i + 1] <- neighborsSignificance.[i + 1] + 0x10uy
            neighborsSignificance.[i] <- neighborsSignificance.[i] + 0x04uy
        if row + 1 < height then
            let i = index + width
            if column > 0 then neighborsSignificance.[i - 1] <- neighborsSignificance.[i - 1] + 0x10uy
            if column + 1 < width then neighborsSignificance.[i + 1] <- neighborsSignificance.[i + 1] + 0x10uy
            neighborsSignificance.[i] <- neighborsSignificance.[i] + 0x04uy
        if column > 0 then neighborsSignificance.[index - 1] <- neighborsSignificance.[index - 1] + 1uy
        if column + 1 < width then neighborsSignificance.[index + 1] <- neighborsSignificance.[index + 1] + 1uy
        neighborsSignificance.[index] <- neighborsSignificance.[index] ||| 0x80uy

    let decodeSignBit (row: int) (column: int) (index: int) =
        let mutable contribution = 0
        let left = column > 0 && coefficientsMagnitude.[index - 1] <> 0u
        let right = column + 1 < width && coefficientsMagnitude.[index + 1] <> 0u
        let horizontal =
            if right then
                let s1 = int coefficientsSign.[index + 1]
                if left then 1 - s1 - int coefficientsSign.[index - 1] else 1 - s1 - s1
            elif left then
                let s0 = int coefficientsSign.[index - 1]
                1 - s0 - s0
            else 0
        contribution <- horizontal * 3
        let above = row > 0 && coefficientsMagnitude.[index - width] <> 0u
        let below = row + 1 < height && coefficientsMagnitude.[index + width] <> 0u
        if below then
            let s1 = int coefficientsSign.[index + width]
            contribution <- contribution + (if above then 1 - s1 - int coefficientsSign.[index - width] else 1 - s1 - s1)
        elif above then
            let s0 = int coefficientsSign.[index - width]
            contribution <- contribution + 1 - s0 - s0
        let contextLabel = if contribution >= 0 then 9 + contribution else 9 - contribution
        let decoded = (getDecoder()).ReadBit(contexts, contextLabel)
        byte (if contribution >= 0 then decoded else decoded ^^^ 1)

    do
        contexts.[0] <- 8uy
        contexts.[17] <- 92uy
        contexts.[18] <- 6uy

    member _.Width = width
    member _.Height = height
    member _.CoefficientsSign = coefficientsSign
    member _.CoefficientsMagnitude = coefficientsMagnitude
    member _.BitsDecoded = bitsDecoded
    // Source-compatible spellings used by pdf.js copyCoefficients.
    member _.CoefficentsSign = coefficientsSign
    member _.CoefficentsMagnitude = coefficientsMagnitude
    member _.NeighborsSignificance = neighborsSignificance
    member _.ProcessingFlags = processingFlags
    member _.Contexts = contexts
    member _.SetDecoder(value: Decoder) = decoder <- Some value
    member _.Reset() =
        Array.fill contexts 0 contexts.Length 0uy
        contexts.[0] <- 8uy
        contexts.[17] <- 92uy
        contexts.[18] <- 6uy

    member _.RunSignificancePropagationPass() =
        let d = getDecoder()
        for i0 in 0 .. 4 .. height - 1 do
            for j in 0 .. width - 1 do
                let mutable index = i0 * width + j
                for i1 in 0 .. 3 do
                    let i = i0 + i1
                    if i < height then
                        processingFlags.[index] <- processingFlags.[index] &&& 0xfeuy
                        if coefficientsMagnitude.[index] = 0u && neighborsSignificance.[index] <> 0uy then
                            let decision = d.ReadBit(contexts, int labels.[int neighborsSignificance.[index]])
                            if decision <> 0 then
                                coefficientsSign.[index] <- decodeSignBit i j index
                                coefficientsMagnitude.[index] <- 1u
                                setNeighborsSignificance i j index
                                processingFlags.[index] <- processingFlags.[index] ||| 2uy
                            bitsDecoded.[index] <- bitsDecoded.[index] + 1uy
                            processingFlags.[index] <- processingFlags.[index] ||| 1uy
                    index <- index + width

    member _.RunMagnitudeRefinementPass() =
        let d = getDecoder()
        let length = coefficientCount
        let width4 = width * 4
        let mutable index0 = 0
        while index0 < length do
            let indexNext = min length (index0 + width4)
            for j in 0 .. width - 1 do
                let mutable index = index0 + j
                while index < indexNext do
                    if coefficientsMagnitude.[index] <> 0u && (processingFlags.[index] &&& 1uy) = 0uy then
                        let first = (processingFlags.[index] &&& 2uy) <> 0uy
                        let contextLabel = if first then (processingFlags.[index] <- processingFlags.[index] ^^^ 2uy; if (neighborsSignificance.[index] &&& 127uy) = 0uy then 15 else 14) else 16
                        coefficientsMagnitude.[index] <- (coefficientsMagnitude.[index] <<< 1) ||| uint32 (d.ReadBit(contexts, contextLabel))
                        bitsDecoded.[index] <- bitsDecoded.[index] + 1uy
                        processingFlags.[index] <- processingFlags.[index] ||| 1uy
                    index <- index + width
            index0 <- indexNext

    member _.RunCleanupPass() =
        let d = getDecoder()
        let mutable i0 = 0
        while i0 < height do
            let iNext = min (i0 + 4) height
            let indexBase = i0 * width
            let checkAllEmpty = i0 + 3 < height
            for j in 0 .. width - 1 do
                let index0 = indexBase + j
                let allEmpty =
                    checkAllEmpty
                    && processingFlags.[index0] = 0uy && processingFlags.[index0 + width] = 0uy
                    && processingFlags.[index0 + 2 * width] = 0uy && processingFlags.[index0 + 3 * width] = 0uy
                    && neighborsSignificance.[index0] = 0uy && neighborsSignificance.[index0 + width] = 0uy
                    && neighborsSignificance.[index0 + 2 * width] = 0uy && neighborsSignificance.[index0 + 3 * width] = 0uy
                let mutable i1 = 0
                let mutable index = index0
                let mutable i = i0
                if allEmpty then
                    if d.ReadBit(contexts, 18) = 0 then
                        for r in 0 .. 3 do bitsDecoded.[index0 + r * width] <- bitsDecoded.[index0 + r * width] + 1uy
                        index <- -1
                    else
                        i1 <- (d.ReadBit(contexts,17) <<< 1) ||| d.ReadBit(contexts,17)
                        if i1 <> 0 then i <- i0 + i1; index <- index + i1 * width
                        coefficientsSign.[index] <- decodeSignBit i j index
                        coefficientsMagnitude.[index] <- 1u
                        setNeighborsSignificance i j index
                        processingFlags.[index] <- processingFlags.[index] ||| 2uy
                        index <- index0
                        for row in i0 .. i do
                            bitsDecoded.[index] <- bitsDecoded.[index] + 1uy
                            index <- index + width
                        i1 <- i1 + 1
                if index <> -1 then
                    for row in i0 + i1 .. iNext - 1 do
                        if coefficientsMagnitude.[index] = 0u && (processingFlags.[index] &&& 1uy) = 0uy then
                            if d.ReadBit(contexts, int labels.[int neighborsSignificance.[index]]) <> 0 then
                                coefficientsSign.[index] <- decodeSignBit row j index
                                coefficientsMagnitude.[index] <- 1u
                                setNeighborsSignificance row j index
                                processingFlags.[index] <- processingFlags.[index] ||| 2uy
                            bitsDecoded.[index] <- bitsDecoded.[index] + 1uy
                        index <- index + width
            i0 <- iNext

    member _.CheckSegmentationSymbol() =
        let d = getDecoder()
        let symbol = (d.ReadBit(contexts,17) <<< 3) ||| (d.ReadBit(contexts,17) <<< 2) ||| (d.ReadBit(contexts,17) <<< 1) ||| d.ReadBit(contexts,17)
        if symbol <> 0xA then invalidOp "JPX error: Invalid segmentation symbol"
