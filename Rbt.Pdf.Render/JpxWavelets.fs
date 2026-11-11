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
// Port of pdf.js v3.11.174 src/core/jpx.js Transform,
// IrreversibleTransform and ReversibleTransform (JPEG 2000 Annex F).
module Rbt.Pdf.Render.JpxWavelets
open System


/// A two-dimensional wavelet subband in row-major order.
type Level =
    { width: int
      height: int
      items: float32[]
      x0: int
      y0: int }

let private validateLevel name (level: Level) =
    if isNull level.items || level.width < 0 || level.height < 0 then
        invalidArg name "Wavelet dimensions must be non-negative and item storage must be present"
    let expected = int64 level.width * int64 level.height
    if expected > 64000000L || level.items.Length <> int expected then
        invalidArg name "Wavelet dimensions do not match item storage or exceed the allocation limit"

let private toInt32 (value: float) =
    // JavaScript's bitwise operators first apply ToInt32, including wraparound.
    if Double.IsNaN value || Double.IsInfinity value || value = 0.0 then 0
    else
        let truncated = Math.Truncate value
        let modulo = ((truncated % 4294967296.0) + 4294967296.0) % 4294967296.0
        if modulo >= 2147483648.0 then int (modulo - 4294967296.0) else int modulo

let private extend (buffer: float32[]) offset size =
    // Section F.3.7: symmetric extension, with the source's maximum padding of four.
    let mutable i1 = offset - 1
    let mutable j1 = offset + 1
    let mutable i2 = offset + size - 2
    let mutable j2 = offset + size
    for _ = 0 to 2 do
        buffer[i1] <- buffer[j1]
        i1 <- i1 - 1
        j1 <- j1 + 1
        buffer[j2] <- buffer[i2]
        j2 <- j2 + 1
        i2 <- i2 - 1
    buffer[i1] <- buffer[j1]
    buffer[j2] <- buffer[i2]

let private filter reversible (x: float32[]) offset length =
    let len = length >>> 1
    if reversible then
        let mutable j = offset
        for _ = 0 to len do
            let adjustment = (toInt32 (float x[j - 1] + float x[j + 1] + 2.0)) >>> 2
            x[j] <- float32 (float x[j] - float adjustment)
            j <- j + 2
        j <- offset + 1
        for _ = 0 to len - 1 do
            let adjustment = (toInt32 (float x[j - 1] + float x[j + 1])) >>> 1
            x[j] <- float32 (float x[j] + float adjustment)
            j <- j + 2
    else
        let alpha = -1.586134342059924
        let beta = -0.052980118572961
        let gamma = 0.882911075530934
        let delta = 0.443506852043971
        let k = 1.230174104914001
        let ki = 1.0 / k
        // As in pdf.js, Float32Array assignment rounds after each update.
        let scaleEven = offset - 3
        for n = 0 to len + 3 do
            x[scaleEven + n * 2] <- float32 (float x[scaleEven + n * 2] * ki)

        let mutable j = offset - 2
        let mutable current = delta * float x[j - 1]
        let mutable n = len + 3
        while n > 0 do
            let next = delta * float x[j + 1]
            x[j] <- float32 (k * float x[j] - current - next)
            n <- n - 1
            if n > 0 then
                j <- j + 2
                current <- delta * float x[j + 1]
                x[j] <- float32 (k * float x[j] - current - next)
                n <- n - 1
            j <- j + 2

        j <- offset - 1
        current <- gamma * float x[j - 1]
        n <- len + 2
        while n > 0 do
            let next = gamma * float x[j + 1]
            x[j] <- float32 (float x[j] - current - next)
            n <- n - 1
            if n > 0 then
                j <- j + 2
                current <- gamma * float x[j + 1]
                x[j] <- float32 (float x[j] - current - next)
                n <- n - 1
            j <- j + 2

        j <- offset
        current <- beta * float x[j - 1]
        n <- len + 1
        while n > 0 do
            let next = beta * float x[j + 1]
            x[j] <- float32 (float x[j] - current - next)
            n <- n - 1
            if n > 0 then
                j <- j + 2
                current <- beta * float x[j + 1]
                x[j] <- float32 (float x[j] - current - next)
                n <- n - 1
            j <- j + 2

        if len <> 0 then
            j <- offset + 1
            current <- alpha * float x[j - 1]
            n <- len
            while n > 0 do
                let next = alpha * float x[j + 1]
                x[j] <- float32 (float x[j] - current - next)
                n <- n - 1
                if n > 0 then
                    j <- j + 2
                    current <- alpha * float x[j + 1]
                    x[j] <- float32 (float x[j] - current - next)
                    n <- n - 1
                j <- j + 2

let private oddPhaseFilter reversible (x: float32[]) offset length =
    let last = offset + length
    let refresh () = extend x offset length
    let lift coefficient start =
        let mutable j = offset + start
        while j < last do
            x[j] <- float32 (float x[j] - coefficient * (float x[j - 1] + float x[j + 1]))
            j <- j + 2
        refresh ()
    if reversible then
        let mutable j = offset + 1
        while j < last do
            let adjustment = (toInt32 (float x[j - 1] + float x[j + 1] + 2.0)) >>> 2
            x[j] <- float32 (float x[j] - float adjustment)
            j <- j + 2
        refresh ()
        j <- offset
        while j < last do
            let adjustment = (toInt32 (float x[j - 1] + float x[j + 1])) >>> 1
            x[j] <- float32 (float x[j] + float adjustment)
            j <- j + 2
        refresh ()
    else
        let alpha = -1.586134342059924
        let beta = -0.052980118572961
        let gamma = 0.882911075530934
        let delta = 0.443506852043971
        let k = 1.230174104914001
        let mutable j = offset
        while j < last do
            x[j] <- float32 (float x[j] / k)
            j <- j + 2
        refresh ()
        j <- offset + 1
        while j < last do
            x[j] <- float32 (k * float x[j] - delta * float x[j - 1] - delta * float x[j + 1])
            j <- j + 2
        refresh ()
        lift gamma 0
        lift beta 1
        lift alpha 0

let private iterate reversible (ll: Level) (high: Level) =
    validateLevel "ll" ll
    validateLevel "high" high
    let width, height = high.width, high.height
    if high.items.Length = 0 then high
    else
        let xParity = high.x0 &&& 1
        let yParity = high.y0 &&& 1
        if int64 ll.width * 2L > int64 (width - xParity + 1)
           || int64 ll.height * 2L > int64 (height - yParity + 1) then
            invalidArg "levels" "Low-frequency band does not fit the reconstructed dimensions"
        let items = high.items
        for y = 0 to ll.height - 1 do
            let targetRow = (y * 2 + yParity) * width
            let sourceRow = y * ll.width
            for x = 0 to ll.width - 1 do
                items[targetRow + x * 2 + xParity] <- ll.items[sourceRow + x]

        let padding = 4
        if width = 1 then
            if xParity <> 0 then
                for y = 0 to height - 1 do
                    items[y] <- items[y] * 0.5f
        else
            let row = Array.zeroCreate<float32> (width + padding * 2 + xParity)
            for y = 0 to height - 1 do
                Array.Copy(items, y * width, row, padding, width)
                extend row padding width
                if xParity = 0 then filter reversible row padding width
                else oddPhaseFilter reversible row padding width
                Array.Copy(row, padding, items, y * width, width)

        if height = 1 then
            if yParity <> 0 then
                for x = 0 to width - 1 do items[x] <- items[x] * 0.5f
        else
            let column = Array.zeroCreate<float32> (height + padding * 2 + yParity)
            for x = 0 to width - 1 do
                for y = 0 to height - 1 do column[padding + y] <- items[y * width + x]
                extend column padding height
                if yParity = 0 then filter reversible column padding height
                else oddPhaseFilter reversible column padding height
                for y = 0 to height - 1 do items[y * width + x] <- column[padding + y]
        { width = width; height = height; items = items; x0 = high.x0; y0 = high.y0 }

/// Reconstruct all decomposition levels using each level's own origin.
let reconstruct (reversible: bool) (levels: Level[]) : Level =
    if isNull levels || levels.Length = 0 then invalidArg "levels" "At least one wavelet level is required"
    validateLevel "levels" levels[0]
    let mutable result = levels[0]
    for i = 1 to levels.Length - 1 do
        result <- iterate reversible result levels[i]
    result
