module Rbt.Pdf.Render.PdfSampled

open System

let private finite (x: float) = not (Double.IsNaN x || Double.IsInfinity x)

let private validPairs (values: float[]) =
    values <> null
    && values.Length > 0
    && values.Length % 2 = 0
    && Array.forall finite values
    && (let mutable ok = true
        let mutable i = 0
        while ok && i < values.Length do
            if values.[i] > values.[i + 1] then ok <- false
            i <- i + 2
        ok)

/// Construct a PDF type 0 sampled function. Invalid or excessively large inputs return None.
let create
    (domain: float[])
    (range: float[])
    (size: int[])
    (bits: int)
    (order: int)
    (encode: float[])
    (decode: float[])
    (data: byte[])
    : (float[] -> float[] -> bool) option =
    let dimensions = if size = null then 0 else size.Length
    let outputs = if range = null then 0 else range.Length / 2
    let legalBits = bits = 1 || bits = 2 || bits = 4 || bits = 8 || bits = 12 || bits = 16 || bits = 24 || bits = 32
    let legalOrder = order = 1
    let mutable sampleCount = 1L
    let mutable validSize = dimensions > 0 && dimensions <= 8
    if validSize then
        for i in 0 .. dimensions - 1 do
            if size.[i] <= 0 || sampleCount > 1000000L / int64 (max 1 size.[i]) then
                validSize <- false
            else
                sampleCount <- sampleCount * int64 size.[i]
    let neighbors = 2.0 ** float dimensions
    let validScalars (values: float[]) expected =
        values <> null && values.Length = expected && Array.forall finite values
    let validDomain =
        validPairs domain
        && (let mutable ok = true
            let mutable i = 0
            while ok && i < domain.Length do
                if domain.[i] >= domain.[i + 1] || not (finite (domain.[i + 1] - domain.[i])) then ok <- false
                i <- i + 2
            ok)
    let finiteSpans (values: float[]) expected =
        values <> null
        && values.Length = expected
        && (let mutable ok = true
            let mutable i = 0
            while ok && i < values.Length do
                if not (finite (values.[i + 1] - values.[i])) then ok <- false
                i <- i + 2
            ok)
    let valid =
        validDomain
        && validPairs range
        && finiteSpans range range.Length
        && finiteSpans encode (dimensions * 2)
        && finiteSpans decode (outputs * 2)
        && dimensions > 0
        && domain.Length = dimensions * 2
        && outputs > 0
        && outputs <= 64
        && validSize
        && legalBits
        && legalOrder
        && neighbors <= 65536.0
        && neighbors * float outputs * float bits <= 2000000.0
        && validScalars encode (dimensions * 2)
        && validScalars decode (outputs * 2)
        && data <> null
    if not valid then None
    else
        let neededBits = sampleCount * int64 outputs * int64 bits
        let neededBytes = (neededBits + 7L) / 8L
        if neededBytes > int64 data.Length then None
        else
            let strides = Array.zeroCreate<int> dimensions
            let mutable stride = 1
            for d in 0 .. dimensions - 1 do
                strides.[d] <- stride
                stride <- stride * size.[d]
            let maxSample = if bits = 32 then UInt32.MaxValue else (1u <<< bits) - 1u
            let readSample sample output =
                let mutable position = (sample * outputs + output) * bits
                let mutable remaining = bits
                let mutable value = 0u
                while remaining > 0 do
                    let bitInByte = position % 8
                    let take = min remaining (8 - bitInByte)
                    let shift = 8 - bitInByte - take
                    let chunk = (int data.[position / 8] >>> shift) &&& ((1 <<< take) - 1)
                    value <- (value <<< take) ||| uint32 chunk
                    position <- position + take
                    remaining <- remaining - take
                float value / float maxSample
            let lower = Array.zeroCreate<int> dimensions
            let fractions = Array.zeroCreate<float> dimensions
            let result = Array.zeroCreate<float> outputs
            let eval (input: float[]) (output: float[]) =
                if input = null || input.Length <> dimensions || output = null || output.Length <> outputs then false
                else
                    let mutable good = true
                    let mutable i = 0
                    while good && i < dimensions do
                        if not (finite input.[i]) then good <- false
                        i <- i + 1
                    if not good then false
                    else
                        let mutable encodedGood = true
                        for d in 0 .. dimensions - 1 do
                            let x = max domain.[2*d] (min domain.[2*d+1] input.[d])
                            let t = (x - domain.[2*d]) / (domain.[2*d+1] - domain.[2*d])
                            let encoded = encode.[2*d] + t * (encode.[2*d+1] - encode.[2*d])
                            if Double.IsNaN encoded then encodedGood <- false
                            else
                                let position = max 0.0 (min (float (size.[d] - 1)) encoded)
                                lower.[d] <- int (floor position)
                                fractions.[d] <- position - float lower.[d]
                        if not encodedGood then false
                        else
                            for outputIndex in 0 .. outputs - 1 do
                                result.[outputIndex] <- 0.0
                            for vertex in 0 .. (1 <<< dimensions) - 1 do
                                let mutable sampleIndex = 0
                                let mutable weight = 1.0
                                for d in 0 .. dimensions - 1 do
                                    if (vertex &&& (1 <<< d)) = 0 then
                                        sampleIndex <- sampleIndex + lower.[d] * strides.[d]
                                        weight <- weight * (1.0 - fractions.[d])
                                    else
                                        let upper = min (size.[d] - 1) (lower.[d] + 1)
                                        sampleIndex <- sampleIndex + upper * strides.[d]
                                        weight <- weight * fractions.[d]
                                for outputIndex in 0 .. outputs - 1 do
                                    result.[outputIndex] <- result.[outputIndex] + weight * readSample sampleIndex outputIndex
                            for outputIndex in 0 .. outputs - 1 do
                                let v = decode.[2*outputIndex] + result.[outputIndex] * (decode.[2*outputIndex+1] - decode.[2*outputIndex])
                                output.[outputIndex] <- max range.[2*outputIndex] (min range.[2*outputIndex+1] v)
                            true
            Some eval
