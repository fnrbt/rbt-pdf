// CCITT T.4/T.6 code tables are derived from PDF.js ccitt.js, Copyright 2012
// Mozilla Foundation and Copyright 1996-2003 Glyph & Cog, LLC, Apache-2.0:
// https://www.apache.org/licenses/LICENSE-2.0
module Rbt.Pdf.Render.CcittDecode

open System

type Options = {
    K: int
    Columns: int
    Rows: int
    EndOfLine: bool
    EncodedByteAlign: bool
    EndOfBlock: bool
    BlackIs1: bool
}

let private whiteCodes = "00110101,000111,0111,1000,1011,1100,1110,1111,10011,10100,00111,01000,001000,000011,110100,110101,101010,101011,0100111,0001100,0001000,0010111,0000011,0000100,0101000,0101011,0010011,0100100,0011000,00000010,00000011,00011010,00011011,00010010,00010011,00010100,00010101,00010110,00010111,00101000,00101001,00101010,00101011,00101100,00101101,00000100,00000101,00001010,00001011,01010010,01010011,01010100,01010101,00100100,00100101,01011000,01011001,01011010,01011011,01001010,01001011,00110010,00110011,00110100,11011,10010,010111,0110111,00110110,00110111,01100100,01100101,01101000,01100111,011001100,011001101,011010010,011010011,011010100,011010101,011010110,011010111,011011000,011011001,011011010,011011011,010011000,010011001,010011010,011000,010011011,00000001000,00000001100,00000001101,000000010010,000000010011,000000010100,000000010101,000000010110,000000010111,000000011100,000000011101,000000011110,000000011111"
let private blackCodes = "0000110111,010,11,10,011,0011,0010,00011,000101,000100,0000100,0000101,0000111,00000100,00000111,000011000,0000010111,0000011000,0000001000,00001100111,00001101000,00001101100,00000110111,00000101000,00000010111,00000011000,000011001010,000011001011,000011001100,000011001101,000001101000,000001101001,000001101010,000001101011,000011010010,000011010011,000011010100,000011010101,000011010110,000011010111,000001101100,000001101101,000011011010,000011011011,000001010100,000001010101,000001010110,000001010111,000001100100,000001100101,000001010010,000001010011,000000100100,000000110111,000000111000,000000100111,000000101000,000001011000,000001011001,000000101011,000000101100,000001011010,000001100110,000001100111,0000001111,000011001000,000011001001,000001011011,000000110011,000000110100,000000110101,0000001101100,0000001101101,0000001001010,0000001001011,0000001001100,0000001001101,0000001110010,0000001110011,0000001110100,0000001110101,0000001110110,0000001110111,0000001010010,0000001010011,0000001010100,0000001010101,0000001011010,0000001011011,0000001100100,0000001100101,00000001000,00000001100,00000001101,000000010010,000000010011,000000010100,000000010101,000000010110,000000010111,000000011100,000000011101,000000011110,000000011111"

// Expanded prefix tables: packed (bit length, terminating/makeup run).
let private buildTable width (codes: string) =
    let table = Array.zeroCreate<int> (1 <<< width)
    let strings = codes.Split(',')
    for i in 0 .. strings.Length - 1 do
        let str = strings.[i]
        let mutable code = 0
        for c in str do code <- (code <<< 1) ||| (if c = '1' then 1 else 0)
        let run = if i < 64 then i else (i - 63) * 64
        let shift = width - str.Length
        for suffix in 0 .. (1 <<< shift) - 1 do
            table.[(code <<< shift) ||| suffix] <- (str.Length <<< 16) ||| (run + 1)
    table

let private whiteTable = buildTable 13 whiteCodes
let private blackTable = buildTable 13 blackCodes
exception private InvalidFax

type private Reader(data: byte[]) =
    let mutable position = 0
    member _.Position with get() = position and set v = position <- v
    member _.Remaining = data.Length * 8 - position
    member _.Peek(n) =
        let mutable result = 0
        for j in 0 .. n-1 do
            let p = position + j
            result <- result <<< 1
            if p < data.Length * 8 then result <- result ||| ((int data.[p/8] >>> (7-p%8)) &&& 1)
        result
    member this.Take(n) =
        if n > this.Remaining then raise InvalidFax
        let v = this.Peek(n)
        position <- position + n
        v
    member _.Align() = position <- (position + 7) &&& ~~~7

let private readRun (r: Reader) black =
    let table = if black then blackTable else whiteTable
    let mutable total = 0
    let mutable more = true
    while more do
        let entry = table.[r.Peek(13)]
        let len = entry >>> 16
        if len = 0 || len > r.Remaining then raise InvalidFax
        r.Take(len) |> ignore
        let run = (entry &&& 65535) - 1
        total <- total + run
        if total > 100000000 then raise InvalidFax
        more <- run >= 64
    total

let private mode (r: Reader) =
    if r.Take(1) = 1 then 0
    elif r.Take(1) = 1 then if r.Take(1) = 1 then 1 else -1
    elif r.Take(1) = 1 then 10 // horizontal
    elif r.Take(1) = 1 then 11 // pass
    elif r.Take(1) = 1 then if r.Take(1) = 1 then 2 else -2
    elif r.Take(1) = 1 then if r.Take(1) = 1 then 3 else -3
    else raise InvalidFax

let private consumeEol (r: Reader) required =
    let saved = r.Position
    let mutable zeros = 0
    while r.Remaining > 0 && r.Peek(1) = 0 do
        r.Take(1) |> ignore
        zeros <- zeros + 1
    if zeros >= 11 && r.Remaining > 0 && r.Take(1) = 1 then true
    else
        r.Position <- saved
        if required then raise InvalidFax
        false

/// Decode a PDF CCITTFaxDecode payload. Rows are byte-padded, MSB first.
let decode (data: byte[]) (options: Options) : byte[] option =
    let width, height = options.Columns, options.Rows
    if width <= 0 || height <= 0 || int64 width * int64 height > 100000000L then None else
    try
        let stride = (width + 7) / 8
        let output = Array.create (stride * height) 255uy
        let r = Reader(data)
        let mutable reference = [|width; width|]
        for y in 0 .. height - 1 do
            if options.EncodedByteAlign && y > 0 then r.Align()
            if options.K >= 0 || options.EndOfLine then consumeEol r options.EndOfLine |> ignore
            let twoDim = if options.K < 0 then true elif options.K = 0 then false else r.Take(1) = 0
            let mutable x = 0
            let mutable black = false
            let mutable first = true
            let mutable steps = 0
            let changes = ResizeArray<int>()
            let paint finish =
                if finish < x || finish > width then raise InvalidFax
                if black then
                    let mutable p = x
                    while p < finish && p % 8 <> 0 do
                        let idx = y*stride+p/8
                        output.[idx] <- output.[idx] &&& (~~~(1uy <<< (7-p%8)))
                        p <- p + 1
                    while p + 8 <= finish do
                        output.[y*stride+p/8] <- 0uy
                        p <- p + 8
                    while p < finish do
                        let idx = y*stride+p/8
                        output.[idx] <- output.[idx] &&& (~~~(1uy <<< (7-p%8)))
                        p <- p + 1
                x <- finish
            let change () = changes.Add(x); black <- not black
            while x < width do
                steps <- steps + 1
                if steps > width*4+100 then raise InvalidFax
                if not twoDim then
                    paint (x + readRun r black)
                    change ()
                else
                    let mutable j = if black then 1 else 0
                    while j < reference.Length - 2 && (reference.[j] < x || (reference.[j] = x && not first)) do j <- j + 2
                    let b1, b2 = reference.[j], reference.[j+1]
                    match mode r with
                    | 10 ->
                        paint (x + readRun r black)
                        change ()
                        paint (x + readRun r black)
                        change ()
                    | 11 -> paint b2
                    | delta -> paint (b1 + delta); change ()
                first <- false
            changes.Add(width)
            changes.Add(width)
            reference <- changes.ToArray()
        if options.BlackIs1 then
            for i in 0 .. output.Length - 1 do output.[i] <- ~~~output.[i]
        Some output
    with InvalidFax -> None

/// JBIG2 MMR bitmap, with 0 for black and 1 for white.
let decodeMmr data width height =
    decode data { K = -1; Columns = width; Rows = height; EndOfLine = false; EncodedByteAlign = false; EndOfBlock = false; BlackIs1 = false }
