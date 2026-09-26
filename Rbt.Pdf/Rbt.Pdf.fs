namespace Rbt.Pdf

open System
open System.IO
open System.Text
#if !FABLE_COMPILER
open System.IO.Compression
#endif

[<AutoOpen>]
module Types =
    type ObjNum = int
    type GenNum = int
    type IndirectRef = ObjNum * GenNum

    type PdfObj =
        | PdfInt of int64
        | PdfReal of float
        | PdfBool of bool
        | PdfNull
        | PdfString of byte[]
        | PdfName of string
        | PdfArray of PdfObj list
        | PdfDict of Map<string, PdfObj>
        | PdfStream of header: Map<string, PdfObj> * data: byte[]
        | PdfIndirect of IndirectRef

    type XRefEntry =
        | Free of nextFree: int * gen: int
        | InUse of pos: int64 * gen: int * cached: PdfObj option
        | Compressed of objStream: int * index: int

    type PdfDocument = {
        mutable Data: byte[]
        mutable XRef: XRefEntry[]
        mutable XRefType: string  // "table" or "stream"
        mutable Trailer: Map<string, PdfObj>
        mutable Catalog: PdfObj
        mutable Modified: Set<int>
    }

module Binary =
    let slice (data: byte[]) start len =
        if start < 0 || start + len > data.Length then
            Array.empty
        else
            data.[start .. start + len - 1]

    let findLast (pattern: byte[]) (data: byte[]) =
        let rec search i =
            if i < 0 then -1
            elif data.[i .. i + pattern.Length - 1] = pattern then i
            else search (i - 1)
        search (data.Length - pattern.Length)

    let findLastInRange (pattern: byte[]) (data: byte[]) maxBack =
        let start = max 0 (data.Length - maxBack)
        let chunk = data.[start ..]
        match findLast pattern chunk with
        | -1 -> -1
        | i -> start + i

    /// True when `pattern` occurs in `data` at `pos`.
    let matchesAt (data: byte[]) (pos: int) (pattern: byte[]) =
        if pos < 0 || pos + pattern.Length > data.Length then false
        else
            let mutable i = 0
            while i < pattern.Length && data.[pos + i] = pattern.[i] do i <- i + 1
            i = pattern.Length

    /// First index of `pattern` in `data` at or after `start` and before `limit`
    /// (the match must start before `limit`), or -1.
    let indexOfBefore (pattern: byte[]) (data: byte[]) (start: int) (limit: int) =
        let last = min (data.Length - pattern.Length) (limit - 1)
        let mutable i = max 0 start
        let mutable found = -1
        while found < 0 && i <= last do
            if data.[i] = pattern.[0] && matchesAt data i pattern then found <- i
            else i <- i + 1
        found

    /// First index of `pattern` in `data` at or after `start`, or -1.
    let indexOf (pattern: byte[]) (data: byte[]) (start: int) =
        indexOfBefore pattern data start data.Length

[<AutoOpen>]
module Compat =
    let bytesToAscii (data: byte[]) =
        String(data |> Array.map char)

    let bytesToAsciiRange (data: byte[]) (offset: int) (count: int) =
        String(data.[offset .. offset + count - 1] |> Array.map char)

    let asciiBytes (text: string) =
        Array.init text.Length (fun i -> byte (int text.[i] &&& 0x7F))

    let latin1String (data: byte[]) =
        String(data |> Array.map char)

    let latin1Bytes (text: string) =
        Array.init text.Length (fun i -> byte (int text.[i] &&& 0xFF))

    let bytesToHex (data: byte[]) =
        let chars = "0123456789ABCDEF"
        let sb = StringBuilder(data.Length * 2)
        for b in data do
            sb.Append(chars.[int b >>> 4]).Append(chars.[int b &&& 0x0F]) |> ignore
        sb.ToString()

    let hexToBytes (hex: string) =
        Array.init (hex.Length / 2) (fun i ->
            byte (Convert.ToInt32(hex.Substring(i * 2, 2), 16)))

module Lexer =
    type Token =
        | TokInt of int64
        | TokReal of float
        | TokName of string
        | TokString of byte[]
        | TokHexString of byte[]
        | TokKeyword of string
        | TokOpenArray
        | TokCloseArray
        | TokOpenDict
        | TokCloseDict
        | TokEof

    type LexState = { Data: byte[]; mutable Pos: int }

    let create data pos = { Data = data; Pos = pos }

    let peek (s: LexState) =
        if s.Pos >= s.Data.Length then None
        else Some s.Data.[s.Pos]

    let advance (s: LexState) n = s.Pos <- s.Pos + n

    let read (s: LexState) =
        match peek s with
        | None -> None
        | Some b -> advance s 1; Some b

    let isWhitespace b = b = 0uy || b = 9uy || b = 10uy || b = 12uy || b = 13uy || b = 32uy
    let isDelimiter b = Array.contains b [| 40uy; 41uy; 60uy; 62uy; 91uy; 93uy; 123uy; 125uy; 47uy; 37uy |]
    let isDigit b = b >= 48uy && b <= 57uy
    let isHexDigit b = isDigit b || (b >= 65uy && b <= 70uy) || (b >= 97uy && b <= 102uy)

    let skipWhitespaceAndComments (s: LexState) =
        let rec loop () =
            match peek s with
            | None -> ()
            | Some b when isWhitespace b -> advance s 1; loop ()
            | Some 37uy -> // %
                while peek s |> Option.map (fun b -> b <> 10uy && b <> 13uy) |> Option.defaultValue false do
                    advance s 1
                loop ()
            | _ -> ()
        loop ()

    let readName (s: LexState) =
        let sb = StringBuilder()
        let rec loop () =
            match peek s with
            | None -> ()
            | Some b when isWhitespace b || isDelimiter b -> ()
            | Some 35uy -> // #
                advance s 1
                let h1 = read s
                let h2 = read s
                match h1, h2 with
                | Some a, Some b ->
                    let hex = bytesToAscii [| a; b |]
                    sb.Append(char (Convert.ToInt32(hex, 16))) |> ignore
                | _ -> ()
                loop ()
            | Some b ->
                advance s 1
                sb.Append(char b) |> ignore
                loop ()
        loop ()
        sb.ToString()

    let readNumber (s: LexState) (first: byte) =
        let sb = StringBuilder()
        sb.Append(char first) |> ignore
        let mutable isReal = (first = 46uy)
        let rec loop () =
            match peek s with
            | Some b when isDigit b ->
                advance s 1
                sb.Append(char b) |> ignore
                loop ()
            | Some 46uy when not isReal ->
                advance s 1
                sb.Append('.') |> ignore
                isReal <- true
                loop ()
            | _ -> ()
        loop ()
        let str = sb.ToString()
        if isReal then TokReal (Double.Parse(str))
        else TokInt (Int64.Parse(str))

    let readLiteralString (s: LexState) =
        let bytes = ResizeArray<byte>()
        let mutable depth = 1
        while depth > 0 do
            match read s with
            | None -> failwith "Unterminated string"
            | Some 40uy -> depth <- depth + 1; bytes.Add(40uy)
            | Some 41uy ->
                depth <- depth - 1
                if depth > 0 then bytes.Add(41uy)
            | Some 92uy -> // backslash
                match read s with
                | None -> ()
                | Some 110uy -> bytes.Add(10uy)  // \n
                | Some 114uy -> bytes.Add(13uy)  // \r
                | Some 116uy -> bytes.Add(9uy)   // \t
                | Some 98uy -> bytes.Add(8uy)    // \b
                | Some 102uy -> bytes.Add(12uy)  // \f
                | Some b when b >= 48uy && b <= 55uy ->
                    let mutable octal = int b - 48
                    for _ in 1..2 do
                        match peek s with
                        | Some b2 when b2 >= 48uy && b2 <= 55uy ->
                            advance s 1
                            octal <- octal * 8 + (int b2 - 48)
                        | _ -> ()
                    bytes.Add(byte octal)
                | Some 13uy ->
                    match peek s with
                    | Some 10uy -> advance s 1
                    | _ -> ()
                | Some 10uy -> ()
                | Some b -> bytes.Add(b)
            | Some b -> bytes.Add(b)
        bytes.ToArray()

    let readHexString (s: LexState) =
        let sb = StringBuilder()
        let rec loop () =
            match read s with
            | None -> failwith "Unterminated hex string"
            | Some 62uy -> () // >
            | Some b when isHexDigit b ->
                sb.Append(char b) |> ignore
                loop ()
            | Some _ -> loop () // skip whitespace
        loop ()
        let hex = sb.ToString()
        let hex = if hex.Length % 2 = 1 then hex + "0" else hex
        hexToBytes hex

    let readKeyword (s: LexState) (first: byte) =
        let sb = StringBuilder()
        sb.Append(char first) |> ignore
        let rec loop () =
            match peek s with
            | None -> ()
            | Some b when isWhitespace b || isDelimiter b -> ()
            | Some b ->
                advance s 1
                sb.Append(char b) |> ignore
                loop ()
        loop ()
        sb.ToString()

    let nextToken (s: LexState) =
        skipWhitespaceAndComments s
        match read s with
        | None -> TokEof
        | Some 47uy -> TokName (readName s)  // /
        | Some 40uy -> TokString (readLiteralString s)  // (
        | Some 60uy ->  // <
            match peek s with
            | Some 60uy -> advance s 1; TokOpenDict
            | _ -> TokHexString (readHexString s)
        | Some 62uy ->  // >
            match read s with
            | Some 62uy -> TokCloseDict
            | _ -> failwith "Unexpected >"
        | Some 91uy -> TokOpenArray  // [
        | Some 93uy -> TokCloseArray  // ]
        | Some b when isDigit b || b = 43uy || b = 45uy || b = 46uy ->
            readNumber s b
        | Some b -> TokKeyword (readKeyword s b)

module Parser =
    open Lexer

    /// Skip the EOL that follows the `stream` keyword (CRLF, LF, or a bare CR).
    let skipStreamEol (s: LexState) =
        match peek s with
        | Some 13uy ->
            advance s 1
            if peek s = Some 10uy then advance s 1
        | Some 10uy -> advance s 1
        | _ -> ()

    /// Read a stream body starting at s.Pos (just past the EOL after `stream`)
    /// and consume the following `endstream`/`endobj`.
    ///
    /// A known /Length is trusted when `endstream` follows it (after optional
    /// whitespace). Otherwise (no usable length, or a wrong one) the data runs
    /// to the next `endstream`, less only the single EOL that precedes that
    /// keyword: the data's own trailing CR/LF bytes are part of the stream
    /// (compressed data often ends in one).
    let readStreamData (s: LexState) (declared: int64 option) : byte[] =
        let data = s.Data
        let start = s.Pos
        let endstreamAfter p =
            let mutable q = p
            while q < data.Length && isWhitespace data.[q] do q <- q + 1
            Binary.matchesAt data q "endstream"B
        let body =
            match declared with
            | Some n when n >= 0L && int64 start + n <= int64 data.Length && endstreamAfter (start + int n) ->
                s.Pos <- start + int n
                Binary.slice data start (int n)
            | _ ->
                match Binary.indexOf "endstream"B data start with
                | -1 ->
                    // No endstream at all: take the declared length if it fits, else the rest.
                    let n =
                        match declared with
                        | Some n when n >= 0L && int64 start + n <= int64 data.Length -> int n
                        | _ -> data.Length - start
                    s.Pos <- start + n
                    Binary.slice data start n
                | found ->
                    let mutable e = found
                    if e > start && data.[e - 1] = 10uy then e <- e - 1
                    if e > start && data.[e - 1] = 13uy then e <- e - 1
                    s.Pos <- found
                    Binary.slice data start (e - start)
        skipWhitespaceAndComments s
        if Binary.matchesAt data s.Pos "endstream"B then s.Pos <- s.Pos + 9
        let saved = s.Pos
        match nextToken s with
        | TokKeyword "endobj" -> ()
        | _ -> s.Pos <- saved
        body

    /// Finish an object body whose value `obj` has been parsed: a stream body
    /// if `stream` follows, else the (optional) `endobj`. An indirect /Length is
    /// resolved through `resolveLength` when the caller can.
    let finishObjBody (resolveLength: PdfObj -> int64 option) (s: LexState) (obj: PdfObj) : PdfObj =
        let saved = s.Pos
        match nextToken s with
        | TokKeyword "endobj" -> obj
        | TokKeyword "stream" ->
            match obj with
            | PdfDict header ->
                skipStreamEol s
                let declared =
                    match Map.tryFind "Length" header with
                    | Some (PdfInt n) -> Some n
                    | Some (PdfIndirect _ as r) -> resolveLength r
                    | _ -> None
                PdfStream (header, readStreamData s declared)
            | _ -> failwith "Stream without dict header"
        | _ ->
            // A missing endobj is common in damaged files; the value stands.
            s.Pos <- saved
            obj

    let rec parseObj (s: LexState) : PdfObj =
        match nextToken s with
        | TokInt n -> PdfInt n
        | TokReal r -> PdfReal r
        | TokName n -> PdfName n
        | TokString bytes -> PdfString bytes
        | TokHexString bytes -> PdfString bytes
        | TokKeyword "true" -> PdfBool true
        | TokKeyword "false" -> PdfBool false
        | TokKeyword "null" -> PdfNull
        | TokOpenArray -> parseArray s []
        | TokOpenDict -> PdfDict (parseDict s [])
        | TokEof -> failwith "Unexpected EOF"
        | tok -> failwithf "Unexpected token: %A" tok

    and parseArray (s: LexState) acc =
        match nextToken s with
        | TokCloseArray -> PdfArray (List.rev acc)
        | TokInt n ->
            let savedPos = s.Pos
            let obj = resolveIndirect s (PdfInt n) acc savedPos
            parseArray s obj
        | TokReal r -> parseArray s (PdfReal r :: acc)
        | TokName n -> parseArray s (PdfName n :: acc)
        | TokString bytes -> parseArray s (PdfString bytes :: acc)
        | TokHexString bytes -> parseArray s (PdfString bytes :: acc)
        | TokKeyword "true" -> parseArray s (PdfBool true :: acc)
        | TokKeyword "false" -> parseArray s (PdfBool false :: acc)
        | TokKeyword "null" -> parseArray s (PdfNull :: acc)
        | TokOpenArray ->
            let nested = parseArray s []
            parseArray s (nested :: acc)
        | TokOpenDict ->
            let nested = PdfDict (parseDict s [])
            parseArray s (nested :: acc)
        | tok -> failwithf "Unexpected in array: %A" tok

    and resolveIndirect (s: LexState) first acc savedPos =
        match nextToken s with
        | TokInt gen ->
            let savedPos2 = s.Pos
            match nextToken s with
            | TokKeyword "R" ->
                match first with
                | PdfInt objNum -> PdfIndirect (int objNum, int gen) :: acc
                | _ -> failwith "Invalid indirect reference"
            | _ ->
                // Not "R", restore to after first int
                s.Pos <- savedPos
                first :: acc
        | TokKeyword "R" ->
            match acc, first with
            | PdfInt gen :: rest, PdfInt objNum ->
                PdfIndirect (int objNum, int gen) :: rest
            | _ -> failwith "Invalid indirect reference"
        | TokCloseArray ->
            s.Pos <- savedPos  // restore
            first :: acc
        | _ ->
            s.Pos <- savedPos  // restore
            first :: acc

    and parseDict (s: LexState) acc =
        match nextToken s with
        | TokCloseDict -> acc |> List.rev |> Map.ofList
        | TokName key ->
            let value = parseValue s
            parseDict s ((key, value) :: acc)
        | tok -> failwithf "Expected name in dict, got %A" tok

    and parseValue (s: LexState) : PdfObj =
        match nextToken s with
        | TokInt n ->
            // Could be: plain int, or start of "objNum genNum R" indirect ref
            let savedPos = s.Pos
            match nextToken s with
            | TokInt gen ->
                match nextToken s with
                | TokKeyword "R" -> PdfIndirect (int n, int gen)
                | TokKeyword "obj" -> parseObjBody s
                | _ ->
                    // Not an indirect ref or obj, back up to after first int
                    s.Pos <- savedPos
                    PdfInt n
            | TokKeyword "R" -> PdfIndirect (int n, 0)
            | _ ->
                // Just a plain integer, restore position
                s.Pos <- savedPos
                PdfInt n
        | TokReal r -> PdfReal r
        | TokName n -> PdfName n
        | TokString bytes -> PdfString bytes
        | TokHexString bytes -> PdfString bytes
        | TokKeyword "true" -> PdfBool true
        | TokKeyword "false" -> PdfBool false
        | TokKeyword "null" -> PdfNull
        | TokOpenArray -> parseArray s []
        | TokOpenDict -> PdfDict (parseDict s [])
        | tok -> failwithf "Unexpected value token: %A" tok

    and parseObjBody (s: LexState) : PdfObj =
        finishObjBody (fun _ -> None) s (parseObj s)

    /// Parse `n g obj ... endobj` at s.Pos, resolving an indirect stream
    /// /Length through `resolveLength`.
    let parseIndirectObjWith (resolveLength: PdfObj -> int64 option) (s: LexState) expectedObjNum (_expectedGen: int) =
        match nextToken s with
        | TokInt objNum ->
            if expectedObjNum >= 0 && int objNum <> expectedObjNum then
                failwithf "Expected obj %d, got %d" expectedObjNum (int objNum)
            match nextToken s with
            | TokInt _ ->
                match nextToken s with
                | TokKeyword "obj" -> finishObjBody resolveLength s (parseObj s)
                | tok -> failwithf "Expected obj, got %A" tok
            | tok -> failwithf "Expected gen, got %A" tok
        | tok -> failwithf "Expected objnum, got %A" tok

    let parseIndirectObj (s: LexState) expectedObjNum expectedGen =
        parseIndirectObjWith (fun _ -> None) s expectedObjNum expectedGen

    /// True when `n g obj` starts at `pos` (after whitespace) for object `objNum`.
    let objectHeaderAt (data: byte[]) (pos: int64) (objNum: int) =
        if pos < 0L || pos >= int64 data.Length then false
        else
            let s = create data (int pos)
            try
                match nextToken s, nextToken s, nextToken s with
                | TokInt n, TokInt _, TokKeyword "obj" -> int n = objNum
                | _ -> false
            with _ -> false

module XRef =
    open Lexer

    /// Grow the xref array to hold at least `size` entries, padding with Free.
    /// (The original code hard-capped at 65536 and silently dropped objects with
    /// higher numbers — a real failure on PDFs with many incremental updates.)
    let ensureSize (xref: XRefEntry[]) (size: int) : XRefEntry[] =
        if size <= xref.Length then xref
        else Array.init size (fun i -> if i < xref.Length then xref.[i] else Free (0, 0))

    let parseXRefTable (s: LexState) (xref0: XRefEntry[]) : XRefEntry[] =
        let mutable xref = xref0
        let readLine () =
            let start = s.Pos
            while s.Pos < s.Data.Length && s.Data.[s.Pos] <> 10uy && s.Data.[s.Pos] <> 13uy do
                s.Pos <- s.Pos + 1
            let line = bytesToAscii s.Data.[start .. s.Pos - 1]
            if s.Pos < s.Data.Length && s.Data.[s.Pos] = 13uy then s.Pos <- s.Pos + 1
            if s.Pos < s.Data.Length && s.Data.[s.Pos] = 10uy then s.Pos <- s.Pos + 1
            line

        let parseInt (text: string) =
            match Int64.TryParse text with
            | true, v -> Some v
            | _ -> None

        // Lenient: `trailer` may share a line with its dictionary (the caller
        // parses the dictionary from s.Pos), entries are split on whitespace
        // rather than read at fixed columns, and a subsection whose count
        // overruns into `trailer` stops there. Anything else unparseable raises,
        // and the caller falls back to reconstructing the xref.
        let rec parseSubsections () =
            skipWhitespaceAndComments s
            if Binary.matchesAt s.Data s.Pos "trailer"B then
                s.Pos <- s.Pos + 7
            elif s.Pos >= s.Data.Length then
                failwith "xref table has no trailer"
            else
                let line = readLine ()
                let parts = line.Split([| ' '; '\t'; '\012'; '\000' |], StringSplitOptions.RemoveEmptyEntries)
                match parts |> Array.truncate 2 |> Array.map parseInt with
                | [| Some startObj; Some count |] when startObj >= 0L && count >= 0L && startObj + count < 50_000_000L ->
                    let startObj, count = int startObj, int count
                    xref <- ensureSize xref (startObj + count)
                    let mutable i = 0
                    while i < count do
                        skipWhitespaceAndComments s
                        if Binary.matchesAt s.Data s.Pos "trailer"B then
                            i <- count  // count overran the entries
                        else
                            let entryLine = readLine ()
                            let fields = entryLine.Split([| ' '; '\t'; '\012'; '\000' |], StringSplitOptions.RemoveEmptyEntries)
                            if fields.Length < 3 then failwithf "Malformed xref entry: %s" entryLine
                            let objNum = startObj + i
                            match parseInt fields.[0], parseInt fields.[1] with
                            | Some pos, Some gen ->
                                if objNum < xref.Length &&
                                   (match xref.[objNum] with Free (0, 0) -> true | _ -> false) then
                                    xref.[objNum] <-
                                        if fields.[2].StartsWith "f" then Free (int pos, int gen)
                                        else InUse (pos, int gen, None)
                            | _ -> failwithf "Malformed xref entry: %s" entryLine
                            i <- i + 1
                    parseSubsections ()
                | _ -> failwithf "Malformed xref subsection: %s" line

        parseSubsections ()
        xref

    let parseXRefStream (data: byte[]) (header: Map<string, PdfObj>) (xref0: XRefEntry[]) : XRefEntry[] =
        let mutable xref = xref0
        let size =
            match Map.tryFind "Size" header with
            | Some (PdfInt n) -> int n
            | _ -> xref.Length

        let index =
            match Map.tryFind "Index" header with
            | Some (PdfArray arr) ->
                arr |> List.choose (function PdfInt n -> Some (int n) | _ -> None)
            | _ -> [0; size]

        let widths =
            match Map.tryFind "W" header with
            | Some (PdfArray arr) ->
                arr |> List.choose (function PdfInt n -> Some (int n) | _ -> None)
            | _ -> failwith "Missing W in xref stream"

        let readInt (data: byte[]) offset width =
            let mutable v = 0L
            for i in 0 .. width - 1 do
                v <- (v <<< 8) ||| int64 data.[offset + i]
            v

        let w0, w1, w2 =
            match widths with
            | [a; b; c] -> a, b, c
            | _ -> failwith "Invalid W array"

        let entrySize = w0 + w1 + w2
        let mutable dataPos = 0
        let mutable i = 0
        while i < index.Length - 1 do
            let startObj = index.[i]
            let count = index.[i + 1]
            xref <- ensureSize xref (startObj + count)
            for j in 0 .. count - 1 do
                let objNum = startObj + j
                let typ = if w0 = 0 then 1L else readInt data dataPos w0
                let field1 = readInt data (dataPos + w0) w1
                let field2 = readInt data (dataPos + w0 + w1) w2
                if objNum < xref.Length &&
                   (match xref.[objNum] with Free (0, 0) -> true | _ -> false) then
                    xref.[objNum] <-
                        match typ with
                        | 0L -> Free (int field1, int field2)
                        | 1L -> InUse (field1, int field2, None)
                        | 2L -> Compressed (int field1, int field2)
                        | _ -> InUse (field1, int field2, None)
                dataPos <- dataPos + entrySize
            i <- i + 2
        xref

module Streams =
    let decompress (filter: string) (data: byte[]) =
        match filter with
        | "FlateDecode" | "Fl" ->
#if FABLE_COMPILER
            try Rbt.OpenType.Inflate.inflate data with _ -> data
#else
            try
                use input = new MemoryStream(data)
                // Skip zlib header (2 bytes)
                input.ReadByte() |> ignore
                input.ReadByte() |> ignore
                use deflate = new DeflateStream(input, CompressionMode.Decompress)
                use output = new MemoryStream()
                deflate.CopyTo(output)
                output.ToArray()
            with _ ->
                // Try without skipping header
                use input = new MemoryStream(data)
                use deflate = new DeflateStream(input, CompressionMode.Decompress)
                use output = new MemoryStream()
                try
                    deflate.CopyTo(output)
                    output.ToArray()
                with _ -> data
#endif
        | "ASCIIHexDecode" | "AHx" ->
            // Hex digits until '>' EOD; whitespace ignored; odd trailing nibble padded with 0.
            let out = ResizeArray<byte>(data.Length / 2)
            let mutable hi = -1
            let mutable i = 0
            let mutable stop = false
            while i < data.Length && not stop do
                let c = char data.[i]
                if c = '>' then stop <- true
                else
                    let v =
                        if c >= '0' && c <= '9' then int c - int '0'
                        elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
                        elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
                        else -1
                    if v >= 0 then
                        if hi < 0 then hi <- v
                        else out.Add(byte ((hi <<< 4) ||| v)); hi <- -1
                i <- i + 1
            if hi >= 0 then out.Add(byte (hi <<< 4))
            out.ToArray()
        | "ASCII85Decode" | "A85" ->
            // Base-85: groups of 5 chars ('!'..'u') -> 4 bytes; 'z' -> 4 zero bytes;
            // '~>' EOD; whitespace ignored. A short final group decodes to count-1 bytes.
            let out = ResizeArray<byte>(data.Length * 4 / 5 + 4)
            let mutable tuple = 0u
            let mutable count = 0
            let mutable i = 0
            let mutable stop = false
            // An optional '<~' lead-in is permitted; skip it if present.
            if data.Length >= 2 && data.[0] = byte '<' && data.[1] = byte '~' then i <- 2
            while i < data.Length && not stop do
                let c = data.[i]
                if c = byte '~' then stop <- true
                elif c = byte 'z' && count = 0 then
                    out.Add(0uy); out.Add(0uy); out.Add(0uy); out.Add(0uy)
                elif c >= byte '!' && c <= byte 'u' then
                    tuple <- tuple * 85u + uint32 (int c - int '!')
                    count <- count + 1
                    if count = 5 then
                        out.Add(byte (tuple >>> 24))
                        out.Add(byte (tuple >>> 16))
                        out.Add(byte (tuple >>> 8))
                        out.Add(byte tuple)
                        tuple <- 0u
                        count <- 0
                i <- i + 1
            if count > 0 then
                // Pad the partial group with 'u' (84), emit count-1 high bytes.
                for _ in count .. 4 do tuple <- tuple * 85u + 84u
                for j in 0 .. count - 2 do out.Add(byte (tuple >>> (24 - j * 8)))
            out.ToArray()
        | "LZWDecode" | "LZW" ->
            // Variable-width (9..12 bit) LZW with EarlyChange=1 (PDF default).
            let out = ResizeArray<byte>(data.Length * 3)
            let dict = ResizeArray<byte[]>(4096)
            let reset () =
                dict.Clear()
                for k in 0 .. 255 do dict.Add([| byte k |])
                dict.Add([||])   // 256 = ClearTable
                dict.Add([||])   // 257 = EOD
            reset ()
            let mutable codeWidth = 9
            let mutable bitBuf = 0
            let mutable bitCnt = 0
            let mutable prev : byte[] option = None
            let mutable i = 0
            let mutable stop = false
            while i < data.Length && not stop do
                bitBuf <- (bitBuf <<< 8) ||| int data.[i]
                bitCnt <- bitCnt + 8
                i <- i + 1
                while bitCnt >= codeWidth && not stop do
                    bitCnt <- bitCnt - codeWidth
                    let code = (bitBuf >>> bitCnt) &&& ((1 <<< codeWidth) - 1)
                    if code = 257 then stop <- true
                    elif code = 256 then
                        reset (); codeWidth <- 9; prev <- None
                    else
                        let entry =
                            if code < dict.Count then dict.[code]
                            elif code = dict.Count && prev.IsSome then
                                Array.append prev.Value [| prev.Value.[0] |]
                            else [||]
                        if entry.Length > 0 then
                            out.AddRange(entry)
                            match prev with
                            | Some p -> dict.Add(Array.append p [| entry.[0] |])
                            | None -> ()
                            prev <- Some entry
                            // EarlyChange: widen one code before the width is exhausted.
                            if dict.Count + 1 >= (1 <<< codeWidth) && codeWidth < 12 then
                                codeWidth <- codeWidth + 1
            out.ToArray()
        | _ -> data

    // Reverse a PNG (Predictor 10-15) or TIFF (Predictor 2) predictor that was
    // applied before compression. Many PDFs predict xref/object/content streams
    // to improve Flate ratios; without reversing it the bytes are meaningless.
    let private applyPredictor (parms: Map<string, PdfObj>) (data: byte[]) : byte[] =
        let dictInt key dflt =
            match Map.tryFind key parms with Some (PdfInt n) -> int n | _ -> dflt
        let predictor = dictInt "Predictor" 1
        if predictor < 2 || data.Length = 0 then data
        else
            let colors = dictInt "Colors" 1
            let bpc = dictInt "BitsPerComponent" 8
            let columns = dictInt "Columns" 1
            let bpp = max 1 ((colors * bpc + 7) / 8)
            let rowLen = (columns * colors * bpc + 7) / 8
            if rowLen <= 0 then data
            elif predictor = 2 then
                // TIFF predictor 2: horizontal differencing (8-bit components only).
                if bpc <> 8 then data
                else
                    let out = Array.copy data
                    let nRows = out.Length / rowLen
                    for r in 0 .. nRows - 1 do
                        let off = r * rowLen
                        for i in bpp .. rowLen - 1 do
                            out.[off + i] <- out.[off + i] + out.[off + i - bpp]
                    out
            else
                // PNG predictors: each row is prefixed by a filter-type byte.
                let stride = rowLen + 1
                let nRows = data.Length / stride
                let out = Array.zeroCreate (nRows * rowLen)
                let prevRow = Array.zeroCreate rowLen
                for r in 0 .. nRows - 1 do
                    let ft = data.[r * stride]
                    let src = r * stride + 1
                    let dst = r * rowLen
                    for i in 0 .. rowLen - 1 do
                        let raw = data.[src + i]
                        let a = if i >= bpp then out.[dst + i - bpp] else 0uy
                        let b = prevRow.[i]
                        let c = if i >= bpp then prevRow.[i - bpp] else 0uy
                        out.[dst + i] <-
                            match ft with
                            | 0uy -> raw                                  // None
                            | 1uy -> raw + a                              // Sub
                            | 2uy -> raw + b                              // Up
                            | 3uy -> raw + byte ((int a + int b) / 2)     // Average
                            | 4uy ->                                       // Paeth
                                let p = int a + int b - int c
                                let pa = abs (p - int a)
                                let pb = abs (p - int b)
                                let pc = abs (p - int c)
                                raw + (if pa <= pb && pa <= pc then a elif pb <= pc then b else c)
                            | _ -> raw
                    Array.blit out dst prevRow 0 rowLen
                out

    let decodeStream (header: Map<string, PdfObj>) (data: byte[]) =
        let filters =
            match Map.tryFind "Filter" header with
            | Some (PdfName f) -> [f]
            | Some (PdfArray arr) ->
                arr |> List.choose (function PdfName f -> Some f | _ -> None)
            | _ -> []
        let decoded = filters |> List.fold (fun d f -> decompress f d) data
        // Apply the predictor from DecodeParms (a.k.a. DP), if any. For an array of
        // parms (one per filter) the predictor lives in the sole dict entry.
        let parms =
            match Map.tryFind "DecodeParms" header |> Option.orElse (Map.tryFind "DP" header) with
            | Some (PdfDict d) -> Some d
            | Some (PdfArray arr) -> arr |> List.tryPick (function PdfDict d -> Some d | _ -> None)
            | _ -> None
        match parms with
        | Some d -> applyPredictor d decoded
        | None -> decoded

module Pdf =
    open Lexer
    open Parser

    let private emptyXRef size =
        Array.init size (fun _ -> Free (0, 0))

    /// Offset of the `%PDF-` header. The spec lets it appear anywhere in the
    /// first 1024 bytes (some producers prepend junk); -1 when absent.
    let findHeader (data: byte[]) =
        Binary.indexOfBefore "%PDF-"B data 0 1024

    let private startXRefValue (data: byte[]) (pos: int) =
        let s = Lexer.create data (pos + 9)
        Lexer.skipWhitespaceAndComments s
        match Lexer.nextToken s with
        | TokInt n -> Some (int64 n)
        | _ -> None

    let findStartXRef (data: byte[]) =
        let pattern = asciiBytes "startxref"
        let pos =
            match Binary.findLastInRange pattern data 1024 with
            | -1 -> Binary.findLast pattern data  // trailing junk after %%EOF
            | pos -> pos
        if pos < 0 then failwith "No startxref found"
        match startXRefValue data pos with
        | Some n -> n
        | None -> failwith "Invalid startxref"

    /// Follow startxref and the /Prev chain, reading xref tables and streams.
    let private readXRefChain (data: byte[]) =
        let startXRef = findStartXRef data
        let mutable xref = emptyXRef 256  // grows as xref sections are parsed
        let mutable trailer = Map.empty
        let mutable xrefType = "table"
        let mutable prevXRef = Some startXRef
        let visited = System.Collections.Generic.HashSet<int64>()

        let readXRefStreamAt (pos: int64) =
            if pos < 0L || pos >= int64 data.Length then failwithf "xref offset %d is outside the file" pos
            let s = Lexer.create data (int pos)
            match Lexer.nextToken s, Lexer.nextToken s, Lexer.nextToken s with
            | TokInt _, TokInt _, TokKeyword "obj" ->
                match parseObjBody s with
                | PdfStream (header, rawData) ->
                    let decoded = Streams.decodeStream header rawData
                    xref <- XRef.parseXRefStream decoded header xref
                    header
                | _ -> failwithf "Object at xref offset %d is not an xref stream" pos
            | _ -> failwithf "No xref at offset %d" pos

        while prevXRef.IsSome do
            let pos = prevXRef.Value
            prevXRef <- None
            if pos < 0L || pos >= int64 data.Length then failwithf "xref offset %d is outside the file" pos
            if visited.Add pos then
                let s = Lexer.create data (int pos)
                Lexer.skipWhitespaceAndComments s
                let keywordPos = s.Pos
                match Lexer.nextToken s with
                | TokKeyword "xref" ->
                    xref <- XRef.parseXRefTable s xref
                    match parseObj s with
                    | PdfDict d ->
                        if trailer.IsEmpty then trailer <- d
                        // Hybrid files: a table plus an xref stream for compressed objects.
                        match Map.tryFind "XRefStm" d with
                        | Some (PdfInt n) when visited.Add n ->
                            try readXRefStreamAt n |> ignore with _ -> ()
                        | _ -> ()
                        prevXRef <-
                            match Map.tryFind "Prev" d with
                            | Some (PdfInt n) -> Some n
                            | _ -> None
                    | _ -> failwith "xref table has no trailer dictionary"
                | TokInt _ ->
                    xrefType <- "stream"
                    let header = readXRefStreamAt (int64 keywordPos)
                    if trailer.IsEmpty then trailer <- header
                    prevXRef <-
                        match Map.tryFind "Prev" header with
                        | Some (PdfInt n) -> Some n
                        | _ -> None
                | tok -> failwithf "No xref at offset %d (found %A)" pos tok
        xref, trailer, xrefType

    /// Every `n g obj` in the file, in file order, plus the trailer
    /// dictionaries that follow `trailer` keywords, found by scanning the
    /// bytes, as mutool and pdf.js do when the xref is unusable. Each object's
    /// body is parsed to skip over it, so byte sequences inside stream data are
    /// not mistaken for objects.
    let scanObjects (data: byte[]) =
        let objects = ResizeArray<int * int * int64 * PdfObj option>()
        let trailers = ResizeArray<Map<string, PdfObj>>()
        let isRegular b = not (isWhitespace b || isDelimiter b)
        let readInt (pos: int) =
            let mutable e = pos
            let mutable v = 0L
            while e < data.Length && isDigit data.[e] && e - pos < 10 do
                v <- v * 10L + int64 (data.[e] - 48uy)
                e <- e + 1
            if e = pos || (e < data.Length && isDigit data.[e]) then None else Some (v, e)
        let skipWs (pos: int) =
            let mutable p = pos
            while p < data.Length && isWhitespace data.[p] do p <- p + 1
            p
        let mutable i = 0
        while i < data.Length do
            let b = data.[i]
            let boundary = i = 0 || not (isRegular data.[i - 1])
            if boundary && isDigit b then
                let header =
                    match readInt i with
                    | Some (num, e1) when e1 < data.Length && isWhitespace data.[e1] ->
                        match readInt (skipWs e1) with
                        | Some (gen, e2) when e2 < data.Length && isWhitespace data.[e2] ->
                            let k = skipWs e2
                            if Binary.matchesAt data k "obj"B && (k + 3 >= data.Length || not (isRegular data.[k + 3])) then
                                Some (int num, int gen, k + 3)
                            else None
                        | _ -> None
                    | _ -> None
                match header with
                | Some (num, gen, bodyStart) ->
                    let s = Lexer.create data bodyStart
                    let parsed =
                        try Some (finishObjBody (fun _ -> None) s (parseObj s))
                        with _ -> None
                    objects.Add((num, gen, int64 i, parsed))
                    i <- (match parsed with Some _ when s.Pos > bodyStart -> s.Pos | _ -> bodyStart)
                | None -> i <- i + 1
            elif boundary && b = 116uy && Binary.matchesAt data i "trailer"B then
                let s = Lexer.create data (i + 7)
                match (try Some (parseObj s) with _ -> None) with
                | Some (PdfDict d) ->
                    trailers.Add d
                    i <- s.Pos
                | _ -> i <- i + 7
            else i <- i + 1
        objects, trailers

    /// Rebuild the cross-reference table (and a trailer) from a byte scan.
    let reconstructXRef (data: byte[]) =
        let objects, trailers = scanObjects data
        let maxNum = objects |> Seq.fold (fun m (n, _, _, _) -> max m n) 0
        let mutable xref = emptyXRef (maxNum + 1)
        // Later definitions win, as in an incrementally updated file.
        for (num, gen, pos, _) in objects do
            if num > 0 then xref.[num] <- InUse (pos, gen, None)
        let typeOf (obj: PdfObj option) =
            match obj with
            | Some (PdfDict d) | Some (PdfStream (d, _)) ->
                match Map.tryFind "Type" d with
                | Some (PdfName t) -> t
                | _ -> ""
            | _ -> ""
        // Objects inside object streams, where no direct definition exists.
        let direct = objects |> Seq.map (fun (n, _, _, _) -> n) |> Set.ofSeq
        for (streamNum, _, _, parsed) in objects do
            match parsed with
            | Some (PdfStream (header, raw)) when typeOf parsed = "ObjStm" ->
                try
                    let decoded = Streams.decodeStream header raw
                    let n = match Map.tryFind "N" header with Some (PdfInt n) -> int n | _ -> 0
                    let s = Lexer.create decoded 0
                    for index in 0 .. n - 1 do
                        match Lexer.nextToken s, Lexer.nextToken s with
                        | TokInt num, TokInt _ when int num > 0 && not (direct.Contains (int num)) ->
                            xref <- XRef.ensureSize xref (int num + 1)
                            xref.[int num] <- Compressed (streamNum, index)
                        | _ -> ()
                with _ -> ()
            | _ -> ()
        xref.[0] <- Free (0, 65535)
        // Trailer: the last one naming a catalog, from `trailer` keywords or
        // xref-stream headers; else the last /Type /Catalog object.
        let xrefHeaders =
            objects |> Seq.choose (fun (_, _, _, parsed) ->
                match parsed with
                | Some (PdfStream (header, _)) when typeOf parsed = "XRef" -> Some header
                | _ -> None)
        let candidates = Seq.append trailers xrefHeaders |> Seq.filter (fun t -> t.ContainsKey "Root") |> List.ofSeq
        let trailer =
            match List.tryLast candidates with
            | Some t -> t |> Map.filter (fun k _ -> k = "Root" || k = "Info" || k = "ID" || k = "Encrypt")
            | None ->
                match objects |> Seq.filter (fun (_, _, _, parsed) -> typeOf parsed = "Catalog") |> Seq.tryLast with
                | Some (num, gen, _, _) -> Map.ofList [ "Root", PdfIndirect (num, gen) ]
                | None -> Map.empty
        xref, trailer

    let rec deref (doc: PdfDocument) (obj: PdfObj) : PdfObj =
        match obj with
        | PdfIndirect (objNum, _) ->
            if objNum < 0 || objNum >= doc.XRef.Length then PdfNull
            else
                match doc.XRef.[objNum] with
                | Free _ -> PdfNull
                | InUse (_, _, Some cached) -> cached
                | InUse (pos, g, None) ->
                    let parseAt (pos: int64) =
                        if pos < 0L || pos >= int64 doc.Data.Length then failwith "object offset outside the file"
                        let s = Lexer.create doc.Data (int pos)
                        let resolveLength (lengthRef: PdfObj) =
                            match lengthRef with
                            | PdfIndirect (n, _) when n = objNum -> None
                            | _ ->
                                match deref doc lengthRef with
                                | PdfInt n -> Some n
                                | _ -> None
                        Parser.parseIndirectObjWith resolveLength s objNum g
                    let loaded =
                        try parseAt pos
                        with _ ->
                            // The xref entry is wrong: rebuild the table by scanning and retry.
                            repairXRef doc
                            match doc.XRef.[objNum] with
                            | InUse (pos2, _, None) when pos2 <> pos -> (try parseAt pos2 with _ -> PdfNull)
                            | InUse (_, _, Some cached) -> cached
                            | Compressed _ -> deref doc obj
                            | _ -> PdfNull
                    match doc.XRef.[objNum] with
                    | InUse (p, g2, _) -> doc.XRef.[objNum] <- InUse (p, g2, Some loaded)
                    | _ -> ()
                    loaded
                | Compressed (streamObj, index) ->
                    // Guard against cyclic object-stream references (a malformed
                    // xref where an object resolves, via its object stream, back to
                    // itself). A valid /ObjStm is never itself compressed, so any
                    // re-entry here is a cycle: mark this slot resolved-to-null
                    // before recursing so the cycle terminates instead of looping.
                    doc.XRef.[objNum] <- InUse (0L, 0, Some PdfNull)
                    // Load from object stream
                    let stream = deref doc (PdfIndirect (streamObj, 0))
                    match stream with
                    | PdfStream (header, rawData) ->
                        let decoded = Streams.decodeStream header rawData
                        let n =
                            match Map.tryFind "N" header with
                            | Some (PdfInt n) -> int n
                            | _ -> 0
                        let first =
                            match Map.tryFind "First" header with
                            | Some (PdfInt n) -> int n
                            | _ -> 0
                        // Parse index
                        let s = Lexer.create decoded 0
                        let offsets = Array.zeroCreate n
                        for i in 0 .. n - 1 do
                            match Lexer.nextToken s with
                            | TokInt num ->
                                match Lexer.nextToken s with
                                | TokInt off -> offsets.[i] <- (int num, int off)
                                | _ -> ()
                            | _ -> ()
                        if index < n then
                            let _, off = offsets.[index]
                            let s2 = Lexer.create decoded (first + off)
                            let loaded = try Parser.parseObj s2 with _ -> PdfNull
                            doc.XRef.[objNum] <- InUse (0L, 0, Some loaded)
                            loaded
                        else PdfNull
                    | _ -> PdfNull
        | _ -> obj

    /// Replace every unloaded xref entry whose offset does not hold its object
    /// with the position found by a byte scan (or a null object when the scan
    /// has none), and add scanned objects the xref lacks. After a repair every
    /// entry is valid, so a document is scanned at most once.
    and repairXRef (doc: PdfDocument) =
        let scanned, _ = reconstructXRef doc.Data
        let xref = XRef.ensureSize doc.XRef scanned.Length
        for num in 1 .. xref.Length - 1 do
            let fromScan = if num < scanned.Length then Some scanned.[num] else None
            match xref.[num] with
            | InUse (pos, gen, None) when not (Parser.objectHeaderAt doc.Data pos num) ->
                xref.[num] <-
                    match fromScan with
                    | Some (InUse _ as e) | Some (Compressed _ as e) -> e
                    | _ -> InUse (pos, gen, Some PdfNull)
            | Free (0, 0) ->
                match fromScan with
                | Some (InUse _ as e) | Some (Compressed _ as e) -> xref.[num] <- e
                | _ -> ()
            | _ -> ()
        doc.XRef <- xref

    let openData (data: byte[]) : PdfDocument =
        let offset = findHeader data
        if offset < 0 || data.Length - offset < 8 then
            failwith "Not a PDF file"
        // Offsets are relative to the header, so drop anything before it.
        let data = if offset > 0 then Array.sub data offset (data.Length - offset) else data

        let fromChain =
            try
                let xref, trailer, xrefType = readXRefChain data
                if trailer.ContainsKey "Root" then Some (xref, trailer, xrefType) else None
            with _ -> None

        let build (xref, trailer: Map<string, PdfObj>, xrefType) =
            match Map.tryFind "Root" trailer with
            | Some catalog ->
                Some {
                    Data = data
                    XRef = xref
                    XRefType = xrefType
                    Trailer = trailer
                    Catalog = catalog
                    Modified = Set.empty
                }
            | None -> None

        let isCatalog (doc: PdfDocument) =
            match (try deref doc doc.Catalog with _ -> PdfNull) with
            | PdfDict d -> d.ContainsKey "Pages" || Map.tryFind "Type" d = Some (PdfName "Catalog")
            | _ -> false

        // deref repairs individual bad offsets itself; reconstruct outright when
        // the xref chain is unreadable or its catalog does not resolve.
        match fromChain |> Option.bind build with
        | Some doc when isCatalog doc -> doc
        | _ ->
            let xref, trailer = reconstructXRef data
            match build (xref, trailer, "table") with
            | Some doc -> doc
            | None -> failwith "No catalog found"

    let openFile (path: string) =
#if FABLE_COMPILER
        failwith "Pdf.openFile is not available in Fable; use openData with caller-provided bytes."
#else
        File.ReadAllBytes(path) |> openData
#endif

    let dictGet (doc: PdfDocument) (key: string) (obj: PdfObj) =
        match deref doc obj with
        | PdfDict d -> Map.tryFind key d
        | PdfStream (h, _) -> Map.tryFind key h
        | _ -> None

    let getPages (doc: PdfDocument) =
        let catalog = deref doc doc.Catalog
        match dictGet doc "Pages" catalog with
        | Some pages -> pages
        | None -> failwith "No Pages in catalog"

    let rec private countPages (doc: PdfDocument) (node: PdfObj) =
        let n = deref doc node
        match dictGet doc "Type" n with
        | Some (PdfName "Page") -> 1
        | Some (PdfName "Pages") ->
            match dictGet doc "Count" n with
            | Some (PdfInt c) -> int c
            | _ -> 0
        | _ -> 0

    let numPages (doc: PdfDocument) =
        countPages doc (getPages doc)

    let getPageCount (doc: PdfDocument) : int =
        numPages doc

    let rec private findPage (doc: PdfDocument) (node: PdfObj) (pageNum: int) (currentPage: int) =
        let n = deref doc node
        match dictGet doc "Type" n with
        | Some (PdfName "Page") ->
            if currentPage = pageNum then Some node
            else None
        | Some (PdfName "Pages") ->
            match dictGet doc "Kids" n with
            | Some (PdfArray kids) ->
                let mutable current = currentPage
                let mutable result = None
                for kid in kids do
                    if result.IsNone then
                        let count = countPages doc kid
                        if pageNum >= current && pageNum < current + count then
                            result <- findPage doc kid pageNum current
                        current <- current + count
                result
            | _ -> None
        | _ -> None

    let getPage (doc: PdfDocument) (pageNum: int) =
        findPage doc (getPages doc) pageNum 1

    /// An inheritable page attribute (MediaBox, CropBox, Resources, Rotate):
    /// the page's own, else the nearest ancestor's. A cyclic /Parent chain ends
    /// the search.
    let getPageInheritable (doc: PdfDocument) (pageRef: PdfObj) (name: string) =
        let rec search depth node =
            if depth > 64 then None
            else
                let n = deref doc node
                match dictGet doc name n with
                | Some v -> Some v
                | None ->
                    match dictGet doc "Parent" n with
                    | Some parent -> search (depth + 1) parent
                    | None -> None
        search 0 pageRef

    /// Leaf page refs in document order, found by walking /Kids (a stale
    /// /Count is ignored). Cyclic or implausibly deep branches are skipped.
    /// Leaves missing /Type /Page are accepted.
    let getPageRefs (doc: PdfDocument) : PdfObj list =
        let pages = ResizeArray<PdfObj>()
        let seen = System.Collections.Generic.HashSet<int>()
        let rec walk depth (node: PdfObj) =
            let fresh =
                match node with
                | PdfIndirect (num, _) -> seen.Add num
                | _ -> true
            if fresh && depth <= 64 then
                match deref doc node with
                | PdfDict d ->
                    match Map.tryFind "Type" d |> Option.map (deref doc) with
                    | Some (PdfName "Pages") ->
                        match Map.tryFind "Kids" d |> Option.map (deref doc) with
                        | Some (PdfArray kids) -> for kid in kids do walk (depth + 1) kid
                        | _ -> ()
                    | Some (PdfName "Page") -> pages.Add node
                    | _ ->
                        match Map.tryFind "Kids" d |> Option.map (deref doc) with
                        | Some (PdfArray kids) -> for kid in kids do walk (depth + 1) kid
                        | _ -> pages.Add node
                | _ -> ()
        walk 0 (getPages doc)
        List.ofSeq pages

    let getStreamData (doc: PdfDocument) (obj: PdfObj) =
        match deref doc obj with
        | PdfStream (header, data) -> Streams.decodeStream header data
        | _ -> Array.empty

module Output =
    let private hexEncode (data: byte[]) =
        bytesToHex data

    let rec serialize (obj: PdfObj) =
        let sb = StringBuilder()
        serializeTo sb obj
        sb.ToString()

    and serializeTo (sb: StringBuilder) (obj: PdfObj) =
        match obj with
        | PdfInt n -> sb.Append(n.ToString()).Append(' ') |> ignore
        | PdfReal r -> sb.Append(r).Append(' ') |> ignore
        | PdfBool b -> sb.Append(if b then "true " else "false ") |> ignore
        | PdfNull -> sb.Append("null ") |> ignore
        | PdfString data -> sb.Append('<').Append(hexEncode data).Append("> ") |> ignore
        | PdfName n -> sb.Append('/').Append(n).Append(' ') |> ignore
        | PdfArray arr ->
            sb.Append("[ ") |> ignore
            for item in arr do serializeTo sb item
            sb.Append("] ") |> ignore
        | PdfDict d ->
            sb.Append("<<\n") |> ignore
            for KeyValue(k, v) in d do
                sb.Append('/').Append(k).Append(' ') |> ignore
                serializeTo sb v
                sb.Append('\n') |> ignore
            sb.Append(">> ") |> ignore
        | PdfStream (header, data) ->
            let header = header |> Map.add "Length" (PdfInt (int64 data.Length))
            serializeTo sb (PdfDict header)
            sb.Append("\nstream\n") |> ignore
            sb.Append(latin1String data) |> ignore
            sb.Append("\nendstream\n") |> ignore
        | PdfIndirect (objNum, gen) ->
            sb.Append(objNum).Append(' ').Append(gen).Append(" R ") |> ignore

module Write =
    let private padZero (n: int64) (width: int) =
        n.ToString().PadLeft(width, '0')

    let createNew () : PdfDocument =
        let catalog = PdfDict (Map.ofList [("Type", PdfName "Catalog")])
        {
            Data = Array.empty
            XRef = [| Free (0, 65535); InUse (0L, 0, Some catalog) |]
            XRefType = "table"
            Trailer = Map.empty
            Catalog = PdfIndirect (1, 0)
            Modified = set [0; 1]  // Object 0 (free list head) must be in xref
        }

    let newIndirectObj (doc: PdfDocument) (obj: PdfObj) =
        let objNum = doc.XRef.Length
        let newXRef = Array.append doc.XRef [| InUse (0L, 0, Some obj) |]
        doc.XRef <- newXRef
        doc.Modified <- doc.Modified.Add(objNum)
        PdfIndirect (objNum, 0)

    let setObj (doc: PdfDocument) (objNum: int) (obj: PdfObj) =
        match doc.XRef.[objNum] with
        | InUse (pos, gen, _) ->
            doc.XRef.[objNum] <- InUse (pos, gen, Some obj)
            doc.Modified <- doc.Modified.Add(objNum)
        | _ -> ()

    let newStream (doc: PdfDocument) (data: byte[]) (extraHeaders: (string * PdfObj) list) =
        let header =
            extraHeaders
            |> List.append [("Length", PdfInt (int64 data.Length))]
            |> Map.ofList
        newIndirectObj doc (PdfStream (header, data))

    let update (doc: PdfDocument) =
        if doc.Modified.IsEmpty then Array.empty
        else
            let sb = StringBuilder()

            // Header for new file
            if doc.Data.Length = 0 then
                sb.Append("%PDF-1.6\n%\xe2\xe3\xcf\xd3\n") |> ignore

            sb.Append('\n') |> ignore

            // Write modified objects
            let positions = System.Collections.Generic.Dictionary<int, int64>()
            for objNum in doc.Modified |> Set.toSeq |> Seq.sort do
                match doc.XRef.[objNum] with
                | InUse (_, gen, Some obj) ->
                    positions.[objNum] <- int64 doc.Data.Length + int64 sb.Length
                    sb.Append(objNum).Append(' ').Append(gen).Append(" obj\n") |> ignore
                    Output.serializeTo sb obj
                    sb.Append(" endobj\n") |> ignore
                | _ -> ()

            // Build xref ranges
            let mutable ranges = []
            let mutable rangeStart = -1
            let mutable rangeEnd = -1
            for objNum in doc.Modified |> Set.toSeq |> Seq.sort do
                if rangeStart < 0 then
                    rangeStart <- objNum
                    rangeEnd <- objNum + 1
                elif objNum = rangeEnd then
                    rangeEnd <- rangeEnd + 1
                else
                    ranges <- (rangeStart, rangeEnd - rangeStart) :: ranges
                    rangeStart <- objNum
                    rangeEnd <- objNum + 1
            if rangeStart >= 0 then
                ranges <- (rangeStart, rangeEnd - rangeStart) :: ranges
            ranges <- List.rev ranges

            let startxref = int64 doc.Data.Length + int64 sb.Length

            // Write xref table
            sb.Append("xref\n") |> ignore
            for (start, count) in ranges do
                sb.Append(start).Append(' ').Append(count).Append('\n') |> ignore
                for i in start .. start + count - 1 do
                    let pos, gen, typ =
                        match doc.XRef.[i] with
                        | Free (next, g) -> int64 next, g, 'f'
                        | InUse (p, g, _) ->
                            (if positions.ContainsKey(i) then positions.[i] else p), g, 'n'
                        | Compressed _ -> 0L, 0, 'n'
                    sb.Append(padZero pos 10).Append(' ')
                      .Append(padZero (int64 gen) 5).Append(' ')
                      .Append(typ).Append(" \n") |> ignore

            // Write trailer
            sb.Append("trailer\n") |> ignore
            let trailer =
                doc.Trailer
                |> Map.add "Size" (PdfInt (int64 doc.XRef.Length))
                |> Map.add "Root" doc.Catalog
                |> if doc.Data.Length > 0 then
                       Map.add "Prev" (PdfInt (Pdf.findStartXRef doc.Data |> int64))
                   else id
            Output.serializeTo sb (PdfDict trailer)
            sb.Append("\nstartxref\n").Append(startxref.ToString()).Append("\n%%EOF\n") |> ignore

            doc.Modified <- Set.empty
            latin1Bytes (sb.ToString())

    let save (doc: PdfDocument) (path: string) =
#if FABLE_COMPILER
        failwith "Pdf.Write.save is not available in Fable; use update/toBytes and let the caller persist bytes."
#else
        let update = update doc
        use fs = new FileStream(path, FileMode.Create)
        if doc.Data.Length > 0 then
            fs.Write(doc.Data, 0, doc.Data.Length)
        fs.Write(update, 0, update.Length)
#endif

    let saveIncremental (doc: PdfDocument) (path: string) =
#if FABLE_COMPILER
        failwith "Pdf.Write.saveIncremental is not available in Fable."
#else
        let update = update doc
        use fs = new FileStream(path, FileMode.Append)
        fs.Write(update, 0, update.Length)
#endif

module PageOps =
    let extractPage (doc: PdfDocument) (pageNum: int) =
        match Pdf.getPage doc pageNum with
        | Some pageRef ->
            let newDoc = Write.createNew ()
            let page = Pdf.deref doc pageRef
            let pageObj = Write.newIndirectObj newDoc page
            let pages = PdfDict (Map.ofList [
                ("Type", PdfName "Pages")
                ("Kids", PdfArray [pageObj])
                ("Count", PdfInt 1L)
            ])
            let pagesRef = Write.newIndirectObj newDoc pages
            let catalog = PdfDict (Map.ofList [
                ("Type", PdfName "Catalog")
                ("Pages", pagesRef)
            ])
            match newDoc.Catalog with
            | PdfIndirect (objNum, _) -> Write.setObj newDoc objNum catalog
            | _ -> ()
            Some newDoc
        | None -> None

    let getPageContents (doc: PdfDocument) (pageRef: PdfObj) =
        match Pdf.dictGet doc "Contents" pageRef with
        | Some (PdfArray contents) ->
            contents
            |> List.map (Pdf.getStreamData doc)
            |> Array.concat
        | Some stream -> Pdf.getStreamData doc stream
        | None -> Array.empty

    let getMediaBox (doc: PdfDocument) (pageRef: PdfObj) =
        let asFloat = function
            | PdfInt n -> Some (float n)
            | PdfReal f -> Some f
            | _ -> None
        match Pdf.getPageInheritable doc pageRef "MediaBox" with
        | Some (PdfArray [a; b; c; d]) ->
            match asFloat a, asFloat b, asFloat c, asFloat d with
            | Some x0, Some y0, Some x1, Some y1 -> Some (x0, y0, x1, y1)
            | _ -> None
        | _ -> None

/// Content stream tokenizer for PDF page content
module ContentStream =
    open System

    type Token =
        | TNumber of float
        | TString of byte[]
        | THexString of byte[]
        | TName of string
        | TOperator of string
        | TArrayStart
        | TArrayEnd
        | TBool of bool
        | TInlineImageData of byte[]

    let private isWhitespace b = b = 0uy || b = 9uy || b = 10uy || b = 12uy || b = 13uy || b = 32uy
    let private isDelimiter b = b = 40uy || b = 41uy || b = 60uy || b = 62uy || b = 91uy || b = 93uy || b = 47uy || b = 37uy || b = 123uy || b = 125uy
    let private isDigit b = b >= 48uy && b <= 57uy

    let tokenize (data: byte[]) : Token list =
        let len = data.Length
        let mutable pos = 0
        let tokens = ResizeArray<Token>()

        let peek () = if pos < len then Some data.[pos] else None
        let advance () = pos <- pos + 1
        let read () = if pos < len then let b = data.[pos] in pos <- pos + 1; Some b else None

        let skipWhitespaceAndComments () =
            let mutable cont = true
            while cont do
                match peek () with
                | Some b when isWhitespace b -> advance (); ()
                | Some 37uy -> // %
                    while (match peek () with Some b when b <> 10uy && b <> 13uy -> true | _ -> false) do advance ()
                | _ -> cont <- false

        let readLiteralString () =
            advance () // skip (
            let buf = ResizeArray<byte>()
            let mutable depth = 1
            while depth > 0 && pos < len do
                let b = data.[pos]
                advance ()
                match b with
                | 40uy -> depth <- depth + 1; buf.Add(b)
                | 41uy -> depth <- depth - 1; if depth > 0 then buf.Add(b)
                | 92uy -> // backslash
                    match read () with
                    | Some 110uy -> buf.Add(10uy) // \n
                    | Some 114uy -> buf.Add(13uy) // \r
                    | Some 116uy -> buf.Add(9uy)  // \t
                    | Some 98uy  -> buf.Add(8uy)  // \b
                    | Some 102uy -> buf.Add(12uy) // \f
                    | Some 40uy  -> buf.Add(40uy) // \(
                    | Some 41uy  -> buf.Add(41uy) // \)
                    | Some 92uy  -> buf.Add(92uy) // \\
                    | Some d when isDigit d ->
                        let mutable octal = int d - 48
                        for _ in 1..2 do
                            match peek () with
                            | Some d2 when isDigit d2 && d2 <= 55uy -> advance (); octal <- octal * 8 + (int d2 - 48)
                            | _ -> ()
                        buf.Add(byte octal)
                    | Some 13uy -> // line continuation \<CR> or \<CR><LF>
                        match peek () with Some 10uy -> advance () | _ -> ()
                    | Some 10uy -> () // line continuation \<LF>
                    | Some other -> buf.Add(other)
                    | None -> ()
                | _ -> buf.Add(b)
            TString (buf.ToArray())

        let readHexString () =
            advance () // skip <
            let buf = ResizeArray<byte>()
            let mutable hi = -1
            let mutable stop = false
            while pos < len && not stop do
                let b = data.[pos]
                advance ()
                if b = 62uy then // >
                    if hi >= 0 then buf.Add(byte (hi <<< 4))
                    stop <- true
                elif not (isWhitespace b) then
                    let nibble =
                        if b >= 48uy && b <= 57uy then int b - 48
                        elif b >= 65uy && b <= 70uy then int b - 55
                        elif b >= 97uy && b <= 102uy then int b - 87
                        else -1
                    if nibble >= 0 then
                        if hi < 0 then hi <- nibble
                        else buf.Add(byte ((hi <<< 4) ||| nibble)); hi <- -1
            THexString (buf.ToArray())

        let readNumber (first: byte) =
            let sb = Text.StringBuilder()
            sb.Append(char first) |> ignore
            let mutable isReal = (first = 46uy)
            let mutable cont = true
            while cont do
                match peek () with
                | Some b when isDigit b -> advance (); sb.Append(char b) |> ignore
                | Some 46uy when not isReal -> advance (); sb.Append('.') |> ignore; isReal <- true
                | _ -> cont <- false
            let str = sb.ToString()
            match Double.TryParse(str, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, v -> TNumber v
            | _ -> TNumber 0.0

        let readName () =
            advance () // skip /
            let sb = Text.StringBuilder()
            let mutable cont = true
            while cont do
                match peek () with
                | None -> cont <- false
                | Some b when isWhitespace b || isDelimiter b -> cont <- false
                | Some 35uy -> // #hex
                    advance ()
                    let h1 = read ()
                    let h2 = read ()
                    match h1, h2 with
                    | Some a, Some b ->
                        let hex = bytesToAscii [| a; b |]
                        match Int32.TryParse(hex, Globalization.NumberStyles.HexNumber, Globalization.CultureInfo.InvariantCulture) with
                        | true, v -> sb.Append(char v) |> ignore
                        | _ -> ()
                    | _ -> ()
                | Some b -> advance (); sb.Append(char b) |> ignore
            TName (sb.ToString())

        let readInlineImage () =
            // After "BI" keyword: parse key/value pairs until "ID", then raw data until EI
            tokens.Add(TOperator "BI")
            let mutable foundID = false
            while not foundID && pos < len do
                skipWhitespaceAndComments ()
                match peek () with
                | None -> foundID <- true
                | Some 47uy -> tokens.Add(readName ())
                | Some b when isDigit b || b = 46uy -> advance (); tokens.Add(readNumber b)
                | Some 43uy -> advance (); (match peek () with Some b when isDigit b || b = 46uy -> advance (); tokens.Add(readNumber b) | _ -> ())
                | Some 45uy ->
                    advance ()
                    match peek () with
                    | Some b when isDigit b || b = 46uy -> advance (); let tok = readNumber b in (match tok with TNumber v -> tokens.Add(TNumber -v) | t -> tokens.Add(t))
                    | _ -> tokens.Add(TNumber 0.0)
                | Some 91uy -> advance (); tokens.Add(TArrayStart)
                | Some 93uy -> advance (); tokens.Add(TArrayEnd)
                | Some 40uy -> tokens.Add(readLiteralString ())
                | Some 60uy ->
                    if pos + 1 < len && data.[pos + 1] = 60uy then advance (); advance (); tokens.Add(TOperator "<<")
                    else tokens.Add(readHexString ())
                | _ ->
                    let ksb = Text.StringBuilder()
                    let mutable kcont = true
                    while kcont do
                        match peek () with
                        | Some b when not (isWhitespace b) && not (isDelimiter b) -> advance (); ksb.Append(char b) |> ignore
                        | _ -> kcont <- false
                    let kw = ksb.ToString()
                    if kw = "ID" then
                        foundID <- true
                        if pos < len then advance ()
                        let startPos = pos
                        let mutable foundEI = false
                        while not foundEI && pos < len do
                            if data.[pos] = 69uy && pos + 1 < len && data.[pos + 1] = 73uy then
                                let prevOk = pos = startPos || isWhitespace data.[pos - 1]
                                let nextOk = pos + 2 >= len || isWhitespace data.[pos + 2] || isDelimiter data.[pos + 2]
                                if prevOk && nextOk then
                                    let imgData = data.[startPos .. pos - 1]
                                    let trimmed =
                                        let mutable e = imgData.Length - 1
                                        while e >= 0 && isWhitespace imgData.[e] do e <- e - 1
                                        if e < imgData.Length - 1 then imgData.[0..e] else imgData
                                    tokens.Add(TInlineImageData trimmed)
                                    pos <- pos + 2
                                    tokens.Add(TOperator "EI")
                                    foundEI <- true
                                else pos <- pos + 1
                            else pos <- pos + 1
                        if not foundEI then
                            tokens.Add(TInlineImageData (data.[startPos .. len - 1]))
                            tokens.Add(TOperator "EI")
                    elif kw = "true" then tokens.Add(TBool true)
                    elif kw = "false" then tokens.Add(TBool false)
                    elif kw.Length > 0 then tokens.Add(TName kw)

        let readKeyword () =
            let sb = Text.StringBuilder()
            let mutable cont = true
            while cont do
                match peek () with
                | Some b when not (isWhitespace b) && not (isDelimiter b) -> advance (); sb.Append(char b) |> ignore
                | _ -> cont <- false
            let s = sb.ToString()
            match s with
            | "true" -> TBool true
            | "false" -> TBool false
            | "BI" -> readInlineImage (); TOperator "__BI_HANDLED__"
            | _ -> TOperator s

        while pos < len do
            skipWhitespaceAndComments ()
            match peek () with
            | None -> pos <- len
            | Some 40uy -> tokens.Add(readLiteralString ())
            | Some 60uy ->
                if pos + 1 < len && data.[pos + 1] = 60uy then
                    advance (); advance ()
                    tokens.Add(TOperator "<<")
                else
                    tokens.Add(readHexString ())
            | Some 62uy ->
                if pos + 1 < len && data.[pos + 1] = 62uy then
                    advance (); advance ()
                    tokens.Add(TOperator ">>")
                else advance ()
            | Some 91uy -> advance (); tokens.Add(TArrayStart)
            | Some 93uy -> advance (); tokens.Add(TArrayEnd)
            | Some 47uy -> tokens.Add(readName ())
            | Some b when isDigit b || b = 46uy -> advance (); tokens.Add(readNumber b)
            | Some 43uy -> // +
                advance ()
                match peek () with
                | Some b when isDigit b || b = 46uy -> advance (); tokens.Add(readNumber b)
                | _ -> ()
            | Some 45uy -> // -
                advance ()
                match peek () with
                | Some b when isDigit b || b = 46uy ->
                    advance ()
                    let tok = readNumber b
                    match tok with
                    | TNumber v -> tokens.Add(TNumber -v)
                    | t -> tokens.Add(t)
                | _ -> tokens.Add(TNumber 0.0)
            | Some _ ->
                let prevPos = pos
                let kw = readKeyword ()
                if pos = prevPos then
                    advance () // skip unrecognized delimiter/byte to avoid infinite loop
                else
                    match kw with TOperator "__BI_HANDLED__" -> () | _ -> tokens.Add(kw)

        tokens |> Seq.toList

/// Font resolution for reading PDF fonts — pure PDF data, no renderer dependency
module FontResolve =
    open System

    // ── Text Decoding ────────────────────────────────────────

    let private decodeWinAnsi (data: byte[]) : string =
        let winAnsiDecode = [|
            '\u20AC'; '\u0081'; '\u201A'; '\u0192'; '\u201E'; '\u2026'; '\u2020'; '\u2021'
            '\u02C6'; '\u2030'; '\u0160'; '\u2039'; '\u0152'; '\u008D'; '\u017D'; '\u008F'
            '\u0090'; '\u2018'; '\u2019'; '\u201C'; '\u201D'; '\u2022'; '\u2013'; '\u2014'
            '\u02DC'; '\u2122'; '\u0161'; '\u203A'; '\u0153'; '\u009D'; '\u017E'; '\u0178'
        |]
        let sb = System.Text.StringBuilder(data.Length)
        for b in data do
            let c =
                if b >= 0x80uy && b <= 0x9Fuy then winAnsiDecode.[int b - 0x80]
                else char b
            sb.Append(c) |> ignore
        sb.ToString()

    let private decodeMacRoman (data: byte[]) : string =
        let macRomanDecode = [|
            '\u00C4'; '\u00C5'; '\u00C7'; '\u00C9'; '\u00D1'; '\u00D6'; '\u00DC'; '\u00E1'
            '\u00E0'; '\u00E2'; '\u00E4'; '\u00E3'; '\u00E5'; '\u00E7'; '\u00E9'; '\u00E8'
            '\u00EA'; '\u00EB'; '\u00ED'; '\u00EC'; '\u00EE'; '\u00EF'; '\u00F1'; '\u00F3'
            '\u00F2'; '\u00F4'; '\u00F6'; '\u00F5'; '\u00FA'; '\u00F9'; '\u00FB'; '\u00FC'
            '\u2020'; '\u00B0'; '\u00A2'; '\u00A3'; '\u00A7'; '\u2022'; '\u00B6'; '\u00DF'
            '\u00AE'; '\u00A9'; '\u2122'; '\u00B4'; '\u00A8'; '\u2260'; '\u00C6'; '\u00D8'
            '\u221E'; '\u00B1'; '\u2264'; '\u2265'; '\u00A5'; '\u00B5'; '\u2202'; '\u2211'
            '\u220F'; '\u03C0'; '\u222B'; '\u00AA'; '\u00BA'; '\u2126'; '\u00E6'; '\u00F8'
            '\u00BF'; '\u00A1'; '\u00AC'; '\u221A'; '\u0192'; '\u2248'; '\u2206'; '\u00AB'
            '\u00BB'; '\u2026'; '\u00A0'; '\u00C0'; '\u00C3'; '\u00D5'; '\u0152'; '\u0153'
            '\u2013'; '\u2014'; '\u201C'; '\u201D'; '\u2018'; '\u2019'; '\u00F7'; '\u25CA'
            '\u00FF'; '\u0178'; '\u2044'; '\u20AC'; '\u2039'; '\u203A'; '\uFB01'; '\uFB02'
            '\u2021'; '\u00B7'; '\u201A'; '\u201E'; '\u2030'; '\u00C2'; '\u00CA'; '\u00C1'
            '\u00CB'; '\u00C8'; '\u00CD'; '\u00CE'; '\u00CF'; '\u00CC'; '\u00D3'; '\u00D4'
            '\uF8FF'; '\u00D2'; '\u00DA'; '\u00DB'; '\u00D9'; '\u0131'; '\u02C6'; '\u02DC'
            '\u00AF'; '\u02D8'; '\u02D9'; '\u02DA'; '\u00B8'; '\u02DD'; '\u02DB'; '\u02C7'
        |]
        let sb = System.Text.StringBuilder(data.Length)
        for b in data do
            let c =
                if b >= 0x80uy then macRomanDecode.[int b - 0x80]
                else char b
            sb.Append(c) |> ignore
        sb.ToString()

    let decodeByEncoding (encoding: string) (data: byte[]) : string =
        match encoding with
        | "MacRomanEncoding" -> decodeMacRoman data
        | _ -> decodeWinAnsi data

    let decodeWithToUnicode (cmapData: byte[]) (stringBytes: byte[]) : string option =
        try
            let cmapText = bytesToAscii cmapData
            let charMap = Collections.Generic.Dictionary<int, string>()

            let bfcharPattern = Text.RegularExpressions.Regex(@"beginbfchar\s*(.*?)\s*endbfchar", Text.RegularExpressions.RegexOptions.Singleline)
            for m in bfcharPattern.Matches(cmapText) do
                let lines = m.Groups.[1].Value.Trim().Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
                for line in lines do
                    let parts = line.Trim().Split([|'<'; '>'|], StringSplitOptions.RemoveEmptyEntries)
                    if parts.Length >= 2 then
                        let srcCode = Convert.ToInt32(parts.[0].Trim(), 16)
                        let dstHex = parts.[1].Trim().Replace(" ", "")
                        let unicode =
                            if dstHex.Length <= 4 then string (char (Convert.ToInt32(dstHex, 16)))
                            else
                                let sb = System.Text.StringBuilder()
                                for i in 0..4..dstHex.Length-4 do
                                    sb.Append(char (Convert.ToInt32(dstHex.[i..i+3], 16))) |> ignore
                                sb.ToString()
                        charMap.[srcCode] <- unicode

            let bfrangePattern = Text.RegularExpressions.Regex(@"beginbfrange\s*(.*?)\s*endbfrange", Text.RegularExpressions.RegexOptions.Singleline)
            for m in bfrangePattern.Matches(cmapText) do
                let lines = m.Groups.[1].Value.Trim().Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
                for line in lines do
                    let parts = line.Trim().Split([|'<'; '>'|], StringSplitOptions.RemoveEmptyEntries)
                    if parts.Length >= 3 then
                        let srcLo = Convert.ToInt32(parts.[0].Trim(), 16)
                        let srcHi = Convert.ToInt32(parts.[1].Trim(), 16)
                        let dstStart = Convert.ToInt32(parts.[2].Trim(), 16)
                        for code in srcLo..srcHi do
                            charMap.[code] <- string (char (dstStart + code - srcLo))

            if charMap.Count = 0 then None
            else
                let sb = System.Text.StringBuilder()
                // Determine byte width from the cmap's codespacerange (e.g. <0000><FFFF> = 2-byte,
                // <00><FF> = 1-byte). More reliable than checking key magnitudes, since composite
                // fonts with Identity-H encoding can have small CID values (<=255) that are still 2-byte.
                let isTwoByte =
                    let csPattern = Text.RegularExpressions.Regex(@"begincodespacerange\s*(.*?)\s*endcodespacerange", Text.RegularExpressions.RegexOptions.Singleline)
                    match csPattern.Match(cmapText) with
                    | m when m.Success ->
                        // First hex token in the range: 4+ hex digits => 2-byte codes.
                        let tokenPattern = Text.RegularExpressions.Regex(@"<([0-9A-Fa-f]+)>")
                        match tokenPattern.Match(m.Groups.[1].Value) with
                        | t when t.Success -> t.Groups.[1].Value.Length >= 4
                        | _ -> charMap.Keys |> Seq.exists (fun k -> k > 255)
                    | _ -> charMap.Keys |> Seq.exists (fun k -> k > 255)
                if isTwoByte then
                    let mutable i = 0
                    while i < stringBytes.Length - 1 do
                        let code = (int stringBytes.[i] <<< 8) ||| int stringBytes.[i + 1]
                        match charMap.TryGetValue(code) with
                        | true, s -> sb.Append(s) |> ignore
                        | _ -> sb.Append('\uFFFD') |> ignore
                        i <- i + 2
                else
                    for b in stringBytes do
                        match charMap.TryGetValue(int b) with
                        | true, s -> sb.Append(s) |> ignore
                        | _ -> sb.Append(char b) |> ignore
                Some (sb.ToString())
        with _ -> None

    // ── Glyph Name Tables ───────────────────────────────────

    let private glyphNameForByte (encoding: string) (code: int) : string =
        let asciiNames = [|
            "space";"exclam";"quotedbl";"numbersign";"dollar";"percent";"ampersand";"quotesingle"
            "parenleft";"parenright";"asterisk";"plus";"comma";"hyphen";"period";"slash"
            "zero";"one";"two";"three";"four";"five";"six";"seven";"eight";"nine"
            "colon";"semicolon";"less";"equal";"greater";"question";"at"
            "A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";"N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z"
            "bracketleft";"backslash";"bracketright";"asciicircum";"underscore";"grave"
            "a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"
            "braceleft";"bar";"braceright";"asciitilde"
        |]
        if code >= 0x20 && code <= 0x7E then asciiNames.[code - 0x20]
        elif code = 0 then ".notdef"
        else
        match encoding with
        | "MacRomanEncoding" ->
            let macHigh = [|
                "Adieresis";"Aring";"Ccedilla";"Eacute";"Ntilde";"Odieresis";"Udieresis";"aacute"
                "agrave";"acircumflex";"adieresis";"atilde";"aring";"ccedilla";"eacute";"egrave"
                "ecircumflex";"edieresis";"iacute";"igrave";"icircumflex";"idieresis";"ntilde";"oacute"
                "ograve";"ocircumflex";"odieresis";"otilde";"uacute";"ugrave";"ucircumflex";"udieresis"
                "dagger";"degree";"cent";"sterling";"section";"bullet";"paragraph";"germandbls"
                "registered";"copyright";"trademark";"acute";"dieresis";"notequal";"AE";"Oslash"
                "infinity";"plusminus";"lessequal";"greaterequal";"yen";"mu";"partialdiff";"summation"
                "product";"pi";"integral";"ordfeminine";"ordmasculine";"Omega";"ae";"oslash"
                "questiondown";"exclamdown";"logicalnot";"radical";"florin";"approxequal";"Delta";"guillemotleft"
                "guillemotright";"ellipsis";"space";"Agrave";"Atilde";"Otilde";"OE";"oe"
                "endash";"emdash";"quotedblleft";"quotedblright";"quoteleft";"quoteright";"divide";"lozenge"
                "ydieresis";"Ydieresis";"fraction";"Euro";"guilsinglleft";"guilsinglright";"fi";"fl"
                "daggerdbl";"periodcentered";"quotesinglbase";"quotedblbase";"perthousand";"Acircumflex";"Ecircumflex";"Aacute"
                "Edieresis";"Egrave";"Iacute";"Icircumflex";"Idieresis";"Igrave";"Oacute";"Ocircumflex"
                "apple";"Ograve";"Uacute";"Ucircumflex";"Ugrave";"dotlessi";"circumflex";"tilde"
                "macron";"breve";"dotaccent";"ring";"cedilla";"hungarumlaut";"ogonek";"caron"
            |]
            if code >= 0x80 && code <= 0xFF then macHigh.[code - 0x80] else ""
        | _ ->
            let winHigh = [|
                "Euro";"";"quotesinglbase";"florin";"quotedblbase";"ellipsis";"dagger";"daggerdbl"
                "circumflex";"perthousand";"Scaron";"guilsinglleft";"OE";"";"Zcaron";""
                "";"quoteleft";"quoteright";"quotedblleft";"quotedblright";"bullet";"endash";"emdash"
                "tilde";"trademark";"scaron";"guilsinglright";"oe";"";"zcaron";"Ydieresis"
                "space";"exclamdown";"cent";"sterling";"currency";"yen";"brokenbar";"section"
                "dieresis";"copyright";"ordfeminine";"guillemotleft";"logicalnot";"hyphen";"registered";"macron"
                "degree";"plusminus";"twosuperior";"threesuperior";"acute";"mu";"paragraph";"periodcentered"
                "cedilla";"onesuperior";"ordmasculine";"guillemotright";"onequarter";"onehalf";"threequarters";"questiondown"
                "Agrave";"Aacute";"Acircumflex";"Atilde";"Adieresis";"Aring";"AE";"Ccedilla"
                "Egrave";"Eacute";"Ecircumflex";"Edieresis";"Igrave";"Iacute";"Icircumflex";"Idieresis"
                "Eth";"Ntilde";"Ograve";"Oacute";"Ocircumflex";"Otilde";"Odieresis";"multiply"
                "Oslash";"Ugrave";"Uacute";"Ucircumflex";"Udieresis";"Yacute";"Thorn";"germandbls"
                "agrave";"aacute";"acircumflex";"atilde";"adieresis";"aring";"ae";"ccedilla"
                "egrave";"eacute";"ecircumflex";"edieresis";"igrave";"iacute";"icircumflex";"idieresis"
                "eth";"ntilde";"ograve";"oacute";"ocircumflex";"otilde";"odieresis";"divide"
                "oslash";"ugrave";"uacute";"ucircumflex";"udieresis";"yacute";"thorn";"ydieresis"
            |]
            if code >= 0x80 && code <= 0xFF then winHigh.[code - 0x80] else ""

    // ── Standard 14 Font Widths ──────────────────────────────

    module Standard14 =
        let private courier = Array.create 256 600

        let private helvetica = [|
            0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
            278;278;355;556;556;889;667;191;333;333;389;584;278;333;278;278
            556;556;556;556;556;556;556;556;556;556;278;278;584;584;584;556
            1015;667;667;722;722;667;611;778;722;278;500;667;556;833;722;778
            667;778;722;667;611;722;667;944;667;667;611;278;278;278;469;556
            333;556;556;500;556;556;278;556;556;222;222;500;222;833;556;556
            556;556;333;500;278;556;500;722;500;500;500;334;260;334;584;0
            556;0;222;556;333;1000;556;556;333;1000;667;333;1000;0;611;0
            0;222;222;333;333;350;556;1000;333;1000;500;333;944;0;500;667
            278;333;556;556;556;556;260;556;333;737;370;556;584;333;737;333
            400;584;333;333;333;556;537;278;333;333;365;556;834;834;834;611
            667;667;667;667;667;667;1000;722;667;667;667;667;278;278;278;278
            722;722;778;778;778;778;778;584;778;722;722;722;722;667;667;611
            556;556;556;556;556;556;889;500;556;556;556;556;278;278;278;278
            556;556;556;556;556;556;556;584;611;556;556;556;556;500;556;500
            |]

        let private helveticaBold = [|
            0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
            278;333;474;556;556;889;722;238;333;333;389;584;278;333;278;278
            556;556;556;556;556;556;556;556;556;556;333;333;584;584;584;611
            975;722;722;722;722;667;611;778;722;278;556;722;611;833;722;778
            667;778;722;667;611;722;667;944;667;667;611;333;278;333;584;556
            333;556;611;556;611;556;333;611;611;278;278;556;278;889;611;611
            611;611;389;556;333;611;556;778;556;556;500;389;280;389;584;0
            556;0;278;556;500;1000;556;556;333;1000;667;333;1000;0;611;0
            0;278;278;500;500;350;556;1000;333;1000;556;333;944;0;500;667
            278;333;556;556;556;556;280;556;333;737;370;556;584;333;737;333
            400;584;333;333;333;611;556;278;333;333;365;556;834;834;834;611
            722;722;722;722;722;722;1000;722;667;667;667;667;278;278;278;278
            722;722;778;778;778;778;778;584;778;722;722;722;722;667;667;611
            556;556;556;556;556;556;889;556;556;556;556;556;278;278;278;278
            611;611;611;611;611;611;611;584;611;611;611;611;611;556;611;556
            |]

        let private helveticaOblique = helvetica
        let private helveticaBoldOblique = helveticaBold

        let private timesRoman = [|
            0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
            250;333;408;500;500;833;778;180;333;333;500;564;250;333;250;278
            500;500;500;500;500;500;500;500;500;500;278;278;564;564;564;444
            921;722;667;667;722;611;556;722;722;333;389;722;611;889;722;722
            556;722;667;556;611;722;722;944;722;722;611;333;278;333;469;500
            333;444;500;444;500;444;333;500;500;278;278;500;278;778;500;500
            500;500;333;389;278;500;500;722;500;500;444;480;200;480;541;0
            500;0;333;500;444;1000;500;500;333;1000;556;333;889;0;611;0
            0;333;333;444;444;350;500;1000;333;980;389;333;722;0;444;722
            250;333;500;500;500;500;200;500;333;760;276;500;564;333;760;333
            400;564;300;300;333;500;453;250;333;300;310;500;750;750;750;444
            722;722;722;722;722;722;889;667;611;611;611;611;333;333;333;333
            722;722;722;722;722;722;722;564;722;722;722;722;722;722;556;500
            444;444;444;444;444;444;667;444;444;444;444;444;278;278;278;278
            500;500;500;500;500;500;500;564;500;500;500;500;500;500;500;500
            |]

        let private timesBold = [|
            0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
            250;333;555;500;500;1000;833;278;333;333;500;570;250;333;250;278
            500;500;500;500;500;500;500;500;500;500;333;333;570;570;570;500
            930;722;667;722;722;667;611;778;778;389;500;778;667;944;722;778
            611;778;722;556;667;722;722;1000;722;722;667;333;278;333;581;500
            333;500;556;444;556;444;333;500;556;278;333;556;278;833;556;500
            556;556;444;389;333;556;500;722;500;500;444;394;220;394;520;0
            500;0;333;500;500;1000;500;500;333;1000;556;333;1000;0;667;0
            0;333;333;500;500;350;500;1000;333;1000;389;333;722;0;444;722
            250;333;500;500;500;500;220;500;333;747;300;500;570;333;747;333
            400;570;300;300;333;556;540;250;333;300;330;500;750;750;750;500
            722;722;722;722;722;722;1000;722;667;667;667;667;389;389;389;389
            722;722;778;778;778;778;778;570;778;722;722;722;722;722;611;556
            500;500;500;500;500;500;722;444;444;444;444;444;278;278;278;278
            500;556;500;500;500;500;500;570;500;556;556;556;556;500;556;500
            |]

        let private timesItalic = [|
            0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
            250;333;420;500;500;833;778;214;333;333;500;675;250;333;250;278
            500;500;500;500;500;500;500;500;500;500;333;333;675;675;675;500
            920;611;611;667;722;611;611;722;722;333;444;667;556;833;667;722
            611;722;611;500;556;722;611;833;611;556;556;389;278;389;422;500
            333;500;500;444;500;444;278;500;500;278;278;444;278;722;500;500
            500;500;389;389;278;500;444;667;444;444;389;400;275;400;541;0
            500;0;333;500;556;889;500;500;333;1000;500;333;944;0;556;0
            0;333;333;556;556;350;500;889;333;980;389;333;667;0;389;556
            250;389;500;500;500;500;275;500;333;760;276;500;675;333;760;333
            400;675;300;300;333;500;523;250;333;300;310;500;750;750;750;500
            611;611;611;611;611;611;889;667;611;611;611;611;333;333;333;333
            722;667;722;722;722;722;722;675;722;722;722;722;722;556;611;500
            500;500;500;500;500;500;667;444;444;444;444;444;278;278;278;278
            500;500;500;500;500;500;500;675;500;500;500;500;500;444;500;444
            |]

        let private timesBoldItalic = [|
            0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
            250;389;555;500;500;833;778;278;333;333;500;570;250;333;250;278
            500;500;500;500;500;500;500;500;500;500;333;333;570;570;570;500
            832;667;667;667;722;667;667;722;778;389;500;667;611;889;722;722
            611;722;667;556;611;722;667;889;667;611;611;333;278;333;570;500
            333;500;500;444;500;444;333;500;556;278;278;500;278;778;556;500
            500;500;389;389;278;556;444;667;500;444;389;348;220;348;570;0
            500;0;333;500;500;1000;500;500;333;1000;556;333;944;0;611;0
            0;333;333;500;500;350;500;1000;333;1000;389;333;722;0;389;611
            250;389;500;500;500;500;220;500;333;747;266;500;606;333;747;333
            400;570;300;300;333;576;500;250;333;300;300;500;750;750;750;500
            667;667;667;667;667;667;944;667;667;667;667;667;389;389;389;389
            722;722;722;722;722;722;722;570;722;722;722;722;722;611;611;500
            500;500;500;500;500;500;722;444;444;444;444;444;278;278;278;278
            500;556;500;500;500;500;500;570;500;556;556;556;556;444;500;444
            |]

        let private symbol = [|
            0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
            250;333;713;500;549;833;778;439;333;333;500;549;250;549;250;278
            500;500;500;500;500;500;500;500;500;500;278;278;549;549;549;444
            549;722;667;722;612;611;763;603;722;333;631;722;686;889;722;722
            768;741;556;592;611;690;439;768;645;795;611;333;863;333;658;500
            500;631;549;549;494;439;521;411;603;329;603;549;549;576;521;549
            549;521;549;603;439;576;713;686;493;686;494;480;200;480;549;0
            0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
            0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
            0;620;247;549;167;713;500;753;753;753;753;1042;987;603;987;603
            400;549;411;549;549;713;494;460;549;549;549;549;1000;603;1000;658
            823;686;795;987;768;768;823;768;768;713;713;713;713;713;713;713
            768;713;790;790;890;823;549;250;713;603;603;1042;987;603;987;603
            494;329;790;790;786;713;384;384;384;384;384;384;494;494;494;494
            0;329;274;686;686;686;384;384;384;384;384;384;494;494;790;0
            |]

        let private zapfDingbats = [|
            0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
            278;974;961;974;980;719;789;790;791;690;960;939;549;855;911;933
            911;945;974;755;846;762;761;571;677;763;760;759;754;494;552;537
            577;692;786;788;788;790;793;794;816;823;789;841;823;833;816;831
            923;744;723;749;790;792;695;776;768;792;759;707;708;682;701;826
            815;789;789;707;687;696;689;786;787;713;791;785;791;873;761;762
            762;759;759;892;892;788;784;438;138;277;415;392;392;668;668;0
            390;390;317;317;276;276;509;509;410;410;234;234;334;334;0;0
            0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
            0;732;544;544;910;667;760;760;776;595;694;626;788;788;788;788
            788;788;788;788;788;788;788;788;788;788;788;788;788;788;788;788
            788;788;788;788;788;788;788;788;788;788;788;788;788;788;788;788
            788;788;788;788;894;838;1016;458;748;924;748;918;927;928;928;834
            873;828;924;924;917;930;931;463;883;836;836;867;867;696;696;874
            0;874;760;946;771;865;771;888;967;888;831;873;927;970;918;0
            |]

        let lookup (name: string) : int[] option =
            let n = if name.Length > 7 && name.[6] = '+' then name.Substring(7) else name
            match n with
            | "Helvetica" -> Some helvetica
            | "Helvetica-Bold" -> Some helveticaBold
            | "Helvetica-Oblique" | "Helvetica-Italic" -> Some helveticaOblique
            | "Helvetica-BoldOblique" | "Helvetica-BoldItalic" -> Some helveticaBoldOblique
            | "Times-Roman" | "Times" -> Some timesRoman
            | "Times-Bold" -> Some timesBold
            | "Times-Italic" -> Some timesItalic
            | "Times-BoldItalic" -> Some timesBoldItalic
            | "Courier" -> Some courier
            | "Courier-Bold" -> Some courier
            | "Courier-Oblique" | "Courier-Italic" -> Some courier
            | "Courier-BoldOblique" | "Courier-BoldItalic" -> Some courier
            | "Symbol" -> Some symbol
            | "ZapfDingbats" -> Some zapfDingbats
            | _ -> None

    // ── Font Data Types ─────────────────────────────────────

    type PdfFontData =
        | NoFontData
        | CffFontData of data: byte[] * glyphWidths: int[] * ascender: int * descender: int
        | Type1FontData of data: byte[] * length1: int * length2: int * glyphWidths: int[] * ascender: int * descender: int
        | TrueTypeFontData of data: byte[]

    type PdfFontInfo = {
        ToUnicodeCMap: byte[] option
        Widths: float[] option
        FirstChar: int
        Encoding: string
        EncodingDiffs: Map<int, string>
        ByteToGlyph: int[] option
        IsComposite: bool
        CIDWidths: float[] option
        CIDToGIDMap: int[] option
        FontData: PdfFontData
    }

    let defaultInfo = {
        ToUnicodeCMap = None; Widths = None; FirstChar = 0; Encoding = "WinAnsi"
        EncodingDiffs = Map.empty; ByteToGlyph = None; IsComposite = false
        CIDWidths = None; CIDToGIDMap = None; FontData = NoFontData
    }

    // ── Font Helpers ────────────────────────────────────────

    let private fontCache = Collections.Generic.Dictionary<string, PdfFontData * int[] option>()

    let private parseFontCmap (data: byte[]) : int[] option =
        try
            let r i = (int data.[i] <<< 8) ||| int data.[i+1]
            let r32 i = (int data.[i] <<< 24) ||| (int data.[i+1] <<< 16) ||| (int data.[i+2] <<< 8) ||| int data.[i+3]
            let numTables = r 4
            let mutable cmapOff = 0
            for i in 0..numTables-1 do
                let off = 12 + i * 16
                if data.[off] = byte 'c' && data.[off+1] = byte 'm' && data.[off+2] = byte 'a' && data.[off+3] = byte 'p' then
                    cmapOff <- r32 (off + 8)
            if cmapOff = 0 then None
            else
                let numSub = r (cmapOff + 2)
                let map = Array.create 256 0
                let mutable found = false
                for i in 0..numSub-1 do
                    if not found then
                        let sOff = cmapOff + 4 + i * 8
                        let subtableOff = cmapOff + r32 (sOff + 4)
                        let fmt = r subtableOff
                        if fmt = 6 then
                            let firstCode = r (subtableOff + 6)
                            let entryCount = r (subtableOff + 8)
                            for j in 0..entryCount-1 do
                                let code = firstCode + j
                                if code < 256 then
                                    map.[code] <- r (subtableOff + 10 + j * 2)
                            found <- true
                        elif fmt = 4 then
                            let segCount = r (subtableOff + 6) / 2
                            let endCodeOff = subtableOff + 14
                            let startCodeOff = endCodeOff + segCount * 2 + 2
                            let idDeltaOff = startCodeOff + segCount * 2
                            let idRangeOff = idDeltaOff + segCount * 2
                            for j in 0..segCount-1 do
                                let startCode = r (startCodeOff + j * 2)
                                let endCode = r (endCodeOff + j * 2)
                                let delta = int16 (r (idDeltaOff + j * 2))
                                let rangeOff = r (idRangeOff + j * 2)
                                if startCode < 256 && endCode < 65535 then
                                    for code in startCode..min endCode 255 do
                                        let gid =
                                            if rangeOff = 0 then (code + int delta) &&& 0xFFFF
                                            else
                                                let idx = idRangeOff + j * 2 + rangeOff + (code - startCode) * 2
                                                if idx + 1 < data.Length then
                                                    let g = r idx
                                                    if g = 0 then 0 else (g + int delta) &&& 0xFFFF
                                                else 0
                                        map.[code] <- gid
                            found <- true
                if found then Some map else None
        with _ -> None

    let private buildNameBasedByteMap (encoding: string) (encodingDiffs: Map<int, string>) (nameToGlyph: Collections.Generic.Dictionary<string, int>) : int[] =
        let byteMap = Array.zeroCreate 256
        for code in 0..255 do
            let name =
                match Map.tryFind code encodingDiffs with
                | Some n -> n
                | None -> glyphNameForByte encoding code
            if name.Length > 0 then
                match nameToGlyph.TryGetValue(name) with
                | true, gid -> byteMap.[code] <- gid
                | _ -> ()
        byteMap

    let private buildGlyphWidths (numGlyphs: int) (widths: float[] option) (firstChar: int) (byteMap: int[]) : int[] =
        let glyphWidths = Array.create (max numGlyphs 1) 0
        match widths with
        | Some ws ->
            for i in 0 .. ws.Length - 1 do
                let code = firstChar + i
                if code < 256 then
                    let gid = byteMap.[code]
                    if gid > 0 && gid < glyphWidths.Length then
                        glyphWidths.[gid] <- int (ws.[i])
        | None -> ()
        glyphWidths

    let private getMetrics (doc: PdfDocument) (fdObj: PdfObj) =
        let ascender =
            match Pdf.dictGet doc "Ascent" fdObj with
            | Some (PdfInt n) -> int n | Some (PdfReal f) -> int f | _ -> 800
        let descender =
            match Pdf.dictGet doc "Descent" fdObj with
            | Some (PdfInt n) -> int n | Some (PdfReal f) -> int f | _ -> -200
        ascender, descender

    let private parseCIDWidths (doc: PdfDocument) (wArr: PdfObj list) (defaultWidth: float) : float[] =
        let widths = Array.create 65536 defaultWidth
        let items = wArr |> Array.ofList
        let mutable i = 0
        while i < items.Length do
            match Pdf.deref doc items.[i] with
            | PdfInt cid ->
                if i + 1 < items.Length then
                    match Pdf.deref doc items.[i + 1] with
                    | PdfArray ws ->
                        let startCid = int cid
                        let mutable j = 0
                        for w in ws do
                            let wv = match Pdf.deref doc w with PdfInt n -> float n | PdfReal f -> f | _ -> defaultWidth
                            let c = startCid + j
                            if c < widths.Length then widths.[c] <- wv
                            j <- j + 1
                        i <- i + 2
                    | PdfInt cidLast when i + 2 < items.Length ->
                        let w = match Pdf.deref doc items.[i + 2] with PdfInt n -> float n | PdfReal f -> f | _ -> defaultWidth
                        for c in int cid .. int cidLast do
                            if c < widths.Length then widths.[c] <- w
                        i <- i + 3
                    | _ -> i <- i + 1
                else i <- i + 1
            | _ -> i <- i + 1
        widths

    let private parseCIDToGIDMap (doc: PdfDocument) (obj: PdfObj) : int[] option =
        match Pdf.deref doc obj with
        | PdfName "Identity" -> None
        | _ ->
            try
                let data = Pdf.getStreamData doc obj
                if data.Length >= 2 then
                    let count = data.Length / 2
                    let map = Array.create count 0
                    for i in 0..count-1 do
                        map.[i] <- (int data.[i * 2] <<< 8) ||| int data.[i * 2 + 1]
                    Some map
                else None
            with _ -> None

    // ── Font Resolution ─────────────────────────────────────

    let private resolveType0 (doc: PdfDocument) (fontObj: PdfObj) (toUnicode: byte[] option) (baseFontName: string) : PdfFontInfo =
        try
            let descendantFonts =
                match Pdf.dictGet doc "DescendantFonts" fontObj with
                | Some (PdfArray (df :: _)) -> Some (Pdf.deref doc df)
                | _ -> None

            match descendantFonts with
            | Some cidFont ->
                let defaultWidth =
                    match Pdf.dictGet doc "DW" cidFont with
                    | Some (PdfInt n) -> float n | Some (PdfReal f) -> f | _ -> 1000.0

                let cidWidths =
                    match Pdf.dictGet doc "W" cidFont with
                    | Some (PdfArray wArr) -> Some (parseCIDWidths doc wArr defaultWidth)
                    | _ -> None

                let cidToGIDMap =
                    match Pdf.dictGet doc "CIDToGIDMap" cidFont with
                    | Some obj -> parseCIDToGIDMap doc obj
                    | None -> None

                let fontData =
                    match fontCache.TryGetValue(baseFontName) with
                    | true, (fd, _) -> fd
                    | _ ->
                        let fd =
                            match Pdf.dictGet doc "FontDescriptor" cidFont with
                            | Some fdRef ->
                                let fdObj = Pdf.deref doc fdRef
                                let ff2 = Pdf.dictGet doc "FontFile2" fdObj
                                let ff3 = Pdf.dictGet doc "FontFile3" fdObj
                                let fontStream = ff2 |> Option.orElse ff3
                                let isCff = ff3.IsSome && ff2.IsNone
                                match fontStream with
                                | Some s ->
                                    try
                                        let data = Pdf.getStreamData doc s
                                        if data.Length > 0 then
                                            if isCff then
                                                let ascender, descender = getMetrics doc fdObj
                                                CffFontData (data, [||], ascender, descender)
                                            else
                                                TrueTypeFontData data
                                        else NoFontData
                                    with _ -> NoFontData
                                | None -> NoFontData
                            | None -> NoFontData
                        fontCache.[baseFontName] <- (fd, None)
                        fd

                { ToUnicodeCMap = toUnicode; Widths = None; FirstChar = 0; Encoding = "Identity-H"
                  EncodingDiffs = Map.empty; ByteToGlyph = None
                  IsComposite = true; CIDWidths = cidWidths; CIDToGIDMap = cidToGIDMap; FontData = fontData }
            | None -> { defaultInfo with ToUnicodeCMap = toUnicode; IsComposite = true }
        with _ -> { defaultInfo with ToUnicodeCMap = toUnicode; IsComposite = true }

    let resolve (doc: PdfDocument) (resources: PdfObj) (fontName: string) : PdfFontInfo =
        match Pdf.dictGet doc "Font" resources with
        | Some fontDict ->
            match Pdf.dictGet doc fontName fontDict with
            | Some fontObj ->
                let toUnicode =
                    match Pdf.dictGet doc "ToUnicode" fontObj with
                    | Some cmapStream ->
                        try Some (Pdf.getStreamData doc cmapStream) with _ -> None
                    | None -> None

                let subtype =
                    match Pdf.dictGet doc "Subtype" fontObj with
                    | Some (PdfName s) -> s | _ -> ""

                let baseFontName =
                    match Pdf.dictGet doc "BaseFont" fontObj with
                    | Some (PdfName n) -> n | _ -> fontName

                if subtype = "Type0" then
                    resolveType0 doc fontObj toUnicode baseFontName
                else

                let firstChar =
                    match Pdf.dictGet doc "FirstChar" fontObj with
                    | Some (PdfInt n) -> int n
                    | _ -> 0

                let widths =
                    match Pdf.dictGet doc "Widths" fontObj |> Option.map (Pdf.deref doc) with
                    | Some (PdfArray ws) ->
                        ws |> List.map (fun o ->
                            match Pdf.deref doc o with
                            | PdfInt n -> float n
                            | PdfReal f -> f
                            | _ -> 0.0)
                        |> Array.ofList |> Some
                    | _ ->
                        match Standard14.lookup baseFontName with
                        | Some stdWidths -> Some (stdWidths |> Array.map float)
                        | None -> None

                let encoding, encodingDiffs =
                    match Pdf.dictGet doc "Encoding" fontObj with
                    | Some (PdfName enc) -> enc, Map.empty
                    | Some encObj ->
                        let encObj = Pdf.deref doc encObj
                        let baseEnc =
                            match Pdf.dictGet doc "BaseEncoding" encObj with
                            | Some (PdfName n) -> n | _ -> "WinAnsi"
                        let diffs =
                            match Pdf.dictGet doc "Differences" encObj with
                            | Some (PdfArray items) ->
                                let mutable code = 0
                                let mutable m = Map.empty
                                for item in items do
                                    match Pdf.deref doc item with
                                    | PdfInt n -> code <- int n
                                    | PdfName name -> m <- m |> Map.add code name; code <- code + 1
                                    | _ -> ()
                                m
                            | _ -> Map.empty
                        baseEnc, diffs
                    | _ -> "WinAnsi", Map.empty

                // When the PDF font dict omits /Encoding, the embedded font's own
                // built-in encoding is authoritative (common with subset fonts).
                let pdfHasEncoding = (Pdf.dictGet doc "Encoding" fontObj).IsSome

                let fontData, byteToGlyph =
                    match fontCache.TryGetValue(baseFontName) with
                    | true, (fd, bg) -> fd, bg
                    | _ ->
                        let fd, bg =
                            match Pdf.dictGet doc "FontDescriptor" fontObj with
                            | Some fdRef ->
                                let fdObj = Pdf.deref doc fdRef
                                let ff2 = Pdf.dictGet doc "FontFile2" fdObj
                                let ff3 = Pdf.dictGet doc "FontFile3" fdObj
                                let ff1 = Pdf.dictGet doc "FontFile" fdObj
                                let fontStream = ff2 |> Option.orElse ff3 |> Option.orElse ff1
                                let isCff = ff3.IsSome && ff2.IsNone
                                match fontStream with
                                | Some s ->
                                    try
                                        let data = Pdf.getStreamData doc s
                                        if data.Length > 0 then
                                            if isCff then
                                                let nameToGlyph, numGlyphs = Rbt.OpenType.Cff.getCffNameToGlyph data 0
                                                // Fall back to the CFF built-in encoding when the PDF has none.
                                                let enc, diffs =
                                                    if not pdfHasEncoding then
                                                        let builtin = Rbt.OpenType.Cff.getCffBuiltinEncoding data 0
                                                        if not builtin.IsEmpty then "", builtin
                                                        else encoding, encodingDiffs
                                                    else encoding, encodingDiffs
                                                let byteMap = buildNameBasedByteMap enc diffs nameToGlyph
                                                let glyphWidths = buildGlyphWidths numGlyphs widths firstChar byteMap
                                                let ascender, descender = getMetrics doc fdObj
                                                CffFontData (data, glyphWidths, ascender, descender), Some byteMap
                                            elif ff1.IsSome then
                                                let streamObj = Pdf.deref doc ff1.Value
                                                let length1 =
                                                    match Pdf.dictGet doc "Length1" streamObj with
                                                    | Some (PdfInt n) -> int n | _ -> 0
                                                let length2 =
                                                    match Pdf.dictGet doc "Length2" streamObj with
                                                    | Some (PdfInt n) -> int n | _ -> 0
                                                try
                                                    let t1 = Rbt.OpenType.Type1.parseType1 data length1 length2
                                                    let nameToGlyph = Rbt.OpenType.Type1.nameToGlyphIndex t1
                                                    let numGlyphs = t1.CharStrings.Length
                                                    // Fall back to the Type1 built-in encoding when the PDF has none.
                                                    let enc, diffs =
                                                        if not pdfHasEncoding && not t1.BuiltinEncoding.IsEmpty then "", t1.BuiltinEncoding
                                                        else encoding, encodingDiffs
                                                    let byteMap = buildNameBasedByteMap enc diffs nameToGlyph
                                                    let glyphWidths = buildGlyphWidths numGlyphs widths firstChar byteMap
                                                    let ascender, descender = getMetrics doc fdObj
                                                    Type1FontData (data, length1, length2, glyphWidths, ascender, descender), Some byteMap
                                                with _ -> NoFontData, None
                                            else
                                                let gm = parseFontCmap data
                                                TrueTypeFontData data, gm
                                        else NoFontData, None
                                    with _ -> NoFontData, None
                                | None -> NoFontData, None
                            | None -> NoFontData, None
                        fontCache.[baseFontName] <- (fd, bg)
                        fd, bg

                { ToUnicodeCMap = toUnicode; Widths = widths; FirstChar = firstChar; Encoding = encoding
                  EncodingDiffs = encodingDiffs; ByteToGlyph = byteToGlyph
                  IsComposite = false; CIDWidths = None; CIDToGIDMap = None; FontData = fontData }
            | None -> defaultInfo
        | None -> defaultInfo

    let charWidth (fi: PdfFontInfo) (charCode: int) : float =
        match fi.CIDWidths with
        | Some cw ->
            if charCode >= 0 && charCode < cw.Length then cw.[charCode]
            else 1000.0
        | None ->
        match fi.Widths with
        | Some ws ->
            let idx = charCode - fi.FirstChar
            if idx >= 0 && idx < ws.Length then ws.[idx]
            else 600.0
        | None -> 600.0

/// PDF outline (bookmarks / table of contents) extraction
module Outline =
    type OutlineItem = {
        Title: string
        Page: int option
        Children: OutlineItem list
    }

    let private decodeTitle (data: byte[]) : string =
        if data.Length >= 2 && data.[0] = 0xFEuy && data.[1] = 0xFFuy then
            // UTF-16BE with BOM
            let sb = Text.StringBuilder()
            let mutable i = 2
            while i + 1 < data.Length do
                let ch = (int data.[i] <<< 8) ||| int data.[i + 1]
                sb.Append(char ch) |> ignore
                i <- i + 2
            sb.ToString()
        else
            latin1String data

    let resolveDestPage (doc: PdfDocument) (dest: PdfObj) : int option =
        let pageRef =
            match Pdf.deref doc dest with
            | PdfArray (first :: _) -> Some first
            | PdfName namedDest ->
                // Look up in /Dests or /Names/Dests
                let catalog = Pdf.deref doc doc.Catalog
                let fromDests =
                    match Pdf.dictGet doc "Dests" catalog with
                    | Some dests ->
                        match Pdf.dictGet doc namedDest dests with
                        | Some d ->
                            match Pdf.deref doc d with
                            | PdfArray (first :: _) -> Some first
                            | _ -> None
                        | None -> None
                    | None -> None
                fromDests
            | _ -> None
        match pageRef with
        | Some pr ->
            let target = Pdf.deref doc pr
            // Find which page number this is by iterating pages
            let total = Pdf.numPages doc
            let mutable result = None
            for p in 1 .. total do
                if result.IsNone then
                    match Pdf.getPage doc p with
                    | Some ref ->
                        let page = Pdf.deref doc ref
                        if System.Object.ReferenceEquals(page, target) || ref = pr then
                            result <- Some p
                    | None -> ()
            result
        | None -> None

    let private readSiblingChain (doc: PdfDocument) (first: PdfObj) : OutlineItem list =
        let rec readChain (current: PdfObj) acc =
            let node = Pdf.deref doc current
            match node with
            | PdfNull -> List.rev acc
            | _ ->
                let title =
                    match Pdf.dictGet doc "Title" node with
                    | Some titleObj ->
                        match Pdf.deref doc titleObj with
                        | PdfString data -> decodeTitle data
                        | _ -> ""
                    | None -> ""
                let page =
                    match Pdf.dictGet doc "Dest" node with
                    | Some dest -> resolveDestPage doc dest
                    | None ->
                        match Pdf.dictGet doc "A" node with
                        | Some action ->
                            let a = Pdf.deref doc action
                            match Pdf.dictGet doc "S" a with
                            | Some (PdfName "GoTo") ->
                                match Pdf.dictGet doc "D" a with
                                | Some dest -> resolveDestPage doc dest
                                | None -> None
                            | _ -> None
                        | None -> None
                let children =
                    match Pdf.dictGet doc "First" node with
                    | Some child -> readChain child []
                    | None -> []
                let item = { Title = title; Page = page; Children = children }
                match Pdf.dictGet doc "Next" node with
                | Some next -> readChain next (item :: acc)
                | None -> List.rev (item :: acc)
        readChain first []

    let extract (doc: PdfDocument) : OutlineItem list =
        let catalog = Pdf.deref doc doc.Catalog
        match Pdf.dictGet doc "Outlines" catalog with
        | Some outlines ->
            let outlineObj = Pdf.deref doc outlines
            match Pdf.dictGet doc "First" outlineObj with
            | Some first -> readSiblingChain doc first
            | None -> []
        | None -> []

/// Navigation through PDF structures using path operators
module Nav =
    /// A cursor into a PDF document at a specific object
    type PdfNav = { Doc: PdfDocument; Obj: PdfObj }

    /// Create a navigator starting at an object
    let at (doc: PdfDocument) (obj: PdfObj) = { Doc = doc; Obj = obj }

    /// Start navigation at the catalog
    let catalog (doc: PdfDocument) = { Doc = doc; Obj = doc.Catalog }

    /// Get the dereferenced value at current position
    let value (nav: PdfNav) = Pdf.deref nav.Doc nav.Obj

    /// Get the raw (possibly indirect) object
    let raw (nav: PdfNav) = nav.Obj

    /// Navigate into a dict key
    let key (k: string) (nav: PdfNav) =
        match Pdf.deref nav.Doc nav.Obj with
        | PdfDict d ->
            match Map.tryFind k d with
            | Some v -> { nav with Obj = v }
            | None -> { nav with Obj = PdfNull }
        | PdfStream (h, _) ->
            match Map.tryFind k h with
            | Some v -> { nav with Obj = v }
            | None -> { nav with Obj = PdfNull }
        | _ -> { nav with Obj = PdfNull }

    /// Navigate into an array index
    let nth (i: int) (nav: PdfNav) =
        match Pdf.deref nav.Doc nav.Obj with
        | PdfArray arr when i >= 0 && i < List.length arr ->
            { nav with Obj = List.item i arr }
        | _ -> { nav with Obj = PdfNull }

    /// Check if current position is null
    let isNull (nav: PdfNav) =
        match Pdf.deref nav.Doc nav.Obj with
        | PdfNull -> true
        | _ -> false

    /// Try to get an int value
    let asInt (nav: PdfNav) =
        match Pdf.deref nav.Doc nav.Obj with
        | PdfInt n -> Some n
        | _ -> None

    /// Try to get a float value (accepts int or real)
    let asFloat (nav: PdfNav) =
        match Pdf.deref nav.Doc nav.Obj with
        | PdfInt n -> Some (float n)
        | PdfReal r -> Some r
        | _ -> None

    /// Try to get a name value
    let asName (nav: PdfNav) =
        match Pdf.deref nav.Doc nav.Obj with
        | PdfName n -> Some n
        | _ -> None

    /// Try to get a string value
    let asString (nav: PdfNav) =
        match Pdf.deref nav.Doc nav.Obj with
        | PdfString s -> Some s
        | _ -> None

    /// Try to get an array as a list of navigators
    let asArray (nav: PdfNav) =
        match Pdf.deref nav.Doc nav.Obj with
        | PdfArray arr -> Some (arr |> List.map (fun o -> { nav with Obj = o }))
        | _ -> None

    /// Get stream data (decoded)
    let streamData (nav: PdfNav) =
        Pdf.getStreamData nav.Doc nav.Obj

/// Path operator for navigation: doc/>"Pages"/"Kids"/!0
[<AutoOpen>]
module NavOps =
    /// Navigate into a dict key: nav /> "KeyName"
    let (/>) (nav: Nav.PdfNav) (key: string) = Nav.key key nav

    /// Navigate into an array index: nav /! 0
    let (/!) (nav: Nav.PdfNav) (index: int) = Nav.nth index nav

    /// Start navigation from document catalog
    let (/%>) (doc: PdfDocument) (key: string) = Nav.catalog doc |> Nav.key key

    /// Pipe-friendly key access
    let (|/>) (nav: Nav.PdfNav) (key: string) = Nav.key key nav

/// Shape extraction: typed parsing of PDF structures
module Shape =
    /// A shape extracts a typed value from a PDF object
    type Shape<'a> = PdfDocument -> PdfObj -> Result<'a, string>

    /// Run a shape against a navigator
    let parse (shape: Shape<'a>) (nav: Nav.PdfNav) : Result<'a, string> =
        shape nav.Doc nav.Obj

    /// Run a shape against doc + obj
    let extract (shape: Shape<'a>) (doc: PdfDocument) (obj: PdfObj) =
        shape doc obj

    // === Primitive shapes ===

    let pdfInt : Shape<int64> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfInt n -> Ok n
        | other -> Error $"expected int, got {other}"

    let pdfInt32 : Shape<int> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfInt n -> Ok (int n)
        | other -> Error $"expected int, got {other}"

    let pdfReal : Shape<float> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfInt n -> Ok (float n)
        | PdfReal r -> Ok r
        | other -> Error $"expected real, got {other}"

    let pdfBool : Shape<bool> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfBool b -> Ok b
        | other -> Error $"expected bool, got {other}"

    let pdfName : Shape<string> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfName n -> Ok n
        | other -> Error $"expected name, got {other}"

    let pdfString : Shape<byte[]> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfString s -> Ok s
        | other -> Error $"expected string, got {other}"

    let pdfStringText : Shape<string> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfString s -> Ok (Encoding.UTF8.GetString(s))
        | other -> Error $"expected string, got {other}"

    let pdfAny : Shape<PdfObj> = fun doc obj -> Ok (Pdf.deref doc obj)

    let pdfRaw : Shape<PdfObj> = fun _ obj -> Ok obj

    let pdfNull : Shape<unit> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfNull -> Ok ()
        | other -> Error $"expected null, got {other}"

    /// Match a specific name value
    let nameEq (expected: string) : Shape<unit> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfName n when n = expected -> Ok ()
        | PdfName n -> Error $"expected name '{expected}', got '{n}'"
        | other -> Error $"expected name, got {other}"

    /// Match a name starting with / (convenience for PDF-style names)
    let name (expected: string) : Shape<unit> =
        let n = if expected.StartsWith("/") then expected.Substring(1) else expected
        nameEq n

    // === Combinators ===

    /// List of items matching a shape
    let listOf (itemShape: Shape<'a>) : Shape<'a list> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfArray items ->
            let rec loop acc = function
                | [] -> Ok (List.rev acc)
                | x :: xs ->
                    match itemShape doc x with
                    | Ok v -> loop (v :: acc) xs
                    | Error e -> Error $"in array: {e}"
            loop [] items
        | other -> Error $"expected array, got {other}"

    /// Optional shape - returns None if extraction fails
    let opt (shape: Shape<'a>) : Shape<'a option> = fun doc obj ->
        match shape doc obj with
        | Ok v -> Ok (Some v)
        | Error _ -> Ok None

    /// Shape with default value on failure
    let orDefault (defaultVal: 'a) (shape: Shape<'a>) : Shape<'a> = fun doc obj ->
        match shape doc obj with
        | Ok v -> Ok v
        | Error _ -> Ok defaultVal

    /// Map over a shape result
    let map (f: 'a -> 'b) (shape: Shape<'a>) : Shape<'b> = fun doc obj ->
        shape doc obj |> Result.map f

    /// Either of two shapes
    let either (shape1: Shape<'a>) (shape2: Shape<'a>) : Shape<'a> = fun doc obj ->
        match shape1 doc obj with
        | Ok v -> Ok v
        | Error _ -> shape2 doc obj

    // === Dict key extraction ===

    /// Required dict key
    let key (keyName: string) (valueShape: Shape<'a>) : Shape<'a> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfDict d ->
            match Map.tryFind keyName d with
            | Some v -> valueShape doc v
            | None -> Error $"missing required key '{keyName}'"
        | PdfStream (h, _) ->
            match Map.tryFind keyName h with
            | Some v -> valueShape doc v
            | None -> Error $"missing required key '{keyName}'"
        | other -> Error $"expected dict, got {other}"

    /// Optional dict key
    let keyOpt (keyName: string) (valueShape: Shape<'a>) : Shape<'a option> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfDict d ->
            match Map.tryFind keyName d with
            | Some v -> valueShape doc v |> Result.map Some
            | None -> Ok None
        | PdfStream (h, _) ->
            match Map.tryFind keyName h with
            | Some v -> valueShape doc v |> Result.map Some
            | None -> Ok None
        | other -> Error $"expected dict, got {other}"

    /// Optional dict key with default
    let keyOrDefault (keyName: string) (defaultVal: 'a) (valueShape: Shape<'a>) : Shape<'a> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfDict d ->
            match Map.tryFind keyName d with
            | Some v -> valueShape doc v
            | None -> Ok defaultVal
        | PdfStream (h, _) ->
            match Map.tryFind keyName h with
            | Some v -> valueShape doc v
            | None -> Ok defaultVal
        | other -> Error $"expected dict, got {other}"

    // === Computation Expression Builder ===

    type ShapeBuilder() =
        member _.Bind(shape: Shape<'a>, f: 'a -> Shape<'b>) : Shape<'b> =
            fun doc obj ->
                match shape doc obj with
                | Ok a -> (f a) doc obj
                | Error e -> Error e

        member _.Return(x: 'a) : Shape<'a> = fun _ _ -> Ok x

        member _.ReturnFrom(shape: Shape<'a>) : Shape<'a> = shape

        member _.Zero() : Shape<unit> = fun _ _ -> Ok ()

        member _.Combine(shape1: Shape<unit>, shape2: Shape<'a>) : Shape<'a> =
            fun doc obj ->
                match shape1 doc obj with
                | Ok () -> shape2 doc obj
                | Error e -> Error e

        member _.Delay(f: unit -> Shape<'a>) : Shape<'a> = fun doc obj -> f () doc obj

        member this.For(items: 'a seq, body: 'a -> Shape<unit>) : Shape<unit> =
            fun doc obj ->
                let mutable result = Ok ()
                for item in items do
                    match result with
                    | Ok () -> result <- body item doc obj
                    | Error _ -> ()
                result

    let shape = ShapeBuilder()

    // === Convenience tuple extractors ===

    let tuple2 (s1: Shape<'a>) (s2: Shape<'b>) : Shape<'a * 'b> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfArray [o1; o2] ->
            match s1 doc o1, s2 doc o2 with
            | Ok v1, Ok v2 -> Ok (v1, v2)
            | Error e, _ -> Error e
            | _, Error e -> Error e
        | other -> Error $"expected 2-element array, got {other}"

    let tuple4 (s1: Shape<'a>) (s2: Shape<'b>) (s3: Shape<'c>) (s4: Shape<'d>)
               : Shape<'a * 'b * 'c * 'd> = fun doc obj ->
        match Pdf.deref doc obj with
        | PdfArray [o1; o2; o3; o4] ->
            match s1 doc o1, s2 doc o2, s3 doc o3, s4 doc o4 with
            | Ok v1, Ok v2, Ok v3, Ok v4 -> Ok (v1, v2, v3, v4)
            | Error e, _, _, _ -> Error e
            | _, Error e, _, _ -> Error e
            | _, _, Error e, _ -> Error e
            | _, _, _, Error e -> Error e
        | other -> Error $"expected 4-element array, got {other}"

    /// Extract a rectangle (common in PDF)
    let rect : Shape<float * float * float * float> =
        tuple4 pdfReal pdfReal pdfReal pdfReal

/// Color types for PDF drawing
module Color =
    type Color =
        | Gray of float
        | RGB of r: float * g: float * b: float
        | CMYK of c: float * m: float * y: float * k: float

    let gray v = Gray v
    let black = Gray 0.0
    let white = Gray 1.0
    let rgb r g b = RGB (r, g, b)
    let red = RGB (1.0, 0.0, 0.0)
    let green = RGB (0.0, 1.0, 0.0)
    let blue = RGB (0.0, 0.0, 1.0)
    let cmyk c m y k = CMYK (c, m, y, k)

    let toFillOps = function
        | Gray g -> $"{g} g"
        | RGB (r, g, b) -> $"{r} {g} {b} rg"
        | CMYK (c, m, y, k) -> $"{c} {m} {y} {k} k"

    let toStrokeOps = function
        | Gray g -> $"{g} G"
        | RGB (r, g, b) -> $"{r} {g} {b} RG"
        | CMYK (c, m, y, k) -> $"{c} {m} {y} {k} K"

/// Standard page sizes (in points, 72 points = 1 inch)
module PageSize =
    let letter = (612.0, 792.0)
    let legal = (612.0, 1008.0)
    let a4 = (595.0, 842.0)
    let a3 = (842.0, 1190.0)
    let a5 = (420.0, 595.0)
    let landscape (w, h) = (h, w)

/// Content stream drawing operations
module Draw =
    open Color

    type DrawOp =
        | Raw of string
        | SaveState
        | RestoreState
        | SetFillColor of Color
        | SetStrokeColor of Color
        | SetLineWidth of float
        | SetLineCap of int      // 0=butt, 1=round, 2=square
        | SetLineJoin of int     // 0=miter, 1=round, 2=bevel
        | SetMiterLimit of float
        | SetDash of float list * float
        | Transform of float * float * float * float * float * float
        | MoveTo of float * float
        | LineTo of float * float
        | CurveTo of float * float * float * float * float * float
        | CurveToV of float * float * float * float
        | CurveToY of float * float * float * float
        | Rect of float * float * float * float
        | ClosePath
        | Stroke
        | Fill
        | FillEvenOdd
        | FillAndStroke
        | FillAndStrokeEvenOdd
        | ClipPath
        | ClipPathEvenOdd
        | EndPath
        | BeginText
        | EndText
        | SetFont of string * float
        | TextMatrix of float * float * float * float * float * float
        | TextPosition of float * float
        | TextLeading of float
        | ShowText of string
        | ShowTextNextLine of string
        | ShowTextHex of string  // For embedded fonts (glyph IDs as hex)
        /// Show text with kerning adjustments using TJ operator
        /// Takes array of (glyphId, kernAdjust) where kernAdjust is in 1/1000 em
        /// Negative kernAdjust moves right (less space), positive moves left (more space)
        | ShowTextHexWithKern of (int * int) array
        | DrawXObject of string
        /// Set fill alpha (transparency) - uses ExtGState
        | SetFillAlpha of float
        /// Set stroke alpha (transparency) - uses ExtGState
        | SetStrokeAlpha of float

    /// WinAnsi encoding map for Rbt.Unicode code points 0x80-0x9F
    let private winAnsiMap =
        [
            0x20AC, 0x80; 0x201A, 0x82; 0x0192, 0x83; 0x201E, 0x84
            0x2026, 0x85; 0x2020, 0x86; 0x2021, 0x87; 0x02C6, 0x88
            0x2030, 0x89; 0x0160, 0x8A; 0x2039, 0x8B; 0x0152, 0x8C
            0x017D, 0x8E; 0x2018, 0x91; 0x2019, 0x92; 0x201C, 0x93
            0x201D, 0x94; 0x2022, 0x95; 0x2013, 0x96; 0x2014, 0x97
            0x02DC, 0x98; 0x2122, 0x99; 0x0161, 0x9A; 0x203A, 0x9B
            0x0153, 0x9C; 0x017E, 0x9E; 0x0178, 0x9F
        ] |> Map.ofList

    /// Encode string to WinAnsi and escape for PDF literal string
    let private encodeText (s: string) =
        let sb = System.Text.StringBuilder()
        for c in s do
            let code = int c
            let b =
                if code < 0x80 then byte code
                elif code >= 0xA0 && code <= 0xFF then byte code
                else
                    match winAnsiMap.TryFind code with
                    | Some v -> byte v
                    | None -> byte '?'
            match char b with
            | '\\' -> sb.Append("\\\\") |> ignore
            | '(' -> sb.Append("\\(") |> ignore
            | ')' -> sb.Append("\\)") |> ignore
            | _ when b < 32uy || b > 126uy ->
                sb.Append(sprintf "\\%03o" b) |> ignore
            | _ -> sb.Append(char b) |> ignore
        sb.ToString()

    let private fmt (f: float) =
        let s = f.ToString("0.####")
        if s = "-0" then "0" else s

    let toOperator = function
        | Raw s -> s
        | SaveState -> "q"
        | RestoreState -> "Q"
        | SetFillColor c -> toFillOps c
        | SetStrokeColor c -> toStrokeOps c
        | SetLineWidth w -> $"{fmt w} w"
        | SetLineCap c -> $"{c} J"
        | SetLineJoin j -> $"{j} j"
        | SetMiterLimit m -> $"{fmt m} M"
        | SetDash (pattern, phase) ->
            let arr = pattern |> List.map fmt |> String.concat " "
            $"[{arr}] {fmt phase} d"
        | Transform (a, b, c, d, e, f) ->
            $"{fmt a} {fmt b} {fmt c} {fmt d} {fmt e} {fmt f} cm"
        | MoveTo (x, y) -> $"{fmt x} {fmt y} m"
        | LineTo (x, y) -> $"{fmt x} {fmt y} l"
        | CurveTo (x1, y1, x2, y2, x3, y3) ->
            $"{fmt x1} {fmt y1} {fmt x2} {fmt y2} {fmt x3} {fmt y3} c"
        | CurveToV (x2, y2, x3, y3) -> $"{fmt x2} {fmt y2} {fmt x3} {fmt y3} v"
        | CurveToY (x1, y1, x3, y3) -> $"{fmt x1} {fmt y1} {fmt x3} {fmt y3} y"
        | Rect (x, y, w, h) -> $"{fmt x} {fmt y} {fmt w} {fmt h} re"
        | ClosePath -> "h"
        | Stroke -> "S"
        | Fill -> "f"
        | FillEvenOdd -> "f*"
        | FillAndStroke -> "B"
        | FillAndStrokeEvenOdd -> "B*"
        | ClipPath -> "W"
        | ClipPathEvenOdd -> "W*"
        | EndPath -> "n"
        | BeginText -> "BT"
        | EndText -> "ET"
        | SetFont (name, size) -> $"/{name} {fmt size} Tf"
        | TextMatrix (a, b, c, d, e, f) ->
            $"{fmt a} {fmt b} {fmt c} {fmt d} {fmt e} {fmt f} Tm"
        | TextPosition (x, y) -> $"{fmt x} {fmt y} Td"
        | TextLeading leading -> $"{fmt leading} TL"
        | ShowText s -> $"({encodeText s}) Tj"
        | ShowTextNextLine s -> $"({encodeText s}) '"
        | ShowTextHex hex -> $"<{hex}> Tj"
        | ShowTextHexWithKern glyphsWithKern ->
            // Build TJ array: [<glyph> kern <glyph> kern ...]
            // Kern values are in 1/1000 em, positive = move left
            let parts =
                glyphsWithKern
                |> Array.map (fun (glyphId, kern) ->
                    if kern = 0 then
                        sprintf "<%04X>" glyphId
                    else
                        sprintf "<%04X> %d" glyphId kern)
            "[" + String.concat " " parts + "] TJ"
        | DrawXObject name -> $"/{name} Do"
        | SetFillAlpha a ->
            // Use a named graphics state - the name encodes the alpha value
            let gsName = sprintf "GS_ca_%d" (int (a * 1000.0))
            $"/{gsName} gs"
        | SetStrokeAlpha a ->
            let gsName = sprintf "GS_CA_%d" (int (a * 1000.0))
            $"/{gsName} gs"

    let render (ops: DrawOp list) =
        ops |> List.map toOperator |> String.concat "\n"

    /// Builder for drawing operations
    type DrawBuilder() =
        member _.Yield(_) = []
        member _.Zero() = []
        member _.Combine(a, b) = a @ b
        member _.Delay(f) = f()
        member _.For(items, body) = items |> Seq.collect body |> List.ofSeq

        // Include a list of DrawOps
        [<CustomOperation("ops")>]
        member _.Ops(current, toAdd: DrawOp list) = current @ toAdd

        // Graphics state
        [<CustomOperation("save")>]
        member _.Save(ops) = ops @ [SaveState]

        [<CustomOperation("restore")>]
        member _.Restore(ops) = ops @ [RestoreState]

        [<CustomOperation("fillColor")>]
        member _.FillColor(ops, c) = ops @ [SetFillColor c]

        [<CustomOperation("strokeColor")>]
        member _.StrokeColor(ops, c) = ops @ [SetStrokeColor c]

        [<CustomOperation("lineWidth")>]
        member _.LineWidth(ops, w) = ops @ [SetLineWidth w]

        [<CustomOperation("lineCap")>]
        member _.LineCap(ops, c) = ops @ [SetLineCap c]

        [<CustomOperation("lineJoin")>]
        member _.LineJoin(ops, j) = ops @ [SetLineJoin j]

        [<CustomOperation("dash")>]
        member _.Dash(ops, pattern, phase) = ops @ [SetDash (pattern, phase)]

        [<CustomOperation("solidLine")>]
        member _.SolidLine(ops) = ops @ [SetDash ([], 0.0)]

        // Transforms
        [<CustomOperation("translate")>]
        member _.Translate(ops, x, y) = ops @ [Transform (1.0, 0.0, 0.0, 1.0, x, y)]

        [<CustomOperation("scale")>]
        member _.Scale(ops, sx, sy) = ops @ [Transform (sx, 0.0, 0.0, sy, 0.0, 0.0)]

        [<CustomOperation("rotate")>]
        member _.Rotate(ops, degrees) =
            let r = degrees * System.Math.PI / 180.0
            let c, s = cos r, sin r
            ops @ [Transform (c, s, -s, c, 0.0, 0.0)]

        [<CustomOperation("matrix")>]
        member _.Matrix(ops, a, b, c, d, e, f) = ops @ [Transform (a, b, c, d, e, f)]

        // Path construction
        [<CustomOperation("moveTo")>]
        member _.MoveTo(ops, x, y) = ops @ [MoveTo (x, y)]

        [<CustomOperation("lineTo")>]
        member _.LineTo(ops, x, y) = ops @ [LineTo (x, y)]

        [<CustomOperation("curveTo")>]
        member _.CurveTo(ops, x1, y1, x2, y2, x3, y3) =
            ops @ [CurveTo (x1, y1, x2, y2, x3, y3)]

        [<CustomOperation("rect")>]
        member _.Rect(ops, x, y, w, h) = ops @ [Rect (x, y, w, h)]

        [<CustomOperation("circle")>]
        member _.Circle(ops, cx, cy, r) =
            // Approximate circle with 4 Bezier curves
            let k = 0.5522847498  // magic number for circle approximation
            let kr = k * r
            ops @ [
                MoveTo (cx + r, cy)
                CurveTo (cx + r, cy + kr, cx + kr, cy + r, cx, cy + r)
                CurveTo (cx - kr, cy + r, cx - r, cy + kr, cx - r, cy)
                CurveTo (cx - r, cy - kr, cx - kr, cy - r, cx, cy - r)
                CurveTo (cx + kr, cy - r, cx + r, cy - kr, cx + r, cy)
                ClosePath
            ]

        [<CustomOperation("closePath")>]
        member _.ClosePath(ops) = ops @ [ClosePath]

        // Painting
        [<CustomOperation("stroke")>]
        member _.Stroke(ops) = ops @ [Stroke]

        [<CustomOperation("fill")>]
        member _.Fill(ops) = ops @ [Fill]

        [<CustomOperation("fillStroke")>]
        member _.FillStroke(ops) = ops @ [FillAndStroke]

        [<CustomOperation("clip")>]
        member _.Clip(ops) = ops @ [ClipPath; EndPath]

        [<CustomOperation("endPath")>]
        member _.EndPath(ops) = ops @ [EndPath]

        // Text
        [<CustomOperation("text")>]
        member _.Text(ops, fontName, fontSize, x, y, str) =
            ops @ [
                BeginText
                SetFont (fontName, fontSize)
                TextMatrix (1.0, 0.0, 0.0, 1.0, x, y)
                ShowText str
                EndText
            ]

        [<CustomOperation("textBlock")>]
        member _.TextBlock(ops, fontName, fontSize, x, y, leading, lines: string list) =
            ops @ [
                BeginText
                SetFont (fontName, fontSize)
                TextLeading leading
                TextMatrix (1.0, 0.0, 0.0, 1.0, x, y)
            ] @ (lines |> List.mapi (fun i s ->
                if i = 0 then ShowText s else ShowTextNextLine s
            )) @ [EndText]

        // XObjects (images)
        [<CustomOperation("image")>]
        member _.Image(ops, name, x, y, width, height) =
            ops @ [
                SaveState
                Transform (width, 0.0, 0.0, height, x, y)
                DrawXObject name
                RestoreState
            ]

        // Convenience shapes
        [<CustomOperation("line")>]
        member _.Line(ops, x1, y1, x2, y2) =
            ops @ [MoveTo (x1, y1); LineTo (x2, y2); Stroke]

        [<CustomOperation("filledRect")>]
        member _.FilledRect(ops, x, y, w, h) =
            ops @ [Rect (x, y, w, h); Fill]

        [<CustomOperation("strokedRect")>]
        member _.StrokedRect(ops, x, y, w, h) =
            ops @ [Rect (x, y, w, h); Stroke]

        [<CustomOperation("filledCircle")>]
        member _.FilledCircle(ops, cx, cy, r) =
            let k = 0.5522847498
            let kr = k * r
            ops @ [
                MoveTo (cx + r, cy)
                CurveTo (cx + r, cy + kr, cx + kr, cy + r, cx, cy + r)
                CurveTo (cx - kr, cy + r, cx - r, cy + kr, cx - r, cy)
                CurveTo (cx - r, cy - kr, cx - kr, cy - r, cx, cy - r)
                CurveTo (cx + kr, cy - r, cx + r, cy - kr, cx + r, cy)
                Fill
            ]

        [<CustomOperation("raw")>]
        member _.RawOp(ops, s) = ops @ [Raw s]

    let draw = DrawBuilder()

/// Image handling for PDF embedding
module Image =
    type ImageInfo = {
        Width: int
        Height: int
        ColorSpace: string
        BitsPerComponent: int
        Data: byte[]
        Filter: string option
    }

    let private readBigEndianUInt16 (data: byte[]) offset =
        (int data.[offset] <<< 8) ||| int data.[offset + 1]

    let private readBigEndianUInt32 (data: byte[]) offset =
        (int data.[offset] <<< 24) ||| (int data.[offset + 1] <<< 16) |||
        (int data.[offset + 2] <<< 8) ||| int data.[offset + 3]

    /// Parse JPEG and return image info (keeps JPEG data as-is with DCTDecode)
    let fromJpeg (data: byte[]) =
        if data.Length < 2 || data.[0] <> 0xFFuy || data.[1] <> 0xD8uy then
            Error "Not a JPEG file"
        else
            // Find SOF0 marker for dimensions
            let rec findSOF i =
                if i >= data.Length - 1 then None
                elif data.[i] = 0xFFuy then
                    let marker = data.[i + 1]
                    if marker >= 0xC0uy && marker <= 0xC3uy then
                        // SOF marker found
                        let height = readBigEndianUInt16 data (i + 5)
                        let width = readBigEndianUInt16 data (i + 7)
                        let components = int data.[i + 9]
                        Some (width, height, components)
                    elif marker = 0xD9uy then None  // EOI
                    elif marker = 0xD8uy || marker = 0x01uy ||
                         (marker >= 0xD0uy && marker <= 0xD7uy) then
                        findSOF (i + 2)
                    else
                        let len = readBigEndianUInt16 data (i + 2)
                        findSOF (i + 2 + len)
                else findSOF (i + 1)

            match findSOF 0 with
            | Some (w, h, comp) ->
                let colorSpace = if comp = 1 then "DeviceGray"
                                 elif comp = 3 then "DeviceRGB"
                                 else "DeviceCMYK"
                Ok {
                    Width = w
                    Height = h
                    ColorSpace = colorSpace
                    BitsPerComponent = 8
                    Data = data
                    Filter = Some "DCTDecode"
                }
            | None -> Error "Could not find JPEG dimensions"

    /// Parse PNG and extract raw pixel data for PDF embedding (FlateDecode)
    let fromPng (data: byte[]) =
#if FABLE_COMPILER
        Error "PNG decoding is not available in Fable; provide pre-encoded image streams or use JPEG."
#else
        // PNG signature: 89 50 4E 47 0D 0A 1A 0A
        let pngSig = [|0x89uy; 0x50uy; 0x4Euy; 0x47uy; 0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy|]
        if data.Length < 8 || data.[0..7] <> pngSig then
            Error "Not a PNG file"
        else
            let readU32 offset = readBigEndianUInt32 data offset
            // IHDR is always first chunk after signature (at offset 8)
            let width = readU32 16
            let height = readU32 20
            let bitDepth = int data.[24]
            let colorType = int data.[25]

            // Collect IDAT chunks
            let rec collectIDAT pos acc =
                if pos >= data.Length - 12 then acc
                else
                    let chunkLen = readU32 pos
                    let chunkType = bytesToAsciiRange data (pos + 4) 4
                    let nextPos = pos + 12 + chunkLen
                    if chunkType = "IDAT" then
                        collectIDAT nextPos (acc @ [data.[pos+8 .. pos+7+chunkLen]])
                    elif chunkType = "IEND" then acc
                    else collectIDAT nextPos acc

            let idatChunks = collectIDAT 8 []
            let idatData = Array.concat idatChunks

            // Decompress zlib data
            use ms = new MemoryStream(idatData)
            ms.ReadByte() |> ignore  // CMF
            ms.ReadByte() |> ignore  // FLG
            use deflate = new DeflateStream(ms, CompressionMode.Decompress)
            use output = new MemoryStream()
            deflate.CopyTo(output)
            let rawWithFilters = output.ToArray()

            // Determine bytes per pixel and row
            let components = match colorType with
                             | 0 -> 1  // Grayscale
                             | 2 -> 3  // RGB
                             | 3 -> 1  // Indexed (palette)
                             | 4 -> 2  // Grayscale + Alpha
                             | 6 -> 4  // RGBA
                             | _ -> 3
            let bpp = components * (bitDepth / 8)
            let rowBytes = width * bpp
            let scanlineBytes = rowBytes + 1  // +1 for filter byte

            // Decode PNG filters and extract raw pixels
            let pixelData = Array.zeroCreate (height * rowBytes)
            let prevRow = Array.zeroCreate rowBytes
            for row in 0 .. height - 1 do
                let filterType = rawWithFilters.[row * scanlineBytes]
                let srcOffset = row * scanlineBytes + 1
                let dstOffset = row * rowBytes
                for x in 0 .. rowBytes - 1 do
                    let raw = rawWithFilters.[srcOffset + x]
                    let a = if x >= bpp then pixelData.[dstOffset + x - bpp] else 0uy
                    let b = prevRow.[x]
                    let c = if x >= bpp then prevRow.[x - bpp] else 0uy
                    let decoded =
                        match filterType with
                        | 0uy -> raw  // None
                        | 1uy -> raw + a  // Sub
                        | 2uy -> raw + b  // Up
                        | 3uy -> raw + byte ((int a + int b) / 2)  // Average
                        | 4uy ->  // Paeth
                            let p = int a + int b - int c
                            let pa = abs (p - int a)
                            let pb = abs (p - int b)
                            let pc = abs (p - int c)
                            raw + (if pa <= pb && pa <= pc then a elif pb <= pc then b else c)
                        | _ -> raw
                    pixelData.[dstOffset + x] <- decoded
                Array.Copy(pixelData, dstOffset, prevRow, 0, rowBytes)

            // Handle alpha: for RGBA, convert to RGB (drop alpha) or keep if needed
            // For signatures on white background, we can composite to white
            let finalData, finalColorSpace, finalComponents =
                if colorType = 6 then  // RGBA -> RGB, composite on white
                    let rgbData = Array.zeroCreate (height * width * 3)
                    for i in 0 .. (height * width) - 1 do
                        let r = float pixelData.[i * 4]
                        let g = float pixelData.[i * 4 + 1]
                        let b = float pixelData.[i * 4 + 2]
                        let a = float pixelData.[i * 4 + 3] / 255.0
                        // Composite on white
                        rgbData.[i * 3] <- byte (r * a + 255.0 * (1.0 - a))
                        rgbData.[i * 3 + 1] <- byte (g * a + 255.0 * (1.0 - a))
                        rgbData.[i * 3 + 2] <- byte (b * a + 255.0 * (1.0 - a))
                    rgbData, "DeviceRGB", 3
                elif colorType = 4 then  // Grayscale + Alpha -> Grayscale
                    let grayData = Array.zeroCreate (height * width)
                    for i in 0 .. (height * width) - 1 do
                        let g = float pixelData.[i * 2]
                        let a = float pixelData.[i * 2 + 1] / 255.0
                        grayData.[i] <- byte (g * a + 255.0 * (1.0 - a))
                    grayData, "DeviceGray", 1
                else
                    let colorSpace = match colorType with
                                     | 0 -> "DeviceGray"
                                     | 2 -> "DeviceRGB"
                                     | _ -> "DeviceRGB"
                    pixelData, colorSpace, components

            // PDF FlateDecode streams are zlib-wrapped deflate streams.
            use compressedMs = new MemoryStream()
            use zlibOut = new ZLibStream(compressedMs, CompressionLevel.Optimal, true)
            zlibOut.Write(finalData, 0, finalData.Length)
            zlibOut.Close()

            Ok {
                Width = width
                Height = height
                ColorSpace = finalColorSpace
                BitsPerComponent = bitDepth
                Data = compressedMs.ToArray()
                Filter = Some "FlateDecode"
            }
#endif

    /// Parse image from base64 data URL (PNG or JPEG)
    let fromDataUrl (dataUrl: string) =
        if dataUrl.StartsWith("data:image/png;base64,") then
            let b64 = dataUrl.Substring(22)
            fromPng (Convert.FromBase64String(b64))
        elif dataUrl.StartsWith("data:image/jpeg;base64,") then
            let b64 = dataUrl.Substring(23)
            fromJpeg (Convert.FromBase64String(b64))
        else
            Error "Unsupported image format"

    /// Create image XObject from ImageInfo
    let toXObject (doc: PdfDocument) (info: ImageInfo) =
        let filterPart =
            match info.Filter with
            | Some f -> [("Filter", PdfName f)]
            | None -> []
        let baseHeader = [
            ("Type", PdfName "XObject")
            ("Subtype", PdfName "Image")
            ("Width", PdfInt (int64 info.Width))
            ("Height", PdfInt (int64 info.Height))
            ("ColorSpace", PdfName info.ColorSpace)
            ("BitsPerComponent", PdfInt (int64 info.BitsPerComponent))
        ]
        let header = baseHeader @ filterPart
        let headerDict = header |> Map.ofList
        Write.newIndirectObj doc (PdfStream (headerDict, info.Data))

/// Font handling for PDF documents
module Font =
    open System
    open System.IO
    open System.Text

    /// WinAnsi encoding: maps Rbt.Unicode code points to WinAnsi bytes
    /// Covers Latin-1 supplement and some extended Latin characters
    let private winAnsiMap =
        // Characters 0x80-0x9F in WinAnsi are different from Rbt.Unicode
        [
            0x20AC, 0x80  // Euro sign
            0x201A, 0x82  // Single low-9 quotation mark
            0x0192, 0x83  // Latin small letter f with hook
            0x201E, 0x84  // Double low-9 quotation mark
            0x2026, 0x85  // Horizontal ellipsis
            0x2020, 0x86  // Dagger
            0x2021, 0x87  // Double dagger
            0x02C6, 0x88  // Modifier letter circumflex accent
            0x2030, 0x89  // Per mille sign
            0x0160, 0x8A  // Latin capital letter S with caron
            0x2039, 0x8B  // Single left-pointing angle quotation mark
            0x0152, 0x8C  // Latin capital ligature OE
            0x017D, 0x8E  // Latin capital letter Z with caron
            0x2018, 0x91  // Left single quotation mark
            0x2019, 0x92  // Right single quotation mark
            0x201C, 0x93  // Left double quotation mark
            0x201D, 0x94  // Right double quotation mark
            0x2022, 0x95  // Bullet
            0x2013, 0x96  // En dash
            0x2014, 0x97  // Em dash
            0x02DC, 0x98  // Small tilde
            0x2122, 0x99  // Trade mark sign
            0x0161, 0x9A  // Latin small letter s with caron
            0x203A, 0x9B  // Single right-pointing angle quotation mark
            0x0153, 0x9C  // Latin small ligature oe
            0x017E, 0x9E  // Latin small letter z with caron
            0x0178, 0x9F  // Latin capital letter Y with diaeresis
        ] |> Map.ofList

    /// Encode a Rbt.Unicode string to WinAnsi bytes for standard PDF fonts
    let toWinAnsi (s: string) : byte[] =
        s |> Seq.map (fun c ->
            let code = int c
            if code < 0x80 then
                byte code  // ASCII passthrough
            elif code >= 0xA0 && code <= 0xFF then
                byte code  // Latin-1 supplement maps directly
            else
                match winAnsiMap.TryFind code with
                | Some b -> byte b
                | None -> byte '?'  // Unmappable character
        ) |> Array.ofSeq

    /// Escape a byte array for PDF literal string
    let escapeBytes (bytes: byte[]) : string =
        let sb = StringBuilder()
        for b in bytes do
            match char b with
            | '\\' -> sb.Append("\\\\") |> ignore
            | '(' -> sb.Append("\\(") |> ignore
            | ')' -> sb.Append("\\)") |> ignore
            | c when b < 32uy || b > 126uy ->
                sb.Append(sprintf "\\%03o" b) |> ignore
            | c -> sb.Append(c) |> ignore
        sb.ToString()

    /// TrueType font table entry
    type TableEntry = {
        Tag: string
        Checksum: uint32
        Offset: uint32
        Length: uint32
    }

    /// Parsed TrueType font data
    type TrueTypeFont = {
        Data: byte[]
        Tables: Map<string, TableEntry>
        UnitsPerEm: int
        Ascender: int
        Descender: int
        NumGlyphs: int
        GlyphWidths: int[]  // indexed by glyph ID
        Cmap: Map<int, int>  // Rbt.Unicode code point to glyph ID
        /// Kerning pairs: (leftGlyphId, rightGlyphId) -> x adjustment in font units
        KerningPairs: Map<int * int, int>
    }

    let private readUInt16BE (data: byte[]) offset =
        (uint16 data.[offset] <<< 8) ||| uint16 data.[offset + 1]

    let private readInt16BE (data: byte[]) offset =
        int16 ((uint16 data.[offset] <<< 8) ||| uint16 data.[offset + 1])

    let private readUInt32BE (data: byte[]) offset =
        (uint32 data.[offset] <<< 24) |||
        (uint32 data.[offset + 1] <<< 16) |||
        (uint32 data.[offset + 2] <<< 8) |||
        uint32 data.[offset + 3]

    /// Parse GPOS table to extract kerning pairs
    /// Returns Map of (leftGlyphId, rightGlyphId) -> x adjustment
    let private parseGposKerning (data: byte[]) (tables: Map<string, TableEntry>) : Map<int * int, int> =
        match tables.TryFind "GPOS" with
        | None -> Map.empty
        | Some gposEntry ->
            try
                let gposOffset = int gposEntry.Offset
                let majorVersion = readUInt16BE data gposOffset
                let minorVersion = readUInt16BE data (gposOffset + 2)
                if majorVersion <> 1us then Map.empty
                else
                    // Read header offsets
                    let scriptListOffset = gposOffset + int (readUInt16BE data (gposOffset + 4))
                    let featureListOffset = gposOffset + int (readUInt16BE data (gposOffset + 6))
                    let lookupListOffset = gposOffset + int (readUInt16BE data (gposOffset + 8))

                    // Parse lookup list
                    let lookupCount = int (readUInt16BE data lookupListOffset)
                    let kerningPairs = ResizeArray<(int * int) * int>()

                    for lookupIdx in 0 .. lookupCount - 1 do
                        let lookupOffset = lookupListOffset + int (readUInt16BE data (lookupListOffset + 2 + lookupIdx * 2))
                        let lookupType = readUInt16BE data lookupOffset
                        let lookupFlag = readUInt16BE data (lookupOffset + 2)
                        let subtableCount = int (readUInt16BE data (lookupOffset + 4))

                        // Type 2 = Pair Adjustment (kerning)
                        if lookupType = 2us then
                            for subtableIdx in 0 .. subtableCount - 1 do
                                let subtableOffset = lookupOffset + int (readUInt16BE data (lookupOffset + 6 + subtableIdx * 2))
                                let posFormat = readUInt16BE data subtableOffset

                                if posFormat = 1us then
                                    // Format 1: Individual pairs
                                    let coverageOffset = subtableOffset + int (readUInt16BE data (subtableOffset + 2))
                                    let valueFormat1 = int (readUInt16BE data (subtableOffset + 4))
                                    let valueFormat2 = int (readUInt16BE data (subtableOffset + 6))
                                    let pairSetCount = int (readUInt16BE data (subtableOffset + 8))

                                    // Parse coverage table to get first glyph IDs
                                    let coverageFormat = readUInt16BE data coverageOffset
                                    let firstGlyphs =
                                        if coverageFormat = 1us then
                                            let glyphCount = int (readUInt16BE data (coverageOffset + 2))
                                            [| for i in 0 .. glyphCount - 1 ->
                                                int (readUInt16BE data (coverageOffset + 4 + i * 2)) |]
                                        else
                                            [||]  // Range format not implemented

                                    // Calculate value record sizes
                                    let valueSize1 =
                                        (if valueFormat1 &&& 0x0001 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0002 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0004 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0008 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0010 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0020 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0040 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0080 <> 0 then 2 else 0)
                                    let valueSize2 =
                                        (if valueFormat2 &&& 0x0001 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0002 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0004 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0008 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0010 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0020 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0040 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0080 <> 0 then 2 else 0)

                                    // Parse each pair set
                                    for pairSetIdx in 0 .. min (pairSetCount - 1) (firstGlyphs.Length - 1) do
                                        let pairSetOffset = subtableOffset + int (readUInt16BE data (subtableOffset + 10 + pairSetIdx * 2))
                                        let pairCount = int (readUInt16BE data pairSetOffset)
                                        let firstGlyph = firstGlyphs.[pairSetIdx]
                                        let pairRecordSize = 2 + valueSize1 + valueSize2

                                        for pairIdx in 0 .. pairCount - 1 do
                                            let pairOffset = pairSetOffset + 2 + pairIdx * pairRecordSize
                                            let secondGlyph = int (readUInt16BE data pairOffset)
                                            // XAdvance is the 3rd field if present (after XPlacement, YPlacement)
                                            let xAdvanceOffset =
                                                pairOffset + 2 +
                                                (if valueFormat1 &&& 0x0001 <> 0 then 2 else 0) +  // XPlacement
                                                (if valueFormat1 &&& 0x0002 <> 0 then 2 else 0)   // YPlacement
                                            if valueFormat1 &&& 0x0004 <> 0 then  // Has XAdvance
                                                let xAdvance = int (readInt16BE data xAdvanceOffset)
                                                if xAdvance <> 0 then
                                                    kerningPairs.Add(((firstGlyph, secondGlyph), xAdvance))

                                elif posFormat = 2us then
                                    // Format 2: Class-based pairs
                                    let coverageOffset = subtableOffset + int (readUInt16BE data (subtableOffset + 2))
                                    let valueFormat1 = int (readUInt16BE data (subtableOffset + 4))
                                    let valueFormat2 = int (readUInt16BE data (subtableOffset + 6))
                                    let classDef1Offset = subtableOffset + int (readUInt16BE data (subtableOffset + 8))
                                    let classDef2Offset = subtableOffset + int (readUInt16BE data (subtableOffset + 10))
                                    let class1Count = int (readUInt16BE data (subtableOffset + 12))
                                    let class2Count = int (readUInt16BE data (subtableOffset + 14))

                                    // Parse class definitions (Format 2 - range-based)
                                    let parseClassDef offset =
                                        let format = readUInt16BE data offset
                                        if format = 1us then
                                            let startGlyph = int (readUInt16BE data (offset + 2))
                                            let glyphCount = int (readUInt16BE data (offset + 4))
                                            [| for i in 0 .. glyphCount - 1 ->
                                                (startGlyph + i, int (readUInt16BE data (offset + 6 + i * 2))) |]
                                            |> Map.ofArray
                                        elif format = 2us then
                                            let rangeCount = int (readUInt16BE data (offset + 2))
                                            let result = ResizeArray<int * int>()
                                            for i in 0 .. rangeCount - 1 do
                                                let rangeOffset = offset + 4 + i * 6
                                                let startGlyph = int (readUInt16BE data rangeOffset)
                                                let endGlyph = int (readUInt16BE data (rangeOffset + 2))
                                                let classVal = int (readUInt16BE data (rangeOffset + 4))
                                                for g in startGlyph .. endGlyph do
                                                    result.Add((g, classVal))
                                            result |> Seq.map id |> Map.ofSeq
                                        else Map.empty

                                    let class1Map = parseClassDef classDef1Offset
                                    let class2Map = parseClassDef classDef2Offset

                                    // Calculate value record size
                                    let valueSize1 =
                                        (if valueFormat1 &&& 0x0001 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0002 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0004 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0008 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0010 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0020 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0040 <> 0 then 2 else 0) +
                                        (if valueFormat1 &&& 0x0080 <> 0 then 2 else 0)
                                    let valueSize2 =
                                        (if valueFormat2 &&& 0x0001 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0002 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0004 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0008 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0010 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0020 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0040 <> 0 then 2 else 0) +
                                        (if valueFormat2 &&& 0x0080 <> 0 then 2 else 0)

                                    let class2RecordSize = valueSize1 + valueSize2
                                    let class1RecordSize = class2Count * class2RecordSize

                                    // Build reverse maps: class -> glyph list
                                    let glyphsForClass1 =
                                        class1Map
                                        |> Map.toSeq
                                        |> Seq.groupBy snd
                                        |> Seq.map (fun (cls, pairs) -> cls, pairs |> Seq.map fst |> Array.ofSeq)
                                        |> Map.ofSeq
                                    let glyphsForClass2 =
                                        class2Map
                                        |> Map.toSeq
                                        |> Seq.groupBy snd
                                        |> Seq.map (fun (cls, pairs) -> cls, pairs |> Seq.map fst |> Array.ofSeq)
                                        |> Map.ofSeq

                                    // Read class1/class2 value records
                                    let recordsOffset = subtableOffset + 16
                                    for c1 in 0 .. class1Count - 1 do
                                        for c2 in 0 .. class2Count - 1 do
                                            let recOffset = recordsOffset + c1 * class1RecordSize + c2 * class2RecordSize
                                            // XAdvance offset within value record
                                            let xAdvanceOffset =
                                                recOffset +
                                                (if valueFormat1 &&& 0x0001 <> 0 then 2 else 0) +
                                                (if valueFormat1 &&& 0x0002 <> 0 then 2 else 0)
                                            if valueFormat1 &&& 0x0004 <> 0 then
                                                let xAdvance = int (readInt16BE data xAdvanceOffset)
                                                if xAdvance <> 0 then
                                                    // Expand classes to glyph pairs
                                                    let g1s = glyphsForClass1.TryFind c1 |> Option.defaultValue [||]
                                                    let g2s = glyphsForClass2.TryFind c2 |> Option.defaultValue [||]
                                                    for g1 in g1s do
                                                        for g2 in g2s do
                                                            kerningPairs.Add(((g1, g2), xAdvance))

                    kerningPairs |> Seq.map id |> Map.ofSeq
            with _ -> Map.empty  // Silently ignore parsing errors

    /// Parse a TrueType font file
    let parseTrueType (data: byte[]) : TrueTypeFont =
        let tableDirectoryOffset =
            if data.Length >= 16 && data.[0] = byte 't' && data.[1] = byte 't' && data.[2] = byte 'c' && data.[3] = byte 'f' then
                let numFonts = int (readUInt32BE data 8)
                if numFonts > 0 then int (readUInt32BE data 12) else 0
            else 0

        // Read offset table
        let numTables = int (readUInt16BE data (tableDirectoryOffset + 4))

        // Read table directory
        let tables =
            [0 .. numTables - 1]
            |> List.map (fun i ->
                let offset = tableDirectoryOffset + 12 + i * 16
                let tag = bytesToAsciiRange data offset 4
                let entry = {
                    Tag = tag
                    Checksum = readUInt32BE data (offset + 4)
                    Offset = readUInt32BE data (offset + 8)
                    Length = readUInt32BE data (offset + 12)
                }
                tag, entry)
            |> Map.ofList

        // Read 'head' table for unitsPerEm
        let headOffset = int tables.["head"].Offset
        let unitsPerEm = int (readUInt16BE data (headOffset + 18))

        // Read 'hhea' table for ascender/descender and numOfLongHorMetrics
        let hheaOffset = int tables.["hhea"].Offset
        let ascender = int (readInt16BE data (hheaOffset + 4))
        let descender = int (readInt16BE data (hheaOffset + 6))
        let numOfLongHorMetrics = int (readUInt16BE data (hheaOffset + 34))

        // Read 'maxp' table for numGlyphs
        let maxpOffset = int tables.["maxp"].Offset
        let numGlyphs = int (readUInt16BE data (maxpOffset + 4))

        // Read 'hmtx' table for glyph widths
        let hmtxOffset = int tables.["hmtx"].Offset
        let glyphWidths = Array.zeroCreate numGlyphs
        for i in 0 .. numOfLongHorMetrics - 1 do
            glyphWidths.[i] <- int (readUInt16BE data (hmtxOffset + i * 4))
        // Remaining glyphs use the last width
        let lastWidth = if numOfLongHorMetrics > 0 then glyphWidths.[numOfLongHorMetrics - 1] else 0
        for i in numOfLongHorMetrics .. numGlyphs - 1 do
            glyphWidths.[i] <- lastWidth

        // Parse 'cmap' table for Rbt.Unicode mapping
        let cmapFormat4 =
            match tables.TryFind "cmap" with
            | None -> None
            | Some cmapEntry ->
                let cmapOffset = int cmapEntry.Offset
                let numSubtables = int (readUInt16BE data (cmapOffset + 2))
                // Find Rbt.Unicode subtable (platform 3, encoding 1 or platform 0)
                let mutable unicodeOffset = None
                for i in 0 .. numSubtables - 1 do
                    let subtableOffset = cmapOffset + 4 + i * 8
                    let platformID = readUInt16BE data subtableOffset
                    let encodingID = readUInt16BE data (subtableOffset + 2)
                    let offset = readUInt32BE data (subtableOffset + 4)
                    if (platformID = 3us && encodingID = 1us) || platformID = 0us then
                        unicodeOffset <- Some (cmapOffset + int offset)

                // Also look for format 12 subtable (platform 3, encoding 10 for full Rbt.Unicode)
                let mutable format12Offset = None
                for i in 0 .. numSubtables - 1 do
                    let subtableOffset = cmapOffset + 4 + i * 8
                    let platformID = readUInt16BE data subtableOffset
                    let encodingID = readUInt16BE data (subtableOffset + 2)
                    let offset = readUInt32BE data (subtableOffset + 4)
                    if platformID = 3us && encodingID = 10us then
                        format12Offset <- Some (cmapOffset + int offset)

                // Prefer format 12 over format 4 for full Rbt.Unicode coverage
                match format12Offset with
                | Some offset ->
                    let format = readUInt16BE data offset
                    if format = 12us then
                        let numGroups = int (readUInt32BE data (offset + 12))
                        let mapping = ResizeArray<int * int>()
                        for i in 0 .. numGroups - 1 do
                            let groupOffset = offset + 16 + i * 12
                            let startCode = int (readUInt32BE data groupOffset)
                            let endCode = int (readUInt32BE data (groupOffset + 4))
                            let startGlyphId = int (readUInt32BE data (groupOffset + 8))
                            for code in startCode .. endCode do
                                let glyphId = startGlyphId + (code - startCode)
                                if glyphId <> 0 then
                                    mapping.Add((code, glyphId))
                        Some (mapping.ToArray())
                    else None
                | None ->
                    match unicodeOffset with
                    | None -> None
                    | Some offset ->
                        let format = readUInt16BE data offset
                        if format <> 4us then None
                        else
                            let segCount = int (readUInt16BE data (offset + 6)) / 2
                            let endCodes = [| for i in 0 .. segCount - 1 -> int (readUInt16BE data (offset + 14 + i * 2)) |]
                            let startCodes = [| for i in 0 .. segCount - 1 -> int (readUInt16BE data (offset + 16 + segCount * 2 + i * 2)) |]
                            let idDeltas = [| for i in 0 .. segCount - 1 -> int (readInt16BE data (offset + 16 + segCount * 4 + i * 2)) |]
                            let idRangeOffsetBase = offset + 16 + segCount * 6

                            // Build Rbt.Unicode -> GlyphID mapping
                            let mapping = ResizeArray<int * int>()
                            for seg in 0 .. segCount - 1 do
                                let idRangeOffset = int (readUInt16BE data (idRangeOffsetBase + seg * 2))
                                for code in startCodes.[seg] .. endCodes.[seg] do
                                    if code <> 0xFFFF then
                                        let glyphId =
                                            if idRangeOffset = 0 then
                                                (code + idDeltas.[seg]) &&& 0xFFFF
                                            else
                                                let glyphIndexAddr = idRangeOffsetBase + seg * 2 + idRangeOffset + (code - startCodes.[seg]) * 2
                                                let glyphIndex = int (readUInt16BE data glyphIndexAddr)
                                                if glyphIndex = 0 then 0
                                                else (glyphIndex + idDeltas.[seg]) &&& 0xFFFF
                                        if glyphId <> 0 then
                                            mapping.Add((code, glyphId))
                            Some (mapping.ToArray())

        // Convert to Map for fast lookup
        let cmapMap =
            match cmapFormat4 with
            | Some arr -> arr |> Array.fold (fun m (cp, gid) -> Map.add cp gid m) Map.empty
            | None -> Map.empty

        // Parse GPOS kerning pairs
        let kerningPairs = parseGposKerning data tables

        {
            Data = data
            Tables = tables
            UnitsPerEm = unitsPerEm
            Ascender = ascender
            Descender = descender
            NumGlyphs = numGlyphs
            GlyphWidths = glyphWidths
            Cmap = cmapMap
            KerningPairs = kerningPairs
        }

    /// Load a TrueType font from file
    let loadTrueType (path: string) : TrueTypeFont =
#if FABLE_COMPILER
        failwith "Font.loadTrueType is not available in Fable; use parseTrueType with caller-provided bytes."
#else
        let data = File.ReadAllBytes(path)
        parseTrueType data
#endif

    /// Get glyph ID for a Rbt.Unicode code point
    let getGlyphId (font: TrueTypeFont) (codePoint: int) : int =
        font.Cmap.TryFind codePoint |> Option.defaultValue 0

    /// Encode text as hex string using glyph IDs (for embedded fonts)
    let toGlyphHex (font: TrueTypeFont) (text: string) : string =
        text
        |> Seq.map (fun c -> getGlyphId font (int c))
        |> Seq.map (sprintf "%04X")
        |> String.concat ""

    /// Get text width in font units (without kerning)
    let getTextWidth (font: TrueTypeFont) (text: string) : int =
        text
        |> Seq.sumBy (fun c ->
            let gid = getGlyphId font (int c)
            if gid < font.GlyphWidths.Length then font.GlyphWidths.[gid] else 0)

    /// Get kerning adjustment for a glyph pair in font units
    let getKerning (font: TrueTypeFont) (leftGlyph: int) (rightGlyph: int) : int =
        font.KerningPairs.TryFind (leftGlyph, rightGlyph)
        |> Option.defaultValue 0

    /// Get glyph advances with kerning applied
    /// Returns array of (glyphId, advance) for each character
    let getGlyphAdvances (font: TrueTypeFont) (text: string) : (int * int) array =
        if text.Length = 0 then [||]
        else
            let glyphs = text |> Seq.map (fun c -> getGlyphId font (int c)) |> Array.ofSeq
            glyphs
            |> Array.mapi (fun i gid ->
                let baseWidth =
                    if gid < font.GlyphWidths.Length then font.GlyphWidths.[gid] else 0
                let kern =
                    if i < glyphs.Length - 1 then
                        getKerning font gid glyphs.[i + 1]
                    else 0
                (gid, baseWidth + kern))

    /// Get text width in font units with kerning
    let getTextWidthWithKerning (font: TrueTypeFont) (text: string) : int =
        getGlyphAdvances font text
        |> Array.sumBy snd

    let private utf16Hex (text: string) =
        let sb = StringBuilder(text.Length * 4)
        for ch in text do
            sb.AppendFormat("{0:X4}", int ch) |> ignore
        sb.ToString()

    /// Generate a ToUnicode CMap for text extraction
    let generateToUnicodeCMapText (usedGlyphs: (int * string) seq) : string =
        let entries = usedGlyphs |> Seq.toArray
        let sb = StringBuilder()
        sb.AppendLine("/CIDInit /ProcSet findresource begin") |> ignore
        sb.AppendLine("12 dict begin") |> ignore
        sb.AppendLine("begincmap") |> ignore
        sb.AppendLine("/CIDSystemInfo <<") |> ignore
        sb.AppendLine("  /Registry (Adobe)") |> ignore
        sb.AppendLine("  /Ordering (UCS)") |> ignore
        sb.AppendLine("  /Supplement 0") |> ignore
        sb.AppendLine(">> def") |> ignore
        sb.AppendLine("/CMapName /Adobe-Identity-UCS def") |> ignore
        sb.AppendLine("/CMapType 2 def") |> ignore
        sb.AppendLine("1 begincodespacerange") |> ignore
        sb.AppendLine("<0000> <FFFF>") |> ignore
        sb.AppendLine("endcodespacerange") |> ignore

        // Write mappings in chunks of 100
        let chunks = entries |> Array.chunkBySize 100
        for chunk in chunks do
            sb.AppendLine($"{chunk.Length} beginbfchar") |> ignore
            for (glyphId, unicodeText) in chunk do
                sb.AppendLine(sprintf "<%04X> <%s>" glyphId (utf16Hex unicodeText)) |> ignore
            sb.AppendLine("endbfchar") |> ignore

        sb.AppendLine("endcmap") |> ignore
        sb.AppendLine("CMapName currentdict /CMap defineresource pop") |> ignore
        sb.AppendLine("end") |> ignore
        sb.AppendLine("end") |> ignore
        sb.ToString()

    /// Generate a ToUnicode CMap for single-codepoint mappings.
    let generateToUnicodeCMap (usedGlyphs: (int * int) seq) : string =
        usedGlyphs
        |> Seq.map (fun (gid, unicode) -> gid, string (char unicode))
        |> generateToUnicodeCMapText

    /// Create a font descriptor dictionary
    let createFontDescriptor (doc: PdfDocument) (font: TrueTypeFont) (fontName: string) (fontFileRef: PdfObj) =
        let scale = 1000.0 / float font.UnitsPerEm
        let ascent = int (float font.Ascender * scale)
        let descent = int (float font.Descender * scale)
        let flags = 4  // Symbolic (we're using glyph IDs)

        let dict = Map.ofList [
            ("Type", PdfName "FontDescriptor")
            ("FontName", PdfName fontName)
            ("Flags", PdfInt (int64 flags))
            ("FontBBox", PdfArray [PdfInt 0L; PdfInt (int64 descent); PdfInt 1000L; PdfInt (int64 ascent)])
            ("ItalicAngle", PdfInt 0L)
            ("Ascent", PdfInt (int64 ascent))
            ("Descent", PdfInt (int64 descent))
            ("CapHeight", PdfInt (int64 ascent))
            ("StemV", PdfInt 80L)
            ("FontFile2", fontFileRef)
        ]
        Write.newIndirectObj doc (PdfDict dict)

    /// Embed a TrueType font in the document
    let embedTrueTypeWithToUnicode (doc: PdfDocument) (font: TrueTypeFont) (fontName: string) (customToUnicode: Map<int, string>) : PdfObj =
        // Create the font file stream
        let fontFileDict = Map.ofList [
            ("Length1", PdfInt (int64 font.Data.Length))
        ]
        let fontFileRef = Write.newIndirectObj doc (PdfStream (fontFileDict, font.Data))

        // Create font descriptor
        let fontDescRef = createFontDescriptor doc font fontName fontFileRef

        // Calculate widths array for ALL glyphs (needed for ligatures with high glyph IDs)
        let scale = 1000.0 / float font.UnitsPerEm
        let widths = [|
            for i in 0 .. font.NumGlyphs - 1 ->
                PdfInt (int64 (float font.GlyphWidths.[i] * scale))
        |]

        // Create CIDFont dictionary
        let cidFontDict = Map.ofList [
            ("Type", PdfName "Font")
            ("Subtype", PdfName "CIDFontType2")
            ("BaseFont", PdfName fontName)
            ("CIDSystemInfo", PdfDict (Map.ofList [
                ("Registry", PdfString (asciiBytes "Adobe"))
                ("Ordering", PdfString (asciiBytes "Identity"))
                ("Supplement", PdfInt 0L)
            ]))
            ("FontDescriptor", fontDescRef)
            ("DW", PdfInt 1000L)
            ("W", PdfArray [PdfInt 0L; PdfArray (Array.toList widths)])
        ]
        let cidFontRef = Write.newIndirectObj doc (PdfDict cidFontDict)

        // Generate ToUnicode CMap for text extraction
        // Invert the Cmap (Rbt.Unicode -> GlyphID) to get (GlyphID -> Rbt.Unicode)
        let glyphToUnicode =
            font.Cmap
            |> Map.toSeq
            |> Seq.map (fun (unicode, glyphId) -> (glyphId, unicode))
            |> Seq.map (fun (glyphId, unicode) -> glyphId, string (char unicode))
            |> Map.ofSeq
            |> fun cmap -> Map.fold (fun acc gid text -> Map.add gid text acc) cmap customToUnicode
            |> Map.toSeq
        let toUnicodeCMap = generateToUnicodeCMapText glyphToUnicode
        let toUnicodeCMapBytes = asciiBytes toUnicodeCMap
        let toUnicodeRef = Write.newIndirectObj doc (PdfStream (Map.empty, toUnicodeCMapBytes))

        // Create the Type0 font with ToUnicode CMap
        let fontDict = Map.ofList [
            ("Type", PdfName "Font")
            ("Subtype", PdfName "Type0")
            ("BaseFont", PdfName fontName)
            ("Encoding", PdfName "Identity-H")
            ("DescendantFonts", PdfArray [cidFontRef])
            ("ToUnicode", toUnicodeRef)
        ]
        Write.newIndirectObj doc (PdfDict fontDict)

    /// Embed a TrueType font in the document
    let embedTrueType (doc: PdfDocument) (font: TrueTypeFont) (fontName: string) : PdfObj =
        embedTrueTypeWithToUnicode doc font fontName Map.empty

/// High-level document builder
module Doc =
    open Color
    open Draw

    type FontRef = { Name: string; ObjRef: PdfObj }

    /// Embedded TrueType font with glyph mapping
    type EmbeddedFontRef = {
        Name: string
        ObjRef: PdfObj
        Font: Font.TrueTypeFont
    }

    /// Standard PDF fonts (no embedding needed)
    let standardFonts = [
        "Helvetica"; "Helvetica-Bold"; "Helvetica-Oblique"; "Helvetica-BoldOblique"
        "Times-Roman"; "Times-Bold"; "Times-Italic"; "Times-BoldItalic"
        "Courier"; "Courier-Bold"; "Courier-Oblique"; "Courier-BoldOblique"
        "Symbol"; "ZapfDingbats"
    ]

    type DocBuilder = {
        Doc: PdfDocument
        mutable Fonts: Map<string, FontRef>
        mutable EmbeddedFonts: Map<string, EmbeddedFontRef>
        mutable Images: Map<string, PdfObj>
        mutable Pages: PdfObj list
    }

    let create () =
        {
            Doc = Write.createNew ()
            Fonts = Map.empty
            EmbeddedFonts = Map.empty
            Images = Map.empty
            Pages = []
        }

    let private ensureFont (builder: DocBuilder) (fontFamily: string) =
        match Map.tryFind fontFamily builder.Fonts with
        | Some f -> f
        | None ->
            let fontObj = PdfDict (Map.ofList [
                ("Type", PdfName "Font")
                ("Subtype", PdfName "Type1")
                ("BaseFont", PdfName fontFamily)
                ("Encoding", PdfName "WinAnsiEncoding")
            ])
            let fontRef = Write.newIndirectObj builder.Doc fontObj
            let name = $"F{Map.count builder.Fonts}"
            let f = { Name = name; ObjRef = fontRef }
            builder.Fonts <- Map.add fontFamily f builder.Fonts
            f

    /// Load and embed a TrueType font from file
    let loadFont (builder: DocBuilder) (fontName: string) (path: string) =
        let ttf = Font.loadTrueType path
        let fontRef = Font.embedTrueType builder.Doc ttf fontName
        let resourceName = $"F{Map.count builder.Fonts + Map.count builder.EmbeddedFonts}"
        let f = { Name = resourceName; ObjRef = fontRef; Font = ttf }
        builder.EmbeddedFonts <- Map.add fontName f builder.EmbeddedFonts
        fontName

    /// Load and embed a TrueType font from bytes
    let loadFontBytes (builder: DocBuilder) (fontName: string) (data: byte[]) =
        let ttf = Font.parseTrueType data
        let fontRef = Font.embedTrueType builder.Doc ttf fontName
        let resourceName = $"F{Map.count builder.Fonts + Map.count builder.EmbeddedFonts}"
        let f = { Name = resourceName; ObjRef = fontRef; Font = ttf }
        builder.EmbeddedFonts <- Map.add fontName f builder.EmbeddedFonts
        fontName

    /// Subset-prefix is a six-uppercase-letter PostScript convention that
    /// signals "this BaseFont is a subset of the named font". Some readers
    /// (Acrobat in particular) refuse to merge subset fonts that share a
    /// BaseFont — the prefix avoids that. We derive it deterministically
    /// from the used-glyph set so the same subset gets the same prefix
    /// across runs.
    let private subsetPrefix (usedGids: Set<int>) : string =
        let mutable h = 2166136261u
        for gid in usedGids do
            for ch in string gid do
                h <- (h ^^^ uint32 (int ch)) * 16777619u
            h <- (h ^^^ uint32 (int ',')) * 16777619u
        let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        System.String([|
            for i in 0 .. 5 do
                h <- (h ^^^ (h >>> 13)) * 16777619u
                chars.[int (h % 26u)]
        |])

    /// Load and embed a *compacted, subsetted* TrueType font. The subsetter
    /// renumbers glyphs to 0..N-1 (where N is the closure of used glyphs +
    /// composite refs + .notdef), rebuilds glyf/loca/cmap/hmtx, drops
    /// layout/variable-font/Apple AAT tables, and downgrades `post` to v3.
    ///
    /// Returns a `Map<int, int>` mapping original glyph IDs → new glyph IDs.
    /// Callers must apply this map to any content-stream glyph references
    /// (ShowTextHex, ShowTextHexWithKern) before encoding pages — since the
    /// font in the PDF only knows the new numbering.
    ///
    /// If `usedGids` is empty, the font is full-embedded with an identity
    /// remap (defensive — the font was loaded but never drawn).
    let loadFontBytesSubset
        (builder: DocBuilder) (fontName: string) (data: byte[]) (usedGids: Set<int>)
        : Map<int, int> =
        if Set.isEmpty usedGids then
            loadFontBytes builder fontName data |> ignore
            Map.empty
        else
            let subsetBytes, remap = Rbt.OpenType.Subset.subsetCompact data usedGids
            let ttf = Font.parseTrueType subsetBytes
            // Rename the internal font to "ABCDEF+OriginalName" — a PDF
            // subset-font convention. This only affects the BaseFont name
            // emitted by embedTrueType; the resource-name keying stays
            // `fontName` so callers can still reference it as before.
            let prefixedName = sprintf "%s+%s" (subsetPrefix usedGids) fontName
            let fontRef = Font.embedTrueType builder.Doc ttf prefixedName
            let resourceName =
                $"F{Map.count builder.Fonts + Map.count builder.EmbeddedFonts}"
            let f = { Name = resourceName; ObjRef = fontRef; Font = ttf }
            builder.EmbeddedFonts <- Map.add fontName f builder.EmbeddedFonts
            remap

    /// Load and embed a compacted, subsetted TrueType font with extra
    /// glyph-to-text mappings for shaped ligatures/clusters.
    let loadFontBytesSubsetWithToUnicode
        (builder: DocBuilder) (fontName: string) (data: byte[]) (usedGids: Set<int>) (customToUnicode: Map<int, string>)
        : Map<int, int> =
        if Set.isEmpty usedGids then
            let ttf = Font.parseTrueType data
            let fontRef = Font.embedTrueTypeWithToUnicode builder.Doc ttf fontName customToUnicode
            let resourceName = $"F{Map.count builder.Fonts + Map.count builder.EmbeddedFonts}"
            let f = { Name = resourceName; ObjRef = fontRef; Font = ttf }
            builder.EmbeddedFonts <- Map.add fontName f builder.EmbeddedFonts
            Map.empty
        else
            let subsetBytes, remap = Rbt.OpenType.Subset.subsetCompact data usedGids
            let ttf = Font.parseTrueType subsetBytes
            // Rename the internal font to "ABCDEF+OriginalName" — a PDF
            // subset-font convention. This only affects the BaseFont name
            // emitted by embedTrueType; the resource-name keying stays
            // `fontName` so callers can still reference it as before.
            let prefixedName = sprintf "%s+%s" (subsetPrefix usedGids) fontName
            let remappedToUnicode =
                customToUnicode
                |> Map.toSeq
                |> Seq.choose (fun (oldGid, text) ->
                    Map.tryFind oldGid remap |> Option.map (fun newGid -> newGid, text))
                |> Map.ofSeq
            let fontRef = Font.embedTrueTypeWithToUnicode builder.Doc ttf prefixedName remappedToUnicode
            let resourceName =
                $"F{Map.count builder.Fonts + Map.count builder.EmbeddedFonts}"
            let f = { Name = resourceName; ObjRef = fontRef; Font = ttf }
            builder.EmbeddedFonts <- Map.add fontName f builder.EmbeddedFonts
            remap

    /// Get the resource name for an embedded font
    let getEmbeddedFont (builder: DocBuilder) (fontName: string) =
        Map.tryFind fontName builder.EmbeddedFonts

    /// Create drawing ops for text using an embedded font (for Rbt.Unicode/CJK)
    let embeddedText (builder: DocBuilder) (fontName: string) (fontSize: float) (x: float) (y: float) (text: string) =
        match getEmbeddedFont builder fontName with
        | None -> []
        | Some ef ->
            let hex = Font.toGlyphHex ef.Font text
            [
                BeginText
                SetFont (ef.Name, fontSize)
                TextMatrix (1.0, 0.0, 0.0, 1.0, x, y)
                ShowTextHex hex
                EndText
            ]

    /// Add a JPEG image, returns the image name for use in drawing
    let addJpeg (builder: DocBuilder) (name: string) (data: byte[]) =
        match Image.fromJpeg data with
        | Ok info ->
            let imgRef = Image.toXObject builder.Doc info
            builder.Images <- Map.add name imgRef builder.Images
            Ok name
        | Error e -> Error e

    /// Add a JPEG image from file
    let addJpegFile (builder: DocBuilder) (name: string) (path: string) =
#if FABLE_COMPILER
        Error "addJpegFile is not available in Fable; use addJpeg with caller-provided bytes."
#else
        addJpeg builder name (File.ReadAllBytes path)
#endif

    /// Add a PNG image, returns the image name for use in drawing
    let addPng (builder: DocBuilder) (name: string) (data: byte[]) =
        match Image.fromPng data with
        | Ok info ->
            let imgRef = Image.toXObject builder.Doc info
            builder.Images <- Map.add name imgRef builder.Images
            Ok name
        | Error e -> Error e

    /// Add a PNG image from file
    let addPngFile (builder: DocBuilder) (name: string) (path: string) =
#if FABLE_COMPILER
        Error "addPngFile is not available in Fable; use addPng with caller-provided bytes."
#else
        addPng builder name (File.ReadAllBytes path)
#endif

    /// Add image by auto-detecting format (PNG or JPEG)
    let addImage (builder: DocBuilder) (name: string) (data: byte[]) =
        // Check PNG signature
        if data.Length >= 8 && data.[0] = 0x89uy && data.[1] = 0x50uy then
            addPng builder name data
        // Check JPEG signature
        elif data.Length >= 2 && data.[0] = 0xFFuy && data.[1] = 0xD8uy then
            addJpeg builder name data
        else
            Error "Unknown image format"

    /// Extract alpha values used in drawing operations for ExtGState
    let private extractAlphaValues (ops: DrawOp list) : (string * float * bool) list =
        ops |> List.choose (function
            | SetFillAlpha a ->
                let name = sprintf "GS_ca_%d" (int (a * 1000.0))
                Some (name, a, true)  // true = fill alpha
            | SetStrokeAlpha a ->
                let name = sprintf "GS_CA_%d" (int (a * 1000.0))
                Some (name, a, false)  // false = stroke alpha
            | _ -> None)
        |> List.distinctBy (fun (name, _, _) -> name)

    /// Create ExtGState objects for transparency
    let private createExtGStates (doc: PdfDocument) (alphaValues: (string * float * bool) list) : Map<string, PdfObj> =
        alphaValues
        |> List.map (fun (name, alpha, isFill) ->
            let dict =
                if isFill then
                    Map.ofList [("Type", PdfName "ExtGState"); ("ca", PdfReal alpha)]
                else
                    Map.ofList [("Type", PdfName "ExtGState"); ("CA", PdfReal alpha)]
            let objRef = Write.newIndirectObj doc (PdfDict dict)
            (name, objRef))
        |> Map.ofList

    /// Create resources dictionary for a page
    let private makeResources (builder: DocBuilder) (usedFonts: string list) (usedImages: string list) (extGStates: Map<string, PdfObj>) =
        // Standard fonts
        let standardFontEntries =
            usedFonts
            |> List.choose (fun family ->
                Map.tryFind family builder.Fonts
                |> Option.map (fun f -> (f.Name, f.ObjRef)))

        // Embedded fonts
        let embeddedFontEntries =
            usedFonts
            |> List.choose (fun fontName ->
                Map.tryFind fontName builder.EmbeddedFonts
                |> Option.map (fun f -> (f.Name, f.ObjRef)))

        let fontDict = Map.ofList (standardFontEntries @ embeddedFontEntries)

        let imageDict =
            usedImages
            |> List.choose (fun name ->
                Map.tryFind name builder.Images
                |> Option.map (fun img -> (name, img)))
            |> Map.ofList

        let resources = [
            if not fontDict.IsEmpty then
                ("Font", PdfDict fontDict)
            if not imageDict.IsEmpty then
                ("XObject", PdfDict imageDict)
            if not extGStates.IsEmpty then
                ("ExtGState", PdfDict extGStates)
        ]
        PdfDict (Map.ofList resources)

    /// Add a page with drawing operations
    let addPage (builder: DocBuilder) (width: float, height: float) (fontFamilies: string list) (imageNames: string list) (ops: DrawOp list) =
        // Ensure standard fonts exist. Embedded fonts are already loaded in
        // builder.EmbeddedFonts and must be preferred when remapping SetFont,
        // otherwise subset resources are present but never selected.
        let fonts =
            fontFamilies
            |> List.choose (fun family ->
                match Map.tryFind family builder.EmbeddedFonts with
                | Some _ -> None
                | None -> Some (ensureFont builder family))

        // Replace font family names with resource names in ops
        let remappedOps = ops |> List.map (function
            | SetFont (family, size) ->
                match Map.tryFind family builder.EmbeddedFonts with
                | Some f -> SetFont (f.Name, size)
                | None ->
                    match fonts |> List.tryFind (fun f -> Map.tryFind family builder.Fonts = Some f) with
                    | Some f -> SetFont (f.Name, size)
                    | None ->
                        let f = ensureFont builder family
                        SetFont (f.Name, size)
            | op -> op
        )

        let contentBytes = render remappedOps |> Encoding.UTF8.GetBytes
        let contentRef = Write.newStream builder.Doc contentBytes []

        // Extract and create ExtGState resources for transparency
        let alphaValues = extractAlphaValues ops
        let extGStates = createExtGStates builder.Doc alphaValues

        let resources = makeResources builder fontFamilies imageNames extGStates

        let pageDict = PdfDict (Map.ofList [
            ("Type", PdfName "Page")
            ("MediaBox", PdfArray [PdfInt 0L; PdfInt 0L; PdfInt (int64 width); PdfInt (int64 height)])
            ("Contents", contentRef)
            ("Resources", resources)
        ])

        let pageRef = Write.newIndirectObj builder.Doc pageDict
        builder.Pages <- builder.Pages @ [pageRef]
        pageRef

    /// Finalize the document and save to file
    let save (builder: DocBuilder) (path: string) =
        // Create Pages node
        let pagesDict = PdfDict (Map.ofList [
            ("Type", PdfName "Pages")
            ("Kids", PdfArray builder.Pages)
            ("Count", PdfInt (int64 (List.length builder.Pages)))
        ])
        let pagesRef = Write.newIndirectObj builder.Doc pagesDict

        // Update each page's Parent
        for pageRef in builder.Pages do
            match pageRef with
            | PdfIndirect (objNum, _) ->
                match builder.Doc.XRef.[objNum] with
                | InUse (pos, gen, Some (PdfDict d)) ->
                    let updated = d |> Map.add "Parent" pagesRef
                    Write.setObj builder.Doc objNum (PdfDict updated)
                | _ -> ()
            | _ -> ()

        // Update catalog
        match builder.Doc.Catalog with
        | PdfIndirect (objNum, _) ->
            Write.setObj builder.Doc objNum (PdfDict (Map.ofList [
                ("Type", PdfName "Catalog")
                ("Pages", pagesRef)
            ]))
        | _ -> ()

        Write.save builder.Doc path

    /// Get raw PDF bytes without saving
    let toBytes (builder: DocBuilder) =
        // Same as save but returns bytes
        let pagesDict = PdfDict (Map.ofList [
            ("Type", PdfName "Pages")
            ("Kids", PdfArray builder.Pages)
            ("Count", PdfInt (int64 (List.length builder.Pages)))
        ])
        let pagesRef = Write.newIndirectObj builder.Doc pagesDict

        for pageRef in builder.Pages do
            match pageRef with
            | PdfIndirect (objNum, _) ->
                match builder.Doc.XRef.[objNum] with
                | InUse (pos, gen, Some (PdfDict d)) ->
                    let updated = d |> Map.add "Parent" pagesRef
                    Write.setObj builder.Doc objNum (PdfDict updated)
                | _ -> ()
            | _ -> ()

        match builder.Doc.Catalog with
        | PdfIndirect (objNum, _) ->
            Write.setObj builder.Doc objNum (PdfDict (Map.ofList [
                ("Type", PdfName "Catalog")
                ("Pages", pagesRef)
            ]))
        | _ -> ()

        Write.update builder.Doc

/// Table layout for PDF documents
module Table =
    open Color
    open Draw

    type HAlign = Left | Center | Right
    type VAlign = Top | Middle | Bottom

    type CellStyle = {
        Font: string
        FontSize: float
        TextColor: Color
        BackgroundColor: Color option
        HAlign: HAlign
        VAlign: VAlign
        PaddingLeft: float
        PaddingRight: float
        PaddingTop: float
        PaddingBottom: float
    }

    type BorderStyle = {
        Color: Color
        Width: float
    }

    type TableStyle = {
        BorderOuter: BorderStyle option
        BorderInner: BorderStyle option
        BorderHeader: BorderStyle option
        HeaderStyle: CellStyle
        CellStyle: CellStyle
        AlternateRowColor: Color option
    }

    let defaultCellStyle = {
        Font = "Helvetica"
        FontSize = 10.0
        TextColor = black
        BackgroundColor = None
        HAlign = Left
        VAlign = Middle
        PaddingLeft = 4.0
        PaddingRight = 4.0
        PaddingTop = 3.0
        PaddingBottom = 3.0
    }

    let defaultHeaderStyle = {
        defaultCellStyle with
            Font = "Helvetica-Bold"
            BackgroundColor = Some (gray 0.9)
    }

    let defaultBorder = { Color = black; Width = 0.5 }

    let defaultTableStyle = {
        BorderOuter = Some defaultBorder
        BorderInner = Some { defaultBorder with Width = 0.25 }
        BorderHeader = Some { defaultBorder with Width = 0.75 }
        HeaderStyle = defaultHeaderStyle
        CellStyle = defaultCellStyle
        AlternateRowColor = None
    }

    type Cell = {
        Text: string
        Style: CellStyle option  // None = use row/table default
        ColSpan: int
    }

    type Row = {
        Cells: Cell list
        IsHeader: bool
        Style: CellStyle option
    }

    type Table = {
        Rows: Row list
        ColumnWidths: float list  // absolute widths in points
        Style: TableStyle
    }

    let cell text = { Text = text; Style = None; ColSpan = 1 }
    let cellStyled style text = { Text = text; Style = Some style; ColSpan = 1 }
    let cellSpan colspan text = { Text = text; Style = None; ColSpan = colspan }

    let row cells = { Cells = cells; IsHeader = false; Style = None }
    let headerRow cells = { Cells = cells; IsHeader = true; Style = None }
    let styledRow style cells = { Cells = cells; IsHeader = false; Style = Some style }

    let table style colWidths rows = { Rows = rows; ColumnWidths = colWidths; Style = style }

    /// Calculate row height based on content and style
    let private calcRowHeight (row: Row) (defaultStyle: CellStyle) =
        let style = row.Style |> Option.defaultValue defaultStyle
        style.FontSize + style.PaddingTop + style.PaddingBottom + 2.0

    /// Render a table to draw operations, returns (ops, totalHeight)
    let render (x: float) (y: float) (tbl: Table) : DrawOp list * float =
        let mutable ops = []
        let mutable currentY = y

        let tableWidth = tbl.ColumnWidths |> List.sum

        // Calculate all row heights first
        let rowHeights = tbl.Rows |> List.map (fun row ->
            let baseStyle = if row.IsHeader then tbl.Style.HeaderStyle else tbl.Style.CellStyle
            calcRowHeight row baseStyle
        )

        let totalHeight = rowHeights |> List.sum

        // Draw backgrounds and cells
        let mutable rowIndex = 0
        for row in tbl.Rows do
            let rowHeight = rowHeights.[rowIndex]
            let baseStyle = if row.IsHeader then tbl.Style.HeaderStyle else tbl.Style.CellStyle
            let rowStyle = row.Style |> Option.defaultValue baseStyle

            // Alternate row coloring
            let bgColor =
                match row.IsHeader, tbl.Style.AlternateRowColor with
                | true, _ -> rowStyle.BackgroundColor
                | false, Some altColor when rowIndex % 2 = 1 -> Some altColor
                | _ -> rowStyle.BackgroundColor

            // Draw row background
            match bgColor with
            | Some color ->
                ops <- ops @ [
                    SetFillColor color
                    Rect (x, currentY - rowHeight, tableWidth, rowHeight)
                    Fill
                ]
            | None -> ()

            // Draw cells
            let mutable cellX = x
            let mutable colIndex = 0
            for cell in row.Cells do
                let cellStyle = cell.Style |> Option.defaultValue rowStyle

                // Calculate cell width (handle colspan)
                let cellWidth =
                    tbl.ColumnWidths
                    |> List.skip colIndex
                    |> List.take (min cell.ColSpan (tbl.ColumnWidths.Length - colIndex))
                    |> List.sum

                // Cell background (if different from row)
                match cell.Style |> Option.bind (fun s -> s.BackgroundColor) with
                | Some color when bgColor <> Some color ->
                    ops <- ops @ [
                        SetFillColor color
                        Rect (cellX, currentY - rowHeight, cellWidth, rowHeight)
                        Fill
                    ]
                | _ -> ()

                // Calculate text position
                let textWidth = float cell.Text.Length * cellStyle.FontSize * 0.5  // rough estimate
                let textX =
                    match cellStyle.HAlign with
                    | Left -> cellX + cellStyle.PaddingLeft
                    | Center -> cellX + (cellWidth - textWidth) / 2.0
                    | Right -> cellX + cellWidth - cellStyle.PaddingRight - textWidth

                let textY =
                    match cellStyle.VAlign with
                    | Top -> currentY - cellStyle.PaddingTop - cellStyle.FontSize
                    | Middle -> currentY - rowHeight / 2.0 - cellStyle.FontSize / 3.0
                    | Bottom -> currentY - rowHeight + cellStyle.PaddingBottom

                // Draw text
                ops <- ops @ [
                    SetFillColor cellStyle.TextColor
                    BeginText
                    SetFont (cellStyle.Font, cellStyle.FontSize)
                    TextMatrix (1.0, 0.0, 0.0, 1.0, textX, textY)
                    ShowText cell.Text
                    EndText
                ]

                cellX <- cellX + cellWidth
                colIndex <- colIndex + cell.ColSpan

            currentY <- currentY - rowHeight
            rowIndex <- rowIndex + 1

        // Draw borders
        let drawHLine y1 border =
            ops <- ops @ [
                SetStrokeColor border.Color
                SetLineWidth border.Width
                MoveTo (x, y1)
                LineTo (x + tableWidth, y1)
                Stroke
            ]

        let drawVLine x1 yTop yBottom border =
            ops <- ops @ [
                SetStrokeColor border.Color
                SetLineWidth border.Width
                MoveTo (x1, yTop)
                LineTo (x1, yBottom)
                Stroke
            ]

        // Outer border
        match tbl.Style.BorderOuter with
        | Some border ->
            ops <- ops @ [
                SetStrokeColor border.Color
                SetLineWidth border.Width
                Rect (x, y - totalHeight, tableWidth, totalHeight)
                Stroke
            ]
        | None -> ()

        // Inner horizontal lines
        match tbl.Style.BorderInner with
        | Some border ->
            let mutable lineY = y
            let mutable idx = 0
            for rh in rowHeights do
                lineY <- lineY - rh
                // Don't draw line after last row (outer border handles it)
                if idx < rowHeights.Length - 1 then
                    // Check if this is header border
                    let isHeaderBorder =
                        idx < tbl.Rows.Length - 1
                        && tbl.Rows.[idx].IsHeader
                        && not tbl.Rows.[idx + 1].IsHeader
                    let b =
                        if isHeaderBorder then
                            tbl.Style.BorderHeader |> Option.defaultValue border
                        else
                            border
                    drawHLine lineY b
                idx <- idx + 1
        | None -> ()

        // Inner vertical lines
        match tbl.Style.BorderInner with
        | Some border ->
            let mutable lineX = x
            for i, w in tbl.ColumnWidths |> List.indexed do
                lineX <- lineX + w
                if i < tbl.ColumnWidths.Length - 1 then
                    drawVLine lineX y (y - totalHeight) border
        | None -> ()

        (ops, totalHeight)

    /// Helper to create evenly spaced columns
    let evenColumns count totalWidth =
        List.replicate count (totalWidth / float count)

    /// Helper to create proportional columns
    let proportionalColumns (proportions: float list) totalWidth =
        let total = proportions |> List.sum
        proportions |> List.map (fun p -> totalWidth * p / total)

/// PDF editing operations: merge, split, rotate, reorder, delete pages
module Operations =
    open System.IO

    /// Page attributes a page may inherit from its ancestors in the page tree.
    let private inheritableKeys = [ "MediaBox"; "CropBox"; "Resources"; "Rotate" ]

    let private isPageNode (src: PdfDocument) (d: Map<string, PdfObj>) =
        match Map.tryFind "Type" d |> Option.map (Pdf.deref src) with
        | Some (PdfName "Page") | Some (PdfName "Pages") -> true
        | _ -> false

    /// Builds a new document from pages copied out of source documents.
    ///
    /// Copied pages carry their inherited attributes (MediaBox, CropBox,
    /// Resources, Rotate from ancestor Pages nodes). Indirect objects are
    /// copied through a work queue, each source object once, so reference
    /// cycles (an annotation's /P back to its page, /Parent chains) and long
    /// reference chains neither loop nor overflow the stack. References to
    /// pages that are not being copied, and to page-tree nodes, become null
    /// rather than dragging the rest of the source document along.
    type private PageCopier() =
        let xref = ResizeArray<XRefEntry>([ Free (0, 65535); InUse (0L, 0, Some PdfNull) ])
        let kids = ResizeArray<PdfObj>()
        let reserve () =
            xref.Add(InUse (0L, 0, Some PdfNull))
            xref.Count - 1
        let set (num: int) (obj: PdfObj) = xref.[num] <- InUse (0L, 0, Some obj)

        member _.PageCount = kids.Count

        /// Copy `pages` (page refs of `src`) in order.
        member _.CopyPages(src: PdfDocument, pages: PdfObj list) =
            let map = System.Collections.Generic.Dictionary<int, int>()
            let nulls = System.Collections.Generic.HashSet<int>()
            let queue = System.Collections.Generic.Queue<int * int>()
            // Map copied pages first so links and annotations land on the copies.
            let targets =
                pages |> List.map (fun pageRef ->
                    let dst = reserve ()
                    match pageRef with
                    | PdfIndirect (num, _) when not (map.ContainsKey num) -> map.[num] <- dst
                    | _ -> ()
                    pageRef, dst)
            let rec copy (obj: PdfObj) : PdfObj =
                match obj with
                | PdfIndirect (num, _) ->
                    match map.TryGetValue num with
                    | true, dst -> PdfIndirect (dst, 0)
                    | _ when nulls.Contains num -> PdfNull
                    | _ ->
                        match Pdf.deref src obj with
                        | PdfDict d when isPageNode src d ->
                            nulls.Add num |> ignore
                            PdfNull
                        | _ ->
                            let dst = reserve ()
                            map.[num] <- dst
                            queue.Enqueue((num, dst))
                            PdfIndirect (dst, 0)
                | PdfArray items -> PdfArray (items |> List.map copy)
                | PdfDict d -> PdfDict (d |> Map.map (fun _ v -> copy v))
                | PdfStream (header, data) -> PdfStream (header |> Map.map (fun _ v -> copy v), data)
                | other -> other
            for pageRef, dst in targets do
                match Pdf.deref src pageRef with
                | PdfDict d ->
                    let withInherited =
                        inheritableKeys |> List.fold (fun (acc: Map<string, PdfObj>) key ->
                            if acc.ContainsKey key then acc
                            else
                                match Pdf.getPageInheritable src pageRef key with
                                | Some v -> acc.Add(key, v)
                                | None -> acc) d
                    let withInherited =
                        if withInherited.ContainsKey "MediaBox" then withInherited
                        else withInherited.Add("MediaBox", PdfArray [ PdfInt 0L; PdfInt 0L; PdfInt 612L; PdfInt 792L ])
                    let copied =
                        withInherited
                        |> Map.remove "Parent"
                        |> Map.add "Type" (PdfName "Page")
                        |> Map.map (fun _ v -> copy v)
                    set dst (PdfDict copied)
                    kids.Add(PdfIndirect (dst, 0))
                | _ -> failwith "Invalid page object"
                while queue.Count > 0 do
                    let num, dst = queue.Dequeue()
                    set dst (copy (Pdf.deref src (PdfIndirect (num, 0))))

        /// The finished document: catalog (object 1), one flat Pages node, and
        /// every copied object, all marked for writing.
        member _.ToDocument() : PdfDocument =
            let pagesNum = reserve ()
            set pagesNum (PdfDict (Map.ofList [
                "Type", PdfName "Pages"
                "Kids", PdfArray (List.ofSeq kids)
                "Count", PdfInt (int64 kids.Count) ]))
            for kid in kids do
                match kid with
                | PdfIndirect (num, _) ->
                    match xref.[num] with
                    | InUse (_, _, Some (PdfDict d)) -> set num (PdfDict (d.Add("Parent", PdfIndirect (pagesNum, 0))))
                    | _ -> ()
                | _ -> ()
            set 1 (PdfDict (Map.ofList [ "Type", PdfName "Catalog"; "Pages", PdfIndirect (pagesNum, 0) ]))
            {
                Data = Array.empty
                XRef = xref.ToArray()
                XRefType = "table"
                Trailer = Map.empty
                Catalog = PdfIndirect (1, 0)
                Modified = Set.ofSeq [ 0 .. xref.Count - 1 ]
            }

    /// Get the total page count (walks the page tree)
    let private pageCount (doc: PdfDocument) : int = Pdf.getPageRefs doc |> List.length

    /// Extract specific pages from a PDF (1-indexed); out-of-range numbers are skipped.
    let extractPages (doc: PdfDocument) (pageNums: int list) : PdfDocument =
        let all = Pdf.getPageRefs doc |> Array.ofList
        let copier = PageCopier()
        let pages = pageNums |> List.filter (fun n -> n >= 1 && n <= all.Length) |> List.map (fun n -> all.[n - 1])
        copier.CopyPages(doc, pages)
        copier.ToDocument()

    /// Split a PDF at given page numbers, returning multiple documents
    /// splitAt = [3; 5] splits into pages 1-2, 3-4, 5-end
    let splitAt (doc: PdfDocument) (splitPoints: int list) : PdfDocument list =
        let pageCount' = pageCount doc
        let sortedPoints = splitPoints |> List.sort |> List.distinct |> List.filter (fun p -> p > 1 && p <= pageCount')
        let ranges =
            let starts = 1 :: sortedPoints
            let ends = (sortedPoints |> List.map (fun p -> p - 1)) @ [pageCount']
            List.zip starts ends
            |> List.map (fun (s, e) -> [s..e])
        ranges |> List.map (extractPages doc)

    /// Rotate pages by given degrees (must be multiple of 90)
    let rotatePages (doc: PdfDocument) (pageNums: int list) (degrees: int) : unit =
        if degrees % 90 <> 0 then failwith "Rotation must be multiple of 90"
        let normalizedRotation = ((degrees % 360) + 360) % 360
        let all = Pdf.getPageRefs doc |> Array.ofList

        for pageNum in pageNums do
            if pageNum >= 1 && pageNum <= all.Length then
                match all.[pageNum - 1] with
                | PdfIndirect (objNum, _) as pageRef ->
                    match Pdf.deref doc pageRef with
                    | PdfDict d ->
                        let currentRotation =
                            match Pdf.getPageInheritable doc pageRef "Rotate" with
                            | Some (PdfInt r) -> int r
                            | _ -> 0
                        let newRotation = (currentRotation + normalizedRotation) % 360
                        let updated = d |> Map.add "Rotate" (PdfInt (int64 newRotation))
                        Write.setObj doc objNum (PdfDict updated)
                    | _ -> ()
                | _ -> ()

    /// Delete pages from document (1-indexed)
    let deletePages (doc: PdfDocument) (pageNums: int list) : PdfDocument =
        let pc = pageCount doc
        let toKeep = [1..pc] |> List.filter (fun n -> not (List.contains n pageNums))
        extractPages doc toKeep

    /// Reorder pages according to new order (list of 1-indexed page numbers)
    let reorderPages (doc: PdfDocument) (newOrder: int list) : PdfDocument =
        extractPages doc newOrder

    /// Merge multiple PDFs into one
    let merge (docs: PdfDocument list) : PdfDocument =
        let copier = PageCopier()
        for doc in docs do
            copier.CopyPages(doc, Pdf.getPageRefs doc)
        copier.ToDocument()

    /// Merge PDFs from byte arrays
    let mergeBytes (pdfBytes: byte[] list) : byte[] =
        let docs = pdfBytes |> List.map Pdf.openData
        let merged = merge docs
        Write.update merged

    /// Get document as bytes
    let toBytes (doc: PdfDocument) : byte[] =
        Write.update doc

    // Annotation types
    type Point = { X: float; Y: float }
    type StrokeAnnotation = { Page: int; Points: Point list; Color: string; Width: float; Opacity: float }
    type TextAnnotation = { Page: int; X: float; Y: float; Text: string; FontSize: float; Color: string }
    type ImageAnnotation = { Page: int; X: float; Y: float; Width: float; Height: float; PngData: byte[] }

    /// Parse hex color (#RRGGBB or #RGB) to RGB floats (0-1)
    let private parseColor (hex: string) =
        let hex = if hex.StartsWith("#") then hex.Substring(1) else hex
        try
            if hex.Length = 6 then
                let r = Convert.ToInt32(hex.Substring(0, 2), 16)
                let g = Convert.ToInt32(hex.Substring(2, 2), 16)
                let b = Convert.ToInt32(hex.Substring(4, 2), 16)
                (float r / 255.0, float g / 255.0, float b / 255.0)
            elif hex.Length = 3 then
                let r = Convert.ToInt32(hex.Substring(0, 1) + hex.Substring(0, 1), 16)
                let g = Convert.ToInt32(hex.Substring(1, 1) + hex.Substring(1, 1), 16)
                let b = Convert.ToInt32(hex.Substring(2, 1) + hex.Substring(2, 1), 16)
                (float r / 255.0, float g / 255.0, float b / 255.0)
            else
                (0.0, 0.0, 0.0)
        with _ -> (0.0, 0.0, 0.0)

    /// Format float for PDF (avoid locale issues)
    let private fmtFloat (f: float) = f.ToString("0.###", System.Globalization.CultureInfo.InvariantCulture)

    /// Generate PDF content stream for stroke annotations on a page
    let private generateStrokeContent (pageHeight: float) (strokes: StrokeAnnotation list) =
        let sb = StringBuilder()
        sb.Append("q\n") |> ignore  // Save graphics state

        for stroke in strokes do
            if stroke.Points.Length >= 2 then
                let (r, g, b) = parseColor stroke.Color
                // Set stroke color and width
                sb.Append(fmtFloat r).Append(" ").Append(fmtFloat g).Append(" ").Append(fmtFloat b).Append(" RG\n") |> ignore
                sb.Append(fmtFloat stroke.Width).Append(" w\n") |> ignore
                sb.Append("1 J 1 j\n") |> ignore  // Round line cap and join

                // Set opacity if not 1.0
                if stroke.Opacity < 1.0 then
                    sb.Append("/GS1 gs\n") |> ignore

                // Draw path - flip Y coordinate (PDF origin is bottom-left)
                let first = stroke.Points.Head
                sb.Append(fmtFloat first.X).Append(" ").Append(fmtFloat (pageHeight - first.Y)).Append(" m\n") |> ignore
                for pt in stroke.Points.Tail do
                    sb.Append(fmtFloat pt.X).Append(" ").Append(fmtFloat (pageHeight - pt.Y)).Append(" l\n") |> ignore
                sb.Append("S\n") |> ignore  // Stroke

        sb.Append("Q\n") |> ignore  // Restore graphics state
        asciiBytes (sb.ToString())

    /// Generate PDF content stream for text annotations on a page
    let private generateTextContent (pageHeight: float) (texts: TextAnnotation list) =
        let sb = StringBuilder()
        sb.Append("q\n") |> ignore

        for text in texts do
            let (r, g, b) = parseColor text.Color
            sb.Append("BT\n") |> ignore
            sb.Append("/F1 ").Append(fmtFloat text.FontSize).Append(" Tf\n") |> ignore
            sb.Append(fmtFloat r).Append(" ").Append(fmtFloat g).Append(" ").Append(fmtFloat b).Append(" rg\n") |> ignore
            // Position - flip Y, adjust for baseline
            let y = pageHeight - text.Y
            sb.Append(fmtFloat text.X).Append(" ").Append(fmtFloat y).Append(" Td\n") |> ignore
            // Escape text for PDF string
            let escaped = text.Text.Replace("\\", "\\\\").Replace("(", "\\(").Replace(")", "\\)")
            sb.Append("(").Append(escaped).Append(") Tj\n") |> ignore
            sb.Append("ET\n") |> ignore

        sb.Append("Q\n") |> ignore
        asciiBytes (sb.ToString())

    /// Add annotations to a document, modifying it in place
    let addAnnotations (doc: PdfDocument) (strokes: StrokeAnnotation list) (texts: TextAnnotation list) (images: ImageAnnotation list) =
        let pc = pageCount doc

        // Group annotations by page
        let strokesByPage = strokes |> List.groupBy (fun s -> s.Page) |> Map.ofList
        let textsByPage = texts |> List.groupBy (fun t -> t.Page) |> Map.ofList
        let imagesByPage = images |> List.groupBy (fun i -> i.Page) |> Map.ofList

        for pageNum in 1 .. pc do
            let pageStrokes = strokesByPage |> Map.tryFind pageNum |> Option.defaultValue []
            let pageTexts = textsByPage |> Map.tryFind pageNum |> Option.defaultValue []
            let pageImages = imagesByPage |> Map.tryFind pageNum |> Option.defaultValue []

            if not (List.isEmpty pageStrokes && List.isEmpty pageTexts && List.isEmpty pageImages) then
                match Pdf.getPage doc pageNum with
                | Some pageRef ->
                    match pageRef with
                    | PdfIndirect (objNum, _) ->
                        let pageObj = Pdf.deref doc pageRef
                        // Get page dimensions
                        let pageHeight =
                            match PageOps.getMediaBox doc pageRef with
                            | Some (_, y0, _, y1) -> y1 - y0
                            | None -> 792.0  // Default letter height

                        // Generate content streams
                        let content = StringBuilder()
                        if not (List.isEmpty pageStrokes) then
                            content.Append(bytesToAscii (generateStrokeContent pageHeight pageStrokes)) |> ignore
                        if not (List.isEmpty pageTexts) then
                            content.Append(bytesToAscii (generateTextContent pageHeight pageTexts)) |> ignore
                        // Images would require XObject embedding - skip for now

                        if content.Length > 0 then
                            // Create new content stream
                            let newContentStream = Write.newStream doc (asciiBytes (content.ToString())) []

                            // Append to page's Contents
                            match pageObj with
                            | PdfDict d ->
                                let newContents =
                                    match Map.tryFind "Contents" d with
                                    | Some (PdfArray arr) -> PdfArray (arr @ [newContentStream])
                                    | Some existing -> PdfArray [existing; newContentStream]
                                    | None -> PdfArray [newContentStream]
                                let updated = d |> Map.add "Contents" newContents

                                // Ensure there's a font resource for text
                                let withResources =
                                    if not (List.isEmpty pageTexts) then
                                        let resources =
                                            match Map.tryFind "Resources" updated with
                                            | Some (PdfDict r) -> r
                                            | _ -> Map.empty
                                        let fonts =
                                            match Map.tryFind "Font" resources with
                                            | Some (PdfDict f) -> f
                                            | _ -> Map.empty
                                        // Add Helvetica as F1 if not present
                                        let fonts' =
                                            if Map.containsKey "F1" fonts then fonts
                                            else
                                                let fontDict = PdfDict (Map.ofList [
                                                    ("Type", PdfName "Font")
                                                    ("Subtype", PdfName "Type1")
                                                    ("BaseFont", PdfName "Helvetica")
                                                ])
                                                fonts |> Map.add "F1" fontDict
                                        let resources' = resources |> Map.add "Font" (PdfDict fonts')
                                        updated |> Map.add "Resources" (PdfDict resources')
                                    else updated
                                Write.setObj doc objNum (PdfDict withResources)
                            | _ -> ()
                    | _ -> ()
                | None -> ()
