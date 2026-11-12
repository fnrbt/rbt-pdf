module Rbt.Pdf.Render.Fonts

open Rbt.Pdf
open Rbt.Renderer

type FontInfo = {
    ToUnicodeCMap: byte[] option
    Widths: float[] option
    FirstChar: int
    MissingWidth: float
    Encoding: string
    EncodingDiffs: Map<int, string>
    EmbeddedFont: Text.FontData option
    ByteToGlyph: int[] option
    UnicodeGlyphs: bool
    IsComposite: bool
    CIDWidths: float[] option
    CIDToGIDMap: int[] option
    DefaultCIDWidth: float
    IsVertical: bool
    DefaultVerticalOriginY: float
    DefaultVerticalAdvance: float
    VerticalMetrics: Map<int, float * float * float>
    Type3Font: PdfObj option
    Type3Matrix: Matrix3x2
    Type3Resources: PdfObj option
    Type3CharProcs: PdfObj option
    CodeSpaces: (int * int * int) list
    CIDMap: Map<int * int, int>
    CIDUnicode: Map<int * int, string>
    UnicodeEncoded: bool
    IsCJK: bool
    UnicodeMap: Map<int * int, string>
    HasExplicitWidths: bool
}

let private loadEmbeddedFont (fd: FontResolve.PdfFontData) : Text.FontData option =
    match fd with
    | FontResolve.CffFontData (data, glyphWidths, ascender, descender) ->
        try Some (Text.loadCff data glyphWidths ascender descender 1000) with _ -> None
    | FontResolve.Type1FontData (data, length1, length2, glyphWidths, ascender, descender) ->
        try Some (Text.loadType1 data length1 length2 glyphWidths ascender descender 1000) with _ -> None
    | FontResolve.TrueTypeFontData data ->
        try Some (Text.loadFont data) with _ -> None
    | FontResolve.NoFontData -> None

let private cmapText (data: byte[]) = data |> Array.map char |> System.String

let private cmapBlocks name (text: string) =
    System.Text.RegularExpressions.Regex.Matches(text, "begin" + name + @"\s*(.*?)\s*end" + name, System.Text.RegularExpressions.RegexOptions.Singleline)
    |> Seq.cast<System.Text.RegularExpressions.Match>
    |> Seq.map (fun m -> m.Groups.[1].Value)

let private hexValue (s: string) = System.Convert.ToInt32(s, 16)

let private parseCodeSpaces text =
    [ for block in cmapBlocks "codespacerange" text do
        for m in System.Text.RegularExpressions.Regex.Matches(block, @"<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>") |> Seq.cast<System.Text.RegularExpressions.Match> do
            let lo = m.Groups.[1].Value
            let hi = m.Groups.[2].Value
            if lo.Length = hi.Length && lo.Length >= 2 && lo.Length <= 8 && lo.Length % 2 = 0 then
                yield lo.Length / 2, hexValue lo, hexValue hi ]

let private parseCIDMap text =
    let mutable result = Map.empty
    for block in cmapBlocks "cidchar" text do
        for m in System.Text.RegularExpressions.Regex.Matches(block, @"<([0-9A-Fa-f]+)>\s+(\d+)") |> Seq.cast<System.Text.RegularExpressions.Match> do
            let s = m.Groups.[1].Value
            if s.Length <= 8 then result <- Map.add (s.Length / 2, hexValue s) (int m.Groups.[2].Value) result
    for block in cmapBlocks "cidrange" text do
        for m in System.Text.RegularExpressions.Regex.Matches(block, @"<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>\s+(\d+)") |> Seq.cast<System.Text.RegularExpressions.Match> do
            let s = m.Groups.[1].Value
            let e = m.Groups.[2].Value
            if s.Length = e.Length && s.Length <= 8 then
                let lo, hi, cid = hexValue s, hexValue e, int m.Groups.[3].Value
                if hi >= lo && int64 hi - int64 lo <= 65535L then
                    for code in lo .. hi do result <- Map.add (s.Length / 2, code) (cid + code - lo) result
    result

let private unicodeHex (s: string) =
    if s.Length % 4 <> 0 then ""
    else [| for i in 0 .. 4 .. s.Length - 4 -> char (hexValue (s.Substring(i, 4))) |] |> System.String

let private parseUnicodeMap text =
    let mutable result = Map.empty
    for block in cmapBlocks "bfchar" text do
        for m in System.Text.RegularExpressions.Regex.Matches(block, @"<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>") |> Seq.cast<System.Text.RegularExpressions.Match> do
            let s = m.Groups.[1].Value
            if s.Length <= 8 then result <- Map.add (s.Length / 2, hexValue s) (unicodeHex m.Groups.[2].Value) result
    for block in cmapBlocks "bfrange" text do
        for m in System.Text.RegularExpressions.Regex.Matches(block, @"<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>\s*(<([0-9A-Fa-f]+)>|\[([^\]]*)\])") |> Seq.cast<System.Text.RegularExpressions.Match> do
            let s, e = m.Groups.[1].Value, m.Groups.[2].Value
            if s.Length = e.Length && s.Length <= 8 then
                let lo, hi = hexValue s, hexValue e
                if hi >= lo && int64 hi - int64 lo <= 65535L then
                    let values =
                        if m.Groups.[5].Success then
                            System.Text.RegularExpressions.Regex.Matches(m.Groups.[5].Value, @"<([0-9A-Fa-f]+)>")
                            |> Seq.cast<System.Text.RegularExpressions.Match>
                            |> Seq.map (fun x -> unicodeHex x.Groups.[1].Value) |> Seq.toArray
                        else
                            let mutable chars = (unicodeHex m.Groups.[4].Value).ToCharArray()
                            [| for _ in 0 .. hi - lo do
                                yield System.String(chars)
                                let mutable index = chars.Length - 1
                                let mutable carry = true
                                while index >= 0 && carry do
                                    let next = int chars.[index] + 1
                                    chars.[index] <- char (next &&& 0xffff)
                                    carry <- next > 0xffff
                                    index <- index - 1 |]
                    for i in 0 .. min (hi - lo) (values.Length - 1) do
                        result <- Map.add (s.Length / 2, lo + i) values.[i] result
    result

type private ParsedCMap = {
    Spaces: (int * int * int) list
    CIDs: Map<int * int, int>
    Unicode: Map<int * int, string>
    WMode: int option
}

let private emptyCMap = { Spaces = []; CIDs = Map.empty; Unicode = Map.empty; WMode = None }

let private mergeCMaps baseMap child =
    { Spaces = List.distinct (baseMap.Spaces @ child.Spaces)
      CIDs = Map.fold (fun acc k v -> Map.add k v acc) baseMap.CIDs child.CIDs
      Unicode = Map.fold (fun acc k v -> Map.add k v acc) baseMap.Unicode child.Unicode
      WMode = Option.orElse child.WMode baseMap.WMode }

let private parseCMap text =
    let mode = System.Text.RegularExpressions.Regex.Match(text, @"/WMode\s+([01])\s+def")
    { Spaces = parseCodeSpaces text
      CIDs = parseCIDMap text
      Unicode = parseUnicodeMap text
      WMode = if mode.Success then Some (int mode.Groups.[1].Value) else None }

// Stream dictionary UseCMap and PostScript usecmap both inherit before local entries.
let private loadCMap cmaps (doc: PdfDocument) source =
    let rec fromName visited depth name =
        if depth >= 32 || Set.contains name visited then emptyCMap
        elif name = "Identity-H" || name = "Identity-V" then
            { emptyCMap with Spaces = [2, 0, 65535]; WMode = Some (if name = "Identity-V" then 1 else 0) }
        else
            match Map.tryFind name cmaps with
            | Some bytes -> fromText (Set.add name visited) (depth + 1) bytes None
            | None -> emptyCMap
    and fromObject visited depth obj =
        if depth >= 32 then emptyCMap
        else
            match Pdf.deref doc obj with
            | PdfName name -> fromName visited depth name
            | PdfStream _ ->
                let parent = Pdf.dictGet doc "UseCMap" obj
                let data = try Pdf.getStreamData doc obj with _ -> [||]
                fromText visited (depth + 1) data parent
            | _ -> emptyCMap
    and fromText visited depth bytes parent =
        let text = cmapText bytes
        let mutable inherited = parent |> Option.map (fromObject visited depth) |> Option.defaultValue emptyCMap
        for m in System.Text.RegularExpressions.Regex.Matches(text, @"/([^\s/<>\[\]()]+)\s+usecmap") |> Seq.cast<System.Text.RegularExpressions.Match> do
            inherited <- mergeCMaps inherited (fromName visited depth m.Groups.[1].Value)
        mergeCMaps inherited (parseCMap text)
    source |> Option.map (fromObject Set.empty 0) |> Option.defaultValue emptyCMap

let private verticalWidths (doc: PdfDocument) descendant =
    let num obj =
        match Pdf.deref doc obj with PdfInt n -> Some (float n) | PdfReal n -> Some n | _ -> None
    let defaults =
        match descendant |> Option.bind (Pdf.dictGet doc "DW2") |> Option.map (Pdf.deref doc) with
        | Some (PdfArray [y; advance]) -> Option.defaultValue 880.0 (num y), Option.defaultValue -1000.0 (num advance)
        | _ -> 880.0, -1000.0
    let mutable metrics = Map.empty
    let rec parse values =
        match values with
        | start :: next :: rest ->
            match Pdf.deref doc start, Pdf.deref doc next with
            | PdfInt first, PdfArray entries when first >= 0L && first <= 65535L ->
                let rec triples cid items =
                    match items with
                    | a :: x :: y :: tail when cid <= 65535 ->
                        match num a, num x, num y with
                        | Some av, Some vx, Some vy -> metrics <- Map.add cid (av, vx, vy) metrics
                        | _ -> ()
                        triples (cid + 1) tail
                    | _ -> ()
                triples (int first) entries
                parse rest
            | PdfInt first, PdfInt last when first >= 0L && last >= first && last <= 65535L ->
                match rest with
                | a :: x :: y :: tail ->
                    match num a, num x, num y with
                    | Some av, Some vx, Some vy ->
                        for cid in int first .. int last do metrics <- Map.add cid (av, vx, vy) metrics
                    | _ -> ()
                    parse tail
                | _ -> ()
            | _ -> ()
        | _ -> ()
    match descendant |> Option.bind (Pdf.dictGet doc "W2") |> Option.map (Pdf.deref doc) with
    | Some (PdfArray values) -> parse values
    | _ -> ()
    fst defaults, snd defaults, metrics

let resolve (cmaps: Map<string, byte[]>) (doc: PdfDocument) (resources: PdfObj) (fontName: string) : FontInfo =
    let pfi = FontResolve.resolve doc resources fontName
    let fontObj = Pdf.dictGet doc "Font" resources |> Option.bind (Pdf.dictGet doc fontName)
    let get key = fontObj |> Option.bind (Pdf.dictGet doc key)
    let type3 = match get "Subtype" with Some (PdfName "Type3") -> fontObj | _ -> None
    let number = function PdfInt n -> float n | PdfReal n -> n | _ -> 0.0
    let matrix =
        match get "FontMatrix" |> Option.map (Pdf.deref doc) with
        | Some (PdfArray [a;b;c;d;e;f]) ->
            { M11 = number a; M12 = number b; M21 = number c; M22 = number d; DX = number e; DY = number f }
        | _ -> Matrix3x2.scale 0.001 0.001
    let descendant =
        match get "DescendantFonts" |> Option.map (Pdf.deref doc) with
        | Some (PdfArray (cid :: _)) -> Some cid
        | _ -> None
    let defaultWidth =
        match descendant |> Option.bind (Pdf.dictGet doc "DW") with
        | Some (PdfInt n) -> float n | Some (PdfReal n) -> n | _ -> 1000.0
    // Simple-font widths outside /FirstChar../LastChar use FontDescriptor /MissingWidth.
    let missingWidth =
        fontObj
        |> Option.bind (Pdf.dictGet doc "FontDescriptor")
        |> Option.bind (Pdf.dictGet doc "MissingWidth")
        |> Option.map (Pdf.deref doc)
        |> Option.bind (function PdfInt n -> Some (float n) | PdfReal n -> Some n | _ -> None)
        |> Option.defaultValue 0.0
    let ordering =
        descendant
        |> Option.bind (Pdf.dictGet doc "CIDSystemInfo")
        |> Option.bind (Pdf.dictGet doc "Ordering")
        |> Option.map (Pdf.deref doc)
        |> Option.bind (function PdfString b -> Some (cmapText b) | PdfName n -> Some n | _ -> None)
    let sourceEncoding =
        match get "Encoding" |> Option.map (Pdf.deref doc) with Some (PdfName n) -> n | _ -> ""
    let encoding = loadCMap cmaps doc (get "Encoding")
    let unicode = loadCMap cmaps doc (get "ToUnicode")
    let cidUnicode =
        ordering
        |> Option.map (fun name -> loadCMap cmaps doc (Some (PdfName ("Adobe-" + name + "-UCS2"))))
        |> Option.map (fun cmap -> cmap.Unicode)
        |> Option.defaultValue Map.empty
    // Simple fonts always consume one byte per character. A ToUnicode CMap
    // can advertise a wider codespace even when its actual mappings use
    // one-byte source codes; that codespace must not change text decoding.
    let spaces = if pfi.IsComposite then
                     if List.isEmpty encoding.Spaces then unicode.Spaces else encoding.Spaces
                 else [1, 0, 255]
    let verticalOriginY, verticalAdvance, verticalMetrics = verticalWidths doc descendant
    let writingMode =
        get "Encoding"
        |> Option.bind (Pdf.dictGet doc "WMode")
        |> Option.map (Pdf.deref doc)
        |> Option.bind (function PdfInt n -> Some (int n) | _ -> None)
        |> Option.orElseWith (fun () -> encoding.WMode)
        |> Option.defaultValue 0
    let symbolic =
        match get "FontDescriptor" |> Option.bind (Pdf.dictGet doc "Flags") with
        | Some (PdfInt flags) -> flags &&& 4L <> 0L
        | _ -> false
    let unicodeGlyphs =
        match pfi.FontData with
        | FontResolve.TrueTypeFontData _ -> not pfi.IsComposite && (not symbolic || not pfi.EncodingDiffs.IsEmpty)
        | _ -> false
    { ToUnicodeCMap = pfi.ToUnicodeCMap; Widths = pfi.Widths; FirstChar = pfi.FirstChar
      MissingWidth = missingWidth
      Encoding = pfi.Encoding; EncodingDiffs = pfi.EncodingDiffs; EmbeddedFont = loadEmbeddedFont pfi.FontData
      ByteToGlyph = pfi.ByteToGlyph; UnicodeGlyphs = unicodeGlyphs; IsComposite = pfi.IsComposite
      CIDWidths = pfi.CIDWidths; CIDToGIDMap = pfi.CIDToGIDMap; DefaultCIDWidth = defaultWidth
      IsVertical = writingMode = 1; DefaultVerticalOriginY = verticalOriginY
      DefaultVerticalAdvance = verticalAdvance; VerticalMetrics = verticalMetrics
      Type3Font = type3; Type3Matrix = matrix
      Type3Resources = if type3.IsSome then get "Resources" else None
      Type3CharProcs = if type3.IsSome then get "CharProcs" else None
      CodeSpaces = spaces
      CIDMap = encoding.CIDs
      UnicodeMap = unicode.Unicode
      CIDUnicode = cidUnicode
      UnicodeEncoded = sourceEncoding.StartsWith("Uni") && (sourceEncoding.Contains("UCS2") || sourceEncoding.Contains("UTF16"))
      IsCJK = ordering |> Option.exists (fun n -> n = "Japan1" || n = "GB1" || n = "CNS1" || n = "Korea1")
      HasExplicitWidths = (get "Widths").IsSome }

let decodeByEncoding encoding data = FontResolve.decodeByEncoding encoding data
let decodeWithToUnicode cmapData stringBytes = FontResolve.decodeWithToUnicode cmapData stringBytes

let private nameToUnicode =
    [ for encoding in ["MacRomanEncoding"; "WinAnsiEncoding"] do
        for code in 32 .. 255 do
            let name = FontResolve.glyphNameForByte encoding code
            if name <> "" && name <> ".notdef" then
                yield name, decodeByEncoding encoding [| byte code |] ]
    |> Map.ofList
    |> Map.add "space" " "
    |> Map.add "hyphen" "-"

let glyphName (fi: FontInfo) code =
    Map.tryFind code fi.EncodingDiffs
    |> Option.defaultWith (fun () -> FontResolve.glyphNameForByte fi.Encoding code)

let label (fi: FontInfo) code (bytes: byte[]) =
    let fallback () =
        match Map.tryFind code fi.EncodingDiffs with
        | Some name ->
            let baseName = name.Split('.').[0]
            let fromCodePoint value =
                if value < 0 || value > 0x10ffff || (value >= 0xd800 && value <= 0xdfff) then None
                elif value <= 0xffff then Some (string (char value))
                else
                    let n = value - 0x10000
                    Some (System.String([| char (0xd800 + (n >>> 10)); char (0xdc00 + (n &&& 0x3ff)) |]))
            let named =
                try
                    if baseName.StartsWith("uni") && baseName.Length > 3 && (baseName.Length - 3) % 4 = 0 then
                        let parts = [ for i in 3 .. 4 .. baseName.Length - 4 -> fromCodePoint (hexValue (baseName.Substring(i, 4))) ]
                        if parts |> List.forall Option.isSome then Some (parts |> List.choose id |> String.concat "") else None
                    elif baseName.StartsWith("u") && baseName.Length >= 5 && baseName.Length <= 7 then
                        fromCodePoint (hexValue (baseName.Substring(1)))
                    else None
                with _ -> None
            if baseName.Length = 1 then baseName
            else
                named |> Option.orElseWith (fun () -> Map.tryFind baseName nameToUnicode)
                |> Option.defaultWith (fun () -> decodeByEncoding fi.Encoding bytes)
        | None -> decodeByEncoding fi.Encoding bytes
    match Map.tryFind (bytes.Length, code) fi.UnicodeMap with
    | Some text -> text
    | None when fi.UnicodeEncoded && bytes.Length % 2 = 0 ->
        [| for i in 0 .. 2 .. bytes.Length - 2 -> char ((int bytes.[i] <<< 8) ||| int bytes.[i + 1]) |]
        |> System.String
    | None when fi.IsComposite ->
        let cid = Map.tryFind (bytes.Length, code) fi.CIDMap |> Option.defaultValue code
        Map.tryFind (2, cid) fi.CIDUnicode |> Option.defaultWith fallback
    | None -> fallback ()

let sourceCodes (fi: FontInfo) (data: byte[]) =
    let result = ResizeArray<int * byte[]>()
    let mutable offset = 0
    while offset < data.Length do
        let mutable code = 0
        let mutable length = 0
        let mutable n = 0
        let maxLength = min 4 (data.Length - offset)
        while length = 0 && n < maxLength do
            code <- (code <<< 8) ||| int data.[offset + n]
            n <- n + 1
            if fi.CodeSpaces |> List.exists (fun (size, lo, hi) -> size = n && code >= lo && code <= hi) then length <- n
        if length = 0 then
            length <- min (if fi.IsComposite then 2 else 1) (data.Length - offset)
            code <- 0
            for j in 0 .. length - 1 do code <- (code <<< 8) ||| int data.[offset + j]
        result.Add(code, data.[offset .. offset + length - 1])
        offset <- offset + length
    result.ToArray()

let cid (fi: FontInfo) code length =
    Map.tryFind (length, code) fi.CIDMap |> Option.defaultValue code

let charWidth (fi: FontInfo) (charCode: int) : float =
    match fi.CIDWidths with
    | Some cw ->
        if charCode >= 0 && charCode < cw.Length then cw.[charCode]
        else fi.DefaultCIDWidth
    | None when fi.IsComposite -> fi.DefaultCIDWidth
    | None ->
        match fi.Widths with
        | Some ws ->
            let idx = charCode - fi.FirstChar
            if idx >= 0 && idx < ws.Length then ws.[idx]
            else fi.MissingWidth
        | None -> 600.0

let verticalMetrics (fi: FontInfo) cid =
    Map.tryFind cid fi.VerticalMetrics
    |> Option.defaultWith (fun () -> fi.DefaultVerticalAdvance, charWidth fi cid / 2.0, fi.DefaultVerticalOriginY)
