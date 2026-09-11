module Rbt.Pdf.Render.Fonts

open Rbt.Pdf
open Rbt.Renderer

type FontInfo = {
    ToUnicodeCMap: byte[] option
    Widths: float[] option
    FirstChar: int
    Encoding: string
    EncodingDiffs: Map<int, string>
    EmbeddedFont: Text.FontData option
    ByteToGlyph: int[] option
    IsComposite: bool
    CIDWidths: float[] option
    CIDToGIDMap: int[] option
}

let private defaultInfo = { ToUnicodeCMap = None; Widths = None; FirstChar = 0; Encoding = "WinAnsi"; EncodingDiffs = Map.empty; EmbeddedFont = None; ByteToGlyph = None; IsComposite = false; CIDWidths = None; CIDToGIDMap = None }

let private embeddedFontCache = System.Collections.Generic.Dictionary<string, Text.FontData option>()

let private loadEmbeddedFont (fd: FontResolve.PdfFontData) : Text.FontData option =
    match fd with
    | FontResolve.CffFontData (data, glyphWidths, ascender, descender) ->
        try Some (Text.loadCff data glyphWidths ascender descender 1000) with _ -> None
    | FontResolve.Type1FontData (data, length1, length2, glyphWidths, ascender, descender) ->
        try Some (Text.loadType1 data length1 length2 glyphWidths ascender descender 1000) with _ -> None
    | FontResolve.TrueTypeFontData data ->
        try Some (Text.loadFont data) with _ -> None
    | FontResolve.NoFontData -> None

let resolve (doc: PdfDocument) (resources: PdfObj) (fontName: string) : FontInfo =
    let pfi = FontResolve.resolve doc resources fontName
    let baseFontName =
        match Pdf.dictGet doc "Font" resources with
        | Some fontDict ->
            match Pdf.dictGet doc fontName fontDict with
            | Some fontObj ->
                match Pdf.dictGet doc "BaseFont" fontObj with
                | Some (PdfName n) -> n | _ -> fontName
            | None -> fontName
        | None -> fontName
    let embeddedFont =
        match embeddedFontCache.TryGetValue(baseFontName) with
        | true, ef -> ef
        | _ ->
            let ef = loadEmbeddedFont pfi.FontData
            embeddedFontCache.[baseFontName] <- ef
            ef
    { ToUnicodeCMap = pfi.ToUnicodeCMap; Widths = pfi.Widths; FirstChar = pfi.FirstChar
      Encoding = pfi.Encoding; EncodingDiffs = pfi.EncodingDiffs; EmbeddedFont = embeddedFont
      ByteToGlyph = pfi.ByteToGlyph; IsComposite = pfi.IsComposite
      CIDWidths = pfi.CIDWidths; CIDToGIDMap = pfi.CIDToGIDMap }

let decodeByEncoding encoding data = FontResolve.decodeByEncoding encoding data
let decodeWithToUnicode cmapData stringBytes = FontResolve.decodeWithToUnicode cmapData stringBytes
let charWidth (fi: FontInfo) (charCode: int) : float =
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
