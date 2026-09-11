module Rbt.Pdf.Web.GlyphCache

open System.Collections.Generic
open Rbt.Renderer

/// Cache of glyph outlines as Path2D, keyed by combined (font.Id, glyphId).
/// Uses a single int key to avoid struct-tuple allocation per lookup under Fable.
let private cache = Dictionary<int64, WebApi.Path2D option>()

let inline private key (fontId: int) (gid: int) = (int64 fontId <<< 20) ||| int64 gid

/// Get a cached Path2D for a glyph. Builds from Text.glyphOutline on first access.
/// Returns None for glyphs without outlines.
let get (font: Text.FontData) (gid: int) : WebApi.Path2D option =
    let k = key font.Id gid
    match cache.TryGetValue(k) with
    | true, p -> p
    | _ ->
        let segs = Text.glyphOutline font gid
        let p =
            if segs.IsEmpty then None
            else Some (WebApi.toPath2D segs)
        cache.[k] <- p
        p

/// Clear the entire glyph cache (e.g. on document close).
let clear () =
    cache.Clear()
