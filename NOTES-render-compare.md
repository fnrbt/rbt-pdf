# Render comparison notes (rbt-pdf-web vs pdftoppm)

Date: 2026-09-11. Ground truth: `pdftoppm -r 96`. Ours: headless Chromium screenshots + canvas pixel reads.

## Closed (not bugs)

### wiki "gray bars" — NOT A BUG
What looked like thick gray bars in screenshots are 1px-tall table rules/underlines
rendered correctly. Verified by reading live canvas pixels (bars are h:1) and by
dumping the Cmd list (GRAY FillPaths with h=1.0 at correct positions). The
screenshot downscale made them look thick. No action.

## Open gaps (cause not positively identified — do not fix blind)

### 1. git-cheat header icons missing
Truth shows faint white line-art icons on the green header. Our render has plain
green. Facts established via fsi dumps:
- Page 1 emits 150 top cmds: 155 FillPath, 149 DrawGlyphs, 14 Clip, 1 Group.
  Zero StrokePath, zero DrawImage.
- No white fills emitted anywhere with y > 650 (header region).
- Page content stream uses only: BT/ET/Q/TJ/Tc/Tf/Tj/Tm/W/c/cs/f/gs/h/k/l/m/n/q/re/sc.
  No Form XObjects, no `sh`, no `Do`, no `S`/`B`.
- `sc`/`scn` and `cs` ARE handled (Render.fs:656-674).
Suspects (unconfirmed): pattern fills via `/Pattern` colorspace resolving to wrong
color; low-alpha fills via `gs` that CanvasBackend ignores; Type3 glyphs.
Next step: dump `cs`/`sc` operands + `gs` dicts in the header region of the content
stream and see what colorspace the icon paths use.

### 2. pdf20-simple "Hello World" vs "HelloWorld" spacing
Ours inserts a visible gap between "Hello" and "World"; poppler renders them
adjacent. Both come from the same content stream, so this is a glyph-advance or
text-positioning difference (our advances slightly wider, or Tc/Tw handling).
Could also be font-metrics (our embedded-font widths vs poppler's). Needs a
glyph-by-glyph X-position dump vs poppler's text extraction to localize. Not
investigated further.

### 3. CPU pegging on dense/multi-page docs (tracemonkey 14pp, css-spec 430pp)
`renderPage` is fully synchronous (parse already done; content-stream interp +
glyph outline building + Path2D construction all on main thread). One-page-per-rAF
helps but each page is still a multi-100ms main-thread block; 430 pages also means
430 IntersectionObserver entries + 430 canvases. This is the v2-worker motivation;
no incremental fix attempted. The eviction logic (canvas.width=0 on scroll-out) is
correct and does not loop (page divs keep explicit height + flex:none).

## Incidental (throwaway scripts, not product bugs)
- Native PPM writer dropped the R-channel mask (`v >>> 16` without `&&& 0xFFu`),
  painting everything red. Fixed in the script, not the product.
- `?url=` param only reads on full page load; in-page dropdown switching via
  dispatchEvent needs `{bubbles:true}`. Dev-shell quirks, not product bugs.
