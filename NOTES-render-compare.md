# Render comparison notes (rbt-pdf-web vs pdftoppm)

Date: 2026-09-11. Ground truth: `pdftoppm -r 96`. Ours: headless Chromium screenshots + canvas pixel reads.

Current reproducible corpus: [tests/render-corpus/README.md](tests/render-corpus/README.md).
It contains 31 pinned upstream fixtures plus 40 generated feature cases, fetch
and generation scripts, and a native-versus-Poppler rendering driver. Its initial
verification section records visual mismatches and a Poppler JPX crash; successful
process exits must not be treated as visual passes.

## Resolved comparisons

### wiki "gray bars" — NOT A BUG
What looked like thick gray bars in screenshots are 1px-tall table rules/underlines
rendered correctly. Verified by reading live canvas pixels (bars are h:1) and by
dumping the Cmd list (GRAY FillPaths with h=1.0 at correct positions). The
screenshot downscale made them look thick. No action.

### PDF 2.0 "Hello World" spacing — fixed
The PDF Association's original sample has an explicit width array starting at
character 33. Space (32) uses its FontDescriptor's `/MissingWidth 0`, not a
600-unit fallback. Native regressions cover inline and indirect width/descriptor
objects; the actual packaged Chromium worker places `World` at **154.672 pt**,
matching the document widths. The original is now pinned in the corpus.

## Renderer fixes (2026-11-11)

- Path coordinates now capture the CTM when each segment is constructed,
  including rectangles, curves and clipping paths. Stroke pen/dash metrics
  still use the CTM at paint time.
- `h` restores the current point to the subpath start, including for a following
  `v` curve.
- Text is emitted in content-stream order and participates in path clipping;
  later opaque graphics can cover earlier text.
- Verification: `dotnet run --project tests/Rbt.Pdf.Tests` passes synthetic-PDF
  geometry/order regressions and native raster pixel checks for transformed
  fills/clips, `Q` restoration, text overpainting and text clipping. Browser
  screenshots were not checked in this pass.

## Corpus-driven fixes (2026-11-11)

- Shared page geometry now applies inherited CropBox and Rotate, intersects crop
  with MediaBox, and translates nonzero page origins. Browser placeholders,
  rendered commands and selection bounds use the same viewport.
- Text uses the full text matrix and CTM, including rise, horizontal scaling,
  reflection and spacing. Glyph positions and subsequent `Tj`/`TJ` advances use
  the PDF widths rather than mixing them with substitute-font advances.
- Form XObjects clip their content to their transformed BBox.
- `sh` supports axial DeviceGray/RGB/CMYK gradients with type 2/type 3 functions,
  component-function arrays, Domain, Extend and BBox. Nonlinear functions use
  sampled gradient stops. Canvas now paints the emitted linear gradients.
- Fable restores use the dependency packages' FSharp.Core minimum version.
- The original `synth-path-ctm` fixture placed `cm` inside path construction.
  That is invalid PDF syntax, not a reliable visual oracle; the fixture and
  regression now set the CTM before constructing the path. See the
  [PDF Association ruling](https://github.com/pdf-association/pdf-issues/issues/563).
- Verification: regression executable passed; 12 affected corpus PDFs / 16
  sampled pages completed in native and Poppler renderers. Paired images confirm
  the corrected path fixture, page origin, text transforms and axial gradient.
  Chromium screenshots/pixels verified gradient clipping, transformed text and
  rotated pages; crop/origin/rotation canvas sizes match browser layout.
- Radial/mesh shading, patterns, text rendering modes and image/font gaps were
  addressed in the subsequent pass below. Substitute glyph outlines can still
  differ from the document font.

## Codec, font and paint coverage

- Added radial and mesh shadings (types 3–7), colored/uncolored tiling patterns,
  shading patterns, and patterned strokes. Tiling cells are rasterized once per
  paint and repeated as a surface, avoiding fractional-zoom cell-edge seams.
- Text rendering modes now stroke outlines and accumulate the union of text
  clipping glyphs until `ET`. Ordinary solid fill text retains glyph batching.
  Type 3 charprocs use their FontMatrix, and font resolution no longer aliases
  different font objects/documents with the same BaseFont name.
- Image decoding covers packed and 16-bit samples, Decode arrays, Indexed and
  CMYK samples, explicit JPEG ColorTransform, stencils, color-key/soft masks and
  Matte unblending. Native image placement preserves arbitrary affine transforms
  and `/Interpolate false`; alpha is not premultiplied twice.
- CCITT Group 3/4, JBIG2 and JPX filters have managed F# implementations usable
  natively and through Fable. Existing JPEG and bit-reader primitives come from
  Rbt.Image; the reusable MQ arithmetic decoder was added there. PDF filter
  parameters, codec integration and image semantics remain in rbt-pdf. No
  Render.NET or pdf.js runtime dependency was added.
- Mixed-length CID CMaps, Differences names and bundled Japanese/Chinese maps
  resolve text; Noto Sans CJK supplies missing Unicode glyphs.
- Verification: 23 affected documents / 25 sampled pages completed in native and
  Poppler; six image cases were rerendered after CMYK/alpha/sampling corrections.
  Native-only `bug_jpx.pdf` also renders; its known Poppler crash was not rerun.
  Native/Poppler contact sheets were visually inspected. Chromium checks covered
  text modes, radial/mesh shading, tiling patterns, CMYK JPEG, JPX alpha, JBIG2,
  CCITT and Japanese text. Browser canvas captures must be composited over the
  viewer's white page background, not interpreted as opaque raw RGB.
- `jpx-smaskindata.pdf` is not a reliable Poppler alpha oracle: extracted JPX
  samples match FFmpeg exactly, but Poppler displays incorrect embedded-alpha
  colors. With associated alpha and Matte, use `(C' - (1-a)M) / a` before color
  conversion and final premultiplication (PDF 32000-1 §11.6.5.3). The first
  half-transparent white sample over gray is RGB 191,191,191, not green.

### Assets, packaging and remaining limits

- Rbt.Image **0.1.5**, Rbt.Text **0.1.7**, and
  Rbt.Renderer/Core/Canvas **0.1.14** are published through the normal protected
  Forge tag actions. The earlier missing-native-package incident was fixed in
  Forge; no manual rollout is needed for these library releases.
- Codec ports retain Apache-2.0 attribution and the bundled PDF.js license.
  Adobe CMaps retain their upstream redistribution notices.
- [Noto Sans CJK SC](https://github.com/notofonts/noto-cjk/blob/main/Sans/OTF/SimplifiedChinese/NotoSansCJKsc-Regular.otf)
  is bundled with its OFL license; SHA-256:
  `2c76254f6fc379fddfce0a7e84fb5385bb135d3e399294f6eeb6680d0365b74b`.
  Workers fetch and parse this font only after encountering a missing CJK glyph;
  Latin-only documents do not download it, and zooming reuses it.
- Custom RenderOptions now supply a `Rasterize` callback for transparent pattern
  cells; native options and the browser app provide their respective backends.
- Shading functions support types 0/2/3/4, including two-input function-based
  shading type 1 with Domain, Matrix and BBox. Sampled functions support all legal
  sample bit depths, linear and cubic interpolation, Encode/Decode and range
  clipping. Cubic `/Order 3` uses tensor-product uniform Catmull–Rom with secant
  endpoint tangents; dimensions smaller than four samples fall back to linear.
  Calculator functions use bounded parsing, typed operands and reusable execution
  storage. Limits include eight sampled
  dimensions, one million grid points, 64 outputs, and a 1,024-entry calculator
  stack with 100,000 execution steps. Patch meshes still use fixed 16×16
  tessellation; shading/pattern surfaces are capped at four million pixels.
- ICC support covers Gray/RGB matrix/TRC and RGB/CMYK `mft1`/`mft2` A2B0 profiles,
  explicit RGB B2A0 LUT transforms, and calibrated Gray/RGB/Lab spaces.
  Unsupported profiles use the PDF Alternate; this is not a complete ICC implementation.
- JPX does not support every profile: COC, ROI, progression changes, packed
  headers, palettes/component mappings and some codeblock styles are rejected.
- The 36 bundled CMaps remain a selected subset. Vertical CID `W2`/`DW2`,
  inherited CMaps and mixed-width codes now have focused regressions.
- Supplied normal annotation appearances render with state selection, visibility,
  placement, Form clipping and transparency. Missing text-field, checkbox and
  radio appearances are synthesized without replacing valid supplied appearances.
  Text synthesis uses AcroForm fonts and source encodings; hidden widgets remain
  invisible. Interactive editing, pushbutton/choice/signature synthesis, rich-text
  layout and NoZoom/NoRotate remain unsupported.
- Transparency supports the standard blend modes, isolated/non-isolated and
  knockout groups, and alpha/luminosity soft masks. Page and Form groups support
  DeviceRGB, invertible RGB matrix/TRC ICC spaces, and RGB LUT profiles with
  supported explicit B2A0 transforms on Raster, Canvas and GL.
  Source paints retain their original components until conversion into the active
  group space; the same ICC stream stays encoded without a lossy round trip.
  Profile conversion belongs to Image and compositing to Renderer.
  RGB matrix/TRC and LUT luminosity masks composite in the declared group space
  before XYZ Y extraction and transfer; backdrop color also applies outside the BBox.
  Managed alpha masks and DeviceGray soft masks remain supported. RGB/CMYK
  `mft1`/`mft2` ICC sources convert directly through unclipped D50 XYZ into managed
  RGB groups. CMYK group blending and missing/unsupported LUT inverses
  remain unsupported.
  GL supports managed groups and custom pixel filters through CPU readback
  compositing, including PDF luminosity and transfer masks. `/AIS` separates
  geometric coverage from painted opacity for paths, text, patterns, images and
  nested groups. Arbitrary geometry-expanding Renderer filters remain outside
  the PDF coverage contract. These checks are regression evidence, not PDF
  conformance certification.

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
The original document's URL/hash/local copy could not be recovered in the current
checkout, local files or available history. Public lookalikes do not establish
the cause. Recover the original PDF before choosing a reduced regression.


## Dense-document rendering — moved to workers
PDF parsing, font loading, interpretation and Canvas painting now run in a module
worker. The UI receives transferred ImageBitmaps and primitive text-box tuples;
it does not import the PDF renderer to reconstruct selection records.
Generation checks discard stale zoom results; replacing or disposing a viewer
terminates its worker. Offscreen pages retain layout but release canvas pixels.
A dense mesh document rendered while a 16 ms UI heartbeat continued: 60 ticks,
maximum observed gap 19.3 ms. A separate 500-page scenario reached the last page,
returned to the first through rapid zoom changes, and evicted offscreen canvas
pixels; its largest observed heartbeat gap was 26.8 ms. These are scenario checks,
not universal latency or adversarial-resource bounds.

## Shared graphics ownership and current proof

- `rbt-image` owns arithmetic decoding and ICC/profile color conversion.
- `rbt-renderer` owns affine image sampling, premultiplied paint handling,
  blend/group compositing, explicit closed-stroke joins, and the reusable
  `Rbt.Renderer.Canvas` backend (including worker-safe canvas/glyph utilities).
- `rbt-pdf` retains PDF codecs/filter parameters, color-space dictionaries,
  transparency interpretation, CMaps/metrics, worker lifecycle and selection UI.
- Fifteen affected native/Poppler pages completed. Thirteen pages were also
  captured from the packaged worker viewer; native/Canvas color samples agree,
  including tinted ICC LUT16 Lab values. All 16 blend overlap samples differ by
  at most one byte between native and Canvas.
- Poppler ignores knockout in the generated group fixture and ignores ICC
  `/Range` for its default image Decode. Use the declared fixture mathematics,
  not those reference pixels. Native and Canvas agree on the corrected results.
- LittleCMS supplies independent ICC expectations: 50 shared-library checks pass.
  Native and browser vertical text agree; Poppler lacks the fixture's substitute
  glyphs, so selection metrics are checked directly rather than against its boxes.
- Final worker lifecycle checks preserve vertical `ABCD` selection, reject a
  superseded open, retain one replacement worker, and leave zero workers/canvases
  after disposal. Multipage scrolling releases offscreen canvas pixels. A main
  target CDP script inventory contains no PDF renderer scripts; worker resource
  requests alone must not be mistaken for main-thread script execution.

## Current font and graphics fixes

- Rbt.Text **0.1.7** accepts glyph-indexed TrueType subsets without `cmap`.
  Arabic CID text retains its actual outlines instead of falling back after a
  font-load exception. Explicit empty or short CIDToGIDMap streams map missing
  entries to glyph zero; only absent or Identity maps imply identity.
- PDF pages blend inside an isolated page group, not against the caller's
  background. Page knockout is honored. Shared Renderer **0.1.6** preserves the
  working/initial backdrop through nested transparency groups, including their
  own blend mode, opacity and mask.
- Canvas no longer promotes positive fractional strokes to hairlines. Native
  zero-width outlines now retain a device hairline under shrinking transforms.
  The dense Form and reduced stroke fixtures distinguish these two defects.
- PDF JPEG `/Decode` maps the decoded components directly; standalone Adobe
  JPEG polarity conventions must not invert them again. The original CMYK
  photograph now retains its colors, with explicit/default Decode regressions.
- Core PDF Flate decoding reuses Rbt.Image's bounded inflater on both backends,
  retaining actual decoded prefixes from damaged streams rather than feeding
  compressed bytes to the content parser. Indirect `/Contents` arrays are
  dereferenced before concatenation. The original truncated-stream document now
  paints its recovered text in native and browser output.
- Chromium completed all **65 documents / 74 sampled pages** through the actual
  packaged worker. Original Arabic text, PDF 2.0 spacing, transparency and dense
  Forms have targeted visual/pixel evidence. Completion alone is not conformance
  or pixel-perfect parity with Poppler.

## ICC-space and cubic follow-up

- RGB matrix/TRC page and Form groups now blend in their declared profile space.
  Source color provenance survives Form inheritance; images, shadings and patterns
  convert directly through Image's D50 PCS API into the active working space.
  Renderer converts the group's contribution before parent-space blending,
  opacity and masks. Display-only groups avoid identity color conversions.
- Cubic sampled functions now have bounded tensor-product evaluation without
  per-evaluation allocation. Small dimensions retain linear interpolation.
- A new mask regression exposed packed blending's `/256` approximation; exact
  rounded `/255` now preserves opaque destination alpha.
- The corpus has 71 documents / 592 pages. All 25 affected native and Chromium
  pages completed. Independent LittleCMS expectations match managed-color
  interior samples within two channel values; numeric and visual checks cover
  cubic peaks, wide-gamut inheritance, nested groups, masks and boundary opacity.
  Shared Image's 51 ICC checks and the native PDF regression executable pass.

## ICC luminosity and GL follow-up

- Luminosity masks preserve encoded group components through compositing,
  honoring isolation, knockout and the opaque group-space backdrop. ICC masks
  then use D50 XYZ Y; device spaces retain their device-dependent gray rule.
  Transfer applies after luminosity extraction, including outside the BBox.
  Alpha-mask transfer outside the BBox uses zero, not backdrop luminosity.
- The corpus now has 73 documents / 594 pages. All 14 targeted native and
  Chromium pages completed; new luminosity samples match independent
  gamma/matrix expectations within two channel values. Native regressions cover
  knockout, isolation, backdrop defaults, clipping and transfer boundaries.
- Renderer owns GL managed-group backdrop/contribution conversion; no PDF or
  ICC parsing was added there. Eleven live Mesa llvmpipe EGL cases passed
  against independent expectations and Raster probes, covering isolation,
  knockout, nested groups, mask/opacity, blend mode and clip-state restoration.
  That pass verified software Mesa, not a physical GPU; custom GL pixel filters
  were still unsupported at that release.

## LUT source colors and GL filters follow-up

- Image exposes unclipped D50 XYZ PCS for supported RGB/CMYK lut8/lut16 A2B0
  profiles, including XYZ and legacy Lab encodings. PDF supplies all source
  components and converts into the managed RGB blending space before blending.
- Renderer owns full-frame custom GL callbacks, nested stencil preservation and
  balanced gradient interval selection. PDF-specific mask and color semantics
  remain here; profile interpretation remains in Image.
- The corpus has 78 documents / 599 pages. Fourteen targeted documents rendered
  on native Raster, live Mesa llvmpipe GL and Chromium Canvas. Independent pixel
  checks cover LUT colors, masks, nested clips and knockout combinations; dense
  CMYK gradients match native within one channel value across 640 samples.
  Ten generic live EGL callback scenarios and 77 Image ICC checks also passed.
- At that release, LUT inverse transforms, four-component CMYK group blending
  and `/AIS true` remained unsupported. GL callbacks use CPU readback/upload,
  not GPU execution.

## RGB LUT blending and premultiplication follow-up

- Image interprets explicit RGB lut8/lut16 B2A0 transforms with XYZ or legacy
  Lab PCS. It does not numerically invert forward LUTs. Malformed or missing
  inverses leave supported forward transforms usable.
- PDF preserves components for the same ICC profile stream, including Indexed
  paints and nested groups. Different group profiles still convert through PCS.
  Non-reciprocal test tables expose accidental forward/inverse round trips.
- Native rendering exposed downward premultiplication rounding in Renderer,
  amplified by Lab conversion. Renderer now uses integer rounded division by
  255; a consumer regression checks constant-color source-over at every channel
  value and alpha. ICC parsing remains in Image; PDF semantics remain here.
- The corpus has 81 documents / 602 pages. Twelve targeted pages passed native
  Raster, live Mesa llvmpipe GL and Chromium Canvas checks. New page/Form/mask
  samples match independent LittleCMS and luminosity expectations within two
  channel levels. The pass also covers opacity, Multiply, alpha masks, nested
  same/different profiles and inverse luminosity transfer. Image has 109 passing
  ICC checks. GL verification uses software Mesa, not a physical GPU.

## Alpha-is-shape closure

- PDF tracks `/AIS` through graphics-state save/restore and stream boundaries.
  Alpha constants and soft masks affect shape only when AIS is true; stencil,
  color-key and explicit image masks retain intrinsic geometric coverage.
- Renderer owns lazy `WithShape` commands and shared `Cmd.shapeOf` extraction
  across Raster, GL and Canvas. Group coverage unions child shapes rather than
  deriving coverage from final painted alpha. PDF owns image-mask precedence,
  pattern initial state and transparency-group semantics.
- Four new fixtures bring the corpus to 85 documents / 606 pages. Nineteen
  targeted pages rendered on all three backends; 348 AIS pixel samples and 66
  retained ICC samples match independent expectations within two channel levels.
  Thirteen generic shape scenarios pass on native and live llvmpipe GL; native
  regressions and Fable compilation pass.

## Incidental (throwaway scripts, not product bugs)
- Native PPM writer dropped the R-channel mask (`v >>> 16` without `&&& 0xFFu`),
  painting everything red. Fixed in the script, not the product.
- `?url=` param only reads on full page load; in-page dropdown switching via
  dispatchEvent needs `{bubbles:true}`. Dev-shell quirks, not product bugs.

## Full-corpus comparison and font/appearance fixes

- All 85 PDFs / 606 pages were rendered against Poppler: native completed 606,
  Poppler 605, with its known malformed-JPX SIGSEGV as the only execution failure.
  All paired dimensions matched. This is execution coverage, not 606 visual passes.
- Visually triaged all 105 paired non-stress pages. All 500 stress pages passed
  color, bounds and OCR page-number checks in both renderers. Browser baseline:
  all 53 upstream pages.
- Fixed simple-font source decoding incorrectly following a wider ToUnicode
  codespace, missing format-0 symbolic TrueType glyph maps, and omitted supplied
  annotation appearances. Appearance painting reuses normal Form semantics.
- Fixed shared GL text rasterization at untransformed font sizes. Axis-aligned
  glyphs use device resolution and preserve horizontal stretch; other transformed
  small outline fonts use scalable distance fields.
- Seven affected/control documents / eight pages were checked on native, Poppler,
  browser and live software GL. Native regressions, Fable compilation and twelve
  generic GL text checks pass. Widget appearance synthesis remains unsupported.
- Large JPX and JBIG2 differences were not blindly “fixed”: independent JPX alpha
  calculations agree with our pixels, and all 8,415,000 JBIG2 source pixels match
  Poppler extraction. The latter differs in display reduction filtering. See the
  corpus README for per-page evidence and unresolved reference/quality differences.

## Widget appearances and shared reduction (0.1.16)

- Missing text-field, checkbox and radio appearances now synthesize from inherited
  field state; valid supplied appearances remain authoritative. Caption styling,
  numeric `/Opt` child states, unison radios and absent/empty border colors are covered.
- The fractional premultiplied resampling kernel lives in Image 0.1.5.
  Renderer 0.1.14 owns transformed reduction sizing and Raster/Canvas/GL dispatch;
  PDF opts into reduction without changing decoded source pixels.
- The final full run covers 85 PDFs / 606 native pages, with 605 Poppler pairs.
  Only the known Poppler JPX crash remains an execution failure. Eighteen changed
  pages were visually reviewed; all 500 stress-page native outputs remain byte-identical.
- Nine focused pages run on all three backends with independent widget/image/alpha
  samples and browser worker-disposal checks. Dense JBIG2 comparison error improves
  16.2%; other sampling metrics are mixed. Cross-backend thin-line differences remain.
- Detailed evidence and remaining form limitations are in
  `tests/render-corpus/README.md`, “Missing widget appearances and image reduction”.

## Canvas extent and fractional glyph fixes (0.1.17)

- Remove binary64 rounding noise before allocating browser page canvases; genuine
  fractional extents still round upward. Both formerly oversized corpus canvases
  now match pdf.js at DPR 1.25.
- Pin Renderer/Core/Canvas 0.1.15. Its shared CPU glyph fix preserves fractional
  outline sizes instead of using rounded integer-ppem bitmap cache entries.
  The direct-outline regressions pass at 12.25, 12.75 and 0.75 pixels with and
  without explicit AA. Image remains 0.1.5; Text remains 0.1.7.
- All 106 non-stress pages / 84 PDFs complete in the browser against pdf.js
  6.5.438, with all canvas dimensions matching. The other 104 Fable captures
  remain pixel-identical; the two corrected canvases retain their common pixels.
- Independent ICC blending and subpixel stroke-coverage oracles support Fable's
  disputed transparency and dense-Form output. No PDF color or Canvas stroke
  changes were made merely to match pdf.js. Font/fallback differences and
  Chromium's fractional clip-edge coverage remain documented limitations.
- Reproducible comparison tooling and detailed evidence are described in
  `tests/render-corpus/README.md`.

## Browser reliability and lifecycle (0.1.18)

- Correct Fable CMap hexadecimal parsing when values begin with `0b`. Explicit
  `0x` prefixes preserve Adobe CID-to-Unicode mappings; GBK `c4f8` renders and
  extracts as `镍` rather than `Äø`. Five browser mapping cases and the full GBK
  specimen row pass; native regressions and Fable compilation pass.
- Evict canvas/text-layer DOM nodes, not only their contents, and release page
  placeholders on viewer disposal. A single 500-page session retains seven live
  canvas/text-layer pairs at settled checkpoints instead of 500 visited pairs.
  Page/color checks, rapid navigation/zoom, replacement, and disposal pass;
  562 delivered bitmaps close and all four workers terminate.
- Controlled embedded Latin/CJK comparisons separate rasterization from font
  substitution. No shared font/graphics correction was justified by their
  remaining pixel differences.
- Two full 106-page passes in a dedicated Chromium process reproduce all Fable
  pixels and 105 pdf.js pages exactly. The dense-Form reference retains 25 varying
  pixels with maximum channel delta 3; no tolerance or pixel rewriting hides it.
  Only the CJK specimen changes in Fable versus the preceding source baseline.
- Reproduction steps, environment limits and evidence are in the corpus README,
  “Browser reliability, controlled fonts and 500-page lifecycle”.
- Add five size/hash-pinned real documents: a 229-page digital manual, a 443-page
  scanned book, and Devanagari, Chinese and Arabic reports. The manifest records
  source-specific rights; downloads remain outside the repository and packages.
- Handle mismatched PDF/JBIG2 page dimensions by composing their intersection
  with the PDF output stride. The real scan and mask decode identically to
  Poppler across 24,461,236 pixels. Precompute overflow-safe coordinate windows
  instead of doing per-pixel 64-bit arithmetic in Fable's JBIG2 loops.
- Support DeviceCMYK and bidirectional ICC CMYK transparency groups, retaining
  the black component through images, text, paths, patterns, shadings and
  luminosity masks. Generic four-component compositing lives in Renderer 0.1.16;
  explicit four-output ICC inverses live in Image 0.1.6.
- Evaluate nested component groups once against their live backdrop and reuse
  compositor scratch surfaces. Avoid eagerly decoding browser JPEG previews
  that the native-component PDF image path never consumes.
- Bound the viewer to one outstanding render and prioritize actual viewport
  overlap before nearby prefetch pages. The final 500-page run checks every
  page, closes all 530 delivered bitmaps, terminates all four workers, and
  observes a peak of one pending render.
- Final real-document workloads complete without rendering errors. Eight
  four-component Canvas scenes match native pixels exactly; the existing
  106-page browser corpus remains pixel-identical to the prior corrected source.
  The previously rejected UNESCO CMYK cover now renders against both references.
  Image-heavy pages remain slow; zoom discards stale results but cannot interrupt
  a synchronous decode already executing in the worker.
- A controlled, post-GC worker comparison on Chinese GEM page 3 retains 42.1%
  less V8 backing storage than 0.1.17 with identical full-canvas pixels and text
  length. This is scoped worker storage, not total browser memory.
