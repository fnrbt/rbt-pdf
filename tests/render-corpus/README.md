# PDF rendering corpus

The collection contains **85 PDFs (3,281,868 bytes)**: 30 Mozilla pdf.js
fixtures pinned to commit `94a48f3c844d4677b264b6e7c0a1e239c245189f`,
one PDF Association PDF 2.0 example, and 54 generated fixtures.

Coverage includes embedded/subset/Type 3/CID fonts, Arabic and CJK text,
JPEG/JPEG2000/JBIG2, packed images and masks, transparency/blending,
shading/patterns, Form XObjects and AcroForms, offset page boxes/rotation,
text state and clipping, malformed streams, and multipage/performance cases.
The collection has **606 pages**, including a generated 500-page document for
scrolling, zoom cancellation and offscreen canvas eviction checks.

Two complementary sets:

- `upstream.json`: curated upstream regression/real-world documents, with pinned
  URLs, SHA-256 hashes, coverage tags and provenance. See each entry's license
  field; availability in an upstream test suite is not a blanket redistribution
  license for the individual document.
- `synthetic.json`: deterministic, locally generated feature-isolation PDFs.
  Each entry describes the expected picture. `generate.py` is the fixture source.

PDFs and render outputs are local artifacts, excluded from git and package
publishing. Commit the scripts and manifests; recreate the artifacts as below.
No private/customer documents belong in this corpus.

## Obtain the PDFs

From the repository root (Python 3 standard library; network needed only for
upstream downloads):

```sh
python3 tests/render-corpus/fetch.py
python3 tests/render-corpus/generate.py
```

The upstream manifest pins the exact downloaded bytes. The generator produces
stable bytes and rewrites the synthetic manifest with their hashes.

## Render and inspect

Prerequisites: .NET 10 SDK, `pdfinfo` and `pdftoppm` (Poppler), and the repository's
`assets/NotoSans-Regular.ttf`. The native renderer project uses the same package
references as the product. The browser renderer is not exercised by this driver.

```sh
python3 tests/render-corpus/verify.py
```
Poppler uses `-cropbox` to match a viewer's visible page rather than the full
MediaBox. Native output retains whatever page geometry the product implements;
do not normalize away differences in size or orientation.


This verifies all PDF sizes/hashes before rendering, builds the native runner
once, and samples the first, middle and last pages of each document (one page
for single-page documents). Each renderer invocation has a 60-second timeout;
two documents run concurrently. Each native page runs in a fresh process so
font caches or crashes cannot contaminate other documents.

Outputs are paired `page-N-reference.ppm` and `page-N-native.ppm` under
`results/<id>/`. `results/report.json` records page counts, sampled pages,
subprocess status, duration, and diagnostics. Failed invocations cause a nonzero
exit status; they do not silently remove corpus entries. Previous sampled-page
images are deleted before rendering so failed runs do not leave stale successes.

Useful options:

- `--reference-only`: validate and produce Poppler references without .NET.
- `--ids ID [ID ...]`: focus on manifest IDs.
- `--all-pages`: render every page, including large multipage documents.
- `--max-side 1600`: raise output resolution (default 800 pixels).
- `--timeout 120 --jobs 1`: adjust time limit/concurrency for dense documents.

**A successful render is not a visual pass.** Compare paired images against the
synthetic `expected` descriptions and Poppler. Antialiasing and substitute fonts
can differ legitimately; there is deliberately no arbitrary pixel-difference
pass threshold. Page geometry differences, blank/missing content and incorrect
paint order remain failures even when both processes exit successfully.

The corpus is a regression starting point, not a PDF conformance suite. It does
not establish browser Canvas parity, interactive annotation/form support,
password handling, accessibility, or bounded resource use for adversarial PDFs.

## Initial verification (2026-11-11)

All 47 PDFs passed size/hash checks; regenerating the 17 synthetic files preserved
their exact bytes. The collection has 69 pages; default sampling exercised 54.
The native runner built without warnings or errors.

| Renderer | Completed sampled pages | Failed sampled pages |
| --- | ---: | ---: |
| Poppler 26.01.0 | 53 | 1 |
| Rbt.Pdf native | 54 | 0 |

`upstream-bug-jpx` crashes Poppler with SIGSEGV after malformed JP2/J2K warnings.
Keep it as a robustness fixture, **not** as a usable Poppler visual oracle.
The verifier intentionally returns failure for this case rather than masking it.

Visual inspection of four paired samples found:

- `synth-path-ctm`: native and Poppler trapezoids differ; needs investigation.
- `synth-media-offset`: native output is blank, while Poppler shows both markers.
- `synth-text-transform`: native loses text rotation/rise and differs in spacing.
- `synth-axial-gradient`: native output is blank, while Poppler shows the gradient.

These are baseline observations, not fixes or visual approvals of the remaining
fixtures. The generated `results/comparison-sample.png` records those four pairs
(Poppler left, native right, in the order above).

## After corpus-driven fixes (2026-11-11)

Rechecked 12 affected documents / 16 sampled pages: both native and Poppler
completed all of them. Regression tests also pass for crop origin/rotation,
text transforms and PDF advances, transformed Form BBox clipping, and nonlinear
axial shading with asymmetric extension.

The path fixture was corrected: its original mid-path `cm` was invalid PDF
syntax, so the previous implementation disagreement was not a conformance
failure. The updated fixture sets CTM before path construction; both renderers
now show the intended trapezoid. See the
[PDF Association ruling](https://github.com/pdf-association/pdf-issues/issues/563).

Native/Poppler pairs now show the nonzero-origin markers, rotated/raised/spaced
text and axial gradient correctly (`results/comparison-fixed.png`). Substitute
font outlines and antialiasing are not expected to match pixel-for-pixel.

The Fable viewer was also checked in Chromium: gradient endpoint colors and clip
pixels, crop/origin/rotation layout and canvas dimensions, and screenshots of
rotated text/pages. No browser errors were reported in these scenarios.

## Codec, font and paint follow-up

The next pass exercised 23 affected PDFs / 25 sampled pages in both native and
Poppler renderers, followed by six image rerenders and two pattern rerenders
after corrections. All those invocations completed. Paired images were inspected
for radial/mesh shading, tiling/shading patterns, text modes, fonts, CMYK JPEG,
packed samples and masks. `synth-images-fax` adds deterministic Group 3/4 coverage.

Chromium surface checks cover text stroking/clipping, radial/mesh shading,
tiling patterns, CMYK JPEG, JPX embedded alpha, JBIG2, fax and Japanese text.
An additional pixel-checked scenario exercises patterned strokes at fractional
zoom and a rotated four-color image. Captured transparent canvas pixels are
composited over white before comparisons, matching the viewer page background.
Native regressions cover these paint/image/text boundaries, Type 3 advances,
font identity and multilevel JPEG2000 tile reconstruction.

The JPX corpus alone was too weak to establish wavelet correctness: its alpha
examples have no decomposition levels and its malformed example decodes black.
Generated gradient/checker codestreams exercise multilevel reversible and
irreversible wavelets, tiling and color transforms. FFmpeg comparisons found and
fixed odd-origin tile parity and irreversible low-pass scaling errors; seven
64×48 RGB cases then matched all 9,216 decoded bytes exactly.

Do not treat Poppler as infallible: `jpx-smaskindata` disagrees with the correct
embedded-alpha/Matte calculation despite its decoded JPX samples matching
FFmpeg. Native and browser outputs agree on the corrected colors. The known
Poppler-crashing `bug_jpx` was rendered natively only, not rerun in Poppler.

See [renderer notes](../../NOTES-render-compare.md#codec-font-and-paint-coverage)
for implementation boundaries, bundled font/CMap provenance and remaining
unsupported features. Rbt.Image 0.1.1 and Renderer/Core/Canvas 0.1.5 are published;
fresh-cache registry-only native regressions and the Fable package build pass.
These targeted browser checks do not certify parity across the full corpus.

Both durable tiled JPX regressions also pass through the real Chromium viewer:
all 9,216 RGB samples match the source image for the reversible fixture and the
independent FFmpeg output for the irreversible, color-transformed fixture.

## Transparency, color and worker follow-up

Ten additional generated fixtures isolate all 16 blend modes, isolated and
non-isolated knockout groups, alpha/luminosity graphics masks, explicit/default
vertical CID metrics, and ICC Gray/RGB/CMYK profiles including Range/Decode.
`icc_profiles.py` builds the profiles using only the standard library; its optional
LittleCMS oracle supplies independent expected colors for shared Rbt.Image tests.

Fifteen affected native/Poppler pages completed. Thirteen actual worker-rendered
pages were captured and checked against native output, including text modes,
fax, soft masks and the new color/vertical fixtures. Blend overlap samples differ
by at most one byte; image, fill and stroke samples agree for the ICC profiles.
The native regression suite also distinguishes explicit closed-stroke joins from
open paths that merely return to their start.

Two reference limitations matter: Poppler ignores knockout in this group fixture,
and ignores the ICC Range when deriving the default image Decode. The knockout
overlap is green with retained blue backdrop, not a red/green mixture; the bounded
Gray image must use the same component range as its vector counterpart.

Chromium worker resource entries confirm no CJK font fetch for Latin-only pages,
one CJK fetch for Japanese text, and reuse through rapid zoom changes. A dense
mesh page rendered with a live UI heartbeat (maximum observed gap 19.3 ms).
These are targeted behavior checks, not a full-corpus browser certification.

## Full-corpus and release pass

The collection now has 65 documents and 586 pages. First/middle/last sampling
exercises **74 pages**, not every page of the 500-page fixture.

- Native and the actual Chromium worker completed all 74 sampled pages.
  The paired native/Poppler run covered 64 documents / 73 pages; the known
  Poppler-crashing `upstream-bug-jpx` was checked only natively and in Chromium.
- The full set of sampled comparisons was inspected. Targeted follow-up checks
  cover Arabic cmap-less CID fonts, PDF 2.0 MissingWidth, nested transparency,
  dense Form strokes, zero-width hairlines, CMYK JPEG Decode, and an indirect
  Contents array containing truncated Flate data. Native regressions cover the
  corresponding boundaries.
- Two-input sampled/calculator shading and calculator branches now have reduced
  fixtures alongside the upstream function-based shading example.
- The 500-page viewer scenario reaches page 500, returns to page 1 after rapid
  zoom changes, evicts offscreen canvas pixels, and terminates its worker on
  disposal. The maximum observed heartbeat gap was 26.8 ms, not a general bound.
- Fresh-cache, registry-only builds use Image 0.1.1, Text 0.1.7, and
  Renderer/Core/Canvas 0.1.6. Browser module requests confirm the published
  Canvas package rather than a local checkout.

Disable the browser HTTP cache when rebuilding modules at the same URL; stale
worker imports can otherwise make a corrected build look unchanged. Composite
transparent canvas captures over white before comparing the viewer surface.

The upstream function-based shading example's lower-left calculator uses real
`floor` results with integer-only `mod`, without `cvi`. It is rejected; Poppler's
black cell is not a successful-function oracle. See the
[PDF Reference's Type 4 operator table](https://files.connorslade.com/Books/PDF%201.4%20Refrence/B-Operators_in_Type_4_Functions.pdf).
Retain the earlier reference caveats for JPX alpha, knockout, vertical substitute
glyphs and ICC Range. Unprofiled CMYK color, font substitution and antialiasing
can differ from Poppler. The subsequent ICC/cubic pass below extends this proof;
interactive form support remains outside it.

## ICC blending and cubic sampled functions

Six additional fixtures isolate linear-RGB and Adobe-RGB page blending across
vector, image, shading and pattern paints; isolated and inherited nonisolated
Form groups; source-color inheritance; group opacity and alpha masks; and
one-/two-input cubic sampled functions. Nonisolated Form groups inherit their
parent's blending space rather than declaring a different one.

`icc_profiles.py` includes an independent LittleCMS oracle for the final group
components: linear red/half-blue maps near `(188,0,187)`, while Adobe green with
quarter-red maps near `(0,192,0)`. Converting and clipping each source to sRGB
before blending gives the wrong answer. Poppler's output for these fixtures
uses the unconverted group components, so it is not their color oracle.

For cubic interpolation, [ISO 32000-1 §7.10.2](https://opensource.adobe.com/dc-acrobat-sdk-docs/pdfstandards/PDF32000_2008.pdf)
specifies Order 3 and fallback when Size is below four, but does not specify
unique endpoint conditions. This implementation uses uniform Catmull–Rom with
first/last secant tangents. The four-knot fixture's decoded midpoint is 0.9;
linear interpolation gives 0.8, as observed in the Poppler reference. Numeric
regressions cover endpoints, mixed cubic/linear dimensions, reversed Encode,
Decode/Range clipping and invalid orders.

Verification: all **25 affected PDFs / 25 pages** completed natively and in the
actual Chromium worker. The four managed-color fixtures' interior samples match
the independent expectations within two channel values; both cubic midpoints
match the expected peak. Native regressions cover the group boundary, knockout,
mask/opacity, exact `/255` packed source-over and DeviceGray mask compatibility.
Shared Image ICC checks pass (51 checks). Contact sheets include the original
Adobe-profile transparency and nonisolated gray-mask documents. That pass only
compiled GL; the subsequent live-GL verification is described below.

## ICC luminosity and live GL follow-up

Two additional fixtures distinguish encoded ICC components from luminosity:
Adobe RGB half gray has XYZ Y near 0.218, not 0.5. They cover opaque and
half-transparent paints, white/default-black backdrops, isolated/nonisolated
Multiply, clipping to the mask BBox, inverse transfer and the backdrop outside
the BBox. The group composites in its encoded space before final XYZ Y
extraction and transfer, as required by
[PDF Reference §§7.4.2 and 7.5.4](https://files.connorslade.com/Books/PDF%201.4%20Refrence/7-Transparency.pdf).

All **14 affected PDFs / 14 pages** completed natively and in the actual
Chromium worker. New fixture samples agree between those backends and with
independent gamma/matrix luminosity expectations within two channel values.
Native regressions additionally cover knockout and alpha-mask transfer outside
the BBox. The existing transparency/ICC contact sheet was visually inspected.

Shared Renderer GL color-managed groups passed 11 real surfaceless EGL cases
on Mesa llvmpipe: isolation/knockout, opacity, blend mode, mask, zero opacity,
nested groups, clipping and following paint. The implementation retains GL's
CPU readback/compositing path. At that release, custom pixel filters including
PDF luminosity and transfer masks were unsupported on GL.

## LUT source colors and GL pixel callbacks

Four fixtures place CMYK lut8/lut16 sources with XYZ/Lab PCS inside an Adobe RGB
page blending space. Each compares vector, image, shading and pattern paints.
LittleCMS independently converts source components into Adobe RGB before
compositing and converting the resulting group to display RGB. Lab fixtures
produce approximately `(173,89,89)` and `(173,87,87)`; both wide-gamut XYZ
fixtures produce `(0,140,0)`. Native, browser and GL samples match these expectations.
The fourth device component and out-of-sRGB-gamut PCS values must survive.

`synth-nested-clip-paints` intersects rectangular and diamond clips around
consecutive fills and shading, then checks parent restoration with blue and
magenta paints. Independent geometric samples distinguish a lost clip from
ordinary antialiasing differences.

GL custom callbacks use full-frame, top-left premultiplied pixels with CPU
readback/upload. They support empty-child backdrop seeding, mixed CPU/GPU filter
ordering, masks and transfer functions; they are not GPU-native arbitrary code.
ICC LUT sources are supported in managed RGB groups, but LUT inverse transforms
and four-component CMYK group blending remain outside this change.

Fourteen targeted documents completed on native Raster, live Mesa llvmpipe GL
and Chromium Canvas. Pixel expectations cover the LUT quadrants, luminosity and
backdrop masks, nested clipping, and isolated/non-isolated knockout groups.
The old CMYK lut8/lut16 fixtures also exposed a dense-gradient shader failure:
balanced interval selection preserves all 129 stops and eliminates the black
tail. Their 640 interior ramp samples match native within one channel value.
Ten additional live EGL cases verify callback ordering, replacement/in-place
arrays, premultiplication/orientation, empty seeding, transformed clips, masks,
backdrop filters, shadow order and exception propagation. These are software
Mesa checks, not physical-GPU verification.

## Explicit RGB LUT blending spaces

Three new fixtures cover bidirectional RGB lut8 XYZ page blending, lut16 Lab
Form blending, and LUT luminosity masks with inverse transfer. Page quadrants
compare vector/image/shading/pattern sources. Forms exercise same-profile
nesting, cross-profile conversion, nonisolated backdrop, opacity, Multiply and
alpha masks. Deliberately non-reciprocal B2A0 tables catch unwanted profile
round trips; they are not numerically derived inverses.

Image owns the explicit B2A0 transforms. PDF preserves components when the
source and working space name the same ICC stream and selects ordinary nested
groups in that case. Renderer owns rounded premultiplication: truncation had
changed constant encoded colors during native source-over, visible as a Lab
color shift. A regression covers every byte component and source alpha.

All **12 targeted PDFs / 12 pages** completed on native Raster, live Mesa
llvmpipe GL and the Chromium worker. LittleCMS group colors and independent
luminosity/transfer expectations match within two channel levels; the earlier
CMYK LUT source fixtures also retain their expected colors on all three
backends. The native regression suite, Fable build and 109 Image ICC checks
pass. This is software-GL and targeted browser evidence, not full-corpus
conformance. CMYK blending surfaces, unsupported/missing LUT inverse tables,
and `mAB`/`mBA` remain outside the supported subset.

## Alpha-is-shape knockout coverage

Four fixtures cover `/AIS` false/true in isolated and nonisolated knockout
groups: fills, strokes, shading, tiling patterns, Type 3 text, alpha/luminosity
masks and transfer functions, nested ICC groups, and image masks. Coverage is
independent of painted opacity, including fully transparent objects and
unpainted portions of group bounds and pattern cells.

Image cases distinguish stencil/color-key geometry from soft alpha and check
mask precedence. Embedded JPX alpha overrides image and graphics-state masks;
`SMaskInData` on non-JPX images is ignored. Stencils retain graphics-state
soft masks. The shared `ais-rgba-jpx.base64` resource is a lossless 32×8 RGBA
codestream generated with FFmpeg/libopenjpeg; an independent FFmpeg decode
matches all 1,024 source bytes. Precedence follows the
[ISO-approved clarification](https://github.com/pdf-association/pdf-issues/issues/215)
and [image dictionary errata](https://pdf-issues.pdfa.org/32000-2-2020/clause08.html).

All **19 targeted pages** rendered on native Raster, live Mesa llvmpipe GL and
the Chromium worker. Each backend matches 116 independently calculated AIS
pixel samples within two channel levels; 66 additional cross-backend ICC
samples retain their previous expectations. Thirteen shared Renderer scenarios
also pass on native and GL, including lazy shape evaluation, zero-opacity
knockout and nested managed groups. Native regressions and the Fable build pass.
Evidence is targeted and uses software GL, not physical-GPU or full-corpus
conformance certification.

## Full-corpus comparison

The 0.1.14 baseline ran `verify.py --all-pages --jobs 4` at an 800-pixel maximum
side on all **85 PDFs / 606 pages**, after manifest size/hash validation.
Native completed **606/606** pages; Poppler completed **605/606**. The sole
execution failure was Poppler's SIGSEGV on `upstream-bug-jpx`; no native
crashes, missing outputs or timeouts occurred. All 605 paired page dimensions
matched. Completion is not a visual pass.

All **105 paired non-stress pages** were visually triaged using ranked
reference/native/difference contact sheets, with enlarged inspections for sparse
text. All **500 stress pages**, in both renderers, passed rectangle-color,
outside-bounds and OCR checks against their individual `Page NNNN` labels.
The browser also rendered all **53 upstream pages**. This is not a full
606-page browser or GL run.

The baseline exposed missing tax-form labels, missing supplied annotation
appearances, and missing-glyph boxes in `upstream-rotated`. The latter ranked
near the bottom by mean pixel difference because its page is mostly blank:
numeric rankings alone would miss it.

Not every large difference warrants changing the renderer:

- All **8,415,000 JBIG2 source pixels** in the dense scan match independent
  Poppler extraction. The displayed difference is reduction filtering: native
  samples the binary image while Poppler's reference contains gray coverage.
- Independent FFmpeg JPX samples and scalar premultiplied/matte calculations
  match native, browser and GL at all 24 checked positions. Poppler's colors
  in `upstream-jpx-smaskindata` are not the expected output.
- ICC/AIS fixtures require their independent color/compositing oracles rather
  than Poppler equality. Existing ICC pixel expectations still match the
  full-run native images.
- The missing lower-left panel in `upstream-function-based-shading` uses
  `floor` on real values and then integer-only `mod`. Native/browser reject
  that invalid calculator program; Poppler paints a black panel. Neither is
  evidence of a correctly rendered checkerboard.
- `upstream-embedded-font` paints a Chinese glyph natively but not in Poppler;
  the reference disagreement remains unresolved. Font outlines, antialiasing,
  thin-stroke coverage, mesh tessellation and cubic interpolation also retain
  visible differences; they are not blanket visual approvals.

Local evidence: `results/full-corpus-baseline-report.json`,
`full-corpus-ranking.json`, `full-corpus-triage.json`, `full-triage-00.png`
through `full-triage-13.png`, `full-stress-page-checks.json`, and
`full-image-oracles.json`. These generated artifacts remain excluded from git.

### Corpus-driven corrections (PDF 0.1.15)

- Simple fonts consume one byte per character regardless of the `/ToUnicode`
  codespace. This restores labels and fine print in `upstream-160f-2019`.
- Embedded symbolic TrueType fonts accept format-0 byte-to-glyph cmaps,
  restoring `upstream-rotated` instead of painting missing-glyph boxes.
- Normal annotation appearance streams render with selected `/AS`, visibility
  flags, Rect placement and normal Form clipping/transparency semantics. Pages
  containing only annotations render too. This restores supplied tax-form
  fields and the controls in `upstream-form-two-pages`.
- Shared Renderer GL text uses device-resolution glyph bitmaps for axis-aligned
  scales, including horizontal stretch. Other transformed small outline text
  uses scalable distance fields. One-point PDF text no longer expands a
  one-pixel bitmap into blocks.

After correction, **7 affected/control PDFs / 8 pages** completed on native,
Poppler, browser and live software GL; their images were inspected. Native
regressions and Fable compilation pass. Twelve focused shared-Renderer GL
checks cover transformations and small-text scale equivalence. Browser disposal
checks terminate each worker and empty the viewer root.

That pass did **not** synthesize widget appearances when `/AP` or the selected
state was absent. Subsequent object inspection corrected the tax-form inventory:
its 16 widgets without appearances are **nine pushbuttons and seven hidden text
fields**, not 16 missing visible fields. The two missing `/Off` appearances belong
to **radio buttons**, not checkboxes. Hidden widgets must remain invisible.
Interactive forms and NoZoom/NoRotate were not established by that pass.
Previously listed reference/quality disagreements remain separately classified.

Post-fix evidence: `results/full-corpus-fixed-report.json`,
`full-fixed-backends.json`, and `full-fixed-backends.png`.

### Missing widget appearances and image reduction

Missing text-field, checkbox and radio appearances are synthesized; supplied
appearance streams and valid selected states remain authoritative. Text fields
use inherited field values, AcroForm font resources and default appearance,
including alignment, multiline wrapping, comb cells, rotation and password masking.
Font encoding uses actual source codes rather than replacing unsupported text
with question marks. Hidden widgets remain hidden.

Button synthesis respects `/MK` colors and captions, absent or empty border colors,
`/Opt` numeric child-state names and `RadiosInUnison`. In the tax form, the two
missing Off states have transparent borders and a cross caption; synthesizing
circles would add incorrect marks to the page's existing square boxes.
These rules follow PDF 32000-1 tables 189 and 227 and §12.7.4.2.3
([specification](https://opensource.adobe.com/dc-acrobat-sdk-docs/pdfstandards/PDF32000_2008.pdf)).

PDF image painting opts into device-scale area reduction. The shared
premultiplied region filter belongs to **Rbt.Image**; transformed sizing and
Raster/Canvas/GL dispatch belong to **Rbt.Renderer**. Decoded source pixels are
unchanged. The original renderer `DrawImage` sampling contract is unchanged.

This does not add interactive editing, pushbutton/choice/signature appearance
synthesis, rich-text `/RV` layout, or NoZoom/NoRotate support. Text synthesis needs
an available default-appearance font and representable source codes.

Verification for PDF **0.1.16**, using published Image **0.1.5** and
Renderer/Core/Canvas **0.1.14**:

- All **85 PDFs / 606 pages** rerendered natively. Poppler completed **605**;
  `upstream-bug-jpx` retains its reference-process SIGSEGV (exit -11), while native succeeds.
- All 605 paired dimensions match. All **18 changed pages** were visually compared
  with saved outputs and Poppler; all **500 stress-page native outputs are byte-identical**
  to the saved baseline, with unchanged comparison metrics.
- Dense JBIG2 mean absolute RGB error fell from **23.7860 to 19.9237** at 800 pixels
  (16.2%). Other sampling/antialiasing metrics are mixed, not universally better.
- **8 focused PDFs / 9 pages** completed on native, browser Canvas and Mesa GL.
  Widget appearance, source-image coverage, alpha and JPX samples passed independent
  pixel checks. Password appearance text remained masked; all eight browser workers
  terminated and their viewer roots emptied.
- Native regressions and Fable compilation pass. Six shared reduction cases pass
  on CPU, Canvas and GL. Image's six resampler checks pass; both target assemblies
  compile, and netstandard2.0 matches net10.0 on 18 JPEG/WebP/PNG frame/size cases.

This is not pixel parity: thin-line rendering still differs across backends,
notably the tax-form grid. Previously classified reference disagreements remain.
Final evidence: `results/widget-reduction-full-report.json`,
`widget-reduction-comparison.json`, `widget-final-backends.json`,
`widget-final-oracles.json`, `widget-final-browser-lifecycle.json`,
`widget-final-focus-backends.png`, `widget-final-triage-{0,1,2}.png`, and
`widget-tax-radio-detail.png`.

## Published Fable versus pdf.js

Compared **@rbt/pdf 0.1.16** with **pdfjs-dist 6.5.438**
([pinned upstream release metadata](https://registry.npmjs.org/pdfjs-dist/6.5.438))
in an actual Chromium browser. Both published tarballs were checked against their
SHA-512 registry integrity values. This uses the Fable `openPdf` viewer and its
real worker/page-view path, not the native renderer or a direct worker shortcut.
pdf.js uses its packaged worker, CMaps, standard fonts and WASM, with display
intent and canvas annotation appearances enabled.

Both completed **106 pages from 84 PDFs**, excluding only the 500-page stress
document. The target was 800 physical pixels on the longest side, using a shared
PDF-space scale; the recorded browser DPR was 1.25. **104 pairs have equal
dimensions** and five are pixel-identical. Two Fable outputs have one extra row
or column: `upstream-colorkeymask` is 566×801 versus 566×800, and
`synth-media-offset` is 801×534 versus 800×534. These are retained as geometry
differences, not resized or silently excluded.

Every page was visually reviewed. These categories describe differences, **not
a pass rate or a count of bugs in either reader**:

| Visual assessment | Pages |
|---|---:|
| Font or raster-quality differences | 52 |
| Visually close | 24 |
| Substantial content/color/shape disagreement | 27 |
| One-pixel canvas-size discrepancy | 2 |
| Unresolved malformed-function disagreement | 1 |

Concrete findings:

- Text and document layouts generally remain recognizable in both. Font weight,
  glyph shape and small-text rasterization differ. The dense non-embedded CJK
  example has text in both, but character-by-character equivalence is not established.
  pdf.js reported unavailable HeiseiMin-W3, KozMinPr6N-Regular and STSong-Light
  system fonts. Its missing `Fixture` font warnings also qualify the synthetic
  vertical-CID comparison; those files are not proof of general vertical-font failure.
- Thin-line/grid visibility remains different. The tax-form grid and dense
  Form/pattern example are useful follow-up cases. The latter has much darker
  patterned regions in pdf.js; Poppler resembles Fable, so it is not automatically
  a Fable defect.
- The biggest numerical differences are synthetic ICC blending/luminosity and
  AIS/mask cases. Corpus expectations often favor Fable, but the comparison alone
  does not certify either implementation. Keep independent numeric/specification
  checks rather than changing output merely to match pdf.js.
- The malformed calculator tile in `upstream-function-based-shading` is white in
  Fable, a checkerboard in pdf.js and black in Poppler. Its real-valued `floor`
  result is supplied to integer `mod`; this remains an invalid-input/reference
  disagreement, not established missing valid content.
- Both browser implementations render `upstream-bug-jpx` pixel-identically,
  despite the known Poppler crash. The JPX embedded-alpha example is also close
  to pdf.js, unlike the previously documented Poppler alpha disagreement.

The source follow-up below resolves the two canvas discrepancies and isolates
the real transparency and dense-Form disagreements without blindly matching
pdf.js. Font/fallback and raster-quality differences remain. This deliberately
feature-focused corpus does not measure general pdf.js conformance parity, interactive forms, accessibility,
security or large-document performance. Recorded durations are diagnostic only:
the Fable viewer may pre-render neighboring pages while pdf.js renders the
requested page.

Evidence lives under `results/browser-parity/`: `packages.json`, `cases.json`,
`report.json`, `metrics.json`, `visual-review.json`, `browser-console.json`,
`contact-001.png` through `contact-027.png`, and the original per-page PNGs.
The visual-review attribution notes are hypotheses where independent correctness
has not been established.

### Source corrections and independent oracles

The source viewer now removes binary64 arithmetic noise before rounding canvas
extents upward. At DPR 1.25, `upstream-colorkeymask` is 566×800 rather than 566×801,
and `synth-media-offset` is 800×534 rather than 801×534. Genuine fractional
extents still round upward: a 300×200-point probe rendered with maximum side
800.001 or 800.25 allocates 801×534; a maximum side of 0.75 allocates 1×1.
This is an allocation correction, not a clip-antialiasing change: Chromium's
plain Canvas clip-plus-layer drawing independently drops the probe's quarter-pixel
right-edge coverage, unlike unclipped drawing.

The full 106-page / 84-PDF browser comparison was rerun against pdf.js 6.5.438.
Both engines completed every page and all 106 canvas dimensions now match.
All 104 previously equal-size Fable captures are pixel-identical to the published
baseline; the two resized captures preserve every pixel in their common extent.
The pdf.js rerender itself changed pixels on 46 pages, so reference capture
stability is not assumed. Evidence is separate under `results/browser-parity-fix/`:
`report.json`, `metrics.json`, `baseline-pixel-comparison.json`,
`verification-summary.json`, boundary probes and 27 contact sheets. This is a
source build, not a newly published package.

Independent inspection did not justify changing the disputed PDF semantics:

- `upstream-transparency-group` declares an Adobe RGB (1998) ICC blending space.
  At pixel (355,250), decoded shading functions, `/Difference` in that space and
  LittleCMS conversion predict RGB (0,187,57); Fable gives (0,186,56).
  Applying Difference after converting operands to sRGB instead predicts
  (48,192,68), close to Poppler (48,191,68) and pdf.js (46,191,68).
- `upstream-perf-structure-forms` contains 3,072 opaque stars, not a transparency
  or pattern operation in the disputed region. Its transformed black stroke is
  0.105 pt, approximately 0.106 browser pixel. A 64×64 geometric coverage oracle
  at (150,100) finds 7.50% black-stroke coverage. Fable's (85,101,176) and Poppler's
  (89,99,159) do not support thickening strokes to match pdf.js's (16,20,34).

A separate shared CPU text defect was fixed in `../rbt-renderer`: fractional-size
outline glyphs no longer use integer-ppem bitmap cache entries that resize their
ink independently of layout. Direct-outline regressions pass at 12.25, 12.75
and 0.75 pixels, with and without an explicit AA wrapper. This does not change
Canvas output, and no Canvas font/stroke change was justified by this investigation.

### Replaying browser comparisons

Unpack `npm pack @rbt/pdf@0.1.16` under
`results/browser-parity/vendor/rbt/`, and
`npm pack pdfjs-dist@6.5.438 --registry=https://registry.npmjs.org` under
`results/browser-parity/vendor/pdfjs/`, preserving each tarball's `package/`
directory. Serve the repository root over HTTP and open
`tests/render-corpus/browser-parity.html`. After `window.parity.ready`, call:

```js
await window.parity.render("/tests/render-corpus/downloads/pdf20-simple.pdf", 1, 800)
```

For a source build, run `pnpm run build` and open the harness with `?rbt=/`.
The `rbt` parameter selects a package root containing `dist/` and `assets/`;
without it, the pinned published package above remains the default. Save source
results separately from the published baseline.

The returned `fable` and `pdfjs` records contain status, original canvas size,
PNG data URL, text and diagnostic duration. Both outputs are displayed and their
documents disposed. Save decoded PNGs and their repository-relative paths in a
report with `versions` and `pages: [{id, page, fable, pdfjs}]`; retain failed pages.
`compare-browser.py [report.json]` generates metrics and all-page contact sheets
(requires Pillow and NumPy; this run used 12.3.0 and 2.5.4).
Opaque RGBA is compared as RGB without changing pixels; unequal geometries and
nonopaque images are explicitly excluded from pixel metrics. Raw error and the
quarter-size blurred metric rank inspection work; neither is a correctness threshold.

## Browser reliability, controlled fonts and 500-page lifecycle

The post-0.1.17 source fixes were checked with a Fable build, the native regression
runner, two complete 106-page browser comparison passes, and a separate 500-page
viewer session. This covers all 606 corpus pages in the browser, but only the
106 non-stress pages were compared with pdf.js in this run.

### Reference repeatability

The managed Chromium environment produced different text pixels across repeated
pdf.js document loads. Identical `fillText` arguments and identical font bytes
also reproduced variation without either PDF renderer. Waiting for
`document.fonts.ready`, reloading the document, and requesting a readback-oriented
Canvas did not eliminate it. This is evidence against attributing those
differences to PDF parsing or a font-loading race; the exact managed-browser
configuration responsible was not established.

Use a dedicated, fresh Chromium process for comparison captures. The measured
environment was Chrome **156.0.8078.79**, Linux, DPR **1.25**, software rasterization,
pdf.js **6.5.438**, and unchanged display intent/font handling. For example, after
serving the repository and building the source viewer:

```sh
profile=$(mktemp -d)
google-chrome --headless=new --disable-gpu --force-color-profile=srgb \
  --force-device-scale-factor=1.25 --user-data-dir="$profile" \
  --remote-debugging-address=127.0.0.1 --remote-debugging-port=9238
```

Connect browser automation to that process and open
`browser-parity.html?rbt=/`. Record the actual browser version, DPR and package
versions, rather than assuming requested emulation settings took effect.
The default reference Canvas profile is `display`; `&referenceCanvas=readback`
is available for diagnostics and is recorded with the actual context attributes.
The harness now awaits pdf.js document destruction before starting the next case.

Across two full passes, all **106 Fable** captures were pixel-identical between
runs. **105 pdf.js** captures were identical; `upstream-perf-structure-forms`
differed at **25 pixels**, with maximum channel delta **3**, in the dense graphic
region. This remaining reference variation is retained, not filtered or counted
as a Fable defect. Evidence: `results/browser-reliability/findings.json`,
`clean-pass1.json`, `clean-pass2.json`, `repeat-summary.json` and original PNGs.

### Controlled fonts and CMap correction

The embedded controls use the same PDF/font bytes in both engines:
`Embedded_font.pdf` contains subset NotoSansTC-DemiLight, while the selected
TrueType line in `mixedfonts.pdf` uses embedded DejaVuSans. In isolated-browser
captures at maximum side 1200, the DejaVuSans line has identical thresholded ink
bounds `[66,314–378,327]`; the CJK glyph `除` has bounds `[232,154–267,192]`
in Fable and `[232,154–267,191]` in pdf.js. Raster masks still differ; these
measurements do not establish an advance or missing-glyph defect. Crop coordinates,
thresholds and captures are in `results/controlled-fonts/isolated/report.json`.
The nonembedded KozMin/STSong specimen remains a separate fallback-font comparison.

That specimen exposed a real Fable CMap bug: the integer parser interpreted an
unprefixed leading `0b` as binary even when hexadecimal was requested. Prefixing
CMap hex values explicitly with `0x` preserves CID keys such as `0b5b`, so GBK
`c4f8` now maps to `镍`, not the spurious Latin text `Äø`.
`font-regression.html?rbt=/` checks actual Adobe CMap resolution for five source
codes and the viewer's complete expected GBK row. Inspect `window.fontRegression`
for completion and results. Its source run passes at DPR 1.25.
Only the dense nonembedded CJK page changed among the 106 Fable captures versus
the preceding source baseline; the other 105 remain pixel-identical.

### Long-document lifecycle

Open `browser-stress.html` after building, then call `await window.stress.run()`.
An optional `?rbt=<package-root>` selects a package containing `dist/` and `assets/`.
The harness checks every page's number and alternating fill color in one
500-page viewer session, then exercises reverse jumps, jump/zoom bursts, zoom with
pending work, replacement during opening/rendering, and disposal during rendering.
It observes real worker messages and bitmap closes without retaining bitmaps.

The baseline passed rendering but retained 500 empty canvas/text-layer pairs after
navigation. Page eviction now removes those layers and clears their references;
disposal also releases the page-placeholder array held by an externally retained
viewer. At each 50-page settled checkpoint, the fixed run retained **7 canvas and
7 text-layer nodes**, matching the live pages, with **3,360,000 canvas backing bytes**.
All **500 distinct pages** passed. All **562 delivered bitmaps** were closed;
**4 workers** were created and terminated, with no worker errors or survivors.
Disposal left no page/layer nodes or canvas backing bytes.

These are scoped canvas/DOM/lifecycle measurements, not a total browser-memory
bound. Heap telemetry may be shared with other tabs. Stale zoom results are
discarded; document replacement/disposal terminates workers. The check does not
claim that zoom interrupts synchronous rendering already executing in a worker.
Evidence: `results/browser-stress/baseline-0.1.17-bitmap.json` and
`results/browser-stress/source-verified.json`.

## Real-world document coverage

The `real-world` entries in `upstream.json` pin five additional documents by byte
size and SHA-256. Run `python3 tests/render-corpus/fetch.py` to fetch or verify
them. The per-file download limit is 64 MiB; pinned sizes and hashes remain
mandatory. Downloaded documents are not included in the repository or packages.

| Document | Pages | Coverage | Representative pages |
| --- | ---: | --- | --- |
| GNU Make manual | 229 | Long born-digital text/code, embedded Type 1 fonts | 10, 100, 220 |
| The Jungle, 1920 imprint | 443 | Scanned book, JBIG2 images/masks, OCR text | 10, 100, 400 |
| UNESCO Bhasha Matters | 185 | Devanagari, mixed embedded fonts, photographs and CMYK JPEG | 1, 35, 45, 78, 87 |
| UNESCO Chinese GEM introduction | 10 | Embedded Chinese CID fonts, complex layout, CMYK photographs | 3, 5, 8 |
| UNESCO Arabic gender report | 70 | Embedded Arabic CID fonts, RTL text, charts | 10, 30, 60 |

Provenance and rights are recorded per manifest entry. Public availability is not
a redistribution license: the Chinese report states all rights reserved; the
Arabic report's CC license covers its text, not all photographs or other content.
The GNU manual uses the GFDL, Bhasha Matters uses CC BY-SA 3.0 IGO with third-party
exceptions, and the scanned book's public-domain status is jurisdiction-dependent.
These documents add real workload diversity; representative-page measurements do
not constitute a full-document pixel comparison or general PDF conformance proof.

### Real-document measurements and fixes (0.1.18)

`browser-real-world.html?rbt=/` exercises the source viewer; pass
`?rbt=/path/to/extracted/package/` to exercise an npm package. Wait for
`window.realWorld.ready`, then call `await window.realWorld.run()`. The report
separates open-ready time, worker response, installed visible canvas/text,
revisit, rapid navigation/zoom, replacement and disposal. Errors are retained.
`window.realWorld.run({documents: [...], timeoutMs: 90000})` selects workloads;
the default includes the five real documents and tracemonkey as a control.

Final source observations in managed Chromium 156, DPR 1.25:

| Document | Selected cold-page times (seconds) | Rendering errors |
| --- | --- | ---: |
| tracemonkey control | 0.28, 0.08, 0.16 | 0 |
| GNU Make | 0.24, 0.13, 0.24 | 0 |
| The Jungle | 6.24, 14.84, 17.09 | 0 |
| Bhasha Matters | 13.97, 2.13, 2.73, 4.89, 2.05 | 0 |
| Chinese GEM | 23.91, 2.24, 2.25 | 0 |
| Arabic gender report | 0.25, 0.38, 0.21 | 0 |

These are individual observed workloads, not statistical benchmarks or equal-work
speed comparisons with pdf.js. All six workloads finish with zero live workers,
pending renders, page/layer nodes and canvas backing bytes; delivered bitmaps are
closed. Source baseline and final reports are in `results/real-world/`.
Window heap figures are not a viewer-only total-memory bound.

The scan exposed a PDF/JBIG2 width mismatch (1034 versus 1035 pixels). The decoder
now composes the valid intersection without changing output stride. Two scan
images and their shared mask match Poppler across **24,461,236 decoded pixels**.
CPU profiling then identified generic JBIG2 bitmap decoding as the dominant scan
cost. Moving overflow-safe coordinate calculations outside pixel loops reduced
the observed page-400 cold visit from 32.77 to 17.09 seconds. The native regression
suite covers mismatched dimensions and extreme adaptive/refinement offsets.

The cover exposed valid four-channel ICC CMYK transparency with Multiply.
Image 0.1.6 supplies explicit CMYK B2A LUT inverses; Renderer 0.1.16 preserves four
components through group compositing on native and Canvas paths. PDF-specific
color spaces, codecs and paint interpretation remain here. DeviceCMYK groups,
patterns, shadings and luminosity masks also preserve K. Eight component scenes
(including nonseparable modes, knockout, clipping and nested boundaries) match
all native/Canvas pixels. GL integration compiles but was not runtime-exercised.

At 451×600, the actual cover's mean absolute RGB-channel difference is 2.55/255
against pdf.js and 3.90/255 against Poppler; maximum differences are 156 and 197.
No resampling or tolerance filtering hides those differences. Visual references
and measurements are `results/real-world/bhasha-cover-*.png`,
`bhasha-cover-reference.json` and `bhasha-cover-metrics.json`.

The final dedicated-Chromium 106-page run has no failures and every Fable image
is pixel-identical to the preceding corrected-source baseline. It is recorded
in `results/cmyk-release/corpus/report.json`; this is not a full-page comparison
of all pages in the five new long documents.

The final 500-page lifecycle run records 530 delivered/closed bitmaps, four
created/terminated workers and one peak pending render
(`results/real-world/source-final-stress.json`). The viewer selects actual
viewport overlap before prefetch and no longer queues parallel synchronous work.
JPEG preview preparation also excludes declared-color-space images whose PDF
decoder consumes native components instead. Expensive image/scan decoding remains
a performance limitation; stale-zoom handling does not preempt work already
executing synchronously in the worker.

Controlled Chinese GEM page-3 retention, after the visible canvas/text is
installed, all worker renders drain, and worker GC runs:

| Viewer | Worker V8 backing-storage bytes |
| --- | ---: |
| Released 0.1.17 | 90,019,434 |
| 0.1.18 source | 52,152,377 |

Both arms use the same viewport, DPR, document, zoom and page sequence; their
full-canvas pixel hashes and text lengths match. The reduction is 42.1%.
This is worker backing storage, not total browser memory or byte-for-byte
attribution to JPEG buffers. No timing inference is drawn from this experiment.
Evidence: `results/real-world/jpeg-retained-memory-ab.json`.
