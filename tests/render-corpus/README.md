# PDF rendering corpus

The collection contains **71 PDFs (3,198,409 bytes)**: 30 Mozilla pdf.js
fixtures pinned to commit `94a48f3c844d4677b264b6e7c0a1e239c245189f`,
one PDF Association PDF 2.0 example, and 40 generated fixtures.

Coverage includes embedded/subset/Type 3/CID fonts, Arabic and CJK text,
JPEG/JPEG2000/JBIG2, packed images and masks, transparency/blending,
shading/patterns, Form XObjects and AcroForms, offset page boxes/rotation,
text state and clipping, malformed streams, and multipage/performance cases.
The collection has **592 pages**, including a generated 500-page document for
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
Adobe-profile transparency and nonisolated gray-mask documents. GL compiled but
was not exercised live; it explicitly rejects managed-color groups.
