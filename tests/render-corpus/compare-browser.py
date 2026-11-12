#!/usr/bin/env python3
"""Compare captured Fable/pdfjs browser PNGs without changing image geometry.

Requires Pillow and NumPy in the Python environment; neither is installed here.
"""

import argparse
from contextlib import ExitStack
import json
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw, ImageFilter, ImageFont, ImageOps


REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_REPORT = Path(__file__).resolve().parent / "results/browser-parity/report.json"
PAGES_PER_SHEET = 4
PANEL_WIDTH = 480
PANEL_HEIGHT = 330
GAP = 18
ROW_HEIGHT = 410
SHEET_WIDTH = 3 * PANEL_WIDTH + 4 * GAP
INK_CUTOFF = 245


def source_path(value):
    """The capture report refers to PNGs from the repository root."""
    if not isinstance(value, str) or not value:
        raise ValueError("missing PNG path")
    path = Path(value)
    if path.is_absolute():
        raise ValueError(f"PNG path must be repository-relative: {value}")
    resolved = (REPO_ROOT / path).resolve()
    if not resolved.is_relative_to(REPO_ROOT):
        raise ValueError(f"PNG path escapes repository: {value}")
    return resolved


def ink_stats(rgb):
    ink = np.any(rgb < INK_CUTOFF, axis=2)
    ys, xs = np.nonzero(ink)
    return {
        "fraction": float(ink.mean()),
        "bbox": [int(xs.min()), int(ys.min()), int(xs.max()) + 1, int(ys.max()) + 1]
        if xs.size else None,
    }


def paired_metrics(fable, pdfjs):
    a = np.asarray(fable)
    b = np.asarray(pdfjs)
    differences = np.abs(a.astype(np.int16) - b.astype(np.int16))
    max_channel = differences.max(axis=2)
    # BOX reduction and a fixed one-pixel blur in the reduced coordinate system
    # suppress small AA/registration differences; this is NOT an alignment fix.
    reduced_size = (max(1, fable.width // 4), max(1, fable.height // 4))
    smooth_a = np.asarray(fable.resize(reduced_size, Image.Resampling.BOX)
                          .filter(ImageFilter.GaussianBlur(1)), dtype=np.int16)
    smooth_b = np.asarray(pdfjs.resize(reduced_size, Image.Resampling.BOX)
                          .filter(ImageFilter.GaussianBlur(1)), dtype=np.int16)
    metrics = {
        "mae": float(differences.mean()),
        "pixelsMaxChannelGt16Percent": float(np.count_nonzero(max_channel > 16) * 100 / max_channel.size),
        "blurredQuarterMae": float(np.abs(smooth_a - smooth_b).mean()),
        "fableInk": ink_stats(a),
        "pdfjsInk": ink_stats(b),
    }
    # Display only: white means identical, increasingly red means larger differences.
    intensity = np.minimum(max_channel.astype(np.uint16) * 4, 255).astype(np.uint8)
    diff = ImageOps.colorize(Image.fromarray(intensity), black="white", white="#b00000")
    return metrics, diff


def font(size, bold=False):
    try:
        return ImageFont.truetype("DejaVuSans-Bold.ttf" if bold else "DejaVuSans.ttf", size)
    except OSError:
        return ImageFont.load_default(size=size)


def draw_panel(sheet, image, x, y, label, small_font):
    draw = ImageDraw.Draw(sheet)
    draw.text((x, y), label, fill="#202020", font=small_font)
    top = y + 23
    draw.rectangle((x, top, x + PANEL_WIDTH - 1, top + PANEL_HEIGHT - 1), outline="#bbbbbb")
    if image is None:
        draw.text((x + 14, top + 15), "No comparable image", fill="#666666", font=small_font)
    else:
        preview = image.copy()
        if preview.mode != "RGB":
            preview = preview.convert("RGB")  # Sheet preview only; no metric compares this conversion.
        preview.thumbnail((PANEL_WIDTH - 2, PANEL_HEIGHT - 2), Image.Resampling.LANCZOS)
        sheet.paste(preview, (x + (PANEL_WIDTH - preview.width) // 2,
                              top + (PANEL_HEIGHT - preview.height) // 2))


def draw_row(sheet, row, index, slot, images, diff, title_font, body_font):
    y = 20 + slot * ROW_HEIGHT
    draw = ImageDraw.Draw(sheet)
    name = f"#{index + 1:03d}  {row['id']}  page {row['page']}"
    draw.text((GAP, y), name, fill="#181818", font=title_font)
    metrics = row.get("metrics")
    if metrics:
        detail = (f"MAE {metrics['mae']:.3f}  |  >16 {metrics['pixelsMaxChannelGt16Percent']:.2f}%"
                  f"  |  blurred 1/4 MAE {metrics['blurredQuarterMae']:.3f}")
    else:
        detail = f"{row['status']}: {row.get('reason', 'no pixel comparison')}"
    draw.text((GAP, y + 24), detail[:155], fill="#444444", font=body_font)
    labels = ("Fable", "pdfjs", "Diff (max-channel x4; white = identical)")
    for column, image in enumerate((images.get("fable"), images.get("pdfjs"), diff)):
        side = ("fable", "pdfjs")[column] if column < 2 else None
        suffix = ""
        if side is not None:
            info = row[side]
            suffix = f"  {info.get('size')}  {info.get('mode') or ''}"
            if info.get("mode") == "RGBA" and not info.get("alphaOpaque"):
                image = None  # Never flatten nonopaque alpha, even for a sheet preview.
                suffix += "  (nonopaque; preview omitted)"
            if info.get("imageError"):
                suffix = f"  {info['imageError']}"
            elif not info.get("ok"):
                suffix += "  (render failed)"
        x = GAP + column * (PANEL_WIDTH + GAP)
        draw_panel(sheet, image, x, y + 50, labels[column] + suffix, body_font)


def analyze_page(page, stack):
    row = {"id": page["id"], "page": page["page"]}
    images = {}
    for side in ("fable", "pdfjs"):
        capture = page[side]
        info = {key: capture.get(key) for key in
                ("ok", "width", "height", "path", "text", "seconds", "error") if key in capture}
        row[side] = info
        try:
            path = source_path(capture.get("path"))
            image = stack.enter_context(Image.open(path))
            image.load()  # Detect truncated/corrupt PNGs before recording the image as usable.
            if image.format != "PNG":
                raise ValueError(f"not a PNG: {image.format}")
            images[side] = image
            info["size"] = list(image.size)
            info["mode"] = image.mode
            if image.mode == "RGBA":
                info["alphaOpaque"] = image.getchannel("A").getextrema() == (255, 255)
        except (OSError, ValueError) as exc:
            info["imageError"] = str(exc)
    a, b = row["fable"], row["pdfjs"]
    row["reportedDimensionMismatch"] = (a.get("width"), a.get("height")) != (b.get("width"), b.get("height"))
    diff = None
    if not a.get("ok") or not b.get("ok"):
        row["status"] = "render_failure"
        row["reason"] = "; ".join(f"{side}: {row[side].get('error') or 'render failed'}"
                                  for side in ("fable", "pdfjs") if not row[side].get("ok"))
    elif "fable" not in images or "pdfjs" not in images:
        row["status"] = "image_error"
        row["reason"] = "; ".join(f"{side}: {row[side]['imageError']}"
                                  for side in ("fable", "pdfjs") if "imageError" in row[side])
    elif images["fable"].size != images["pdfjs"].size:
        row["status"] = "dimension_mismatch"
        row["reason"] = f"PNG sizes {a['size']} vs {b['size']}; no rescaling"
    elif any(info.get("mode") not in ("RGB", "RGBA") or
             (info.get("mode") == "RGBA" and not info.get("alphaOpaque"))
             for info in (a, b)):
        row["status"] = "non_rgb"
        row["reason"] = (f"PNG modes {a['mode']} vs {b['mode']}; only RGB or fully opaque RGBA "
                         "accepted; nonopaque alpha is not composited")
    else:
        row["status"] = "paired"
        fable = images["fable"].convert("RGB") if a["mode"] == "RGBA" else images["fable"]
        pdfjs = images["pdfjs"].convert("RGB") if b["mode"] == "RGBA" else images["pdfjs"]
        row["metrics"], diff = paired_metrics(fable, pdfjs)
    return row, images, diff


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("report", nargs="?", type=Path, default=DEFAULT_REPORT,
                        help=f"capture report JSON (default: {DEFAULT_REPORT}); requires Pillow and NumPy")
    args = parser.parse_args()
    report_path = args.report.resolve()
    report = json.loads(report_path.read_text(encoding="utf-8"))
    if not isinstance(report.get("pages"), list):
        parser.error("report must contain a pages array")
    output = report_path.parent
    output.mkdir(parents=True, exist_ok=True)
    title_font = font(19, bold=True)
    body_font = font(16)
    rows = []
    sheets = []
    sheet = None
    for index, page in enumerate(report["pages"]):
        slot = index % PAGES_PER_SHEET
        if slot == 0:
            sheet = Image.new("RGB", (SHEET_WIDTH, 20 + ROW_HEIGHT * min(PAGES_PER_SHEET, len(report["pages"]) - index)), "white")
        with ExitStack() as stack:
            row, images, diff = analyze_page(page, stack)
            sheet_name = f"contact-{index // PAGES_PER_SHEET + 1:03d}.png"
            row["contactSheet"] = sheet_name
            row["contactRow"] = slot + 1
            draw_row(sheet, row, index, slot, images, diff, title_font, body_font)
            rows.append(row)
        if slot == PAGES_PER_SHEET - 1 or index == len(report["pages"]) - 1:
            sheet.save(output / sheet_name)
            sheets.append(sheet_name)
            sheet.close()
    paired = [row for row in rows if row["status"] == "paired"]
    ranking = sorted(paired, key=lambda row: (-row["metrics"]["mae"], row["id"], row["page"]))
    other = sorted((row for row in rows if row["status"] != "paired"),
                   key=lambda row: (row["status"], row["id"], row["page"]))
    counts = {status: sum(row["status"] == status for row in rows)
              for status in ("paired", "render_failure", "image_error", "dimension_mismatch", "non_rgb")}
    result = {
        "sourceReport": str(report_path),
        "versions": report.get("versions"),
        "definitions": {
            "pixelInput": "RGB or RGBA with every alpha byte 255; opaque alpha is dropped without compositing, original PNG mode retained per side",
            "mae": "Mean absolute per-channel RGB difference (0-255), original pixels",
            "pixelsMaxChannelGt16Percent": "Percent of pixels whose largest absolute RGB channel difference exceeds 16",
            "ink": f"Fraction and [left,top,right,bottom) bbox of pixels with any RGB channel < {INK_CUTOFF}; null bbox if blank",
            "blurredQuarterMae": "RGB MAE after independent BOX downsampling to (max(1,width//4),max(1,height//4)) and Gaussian blur radius 1 in reduced pixels; heuristic only, no registration",
            "ranking": "Comparable pairs descending by raw MAE; no pass/fail threshold",
        },
        "counts": counts,
        "contactSheets": sheets,
        "pages": rows,
        "ranking": [{"rank": rank, "id": row["id"], "page": row["page"],
                     "fablePath": row["fable"].get("path"), "pdfjsPath": row["pdfjs"].get("path"),
                     "contactSheet": row["contactSheet"], "contactRow": row["contactRow"],
                     **row["metrics"]}
                    for rank, row in enumerate(ranking, 1)],
        "notCompared": [{"id": row["id"], "page": row["page"], "status": row["status"],
                         "reason": row["reason"], "fablePath": row["fable"].get("path"),
                         "pdfjsPath": row["pdfjs"].get("path"), "contactSheet": row["contactSheet"],
                         "contactRow": row["contactRow"]} for row in other],
    }
    (output / "metrics.json").write_text(json.dumps(result, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    print(f"{len(rows)} pages: {counts}; {len(sheets)} contact sheets; {output / 'metrics.json'}")


if __name__ == "__main__":
    main()
