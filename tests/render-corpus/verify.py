#!/usr/bin/env python3
"""Check corpus integrity and render sampled pages with Poppler and Rbt.Pdf."""
import argparse
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parent


def run(command, timeout):
    started = time.monotonic()
    try:
        result = subprocess.run(command, capture_output=True, text=True, timeout=timeout)
        return {"ok": result.returncode == 0, "exitCode": result.returncode,
                "seconds": round(time.monotonic() - started, 3),
                "stdout": result.stdout[-8000:], "stderr": result.stderr[-8000:]}
    except subprocess.TimeoutExpired:
        return {"ok": False, "error": "timeout", "seconds": timeout}
    except OSError as error:
        return {"ok": False, "error": str(error)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ids", nargs="+", help="Only these manifest IDs")
    parser.add_argument("--reference-only", action="store_true")
    parser.add_argument("--all-pages", action="store_true", help="Default: first, middle, last")
    parser.add_argument("--max-side", type=int, default=800)
    parser.add_argument("--timeout", type=int, default=60, help="Seconds per subprocess")
    parser.add_argument("--jobs", type=int, default=2)
    args = parser.parse_args()
    if min(args.max_side, args.timeout, args.jobs) < 1:
        parser.error("max-side, timeout and jobs must be positive")
    entries = []
    for manifest in ("upstream.json", "synthetic.json"):
        entries.extend(json.loads((ROOT / manifest).read_text())["files"])
    ids = [entry["id"] for entry in entries]
    if len(ids) != len(set(ids)):
        parser.error("Duplicate corpus IDs")
    if args.ids:
        missing = set(args.ids) - set(ids)
        if missing:
            parser.error(f"Unknown IDs: {sorted(missing)}")
        entries = [entry for entry in entries if entry["id"] in args.ids]
    # Validate everything before launching renderers; missing/changed PDFs are not
    # rendering failures and must not silently shrink the corpus.
    for entry in entries:
        data = (ROOT / entry["path"]).read_bytes()
        if len(data) != entry["bytes"] or hashlib.sha256(data).hexdigest() != entry["sha256"]:
            raise ValueError(f"Integrity mismatch: {entry['path']}")
        if b"%PDF-" not in data[:1024]:
            raise ValueError(f"Not a PDF: {entry['path']}")
    output = ROOT / "results"
    output.mkdir(exist_ok=True)
    dll = ROOT / "bin/Release/net10.0/RenderCorpus.dll"
    if not args.reference_only:
        subprocess.run(["dotnet", "build", str(ROOT / "RenderCorpus.fsproj"),
                        "-c", "Release", "--nologo"], check=True, timeout=300)

    def verify(entry):
        source = str(ROOT / entry["path"])
        record = {"id": entry["id"], "path": entry["path"], "sha256": entry["sha256"]}
        info = run(["pdfinfo", source], args.timeout)
        record["pdfinfo"] = info
        match = re.search(r"^Pages:\s+(\d+)", info.get("stdout", ""), re.MULTILINE)
        if not info["ok"] or not match:
            record["ok"] = False
            return record
        count = int(match.group(1))
        record["pageCount"] = count
        pages = list(range(1, count + 1)) if args.all_pages else sorted({1, (count + 1) // 2, count})
        record["pages"] = []
        folder = output / entry["id"]
        folder.mkdir(exist_ok=True)
        for page in pages:
            prefix = folder / f"page-{page}-reference"
            native = folder / f"page-{page}-native.ppm"
            # Never mistake an earlier successful image for this run's output.
            prefix.with_suffix(".ppm").unlink(missing_ok=True)
            native.unlink(missing_ok=True)
            reference = run(["pdftoppm", "-cropbox", "-f", str(page), "-l", str(page), "-singlefile",
                             "-scale-to", str(args.max_side), source, str(prefix)], args.timeout)
            reference["ok"] = reference["ok"] and prefix.with_suffix(".ppm").is_file()
            result = {"page": page, "reference": reference}
            if not args.reference_only:
                result["native"] = run(["dotnet", str(dll), source, str(page),
                                        str(args.max_side), str(native)], args.timeout)
                result["native"]["ok"] = result["native"]["ok"] and native.is_file()
            record["pages"].append(result)
        record["ok"] = all(page["reference"]["ok"] and page.get("native", {"ok": True})["ok"]
                           for page in record["pages"])
        print(f"{'ok' if record['ok'] else 'FAIL'} {entry['id']}: {len(pages)}/{count} pages", flush=True)
        return record

    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        records = list(pool.map(verify, entries))
    report = {"maxSide": args.max_side, "sampling": "all" if args.all_pages else "first-middle-last",
              "referenceOnly": args.reference_only,
              "note": "Success means rendering completed, not visual equivalence. Inspect paired images.",
              "files": records}
    report_path = output / "report.json"
    report_path.write_text(json.dumps(report, indent=2) + "\n")
    failed = sum(not record["ok"] for record in records)
    print(f"{len(records)} files verified; {failed} failures. Report: {report_path}")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
