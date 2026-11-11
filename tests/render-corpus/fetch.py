#!/usr/bin/env python3
"""Fetch missing pinned upstream PDFs and verify every local fixture."""

import hashlib
import json
import os
from pathlib import Path
import sys
import tempfile
import urllib.error
import urllib.request

ROOT = Path(__file__).resolve().parent
MAX_BYTES = 10 * 1024 * 1024


def verify(path, entry):
    digest = hashlib.sha256()
    size = 0
    with path.open("rb") as source:
        while chunk := source.read(64 * 1024):
            size += len(chunk)
            if size > entry["bytes"]:
                raise ValueError(f"{path}: exceeds expected size")
            digest.update(chunk)
    if size != entry["bytes"] or digest.hexdigest() != entry["sha256"]:
        raise ValueError(f"{path}: size or SHA-256 mismatch")


def fetch(entry):
    relative = Path(entry["path"])
    if len(relative.parts) != 2 or relative.parts[0] != "downloads" or relative.suffix != ".pdf":
        raise ValueError(f"Invalid fixture path: {relative}")
    size = entry["bytes"]
    if not isinstance(size, int) or isinstance(size, bool) or not 0 < size <= MAX_BYTES:
        raise ValueError(f"Invalid fixture size: {size}")
    target = ROOT / relative
    if target.exists():
        verify(target, entry)
        print(f"verified {relative}")
        return

    target.parent.mkdir(parents=True, exist_ok=True)
    temp_path = None
    try:
        request = urllib.request.Request(entry["url"], headers={"User-Agent": "rbt-pdf-render-corpus"})
        with urllib.request.urlopen(request, timeout=60) as response:
            with tempfile.NamedTemporaryFile(dir=target.parent, prefix=".fetch-", delete=False) as output:
                temp_path = Path(output.name)
                received = 0
                while chunk := response.read(64 * 1024):
                    received += len(chunk)
                    if received > size:
                        raise ValueError(f"{relative}: download exceeds expected size")
                    output.write(chunk)
        verify(temp_path, entry)
        os.replace(temp_path, target)
        print(f"downloaded {relative}")
    finally:
        if temp_path is not None:
            temp_path.unlink(missing_ok=True)


def main():
    manifest = json.loads((ROOT / "upstream.json").read_text(encoding="utf-8"))
    if manifest.get("schemaVersion") != 1 or not isinstance(manifest.get("files"), list):
        raise ValueError("Unsupported upstream manifest")
    for entry in manifest["files"]:
        fetch(entry)


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, KeyError, urllib.error.URLError) as exc:
        print(f"fetch failed: {exc}", file=sys.stderr)
        sys.exit(1)
