"""Evaluate aggregate Tesseract behavior on committed synthetic fixtures."""

from __future__ import annotations

import argparse
import json
import tempfile
from pathlib import Path

from PIL import Image

from benchmark_tesseract_initial_epi import _match_anchors, _render_pages, _run_tsv, _unique_expected


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--fixtures", type=Path, required=True)
    parser.add_argument("--template", type=Path, required=True)
    parser.add_argument("--tesseract", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    blank_pages = _render_pages(args.template, 200 / 72)
    levels = ("clean", "moderate", "borderline")
    rows = []
    with tempfile.TemporaryDirectory(prefix="bcapture-tesseract-synthetic-") as temporary:
        temp_dir = Path(temporary)
        for level in levels:
            page_rows = []
            for page_number in (1, 6):
                image = Path(args.fixtures) / f"initial_epi_{level}_page_{page_number:02d}.jpg"
                observed = _run_tsv(args.tesseract, Image.open(image).convert("RGB"), temp_dir, f"{level}_{page_number}", 6)
                expected_tokens = _run_tsv(args.tesseract, blank_pages[page_number - 1], temp_dir, f"blank_{page_number}", 6)
                anchors = _match_anchors(_unique_expected(expected_tokens), observed)
                page_rows.append(
                    {
                        "page": page_number,
                        "tokens": len(observed),
                        "anchors_expected": anchors["expected"],
                        "anchors_recovered": anchors["recovered"],
                        "anchor_confidence_min": anchors["confidence_min"],
                        "anchor_confidence_max": anchors["confidence_max"],
                    }
                )
            rows.append({"level": level, "pages": page_rows})
    args.output.write_text(json.dumps({"privacy_note": "Synthetic fixtures only; OCR text is omitted.", "levels": rows}, indent=2) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
