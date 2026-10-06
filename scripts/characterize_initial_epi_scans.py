"""Characterize image-only Initial Epi PDFs without emitting document text.

The utility is intentionally form-neutral and privacy-preserving. It assigns
stable case IDs from sorted input order and reports only PDF/page/image
metadata and aggregate text counts. It never prints source filenames, PDF
metadata values, OCR text, or extracted field values.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any

from pypdf import PdfReader


def _deref(value: Any) -> Any:
    return value.get_object() if hasattr(value, "get_object") else value


def _image_metadata(page: Any) -> list[dict[str, Any]]:
    resources = _deref(page.get("/Resources"))
    if not resources:
        return []
    xobjects = _deref(resources.get("/XObject"))
    if not xobjects:
        return []
    images: list[dict[str, Any]] = []
    for reference in xobjects.values():
        image = _deref(reference)
        if image.get("/Subtype") != "/Image":
            continue
        colorspace = _deref(image.get("/ColorSpace"))
        images.append(
            {
                "width": int(image.get("/Width", 0) or 0),
                "height": int(image.get("/Height", 0) or 0),
                "colorspace": str(colorspace),
                "bits_per_component": int(image.get("/BitsPerComponent", 0) or 0),
                "filter": str(image.get("/Filter")),
            }
        )
    return images


def _page_orientation(width: float, height: float) -> str:
    if abs(width - height) < 0.01:
        return "square"
    return "portrait" if height > width else "landscape"


def _dpi(image: dict[str, Any], width: float, height: float) -> tuple[float, float]:
    return (
        image["width"] / (width / 72.0) if width else 0.0,
        image["height"] / (height / 72.0) if height else 0.0,
    )


def _render_stats(path: Path, scale: float) -> list[dict[str, Any]]:
    import numpy as np
    import pypdfium2 as pdfium

    document = pdfium.PdfDocument(str(path))
    stats: list[dict[str, Any]] = []
    try:
        for page_number in range(len(document)):
            image = document.get_page(page_number).render(scale=scale).to_numpy()
            pixels = image[..., :3].astype(float)
            grayscale = pixels.mean(axis=2)
            stats.append(
                {
                    "page": page_number + 1,
                    "render_shape": list(image.shape),
                    "render_channels": int(image.shape[2]) if image.ndim == 3 else 1,
                    "mean_intensity": round(float(grayscale.mean()), 3),
                    "intensity_sd": round(float(grayscale.std()), 3),
                    "dark_pixel_fraction": round(float(np.mean(grayscale < 64)), 6),
                    "light_pixel_fraction": round(float(np.mean(grayscale > 245)), 6),
                }
            )
    finally:
        document.close()
    return stats


def characterize(path: Path, case_id: str, render_scale: float | None) -> dict[str, Any]:
    row: dict[str, Any] = {"case_id": case_id}
    try:
        reader = PdfReader(str(path), strict=False)
        pages = list(reader.pages)
        page_rows: list[dict[str, Any]] = []
        all_images: list[dict[str, Any]] = []
        for page_number, page in enumerate(pages, start=1):
            width = float(page.mediabox.width)
            height = float(page.mediabox.height)
            images = _image_metadata(page)
            all_images.extend(images)
            page_rows.append(
                {
                    "page": page_number,
                    "width_pt": round(width, 3),
                    "height_pt": round(height, 3),
                    "orientation": _page_orientation(width, height),
                    "rotation": int(page.get("/Rotate", 0) or 0),
                    "image_count": len(images),
                    "image_dimensions": sorted(
                        {f"{item['width']}x{item['height']}" for item in images}
                    ),
                    "image_colorspaces": sorted({item["colorspace"] for item in images}),
                    "image_filters": sorted({item["filter"] for item in images}),
                    "image_dpi": [
                        [round(value, 1) for value in _dpi(item, width, height)]
                        for item in images
                    ],
                }
            )

        import pdfplumber

        with pdfplumber.open(str(path)) as pdf:
            text_counts = [len(page.chars) for page in pdf.pages]
            word_counts = [len(page.extract_words()) for page in pdf.pages]

        row.update(
            {
                "page_count": len(pages),
                "acroform_field_count": len(reader.get_fields() or {}),
                "page_rows": page_rows,
                "text_char_counts": text_counts,
                "word_counts": word_counts,
                "total_text_chars": sum(text_counts),
                "total_words": sum(word_counts),
                "has_hidden_or_selectable_text": sum(text_counts) > 0,
                "image_count": len(all_images),
                "has_raster_images": bool(all_images),
            }
        )
        if render_scale is not None:
            row["render_stats"] = _render_stats(path, render_scale)
    except Exception as error:  # diagnostics must retain a sanitized failure row
        row["diagnostic_error_type"] = type(error).__name__
        row["diagnostic_error"] = str(error)[:160]
    return row


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir", type=Path, nargs="?")
    parser.add_argument(
        "--file",
        dest="files",
        type=Path,
        action="append",
        default=[],
        help="Analyze an explicit PDF path; may be supplied more than once.",
    )
    parser.add_argument("--render-scale", type=float, default=None)
    parser.add_argument("--output", type=Path, default=None)
    args = parser.parse_args()
    if args.files:
        paths = sorted(args.files)
    elif args.input_dir is not None:
        paths = sorted(args.input_dir.glob("*.pdf"))
    else:
        parser.error("provide input_dir or at least one --file")
    rows = [
        characterize(path, f"case_{index:03d}", args.render_scale)
        for index, path in enumerate(paths, start=1)
    ]
    payload = {
        "input_pdf_count": len(paths),
        "privacy_note": "Case IDs and aggregate metrics only; source names and document text are omitted.",
        "cases": rows,
    }
    output = json.dumps(payload, indent=2, sort_keys=True)
    if args.output is None:
        print(output)
    else:
        args.output.write_text(output + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
