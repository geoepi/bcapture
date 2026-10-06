"""Create deterministic synthetic raster fixtures for bounded OCR experiments."""

from __future__ import annotations

import argparse
import io
from pathlib import Path

import numpy as np
import pypdfium2 as pdfium
from PIL import Image, ImageEnhance, ImageFilter


def _render_pages(source: Path, scale: float) -> dict[int, Image.Image]:
    document = pdfium.PdfDocument(str(source))
    pages: dict[int, Image.Image] = {}
    try:
        for page_number in (1, 6):
            array = document.get_page(page_number - 1).render(scale=scale).to_numpy()
            pages[page_number] = Image.fromarray(array[..., :3].astype(np.uint8), mode="RGB")
    finally:
        document.close()
    return pages


def _jpeg_roundtrip(image: Image.Image, quality: int) -> Image.Image:
    buffer = io.BytesIO()
    image.save(buffer, format="JPEG", quality=quality, optimize=False, progressive=False)
    buffer.seek(0)
    return Image.open(buffer).convert("RGB")


def _degrade(image: Image.Image, angle: float, contrast: float, noise_sd: float, quality: int, seed: int) -> Image.Image:
    transformed = image.rotate(angle, resample=Image.Resampling.BICUBIC, expand=False, fillcolor=(248, 248, 248))
    transformed = ImageEnhance.Contrast(transformed).enhance(contrast)
    if noise_sd:
        rng = np.random.default_rng(seed)
        pixels = np.asarray(transformed).astype(float)
        pixels += rng.normal(0, noise_sd, pixels.shape[:2] + (1,))
        transformed = Image.fromarray(np.clip(pixels, 0, 255).astype(np.uint8), mode="RGB")
    if quality <= 60:
        transformed = transformed.filter(ImageFilter.GaussianBlur(radius=0.35))
    return _jpeg_roundtrip(transformed, quality)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    pages = _render_pages(args.input, 200 / 72)
    levels = {
        "clean": (0.0, 1.0, 0.0, 95),
        "moderate": (1.0, 0.88, 3.0, 70),
        "borderline": (2.5, 0.68, 7.0, 45),
    }
    for level, settings in levels.items():
        for page_number, page in pages.items():
            output = page if level == "clean" else _degrade(page, *settings, seed=20261006 + page_number)
            quality = {"clean": 95, "moderate": 85, "borderline": 70}[level]
            output.save(
                args.output_dir / f"initial_epi_{level}_page_{page_number:02d}.jpg",
                format="JPEG",
                quality=quality,
                optimize=False,
                progressive=False,
            )


if __name__ == "__main__":
    main()
