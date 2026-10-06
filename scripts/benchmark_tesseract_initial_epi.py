"""Benchmark local Tesseract on scanned Initial Epi pages without emitting text.

The utility is intentionally diagnostic rather than an extraction route.  It
renders the supplied PDFs into a temporary directory, runs Tesseract TSV OCR,
and emits only sanitized aggregate metrics.  Raw page images, TSV output, and
recognized strings are never written to the repository or included in the
JSON report.
"""

from __future__ import annotations

import argparse
import csv
import itertools
import json
import math
import re
import subprocess
import tempfile
import time
from pathlib import Path
from typing import Any

import numpy as np
import pypdfium2 as pdfium
from PIL import Image, ImageOps


def _read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as stream:
        return list(csv.DictReader(stream))


def _float(value: Any) -> float:
    return float(value) if value not in (None, "") else 0.0


def _int(value: Any) -> int:
    return int(float(value)) if value not in (None, "") else 0


def _normal(value: str) -> str:
    return re.sub(r"[^a-z0-9]+", "", value.lower())


def _fuzzy_normal(value: str) -> str:
    value = _normal(value)
    return value.translate(str.maketrans({"0": "o", "1": "l", "5": "s", "8": "b"}))


def _confidence(value: str) -> float:
    try:
        return float(value)
    except (TypeError, ValueError):
        return -1.0


def _engine_info(tesseract: Path) -> dict[str, Any]:
    version = subprocess.run(
        [str(tesseract), "--version"], capture_output=True, text=True, check=True
    ).stdout.splitlines()[0].strip()
    language_lines = subprocess.run(
        [str(tesseract), "--list-langs"], capture_output=True, text=True, check=True
    ).stdout.splitlines()
    languages = sorted(
        line.strip() for line in language_lines[1:] if line.strip() and not line.startswith("List")
    )
    return {"version": version, "languages": languages}


def _render_pages(path: Path, scale: float) -> list[Image.Image]:
    document = pdfium.PdfDocument(str(path))
    pages: list[Image.Image] = []
    try:
        for page_index in range(len(document)):
            array = document.get_page(page_index).render(scale=scale).to_numpy()
            pages.append(Image.fromarray(array[..., :3].astype(np.uint8), mode="RGB"))
    finally:
        document.close()
    return pages


def _run_tsv(tesseract: Path, image: Image.Image, temp_dir: Path, stem: str, psm: int) -> list[dict[str, Any]]:
    image_path = temp_dir / f"{stem}.png"
    image.save(image_path, format="PNG")
    result = subprocess.run(
        [str(tesseract), str(image_path), "stdout", "--psm", str(psm), "-l", "eng", "tsv"],
        capture_output=True,
        text=True,
        encoding="utf-8",
        errors="replace",
        check=True,
    )
    rows = list(csv.DictReader(result.stdout.splitlines(), delimiter="\t"))
    tokens: list[dict[str, Any]] = []
    for row in rows:
        text = str(row.get("text") or "").strip()
        confidence = _confidence(row.get("conf", ""))
        if not text or confidence < 0:
            continue
        tokens.append(
            {
                "text": text,
                "norm": _normal(text),
                "fuzzy_norm": _fuzzy_normal(text),
                "x": _float(row.get("left")),
                "y": _float(row.get("top")),
                "w": _float(row.get("width")),
                "h": _float(row.get("height")),
                "confidence": confidence,
            }
        )
    return tokens


def _center(token: dict[str, Any]) -> tuple[float, float]:
    return token["x"] + token["w"] / 2.0, token["y"] + token["h"] / 2.0


def _distance(first: tuple[float, float], second: tuple[float, float]) -> float:
    return math.hypot(first[0] - second[0], first[1] - second[1])


def _anchor_candidates(tokens: list[dict[str, Any]]) -> list[dict[str, Any]]:
    return [
        token
        for token in tokens
        if len(token["norm"]) >= 3 and any(character.isalpha() for character in token["norm"])
    ]


def _unique_expected(tokens: list[dict[str, Any]]) -> list[dict[str, Any]]:
    candidates = _anchor_candidates(tokens)
    counts: dict[str, int] = {}
    for token in candidates:
        counts[token["norm"]] = counts.get(token["norm"], 0) + 1
    return [token for token in candidates if counts[token["norm"]] == 1 and token["confidence"] >= 45]


def _match_anchors(expected: list[dict[str, Any]], observed: list[dict[str, Any]]) -> dict[str, Any]:
    available = list(_anchor_candidates(observed))
    used: set[int] = set()
    pairs: list[dict[str, Any]] = []
    exact = 0
    fuzzy = 0
    for expected_token in expected:
        expected_center = _center(expected_token)
        exact_candidates = [
            (index, token)
            for index, token in enumerate(available)
            if index not in used and token["norm"] == expected_token["norm"]
        ]
        mode = "exact"
        candidates = exact_candidates
        if not candidates and len(expected_token["fuzzy_norm"]) >= 4:
            candidates = [
                (index, token)
                for index, token in enumerate(available)
                if index not in used
                and len(token["fuzzy_norm"]) >= 4
                and _similarity(expected_token["fuzzy_norm"], token["fuzzy_norm"]) >= 0.84
            ]
            mode = "fuzzy"
        if not candidates:
            continue
        index, token = min(candidates, key=lambda item: _distance(expected_center, _center(item[1])))
        if _distance(expected_center, _center(token)) > 360:
            continue
        used.add(index)
        if mode == "exact":
            exact += 1
        else:
            fuzzy += 1
        pairs.append({"expected": expected_token, "observed": token, "match": mode})
    confidences = [pair["observed"]["confidence"] for pair in pairs]
    return {
        "expected": len(expected),
        "exact": exact,
        "fuzzy": fuzzy,
        "recovered": len(pairs),
        "confidence_mean": round(float(np.mean(confidences)), 3) if confidences else None,
        "confidence_min": round(float(np.min(confidences)), 3) if confidences else None,
        "confidence_max": round(float(np.max(confidences)), 3) if confidences else None,
        "pairs": pairs,
    }


def _similarity(first: str, second: str) -> float:
    from difflib import SequenceMatcher

    return SequenceMatcher(None, first, second).ratio()


def _fit_affine(pairs: list[dict[str, Any]], threshold_px: float = 22.0) -> dict[str, Any]:
    if len(pairs) < 3:
        return {"success": False, "reason": "insufficient_anchors", "pairs": len(pairs)}
    source = np.array([_center(pair["observed"]) for pair in pairs], dtype=float)
    target = np.array([_center(pair["expected"]) for pair in pairs], dtype=float)
    rng = np.random.default_rng(20261006)
    samples: list[tuple[int, int, int]] = list(itertools.islice(itertools.combinations(range(len(pairs)), 3), 500))
    if len(pairs) > 18:
        samples = [tuple(rng.choice(len(pairs), 3, replace=False).tolist()) for _ in range(500)]
    best: tuple[int, float, np.ndarray, np.ndarray] | None = None
    for sample in samples:
        matrix = np.c_[source[list(sample)], np.ones(3)]
        if np.linalg.matrix_rank(matrix) < 3:
            continue
        coefficients, _, _, _ = np.linalg.lstsq(matrix, target[list(sample)], rcond=None)
        predicted = np.c_[source, np.ones(len(source))] @ coefficients
        residuals = np.linalg.norm(predicted - target, axis=1)
        inliers = residuals <= threshold_px
        score = (int(np.sum(inliers)), -float(np.mean(residuals[inliers])) if np.any(inliers) else -float("inf"))
        if best is None or score > (best[0], best[1]):
            best = (score[0], score[1], coefficients, residuals)
    if best is None or best[0] < 3:
        return {"success": False, "reason": "degenerate_anchor_geometry", "pairs": len(pairs)}
    inliers = best[3] <= threshold_px
    matrix = np.c_[source[inliers], np.ones(int(np.sum(inliers)))]
    coefficients, _, _, _ = np.linalg.lstsq(matrix, target[inliers], rcond=None)
    predicted = np.c_[source, np.ones(len(source))] @ coefficients
    residuals = np.linalg.norm(predicted - target, axis=1)
    inlier_residuals = residuals[inliers]
    spread_x = float(np.ptp(target[inliers, 0])) if np.any(inliers) else 0.0
    spread_y = float(np.ptp(target[inliers, 1])) if np.any(inliers) else 0.0
    guardrail_success = (
        int(np.sum(inliers)) >= 6
        and spread_x >= 300
        and spread_y >= 300
        and float(np.max(inlier_residuals)) <= 22
    )
    affine = np.array(
        [[coefficients[0, 0], coefficients[1, 0], coefficients[2, 0]],
         [coefficients[0, 1], coefficients[1, 1], coefficients[2, 1]],
         [0.0, 0.0, 1.0]],
        dtype=float,
    )
    return {
        "success": bool(guardrail_success),
        "reason": "accepted" if guardrail_success else "guardrail_failed",
        "transform": "affine",
        "anchors_used": int(np.sum(inliers)),
        "outliers_removed": int(np.sum(~inliers)),
        "spread_x_px": round(spread_x, 3),
        "spread_y_px": round(spread_y, 3),
        "residual_rmse_px": round(float(np.sqrt(np.mean(inlier_residuals**2))), 3),
        "residual_max_px": round(float(np.max(inlier_residuals)), 3),
        "residual_rmse_pt": round(float(np.sqrt(np.mean(inlier_residuals**2)) * 72 / 200), 3),
        "matrix": affine.tolist(),
        "pair_inliers": [bool(item) for item in inliers],
    }


def _apply_affine(matrix: np.ndarray, point: tuple[float, float]) -> tuple[float, float]:
    value = matrix @ np.array([point[0], point[1], 1.0])
    return float(value[0]), float(value[1])


def _pdf_point(pixel: tuple[float, float], page_width_pt: float, page_height_pt: float) -> tuple[float, float]:
    return pixel[0] * 72 / 200, page_height_pt - pixel[1] * 72 / 200


def _field_class(field: dict[str, str]) -> str:
    name = str(field.get("field") or "").lower()
    width = _float(field.get("x2")) - _float(field.get("x1"))
    if any(term in name for term in ("date", "year", "month", "day", "age", "number", "count", "time")):
        return "date_or_numeric"
    if width >= 180:
        return "multi_word"
    return "short_text"


def _field_assignment(
    page_number: int,
    tokens: list[dict[str, Any]],
    blank_tokens: list[dict[str, Any]],
    registration: dict[str, Any],
    fields: list[dict[str, str]],
    page_width_pt: float,
    page_height_pt: float,
) -> dict[str, Any]:
    if not registration.get("success"):
        return {"status": "not_registered", "regions_with_tokens": 0, "nonbaseline_tokens": 0}
    matrix = np.array(registration["matrix"], dtype=float)
    baseline = {(token["norm"], round(_center(token)[0]), round(_center(token)[1])) for token in blank_tokens}
    by_class: dict[str, dict[str, int]] = {}
    assigned: list[dict[str, Any]] = []
    for token in tokens:
        canonical_pixel = _apply_affine(matrix, _center(token))
        x_pt, y_pt = _pdf_point(canonical_pixel, page_width_pt, page_height_pt)
        for field in fields:
            if _int(field.get("page")) != page_number or field.get("field_type") == "Btn":
                continue
            if not (_float(field.get("x1")) <= x_pt <= _float(field.get("x2")) and _float(field.get("y1")) <= y_pt <= _float(field.get("y2"))):
                continue
            field_class = _field_class(field)
            row = by_class.setdefault(field_class, {"regions": 0, "tokens": 0, "nonbaseline_tokens": 0})
            row["tokens"] += 1
            baseline_match = any(
                norm == token["norm"] and abs(x_pt - bx * 72 / 200) < 8 and abs((page_height_pt - y_pt) - by * 72 / 200) < 8
                for norm, bx, by in baseline
            )
            if not baseline_match:
                row["nonbaseline_tokens"] += 1
            assigned.append({"class": field_class, "baseline": baseline_match, "confidence": token["confidence"]})
            break
    for row in by_class.values():
        row["regions"] = int(row["tokens"] > 0)
    nonbaseline = [row["confidence"] for row in assigned if not row["baseline"]]
    return {
        "status": "assigned" if assigned else "no_field_tokens",
        "regions_with_tokens": len({item["class"] for item in assigned}),
        "nonbaseline_tokens": len(nonbaseline),
        "confidence_min": round(float(min(nonbaseline)), 3) if nonbaseline else None,
        "confidence_max": round(float(max(nonbaseline)), 3) if nonbaseline else None,
        "by_class": by_class,
        "normalization_success": "not_measured",
        "semantic_parse_success": "not_measured",
    }


def _control_scores(
    scan: Image.Image,
    blank: Image.Image,
    registration: dict[str, Any],
    widgets: list[dict[str, str]],
    page_number: int,
    page_height_pt: float,
    threshold: float,
) -> list[float]:
    if not registration.get("success"):
        return []
    matrix = np.array(registration["matrix"], dtype=float)
    inverse = np.linalg.inv(matrix)
    data = tuple(inverse[:2, :].reshape(-1).tolist())
    warped = scan.transform(blank.size, Image.Transform.AFFINE, data=data, resample=Image.Resampling.BILINEAR)
    blank_gray = np.asarray(ImageOps.grayscale(blank), dtype=float)
    scan_gray = np.asarray(ImageOps.grayscale(warped), dtype=float)
    scores: list[float] = []
    for widget in widgets:
        if _int(widget.get("page")) != page_number:
            continue
        x1 = max(0, int(_float(widget.get("x1")) * 200 / 72 - 8))
        x2 = min(blank_gray.shape[1], int(_float(widget.get("x2")) * 200 / 72 + 8))
        y1 = max(0, int((page_height_pt - _float(widget.get("y2"))) * 200 / 72 - 8))
        y2 = min(blank_gray.shape[0], int((page_height_pt - _float(widget.get("y1"))) * 200 / 72 + 8))
        if x2 <= x1 or y2 <= y1:
            continue
        difference = np.abs(scan_gray[y1:y2, x1:x2] - blank_gray[y1:y2, x1:x2])
        scores.append(float(np.mean(difference) / 255 * 100))
    return scores


def _summarize_scores(scores: list[float], threshold: float) -> dict[str, Any]:
    if not scores:
        return {"count": 0, "threshold": threshold, "min": None, "median": None, "max": None, "above_threshold": 0, "ambiguous": 0}
    array = np.array(scores, dtype=float)
    return {
        "count": len(scores),
        "threshold": threshold,
        "min": round(float(np.min(array)), 3),
        "median": round(float(np.median(array)), 3),
        "max": round(float(np.max(array)), 3),
        "above_threshold": int(np.sum(array >= threshold)),
        "ambiguous": int(np.sum(np.abs(array - threshold) <= 2.0)),
    }


def _best_config(configs: dict[str, dict[str, Any]]) -> str:
    def score(item: dict[str, Any]) -> tuple[int, int, float]:
        registrations = [page["registration"] for page in item["pages"]]
        successful = sum(bool(registration.get("success")) for registration in registrations)
        recovered = sum(page["anchors"]["recovered"] for page in item["pages"])
        residuals = [page["registration"].get("residual_rmse_pt") for page in item["pages"] if page["registration"].get("success")]
        residual = -float(np.mean(residuals)) if residuals else -999.0
        return successful, recovered, residual

    return max(configs, key=lambda key: score(configs[key]))


def benchmark(path: Path, case_id: str, tesseract: Path, template_dir: Path, scale: float, psm: int, temp_dir: Path) -> dict[str, Any]:
    started = time.perf_counter()
    template_pages = _render_pages(template_dir / "blank_printed.pdf", scale)
    scan_pages = _render_pages(path, scale)
    render_seconds = time.perf_counter() - started
    fields = _read_csv(template_dir / "fields.csv")
    widgets = _read_csv(template_dir / "widgets.csv")
    manifest = _read_csv(template_dir / "manifest.csv")[0]
    threshold = _float(manifest.get("control_threshold"))
    page_width_pt = _float(manifest.get("printed_page_width")) or 612.0
    page_height_pt = _float(manifest.get("printed_page_height")) or 792.0
    configs: dict[str, dict[str, Any]] = {}
    ocr_started = time.perf_counter()
    blank_ocr_cache: dict[tuple[str, int], list[dict[str, Any]]] = {}
    for config_name in ("raw", "grayscale_autocontrast"):
        configs[config_name] = {"pages": [], "ocr_seconds": 0.0}
        for page_index, (scan, blank) in enumerate(zip(scan_pages, template_pages), start=1):
            scan_input = scan if config_name == "raw" else ImageOps.autocontrast(ImageOps.grayscale(scan)).convert("RGB")
            blank_input = blank if config_name == "raw" else ImageOps.autocontrast(ImageOps.grayscale(blank)).convert("RGB")
            page_started = time.perf_counter()
            observed = _run_tsv(tesseract, scan_input, temp_dir, f"{case_id}_{config_name}_{page_index}_scan", psm)
            cache_key = (config_name, page_index)
            if cache_key not in blank_ocr_cache:
                blank_ocr_cache[cache_key] = _run_tsv(tesseract, blank_input, temp_dir, f"{case_id}_{config_name}_{page_index}_blank", psm)
            expected_tokens = blank_ocr_cache[cache_key]
            configs[config_name]["ocr_seconds"] += time.perf_counter() - page_started
            expected = _unique_expected(expected_tokens)
            anchors = _match_anchors(expected, observed)
            registration = _fit_affine(anchors["pairs"])
            assignments = _field_assignment(page_index, observed, expected_tokens, registration, fields, page_width_pt, page_height_pt)
            controls = _control_scores(scan, blank, registration, widgets, page_index, page_height_pt, threshold)
            configs[config_name]["pages"].append(
                {
                    "page": page_index,
                    "observed_tokens": len(observed),
                    "anchors": {key: value for key, value in anchors.items() if key != "pairs"},
                    "registration": {key: value for key, value in registration.items() if key not in ("matrix", "pair_inliers")},
                    "registration_matrix": registration.get("matrix"),
                    "assignments": assignments,
                    "controls": _summarize_scores(controls, threshold),
                }
            )
    ocr_seconds = time.perf_counter() - ocr_started
    selected = _best_config(configs)
    selected_pages = configs[selected]["pages"]
    successful_registrations = [page for page in selected_pages if page["registration"].get("success")]
    all_controls = [page["controls"] for page in selected_pages if page["controls"]["count"]]
    return {
        "case_id": case_id,
        "page_count": len(scan_pages),
        "page_size_pt": [page_width_pt, page_height_pt],
        "selected_preprocessing": selected,
        "render_seconds": round(render_seconds, 3),
        "ocr_seconds": round(ocr_seconds, 3),
        "total_seconds": round(time.perf_counter() - started, 3),
        "config_summary": {
            name: {
                "ocr_seconds": round(value["ocr_seconds"], 3),
                "pages_with_successful_registration": sum(bool(page["registration"].get("success")) for page in value["pages"]),
                "anchors_expected": sum(page["anchors"]["expected"] for page in value["pages"]),
                "anchors_recovered": sum(page["anchors"]["recovered"] for page in value["pages"]),
            }
            for name, value in configs.items()
        },
        "selected_summary": {
            "pages_with_successful_registration": len(successful_registrations),
            "anchors_expected": sum(page["anchors"]["expected"] for page in selected_pages),
            "anchors_recovered": sum(page["anchors"]["recovered"] for page in selected_pages),
            "anchor_confidence_min": min((page["anchors"]["confidence_min"] for page in selected_pages if page["anchors"]["confidence_min"] is not None), default=None),
            "anchor_confidence_max": max((page["anchors"]["confidence_max"] for page in selected_pages if page["anchors"]["confidence_max"] is not None), default=None),
            "registration_residual_pt_median": round(float(np.median([page["registration"]["residual_rmse_pt"] for page in successful_registrations])), 3) if successful_registrations else None,
            "registration_residual_pt_max": max((page["registration"]["residual_rmse_pt"] for page in successful_registrations), default=None),
            "field_assignment_pages": sum(page["assignments"]["status"] == "assigned" for page in selected_pages),
            "field_nonbaseline_tokens": sum(page["assignments"].get("nonbaseline_tokens", 0) for page in selected_pages),
            "control_pages": len(all_controls),
            "control_widgets": sum(control["count"] for control in all_controls),
            "control_above_threshold": sum(control["above_threshold"] for control in all_controls),
            "control_ambiguous": sum(control["ambiguous"] for control in all_controls),
            "control_score_min": min((control["min"] for control in all_controls if control["min"] is not None), default=None),
            "control_score_median": float(np.median([control["median"] for control in all_controls])) if all_controls else None,
            "control_score_max": max((control["max"] for control in all_controls if control["max"] is not None), default=None),
        },
        "selected_page_metrics": [
            {
                "page": page["page"],
                "observed_tokens": page["observed_tokens"],
                "anchors": page["anchors"],
                "registration": page["registration"],
                "assignments": page["assignments"],
                "controls": page["controls"],
            }
            for page in selected_pages
        ],
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--file", dest="files", action="append", type=Path, required=True)
    parser.add_argument("--tesseract", type=Path, required=True)
    parser.add_argument("--template-dir", type=Path, default=Path("inst/extdata/templates/initial_epi/2024-05-28"))
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--scale", type=float, default=200 / 72)
    parser.add_argument("--psm", type=int, default=6)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="bcapture-tesseract-") as temporary:
        temp_dir = Path(temporary)
        cases = [
            benchmark(path, f"case_{index:03d}", args.tesseract, args.template_dir, args.scale, args.psm, temp_dir)
            for index, path in enumerate(args.files, start=1)
        ]
    payload = {
        "privacy_note": "Case IDs and aggregate metrics only; source names, OCR text, identifiers, and field values are omitted.",
        "engine": _engine_info(args.tesseract),
        "configuration": {"language": "eng", "psm": args.psm, "render_dpi": 200, "guardrails": {"minimum_anchors": 6, "minimum_spread_px": 300, "maximum_inlier_residual_px": 22}},
        "cases": cases,
    }
    args.output.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
