"""Derive versioned spatial metadata from immutable bcapture PDF templates.

The script is intentionally deterministic.  It writes field/widget geometry,
page registration, baseline text, and a manifest beside each canonical pair.
When --evaluation-dir is supplied, control thresholds are calibrated from the
paired synthetic populated/printed fixtures before being recorded in the
manifest.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import re
import sys
from collections import Counter, defaultdict
from pathlib import Path

import numpy as np
import pdfplumber
import pypdfium2 as pdfium
from pypdf import PdfReader

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import acroform


def deref(value):
    return value.get_object() if hasattr(value, "get_object") else value


def clean(value):
    if value is None:
        return None
    value = str(value)
    return value[1:] if value.startswith("/") else value


def text_value(value):
    if value is None:
        return None
    return value.decode("utf-8", errors="replace") if isinstance(value, bytes) else str(value)


def field_name(annotation):
    parts = []
    current = deref(annotation)
    while current is not None:
        if current.get("/T") is not None:
            parts.append(text_value(current.get("/T")))
        current = deref(current.get("/Parent"))
    return ".".join(reversed(parts)) if parts else None


def inherited(annotation, key):
    current = deref(annotation)
    while current is not None:
        value = current.get(key)
        if value is not None:
            return value
        current = deref(current.get("/Parent"))
    return None


def appearance_states(annotation):
    ap = deref(deref(annotation).get("/AP"))
    normal = deref(ap.get("/N")) if ap else None
    if not normal or not hasattr(normal, "keys"):
        return []
    # A Form XObject is a stream with /Type and /Subtype.  A button state map
    # is a plain dictionary whose keys are the export-state names.
    if normal.get("/Type") is not None or normal.get("/Subtype") is not None:
        return []
    return [clean(key) for key in normal.keys()]


def options(field):
    values = inherited(field, "/Opt")
    if values is None:
        return []
    result = []
    for value in values:
        value = deref(value)
        if isinstance(value, (list, tuple)):
            value = value[0] if value else None
        value = clean(value)
        if value is not None:
            result.append(value)
    return result


def normalized_states(states):
    return sorted({str(x) for x in states if x not in (None, "")})


def normalized_options(values):
    return sorted({str(x) for x in values if x not in (None, "")})


def schema_hash(fields):
    definitions = []
    for field in fields:
        field_type = field["field_type"]
        definitions.append({
            "field": field["field"],
            "field_type": field_type,
            "states": normalized_states(field.get("states", [])),
            "options": normalized_options(field.get("options", [])),
            "field_flags": int(field.get("field_flags") or 0) if field_type == "Ch" else 0,
        })
    payload = json.dumps(sorted(definitions, key=lambda item: item["field"]), separators=(",", ":"), ensure_ascii=True)
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def read_widgets(path):
    reader = PdfReader(str(path))
    rows = []
    index = 0
    for page_number, page in enumerate(reader.pages, 1):
        for reference in page.get("/Annots") or []:
            annotation = deref(reference)
            if clean(annotation.get("/Subtype")) != "Widget":
                continue
            index += 1
            field = field_name(annotation)
            parent = deref(annotation.get("/Parent"))
            field_type = clean(inherited(annotation, "/FT"))
            rect = [float(value) for value in annotation.get("/Rect") or []]
            states = appearance_states(annotation)
            rows.append({
                "page": page_number,
                "widget_index": index,
                "field": field,
                "field_type": field_type,
                "parent_field": clean(parent.get("/T")) if parent else None,
                "x1": rect[0], "y1": rect[1], "x2": rect[2], "y2": rect[3],
                "state_candidates": [state for state in states if state != "Off"],
                "states": states,
                "active_state": clean(annotation.get("/AS") or (parent.get("/V") if parent else None)),
            })
    return reader, rows


def read_fields(interactive_path, widgets):
    reader = PdfReader(str(interactive_path))
    form_fields = reader.get_fields() or {}
    by_field = defaultdict(list)
    for widget in widgets:
        by_field[widget["field"]].append(widget)
    rows = []
    for field_index, (name, field) in enumerate(form_fields.items(), 1):
        name = text_value(name)
        field_type = clean(inherited(field, "/FT"))
        field_widgets = by_field.get(name, [])
        states = []
        for widget in field_widgets:
            states.extend(widget["states"])
        if not states:
            states = appearance_states(field)
        states = list(dict.fromkeys(state for state in states if state is not None))
        rects = [widget for widget in field_widgets if widget["field_type"] in (field_type, None)]
        if rects:
            page = rects[0]["page"]
            x1 = min(widget["x1"] for widget in rects)
            y1 = min(widget["y1"] for widget in rects)
            x2 = max(widget["x2"] for widget in rects)
            y2 = max(widget["y2"] for widget in rects)
        else:
            page, x1, y1, x2, y2 = None, None, None, None, None
        field_flags = inherited(field, "/Ff")
        default = clean(inherited(field, "/DV"))
        value = clean(field.get("/V"))
        rows.append({
            "field_index": field_index,
            "field": name,
            "alternative_name": clean(field.get("/TU")),
            "field_type": field_type,
            "field_flags": int(field_flags) if field_flags is not None else None,
            "default_value_raw": text_value(inherited(field, "/DV")),
            "default_value": default,
            "is_default_value": value is not None and default is not None and value == default,
            "states": states,
            "options": options(field),
            "is_multiselect": field_type == "Ch" and bool((int(field_flags or 0) >> 21) & 1),
            "page": page,
            "x1": x1, "y1": y1, "x2": x2, "y2": y2,
        })
    return reader, rows


def transform(rect, source_width, source_height, target_width, target_height):
    scale = min(target_width / source_width, target_height / source_height)
    dx = (target_width - scale * source_width) / 2
    dy = (target_height - scale * source_height) / 2
    return scale, dx, dy, [scale * rect[0] + dx, scale * rect[1] + dy, scale * rect[2] + dx, scale * rect[3] + dy]


def baseline_text(page, rect, page_height):
    x1, y1, x2, y2 = rect
    words = []
    for word in page.extract_words():
        center_x = (word["x0"] + word["x1"]) / 2
        center_y = (word["top"] + word["bottom"]) / 2
        if x1 - 1.5 <= center_x <= x2 + 1.5 and page_height - y2 - 1.5 <= center_y <= page_height - y1 + 1.5:
            words.append(word)
    words.sort(key=lambda item: (round(item["top"], 2), item["x0"]))
    return " ".join(word["text"] for word in words)


def render_pages(path, scale=2.0):
    document = pdfium.PdfDocument(str(path))
    images = [document.get_page(i).render(scale=scale).to_numpy() for i in range(len(document))]
    document.close()
    return images


def control_score(input_image, blank_image, rect, page_height, scale=2.0, pad=2.0):
    left = max(0, int((rect[0] - pad) * scale))
    right = min(input_image.shape[1], int((rect[2] + pad) * scale) + 1)
    top = max(0, int((page_height - rect[3] - pad) * scale))
    bottom = min(input_image.shape[0], int((page_height - rect[1] + pad) * scale) + 1)
    first = input_image[top:bottom, left:right, :3].astype(float)
    second = blank_image[top:bottom, left:right, :3].astype(float)
    height = min(first.shape[0], second.shape[0])
    width = min(first.shape[1], second.shape[1])
    return float(np.abs(first[:height, :width] - second[:height, :width]).mean())


def calibrate_threshold(template, evaluation_dir, widget_rows):
    if evaluation_dir is None:
        return None
    eval_dir = Path(evaluation_dir)
    if template["form_family"] == "initial_epi":
        interactive = eval_dir / "synthetic_epi_interactive.pdf"
        printed = eval_dir / "synthetic_epi_print.pdf"
    else:
        interactive = eval_dir / "synthetic_bcap_audit_interactive.pdf"
        printed = eval_dir / "synthetic_bcap_audit_print.pdf"
    if not interactive.exists() or not printed.exists():
        return None
    _, populated_widgets = read_widgets(interactive)
    _, printed_widgets = read_widgets(template["printed_path"])
    populated_state = {(row["page"], row["field"], tuple(round(row[key], 3) for key in ("x1", "y1", "x2", "y2"))): row["active_state"] not in (None, "Off") for row in populated_widgets}
    source_reader = PdfReader(str(template["interactive_path"]))
    target_reader = PdfReader(str(template["printed_path"]))
    input_reader = PdfReader(str(printed))
    blank_images = render_pages(template["printed_path"])
    input_images = render_pages(printed)
    scores = {"selected": [], "unselected": []}
    for row in widget_rows:
        if row["field_type"] != "Btn" or not row["state_candidates"]:
            continue
        page = row["page"]
        source_page = source_reader.pages[page - 1]
        target_page = target_reader.pages[page - 1]
        input_page = input_reader.pages[page - 1]
        _, _, _, rect = transform([row["x1"], row["y1"], row["x2"], row["y2"]], float(source_page.mediabox.width), float(source_page.mediabox.height), float(input_page.mediabox.width), float(input_page.mediabox.height))
        key = (page, row["field"], tuple(round(row[key], 3) for key in ("x1", "y1", "x2", "y2")))
        category = "selected" if populated_state.get(key, False) else "unselected"
        scores[category].append(control_score(input_images[page - 1], blank_images[page - 1], rect, float(input_page.mediabox.height)))
    if not scores["selected"] or not scores["unselected"]:
        return None
    low = max(scores["unselected"])
    high = min(scores["selected"])
    if high <= low:
        raise RuntimeError(f"Control calibration did not separate selected and unselected marks for {template['form_family']}: max_unselected={low}, min_selected={high}")
    return {
        "threshold": round((low + high) / 2, 6),
        "selected_count": len(scores["selected"]),
        "unselected_count": len(scores["unselected"]),
        "max_unselected": round(low, 6),
        "min_selected": round(high, 6),
    }


def write_csv(path, rows, columns):
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=columns, extrasaction="ignore")
        writer.writeheader()
        for row in rows:
            writer.writerow({column: row.get(column) for column in columns})


def derive(template_dir, evaluation_dir=None):
    manifest_path = template_dir / "manifest.csv"
    with manifest_path.open(newline="", encoding="utf-8") as stream:
        manifest = next(csv.DictReader(stream))
    interactive_path = template_dir / manifest["interactive_filename"]
    printed_path = template_dir / manifest["printed_filename"]
    interactive_reader, widgets = read_widgets(interactive_path)
    printed_reader, _ = read_widgets(printed_path)
    _, fields = read_fields(interactive_path, widgets)
    canonical_fields = {row["field"]: row for row in acroform.extract_form(str(interactive_path))["fields"]}
    for field in fields:
        canonical = canonical_fields[field["field"]]
        for key in ("alternative_name", "field_type", "field_flags", "default_value_raw", "default_value", "is_default_value", "is_multiselect"):
            field[key] = canonical.get(key)
        field["states"] = str(canonical.get("states") or "").split("|") if canonical.get("states") else []
        field["options"] = str(canonical.get("options") or "").split("|") if canonical.get("options") else []
    for page_number, (source_page, target_page) in enumerate(zip(interactive_reader.pages, printed_reader.pages), 1):
        scale, dx, dy, _ = transform([0, 0, float(source_page.mediabox.width), float(source_page.mediabox.height)], float(source_page.mediabox.width), float(source_page.mediabox.height), float(target_page.mediabox.width), float(target_page.mediabox.height))
        manifest.setdefault("page_count", len(interactive_reader.pages))
        manifest.setdefault("interactive_page_width", float(source_page.mediabox.width))
        manifest.setdefault("interactive_page_height", float(source_page.mediabox.height))
        manifest.setdefault("printed_page_width", float(target_page.mediabox.width))
        manifest.setdefault("printed_page_height", float(target_page.mediabox.height))
        if page_number == 1:
            manifest["derived_first_page_scale"] = scale
            manifest["derived_first_page_dx"] = dx
            manifest["derived_first_page_dy"] = dy
    manifest.update({
        "interactive_sha256": sha256(interactive_path),
        "printed_sha256": sha256(printed_path),
        "interactive_page_count": len(interactive_reader.pages),
        "printed_page_count": len(printed_reader.pages),
        "canonical_field_count": len(fields),
        "canonical_widget_count": len(widgets),
        "canonical_schema_hash": schema_hash(fields),
        "template_schema_version": "1",
        "registration_model": "scale_translation",
        "control_render_scale": "2",
        "control_crop_padding_pt": "2",
        "anchor_min_fraction": "0.60",
        "anchor_max_residual_pt": "8",
    })
    template = {"form_family": manifest["form_family"], "interactive_path": interactive_path, "printed_path": printed_path}
    calibration = calibrate_threshold(template, evaluation_dir, widgets)
    if calibration:
        manifest.update({f"control_{key}": value for key, value in calibration.items()})
        manifest["control_calibration_status"] = "synthetic_fixture"
    else:
        manifest["control_threshold"] = "10.225" if manifest["form_family"] == "initial_epi" else "25.0"
        manifest["control_calibration_status"] = "not_run"
    with pdfplumber.open(str(printed_path)) as printed_pdf:
        for field in fields:
            field_widgets = [widget for widget in widgets if widget["field"] == field["field"]]
            if field["page"] is None:
                field["printed_baseline_text"] = ""
                continue
            page = printed_pdf.pages[field["page"] - 1]
            source_page = interactive_reader.pages[field["page"] - 1]
            target_page = printed_reader.pages[field["page"] - 1]
            scale, dx, dy, rect = transform([field["x1"], field["y1"], field["x2"], field["y2"]], float(source_page.mediabox.width), float(source_page.mediabox.height), float(target_page.mediabox.width), float(target_page.mediabox.height))
            field.update({"printed_x1": rect[0], "printed_y1": rect[1], "printed_x2": rect[2], "printed_y2": rect[3], "printed_scale": scale, "printed_dx": dx, "printed_dy": dy, "printed_baseline_text": baseline_text(page, rect, float(target_page.mediabox.height))})
    for widget in widgets:
        source_page = interactive_reader.pages[widget["page"] - 1]
        target_page = printed_reader.pages[widget["page"] - 1]
        scale, dx, dy, rect = transform([widget["x1"], widget["y1"], widget["x2"], widget["y2"]], float(source_page.mediabox.width), float(source_page.mediabox.height), float(target_page.mediabox.width), float(target_page.mediabox.height))
        widget.update({"printed_x1": rect[0], "printed_y1": rect[1], "printed_x2": rect[2], "printed_y2": rect[3], "printed_scale": scale, "printed_dx": dx, "printed_dy": dy, "state_candidates": "|".join(widget["state_candidates"]), "states": "|".join(widget["states"])})
    pages = []
    for page_number, (source_page, target_page) in enumerate(zip(interactive_reader.pages, printed_reader.pages), 1):
        scale, dx, dy, _ = transform([0, 0, float(source_page.mediabox.width), float(source_page.mediabox.height)], float(source_page.mediabox.width), float(source_page.mediabox.height), float(target_page.mediabox.width), float(target_page.mediabox.height))
        pages.append({"page": page_number, "interactive_width": float(source_page.mediabox.width), "interactive_height": float(source_page.mediabox.height), "printed_width": float(target_page.mediabox.width), "printed_height": float(target_page.mediabox.height), "scale": scale, "dx": dx, "dy": dy})
    base_columns = ["form_family", "form_version", "interactive_filename", "printed_filename", "interactive_sha256", "printed_sha256", "interactive_page_count", "printed_page_count", "interactive_page_width", "interactive_page_height", "printed_page_width", "printed_page_height", "canonical_field_count", "canonical_widget_count", "canonical_schema_hash", "template_schema_version", "registration_model", "derived_first_page_scale", "derived_first_page_dx", "derived_first_page_dy", "control_render_scale", "control_crop_padding_pt", "control_threshold", "control_selected_count", "control_unselected_count", "control_max_unselected", "control_min_selected", "control_calibration_status", "anchor_min_fraction", "anchor_max_residual_pt"]
    write_csv(manifest_path, [manifest], base_columns)
    write_csv(template_dir / "page_geometry.csv", pages, list(pages[0]))
    serialized_fields = []
    for field in fields:
        serialized = dict(field)
        serialized["states"] = "|".join(field.get("states", []))
        serialized["options"] = "|".join(field.get("options", []))
        serialized_fields.append(serialized)
    write_csv(template_dir / "fields.csv", serialized_fields, ["field_index", "field", "alternative_name", "field_type", "field_flags", "default_value_raw", "default_value", "is_default_value", "states", "options", "is_multiselect", "page", "x1", "y1", "x2", "y2", "printed_x1", "printed_y1", "printed_x2", "printed_y2", "printed_scale", "printed_dx", "printed_dy", "printed_baseline_text"])
    write_csv(template_dir / "widgets.csv", widgets, ["page", "widget_index", "field", "field_type", "parent_field", "x1", "y1", "x2", "y2", "state_candidates", "states", "printed_x1", "printed_y1", "printed_x2", "printed_y2", "printed_scale", "printed_dx", "printed_dy"])
    write_csv(template_dir / "printed_regions.csv", [{"field": field["field"], "page": field["page"], "x1": field["printed_x1"], "y1": field["printed_y1"], "x2": field["printed_x2"], "y2": field["printed_y2"], "baseline_text": field["printed_baseline_text"]} for field in fields], ["field", "page", "x1", "y1", "x2", "y2", "baseline_text"])
    return manifest


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--template-root", default="inst/extdata/templates")
    parser.add_argument("--evaluation-dir")
    args = parser.parse_args()
    root = Path(args.template_root)
    templates = sorted(path.parent for path in root.glob("*/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]/manifest.csv"))
    for template_dir in templates:
        derive(template_dir, args.evaluation_dir)
        print(template_dir)


if __name__ == "__main__":
    main()
