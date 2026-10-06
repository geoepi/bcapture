"""Template-guided extraction for known selectable-text flattened PDFs."""

from __future__ import annotations

import csv
import hashlib
import re
from collections import defaultdict
from pathlib import Path

import pypdf
from acroform import _pdf_metadata


class SpatialExtractionError(RuntimeError):
    """An expected, classified failure of spatial-template extraction."""

    def __init__(self, failure_type, message):
        super().__init__(message)
        self.failure_type = failure_type


def _parse_pipe(value):
    if value is None or value == "":
        return []
    return [item for item in str(value).split("|") if item]


def _as_bool(value):
    return str(value).lower() in {"true", "1", "yes"}


def _as_int(value):
    return int(value) if value not in (None, "") else None


def _as_float(value):
    return float(value) if value not in (None, "") else None


def _read_csv(path):
    with Path(path).open(newline="", encoding="utf-8") as stream:
        return list(csv.DictReader(stream))


def _metadata(reader):
    return _pdf_metadata(reader)


def _load_template(template_dir):
    template_dir = Path(template_dir)
    manifest = _read_csv(template_dir / "manifest.csv")[0]
    fields = _read_csv(template_dir / "fields.csv")
    widgets = _read_csv(template_dir / "widgets.csv")
    pages = _read_csv(template_dir / "page_geometry.csv")
    for field in fields:
        for key in ("field_index", "field_flags", "page"):
            field[key] = _as_int(field.get(key))
        for key in ("x1", "y1", "x2", "y2", "printed_x1", "printed_y1", "printed_x2", "printed_y2"):
            field[key] = _as_float(field.get(key))
        field["is_default_value"] = _as_bool(field.get("is_default_value"))
        field["is_multiselect"] = _as_bool(field.get("is_multiselect"))
        field["states"] = _parse_pipe(field.get("states"))
        field["options"] = _parse_pipe(field.get("options"))
    for widget in widgets:
        for key in ("page", "widget_index"):
            widget[key] = _as_int(widget.get(key))
        for key in ("x1", "y1", "x2", "y2", "printed_x1", "printed_y1", "printed_x2", "printed_y2"):
            widget[key] = _as_float(widget.get(key))
        widget["state_candidates"] = _parse_pipe(widget.get("state_candidates"))
        widget["states"] = _parse_pipe(widget.get("states"))
    for page in pages:
        for key in ("page",):
            page[key] = _as_int(page.get(key))
        for key in ("interactive_width", "interactive_height", "printed_width", "printed_height", "scale", "dx", "dy"):
            page[key] = _as_float(page.get(key))
    return {"dir": template_dir, "manifest": manifest, "fields": fields, "widgets": widgets, "pages": pages}


def _words_in_region(page, rect, page_height):
    x1, y1, x2, y2 = rect
    words = []
    for word in page.extract_words(keep_blank_chars=False, use_text_flow=False):
        center_x = (word["x0"] + word["x1"]) / 2
        center_y = (word["top"] + word["bottom"]) / 2
        if x1 - 1.5 <= center_x <= x2 + 1.5 and page_height - y2 - 1.5 <= center_y <= page_height - y1 + 1.5:
            words.append(word)
    words.sort(key=lambda item: (round(item["top"], 2), item["x0"]))
    return words


def _clean_underlines(text):
    text = str(text)
    # Some flattened PDFs emit U+FFFD between otherwise valid glyphs when a
    # subsetted font has incomplete ToUnicode mappings.  The replacement
    # character is encoding damage, not user-entered content.
    text = text.replace("\ufffd", "")
    compact = text.replace("_", "")
    if not compact:
        return ""
    # Cairo sometimes interleaves decorative underline glyphs between every
    # entered character. Preserve sparse underscores in genuine values (for
    # example FileNameField) but collapse underline-heavy tokens.
    if compact and text.count("_") >= max(2, int(len(compact) * 0.35)):
        return compact
    return text


def _char_rows(page, rect, page_height):
    x1, y1, x2, y2 = rect
    chars = []
    overflow = []
    for char in page.chars:
        center_x = (char["x0"] + char["x1"]) / 2
        center_y = (char["top"] + char["bottom"]) / 2
        height = float(char["bottom"] - char["top"])
        if height < 5 or not (page_height - y2 - 1.5 <= center_y <= page_height - y1 + 1.5):
            continue
        if x1 - 1.5 <= center_x <= x2 + 1.5:
            chars.append(char)
        elif x2 + 1.5 < center_x <= x2 + 8:
            overflow.append(char)
    chars.sort(key=lambda char: (char["top"], char["x0"]))
    rows = []
    for char in chars:
        center_y = (char["top"] + char["bottom"]) / 2
        if not rows or abs(center_y - rows[-1]["center_y"]) > 2:
            rows.append({"center_y": center_y, "chars": [char]})
        else:
            rows[-1]["chars"].append(char)
    values = []
    for row in rows:
        row["chars"].sort(key=lambda char: char["x0"])
        # Preserve a contiguous trailing word when the printed value slightly
        # overflows the interactive widget rectangle.  Only consider glyphs
        # explicitly outside the normal region, and stop at a normal word gap
        # so adjacent labels are not absorbed.
        trailing = sorted(
            (char for char in overflow if abs(((char["top"] + char["bottom"]) / 2) - row["center_y"]) <= 2),
            key=lambda char: char["x0"],
        )
        if (
            len(trailing) == 1
            and str(row["chars"][-1]["text"]).strip()
            and str(trailing[0]["text"]).strip()
            and str(trailing[0]["text"]).isalpha()
        ):
            for char in trailing:
                if char["x0"] - row["chars"][-1]["x1"] > 1.8:
                    break
                row["chars"].append(char)
        encoding_damaged = any(str(char["text"]) == "\ufffd" for char in row["chars"])
        pieces = []
        previous = None
        for char in row["chars"]:
            text = str(char["text"])
            if text == "\ufffd":
                continue
            if not encoding_damaged and previous is not None and ((char["x0"] - previous["x1"]) > 1.8 or text == " "):
                pieces.append(" ")
            pieces.append(text)
            previous = char
        text = _clean_underlines("".join(pieces)).strip()
        text = re.sub(r"\s+", " ", text)
        text = re.sub(r"^:", "", text).strip()
        if text and text not in {":", "."}:
            values.append(text)
    return values


def _normalized_text(text):
    return re.sub(r"\s+", " ", _clean_underlines(text)).strip()


def _recover_text(incoming_page, blank_page, rect, page_height, field):
    observed = _char_rows(incoming_page, rect, page_height)
    baseline = {_normalized_text(value) for value in _char_rows(blank_page, rect, page_height)}
    value_rows = [value for value in observed if _normalized_text(value) not in baseline]
    if len(value_rows) > 1 and len(value_rows[-1].strip()) == 1 and value_rows[-1].strip().isalpha():
        value_rows = value_rows[:-1]
    value = " ".join(value_rows).strip()
    for artifact in sorted((item for item in baseline if item and len(item) <= 3), key=len, reverse=True):
        escaped = re.escape(artifact)
        value = re.sub(rf"\s*{escaped}$", "", value).strip()
        value = re.sub(rf"^{escaped}\s*", "", value).strip()
    value = value.rstrip(")").strip()
    placeholders = {"Select or Type", "Select (Ctrl for multi)", "Select", "(Ctrl", "for", "multi)"}
    if value in placeholders or value.startswith("Select (Ctrl for multi)"):
        value = ""
    if field["field_type"] == "Ch" and not value:
        value = str(field.get("default_value") or "").strip()
    return value or None


def _transform_rect(rect, source_page, target_page):
    source_width = float(source_page.mediabox.width)
    source_height = float(source_page.mediabox.height)
    target_width = float(target_page.mediabox.width)
    target_height = float(target_page.mediabox.height)
    scale = min(target_width / source_width, target_height / source_height)
    dx = (target_width - scale * source_width) / 2
    dy = (target_height - scale * source_height) / 2
    return scale, dx, dy, [scale * rect[0] + dx, scale * rect[1] + dy, scale * rect[2] + dx, scale * rect[3] + dy]


def _anchor_quality(blank_pdf, incoming_pdf, expected_printed_pages, max_residual):
    matched = 0
    eligible = 0
    residuals = []
    if len(blank_pdf.pages) != len(incoming_pdf.pages):
        return matched, eligible, residuals
    for blank_page, incoming_page in zip(blank_pdf.pages, incoming_pdf.pages):
        target_width = float(incoming_page.width)
        target_height = float(incoming_page.height)
        expected_width = float(expected_printed_pages[0]["printed_width"])
        expected_height = float(expected_printed_pages[0]["printed_height"])
        scale = min(target_width / expected_width, target_height / expected_height)
        dx = (target_width - scale * expected_width) / 2
        dy = (target_height - scale * expected_height) / 2
        candidates = defaultdict(list)
        for word in incoming_page.extract_words():
            text = str(word["text"]).strip()
            if len(text) < 3 or "_" in text or text.startswith("(cid:"):
                continue
            candidates[text].append(word)
        for word in blank_page.extract_words():
            text = str(word["text"]).strip()
            if len(text) < 3 or "_" in text or text.startswith("(cid:"):
                continue
            eligible += 1
            options = candidates.get(text, [])
            if not options:
                continue
            bx = scale * ((word["x0"] + word["x1"]) / 2) + dx
            by = target_height - (scale * ((word["top"] + word["bottom"]) / 2) + dy)
            nearest = min(options, key=lambda item: abs(((item["x0"] + item["x1"]) / 2) - bx) + abs(((item["top"] + item["bottom"]) / 2) - (target_height - by)))
            ix = (nearest["x0"] + nearest["x1"]) / 2
            iy = nearest["top"] + (nearest["bottom"] - nearest["top"]) / 2
            distance = ((ix - bx) ** 2 + (iy - (target_height - by)) ** 2) ** 0.5
            if distance <= max_residual:
                matched += 1
                residuals.append(distance)
    return matched, eligible, residuals


def _render_pages(path, scale):
    import pypdfium2 as pdfium

    document = pdfium.PdfDocument(str(path))
    images = [document.get_page(i).render(scale=scale).to_numpy() for i in range(len(document))]
    document.close()
    return images


def _control_score(input_image, blank_image, rect, page_height, scale, padding):
    left = max(0, int((rect[0] - padding) * scale))
    right = min(input_image.shape[1], int((rect[2] + padding) * scale) + 1)
    top = max(0, int((page_height - rect[3] - padding) * scale))
    bottom = min(input_image.shape[0], int((page_height - rect[1] + padding) * scale) + 1)
    first = input_image[top:bottom, left:right, :3].astype(float)
    second = blank_image[top:bottom, left:right, :3].astype(float)
    height = min(first.shape[0], second.shape[0])
    width = min(first.shape[1], second.shape[1])
    return float(abs(first[:height, :width] - second[:height, :width]).mean())


def _field_row(field, method, registration, value_raw=None, value=None, populated=False, status="success", evidence=None, ambiguity=None, region=None):
    return {
        "field_index": field["field_index"],
        "page": field["page"],
        "field": field["field"],
        "alternative_name": field.get("alternative_name"),
        "field_type": field["field_type"],
        "field_flags": field.get("field_flags"),
        "value_raw": value_raw,
        "value": value,
        "default_value_raw": field.get("default_value_raw"),
        "default_value": field.get("default_value"),
        "is_default_value": bool(value is not None and field.get("default_value") not in (None, "") and value == field.get("default_value")),
        "states": "|".join(field.get("states", [])) or None,
        "options": "|".join(field.get("options", [])) or None,
        "is_multiselect": field.get("is_multiselect", False),
        "is_populated": bool(populated),
        "extraction_method": method,
        "extraction_status": status,
        "evidence_class": evidence,
        "ambiguity_reason": ambiguity,
        "source_page": field["page"],
        "source_region_x1": region[0] if region else None,
        "source_region_y1": region[1] if region else None,
        "source_region_x2": region[2] if region else None,
        "source_region_y2": region[3] if region else None,
        "registration_method": registration["method"],
        "registration_quality": registration["quality"],
        "registration_residual_pt": registration["residual"],
        "template_family": registration["family"],
        "template_version": registration["version"],
    }


def extract_spatial(pdf_path, template_dir):
    """Extract a recognized flattened PDF using a versioned canonical template."""

    import pdfplumber

    template = _load_template(template_dir)
    manifest = template["manifest"]
    source_path = Path(template["dir"]) / manifest["interactive_filename"]
    blank_path = Path(template["dir"]) / manifest["printed_filename"]
    incoming_path = Path(pdf_path)
    try:
        reader = pypdf.PdfReader(str(incoming_path))
    except Exception as error:
        raise SpatialExtractionError("pdf_read_error", f"Could not read PDF: {error}") from error
    if reader.get_fields():
        raise SpatialExtractionError("acroform_fields_present", "Spatial extraction is only a fallback for PDFs without AcroForm fields.")
    source_reader = pypdf.PdfReader(str(source_path))
    with pdfplumber.open(str(incoming_path)) as incoming_pdf, pdfplumber.open(str(blank_path)) as blank_pdf:
        character_count = sum(len(page.chars) for page in incoming_pdf.pages)
        if character_count < 50:
            raise SpatialExtractionError("no_usable_digital_content", "The PDF has no usable selectable digital text layer for spatial extraction.")
        if len(reader.pages) != int(manifest["printed_page_count"]):
            raise SpatialExtractionError("unrecognized_flattened_form", "Page count does not match the canonical printed template.")
        for page, expected in zip(incoming_pdf.pages, template["pages"]):
            if abs(float(page.width) - float(expected["printed_width"])) > 2 or abs(float(page.height) - float(expected["printed_height"])) > 2:
                raise SpatialExtractionError("registration_failed", "Incoming page geometry is outside the canonical printed-template tolerance.")
        matched, eligible, residuals = _anchor_quality(blank_pdf, incoming_pdf, template["pages"], float(manifest.get("anchor_max_residual_pt", 8)))
        fraction = matched / eligible if eligible else 0
        if fraction < float(manifest.get("anchor_min_fraction", 0.60)):
            raise SpatialExtractionError("unrecognized_flattened_form", f"Static layout agreement is too low ({fraction:.3f}).")
        quality = "high" if fraction >= 0.85 and (max(residuals) if residuals else 0) <= 2 else "acceptable"
        registration = {
            "method": "scale_translation",
            "quality": quality,
            "residual": float(sum(residuals) / len(residuals)) if residuals else None,
            "family": manifest["form_family"],
            "version": manifest["form_version"],
        }
        blank_images = _render_pages(blank_path, float(manifest.get("control_render_scale", 2)))
        input_images = _render_pages(incoming_path, float(manifest.get("control_render_scale", 2)))
        threshold = float(manifest.get("control_threshold", 0))
        widgets_by_field = defaultdict(list)
        for widget in template["widgets"]:
            widgets_by_field[widget["field"]].append(widget)
        control_observations = {}
        for field_name, widgets in widgets_by_field.items():
            observations = []
            for widget in widgets:
                if widget["field_type"] != "Btn" or not widget["state_candidates"]:
                    continue
                page_number = widget["page"]
                source_page = source_reader.pages[page_number - 1]
                target_page = reader.pages[page_number - 1]
                _, _, _, rect = _transform_rect([widget["x1"], widget["y1"], widget["x2"], widget["y2"]], source_page, target_page)
                score = _control_score(input_images[page_number - 1], blank_images[page_number - 1], rect, float(target_page.mediabox.height), float(manifest.get("control_render_scale", 2)), float(manifest.get("control_crop_padding_pt", 2)))
                observations.append({"state": widget["state_candidates"][0], "selected": score > threshold, "score": score})
            control_observations[field_name] = observations
        rows = []
        for field in template["fields"]:
            page_number = field["page"]
            source_page = source_reader.pages[page_number - 1] if page_number else None
            target_page = reader.pages[page_number - 1] if page_number else None
            region = None
            if source_page is not None and target_page is not None:
                _, _, _, region = _transform_rect([field["x1"], field["y1"], field["x2"], field["y2"]], source_page, target_page)
            if field["field_type"] == "Btn":
                observations = control_observations.get(field["field"], [])
                selected = [item for item in observations if item["selected"]]
                if len(selected) > 1:
                    rows.append(_field_row(field, "spatial_mark", registration, status="ambiguous", evidence="ambiguous", ambiguity="multiple_selected_control_regions", region=region))
                elif len(selected) == 1:
                    state = selected[0]["state"]
                    rows.append(_field_row(field, "spatial_mark", registration, value_raw="/" + state, value=state, populated=True, evidence="mark_difference", region=region))
                elif observations:
                    rows.append(_field_row(field, "spatial_mark", registration, value_raw="/Off", value="Off", evidence="blank_baseline", region=region))
                else:
                    rows.append(_field_row(field, "spatial_mark", registration, status="unsupported", evidence="no_valid_appearance_state", region=region))
            elif field["field_type"] in ("Tx", "Ch") and page_number:
                value = _recover_text(incoming_pdf.pages[page_number - 1], blank_pdf.pages[page_number - 1], region, float(target_page.mediabox.height), field)
                raw = value
                is_default = field["field_type"] == "Ch" and value is not None and value == field.get("default_value")
                rows.append(_field_row(field, "spatial_text", registration, value_raw=raw, value=value, populated=bool(value and not is_default), status="default_placeholder" if is_default else ("blank" if value is None else "success"), evidence="template_baseline" if is_default or value is None else "field_region_text", region=region))
            else:
                rows.append(_field_row(field, "spatial_text", registration, status="unsupported", evidence="unsupported_field_type", region=region))
    populated_count = sum(row["is_populated"] for row in rows)
    return {
        "has_acroform_fields": False,
        "number_of_pages": len(reader.pages),
        "number_of_fields": len(rows),
        "number_of_widgets": 0,
        "number_of_populated_fields": populated_count,
        "fields": rows,
        "widgets": [],
        "form_schema_hash": None,
        "source_form_schema_hash": None,
        "canonical_template_schema_hash": manifest["canonical_schema_hash"],
        "template_family": manifest["form_family"],
        "template_version": manifest["form_version"],
        "registration_method": registration["method"],
        "registration_quality": registration["quality"],
        "registration_residual_pt": registration["residual"],
        "number_of_canonical_fields": int(manifest["canonical_field_count"]),
        "number_of_canonical_widgets": int(manifest["canonical_widget_count"]),
        "source_widget_count": 0,
        "extraction_method": "spatial_template",
        "extraction_status": "success",
        "anchor_fraction": fraction,
        "anchor_matches": matched,
        "anchor_eligible": eligible,
        "control_threshold": threshold,
        "pdf_metadata": _metadata(reader),
        "pypdf_version": getattr(pypdf, "__version__", "unknown"),
    }
