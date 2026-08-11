"""Low-level PDF AcroForm interrogation for bcapture."""

import hashlib
import json

import pypdf


def pdf_value_to_string(value):
    if value is None:
        return None
    if isinstance(value, bytes):
        return value.decode("utf-8", errors="replace")
    return str(value)


def clean_field_value(value):
    value = pdf_value_to_string(value)
    if value is None:
        return None
    value = value.strip()
    return value[1:] if value.startswith("/") else value


def states_to_string(states):
    if not states:
        return None
    return "|".join(clean_field_value(state) for state in states if state is not None)


def _field_name(annotation):
    current = annotation
    parts = []
    while current is not None:
        name = current.get("/T")
        if name is not None:
            parts.append(pdf_value_to_string(name))
        current = current.get("/Parent")
    return ".".join(reversed(parts)) if parts else None


def _annotation_pages(reader):
    pages = {}
    for page_number, page in enumerate(reader.pages, start=1):
        annotations = page.get("/Annots") or []
        for annotation_ref in annotations:
            annotation = annotation_ref.get_object()
            name = _field_name(annotation)
            if name is not None:
                pages.setdefault(name, page_number)
    return pages


def _field_states(field):
    states = field.get("/_States_")
    if states:
        return list(states)
    appearance = field.get("/AP")
    normal = appearance.get("/N") if appearance else None
    if normal and hasattr(normal, "keys"):
        return list(normal.keys())
    return []


def get_page_field_map(reader):
    return _annotation_pages(reader)


def _pdf_metadata(reader):
    result = {}
    metadata = reader.metadata or {}
    for key, value in metadata.items():
        key = pdf_value_to_string(key).lstrip("/").lower()
        key = "".join(character if character.isalnum() else "_" for character in key)
        result["pdf_" + key] = pdf_value_to_string(value)
    return result


def _schema_hash(fields):
    definitions = [{"field": f["field"], "field_type": f["field_type"], "states": f["states"]} for f in fields]
    payload = json.dumps(definitions, ensure_ascii=True, separators=(",", ":"))
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def schema_hash(fields):
    """Expose schema hashing for lightweight package tests."""
    return _schema_hash(fields)


def pypdf_version():
    return getattr(pypdf, "__version__", "unknown")


def extract_form(pdf_path):
    reader = pypdf.PdfReader(pdf_path)
    page_map = get_page_field_map(reader)
    form_fields = reader.get_fields() or {}
    fields = []
    for field_index, (field_name, field) in enumerate(form_fields.items(), start=1):
        field_name = pdf_value_to_string(field_name)
        states = _field_states(field)
        raw_value = field.get("/V")
        normalized = clean_field_value(raw_value)
        field_type = clean_field_value(field.get("/FT"))
        is_button_off = normalized == "Off" and field_type in ("Btn", "button")
        is_populated = normalized is not None and normalized != "" and not is_button_off
        fields.append({
            "field_index": field_index,
            "page": page_map.get(field_name),
            "field": field_name,
            "alternative_name": clean_field_value(field.get("/TU")),
            "field_type": field_type,
            "value_raw": pdf_value_to_string(raw_value),
            "value": normalized,
            "states": states_to_string(states),
            "is_populated": is_populated,
        })
    return {
        "has_acroform_fields": bool(form_fields),
        "number_of_pages": len(reader.pages),
        "number_of_fields": len(fields),
        "fields": fields,
        "form_schema_hash": _schema_hash(fields) if fields else None,
        "pdf_metadata": _pdf_metadata(reader),
    }
