"""Low-level PDF AcroForm interrogation for bcapture."""

import hashlib
import json

import pypdf


def _deref(value):
    return value.get_object() if hasattr(value, "get_object") else value


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


def normalized_states(states):
    """Return a duplicate-free, sorted state list for semantic comparison."""
    values = []
    for state in states or []:
        value = clean_field_value(state)
        if value is not None and value != "" and value not in values:
            values.append(value)
    return sorted(values)


def _field_name(annotation):
    current = _deref(annotation)
    parts = []
    while current is not None:
        name = current.get("/T")
        if name is not None:
            parts.append(pdf_value_to_string(name))
        current = _deref(current.get("/Parent"))
    return ".".join(reversed(parts)) if parts else None


def _parent_field_name(annotation):
    parent = _deref(_deref(annotation).get("/Parent"))
    if parent is None:
        return None
    return clean_field_value(parent.get("/T"))


def _inherited_value(annotation, key):
    current = _deref(annotation)
    while current is not None:
        value = current.get(key)
        if value is not None:
            return value
        current = _deref(current.get("/Parent"))
    return None


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
    field = _deref(field)
    states = field.get("/_States_")
    if states:
        return list(states)
    appearance = _deref(field.get("/AP"))
    normal = appearance.get("/N") if appearance else None
    if normal and hasattr(normal, "keys"):
        return list(normal.keys())
    return []


def _rect(annotation):
    rect = _deref(annotation).get("/Rect") or []
    values = [float(value) for value in rect]
    return (values + [None, None, None, None])[:4]


def extract_widgets(reader):
    """Extract widget annotations without making them analytical fields."""
    widgets = []
    widget_index = 0
    for page_number, page in enumerate(reader.pages, start=1):
        annotations = page.get("/Annots") or []
        for annotation_ref in annotations:
            annotation = _deref(annotation_ref)
            if clean_field_value(annotation.get("/Subtype")) != "Widget":
                continue
            widget_index += 1
            field_name = clean_field_value(annotation.get("/T"))
            full_field_name = _field_name(annotation)
            parent = _deref(annotation.get("/Parent"))
            parent_value = parent.get("/V") if parent is not None else None
            field_value = _inherited_value(annotation, "/V")
            field_type = clean_field_value(_inherited_value(annotation, "/FT"))
            states = _field_states(annotation)
            if not states and parent is not None:
                states = _field_states(parent)
            rect_x1, rect_y1, rect_x2, rect_y2 = _rect(annotation)
            widgets.append({
                "page": page_number,
                "widget_index": widget_index,
                "field_name": field_name,
                "full_field_name": full_field_name,
                "parent_field_name": _parent_field_name(annotation),
                "field_type": field_type,
                "rect_x1": rect_x1,
                "rect_y1": rect_y1,
                "rect_x2": rect_x2,
                "rect_y2": rect_y2,
                "appearance_state": clean_field_value(annotation.get("/AS")),
                "value": clean_field_value(field_value),
                "parent_value": clean_field_value(parent_value),
                "states": states_to_string(states),
            })
    return widgets


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
    definitions = []
    for field in fields:
        raw_states = field.get("states")
        if isinstance(raw_states, str):
            raw_states = raw_states.split("|") if raw_states else []
        definitions.append({
            "field": field["field"],
            "field_type": clean_field_value(field.get("field_type")),
            "states": normalized_states(raw_states),
        })
    definitions.sort(key=lambda definition: definition["field"] or "")
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
    widgets = extract_widgets(reader)
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
        "number_of_widgets": len(widgets),
        "fields": fields,
        "widgets": widgets,
        "form_schema_hash": _schema_hash(fields) if fields else None,
        "pdf_metadata": _pdf_metadata(reader),
    }
