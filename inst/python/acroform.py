"""Form-neutral low-level PDF AcroForm interrogation."""

import hashlib
import json

import pypdf
from pdf_structure import audited_fields, PDFStructureError, extract_epi_document, standalone_preflight


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


def _choice_option(option):
    option = _deref(option)
    if isinstance(option, (list, tuple)):
        # PDF choice options may be [export value, display value].  The
        # exported value is the stable raw option, with display as fallback.
        option = option[0] if option else None
    return clean_field_value(option)


def _field_options(field):
    options = _inherited_value(field, "/Opt")
    if options is None:
        return []
    return [_choice_option(option) for option in options if _choice_option(option) is not None]


def options_to_string(options):
    if not options:
        return None
    return "|".join(str(option) for option in options)


def normalized_options(options):
    """Return a duplicate-free, sorted option list for schema comparison."""
    if isinstance(options, str):
        options = options.split("|") if options else []
    values = []
    for option in options or []:
        value = _choice_option(option)
        if value is not None and value != "" and value not in values:
            values.append(value)
    return sorted(values)


def is_multiselect(field_flags, field_type):
    """Interpret the PDF Choice MultiSelect flag (bit 22, zero-indexed 21)."""
    return clean_field_value(field_type) == "Ch" and bool((int(field_flags or 0) >> 21) & 1)


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
            field_flags = _inherited_value(annotation, "/Ff")
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
                "field_flags": int(field_flags) if field_flags is not None else None,
                "rect_x1": rect_x1,
                "rect_y1": rect_y1,
                "rect_x2": rect_x2,
                "rect_y2": rect_y2,
                "appearance_state": clean_field_value(annotation.get("/AS")),
                "value": clean_field_value(field_value),
                "parent_value": clean_field_value(parent_value),
                "states": states_to_string(states),
                "options": options_to_string(_field_options(annotation)),
            })
    return widgets


def get_page_field_map(reader):
    return _annotation_pages(reader)


def _pdf_metadata(reader):
    result = {}
    omitted = []
    metadata = reader.metadata or {}
    for key, value in metadata.items():
        raw_key = pdf_value_to_string(key)
        if "\x00" in raw_key:
            omitted.append("unrepresentable_metadata_key")
            continue
        key = raw_key.lstrip("/").lower()
        key = "".join(character if character.isalnum() else "_" for character in key)
        converted = pdf_value_to_string(value)
        if converted is not None and "\x00" in converted:
            omitted.append("pdf_" + key)
        else:
            result["pdf_" + key] = converted
    if omitted:
        result["pdf_metadata_omitted_nul_count"] = len(omitted)
        result["pdf_metadata_omitted_nul_keys"] = "|".join(sorted(omitted))
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
            "options": normalized_options(field.get("options", [])),
            "field_flags": int(field.get("field_flags") or 0) if clean_field_value(field.get("field_type")) == "Ch" else 0,
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
    standalone_preflight(reader)
    return _extract_reader(reader)


def extract_epi_form(pdf_path, interactive_page_count, printed_page_count,
                     canonical_field_count, canonical_widget_count, canonical_schema_hash):
    return extract_epi_document(pdf_path, interactive_page_count, printed_page_count,
                                canonical_field_count, canonical_widget_count, canonical_schema_hash)


def _extract_reader(reader):
    form_fields = audited_fields(reader)
    page_map = get_page_field_map(reader)
    widgets = extract_widgets(reader)
    fields = []
    for field_index, (field_name, field) in enumerate(form_fields.items(), start=1):
        field_name = pdf_value_to_string(field_name)
        states = _field_states(field)
        raw_value = field.get("/V")
        raw_default = _inherited_value(field, "/DV")
        normalized = clean_field_value(raw_value)
        normalized_default = clean_field_value(raw_default)
        field_type = clean_field_value(_inherited_value(field, "/FT"))
        field_flags = _inherited_value(field, "/Ff")
        options = _field_options(field)
        is_button_off = normalized == "Off" and field_type in ("Btn", "button")
        is_populated = normalized is not None and normalized != "" and not is_button_off
        fields.append({
            "field_index": field_index,
            "page": page_map.get(field_name),
            "field": field_name,
            "alternative_name": clean_field_value(field.get("/TU")),
            "field_type": field_type,
            "field_flags": int(field_flags) if field_flags is not None else None,
            "value_raw": pdf_value_to_string(raw_value),
            "value": normalized,
            "default_value_raw": pdf_value_to_string(raw_default),
            "default_value": normalized_default,
            "is_default_value": normalized is not None and normalized_default is not None and normalized == normalized_default,
            "states": states_to_string(states),
            "options": options_to_string(options),
            "is_multiselect": is_multiselect(field_flags, field_type),
            "is_populated": is_populated,
        })
    for row in fields + widgets:
        if any(isinstance(value, str) and "\x00" in value for value in row.values()):
            raise PDFStructureError("unsupported_pdf_string", "A logical field or widget contains an unrepresentable PDF string.")
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
