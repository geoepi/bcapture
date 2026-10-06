"""Fail-closed routing of PDF containers without writing their contents to disk."""
from __future__ import annotations
import hashlib
import io
import re
from pypdf import PdfReader

class PDFStructureError(RuntimeError):
    def __init__(self, failure_type, message):
        super().__init__(message)
        self.failure_type = failure_type

def _resolve(obj):
    return obj.get_object() if hasattr(obj, "get_object") else obj

def _identity(obj):
    reference = getattr(obj, "indirect_reference", None) or obj
    if hasattr(reference, "idnum"):
        return (id(reference.pdf), reference.idnum, reference.generation)
    return id(obj)

def _qualified_name(field):
    parts, seen = [], set()
    current = field
    while current is not None:
        current = _resolve(current)
        key = _identity(current)
        if key in seen:
            raise PDFStructureError("ambiguous_acroform_structure", "Cyclic AcroForm parent structure.")
        seen.add(key)
        if current.get("/T"):
            parts.append(str(current["/T"]))
        current = current.get("/Parent")
    return ".".join(reversed(parts))

def audited_fields(reader):
    """Reject dictionary-key collisions before get_fields can discard a field."""
    root = reader.trailer["/Root"]
    acroform = _resolve(root.get("/AcroForm")) or {}
    names, mappings, visited, active = set(), set(), set(), set()
    logical_count = 0
    def walk(references):
        nonlocal logical_count
        for ref in references or []:
            field = _resolve(ref)
            identity = _identity(field)
            if identity in active:
                raise PDFStructureError("ambiguous_acroform_structure", "Cyclic AcroForm field tree.")
            if identity in visited:
                continue
            visited.add(identity)
            active.add(identity)
            if "/T" in field or "/TM" in field:
                logical_count += 1
                name = _qualified_name(field)
                mapping = str(field.get("/TM", ""))
                if (name and name in names) or (mapping and mapping in mappings):
                    raise PDFStructureError("ambiguous_acroform_structure", "Multiple logical fields share an AcroForm mapping key.")
                if name:
                    names.add(name)
                if mapping:
                    mappings.add(mapping)
            walk(field.get("/Kids"))
            active.remove(identity)
    walk(acroform.get("/Fields"))
    fields = reader.get_fields() or {}
    if logical_count != len(fields):
        raise PDFStructureError("ambiguous_acroform_structure", "The AcroForm field tree and parsed logical fields disagree.")
    return fields

def embedded_specs(reader):
    """Enumerate FileSpecs by object identity, never by attachment filename."""
    root = reader.trailer["/Root"]
    specs, seen_specs, seen_nodes = [], set(), set()
    def add(ref):
        spec = _resolve(ref)
        identity = _identity(spec)
        if identity not in seen_specs:
            seen_specs.add(identity)
            specs.append(spec)
    def walk(ref):
        node = _resolve(ref)
        if not node or _identity(node) in seen_nodes:
            return
        seen_nodes.add(_identity(node))
        entries = node.get("/Names", [])
        if len(entries) % 2:
            raise PDFStructureError("unsupported_pdf_container", "Malformed embedded-file name tree.")
        for index in range(1, len(entries), 2):
            add(entries[index])
        for kid in node.get("/Kids", []):
            walk(kid)
    names = _resolve(root.get("/Names")) or {}
    walk(names.get("/EmbeddedFiles"))
    for spec in root.get("/AF", []):
        add(spec)
    for page in reader.pages:
        for ref in page.get("/Annots", []):
            annotation = _resolve(ref)
            if str(annotation.get("/Subtype")) == "/FileAttachment" and annotation.get("/FS"):
                add(annotation["/FS"])
    return specs

def _epi_signature(names):
    return all(anchor in names or any(re.fullmatch(re.escape(anchor)+"[a-d]", str(name)) for name in names)
               for anchor in ("premid", "p0001", "p0100", "p0300"))

def _reject_xfa(reader):
    acroform = _resolve(reader.trailer["/Root"].get("/AcroForm")) or {}
    if "/XFA" in acroform:
        raise PDFStructureError("unsupported_xfa_form", "XFA and hybrid XFA forms require separate template support.")


def _form_attachments(specs):
    """Exclude only structurally identified, non-PDF C2PA provenance manifests."""
    attachments = []
    for ordinal, spec in enumerate(specs, 1):
        try:
            ef = _resolve(spec.get("/EF")) or {}
            ref = ef.get("/UF") or ef.get("/F") or next(iter(ef.values()), None)
            if ref is None:
                raise ValueError("Missing embedded stream")
            stream = _resolve(ref)
            data = stream.get_data()
            subtype = str(stream.get("/Subtype", "")).replace("#2F", "/").lower().lstrip("/")
        except Exception as error:
            raise PDFStructureError("unsupported_pdf_container", "An embedded file stream could not be inspected.") from error
        pdf_like = b"%PDF-" in data[:1024] or subtype == "application/pdf"
        ancillary = (str(spec.get("/AFRelationship")) == "/C2PA_Manifest"
                     and subtype == "application/c2pa" and not pdf_like)
        if ancillary:
            continue
        if not pdf_like:
            raise PDFStructureError("unsupported_non_pdf_attachment", "An unrecognized non-PDF attachment requires separate review.")
        attachments.append((ordinal, data))
    return attachments


def standalone_preflight(reader):
    if reader.is_encrypted:
        raise PDFStructureError("encrypted_pdf", "Encrypted PDFs require separate authorization.")
    _reject_xfa(reader)
    attachments = _form_attachments(embedded_specs(reader))
    if "/Collection" in reader.trailer["/Root"] or attachments:
        raise PDFStructureError("compound_pdf_structure", "This form family does not support PDF containers.")


def extract_epi_document(path, interactive_page_count, printed_page_count,
                         canonical_field_count, canonical_widget_count, canonical_schema_hash):
    """Reuse canonical AcroForm extraction for one unambiguous embedded form."""
    from acroform import _extract_reader
    reader = PdfReader(path)
    if reader.is_encrypted:
        raise PDFStructureError("encrypted_pdf", "Encrypted PDFs require separate authorization.")
    _reject_xfa(reader)
    fields = audited_fields(reader)
    specs = embedded_specs(reader)
    attachments = _form_attachments(specs)
    portfolio = "/Collection" in reader.trailer["/Root"]
    provenance = {"source_pdf_structure": "standalone", "source_container_pages": len(reader.pages),
                  "embedded_file_count": len(specs), "embedded_pdf_ordinal": None, "embedded_pdf_sha256": None}
    if not attachments:
        if portfolio:
            raise PDFStructureError("unsupported_pdf_portfolio", "The PDF Portfolio has no inspectable embedded files.")
        if fields and _epi_signature(fields) and len(reader.pages) != interactive_page_count:
            raise PDFStructureError("ambiguous_form_pages", "The Initial Epi AcroForm page count differs from the registered interactive template.")
        parsed = _extract_reader(reader)
        parsed.update(provenance)
        return parsed
    if fields:
        raise PDFStructureError("compound_pdf_structure", "Top-level AcroForm fields and embedded content require separate review.")
    candidates = []
    other_pdf_pages = 0
    for ordinal, data in attachments:
        try:
            attached = PdfReader(io.BytesIO(data))
            if attached.is_encrypted:
                raise PDFStructureError("encrypted_or_unreadable_embedded_pdf", "An embedded PDF is encrypted.")
            _reject_xfa(attached)
            if "/Collection" in attached.trailer["/Root"] or _form_attachments(embedded_specs(attached)):
                raise PDFStructureError("unsupported_nested_pdf_container", "Nested PDF containers require separate review.")
            attached_fields = audited_fields(attached)
            # Force page-tree traversal before accepting a supposedly empty attachment.
            len(attached.pages)
        except PDFStructureError:
            raise
        except Exception as error:
            raise PDFStructureError("encrypted_or_unreadable_embedded_pdf", "An embedded PDF could not be inspected.") from error
        if attached_fields:
            candidates.append((ordinal, hashlib.sha256(data).hexdigest(), attached, attached_fields))
        else:
            other_pdf_pages += len(attached.pages)
    if len(candidates) > 1:
        raise PDFStructureError("multiple_embedded_acroforms", "Multiple embedded PDFs have AcroForm fields; no form was selected.")
    if not candidates:
        raise PDFStructureError("unsupported_embedded_flattened_pdf", "The container has no supported embedded AcroForm; embedded spatial extraction is unsupported.")
    ordinal, checksum, attached, names = candidates[0]
    if not _epi_signature(names):
        raise PDFStructureError("unexpected_form_type", "The sole embedded AcroForm does not have the Initial Epi signature.")
    if len(attached.pages) != interactive_page_count:
        raise PDFStructureError("unsupported_embedded_form_layout", "The embedded AcroForm page count differs from the canonical Initial Epi template.")
    if len(reader.pages) + other_pdf_pages >= printed_page_count:
        raise PDFStructureError("ambiguous_embedded_pdf", "The remaining PDF pages could contain another form; no attachment was selected.")
    parsed = _extract_reader(attached)
    if (parsed["number_of_fields"] != canonical_field_count
            or parsed["number_of_widgets"] != canonical_widget_count
            or parsed["form_schema_hash"] != canonical_schema_hash):
        raise PDFStructureError("unsupported_embedded_form_schema", "The embedded AcroForm does not match the registered canonical schema and control counts.")
    provenance.update(source_pdf_structure="pdf_portfolio" if portfolio else "embedded_files",
                      embedded_pdf_ordinal=ordinal, embedded_pdf_sha256=checksum)
    parsed.update(provenance)
    return parsed
