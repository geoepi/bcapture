"""Non-confidential PDF architectures for structural regression tests."""
import io
from pathlib import Path
from pypdf import PdfReader, PdfWriter
from pypdf.generic import (ArrayObject, DictionaryObject, NameObject,
                          NumberObject, TextStringObject, DecodedStreamObject)

def set_template(path):
    global _template
    _template = Path(path)

def _bytes(writer):
    stream = io.BytesIO()
    writer.write(stream)
    return stream.getvalue()

def epi_bytes(duplicate=False, mapped_duplicate=False, nul_value=False, minimal=False):
    if not (duplicate or mapped_duplicate or nul_value or minimal):
        writer = PdfWriter()
        writer.clone_document_from_reader(PdfReader(_template))
        return _bytes(writer)
    writer = PdfWriter()
    for _ in range(12):
        writer.add_blank_page(612, 792)
    fields = ArrayObject()
    names = ["premid", "p0001", "p0100a", "p0300", "premname"]
    if duplicate or mapped_duplicate:
        names.append("premid" if duplicate else "other")
    for i, name in enumerate(names):
        field = DictionaryObject({NameObject("/T"): TextStringObject(name),
            NameObject("/FT"): NameObject("/Tx"), NameObject("/Subtype"): NameObject("/Widget"),
            NameObject("/Rect"): ArrayObject([NumberObject(n) for n in [10, 10, 100, 30]]),
            NameObject("/V"): TextStringObject("SYNTHETIC\x00VALUE" if nul_value and i==0 else "SYNTHETIC")})
        if mapped_duplicate and i in (0, len(names)-1):
            field[NameObject("/TM")] = TextStringObject("shared")
        ref = writer._add_object(field)
        fields.append(ref)
    writer.pages[0][NameObject("/Annots")] = fields
    writer._root_object[NameObject("/AcroForm")] = writer._add_object(DictionaryObject({NameObject("/Fields"): fields}))
    return _bytes(writer)

def create_structure(path, kind):
    if kind in {"radio_group", "unnamed_ancestor"}:
        writer = PdfWriter()
        writer.add_blank_page(612, 792)
        if kind == "unnamed_ancestor":
            ancestor = writer._add_object(DictionaryObject())
            field = DictionaryObject({NameObject("/T"): TextStringObject("leaf"), NameObject("/Parent"): ancestor,
                                      NameObject("/FT"): NameObject("/Tx"), NameObject("/V"): TextStringObject("SYNTHETIC")})
            fields = ArrayObject([writer._add_object(field)])
        else:
            parent = DictionaryObject({NameObject("/T"): TextStringObject("radio"), NameObject("/FT"): NameObject("/Btn"),
                NameObject("/Ff"): NumberObject(1 << 15), NameObject("/V"): NameObject("/Yes")})
            parent_ref = writer._add_object(parent)
            kids = ArrayObject()
            for state in ("Yes", "No"):
                appearance = DecodedStreamObject()
                appearance.set_data(b"")
                appearance_ref = writer._add_object(appearance)
                child = DictionaryObject({NameObject("/Subtype"): NameObject("/Widget"), NameObject("/Parent"): parent_ref,
                    NameObject("/Rect"): ArrayObject([NumberObject(n) for n in [10,10,20,20]]),
                    NameObject("/AP"): DictionaryObject({NameObject("/N"): DictionaryObject({NameObject("/Off"):appearance_ref, NameObject("/"+state):appearance_ref})})})
                kids.append(writer._add_object(child))
            parent[NameObject("/Kids")] = kids
            writer.pages[0][NameObject("/Annots")] = kids
            fields = ArrayObject([parent_ref])
        writer._root_object[NameObject("/AcroForm")] = writer._add_object(DictionaryObject({NameObject("/Fields"):fields}))
        Path(path).write_bytes(_bytes(writer))
        return
    if kind in {"concatenated_epi", "epi_supplemental"}:
        writer = PdfWriter()
        writer.append(PdfReader(io.BytesIO(epi_bytes())))
        if kind == "concatenated_epi":
            writer.append(PdfReader(io.BytesIO(epi_bytes())))
        else:
            for _ in range(3):
                writer.add_blank_page(612, 792)
        Path(path).write_bytes(_bytes(writer))
        return
    if kind in {"duplicate_fields", "mapped_duplicate_fields", "nul_field"}:
        Path(path).write_bytes(epi_bytes(duplicate=kind=="duplicate_fields", mapped_duplicate=kind=="mapped_duplicate_fields", nul_value=kind=="nul_field"))
        return
    writer = PdfWriter()
    writer.add_blank_page(612, 792)
    if kind in {"xfa", "hybrid_xfa"}:
        if kind == "hybrid_xfa":
            writer.clone_document_from_reader(PdfReader(io.BytesIO(epi_bytes())))
        else:
            writer._root_object[NameObject("/AcroForm")] = DictionaryObject({NameObject("/Fields"):ArrayObject()})
        writer._root_object["/AcroForm"][NameObject("/XFA")] = TextStringObject("SYNTHETIC XFA")
        Path(path).write_bytes(_bytes(writer))
        return
    if kind in {"image_scan", "nul_metadata_scan", "nul_metadata_key_scan"}:
        image = DecodedStreamObject()
        image.set_data(b"\xff\xff\xff")
        image.update({NameObject("/Type"): NameObject("/XObject"), NameObject("/Subtype"): NameObject("/Image"),
                      NameObject("/Width"): NumberObject(1), NameObject("/Height"): NumberObject(1),
                      NameObject("/ColorSpace"): NameObject("/DeviceRGB"), NameObject("/BitsPerComponent"): NumberObject(8)})
        writer.pages[0][NameObject("/Resources")] = DictionaryObject({NameObject("/XObject"): DictionaryObject({NameObject("/Im0"): writer._add_object(image)})})
        content = DecodedStreamObject()
        content.set_data(b"q 612 0 0 792 0 0 cm /Im0 Do Q")
        writer.pages[0][NameObject("/Contents")] = writer._add_object(content)
        if kind == "nul_metadata_scan":
            writer.add_metadata({"/Producer": "SYNTHETIC\x00PRODUCER"})
        if kind == "nul_metadata_key_scan":
            writer.add_metadata({"/SYNTHETIC\x00KEY": "SYNTHETIC"})
    elif kind == "encrypted_attachment":
        attached = PdfWriter()
        attached.add_blank_page(612, 792)
        attached.encrypt("synthetic-password")
        writer.add_attachment("opaque.bin", _bytes(attached))
    elif kind == "unreadable_attachment":
        writer.add_attachment("opaque.bin", b"%PDF-1.7\ninvalid synthetic PDF\n")
    elif kind in {"missing_stream", "malformed_stream"}:
        writer.add_attachment("opaque.bin", b"SYNTHETIC")
        spec = writer._root_object["/Names"]["/EmbeddedFiles"]["/Names"][1].get_object()
        if kind == "missing_stream":
            del spec["/EF"]
        else:
            spec[NameObject("/EF")] = DictionaryObject({NameObject("/F"): NumberObject(1)})
    elif kind in {"c2pa_form", "c2pa_empty", "c2pa_wrong_type", "c2pa_pdf", "unknown_attachment"}:
        if kind in {"c2pa_form", "c2pa_pdf"}:
            writer.clone_document_from_reader(PdfReader(io.BytesIO(epi_bytes())))
        writer.add_attachment("opaque.bin", epi_bytes() if kind == "c2pa_pdf" else b"SYNTHETIC MANIFEST")
        spec = writer._root_object["/Names"]["/EmbeddedFiles"]["/Names"][1].get_object()
        spec[NameObject("/AFRelationship")] = NameObject("/C2PA_Manifest" if kind != "unknown_attachment" else "/Data")
        stream = spec["/EF"]["/F"].get_object()
        stream[NameObject("/Subtype")] = NameObject("/application/c2pa" if kind != "c2pa_wrong_type" else "/application/octet-stream")
    else:
        supplemental = PdfWriter()
        supplemental.add_blank_page(612, 792)
        if kind == "top_level_and_embedded":
            writer = PdfWriter()
            writer.clone_document_from_reader(PdfReader(io.BytesIO(epi_bytes())))
        if kind == "long_supplement":
            for _ in range(10):
                supplemental.add_blank_page(612, 792)
        if kind == "nested_container":
            supplemental.add_attachment("inner.bin", epi_bytes())
            supplemental._root_object[NameObject("/Collection")] = DictionaryObject()
        writer.add_attachment("same.bin", _bytes(supplemental))
        if kind != "flattened_attachment":
            writer.add_attachment("same.bin", epi_bytes(minimal=kind=="partial_embedded"))
        if kind == "multiple_embedded":
            writer.add_attachment("another.bin", epi_bytes())
        if kind == "incompatible_embedded":
            other = PdfWriter()
            other.clone_document_from_reader(PdfReader(io.BytesIO(epi_bytes())))
            other_fields = other._root_object["/AcroForm"]["/Fields"]
            for field in other_fields:
                field.get_object()[NameObject("/T")] = TextStringObject("other_"+str(field.get_object()["/T"]))
            writer = PdfWriter()
            writer.add_blank_page(612, 792)
            writer.add_attachment("opaque.bin", _bytes(other))
        if kind == "af_only":
            node = writer._root_object["/Names"]["/EmbeddedFiles"]
            writer._root_object[NameObject("/AF")] = ArrayObject(node["/Names"][1::2])
            del writer._root_object["/Names"]
        if kind != "embedded":
            writer._root_object[NameObject("/Collection")] = DictionaryObject()
    Path(path).write_bytes(_bytes(writer))
