"""Build a small AcroForm PDF for the optional pypdf integration test."""

from pypdf import PdfWriter
from pypdf.generic import (
    ArrayObject,
    BooleanObject,
    DictionaryObject,
    NameObject,
    NumberObject,
    ByteStringObject,
    TextStringObject,
)


def create_synthetic_acroform(path, signature_variant=0):
    writer = PdfWriter()
    page = writer.add_blank_page(width=300, height=300)
    fields = ArrayObject()

    def add_field(name, field_type, value=None, default=None, options=None, flags=0, states=None):
        field = DictionaryObject({
            NameObject("/Type"): NameObject("/Annot"),
            NameObject("/Subtype"): NameObject("/Widget"),
            NameObject("/FT"): NameObject(field_type),
            NameObject("/T"): TextStringObject(name),
            NameObject("/Rect"): ArrayObject([NumberObject(10), NumberObject(10), NumberObject(100), NumberObject(30)]),
            NameObject("/F"): NumberObject(4),
            NameObject("/Ff"): NumberObject(flags),
        })
        if value is not None:
            field[NameObject("/V")] = value if isinstance(value, (ArrayObject, DictionaryObject)) else TextStringObject(value)
        if default is not None:
            field[NameObject("/DV")] = TextStringObject(default)
        if options is not None:
            field[NameObject("/Opt")] = ArrayObject([TextStringObject(option) for option in options])
        if states is not None:
            normal = DictionaryObject()
            for state in states:
                normal[NameObject("/" + state)] = DictionaryObject()
            field[NameObject("/AP")] = DictionaryObject({NameObject("/N"): normal})
        reference = writer._add_object(field)
        page[NameObject("/Annots")].append(reference) if "/Annots" in page else page.__setitem__(NameObject("/Annots"), ArrayObject([reference]))
        fields.append(reference)

    add_field("text", "/Tx", value="hello", default="hello")
    add_field("button", "/Btn", value="1", states=["Off", "1", "3"])
    add_field("choice", "/Ch", value="A", default="Select or Type", options=["A", "B", "C"])
    add_field("multichoice", "/Ch", value="A", options=["A", "B", "C"], flags=1 << 21)
    signature_entries = [
        ("/Type", NameObject("/Sig")),
        ("/Filter", NameObject("/Adobe.PPKLite")),
        ("/SubFilter", NameObject("/adbe.pkcs7.detached")),
        ("/Name", TextStringObject("Synthetic Signer")),
        ("/ByteRange", ArrayObject([NumberObject(0), NumberObject(10), NumberObject(20), NumberObject(30)])),
        ("/Contents", ByteStringObject(b"SYNTHETIC-SIGNATURE")),
    ]
    if signature_variant:
        signature_entries = list(reversed(signature_entries))
    signed_signature = DictionaryObject()
    for key, value in signature_entries:
        signed_signature[NameObject(key)] = value
    add_field("signature_signed", "/Sig", value=signed_signature)
    add_field("signature_empty", "/Sig")
    writer._root_object[NameObject("/AcroForm")] = writer._add_object(DictionaryObject({
        NameObject("/Fields"): fields,
        NameObject("/NeedAppearances"): BooleanObject(True),
    }))
    with open(path, "wb") as output:
        writer.write(output)
