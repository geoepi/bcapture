# BCAP semantic validation and privacy workflow

The BCAP privacy boundary is intentionally separate from raw extraction:

~~~
PDFs
  -> extract_hpai()
  -> diagnose_hpai()
  -> validate_hpai()
  -> deidentify_hpai()
  -> validate_hpai_privacy()
  -> safe output + private crosswalk
~~~

validate_hpai() evaluates the extraction manifest and long field table. It
allows only the known unrecognized_flattened_form outcome as an excluded
record class. It checks categorical responses against extracted button/choice
domains, normalizes the documented BCAP date fields, and rejects unknown field
types or incomplete successful extraction products. It does not infer
undocumented business rules, table semantics, or cross-field contradictions.

## Privacy classification

The canonical 2025-12-08 BCAP template contains 225 fields:

| Classification | Count | Policy |
| --- | ---: | --- |
| SAFE_RETAIN | 160 | Audit controls, categorical responses, and signature presence |
| SAFE_NORMALIZE | 3 | Deterministic ISO normalization for BCAP date fields |
| PSEUDONYMIZE | 15 | Premises, person, organization, phone, and email linkage |
| WITHHOLD | 47 | Facility locations, technical fields, and arbitrary text |
| CROSSWALK_ONLY | 0 | Reserved for future explicit policy |
| REVIEW | 0 | Unknown field types fail closed rather than entering output |

The policy is conservative. It does not claim unrestricted public-release
anonymization. Facility locations and arbitrary text are withheld because no
BCAP-specific release policy or robust text scrubber is established. Direct
identifiers are replaced by stable pseudonyms, and original values remain only
in the private crosswalk.

The /Sig values are structural: populated signatures are
signature_present; signer, certificate, timestamp, ByteRange, and Contents
metadata are not retained.

## Crosswalk and finalization

deidentify_hpai() accepts extracted products rather than raw PDFs. Record and
entity pseudonyms are deterministic and reused from an existing valid
crosswalk. The crosswalk must be physically separate from safe output and
outside version control. Safe output contains no source filenames, source
checksums, raw values, or crosswalk columns.

Finalization writes temporary sibling trees and commits them only after:

~~~
semantic errors = 0
privacy errors = 0
privacy warnings = 0
no REVIEW fields
crosswalk separation and reuse checks pass
~~~

The strict privacy scanner is field-aware. It detects retained email/phone
patterns, pseudonymization failures, withheld values, signature metadata,
crosswalk contamination, and known-source values in pseudonymized fields. It
does not treat equal numeric strings in unrelated retained control fields as a
leak.
