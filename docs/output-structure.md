# Extraction output contract

For a step-by-step user workflow that produces these extraction products, see
the [BCAP workflow tutorial](tutorials/bcap-workflow.md).

`extract_hpai()` writes extraction products below the requested `out_dir`:

```text
out_dir/
├── audits/
│   └── <audit_id>/
│       ├── <audit_id>_fields_long.csv
│       ├── <audit_id>_populated_fields_long.csv
│       ├── <audit_id>_fields_wide.csv
│       ├── <audit_id>_metadata.csv
│       └── <audit_id>_widgets.csv
├── combined/
│   ├── hpai_fields_long.csv
│   ├── hpai_populated_fields_long.csv
│   ├── hpai_audits_wide.csv
│   ├── hpai_metadata.csv
│   └── hpai_widgets.csv
├── diagnostics/
│   ├── schema_summary.csv
│   ├── schema_groups.csv
│   ├── schema_pairwise_comparison.csv
│   ├── field_presence_differences.csv
│   ├── field_type_differences.csv
│   ├── field_state_differences.csv
│   ├── field_order_differences.csv
│   ├── widget_encoding_differences.csv
│   └── schema_diagnostics.md
└── extraction_manifest.csv
```

## Per-audit field products

`fields_long` is the canonical, loss-minimizing representation. It has one
row per PDF field, including blank fields and unchecked `/Off` button fields.
Its stable columns are `audit_id`, `source_file`, `source_relpath`,
`source_md5`, `field_index`, `page`, `field`, `alternative_name`,
`field_type`, `value_raw`, `value`, `states`, `is_populated`, and
`extraction_method`. `source_relpath` is relative to `in_dir`; absolute host
paths are not persisted.

`value_raw` retains the source PDF representation. `value` is a display value:
`/Yes`, `/No`, and `/Off` become `Yes`, `No`, and `Off`. `/Off` is retained in
the master table but has `is_populated = FALSE`; missing and blank values are
also not populated. This distinguishes an unchecked remediation box from an
unknown field.

`populated_fields_long` has the same columns and is restricted to
`is_populated == TRUE`. It is a convenience QA product. `fields_wide` has one
row, begins with provenance columns `audit_id`, `source_file`,
`source_relpath`, `source_md5`, and `form_schema_hash`, and then uses the PDF
field names as columns without unnecessary semantic recoding.

`extraction_method` is currently `acroform`. Future extraction modes can use
`ocr`, `handwriting`, or `manual_review` without changing the core table.

`number_of_fields` always counts logical fields returned by
`pypdf.PdfReader.get_fields()`. `number_of_widgets` counts `/Widget`
annotations encountered across pages. A logical field may have multiple
widgets, especially for radio/button controls. The widget table is a
diagnostic representation and is not the canonical analytical extraction.

Field types use the normalized convention `Btn`, `Tx`, `Ch`, and `Sig`.

## Metadata and manifest

`metadata` has one row per successful PDF with page, field, population, schema,
checksum, method, pypdf version, and UTC extraction time. Available PDF
metadata fields are appended with a `pdf_` prefix.

`extraction_manifest.csv` has one row for every input PDF, whether successful
or failed. A failure has `status = failed` and a useful `failure_type`, such as
`no_acroform_fields` or `pdf_read_error`; it does not create an empty success
record. The manifest includes the same provenance fields and extraction counts.

## Combined products, schema hashes, and diagnostics

Combined products row-bind successful per-audit products and use union-of-
columns behavior when schemas differ. `form_schema_hash` is a SHA-256 signature
of the logical field name, normalized field type, and normalized valid state set.
Fields are sorted by name and states are deduplicated and sorted before hashing;
field order, page position, current values, and populated status do not affect
the hash. `schema_group` assigns deterministic human-readable labels such as
`schema_001` within a batch.

`diagnostics/` contains differences rather than another complete extraction.
It compares field presence, field types, normalized state sets, field ordering,
schema pairs, and possible alternate widget encodings. Field order is an
`INFO`-level difference and is excluded from schema identity. A possible
split-radio or multi-widget encoding is diagnostic evidence only; bcapture does
not merge fields, repair PDF structures, or infer BCAP semantics.

A schema difference does not necessarily indicate a different visible form
version. PDF software can serialize AcroForm widgets and radio groups
differently. bcapture preserves those raw structural differences and reports
them separately from semantic interpretation.

`populated_fields_long` and `audits_wide` are convenience representations
derived from the canonical `fields_long` output. They should not replace the
long table for archival or future semantic collation.
