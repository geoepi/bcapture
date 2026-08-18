# Initial Epi de-identification

`deidentify_epi()` is the privacy boundary between identifiable collated
Initial Epi data and controlled-use analytical data:

```text
PDF -> extract_epi() -> collate_epi() -> validate_epi() -> deidentify_epi()
                                                        -> analyze
```

The function creates a second dataset. It treats the existing `out_dir` as
read-only and does not rewrite extraction, collation, or validation products.

## Terminology and scope

The output is de-identified and pseudonymized, not irreversibly anonymous.
The private crosswalk permits authorized re-identification, so it is sensitive
and must be stored separately from the analysis dataset. This implementation
does not make legal, regulatory, HIPAA, compliance, or public-release claims.
The `analysis` profile is intended for controlled analytical use, not
unrestricted distribution.

```r
deidentify_epi(
  out_dir = "epi_output",
  deidentified_dir = "epi_analysis",
  crosswalk_dir = "D:/secure/bcapture_crosswalks/epi_project",
  version = "2024-05-28",
  profile = "analysis",
  strict = TRUE
)
```

Only the versioned `2024-05-28` dictionary and `analysis` profile are supported
in this phase. The policy registry is designed for future profiles such as
`sharing`, `restricted_sharing`, and `public_release_candidate`, but those
profiles are not implemented.

## Policy and transformations

The policy in `inst/extdata/dictionaries/initial_epi/2024-05-28/deidentification_rules.csv`
covers all 497 logical fields exactly once. It is selected by semantic
`raw_field` and records privacy class, action, pseudonym class, coarsening
metadata, and notes. Unknown source fields or emitted value columns cannot
silently pass in strict mode.

Direct identifiers such as premises IDs, premises names, people, organizations,
and combined contact strings are replaced with opaque crosswalk-backed
pseudonyms where their relationships are useful. Dedicated phone and email
fields, street addresses, and exact premises latitude/longitude are dropped.
County is retained where supplied as a structured field. Source filenames,
relative paths, source checksums, and raw entered values are not carried into
the analysis outputs.

Dates and scientifically necessary numeric values are retained by the analysis
profile as explicit quasi-identifiers. This is intentional and is one reason
the result is not an anonymous public-release dataset. No geocoding, fuzzy
matching, hashing-based pseudonymization, cryptography, OCR, or risk scoring is
performed.

Unconstrained narrative and other/specify fields are classified as
`sensitive_free_text` with `review_remove`. Their values are removed from the
analysis data and recorded only as non-sensitive metadata in
`privacy/deidentification_review.csv`. The package does not claim to find all
identifiers in arbitrary prose or to perform automated natural-language
redaction. Combined name/company/location/phone fields are pseudonymized as a
whole rather than parsed.

## Output and audit

The de-identified directory preserves the relational architecture:

```text
deidentified_dir/
├── collated/
│   ├── epi_forms.csv
│   ├── epi_responses_long.csv
│   ├── epi_multiselect_responses.csv
│   ├── epi_ai_tests.csv ... epi_egg_movements.csv
│   └── validation products when available
└── privacy/
    ├── policy_snapshot.csv
    ├── deidentification_manifest.csv
    ├── deidentification_audit.csv
    ├── deidentification_review.csv
    └── privacy_leak_audit.csv
```

All collated tables use `case_id` in place of `form_id`. Case, premises, and
entity pseudonyms preserve joins and repeated-table row structure. The audit
records counts and actions without source values. A mandatory privacy audit
checks structural policy coverage, known source-value survival, and
conservative email/phone heuristics before finalization. Confirmed leaks always
fail. In strict mode, potential heuristic warnings also prevent finalization.
With `strict = FALSE`, potential warnings may produce a clearly marked
`review` result; confirmed leaks are never accepted.

## Crosswalk storage and reuse

The private destination contains `record_crosswalk.csv`,
`entity_crosswalk.csv`, and `crosswalk_manifest.csv`. It also receives a
non-sensitive `CROSSWALK_README.md`. The crosswalk is rejected when nested with
the source or de-identified output and, when detectable, when inside a Git
working tree. Best-effort restrictive filesystem permissions are attempted, but
secure storage and access control remain the user's responsibility.

Existing valid crosswalk rows are reused without renumbering. Cases use
`source_sha256` when available and fall back to a private `form_id` key when it
is not. Entity matching uses only trim, repeated-whitespace, and case
normalization within the same pseudonym class. Near matches are not merged.
New cases and entities extend the existing numbering. Crosswalk contents are
never returned by `deidentify_epi()` or printed to the console.

Crosswalk files must be stored separately from de-identified outputs and must
not be committed to version control or distributed with analysis datasets.

## Limitations

This layer is a conservative privacy transformation for controlled scientific
work. It is not irreversible anonymization, does not eliminate all
re-identification risk, does not certify public release, and does not replace
project-specific disclosure review or secure storage controls. A future sharing
or public-release profile will require a separate risk review and stronger
quasi-identifier treatment.
