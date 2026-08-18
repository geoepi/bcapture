# bcapture 0.0.0.9000

* Added user-facing synthetic workflow tutorials for BCAP extraction and the
  complete Initial Epi extraction, collation, validation, and de-identification
  workflow.

* Added `deidentify_epi()` with a versioned Initial Epi privacy policy,
  crosswalk-backed pseudonyms, coarse geography handling, conservative
  free-text withholding, and an automatic privacy leak audit for controlled-use
  pseudonymized analysis outputs.

* Hardened Initial Epi batch collation and validation: fixed explicit
  two-digit-year parsing, refined scalar-date versus date-expression semantics,
  scoped conditional child-table evidence by form and declared filters, and
  restored dictionary-defined repeated-row field/page provenance.

* Added `validate_epi()` for version-aware Initial Epi data-quality and
  logical-consistency checks, including parse diagnostics, chronology,
  conditional responses, codebooks, and repeated-table validation.

* Added the versioned 2024-05-28 Initial Epi semantic dictionary and
  `collate_epi()`, including explicit codebooks, multiselect normalization,
  repeated relational tables, provenance, coverage artifacts, and strict
  unknown-field diagnostics.
* Added `extract_epi()`, `extract_epi_file()`, and `diagnose_epi()` for raw
  Initial Epidemiological Interview AcroForm extraction.
* Added PDF default-value (`/DV`) capture, placeholder exclusion,
  choice-option extraction, multi-select detection, and distributed Epi
  form-family signature validation.

* Initial package architecture and structured HPAI BCAP AcroForm extraction.

* Added normalized, order-independent form schema hashes, deterministic schema
  groups, logical-field/widget distinction, widget-level diagnostics,
  `diagnose_hpai()`, detailed schema reports, and actionable multi-schema
  warnings.
