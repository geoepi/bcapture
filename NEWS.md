# bcapture 0.0.0.9000

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
