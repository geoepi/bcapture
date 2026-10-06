# bcapture 0.0.0.9000

* Added conservative in-memory Initial Epi Portfolio/embedded-PDF extraction
  for a unique canonical interactive form, with submitted-container and selected
  member provenance. Ambiguous aggregates, duplicate logical fields, XFA,
  encrypted/unreadable attachments, and unsupported member schemas fail explicitly.
  BCAP containers remain unsupported. Recognized C2PA provenance manifests
  preserve standalone AcroForm and flattened extraction.

* Classified scans before spatial page-count checks, retained typed Python
  failures and attempted extraction routes, and omitted unrepresentable NUL
  metadata with an omission indicator. Logical field strings are never repaired.

* Reduced repeated button-diagnostic comparisons while preserving ordered
  candidate rows, states, and geometric evidence.

* Added versioned template-guided extraction for selectable-text flattened
  Initial Epi and BCAP PDFs, with page-registration provenance, rendered
  control-mark evidence, explicit failure classes, and mixed-batch route
  diagnostics. OCR and handwriting remain unsupported.

* Added descriptive summaries, visualization, and self-contained Quarto HTML reporting for Initial Epi analytical features.

* Added a versioned, privacy-gated, auditable case-level Initial Epi feature
  registry and `derive_epi_features()` for de-identified analytical data.

* Added reusable Initial Epi visualization functions for categorical,
  multiselect, numeric, repeated-table, and validation summaries.

* Added Quarto-based self-contained HTML reporting for de-identified Initial
  Epi analytical summaries, including descriptive and data-quality reports.

* Added summarize_epi() for metadata-driven descriptive summaries of
  de-identified Initial Epi analytical data, including categorical,
  multiselect, numeric, date, repeated-table, and validation summaries.

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
