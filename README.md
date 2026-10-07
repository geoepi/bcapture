<p align="center">
  <img src="images/bcapture_hex.png" width="350" alt="hex sticker">
</p>
  
    
# bCAPture

`bcapture` provides tools for extracting, organizing, validating, summarizing,
and viewing data from Biosecurity Compliance Audit Program (bCAP) forms.

The current development version focuses on structured extraction from HPAI
BCAP and Initial Epi PDF forms. See the package documentation
and [the output contract](docs/output-structure.md) for details.

## Purpose

The package establishes a stable extraction layer for the broader BCAP audit
workflow: extraction, collation, validation, summarization, and reporting.
Initial Epi extraction, semantic collation, validation, de-identification,
metadata-driven descriptive summaries, Quarto HTML reporting, and auditable
case-level analytical feature derivation are implemented. Reporting consumes
summary products and does not recalculate their descriptive statistics.

## Installation

Install the development version from the repository with your preferred R
development workflow. The package imports `reticulate`; Python is initialized
only when an extraction function is called.

## Python dependency

Extraction requires Python and the `pypdf` package. Recognized selectable-text
flattened PDFs additionally use `pdfplumber` and `pypdfium2` for spatial text
and rendered-control evidence. `bcapture` requests these packages lazily
through reticulate and does not initialize Python when the package is loaded.
No Python pandas dependency is used.

## Quick start

```r
library(bcapture)

result <- extract_hpai(
  in_dir = "completed_audits",
  out_dir = "bcapture_output"
)

epi_result <- extract_epi(
  in_dir = "completed_epi_interviews",
  out_dir = "epi_output"
)

epi <- collate_epi(
  out_dir = "epi_output"
)

validation <- validate_epi(
  out_dir = "epi_output"
)

analysis <- deidentify_epi(
  out_dir = "epi_output",
  deidentified_dir = "epi_analysis",
  crosswalk_dir = "D:/secure/bcapture_crosswalks/example_epi_project"
)

summary <- summarize_epi(
  deidentified_dir = "epi_analysis"
)

features <- derive_epi_features(
  deidentified_dir = "epi_analysis"
)

feature_summary <- summarize_epi_features(features)

plot_epi_features(
  feature_summary,
  domain = "environment_wildlife",
  type = "prevalence"
)

feature_report <- render_epi_report(
  deidentified_dir = "epi_analysis",
  report = "features"
)

plot_epi_validation(
  summary,
  type = "status"
)

summary_report <- render_epi_report(
  deidentified_dir = "epi_analysis",
  report = "summary"
)

quality_report <- render_epi_report(
  deidentified_dir = "epi_analysis",
  report = "quality"
)
```

The Initial Epi workflow branches after de-identification:

```text
PDF -> extract -> collate -> validate -> de-identify
                                      |-> summarize -> visualize -> report
                                      `-> derive features -> summarize -> visualize -> report
```

PDF extraction produces raw AcroForm data, collation produces semantic
relational data, validation produces reviewable data-quality findings, and
`deidentify_epi()` creates a separately stored pseudonymized/de-identified
analysis dataset. The crosswalk path must be outside the project repository
and outside the de-identified output. The analysis profile is for controlled
use; it is not an unrestricted public-release or irreversible anonymization
workflow. `summarize_epi()` describes a collection of cases;
`derive_epi_features()` creates transparent analysis-ready attributes for each
case, while `summarize_epi_features()` describes those attributes without
recomputing them. Neither downstream branch is required by the other.

## Tutorials

For complete synthetic user workflows, see the
[bCAPture tutorials](docs/tutorials/README.md):

- [BCAP workflow](docs/tutorials/bcap-workflow.md)
- [Initial Epi workflow](docs/tutorials/initial-epi-workflow.md)
- [Initial Epi de-identification](docs/tutorials/initial-epi-deidentification.md)
- [Initial Epi descriptive summaries](docs/initial-epi-summaries.md)
- [Initial Epi analytical features](docs/initial-epi-analytic-features.md)
- [Initial Epi feature analysis](docs/initial-epi-feature-analysis.md)
- [Initial Epi feature analysis tutorial](docs/tutorials/initial-epi-feature-analysis.md)
- [Initial Epi visualization](docs/tutorials/initial-epi-visualization.md)
- [Initial Epi HTML reporting](docs/tutorials/initial-epi-reporting.md)

The extractor reads interactive PDF form fields directly when available. The
supported input classes are:

- canonical interactive Initial Epi and BCAP AcroForm PDFs;
- selectable-text flattened Initial Epi and BCAP PDFs that register against a
  packaged, versioned template; and
- a supported PDF container or Portfolio containing exactly one recognized
  canonical Initial Epi AcroForm, recovered through guarded in-memory
  extraction.

The container route is not generic PDF Portfolio support: ambiguous containers,
multiple candidate forms, nested containers, XFA, unknown attachments, and
incompatible member signatures fail explicitly. Image-only scans, OCR-only
inputs, handwritten forms, and arbitrary unrecognized flattened layouts are
recognized as unsupported; OCR is outside the current extraction scope.

AcroForm `/Sig` fields are normalized deterministically: a populated signature
field is represented by the structural marker `signature_present`, while an
unsigned field retains the existing empty/`NA` representation. No signer,
certificate, byte-range, contents, or signature-verification metadata is
exposed by extraction.

## Output structure

```text
bcapture_output/
├── audits/<audit_id>/
│   ├── <audit_id>_fields_long.csv
│   ├── <audit_id>_populated_fields_long.csv
│   ├── <audit_id>_fields_wide.csv
│   └── <audit_id>_metadata.csv
├── combined/
│   ├── hpai_fields_long.csv
│   ├── hpai_populated_fields_long.csv
│   ├── hpai_audits_wide.csv
│   └── hpai_metadata.csv
└── extraction_manifest.csv
```

`fields_long` is the canonical raw structured output. It retains every PDF
field, raw and normalized values, provenance, page and field order, available
button states, and `is_populated`. `/Off` remains `Off` in the master table but
is not populated. See `docs/output-structure.md` for the complete contract.

## Initial Epi Interview extraction

Initial Epi extraction is separate from BCAP audit extraction and targets the
`HPAI Response / Initial Epidemiological (Epi) Interview` May 28, 2024
template. It preserves raw AcroForm values, defaults, choice options, button
states, and multi-select flags. `collate_epi("epi_output")` applies the
versioned 2024-05-28 semantic dictionary and writes analysis-ready scalar and
repeated relational tables. Use `diagnose_epi("epi_output")` for Epi schema
diagnostics. Other APHIS/HPAI PDF forms are not implied to be supported.
Image-only scans and incompatible or unknown signatures are controlled
unsupported outcomes that require OCR, template, or version review outside
this package.

## Schema diagnostics

Batch extraction runs normalized schema diagnostics by default. To inspect an
existing extraction directory directly:

```r
diagnostics <- diagnose_hpai("bcapture_output")
```

Diagnostic products appear under `bcapture_output/diagnostics/`. They separate
logical-field presence, field-type, response-state, field-order, and widget
encoding differences. PDF widget serialization can vary even when forms look
identical, so no automatic semantic reconciliation is performed.

For Epi batches, `extraction_manifest.csv` is also the route and failure audit:
it records success or controlled failure, failure class, extraction method,
template/schema identity, registration evidence where applicable, and PDF
container/member provenance. A selected embedded member is processed in
memory; the submitted container remains the source whose checksum is recorded.

## Flattened selectable-text PDFs

The spatial fallback is deliberately template-guided rather than a general
PDF or OCR parser. Each supported family has an immutable canonical
interactive/printed pair and derived field, widget, page-geometry, and
registration metadata under `inst/extdata/templates/`. A successful spatial
record has `extraction_method = "spatial_template"` in metadata and
`spatial_text`/`spatial_mark` at field level. Its observed `form_schema_hash`
is `NA`; `canonical_template_schema_hash` identifies the versioned logical
schema, and `number_of_source_widgets` is zero because flattened PDFs no
longer contain source widgets. Registration quality, anchor agreement, field
regions, and evidence classes are retained for auditability.

The fallback rejects page-count or layout mismatches, image-only inputs, and
ambiguous control states. Mixed batches can contain AcroForm and spatial
records; diagnostics use the canonical template hash as the schema identity
for spatial records so they remain comparable without inventing an observed
AcroForm hash.

## Failure handling

Batch extraction performs preflight checks for input files, sanitized audit-ID
collisions, and existing output directories before writing. A failed PDF does
not stop the batch; it receives a `failed` manifest row with a useful failure
type. Scanned, OCR-only, and unrecognized flattened forms are recorded with a
classified failure such as `no_usable_digital_content`,
`unrecognized_flattened_form`, or `registration_failed`.

Extraction failures are distinct from semantic review findings. For BCAP,
`validate_hpai()` evaluates supported extracted records without inventing
undocumented business rules. `deidentify_hpai()` and
`validate_hpai_privacy()` form a conservative, strict privacy boundary:
arbitrary text and facility locations are withheld, direct identifiers are
crosswalk-backed pseudonyms, and safe output is finalized only with zero
semantic and privacy errors or warnings. The four unsupported flattened forms
remain outside the BCAP de-identification population.

## Current limitations

OCR, handwriting recognition, audit scoring, dashboards, inferential analysis,
BCAP semantic collation, and BCAP summaries are not implemented. BCAP
validation is limited to documented extraction-schema checks; it does not
infer biological meaning or undocumented audit rules.

## Development roadmap

Future layers may add BCAP semantic collation, summaries, and viewing.
