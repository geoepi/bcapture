<p align="center">
  <img src="images/bcapture_hex.png" width="350" alt="hex sticker">
</p>
  
    
# bCAPture

`bcapture` provides tools for extracting, organizing, validating, summarizing,
and viewing data from Biosecurity Compliance Audit Program (bCAP) forms.

The current development version focuses on structured extraction from
electronically completed HPAI BCAP PDF forms. See the package documentation
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

Extraction requires Python and the `pypdf` package. `bcapture` requests
`pypdf` lazily through reticulate and does not initialize Python when the
package is loaded. No Python pandas dependency is used.

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

The current extractor reads interactive PDF form fields directly. It does not
use OCR and does not yet extract handwritten values from scanned forms.

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

## Failure handling

Batch extraction performs preflight checks for input files, sanitized audit-ID
collisions, and existing output directories before writing. A failed PDF does
not stop the batch; it receives a `failed` manifest row with a useful failure
type. Flattened or scanned forms are recorded as `no_acroform_fields`.

## Current limitations

OCR, handwriting recognition, audit scoring, dashboards, and inferential
analysis are not implemented. Descriptive summaries are controlled-use
products; the Initial Epi dictionary describes the questionnaire and its
encodings, and no layer infers biological meaning or risk.

## Development roadmap

Future layers will add `collate_hpai()`, `validate_hpai()`,
`summarize_hpai()`, and `view_hpai()`.
