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
Only the first layer is implemented here, preserving source PDF field
semantics for later versioned field dictionaries.

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
```

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
template family. It preserves raw AcroForm values, defaults, choice options,
button states, and multi-select flags; it does not yet recode responses or
collate repeated questionnaire tables. Use `diagnose_epi("epi_output")` for
the Epi schema diagnostics. Other APHIS/HPAI PDF forms are not implied to be
supported.

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

OCR, handwriting recognition, semantic recoding, Epi collation, audit scoring,
summaries, dashboards, and report generation are not implemented yet.

## Development roadmap

Future layers will add `collate_hpai()`, `validate_hpai()`,
`summarize_hpai()`, and `view_hpai()`. The next step is a versioned BCAP field
dictionary that maps raw PDF fields to their semantic meaning.
