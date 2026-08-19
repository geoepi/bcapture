# Initial Epi HTML reporting tutorial

This tutorial renders human-readable reports from the synthetic or controlled
use de-identified Initial Epi output created by the earlier workflow. Use an
existing `epi_deid_out` containing a completed `summary/` directory.

## Requirements

Install Quarto separately and ensure `quarto --version` works in the R
session's `PATH`. The reports are rendered only when requested and do not
install Quarto automatically.

## Render the reports

```r
summary_report <- render_epi_report(
  deidentified_dir = epi_deid_out,
  report = "summary"
)

quality_report <- render_epi_report(
  deidentified_dir = epi_deid_out,
  report = "quality"
)

summary_report$output_file
quality_report$output_file
```

The default output directory is `epi_deid_out/reports/`. The two HTML files
are `initial_epi_summary.html` and `initial_epi_quality.html`, alongside the
safe `report_manifest.csv`. The summary CSV products remain in `summary/` and
are not modified by rendering.

The Summary Report is a factual descriptive view of questionnaire responses.
The Data Quality Report is a review view of validation, completeness, parsing,
privacy metadata, and analytical readiness. Both are controlled-use products;
they do not contain the private crosswalk and are not automatically suitable
for unrestricted public release.
