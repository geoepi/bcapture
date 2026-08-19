# Initial Epi Quarto HTML reporting

`render_epi_report()` is the first human-readable reporting layer after
`summarize_epi()`. It creates two separate controlled-use HTML reports:

* an Initial Epi descriptive summary report; and
* an Initial Epi data-quality review report.

The reporting layer does not recalculate the descriptive statistics produced by
`summarize_epi()`. It reads the existing `summary/*.csv` products and performs
only presentation transformations such as formatting, ordering, label
wrapping, and display-only ratios from supplied counts.

## Requirements and privacy boundary

Quarto must be installed and available as `quarto` on `PATH`. The renderer
does not install Quarto or fall back to R Markdown. `ggplot2` is used only by
the report templates and is listed in `Suggests`.

The input must be a successful `deidentify_epi()` output containing a complete
`summary/` directory created by `summarize_epi()`. The renderer never invokes
`summarize_epi()` silently. It does not accept or search for a crosswalk and
does not read identifiable `out_dir` products. The quality report may read
aggregate metadata from the de-identification privacy manifest, review, and
leak-audit products; it never stages review rows or tested strings.

Reports remain controlled-use analytical products and are not automatically
suitable for unrestricted public release.

## Rendering reports

```r
summary_report <- render_epi_report(
  deidentified_dir = "epi_analysis",
  report = "summary"
)

quality_report <- render_epi_report(
  deidentified_dir = "epi_analysis",
  report = "quality"
)
```

By default, reports are written below `epi_analysis/reports/` as
`initial_epi_summary.html` and `initial_epi_quality.html`. A custom HTML
basename can be supplied with `output_file`; it is always written below
`output_dir`. Existing reports are protected unless `overwrite = TRUE`.
Only the requested report is replaced, and analytical products under
`collated/`, `validation/`, `privacy/`, and `summary/` are protected.

Each render also maintains `reports/report_manifest.csv` with safe provenance:
report type and filename, form and summary versions, profile, status, case
count, package version, Quarto version, and generation time. Absolute paths,
source filenames, case IDs, and crosswalk information are not written there.

## Report contents

The Summary Report contains the dataset overview, validation status,
section-driven categorical and multiselect summaries, repeated-table overview,
numeric summaries, date summaries, denominator and missingness notes, and safe
provenance. Zero-count codebook categories and explicit `Not applicable`
responses remain visible. Numeric output uses supplied aggregate statistics and
does not fabricate histograms or other distributions.

The Data Quality Report contains analytical readiness, validation statuses and
rules, numeric and date parsing quality, a descriptive missingness overview,
aggregate privacy review and leak-audit metadata when available,
repeated-table completeness, and safe provenance. It does not create a single
quality score or infer biological meaning.

Both templates use a restrained accessible style, a table of contents, and
self-contained HTML resources. The final HTML can be copied without a sibling
`*_files/` or `site_libs/` directory.

## Output structure and limitations

```text
epi_analysis/
├── collated/
├── validation/
├── privacy/
├── summary/
└── reports/
    ├── initial_epi_summary.html
    ├── initial_epi_quality.html
    └── report_manifest.csv
```

The reports are descriptive and review-oriented. This phase does not add
inferential statistics, risk scores, modeling, dashboards, Shiny apps, raw
case-level interactive tables, or new biological derived variables.
