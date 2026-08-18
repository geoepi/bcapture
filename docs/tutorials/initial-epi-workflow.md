# Initial Epi Extraction, Collation, and Validation Workflow

This tutorial covers the current Initial Epi workflow:

~~~
Interactive Initial Epi PDF
        ↓
    extract_epi()
        ↓
    diagnose_epi()
        ↓
    collate_epi()
        ↓
   validate_epi()
~~~

De-identification is a separate privacy boundary covered in the
[Initial Epi de-identification tutorial](initial-epi-deidentification.md).
> The examples use fictional information. Do not copy controlled or
> operational data into tutorial directories merely to reproduce the example.

## Setup

~~~r
library(bcapture)
library(dplyr)
library(readr)

tutorial_root <- file.path(tempdir(), "bcapture_tutorial")
epi_input <- file.path(tutorial_root, "input", "epi")
epi_single_out <- file.path(tutorial_root, "output", "epi_single")
epi_batch_out <- file.path(tutorial_root, "output", "epi_batch")

dir.create(epi_input, recursive = TRUE, showWarnings = FALSE)
~~~

Prepare two fictional interactive Initial Epi PDFs:

~~~
epi_demo_clean.pdf
epi_demo_review.pdf
~~~

These PDFs are not included in the package. Use a working copy of the
supported May 28, 2024 template, enter fictional values, and preserve its
AcroForm fields. Do not use Print to PDF, flatten the form, or scan it.

Suggested fictional entries include:

~~~
TESTPREM001
Example Poultry Farm
123 Test Road
Example County
38.123456
-97.654321
Alice Example
555-010-1234
alice@example.invalid
~~~

For the second form, use `TESTPREM002`, `Demonstration Duck Farm`,
`456 Sample Avenue`, `Example County`, and `Carol Example`. These are
fabricated tutorial values only.

### Optional deliberate review entry

Enter `Daily` in a field that expects a scalar calendar date in
`epi_demo_review.pdf`. This creates a deliberate synthetic data-quality
finding. It is optional for users who only want a normal successful workflow.

## Extract one form

~~~r
epi_one <- extract_epi_file(
  pdf_file = file.path(epi_input, "epi_demo_clean.pdf"),
  out_dir = epi_single_out
)

epi_one$status
names(epi_one)
nrow(epi_one$fields)
nrow(epi_one$populated_fields)

stopifnot(nrow(epi_one$fields) == 497L)
~~~

Inspect a small set of populated fields:

~~~r
epi_one$populated_fields |>
  select(any_of(c("page", "field", "alternative_name", "field_type",
                  "value", "default_value", "states", "options",
                  "is_multiselect"))) |>
  print(n = 25)
~~~

## Extract and diagnose a batch

~~~r
epi_manifest <- extract_epi(
  in_dir = epi_input,
  out_dir = epi_batch_out,
  diagnostics = TRUE
)

epi_manifest |>
  select(any_of(c("form_id", "status", "failure_type", "number_of_fields",
                  "number_of_widgets", "number_of_populated_fields",
                  "schema_group")))

epi_diag <- diagnose_epi(epi_batch_out)
epi_diag$summary
epi_diag$schema_groups
~~~

Both supported synthetic forms should succeed and report 497 logical fields.
Forms made from the same template should generally normalize to one schema.

## Collate semantic data

Raw extraction preserves what the PDF contains. Collation applies the
versioned dictionary and creates semantic scalar and repeated relational
tables:

~~~r
epi <- collate_epi(
  out_dir = epi_batch_out,
  version = "2024-05-28",
  strict = TRUE
)

names(epi)
~~~

The returned components include `forms`, `responses`, `multiselect`,
`ai_tests`, `houses`, `mortality_disposal`, `manure_destinations`,
`imported_materials`, `worker_visits`, `crews`, `visitors`,
`shared_equipment`, `bird_movements`, `egg_movements`, `coverage`, and
manifest/diagnostic objects.

## Check dictionary coverage

~~~r
nrow(epi$coverage)
table(epi$coverage$mapped)

epi$manifest |>
  select(form_id, raw_fields, mapped_fields, unknown_fields, populated_fields,
         populated_mapped_fields, populated_unmapped_fields, status)
~~~

For the supported 2024-05-28 form, expect 497 mapped fields and no populated
unmapped fields:

~~~
497/497 dictionary coverage
populated_unmapped_fields = 0
~~~

## Inspect semantic and relational data

At this point the collated data are still identifiable:

~~~r
epi$forms |>
  select(any_of(c("form_id", "premises_id", "premises_name", "premises_county")))

epi$houses
epi$visitors
epi$bird_movements
epi$egg_movements
~~~

Zero-row tables simply mean that no substantive response was recorded for that
section. Do not use this identifiable representation as a public analytical
dataset.

For a populated repeated table, inspect provenance:

~~~r
epi$visitors |>
  select(any_of(c("form_id", "row_index", "row_label", "raw_fields",
                  "source_pages")))
~~~

`raw_fields` identifies the contributing questionnaire fields and
`source_pages` preserves source-page traceability.

## Check date parsing

If you entered `01/15/25` in a scalar date field or date-bearing repeated
section, inspect the corresponding output:

~~~r
epi$ai_tests |>
  select(any_of(c("form_id", "date_raw", "date")))
~~~

The expected parsed date is `2025-01-15`, not year 25. Do not assume an
AI-test row exists unless you populated that section.

## Validate data quality

~~~r
validation <- validate_epi(
  out_dir = epi_batch_out,
  version = "2024-05-28",
  strict = FALSE
)

validation
attr(validation, "validation_summary")
attr(validation, "validation_form_summary")
~~~

Findings use `INFO`, `WARNING`, and `ERROR` severities. A validation finding
is a review result, not an extraction failure. An optional `Daily` entry may
produce a scalar-date review finding; it does not mean extraction or collation
failed.

Validation products are written under:

~~~
epi_batch_out/validation/
├── validation_results.csv
├── validation_summary.csv
├── validation_form_summary.csv
└── validation_report.md
~~~

The report uses counts and rule metadata rather than repeating respondent or
business values. See the [validation contract](../initial-epi-validation.md).

## Next: create a de-identified analysis dataset

After extraction, collation, and validation, continue to the
[Initial Epi de-identification tutorial](initial-epi-deidentification.md).
