# BCAP Form Extraction Workflow

This tutorial covers the current BCAP workflow:

~~~
Interactive BCAP PDF
        ↓
   extract_hpai()
        ↓
structured AcroForm data
        ↓
   diagnose_hpai()
        ↓
schema diagnostics
~~~

The workflow ends at raw structured extraction and schema review. BCAP
semantic collation and validation are future layers.

> The examples use fictional information. Do not copy controlled or
> operational data into tutorial directories merely to reproduce the example.

## Setup

~~~r
library(bcapture)
library(dplyr)
library(readr)

tutorial_root <- file.path(tempdir(), "bcapture_tutorial")
bcap_input <- file.path(tutorial_root, "input", "bcap")
bcap_single_out <- file.path(tutorial_root, "output", "bcap_single")
bcap_batch_out <- file.path(tutorial_root, "output", "bcap_batch")

dir.create(bcap_input, recursive = TRUE, showWarnings = FALSE)
~~~

Make working copies of a blank interactive BCAP form and enter fictional
values. Place files such as these in `bcap_input`:

~~~
bcap_demo_01.pdf
bcap_demo_02.pdf
~~~

These PDFs are not included in the package. Enter enough values to exercise a
text field, a Yes/No or other button field, a comment field, and more than one
form section. Values such as `Example Premises`, `Example Auditor`, and
`Synthetic test only` are sufficient.

Save the normal interactive PDF. Do not use Print to PDF, flatten the form, or
scan a completed form; the extractor requires AcroForm fields.

## Extract one form

~~~r
bcap_one <- extract_hpai_file(
  pdf_file = file.path(bcap_input, "bcap_demo_01.pdf"),
  out_dir = bcap_single_out
)

bcap_one$status
names(bcap_one)
nrow(bcap_one$fields)
nrow(bcap_one$populated_fields)

bcap_one$populated_fields |>
  select(any_of(c("page", "field", "alternative_name", "field_type",
                  "value", "states"))) |>
  print(n = 20)
~~~

Compare several values with the fictional entries in the PDF. Confirm that
button states and comments are represented as expected.

## Extract a batch

~~~r
bcap_manifest <- extract_hpai(
  in_dir = bcap_input,
  out_dir = bcap_batch_out,
  diagnostics = TRUE
)

bcap_manifest |>
  select(any_of(c("audit_id", "status", "failure_type", "number_of_fields",
                  "number_of_widgets", "number_of_populated_fields",
                  "schema_group")))
~~~

Review `status`, `failure_type`, `schema_group`, `number_of_fields`, and
`number_of_populated_fields`. A failed PDF receives a failed manifest row with
a failure type rather than becoming an empty successful record.

## Diagnose schemas

Batch extraction writes diagnostics by default. Run them explicitly:

~~~r
bcap_diag <- diagnose_hpai(bcap_batch_out)
names(bcap_diag)
bcap_diag$summary
bcap_diag$schema_groups
~~~

Normalized diagnostics compare logical field presence, field types, button
states, field order, and widget encodings. PDFs that look identical can still
have different internal AcroForm encodings. `bcapture` reports those
differences; it does not infer that structurally different forms are
semantically interchangeable.

## Inspect the output tree

The conceptual batch layout is:

~~~
bcap_batch_out/
├── audits/
├── combined/
├── diagnostics/
└── extraction_manifest.csv
~~~

See the [output contract](../output-structure.md) for detailed file and column
definitions.

## Validate and de-identify supported records

After extraction and structural diagnostics, the supported BCAP workflow is:

~~~
extract_hpai()
diagnose_hpai()
validate_hpai()
deidentify_hpai()
validate_hpai_privacy()
~~~

validate_hpai() applies only checks supported by the extracted schema:
controlled extraction failures, categorical domains, date parsing, and field
type coverage. It does not infer undocumented business rules.

deidentify_hpai() accepts an extracted output directory, not raw PDFs. Use
separate operational paths for confidential extraction, safe output, and the
private crosswalk. A short-path layout is recommended on Windows:

~~~r
validation <- validate_hpai("D:/secure/bcap/a_conf")

safe <- deidentify_hpai(
  out_dir = "D:/secure/bcap/a_conf",
  deidentified_dir = "D:/secure/bcap/a_safe",
  crosswalk_dir = "D:/secure/bcap/a_xwalk"
)

validate_hpai_privacy(
  deidentified_dir = "D:/secure/bcap/a_safe",
  crosswalk_dir = "D:/secure/bcap/a_xwalk"
)
~~~

The policy classifies canonical BCAP fields as SAFE_RETAIN, SAFE_NORMALIZE,
PSEUDONYMIZE, or WITHHOLD. The current canonical template has 160 retained, 3
normalized, 15 pseudonymized, and 47 withheld fields; no canonical field is
silently assigned REVIEW. Arbitrary text is withheld rather than redacted
speculatively. `/Sig` fields remain the structural signature_present marker
only.

Safe output is fail-closed. Any blocking semantic issue, privacy error,
privacy warning, unresolved policy field, crosswalk conflict, or unsafe path
prevents finalization. The four 17-page unrecognized_flattened_form records
remain controlled extraction failures and are not passed to de-identification.
For the complete Initial Epi workflow, continue to the [Initial Epi workflow
tutorial](initial-epi-workflow.md).
