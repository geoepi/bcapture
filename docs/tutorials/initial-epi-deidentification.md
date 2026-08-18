# Initial Epi De-identification Workflow

This tutorial covers the privacy boundary:

~~~
validated collated data
        ↓
deidentify_epi()
        ↓
pseudonymized analysis dataset
        +
separate private crosswalk
~~~

> `deidentify_epi()` creates a pseudonymized/de-identified analytical dataset.
> It does not create irreversibly anonymous data because a separately stored
> crosswalk permits authorized re-identification.

The `analysis` profile is for controlled analytical use, not public-release
certification. A de-identified analysis directory and its private crosswalk
must never be distributed together. Store the crosswalk outside the repository
and in an appropriately access-controlled location.

> The examples use fictional information. Do not copy controlled or
> operational data into tutorial directories merely to reproduce the example.

## Setup

This tutorial assumes that you have already completed:

~~~r
extract_epi()
collate_epi()
validate_epi()
~~~

Use the same disposable workspace as the
[Initial Epi workflow tutorial](initial-epi-workflow.md):

~~~r
tutorial_root <- file.path(tempdir(), "bcapture_tutorial")
epi_batch_out <- file.path(tutorial_root, "output", "epi_batch")
epi_deid_out <- file.path(tutorial_root, "output", "epi_deidentified")

crosswalk_root <- file.path(tempdir(), "bcapture_private_crosswalk")
epi_crosswalk <- file.path(crosswalk_root, "epi_demo")
~~~

For controlled work, replace `epi_crosswalk` with an access-controlled
location outside both the repository and the de-identified output directory.
Do not create or inspect a crosswalk inside the repository.

## Run de-identification

~~~r
deid <- deidentify_epi(
  out_dir = epi_batch_out,
  deidentified_dir = epi_deid_out,
  crosswalk_dir = epi_crosswalk,
  version = "2024-05-28",
  profile = "analysis",
  strict = TRUE
)

deid$status
deid$forms_processed
deid$manifest
deid$audit
~~~

The returned object intentionally does not contain crosswalk mappings.

## Output structure

The de-identified output is separate from the private crosswalk:

~~~
epi_deidentified/
├── collated/
├── validation/
└── privacy/

private crosswalk directory/
├── CROSSWALK_README.md
├── record_crosswalk.csv
├── entity_crosswalk.csv
└── crosswalk_manifest.csv
~~~

The crosswalk files should never be copied into the de-identified directory,
the repository, or a shared analysis package.

## Inspect the safe form output

~~~r
safe_forms <- read_csv(
  file.path(epi_deid_out, "collated", "epi_forms.csv"),
  show_col_types = FALSE
)

names(safe_forms)

stopifnot("case_id" %in% names(safe_forms))
stopifnot(!"form_id" %in% names(safe_forms))
~~~

Depending on the policy, pseudonym columns use opaque values such as
`CASE-xxxxxx`, `PREMISES-xxxxxx`, `PERSON-xxxxxx`, `ORG-xxxxxx`,
`ENTITY-xxxxxx`, `CONTACT-xxxxxx`, and `LOCATION-xxxxxx`. They preserve
analytical joins without exposing the original identifying values.

## Geography and direct contact protection

Under the current analysis profile:

~~~
county               retained where approved
street address       removed
exact latitude        removed
exact longitude       removed
~~~

~~~r
safe_forms |>
  select(any_of(c("case_id", "premises_county", "premises_address",
                  "premises_latitude", "premises_longitude")))
~~~

Removed fields may remain as all-missing columns or be absent, depending on the
implemented output representation. Dedicated phone numbers, email addresses,
street addresses, source filenames, source paths, and source checksums are not
retained in ordinary de-identified analytical output.

## Privacy manifest and review products

~~~r
privacy_manifest <- read_csv(
  file.path(epi_deid_out, "privacy", "deidentification_manifest.csv"),
  show_col_types = FALSE
)

privacy_leaks <- read_csv(
  file.path(epi_deid_out, "privacy", "privacy_leak_audit.csv"),
  show_col_types = FALSE
)

privacy_manifest
privacy_leaks
~~~

For a clean strict run, expect `privacy_errors = 0` and `status = "passed"`.

Sensitive free-text fields are withheld:

~~~r
review <- read_csv(
  file.path(epi_deid_out, "privacy", "deidentification_review.csv"),
  show_col_types = FALSE
)

review
~~~

The review table contains field metadata and review status, not the withheld
narrative value. `bcapture` does not claim to perform general natural-language
de-identification.

## Optional independent check

Search only for fictional values deliberately entered in the tutorial:

~~~r
synthetic_identifiers <- c(
  "TESTPREM001", "TESTPREM002", "Example Poultry Farm",
  "Demonstration Duck Farm", "123 Test Road", "456 Sample Avenue",
  "Alice Example", "Bob Example", "Carol Example", "Example Transport LLC",
  "555-010-1234", "alice@example.invalid", "38.123456", "-97.654321"
)

scan_for_terms <- function(root, terms) {
  files <- list.files(root, pattern = "\\.csv$|\\.md$",
                      recursive = TRUE, full.names = TRUE)
  sum(vapply(files, function(file) {
    text <- paste(readLines(file, warn = FALSE), collapse = "\n")
    any(vapply(terms, function(term)
      grepl(tolower(term), tolower(text), fixed = TRUE), logical(1)))
  }, logical(1)))
}

known_identifier_hits <- scan_for_terms(epi_deid_out, synthetic_identifiers)
known_identifier_hits
~~~

The expected result is `0` known synthetic identifier hits. This is a check
against deliberately inserted examples, not proof of anonymity.

## Crosswalk separation and guardrails

List filenames without printing mappings:

~~~r
list.files(epi_crosswalk)

list.files(
  epi_deid_out,
  recursive = TRUE,
  pattern = "crosswalk",
  ignore.case = TRUE
)
~~~

The second result should be `character(0)`.

After a de-identified output exists, this intentionally fails because the
crosswalk is located inside an output tree:

~~~r
bad_crosswalk <- file.path(epi_deid_out, "crosswalk")

tryCatch(
  deidentify_epi(
    out_dir = epi_batch_out,
    deidentified_dir = file.path(tutorial_root, "output", "bad_deid_test"),
    crosswalk_dir = bad_crosswalk
  ),
  error = function(error) message(conditionMessage(error))
)
~~~

A crosswalk inside a Git working tree is also rejected. Do not create
sensitive-looking files in the repository merely to demonstrate that guardrail.

## Stable pseudonym reuse

~~~r
epi_deid_out_2 <- file.path(
  tutorial_root,
  "output",
  "epi_deidentified_repeat"
)

deid_2 <- deidentify_epi(
  out_dir = epi_batch_out,
  deidentified_dir = epi_deid_out_2,
  crosswalk_dir = epi_crosswalk,
  version = "2024-05-28",
  profile = "analysis",
  strict = TRUE
)

safe_forms_2 <- read_csv(
  file.path(epi_deid_out_2, "collated", "epi_forms.csv"),
  show_col_types = FALSE
)

identical(safe_forms$case_id, safe_forms_2$case_id)
~~~

The expected result is `TRUE`. Reusing the crosswalk preserves stable joins
across repeated runs and additional batches. Existing valid mappings are reused
without renumbering.

## Overwrite behavior

`overwrite = FALSE` is the safe default. An existing de-identified destination
is protected. To intentionally replace the analytical output:

~~~r
deid_overwrite <- deidentify_epi(
  out_dir = epi_batch_out,
  deidentified_dir = epi_deid_out,
  crosswalk_dir = epi_crosswalk,
  overwrite = TRUE,
  strict = TRUE
)
~~~

Replacing the analytical output reuses the existing valid crosswalk; it does
not recreate or renumber that crosswalk.

## The identifiable source is not modified

`deidentify_epi()` creates a second dataset. It does not sanitize or overwrite
the identifiable products under:

~~~
epi_batch_out/collated/
~~~

Keep the source workflow and de-identified analytical workflow as separate
artifacts. For technical policy and audit details, see the
[de-identification contract](../initial-epi-deidentification.md).

## Next: summarize the de-identified dataset

Once the de-identification output has passed its privacy review and validation
is available, create controlled-use descriptive products:

    summary <- summarize_epi(
      deidentified_dir = epi_deid_out
    )

    summary$overview
    summary$scalar_frequencies
    summary$numeric_summaries

summarize_epi() reads only epi_deid_out. It does not read or require the
private crosswalk. See the [Initial Epi summary documentation](../initial-epi-summaries.md)
for denominator definitions, validation handling, and output products.
