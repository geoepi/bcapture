# Initial Epi data-quality validation

`validate_epi()` is the data-quality layer after extraction and semantic
collation:

```text
PDF
 ↓ extract_epi()
raw AcroForm data
 ↓ collate_epi()
semantic relational data
 ↓ validate_epi()
reviewable data-quality findings
```

It reads existing successful products and does not rerun either upstream
function. It identifies potential data-quality and logical-consistency issues;
it does not repair or reinterpret respondent or interviewer entries, infer
missing information, calculate risk, or provide a completeness score.

The layers have distinct meanings:

* **Extraction failure:** the PDF could not be read structurally.
* **Collation parse warning:** an entered value could not be converted to its
  expected type; the original raw value remains available for review.
* **Validation warning:** a readable record merits review because of a range,
  chronology, conditional, or table-consistency issue.
* **Validation error:** a structural or logical contradiction prevents reliable
  interpretation.

## Interface and products

```r
validation <- validate_epi(
  out_dir = "epi_output",
  version = "2024-05-28",
  write = TRUE,
  strict = FALSE
)
```

Products are written to `epi_output/validation/`:

* `validation_results.csv` contains one finding per row and stable source
  identifiers, not respondent values.
* `validation_summary.csv` aggregates severity, category, and rule ID.
* `validation_form_summary.csv` reports finding counts and `valid`, `review`,
  or `error` status for each form. `review` does not mean the interview is
  unusable.
* `validation_report.md` contains counts and rule IDs only.

The versioned registry in
`inst/extdata/dictionaries/initial_epi/2024-05-28/validation_rules.csv`
drives conditional checks and repeated-table core-field checks. The controlled
validation categories are `parse`, `range`, `chronology`, `conditional`,
`table_structure`, `codebook`, and `cross_field`; severities are `INFO`,
`WARNING`, and `ERROR`.

Conditional evidence from a repeated child table is evaluated independently
for each `form_id`. A child row from one form cannot satisfy or contradict a
parent response on another form. When a registry rule declares `child_filter`
metadata, the engine also applies every declared column/value subset after
form scoping. This distinguishes, for example, movements onto versus off the
premises and eggs versus egg products without hard-coded rule branches.

Parse validation applies only to fields declared as scalar `date` or
`numeric`. Values from `date_text` fields are preserved as temporal-expression
text and are not warned on merely because they contain multiple dates,
recurrence, or descriptive scheduling language.

With `strict = FALSE`, all findings are returned. With `strict = TRUE`, all
findings are still written and returned, but an `ERROR` finding emits a warning
to signal failed strict validation. Warnings do not stop processing.
