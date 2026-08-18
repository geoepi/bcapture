# Initial Epi semantic collation

`extract_epi()` is the loss-minimizing raw extraction layer. It writes every
AcroForm field to `combined/epi_fields_long.csv`, including blank fields,
raw PDF values, normalized display values, form IDs, source files, and schema
metadata. `collate_epi()` reads that canonical long product plus the extraction
manifest and metadata. It does not re-run extraction and does not use the
convenience wide or populated-only products as its source.

## Dictionary versioning

The supported source form is the USDA/APHIS **HPAI Response / Initial
Epidemiological (Epi) Interview**, May 28, 2024. Its dictionary is stored under
`inst/extdata/dictionaries/initial_epi/2024-05-28/` as a manifest, 497-row
field mapping, explicit codebooks, and a repeated-table registry. A later
form revision gets a new directory; historical mappings are not modified.
`load_epi_dictionary()` computes a stable SHA-256 hash from the complete bundle
and carries it into every collated product.

## Outputs

`collate_epi()` writes scalar `epi_forms.csv` and semantic
`epi_responses_long.csv`, preserving `raw_field` and `raw_value`. Multi-select
controls are additionally split into one selected item per row in
`epi_multiselect_responses.csv`; the original delimited value remains in the
long response table. Repeated structures are normalized into relational files
for AI tests, houses, mortality disposal, manure destinations, imported
materials, worker visits, crews, visitors, shared equipment, bird movements,
and egg movements. Blank repeated rows are omitted, while partial rows are
retained with `form_id` and `row_index`.

`epi_forms.csv` is intentionally a character-valued convenience table so its
existing wide-column contract remains stable. The canonical semantic-long
table adds type-stable `date_value` and `numeric_value` companions. These are
populated only for scalar fields whose dictionary `data_type` is `date` or
`numeric`; `raw_value` and the existing character `value` are unchanged.

Each relational output includes dictionary provenance. `raw_fields` lists the
dictionary-defined raw PDF fields that contribute to the logical row in
deterministic order, and `source_pages` contains sorted, deduplicated source
pages. Date and numeric relational fields retain a `*_raw` column and are
parsed only when the dictionary declares the type. Supported scalar date
formats are explicitly `mm/dd/yyyy` and `mm/dd/yy`, including one-digit month
or day values. A two-digit year is parsed with the two-digit-year format and is
never first accepted as a literal year such as 0025. Failed scalar parsing
yields a typed `NA` and a diagnostic in
`collation_parse_diagnostics.csv`; the original text is retained.

The dictionary distinguishes scalar `date` fields from `date_text`. The latter
is used where the printed form permits date(s), multiple dates, a range,
recurrence, or descriptive scheduling text. `date_text` values remain
character data and are not treated as scalar-date parse failures. Collation
does not guess dates from arbitrary prose.

Relational type contracts are checked before outputs are written: raw values,
codes, labels, and identifiers are character; dates are `Date`; numeric
measurements are double; and row indices are integer.

## Strictness and provenance

With `strict = TRUE` (the default), populated raw fields that are not in the
dictionary stop collation with the raw field name. Missing, unpopulated raw
fields are reported but do not stop the run. `strict = FALSE` continues and
records unknown fields in `collation_manifest.csv` and
`dictionary_coverage.csv`. Schema hashes are retained as provenance and
diagnostic information but are not required to match; semantic matching is by
the supported raw field name.

The source form identifies study data as Confidential Business Information.
Never commit populated PDFs, extracted respondent data, real premises IDs,
contact information, or CBI. The dictionary and tests contain metadata and
synthetic values only.

```r
extract_epi(
  in_dir = "completed_epi",
  out_dir = "epi_output"
)

epi <- collate_epi(
  out_dir = "epi_output"
)
```
