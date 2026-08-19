# Initial Epi descriptive summaries

summarize_epi() is the first analytical layer after Initial Epi
de-identification. It creates deterministic, reusable descriptive products
from the versioned semantic dictionary, codebooks, the privacy policy snapshot,
and the de-identified relational tables.

> summarize_epi() consumes the de-identified analytical dataset. It does not
> read the private re-identification crosswalk.

The summary products are controlled-use analytical products and are not
automatically suitable for unrestricted public release. This layer does not
apply small-cell suppression, coarsening, weighting, inferential statistics,
risk scoring, or biological interpretation.

## Input and privacy boundary

The input must be a successful deidentify_epi() output directory containing
the collated/ and privacy/ products. The source de-identification manifest
must match the requested form version and analysis profile. Strict mode
requires source status passed, zero privacy errors, and validation products.
It never attempts to repair an identifiable directory or locate a crosswalk.

The policy snapshot written with the de-identified data is the privacy source
of truth. Retained fields are eligible for substantive summaries. Fields with
drop, review_remove, or pseudonymize actions are explicitly recorded as
excluded_privacy in the field inventory and are not summarized as content.

With strict = FALSE, a source status of review or absent validation may
produce a summary whose manifest status is review. A source with any privacy
errors is always rejected.

## Running the summary layer

Example:

    summary <- summarize_epi(
      deidentified_dir = "epi_analysis",
      version = "2024-05-28"
    )

    summary$overview
    summary$scalar_frequencies
    summary$numeric_summaries

The default writes products below epi_analysis/summary/. An existing
non-empty summary directory is protected unless overwrite = TRUE; only the
summary/ directory is replaced.

## Summary products

The output contains:

* dataset_overview.csv: form version, privacy/validation status, case and
  county counts, interview-date range, and repeated-record totals.
* summary_field_inventory.csv: one disposition for every logical semantic
  variable, including metadata, privacy action, summary type, and notes.
* scalar_frequencies.csv: coded and uncoded scalar categorical frequencies.
* multiselect_frequencies.csv: selected-item counts for multiselect questions.
* repeated_categorical_frequencies.csv: repeated-record categorical
  frequencies.
* numeric_summaries.csv and date_summaries.csv: typed parsing counts and
  descriptive statistics.
* case_table_counts.csv and repeated_table_summaries.csv: case-by-table
  record counts, including zero-record case/table combinations.
* validation_rule_summary.csv and validation_status_summary.csv: quality
  metadata, not biological variables.
* summary_manifest.csv: schema version and safe provenance.

Questionnaire labels, section names, question text, units, and dictionary
metadata are carried into the products so that they can be used by later
reporting layers without reopening the source questionnaire.

## Denominators and missingness

Scalar frequencies use all cases in epi_forms.csv as n_cases_total.
n_answered counts populated, usable retained responses, and
n_missing = n_cases_total - n_answered. Codebook levels are retained even
when their count is zero. Explicit questionnaire categories such as Not
applicable and Don't know are answered categories, not missing values.
Percentages name their denominators explicitly:
percent_of_answered and percent_of_all_cases.

Uncoded scalar values are not truncated or collapsed into Other; ties are
ordered alphabetically after descending frequency. County is retained only
where the analysis privacy policy explicitly allows it.

Multiselect denominators are distinct cases with at least one selected item.
percent_of_selecting_cases and percent_of_all_cases are reported separately.
Item percentages are not expected to sum to 100%, and an absent selection is
not interpreted as an explicit No.

Repeated categorical summaries use emitted records as the denominator:
n_records_total, n_nonmissing, and percent_of_nonmissing. They also report
distinct cases contributing each response. Numeric and date summaries
distinguish populated, successfully parsed, populated-but-unparsed, and
ordinary missing values. Statistics use successfully parsed values only;
quantiles use standard R type-7 definitions and dates retain Date class in the
returned object.

Typed values already produced by collation are consumed directly. The summary
layer does not introduce a new date or numeric parser and does not reinterpret
date expressions such as Daily or End on 1/15.

## Validation handling

Validation findings remain quality metadata. Cases with valid, review, or
error statuses are all retained in descriptive summaries; no automatic
clean-case subset is created. When validation is unavailable in non-strict
mode, validation products are typed empty tables and the summary manifest is
review.

The summary layer only describes data. It does not turn validation rules into
exposure variables, recode questionnaire responses into risk categories, or
draw epidemiological conclusions.
