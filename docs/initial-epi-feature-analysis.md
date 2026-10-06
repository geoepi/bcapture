# Initial Epi feature analysis

`summarize_epi_features()` makes the case-level analytical feature layer
inspectable before modeling. It consumes only the products already written by
`derive_epi_features()` under `features/`, or that function's in-memory return
value. It does not rerun feature derivations and does not read extraction data,
identifiable collated data, or either private crosswalk.

This layer is descriptive. It reports recorded or derived questionnaire states,
missingness, domain coverage, descriptive numeric/count distributions, and
consistency findings. It does not estimate associations, assign risk direction,
create a score, or imply causation.

## Summary products

With `write = TRUE`, the function atomically writes only
`features/summary/`:

- `feature_overview.csv`
- `binary_feature_summary.csv`
- `categorical_feature_summary.csv`
- `numeric_feature_summary.csv`
- `count_feature_summary.csv`
- `feature_missingness_summary.csv`
- `domain_coverage_summary.csv`
- `consistency_summary.csv`
- `feature_summary_manifest.csv`

Binary prevalence uses explicit TRUE and FALSE values as its known denominator.
A missing binary value is never treated as FALSE. Categorical percentages use
all recorded questionnaire states as the known denominator, so `Don't know`
and `Not applicable` remain categories rather than missing values. Categories
defined by the versioned codebook are retained even when their observed count
is zero.

Numeric products report unrounded mean, standard deviation, median, quartiles,
minimum, and maximum. Count products additionally report zero and positive
counts. A zero count means zero qualifying retained-content records; it does
not confirm that the underlying event did not occur.

Missingness is defined using each feature's type-specific recorded-value
semantics. Domain coverage counts known and missing case-feature cells and the
cases with any or all domain features known. Coverage describes information
completeness, not importance, quality, exposure, or risk. Consistency summaries
aggregate findings by severity, finding type, feature, and domain and provide
severity totals without source values.

The summary manifest has `feature_summary_schema_version = 1`. Status is
`passed` only when the source feature manifest passed, every registered feature
was summarized, and there are no ERROR or WARNING consistency findings.
Otherwise a safe, nonfatal result is marked `review`.

## Visualization

`plot_epi_features()` reads the feature summary products and returns an ordinary
ggplot object without saving files. Its plot types are:

- `prevalence`: TRUE among known binary states, or category percentages for one
  categorical feature;
- `missingness`: percent missing, optionally filtered by domain or feature;
- `distribution`: actual de-identified numeric/count values for exactly one
  feature, shown as jittered points with a median/interquartile box;
- `coverage`: percent of case-feature cells known by domain; and
- `consistency`: finding counts by domain, finding type, and severity.

Distribution plots are the one intentional case-level exception. Their input is
minimized to feature metadata and numeric values and does not contain `case_id`.
No fitted curve or inferential statistic is added.

## Feature HTML report

`render_epi_report(deidentified_dir, report = "features")` creates
`reports/initial_epi_features.html`. The self-contained report uses
`features/summary/*.csv` as the authority for aggregate statistics and uses only
minimized, identifier-free numeric/count values for distribution figures. It
contains overview, coverage, prevalence, categorical, numeric/count,
missingness, consistency, feature-definition, provenance, and scope sections.
It does not embed `epi_features.csv`, `feature_long.csv`, case identifiers, or
pseudonym identifiers.

The report is for controlled analytical use and is not automatically suitable
for unrestricted public release.

## Privacy and immutability boundaries

Summarization may modify only `features/summary/`. Plotting modifies no files.
Feature rendering may modify only `reports/`. The functions do not access
`record_crosswalk.csv`, `entity_crosswalk.csv`, identifiable collation, or raw
extraction output.

## Before future modeling

The next phase must begin with outcome definition, not immediate model fitting.
Before regression or other inference, establish the outcome, unit of analysis,
sampling frame, candidate predictors, post-outcome or potentially endogenous
features, and temporal ordering. Those decisions are intentionally outside the
scope of this descriptive layer.
