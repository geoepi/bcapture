# Tutorial: Initial Epi feature analysis

This tutorial continues from a synthetic de-identified Initial Epi output. The
examples remain descriptive and use the implemented `environment_wildlife`
domain and `worker_visit_records` feature.

```r
library(bcapture)

epi_deid_out <- "synthetic_epi_deidentified"

features <- derive_epi_features(
  deidentified_dir = epi_deid_out
)

feature_summary <- summarize_epi_features(features)
```

The returned object contains the same nine aggregate products written below
`features/summary/`, plus minimized numeric/count values for plotting. Inspect
the denominator explicitly:

```r
feature_summary$binary[
  feature_summary$binary$feature_name ==
    "wild_birds_observed_reported",
  c("n_known", "n_true", "n_false", "n_missing",
    "percent_true_of_known")
]
```

Plot recorded prevalence for the environment and wildlife domain:

```r
plot_epi_features(
  feature_summary,
  type = "prevalence",
  domain = "environment_wildlife"
)
```

Plot one count distribution. Zero points mean zero qualifying retained-content
rows, not a confirmed absence of worker visits.

```r
plot_epi_features(
  feature_summary,
  type = "distribution",
  feature = "worker_visit_records"
)
```

Other descriptive views use the same object:

```r
plot_epi_features(feature_summary, type = "missingness")
plot_epi_features(feature_summary, type = "coverage")
plot_epi_features(feature_summary, type = "consistency")
```

Finally, render the identifier-minimized self-contained report:

```r
render_epi_report(
  deidentified_dir = epi_deid_out,
  report = "features"
)
```

The default output is `reports/initial_epi_features.html`. Aggregate values are
read from `features/summary/*.csv`; only numeric/count distribution values are
staged from the case-level feature layer, and they are staged without case
identifiers.

Do not proceed directly from this tutorial to model fitting. The next workflow
must first define the epidemiological outcome, unit of analysis, sampling frame,
candidate predictors, temporal ordering, and features that may be post-outcome
or endogenous.
