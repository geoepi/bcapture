# Initial Epi analytical feature workflow

This tutorial begins with a successful synthetic `deidentify_epi()` output
directory. Keep the private crosswalk physically separate; feature derivation
does not need it.

```r
library(bcapture)

features <- derive_epi_features(
  deidentified_dir = "epi_analysis",
  version = "2024-05-28",
  feature_version = "1"
)
```

Review provenance and definitions before analysis:

```r
features$manifest
features$feature_dictionary[, c(
  "feature_name", "domain_label", "feature_type",
  "source_question_id", "derivation", "missing_behavior"
)]
```

The wide product contains one controlled-use row per case. For example:

```r
features$features[, c(
  "case_id",
  "baseline_mortality",
  "worker_other_premises_visits_reported",
  "worker_visit_records",
  "birds_moved_off_reported",
  "bird_movement_records",
  "supplemental_feed_response"
)]
```

The typed-long product is preferable when feature type and missingness status
must be explicit:

```r
features$feature_long[, c(
  "case_id", "domain_id", "feature_name", "feature_type",
  "value_status", "logical_value", "numeric_value", "character_value"
)]
```

Always inspect consistency and derivation quality metadata:

```r
features$consistency_findings
features$audit
features$feature_summary
features$domain_summary
```

A zero record count is not automatically an explicit No response. For example,
`worker_visit_records == 0` and
`worker_other_premises_visits_reported == TRUE` are retained together and
flagged rather than reconciled silently.

For interactive work without writing `features/`:

```r
features <- derive_epi_features(
  deidentified_dir = "epi_analysis",
  write = FALSE
)
```

These case-level products remain controlled-use de-identified analytical data.
They are not risk scores, do not imply causation, and are not automatically
suitable for unrestricted public release.
