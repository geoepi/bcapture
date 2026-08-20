# Initial Epi scientific visualization

The visualization layer provides reusable static `ggplot2` objects for the
de-identified Initial Epi summary products created by `summarize_epi()`. It is
descriptive only: it does not add inferential statistics, models, risk scores,
maps, networks, or derived biological variables.

The functions are:

```r
plot_epi_categorical()
plot_epi_multiselect()
plot_epi_numeric()
plot_epi_repeated()
plot_epi_validation()
```

Each accepts either an in-memory result from `summarize_epi(write = FALSE)` or
a de-identified directory containing an existing `summary/` directory. A
missing summary directory is an error; plotting never runs `summarize_epi()`
implicitly. The functions read summary products only and return ordinary
`ggplot` objects. They do not save files, access identifiable Initial Epi data,
read the private re-identification crosswalk, or read collated case-level
tables.

## Statistical authority

Plot values are taken directly from the summary products:

- categorical responses use `scalar_frequencies.csv`;
- multiselect responses use `multiselect_frequencies.csv`;
- numeric intervals use `numeric_summaries.csv`;
- repeated categorical responses and record counts use the two repeated-table
  summary products; and
- validation plots use the validation status and rule summary products.

The plotting layer does not redefine counts, denominators, medians, quartiles,
date statistics, validation findings, or repeated-table counts. Categorical
plots retain zero-count codebook levels and explicit `Not applicable` values.
Multiselect captions identify whether percentages use all cases or selecting
cases and state that item percentages need not sum to 100%. Repeated
categorical plots identify their unit as records with a nonmissing response,
not cases.

Numeric plots show only the supplied median, Q1--Q3, and minimum--maximum
intervals. They do not reconstruct a distribution from aggregate statistics.
Missing and unparsed counts are included in captions where relevant.

## Customization

```r
summary <- summarize_epi(deidentified_dir = "epi_analysis", write = FALSE)
p <- plot_epi_categorical(summary, variable = "closest_field_tilled_last_fall")
p + ggplot2::labs(title = "Field preparation responses")
```

Variable selection uses exact canonical names or unique question IDs. An
ambiguous question ID or an unknown identifier produces an actionable error;
the resolver does not fuzzy-match names. Returned plots can be modified with
ordinary `ggplot2` layers and themes, or saved by the caller with
`ggplot2::ggsave()`.

These plots remain controlled-use analytical products. They are not
automatically suitable for unrestricted public release.
