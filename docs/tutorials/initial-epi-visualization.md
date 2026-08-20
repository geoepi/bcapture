# Initial Epi Visualization Tutorial

This tutorial uses the synthetic de-identified workflow. It does not require
or expose identifiable data or the private crosswalk.

After creating a de-identified output and its summary products:

```r
epi_summary <- summarize_epi(
  deidentified_dir = epi_deid_out,
  write = FALSE
)

plot_epi_categorical(
  epi_summary,
  variable = "closest_field_tilled_last_fall"
)

plot_epi_multiselect(
  epi_summary,
  variable = "C3"
)

plot_epi_numeric(
  epi_summary,
  variable = "baseline_mortality"
)

plot_epi_repeated(
  epi_summary,
  type = "counts"
)

plot_epi_validation(
  epi_summary,
  type = "status"
)
```

The examples return `ggplot2` objects for display in the RStudio plot pane,
Quarto, or another supported graphics device. They do not write image files.
Use `ggplot2::ggsave()` explicitly when a file is wanted.

Categorical and multiselect plots use percentages already supplied by
`summarize_epi()`. Numeric plots show aggregate intervals rather than a
histogram or an inferred distribution. Repeated categorical plots use records
with a nonmissing response as their unit, while validation status is reported
at the case level.

For report output, continue to use
`render_epi_report(deidentified_dir = epi_deid_out, report = "summary")` or
`report = "quality"`. The report layer and these reusable plots share the same
summary-product authority and privacy boundary.
