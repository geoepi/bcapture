test_that("feature plots use supplied values filters and create no files", {
  deidentified_dir <- .feature_analysis_fixture()
  features <- bcapture::derive_epi_features(
    deidentified_dir, write = FALSE, quiet = TRUE
  )
  summary <- bcapture::summarize_epi_features(
    features, write = FALSE, quiet = TRUE
  )
  before <- .feature_analysis_checksums(deidentified_dir, include_summary = TRUE)

  prevalence <- bcapture::plot_epi_features(summary, type = "prevalence")
  expect_s3_class(prevalence, "ggplot")
  expected_binary <- summary$binary[order(
    summary$binary$percent_true_of_known, na.last = TRUE), , drop = FALSE]
  expect_equal(prevalence$data$percent_true_of_known,
    expected_binary$percent_true_of_known)
  categorical <- bcapture::plot_epi_features(
    summary, type = "prevalence", feature = "supplemental_feed_response"
  )
  expect_s3_class(categorical, "ggplot")
  expect_setequal(categorical$data$category,
    c("Yes", "No", "Don't know", "Not applicable"))

  missingness <- bcapture::plot_epi_features(
    summary, type = "missingness", domain = "environment_wildlife"
  )
  expect_s3_class(missingness, "ggplot")
  expect_true(all(missingness$data$domain_id == "environment_wildlife"))
  expect_equal(missingness$data$percent_missing,
    sort(missingness$data$percent_missing, decreasing = TRUE))

  distribution <- bcapture::plot_epi_features(
    summary, type = "distribution", feature = "worker_visit_records"
  )
  expect_s3_class(distribution, "ggplot")
  expect_equal(distribution$data$numeric_value, c(2, 0, 1, 0, 3))
  expect_false("case_id" %in% names(distribution$data))

  coverage <- bcapture::plot_epi_features(summary, type = "coverage")
  expect_s3_class(coverage, "ggplot")
  expect_equal(coverage$data$percent_known_cells,
    sort(summary$domain_coverage$percent_known_cells))
  consistency <- bcapture::plot_epi_features(summary, type = "consistency")
  expect_s3_class(consistency, "ggplot")
  expect_equal(sum(consistency$data$n_findings),
    sum(summary$consistency$n_findings[
      summary$consistency$summary_level == "finding"]))

  filtered <- bcapture::plot_epi_features(
    summary, type = "missingness", feature = "baseline_mortality"
  )
  expect_equal(unique(filtered$data$feature_name), "baseline_mortality")
  expect_error(bcapture::plot_epi_features(
    summary, type = "distribution", feature = "not_a_feature"
  ), "No feature matched")
  expect_error(bcapture::plot_epi_features(
    summary, type = "missingness", domain = "not_a_domain"
  ), "No feature domain matched")
  expect_error(bcapture::plot_epi_features(features),
    "run summarize_epi_features")
  expect_identical(before,
    .feature_analysis_checksums(deidentified_dir, include_summary = TRUE))
})

test_that("feature plots read written summary products without writing", {
  deidentified_dir <- .feature_analysis_fixture()
  bcapture::derive_epi_features(deidentified_dir, write = TRUE, quiet = TRUE)
  bcapture::summarize_epi_features(deidentified_dir, write = TRUE, quiet = TRUE)
  before <- .feature_analysis_checksums(deidentified_dir, include_summary = TRUE)
  plot <- bcapture::plot_epi_features(
    deidentified_dir, type = "distribution", feature = "worker_visit_records"
  )
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$data$numeric_value, c(2, 0, 1, 0, 3))
  expect_identical(before,
    .feature_analysis_checksums(deidentified_dir, include_summary = TRUE))
  expect_error(bcapture::plot_epi_features(tempfile("missing-feature-summary-")),
    "run summarize_epi_features")
})
