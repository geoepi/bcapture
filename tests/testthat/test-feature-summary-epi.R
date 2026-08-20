test_that("feature summaries retain exact typed semantics and coverage", {
  deidentified_dir <- .feature_analysis_fixture()
  features <- bcapture::derive_epi_features(
    deidentified_dir, write = TRUE, quiet = TRUE
  )

  numeric_rows <- features$feature_long$feature_name == "baseline_mortality"
  features$feature_long$numeric_value[numeric_rows] <- c(1, 2, 3, NA, 5)
  features$feature_long$value[numeric_rows] <- c("1", "2", "3", NA, "5")
  features$feature_long$value_status[numeric_rows] <- c(
    "known", "known", "known", "missing", "known"
  )
  features$consistency_findings <- dplyr::bind_rows(
    features$consistency_findings[
      features$consistency_findings$severity == "WARNING", , drop = FALSE],
    dplyr::mutate(features$consistency_findings[
      features$consistency_findings$severity == "WARNING", , drop = FALSE],
      case_id = "CASE-000002"),
    features$consistency_findings[
      features$consistency_findings$severity == "INFO", , drop = FALSE][1L, , drop = FALSE]
  )
  before <- .feature_analysis_checksums(deidentified_dir)
  result <- bcapture::summarize_epi_features(
    features, write = TRUE, quiet = TRUE
  )

  binary <- result$binary[result$binary$feature_name ==
    "carcass_bin_available_reported", , drop = FALSE]
  expect_equal(binary[c("n_cases", "n_known", "n_true", "n_false", "n_missing")],
    tibble::tibble(n_cases = 5L, n_known = 4L, n_true = 2L,
      n_false = 2L, n_missing = 1L))
  expect_equal(binary$percent_true_of_known, 50)
  expect_equal(binary$percent_true_of_all_cases, 40)

  categorical <- result$categorical[result$categorical$feature_name ==
    "supplemental_feed_response", , drop = FALSE]
  expect_setequal(categorical$category,
    c("Yes", "No", "Don't know", "Not applicable"))
  expect_equal(categorical$n[match(
    c("Yes", "No", "Don't know", "Not applicable"), categorical$category)],
    rep(1L, 4L))
  expect_true(all(categorical$n_known == 4L))
  expect_true(all(categorical$n_missing == 1L))
  expect_true(any(result$categorical$feature_name ==
    "wildlife_management_plan_response" & result$categorical$n == 0L))

  count <- result$count[result$count$feature_name == "worker_visit_records", , drop = FALSE]
  expect_equal(count$n_zero, 2L)
  expect_equal(count$n_positive, 3L)
  expect_equal(count$median, 1)
  numeric <- result$numeric[result$numeric$feature_name == "baseline_mortality", , drop = FALSE]
  expect_equal(numeric$n_nonmissing, 4L)
  expect_equal(numeric$n_missing, 1L)
  expect_equal(numeric$mean, 2.75)
  expect_equal(numeric$sd, stats::sd(c(1, 2, 3, 5)))
  expect_equal(numeric$median, 2.5)
  expect_equal(numeric$q25, 1.75)
  expect_equal(numeric$q75, 3.5)
  expect_equal(numeric$min, 1)
  expect_equal(numeric$max, 5)

  missing <- result$missingness[result$missingness$feature_name ==
    "supplemental_feed_response", , drop = FALSE]
  expect_equal(missing$n_known, 4L)
  expect_equal(missing$n_missing, 1L)
  expect_equal(missing$percent_missing, 20)
  expect_equal(nrow(result$missingness), 40L)

  severity <- result$consistency[result$consistency$summary_level ==
    "severity_total", , drop = FALSE]
  expect_equal(severity$n_findings[match(c("WARNING", "INFO", "ERROR"),
    severity$severity)], c(2L, 1L, 0L))
  expect_equal(result$overview$n_consistency_findings, 3L)
  expect_equal(result$overview$n_consistency_warnings, 2L)
  expect_equal(result$overview$n_consistency_errors, 0L)
  expect_equal(result$manifest$feature_summary_schema_version, 1L)
  expect_equal(result$manifest$status, "review")
  expect_equal(c(nrow(result$binary), nrow(result$categorical),
    nrow(result$numeric), nrow(result$count)), c(23L, 7L, 4L, 11L))
  expect_identical(before, .feature_analysis_checksums(deidentified_dir))

  expected_files <- unname(bcapture:::.epi_feature_summary_files)
  expect_true(all(file.exists(file.path(
    deidentified_dir, "features", "summary", expected_files
  ))))
  expect_error(bcapture::summarize_epi_features(
    deidentified_dir, quiet = TRUE
  ), "already exists")
  expect_silent(bcapture::summarize_epi_features(
    deidentified_dir, overwrite = TRUE, quiet = TRUE
  ))
})

test_that("domain coverage and write false are exact and deterministic", {
  deidentified_dir <- .feature_analysis_fixture()
  features <- bcapture::derive_epi_features(
    deidentified_dir, write = FALSE, quiet = TRUE
  )
  selected <- c(
    "carcass_bin_available_reported", "supplemental_feed_response",
    "worker_visit_records"
  )
  features$feature_dictionary <- features$feature_dictionary[
    features$feature_dictionary$feature_name %in% selected, , drop = FALSE]
  features$feature_dictionary$domain_id <- "fixture_domain"
  features$feature_dictionary$domain_label <- "Fixture domain"
  features$feature_dictionary$domain_order <- 1L
  features$feature_long <- features$feature_long[
    features$feature_long$feature_name %in% selected, , drop = FALSE]
  features$feature_long$domain_id <- "fixture_domain"
  features$feature_long$domain_label <- "Fixture domain"
  binary_rows <- features$feature_long$feature_name ==
    "carcass_bin_available_reported"
  features$feature_long$logical_value[binary_rows] <- c(TRUE, FALSE, TRUE, NA, FALSE)
  features$feature_long$value[binary_rows] <- c("TRUE", "FALSE", "TRUE", NA, "FALSE")
  features$feature_long$value_status[binary_rows] <- c(
    "known", "known", "known", "missing", "known"
  )
  features$consistency_findings <- features$consistency_findings[0, , drop = FALSE]
  features$manifest$n_features <- 3L
  features$manifest$n_domains <- 1L
  features$manifest$n_consistency_findings <- 0L
  features$manifest$n_consistency_warnings <- 0L
  features$manifest$n_consistency_errors <- 0L
  features$manifest$status <- "passed"

  result <- bcapture::summarize_epi_features(
    features, write = FALSE, quiet = TRUE
  )
  coverage <- result$domain_coverage
  expect_equal(coverage$n_case_feature_cells, 15L)
  expect_equal(coverage$n_known_cells, 13L)
  expect_equal(coverage$n_missing_cells, 2L)
  expect_equal(coverage$percent_known_cells, 100 * 13 / 15)
  expect_equal(coverage$n_cases_with_any_known_feature, 5L)
  expect_equal(coverage$percent_cases_with_any_known_feature, 100)
  expect_equal(coverage$n_cases_with_all_features_known, 3L)
  expect_equal(coverage$percent_cases_with_all_features_known, 60)
  expect_false(dir.exists(file.path(deidentified_dir, "features")))

  second <- bcapture::summarize_epi_features(
    features, write = FALSE, quiet = TRUE
  )
  stable <- setdiff(names(result), c("manifest", "output_dir"))
  expect_identical(result[stable], second[stable])
  expect_error(bcapture::summarize_epi_features(
    tempfile("missing-feature-products-"), write = FALSE, quiet = TRUE
  ), "Feature products are missing; run derive_epi_features\\(\\) first\\.")
})
