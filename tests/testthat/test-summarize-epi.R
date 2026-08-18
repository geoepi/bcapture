.summary_fixture <- function() {
  parsed <- parse(testthat::test_path("test-deidentify-epi.R"))
  helper_env <- new.env(parent = globalenv())
  eval(parsed[[1L]], helper_env)
  eval(parsed[[2L]], helper_env)
  with_values <- function(base, values) {
    base[names(values)] <- values
    base
  }
  values <- list(
    synthetic_1 = with_values(helper_env$.deid_values(), c(
      p0019 = "1", p0314 = "na", p0005 = "1",
      p0001 = "01/01/2025", p0125a = "On", p0125b = "On",
      p0133 = "1", p0133a = "01/01/2025", p0133b = "Visitor A",
      p0134 = "1", p0134a = "01/02/2025", p0134b = "Visitor B"
    )),
    synthetic_2 = with_values(
      helper_env$.deid_values(owner = "Second Owner", premises = "Second Farm"), c(
      p0019 = "3", p0005 = "2", p0001 = "01/03/2025", p0125b = "On",
      p0133 = "1", p0133a = "01/03/2025", p0133b = "Visitor C"
    )),
    synthetic_3 = with_values(
      helper_env$.deid_values(owner = "Third Owner", premises = "Third Farm"), c(
      p0019 = NA_character_, p0005 = "not numeric", p0001 = "Daily"
    ))
  )
  values[[2L]]$premid <- "SYNTHETIC-PREMISES-002"
  values[[3L]]$premid <- "SYNTHETIC-PREMISES-003"
  source_dir <- helper_env$.deid_synthetic_output(
    values, schemas = c("schema_a", "schema_a", "schema_a")
  )
  deidentified_dir <- tempfile("epi-summary-deidentified-")
  crosswalk_dir <- tempfile("epi-summary-crosswalk-")
  bcapture::deidentify_epi(source_dir, deidentified_dir, crosswalk_dir, quiet = TRUE)
  unlink(crosswalk_dir, recursive = TRUE, force = TRUE)
  validation_dir <- file.path(deidentified_dir, "validation")
  dir.create(validation_dir, recursive = TRUE)
  visitors_path <- file.path(deidentified_dir, "collated", "epi_visitors.csv")
  visitors <- readr::read_csv(visitors_path, show_col_types = FALSE)
  readr::write_csv(visitors[visitors$case_id != "CASE-000002", , drop = FALSE],
    visitors_path)
  readr::write_csv(tibble::tibble(
    case_id = sprintf("CASE-%06d", 1:3),
    n_info = 0L, n_warning = c(0L, 1L, 0L), n_error = c(0L, 0L, 1L),
    validation_status = c("valid", "review", "error")
  ), file.path(validation_dir, "validation_form_summary.csv"))
  readr::write_csv(tibble::tibble(
    severity = "ERROR", validation_type = "synthetic", rule_id = "synthetic_rule",
    case_id = "CASE-000003", raw_field = NA_character_,
    canonical_name = NA_character_, message = "Synthetic quality finding."
  ), file.path(validation_dir, "validation_results.csv"))
  readr::write_csv(tibble::tibble(
    severity = "ERROR", validation_type = "synthetic", rule_id = "synthetic_rule",
    n_findings = 1L, n_forms = 1L
  ), file.path(validation_dir, "validation_summary.csv"))
  deidentified_dir
}

.summary_source_checksums <- function(path) {
  files <- fs::dir_ls(path, recurse = TRUE, type = "file")
  files <- files[!grepl("summary", files, fixed = TRUE)]
  stats::setNames(as.character(tools::md5sum(files)), fs::path_rel(files, path))
}

test_that("summarize_epi creates metadata-driven descriptive products", {
  deidentified_dir <- .summary_fixture()
  before <- .summary_source_checksums(deidentified_dir)
  result <- bcapture::summarize_epi(deidentified_dir, quiet = TRUE)

  expect_equal(result$manifest$status, "passed")
  expect_equal(result$manifest$n_cases, 3L)
  expect_equal(result$manifest$n_unsupported_retained_fields, 0L)
  expect_equal(nrow(result$field_inventory), 150L)
  expect_equal(sum(result$field_inventory$summary_status == "summarized"), 102L)
  expect_equal(sum(result$field_inventory$summary_status == "excluded_privacy"), 48L)
  expect_true(all(file.exists(file.path(
    deidentified_dir, "summary",
    c("dataset_overview.csv", "summary_field_inventory.csv",
      "scalar_frequencies.csv", "multiselect_frequencies.csv",
      "repeated_categorical_frequencies.csv", "numeric_summaries.csv",
      "date_summaries.csv", "case_table_counts.csv",
      "repeated_table_summaries.csv", "validation_rule_summary.csv",
      "validation_status_summary.csv", "summary_manifest.csv")
  ))))

  coded <- result$scalar_frequencies[
    result$scalar_frequencies$canonical_name == "eggs_for_research", , drop = FALSE
  ]
  expect_equal(coded$response_label, c("Yes", "No"))
  expect_equal(coded$n_response, c(1L, 1L))
  expect_true(all(coded$n_answered == 2L))
  expect_true(all(coded$n_missing == 1L))
  expect_equal(coded$percent_of_answered, c(50, 50))
  expect_equal(coded$percent_of_all_cases, c(100 / 3, 100 / 3))

  not_applicable <- result$scalar_frequencies[
    result$scalar_frequencies$canonical_name == "supplemental_feed", , drop = FALSE
  ]
  expect_true(any(not_applicable$response_label == "Not applicable"))
  expect_true(all(not_applicable$n_answered == 1L))
  expect_true(all(not_applicable$n_missing == 2L))
  expect_equal(not_applicable$n_response[not_applicable$response_label == "Not applicable"], 1L)

  numeric <- result$numeric_summaries[
    result$numeric_summaries$canonical_name == "baseline_mortality", , drop = FALSE
  ]
  expect_equal(numeric$n_total, 3L)
  expect_equal(numeric$n_populated, 3L)
  expect_equal(numeric$n_parsed, 2L)
  expect_equal(numeric$n_unparsed, 1L)
  expect_equal(numeric$n_missing, 0L)
  expect_equal(numeric$mean, 1.5)
  expect_equal(numeric$median, 1.5)
  expect_equal(numeric$q25, 1.25)
  expect_equal(numeric$q75, 1.75)

  dates <- result$date_summaries[
    result$date_summaries$canonical_name == "today_date", , drop = FALSE
  ]
  expect_s3_class(dates$min_date, "Date")
  expect_equal(dates$n_populated, 3L)
  expect_equal(dates$n_parsed, 2L)
  expect_equal(dates$n_unparsed, 1L)
  expect_equal(as.character(dates$min_date), "2025-01-01")
  expect_equal(as.character(dates$median_date), "2025-01-02")
  expect_equal(as.character(dates$max_date), "2025-01-03")

  multi <- result$multiselect_frequencies[
    result$multiselect_frequencies$question_id == "C3", , drop = FALSE
  ]
  item_a <- multi[multi$item_code == "p0125a", , drop = FALSE]
  item_b <- multi[multi$item_code == "p0125b", , drop = FALSE]
  expect_equal(item_a$n_cases_with_any_selection, 2L)
  expect_equal(item_a$n_cases_selecting_item, 1L)
  expect_equal(item_b$n_cases_selecting_item, 2L)
  expect_equal(item_a$percent_of_selecting_cases, 50)
  expect_equal(item_b$percent_of_all_cases, 200 / 3)

  visitor_counts <- result$case_table_counts[
    result$case_table_counts$table_name == "visitors", , drop = FALSE
  ]
  expect_equal(visitor_counts$n_records, c(2L, 0L, 1L))
  visitor_summary <- result$repeated_table_summaries[
    result$repeated_table_summaries$table_name == "visitors", , drop = FALSE
  ]
  expect_equal(visitor_summary$n_records_total, 3L)
  expect_equal(visitor_summary$n_cases_with_records, 2L)
  expect_equal(visitor_summary$mean_records_per_case, 1)
  expect_equal(visitor_summary$median_records_per_case, 1)

  validation <- result$validation_status_summary
  expect_equal(validation$validation_status, c("valid", "review", "error"))
  expect_equal(validation$n_cases, c(1L, 1L, 1L))
  expect_equal(result$validation_rule_summary$n_cases, 1L)
  expect_equal(result$overview$n_validation_error, 1L)

  expect_true(any(result$field_inventory$summary_status == "excluded_privacy" &
    result$field_inventory$canonical_name == "premises_name"))
  content_products <- result[c(
    "scalar_frequencies", "multiselect_frequencies",
    "repeated_categorical_frequencies", "numeric_summaries", "date_summaries"
  )]
  expect_false(any(vapply(content_products, function(x)
    any(grepl("PREMISES-|PERSON-|ORG-|ENTITY-|CONTACT-|LOCATION-", as.character(unlist(x)))),
    logical(1))))
  expect_identical(before, .summary_source_checksums(deidentified_dir))
})

test_that("summarize_epi enforces privacy and validation gates", {
  deidentified_dir <- .summary_fixture()
  manifest_path <- file.path(deidentified_dir, "privacy", "deidentification_manifest.csv")
  manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
  manifest$status <- "review"
  readr::write_csv(manifest, manifest_path)
  expect_error(bcapture::summarize_epi(deidentified_dir, quiet = TRUE), "status passed")
  reviewed <- bcapture::summarize_epi(deidentified_dir, strict = FALSE, quiet = TRUE)
  expect_equal(reviewed$manifest$status, "review")

  manifest$privacy_errors <- 1L
  readr::write_csv(manifest, manifest_path)
  expect_error(bcapture::summarize_epi(deidentified_dir, strict = FALSE, quiet = TRUE),
    "privacy_errors")

  deidentified_dir <- .summary_fixture()
  unlink(file.path(deidentified_dir, "validation"), recursive = TRUE, force = TRUE)
  expect_error(bcapture::summarize_epi(deidentified_dir, quiet = TRUE), "Validation products")
  no_validation <- bcapture::summarize_epi(
    deidentified_dir, strict = FALSE, overwrite = TRUE, quiet = TRUE
  )
  expect_false(no_validation$manifest$validation_available)
  expect_equal(no_validation$manifest$status, "review")
  expect_equal(nrow(no_validation$validation_status_summary), 0L)
})

test_that("summarize_epi rejects identifiable products and protects output", {
  deidentified_dir <- .summary_fixture()
  forms_path <- file.path(deidentified_dir, "collated", "epi_forms.csv")
  forms <- readr::read_csv(forms_path, show_col_types = FALSE)
  forms$form_id <- "identifiable"
  readr::write_csv(forms, forms_path)
  expect_error(bcapture::summarize_epi(deidentified_dir, quiet = TRUE),
    "Identifiable-source columns")

  deidentified_dir <- .summary_fixture()
  bcapture::summarize_epi(deidentified_dir, quiet = TRUE)
  expect_error(bcapture::summarize_epi(deidentified_dir, quiet = TRUE), "already exists")
  expect_silent(bcapture::summarize_epi(
    deidentified_dir, overwrite = TRUE, quiet = TRUE
  ))
})
