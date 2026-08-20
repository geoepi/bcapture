.feature_fixture <- function() {
  parsed <- parse(testthat::test_path("test-deidentify-epi.R"))
  helper_env <- new.env(parent = globalenv())
  eval(parsed[[1L]], helper_env)
  eval(parsed[[2L]], helper_env)
  with_values <- function(base, values) {
    base[names(values)] <- values
    base
  }
  base <- lapply(seq_len(5L), function(i) {
    value <- helper_env$.deid_values(
      owner = paste("Synthetic Owner", i), premises = paste("Synthetic Farm", i)
    )
    value$premid <- sprintf("SYNTHETIC-PREMISES-%03d", i)
    value
  })
  values <- list(
    synthetic_1 = with_values(base[[1L]], c(
      p0005 = "1", p0106 = "1", p0314 = "1", p0122 = "1",
      p0123a = "Manager", p0123b = "01/01/2025",
      p0123d = "Veterinarian", p0123e = "01/02/2025"
    )),
    synthetic_2 = with_values(base[[2L]], c(
      p0005 = "2", p0106 = "3", p0314 = "3", p0122 = "3"
    )),
    synthetic_3 = with_values(base[[3L]], c(
      p0005 = "3", p0106 = "1", p0314 = "4", p0122 = "3",
      p0123a = "Service worker", p0123b = "01/03/2025"
    )),
    synthetic_4 = with_values(base[[4L]], c(
      p0005 = "4", p0106 = "3", p0314 = "na", p0122 = "1"
    )),
    synthetic_5 = with_values(base[[5L]], c(
      p0005 = NA_character_, p0106 = NA_character_, p0314 = NA_character_,
      p0122 = NA_character_, p0123a = "Worker A", p0123b = "01/04/2025",
      p0123d = "Worker B", p0123e = "01/05/2025",
      p0123g = "Worker C", p0123h = "01/06/2025"
    ))
  )
  source_dir <- helper_env$.deid_synthetic_output(values, schemas = rep("schema_a", 5L))
  deidentified_dir <- tempfile("epi-feature-deidentified-")
  crosswalk_dir <- tempfile("epi-feature-crosswalk-")
  bcapture::deidentify_epi(source_dir, deidentified_dir, crosswalk_dir, quiet = TRUE)
  unlink(crosswalk_dir, recursive = TRUE, force = TRUE)
  validation_dir <- file.path(deidentified_dir, "validation")
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(tibble::tibble(
    case_id = sprintf("CASE-%06d", 1:5), n_info = 0L, n_warning = 0L,
    n_error = 0L, validation_status = c("valid", "review", "error", "valid", "valid")
  ), file.path(validation_dir, "validation_form_summary.csv"))
  deidentified_dir
}

.feature_source_checksums <- function(path) {
  roots <- file.path(path, c("collated", "validation", "privacy", "summary", "reports"))
  roots <- roots[dir.exists(roots)]
  files <- unlist(lapply(roots, fs::dir_ls, recurse = TRUE, type = "file"), use.names = FALSE)
  stats::setNames(as.character(tools::md5sum(files)), fs::path_rel(files, path))
}

.feature_keep_cases <- function(path, cases) {
  roots <- c(file.path(path, "collated"), file.path(path, "validation"))
  files <- unlist(lapply(roots[dir.exists(roots)], fs::dir_ls,
    regexp = "[.]csv$", type = "file"), use.names = FALSE)
  for (file in files) {
    data <- readr::read_csv(file, show_col_types = FALSE, na = c("", "NA"))
    if ("case_id" %in% names(data)) {
      data <- data[as.character(data$case_id) %in% cases, , drop = FALSE]
      readr::write_csv(data, file, na = "")
    }
  }
  invisible(path)
}

test_that("feature registry is versioned traceable and privacy retained", {
  deidentified_dir <- .feature_fixture()
  input <- bcapture:::.epi_summary_validate_inputs(deidentified_dir, "2024-05-28")
  registry <- bcapture:::.epi_feature_registry()

  expect_equal(nrow(registry$features), 40L)
  expect_equal(nrow(registry$domains), 9L)
  expect_equal(anyDuplicated(registry$features$feature_name), 0L)
  expect_true(all(registry$features$feature_type %in%
    c("binary", "count", "numeric", "categorical")))
  expect_identical(registry$hash,
    as.character(registry$manifest$feature_registry_hash[[1L]]))
  expect_silent(bcapture:::.epi_feature_validate_registry(
    registry, input$dictionary, input$policy
  ))
  expect_true(dir.exists(bcapture:::.epi_feature_registry_root("2024-05-28")))
  installed_root <- system.file(
    "extdata", "features", "initial_epi", "2024-05-28", package = "bcapture"
  )
  expect_true(nzchar(installed_root))
  expect_true(all(file.exists(file.path(installed_root,
    c("features.csv", "domains.csv", "manifest.csv", "README.md", "design_audit.csv")))))
})

test_that("binary coded numeric count and categorical semantics are exact", {
  result <- bcapture::derive_epi_features(.feature_fixture(), write = FALSE, quiet = TRUE)

  expect_equal(result$features$carcass_bin_available_reported,
    c(TRUE, FALSE, TRUE, FALSE, NA))
  expect_equal(result$features$baseline_mortality, c(1, 2, 3, 4, NA))
  expect_equal(result$features$supplemental_feed_response,
    c("Yes", "No", "Don't know", "Not applicable", NA))
  expect_equal(result$features$worker_visit_records, c(2L, 0L, 1L, 0L, 3L))

  supplemental <- result$feature_long[
    result$feature_long$feature_name == "supplemental_feed_response", , drop = FALSE]
  expect_equal(supplemental$value_status,
    c("known", "known", "unknown", "not_applicable", "missing"))
  expect_equal(supplemental$character_value,
    c("Yes", "No", "Don't know", "Not applicable", NA))
  count_summary <- result$feature_summary[
    result$feature_summary$feature_name == "worker_visit_records", , drop = FALSE]
  expect_equal(count_summary$mean, 1.2)
  expect_equal(count_summary$median, 1)
  expect_equal(count_summary$min, 0)
  expect_equal(count_summary$max, 3)
})

test_that("parent child contradictions are preserved and flagged", {
  result <- bcapture::derive_epi_features(.feature_fixture(), write = FALSE, quiet = TRUE)
  expect_equal(result$features$worker_other_premises_visits_reported,
    c(TRUE, FALSE, FALSE, TRUE, NA))
  expect_equal(result$features$worker_visit_records, c(2L, 0L, 1L, 0L, 3L))

  findings <- result$consistency_findings[
    result$consistency_findings$feature_name ==
      "worker_other_premises_visits_reported", , drop = FALSE]
  expect_equal(findings$case_id, c("CASE-000003", "CASE-000004", "CASE-000005"))
  expect_equal(findings$severity, c("WARNING", "INFO", "INFO"))
  expect_equal(findings$finding_type, c(
    "parent_no_child_rows", "parent_yes_no_child_rows", "parent_missing_child_rows"
  ))
  expect_equal(result$manifest$status, "review")
  expect_equal(result$manifest$n_consistency_warnings, 1L)
  expect_equal(result$manifest$n_consistency_errors, 0L)
})

test_that("zero child rows never replace a parent response", {
  result <- bcapture::derive_epi_features(.feature_fixture(), write = FALSE, quiet = TRUE)
  expect_false(result$features$worker_other_premises_visits_reported[[2L]])
  expect_equal(result$features$worker_visit_records[[2L]], 0L)
  expect_true(result$features$worker_other_premises_visits_reported[[4L]])
  expect_equal(result$features$worker_visit_records[[4L]], 0L)
  expect_true(is.na(result$features$worker_other_premises_visits_reported[[5L]]))
  expect_equal(result$features$worker_visit_records[[5L]], 3L)
})

test_that("privacy and identifiable input gates cannot be bypassed", {
  deidentified_dir <- .feature_fixture()
  manifest_path <- file.path(deidentified_dir, "privacy", "deidentification_manifest.csv")
  manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
  manifest$status <- "review"
  readr::write_csv(manifest, manifest_path)
  expect_error(bcapture::derive_epi_features(deidentified_dir, write = FALSE, quiet = TRUE),
    "status passed")
  reviewed <- bcapture::derive_epi_features(
    deidentified_dir, write = FALSE, strict = FALSE, quiet = TRUE
  )
  expect_equal(reviewed$manifest$status, "review")

  manifest$privacy_errors <- 1L
  readr::write_csv(manifest, manifest_path)
  expect_error(bcapture::derive_epi_features(
    deidentified_dir, write = FALSE, strict = FALSE, quiet = TRUE
  ), "privacy_errors")

  deidentified_dir <- .feature_fixture()
  forms_path <- file.path(deidentified_dir, "collated", "epi_forms.csv")
  forms <- readr::read_csv(forms_path, show_col_types = FALSE)
  forms$form_id <- "identifiable"
  readr::write_csv(forms, forms_path)
  expect_error(bcapture::derive_epi_features(deidentified_dir, write = FALSE, quiet = TRUE),
    "Identifiable-source columns")

  deidentified_dir <- .feature_fixture()
  unlink(file.path(deidentified_dir, "validation"), recursive = TRUE, force = TRUE)
  expect_error(bcapture::derive_epi_features(deidentified_dir, write = FALSE, quiet = TRUE),
    "Validation products")
  no_validation <- bcapture::derive_epi_features(
    deidentified_dir, write = FALSE, strict = FALSE, quiet = TRUE
  )
  expect_false(no_validation$manifest$validation_available)
  expect_true(all(no_validation$features$validation_status == "not_available"))
  expect_equal(no_validation$manifest$status, "review")

  deidentified_dir <- .feature_fixture()
  unlink(file.path(deidentified_dir, "privacy", "deidentification_manifest.csv"))
  expect_error(bcapture::derive_epi_features(deidentified_dir, write = FALSE, quiet = TRUE),
    "deidentification_manifest")
})

test_that("registry validation rejects privacy-excluded sources", {
  deidentified_dir <- .feature_fixture()
  input <- bcapture:::.epi_summary_validate_inputs(deidentified_dir, "2024-05-28")
  registry <- bcapture:::.epi_feature_registry()
  registry$features$source_variable[[1L]] <- "premises_name"
  registry$hash <- bcapture:::.epi_feature_registry_hash(
    registry$features, registry$domains
  )
  registry$manifest$feature_registry_hash[[1L]] <- registry$hash
  expect_error(bcapture:::.epi_feature_validate_registry(
    registry, input$dictionary, input$policy
  ), "privacy action is not retain")
})

test_that("unknown codes stop strictly and remain auditable non-strictly", {
  deidentified_dir <- .feature_fixture()
  path <- file.path(deidentified_dir, "collated", "epi_responses_long.csv")
  responses <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
  index <- responses$case_id == "CASE-000001" &
    responses$canonical_name == "carcass_bin_available"
  responses$response_code[index] <- "999"
  responses$response_label[index] <- "Undefined synthetic code"
  responses$value[index] <- "Undefined synthetic code"
  responses$raw_value[index] <- "999"
  readr::write_csv(responses, path, na = "")
  expect_error(bcapture::derive_epi_features(deidentified_dir, write = FALSE, quiet = TRUE),
    "cannot be interpreted safely")
  result <- bcapture::derive_epi_features(
    deidentified_dir, write = FALSE, strict = FALSE, quiet = TRUE
  )
  value <- result$feature_long[
    result$feature_long$case_id == "CASE-000001" &
      result$feature_long$feature_name == "carcass_bin_available_reported", , drop = FALSE]
  expect_equal(value$value_status, "unsupported")
  expect_true(is.na(value$logical_value))
  expect_equal(result$manifest$n_unsupported_derivations, 1L)
  expect_equal(result$manifest$status, "review")
})

test_that("derivation is independent of input row order", {
  deidentified_dir <- .feature_fixture()
  first <- bcapture::derive_epi_features(deidentified_dir, write = FALSE, quiet = TRUE)
  paths <- file.path(deidentified_dir, "collated", c(
    "epi_responses_long.csv", "epi_worker_visits.csv", "epi_visitors.csv"
  ))
  set.seed(42)
  for (path in paths) {
    data <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
    data <- data[sample(seq_len(nrow(data))), , drop = FALSE]
    readr::write_csv(data, path, na = "")
  }
  second <- bcapture::derive_epi_features(deidentified_dir, write = FALSE, quiet = TRUE)
  expect_identical(first$features, second$features)
  expect_identical(first$feature_long, second$feature_long)
  expect_identical(first$feature_summary, second$feature_summary)
  expect_identical(first$consistency_findings, second$consistency_findings)
})

test_that("duplicate row identities are counted and flagged without deduplication", {
  deidentified_dir <- .feature_fixture()
  path <- file.path(deidentified_dir, "collated", "epi_worker_visits.csv")
  workers <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
  duplicate <- workers[workers$case_id == "CASE-000001", , drop = FALSE][1L, , drop = FALSE]
  readr::write_csv(dplyr::bind_rows(workers, duplicate), path, na = "")
  result <- bcapture::derive_epi_features(deidentified_dir, write = FALSE, quiet = TRUE)
  expect_equal(result$features$worker_visit_records[[1L]], 3L)
  expect_true(any(result$consistency_findings$case_id == "CASE-000001" &
    result$consistency_findings$finding_type == "duplicate_child_row_identity"))
})

test_that("write overwrite and source immutability contracts hold", {
  deidentified_dir <- .feature_fixture()
  before <- .feature_source_checksums(deidentified_dir)
  result <- bcapture::derive_epi_features(deidentified_dir, quiet = TRUE)
  expected <- c(
    "epi_features.csv", "feature_dictionary.csv", "feature_long.csv",
    "feature_summary.csv", "feature_domain_summary.csv",
    "feature_consistency_findings.csv", "feature_derivation_audit.csv",
    "feature_manifest.csv"
  )
  expect_true(all(file.exists(file.path(deidentified_dir, "features", expected))))
  expect_identical(before, .feature_source_checksums(deidentified_dir))
  expect_error(bcapture::derive_epi_features(deidentified_dir, quiet = TRUE),
    "already exists")
  expect_silent(bcapture::derive_epi_features(
    deidentified_dir, overwrite = TRUE, quiet = TRUE
  ))
  expect_identical(before, .feature_source_checksums(deidentified_dir))
  expect_equal(result$manifest$n_features, 40L)
  expect_false(any(grepl(
    "PREMISES-|PERSON-|ORG-|ENTITY-|CONTACT-|LOCATION-",
    as.character(unlist(result[c("features", "feature_long")])))
  ))
})

test_that("write false single-case all-missing all-false and empty inputs are stable", {
  deidentified_dir <- .feature_fixture()
  .feature_keep_cases(deidentified_dir, "CASE-000002")
  result <- bcapture::derive_epi_features(deidentified_dir, write = FALSE, quiet = TRUE)
  expect_false(dir.exists(file.path(deidentified_dir, "features")))
  expect_equal(nrow(result$features), 1L)
  expect_false(result$features$carcass_bin_available_reported[[1L]])
  expect_equal(result$feature_summary$n_true[
    result$feature_summary$feature_name == "carcass_bin_available_reported"], 0L)
  expect_true(is.na(result$feature_summary$sd[
    result$feature_summary$feature_name == "baseline_mortality"]))

  missing_dir <- .feature_fixture()
  .feature_keep_cases(missing_dir, "CASE-000005")
  missing <- bcapture::derive_epi_features(missing_dir, write = FALSE, quiet = TRUE)
  expect_true(is.na(missing$features$baseline_mortality[[1L]]))
  expect_equal(missing$feature_summary$n_missing[
    missing$feature_summary$feature_name == "baseline_mortality"], 1L)

  empty_dir <- .feature_fixture()
  .feature_keep_cases(empty_dir, character())
  expect_error(bcapture::derive_epi_features(empty_dir, write = FALSE, quiet = TRUE),
    "No de-identified Initial Epi cases")
})
