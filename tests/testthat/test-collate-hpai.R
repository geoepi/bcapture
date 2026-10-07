phase3_write_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, path, na = "")
}

phase3_make_epi_safe <- function(root, record_ids = c("CASE-000001", "CASE-000002"), premises = c("PREMISES-000001", "PREMISES-000002"), today = rep("2025-01-01", length(record_ids))) {
  expected <- bcapture:::.phase3_epi_expected("2024-05-28")
  forms <- as.data.frame(setNames(lapply(expected$forms, function(x) rep(NA_character_, length(record_ids))), expected$forms), stringsAsFactors = FALSE)
  forms$case_id <- record_ids
  forms$form_version <- "2024-05-28"
  forms$dictionary_version <- "1"
  forms$dictionary_hash <- "synthetic-dictionary"
  forms$form_schema_hash <- "synthetic-schema"
  forms$schema_group <- "epi-schema-1"
  forms$premises_id <- premises
  forms$today_date <- today
  phase3_write_csv(forms, file.path(root, "collated", "epi_forms.csv"))
  phase3_write_csv(as.data.frame(setNames(lapply(expected$responses, function(x) character()), expected$responses)), file.path(root, "collated", "epi_responses_long.csv"))
  phase3_write_csv(as.data.frame(setNames(lapply(expected$multiselect, function(x) character()), expected$multiselect)), file.path(root, "collated", "epi_multiselect_responses.csv"))
  for (name in names(expected$tables)) phase3_write_csv(as.data.frame(setNames(lapply(expected$tables[[name]], function(x) character()), expected$tables[[name]])), file.path(root, "collated", paste0(name, ".csv")))
  phase3_write_csv(data.frame(case_id = record_ids, validation_status = "valid", stringsAsFactors = FALSE), file.path(root, "validation", "validation_form_summary.csv"))
  phase3_write_csv(data.frame(severity = character(), validation_type = character(), rule_id = character(), n_findings = character(), n_forms = character(), stringsAsFactors = FALSE), file.path(root, "validation", "validation_summary.csv"))
  phase3_write_csv(data.frame(form_version = "2024-05-28", profile = "analysis", forms_processed = length(record_ids), privacy_errors = 0, privacy_warnings = 0, status = "passed", stringsAsFactors = FALSE), file.path(root, "privacy", "deidentification_manifest.csv"))
  phase3_write_csv(data.frame(severity = character(), stringsAsFactors = FALSE), file.path(root, "privacy", "privacy_leak_audit.csv"))
  invisible(root)
}

phase3_make_bcap_safe <- function(root, record_ids = c("BCAP-CASE-000001", "BCAP-CASE-000002"), premises = c("PREMISES-000001", "PREMISES-000002")) {
  fields <- do.call(rbind, lapply(seq_along(record_ids), function(i) {
    data.frame(record_id = record_ids[[i]], field_index = c("1", "2"), page = c("1", "1"), field = c("npin", "Q1_1_1"), field_type = c("Tx", "Btn"), states = c("", "Off|Yes"), options = c("", "Yes"), is_populated = c("TRUE", "TRUE"), value = c(premises[[i]], "TRUE"), privacy_class = c("PSEUDONYMIZE", "SAFE_RETAIN"), privacy_action = c("PSEUDONYMIZE", "SAFE_RETAIN"), identifier_class = c("premises_identifier", "audit_control"), pseudonym_class = c("PREMISES", ""), stringsAsFactors = FALSE)
  }))
  metadata <- data.frame(record_id = record_ids, form_type = "bcap", schema_group = "schema_001", extraction_method = "acroform", template_family = "bcap", template_version = "2025-12-08", number_of_fields = 2, safe_populated_fields = 2, stringsAsFactors = FALSE)
  phase3_write_csv(fields, file.path(root, "combined", "hpai_fields_long.csv"))
  phase3_write_csv(metadata, file.path(root, "combined", "hpai_metadata.csv"))
  phase3_write_csv(data.frame(records_evaluated = length(record_ids), error_count = 0, warning_count = 0, status = "passed", stringsAsFactors = FALSE), file.path(root, "privacy", "privacy_audit_summary.csv"))
  phase3_write_csv(data.frame(form_family = "bcap", records_processed = length(record_ids), privacy_errors = 0, privacy_warnings = 0, status = "passed", stringsAsFactors = FALSE), file.path(root, "privacy", "deidentification_manifest.csv"))
  phase3_write_csv(data.frame(records_evaluated = length(record_ids), error_count = 0, warning_count = 0, status = "passed", stringsAsFactors = FALSE), file.path(root, "validation", "validation_summary.csv"))
  phase3_write_csv(data.frame(severity = character(), stringsAsFactors = FALSE), file.path(root, "privacy", "privacy_leak_audit.csv"))
  invisible(root)
}

phase3_make_crosswalk <- function(root, form_type, record_ids, premises_keys, premises_pseudonyms) {
  if (identical(form_type, "epi")) {
    data <- data.frame(case_id = record_ids, source_key = paste0("sha256:", seq_along(record_ids)), form_id = record_ids, source_sha256 = paste0("sha256-", seq_along(record_ids)), premises_pseudonym = premises_pseudonyms, premises_key = premises_keys, stringsAsFactors = FALSE)
  } else {
    data <- data.frame(source_key = record_ids, record_id = record_ids, source_file = paste0(record_ids, ".pdf"), source_md5 = paste0("md5-", seq_along(record_ids)), premises_key = premises_keys, premises_pseudonym = premises_pseudonyms, stringsAsFactors = FALSE)
  }
  phase3_write_csv(data, file.path(root, "record_crosswalk.csv"))
  invisible(root)
}

test_that("field metadata is complete and includes withheld fields", {
  required <- c("field_name", "canonical_name", "form_type", "form_version", "section", "subsection", "question_number", "question_text", "response_type", "source_page", "source_location", "source_field_name", "units", "allowed_values", "privacy_class", "included_in_safe_output", "table_name", "row_semantics", "column_semantics", "notes")
  for (form in c("epi", "bcap")) {
    metadata <- field_metadata(form)
    expect_true(all(required %in% names(metadata)))
    expect_gt(nrow(metadata), 0L)
    identity <- with(metadata, paste(form_version, table_name, row_semantics, column_semantics, source_field_name, sep = "\r"))
    expect_equal(anyDuplicated(identity), 0L)
    expect_true(all(metadata$privacy_class %in% c("SAFE_RETAIN", "SAFE_NORMALIZE", "PSEUDONYMIZE", "WITHHOLD", "CROSSWALK_ONLY")))
    if (identical(form, "epi")) expect_true(all(c("premises_address", "premises_id") %in% metadata$field_name))
    if (identical(form, "bcap")) {
      expect_true(any(metadata$privacy_class == "WITHHOLD"))
      expect_true(any(metadata$included_in_safe_output == FALSE))
    }
  }
})

test_that("Epi analysis collation is independent and separate", {
  root <- tempfile("phase3-epi-")
  dir.create(root)
  epi_dir <- phase3_make_epi_safe(file.path(root, "epi"), c("CASE-000001"), c("PREMISES-000001"))
  result <- collate_epi_analysis(epi_dir, file.path(root, "analysis"), run_id = "synthetic", quiet = TRUE)
  expect_identical(result$status, "passed")
  expect_equal(nrow(result$tables$records), 1L)
  expect_false("entity_id" %in% names(result$tables$records))
  expect_true(result$qa$metrics$value[result$qa$metrics$metric == "metadata_main_columns_missing"] == 0L)
  expect_true(result$qa$metrics$value[result$qa$metrics$metric == "metadata_repeated_fields_missing"] == 0L)
  expect_true(all(file.exists(file.path(root, "analysis", c("records.csv", "records.rds", "repeated_items.csv", "repeated_items.rds", "provenance.csv", "field_metadata.csv", "qa_summary.csv", "qa_findings.csv", "run_manifest.csv", "output_hashes.csv")))))
  expect_false(dir.exists(file.path(root, "analysis", "bcap")))
})

test_that("BCAP analysis collation is independent and excludes withheld values", {
  root <- tempfile("phase3-bcap-")
  dir.create(root)
  bcap_dir <- phase3_make_bcap_safe(file.path(root, "bcap"), c("BCAP-CASE-000001"), c("PREMISES-000001"))
  result <- collate_bcap(bcap_dir, file.path(root, "analysis"), run_id = "synthetic", quiet = TRUE)
  expect_identical(result$status, "passed")
  expect_equal(nrow(result$tables$records), 1L)
  expect_equal(nrow(result$tables$repeated_items), 0L)
  expect_false(any(grepl("remediation", names(result$tables$records), fixed = TRUE)))
  expect_true(any(result$tables$field_metadata$privacy_class == "WITHHOLD"))
  expect_equal(result$qa$metrics$value[result$qa$metrics$metric == "metadata_main_columns_missing"], 0L)
})

test_that("wrapper keeps Epi and BCAP outputs separate and permits zero links", {
  root <- tempfile("phase3-wrapper-")
  dir.create(root)
  epi_dir <- phase3_make_epi_safe(file.path(root, "epi"), c("CASE-000001"), c("PREMISES-000001"))
  bcap_dir <- phase3_make_bcap_safe(file.path(root, "bcap"), c("BCAP-CASE-000001"), c("PREMISES-000001"))
  epi_xwalk <- phase3_make_crosswalk(file.path(root, "epi-xwalk"), "epi", "CASE-000001", "epi-only", "PREMISES-000001")
  bcap_xwalk <- phase3_make_crosswalk(file.path(root, "bcap-xwalk"), "bcap", "BCAP-CASE-000001", "bcap-only", "PREMISES-000001")
  result <- collate_hpai(epi_dir, bcap_dir, file.path(root, "analysis"), epi_xwalk, bcap_xwalk, run_id = "synthetic", quiet = TRUE)
  expect_identical(result$status, "passed")
  expect_true(all(c("epi", "bcap", "cross_reference") %in% names(result)))
  expect_equal(result$cross_reference$authoritative_links, 0L)
  expect_false("tables" %in% names(result))
  expect_true(file.exists(file.path(root, "analysis", "epi", "records.csv")))
  expect_true(file.exists(file.path(root, "analysis", "bcap", "records.csv")))
  expect_true(file.exists(file.path(root, "analysis", "metadata", "epi_field_metadata.csv")))
  expect_true(file.exists(file.path(root, "analysis", "qa", "cross_reference.csv")))
})

test_that("authoritative cross-reference returns only explicit shared keys", {
  root <- tempfile("phase3-linkage-")
  dir.create(root)
  epi_dir <- phase3_make_epi_safe(file.path(root, "epi"), c("CASE-000001"), c("PREMISES-000001"))
  bcap_dir <- phase3_make_bcap_safe(file.path(root, "bcap"), c("BCAP-CASE-000001"), c("PREMISES-000001"))
  epi_xwalk <- phase3_make_crosswalk(file.path(root, "epi-xwalk"), "epi", "CASE-000001", "shared-key", "PREMISES-000001")
  bcap_xwalk <- phase3_make_crosswalk(file.path(root, "bcap-xwalk"), "bcap", "BCAP-CASE-000001", "shared-key", "PREMISES-000001")
  result <- cross_reference_hpai(epi_dir, bcap_dir, epi_xwalk, bcap_xwalk)
  expect_identical(result$status, "passed")
  expect_equal(result$authoritative_links, 1L)
  expect_identical(result$linkage$link_basis, "authoritative_crosswalk_premises_key")
  expect_identical(result$linkage$link_confidence, "exact/authoritative")
})

test_that("Phase 3 fails closed on schema and type drift", {
  root <- tempfile("phase3-drift-")
  dir.create(root)
  epi_dir <- phase3_make_epi_safe(file.path(root, "epi"), c("CASE-000001"), c("PREMISES-000001"))
  forms <- readr::read_csv(file.path(epi_dir, "collated", "epi_forms.csv"), show_col_types = FALSE)
  forms$unexpected_source_field <- "drift"
  readr::write_csv(forms, file.path(epi_dir, "collated", "epi_forms.csv"))
  expect_error(collate_epi_analysis(epi_dir, file.path(root, "analysis"), quiet = TRUE), "unexpected columns")
  forms$unexpected_source_field <- NULL
  forms$today_date <- "not-a-date"
  readr::write_csv(forms, file.path(epi_dir, "collated", "epi_forms.csv"))
  expect_error(collate_epi_analysis(epi_dir, file.path(root, "analysis-2"), quiet = TRUE), "unparseable date")
})

test_that("duplicate canonical record keys are blocking", {
  root <- tempfile("phase3-keys-")
  dir.create(root)
  epi_dir <- phase3_make_epi_safe(file.path(root, "epi"), c("CASE-000001", "CASE-000001"), c("PREMISES-000001", "PREMISES-000001"))
  expect_error(collate_epi_analysis(epi_dir, file.path(root, "analysis"), quiet = TRUE), "duplicate or missing record keys")
})

test_that("repeated child rows retain stable item keys and typed values", {
  root <- tempfile("phase3-repeated-")
  dir.create(root)
  epi_dir <- phase3_make_epi_safe(file.path(root, "epi"), c("CASE-000001"), c("PREMISES-000001"))
  epi <- bcapture:::.phase3_epi_read(epi_dir)
  expected <- bcapture:::.phase3_epi_expected("2024-05-28")
  item <- as.data.frame(setNames(lapply(expected$tables$epi_ai_tests, function(x) NA_character_), expected$tables$epi_ai_tests), stringsAsFactors = FALSE)
  item$row_index <- "1"
  item$row_label <- "synthetic"
  item$raw_fields <- "synthetic"
  item$source_pages <- "1"
  item$date_raw <- "2025-01-02"
  item$date <- "2025-01-02"
  item$result_raw <- "positive"
  item$result <- "positive"
  item$test_type_raw <- "PCR"
  item$test_type <- "PCR"
  item$case_id <- "CASE-000001"
  epi$tables$epi_ai_tests <- item
  items <- bcapture:::.phase3_normalize_epi_repeated(epi)
  expect_true(any(items$field_name == "date" & items$value_type == "date" & !is.na(items$value_date)))
  expect_true(any(items$item_id == "CASE-000001::epi_ai_tests::000001"))
  expect_false(any(c("entity_id", "event_id") %in% names(items)))
})

test_that("separate product tables are deterministic across reruns", {
  root <- tempfile("phase3-determinism-")
  dir.create(root)
  epi_dir <- phase3_make_epi_safe(file.path(root, "epi"), c("CASE-000001"), c("PREMISES-000001"))
  first <- collate_epi_analysis(epi_dir, file.path(root, "analysis-1"), run_id = "fixed", quiet = TRUE)
  second <- collate_epi_analysis(epi_dir, file.path(root, "analysis-2"), run_id = "fixed", quiet = TRUE)
  for (name in c("records", "repeated_items", "provenance", "field_metadata")) expect_identical(first$tables[[name]], second$tables[[name]])
  expect_identical(first$output_hashes[, c("product", "format", "sha256", "row_count", "column_count")], second$output_hashes[, c("product", "format", "sha256", "row_count", "column_count")])
})
