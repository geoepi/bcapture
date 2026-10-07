hpai_test_products <- function(root, invalid_date = FALSE) {
  dir.create(file.path(root, "combined"), recursive = TRUE)
  fields <- tibble::tibble(
    audit_id = rep("synthetic-a", 7L),
    source_file = rep("synthetic-a.pdf", 7L),
    source_relpath = rep("synthetic-a.pdf", 7L),
    source_md5 = rep("synthetic-md5", 7L),
    field_index = seq_len(7L),
    page = rep(1L, 7L),
    field = c("safe_button", "npin", "premisesfarm_name", "primary_contact_email",
              "Q1_1_comments", "initial_audit_date", "Auditor_sig_date"),
    alternative_name = NA_character_,
    field_type = c("Btn", "Tx", "Tx", "Tx", "Tx", "Tx", "Sig"),
    field_flags = NA_integer_,
    value_raw = c("Yes", "NPIN-SYNTHETIC", "Synthetic Farm", "synthetic@example.test",
                  "Synthetic narrative", if (invalid_date) "not-a-date" else "1/2/2025",
                  "signature_present"),
    value = c("Yes", "NPIN-SYNTHETIC", "Synthetic Farm", "synthetic@example.test",
              "Synthetic narrative", if (invalid_date) "not-a-date" else "1/2/2025",
              "signature_present"),
    default_value_raw = NA_character_, default_value = NA_character_,
    is_default_value = FALSE, states = c("Yes|No", rep(NA_character_, 6L)),
    options = rep(NA_character_, 7L), is_multiselect = FALSE,
    is_populated = TRUE, extraction_method = "acroform"
  )
  manifest <- tibble::tibble(
    audit_id = "synthetic-a", form_type = "bcap", source_file = "synthetic-a.pdf",
    source_relpath = "synthetic-a.pdf", source_md5 = "synthetic-md5", status = "success",
    failure_type = NA_character_, schema_group = "schema_001",
    extraction_method = "acroform", template_family = NA_character_,
    template_version = NA_character_, number_of_fields = 7L
  )
  readr::write_csv(fields, file.path(root, "combined", "hpai_fields_long.csv"))
  readr::write_csv(manifest, file.path(root, "extraction_manifest.csv"))
  invisible(root)
}

test_that("BCAP policy is complete for the canonical template", {
  policy <- bcapture:::.hpai_schema_inventory(bcapture:::.hpai_template_fields())
  expect_equal(nrow(policy), 225L)
  expect_false(any(policy$privacy_class == "REVIEW"))
  expect_equal(sum(policy$privacy_class == "SAFE_RETAIN"), 160L)
  expect_equal(sum(policy$privacy_class == "SAFE_NORMALIZE"), 3L)
  expect_equal(sum(policy$privacy_class == "PSEUDONYMIZE"), 15L)
  expect_equal(sum(policy$privacy_class == "WITHHOLD"), 47L)
})

test_that("BCAP semantic validation and strict finalization pass synthetic records", {
  root <- tempfile("bcap-hpai-")
  hpai_test_products(root)
  validation <- bcapture::validate_hpai(root, write = FALSE, quiet = TRUE)
  expect_identical(validation$status, "passed")
  expect_equal(validation$summary$error_count, 0L)

  safe <- tempfile("bcap-safe-")
  crosswalk <- tempfile("bcap-crosswalk-")
  result <- bcapture::deidentify_hpai(root, safe, crosswalk, quiet = TRUE)
  expect_identical(result$status, "passed")
  expect_true(dir.exists(safe))
  expect_true(dir.exists(crosswalk))
  privacy <- bcapture::validate_hpai_privacy(safe, crosswalk, write = FALSE, quiet = TRUE)
  expect_identical(privacy$status, "passed")
  safe_fields <- readr::read_csv(file.path(safe, "combined", "hpai_fields_long.csv"), show_col_types = FALSE)
  expect_false(any(safe_fields$value == "NPIN-SYNTHETIC", na.rm = TRUE))
  expect_false(any(safe_fields$value == "synthetic@example.test", na.rm = TRUE))
  expect_equal(safe_fields$value[safe_fields$field == "initial_audit_date"], "2025-01-02")
  expect_equal(safe_fields$value[safe_fields$field == "Auditor_sig_date"], "signature_present")
  expect_true(all(is.na(safe_fields$value[safe_fields$field == "Q1_1_comments"])))
  expect_true(file.exists(file.path(crosswalk, "record_crosswalk.csv")))
  expect_true(file.exists(file.path(crosswalk, "entity_crosswalk.csv")))

  safe_reuse <- tempfile("bcap-safe-reuse-")
  reused <- bcapture::deidentify_hpai(root, safe_reuse, crosswalk, overwrite = TRUE, quiet = TRUE)
  expect_identical(reused$status, "passed")
  safe_reuse_fields <- readr::read_csv(file.path(safe_reuse, "combined", "hpai_fields_long.csv"), show_col_types = FALSE)
  expect_identical(
    safe_fields$value[safe_fields$field == "npin"],
    safe_reuse_fields$value[safe_reuse_fields$field == "npin"]
  )
})

test_that("BCAP semantic gate blocks unparseable dates before output", {
  root <- tempfile("bcap-hpai-invalid-")
  hpai_test_products(root, invalid_date = TRUE)
  validation <- bcapture::validate_hpai(root, write = FALSE, quiet = TRUE)
  expect_identical(validation$status, "failed")
  expect_true(any(validation$findings$issue_type == "unparseable_date"))
  expect_error(
    bcapture::deidentify_hpai(root, tempfile("bcap-safe-blocked-"), tempfile("bcap-cross-blocked-"), quiet = TRUE),
    "semantic validation"
  )
})

test_that("BCAP privacy scanner is field-aware for numeric collisions", {
  safe_fields <- tibble::tibble(
    field = "safe_control", field_type = "Btn", value = "1234567890",
    privacy_action = "SAFE_RETAIN", identifier_class = "audit_control"
  )
  entities <- tibble::tibble(
    pseudonym = "CONTACT-000001", entity_type = "CONTACT",
    original_value = "1234567890", normalized_value = "1234567890",
    first_record_id = "BCAP-CASE-000001", first_raw_field = "primary_contact_phone",
    created_at = "2025-01-01T00:00:00Z"
  )
  findings <- bcapture:::.hpai_privacy_scan(safe_fields, entities)
  expect_equal(nrow(findings), 0L)
  leaked <- safe_fields
  leaked$field <- "primary_contact_phone"
  leaked$privacy_action <- "PSEUDONYMIZE"
  leaked$identifier_class <- "direct_identifier"
  leaked_findings <- bcapture:::.hpai_privacy_scan(leaked, entities)
  expect_true(any(leaked_findings$leak_type == "known_source_value"))
})
