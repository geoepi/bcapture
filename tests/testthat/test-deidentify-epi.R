.deid_synthetic_output <- function(values = list(), schemas = c("schema_a")) {
  dictionary <- bcapture:::load_epi_dictionary()
  out_dir <- tempfile("epi-deid-source-")
  dir.create(file.path(out_dir, "combined"), recursive = TRUE)
  form_ids <- paste0("synthetic_", seq_along(schemas))
  field_rows <- purrr::map2_dfr(form_ids, schemas, function(form_id, schema) {
    fields <- dictionary$fields |>
      dplyr::transmute(
        form_id = form_id, form_type = "initial_epi", source_file = paste0(form_id, ".pdf"),
        source_relpath = paste0(form_id, ".pdf"), source_sha256 = paste0("sha-", form_id),
        field = raw_field, alternative_name, field_type = ifelse(response_type %in% c("coded", "choice"), "Btn", "Tx"),
        field_flags = NA_integer_, value_raw = NA_character_, value = NA_character_,
        default_value_raw = NA_character_, default_value = NA_character_, is_default_value = FALSE,
        states = NA_character_, options = NA_character_, is_multiselect = response_type == "multiselect",
        is_populated = FALSE, extraction_method = "synthetic", page = as.integer(source_page),
        form_schema_hash = schema, schema_group = schema
      )
    supplied <- values[[form_id]]
    if (is.null(supplied)) supplied <- list()
    for (raw_field in names(supplied)) {
      index <- match(raw_field, fields$field)
      if (is.na(index)) next
      fields$value_raw[[index]] <- as.character(supplied[[raw_field]])
      fields$value[[index]] <- sub("^/", "", as.character(supplied[[raw_field]]))
      fields$is_populated[[index]] <- TRUE
    }
    fields
  })
  manifest <- purrr::map2_dfr(form_ids, schemas, function(form_id, schema) tibble::tibble(
    form_id = form_id, form_type = "initial_epi", source_file = paste0(form_id, ".pdf"),
    source_relpath = paste0(form_id, ".pdf"), source_sha256 = paste0("sha-", form_id), status = "success",
    failure_type = NA_character_, error = NA_character_, number_of_pages = 12L,
    number_of_fields = sum(field_rows$form_id == form_id), number_of_widgets = 0L,
    number_of_populated_fields = sum(field_rows$form_id == form_id & field_rows$is_populated),
    form_schema_hash = schema, schema_group = schema, extraction_method = "synthetic",
    pypdf_version = NA_character_, extracted_at_utc = "2024-05-28T00:00:00Z"
  ))
  readr::write_csv(field_rows, file.path(out_dir, "combined", "epi_fields_long.csv"), na = "")
  readr::write_csv(dplyr::select(manifest, form_id, form_type, source_file, source_relpath, source_sha256, form_schema_hash, schema_group), file.path(out_dir, "combined", "epi_metadata.csv"), na = "")
  readr::write_csv(manifest, file.path(out_dir, "extraction_manifest.csv"), na = "")
  bcapture::collate_epi(out_dir, quiet = TRUE)
  out_dir
}

.deid_values <- function(owner = "Owner Person", premises = "Smith Farms") list(
  premid = "SYNTHETIC-PREMISES-001", premname = premises, premadd = "123 Main Street",
  premcnty = "Example County", ownname = owner, ownph = "555-222-3333", owneml = "owner@example.org",
  premlat = "30.123", premlong = "-84.123", p0004 = "Owner Person is mentioned in notes.",
  p0001 = "05/28/2024", p00010a = "House 1", p00010c = "100", p00010e = "10", p00010g = "05/20/2024",
  p00010h = "House 1 onset", p0133b = "Visitor Company / 555-222-3333", p0179oth = "Example Company Crew",
  weename = "Interviewer Person"
)

test_that("the analysis privacy policy covers every logical field", {
  dictionary <- bcapture:::load_epi_dictionary()
  rules <- bcapture:::.epi_deid_load_rules("2024-05-28", "analysis", dictionary)
  expect_equal(nrow(rules), 497L)
  expect_equal(length(unique(rules$raw_field)), 497L)
  expect_setequal(rules$raw_field, dictionary$fields$raw_field)
  expect_true(all(rules$action %in% c("retain", "drop", "pseudonymize", "coarsen", "review_remove")))
  expect_true(all(rules$privacy_class %in% c("direct_identifier", "quasi_identifier", "sensitive_free_text", "provenance_link", "non_identifier")))
  expect_true(all(c("dictionary_version", "schema_group", "raw_field", "canonical_name", "row_index") %in% bcapture:::.epi_deid_allowed_metadata))
})

test_that("deidentify_epi protects direct identifiers and preserves analytical types", {
  source_dir <- .deid_synthetic_output(list(synthetic_1 = .deid_values()))
  deid_dir <- tempfile("epi-deid-output-")
  crosswalk_dir <- tempfile("epi-deid-crosswalk-")
  source_forms <- readr::read_csv(file.path(source_dir, "collated", "epi_forms.csv"), show_col_types = FALSE)
  result <- bcapture::deidentify_epi(source_dir, deid_dir, crosswalk_dir, quiet = TRUE)
  expect_equal(result$status, "passed")
  expect_false(any(c("record_crosswalk", "entity_crosswalk", "crosswalk") %in% names(result)))
  output_forms <- readr::read_csv(file.path(deid_dir, "collated", "epi_forms.csv"), show_col_types = FALSE)
  output_responses <- readr::read_csv(file.path(deid_dir, "collated", "epi_responses_long.csv"), show_col_types = FALSE)
  output_houses <- readr::read_csv(file.path(deid_dir, "collated", "epi_houses.csv"), show_col_types = FALSE)
  expect_true(all(grepl("^CASE-[0-9]{6}$", output_forms$case_id)))
  expect_false("form_id" %in% names(output_forms))
  expect_false(any(c("source_file", "source_relpath", "source_sha256") %in% names(output_forms)))
  expect_equal(output_forms$premises_id, "PREMISES-000001")
  expect_equal(output_forms$premises_name, "PREMISES-000001")
  expect_true(is.na(output_forms$premises_address[[1L]]))
  expect_true(is.na(output_forms$premises_latitude[[1L]]))
  expect_true(is.na(output_forms$premises_longitude[[1L]]))
  expect_equal(output_forms$premises_county, "Example County")
  expect_true(is.na(output_forms$premises_owner_phone[[1L]]))
  expect_true(is.na(output_forms$premises_owner_email[[1L]]))
  expect_true(is.numeric(output_houses$birds_today))
  expect_true(inherits(output_houses$clinical_onset_date, "Date"))
  expect_equal(output_houses$clinical_onset_location, "LOCATION-000001")
  expect_equal(output_forms$company_crew_name, "ORG-000001")
  expect_equal(output_forms$interviewee_name, "PERSON-000001")
  expect_true(any(output_responses$raw_field == "p0004" & is.na(output_responses$raw_value)))
  expect_true(any(output_responses$raw_field == "premname" & grepl("^PREMISES-", output_responses$value)))
  expect_false(any(output_responses$raw_value == "Owner Person", na.rm = TRUE))
  expect_true(file.exists(file.path(deid_dir, "privacy", "deidentification_review.csv")))
  expect_true(nrow(readr::read_csv(file.path(deid_dir, "privacy", "deidentification_review.csv"), show_col_types = FALSE)) > 0L)
  expect_true(file.exists(file.path(crosswalk_dir, "record_crosswalk.csv")))
  expect_true(file.exists(file.path(crosswalk_dir, "entity_crosswalk.csv")))
  expect_false(dir.exists(file.path(deid_dir, "crosswalk")))
  expect_equal(source_forms, readr::read_csv(file.path(source_dir, "collated", "epi_forms.csv"), show_col_types = FALSE))
})

test_that("crosswalk reuse is stable and extension does not renumber", {
  crosswalk_dir <- tempfile("epi-deid-reuse-crosswalk-")
  source_one <- .deid_synthetic_output(list(synthetic_1 = .deid_values()))
  first_dir <- tempfile("epi-deid-reuse-one-")
  first <- bcapture::deidentify_epi(source_one, first_dir, crosswalk_dir, quiet = TRUE)
  first_forms <- readr::read_csv(file.path(first_dir, "collated", "epi_forms.csv"), show_col_types = FALSE)
  second_values <- .deid_values(owner = "Second Owner", premises = "Second Farm")
  second_values$premid <- "SYNTHETIC-PREMISES-002"
  source_two <- .deid_synthetic_output(list(synthetic_1 = .deid_values(), synthetic_2 = second_values), schemas = c("schema_a", "schema_a"))
  second_dir <- tempfile("epi-deid-reuse-two-")
  second <- bcapture::deidentify_epi(source_two, second_dir, crosswalk_dir, quiet = TRUE)
  second_forms <- readr::read_csv(file.path(second_dir, "collated", "epi_forms.csv"), show_col_types = FALSE)
  expect_equal(second_forms$case_id[[1L]], first_forms$case_id[[1L]])
  expect_equal(second_forms$premises_id[[1L]], first_forms$premises_id[[1L]])
  expect_equal(second_forms$case_id[[2L]], "CASE-000002")
  expect_equal(second_forms$premises_id[[2L]], "PREMISES-000002")
  record_crosswalk <- readr::read_csv(file.path(crosswalk_dir, "record_crosswalk.csv"), show_col_types = FALSE)
  expect_equal(nrow(record_crosswalk), 2L)
  expect_true(all(grepl("^CASE-[0-9]{6}$", record_crosswalk$case_id)))
  expect_equal(first$status, "passed")
  expect_equal(second$status, "passed")
})

test_that("path safety rejects nested and version-controlled crosswalks", {
  source_dir <- tempfile("epi-deid-path-source-"); dir.create(source_dir, recursive = TRUE)
  deid_dir <- tempfile("epi-deid-path-output-")
  expect_error(bcapture::deidentify_epi(source_dir, deid_dir, file.path(deid_dir, "crosswalk")), "physically separate")
  git_crosswalk <- file.path(getwd(), "private-crosswalk-test")
  if (is.na(bcapture:::.epi_deid_git_root(normalizePath(git_crosswalk, winslash = "/", mustWork = FALSE)))) skip("Git worktree is not present in the installed package check copy")
  expect_error(bcapture::deidentify_epi(source_dir, deid_dir, git_crosswalk), "Git working tree")
})

test_that("known source values are confirmed leaks and pseudonymized output is clean", {
  leaked <- bcapture:::.epi_deid_scan(list(epi_forms = tibble::tibble(case_id = "CASE-000001", premises_name = "Secret Farm")), "Secret Farm")
  clean <- bcapture:::.epi_deid_scan(list(epi_forms = tibble::tibble(case_id = "CASE-000001", premises_name = "PREMISES-000001")), "Secret Farm")
  expect_true(any(leaked$severity == "ERROR"))
  expect_equal(nrow(clean), 0L)
})
