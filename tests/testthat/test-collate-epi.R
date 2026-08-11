.synthetic_epi_output <- function(values = list(), schemas = c("schema_a"), unknown = FALSE) {
  dictionary <- bcapture:::load_epi_dictionary()
  out_dir <- tempfile("epi-collate-")
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
        states = NA_character_, options = NA_character_, is_multiselect = response_type == "multiselect" |
          raw_field %in% c("p0109c", "p0110c", "p0111c") |
          grepl("^p(013[3-9]|014[0-9]|015[0-9]|016[0-5])d$", raw_field),
        is_populated = FALSE, extraction_method = "synthetic", page = as.integer(source_page),
        form_schema_hash = schema, schema_group = schema
      )
    supplied <- values[[form_id]] %||% list()
    for (raw_field in names(supplied)) {
      index <- match(raw_field, fields$field)
      if (is.na(index)) next
      raw_value <- as.character(supplied[[raw_field]])
      fields$value_raw[[index]] <- raw_value
      fields$value[[index]] <- sub("^/", "", raw_value)
      fields$is_populated[[index]] <- TRUE
    }
    if (isTRUE(unknown) && identical(form_id, form_ids[[1L]])) {
      fields <- dplyr::bind_rows(fields, tibble::tibble(
        form_id = form_id, form_type = "initial_epi", source_file = paste0(form_id, ".pdf"), source_relpath = paste0(form_id, ".pdf"),
        source_sha256 = paste0("sha-", form_id), field = "p9999", alternative_name = NA_character_, field_type = "Tx",
        field_flags = NA_integer_, value_raw = "synthetic unknown", value = "synthetic unknown", default_value_raw = NA_character_,
        default_value = NA_character_, is_default_value = FALSE, states = NA_character_, options = NA_character_,
        is_multiselect = FALSE, is_populated = TRUE, extraction_method = "synthetic", page = 12L,
        form_schema_hash = schema, schema_group = schema
      ))
    }
    fields
  })
  manifest <- purrr::map2_dfr(form_ids, schemas, ~ tibble::tibble(
    form_id = .x, form_type = "initial_epi", source_file = paste0(.x, ".pdf"), source_relpath = paste0(.x, ".pdf"),
    source_sha256 = paste0("sha-", .x), status = "success", failure_type = NA_character_, error = NA_character_,
    number_of_pages = 12L, number_of_fields = sum(field_rows$form_id == .x), number_of_widgets = 0L,
    number_of_populated_fields = sum(field_rows$form_id == .x & field_rows$is_populated), form_schema_hash = .y,
    schema_group = .y, extraction_method = "synthetic", pypdf_version = NA_character_, extracted_at_utc = "2024-05-28T00:00:00Z"
  ))
  metadata <- manifest |>
    dplyr::select(form_id, form_type, source_file, source_relpath, source_sha256, form_schema_hash, schema_group)
  readr::write_csv(field_rows, file.path(out_dir, "combined", "epi_fields_long.csv"), na = "")
  readr::write_csv(metadata, file.path(out_dir, "combined", "epi_metadata.csv"), na = "")
  readr::write_csv(manifest, file.path(out_dir, "extraction_manifest.csv"), na = "")
  out_dir
}

test_that("the Initial Epi dictionary is complete and versioned", {
  dictionary <- bcapture:::load_epi_dictionary()
  expect_equal(nrow(dictionary$fields), 497)
  expect_equal(length(unique(dictionary$fields$raw_field)), 497)
  expect_equal(sum(!is.na(dictionary$fields$table_name)), 386)
  expect_equal(nrow(dictionary$codes), 27)
  expect_equal(nrow(dictionary$tables), 11)
  expect_equal(bcapture:::validate_epi_dictionary(dictionary)$mapped_fields, 497)
})

test_that("collate_epi preserves codebooks, provenance, and repeated records", {
  values <- list(synthetic_1 = list(
    premid = "SYNTHETIC-PREMISES-001", p0001 = "05/28/2024", p0019 = "/1", p0308 = "/4",
    p0303 = "/2", p0315 = "/1", p0109c = "Poultry|Swine", p00010a = "House 1", p00010c = "100",
    p00010e = "10", p0109 = "Synthetic destination", p0109a = "05/27/2024", p0109d = "/2",
    p0133 = "/1", p0133a = "05/26/2024", p0133b = "Synthetic visitor", p0133c = "/3",
    p0151a = "/1", p0151b = "Synthetic equipment", p0151c = "05/25/2024", p0167 = "05/24/2024",
    p0167a = "Synthetic birds", p0167b = "Synthetic source", p0167c = "Synthetic transporter",
    p0182 = "Synthetic egg source", p0182a = "05/23/2024", p0182b = "/1"
  ))
  out_dir <- .synthetic_epi_output(values)
  result <- collate_epi(out_dir, quiet = TRUE)
  expect_equal(nrow(result$coverage), 497)
  expect_equal(sum(result$coverage$mapped), 497)
  expect_equal(result$manifest$populated_unmapped_fields, 0)
  expect_equal(result$responses$response_label[result$responses$raw_field == "p0019"], "Yes")
  expect_equal(result$responses$response_label[result$responses$raw_field == "p0308"], "Don't know")
  expect_equal(result$responses$response_label[result$responses$raw_field == "p0303"], "Tens")
  expect_equal(result$responses$response_label[result$responses$raw_field == "p0315"], "Often")
  expect_equal(result$responses$raw_value[result$responses$raw_field == "p0019"], "/1")
  expect_setequal(result$multiselect$item_label[result$multiselect$raw_field == "p0109c"], c("Poultry", "Swine"))
  expect_equal(nrow(result$houses), 1)
  expect_equal(result$houses$birds_today, 100)
  expect_equal(nrow(result$manure_destinations), 1)
  expect_equal(result$manure_destinations$type, "Composted")
  expect_equal(nrow(result$visitors), 1)
  expect_equal(nrow(result$shared_equipment), 1)
  expect_equal(nrow(result$bird_movements), 1)
  expect_equal(result$bird_movements$direction, "onto")
  expect_equal(nrow(result$egg_movements), 1)
  expect_equal(result$egg_movements$material_type, "eggs")
  expect_true(file.exists(file.path(out_dir, "collated", "epi_responses_long.csv")))
  expect_true(file.exists(file.path(out_dir, "collated", "collation_diagnostics.md")))
})

test_that("blank repeated rows are omitted and schema hashes are provenance only", {
  out_dir <- .synthetic_epi_output(
    values = list(synthetic_1 = list(premid = "ONE"), synthetic_2 = list(premid = "TWO")),
    schemas = c("schema_a", "schema_b")
  )
  result <- collate_epi(out_dir, quiet = TRUE)
  expect_equal(nrow(result$forms), 2)
  expect_equal(sort(unique(result$forms$form_schema_hash)), c("schema_a", "schema_b"))
  expect_equal(nrow(result$houses), 0)
  expect_equal(nrow(result$visitors), 0)
  expect_equal(sum(result$manifest$populated_unmapped_fields), 0)
})

test_that("strict and non-strict modes report unknown fields", {
  out_dir <- .synthetic_epi_output(values = list(synthetic_1 = list(premid = "ONE")), unknown = TRUE)
  expect_error(collate_epi(out_dir, quiet = TRUE), "p9999")
  result <- collate_epi(out_dir, strict = FALSE, quiet = TRUE)
  expect_equal(result$manifest$unknown_fields, 1)
  expect_equal(result$manifest$populated_unmapped_fields, 1)
  expect_true(any(result$coverage$raw_field == "p9999" & !result$coverage$mapped))
})
