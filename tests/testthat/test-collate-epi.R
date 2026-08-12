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
  expect_equal(sum(dictionary$fields$data_type == "date_text"), 23L)
  expect_true(all(dictionary$fields$data_type[dictionary$fields$raw_field %in% c("p0100a", "p0104a", "p0133a", "p0150a")] == "date_text"))
  expect_true(all(dictionary$fields$data_type[dictionary$fields$raw_field %in% c("p0001", "p0007a", "p0151c", "p0167", "p0192a")] == "date"))
  expect_equal(bcapture:::validate_epi_dictionary(dictionary)$mapped_fields, 497)
})

test_that("collate_epi preserves codebooks, provenance, and repeated records", {
  values <- list(synthetic_1 = list(
    premid = "SYNTHETIC-PREMISES-001", p0001 = "05/28/2024", p0005 = "12", p0019 = "/1", p0308 = "/4",
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
  expect_s3_class(result$responses$date_value, "Date")
  expect_true(is.double(result$responses$numeric_value))
  expect_equal(result$responses$date_value[result$responses$raw_field == "p0001"], as.Date("2024-05-28"))
  expect_equal(result$responses$numeric_value[result$responses$raw_field == "p0005"], 12)
  expect_true(is.character(result$forms$today_date))
  expect_true(is.character(result$forms$baseline_mortality))
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

test_that("scalar date parsing distinguishes two- and four-digit years", {
  two_digit <- c("01/15/25", "2/11/25", "03/03/25", "03/04/25")
  four_digit <- c("01/15/2025", "1/7/2025", "2/25/2025")

  expect_equal(
    bcapture:::.epi_parse_date_vector(two_digit),
    as.Date(c("2025-01-15", "2025-02-11", "2025-03-03", "2025-03-04"))
  )
  expect_equal(
    bcapture:::.epi_parse_date_vector(four_digit),
    as.Date(c("2025-01-15", "2025-01-07", "2025-02-25"))
  )
  expect_true(all(as.integer(format(bcapture:::.epi_parse_date_vector(two_digit), "%Y")) >= 2000L))
  expect_true(is.na(bcapture:::.epi_parse_date("")))
  expect_true(is.na(bcapture:::.epi_parse_date("not a date")))
})

test_that("repeated-table dates remain Date across valid, blank, and invalid rows", {
  dictionary <- bcapture:::load_epi_dictionary()
  contract <- bcapture:::.epi_table_type_contract("ai_tests", dictionary)
  records <- tibble::as_tibble(stats::setNames(lapply(unname(contract), function(type) {
    if (identical(type, "integer")) integer(3) else character(3)
  }), names(contract)))
  records$form_id <- rep("synthetic", 3)
  records$row_index <- 1:3
  records$date_raw <- c("01/10/2025", "", "bad date")
  records$date <- NA_character_
  records$date_raw_field <- c("p0007a", "p0008a", "p0009a")
  cast <- bcapture:::.epi_cast_repeated_table(records, "ai_tests", dictionary)
  expect_s3_class(cast$data$date, "Date")
  expect_equal(cast$data$date, as.Date(c("2025-01-10", NA, NA)))
  expect_equal(nrow(cast$diagnostics), 1L)
  expect_equal(cast$diagnostics$raw_field, "p0009a")
})

test_that("date-expression fields preserve text without scalar parsing diagnostics", {
  expressions <- c("Daily", "End on 1/15", "1/10/25, 1/12/25", "1/10/25-1/15/25")
  out_dir <- .synthetic_epi_output(list(synthetic_1 = list(
    p0100a = "Daily", p0133a = expressions[[1L]], p0134a = expressions[[2L]],
    p0135a = expressions[[3L]], p0136a = expressions[[4L]]
  )))
  result <- collate_epi(out_dir, quiet = TRUE)

  expect_true(is.character(result$mortality_disposal$dates))
  expect_equal(result$mortality_disposal$dates, "Daily")
  expect_true(is.character(result$visitors$visit_dates))
  expect_setequal(result$visitors$visit_dates, expressions)
  expect_false(any(result$parse_diagnostics$raw_field %in% c("p0100a", "p0133a", "p0134a", "p0135a", "p0136a")))
})

test_that("repeated-table numerics remain double across valid, blank, and invalid rows", {
  dictionary <- bcapture:::load_epi_dictionary()
  contract <- bcapture:::.epi_table_type_contract("houses", dictionary)
  records <- tibble::as_tibble(stats::setNames(lapply(unname(contract), function(type) {
    if (identical(type, "integer")) integer(3) else character(3)
  }), names(contract)))
  records$form_id <- rep("synthetic", 3)
  records$row_index <- 1:3
  records$birds_today_raw <- c("15", "", "malformed")
  records$birds_today <- NA_character_
  records$birds_today_raw_field <- c("p00010c", "p00011c", "p00012c")
  cast <- bcapture:::.epi_cast_repeated_table(records, "houses", dictionary)
  expect_true(is.double(cast$data$birds_today))
  expect_equal(cast$data$birds_today, c(15, NA_real_, NA_real_))
  expect_equal(nrow(cast$diagnostics), 1L)
  expect_equal(cast$diagnostics$parse_type, "numeric")
})

test_that("all date-bearing repeated tables use a stable Date column", {
  dictionary <- bcapture:::load_epi_dictionary()
  date_columns <- c(
    ai_tests = "date", houses = "clinical_onset_date", manure_destinations = "date",
    imported_materials = "date", worker_visits = "date", crews = "date",
    shared_equipment = "date_last_on_site", bird_movements = "date", egg_movements = "date"
  )
  for (table_name in names(date_columns)) {
    contract <- bcapture:::.epi_table_type_contract(table_name, dictionary)
    records <- tibble::as_tibble(stats::setNames(lapply(unname(contract), function(type) {
      if (identical(type, "integer")) 1L else character(1)
    }), names(contract)))
    column <- date_columns[[table_name]]
    raw_field <- dictionary$fields$raw_field[dictionary$fields$table_name == table_name & dictionary$fields$column_name == column][[1L]]
    records[[paste0(column, "_raw")]] <- "bad date"
    records[[column]] <- NA_character_
    records[[paste0(column, "_raw_field")]] <- raw_field
    cast <- bcapture:::.epi_cast_repeated_table(records, table_name, dictionary)
    expect_true(inherits(cast$data[[column]], "Date"), info = table_name)
    expect_equal(nrow(cast$diagnostics), 1L, info = table_name)
  }
})

test_that("every repeated table row has exact dictionary provenance", {
  out_dir <- .synthetic_epi_output(list(synthetic_1 = list(
    p0007a = "01/15/25", p00010a = "House 1", p0100a = "Daily",
    p0109 = "Destination", p0116 = "Imported product", p0123 = "Premises",
    p0128 = "01/14/25", p0133 = "/1", p0151a = "/1", p0167 = "01/13/25",
    p0182 = "Egg source"
  )))
  result <- collate_epi(out_dir, quiet = TRUE)
  expected <- c(
    ai_tests = "p0007a|p0007b|p0007c",
    houses = "p00010a|p00010b|p00010c|p00010d|p00010e|p00010f|p00010g|p00010h",
    mortality_disposal = "p0100a|p0100b|p0100c|p0100na|p0100off|p0100on|p0100oth",
    manure_destinations = "p0109|p0109a|p0109b|p0109c|p0109d",
    imported_materials = "p0116|p0116a|p0116b",
    worker_visits = "p0123|p0123a|p0123b",
    crews = "p0128|p0128a|p0128b",
    visitors = "p0133|p0133a|p0133b|p0133c|p0133d",
    shared_equipment = "p0151a|p0151b|p0151c|p0151d",
    bird_movements = "p0167|p0167a|p0167b|p0167c",
    egg_movements = "p0182|p0182a|p0182b"
  )
  expected_pages <- c(
    ai_tests = "2", houses = "2", mortality_disposal = "3", manure_destinations = "4",
    imported_materials = "4", worker_visits = "5", crews = "5", visitors = "6",
    shared_equipment = "7", bird_movements = "8", egg_movements = "9"
  )
  valid_raw_fields <- bcapture:::load_epi_dictionary()$fields$raw_field

  for (table_name in names(expected)) {
    table <- result[[table_name]]
    expect_equal(nrow(table), 1L, info = table_name)
    expect_equal(table$raw_fields, expected[[table_name]], info = table_name)
    expect_equal(table$source_pages, expected_pages[[table_name]], info = table_name)
    expect_true(!is.na(table$raw_fields) && nzchar(table$raw_fields), info = table_name)
    expect_true(all(strsplit(table$raw_fields, "|", fixed = TRUE)[[1L]] %in% valid_raw_fields), info = table_name)
  }
})
