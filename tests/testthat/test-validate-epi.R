.synthetic_validation_output <- function(values = list(), values_by_form = NULL) {
  dictionary <- bcapture:::load_epi_dictionary()
  out_dir <- tempfile("epi-validate-")
  dir.create(file.path(out_dir, "combined"), recursive = TRUE)
  if (is.null(values_by_form)) values_by_form <- list(synthetic_1 = values)
  if (is.null(names(values_by_form)) || any(!nzchar(names(values_by_form)))) stop("Synthetic forms must be named.")
  field_rows <- purrr::imap_dfr(values_by_form, function(form_values, form_id) {
    rows <- dictionary$fields |>
      dplyr::transmute(
        form_id = form_id, form_type = "initial_epi", source_file = paste0(form_id, ".pdf"),
        source_relpath = paste0(form_id, ".pdf"), source_sha256 = paste0("sha-", form_id), field = raw_field,
        alternative_name, field_type = ifelse(response_type %in% c("coded", "choice"), "Btn", "Tx"),
        field_flags = NA_integer_, value_raw = NA_character_, value = NA_character_,
        default_value_raw = NA_character_, default_value = NA_character_, is_default_value = FALSE,
        states = NA_character_, options = NA_character_, is_multiselect = response_type == "multiselect" |
          raw_field %in% c("p0109c", "p0110c", "p0111c") |
          grepl("^p(013[3-9]|014[0-9]|015[0-9]|016[0-5])d$", raw_field),
        is_populated = FALSE, extraction_method = "synthetic", page = as.integer(source_page),
        form_schema_hash = "synthetic-schema", schema_group = "synthetic-schema"
      )
    for (raw_field in names(form_values)) {
      index <- match(raw_field, rows$field)
      if (is.na(index)) next
      rows$value_raw[[index]] <- as.character(form_values[[raw_field]])
      rows$value[[index]] <- sub("^/", "", as.character(form_values[[raw_field]]))
      rows$is_populated[[index]] <- TRUE
    }
    rows
  })
  manifest <- purrr::imap_dfr(values_by_form, function(form_values, form_id) tibble::tibble(
    form_id = form_id, form_type = "initial_epi", source_file = paste0(form_id, ".pdf"),
    source_relpath = paste0(form_id, ".pdf"), source_sha256 = paste0("sha-", form_id), status = "success",
    failure_type = NA_character_, error = NA_character_, number_of_pages = 12L,
    number_of_fields = sum(field_rows$form_id == form_id), number_of_widgets = 0L,
    number_of_populated_fields = sum(field_rows$form_id == form_id & field_rows$is_populated), form_schema_hash = "synthetic-schema",
    schema_group = "synthetic-schema", extraction_method = "synthetic", pypdf_version = NA_character_,
    extracted_at_utc = "2024-05-28T00:00:00Z"
  ))
  readr::write_csv(field_rows, file.path(out_dir, "combined", "epi_fields_long.csv"), na = "")
  readr::write_csv(dplyr::select(manifest, form_id, form_type, source_file, source_relpath, source_sha256, form_schema_hash, schema_group), file.path(out_dir, "combined", "epi_metadata.csv"), na = "")
  readr::write_csv(manifest, file.path(out_dir, "extraction_manifest.csv"), na = "")
  bcapture::collate_epi(out_dir, quiet = TRUE)
  out_dir
}

test_that("the Initial Epi validation registry is versioned and controlled", {
  rules <- bcapture:::.epi_validation_rules("2024-05-28")
  expect_equal(nrow(rules), 24L)
  expect_true(all(rules$rule_type %in% c("conditional", "table_structure", "codebook")))
  expect_true(all(rules$severity %in% c("INFO", "WARNING", "ERROR")))
  expected_filters <- c(
    conditional_birds_introduced = "direction=onto",
    conditional_birds_moved_off = "direction=off",
    conditional_eggs_onto = "direction=onto;material_type=eggs",
    conditional_egg_products_onto = "direction=onto;material_type=egg_products",
    conditional_eggs_off = "direction=off;material_type=eggs",
    conditional_egg_products_off = "direction=off;material_type=egg_products"
  )
  expect_equal(
    stats::setNames(rules$child_filter[match(names(expected_filters), rules$rule_id)], names(expected_filters)),
    expected_filters
  )
})

test_that("clean synthetic output returns the exact typed zero-finding result", {
  out_dir <- .synthetic_validation_output()
  result <- bcapture::validate_epi(out_dir, quiet = TRUE, write = TRUE)
  expect_equal(nrow(result), 0L)
  expect_equal(names(result), names(bcapture:::new_validation_results()))
  expect_equal(vapply(result, class, character(1)), vapply(bcapture:::new_validation_results(), class, character(1)))
  expect_true(all(file.exists(file.path(out_dir, "validation", c("validation_results.csv", "validation_summary.csv", "validation_form_summary.csv", "validation_report.md")))))
})

test_that("date-expression fields remain raw without scalar-date warnings or chronology cascades", {
  out_dir <- .synthetic_validation_output(list(
    p0140 = "/1", p0140a = "Daily", p0146 = "/1", p0146a = "End on 1/15",
    p0001 = "01/20/2025", p0002 = "01/15/2025", p0003 = "12/18/2024"
  ))
  result <- bcapture::validate_epi(out_dir, quiet = TRUE, write = FALSE)
  expect_false(any(result$rule_id == "expected_date_unparseable"))
  expect_false(any(result$validation_type == "chronology"))
  visitors <- readr::read_csv(file.path(out_dir, "collated", "epi_visitors.csv"), show_col_types = FALSE)
  expect_true(any(visitors$visit_dates_raw == "Daily"))
  expect_true(any(visitors$visit_dates_raw == "End on 1/15"))
  expect_setequal(visitors$visit_dates, c("Daily", "End on 1/15"))
})

test_that("valid dates support exact 28-day chronology and identify incorrect reference periods", {
  valid <- .synthetic_validation_output(list(p0001 = "02/01/2025", p0002 = "01/29/2025", p0003 = "01/01/2025"))
  expect_false(any(bcapture::validate_epi(valid, quiet = TRUE, write = FALSE)$validation_type == "chronology"))
  invalid <- .synthetic_validation_output(list(p0001 = "02/01/2025", p0002 = "01/29/2025", p0003 = "01/02/2025"))
  result <- bcapture::validate_epi(invalid, quiet = TRUE, write = FALSE)
  expect_true(any(result$rule_id == "reference_period_not_28_days"))
})

test_that("scalar and repeated-table conditional rules distinguish expected follow-up", {
  scalar_ok <- .synthetic_validation_output(list(p0020 = "/1", p00020a = "Synthetic veterinarian"))
  expect_false(any(bcapture::validate_epi(scalar_ok, quiet = TRUE, write = FALSE)$rule_id == "yes_followup_missing"))
  scalar_missing <- .synthetic_validation_output(list(p0020 = "/1"))
  expect_true(any(bcapture::validate_epi(scalar_missing, quiet = TRUE, write = FALSE)$rule_id == "yes_followup_missing"))
  scalar_unexpected <- .synthetic_validation_output(list(p0020 = "/3", p00020a = "Unexpected follow-up"))
  expect_true(any(bcapture::validate_epi(scalar_unexpected, quiet = TRUE, write = FALSE)$rule_id == "no_with_followup_data"))
  table_ok <- .synthetic_validation_output(list(p0122 = "/1", p0123 = "Premises", p0123a = "Worker", p0123b = "01/15/25"))
  expect_false(any(bcapture::validate_epi(table_ok, quiet = TRUE, write = FALSE)$rule_id == "yes_followup_missing"))
  table_missing <- .synthetic_validation_output(list(p0122 = "/1"))
  expect_true(any(bcapture::validate_epi(table_missing, quiet = TRUE, write = FALSE)$rule_id == "yes_followup_missing"))
  table_unexpected <- .synthetic_validation_output(list(p0122 = "/3", p0123 = "Unexpected premises"))
  expect_true(any(bcapture::validate_epi(table_unexpected, quiet = TRUE, write = FALSE)$rule_id == "no_with_followup_data"))
})

test_that("all child-table conditionals are isolated by form in batch validation", {
  cases <- list(
    worker_visits = c(parent = "p0122", child = "p0123"),
    crews = c(parent = "p0127", child = "p0128"),
    birds_onto = c(parent = "p0166", child = "p0167"),
    birds_off = c(parent = "p0172", child = "p0173"),
    eggs_onto = c(parent = "p0181", child = "p0182"),
    egg_products_onto = c(parent = "p0186", child = "p0187"),
    eggs_off = c(parent = "p0191", child = "p0192"),
    egg_products_off = c(parent = "p0196", child = "p0197")
  )
  parents <- vapply(cases, `[[`, character(1), "parent")
  children <- vapply(cases, `[[`, character(1), "child")
  no_values <- stats::setNames(as.list(rep("/3", length(parents))), parents)
  yes_values <- c(
    stats::setNames(as.list(rep("/1", length(parents))), parents),
    stats::setNames(as.list(rep("Synthetic follow-up", length(children))), children)
  )
  out_dir <- .synthetic_validation_output(values_by_form = list(form_a = no_values, form_b = yes_values, form_c = no_values))
  findings <- bcapture::validate_epi(out_dir, quiet = TRUE, write = FALSE)
  targeted <- findings[
    findings$raw_field %in% parents & findings$rule_id %in% c("yes_followup_missing", "no_with_followup_data"),
    , drop = FALSE
  ]
  expect_equal(nrow(targeted), 0L)
})

test_that("declared child-table filters reject the wrong direction or material subset", {
  out_dir <- .synthetic_validation_output(values_by_form = list(
    bird_wrong_direction = list(p0166 = "/1", p0173 = "01/15/25"),
    egg_wrong_material = list(p0191 = "/1", p0197 = "Synthetic egg product"),
    egg_wrong_direction = list(p0181 = "/1", p0192 = "Synthetic eggs off site")
  ))
  findings <- bcapture::validate_epi(out_dir, quiet = TRUE, write = FALSE)

  expect_true(any(findings$form_id == "bird_wrong_direction" & findings$raw_field == "p0166" & findings$rule_id == "yes_followup_missing"))
  expect_true(any(findings$form_id == "egg_wrong_material" & findings$raw_field == "p0191" & findings$rule_id == "yes_followup_missing"))
  expect_true(any(findings$form_id == "egg_wrong_direction" & findings$raw_field == "p0181" & findings$rule_id == "yes_followup_missing"))
})

test_that("malformed numeric values do not cascade to negative-value findings", {
  out_dir <- .synthetic_validation_output(list(p0005 = "not numeric"))
  result <- bcapture::validate_epi(out_dir, quiet = TRUE, write = FALSE)
  expect_true(any(result$rule_id == "expected_numeric_unparseable"))
  expect_false(any(result$rule_id == "negative_value"))
})

test_that("coordinate bounds and negative quantities are validated without requiring coordinates", {
  out_dir <- .synthetic_validation_output(list(premlat = "91", premlong = "-181", p0005 = "-1"))
  result <- bcapture::validate_epi(out_dir, quiet = TRUE, write = FALSE)
  expect_true(all(c("latitude_out_of_range", "longitude_out_of_range", "negative_value") %in% result$rule_id))
  clean <- .synthetic_validation_output()
  expect_false(any(bcapture::validate_epi(clean, quiet = TRUE, write = FALSE)$rule_id %in% c("latitude_out_of_range", "longitude_out_of_range")))
})

test_that("unknown normalized multiselect options are errors", {
  out_dir <- .synthetic_validation_output(list(p0109c = "Poultry|Unregistered option"))
  result <- bcapture::validate_epi(out_dir, quiet = TRUE, write = FALSE)
  expect_true(any(result$rule_id == "unknown_multiselect_option"))
  expect_true(all(result$severity[result$rule_id == "unknown_multiselect_option"] == "ERROR"))
})

test_that("strict mode writes complete findings and signals only errors", {
  out_dir <- .synthetic_validation_output(list(p0310 = "/9"))
  expect_warning(result <- bcapture::validate_epi(out_dir, strict = TRUE, quiet = TRUE), "Strict Initial Epi validation")
  expect_true(any(result$rule_id == "unknown_response_code"))
  expect_true(isTRUE(attr(result, "strict_failure")))
})
