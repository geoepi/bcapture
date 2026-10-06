spatial_fixture_dir <- function() {
  testthat::test_path("fixtures", "spatial")
}

skip_if_spatial_unavailable <- function() {
  available <- tryCatch(
    suppressWarnings(reticulate::py_module_available("pypdf")),
    error = function(error) FALSE
  )
  testthat::skip_if_not(available, "Python spatial PDF dependencies unavailable")
  module <- tryCatch(bcapture:::ensure_spatial_python(), error = function(error) NULL)
  testthat::skip_if(is.null(module), "Python spatial PDF dependencies unavailable")
  invisible(module)
}

truth_values <- function(path) {
  truth <- readr::read_csv(path, show_col_types = FALSE)
  truth$value_normalized <- sub("^/", "", truth$value)
  truth$field_type <- sub("^/", "", truth$field_type)
  truth
}

test_that("versioned spatial templates preserve canonical provenance", {
  epi <- readr::read_csv(
    file.path(bcapture:::spatial_template_dir("initial_epi"), "manifest.csv"),
    show_col_types = FALSE
  )
  bcap <- readr::read_csv(
    file.path(bcapture:::spatial_template_dir("bcap"), "manifest.csv"),
    show_col_types = FALSE
  )
  expect_equal(epi$canonical_schema_hash, "3c2a429404b86f0579943f67efedd6e596797b43ee0e413a8d671626190171c1")
  expect_equal(bcap$canonical_schema_hash, "133888c178fa8905fad8dac4b8e4135ce17782741ce4773518eb13d9cc69d907")
  expect_equal(epi$canonical_field_count, 497)
  expect_equal(epi$canonical_widget_count, 667)
  expect_equal(bcap$canonical_field_count, 225)
  expect_equal(bcap$canonical_widget_count, 316)
  expect_equal(epi$interactive_sha256, "08595167a0030187228a639c99bdfa9e3eaa84b99f044c14c71efd93a4edeee6")
  expect_equal(bcap$printed_sha256, "0e8415de6790eff5902d1c80bad2f9289d377649dfe3241b6e03df220b46c444")
  expect_equal(as.numeric(bcap$derived_first_page_scale), 0.9407069555, tolerance = 1e-10)
  expect_equal(as.numeric(bcap$derived_first_page_dx), 25.9891676, tolerance = 1e-7)
})

test_that("flattened Initial Epi extraction has provenance and no blank false positives", {
  skip_if_spatial_unavailable()
  fixture <- spatial_fixture_dir()
  result <- extract_epi_file(
    file.path(fixture, "synthetic_epi_print.pdf"),
    tempfile("bcapture-spatial-epi-"),
    quiet = TRUE
  )
  expect_identical(result$status, "success")
  expect_true(all(result$fields$extraction_method %in% c("spatial_text", "spatial_mark")))
  expect_equal(nrow(result$widgets), 0L)
  expect_true(all(is.na(result$metadata$form_schema_hash)))
  expect_equal(result$metadata$canonical_template_schema_hash, "3c2a429404b86f0579943f67efedd6e596797b43ee0e413a8d671626190171c1")
  expect_equal(result$metadata$number_of_source_widgets, 0L)

  truth <- truth_values(file.path(fixture, "synthetic_epi_ground_truth.csv"))
  observed <- result$fields[match(truth$field, result$fields$field), , drop = FALSE]
  expect_false(anyNA(observed$field))
  expect_equal(sum(sub("^/", "", observed$value) == truth$value_normalized, na.rm = TRUE), nrow(truth))
  expect_equal(
    sum(observed$field_type == "Btn" & observed$is_populated),
    sum(truth$field_type == "Btn" & truth$value_normalized != "Off")
  )

  blank <- bcapture:::extract_spatial_pdf(
    file.path(bcapture:::spatial_template_dir("initial_epi"), "blank_printed.pdf"),
    "initial_epi"
  )
  blank_fields <- bcapture:::field_rows_to_tibble(blank$fields, extraction_method = "spatial_template")
  expect_equal(sum(blank_fields$is_populated), 0L)
  expect_equal(sum(blank_fields$field_type == "Btn" & blank_fields$value != "Off", na.rm = TRUE), 0L)
})

test_that("flattened BCAP extraction recovers synthetic values exactly", {
  skip_if_spatial_unavailable()
  fixture <- spatial_fixture_dir()
  result <- extract_hpai_file(
    file.path(fixture, "synthetic_bcap_audit_print.pdf"),
    tempfile("bcapture-spatial-bcap-"),
    quiet = TRUE
  )
  expect_identical(result$status, "success")
  expect_true(all(result$fields$extraction_method %in% c("spatial_text", "spatial_mark")))
  expect_equal(nrow(result$widgets), 0L)
  expect_true(all(is.na(result$metadata$form_schema_hash)))
  expect_equal(result$metadata$canonical_template_schema_hash, "133888c178fa8905fad8dac4b8e4135ce17782741ce4773518eb13d9cc69d907")
  expect_equal(result$metadata$number_of_source_widgets, 0L)

  truth <- truth_values(file.path(fixture, "synthetic_bcap_audit_ground_truth.csv"))
  observed <- result$fields[match(truth$field, result$fields$field), , drop = FALSE]
  expect_false(anyNA(observed$field))
  expect_equal(sub("^/", "", observed$value), truth$value_normalized)
  expect_equal(sum(result$fields$is_populated & !result$fields$field %in% truth$field), 0L)
})

test_that("spatial extraction rejects a recognized-family mismatch", {
  skip_if_spatial_unavailable()
  expect_error(
    extract_epi_file(
      file.path(bcapture:::spatial_template_dir("bcap"), "blank_printed.pdf"),
      tempfile("bcapture-spatial-wrong-form-"),
      quiet = TRUE
    ),
    "Page count|canonical printed template"
  )
})

test_that("mixed AcroForm and flattened batches retain route provenance", {
  skip_if_spatial_unavailable()
  input <- tempfile("bcapture-spatial-mixed-in-")
  output <- tempfile("bcapture-spatial-mixed-out-")
  dir.create(input)
  file.copy(
    file.path(bcapture:::spatial_template_dir("initial_epi"), "blank_interactive.pdf"),
    file.path(input, "interactive.pdf")
  )
  file.copy(
    file.path(spatial_fixture_dir(), "synthetic_epi_print.pdf"),
    file.path(input, "flattened.pdf")
  )
  manifest <- extract_epi(input, output, diagnostics = FALSE, quiet = TRUE)
  expect_true(all(manifest$status == "success"))
  expect_setequal(manifest$extraction_method, c("acroform", "spatial_template"))
  fields <- readr::read_csv(file.path(output, "combined", "epi_fields_long.csv"), show_col_types = FALSE)
  expect_true(all(c("extraction_status", "evidence_class", "registration_quality") %in% names(fields)))
  expect_setequal(unique(fields$extraction_method), c("acroform", "spatial_text", "spatial_mark"))
})
