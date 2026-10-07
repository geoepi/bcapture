test_that("pypdf parser extracts core AcroForm mechanics from a synthetic PDF", {
  module <- tryCatch(bcapture:::ensure_acroform_python(), error = function(e) NULL)
  skip_if(is.null(module), "Python/pypdf unavailable")
  fixture <- testthat::test_path("fixtures", "synthetic_acroform.py")
  helper <- new.env()
  reticulate::source_python(fixture, envir = helper)
  pdf <- tempfile(fileext = ".pdf")
  helper$create_synthetic_acroform(pdf)
  parsed <- reticulate::py_to_r(module$extract_form(pdf))
  expect_equal(parsed$number_of_fields, 6L)
  expect_equal(parsed$number_of_widgets, 6L)
  fields <- bcapture:::field_rows_to_tibble(parsed$fields)
  expect_true(all(c("default_value", "options", "field_flags", "is_multiselect") %in% names(fields)))
  expect_equal(fields$value[fields$field == "button"], "1")
  expect_equal(fields$value_raw[fields$field == "button"], "1")
  expect_equal(fields$states[fields$field == "button"], "Off|1|3")
  expect_equal(fields$options[fields$field == "choice"], "A|B|C")
  expect_true(fields$is_multiselect[fields$field == "multichoice"])
  expect_equal(fields$default_value[fields$field == "choice"], "Select or Type")
})

test_that("signature fields use deterministic structural markers", {
  module <- tryCatch(bcapture:::ensure_acroform_python(), error = function(e) NULL)
  skip_if(is.null(module), "Python/pypdf unavailable")
  fixture <- testthat::test_path("fixtures", "synthetic_acroform.py")
  helper <- new.env()
  reticulate::source_python(fixture, envir = helper)
  pdf_a <- tempfile(fileext = ".pdf")
  pdf_b <- tempfile(fileext = ".pdf")
  helper$create_synthetic_acroform(pdf_a, 0L)
  helper$create_synthetic_acroform(pdf_b, 1L)

  parsed_a <- reticulate::py_to_r(module$extract_form(pdf_a))
  parsed_a_repeat <- reticulate::py_to_r(module$extract_form(pdf_a))
  parsed_b <- reticulate::py_to_r(module$extract_form(pdf_b))
  fields_a <- bcapture:::field_rows_to_tibble(parsed_a$fields)
  fields_a_repeat <- bcapture:::field_rows_to_tibble(parsed_a_repeat$fields)
  fields_b <- bcapture:::field_rows_to_tibble(parsed_b$fields)
  widgets_a <- bcapture:::widget_rows_to_tibble(parsed_a$widgets)
  widgets_b <- bcapture:::widget_rows_to_tibble(parsed_b$widgets)

  signed <- fields_a[fields_a$field == "signature_signed", , drop = FALSE]
  empty <- fields_a[fields_a$field == "signature_empty", , drop = FALSE]
  expect_equal(signed$value_raw, "signature_present")
  expect_equal(signed$value, "signature_present")
  expect_true(signed$is_populated)
  expect_true(is.na(empty$value_raw) || empty$value_raw == "")
  expect_true(is.na(empty$value) || empty$value == "")
  expect_false(empty$is_populated)
  expect_equal(fields_a$value[fields_a$field == "text"], "hello")
  expect_equal(fields_a$value[fields_a$field == "button"], "1")
  expect_identical(fields_a, fields_a_repeat)
  expect_equal(
    fields_a[fields_a$field_type != "Sig", c("field", "value_raw", "value")],
    fields_b[fields_b$field_type != "Sig", c("field", "value_raw", "value")]
  )
  expect_equal(
    fields_a[fields_a$field_type == "Sig", c("field", "value_raw", "value", "is_populated")],
    fields_b[fields_b$field_type == "Sig", c("field", "value_raw", "value", "is_populated")]
  )
  expect_identical(parsed_a$form_schema_hash, parsed_b$form_schema_hash)
  expect_equal(
    widgets_a[widgets_a$field_type != "Sig", c("full_field_name", "value", "parent_value")],
    widgets_b[widgets_b$field_type != "Sig", c("full_field_name", "value", "parent_value")]
  )
  expect_false(any(grepl("Synthetic Signer|Adobe\\.PPKLite|ByteRange|Contents|SYNTHETIC-SIGNATURE", unlist(fields_a))))
  expect_false(any(grepl("Synthetic Signer|Adobe\\.PPKLite|ByteRange|Contents|SYNTHETIC-SIGNATURE", unlist(widgets_a))))
})

test_that("Epi batch rejects an incompatible structured AcroForm", {
  skip_if(is.null(tryCatch(bcapture:::ensure_acroform_python(), error = function(e) NULL)), "Python/pypdf unavailable")
  helper <- new.env()
  reticulate::source_python(testthat::test_path("fixtures", "synthetic_acroform.py"), envir = helper)
  input <- tempfile("epi-in-")
  output <- tempfile("epi-out-")
  dir.create(input)
  helper$create_synthetic_acroform(file.path(input, "not_epi.pdf"))
  manifest <- extract_epi(input, output, quiet = TRUE)
  expect_equal(manifest$status, "failed")
  expect_equal(manifest$failure_type, "unexpected_form_type")
  expect_true(file.exists(file.path(output, "extraction_manifest.csv")))
})
