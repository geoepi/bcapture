test_that("Epi signature accepts distributed anchors and rejects BCAP names", {
  expect_true(bcapture:::is_initial_epi_form(c("premid", "premname", "p0001", "p0100", "p0178", "p0300", "p0328")))
  expect_false(bcapture:::is_initial_epi_form(c("Q1_1_1", "Q1_1_result", "audit_results")))
  expect_error(bcapture:::validate_epi_signature(c("premid", "p0001")), "Unexpected form type")
})

test_that("default values and Epi placeholders are retained but not populated", {
  rows <- bcapture:::field_rows_to_tibble(list(
    list(field_index = 1, page = 1, field = "p0109b", value_raw = "Select or Type", value = "Select or Type", default_value_raw = "Select or Type", default_value = "Select or Type", is_default_value = TRUE, is_populated = TRUE),
    list(field_index = 2, page = 1, field = "p0109c", value_raw = "Select (Ctrl for multi)", value = "Select (Ctrl for multi)", default_value_raw = "Select (Ctrl for multi)", default_value = "Select (Ctrl for multi)", is_default_value = TRUE, is_populated = TRUE),
    list(field_index = 3, page = 1, field = "text", value_raw = "same", value = "same", default_value_raw = "same", default_value = "same", is_default_value = TRUE, is_populated = TRUE)
  ))
  rows <- bcapture:::.epi_fields(rows)
  expect_true(all(rows$is_default_value))
  expect_true(all(rows$is_placeholder[1:2]))
  expect_false(any(rows$is_populated[1:2]))
  expect_true(rows$is_populated[[3]])
  expect_equal(rows$value[[1]], "Select or Type")
})

test_that("choice option order is not schema-significant", {
  expect_equal(bcapture:::normalize_option_set("A|B|C"), c("A", "B", "C"))
  expect_identical(bcapture:::normalize_option_set("A|B|C"), bcapture:::normalize_option_set("C|A|B"))
})

test_that("coded button values remain raw", {
  rows <- bcapture:::field_rows_to_tibble(list(list(
    field_index = 1, page = 1, field = "p0020", field_type = "Btn",
    value_raw = "/1", value = "1", states = "1|3", is_populated = TRUE
  )))
  expect_equal(rows$value_raw, "/1")
  expect_equal(rows$value, "1")
  expect_equal(rows$states, "1|3")
  expect_false(any(rows$value == "Yes"))
})

test_that("repeated Epi fields retain distinct raw names", {
  rows <- bcapture:::field_rows_to_tibble(list(
    list(field_index = 1, page = 2, field = "p0010a", value = "one", is_populated = TRUE),
    list(field_index = 2, page = 2, field = "p0010b", value = "two", is_populated = TRUE),
    list(field_index = 3, page = 3, field = "p0011a", value = "three", is_populated = TRUE),
    list(field_index = 4, page = 3, field = "p0011b", value = "four", is_populated = TRUE)
  ))
  expect_identical(rows$field, c("p0010a", "p0010b", "p0011a", "p0011b"))
  wide <- bcapture:::as_single_row_tibble(stats::setNames(as.list(rows$value), rows$field))
  expect_true(all(c("p0010a", "p0010b", "p0011a", "p0011b") %in% names(wide)))
})

test_that("choice field flags identify multi-select", {
  available <- tryCatch(suppressWarnings(reticulate::py_module_available("pypdf")), error = function(e) FALSE)
  skip_if_not(available, "Python/pypdf unavailable")
  module <- tryCatch(bcapture:::ensure_acroform_python(), error = function(e) NULL)
  skip_if(is.null(module), "Python/pypdf unavailable")
  expect_true(isTRUE(module$is_multiselect(as.integer(2^21), "Ch")))
  expect_false(isTRUE(module$is_multiselect(as.integer(0), "Ch")))
})
