test_that("union row binding preserves fields from different schemas", {
  x <- tibble::tibble(audit_id = "a", field_a = "one")
  y <- tibble::tibble(audit_id = "b", field_b = "two")
  combined <- bcapture:::bind_union_rows(list(x, y))
  expect_true(all(c("field_a", "field_b") %in% names(combined)))
  expect_equal(nrow(combined), 2L)
})

test_that("schema hashes are stable and definition-sensitive", {
  available <- tryCatch(suppressWarnings(reticulate::py_module_available("pypdf")), error = function(e) FALSE)
  skip_if_not(available, "Python/pypdf unavailable")
  module <- tryCatch(bcapture:::ensure_hpai_python(), error = function(e) NULL)
  skip_if(is.null(module), "Python/pypdf unavailable")
  a <- module$schema_hash(list(list(field = "a", field_type = "Tx", states = NULL)))
  b <- module$schema_hash(list(list(field = "a", field_type = "Tx", states = NULL)))
  c <- module$schema_hash(list(list(field = "a", field_type = "Btn", states = "Yes|Off")))
  expect_identical(as.character(a), as.character(b))
  expect_false(identical(as.character(a), as.character(c)))
})

test_that("batch failure isolation is represented in the manifest", {
  input <- tempfile("bcapture-batch-")
  out <- tempfile("bcapture-out-")
  dir.create(input)
  writeBin(charToRaw("not a PDF"), file.path(input, "bad.pdf"))
  manifest <- suppressWarnings(extract_hpai(input, out, quiet = TRUE))
  expect_equal(manifest$status, "failed")
  expect_equal(manifest$failure_type, "pdf_read_error")
  expect_true(file.exists(file.path(out, "extraction_manifest.csv")))
})

test_that("synthetic 17e diagnostics preserve raw fields and distinguish differences", {
  out <- tempfile("bcapture-diagnostic-")
  dir.create(file.path(out, "combined"), recursive = TRUE)
  fields <- tibble::tribble(
    ~audit_id, ~source_file, ~source_relpath, ~source_md5, ~field_index, ~page, ~field, ~alternative_name, ~field_type, ~value_raw, ~value, ~states, ~is_populated, ~extraction_method,
    "pdf1", "pdf1.pdf", "pdf1.pdf", "a", 1L, 1L, "Q1_3_17e", NA, "Btn", NA, NA, "No|Off", FALSE, "acroform",
    "pdf1", "pdf1.pdf", "pdf1.pdf", "a", 2L, 1L, "Q1_4_17e", NA, "Btn", "/Yes", "Yes", "Off|Yes", TRUE, "acroform",
    "pdf2", "pdf2.pdf", "pdf2.pdf", "b", 2L, 1L, "Q1_3_17e", NA, "Btn", NA, NA, "Off|No", FALSE, "acroform",
    "pdf2", "pdf2.pdf", "pdf2.pdf", "b", 1L, 1L, "Q1_4_17e", NA, "Btn", "/Yes", "Yes", "Yes|Off", TRUE, "acroform",
    "pdf3", "pdf3.pdf", "pdf3.pdf", "c", 1L, 1L, "Q1_4_17e", NA, "Btn", "/Yes", "Yes", "Yes|No", TRUE, "acroform"
  )
  metadata <- tibble::tribble(
    ~audit_id, ~source_file, ~source_relpath, ~source_md5, ~number_of_pages, ~number_of_fields, ~number_of_widgets, ~number_of_populated_fields, ~form_schema_hash, ~schema_group,
    "pdf1", "pdf1.pdf", "pdf1.pdf", "a", 1L, 2L, 2L, 1L, "hash_a", "schema_001",
    "pdf2", "pdf2.pdf", "pdf2.pdf", "b", 1L, 2L, 2L, 1L, "hash_a", "schema_001",
    "pdf3", "pdf3.pdf", "pdf3.pdf", "c", 1L, 1L, 1L, 1L, "hash_b", "schema_002"
  )
  readr::write_csv(fields, file.path(out, "combined", "hpai_fields_long.csv"))
  readr::write_csv(metadata, file.path(out, "combined", "hpai_metadata.csv"))
  diagnostic <- diagnose_hpai(out, quiet = TRUE)
  expect_equal(diagnostic$field_presence_differences$field, "Q1_3_17e")
  expect_equal(nrow(diagnostic$field_type_differences), 0L)
  expect_true("Q1_4_17e" %in% diagnostic$field_state_differences$field)
  expect_true(nrow(diagnostic$widget_differences) > 0L)
  expect_true(file.exists(file.path(out, "diagnostics", "schema_diagnostics.md")))
  expect_true(all(c("Q1_3_17e", "Q1_4_17e") %in% fields$field))
})

test_that("diagnostic discovery excludes populated derivatives", {
  out <- tempfile("bcapture-discovery-")
  audit_dir <- file.path(out, "audits", "pdf1")
  dir.create(audit_dir, recursive = TRUE)
  fields <- tibble::tibble(
    audit_id = "pdf1", source_file = "pdf1.pdf", source_relpath = "pdf1.pdf",
    source_md5 = "a", field_index = 1L, page = 1L, field = "field1",
    alternative_name = NA_character_, field_type = "Tx", value_raw = "x",
    value = "x", states = NA_character_, is_populated = TRUE,
    extraction_method = "acroform"
  )
  metadata <- tibble::tibble(
    audit_id = "pdf1", source_file = "pdf1.pdf", source_relpath = "pdf1.pdf",
    source_md5 = "a", number_of_pages = 1L, number_of_fields = 1L,
    number_of_widgets = 1L, number_of_populated_fields = 1L,
    form_schema_hash = "hash_a", schema_group = "schema_001"
  )
  readr::write_csv(fields, file.path(audit_dir, "pdf1_fields_long.csv"))
  readr::write_csv(fields, file.path(audit_dir, "pdf1_populated_fields_long.csv"))
  readr::write_csv(metadata, file.path(audit_dir, "pdf1_metadata.csv"))
  diagnostic <- diagnose_hpai(out, write = FALSE, quiet = TRUE)
  expect_equal(nrow(diagnostic$summary), 1L)
  expect_equal(diagnostic$summary$number_of_fields, 1L)
})

test_that("state and field ordering are normalized for schema hashing", {
  expect_equal(bcapture:::normalize_state_set("Yes|No"), c("No", "Yes"))
  expect_equal(bcapture:::normalize_state_set("Off|Yes"), c("Off", "Yes"))
  available <- tryCatch(suppressWarnings(reticulate::py_module_available("pypdf")), error = function(e) FALSE)
  skip_if_not(available, "Python/pypdf unavailable")
  a <- tibble::tibble(field = c("field1", "field2"), field_type = c("Tx", "Btn"), states = c(NA, "Yes|No"))
  b <- tibble::tibble(field = c("field2", "field1"), field_type = c("Btn", "Tx"), states = c("No|Yes", NA))
  c <- tibble::tibble(field = c("field2"), field_type = c("Btn"), states = c("Off|Yes"))
  expect_identical(bcapture:::schema_hash_from_fields(a), bcapture:::schema_hash_from_fields(b))
  expect_false(identical(bcapture:::schema_hash_from_fields(a[2, , drop = FALSE]), bcapture:::schema_hash_from_fields(c)))
})
