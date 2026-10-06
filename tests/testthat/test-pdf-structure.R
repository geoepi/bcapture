structure_helper <- function() {
  available <- tryCatch(suppressWarnings(reticulate::py_module_available("pypdf")), error = function(e) FALSE)
  skip_if_not(available, "Python/pypdf unavailable")
  helper <- new.env()
  reticulate::source_python(testthat::test_path("fixtures", "synthetic_structures.py"), envir = helper)
  helper$set_template(file.path(bcapture:::spatial_template_dir("initial_epi"), "blank_interactive.pdf"))
  helper
}

test_that("one embedded interactive Epi retains container and attachment provenance", {
  helper <- structure_helper()
  for (kind in c("portfolio", "embedded", "af_only")) {
    pdf <- tempfile(fileext = ".pdf")
    helper$create_structure(pdf, kind)
    before <- bcapture:::source_sha256(pdf)
    out <- tempfile("structure-epi-")
    result <- extract_epi_file(pdf, out, quiet = TRUE)
    expect_identical(result$status, "success")
    expect_equal(nrow(result$fields), 497L)
    expect_equal(nrow(result$widgets), 667L)
    expect_true(all(result$fields$extraction_method == "acroform"))
    expect_equal(result$manifest$source_sha256, before)
    expect_equal(bcapture:::source_sha256(pdf), before)
    expect_equal(result$manifest$source_pdf_structure, result$metadata$source_pdf_structure)
      expect_equal(result$manifest$number_of_pages, 12L)
      expect_equal(result$metadata$source_container_pages, 1L)
      expect_equal(result$metadata$embedded_file_count, 2L)
      expect_equal(result$metadata$embedded_pdf_ordinal, 2L)
      expected_hash <- digest::digest(as.raw(helper$epi_bytes()), algo = "sha256", serialize = FALSE)
      expect_equal(result$metadata$embedded_pdf_sha256, expected_hash)
  }
})

test_that("compound and ambiguous structures fail without selecting a form", {
  helper <- structure_helper()
  input <- tempfile("structure-input-")
  output <- tempfile("structure-output-")
  dir.create(input)
  expected <- c(
    multiple_embedded = "multiple_embedded_acroforms",
    top_level_and_embedded = "compound_pdf_structure",
    flattened_attachment = "unsupported_embedded_flattened_pdf",
    incompatible_embedded = "unexpected_form_type",
    encrypted_attachment = "encrypted_or_unreadable_embedded_pdf",
    unreadable_attachment = "encrypted_or_unreadable_embedded_pdf",
    duplicate_fields = "ambiguous_acroform_structure",
    mapped_duplicate_fields = "ambiguous_acroform_structure",
    concatenated_epi = "ambiguous_acroform_structure",
    nul_field = "unsupported_pdf_string",
    long_supplement = "ambiguous_embedded_pdf",
    nested_container = "unsupported_nested_pdf_container",
    partial_embedded = "unsupported_embedded_form_schema",
    epi_supplemental = "ambiguous_form_pages",
    missing_stream = "unsupported_pdf_container", malformed_stream = "unsupported_pdf_container",
    c2pa_wrong_type = "unsupported_non_pdf_attachment", c2pa_pdf = "compound_pdf_structure",
    unknown_attachment = "unsupported_non_pdf_attachment",
    xfa = "unsupported_xfa_form", hybrid_xfa = "unsupported_xfa_form"
  )
  for (kind in names(expected)) helper$create_structure(file.path(input, paste0(kind, ".pdf")), kind)
  manifest <- extract_epi(input, output, diagnostics = FALSE, quiet = TRUE)
  expect_true(all(manifest$status == "failed"))
  expect_equal(manifest$failure_type[match(names(expected), manifest$form_id)], unname(expected))
  expect_false(dir.exists(file.path(output, "forms")))
  expect_true(all(c("source_pdf_structure", "source_container_pages", "embedded_file_count",
    "embedded_pdf_ordinal", "embedded_pdf_sha256") %in% names(manifest)))
  bcap_input <- tempfile("bcap-structure-input-")
  dir.create(bcap_input)
  bcap_kinds <- c("xfa", "hybrid_xfa", "top_level_and_embedded")
  for (kind in bcap_kinds) helper$create_structure(file.path(bcap_input, paste0(kind, ".pdf")), kind)
  bcap <- extract_hpai(bcap_input, tempfile("bcap-structure-"), diagnostics = FALSE, quiet = TRUE)
  expect_equal(bcap$failure_type[match(bcap_kinds, bcap$audit_id)],
    c("unsupported_xfa_form", "unsupported_xfa_form", "compound_pdf_structure"))
})

test_that("scans are classified before page count and metadata NUL cannot abort routing", {
  helper <- structure_helper()
  input <- tempfile("scan-input-")
  output <- tempfile("scan-output-")
  dir.create(input)
  for (kind in c("image_scan", "nul_metadata_scan", "nul_metadata_key_scan")) helper$create_structure(file.path(input, paste0(kind, ".pdf")), kind)
  manifest <- extract_epi(input, output, diagnostics = FALSE, quiet = TRUE)
  expect_true(all(manifest$failure_type == "no_usable_digital_content"))
  expect_true(all(manifest$extraction_method == "spatial_template"))
  expect_true(all(manifest$number_of_pages == 1L))
  module <- bcapture:::ensure_acroform_python()
  parsed <- reticulate::py_to_r(module$extract_form(file.path(input, "nul_metadata_scan.pdf")))
  expect_equal(parsed$pdf_metadata$pdf_metadata_omitted_nul_count, 1L)
  expect_equal(parsed$pdf_metadata$pdf_metadata_omitted_nul_keys, "pdf_producer")
  expect_null(parsed$pdf_metadata$pdf_producer)
  key_parsed <- reticulate::py_to_r(module$extract_form(file.path(input, "nul_metadata_key_scan.pdf")))
  expect_equal(key_parsed$pdf_metadata$pdf_metadata_omitted_nul_count, 1L)
  expect_equal(key_parsed$pdf_metadata$pdf_metadata_omitted_nul_keys, "unrepresentable_metadata_key")
  bcap <- extract_hpai(input, tempfile("scan-bcap-"), diagnostics = FALSE, quiet = TRUE)
  expect_true(all(bcap$failure_type == "no_usable_digital_content"))
  expect_true(all(bcap$extraction_method == "spatial_template"))
})

test_that("C2PA manifests preserve standalone extraction and spatial eligibility", {
  helper <- structure_helper()
  input <- tempfile("c2pa-input-")
  dir.create(input)
  for (kind in c("c2pa_form", "c2pa_empty")) helper$create_structure(file.path(input, paste0(kind, ".pdf")), kind)
  manifest <- extract_epi(input, tempfile("c2pa-output-"), diagnostics = FALSE, quiet = TRUE)
  expect_equal(manifest$status[match("c2pa_form", manifest$form_id)], "success")
  expect_equal(manifest$embedded_file_count, c(1L, 1L))
  expect_equal(manifest$source_pdf_structure, c("standalone", "standalone"))
  expect_equal(manifest$failure_type[match("c2pa_empty", manifest$form_id)], "no_usable_digital_content")
  expect_equal(manifest$extraction_method[match("c2pa_empty", manifest$form_id)], "spatial_template")
  module <- bcapture:::ensure_acroform_python()
  parsed <- reticulate::py_to_r(module$extract_form(file.path(input, "c2pa_form.pdf")))
  expect_equal(parsed$number_of_fields, 497L)
})

test_that("raw-tree audit preserves unnamed widget kids and unnamed ancestors", {
  helper <- structure_helper()
  module <- bcapture:::ensure_acroform_python()
  for (kind in c("radio_group", "unnamed_ancestor")) {
    pdf <- tempfile(fileext = ".pdf")
    helper$create_structure(pdf, kind)
    parsed <- reticulate::py_to_r(module$extract_form(pdf))
    expect_equal(parsed$number_of_fields, 1L)
    expect_equal(parsed$number_of_widgets, if (kind == "radio_group") 2L else 0L)
  }
})

test_that("diagnostic grouping preserves ordered candidate rows and geometry", {
  fields <- tibble::tibble(audit_id=c("a","b","c","d","e","f"), field_type="Btn",
    field=c("Q1_1_same","Q2_1_same","Q3_1_same","Q4_1_same","Q5_1_other","Q6_1_other"),
    states=c("Off|Yes","Off|No","Yes|No","Off|1|3","Off|Yes|No","Off|No"), page=1L, schema_group="synthetic")
  widgets <- tibble::tibble(full_field_name=fields$field, page=1L,
    rect_x1=seq(10,60,10), rect_x2=seq(20,70,10))
  result <- bcapture:::.widget_encoding_candidates(fields, widgets)
  i <- c(1,1,2,2,3,3,5,6)
  j <- c(2,3,1,3,1,2,6,5)
  expect_equal(result$audit_id, fields$audit_id[i])
  expect_equal(result$field, fields$field[i])
  expect_equal(result$companion_field, fields$field[j])
  expect_equal(result$field_states, fields$states[i])
  expect_equal(result$companion_states, fields$states[j])
  expect_true(all(result$page == 1L))
  expect_true(all(grepl("geographically nearby", result$notes)))
  fields$states <- "Off|1|3"
  expect_equal(nrow(bcapture:::.widget_encoding_candidates(fields, widgets)), 0L)
})
