#' Extract HPAI BCAP PDF form data
#'
#' Extracts structured AcroForm field values from completed HPAI Biosecurity
#' Compliance Audit Program PDF forms.
#'
#' @param pdf_file Path to one electronically populated PDF form.
#' @param out_dir Directory in which the `audits/` output directory is created.
#' @param overwrite Whether to replace this audit's existing output directory.
#' @param quiet Suppress progress messages.
#' @return A named list with `audit_id`, `status`, field tables, metadata, and
#'   `output_dir`. The result has class `bcapture_extraction`.
#' @details Python and the `pypdf` package are required and are initialized
#'   lazily only when this function is invoked. Flattened or scanned PDFs with
#'   no AcroForm fields fail with `failure_type = "no_acroform_fields"` in a
#'   batch manifest; OCR and handwriting extraction are not implemented.
#' @export
extract_hpai_file <- function(pdf_file, out_dir, overwrite = FALSE, quiet = FALSE) {
  pdf_file <- validate_scalar_path(pdf_file, "pdf_file")
  out_dir <- validate_scalar_path(out_dir, "out_dir")
  if (!file.exists(pdf_file)) stop("`pdf_file` does not exist.", call. = FALSE)
  if (tolower(fs::path_ext(pdf_file)) != "pdf") stop("`pdf_file` must have a .pdf extension.", call. = FALSE)
  if (dir.exists(out_dir) == FALSE && file.exists(out_dir)) stop("`out_dir` is an existing file, not a directory.", call. = FALSE)
  if (dir.exists(out_dir) == FALSE) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  result <- .extract_hpai_file_impl(pdf_file, out_dir, basename(pdf_file), overwrite, quiet)
  if (!identical(result$status, "success")) stop(result$error %||% "PDF extraction failed.", call. = FALSE)
  result <- .set_schema_group(result, "schema_001", out_dir)
  class(result) <- c("bcapture_extraction", "list")
  result
}

#' Extract HPAI BCAP PDF forms
#'
#' Extracts structured AcroForm field values from completed HPAI Biosecurity
#' Compliance Audit Program PDF forms in a directory.
#'
#' @param in_dir Directory containing PDF forms.
#' @param out_dir Directory in which per-audit and combined products are written.
#' @param recursive Search for PDFs below `in_dir` when `TRUE`.
#' @param overwrite Replace output directories corresponding to this input batch.
#' @param diagnostics Automatically compare successful PDF schemas and write
#'   diagnostic products.
#' @param quiet Suppress progress messages.
#' @return Invisibly, a tibble with one extraction-manifest row per input PDF.
#' @details Python and the `pypdf` package are required and initialized lazily.
#'   Only interactive AcroForm PDFs are supported. Flattened or scanned PDFs
#'   are recorded as failures and do not generate empty successful records.
#'   One failed PDF does not stop the remaining batch. The output directory is
#'   rebuilt from successful extractions in the current invocation.
#' @examples
#' \dontrun{
#' result <- extract_hpai("completed_audits", "bcapture_output")
#' }
#' @export
extract_hpai <- function(in_dir, out_dir, recursive = FALSE, overwrite = FALSE, diagnostics = TRUE, quiet = FALSE) {
  in_dir <- validate_scalar_path(in_dir, "in_dir")
  out_dir <- validate_scalar_path(out_dir, "out_dir")
  if (!dir.exists(in_dir)) stop("`in_dir` does not exist or is not a directory.", call. = FALSE)
  if (file.exists(out_dir) && !dir.exists(out_dir)) stop("`out_dir` is an existing file, not a directory.", call. = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  pdfs <- list.files(in_dir, pattern = "\\.pdf$", recursive = isTRUE(recursive), full.names = TRUE, ignore.case = TRUE)
  pdfs <- sort(as.character(pdfs))
  if (length(pdfs) == 0L) stop("No PDF files were found in `in_dir`.", call. = FALSE)
  source_files <- fs::path_file(pdfs)
  audit_ids <- vapply(fs::path_ext_remove(source_files), sanitize_audit_id, character(1))
  duplicate_ids <- unique(audit_ids[duplicated(audit_ids)])
  if (length(duplicate_ids) > 0L) {
    stop("Sanitized PDF filenames produce duplicate audit IDs: ", paste(duplicate_ids, collapse = ", "), call. = FALSE)
  }
  target_dirs <- fs::path(out_dir, "audits", audit_ids)
  conflicts <- target_dirs[dir.exists(target_dirs)]
  if (length(conflicts) > 0L && !isTRUE(overwrite)) {
    stop("Output already exists for audit ID(s): ", paste(fs::path_file(conflicts), collapse = ", "), ". Set `overwrite = TRUE` to replace only these audit outputs.", call. = FALSE)
  }
  if (!quiet) cli::cli_inform("Extracting {length(pdfs)} PDF{?s}.")
  in_root <- normalizePath(in_dir, winslash = "/", mustWork = FALSE)
  source_relpaths <- vapply(pdfs, function(path) {
    normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
    prefix <- paste0(in_root, "/")
    if (startsWith(normalized, prefix)) substring(normalized, nchar(prefix) + 1L) else basename(normalized)
  }, character(1))
  results <- purrr::map2(pdfs, source_relpaths, ~ .extract_hpai_file_impl(.x, out_dir, .y, overwrite, quiet))
  manifest <- dplyr::bind_rows(purrr::map(results, `[[`, "manifest"))
  manifest$schema_group <- assign_schema_groups(manifest$form_schema_hash)
  results <- purrr::map(results, function(result) {
    if (identical(result$status, "success")) {
      group <- manifest$schema_group[match(result$audit_id, manifest$audit_id)]
      .set_schema_group(result, group, out_dir)
    } else {
      result
    }
  })
  manifest <- dplyr::bind_rows(purrr::map(results, `[[`, "manifest"))
  successful <- purrr::keep(results, ~ identical(.x$status, "success"))
  paths <- combined_output_paths(out_dir)
  dir.create(paths$dir, recursive = TRUE, showWarnings = FALSE)
  write_csv_utf8(bind_union_rows(purrr::map(successful, `[[`, "fields")), paths$fields_long)
  write_csv_utf8(bind_union_rows(purrr::map(successful, `[[`, "populated_fields")), paths$populated_fields_long)
  write_csv_utf8(bind_union_rows(purrr::map(successful, `[[`, "wide")), paths$audits_wide)
  write_csv_utf8(bind_union_rows(purrr::map(successful, `[[`, "metadata")), paths$metadata)
  write_csv_utf8(bind_union_rows(purrr::map(successful, `[[`, "widgets")), paths$widgets)
  write_csv_utf8(manifest, paths$manifest)
  if (isTRUE(diagnostics) && length(successful) > 0L) {
    diagnose_hpai(out_dir, write = TRUE, quiet = quiet)
  }
  invisible(manifest)
}

.extract_hpai_file_impl <- function(pdf_file, out_dir, source_relpath, overwrite, quiet) {
  source_file <- fs::path_file(pdf_file)
  audit_id <- sanitize_audit_id(fs::path_ext_remove(source_file))
  source_md5 <- source_checksum(pdf_file)
  extracted_at <- utc_now()
  base_manifest <- list(audit_id = audit_id, form_type = "bcap", source_file = source_file, source_relpath = as.character(source_relpath), source_md5 = source_md5, status = "failed", failure_type = NA_character_, error = NA_character_, number_of_pages = NA_integer_, number_of_fields = NA_integer_, number_of_widgets = NA_integer_, number_of_populated_fields = NA_integer_, form_schema_hash = NA_character_, schema_group = NA_character_, extraction_method = "acroform", pypdf_version = NA_character_, extracted_at_utc = extracted_at)
  tryCatch({
    module <- ensure_hpai_python()
    parsed <- reticulate::py_to_r(module$extract_form(pdf_file))
    if (!isTRUE(parsed$has_acroform_fields)) stop("No AcroForm fields were detected in the PDF.", call. = FALSE)
    fields <- field_rows_to_tibble(parsed$fields)
    fields <- dplyr::mutate(fields, audit_id = audit_id, source_file = source_file, source_relpath = as.character(source_relpath), source_md5 = source_md5, .before = 1)
    widgets <- widget_rows_to_tibble(parsed$widgets)
    widgets <- dplyr::mutate(widgets, audit_id = audit_id, source_file = source_file, source_relpath = as.character(source_relpath), source_md5 = source_md5, .before = 1)
    populated <- fields[fields$is_populated, , drop = FALSE]
    metadata_values <- c(list(audit_id = audit_id, form_type = "bcap", source_file = source_file, source_relpath = as.character(source_relpath), source_md5 = source_md5, number_of_pages = as.integer(parsed$number_of_pages), number_of_fields = nrow(fields), number_of_widgets = nrow(widgets), number_of_populated_fields = nrow(populated), form_schema_hash = as_optional_character(parsed$form_schema_hash), schema_group = NA_character_, extraction_method = "acroform", pypdf_version = hpai_python_version(module), extracted_at_utc = extracted_at), parsed$pdf_metadata %||% list())
    metadata <- as_single_row_tibble(metadata_values)
    wide_values <- c(list(audit_id = audit_id, source_file = source_file, source_relpath = as.character(source_relpath), source_md5 = source_md5, form_schema_hash = as_optional_character(parsed$form_schema_hash)), stats::setNames(as.list(fields$value), fields$field))
    wide <- as_single_row_tibble(wide_values)
    paths <- audit_output_paths(audit_id, out_dir)
    if (dir.exists(paths$dir) && isTRUE(overwrite)) unlink(paths$dir, recursive = TRUE, force = TRUE)
    if (dir.exists(paths$dir)) stop("Output directory already exists.", call. = FALSE)
    dir.create(paths$dir, recursive = TRUE, showWarnings = FALSE)
    write_csv_utf8(fields, paths$fields_long)
    write_csv_utf8(populated, paths$populated_fields_long)
    write_csv_utf8(wide, paths$fields_wide)
    write_csv_utf8(metadata, paths$metadata)
    write_csv_utf8(widgets, paths$widgets)
    manifest <- base_manifest
    manifest$status <- "success"
    manifest$number_of_pages <- as.integer(parsed$number_of_pages)
    manifest$number_of_fields <- nrow(fields)
    manifest$number_of_widgets <- nrow(widgets)
    manifest$number_of_populated_fields <- nrow(populated)
    manifest$form_schema_hash <- as_optional_character(parsed$form_schema_hash)
    manifest$pypdf_version <- hpai_python_version(module)
    list(audit_id = audit_id, status = "success", fields = fields, populated_fields = populated, wide = wide, metadata = metadata, widgets = widgets, output_dir = paths$dir, manifest = as_single_row_tibble(manifest))
  }, error = function(error) {
    manifest <- base_manifest
    manifest$failure_type <- failure_type_from_error(error)
    manifest$error <- conditionMessage(error)
    if (!quiet) cli::cli_alert_warning("{source_file}: {manifest$failure_type}")
    list(audit_id = audit_id, status = "failed", fields = NULL, populated_fields = NULL, wide = NULL, metadata = NULL, widgets = NULL, output_dir = NA_character_, error = manifest$error, manifest = as_single_row_tibble(manifest))
  })
}

.set_schema_group <- function(result, schema_group, out_dir) {
  if (!identical(result$status, "success")) return(result)
  result$metadata$schema_group <- schema_group
  result$manifest$schema_group <- schema_group
  paths <- audit_output_paths(result$audit_id, out_dir)
  write_csv_utf8(result$metadata, paths$metadata)
  result
}
