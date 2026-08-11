epi_placeholder_values <- c("Select or Type", "Select (Ctrl for multi)")

is_initial_epi_form <- function(fields) {
  names <- if (is.data.frame(fields)) fields$field else fields
  names <- as.character(names)
  all(vapply(c("premid", "p0001", "p0100", "p0300"), function(anchor) {
    anchor %in% names || any(grepl(paste0("^", anchor, "[a-d]$"), names))
  }, logical(1)))
}

validate_epi_signature <- function(fields, warn_optional = FALSE) {
  names <- if (is.data.frame(fields)) fields$field else fields
  names <- unique(as.character(names))
  core <- c("premid", "p0001", "p0100", "p0300")
  additional <- c("premname", "p0178", "p0328")
  has_anchor <- function(anchor) anchor %in% names || any(grepl(paste0("^", anchor, "[a-d]$"), names))
  missing_core <- core[!vapply(core, has_anchor, logical(1))]
  missing_additional <- additional[!vapply(additional, has_anchor, logical(1))]
  valid <- length(missing_core) == 0L
  if (!valid) stop(
    "Unexpected form type: initial Epi form signature is missing core anchor field(s): ",
    paste(missing_core, collapse = ", "), call. = FALSE
  )
  if (isTRUE(warn_optional) && length(missing_additional) > 0L) {
    warning("Initial Epi form signature is missing optional anchor field(s): ", paste(missing_additional, collapse = ", "), call. = FALSE)
  }
  list(valid = valid, missing_core = missing_core, missing_additional = missing_additional)
}

.epi_placeholder <- function(value) {
  value <- trimws(as.character(value %||% NA_character_))
  !is.na(value) && value %in% epi_placeholder_values
}

.epi_fields <- function(fields) {
  fields$is_placeholder <- vapply(fields$value, .epi_placeholder, logical(1))
  fields$is_populated <- !is.na(fields$value) & nzchar(trimws(fields$value)) &
    fields$value != "Off" & !fields$is_placeholder
  fields
}

epi_output_paths <- function(form_id, out_dir) {
  form_dir <- fs::path(out_dir, "forms", form_id)
  list(
    dir = form_dir,
    fields_long = fs::path(form_dir, paste0(form_id, "_fields_long.csv")),
    populated_fields_long = fs::path(form_dir, paste0(form_id, "_populated_fields_long.csv")),
    fields_wide = fs::path(form_dir, paste0(form_id, "_fields_wide.csv")),
    metadata = fs::path(form_dir, paste0(form_id, "_metadata.csv")),
    widgets = fs::path(form_dir, paste0(form_id, "_widgets.csv"))
  )
}

epi_combined_output_paths <- function(out_dir) {
  combined <- fs::path(out_dir, "combined")
  list(
    dir = combined,
    fields_long = fs::path(combined, "epi_fields_long.csv"),
    populated_fields_long = fs::path(combined, "epi_populated_fields_long.csv"),
    forms_wide = fs::path(combined, "epi_forms_wide.csv"),
    metadata = fs::path(combined, "epi_metadata.csv"),
    widgets = fs::path(combined, "epi_widgets.csv"),
    inventory = fs::path(combined, "epi_field_inventory.csv"),
    manifest = fs::path(out_dir, "extraction_manifest.csv")
  )
}

#' Extract one Initial Epidemiological Interview PDF
#'
#' @param pdf_file Path to one interactive Initial Epi Interview PDF.
#' @param out_dir Directory in which the `forms/` output directory is created.
#' @param overwrite Whether to replace this form's existing output directory.
#' @param quiet Suppress progress messages.
#' @return A named extraction result.
#' @export
extract_epi_file <- function(pdf_file, out_dir, overwrite = FALSE, quiet = FALSE) {
  pdf_file <- validate_scalar_path(pdf_file, "pdf_file")
  out_dir <- validate_scalar_path(out_dir, "out_dir")
  if (!file.exists(pdf_file)) stop("`pdf_file` does not exist.", call. = FALSE)
  if (tolower(fs::path_ext(pdf_file)) != "pdf") stop("`pdf_file` must have a .pdf extension.", call. = FALSE)
  if (file.exists(out_dir) && !dir.exists(out_dir)) stop("`out_dir` is an existing file, not a directory.", call. = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  result <- .extract_epi_file_impl(pdf_file, out_dir, basename(pdf_file), overwrite, quiet)
  if (!identical(result$status, "success")) stop(result$error %||% "Initial Epi extraction failed.", call. = FALSE)
  result <- .set_epi_schema_group(result, "schema_001", out_dir)
  class(result) <- c("bcapture_extraction", "list")
  result
}

#' Extract Initial Epidemiological Interview PDFs
#'
#' @param in_dir Directory containing Initial Epi Interview PDFs.
#' @param out_dir Directory in which per-form and combined products are written.
#' @param recursive Search below `in_dir` when `TRUE`.
#' @param overwrite Replace existing form outputs.
#' @param diagnostics Automatically write normalized schema diagnostics.
#' @param quiet Suppress progress messages.
#' @return Invisibly, an extraction manifest.
#' @export
extract_epi <- function(in_dir, out_dir, recursive = FALSE, overwrite = FALSE, diagnostics = TRUE, quiet = FALSE) {
  in_dir <- validate_scalar_path(in_dir, "in_dir")
  out_dir <- validate_scalar_path(out_dir, "out_dir")
  if (!dir.exists(in_dir)) stop("`in_dir` does not exist or is not a directory.", call. = FALSE)
  if (file.exists(out_dir) && !dir.exists(out_dir)) stop("`out_dir` is an existing file, not a directory.", call. = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  pdfs <- sort(list.files(in_dir, pattern = "\\.pdf$", recursive = isTRUE(recursive), full.names = TRUE, ignore.case = TRUE))
  if (length(pdfs) == 0L) stop("No PDF files were found in `in_dir`.", call. = FALSE)
  source_files <- fs::path_file(pdfs)
  form_ids <- vapply(fs::path_ext_remove(source_files), sanitize_form_id, character(1))
  if (anyDuplicated(form_ids)) stop("Sanitized PDF filenames produce duplicate form IDs: ", paste(unique(form_ids[duplicated(form_ids)]), collapse = ", "), call. = FALSE)
  target_dirs <- fs::path(out_dir, "forms", form_ids)
  conflicts <- target_dirs[dir.exists(target_dirs)]
  if (length(conflicts) > 0L && !isTRUE(overwrite)) stop("Output already exists for form ID(s): ", paste(fs::path_file(conflicts), collapse = ", "), ". Set `overwrite = TRUE` to replace only these form outputs.", call. = FALSE)
  in_root <- normalizePath(in_dir, winslash = "/", mustWork = FALSE)
  source_relpaths <- vapply(pdfs, function(path) {
    normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
    prefix <- paste0(in_root, "/")
    if (startsWith(normalized, prefix)) substring(normalized, nchar(prefix) + 1L) else basename(normalized)
  }, character(1))
  if (!quiet) cli::cli_inform("Extracting {length(pdfs)} Initial Epi PDF{?s}.")
  results <- purrr::map2(pdfs, source_relpaths, ~ .extract_epi_file_impl(.x, out_dir, .y, overwrite, quiet))
  manifest <- dplyr::bind_rows(purrr::map(results, `[[`, "manifest"))
  manifest$schema_group <- assign_schema_groups(manifest$form_schema_hash)
  results <- purrr::map(results, function(result) {
    if (identical(result$status, "success")) {
      group <- manifest$schema_group[match(result$form_id, manifest$form_id)]
      .set_epi_schema_group(result, group, out_dir)
    } else result
  })
  manifest <- dplyr::bind_rows(purrr::map(results, `[[`, "manifest"))
  successful <- purrr::keep(results, ~ identical(.x$status, "success"))
  paths <- epi_combined_output_paths(out_dir)
  dir.create(paths$dir, recursive = TRUE, showWarnings = FALSE)
  write_csv_utf8(bind_union_rows(purrr::map(successful, `[[`, "fields")), paths$fields_long)
  write_csv_utf8(bind_union_rows(purrr::map(successful, `[[`, "populated_fields")), paths$populated_fields_long)
  write_csv_utf8(bind_union_rows(purrr::map(successful, `[[`, "wide")), paths$forms_wide)
  write_csv_utf8(bind_union_rows(purrr::map(successful, `[[`, "metadata")), paths$metadata)
  write_csv_utf8(bind_union_rows(purrr::map(successful, `[[`, "widgets")), paths$widgets)
  write_csv_utf8(manifest, paths$manifest)
  if (length(successful) > 0L) {
    fields <- bind_union_rows(purrr::map(successful, `[[`, "fields"))
    metadata_for_inventory <- bind_union_rows(purrr::map(successful, `[[`, "metadata")) |>
      dplyr::select(form_id, schema_group)
    fields <- dplyr::left_join(fields, metadata_for_inventory, by = "form_id")
    inventory <- fields |>
      dplyr::group_by(field, alternative_name, field_type, states, options, default_value, is_multiselect) |>
      dplyr::summarise(pages_seen = paste(sort(unique(page)), collapse = "|"), schema_groups_seen = paste(sort(unique(schema_group)), collapse = "|"), .groups = "drop")
    write_csv_utf8(inventory, paths$inventory)
  }
  if (isTRUE(diagnostics) && length(successful) > 0L) diagnose_epi(out_dir, write = TRUE, quiet = quiet)
  invisible(manifest)
}

.extract_epi_file_impl <- function(pdf_file, out_dir, source_relpath, overwrite, quiet) {
  source_file <- fs::path_file(pdf_file)
  form_id <- sanitize_form_id(fs::path_ext_remove(source_file))
  source_hash <- source_sha256(pdf_file)
  extracted_at <- utc_now()
  base_manifest <- list(form_id = form_id, form_type = "initial_epi", source_file = source_file, source_relpath = as.character(source_relpath), source_sha256 = source_hash, status = "failed", failure_type = NA_character_, error = NA_character_, number_of_pages = NA_integer_, number_of_fields = NA_integer_, number_of_widgets = NA_integer_, number_of_populated_fields = NA_integer_, form_schema_hash = NA_character_, schema_group = NA_character_, extraction_method = "acroform", pypdf_version = NA_character_, extracted_at_utc = extracted_at)
  tryCatch({
    module <- ensure_acroform_python()
    parsed <- reticulate::py_to_r(module$extract_form(pdf_file))
    if (!isTRUE(parsed$has_acroform_fields)) stop("No AcroForm fields were detected in the PDF.", call. = FALSE)
    fields <- .epi_fields(field_rows_to_tibble(parsed$fields))
    validate_epi_signature(fields, warn_optional = FALSE)
    fields <- dplyr::mutate(fields, form_id = form_id, form_type = "initial_epi", source_file = source_file, source_relpath = as.character(source_relpath), source_sha256 = source_hash, .before = 1)
    widgets <- widget_rows_to_tibble(parsed$widgets) |>
      dplyr::mutate(form_id = form_id, form_type = "initial_epi", source_file = source_file, source_relpath = as.character(source_relpath), source_sha256 = source_hash, .before = 1)
    populated <- fields[fields$is_populated %in% TRUE, , drop = FALSE]
    choice <- fields$field_type == "Ch"
    metadata_values <- c(list(form_id = form_id, form_type = "initial_epi", source_file = source_file, source_relpath = as.character(source_relpath), source_sha256 = source_hash, number_of_pages = as.integer(parsed$number_of_pages), number_of_fields = nrow(fields), number_of_widgets = nrow(widgets), number_of_populated_fields = nrow(populated), number_of_choice_fields = sum(choice, na.rm = TRUE), number_of_multiselect_fields = sum(fields$is_multiselect %in% TRUE, na.rm = TRUE), form_schema_hash = as_optional_character(parsed$form_schema_hash), schema_group = NA_character_, extraction_method = "acroform", pypdf_version = hpai_python_version(module), extracted_at_utc = extracted_at), parsed$pdf_metadata %||% list())
    metadata <- as_single_row_tibble(metadata_values)
    wide_values <- c(list(form_id = form_id, form_type = "initial_epi", source_file = source_file, source_relpath = as.character(source_relpath), source_sha256 = source_hash, form_schema_hash = as_optional_character(parsed$form_schema_hash), schema_group = NA_character_), stats::setNames(as.list(fields$value), fields$field))
    wide <- as_single_row_tibble(wide_values)
    paths <- epi_output_paths(form_id, out_dir)
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
    list(form_id = form_id, status = "success", fields = fields, populated_fields = populated, wide = wide, metadata = metadata, widgets = widgets, output_dir = paths$dir, manifest = as_single_row_tibble(manifest))
  }, error = function(error) {
    manifest <- base_manifest
    manifest$failure_type <- failure_type_from_error(error)
    manifest$error <- conditionMessage(error)
    if (!quiet) cli::cli_alert_warning("{source_file}: {manifest$failure_type}")
    list(form_id = form_id, status = "failed", fields = NULL, populated_fields = NULL, wide = NULL, metadata = NULL, widgets = NULL, output_dir = NA_character_, error = manifest$error, manifest = as_single_row_tibble(manifest))
  })
}

.set_epi_schema_group <- function(result, schema_group, out_dir) {
  if (!identical(result$status, "success")) return(result)
  result$metadata$schema_group <- schema_group
  result$manifest$schema_group <- schema_group
  paths <- epi_output_paths(result$form_id, out_dir)
  write_csv_utf8(result$metadata, paths$metadata)
  result
}
