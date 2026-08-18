.epi_dictionary_root <- function(version) {
  if (!is.character(version) || length(version) != 1L || is.na(version) || !nzchar(version)) {
    stop("`version` must be a single dictionary version.", call. = FALSE)
  }
  project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repeat {
    description <- fs::path(project_root, "DESCRIPTION")
    if (file.exists(description) && any(grepl("^Package:\\s*bcapture\\s*$", readLines(description, warn = FALSE)))) break
    parent <- dirname(project_root)
    if (identical(parent, project_root)) {
      project_root <- NA_character_
      break
    }
    project_root <- parent
  }
  source_root <- if (is.na(project_root)) NA_character_ else fs::path(
    project_root, "inst", "extdata", "dictionaries", "initial_epi", version
  )
  root <- if (dir.exists(source_root)) source_root else system.file(
    "extdata", "dictionaries", "initial_epi", version, package = "bcapture"
  )
  if (!dir.exists(root)) {
    stop("Unsupported Initial Epi dictionary version: ", version, call. = FALSE)
  }
  root
}

.read_epi_dictionary_csv <- function(root, name) {
  path <- fs::path(root, name)
  if (!file.exists(path)) stop("Initial Epi dictionary is missing ", name, call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
}

#' Load a versioned Initial Epi semantic dictionary
#'
#' @param version Supported dictionary version, currently `"2024-05-28"`.
#' @return A list containing the manifest, field mappings, codebooks, table
#'   registry, and a stable hash of the complete dictionary bundle.
#' @keywords internal
load_epi_dictionary <- function(version = "2024-05-28") {
  root <- .epi_dictionary_root(version)
  manifest <- .read_epi_dictionary_csv(root, "manifest.csv")
  fields <- .read_epi_dictionary_csv(root, "fields.csv")
  codes <- .read_epi_dictionary_csv(root, "codes.csv")
  tables <- .read_epi_dictionary_csv(root, "tables.csv")
  dictionary <- list(manifest = manifest, fields = fields, codes = codes, tables = tables)
  dictionary$dictionary_hash <- digest::digest(dictionary, algo = "sha256", serialize = TRUE)
  validate_epi_dictionary(dictionary)
  dictionary
}

#' Load the codebook rows for a versioned Initial Epi dictionary
#'
#' @param version Supported dictionary version.
#' @return A tibble with codebook IDs, raw codes, labels, and display order.
#' @keywords internal
load_epi_codebook <- function(version = "2024-05-28") {
  load_epi_dictionary(version)$codes
}

#' Load the repeated-table registry for a versioned Initial Epi dictionary
#'
#' @param version Supported dictionary version.
#' @return A tibble describing registered repeated structures.
#' @keywords internal
load_epi_table_registry <- function(version = "2024-05-28") {
  load_epi_dictionary(version)$tables
}

validate_epi_dictionary <- function(dictionary = NULL, version = "2024-05-28") {
  if (is.null(dictionary)) dictionary <- load_epi_dictionary(version)
  manifest <- dictionary$manifest
  fields <- dictionary$fields
  codes <- dictionary$codes
  tables <- dictionary$tables
  expected <- as.integer(manifest$expected_logical_fields[[1L]])
  problems <- character()
  required_fields <- c(
    "raw_field", "alternative_name", "source_page", "section_id", "section_name",
    "question_id", "question_text", "field_role", "response_type", "data_type",
    "canonical_name", "table_name", "row_index", "row_label", "column_name", "codebook_id"
  )
  if (!identical(as.character(manifest$form_type[[1L]]), "initial_epi")) problems <- c(problems, "manifest form_type is not initial_epi")
  if (!identical(as.character(manifest$form_version[[1L]]), "2024-05-28")) problems <- c(problems, "manifest form_version is not 2024-05-28")
  if (nrow(fields) != expected || expected != 497L) problems <- c(problems, sprintf("expected 497 dictionary fields, found %d", nrow(fields)))
  if (anyDuplicated(fields$raw_field)) problems <- c(problems, "raw_field is not unique")
  missing_columns <- setdiff(required_fields, names(fields))
  if (length(missing_columns) > 0L) problems <- c(problems, paste("missing fields.csv columns:", paste(missing_columns, collapse = ", ")))
  if (length(missing_columns) == 0L) {
    if (any(is.na(fields$section_id) | !nzchar(fields$section_id))) problems <- c(problems, "dictionary fields missing section metadata")
    if (any(is.na(fields$question_id) | !nzchar(fields$question_id))) problems <- c(problems, "dictionary fields missing question metadata")
    if (any(is.na(fields$canonical_name) | !nzchar(fields$canonical_name))) problems <- c(problems, "dictionary fields missing canonical_name")
    allowed_data_types <- c("character", "date", "date_text", "numeric")
    if (any(is.na(fields$data_type) | !fields$data_type %in% allowed_data_types)) {
      problems <- c(problems, "dictionary fields contain unsupported data_type values")
    }
    scalar <- fields[is.na(fields$table_name) | !nzchar(fields$table_name), , drop = FALSE]
    if (anyDuplicated(scalar$canonical_name)) problems <- c(problems, "scalar canonical_name is not unique")
    table_fields <- fields[!is.na(fields$table_name) & nzchar(fields$table_name), , drop = FALSE]
    if (nrow(table_fields) > 0L && (any(is.na(table_fields$row_index)) || any(is.na(table_fields$column_name) | !nzchar(table_fields$column_name)))) {
      problems <- c(problems, "table cells require row_index and column_name")
    }
    if (any(!is.na(fields$table_name) & nzchar(fields$table_name) & !fields$table_name %in% tables$table_name)) {
      problems <- c(problems, "dictionary references an unregistered table")
    }
  }
  codebook_ids <- unique(fields$codebook_id[!is.na(fields$codebook_id) & nzchar(fields$codebook_id)])
  if (length(setdiff(codebook_ids, unique(codes$codebook_id))) > 0L) problems <- c(problems, "dictionary references a missing codebook")
  if (length(problems) > 0L) stop("Initial Epi dictionary validation failed: ", paste(problems, collapse = "; "), call. = FALSE)
  invisible(list(expected_fields = expected, mapped_fields = nrow(fields), unknown_template_fields = 0L, duplicate_mappings = 0L, dictionary_hash = dictionary$dictionary_hash))
}
