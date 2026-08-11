`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

utc_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC", usetz = FALSE)
}

as_optional_character <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) return(NA_character_)
  as.character(x[[1L]])
}

sanitize_audit_id <- function(stem) {
  id <- gsub("[<>:\"/\\\\|?*]", "_", stem, perl = TRUE)
  id <- sub("[ .]+$", "", id, perl = TRUE)
  if (!nzchar(id)) stop("The PDF filename produces an empty audit_id after sanitization.", call. = FALSE)
  id
}

source_checksum <- function(path) unname(as.character(tools::md5sum(path)))

validate_scalar_path <- function(path, argument) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop(sprintf("`%s` must be a single non-empty path.", argument), call. = FALSE)
  }
  path.expand(path)
}

empty_field_table <- function() {
  tibble::tibble(
    audit_id = character(), source_file = character(), source_relpath = character(),
    source_md5 = character(), field_index = integer(), page = integer(),
    field = character(), alternative_name = character(), field_type = character(),
    value_raw = character(), value = character(), states = character(),
    is_populated = logical(), extraction_method = character()
  )
}

field_rows_to_tibble <- function(rows) {
  if (length(rows) == 0L) return(empty_field_table())
  purrr::map_dfr(rows, function(row) {
    tibble::tibble(
      field_index = as.integer(row$field_index %||% NA_integer_),
      page = as.integer(row$page %||% NA_integer_),
      field = as_optional_character(row$field %||% NA_character_),
      alternative_name = as_optional_character(row$alternative_name %||% NA_character_),
      field_type = as_optional_character(row$field_type %||% NA_character_),
      value_raw = as_optional_character(row$value_raw %||% NA_character_),
      value = as_optional_character(row$value %||% NA_character_),
      states = as_optional_character(row$states %||% NA_character_),
      is_populated = isTRUE(row$is_populated), extraction_method = "acroform"
    )
  })
}

as_single_row_tibble <- function(values) {
  values <- values[!vapply(values, is.function, logical(1))]
  tibble::as_tibble(values, .name_repair = "minimal")
}

write_csv_utf8 <- function(x, path) {
  readr::write_csv(x, path, na = "", append = FALSE)
  invisible(path)
}

bind_union_rows <- function(tables) {
  tables <- purrr::compact(tables)
  if (length(tables) == 0L) tibble::tibble() else dplyr::bind_rows(tables)
}

failure_type_from_error <- function(error) {
  message <- conditionMessage(error)
  if (grepl("no AcroForm fields", message, ignore.case = TRUE)) return("no_acroform_fields")
  if (grepl("cannot read|invalid pdf|PdfReader|pypdf", message, ignore.case = TRUE)) return("pdf_read_error")
  if (grepl("output", message, ignore.case = TRUE)) return("output_exists")
  "unknown_error"
}
