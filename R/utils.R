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

sanitize_form_id <- function(stem) {
  id <- gsub("[<>:\"/\\\\|?*]", "_", stem, perl = TRUE)
  id <- sub("[ .]+$", "", id, perl = TRUE)
  if (!nzchar(id)) stop("The PDF filename produces an empty form_id after sanitization.", call. = FALSE)
  id
}

source_checksum <- function(path) unname(as.character(tools::md5sum(path)))

source_sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) return(digest::digest(file = path, algo = "sha256"))
  as.character(unname(tools::md5sum(path)))
}

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
    field_flags = integer(), value_raw = character(), value = character(),
    default_value_raw = character(), default_value = character(), is_default_value = logical(),
    states = character(), options = character(), is_multiselect = logical(),
    is_populated = logical(), extraction_method = character()
  )
}

empty_widget_table <- function() {
  tibble::tibble(
    page = integer(), widget_index = integer(), field_name = character(),
    full_field_name = character(), parent_field_name = character(),
    field_type = character(), rect_x1 = double(), rect_y1 = double(),
    rect_x2 = double(), rect_y2 = double(), appearance_state = character(),
    value = character(), parent_value = character(), states = character(),
    field_flags = integer(), options = character()
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
      field_flags = as.integer(row$field_flags %||% NA_integer_),
      value_raw = as_optional_character(row$value_raw %||% NA_character_),
      value = as_optional_character(row$value %||% NA_character_),
      default_value_raw = as_optional_character(row$default_value_raw %||% NA_character_),
      default_value = as_optional_character(row$default_value %||% NA_character_),
      is_default_value = isTRUE(row$is_default_value),
      states = as_optional_character(row$states %||% NA_character_),
      options = as_optional_character(row$options %||% NA_character_),
      is_multiselect = isTRUE(row$is_multiselect),
      is_populated = isTRUE(row$is_populated), extraction_method = "acroform"
    )
  })
}

widget_rows_to_tibble <- function(rows) {
  if (length(rows) == 0L) return(empty_widget_table())
  purrr::map_dfr(rows, function(row) {
    tibble::tibble(
      page = as.integer(row$page %||% NA_integer_),
      widget_index = as.integer(row$widget_index %||% NA_integer_),
      field_name = as_optional_character(row$field_name %||% NA_character_),
      full_field_name = as_optional_character(row$full_field_name %||% NA_character_),
      parent_field_name = as_optional_character(row$parent_field_name %||% NA_character_),
      field_type = as_optional_character(row$field_type %||% NA_character_),
      rect_x1 = as.numeric(row$rect_x1 %||% NA_real_),
      rect_y1 = as.numeric(row$rect_y1 %||% NA_real_),
      rect_x2 = as.numeric(row$rect_x2 %||% NA_real_),
      rect_y2 = as.numeric(row$rect_y2 %||% NA_real_),
      appearance_state = as_optional_character(row$appearance_state %||% NA_character_),
      value = as_optional_character(row$value %||% NA_character_),
      parent_value = as_optional_character(row$parent_value %||% NA_character_),
      states = as_optional_character(row$states %||% NA_character_)
      , field_flags = as.integer(row$field_flags %||% NA_integer_),
      options = as_optional_character(row$options %||% NA_character_)
    )
  })
}

normalize_state_set <- function(states) {
  if (length(states) == 0L || is.na(states[[1L]]) || !nzchar(states[[1L]])) return(character())
  values <- unlist(strsplit(as.character(states[[1L]]), "\\|", fixed = FALSE), use.names = FALSE)
  values <- sub("^/", "", trimws(values))
  sort(unique(values[nzchar(values)]))
}

normalize_option_set <- function(options) {
  if (length(options) == 0L || is.na(options[[1L]]) || !nzchar(options[[1L]])) return(character())
  values <- unlist(strsplit(as.character(options[[1L]]), "\\|", fixed = FALSE), use.names = FALSE)
  sort(unique(trimws(values[nzchar(trimws(values))])))
}

option_set_to_string <- function(options) paste(normalize_option_set(options), collapse = "|")

state_set_to_string <- function(states) paste(normalize_state_set(states), collapse = "|")

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
  if (grepl("unexpected form type|initial epi form signature", message, ignore.case = TRUE)) return("unexpected_form_type")
  "unknown_error"
}
