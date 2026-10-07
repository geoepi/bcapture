.phase3_version <- "phase3-1"

utils::globalVariables(c("entity_id", "entity_scope", "metric", "product"))
.phase3_epi_version <- "2024-05-28"
.phase3_bcap_versions <- "2025-12-08"

.phase3_nonmissing <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

.phase3_read_csv <- function(path, label) {
  if (!file.exists(path)) stop("Required ", label, " is missing: ", fs::path_file(path), call. = FALSE)
  data <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"),
    name_repair = "minimal",
    progress = FALSE
  )
  if (anyDuplicated(names(data))) stop(label, " contains duplicate columns.", call. = FALSE)
  data
}

.phase3_require_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  unexpected <- setdiff(names(data), required)
  if (length(missing) > 0L) stop(label, " is missing required canonical columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (length(unexpected) > 0L) stop(label, " contains unexpected columns: ", paste(unexpected, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

.phase3_required_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) stop(label, " is missing required canonical columns: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

.phase3_parse_numeric <- function(x, label) {
  x <- trimws(as.character(x))
  missing <- is.na(x) | !nzchar(x)
  valid <- missing | grepl("^[+-]?(?:(?:(?:[0-9]+|[0-9]{1,3}(,[0-9]{3})+)(?:[.][0-9]+)?|[.][0-9]+)(?:[eE][+-]?[0-9]+)?)$", x, perl = TRUE)
  if (any(!valid)) stop(label, " contains an unparseable numeric value; schema/type drift is blocking.", call. = FALSE)
  result <- rep(NA_real_, length(x))
  result[!missing] <- as.numeric(gsub(",", "", x[!missing], fixed = TRUE))
  result
}

.phase3_parse_integer <- function(x, label) {
  x <- trimws(as.character(x))
  missing <- is.na(x) | !nzchar(x)
  valid <- missing | grepl("^[+-]?[0-9]+$", x, perl = TRUE)
  if (any(!valid)) stop(label, " contains an unparseable integer value; schema/type drift is blocking.", call. = FALSE)
  result <- rep(NA_integer_, length(x))
  result[!missing] <- as.integer(x[!missing])
  result
}

.phase3_parse_date_one <- function(value) {
  value <- trimws(as.character(value))
  if (is.na(value) || !nzchar(value)) return(as.Date(NA))
  formats <- c(
    "%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%m-%d-%Y",
    "%m/%d/%y", "%m-%d-%y"
  )
  patterns <- c(
    "^\\d{4}-\\d{1,2}-\\d{1,2}$", "^\\d{4}/\\d{1,2}/\\d{1,2}$",
    "^\\d{1,2}/\\d{1,2}/\\d{4}$", "^\\d{1,2}-\\d{1,2}-\\d{4}$",
    "^\\d{1,2}/\\d{1,2}/\\d{2}$", "^\\d{1,2}-\\d{1,2}-\\d{2}$"
  )
  for (i in seq_along(formats)) {
    if (!grepl(patterns[[i]], value, perl = TRUE)) next
    parsed <- suppressWarnings(as.Date(value, format = formats[[i]]))
    if (!is.na(parsed)) return(parsed)
  }
  as.Date(NA)
}

.phase3_parse_date <- function(x, label) {
  result <- as.Date(rep(NA_character_, length(x)))
  values <- as.character(x)
  for (i in seq_along(values)) result[[i]] <- .phase3_parse_date_one(values[[i]])
  nonmissing <- .phase3_nonmissing(values)
  if (any(nonmissing & is.na(result))) stop(label, " contains an unparseable date; schema/type drift is blocking.", call. = FALSE)
  result
}

.phase3_parse_logical_if_boolean <- function(x) {
  values <- trimws(as.character(x))
  nonmissing <- .phase3_nonmissing(values)
  if (!any(nonmissing)) return(as.character(x))
  normalized <- tolower(values[nonmissing])
  if (!all(normalized %in% c("true", "false", "0", "1"))) return(as.character(x))
  result <- rep(NA, length(values))
  result[nonmissing] <- normalized %in% c("true", "1")
  result
}

.phase3_safe_path <- function(path, argument) {
  path <- validate_scalar_path(path, argument)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

.phase3_path_within <- function(child, parent) {
  child <- sub("/+$", "", normalizePath(child, winslash = "/", mustWork = FALSE))
  parent <- sub("/+$", "", normalizePath(parent, winslash = "/", mustWork = FALSE))
  identical(child, parent) || startsWith(paste0(child, "/"), paste0(parent, "/"))
}

.phase3_git_root <- function(path) {
  current <- normalizePath(path, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(fs::path(current, ".git")) || dir.exists(fs::path(current, ".git"))) return(current)
    parent <- fs::path_dir(current)
    if (identical(parent, current)) break
    current <- parent
  }
  NA_character_
}

.phase3_package_version <- function() {
  value <- tryCatch(as.character(utils::packageVersion("bcapture")), error = function(e) NA_character_)
  if (!is.na(value)) return(value)
  description <- fs::path("DESCRIPTION")
  if (file.exists(description)) {
    line <- grep("^Version:", readLines(description, warn = FALSE), value = TRUE)
    if (length(line) == 1L) return(trimws(sub("^Version:", "", line)))
  }
  NA_character_
}

.phase3_git_sha <- function() {
  root <- .phase3_git_root(getwd())
  if (is.na(root)) return(NA_character_)
  result <- tryCatch(
    system2("git", c("-c", paste0("safe.directory=", root), "-C", root, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  result <- trimws(as.character(result))
  if (length(result) == 1L && grepl("^[0-9a-f]{40}$", result, ignore.case = TRUE)) result else NA_character_
}

.phase3_input_status <- function(directory, form_type) {
  if (!dir.exists(directory)) stop(form_type, " safe output directory does not exist.", call. = FALSE)
  if (identical(form_type, "epi")) {
    manifest <- .phase3_read_csv(fs::path(directory, "privacy", "deidentification_manifest.csv"), "Epi deidentification manifest")
    validation <- .phase3_read_csv(fs::path(directory, "validation", "validation_summary.csv"), "Epi validation summary")
    leaks <- .phase3_read_csv(fs::path(directory, "privacy", "privacy_leak_audit.csv"), "Epi privacy audit")
    .phase3_required_columns(manifest, c("status", "forms_processed", "privacy_errors", "privacy_warnings"), "Epi deidentification manifest")
    .phase3_required_columns(validation, c("severity", "n_findings"), "Epi validation summary")
    .phase3_required_columns(leaks, c("severity"), "Epi privacy audit")
    semantic_errors <- sum(.phase3_parse_numeric(validation$n_findings, "Epi validation finding counts")[validation$severity == "ERROR"], na.rm = TRUE)
    semantic_warnings <- sum(.phase3_parse_numeric(validation$n_findings, "Epi validation finding counts")[validation$severity == "WARNING"], na.rm = TRUE)
    privacy_errors <- sum(.phase3_parse_numeric(manifest$privacy_errors, "Epi privacy error count"), na.rm = TRUE)
    privacy_warnings <- sum(.phase3_parse_numeric(manifest$privacy_warnings, "Epi privacy warning count"), na.rm = TRUE)
    privacy_errors <- privacy_errors + sum(leaks$severity == "ERROR")
    privacy_warnings <- privacy_warnings + sum(leaks$severity == "WARNING")
    if (nrow(manifest) != 1L || !identical(as.character(manifest$status[[1L]]), "passed")) stop("Epi safe output has not passed de-identification.", call. = FALSE)
    if (semantic_errors > 0L) stop("Epi safe output has blocking semantic validation errors.", call. = FALSE)
    if (privacy_errors > 0L || privacy_warnings > 0L) stop("Epi safe output has blocking privacy findings.", call. = FALSE)
    list(semantic_status = if (semantic_warnings > 0L) "review" else "passed", semantic_errors = semantic_errors, semantic_warnings = semantic_warnings, privacy_status = "passed", privacy_errors = privacy_errors, privacy_warnings = privacy_warnings)
  } else {
    manifest <- .phase3_read_csv(fs::path(directory, "privacy", "deidentification_manifest.csv"), "BCAP deidentification manifest")
    validation <- .phase3_read_csv(fs::path(directory, "validation", "validation_summary.csv"), "BCAP validation summary")
    privacy <- .phase3_read_csv(fs::path(directory, "privacy", "privacy_audit_summary.csv"), "BCAP privacy summary")
    leaks <- .phase3_read_csv(fs::path(directory, "privacy", "privacy_leak_audit.csv"), "BCAP privacy audit")
    .phase3_required_columns(manifest, c("status", "records_processed", "privacy_errors", "privacy_warnings"), "BCAP deidentification manifest")
    .phase3_required_columns(validation, c("status", "error_count", "warning_count"), "BCAP validation summary")
    .phase3_required_columns(privacy, c("status", "error_count", "warning_count"), "BCAP privacy summary")
    .phase3_required_columns(leaks, c("severity"), "BCAP privacy audit")
    semantic_errors <- sum(.phase3_parse_numeric(validation$error_count, "BCAP semantic error count"), na.rm = TRUE)
    semantic_warnings <- sum(.phase3_parse_numeric(validation$warning_count, "BCAP semantic warning count"), na.rm = TRUE)
    privacy_errors <- sum(.phase3_parse_numeric(manifest$privacy_errors, "BCAP privacy error count"), na.rm = TRUE) + sum(.phase3_parse_numeric(privacy$error_count, "BCAP privacy error count"), na.rm = TRUE) + sum(leaks$severity == "ERROR")
    privacy_warnings <- sum(.phase3_parse_numeric(manifest$privacy_warnings, "BCAP privacy warning count"), na.rm = TRUE) + sum(.phase3_parse_numeric(privacy$warning_count, "BCAP privacy warning count"), na.rm = TRUE) + sum(leaks$severity == "WARNING")
    if (nrow(manifest) != 1L || !identical(as.character(manifest$status[[1L]]), "passed")) stop("BCAP safe output has not passed de-identification.", call. = FALSE)
    if (nrow(validation) != 1L || !identical(as.character(validation$status[[1L]]), "passed") || semantic_errors > 0L || semantic_warnings > 0L) stop("BCAP safe output has blocking semantic validation findings.", call. = FALSE)
    if (nrow(privacy) != 1L || !identical(as.character(privacy$status[[1L]]), "passed") || privacy_errors > 0L || privacy_warnings > 0L) stop("BCAP safe output has blocking privacy findings.", call. = FALSE)
    list(semantic_status = "passed", semantic_errors = semantic_errors, semantic_warnings = semantic_warnings, privacy_status = "passed", privacy_errors = privacy_errors, privacy_warnings = privacy_warnings)
  }
}

.phase3_epi_expected <- function(version) {
  dictionary <- load_epi_dictionary(version)
  scalar <- dictionary$fields[is.na(dictionary$fields$table_name) | !nzchar(dictionary$fields$table_name), , drop = FALSE]
  list(
    dictionary = dictionary,
    forms = c("case_id", "form_version", "dictionary_version", "dictionary_hash", "form_schema_hash", "schema_group", as.character(scalar$canonical_name)),
    responses = c("form_version", "dictionary_version", "dictionary_hash", "section_id", "section_name", "question_id", "subquestion_id", "raw_field", "canonical_name", "field_role", "response_type", "data_type", "raw_value", "value", "date_value", "numeric_value", "response_code", "response_label", "units", "source_page", "case_id"),
    multiselect = c("raw_field", "canonical_name", "item_code", "item_label", "case_id"),
    tables = stats::setNames(lapply(.epi_table_output_names, function(name) c(setdiff(names(.epi_table_type_contract(sub("^epi_", "", name), dictionary)), "form_id"), "case_id")), .epi_table_output_names)
  )
}

.phase3_epi_read <- function(directory) {
  paths <- fs::path(directory, "collated")
  expected <- .phase3_epi_expected(.phase3_epi_version)
  forms <- .phase3_read_csv(fs::path(paths, "epi_forms.csv"), "Epi forms")
  .phase3_require_columns(forms, expected$forms, "Epi forms")
  responses <- .phase3_read_csv(fs::path(paths, "epi_responses_long.csv"), "Epi response table")
  .phase3_require_columns(responses, expected$responses, "Epi response table")
  multiselect <- .phase3_read_csv(fs::path(paths, "epi_multiselect_responses.csv"), "Epi multiselect table")
  .phase3_require_columns(multiselect, expected$multiselect, "Epi multiselect table")
  tables <- list()
  for (name in names(expected$tables)) {
    table <- .phase3_read_csv(fs::path(paths, paste0(name, ".csv")), paste0("Epi ", name, " table"))
    .phase3_require_columns(table, expected$tables[[name]], paste0("Epi ", name, " table"))
    tables[[name]] <- table
  }
  versions <- unique(forms$form_version[.phase3_nonmissing(forms$form_version)])
  if (length(versions) != 1L || !identical(versions[[1L]], .phase3_epi_version)) stop("Unknown or mixed Initial Epi form version.", call. = FALSE)
  if (any(!.phase3_nonmissing(forms$schema_group))) stop("Epi safe output has a missing source schema version.", call. = FALSE)
  if (anyDuplicated(forms$case_id) || any(!.phase3_nonmissing(forms$case_id))) stop("Epi safe output has duplicate or missing record keys.", call. = FALSE)
  forms$record_id <- as.character(forms$case_id)
  forms$case_id <- NULL
  forms <- forms[, c("record_id", setdiff(names(forms), "record_id")), drop = FALSE]
  list(forms = forms, responses = responses, multiselect = multiselect, tables = tables, dictionary = expected$dictionary, n_records = nrow(forms))
}

.phase3_bcap_read <- function(directory) {
  fields <- .phase3_read_csv(fs::path(directory, "combined", "hpai_fields_long.csv"), "BCAP safe fields")
  metadata <- .phase3_read_csv(fs::path(directory, "combined", "hpai_metadata.csv"), "BCAP safe metadata")
  expected_fields <- c("record_id", "field_index", "page", "field", "field_type", "states", "options", "is_populated", "value", "privacy_class", "privacy_action", "identifier_class", "pseudonym_class")
  expected_metadata <- c("record_id", "form_type", "schema_group", "extraction_method", "template_family", "template_version", "number_of_fields", "safe_populated_fields")
  .phase3_require_columns(fields, expected_fields, "BCAP safe fields")
  .phase3_require_columns(metadata, expected_metadata, "BCAP safe metadata")
  versions <- unique(metadata$template_version[.phase3_nonmissing(metadata$template_version)])
  schema_groups <- unique(metadata$schema_group[.phase3_nonmissing(metadata$schema_group)])
  if (length(versions) > 0L && any(!versions %in% .phase3_bcap_versions)) stop("Unknown BCAP template version.", call. = FALSE)
  if (length(schema_groups) == 0L || any(!grepl("^schema_[0-9]+$", schema_groups))) stop("Unknown BCAP source schema version.", call. = FALSE)
  metadata$template_version[!.phase3_nonmissing(metadata$template_version)] <- metadata$schema_group[!.phase3_nonmissing(metadata$template_version)]
  if (any(!.phase3_nonmissing(metadata$schema_group))) stop("BCAP safe output has a missing source schema version.", call. = FALSE)
  if (anyDuplicated(metadata$record_id) || any(!.phase3_nonmissing(metadata$record_id))) stop("BCAP safe output has duplicate or missing record keys.", call. = FALSE)
  fields$field_index <- .phase3_parse_integer(fields$field_index, "BCAP field indexes")
  fields$page <- .phase3_parse_integer(fields$page, "BCAP field pages")
  fields$is_populated <- .phase3_parse_logical_if_boolean(fields$is_populated)
  fields$record_id <- as.character(fields$record_id)
  fields <- fields[order(fields$record_id, fields$field_index, fields$field), , drop = FALSE]
  if (length(setdiff(unique(fields$record_id), metadata$record_id)) > 0L) stop("BCAP safe fields contain orphan record IDs.", call. = FALSE)
  if (any(!metadata$record_id %in% unique(fields$record_id))) stop("BCAP safe metadata contains records with no safe fields.", call. = FALSE)
  list(fields = fields, metadata = metadata, n_records = nrow(metadata))
}

.phase3_crosswalk <- function(directory, form_type, record_ids) {
  if (is.null(directory)) return(NULL)
  directory <- .phase3_safe_path(directory, paste0(form_type, "_crosswalk_dir"))
  if (!dir.exists(directory)) stop(form_type, " crosswalk directory does not exist.", call. = FALSE)
  record_path <- fs::path(directory, "record_crosswalk.csv")
  if (!file.exists(record_path)) stop(form_type, " crosswalk is incomplete.", call. = FALSE)
  x <- .phase3_read_csv(record_path, paste0(form_type, " record crosswalk"))
  required <- if (identical(form_type, "epi")) {
    c("case_id", "source_key", "form_id", "source_sha256", "premises_pseudonym", "premises_key")
  } else {
    c("source_key", "record_id", "source_file", "source_md5", "premises_key", "premises_pseudonym")
  }
  .phase3_required_columns(x, required, paste0(form_type, " record crosswalk"))
  id_col <- if (identical(form_type, "epi")) "case_id" else "record_id"
  if (anyDuplicated(x[[id_col]]) || anyDuplicated(x$source_key)) stop(form_type, " crosswalk is not one-to-one.", call. = FALSE)
  if (!setequal(as.character(x[[id_col]]), as.character(record_ids))) stop(form_type, " crosswalk does not exactly cover the safe records.", call. = FALSE)
  x <- x[match(record_ids, x[[id_col]]), , drop = FALSE]
  with_key <- .phase3_nonmissing(x$premises_key)
  if (any(vapply(split(as.character(x$premises_pseudonym[with_key]), as.character(x$premises_key[with_key])), function(values) length(unique(values[.phase3_nonmissing(values)])) > 1L, logical(1)))) stop(form_type, " crosswalk has ambiguous premises mappings.", call. = FALSE)
  x
}

.phase3_epi_safe_entity <- function(forms) {
  if (!"premises_id" %in% names(forms)) stop("Epi safe output is missing the canonical premises key field.", call. = FALSE)
  values <- as.character(forms$premises_id)
  if (any(!.phase3_nonmissing(values))) stop("Epi safe output has records without a premises pseudonym.", call. = FALSE)
  if (any(!grepl("^PREMISES-[0-9]{6}$", values))) stop("Epi premises identifiers are not valid pseudonyms.", call. = FALSE)
  values
}

.phase3_bcap_safe_entity <- function(fields, record_ids) {
  candidate_fields <- c("npin", "premisesfarm_name", "prem_special_id")
  candidates <- fields[fields$field %in% candidate_fields & .phase3_nonmissing(fields$value), c("record_id", "value"), drop = FALSE]
  result <- stats::setNames(rep(NA_character_, length(record_ids)), record_ids)
  for (record_id in record_ids) {
    values <- unique(as.character(candidates$value[candidates$record_id == record_id]))
    values <- values[.phase3_nonmissing(values)]
    if (length(values) != 1L) stop("BCAP safe output has missing or ambiguous premises pseudonyms.", call. = FALSE)
    if (!grepl("^PREMISES-[0-9]{6}$", values[[1L]])) stop("BCAP premises identifiers are not valid pseudonyms.", call. = FALSE)
    result[[record_id]] <- values[[1L]]
  }
  unname(result)
}

.phase3_entity_map <- function(epi, bcap, epi_crosswalk, bcap_crosswalk) {
  epi_ids <- epi$forms$record_id
  bcap_ids <- bcap$metadata$record_id
  epi_safe <- .phase3_epi_safe_entity(epi$forms)
  bcap_safe <- .phase3_bcap_safe_entity(bcap$fields, bcap_ids)
  epi_pseudonym <- epi_safe
  bcap_pseudonym <- bcap_safe
  epi_key <- rep(NA_character_, length(epi_ids))
  bcap_key <- rep(NA_character_, length(bcap_ids))
  if (!is.null(epi_crosswalk)) {
    epi_pseudonym <- as.character(epi_crosswalk$premises_pseudonym)
    epi_key <- as.character(epi_crosswalk$premises_key)
    if (any(.phase3_nonmissing(epi_key) & (!.phase3_nonmissing(epi_pseudonym) | epi_pseudonym != epi_safe))) stop("Epi crosswalk and safe premises pseudonyms disagree.", call. = FALSE)
  }
  if (!is.null(bcap_crosswalk)) {
    bcap_pseudonym <- as.character(bcap_crosswalk$premises_pseudonym)
    bcap_key <- as.character(bcap_crosswalk$premises_key)
    if (any(.phase3_nonmissing(bcap_key) & (!.phase3_nonmissing(bcap_pseudonym) | bcap_pseudonym != bcap_safe))) stop("BCAP crosswalk and safe premises pseudonyms disagree.", call. = FALSE)
  }
  shared_keys <- if (!is.null(epi_crosswalk) && !is.null(bcap_crosswalk)) sort(intersect(unique(epi_key[.phase3_nonmissing(epi_key)]), unique(bcap_key[.phase3_nonmissing(bcap_key)]))) else character()
  shared_ids <- stats::setNames(sprintf("ENTITY-%06d", seq_along(shared_keys)), shared_keys)
  entity_for <- function(form_type, pseudonyms, keys, record_ids) {
    result <- character(length(record_ids))
    for (i in seq_along(record_ids)) {
      if (.phase3_nonmissing(keys[[i]]) && keys[[i]] %in% shared_keys) result[[i]] <- unname(shared_ids[[keys[[i]]]]) else result[[i]] <- paste0(toupper(form_type), "-", pseudonyms[[i]])
    }
    result
  }
  epi_entity <- entity_for("epi", epi_pseudonym, epi_key, epi_ids)
  bcap_entity <- entity_for("bcap", bcap_pseudonym, bcap_key, bcap_ids)
  records <- dplyr::bind_rows(
    tibble::tibble(record_id = epi_ids, form_type = "initial_epi", entity_id = epi_entity, event_id = NA_character_, entity_key = epi_key, entity_scope = ifelse(epi_key %in% shared_keys, "shared_authoritative_key", ifelse(is.null(epi_crosswalk), "safe_pseudonym", "private_crosswalk_key"))),
    tibble::tibble(record_id = bcap_ids, form_type = "bcap", entity_id = bcap_entity, event_id = NA_character_, entity_key = bcap_key, entity_scope = ifelse(bcap_key %in% shared_keys, "shared_authoritative_key", ifelse(is.null(bcap_crosswalk), "safe_pseudonym", "private_crosswalk_key")))
  )
  entities <- records |>
    dplyr::group_by(entity_id) |>
    dplyr::summarise(
      entity_type = "premises",
      linkage_scope = ifelse(any(entity_scope == "shared_authoritative_key"), "shared_authoritative_key", "family_scoped_pseudonym"),
      n_records = dplyr::n(), n_epi_records = sum(form_type == "initial_epi"), n_bcap_records = sum(form_type == "bcap"),
      multiple_bcap_per_entity = sum(form_type == "bcap") > 1L, .groups = "drop"
    ) |>
    dplyr::arrange(entity_id)
  list(records = records, entities = entities, shared_keys = shared_keys)
}

.phase3_normalize_epi_forms <- function(forms, dictionary, responses = NULL) {
  date_fields <- as.character(dictionary$fields$canonical_name[dictionary$fields$data_type == "date" & (is.na(dictionary$fields$table_name) | !nzchar(dictionary$fields$table_name))])
  numeric_fields <- as.character(dictionary$fields$canonical_name[dictionary$fields$data_type == "numeric" & (is.na(dictionary$fields$table_name) | !nzchar(dictionary$fields$table_name))])
  type_findings <- tibble::tibble(rule_id = character(), severity = character(), message = character(), count = integer())
  for (column in intersect(date_fields, names(forms))) {
    raw_values <- as.character(forms[[column]])
    response_values <- NULL
    if (!is.null(responses) && all(c("case_id", "canonical_name", "data_type", "date_value") %in% names(responses))) {
      candidates <- responses[responses$canonical_name == column & responses$data_type == "date", c("case_id", "date_value"), drop = FALSE]
      if (anyDuplicated(candidates$case_id)) stop("Epi response table has duplicate scalar date rows for ", column, ".", call. = FALSE)
      if (nrow(candidates) > 0L) response_values <- as.character(candidates$date_value[match(forms$record_id, candidates$case_id)])
    }
    if (is.null(response_values)) {
      forms[[column]] <- .phase3_parse_date(raw_values, paste0("Epi ", column))
    } else {
      parsed <- .phase3_parse_date(response_values, paste0("Epi normalized ", column))
      unresolved <- .phase3_nonmissing(raw_values) & !.phase3_nonmissing(response_values)
      if (any(unresolved)) {
        type_findings <- dplyr::bind_rows(type_findings, tibble::tibble(
          rule_id = "unparseable_date_retained_as_missing", severity = "WARNING",
          message = paste0("Epi ", column, " had populated source entries without an upstream normalized date; the canonical Date value is missing and the source validation warning is retained."),
          count = as.integer(sum(unresolved))
        ))
      }
      forms[[column]] <- parsed
    }
  }
  for (column in intersect(numeric_fields, names(forms))) {
    raw_values <- as.character(forms[[column]])
    response_values <- NULL
    if (!is.null(responses) && all(c("case_id", "canonical_name", "data_type", "numeric_value") %in% names(responses))) {
      candidates <- responses[responses$canonical_name == column & responses$data_type == "numeric", c("case_id", "numeric_value"), drop = FALSE]
      if (anyDuplicated(candidates$case_id)) stop("Epi response table has duplicate scalar numeric rows for ", column, ".", call. = FALSE)
      if (nrow(candidates) > 0L) response_values <- as.character(candidates$numeric_value[match(forms$record_id, candidates$case_id)])
    }
    if (is.null(response_values)) {
      forms[[column]] <- .phase3_parse_numeric(raw_values, paste0("Epi ", column))
    } else {
      parsed <- .phase3_parse_numeric(response_values, paste0("Epi normalized ", column))
      unresolved <- .phase3_nonmissing(raw_values) & !.phase3_nonmissing(response_values)
      if (any(unresolved)) {
        type_findings <- dplyr::bind_rows(type_findings, tibble::tibble(
          rule_id = "unparseable_numeric_retained_as_missing", severity = "WARNING",
          message = paste0("Epi ", column, " had populated source entries without an upstream normalized numeric value; the canonical numeric value is missing and the source validation warning is retained."),
          count = as.integer(sum(unresolved))
        ))
      }
      forms[[column]] <- parsed
    }
  }
  for (column in setdiff(names(forms), c("record_id", date_fields, numeric_fields))) forms[[column]] <- .phase3_parse_logical_if_boolean(forms[[column]])
  list(data = forms, findings = type_findings)
}

.phase3_normalize_bcap <- function(fields) {
  fields$field_index <- as.integer(fields$field_index)
  fields$page <- as.integer(fields$page)
  fields$is_populated <- .phase3_parse_logical_if_boolean(fields$is_populated)
  fields$value_character <- as.character(fields$value)
  fields$value_numeric <- NA_real_
  fields$value_date <- as.Date(rep(NA_character_, nrow(fields)))
  fields$value_logical <- as.logical(rep(NA, nrow(fields)))
  date_rows <- fields$field %in% c("initial_audit_date", "audit_pass_date", "Producer_date") & .phase3_nonmissing(fields$value)
  if (any(date_rows)) fields$value_date[date_rows] <- .phase3_parse_date(fields$value[date_rows], "BCAP date values")
  bool_rows <- fields$field_type %in% c("Btn") & .phase3_nonmissing(fields$value)
  if (any(bool_rows)) {
    parsed <- .phase3_parse_logical_if_boolean(fields$value[bool_rows])
    if (is.logical(parsed)) fields$value_logical[bool_rows] <- parsed
  }
  fields$value <- NULL
  fields
}

.phase3_repeated_items <- function(epi, entity_lookup) {
  result <- tibble::tibble(
    record_id = character(), entity_id = character(), event_id = character(), item_id = character(), section_identity = character(), row_index = integer(), field_name = character(), value_character = character(), value_numeric = double(), value_date = as.Date(character()), value_logical = logical(), value_code = character(), value_type = character()
  )
  metadata_cols <- c("form_version", "dictionary_version", "dictionary_hash", "row_index", "row_label", "raw_fields", "source_pages", "case_id", "record_id")
  for (table_name in names(epi$tables)) {
    table <- epi$tables[[table_name]]
    if (nrow(table) == 0L) next
    if (any(!.phase3_nonmissing(table$row_index))) stop("Epi repeated table has a missing row index.", call. = FALSE)
    table$row_index <- .phase3_parse_integer(table$row_index, paste0("Epi ", table_name, " row indexes"))
    if (any(!.phase3_nonmissing(table$raw_fields))) stop("Epi repeated table has a missing stable row descriptor.", call. = FALSE)
    table <- table[order(table$case_id, table$row_index, table$raw_fields), , drop = FALSE]
    child_key <- paste(table$case_id, table$row_index, table$raw_fields, sep = "\r")
    if (anyDuplicated(child_key)) stop("Epi repeated table has duplicate child keys.", call. = FALSE)
    item_index <- stats::ave(seq_len(nrow(table)), table$case_id, FUN = seq_along)
    value_columns <- setdiff(names(table), metadata_cols)
    for (i in seq_len(nrow(table))) {
      record_id <- as.character(table$case_id[[i]])
      item_id <- paste(record_id, table_name, sprintf("%06d", item_index[[i]]), sep = "::")
      for (field_name in value_columns) {
        if (grepl("_raw$", field_name)) next
        value <- as.character(table[[field_name]][[i]])
        if (!.phase3_nonmissing(value)) next
        value_numeric <- NA_real_
        value_date <- as.Date(NA)
        value_logical <- NA
        value_type <- "character"
        field_meta <- epi$dictionary$fields[epi$dictionary$fields$table_name == sub("^epi_", "", table_name) & epi$dictionary$fields$column_name == field_name, , drop = FALSE]
        data_type <- unique(as.character(field_meta$data_type[.phase3_nonmissing(field_meta$data_type)]))
        if (length(data_type) > 1L) stop("Epi repeated field ", table_name, ".", field_name, " has incompatible dictionary types.", call. = FALSE)
        if (length(data_type) == 1L && identical(data_type[[1L]], "date")) {
          value_date <- .phase3_parse_date(value, paste0("Epi repeated field ", field_name))
          value_type <- "date"
        } else if (length(data_type) == 1L && identical(data_type[[1L]], "numeric")) {
          value_numeric <- .phase3_parse_numeric(value, paste0("Epi repeated field ", field_name))
          value_type <- "numeric"
        } else if (grepl("_code$", field_name)) {
          value_type <- "categorical_code"
        } else {
          parsed_logical <- .phase3_parse_logical_if_boolean(value)
          if (is.logical(parsed_logical)) {
            value_logical <- parsed_logical
            value_type <- "logical"
          }
        }
        result <- dplyr::bind_rows(result, tibble::tibble(record_id = record_id, entity_id = unname(entity_lookup[[record_id]]), event_id = NA_character_, item_id = item_id, section_identity = table_name, row_index = as.integer(table$row_index[[i]]), field_name = field_name, value_character = if (value_type %in% c("date", "logical", "numeric")) NA_character_ else value, value_numeric = value_numeric, value_date = value_date, value_logical = value_logical, value_code = if (value_type == "categorical_code") value else NA_character_, value_type = value_type))
      }
    }
  }
  if (nrow(epi$multiselect) > 0L) {
    if (any(!.phase3_nonmissing(epi$multiselect$case_id))) stop("Epi multiselect table has missing record IDs.", call. = FALSE)
    counts <- stats::ave(seq_len(nrow(epi$multiselect)), epi$multiselect$case_id, FUN = seq_along)
    for (i in seq_len(nrow(epi$multiselect))) {
      record_id <- as.character(epi$multiselect$case_id[[i]])
      item_id <- paste(record_id, "multiselect_responses", sprintf("%06d", counts[[i]]), sep = "::")
      result <- dplyr::bind_rows(result, tibble::tibble(record_id = record_id, entity_id = unname(entity_lookup[[record_id]]), event_id = NA_character_, item_id = item_id, section_identity = "multiselect_responses", row_index = as.integer(counts[[i]]), field_name = as.character(epi$multiselect$canonical_name[[i]]), value_character = as.character(epi$multiselect$item_label[[i]]), value_numeric = NA_real_, value_date = as.Date(NA), value_logical = NA, value_code = as.character(epi$multiselect$item_code[[i]]), value_type = "categorical_item"))
    }
  }
  result[order(result$record_id, result$section_identity, result$row_index, result$field_name), , drop = FALSE]
}

.phase3_linkage <- function(entity_map, records) {
  epi <- entity_map$records[entity_map$records$form_type == "initial_epi" & entity_map$records$entity_scope == "shared_authoritative_key", , drop = FALSE]
  bcap <- entity_map$records[entity_map$records$form_type == "bcap" & entity_map$records$entity_scope == "shared_authoritative_key", , drop = FALSE]
  empty <- tibble::tibble(left_record_id = character(), right_record_id = character(), link_type = character(), shared_entity_id = character(), shared_event_id = character(), link_basis = character(), link_confidence = character(), days_epi_to_bcap = integer(), epi_before_bcap = logical())
  if (nrow(epi) == 0L || nrow(bcap) == 0L) return(empty)
  pairs <- merge(epi[, c("record_id", "entity_id")], bcap[, c("record_id", "entity_id")], by = "entity_id", suffixes = c("_epi", "_bcap"), sort = FALSE)
  if (nrow(pairs) == 0L) return(empty)
  result <- tibble::tibble(left_record_id = as.character(pairs$record_id_epi), right_record_id = as.character(pairs$record_id_bcap), link_type = "L2_shared_entity", shared_entity_id = as.character(pairs$entity_id), shared_event_id = NA_character_, link_basis = "authoritative_crosswalk_premises_key", link_confidence = "exact/authoritative", days_epi_to_bcap = NA_integer_, epi_before_bcap = NA)
  result[order(result$left_record_id, result$right_record_id, result$link_type), , drop = FALSE]
}

.phase3_provenance <- function(records, epi_status, bcap_status, epi_meta, bcap_meta, epi_crosswalk, bcap_crosswalk, run_id) {
  epi <- records[records$form_type == "initial_epi", , drop = FALSE]
  bcap <- records[records$form_type == "bcap", , drop = FALSE]
  epi_source <- stats::setNames(rep(NA_character_, nrow(epi)), epi$record_id)
  bcap_source <- stats::setNames(rep(NA_character_, nrow(bcap)), bcap$record_id)
  if (!is.null(epi_crosswalk)) epi_source <- stats::setNames(as.character(epi_crosswalk$source_sha256), epi_crosswalk$case_id)
  if (!is.null(bcap_crosswalk)) bcap_source <- stats::setNames(as.character(bcap_crosswalk$source_md5), bcap_crosswalk$record_id)
  epi_meta <- epi_meta[match(epi$record_id, epi_meta$record_id), , drop = FALSE]
  bcap_meta <- bcap_meta[match(bcap$record_id, bcap_meta$record_id), , drop = FALSE]
  result <- dplyr::bind_rows(
    tibble::tibble(record_id = epi$record_id, form_type = epi$form_type, form_version = epi$form_version, source_schema_version = epi$source_schema_version, parser_version = .phase3_package_version(), extraction_method = "validated_safe_initial_epi", template_version = NA_character_, deidentification_policy_version = .phase3_epi_version, collation_version = .phase3_version, source_hash_reference = unname(epi_source[epi$record_id]), run_id = run_id),
    tibble::tibble(record_id = bcap$record_id, form_type = bcap$form_type, form_version = bcap$form_version, source_schema_version = bcap$source_schema_version, parser_version = .phase3_package_version(), extraction_method = as.character(bcap_meta$extraction_method), template_version = as.character(bcap_meta$template_version), deidentification_policy_version = "bcap-policy-1", collation_version = .phase3_version, source_hash_reference = unname(bcap_source[bcap$record_id]), run_id = run_id)
  )
  result[order(result$record_id), , drop = FALSE]
}

.phase3_qa <- function(records, entities, linkage, repeated_items, epi, bcap, statuses, source_manifests = list(), type_findings = NULL) {
  findings <- tibble::tibble(rule_id = character(), severity = character(), message = character(), count = integer())
  add_finding <- function(rule_id, severity, message, count) {
    if (count > 0L) findings <<- dplyr::bind_rows(findings, tibble::tibble(rule_id = rule_id, severity = severity, message = message, count = as.integer(count)))
  }
  if (!is.null(type_findings) && nrow(type_findings) > 0L) findings <- dplyr::bind_rows(findings, type_findings)
  add_finding("duplicate_record_id", "ERROR", "Canonical record IDs must be unique.", sum(duplicated(records$record_id)))
  add_finding("duplicate_entity_id", "ERROR", "Canonical entity IDs must be unique in the entity table.", sum(duplicated(entities$entity_id)))
  add_finding("orphan_linkage_endpoint", "ERROR", "Every linkage endpoint must exist in records.", sum(!c(linkage$left_record_id, linkage$right_record_id) %in% records$record_id, na.rm = TRUE))
  add_finding("orphan_child_record", "ERROR", "Every repeated item record ID must exist in records.", sum(!repeated_items$record_id %in% records$record_id, na.rm = TRUE))
  add_finding("orphan_entity_reference", "ERROR", "Every entity reference must exist in entities.", sum(!records$entity_id %in% entities$entity_id, na.rm = TRUE))
  add_finding("duplicate_linkage_row", "ERROR", "Linkage rows must be unique.", sum(duplicated(linkage[c("left_record_id", "right_record_id", "link_type")])) )
  add_finding("missing_required_key", "ERROR", "Canonical record keys must be populated.", sum(!.phase3_nonmissing(records$record_id) | !.phase3_nonmissing(records$entity_id), na.rm = TRUE))
  add_finding("semantic_validation_error", "ERROR", "Upstream semantic validation errors block finalization.", statuses$epi$semantic_errors + statuses$bcap$semantic_errors)
  add_finding("privacy_validation_error", "ERROR", "Upstream privacy validation findings block finalization.", statuses$epi$privacy_errors + statuses$bcap$privacy_errors)
  add_finding("privacy_validation_warning", "ERROR", "Strict privacy output requires zero warnings.", statuses$epi$privacy_warnings + statuses$bcap$privacy_warnings)
  add_finding("semantic_validation_warning", "WARNING", "Upstream semantic warnings are retained as a non-blocking review status.", statuses$epi$semantic_warnings + statuses$bcap$semantic_warnings)
  source_count <- function(manifest, safe_n) {
    if (is.null(manifest)) return(c(discovered = safe_n, supported = safe_n, unsupported = 0L, extraction_failure = 0L))
    status <- tolower(as.character(manifest$status))
    c(discovered = nrow(manifest), supported = sum(status == "success"), unsupported = sum(status != "success"), extraction_failure = sum(status != "success"))
  }
  epi_source <- source_count(source_manifests$epi, nrow(epi$forms))
  bcap_source <- source_count(source_manifests$bcap, nrow(bcap$metadata))
  linked <- unique(c(linkage$left_record_id, linkage$right_record_id))
  unlinked <- setdiff(records$record_id, linked)
  type_warning_count <- if (is.null(type_findings) || nrow(type_findings) == 0L) 0L else sum(type_findings$count)
  all_metrics <- c("safe_records", "linked_records", "unlinked_records", "semantic_warnings", "semantic_errors", "privacy_warnings", "privacy_errors", "type_normalization_warnings", "schema_versions", "unexpected_columns", "missing_required_canonical_keys", "duplicate_record_ids", "duplicate_entity_ids", "duplicate_linkage_rows", "orphan_rows")
  all_values <- c(nrow(records), length(linked), length(unlinked), statuses$epi$semantic_warnings + statuses$bcap$semantic_warnings, statuses$epi$semantic_errors + statuses$bcap$semantic_errors, statuses$epi$privacy_warnings + statuses$bcap$privacy_warnings, statuses$epi$privacy_errors + statuses$bcap$privacy_errors, type_warning_count, length(unique(records$source_schema_version)), 0L, sum(!.phase3_nonmissing(records$record_id) | !.phase3_nonmissing(records$entity_id), na.rm = TRUE), sum(duplicated(records$record_id)), sum(duplicated(entities$entity_id)), sum(duplicated(linkage[c("left_record_id", "right_record_id", "link_type")])), sum(!repeated_items$record_id %in% records$record_id, na.rm = TRUE))
  metrics <- dplyr::bind_rows(
    tibble::tibble(form_type = "initial_epi", metric = names(epi_source), value = as.integer(epi_source)),
    tibble::tibble(form_type = "bcap", metric = names(bcap_source), value = as.integer(bcap_source)),
    tibble::tibble(form_type = "all", metric = all_metrics, value = as.integer(all_values))
  ) |>
    dplyr::arrange(form_type, metric)
  list(findings = findings, metrics = metrics, status = if (any(findings$severity == "ERROR")) "failed" else if (any(findings$severity == "WARNING")) "review" else "passed")
}

.phase3_output_hashes <- function(directory, tables) {
  rows <- list()
  for (name in names(tables)) {
    csv <- fs::path(directory, paste0(name, ".csv"))
    rds <- fs::path(directory, paste0(name, ".rds"))
    rows[[length(rows) + 1L]] <- tibble::tibble(product = name, format = "csv", sha256 = source_sha256(csv), row_count = nrow(tables[[name]]), column_count = ncol(tables[[name]]))
    rows[[length(rows) + 1L]] <- tibble::tibble(product = name, format = "rds", sha256 = source_sha256(rds), row_count = nrow(tables[[name]]), column_count = ncol(tables[[name]]))
  }
  dplyr::bind_rows(rows) |>
    dplyr::arrange(product, format)
}

.phase3_write_products <- function(directory, tables) {
  for (name in names(tables)) {
    write_csv_utf8(tables[[name]], fs::path(directory, paste0(name, ".csv")))
    saveRDS(tables[[name]], fs::path(directory, paste0(name, ".rds")), version = 3)
  }
}

.phase3_collate_unified_legacy <- function(epi_dir, bcap_dir, output_dir, epi_crosswalk_dir = NULL, bcap_crosswalk_dir = NULL, epi_source_manifest = NULL, bcap_source_manifest = NULL, run_id = NULL, overwrite = FALSE, quiet = FALSE) {
  epi_dir <- .phase3_safe_path(epi_dir, "epi_dir")
  bcap_dir <- .phase3_safe_path(bcap_dir, "bcap_dir")
  output_dir <- .phase3_safe_path(output_dir, "output_dir")
  if (!dir.exists(epi_dir) || !dir.exists(bcap_dir)) stop("Epi and BCAP inputs must be existing safe output directories.", call. = FALSE)
  if (.phase3_path_within(output_dir, epi_dir) || .phase3_path_within(output_dir, bcap_dir) || (!is.null(epi_crosswalk_dir) && .phase3_path_within(output_dir, .phase3_safe_path(epi_crosswalk_dir, "epi_crosswalk_dir"))) || (!is.null(bcap_crosswalk_dir) && .phase3_path_within(output_dir, .phase3_safe_path(bcap_crosswalk_dir, "bcap_crosswalk_dir")))) stop("Analysis output must be separate from safe inputs and private crosswalks.", call. = FALSE)
  if (dir.exists(output_dir) && !isTRUE(overwrite)) stop("Analysis output already exists; set overwrite = TRUE only after reviewing the destination.", call. = FALSE)
  if (is.null(run_id)) run_id <- paste0("phase3-", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  if (length(run_id) != 1L || is.na(run_id) || !grepl("^[A-Za-z0-9_.-]+$", run_id)) stop("`run_id` must be one safe path component.", call. = FALSE)
  statuses <- list(epi = .phase3_input_status(epi_dir, "epi"), bcap = .phase3_input_status(bcap_dir, "bcap"))
  epi <- .phase3_epi_read(epi_dir)
  bcap <- .phase3_bcap_read(bcap_dir)
  epi_crosswalk <- .phase3_crosswalk(epi_crosswalk_dir, "epi", epi$forms$record_id)
  bcap_crosswalk <- .phase3_crosswalk(bcap_crosswalk_dir, "bcap", bcap$metadata$record_id)
  entity_map <- .phase3_entity_map(epi, bcap, epi_crosswalk, bcap_crosswalk)
  record_map <- entity_map$records
  epi_normalized <- .phase3_normalize_epi_forms(epi$forms, epi$dictionary, epi$responses)
  epi_forms <- epi_normalized$data
  epi_lookup <- stats::setNames(record_map$entity_id, record_map$record_id)
  epi_forms$entity_id <- unname(epi_lookup[epi_forms$record_id])
  epi_forms$event_id <- NA_character_
  epi_forms <- epi_forms[, c("record_id", "entity_id", "event_id", setdiff(names(epi_forms), c("record_id", "entity_id", "event_id"))), drop = FALSE]
  bcap_fields <- .phase3_normalize_bcap(bcap$fields)
  bcap_fields$entity_id <- unname(epi_lookup[bcap_fields$record_id])
  bcap_fields$event_id <- NA_character_
  bcap_fields <- bcap_fields[, c("record_id", "entity_id", "event_id", setdiff(names(bcap_fields), c("record_id", "entity_id", "event_id"))), drop = FALSE]
  bcap_meta <- bcap$metadata
  bcap_meta$form_version <- bcap_meta$template_version
  bcap_meta$source_schema_version <- bcap_meta$schema_group
  bcap_meta$entity_id <- unname(epi_lookup[bcap_meta$record_id])
  bcap_meta$event_id <- NA_character_
  bcap_meta$validation_status <- "passed"
  bcap_meta$privacy_status <- "passed"
  bcap_dates <- bcap_fields[bcap_fields$field %in% c("initial_audit_date", "audit_pass_date") & !is.na(bcap_fields$value_date), c("record_id", "field", "value_date"), drop = FALSE]
  bcap_date_by_record <- stats::setNames(rep(as.Date(NA), nrow(bcap_meta)), bcap_meta$record_id)
  for (record_id in names(bcap_date_by_record)) {
    rows <- bcap_dates[bcap_dates$record_id == record_id, , drop = FALSE]
    if (nrow(rows) > 0L) bcap_date_by_record[[record_id]] <- rows$value_date[[1L]]
  }
  validation_form <- .phase3_read_csv(fs::path(epi_dir, "validation", "validation_form_summary.csv"), "Epi validation form summary")
  .phase3_required_columns(validation_form, c("case_id", "validation_status"), "Epi validation form summary")
  epi_status <- validation_form$validation_status[match(epi_forms$record_id, validation_form$case_id)]
  if (any(is.na(epi_status))) stop("Epi validation form summary does not cover all safe records.", call. = FALSE)
  epi_forms$validation_status <- as.character(epi_status)
  epi_forms$privacy_status <- "passed"
  epi_forms$form_version <- as.character(epi_forms$form_version)
  epi_forms$source_schema_version <- as.character(epi_forms$schema_group)
  epi_primary_date <- if ("today_date" %in% names(epi_forms)) .phase3_parse_date(as.character(epi_forms$today_date), "Epi primary dates") else as.Date(rep(NA_character_, nrow(epi_forms)))
  records <- dplyr::bind_rows(
    tibble::tibble(record_id = epi_forms$record_id, form_type = "initial_epi", form_version = epi_forms$form_version, source_schema_version = epi_forms$source_schema_version, entity_id = epi_forms$entity_id, event_id = epi_forms$event_id, primary_date = epi_primary_date, primary_date_field = ifelse(is.na(epi_primary_date), NA_character_, "today_date"), validation_status = epi_forms$validation_status, semantic_status = ifelse(epi_forms$validation_status == "valid", "passed", epi_forms$validation_status), privacy_status = epi_forms$privacy_status, extraction_method = "validated_safe_initial_epi", template_family = NA_character_, template_version = NA_character_, source_hash_reference = NA_character_),
    tibble::tibble(record_id = bcap_meta$record_id, form_type = "bcap", form_version = bcap_meta$form_version, source_schema_version = bcap_meta$source_schema_version, entity_id = bcap_meta$entity_id, event_id = bcap_meta$event_id, primary_date = unname(bcap_date_by_record[bcap_meta$record_id]), primary_date_field = ifelse(bcap_meta$record_id %in% bcap_dates$record_id, "initial_audit_date_or_audit_pass_date", NA_character_), validation_status = bcap_meta$validation_status, semantic_status = "passed", privacy_status = bcap_meta$privacy_status, extraction_method = bcap_meta$extraction_method, template_family = bcap_meta$template_family, template_version = bcap_meta$template_version, source_hash_reference = NA_character_)
  )
  records <- records[order(records$record_id), , drop = FALSE]
  linkage <- .phase3_linkage(entity_map, records)
  if (nrow(linkage) > 0L) {
    epi_dates <- stats::setNames(records$primary_date[records$form_type == "initial_epi"], records$record_id[records$form_type == "initial_epi"])
    bcap_dates_record <- stats::setNames(records$primary_date[records$form_type == "bcap"], records$record_id[records$form_type == "bcap"])
    linkage$days_epi_to_bcap <- as.integer(bcap_dates_record[linkage$right_record_id] - epi_dates[linkage$left_record_id])
    linkage$epi_before_bcap <- ifelse(is.na(linkage$days_epi_to_bcap), NA, linkage$days_epi_to_bcap >= 0L)
  }
  repeated <- .phase3_repeated_items(epi, epi_lookup)
  provenance <- .phase3_provenance(records, statuses$epi, statuses$bcap, epi_forms, bcap_meta, epi_crosswalk, bcap_crosswalk, run_id)
  records$source_hash_reference <- provenance$source_hash_reference[match(records$record_id, provenance$record_id)]
  records <- records[, c("record_id", "form_type", "form_version", "source_schema_version", "entity_id", "event_id", "primary_date", "primary_date_field", "validation_status", "semantic_status", "privacy_status", "extraction_method", "template_family", "template_version", "source_hash_reference"), drop = FALSE]
  source_manifests <- list(epi = if (is.null(epi_source_manifest)) NULL else .phase3_read_csv(epi_source_manifest, "Epi source manifest"), bcap = if (is.null(bcap_source_manifest)) NULL else .phase3_read_csv(bcap_source_manifest, "BCAP source manifest"))
  for (name in names(source_manifests)) {
    if (!is.null(source_manifests[[name]])) .phase3_required_columns(source_manifests[[name]], "status", paste0(name, " source manifest"))
  }
  qa <- .phase3_qa(records, entity_map$entities, linkage, repeated, epi, bcap, statuses, source_manifests, epi_normalized$findings)
  if (!identical(qa$status, "passed") && !identical(qa$status, "review")) stop("Phase 3 QA failed; no analysis-ready products were finalized.", call. = FALSE)
  products <- list(records = records, entities = entity_map$entities, epi = epi_forms, bcap = bcap_fields, linkage = linkage, repeated_items = repeated, provenance = provenance, qa_summary = qa$metrics, qa_findings = qa$findings)
  parent <- fs::path_dir(output_dir)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  temp_output <- tempfile("phase3-analysis-", tmpdir = parent)
  dir.create(temp_output, recursive = TRUE, showWarnings = FALSE)
  committed <- FALSE
  on.exit({ if (!committed && dir.exists(temp_output)) unlink(temp_output, recursive = TRUE, force = TRUE) }, add = TRUE)
  .phase3_write_products(temp_output, products)
  hashes <- .phase3_output_hashes(temp_output, products)
  write_csv_utf8(hashes, fs::path(temp_output, "output_hashes.csv"))
  hash_text <- paste(paste(hashes$product, hashes$format, hashes$sha256, sep = ":"), collapse = ";")
  manifest <- tibble::tibble(
    run_id = run_id, timestamp = utc_now(), package_version = .phase3_package_version(), git_sha = .phase3_git_sha(), schema_versions = paste(sort(unique(records$source_schema_version)), collapse = ";"), collation_version = .phase3_version, epi_source_files_discovered = qa$metrics$value[qa$metrics$form_type == "initial_epi" & qa$metrics$metric == "discovered"], bcap_source_files_discovered = qa$metrics$value[qa$metrics$form_type == "bcap" & qa$metrics$metric == "discovered"], epi_supported = qa$metrics$value[qa$metrics$form_type == "initial_epi" & qa$metrics$metric == "supported"], bcap_supported = qa$metrics$value[qa$metrics$form_type == "bcap" & qa$metrics$metric == "supported"], bcap_unsupported = qa$metrics$value[qa$metrics$form_type == "bcap" & qa$metrics$metric == "unsupported"], semantic_status = ifelse(statuses$epi$semantic_warnings > 0L, "review", "passed"), privacy_status = "passed", safe_records = nrow(records), linked_records = length(unique(c(linkage$left_record_id, linkage$right_record_id))), unlinked_records = length(setdiff(records$record_id, unique(c(linkage$left_record_id, linkage$right_record_id)))), qa_status = qa$status, output_hashes = hash_text
  )
  write_csv_utf8(manifest, fs::path(temp_output, "run_manifest.csv"))
  if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE, force = TRUE)
  if (!file.rename(temp_output, output_dir)) stop("Unable to finalize analysis-ready output directory.", call. = FALSE)
  committed <- TRUE
  result <- list(status = qa$status, output_dir = output_dir, tables = products, qa = qa, manifest = manifest, linkage = linkage, production_safe = TRUE)
  class(result) <- c("bcapture_hpai_collation", "list")
  if (!isTRUE(quiet)) cli::cli_inform("Finalized Phase 3 analysis products for {nrow(records)} records; QA status {qa$status}.")
  result
}
