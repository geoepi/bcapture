.epi_validation_types <- c("parse", "range", "chronology", "conditional", "table_structure", "codebook", "cross_field")
.epi_validation_severities <- c("INFO", "WARNING", "ERROR")

#' Create the typed empty Initial Epi validation result.
#'
#' @return A zero-row tibble with the public validation-result schema.
#' @noRd
#' @keywords internal
new_validation_results <- function() {
  tibble::tibble(
    form_id = character(), severity = character(), validation_type = character(),
    rule_id = character(), section_id = character(), question_id = character(),
    raw_field = character(), canonical_name = character(), table_name = character(),
    row_index = integer(), message = character()
  )
}

.epi_validation_rules <- function(version) {
  root <- .epi_dictionary_root(version)
  path <- fs::path(root, "validation_rules.csv")
  if (!file.exists(path)) stop("Initial Epi validation rule registry is missing: ", path, call. = FALSE)
  rules <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
  required <- c("rule_id", "rule_type", "parent_field", "trigger_code", "child_field", "child_table",
                "child_filter", "severity", "description", "table_name", "core_fields", "allowed_values")
  missing <- setdiff(required, names(rules))
  if (length(missing) > 0L) stop("Initial Epi validation registry is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  rules
}

.epi_validation_finding <- function(form_id = NA_character_, severity, validation_type, rule_id,
                                    section_id = NA_character_, question_id = NA_character_,
                                    raw_field = NA_character_, canonical_name = NA_character_,
                                    table_name = NA_character_, row_index = NA_integer_, message) {
  severity <- as.character(severity)[[1L]]
  validation_type <- as.character(validation_type)[[1L]]
  if (!severity %in% .epi_validation_severities) stop("Unsupported validation severity.", call. = FALSE)
  if (!validation_type %in% .epi_validation_types) stop("Unsupported validation type.", call. = FALSE)
  tibble::tibble(
    form_id = as.character(form_id)[[1L]], severity = severity,
    validation_type = validation_type, rule_id = as.character(rule_id)[[1L]],
    section_id = as.character(section_id)[[1L]], question_id = as.character(question_id)[[1L]],
    raw_field = as.character(raw_field)[[1L]], canonical_name = as.character(canonical_name)[[1L]],
    table_name = as.character(table_name)[[1L]], row_index = as.integer(row_index)[[1L]],
    message = as.character(message)[[1L]]
  )
}

.epi_validation_empty_if_none <- function(rows) {
  if (length(rows) == 0L) new_validation_results() else dplyr::bind_rows(rows)
}

.epi_validation_read <- function(path) {
  if (!file.exists(path)) stop("Required collated product is missing: ", fs::path_file(path), call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
}

.epi_validation_populated <- function(value) {
  if (length(value) == 0L || is.na(value[[1L]])) return(FALSE)
  value <- sub("^/", "", trimws(as.character(value[[1L]])))
  nzchar(value) && !value %in% c("Off", epi_placeholder_values)
}

.epi_validation_raw <- function(value) {
  if (length(value) == 0L || is.na(value[[1L]])) return(NA_character_)
  sub("^/", "", trimws(as.character(value[[1L]])))
}

.epi_validation_meta <- function(raw_field, dictionary) {
  index <- match(as.character(raw_field), dictionary$fields$raw_field)
  if (is.na(index)) return(list(section_id = NA_character_, question_id = NA_character_, canonical_name = NA_character_))
  list(
    section_id = as.character(dictionary$fields$section_id[[index]]),
    question_id = as.character(dictionary$fields$question_id[[index]]),
    canonical_name = as.character(dictionary$fields$canonical_name[[index]])
  )
}

.epi_validation_field_finding <- function(row, dictionary, severity, validation_type, rule_id, message,
                                          table_name = NA_character_, row_index = NA_integer_) {
  meta <- .epi_validation_meta(row$raw_field[[1L]], dictionary)
  .epi_validation_finding(row$form_id[[1L]], severity, validation_type, rule_id,
    meta$section_id, meta$question_id, row$raw_field[[1L]], meta$canonical_name,
    table_name, row_index, message)
}

.epi_validation_table_raw_field <- function(dictionary, table_name, row_index, column_name, row_label = NA_character_) {
  fields <- dictionary$fields[dictionary$fields$table_name == table_name &
    dictionary$fields$column_name == column_name & as.integer(dictionary$fields$row_index) == as.integer(row_index), , drop = FALSE]
  if (!is.na(row_label) && nzchar(row_label)) {
    labeled <- fields[as.character(fields$row_label) == as.character(row_label), , drop = FALSE]
    if (nrow(labeled) > 0L) fields <- labeled
  }
  if (nrow(fields) == 0L) NA_character_ else as.character(fields$raw_field[[1L]])
}

.epi_validation_table_filter <- function(table, filter) {
  if (is.na(filter) || !nzchar(filter)) return(rep(TRUE, nrow(table)))
  terms <- unlist(strsplit(filter, ";", fixed = TRUE), use.names = FALSE)
  keep <- rep(TRUE, nrow(table))
  for (term in terms) {
    pair <- strsplit(term, "=", fixed = TRUE)[[1L]]
    if (length(pair) != 2L || !pair[[1L]] %in% names(table)) return(rep(FALSE, nrow(table)))
    keep <- keep & as.character(table[[pair[[1L]]]]) == pair[[2L]]
  }
  keep
}

validate_epi_parse <- function(products, dictionary) {
  rows <- list()
  diagnostics <- products$parse_diagnostics
  if (nrow(diagnostics) > 0L) {
    failed <- diagnostics[diagnostics$status == "failed", , drop = FALSE]
    for (i in seq_len(nrow(failed))) {
      type <- as.character(failed$parse_type[[i]])
      rule <- if (identical(type, "date")) "expected_date_unparseable" else if (identical(type, "numeric")) "expected_numeric_unparseable" else NA_character_
      if (is.na(rule)) next
      meta <- .epi_validation_meta(failed$raw_field[[i]], dictionary)
      rows[[length(rows) + 1L]] <- .epi_validation_finding(
        failed$form_id[[i]], "WARNING", "parse", rule,
        section_id = meta$section_id, question_id = meta$question_id,
        raw_field = failed$raw_field[[i]], canonical_name = meta$canonical_name,
        table_name = failed$table_name[[i]], row_index = as.integer(failed$row_index[[i]]),
        message = paste0("Expected ", type, " value could not be parsed; the original entry remains available in the collated source.")
      )
    }
  }
  responses <- products$responses
  if (nrow(responses) > 0L) {
    fields <- dictionary$fields[is.na(dictionary$fields$table_name) | !nzchar(dictionary$fields$table_name), , drop = FALSE]
    fields <- fields[fields$data_type %in% c("date", "numeric"), , drop = FALSE]
    for (i in seq_len(nrow(responses))) {
      field <- responses$raw_field[[i]]
      meta <- fields[fields$raw_field == field, , drop = FALSE]
      if (nrow(meta) == 0L || !.epi_validation_populated(responses$raw_value[[i]])) next
      raw <- .epi_validation_raw(responses$raw_value[[i]])
      parsed <- if (meta$data_type[[1L]] == "date") .epi_parse_date(raw) else .epi_parse_numeric(raw)
      if ((meta$data_type[[1L]] == "date" && is.na(parsed)) || (meta$data_type[[1L]] == "numeric" && is.na(parsed))) {
        rule <- if (meta$data_type[[1L]] == "date") "expected_date_unparseable" else "expected_numeric_unparseable"
        rows[[length(rows) + 1L]] <- .epi_validation_field_finding(
          responses[i, , drop = FALSE], dictionary, "WARNING", "parse", rule,
          paste0("Expected ", meta$data_type[[1L]], " value could not be parsed; chronology or range checks were not attempted.")
        )
      }
    }
  }
  .epi_validation_empty_if_none(rows)
}

validate_epi_ranges <- function(products, dictionary) {
  rows <- list()
  forms <- products$forms
  coordinate_fields <- dictionary$fields[dictionary$fields$canonical_name %in% c("premises_latitude", "premises_longitude"), , drop = FALSE]
  for (i in seq_len(nrow(forms))) {
    for (j in seq_len(nrow(coordinate_fields))) {
      name <- coordinate_fields$canonical_name[[j]]
      raw <- forms[[name]][[i]]
      if (!.epi_validation_populated(raw)) next
      value <- .epi_parse_numeric(.epi_validation_raw(raw))
      if (is.na(value)) next
      lower <- if (identical(name, "premises_latitude")) -90 else -180
      upper <- if (identical(name, "premises_latitude")) 90 else 180
      if (value < lower || value > upper) {
        rows[[length(rows) + 1L]] <- .epi_validation_finding(
          forms$form_id[[i]], "ERROR", "range", if (identical(name, "premises_latitude")) "latitude_out_of_range" else "longitude_out_of_range",
          coordinate_fields$section_id[[j]], coordinate_fields$question_id[[j]], coordinate_fields$raw_field[[j]], name,
          message = paste0("Parsed ", name, " is outside the form-supported geographic range.")
        )
      }
    }
  }
  scalar_fields <- dictionary$fields[is.na(dictionary$fields$table_name) | !nzchar(dictionary$fields$table_name), , drop = FALSE]
  scalar_fields <- scalar_fields[scalar_fields$data_type == "numeric" & !scalar_fields$canonical_name %in% c("premises_latitude", "premises_longitude"), , drop = FALSE]
  responses <- products$responses
  for (i in seq_len(nrow(responses))) {
    field <- scalar_fields[scalar_fields$raw_field == responses$raw_field[[i]], , drop = FALSE]
    if (nrow(field) == 0L || !.epi_validation_populated(responses$raw_value[[i]])) next
    value <- .epi_parse_numeric(.epi_validation_raw(responses$raw_value[[i]]))
    if (!is.na(value) && value < 0) rows[[length(rows) + 1L]] <- .epi_validation_field_finding(
      responses[i, , drop = FALSE], dictionary, "WARNING", "range", "negative_value",
      "A populated quantity is negative; the entered value was not changed.")
  }
  for (table_name in names(products$tables)) {
    table <- products$tables[[table_name]]
    numeric_fields <- dictionary$fields[dictionary$fields$table_name == table_name & dictionary$fields$data_type == "numeric", , drop = FALSE]
    for (column in unique(numeric_fields$column_name)) {
      if (!column %in% names(table)) next
      for (i in seq_len(nrow(table))) {
        value <- suppressWarnings(as.numeric(table[[column]][[i]]))
        if (!is.na(value) && value < 0) {
          raw_field <- .epi_validation_table_raw_field(dictionary, table_name, table$row_index[[i]], column, table$row_label[[i]] %||% NA_character_)
          row <- tibble::tibble(form_id = as.character(table$form_id[[i]]), raw_field = raw_field)
          rows[[length(rows) + 1L]] <- .epi_validation_field_finding(row, dictionary, "WARNING", "range", "negative_value",
            "A populated quantity is negative; the entered value was not changed.", table_name, as.integer(table$row_index[[i]]))
        }
      }
    }
  }
  .epi_validation_empty_if_none(rows)
}

validate_epi_dates <- function(products, dictionary) {
  responses <- products$responses
  date_fields <- c(today_date = "p0001", clinical_signs_first_observed_date = "p0002", date_28_days_before_clinical_signs = "p0003")
  rows <- list()
  for (form_id in unique(as.character(products$forms$form_id))) {
    current <- responses[responses$form_id == form_id & responses$raw_field %in% unname(date_fields), , drop = FALSE]
    dates <- stats::setNames(as.Date(rep(NA_character_, length(date_fields))), names(date_fields))
    for (name in names(date_fields)) {
      value <- current$raw_value[current$raw_field == date_fields[[name]]]
      if (length(value) > 0L && .epi_validation_populated(value[[1L]])) dates[[name]] <- .epi_parse_date(.epi_validation_raw(value[[1L]]))
    }
    if (is.na(dates[["clinical_signs_first_observed_date"]]) || is.na(dates[["date_28_days_before_clinical_signs"]])) next
    ref_row <- current[current$raw_field == date_fields[["date_28_days_before_clinical_signs"]], , drop = FALSE]
    clinical_row <- current[current$raw_field == date_fields[["clinical_signs_first_observed_date"]], , drop = FALSE]
    if (dates[["date_28_days_before_clinical_signs"]] != dates[["clinical_signs_first_observed_date"]] - 28) {
      rows[[length(rows) + 1L]] <- .epi_validation_field_finding(ref_row, dictionary, "WARNING", "chronology", "reference_period_not_28_days",
        "The entered reference-start date is not 28 days before the clinical-sign date.")
    }
    if (dates[["date_28_days_before_clinical_signs"]] > dates[["clinical_signs_first_observed_date"]]) {
      rows[[length(rows) + 1L]] <- .epi_validation_field_finding(ref_row, dictionary, "ERROR", "chronology", "invalid_reference_period_order",
        "The reference-start date occurs after the clinical-sign date.")
    }
    if (!is.na(dates[["today_date"]]) && dates[["clinical_signs_first_observed_date"]] > dates[["today_date"]]) {
      rows[[length(rows) + 1L]] <- .epi_validation_field_finding(clinical_row, dictionary, "WARNING", "chronology", "clinical_signs_after_interview",
        "The clinical-sign date occurs after the form's today/interview date.")
    }
  }
  .epi_validation_empty_if_none(rows)
}

validate_epi_conditionals <- function(products, dictionary, rules) {
  rows <- list()
  conditional <- rules[rules$rule_type == "conditional", , drop = FALSE]
  for (i in seq_len(nrow(conditional))) {
    rule <- conditional[i, , drop = FALSE]
    for (form_id in unique(as.character(products$forms$form_id))) {
      parent <- products$responses[products$responses$form_id == form_id & products$responses$raw_field == rule$parent_field[[1L]], , drop = FALSE]
      if (nrow(parent) == 0L || !any(.epi_validation_populated(parent$raw_value))) next
      parent <- parent[which(.epi_validation_populated(parent$raw_value))[1L], , drop = FALSE]
      triggered <- identical(as.character(parent$response_code[[1L]]), as.character(rule$trigger_code[[1L]]))
      if (!triggered && !identical(as.character(parent$response_code[[1L]]), "3")) next
      child_fields <- if (is.na(rule$child_field[[1L]])) character() else unlist(strsplit(rule$child_field[[1L]], ";", fixed = TRUE), use.names = FALSE)
      child_rows <- products$responses[products$responses$form_id == form_id & products$responses$raw_field %in% child_fields, , drop = FALSE]
      scalar_present <- nrow(child_rows) > 0L && any(vapply(child_rows$raw_value, .epi_validation_populated, logical(1)))
      table_present <- FALSE
      if (!is.na(rule$child_table[[1L]]) && nzchar(rule$child_table[[1L]])) {
        table <- products$tables[[rule$child_table[[1L]]]]
        if (!is.null(table) && nrow(table) > 0L) table_present <- any(.epi_validation_table_filter(table, rule$child_filter[[1L]]))
      }
      present <- scalar_present || table_present
      if ((triggered && !present) || (!triggered && identical(as.character(parent$response_code[[1L]]), "3") && present)) {
        rule_id <- if (triggered) "yes_followup_missing" else "no_with_followup_data"
        table_name <- if (is.na(rule$child_table[[1L]])) NA_character_ else rule$child_table[[1L]]
        message <- if (triggered) "A Yes response has no populated follow-up field or table record." else "A No response has populated follow-up data; the data were retained for review."
        rows[[length(rows) + 1L]] <- .epi_validation_field_finding(parent, dictionary, as.character(rule$severity[[1L]]), "conditional", rule_id, message, table_name)
      }
    }
  }
  .epi_validation_empty_if_none(rows)
}

validate_epi_tables <- function(products, dictionary, rules) {
  rows <- list()
  for (table_name in names(products$tables)) {
    table <- products$tables[[table_name]]
    if (nrow(table) == 0L) next
    expected <- dictionary$tables$expected_rows[dictionary$tables$table_name == table_name][[1L]]
    for (i in seq_len(nrow(table))) {
      row_index <- suppressWarnings(as.numeric(table$row_index[[i]]))
      raw_field <- if ("raw_fields" %in% names(table)) strsplit(as.character(table$raw_fields[[i]]), "|", fixed = TRUE)[[1L]][1L] else NA_character_
      if (is.na(row_index) || row_index != floor(row_index) || row_index < 1 || row_index > expected) {
        row <- tibble::tibble(form_id = as.character(table$form_id[[i]]), raw_field = raw_field)
        rows[[length(rows) + 1L]] <- .epi_validation_field_finding(row, dictionary, "ERROR", "table_structure", "invalid_row_index",
          "A repeated-table row index is not a valid registered row.", table_name, as.integer(row_index))
      }
    }
    key_columns <- intersect(c("form_id", "row_index", "row_label", "direction", "material_type", "method"), names(table))
    duplicate <- duplicated(table[key_columns]) | duplicated(table[key_columns], fromLast = TRUE)
    for (i in which(duplicate)) {
      raw_field <- if ("raw_fields" %in% names(table)) strsplit(as.character(table$raw_fields[[i]]), "|", fixed = TRUE)[[1L]][1L] else NA_character_
      row <- tibble::tibble(form_id = as.character(table$form_id[[i]]), raw_field = raw_field)
      rows[[length(rows) + 1L]] <- .epi_validation_field_finding(row, dictionary, "ERROR", "table_structure", "duplicate_table_row",
        "A form/table/row combination is duplicated.", table_name, as.integer(table$row_index[[i]]))
    }
  }
  core_rules <- rules[rules$rule_type == "table_structure" & !is.na(rules$core_fields) & nzchar(rules$core_fields), , drop = FALSE]
  for (i in seq_len(nrow(core_rules))) {
    rule <- core_rules[i, , drop = FALSE]
    table <- products$tables[[rule$table_name[[1L]]]]
    if (is.null(table) || nrow(table) == 0L) next
    core <- unlist(strsplit(rule$core_fields[[1L]], "|", fixed = TRUE), use.names = FALSE)
    for (j in seq_len(nrow(table))) {
      populated <- vapply(core, function(column) {
        raw_column <- paste0(column, "_raw")
        value <- if (raw_column %in% names(table)) table[[raw_column]][[j]] else if (column %in% names(table)) table[[column]][[j]] else NA_character_
        .epi_validation_populated(value)
      }, logical(1))
      if (!any(populated)) {
        raw_field <- if ("raw_fields" %in% names(table)) strsplit(as.character(table$raw_fields[[j]]), "|", fixed = TRUE)[[1L]][1L] else NA_character_
        row <- tibble::tibble(form_id = as.character(table$form_id[[j]]), raw_field = raw_field)
        rows[[length(rows) + 1L]] <- .epi_validation_field_finding(row, dictionary, as.character(rule$severity[[1L]]), "table_structure", "incomplete_table_record",
          "A repeated-table record exists but has no populated core identifying or contextual field.", rule$table_name[[1L]], as.integer(table$row_index[[j]]))
      }
    }
  }
  .epi_validation_empty_if_none(rows)
}

validate_epi_codes <- function(products, dictionary, rules) {
  rows <- list()
  responses <- products$responses
  for (i in seq_len(nrow(responses))) {
    field <- dictionary$fields[dictionary$fields$raw_field == responses$raw_field[[i]], , drop = FALSE]
    code <- if ("response_code" %in% names(responses)) responses$response_code[[i]] else NA_character_
    if (nrow(field) == 0L || !.epi_validation_populated(responses$raw_value[[i]]) || is.na(code) || !nzchar(code) || is.na(field$codebook_id[[1L]]) || !nzchar(field$codebook_id[[1L]])) next
    allowed <- dictionary$codes$raw_code[dictionary$codes$codebook_id == field$codebook_id[[1L]]]
    if (!as.character(code) %in% as.character(allowed)) rows[[length(rows) + 1L]] <- .epi_validation_field_finding(responses[i, , drop = FALSE], dictionary, "ERROR", "codebook", "unknown_response_code", "A populated response code is not defined in the versioned codebook.")
  }
  multi <- products$multiselect
  option_rule <- rules[rules$rule_id == "multiselect_allowed_options", , drop = FALSE]
  allowed <- if (nrow(option_rule) == 0L) character() else unlist(strsplit(option_rule$allowed_values[[1L]], "|", fixed = TRUE), use.names = FALSE)
  if (nrow(multi) > 0L && length(allowed) > 0L) for (i in seq_len(nrow(multi))) {
    if (!as.character(multi$item_label[[i]]) %in% allowed) {
      meta <- .epi_validation_meta(multi$raw_field[[i]], dictionary)
      rows[[length(rows) + 1L]] <- .epi_validation_finding(multi$form_id[[i]], "ERROR", "codebook", "unknown_multiselect_option",
        meta$section_id, meta$question_id, multi$raw_field[[i]], meta$canonical_name, message = "A selected multiselect option is not defined by the versioned form registry.")
    }
  }
  if (nrow(responses) > 0L) {
    populated <- responses[vapply(responses$raw_value, .epi_validation_populated, logical(1)), , drop = FALSE]
    if (nrow(populated) > 0L) {
      duplicate <- duplicated(paste(populated$form_id, populated$canonical_name, sep = "\r")) | duplicated(paste(populated$form_id, populated$canonical_name, sep = "\r"), fromLast = TRUE)
      for (i in which(duplicate)) rows[[length(rows) + 1L]] <- .epi_validation_field_finding(populated[i, , drop = FALSE], dictionary, "ERROR", "cross_field", "duplicate_scalar_response", "A scalar canonical response is populated more than once for a form.")
    }
  }
  .epi_validation_empty_if_none(rows)
}

.epi_validation_summary <- function(results) {
  if (nrow(results) == 0L) return(tibble::tibble(severity = character(), validation_type = character(), rule_id = character(), n_findings = integer(), n_forms = integer()))
  groups <- unique(results[c("severity", "validation_type", "rule_id")])
  groups$n_findings <- as.integer(vapply(seq_len(nrow(groups)), function(i) {
    sum(results$severity == groups$severity[[i]] & results$validation_type == groups$validation_type[[i]] & results$rule_id == groups$rule_id[[i]])
  }, integer(1)))
  groups$n_forms <- as.integer(vapply(seq_len(nrow(groups)), function(i) {
    length(unique(results$form_id[results$severity == groups$severity[[i]] & results$validation_type == groups$validation_type[[i]] & results$rule_id == groups$rule_id[[i]]]))
  }, integer(1)))
  groups[c("severity", "validation_type", "rule_id", "n_findings", "n_forms")]
}

.epi_validation_form_summary <- function(results, forms) {
  ids <- unique(as.character(forms$form_id))
  counts <- tibble::tibble(form_id = ids, n_info = integer(length(ids)), n_warning = integer(length(ids)), n_error = integer(length(ids)))
  for (i in seq_along(ids)) {
    current <- results[results$form_id == ids[[i]], , drop = FALSE]
    counts$n_info[[i]] <- sum(current$severity == "INFO")
    counts$n_warning[[i]] <- sum(current$severity == "WARNING")
    counts$n_error[[i]] <- sum(current$severity == "ERROR")
  }
  counts$validation_status <- ifelse(counts$n_error > 0L, "error", ifelse(counts$n_warning > 0L, "review", "valid"))
  counts
}

.epi_validation_report <- function(results, summary, form_summary, version) {
  lines <- c("# Initial Epi validation report", "", paste0("Dictionary version: `", version, "`"), "",
    "This report contains counts and rule identifiers only; respondent values and business information are not included.", "",
    "## Findings", "", paste0("- Forms evaluated: ", nrow(form_summary)), paste0("- INFO: ", sum(form_summary$n_info)), paste0("- WARNING: ", sum(form_summary$n_warning)), paste0("- ERROR: ", sum(form_summary$n_error)), "")
  if (nrow(summary) == 0L) lines <- c(lines, "No validation findings were generated.") else {
    lines <- c(lines, "| Severity | Validation type | Rule ID | Findings | Forms |", "|---|---|---|---:|---:|", vapply(seq_len(nrow(summary)), function(i) paste0("| ", summary$severity[[i]], " | ", summary$validation_type[[i]], " | `", summary$rule_id[[i]], "` | ", summary$n_findings[[i]], " | ", summary$n_forms[[i]], " |"), character(1)))
  }
  paste(lines, collapse = "\n")
}

#' Validate collated Initial Epi semantic products
#'
#' `validate_epi()` reads successful extraction and collation products, reports
#' potential data-quality findings, and never repairs or reinterprets entries.
#'
#' @param out_dir Existing successful `extract_epi()`/`collate_epi()` output directory.
#' @param version Versioned Initial Epi dictionary, currently `"2024-05-28"`.
#' @param write Write validation products below `out_dir/validation`.
#' @param strict Return complete findings but signal a warning when an ERROR exists.
#' @param quiet Suppress progress messages.
#' @return A typed tibble with one row per validation finding.
#' @export
validate_epi <- function(out_dir, version = "2024-05-28", write = TRUE, strict = FALSE, quiet = FALSE) {
  out_dir <- validate_scalar_path(out_dir, "out_dir")
  if (!dir.exists(out_dir)) stop("`out_dir` does not exist or is not a directory.", call. = FALSE)
  collated_dir <- fs::path(out_dir, "collated")
  if (!dir.exists(collated_dir)) stop("`out_dir` does not contain a collated/ directory; run extract_epi() and collate_epi() first.", call. = FALSE)
  dictionary <- load_epi_dictionary(version)
  rules <- .epi_validation_rules(version)
  table_files <- fs::path(collated_dir, paste0(.epi_table_output_names, ".csv"))
  required <- c(fs::path(collated_dir, c("epi_forms.csv", "epi_responses_long.csv", "epi_multiselect_responses.csv", "collation_manifest.csv", "collation_parse_diagnostics.csv")), table_files)
  missing <- required[!file.exists(required)]
  if (length(missing) > 0L) stop("Successful collation products are missing: ", paste(fs::path_file(missing), collapse = ", "), call. = FALSE)
  extraction_manifest_path <- fs::path(out_dir, "extraction_manifest.csv")
  if (!file.exists(extraction_manifest_path)) stop("Successful extraction manifest is missing; validate_epi() does not rerun extraction.", call. = FALSE)
  extraction_manifest <- .epi_validation_read(extraction_manifest_path)
  if (nrow(extraction_manifest) == 0L || any(extraction_manifest$status != "success", na.rm = TRUE)) stop("validate_epi() requires a successful extract_epi() manifest.", call. = FALSE)
  collation_manifest <- .epi_validation_read(fs::path(collated_dir, "collation_manifest.csv"))
  if (nrow(collation_manifest) == 0L || any(!collation_manifest$status %in% c("success", "warning"), na.rm = TRUE)) stop("validate_epi() requires successful collate_epi() products.", call. = FALSE)
  products <- list(
    forms = .epi_validation_read(fs::path(collated_dir, "epi_forms.csv")),
    responses = .epi_validation_read(fs::path(collated_dir, "epi_responses_long.csv")),
    multiselect = .epi_validation_read(fs::path(collated_dir, "epi_multiselect_responses.csv")),
    parse_diagnostics = .epi_validation_read(fs::path(collated_dir, "collation_parse_diagnostics.csv")),
    tables = stats::setNames(lapply(table_files, .epi_validation_read), names(.epi_table_output_names))
  )
  results <- dplyr::bind_rows(
    validate_epi_parse(products, dictionary), validate_epi_ranges(products, dictionary),
    validate_epi_dates(products, dictionary), validate_epi_conditionals(products, dictionary, rules),
    validate_epi_tables(products, dictionary, rules), validate_epi_codes(products, dictionary, rules)
  )
  if (nrow(results) == 0L) results <- new_validation_results()
  summary <- .epi_validation_summary(results)
  form_summary <- .epi_validation_form_summary(results, products$forms)
  if (isTRUE(write)) {
    validation_dir <- fs::path(out_dir, "validation")
    dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)
    write_csv_utf8(results, fs::path(validation_dir, "validation_results.csv"))
    write_csv_utf8(summary, fs::path(validation_dir, "validation_summary.csv"))
    write_csv_utf8(form_summary, fs::path(validation_dir, "validation_form_summary.csv"))
    writeLines(.epi_validation_report(results, summary, form_summary, version), fs::path(validation_dir, "validation_report.md"), useBytes = TRUE)
  }
  strict_failure <- isTRUE(strict) && any(results$severity == "ERROR")
  attr(results, "validation_summary") <- summary
  attr(results, "validation_form_summary") <- form_summary
  attr(results, "strict_failure") <- strict_failure
  if (strict_failure) warning("Strict Initial Epi validation found one or more ERROR findings; complete results were returned and written.", call. = FALSE)
  if (!isTRUE(quiet)) cli::cli_inform("Validated {nrow(products$forms)} Initial Epi form{?s}; {nrow(results)} finding{?s} recorded.")
  results
}
