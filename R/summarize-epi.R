.epi_summary_required_tables <- function() {
  c("epi_forms", "epi_responses_long", "epi_multiselect_responses",
    unname(.epi_table_output_names))
}

.epi_summary_read_csv <- function(path, label) {
  if (!file.exists(path)) stop("Required de-identified product is missing: ", label, call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
}

.epi_summary_empty_scalar <- function() {
  tibble::tibble(
    section_id = character(), section_name = character(), question_id = character(),
    subquestion_id = character(), question_text = character(), raw_field = character(),
    canonical_name = character(), response_type = character(), data_type = character(),
    codebook_id = character(), response_code = character(), response_label = character(),
    response_order = integer(), n_cases_total = integer(), n_answered = integer(),
    n_missing = integer(), n_response = integer(), percent_of_answered = double(),
    percent_of_all_cases = double()
  )
}

.epi_summary_empty_multiselect <- function() {
  tibble::tibble(
    section_id = character(), section_name = character(), question_id = character(),
    question_text = character(), canonical_name = character(), item_code = character(),
    item_label = character(), n_cases_total = integer(),
    n_cases_with_any_selection = integer(), n_cases_selecting_item = integer(),
    percent_of_selecting_cases = double(), percent_of_all_cases = double()
  )
}

.epi_summary_empty_repeated_categorical <- function() {
  tibble::tibble(
    section_id = character(), section_name = character(), question_id = character(),
    subquestion_id = character(), question_text = character(), canonical_name = character(),
    table_name = character(), column_name = character(), codebook_id = character(),
    response_code = character(), response_label = character(), response_order = integer(),
    n_records_total = integer(), n_nonmissing = integer(), n_value = integer(),
    n_cases_with_value = integer(), percent_of_nonmissing = double()
  )
}

.epi_summary_empty_numeric <- function() {
  tibble::tibble(
    section_id = character(), section_name = character(), question_id = character(),
    canonical_name = character(), table_name = character(), column_name = character(),
    units = character(), scope = character(), unit_of_analysis = character(),
    n_total = integer(), n_populated = integer(), n_parsed = integer(),
    n_unparsed = integer(), n_missing = integer(), mean = double(), sd = double(),
    median = double(), q25 = double(), q75 = double(), min = double(), max = double()
  )
}

.epi_summary_empty_date <- function() {
  tibble::tibble(
    section_id = character(), section_name = character(), question_id = character(),
    canonical_name = character(), table_name = character(), column_name = character(),
    scope = character(), unit_of_analysis = character(), n_total = integer(),
    n_populated = integer(), n_parsed = integer(), n_unparsed = integer(),
    n_missing = integer(), min_date = as.Date(character()),
    median_date = as.Date(character()), max_date = as.Date(character())
  )
}

.epi_summary_empty_case_table <- function() {
  tibble::tibble(case_id = character(), table_name = character(), n_records = integer())
}

.epi_summary_empty_repeated_table <- function() {
  tibble::tibble(
    table_name = character(), n_cases_total = integer(), n_cases_with_records = integer(),
    n_records_total = integer(), mean_records_per_case = double(),
    sd_records_per_case = double(), median_records_per_case = double(),
    q25_records_per_case = double(), q75_records_per_case = double(),
    min_records_per_case = double(), max_records_per_case = double()
  )
}

.epi_summary_empty_validation_rule <- function() {
  tibble::tibble(
    severity = character(), validation_type = character(), rule_id = character(),
    n_findings = integer(), n_cases = integer()
  )
}

.epi_summary_empty_validation_status <- function() {
  tibble::tibble(
    validation_status = character(), n_cases = integer(), percent_of_cases = double()
  )
}

.epi_summary_empty_inventory <- function() {
  tibble::tibble(
    section_id = character(), section_name = character(), question_id = character(),
    subquestion_id = character(), question_text = character(), raw_field = character(),
    canonical_name = character(), table_name = character(), column_name = character(),
    response_type = character(), data_type = character(), units = character(),
    codebook_id = character(), privacy_class = character(), privacy_action = character(),
    summary_type = character(), summary_status = character(), notes = character()
  )
}

.epi_summary_empty_overview <- function() {
  tibble::tibble(
    form_version = character(), profile = character(),
    source_deidentification_status = character(), validation_available = logical(),
    n_cases = integer(), n_counties = integer(), first_interview_date = as.Date(character()),
    last_interview_date = as.Date(character()), n_validation_valid = integer(),
    n_validation_review = integer(), n_validation_error = integer(),
    n_repeated_tables = integer(), n_repeated_records = integer()
  )
}

.epi_summary_empty_manifest <- function() {
  tibble::tibble(
    summary_schema_version = integer(), form_version = character(), profile = character(),
    source_policy_hash = character(), source_deidentification_status = character(),
    validation_available = logical(), n_cases = integer(), n_summary_products = integer(),
    n_supported_fields = integer(), n_privacy_excluded_fields = integer(),
    n_unsupported_retained_fields = integer(), status = character(),
    package_version = character(), created_at = character()
  )
}

.epi_summary_populated <- function(x) {
  if (length(x) == 0L || is.na(x[[1L]])) return(FALSE)
  isTRUE(x[[1L]]) || identical(as.character(x[[1L]]), "TRUE") ||
    identical(as.character(x[[1L]]), "1")
}

.epi_summary_value_populated <- function(x) {
  if (length(x) == 0L || is.na(x[[1L]])) return(FALSE)
  nzchar(trimws(as.character(x[[1L]])))
}

.epi_summary_response_populated <- function(data) {
  if (nrow(data) == 0L) return(logical())
  vapply(seq_len(nrow(data)), function(i) {
    if ("is_populated" %in% names(data) &&
        !is.na(data$is_populated[[i]])) {
      return(.epi_summary_populated(data$is_populated[[i]]))
    }
    candidate <- if ("raw_value" %in% names(data)) data$raw_value[[i]] else
      if ("value" %in% names(data)) data$value[[i]] else NA_character_
    .epi_summary_value_populated(candidate)
  }, logical(1))
}

.epi_summary_response_label <- function(data) {
  value <- if ("response_label" %in% names(data)) as.character(data$response_label) else
    rep(NA_character_, nrow(data))
  fallback <- if ("value" %in% names(data)) as.character(data$value) else
    rep(NA_character_, nrow(data))
  missing <- is.na(value) | !nzchar(value)
  value[missing] <- fallback[missing]
  value
}

.epi_summary_date_vector <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- as.character(x)
  ok <- is.na(x) | !nzchar(x) | grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)
  if (any(!ok)) return(as.Date(rep(NA_character_, length(x))))
  as.Date(x)
}

.epi_summary_stats <- function(values) {
  values <- as.numeric(values[!is.na(values)])
  if (length(values) == 0L) {
    return(list(mean = NA_real_, sd = NA_real_, median = NA_real_, q25 = NA_real_,
                q75 = NA_real_, min = NA_real_, max = NA_real_))
  }
  qs <- stats::quantile(values, probs = c(0.25, 0.75), names = FALSE, type = 7)
  list(mean = mean(values), sd = if (length(values) > 1L) stats::sd(values) else NA_real_,
       median = stats::median(values), q25 = qs[[1L]], q75 = qs[[2L]],
       min = min(values), max = max(values))
}

.epi_summary_metadata <- function(row, include_table = FALSE) {
  values <- list(
    section_id = as.character(row$section_id[[1L]]),
    section_name = as.character(row$section_name[[1L]]),
    question_id = as.character(row$question_id[[1L]]),
    question_text = as.character(row$question_text[[1L]]),
    canonical_name = as.character(row$canonical_name[[1L]])
  )
  if ("subquestion_id" %in% names(row)) values$subquestion_id <- as.character(row$subquestion_id[[1L]])
  if (isTRUE(include_table)) {
    values$table_name <- as.character(row$table_name[[1L]])
    values$column_name <- as.character(row$column_name[[1L]])
  }
  values
}

.epi_summary_raw_fields <- function(value) {
  if (is.na(value) || !nzchar(value)) character() else
    unlist(strsplit(as.character(value), "|", fixed = TRUE), use.names = FALSE)
}

.epi_summary_rows_for_group <- function(table, raw_fields) {
  if (nrow(table) == 0L || !"raw_fields" %in% names(table)) return(table)
  keep <- vapply(table$raw_fields, function(x)
    length(intersect(.epi_summary_raw_fields(x), raw_fields)) > 0L, logical(1))
  table[keep, , drop = FALSE]
}

.epi_summary_check_policy <- function(policy, dictionary) {
  required <- c("profile", "raw_field", "canonical_name", "table_name",
                "privacy_class", "action", "pseudonym_class")
  missing <- setdiff(required, names(policy))
  if (length(missing) > 0L) stop(
    "Privacy policy snapshot is missing required columns: ", paste(missing, collapse = ", "),
    call. = FALSE)
  if (anyDuplicated(as.character(policy$raw_field))) stop(
    "Privacy policy snapshot raw fields must be unique.", call. = FALSE)
  if (nrow(policy) != nrow(dictionary$fields) ||
      !setequal(as.character(policy$raw_field), as.character(dictionary$fields$raw_field))) {
    stop("Privacy policy snapshot does not cover the requested semantic dictionary exactly.",
         call. = FALSE)
  }
  if (any(is.na(policy$profile) | as.character(policy$profile) != "analysis")) {
    stop("Privacy policy snapshot must use profile analysis.", call. = FALSE)
  }
  classes <- c("direct_identifier", "quasi_identifier", "sensitive_free_text",
               "provenance_link", "non_identifier")
  actions <- c("retain", "drop", "pseudonymize", "coarsen", "review_remove")
  if (any(is.na(policy$privacy_class) | !policy$privacy_class %in% classes) ||
      any(is.na(policy$action) | !policy$action %in% actions)) {
    stop("Privacy policy snapshot contains unsupported privacy classes or actions.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.epi_summary_build_inventory <- function(dictionary, policy) {
  fields <- dictionary$fields
  index <- match(as.character(fields$raw_field), as.character(policy$raw_field))
  fields$privacy_class <- as.character(policy$privacy_class[index])
  fields$privacy_action <- as.character(policy$action[index])
  scalar <- is.na(fields$table_name) | !nzchar(as.character(fields$table_name))
  multi <- scalar & as.character(fields$response_type) == "multiselect"
  fields$.summary_key <- ifelse(
    multi, paste("multiselect", fields$question_id, sep = "|"),
    ifelse(scalar, paste("scalar", fields$canonical_name, sep = "|"),
      paste("repeated", fields$table_name, fields$canonical_name, fields$column_name,
        ifelse(is.na(fields$codebook_id), "", fields$codebook_id), sep = "|")))
  groups <- split(seq_len(nrow(fields)), fields$.summary_key)
  rows <- purrr::map_dfr(groups, function(indexes) {
    current <- fields[indexes, , drop = FALSE]
    current <- current[order(as.integer(current$source_page), as.integer(current$row_index),
      as.character(current$raw_field)), , drop = FALSE]
    first <- current[1L, , drop = FALSE]
    unique_actions <- unique(current$privacy_action)
    unique_classes <- unique(current$privacy_class)
    if (length(unique_actions) != 1L || length(unique_classes) != 1L) stop(
      "Ambiguous privacy metadata for semantic variable ", first$canonical_name[[1L]], ".",
      call. = FALSE)
    unique_types <- unique(as.character(current$data_type))
    unique_units <- unique(as.character(current$units[!is.na(current$units)]))
    unique_codes <- unique(as.character(current$codebook_id[
      !is.na(current$codebook_id) & nzchar(current$codebook_id)]))
    if (length(unique_types) > 1L || length(unique_units) > 1L || length(unique_codes) > 1L) {
      stop("Ambiguous semantic metadata for ", first$canonical_name[[1L]],
           "; repeated definitions disagree.", call. = FALSE)
    }
    is_scalar <- is.na(first$table_name) || !nzchar(as.character(first$table_name))
    is_multi <- is_scalar && as.character(first$response_type[[1L]]) == "multiselect"
    summary_type <- if (unique_actions[[1L]] != "retain") "excluded_privacy" else
      if (is_multi) "multiselect" else
      if (is_scalar && first$data_type[[1L]] == "numeric") "numeric_scalar" else
      if (is_scalar && first$data_type[[1L]] == "date") "date_scalar" else
      if (is_scalar) "categorical_scalar" else
      if (first$data_type[[1L]] == "numeric") "numeric_repeated" else
      if (first$data_type[[1L]] == "date") "date_repeated" else "categorical_repeated"
    supported <- summary_type != "excluded_privacy"
    note <- if (summary_type == "categorical_repeated" &&
                unique(first$data_type) == "date_text") {
      "Retained date_text is reported as a category; no date parser is applied."
    } else if (!supported) {
      "Excluded from substantive summaries by the source privacy policy."
    } else NA_character_
    tibble::tibble(
      section_id = as.character(first$section_id[[1L]]),
      section_name = as.character(first$section_name[[1L]]),
      question_id = as.character(first$question_id[[1L]]),
      subquestion_id = as.character(first$subquestion_id[[1L]]),
      question_text = as.character(first$question_text[[1L]]),
      raw_field = paste(as.character(current$raw_field), collapse = "|"),
      canonical_name = as.character(first$canonical_name[[1L]]),
      table_name = as.character(first$table_name[[1L]]),
      column_name = as.character(first$column_name[[1L]]),
      response_type = as.character(first$response_type[[1L]]),
      data_type = as.character(first$data_type[[1L]]),
      units = if (length(unique_units) == 0L) NA_character_ else unique_units[[1L]],
      codebook_id = if (length(unique_codes) == 0L) NA_character_ else unique_codes[[1L]],
      privacy_class = unique_classes[[1L]], privacy_action = unique_actions[[1L]],
      summary_type = summary_type,
      summary_status = if (supported) "summarized" else "excluded_privacy",
      notes = note,
      .summary_key = first$.summary_key[[1L]],
      .source_order = (if (all(is.na(current$source_page))) 9999L else
        min(as.integer(current$source_page), na.rm = TRUE)) * 10000L +
        (if (all(is.na(current$row_index))) 9999L else
          min(as.integer(current$row_index), na.rm = TRUE))
    )
  })
  rows <- rows[order(rows$.source_order, rows$question_id, rows$canonical_name,
    rows$codebook_id), , drop = FALSE]
  rows$.summary_key <- NULL
  rows$.source_order <- NULL
  rows
}

.epi_summary_validate_inputs <- function(deidentified_dir, version) {
  if (!dir.exists(deidentified_dir)) stop(
    "deidentified_dir does not exist or is not a directory.", call. = FALSE)
  collated_dir <- fs::path(deidentified_dir, "collated")
  privacy_dir <- fs::path(deidentified_dir, "privacy")
  if (!dir.exists(collated_dir) || !dir.exists(privacy_dir)) stop(
    "deidentified_dir must contain collated/ and privacy/ directories.", call. = FALSE)
  manifest <- .epi_summary_read_csv(fs::path(privacy_dir, "deidentification_manifest.csv"),
    "privacy/deidentification_manifest.csv")
  policy <- .epi_summary_read_csv(fs::path(privacy_dir, "policy_snapshot.csv"),
    "privacy/policy_snapshot.csv")
  if (nrow(manifest) != 1L ||
      !all(c("form_version", "profile", "status", "privacy_errors") %in% names(manifest))) {
    stop("De-identification manifest must contain one complete status row.", call. = FALSE)
  }
  if (as.character(manifest$form_version[[1L]]) != version) stop(
    "De-identification form version does not match version.", call. = FALSE)
  if (as.character(manifest$profile[[1L]]) != "analysis") stop(
    "De-identified input must use profile analysis.", call. = FALSE)
  privacy_errors <- suppressWarnings(as.numeric(manifest$privacy_errors[[1L]]))
  if (is.na(privacy_errors)) stop("De-identification manifest has an invalid privacy_errors value.",
                                  call. = FALSE)
  dictionary <- load_epi_dictionary(version)
  .epi_summary_check_policy(policy, dictionary)
  required <- .epi_summary_required_tables()
  missing <- required[!file.exists(fs::path(collated_dir, paste0(required, ".csv")))]
  if (length(missing) > 0L) stop(
    "De-identified collated products are missing: ",
    paste(paste0(missing, ".csv"), collapse = ", "), call. = FALSE)
  products <- list()
  for (name in required) products[[name]] <- .epi_summary_read_csv(
    fs::path(collated_dir, paste0(name, ".csv")), paste0("collated/", name, ".csv"))
  forbidden <- c("form_id", "source_file", "source_relpath", "source_path", "source_sha256")
  forms <- products$epi_forms
  if (!"case_id" %in% names(forms)) stop(
    "De-identified epi_forms.csv must contain case_id.", call. = FALSE)
  if (anyDuplicated(as.character(forms$case_id)) ||
      any(is.na(forms$case_id) | !nzchar(as.character(forms$case_id)))) stop(
    "De-identified case_id values must be unique and populated.", call. = FALSE)
  for (name in names(products)) {
    bad_columns <- intersect(forbidden, names(products[[name]]))
    if (length(bad_columns) > 0L) stop(
      "Identifiable-source columns detected in de-identified product ", name, ": ",
      paste(bad_columns, collapse = ", "), call. = FALSE)
    if (!"case_id" %in% names(products[[name]])) stop(
      "De-identified product ", name, " must contain case_id.", call. = FALSE)
    unknown <- setdiff(unique(as.character(products[[name]]$case_id)),
      as.character(forms$case_id))
    unknown <- unknown[!is.na(unknown) & nzchar(unknown)]
    if (length(unknown) > 0L) stop(
      "Product ", name, " contains case_id values absent from epi_forms.csv.", call. = FALSE)
  }
  list(manifest = manifest, policy = policy, dictionary = dictionary,
       products = products, privacy_errors = privacy_errors)
}

.epi_summary_validation <- function(deidentified_dir, forms, strict) {
  validation_dir <- fs::path(deidentified_dir, "validation")
  form_path <- fs::path(validation_dir, "validation_form_summary.csv")
  available <- file.exists(form_path)
  if (!available && isTRUE(strict)) stop(
    "Validation products are required in strict mode; run validate_epi() before summarizing.",
    call. = FALSE)
  if (!available) return(list(available = FALSE, status = "review",
    rules = .epi_summary_empty_validation_rule(),
    statuses = .epi_summary_empty_validation_status()))
  form_summary <- .epi_summary_read_csv(form_path, "validation/validation_form_summary.csv")
  if (!all(c("case_id", "validation_status") %in% names(form_summary))) stop(
    "Validation form summary must contain case_id and validation_status.", call. = FALSE)
  if (anyDuplicated(as.character(form_summary$case_id))) stop(
    "Validation form summary contains duplicate case_id values.", call. = FALSE)
  if (!setequal(as.character(form_summary$case_id), as.character(forms$case_id))) stop(
    "Validation form summary does not cover epi_forms.csv exactly.", call. = FALSE)
  statuses <- as.character(form_summary$validation_status)
  if (any(is.na(statuses) | !statuses %in% c("valid", "review", "error"))) stop(
    "Validation status contains unsupported levels.", call. = FALSE)
  status_rows <- tibble::tibble(validation_status = c("valid", "review", "error"))
  status_rows$n_cases <- as.integer(vapply(status_rows$validation_status,
    function(x) sum(statuses == x), integer(1)))
  status_rows$percent_of_cases <- if (nrow(forms) == 0L) NA_real_ else
    100 * status_rows$n_cases / nrow(forms)
  results_path <- fs::path(validation_dir, "validation_results.csv")
  summary_path <- fs::path(validation_dir, "validation_summary.csv")
  if (file.exists(results_path)) {
    results <- .epi_summary_read_csv(results_path, "validation/validation_results.csv")
    if ("case_id" %in% names(results)) {
      unknown <- setdiff(unique(as.character(results$case_id)),
        as.character(forms$case_id))
      unknown <- unknown[!is.na(unknown) & nzchar(unknown)]
      if (length(unknown) > 0L) stop(
        "Validation results contain case_id values absent from epi_forms.csv.",
        call. = FALSE)
    }
    if (nrow(results) == 0L) rules <- .epi_summary_empty_validation_rule() else {
      required <- c("severity", "validation_type", "rule_id")
      if (!all(required %in% names(results))) stop(
        "Validation results are missing rule metadata.", call. = FALSE)
      groups <- split(seq_len(nrow(results)),
        interaction(results[required], drop = TRUE, lex.order = TRUE, sep = "\r"))
      rules <- purrr::map_dfr(groups, function(index) {
        current <- results[index, , drop = FALSE]
        tibble::tibble(
          severity = as.character(current$severity[[1L]]),
          validation_type = as.character(current$validation_type[[1L]]),
          rule_id = as.character(current$rule_id[[1L]]),
          n_findings = as.integer(nrow(current)),
          n_cases = as.integer(if ("case_id" %in% names(current))
            length(unique(current$case_id[!is.na(current$case_id)])) else NA_integer_)
        )
      })
    }
  } else if (file.exists(summary_path)) {
    old <- .epi_summary_read_csv(summary_path, "validation/validation_summary.csv")
    if (!all(c("severity", "validation_type", "rule_id", "n_findings") %in% names(old))) stop(
      "Validation summary is missing rule metadata.", call. = FALSE)
    rules <- tibble::tibble(
      severity = as.character(old$severity), validation_type = as.character(old$validation_type),
      rule_id = as.character(old$rule_id), n_findings = as.integer(old$n_findings),
      n_cases = as.integer(if ("n_forms" %in% names(old)) old$n_forms else NA_integer_)
    )
  } else rules <- .epi_summary_empty_validation_rule()
  list(available = TRUE, status = "passed", rules = rules, statuses = status_rows)
}

.epi_summary_scalar <- function(inventory, responses, n_cases, codes) {
  out <- .epi_summary_empty_scalar()
  rows <- list()
  candidates <- inventory[inventory$summary_type == "categorical_scalar", , drop = FALSE]
  for (i in seq_len(nrow(candidates))) {
    meta <- candidates[i, , drop = FALSE]
    raw_fields <- .epi_summary_raw_fields(meta$raw_field[[1L]])
    raw <- responses[responses$raw_field %in% raw_fields, , drop = FALSE]
    if (nrow(raw) > 0L &&
        anyDuplicated(paste(raw$case_id, raw$raw_field, sep = "\r"))) stop(
      "Scalar semantic responses are duplicated by case and raw field.", call. = FALSE)
    labels <- .epi_summary_response_label(raw)
    answered <- .epi_summary_response_populated(raw) &
      !is.na(labels) & nzchar(trimws(labels))
    codebook <- as.character(meta$codebook_id[[1L]])
    code_rows <- if (!is.na(codebook) && nzchar(codebook))
      codes[codes$codebook_id == codebook, , drop = FALSE] else tibble::tibble()
    observed <- if (nrow(raw) == 0L) character() else as.character(raw$response_code[answered])
    labels <- if (nrow(raw) == 0L) character() else as.character(labels[answered])
    if (nrow(code_rows) > 0L) {
      for (j in seq_len(nrow(code_rows))) {
        code <- as.character(code_rows$raw_code[[j]])
        count <- sum(observed == code, na.rm = TRUE)
        rows[[length(rows) + 1L]] <- c(.epi_summary_metadata(meta), list(
          raw_field = raw_fields[[1L]], response_type = as.character(meta$response_type[[1L]]),
          data_type = as.character(meta$data_type[[1L]]), codebook_id = codebook,
          response_code = code, response_label = as.character(code_rows$response_label[[j]]),
          response_order = as.integer(code_rows$response_order[[j]]),
          n_cases_total = as.integer(n_cases), n_answered = as.integer(sum(answered)),
          n_missing = as.integer(n_cases - sum(answered)), n_response = as.integer(count),
          percent_of_answered = if (sum(answered) == 0L) NA_real_ else
            100 * count / sum(answered),
          percent_of_all_cases = if (n_cases == 0L) NA_real_ else 100 * count / n_cases))
      }
    } else {
      counts <- if (length(labels) == 0L) integer() else table(labels)
      if (length(counts) > 0L) {
        labels_ordered <- names(sort(counts, decreasing = TRUE))
        labels_ordered <- labels_ordered[order(-as.integer(counts[labels_ordered]), labels_ordered)]
        for (label in labels_ordered) {
          count <- as.integer(counts[[label]])
          rows[[length(rows) + 1L]] <- c(.epi_summary_metadata(meta), list(
            raw_field = raw_fields[[1L]], response_type = as.character(meta$response_type[[1L]]),
            data_type = as.character(meta$data_type[[1L]]), codebook_id = NA_character_,
            response_code = NA_character_, response_label = label, response_order = NA_integer_,
            n_cases_total = as.integer(n_cases), n_answered = as.integer(sum(answered)),
            n_missing = as.integer(n_cases - sum(answered)), n_response = count,
            percent_of_answered = if (sum(answered) == 0L) NA_real_ else
              100 * count / sum(answered),
            percent_of_all_cases = if (n_cases == 0L) NA_real_ else 100 * count / n_cases))
        }
      }
    }
  }
  if (length(rows) == 0L) out else dplyr::bind_rows(rows)
}

.epi_summary_multiselect <- function(inventory, responses, selected, n_cases) {
  out <- .epi_summary_empty_multiselect()
  rows <- list()
  candidates <- inventory[inventory$summary_type == "multiselect", , drop = FALSE]
  for (i in seq_len(nrow(candidates))) {
    meta <- candidates[i, , drop = FALSE]
    raw_fields <- .epi_summary_raw_fields(meta$raw_field[[1L]])
    raw <- responses[responses$raw_field %in% raw_fields, , drop = FALSE]
    response_populated <- .epi_summary_response_populated(raw)
    any_cases <- if (nrow(raw) == 0L) character() else
      unique(as.character(raw$case_id[response_populated]))
    any_cases <- any_cases[!is.na(any_cases)]
    chosen <- selected[selected$raw_field %in% raw_fields, , drop = FALSE]
    if (nrow(chosen) == 0L) next
    chosen$item_code <- as.character(chosen$item_code)
    chosen$item_label <- as.character(chosen$item_label)
    groups <- split(seq_len(nrow(chosen)),
      paste(chosen$item_code, chosen$item_label, sep = "\r"), drop = TRUE)
    for (index in groups) {
      current <- chosen[index, , drop = FALSE]
      item_cases <- unique(as.character(current$case_id))
      item_cases <- item_cases[!is.na(item_cases)]
      metadata <- .epi_summary_metadata(meta)
      metadata$canonical_name <- as.character(meta$question_id[[1L]])
      rows[[length(rows) + 1L]] <- c(metadata, list(
        item_code = as.character(current$item_code[[1L]]),
        item_label = as.character(current$item_label[[1L]]),
        n_cases_total = as.integer(n_cases),
        n_cases_with_any_selection = as.integer(length(any_cases)),
        n_cases_selecting_item = as.integer(length(item_cases)),
        percent_of_selecting_cases = if (length(any_cases) == 0L) NA_real_ else
          100 * length(item_cases) / length(any_cases),
        percent_of_all_cases = if (n_cases == 0L) NA_real_ else
          100 * length(item_cases) / n_cases))
    }
  }
  if (length(rows) == 0L) out else
    dplyr::bind_rows(rows) |> dplyr::arrange(section_id, question_id, item_code, item_label)
}

.epi_summary_repeated_categorical <- function(inventory, tables, codes) {
  out <- .epi_summary_empty_repeated_categorical()
  rows <- list()
  candidates <- inventory[inventory$summary_type == "categorical_repeated", , drop = FALSE]
  for (i in seq_len(nrow(candidates))) {
    meta <- candidates[i, , drop = FALSE]
    table_name <- as.character(meta$table_name[[1L]])
    column <- as.character(meta$column_name[[1L]])
    table <- .epi_summary_rows_for_group(tables[[table_name]],
      .epi_summary_raw_fields(meta$raw_field[[1L]]))
    if (!column %in% names(table)) stop(
      "Repeated product ", table_name, " is missing semantic column ", column, ".",
      call. = FALSE)
    raw_column <- paste0(column, "_raw")
    value <- if (raw_column %in% names(table)) table[[raw_column]] else table[[column]]
    populated <- if (nrow(table) == 0L) logical() else
      vapply(value, .epi_summary_value_populated, logical(1))
    labels <- if (nrow(table) == 0L) character() else as.character(table[[column]])
    labels[!populated] <- NA_character_
    codebook <- as.character(meta$codebook_id[[1L]])
    code_column <- paste0(column, "_code")
    code_rows <- if (!is.na(codebook) && nzchar(codebook))
      codes[codes$codebook_id == codebook, , drop = FALSE] else tibble::tibble()
    observed_codes <- rep(NA_character_, nrow(table))
    if (code_column %in% names(table)) observed_codes <- as.character(table[[code_column]])
    if (nrow(code_rows) > 0L) for (j in seq_len(nrow(code_rows))) {
      code <- as.character(code_rows$raw_code[[j]])
      hit <- populated & observed_codes == code
      case_values <- unique(as.character(table$case_id[hit]))
      case_values <- case_values[!is.na(case_values)]
      rows[[length(rows) + 1L]] <- c(.epi_summary_metadata(meta, TRUE), list(
        codebook_id = codebook, response_code = code,
        response_label = as.character(code_rows$response_label[[j]]),
        response_order = as.integer(code_rows$response_order[[j]]),
        n_records_total = as.integer(nrow(table)), n_nonmissing = as.integer(sum(populated)),
        n_value = as.integer(sum(hit, na.rm = TRUE)),
        n_cases_with_value = as.integer(length(case_values)),
        percent_of_nonmissing = if (sum(populated) == 0L) NA_real_ else
          100 * sum(hit, na.rm = TRUE) / sum(populated)))
    } else {
      counts <- if (length(labels[populated]) == 0L) integer() else table(labels[populated])
      if (length(counts) > 0L) for (label in names(counts)[order(-as.integer(counts), names(counts))]) {
        hit <- populated & labels == label
        case_values <- unique(as.character(table$case_id[hit]))
        case_values <- case_values[!is.na(case_values)]
        rows[[length(rows) + 1L]] <- c(.epi_summary_metadata(meta, TRUE), list(
          codebook_id = NA_character_, response_code = NA_character_, response_label = label,
          response_order = NA_integer_, n_records_total = as.integer(nrow(table)),
          n_nonmissing = as.integer(sum(populated)), n_value = as.integer(sum(hit)),
          n_cases_with_value = as.integer(length(case_values)),
          percent_of_nonmissing = 100 * sum(hit) / sum(populated)))
      }
    }
  }
  if (length(rows) == 0L) out else dplyr::bind_rows(rows) |>
    dplyr::arrange(table_name, section_id, question_id, response_order, response_label)
}

.epi_summary_numeric <- function(inventory, responses, tables, n_cases) {
  out <- .epi_summary_empty_numeric()
  rows <- list()
  candidates <- inventory[inventory$summary_type %in%
    c("numeric_scalar", "numeric_repeated"), , drop = FALSE]
  for (i in seq_len(nrow(candidates))) {
    meta <- candidates[i, , drop = FALSE]
    repeated <- meta$summary_type[[1L]] == "numeric_repeated"
    if (!repeated) {
      raw <- responses[responses$raw_field %in%
        .epi_summary_raw_fields(meta$raw_field[[1L]]), , drop = FALSE]
      populated <- .epi_summary_response_populated(raw)
      parsed <- if (nrow(raw) == 0L) numeric() else
        suppressWarnings(as.numeric(raw$numeric_value))
      n_total <- n_cases
      values <- parsed[populated & !is.na(parsed)]
    } else {
      table <- .epi_summary_rows_for_group(tables[[meta$table_name[[1L]]]],
        .epi_summary_raw_fields(meta$raw_field[[1L]]))
      n_total <- nrow(table)
      raw_column <- paste0(meta$column_name[[1L]], "_raw")
      raw_value <- if (raw_column %in% names(table)) table[[raw_column]] else
        table[[meta$column_name[[1L]]]]
      populated <- if (nrow(table) == 0L) logical() else
        vapply(raw_value, .epi_summary_value_populated, logical(1))
      parsed <- if (nrow(table) == 0L) numeric() else
        suppressWarnings(as.numeric(table[[meta$column_name[[1L]]]]))
      values <- parsed[populated & !is.na(parsed)]
    }
    n_populated <- sum(populated)
    n_parsed <- sum(populated & !is.na(parsed))
    s <- .epi_summary_stats(values)
    rows[[length(rows) + 1L]] <- c(.epi_summary_metadata(meta, repeated), list(
      units = as.character(meta$units[[1L]]), scope = if (repeated) "repeated" else "scalar",
      unit_of_analysis = if (repeated) "record" else "case", n_total = as.integer(n_total),
      n_populated = as.integer(n_populated), n_parsed = as.integer(n_parsed),
      n_unparsed = as.integer(n_populated - n_parsed),
      n_missing = as.integer(n_total - n_populated), mean = s$mean, sd = s$sd,
      median = s$median, q25 = s$q25, q75 = s$q75, min = s$min, max = s$max))
  }
  if (length(rows) == 0L) out else dplyr::bind_rows(rows) |>
    dplyr::arrange(scope, section_id, question_id, canonical_name, table_name, column_name)
}

.epi_summary_dates <- function(inventory, responses, tables, n_cases) {
  out <- .epi_summary_empty_date()
  rows <- list()
  candidates <- inventory[inventory$summary_type %in%
    c("date_scalar", "date_repeated"), , drop = FALSE]
  for (i in seq_len(nrow(candidates))) {
    meta <- candidates[i, , drop = FALSE]
    repeated <- meta$summary_type[[1L]] == "date_repeated"
    if (!repeated) {
      raw <- responses[responses$raw_field %in%
        .epi_summary_raw_fields(meta$raw_field[[1L]]), , drop = FALSE]
      n_total <- n_cases
      populated <- .epi_summary_response_populated(raw)
      parsed <- if (nrow(raw) == 0L) as.Date(character()) else
        .epi_summary_date_vector(raw$date_value)
    } else {
      table <- .epi_summary_rows_for_group(tables[[meta$table_name[[1L]]]],
        .epi_summary_raw_fields(meta$raw_field[[1L]]))
      n_total <- nrow(table)
      raw_column <- paste0(meta$column_name[[1L]], "_raw")
      raw_value <- if (raw_column %in% names(table)) table[[raw_column]] else
        table[[meta$column_name[[1L]]]]
      populated <- if (nrow(table) == 0L) logical() else
        vapply(raw_value, .epi_summary_value_populated, logical(1))
      parsed <- if (nrow(table) == 0L) as.Date(character()) else
        .epi_summary_date_vector(table[[meta$column_name[[1L]]]])
    }
    values <- parsed[populated & !is.na(parsed)]
    n_populated <- sum(populated)
    n_parsed <- sum(populated & !is.na(parsed))
    med <- if (length(values) == 0L) as.Date(NA) else
      as.Date(stats::median(as.numeric(values)), origin = "1970-01-01")
    rows[[length(rows) + 1L]] <- c(.epi_summary_metadata(meta, repeated), list(
      scope = if (repeated) "repeated" else "scalar",
      unit_of_analysis = if (repeated) "record" else "case", n_total = as.integer(n_total),
      n_populated = as.integer(n_populated), n_parsed = as.integer(n_parsed),
      n_unparsed = as.integer(n_populated - n_parsed),
      n_missing = as.integer(n_total - n_populated),
      min_date = if (length(values) == 0L) as.Date(NA) else min(values),
      median_date = med, max_date = if (length(values) == 0L) as.Date(NA) else max(values)))
  }
  if (length(rows) == 0L) out else dplyr::bind_rows(rows) |>
    dplyr::arrange(scope, section_id, question_id, canonical_name, table_name, column_name)
}

.epi_summary_repeated_tables <- function(forms, tables, registry) {
  cases <- sort(unique(as.character(forms$case_id)))
  case_rows <- list()
  summary_rows <- list()
  for (table_name in as.character(registry$table_name)) {
    table <- tables[[table_name]]
    counts <- tabulate(match(as.character(table$case_id), cases), nbins = length(cases))
    names(counts) <- cases
    case_rows[[length(case_rows) + 1L]] <- tibble::tibble(
      case_id = cases, table_name = table_name, n_records = as.integer(counts))
    s <- .epi_summary_stats(as.numeric(counts))
    summary_rows[[length(summary_rows) + 1L]] <- tibble::tibble(
      table_name = table_name, n_cases_total = as.integer(length(cases)),
      n_cases_with_records = as.integer(sum(counts > 0L)),
      n_records_total = as.integer(nrow(table)), mean_records_per_case = s$mean,
      sd_records_per_case = s$sd, median_records_per_case = s$median,
      q25_records_per_case = s$q25, q75_records_per_case = s$q75,
      min_records_per_case = s$min, max_records_per_case = s$max)
  }
  list(
    case_table_counts = if (length(case_rows) == 0L) .epi_summary_empty_case_table() else
      dplyr::bind_rows(case_rows),
    repeated_table_summaries = if (length(summary_rows) == 0L)
      .epi_summary_empty_repeated_table() else dplyr::bind_rows(summary_rows)
  )
}

.epi_summary_overview <- function(forms, responses, validation, manifest, repeated) {
  dates <- responses[responses$canonical_name == "today_date", , drop = FALSE]
  date_values <- if (nrow(dates) == 0L) as.Date(character()) else
    .epi_summary_date_vector(dates$date_value)
  date_values <- date_values[!is.na(date_values)]
  counties <- if ("premises_county" %in% names(forms))
    as.character(forms$premises_county) else character()
  counties <- unique(counties[!is.na(counties) & nzchar(trimws(counties))])
  statuses <- validation$statuses
  get_status <- function(x) if (nrow(statuses) == 0L) 0L else
    as.integer(statuses$n_cases[statuses$validation_status == x])
  tibble::tibble(
    form_version = as.character(manifest$form_version[[1L]]),
    profile = as.character(manifest$profile[[1L]]),
    source_deidentification_status = as.character(manifest$status[[1L]]),
    validation_available = isTRUE(validation$available), n_cases = as.integer(nrow(forms)),
    n_counties = as.integer(length(counties)),
    first_interview_date = if (length(date_values) == 0L) as.Date(NA) else min(date_values),
    last_interview_date = if (length(date_values) == 0L) as.Date(NA) else max(date_values),
    n_validation_valid = get_status("valid"), n_validation_review = get_status("review"),
    n_validation_error = get_status("error"),
    n_repeated_tables = as.integer(nrow(repeated$repeated_table_summaries)),
    n_repeated_records = as.integer(sum(repeated$repeated_table_summaries$n_records_total))
  )
}

.epi_summary_write <- function(products, output_dir, overwrite) {
  if (dir.exists(output_dir) &&
      length(fs::dir_ls(output_dir, all = TRUE, fail = FALSE)) > 0L &&
      !isTRUE(overwrite)) stop(
    "Summary output already exists; use overwrite = TRUE to replace it.", call. = FALSE)
  parent <- dirname(output_dir)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  stage <- fs::path(parent, paste0(fs::path_file(output_dir), ".tmp-",
    paste(sample(c(letters, 0:9), 12L, replace = TRUE), collapse = "")))
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  on.exit(if (dir.exists(stage)) fs::dir_delete(stage), add = TRUE)
  files <- c(
    dataset_overview = "dataset_overview.csv",
    field_inventory = "summary_field_inventory.csv",
    scalar_frequencies = "scalar_frequencies.csv",
    multiselect_frequencies = "multiselect_frequencies.csv",
    repeated_categorical_frequencies = "repeated_categorical_frequencies.csv",
    numeric_summaries = "numeric_summaries.csv",
    date_summaries = "date_summaries.csv",
    case_table_counts = "case_table_counts.csv",
    repeated_table_summaries = "repeated_table_summaries.csv",
    validation_rule_summary = "validation_rule_summary.csv",
    validation_status_summary = "validation_status_summary.csv",
    manifest = "summary_manifest.csv"
  )
  product_names <- c(dataset_overview = "overview", field_inventory = "field_inventory",
    scalar_frequencies = "scalar_frequencies", multiselect_frequencies = "multiselect_frequencies",
    repeated_categorical_frequencies = "repeated_categorical_frequencies",
    numeric_summaries = "numeric_summaries", date_summaries = "date_summaries",
    case_table_counts = "case_table_counts", repeated_table_summaries = "repeated_table_summaries",
    validation_rule_summary = "validation_rule_summary",
    validation_status_summary = "validation_status_summary", manifest = "manifest")
  for (name in names(files)) write_csv_utf8(products[[product_names[[name]]]],
    fs::path(stage, files[[name]]))
  if (dir.exists(output_dir)) fs::dir_delete(output_dir)
  if (!file.rename(stage, output_dir)) stop("Unable to finalize summary output directory.",
                                             call. = FALSE)
  on.exit(NULL, add = TRUE)
  invisible(output_dir)
}

#' Create metadata-driven descriptive summaries from de-identified Initial Epi data
#'
#' summarize_epi consumes only the de-identified analytical products created by
#' deidentify_epi. It uses the versioned semantic dictionary, codebooks, and the
#' source privacy-policy snapshot to create deterministic descriptive products.
#' It never reads a private re-identification crosswalk.
#'
#' @param deidentified_dir Successful deidentify_epi output directory.
#' @param version Versioned Initial Epi dictionary, currently 2024-05-28.
#' @param write Write products below deidentified_dir/summary.
#' @param overwrite Replace an existing summary directory.
#' @param strict Enforce passed privacy status, available validation, and no
#'   unsupported retained semantic variables.
#' @param quiet Suppress progress messages.
#' @return A named list of summary tibbles and output_dir.
#' @export
summarize_epi <- function(deidentified_dir, version = "2024-05-28", write = TRUE,
                          overwrite = FALSE, strict = TRUE, quiet = FALSE) {
  deidentified_dir <- validate_scalar_path(deidentified_dir, "deidentified_dir")
  input <- .epi_summary_validate_inputs(deidentified_dir, version)
  source_status <- as.character(input$manifest$status[[1L]])
  if (input$privacy_errors > 0) stop(
    "De-identified input has privacy_errors > 0 and cannot be summarized.", call. = FALSE)
  if (isTRUE(strict) && source_status != "passed") stop(
    "Strict summarization requires source de-identification status passed.", call. = FALSE)
  forms <- input$products$epi_forms
  responses <- input$products$epi_responses_long
  selected <- input$products$epi_multiselect_responses
  if (!all(c("case_id", "canonical_name", "raw_field") %in% names(responses))) stop(
    "epi_responses_long.csv is missing semantic response columns.", call. = FALSE)
  validation <- .epi_summary_validation(deidentified_dir, forms, strict)
  dictionary <- input$dictionary
  codes <- dictionary$codes
  inventory <- .epi_summary_build_inventory(dictionary, input$policy)
  unsupported <- sum(inventory$summary_status == "unsupported")
  if (isTRUE(strict) && unsupported > 0L) stop(
    "Retained semantic variables are unsupported: ", unsupported, call. = FALSE)
  tables <- stats::setNames(
    input$products[paste0("epi_", dictionary$tables$table_name)],
    as.character(dictionary$tables$table_name))
  repeated <- .epi_summary_repeated_tables(forms, tables, dictionary$tables)
  scalar <- .epi_summary_scalar(inventory, responses, nrow(forms), codes)
  multi <- .epi_summary_multiselect(inventory, responses, selected, nrow(forms))
  repeated_categorical <- .epi_summary_repeated_categorical(inventory, tables, codes)
  numeric <- .epi_summary_numeric(inventory, responses, tables, nrow(forms))
  dates <- .epi_summary_dates(inventory, responses, tables, nrow(forms))
  overview <- .epi_summary_overview(forms, responses, validation, input$manifest, repeated)
  summary_status <- if (source_status != "passed" || !validation$available || unsupported > 0L)
    "review" else "passed"
  package_version <- tryCatch(as.character(utils::packageVersion("bcapture")),
    error = function(e) "0.0.0.9000")
  summary_manifest <- tibble::tibble(
    summary_schema_version = 1L,
    form_version = as.character(input$manifest$form_version[[1L]]),
    profile = as.character(input$manifest$profile[[1L]]),
    source_policy_hash = if ("policy_hash" %in% names(input$manifest))
      as.character(input$manifest$policy_hash[[1L]]) else NA_character_,
    source_deidentification_status = source_status,
    validation_available = isTRUE(validation$available), n_cases = as.integer(nrow(forms)),
    n_summary_products = 12L,
    n_supported_fields = as.integer(sum(inventory$summary_status == "summarized")),
    n_privacy_excluded_fields = as.integer(sum(inventory$summary_status == "excluded_privacy")),
    n_unsupported_retained_fields = as.integer(unsupported), status = summary_status,
    package_version = package_version, created_at = utc_now()
  )
  output_dir <- fs::path(deidentified_dir, "summary")
  products <- list(
    overview = overview, field_inventory = inventory,
    scalar_frequencies = scalar, multiselect_frequencies = multi,
    repeated_categorical_frequencies = repeated_categorical, numeric_summaries = numeric,
    date_summaries = dates, case_table_counts = repeated$case_table_counts,
    repeated_table_summaries = repeated$repeated_table_summaries,
    validation_rule_summary = validation$rules,
    validation_status_summary = validation$statuses, manifest = summary_manifest
  )
  if (isTRUE(write)) .epi_summary_write(products, output_dir, overwrite)
  if (!isTRUE(quiet)) cli::cli_inform(
    "Summarized {nrow(forms)} de-identified Initial Epi case{?s}; summary status {summary_status}.")
  c(products, list(output_dir = if (isTRUE(write)) output_dir else NA_character_))
}
