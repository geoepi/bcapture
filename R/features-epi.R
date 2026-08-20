.epi_feature_registry_root <- function(version) {
  project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repeat {
    description <- fs::path(project_root, "DESCRIPTION")
    if (file.exists(description) &&
        any(grepl("^Package:\\s*bcapture\\s*$", readLines(description, warn = FALSE)))) break
    parent <- dirname(project_root)
    if (identical(parent, project_root)) {
      project_root <- NA_character_
      break
    }
    project_root <- parent
  }
  source_root <- if (is.na(project_root)) NA_character_ else fs::path(
    project_root, "inst", "extdata", "features", "initial_epi", version
  )
  root <- if (dir.exists(source_root)) source_root else system.file(
    "extdata", "features", "initial_epi", version, package = "bcapture"
  )
  if (!dir.exists(root)) stop(
    "Unsupported Initial Epi feature registry version directory: ", version,
    call. = FALSE
  )
  root
}

.epi_feature_read_csv <- function(path, label) {
  if (!file.exists(path)) stop("Initial Epi feature registry is missing ", label, ".",
                               call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"),
                  col_types = readr::cols(.default = readr::col_character()))
}

.epi_feature_registry_text <- function(x, order_column) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (order_column %in% names(x)) {
    order_value <- suppressWarnings(as.integer(x[[order_column]]))
    x <- x[order(order_value, na.last = TRUE), , drop = FALSE]
  }
  x[] <- lapply(x, function(value) {
    value <- as.character(value)
    value[is.na(value)] <- ""
    gsub("\\r\\n?", "\\n", value)
  })
  lines <- c(
    paste(names(x), collapse = "\t"),
    if (nrow(x) == 0L) character() else apply(x, 1L, paste, collapse = "\t")
  )
  paste(lines, collapse = "\n")
}

.epi_feature_registry_hash <- function(features, domains) {
  normalized <- paste(
    .epi_feature_registry_text(features, "feature_order"),
    .epi_feature_registry_text(domains, "domain_order"),
    sep = "\n---domains---\n"
  )
  digest::digest(normalized, algo = "sha256", serialize = FALSE)
}

.epi_feature_split <- function(x, separator = "\\|") {
  if (length(x) == 0L || is.na(x) || !nzchar(trimws(as.character(x)))) return(character())
  trimws(strsplit(as.character(x), separator)[[1L]])
}

.epi_feature_registry <- function(version = "2024-05-28", feature_version = "1") {
  root <- .epi_feature_registry_root(version)
  features <- .epi_feature_read_csv(fs::path(root, "features.csv"), "features.csv")
  domains <- .epi_feature_read_csv(fs::path(root, "domains.csv"), "domains.csv")
  manifest <- .epi_feature_read_csv(fs::path(root, "manifest.csv"), "manifest.csv")
  if (nrow(manifest) != 1L ||
      !all(c("feature_schema_version", "feature_version", "form_version",
             "feature_registry_hash") %in% names(manifest))) stop(
    "Feature registry manifest must contain one complete row.", call. = FALSE)
  if (!identical(as.character(manifest$feature_version[[1L]]), as.character(feature_version))) {
    stop("Unsupported Initial Epi feature version: ", feature_version, call. = FALSE)
  }
  if (!identical(as.character(manifest$form_version[[1L]]), as.character(version))) {
    stop("Feature registry form version does not match version.", call. = FALSE)
  }
  features$feature_order <- suppressWarnings(as.integer(features$feature_order))
  domains$domain_order <- suppressWarnings(as.integer(domains$domain_order))
  features <- features[order(features$feature_order), , drop = FALSE]
  domains <- domains[order(domains$domain_order), , drop = FALSE]
  hash <- .epi_feature_registry_hash(features, domains)
  list(features = tibble::as_tibble(features), domains = tibble::as_tibble(domains),
       manifest = manifest, hash = hash, root = root)
}

.epi_feature_policy_actions <- function(raw_fields, policy) {
  index <- match(as.character(raw_fields), as.character(policy$raw_field))
  as.character(policy$action[index])
}

.epi_feature_table_name <- function(source_table) {
  sub("^epi_", "", as.character(source_table))
}

.epi_feature_validate_registry <- function(registry, dictionary, policy) {
  features <- registry$features
  domains <- registry$domains
  required <- c(
    "feature_order", "feature_schema_version", "feature_version", "form_version",
    "feature_name", "feature_label", "domain_id", "feature_type", "source_type",
    "source_table", "source_variable", "source_question_id", "source_codebook_id",
    "derivation_type", "derivation", "true_values", "false_values",
    "missing_behavior", "unit", "description", "notes", "child_table",
    "child_filter", "child_record_fields"
  )
  problems <- character()
  missing_columns <- setdiff(required, names(features))
  if (length(missing_columns) > 0L) problems <- c(
    problems, paste("features.csv is missing columns", paste(missing_columns, collapse = ", ")))
  domain_required <- c("domain_id", "domain_label", "domain_order", "description")
  domain_missing <- setdiff(domain_required, names(domains))
  if (length(domain_missing) > 0L) problems <- c(
    problems, paste("domains.csv is missing columns", paste(domain_missing, collapse = ", ")))
  if (length(problems) > 0L) stop(
    "Initial Epi feature registry validation failed: ", paste(problems, collapse = "; "),
    call. = FALSE
  )
  if (anyDuplicated(features$feature_name)) problems <- c(problems, "feature names are not unique")
  if (anyDuplicated(features$feature_order) || any(is.na(features$feature_order))) {
    problems <- c(problems, "feature order must be unique and populated")
  }
  if (anyDuplicated(domains$domain_id) || anyDuplicated(domains$domain_order) ||
      any(is.na(domains$domain_order))) problems <- c(problems, "domain IDs and order must be unique")
  if (any(is.na(features$feature_name) |
          !grepl("^[a-z][a-z0-9_]*$", features$feature_name))) {
    problems <- c(problems, "feature names must use interpretable snake_case")
  }
  if (any(!features$feature_type %in% c("binary", "count", "numeric", "categorical"))) {
    problems <- c(problems, "unsupported feature type")
  }
  if (any(!features$derivation_type %in% c("direct", "coded", "count", "logical", "aggregate"))) {
    problems <- c(problems, "unsupported derivation type")
  }
  if (any(!features$source_type %in% c("scalar_response", "repeated_table"))) {
    problems <- c(problems, "unsupported source type")
  }
  if (any(!features$domain_id %in% domains$domain_id)) {
    problems <- c(problems, "feature references an undefined domain")
  }
  if (any(is.na(features$description) | !nzchar(features$description) |
          is.na(features$derivation) | !nzchar(features$derivation) |
          is.na(features$missing_behavior) | !nzchar(features$missing_behavior))) {
    problems <- c(problems, "every feature requires description derivation and missingness")
  }
  dictionary_questions <- unique(as.character(dictionary$fields$question_id))
  dictionary_codebooks <- unique(as.character(dictionary$codes$codebook_id))
  for (i in seq_len(nrow(features))) {
    feature <- features[i, , drop = FALSE]
    prefix <- paste0("feature ", feature$feature_name[[1L]], ": ")
    questions <- .epi_feature_split(feature$source_question_id[[1L]], ";")
    if (length(questions) == 0L || any(!questions %in% dictionary_questions)) {
      problems <- c(problems, paste0(prefix, "source question does not resolve"))
    }
    if (identical(feature$source_type[[1L]], "scalar_response")) {
      fields <- dictionary$fields[
        (is.na(dictionary$fields$table_name) | !nzchar(dictionary$fields$table_name)) &
          dictionary$fields$canonical_name == feature$source_variable[[1L]], , drop = FALSE]
      if (nrow(fields) != 1L) {
        problems <- c(problems, paste0(prefix, "scalar source variable does not resolve once"))
      } else {
        actions <- .epi_feature_policy_actions(fields$raw_field, policy)
        if (any(is.na(actions) | actions != "retain")) {
          problems <- c(problems, paste0(prefix, "source privacy action is not retain"))
        }
      }
    } else {
      table_name <- .epi_feature_table_name(feature$source_table[[1L]])
      if (!table_name %in% dictionary$tables$table_name) {
        problems <- c(problems, paste0(prefix, "repeated source table does not resolve"))
      }
      columns <- .epi_feature_split(feature$source_variable[[1L]])
      for (column in columns) {
        fields <- dictionary$fields[!is.na(dictionary$fields$table_name) &
                                      dictionary$fields$table_name == table_name &
                                      !is.na(dictionary$fields$column_name) &
                                      dictionary$fields$column_name == column, , drop = FALSE]
        if (nrow(fields) == 0L) {
          problems <- c(problems, paste0(prefix, "source column ", column, " does not resolve"))
        } else {
          actions <- .epi_feature_policy_actions(fields$raw_field, policy)
          if (any(is.na(actions) | actions != "retain")) {
          problems <- c(problems, paste0(prefix, "source column ", column,
                                          " includes a non-retained privacy action"))
          }
        }
      }
    }
    codebook <- feature$source_codebook_id[[1L]]
    if (!is.na(codebook) && nzchar(codebook) && !codebook %in% dictionary_codebooks) {
      problems <- c(problems, paste0(prefix, "source codebook does not resolve"))
    }
    if (identical(feature$feature_type[[1L]], "binary")) {
      true <- .epi_feature_split(feature$true_values[[1L]])
      false <- .epi_feature_split(feature$false_values[[1L]])
      if (length(true) == 0L || length(false) == 0L || length(intersect(true, false)) > 0L) {
        problems <- c(problems, paste0(prefix, "binary true/false mappings are incomplete"))
      }
      allowed <- as.character(dictionary$codes$raw_code[
        dictionary$codes$codebook_id == codebook])
      if (length(allowed) == 0L || any(!c(true, false) %in% allowed)) {
        problems <- c(problems, paste0(prefix, "binary mapping is not defined by its codebook"))
      }
    }
    child_table <- feature$child_table[[1L]]
    if (!is.na(child_table) && nzchar(child_table)) {
      table_name <- .epi_feature_table_name(child_table)
      if (!table_name %in% dictionary$tables$table_name) {
        problems <- c(problems, paste0(prefix, "child table does not resolve"))
      }
      child_columns <- .epi_feature_split(feature$child_record_fields[[1L]])
      if (length(child_columns) == 0L) {
        problems <- c(problems, paste0(prefix, "child record fields are missing"))
      }
      for (column in child_columns) {
        fields <- dictionary$fields[!is.na(dictionary$fields$table_name) &
                                      dictionary$fields$table_name == table_name &
                                      !is.na(dictionary$fields$column_name) &
                                      dictionary$fields$column_name == column, , drop = FALSE]
        actions <- .epi_feature_policy_actions(fields$raw_field, policy)
        if (nrow(fields) == 0L || any(is.na(actions) | actions != "retain")) {
          problems <- c(problems, paste0(prefix, "child field ", column,
                                          " is unresolved or not retained"))
        }
      }
    }
  }
  expected_hash <- as.character(registry$manifest$feature_registry_hash[[1L]])
  if (is.na(expected_hash) || !nzchar(expected_hash) || expected_hash == "PENDING" ||
      !identical(expected_hash, registry$hash)) {
    problems <- c(problems, "manifest feature_registry_hash does not match normalized registries")
  }
  if (length(problems) > 0L) stop(
    "Initial Epi feature registry validation failed: ",
    paste(unique(problems), collapse = "; "), call. = FALSE
  )
  invisible(TRUE)
}

.epi_feature_input <- function(deidentified_dir, version, strict) {
  input <- .epi_summary_validate_inputs(deidentified_dir, version)
  source_status <- as.character(input$manifest$status[[1L]])
  if (input$privacy_errors > 0) stop(
    "De-identified input has privacy_errors > 0 and cannot be used for feature derivation.",
    call. = FALSE)
  if (!source_status %in% c("passed", "review")) stop(
    "De-identification manifest status must be passed or review.", call. = FALSE)
  if (isTRUE(strict) && source_status != "passed") stop(
    "Strict feature derivation requires source de-identification status passed.",
    call. = FALSE)
  if (nrow(input$products$epi_forms) == 0L) stop(
    "No de-identified Initial Epi cases are available for feature derivation.",
    call. = FALSE)
  input$source_status <- source_status
  input
}

.epi_feature_validation <- function(deidentified_dir, cases, strict) {
  path <- fs::path(deidentified_dir, "validation", "validation_form_summary.csv")
  if (!file.exists(path)) {
    if (isTRUE(strict)) stop(
      "Validation products are required in strict mode; run validate_epi() before deriving features.",
      call. = FALSE)
    return(list(available = FALSE,
                case_status = stats::setNames(rep("not_available", length(cases)), cases),
                counts = c(valid = 0L, review = 0L, error = 0L)))
  }
  validation <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
  if (!all(c("case_id", "validation_status") %in% names(validation))) stop(
    "Validation form summary must contain case_id and validation_status.", call. = FALSE)
  if (anyDuplicated(as.character(validation$case_id)) ||
      !setequal(as.character(validation$case_id), cases)) stop(
    "Validation form summary must cover de-identified cases exactly once.", call. = FALSE)
  statuses <- as.character(validation$validation_status)
  if (any(is.na(statuses) | !statuses %in% c("valid", "review", "error"))) stop(
    "Validation status contains unsupported levels.", call. = FALSE)
  named <- stats::setNames(statuses, as.character(validation$case_id))[cases]
  counts <- c(valid = sum(named == "valid"), review = sum(named == "review"),
              error = sum(named == "error"))
  list(available = TRUE, case_status = named, counts = as.integer(counts))
}

.epi_feature_populated <- function(x) {
  if (inherits(x, "Date")) return(!is.na(x))
  if (is.character(x)) return(!is.na(x) & nzchar(trimws(x)))
  !is.na(x)
}

.epi_feature_truthy <- function(x) {
  !is.na(x) & (x %in% TRUE | as.character(x) %in% c("TRUE", "1"))
}

.epi_feature_empty_long <- function() {
  tibble::tibble(
    case_id = character(), domain_id = character(), domain_label = character(),
    feature_name = character(), feature_label = character(), feature_type = character(),
    value = character(), value_status = character(), logical_value = logical(),
    numeric_value = double(), character_value = character(),
    source_question_id = character(), source_table = character()
  )
}

.epi_feature_scalar <- function(feature, cases, responses, domains, dictionary, strict) {
  source <- responses[responses$canonical_name == feature$source_variable[[1L]], , drop = FALSE]
  if (nrow(source) > 0L && anyDuplicated(as.character(source$case_id))) stop(
    "Scalar feature source contains duplicate case responses for ",
    feature$feature_name[[1L]], ".", call. = FALSE)
  source <- source[match(cases, as.character(source$case_id)), , drop = FALSE]
  present <- !is.na(match(cases, as.character(responses$case_id[
    responses$canonical_name == feature$source_variable[[1L]]])))
  populated <- if (nrow(source) == length(cases))
    .epi_summary_response_populated(source) else rep(FALSE, length(cases))
  type <- feature$feature_type[[1L]]
  value <- rep(NA_character_, length(cases))
  status <- rep("missing", length(cases))
  logical_value <- rep(NA, length(cases))
  numeric_value <- rep(NA_real_, length(cases))
  character_value <- rep(NA_character_, length(cases))
  if (identical(type, "binary")) {
    codes <- if (nrow(source) == length(cases)) as.character(source$response_code) else
      rep(NA_character_, length(cases))
    true <- .epi_feature_split(feature$true_values[[1L]])
    false <- .epi_feature_split(feature$false_values[[1L]])
    logical_value[populated & codes %in% true] <- TRUE
    logical_value[populated & codes %in% false] <- FALSE
    known <- !is.na(logical_value)
    unsupported <- present & populated & !known
    if (isTRUE(strict) && any(unsupported)) stop(
      "Feature ", feature$feature_name[[1L]],
      " contains a populated response code that cannot be interpreted safely.", call. = FALSE)
    status[known] <- "known"
    status[unsupported] <- "unsupported"
    value[known] <- ifelse(logical_value[known], "TRUE", "FALSE")
  } else if (identical(type, "numeric")) {
    parsed <- if (nrow(source) == length(cases) && "numeric_value" %in% names(source))
      suppressWarnings(as.numeric(source$numeric_value)) else rep(NA_real_, length(cases))
    known <- populated & !is.na(parsed)
    numeric_value[known] <- parsed[known]
    value[known] <- format(parsed[known], scientific = FALSE, trim = TRUE)
    status[known] <- "known"
    status[present & populated & !known] <- "unparsed"
  } else if (identical(type, "categorical")) {
    codes <- if (nrow(source) == length(cases)) as.character(source$response_code) else
      rep(NA_character_, length(cases))
    labels <- if (nrow(source) == length(cases)) as.character(source$response_label) else
      rep(NA_character_, length(cases))
    codebook <- feature$source_codebook_id[[1L]]
    allowed <- as.character(dictionary$codes$raw_code[
      dictionary$codes$codebook_id == codebook])
    known_code <- populated & codes %in% allowed & !is.na(labels) & nzchar(trimws(labels))
    unsupported <- present & populated & !known_code
    if (isTRUE(strict) && any(unsupported)) stop(
      "Feature ", feature$feature_name[[1L]],
      " contains a populated response code that cannot be interpreted safely.", call. = FALSE)
    normalized <- tolower(trimws(labels))
    status[known_code] <- "known"
    status[known_code & normalized %in% c("don't know", "dont know", "unknown")] <- "unknown"
    status[known_code & normalized %in% c("not applicable", "n/a")] <- "not_applicable"
    status[unsupported] <- "unsupported"
    character_value[known_code] <- labels[known_code]
    value[known_code] <- labels[known_code]
  }
  domain_label <- domains$domain_label[match(feature$domain_id[[1L]], domains$domain_id)]
  tibble::tibble(
    case_id = cases, domain_id = feature$domain_id[[1L]],
    domain_label = as.character(domain_label), feature_name = feature$feature_name[[1L]],
    feature_label = feature$feature_label[[1L]], feature_type = type, value = value,
    value_status = status, logical_value = logical_value, numeric_value = numeric_value,
    character_value = character_value,
    source_question_id = feature$source_question_id[[1L]],
    source_table = feature$source_table[[1L]]
  )
}

.epi_feature_filter_table <- function(table, filter) {
  if (length(filter) == 0L || is.na(filter) || !nzchar(trimws(filter))) return(table)
  conditions <- .epi_feature_split(filter, ";")
  keep <- rep(TRUE, nrow(table))
  for (condition in conditions) {
    pair <- strsplit(condition, "=", fixed = TRUE)[[1L]]
    if (length(pair) != 2L || !pair[[1L]] %in% names(table)) stop(
      "Feature child filter is invalid: ", condition, call. = FALSE)
    keep <- keep & !is.na(table[[pair[[1L]]]]) &
      as.character(table[[pair[[1L]]]]) == pair[[2L]]
  }
  table[keep, , drop = FALSE]
}

.epi_feature_qualifying_rows <- function(table, fields, filter = NA_character_) {
  table <- .epi_feature_filter_table(table, filter)
  if (nrow(table) == 0L) return(table)
  missing <- setdiff(fields, names(table))
  if (length(missing) > 0L) stop(
    "Repeated feature source is missing columns: ", paste(missing, collapse = ", "),
    call. = FALSE)
  populated <- Reduce(`|`, lapply(fields, function(column) .epi_feature_populated(table[[column]])))
  table[populated, , drop = FALSE]
}

.epi_feature_count <- function(feature, cases, products, domains) {
  table <- products[[feature$source_table[[1L]]]]
  fields <- .epi_feature_split(feature$source_variable[[1L]])
  qualifying <- .epi_feature_qualifying_rows(table, fields)
  counts <- tabulate(match(as.character(qualifying$case_id), cases), nbins = length(cases))
  domain_label <- domains$domain_label[match(feature$domain_id[[1L]], domains$domain_id)]
  tibble::tibble(
    case_id = cases, domain_id = feature$domain_id[[1L]],
    domain_label = as.character(domain_label), feature_name = feature$feature_name[[1L]],
    feature_label = feature$feature_label[[1L]], feature_type = feature$feature_type[[1L]],
    value = as.character(as.integer(counts)), value_status = "known", logical_value = NA,
    numeric_value = as.numeric(counts), character_value = NA_character_,
    source_question_id = feature$source_question_id[[1L]],
    source_table = feature$source_table[[1L]]
  )
}

.epi_feature_empty_consistency <- function() {
  tibble::tibble(
    case_id = character(), feature_name = character(), severity = character(),
    finding_type = character(), parent_question = character(), child_table = character(),
    message = character()
  )
}

.epi_feature_consistency <- function(features, long, cases, products) {
  findings <- list()
  rules <- features[!is.na(features$child_table) & nzchar(features$child_table), , drop = FALSE]
  for (i in seq_len(nrow(rules))) {
    rule <- rules[i, , drop = FALSE]
    table <- products[[rule$child_table[[1L]]]]
    fields <- .epi_feature_split(rule$child_record_fields[[1L]])
    qualifying <- .epi_feature_qualifying_rows(table, fields, rule$child_filter[[1L]])
    counts <- tabulate(match(as.character(qualifying$case_id), cases), nbins = length(cases))
    parent <- long[long$feature_name == rule$feature_name[[1L]], , drop = FALSE]
    parent <- parent[match(cases, parent$case_id), , drop = FALSE]
    add <- function(index, severity, finding_type, message) {
      if (length(index) == 0L) return(invisible(NULL))
      findings[[length(findings) + 1L]] <<- tibble::tibble(
        case_id = cases[index], feature_name = rule$feature_name[[1L]],
        severity = severity, finding_type = finding_type,
        parent_question = rule$source_question_id[[1L]],
        child_table = rule$child_table[[1L]], message = message
      )
      invisible(NULL)
    }
    add(which(parent$value_status == "known" & parent$logical_value %in% FALSE & counts > 0L),
        "WARNING", "parent_no_child_rows",
        "The parent response is No but qualifying child records are present; both are preserved.")
    add(which(parent$value_status == "known" & parent$logical_value %in% TRUE & counts == 0L),
        "INFO", "parent_yes_no_child_rows",
        "The parent response is Yes but no qualifying retained-content child records are present.")
    add(which(parent$value_status == "missing" & counts > 0L),
        "INFO", "parent_missing_child_rows",
        "The parent response is missing but qualifying child records are present; both are preserved.")
    identity_columns <- intersect(c("row_index", "row_label", "direction", "material_type"),
                                  names(qualifying))
    if (nrow(qualifying) > 0L && length(identity_columns) > 0L) {
      keys <- do.call(paste, c(qualifying[c("case_id", identity_columns)], sep = "\r"))
      duplicate_cases <- unique(as.character(qualifying$case_id[duplicated(keys) |
        duplicated(keys, fromLast = TRUE)]))
      add(match(duplicate_cases, cases, nomatch = 0L)[match(duplicate_cases, cases, nomatch = 0L) > 0L],
          "WARNING", "duplicate_child_row_identity",
          "Qualifying child records share a stable row identity; records were counted without deduplication.")
    }
  }
  if (length(findings) == 0L) return(.epi_feature_empty_consistency())
  out <- dplyr::bind_rows(findings)
  out[order(out$case_id, out$feature_name, out$severity, out$finding_type), , drop = FALSE]
}

.epi_feature_wide <- function(cases, validation, features, long) {
  wide <- tibble::tibble(case_id = cases,
                         validation_status = as.character(validation$case_status[cases]))
  for (i in seq_len(nrow(features))) {
    feature <- features[i, , drop = FALSE]
    rows <- long[long$feature_name == feature$feature_name[[1L]], , drop = FALSE]
    rows <- rows[match(cases, rows$case_id), , drop = FALSE]
    if (feature$feature_type[[1L]] == "binary") {
      wide[[feature$feature_name[[1L]]]] <- as.logical(rows$logical_value)
    } else if (feature$feature_type[[1L]] == "count") {
      wide[[feature$feature_name[[1L]]]] <- as.integer(rows$numeric_value)
    } else if (feature$feature_type[[1L]] == "numeric") {
      wide[[feature$feature_name[[1L]]]] <- as.numeric(rows$numeric_value)
    } else {
      wide[[feature$feature_name[[1L]]]] <- as.character(rows$character_value)
    }
  }
  wide
}

.epi_feature_dictionary <- function(features, domains) {
  out <- dplyr::left_join(features, domains[c("domain_id", "domain_label", "domain_order")],
                          by = "domain_id")
  out[c(
    "feature_order", "feature_version", "form_version", "feature_name", "feature_label",
    "domain_id", "domain_label", "domain_order", "feature_type", "derivation_type", "unit",
    "description", "source_question_id", "source_table", "source_variable",
    "source_codebook_id", "derivation", "true_values", "false_values",
    "missing_behavior", "notes", "child_table", "child_filter"
  )]
}

.epi_feature_summary <- function(features, long) {
  rows <- vector("list", nrow(features))
  for (i in seq_len(nrow(features))) {
    feature <- features[i, , drop = FALSE]
    current <- long[long$feature_name == feature$feature_name[[1L]], , drop = FALSE]
    numeric <- current$numeric_value[current$value_status == "known" &
                                       !is.na(current$numeric_value)]
    n_known <- sum(current$value_status == "known")
    n_true <- sum(current$value_status == "known" & current$logical_value %in% TRUE)
    n_false <- sum(current$value_status == "known" & current$logical_value %in% FALSE)
    rows[[i]] <- tibble::tibble(
      feature_name = feature$feature_name[[1L]], domain_id = feature$domain_id[[1L]],
      feature_type = feature$feature_type[[1L]], n_cases = as.integer(nrow(current)),
      n_known = as.integer(n_known), n_true = as.integer(n_true), n_false = as.integer(n_false),
      n_missing = as.integer(sum(current$value_status == "missing")),
      n_unknown = as.integer(sum(current$value_status == "unknown")),
      n_not_applicable = as.integer(sum(current$value_status == "not_applicable")),
      n_other = as.integer(sum(!current$value_status %in%
        c("known", "missing", "unknown", "not_applicable"))),
      percent_true_of_known = if (n_true + n_false == 0L) NA_real_ else
        100 * n_true / (n_true + n_false),
      percent_true_of_all_cases = if (nrow(current) == 0L) NA_real_ else
        100 * n_true / nrow(current), n_nonmissing = as.integer(length(numeric)),
      mean = if (length(numeric) == 0L) NA_real_ else mean(numeric),
      sd = if (length(numeric) < 2L) NA_real_ else stats::sd(numeric),
      median = if (length(numeric) == 0L) NA_real_ else stats::median(numeric),
      q25 = if (length(numeric) == 0L) NA_real_ else
        as.numeric(stats::quantile(numeric, 0.25, names = FALSE)),
      q75 = if (length(numeric) == 0L) NA_real_ else
        as.numeric(stats::quantile(numeric, 0.75, names = FALSE)),
      min = if (length(numeric) == 0L) NA_real_ else min(numeric),
      max = if (length(numeric) == 0L) NA_real_ else max(numeric)
    )
  }
  dplyr::bind_rows(rows)
}

.epi_feature_derivation_audit <- function(features, long, consistency) {
  summary <- .epi_feature_summary(features, long)
  warning_counts <- table(factor(consistency$feature_name[consistency$severity == "WARNING"],
                                 levels = features$feature_name))
  error_counts <- table(factor(consistency$feature_name[consistency$severity == "ERROR"],
                               levels = features$feature_name))
  tibble::tibble(
    feature_name = summary$feature_name, n_cases = summary$n_cases,
    n_true = summary$n_true, n_false = summary$n_false, n_missing = summary$n_missing,
    n_unknown = summary$n_unknown, n_not_applicable = summary$n_not_applicable,
    n_other = summary$n_other, n_nonmissing = summary$n_nonmissing,
    min = summary$min, max = summary$max,
    n_consistency_warnings = as.integer(warning_counts),
    n_consistency_errors = as.integer(error_counts),
    status = ifelse(summary$n_other > 0L | as.integer(warning_counts) > 0L |
                      as.integer(error_counts) > 0L, "review", "passed")
  )
}

.epi_feature_domain_summary <- function(domains, features, long) {
  rows <- vector("list", nrow(domains))
  for (i in seq_len(nrow(domains))) {
    domain <- domains[i, , drop = FALSE]
    current <- long[long$domain_id == domain$domain_id[[1L]], , drop = FALSE]
    cases <- unique(current$case_id)
    known_cases <- unique(current$case_id[current$value_status == "known"])
    recorded_cases <- unique(current$case_id[!current$value_status %in%
      c("missing", "unparsed", "unsupported")])
    rows[[i]] <- tibble::tibble(
      domain_id = domain$domain_id[[1L]], domain_label = domain$domain_label[[1L]],
      domain_order = as.integer(domain$domain_order[[1L]]),
      n_features_defined = as.integer(sum(features$domain_id == domain$domain_id[[1L]])),
      n_cases = as.integer(length(cases)),
      n_case_feature_values_known = as.integer(sum(current$value_status == "known")),
      n_cases_with_any_known_feature = as.integer(length(known_cases)),
      n_cases_with_any_recorded_value = as.integer(length(recorded_cases))
    )
  }
  dplyr::bind_rows(rows)
}

.epi_feature_write <- function(products, output_dir, overwrite) {
  if (dir.exists(output_dir) && !isTRUE(overwrite)) stop(
    "Feature output already exists; use overwrite = TRUE to replace it.", call. = FALSE)
  parent <- dirname(output_dir)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(pattern = paste0(fs::path_file(output_dir), ".tmp-"), tmpdir = parent)
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  on.exit(if (dir.exists(stage)) fs::dir_delete(stage), add = TRUE)
  files <- c(
    features = "epi_features.csv", feature_dictionary = "feature_dictionary.csv",
    feature_long = "feature_long.csv", feature_summary = "feature_summary.csv",
    domain_summary = "feature_domain_summary.csv",
    consistency_findings = "feature_consistency_findings.csv",
    audit = "feature_derivation_audit.csv", manifest = "feature_manifest.csv"
  )
  for (name in names(files)) write_csv_utf8(products[[name]], fs::path(stage, files[[name]]))
  backup <- NA_character_
  if (dir.exists(output_dir)) {
    backup <- tempfile(pattern = paste0(fs::path_file(output_dir), ".backup-"), tmpdir = parent)
    if (!file.rename(output_dir, backup)) stop(
      "Unable to stage the existing feature output for replacement.", call. = FALSE)
  }
  finalized <- file.rename(stage, output_dir)
  if (!finalized) {
    if (!is.na(backup) && dir.exists(backup)) file.rename(backup, output_dir)
    stop("Unable to finalize feature output directory.", call. = FALSE)
  }
  if (!is.na(backup) && dir.exists(backup)) fs::dir_delete(backup)
  on.exit(NULL, add = TRUE)
  invisible(output_dir)
}

#' Derive auditable case-level Initial Epi analytical features
#'
#' `derive_epi_features()` consumes only the controlled-use de-identified
#' products created by [deidentify_epi()]. It applies a versioned registry of
#' transparent questionnaire-semantic derivations and never reads a private
#' crosswalk or identifiable extraction products. Analytical features describe
#' recorded observations; they are not risk scores and do not imply causation.
#'
#' @param deidentified_dir Successful `deidentify_epi()` output directory.
#' @param version Versioned Initial Epi semantic dictionary.
#' @param feature_version Versioned analytical feature registry.
#' @param write Write products below `deidentified_dir/features`.
#' @param overwrite Atomically replace only an existing `features` directory.
#' @param strict Require passed privacy status and available validation and stop
#'   when a populated coded response cannot be interpreted safely.
#' @param quiet Suppress progress messages.
#' @return A named list containing the wide and long feature data, dictionary,
#'   summaries, consistency findings, derivation audit, manifest, and output
#'   directory. No private crosswalk information is returned.
#' @export
derive_epi_features <- function(deidentified_dir, version = "2024-05-28",
                                feature_version = "1", write = TRUE,
                                overwrite = FALSE, strict = TRUE, quiet = FALSE) {
  deidentified_dir <- validate_scalar_path(deidentified_dir, "deidentified_dir")
  input <- .epi_feature_input(deidentified_dir, version, strict)
  registry <- .epi_feature_registry(version, feature_version)
  .epi_feature_validate_registry(registry, input$dictionary, input$policy)
  features <- registry$features
  domains <- registry$domains
  cases <- sort(as.character(input$products$epi_forms$case_id))
  validation <- .epi_feature_validation(deidentified_dir, cases, strict)
  responses <- input$products$epi_responses_long
  if (!all(c("case_id", "canonical_name", "response_code",
             "response_label", "numeric_value") %in% names(responses))) stop(
    "epi_responses_long.csv is missing required semantic response columns.", call. = FALSE)
  long_rows <- vector("list", nrow(features))
  for (i in seq_len(nrow(features))) {
    feature <- features[i, , drop = FALSE]
    if (feature$source_type[[1L]] == "scalar_response") {
      long_rows[[i]] <- .epi_feature_scalar(
        feature, cases, responses, domains, input$dictionary, strict
      )
    } else {
      long_rows[[i]] <- .epi_feature_count(feature, cases, input$products, domains)
    }
  }
  long <- if (length(long_rows) == 0L) .epi_feature_empty_long() else
    dplyr::bind_rows(long_rows)
  long$.feature_order <- features$feature_order[match(long$feature_name, features$feature_name)]
  long <- long[order(long$case_id, long$.feature_order), , drop = FALSE]
  long$.feature_order <- NULL
  consistency <- .epi_feature_consistency(features, long, cases, input$products)
  wide <- .epi_feature_wide(cases, validation, features, long)
  dictionary <- .epi_feature_dictionary(features, domains)
  summary <- .epi_feature_summary(features, long)
  domain_summary <- .epi_feature_domain_summary(domains, features, long)
  audit <- .epi_feature_derivation_audit(features, long, consistency)
  consistency_warnings <- sum(consistency$severity == "WARNING")
  consistency_errors <- sum(consistency$severity == "ERROR")
  unsupported <- sum(long$value_status %in% c("unsupported", "unparsed"))
  source_policy_hash <- if ("policy_hash" %in% names(input$manifest))
    as.character(input$manifest$policy_hash[[1L]]) else NA_character_
  package_version <- tryCatch(as.character(utils::packageVersion("bcapture")),
                              error = function(e) "0.0.0.9000")
  feature_status <- if (input$source_status != "passed" || !validation$available ||
                        consistency_warnings > 0L || consistency_errors > 0L ||
                        unsupported > 0L) "review" else "passed"
  manifest <- tibble::tibble(
    feature_schema_version = 1L, feature_version = as.character(feature_version),
    form_version = as.character(version),
    dictionary_version = as.character(input$dictionary$manifest$dictionary_version[[1L]]),
    dictionary_hash = as.character(input$dictionary$dictionary_hash),
    feature_registry_hash = registry$hash,
    profile = as.character(input$manifest$profile[[1L]]),
    source_policy_hash = source_policy_hash,
    source_deidentification_status = input$source_status,
    validation_available = isTRUE(validation$available),
    n_validation_valid = as.integer(validation$counts[[1L]]),
    n_validation_review = as.integer(validation$counts[[2L]]),
    n_validation_error = as.integer(validation$counts[[3L]]),
    n_cases = as.integer(length(cases)), n_features = as.integer(nrow(features)),
    n_domains = as.integer(nrow(domains)),
    n_consistency_findings = as.integer(nrow(consistency)),
    n_consistency_warnings = as.integer(consistency_warnings),
    n_consistency_errors = as.integer(consistency_errors),
    n_registry_errors = 0L, n_unsupported_derivations = as.integer(unsupported),
    status = feature_status, package_version = package_version, created_at = utc_now()
  )
  output_dir <- fs::path(deidentified_dir, "features")
  products <- list(
    features = wide, feature_long = long, feature_dictionary = dictionary,
    feature_summary = summary, domain_summary = domain_summary,
    consistency_findings = consistency, audit = audit, manifest = manifest
  )
  if (isTRUE(write)) .epi_feature_write(products, output_dir, overwrite)
  if (!isTRUE(quiet)) cli::cli_inform(
    "Derived {nrow(features)} Initial Epi feature{?s} for {length(cases)} de-identified case{?s}; feature status {feature_status}.")
  c(products, list(output_dir = if (isTRUE(write)) output_dir else NA_character_))
}
