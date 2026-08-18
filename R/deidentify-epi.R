.epi_deid_allowed_metadata <- c(
  "form_version", "dictionary_version", "dictionary_hash", "form_schema_hash",
  "schema_group", "section_id", "section_name", "question_id", "subquestion_id",
  "raw_field", "canonical_name", "field_role", "response_type", "data_type",
  "units", "source_page", "row_index", "row_label", "raw_fields", "source_pages",
  "direction", "material_type", "method", "item_code", "item_label", "severity",
  "validation_type", "rule_id", "table_name", "message", "parse_type", "status",
  "n_findings", "n_forms", "n_info", "n_warning", "n_error", "validation_status"
)

.epi_deid_table_metadata <- c(
  "form_id", "case_id", "form_version", "dictionary_version", "dictionary_hash",
  "form_schema_hash", "schema_group", "row_index", "row_label", "raw_fields",
  "source_pages", "direction", "material_type", "method"
)

.epi_deid_missing_like <- function(value) {
  if (inherits(value, "Date")) return(as.Date(NA))
  if (is.integer(value)) return(NA_integer_)
  if (is.double(value)) return(NA_real_)
  if (is.logical(value)) return(NA)
  NA_character_
}

.epi_deid_normalize <- function(value) {
  value <- as.character(value)[[1L]]
  if (length(value) == 0L || is.na(value)) return(NA_character_)
  value <- gsub("[[:space:]]+", " ", trimws(value), perl = TRUE)
  if (!nzchar(value)) return(NA_character_)
  tolower(value)
}

.epi_deid_abs_path <- function(path, argument) {
  path <- validate_scalar_path(path, argument)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

.epi_deid_path_is_within <- function(path, parent) {
  path <- sub("/+$", "", path)
  parent <- sub("/+$", "", parent)
  identical(path, parent) || startsWith(path, paste0(parent, "/"))
}

.epi_deid_git_root <- function(path) {
  current <- path
  repeat {
    marker <- fs::path(current, ".git")
    if (file.exists(marker) || dir.exists(marker)) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) return(NA_character_)
    current <- parent
  }
}

.epi_deid_validate_paths <- function(out_dir, deidentified_dir, crosswalk_dir, overwrite) {
  out_abs <- .epi_deid_abs_path(out_dir, "out_dir")
  deid_abs <- .epi_deid_abs_path(deidentified_dir, "deidentified_dir")
  cross_abs <- .epi_deid_abs_path(crosswalk_dir, "crosswalk_dir")
  if (!dir.exists(out_abs)) stop("`out_dir` does not exist or is not a directory.", call. = FALSE)
  if (identical(deid_abs, out_abs) || identical(cross_abs, out_abs) || identical(cross_abs, deid_abs)) {
    stop("`out_dir`, `deidentified_dir`, and `crosswalk_dir` must be distinct directories.", call. = FALSE)
  }
  if (.epi_deid_path_is_within(cross_abs, deid_abs) || .epi_deid_path_is_within(cross_abs, out_abs) ||
      .epi_deid_path_is_within(deid_abs, cross_abs)) {
    stop("The crosswalk and de-identified output must be physically separate and not nested.", call. = FALSE)
  }
  git_root <- .epi_deid_git_root(cross_abs)
  if (!is.na(git_root)) {
    stop("Refusing to create a private crosswalk inside a Git working tree; store it outside version-controlled directories.", call. = FALSE)
  }
  if (dir.exists(deid_abs)) {
    entries <- fs::dir_ls(deid_abs, all = TRUE, fail = FALSE)
    if (length(entries) > 0L && !isTRUE(overwrite)) {
      stop("`deidentified_dir` already contains output; use `overwrite = TRUE` to replace it.", call. = FALSE)
    }
  }
  list(out_dir = out_abs, deidentified_dir = deid_abs, crosswalk_dir = cross_abs)
}

.epi_deid_read_csv <- function(path, label) {
  if (!file.exists(path)) stop("Required Initial Epi product is missing: ", label, call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
}

.epi_deid_load_rules <- function(version, profile, dictionary) {
  root <- .epi_dictionary_root(version)
  path <- fs::path(root, "deidentification_rules.csv")
  if (!file.exists(path)) stop("Initial Epi de-identification policy is missing.", call. = FALSE)
  rules <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
  required <- c("profile", "raw_field", "canonical_name", "table_name", "privacy_class",
                "action", "pseudonym_class", "coarsen_method", "notes")
  missing <- setdiff(required, names(rules))
  if (length(missing) > 0L) stop("De-identification policy is missing required columns.", call. = FALSE)
  if (!identical(profile, "analysis")) stop("Only `profile = \"analysis\"` is supported.", call. = FALSE)
  rules <- rules[rules$profile == profile, required, drop = FALSE]
  expected <- as.character(dictionary$fields$raw_field)
  if (nrow(rules) != length(expected) || length(unique(rules$raw_field)) != length(expected) ||
      !setequal(rules$raw_field, expected)) {
    stop("De-identification policy coverage must classify every supported logical field exactly once.", call. = FALSE)
  }
  allowed_classes <- c("direct_identifier", "quasi_identifier", "sensitive_free_text", "provenance_link", "non_identifier")
  allowed_actions <- c("retain", "drop", "pseudonymize", "coarsen", "review_remove")
  allowed_pseudonyms <- c("case", "premises", "person", "organization", "entity", "contact", "location")
  if (any(is.na(rules$privacy_class) | !rules$privacy_class %in% allowed_classes) ||
      any(is.na(rules$action) | !rules$action %in% allowed_actions) ||
      any(!is.na(rules$pseudonym_class) & !rules$pseudonym_class %in% allowed_pseudonyms)) {
    stop("De-identification policy contains unsupported privacy classifications or actions.", call. = FALSE)
  }
  if (any(rules$action == "pseudonymize" & (is.na(rules$pseudonym_class) | !nzchar(rules$pseudonym_class)))) {
    stop("Every pseudonymized field must declare a pseudonym class.", call. = FALSE)
  }
  rules
}

.epi_deid_rule <- function(rules, raw_field) {
  index <- match(as.character(raw_field), rules$raw_field)
  if (is.na(index)) return(NULL)
  rules[index, , drop = FALSE]
}

.epi_deid_rule_for_canonical <- function(rules, canonical_name) {
  index <- which(as.character(rules$canonical_name) == as.character(canonical_name))
  if (length(index) == 0L) return(NULL)
  rules[index[[1L]], , drop = FALSE]
}

.epi_deid_assert_columns <- function(data, allowed, label) {
  unknown <- setdiff(names(data), unique(allowed))
  if (length(unknown) > 0L) stop("Privacy metadata allowlist does not cover emitted columns in ", label, ".", call. = FALSE)
  invisible(TRUE)
}

.epi_deid_rule_for_table <- function(table_name, row, column, dictionary, rules) {
  fields_data <- dictionary$fields
  keep <- !is.na(fields_data$table_name) & fields_data$table_name == table_name &
    !is.na(fields_data$column_name) & as.character(fields_data$column_name) == column &
    !is.na(fields_data$row_index) & as.integer(fields_data$row_index) == as.integer(row$row_index)
  fields <- fields_data[keep, , drop = FALSE]
  if ("row_label" %in% names(row) && !is.na(row$row_label[[1L]]) && nzchar(as.character(row$row_label[[1L]]))) {
    labeled <- fields[as.character(fields$row_label) == as.character(row$row_label[[1L]]), , drop = FALSE]
    if (nrow(labeled) > 0L) fields <- labeled
  }
  if (table_name == "egg_movements" && "material_type" %in% names(row) && !is.na(row$material_type[[1L]])) {
    is_eggs <- grepl("^p01(82|83|84|85|92|93|94|95)", fields$raw_field)
    fields <- fields[if (identical(as.character(row$material_type[[1L]]), "eggs")) is_eggs else !is_eggs, , drop = FALSE]
  }
  if (nrow(fields) == 0L) {
    fields <- fields_data[!is.na(fields_data$table_name) & fields_data$table_name == table_name &
      !is.na(fields_data$column_name) & as.character(fields_data$column_name) == column, , drop = FALSE]
  }
  if (nrow(fields) == 0L && "raw_fields" %in% names(row) && !is.na(row$raw_fields[[1L]])) {
    candidates <- unlist(strsplit(as.character(row$raw_fields[[1L]]), "|", fixed = TRUE), use.names = FALSE)
    fields <- dictionary$fields[!is.na(dictionary$fields$column_name) & dictionary$fields$raw_field %in% candidates & dictionary$fields$column_name == column, , drop = FALSE]
  }
  if (nrow(fields) == 0L) return(NULL)
  .epi_deid_rule(rules, fields$raw_field[[1L]])
}

.epi_deid_case_prefix <- function(prefix, values) {
  values <- as.character(values)
  values <- values[grepl(paste0("^", prefix, "-[0-9]+$"), values)]
  if (length(values) == 0L) return(0L)
  max(as.integer(sub(paste0("^", prefix, "-"), "", values)))
}

.epi_deid_validate_record_crosswalk <- function(x) {
  required <- c("case_id", "source_key", "form_id", "source_sha256", "source_file",
                "premises_pseudonym", "premises_key", "premises_code", "original_premises_id",
                "original_premises_name", "created_at")
  if (!all(required %in% names(x))) stop("Existing record crosswalk has an unsupported schema.", call. = FALSE)
  if (anyDuplicated(x$source_key) || anyDuplicated(x$case_id)) stop("Existing record crosswalk contains duplicate case mappings.", call. = FALSE)
  premise_rows <- x[!is.na(x$premises_pseudonym) & nzchar(x$premises_pseudonym), , drop = FALSE]
  if (nrow(premise_rows) > 0L && any(vapply(split(premise_rows$premises_key, premise_rows$premises_pseudonym), function(keys) length(unique(keys)) > 1L, logical(1)))) {
    stop("Existing record crosswalk contains contradictory premises mappings.", call. = FALSE)
  }
  if (any(!grepl("^CASE-[0-9]+$", x$case_id)) || any(!is.na(x$premises_pseudonym) & !grepl("^PREMISES-[0-9]+$", x$premises_pseudonym))) {
    stop("Existing record crosswalk contains invalid pseudonym formats.", call. = FALSE)
  }
  invisible(TRUE)
}

.epi_deid_validate_entity_crosswalk <- function(x) {
  required <- c("pseudonym", "entity_type", "original_value", "normalized_value", "first_case_id", "first_raw_field", "first_table", "created_at")
  if (!all(required %in% names(x))) stop("Existing entity crosswalk has an unsupported schema.", call. = FALSE)
  if (anyDuplicated(x$pseudonym) || anyDuplicated(paste(x$entity_type, x$normalized_value, sep = "|"))) {
    stop("Existing entity crosswalk contains contradictory entity mappings.", call. = FALSE)
  }
  allowed <- c("PERSON", "ORG", "ENTITY", "CONTACT", "LOCATION")
  expected_prefix <- ifelse(x$entity_type == "ORG", "ORG", x$entity_type)
  bad_type <- is.na(x$entity_type) | !x$entity_type %in% allowed
  bad_format <- is.na(x$pseudonym) | !grepl("^(PERSON|ORG|ENTITY|CONTACT|LOCATION)-[0-9]+$", x$pseudonym)
  bad_prefix <- !bad_type & !bad_format & sub("-[0-9]+$", "", x$pseudonym) != expected_prefix
  if (any(bad_type) || any(bad_format) || any(bad_prefix)) {
    stop("Existing entity crosswalk contains invalid entity types or pseudonyms.", call. = FALSE)
  }
  invisible(TRUE)
}

.epi_deid_empty_record_crosswalk <- function() {
  tibble::tibble(case_id = character(), source_key = character(), form_id = character(), source_sha256 = character(),
    source_file = character(), premises_pseudonym = character(), premises_key = character(), premises_code = character(),
    original_premises_id = character(), original_premises_name = character(), created_at = character())
}

.epi_deid_empty_entity_crosswalk <- function() {
  tibble::tibble(pseudonym = character(), entity_type = character(), original_value = character(), normalized_value = character(),
    first_case_id = character(), first_raw_field = character(), first_table = character(), created_at = character())
}

.epi_deid_load_crosswalks <- function(crosswalk_dir) {
  record_path <- fs::path(crosswalk_dir, "record_crosswalk.csv")
  entity_path <- fs::path(crosswalk_dir, "entity_crosswalk.csv")
  manifest_path <- fs::path(crosswalk_dir, "crosswalk_manifest.csv")
  present <- file.exists(c(record_path, entity_path, manifest_path))
  if (any(present) && !all(present)) stop("Existing crosswalk is incomplete; refusing to repair or recreate it.", call. = FALSE)
  if (!any(present)) return(list(record = .epi_deid_empty_record_crosswalk(), entity = .epi_deid_empty_entity_crosswalk(), existing = FALSE))
  record <- readr::read_csv(record_path, show_col_types = FALSE, na = c("", "NA"))
  entity <- readr::read_csv(entity_path, show_col_types = FALSE, na = c("", "NA"))
  manifest <- readr::read_csv(manifest_path, show_col_types = FALSE, na = c("", "NA"))
  record$created_at <- as.character(record$created_at)
  entity$created_at <- as.character(entity$created_at)
  .epi_deid_validate_record_crosswalk(record)
  .epi_deid_validate_entity_crosswalk(entity)
  if (!all(c("crosswalk_schema_version", "form_version", "profile") %in% names(manifest))) stop("Existing crosswalk manifest has an unsupported schema.", call. = FALSE)
  list(record = record, entity = entity, existing = TRUE)
}

.epi_deid_next_id <- function(prefix, existing) {
  sprintf("%s-%06d", prefix, .epi_deid_case_prefix(prefix, existing) + 1L)
}

.epi_deid_add_sensitive <- function(state, value, scope = NULL) {
  value <- as.character(value)
  value <- value[!is.na(value) & nzchar(trimws(value))]
  if (length(value) == 0L) return(invisible(NULL))
  if (is.null(scope)) state$sensitive_values <- unique(c(state$sensitive_values, value)) else state$sensitive_context[[scope]] <- unique(c(state$sensitive_context[[scope]], value))
  invisible(NULL)
}

.epi_deid_event <- function(state, table_name, column_name, rule, value, output, case_id, raw_field) {
  if (length(value) == 0L || is.na(value[[1L]]) || !nzchar(trimws(as.character(value[[1L]])))) return(invisible(NULL))
  output_nonmissing <- !(length(output) == 0L || is.na(output[[1L]]) || !nzchar(trimws(as.character(output[[1L]]))))
  state$events[[length(state$events) + 1L]] <- tibble::tibble(
    table_name = table_name, column_name = column_name, raw_field = raw_field,
    canonical_name = as.character(rule$canonical_name[[1L]]), privacy_class = as.character(rule$privacy_class[[1L]]),
    action = as.character(rule$action[[1L]]), case_id = as.character(case_id),
    n_input_nonmissing = 1L, n_output_nonmissing = as.integer(output_nonmissing),
    n_values_pseudonymized = as.integer(rule$action[[1L]] == "pseudonymize"),
    n_values_removed = as.integer(rule$action[[1L]] %in% c("drop", "review_remove")),
    n_values_retained = as.integer(rule$action[[1L]] == "retain"),
    n_review_removed = as.integer(rule$action[[1L]] == "review_remove")
  )
  invisible(NULL)
}

.epi_deid_entity <- function(state, value, pseudonym_class, case_id, raw_field, table_name) {
  normalized <- .epi_deid_normalize(value)
  if (is.na(normalized)) return(NA_character_)
  entity_type <- toupper(if (identical(pseudonym_class, "organization")) "ORG" else pseudonym_class)
  key <- paste(entity_type, normalized, sep = "|")
  existing <- state$entities[state$entities$entity_type == entity_type & state$entities$normalized_value == normalized, , drop = FALSE]
  if (nrow(existing) > 1L) stop("Entity crosswalk contains contradictory mappings.", call. = FALSE)
  if (nrow(existing) == 1L) {
    if (length(unique(state$entities$original_value[state$entities$entity_type == entity_type & state$entities$normalized_value == normalized])) > 1L) {
      stop("Entity crosswalk contains contradictory normalized mappings.", call. = FALSE)
    }
    return(as.character(existing$pseudonym[[1L]]))
  }
  prefix <- entity_type
  pseudonym <- .epi_deid_next_id(prefix, state$entities$pseudonym)
  state$entities <- dplyr::bind_rows(state$entities, tibble::tibble(
    pseudonym = pseudonym, entity_type = entity_type, original_value = as.character(value), normalized_value = normalized,
    first_case_id = as.character(case_id), first_raw_field = as.character(raw_field), first_table = as.character(table_name), created_at = utc_now()
  ))
  state$entity_keys <- c(state$entity_keys, key)
  pseudonym
}

.epi_deid_transform <- function(state, value, rule, case_id, table_name, column_name, raw_field, record_event = TRUE) {
  if (is.null(rule)) stop("Privacy policy lookup failed for an emitted source-value column.", call. = FALSE)
  if (length(value) == 0L || is.na(value[[1L]]) || !nzchar(trimws(as.character(value[[1L]])))) return(.epi_deid_missing_like(value))
  action <- as.character(rule$action[[1L]])
  if (action == "retain") output <- value
  else if (action %in% c("drop", "review_remove")) output <- .epi_deid_missing_like(value)
  else if (action == "pseudonymize") {
    if (identical(as.character(rule$pseudonym_class[[1L]]), "premises")) {
      stop("Premises pseudonyms must be assigned from the record identity map.", call. = FALSE)
    }
    output <- .epi_deid_entity(state, value, as.character(rule$pseudonym_class[[1L]]), case_id, raw_field, table_name)
  } else stop("Privacy policy action is not implemented.", call. = FALSE)
  if (action != "retain") .epi_deid_add_sensitive(state, value, paste(table_name, column_name, sep = "\r"))
  if (isTRUE(record_event)) .epi_deid_event(state, table_name, column_name, rule, value, output, case_id, raw_field)
  output
}

.epi_deid_case_map <- function(forms, existing, state) {
  form_ids <- as.character(forms$form_id)
  source_sha <- if ("source_sha256" %in% names(forms)) as.character(forms$source_sha256) else rep(NA_character_, nrow(forms))
  source_file <- if ("source_file" %in% names(forms)) as.character(forms$source_file) else rep(NA_character_, nrow(forms))
  source_key <- ifelse(!is.na(source_sha) & nzchar(source_sha), paste0("sha256:", source_sha), paste0("form_id:", form_ids))
  if (anyDuplicated(source_key)) stop("Source records do not have unique stable identities.", call. = FALSE)
  old <- existing$record
  old_keys <- as.character(old$source_key)
  ids <- character(nrow(forms))
  next_id <- .epi_deid_case_prefix("CASE", old$case_id)
  for (i in seq_len(nrow(forms))) {
    hit <- match(source_key[[i]], old_keys)
    if (!is.na(hit)) ids[[i]] <- as.character(old$case_id[[hit]]) else {
      next_id <- next_id + 1L
      ids[[i]] <- sprintf("CASE-%06d", next_id)
    }
  }
  if (anyDuplicated(ids)) stop("Case pseudonym assignment is not unique.", call. = FALSE)
  premises_keys <- character(nrow(forms)); premises_ids <- character(nrow(forms))
  original_id <- if ("premises_id" %in% names(forms)) as.character(forms$premises_id) else rep(NA_character_, nrow(forms))
  original_name <- if ("premises_name" %in% names(forms)) as.character(forms$premises_name) else rep(NA_character_, nrow(forms))
  county <- if ("premises_county" %in% names(forms)) as.character(forms$premises_county) else rep(NA_character_, nrow(forms))
  old_premises <- as.character(old$premises_pseudonym)
  next_premises <- .epi_deid_case_prefix("PREMISES", old_premises)
  for (i in seq_len(nrow(forms))) {
    id_key <- .epi_deid_normalize(original_id[[i]])
    name_key <- .epi_deid_normalize(original_name[[i]])
    county_key <- .epi_deid_normalize(county[[i]])
    premises_keys[[i]] <- if (!is.na(id_key)) paste0("id:", id_key) else if (!is.na(name_key) && !is.na(county_key)) paste0("name_county:", name_key, "|", county_key) else paste0("case:", ids[[i]])
    hit <- match(source_key[[i]], old_keys)
    if (!is.na(hit) && !is.na(old_premises[[hit]]) && nzchar(old_premises[[hit]])) premises_ids[[i]] <- old_premises[[hit]] else {
      prior <- match(premises_keys[[i]], as.character(old$premises_key))
      if (!is.na(prior)) premises_ids[[i]] <- old$premises_pseudonym[[prior]] else {
        next_premises <- next_premises + 1L
        premises_ids[[i]] <- sprintf("PREMISES-%06d", next_premises)
      }
    }
  }
  premise_key_check <- split(premises_keys[premises_keys != ""], premises_ids[premises_keys != ""])
  if (any(vapply(premise_key_check, function(keys) length(unique(keys)) > 1L, logical(1)))) stop("Premises pseudonym assignment is contradictory.", call. = FALSE)
  now <- utc_now()
  new_records <- tibble::tibble(
    case_id = ids, source_key = source_key, form_id = form_ids, source_sha256 = source_sha,
    source_file = source_file, premises_pseudonym = premises_ids, premises_key = premises_keys,
    premises_code = original_id, original_premises_id = original_id, original_premises_name = original_name,
    created_at = now
  )
  old_only <- old[!old$source_key %in% new_records$source_key, , drop = FALSE]
  list(records = new_records, crosswalk_records = dplyr::bind_rows(old_only, new_records), case_ids = ids)
}

.epi_deid_case_join <- function(data, records) {
  if (!"form_id" %in% names(data)) stop("A collated product is missing `form_id`.", call. = FALSE)
  index <- match(as.character(data$form_id), records$form_id)
  if (anyNA(index)) stop("A collated product contains a form without a case mapping.", call. = FALSE)
  data$case_id <- as.character(records$case_id[index])
  data$form_id <- NULL
  data
}

.epi_deid_premises_value <- function(value, records, case_id) {
  if (length(value) == 0L || is.na(value[[1L]]) || !nzchar(trimws(as.character(value[[1L]])))) return(NA_character_)
  index <- match(case_id, records$case_id)
  if (is.na(index)) stop("Premises pseudonym assignment is missing a case mapping.", call. = FALSE)
  as.character(records$premises_pseudonym[[index]])
}

.epi_deid_transform_forms <- function(forms, records, dictionary, rules, state) {
  forms <- .epi_deid_case_join(forms, records)
  forms <- forms[, c("case_id", setdiff(names(forms), "case_id")), drop = FALSE]
  forbidden <- intersect(c("form_id", "source_file", "source_relpath", "source_path", "source_sha256"), names(forms))
  if (length(forbidden) > 0L) forms[forbidden] <- NULL
  scalar_names <- unique(as.character(dictionary$fields$canonical_name[is.na(dictionary$fields$table_name) | !nzchar(dictionary$fields$table_name)]))
  .epi_deid_assert_columns(forms, c("case_id", .epi_deid_allowed_metadata, scalar_names), "epi_forms")
  for (column in setdiff(names(forms), c("case_id", "form_version", "dictionary_version", "dictionary_hash", "form_schema_hash", "schema_group"))) {
    rule <- .epi_deid_rule_for_canonical(rules, column)
    if (is.null(rule)) {
      if (isTRUE(state$strict)) stop("Privacy policy is missing a rule for a scalar output column.", call. = FALSE)
      next
    }
    if (column %in% c("premises_id", "premises_name")) {
      for (i in seq_len(nrow(forms))) {
        .epi_deid_add_sensitive(state, forms[[column]][[i]], paste("epi_forms", column, sep = "\r"))
        forms[[column]][[i]] <- .epi_deid_premises_value(forms[[column]][[i]], records, forms$case_id[[i]])
      }
    } else {
      for (i in seq_len(nrow(forms))) forms[[column]][[i]] <- .epi_deid_transform(state, forms[[column]][[i]], rule, forms$case_id[[i]], "epi_forms", column, rule$raw_field[[1L]])
    }
  }
  forms
}

.epi_deid_transform_responses <- function(responses, records, rules, state) {
  responses <- .epi_deid_case_join(responses, records)
  .epi_deid_assert_columns(responses, c("case_id", .epi_deid_allowed_metadata, "raw_value", "value", "date_value", "numeric_value", "response_code", "response_label", "is_populated"), "epi_responses_long")
  if (!"date_value" %in% names(responses)) responses$date_value <- as.Date(rep(NA_character_, nrow(responses)))
  if (!"numeric_value" %in% names(responses)) responses$numeric_value <- rep(NA_real_, nrow(responses))
  for (i in seq_len(nrow(responses))) {
    rule <- .epi_deid_rule(rules, responses$raw_field[[i]])
    if (is.null(rule)) stop("Privacy policy is missing a semantic long-table field rule.", call. = FALSE)
    raw <- responses$raw_value[[i]]
    if (identical(as.character(rule$pseudonym_class[[1L]]), "premises")) {
      .epi_deid_add_sensitive(state, raw, paste("epi_responses_long", "raw_value", sep = "\r"))
      transformed <- .epi_deid_premises_value(raw, records, responses$case_id[[i]])
    } else transformed <- .epi_deid_transform(state, raw, rule, responses$case_id[[i]], "epi_responses_long", "raw_value", responses$raw_field[[i]])
    responses$raw_value[[i]] <- transformed
    responses$value[[i]] <- if (identical(as.character(rule$action[[1L]]), "retain")) responses$value[[i]] else transformed
    if (!identical(as.character(rule$action[[1L]]), "retain")) {
      responses$response_label[[i]] <- transformed
      .epi_deid_add_sensitive(state, raw, paste("epi_responses_long", "value", sep = "\r"))
      .epi_deid_add_sensitive(state, raw, paste("epi_responses_long", "response_label", sep = "\r"))
      responses$response_code[[i]] <- NA_character_
      responses$date_value[[i]] <- as.Date(NA)
      responses$numeric_value[[i]] <- NA_real_
    }
  }
  responses
}

.epi_deid_transform_multiselect <- function(x, records, rules) {
  if (nrow(x) == 0L && !"form_id" %in% names(x)) return(tibble::tibble(case_id = character(), raw_field = character(), canonical_name = character(), item_code = character(), item_label = character()))
  if (nrow(x) == 0L) return(.epi_deid_case_join(x, records))
  x <- .epi_deid_case_join(x, records)
  .epi_deid_assert_columns(x, c("case_id", "raw_field", "canonical_name", "item_code", "item_label"), "epi_multiselect_responses")
  for (raw_field in unique(as.character(x$raw_field))) {
    rule <- .epi_deid_rule(rules, raw_field)
    if (is.null(rule) || !identical(as.character(rule$action[[1L]]), "retain")) stop("Multiselect policy must explicitly retain or transform every emitted option.", call. = FALSE)
  }
  x
}

.epi_deid_transform_table <- function(x, table_name, records, dictionary, rules, state) {
  x <- .epi_deid_case_join(x, records)
  metadata <- intersect(.epi_deid_table_metadata, names(x))
  value_columns <- setdiff(names(x), metadata)
  for (column in value_columns) {
    base <- sub("(_raw|_code)$", "", column)
    for (i in seq_len(nrow(x))) {
      rule <- .epi_deid_rule_for_table(table_name, x[i, , drop = FALSE], base, dictionary, rules)
      if (is.null(rule)) stop("Privacy policy is missing a repeated-table field rule for table `", table_name, "`, column `", column, "`.", call. = FALSE)
      if (identical(as.character(rule$action[[1L]]), "retain")) next
      if (grepl("_code$", column)) x[[column]][[i]] <- .epi_deid_missing_like(x[[column]][[i]]) else {
        value <- x[[column]][[i]]
        transformed <- .epi_deid_transform(state, value, rule, x$case_id[[i]], paste0("epi_", table_name), column, rule$raw_field[[1L]], record_event = !grepl("_raw$", column))
        x[[column]][[i]] <- transformed
      }
    }
  }
  x
}

.epi_deid_transform_validation <- function(validation_dir, records, destination) {
  if (!dir.exists(validation_dir)) return(FALSE)
  files <- c("validation_results.csv", "validation_summary.csv", "validation_form_summary.csv")
  existing <- files[file.exists(fs::path(validation_dir, files))]
  if (length(existing) == 0L) return(FALSE)
  out <- fs::path(destination, "validation")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  for (name in existing) {
    x <- readr::read_csv(fs::path(validation_dir, name), show_col_types = FALSE, na = c("", "NA"))
    if ("form_id" %in% names(x)) {
      index <- match(as.character(x$form_id), records$form_id)
      if (anyNA(index)) stop("Validation output contains a form without a case mapping.", call. = FALSE)
      x$case_id <- records$case_id[index]
      x$form_id <- NULL
      x <- x[, c("case_id", setdiff(names(x), "case_id")), drop = FALSE]
    }
    .epi_deid_assert_columns(x, c("case_id", .epi_deid_allowed_metadata), paste0("validation/", name))
    readr::write_csv(x, fs::path(out, name), na = "")
  }
  findings <- if (file.exists(fs::path(out, "validation_results.csv"))) readr::read_csv(fs::path(out, "validation_results.csv"), show_col_types = FALSE) else tibble::tibble()
  report <- c("# Initial Epi validation report", "", "This de-identified report contains validation metadata only.", "", paste0("Findings: ", nrow(findings)))
  writeLines(report, fs::path(out, "validation_report.md"), useBytes = TRUE)
  TRUE
}

.epi_deid_scan <- function(outputs, sensitive_values, sensitive_context = list()) {
  sensitive_values <- unique(as.character(sensitive_values[!is.na(sensitive_values) & nzchar(trimws(sensitive_values))]))
  flags <- list()
  add_flag <- function(table_name, column_name, case_id, raw_field, canonical_name, leak_type, severity) {
    flags[[length(flags) + 1L]] <<- tibble::tibble(severity = severity, table_name = table_name, column_name = column_name,
      case_id = as.character(case_id), raw_field = as.character(raw_field), canonical_name = as.character(canonical_name), leak_type = leak_type)
  }
  for (table_name in names(outputs)) {
    x <- outputs[[table_name]]
    if (!is.data.frame(x)) next
    for (column in names(x)) {
      if (inherits(x[[column]], "Date") || (!is.character(x[[column]]) && !is.numeric(x[[column]]))) next
      is_character <- is.character(x[[column]])
      values <- as.character(x[[column]])
      for (i in which(!is.na(values) & nzchar(trimws(values)))) {
        cell <- values[[i]]; normalized_cell <- tolower(trimws(gsub("[[:space:]]+", " ", cell)))
        scoped_values <- unique(c(sensitive_values, sensitive_context[[paste(table_name, column, sep = "\r")]]))
        for (source in scoped_values) {
          normalized_source <- tolower(trimws(gsub("[[:space:]]+", " ", source)))
          numeric_source <- grepl("^[0-9+(). -]+$", normalized_source)
          hit <- if (numeric_source || nchar(normalized_source) < 8L) identical(normalized_cell, normalized_source) else identical(normalized_cell, normalized_source) || grepl(normalized_source, normalized_cell, fixed = TRUE)
          if (isTRUE(hit)) {
            add_flag(table_name, column, if ("case_id" %in% names(x)) x$case_id[[i]] else NA_character_, if ("raw_field" %in% names(x)) x$raw_field[[i]] else NA_character_, if ("canonical_name" %in% names(x)) x$canonical_name[[i]] else NA_character_, "known_source_value", "ERROR")
            break
          }
        }
        if (is_character && grepl("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}", cell, ignore.case = TRUE, perl = TRUE)) {
          add_flag(table_name, column, if ("case_id" %in% names(x)) x$case_id[[i]] else NA_character_, if ("raw_field" %in% names(x)) x$raw_field[[i]] else NA_character_, if ("canonical_name" %in% names(x)) x$canonical_name[[i]] else NA_character_, "email_pattern", "WARNING")
        }
        if (is_character && grepl("(?<![0-9])(?:\\+?1[ .-]?)?(?:[2-9][0-9]{2}[ .-]?[0-9]{3}[ .-]?[0-9]{4})(?![0-9])", cell, perl = TRUE)) {
          add_flag(table_name, column, if ("case_id" %in% names(x)) x$case_id[[i]] else NA_character_, if ("raw_field" %in% names(x)) x$raw_field[[i]] else NA_character_, if ("canonical_name" %in% names(x)) x$canonical_name[[i]] else NA_character_, "phone_pattern", "WARNING")
        }
      }
    }
  }
  if (length(flags) == 0L) tibble::tibble(severity = character(), table_name = character(), column_name = character(), case_id = character(), raw_field = character(), canonical_name = character(), leak_type = character()) else dplyr::bind_rows(flags) |> dplyr::distinct()
}

.epi_deid_audit <- function(rules, events, leak_flags) {
  event_data <- if (length(events) == 0L) tibble::tibble() else dplyr::bind_rows(events)
  group_columns <- c("table_name", "column_name", "raw_field", "canonical_name", "privacy_class", "action")
  count_columns <- c("n_input_nonmissing", "n_output_nonmissing", "n_values_pseudonymized", "n_values_removed", "n_values_retained", "n_review_removed")
  if (nrow(event_data) == 0L) {
    groups <- tibble::as_tibble(stats::setNames(lapply(group_columns, character), group_columns))
    for (column in count_columns) groups[[column]] <- integer()
  } else {
    keys <- do.call(paste, c(event_data[group_columns], sep = "\r"))
    split_rows <- split(seq_len(nrow(event_data)), keys)
    groups <- purrr::map_dfr(split_rows, function(index) {
      row <- event_data[index[[1L]], group_columns, drop = FALSE][1L, , drop = FALSE]
      for (column in count_columns) row[[column]] <- sum(event_data[[column]][index], na.rm = TRUE)
      row
    })
  }
  policy <- rules[, c("table_name", "raw_field", "canonical_name", "privacy_class", "action"), drop = FALSE]
  policy$table_name <- ifelse(is.na(policy$table_name) | !nzchar(policy$table_name), "epi_forms", policy$table_name)
  policy$column_name <- policy$canonical_name
  policy <- policy[, c("table_name", "column_name", "raw_field", "canonical_name", "privacy_class", "action"), drop = FALSE]
  audit <- dplyr::left_join(policy, groups, by = c("table_name", "column_name", "raw_field", "canonical_name", "privacy_class", "action"))
  for (column in count_columns) if (!column %in% names(audit)) audit[[column]] <- 0L
  audit[count_columns] <- lapply(audit[count_columns], function(x) as.integer(dplyr::coalesce(x, 0L)))
  leak_counts <- if (nrow(leak_flags) == 0L) tibble::tibble(table_name = character(), column_name = character(), n_leak_flags = integer()) else leak_flags |> dplyr::count(table_name, column_name, name = "n_leak_flags")
  audit <- dplyr::left_join(audit, leak_counts, by = c("table_name", "column_name"))
  audit$n_leak_flags <- as.integer(dplyr::coalesce(audit$n_leak_flags, 0L))
  audit
}

.epi_deid_write_crosswalk <- function(crosswalk_dir, records, entities, profile, version, policy_hash, existing) {
  dir.create(crosswalk_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- tibble::tibble(
    crosswalk_schema_version = "1", form_version = version, profile = profile, policy_hash = policy_hash,
    created_at = if (isTRUE(existing)) utc_now() else utc_now(), updated_at = utc_now(),
    n_cases = nrow(records), n_premises = length(unique(records$premises_pseudonym)),
    n_people = sum(entities$entity_type == "PERSON"), n_organizations = sum(entities$entity_type == "ORG"),
    n_entities = sum(entities$entity_type == "ENTITY"), n_contacts = sum(entities$entity_type == "CONTACT"),
    n_locations = sum(entities$entity_type == "LOCATION")
  )
  paths <- c(record_crosswalk = fs::path(crosswalk_dir, "record_crosswalk.csv"), entity_crosswalk = fs::path(crosswalk_dir, "entity_crosswalk.csv"), crosswalk_manifest = fs::path(crosswalk_dir, "crosswalk_manifest.csv"))
  values <- list(records, entities, manifest)
  for (i in seq_along(paths)) {
    tmp <- tempfile(pattern = paste0(fs::path_file(paths[[i]]), "-"), tmpdir = crosswalk_dir)
    readr::write_csv(values[[i]], tmp, na = "")
    if (!file.rename(tmp, paths[[i]])) {
      if (!file.copy(tmp, paths[[i]], overwrite = TRUE)) stop("Unable to safely write the private crosswalk.", call. = FALSE)
      unlink(tmp)
    }
  }
  try(Sys.chmod(crosswalk_dir, mode = "0700"), silent = TRUE)
  try(Sys.chmod(unname(paths), mode = "0600"), silent = TRUE)
  invisible(paths)
}

.epi_deid_write_readme <- function(crosswalk_dir) {
  dir.create(crosswalk_dir, recursive = TRUE, showWarnings = FALSE)
  path <- fs::path(crosswalk_dir, "CROSSWALK_README.md")
  if (!file.exists(path)) writeLines(c(
    "# Sensitive re-identification mappings", "", "This directory contains sensitive re-identification mappings.", "",
    "Do not distribute it with de-identified outputs.", "Do not commit it to version control.",
    "Store it in an appropriately access-controlled location.", "",
    "bcapture pseudonymized data are not irreversibly anonymous because these mappings permit re-identification.",
    "Secure storage remains the user's responsibility."
  ), path, useBytes = TRUE)
  invisible(path)
}

.epi_deid_temp_dir <- function(path) {
  parent <- dirname(path)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  tmp <- fs::path(parent, paste0(fs::path_file(path), ".tmp-", paste(sample(c(letters, 0:9), 12L, replace = TRUE), collapse = "")))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  tmp
}

#' Create controlled-use pseudonymized Initial Epi analysis data
#'
#' `deidentify_epi()` creates a second, privacy-protected analytical dataset
#' from collated Initial Epi products. It never modifies the identifiable
#' source directory. The result is pseudonymized/de-identified, not
#' irreversibly anonymous, because a separately stored crosswalk permits
#' authorized re-identification.
#'
#' @param out_dir Existing successful Initial Epi workflow directory.
#' @param deidentified_dir Destination for de-identified analytical products.
#' @param crosswalk_dir Required private destination for re-identification mappings.
#' @param version Versioned Initial Epi dictionary, currently `"2024-05-28"`.
#' @param profile Privacy profile; only `"analysis"` is supported.
#' @param overwrite Replace an existing de-identified output directory; never
#'   destroys or recreates an existing crosswalk.
#' @param strict In strict mode, confirmed leaks and potential heuristic leaks
#'   prevent finalization. With `FALSE`, potential warnings produce `review`,
#'   but confirmed leaks still fail.
#' @param quiet Suppress progress messages.
#' @return A safe summary list containing status, counts, manifest, and audit;
#'   private crosswalk contents are never returned.
#' @export
deidentify_epi <- function(out_dir, deidentified_dir, crosswalk_dir, version = "2024-05-28", profile = "analysis", overwrite = FALSE, strict = TRUE, quiet = FALSE) {
  paths <- .epi_deid_validate_paths(out_dir, deidentified_dir, crosswalk_dir, overwrite)
  collated <- fs::path(paths$out_dir, "collated")
  required <- c("epi_forms.csv", "epi_responses_long.csv", "epi_multiselect_responses.csv", "collation_manifest.csv", "collation_parse_diagnostics.csv", paste0(.epi_table_output_names, ".csv"))
  missing <- required[!file.exists(fs::path(collated, required))]
  if (length(missing) > 0L) stop("Successful collation products are missing; run collate_epi() first.", call. = FALSE)
  dictionary <- load_epi_dictionary(version)
  rules <- .epi_deid_load_rules(version, profile, dictionary)
  crosswalk <- .epi_deid_load_crosswalks(paths$crosswalk_dir)
  forms <- .epi_deid_read_csv(fs::path(collated, "epi_forms.csv"), "epi_forms.csv")
  if (!"form_id" %in% names(forms)) stop("Collated scalar product is missing `form_id`.", call. = FALSE)
  state <- new.env(parent = emptyenv()); state$strict <- isTRUE(strict); state$events <- list(); state$sensitive_values <- character(); state$sensitive_context <- list(); state$entities <- crosswalk$entity; state$entity_keys <- character()
  case_map <- .epi_deid_case_map(forms, crosswalk, state)
  records <- case_map$records
  .epi_deid_add_sensitive(state, records$source_sha256); .epi_deid_add_sensitive(state, records$source_file)
  forms_out <- .epi_deid_transform_forms(forms, records, dictionary, rules, state)
  responses_out <- .epi_deid_transform_responses(.epi_deid_read_csv(fs::path(collated, "epi_responses_long.csv"), "epi_responses_long.csv"), records, rules, state)
  multiselect_out <- .epi_deid_transform_multiselect(.epi_deid_read_csv(fs::path(collated, "epi_multiselect_responses.csv"), "epi_multiselect_responses.csv"), records, rules)
  table_outputs <- list()
  for (table_name in names(.epi_table_output_names)) table_outputs[[table_name]] <- .epi_deid_transform_table(.epi_deid_read_csv(fs::path(collated, paste0(.epi_table_output_names[[table_name]], ".csv")), paste0(.epi_table_output_names[[table_name]], ".csv")), table_name, records, dictionary, rules, state)
  output_tables <- c(list(epi_forms = forms_out, epi_responses_long = responses_out, epi_multiselect_responses = multiselect_out), stats::setNames(table_outputs, paste0("epi_", names(table_outputs))))
  # Write all products to a temporary sibling and finalize only after auditing.
  temp_dir <- .epi_deid_temp_dir(paths$deidentified_dir)
  on.exit(if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(fs::path(temp_dir, "collated"), recursive = TRUE, showWarnings = FALSE)
  for (name in names(output_tables)) readr::write_csv(output_tables[[name]], fs::path(temp_dir, "collated", paste0(name, ".csv")), na = "")
  validation_available <- .epi_deid_transform_validation(fs::path(paths$out_dir, "validation"), records, temp_dir)
  safe_outputs <- c(output_tables)
  if (validation_available) {
    validation_files <- fs::dir_ls(fs::path(temp_dir, "validation"), regexp = "\\.csv$", type = "file")
    for (path in validation_files) safe_outputs[[paste0("validation_", fs::path_ext_remove(fs::path_file(path)))]] <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
  }
  leak_flags <- .epi_deid_scan(safe_outputs, state$sensitive_values, state$sensitive_context)
  confirmed <- sum(leak_flags$severity == "ERROR")
  warnings <- sum(leak_flags$severity == "WARNING")
  if (confirmed > 0L) stop("Privacy audit detected one or more confirmed direct-identifier leaks; no output was finalized.", call. = FALSE)
  if (isTRUE(strict) && warnings > 0L) stop("Privacy audit detected potential identifier leakage in strict mode; no output was finalized.", call. = FALSE)
  audit <- .epi_deid_audit(rules, state$events, leak_flags)
  review <- if (nrow(audit) == 0L) tibble::tibble(case_id = character(), table_name = character(), raw_field = character(), canonical_name = character(), reason = character(), action = character(), review_status = character()) else {
    events <- if (length(state$events) == 0L) tibble::tibble() else dplyr::bind_rows(state$events)
    review_data <- unique(events[events[["action"]] == "review_remove", c("case_id", "table_name", "raw_field", "canonical_name"), drop = FALSE])
    review_data$reason <- "sensitive_free_text"
    review_data$action <- "review_remove"
    review_data$review_status <- "withheld"
    review_data[, c("case_id", "table_name", "raw_field", "canonical_name", "reason", "action", "review_status"), drop = FALSE]
  }
  manifest <- tibble::tibble(form_version = version, profile = profile, policy_version = version, policy_hash = digest::digest(rules, algo = "sha256", serialize = TRUE), package_version = "0.0.0.9000", forms_processed = nrow(records), cases_pseudonymized = length(unique(records$case_id)), premises_pseudonymized = length(unique(records$premises_pseudonym)), persons_pseudonymized = sum(state$entities$entity_type == "PERSON"), organizations_pseudonymized = sum(state$entities$entity_type == "ORG"), entities_pseudonymized = sum(state$entities$entity_type == "ENTITY"), contacts_pseudonymized = sum(state$entities$entity_type == "CONTACT"), locations_pseudonymized = sum(state$entities$entity_type == "LOCATION"), direct_identifier_fields_removed = sum(audit$privacy_class == "direct_identifier" & audit$n_values_removed > 0L), free_text_fields_withheld = sum(audit$privacy_class == "sensitive_free_text" & audit$n_review_removed > 0L), privacy_warnings = warnings, privacy_errors = confirmed, validation_available = validation_available, status = if (warnings > 0L) "review" else "passed", created_at = utc_now())
  privacy_dir <- fs::path(temp_dir, "privacy"); dir.create(privacy_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(rules, fs::path(privacy_dir, "policy_snapshot.csv"), na = "")
  readr::write_csv(audit, fs::path(privacy_dir, "deidentification_audit.csv"), na = "")
  readr::write_csv(review, fs::path(privacy_dir, "deidentification_review.csv"), na = "")
  readr::write_csv(manifest, fs::path(privacy_dir, "deidentification_manifest.csv"), na = "")
  readr::write_csv(leak_flags, fs::path(privacy_dir, "privacy_leak_audit.csv"), na = "")
  .epi_deid_write_readme(paths$crosswalk_dir)
  .epi_deid_write_crosswalk(paths$crosswalk_dir, case_map$crosswalk_records, state$entities, profile, version, manifest$policy_hash[[1L]], crosswalk$existing)
  if (dir.exists(paths$deidentified_dir) && isTRUE(overwrite)) unlink(paths$deidentified_dir, recursive = TRUE, force = TRUE)
  if (!file.rename(temp_dir, paths$deidentified_dir)) stop("Unable to finalize the de-identified output directory.", call. = FALSE)
  on.exit(NULL, add = TRUE)
  if (!isTRUE(quiet)) cli::cli_inform("De-identified {nrow(records)} Initial Epi form{?s}; privacy audit {manifest$status[[1L]}.")
  list(status = manifest$status[[1L]], forms_processed = nrow(records), deidentified_dir = paths$deidentified_dir, manifest = manifest, audit = audit)
}
