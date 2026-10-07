# BCAP semantic validation and privacy boundary --------------------------------

utils::globalVariables(c(
  "observed_in_extraction", "observed_type", "privacy_class", "privacy_action",
  "identifier_class", "pseudonym_class", "form_type", "extraction_method",
  "template_family", "template_version", "record_id", "issue_type",
  "n_input_nonmissing", "n_output_rows"
))

.hpai_privacy_classes <- c(
  "SAFE_RETAIN", "SAFE_NORMALIZE", "PSEUDONYMIZE", "WITHHOLD",
  "CROSSWALK_ONLY", "REVIEW"
)

.hpai_allowed_failure_types <- "unrecognized_flattened_form"
.hpai_date_fields <- c("initial_audit_date", "audit_pass_date", "Producer_date")
.hpai_location_fields <- c(
  "premises_address", "prem_city", "prem_county", "prem_state", "prem_zip", "prem_gps"
)
.hpai_premises_fields <- c("npin", "premisesfarm_name", "prem_special_id")
.hpai_technical_fields <- c("FileNameField", "FileNameGen", "SaveAs")

.hpai_template_fields <- function() {
  path <- system.file("extdata", "templates", "bcap", "2025-12-08", "fields.csv", package = "bcapture")
  if (!nzchar(path) || !file.exists(path)) {
    candidates <- c(
      fs::path("inst", "extdata", "templates", "bcap", "2025-12-08", "fields.csv"),
      fs::path(".", "inst", "extdata", "templates", "bcap", "2025-12-08", "fields.csv")
    )
    available <- candidates[file.exists(candidates)]
    path <- if (length(available) > 0L) available[[1L]] else NA_character_
  }
  if (is.na(path) || !file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
}

.hpai_first_value <- function(x, default = NA_character_) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0L) default else x[[1L]]
}

.hpai_section <- function(field) {
  if (grepl("^Q[0-9]+_[0-9]+", field)) return(sub("^((Q[0-9]+_[0-9]+)).*", "\\1", field))
  "metadata"
}

.hpai_field_policy <- function(field, field_type, alternative_name = NA_character_) {
  field <- as.character(field)[[1L]]
  field_type <- as.character(field_type)[[1L]]
  alternative_name <- as.character(alternative_name)[[1L]]
  if (field %in% .hpai_technical_fields) {
    return(list(
      privacy_class = "WITHHOLD", identifier_class = "technical_metadata",
      pseudonym_class = NA_character_, free_text = FALSE, linkage_required = FALSE,
      notes = "Technical file-generation field; excluded from safe output."
    ))
  }
  if (identical(field_type, "Sig")) {
    return(list(
      privacy_class = "SAFE_RETAIN", identifier_class = "signature_presence",
      pseudonym_class = NA_character_, free_text = FALSE, linkage_required = FALSE,
      notes = "Structural signature-presence marker only; signature metadata is never retained."
    ))
  }
  if (field %in% .hpai_date_fields) {
    return(list(
      privacy_class = "SAFE_NORMALIZE", identifier_class = "quasi_identifier",
      pseudonym_class = NA_character_, free_text = FALSE, linkage_required = FALSE,
      notes = "Date is retained only after deterministic ISO-date normalization."
    ))
  }
  if (field %in% .hpai_location_fields) {
    return(list(
      privacy_class = "WITHHOLD", identifier_class = "location",
      pseudonym_class = NA_character_, free_text = FALSE, linkage_required = FALSE,
      notes = "Facility location is withheld because BCAP-specific release policy is not established."
    ))
  }
  if (field %in% .hpai_premises_fields) {
    return(list(
      privacy_class = "PSEUDONYMIZE", identifier_class = "premises_identifier",
      pseudonym_class = "PREMISES", free_text = FALSE, linkage_required = TRUE,
      notes = "Stable premises linkage is retained only through the private crosswalk."
    ))
  }
  if (grepl("(^|_)(phone|email)$", field, ignore.case = TRUE) ||
      grepl("(^|_)(contact|manager|auditor|producer)_name$", field, ignore.case = TRUE) ||
      field %in% c("umbrella_company", "auditor", "case_manager", "primary_contact_name", "Producer_name")) {
    pseudonym_class <- if (grepl("company|umbrella", field, ignore.case = TRUE)) "ORG" else
      if (grepl("phone|email", field, ignore.case = TRUE)) "CONTACT" else "PERSON"
    return(list(
      privacy_class = "PSEUDONYMIZE", identifier_class = "direct_identifier",
      pseudonym_class = pseudonym_class, free_text = FALSE, linkage_required = TRUE,
      notes = "Direct contact or person/organization value is replaced by a crosswalk-backed pseudonym."
    ))
  }
  if (identical(field_type, "Btn") || identical(field_type, "Ch")) {
    return(list(
      privacy_class = "SAFE_RETAIN", identifier_class = "audit_control",
      pseudonym_class = NA_character_, free_text = FALSE, linkage_required = FALSE,
      notes = "Documented audit control or categorical response is retained."
    ))
  }
  if (identical(field_type, "Tx")) {
    return(list(
      privacy_class = "WITHHOLD", identifier_class = "sensitive_free_text",
      pseudonym_class = NA_character_, free_text = TRUE, linkage_required = FALSE,
      notes = "Arbitrary text is withheld; no speculative NLP redaction is applied."
    ))
  }
  list(
    privacy_class = "REVIEW", identifier_class = "unknown_schema_type",
    pseudonym_class = NA_character_, free_text = FALSE, linkage_required = FALSE,
    notes = "Field type is not covered by the BCAP privacy policy."
  )
}

.hpai_schema_inventory <- function(fields) {
  observed <- fields |>
    dplyr::filter(!is.na(field), nzchar(field)) |>
    dplyr::group_by(field) |>
    dplyr::summarise(
      observed_in_extraction = TRUE,
      observed_type = {
        values <- unique(as.character(field_type[!is.na(field_type) & nzchar(field_type)]))
        if (length(values) == 0L) NA_character_ else paste(sort(values), collapse = "|")
      },
      alternative_name = .hpai_first_value(alternative_name),
      .groups = "drop"
    )
  canonical <- .hpai_template_fields() |>
    dplyr::transmute(
      field = as.character(field), observed_in_extraction = FALSE,
      observed_type = NA_character_, alternative_name = as.character(alternative_name)
    )
  combined <- dplyr::bind_rows(canonical, observed) |>
    dplyr::group_by(field) |>
    dplyr::summarise(
      observed_in_extraction = any(observed_in_extraction),
      observed_type = .hpai_first_value(observed_type),
      alternative_name = .hpai_first_value(alternative_name),
      .groups = "drop"
    ) |>
    dplyr::mutate(field_type = observed_type)
  canonical_type <- .hpai_template_fields() |>
    dplyr::select(field, canonical_type = field_type)
  combined <- dplyr::left_join(combined, canonical_type, by = "field") |>
    dplyr::mutate(field_type = ifelse(is.na(field_type) | !nzchar(field_type), canonical_type, field_type)) |>
    dplyr::select(-canonical_type)
  policy <- purrr::pmap_dfr(
    list(combined$field, combined$field_type, combined$alternative_name),
    function(field, field_type, alternative_name) {
      p <- .hpai_field_policy(field, field_type, alternative_name)
      tibble::tibble(
        field = field, section = .hpai_section(field), field_type = field_type,
        alternative_name = alternative_name, privacy_class = p$privacy_class,
        privacy_action = p$privacy_class, identifier_class = p$identifier_class,
        pseudonym_class = p$pseudonym_class, is_free_text = p$free_text,
        linkage_required = p$linkage_required, notes = p$notes
      )
    }
  )
  dplyr::left_join(policy, combined[, c("field", "observed_in_extraction")], by = "field") |>
    dplyr::arrange(observed_in_extraction, field)
}

.hpai_read_extraction <- function(out_dir) {
  out_dir <- validate_scalar_path(out_dir, "out_dir")
  required <- c(
    fs::path(out_dir, "combined", "hpai_fields_long.csv"),
    fs::path(out_dir, "extraction_manifest.csv")
  )
  if (!all(file.exists(required))) stop("BCAP extraction products are incomplete.", call. = FALSE)
  list(
    out_dir = out_dir,
    fields = readr::read_csv(required[[1L]], show_col_types = FALSE, na = c("", "NA")),
    manifest = readr::read_csv(required[[2L]], show_col_types = FALSE, na = c("", "NA")),
    metadata = if (file.exists(fs::path(out_dir, "combined", "hpai_metadata.csv")))
      readr::read_csv(fs::path(out_dir, "combined", "hpai_metadata.csv"), show_col_types = FALSE, na = c("", "NA")) else tibble::tibble()
  )
}

.hpai_empty_findings <- function() {
  tibble::tibble(
    record_key = character(), severity = character(), issue_type = character(),
    field = character(), field_type = character(), message = character()
  )
}

.hpai_date <- function(value) {
  value <- trimws(as.character(value))
  if (is.na(value) || !nzchar(value)) return(NA_character_)
  formats <- list(
    c("^\\d{4}-\\d{1,2}-\\d{1,2}$", "%Y-%m-%d"),
    c("^\\d{4}/\\d{1,2}/\\d{1,2}$", "%Y/%m/%d"),
    c("^\\d{1,2}/\\d{1,2}/\\d{4}$", "%m/%d/%Y"),
    c("^\\d{1,2}-\\d{1,2}-\\d{4}$", "%m-%d-%Y"),
    c("^\\d{1,2}/\\d{1,2}/\\d{2}$", "%m/%d/%y"),
    c("^\\d{1,2}-\\d{1,2}-\\d{2}$", "%m-%d-%y")
  )
  for (spec in formats) {
    if (!grepl(spec[[1L]], value)) next
    parsed <- suppressWarnings(as.Date(value, format = spec[[2L]]))
    if (!is.na(parsed)) return(format(parsed, "%Y-%m-%d"))
  }
  NA_character_
}

.hpai_allowed_values <- function(row) {
  source <- if (identical(as.character(row$field_type), "Ch")) row$options else row$states
  if (is.null(source) || is.na(source) || !nzchar(source)) return(character())
  normalize <- if (identical(as.character(row$field_type), "Ch")) normalize_option_set else normalize_state_set
  normalize(source)
}

.hpai_nonmissing <- function(value) !is.na(value) & nzchar(trimws(as.character(value)))

#' Validate extracted BCAP audit products
#'
#' validate_hpai applies only schema-supported checks. It permits the known
#' unrecognized_flattened_form extraction outcome while treating other
#' extraction failures, unknown field types, invalid categorical states, and
#' unparseable BCAP dates as blocking errors. It never prints source values.
#'
#' @param out_dir Existing output directory created by extract_hpai.
#' @param write Write sanitized validation products below out_dir.
#' @param strict Whether the returned status must have zero warnings as well as
#'   zero errors to be eligible for safe-output finalization.
#' @param quiet Suppress progress messages.
#' @return A list with status, findings, summary, and schema_inventory.
#' @export
validate_hpai <- function(out_dir, write = TRUE, strict = TRUE, quiet = FALSE) {
  products <- .hpai_read_extraction(out_dir)
  findings <- .hpai_empty_findings()
  add_finding <- function(record_key = NA_character_, severity, issue_type,
                          field = NA_character_, field_type = NA_character_, message) {
    findings <<- dplyr::bind_rows(findings, tibble::tibble(
      record_key = as.character(record_key), severity = as.character(severity),
      issue_type = as.character(issue_type), field = as.character(field),
      field_type = as.character(field_type), message = as.character(message)
    ))
  }
  required_manifest <- c("audit_id", "status", "failure_type")
  missing_manifest <- setdiff(required_manifest, names(products$manifest))
  if (length(missing_manifest) > 0L) {
    add_finding(severity = "ERROR", issue_type = "missing_required",
      message = paste0("Extraction manifest is missing required columns: ", paste(missing_manifest, collapse = ", ")))
  }
  required_fields <- c("audit_id", "field", "field_type", "value", "value_raw", "states", "options")
  missing_fields <- setdiff(required_fields, names(products$fields))
  if (length(missing_fields) > 0L) {
    add_finding(severity = "ERROR", issue_type = "missing_required",
      message = paste0("Field table is missing required columns: ", paste(missing_fields, collapse = ", ")))
  }
  if (nrow(findings) == 0L) {
    successes <- products$manifest[products$manifest$status == "success", , drop = FALSE]
    failures <- products$manifest[products$manifest$status != "success" | is.na(products$manifest$status), , drop = FALSE]
    if (nrow(successes) == 0L) {
      add_finding(severity = "ERROR", issue_type = "missing_required", message = "No successful BCAP audit records are available.")
    }
    if (nrow(failures) > 0L) {
      unknown <- failures[is.na(failures$failure_type) | !failures$failure_type %in% .hpai_allowed_failure_types, , drop = FALSE]
      if (nrow(unknown) > 0L) add_finding(
        record_key = paste(unknown$audit_id, collapse = ","), severity = "ERROR",
        issue_type = "unsupported_semantics", message = "An extraction failure outside the supported BCAP exclusion list was encountered."
      )
    }
    successful_ids <- as.character(successes$audit_id)
    fields <- products$fields[products$fields$audit_id %in% successful_ids, , drop = FALSE]
    inventory <- .hpai_schema_inventory(fields)
    policy <- inventory[inventory$observed_in_extraction, , drop = FALSE]
    for (field_name in unique(fields$field)) {
      field_rows <- fields[fields$field == field_name, , drop = FALSE]
      field_policy <- policy[policy$field == field_name, , drop = FALSE]
      if (nrow(field_policy) == 0L || is.na(field_policy$privacy_class[[1L]]) || field_policy$privacy_class[[1L]] == "REVIEW") {
        add_finding(field = field_name, severity = "ERROR", issue_type = "review_required",
          message = "Field is not covered by the BCAP privacy classification.")
      }
      if (nrow(field_policy) > 0L && field_policy$field_type[[1L]] == "mixed") {
        add_finding(field = field_name, severity = "ERROR", issue_type = "unsupported_semantics",
          message = "Field has inconsistent extracted control types across supported records.")
      }
      for (i in seq_len(nrow(field_rows))) {
        row <- field_rows[i, , drop = FALSE]
        value <- as.character(row$value[[1L]])
        if (!.hpai_nonmissing(value)) value <- as.character(row$value_raw[[1L]])
        if (!.hpai_nonmissing(value)) next
        type <- as.character(row$field_type[[1L]])
        if (type %in% c("Btn", "Ch")) {
          allowed <- .hpai_allowed_values(row)
          candidate <- unlist(strsplit(sub("^/", "", trimws(value)), "\\|", fixed = FALSE), use.names = FALSE)
          candidate <- trimws(sub("^/", "", candidate))
          if (length(allowed) > 0L && any(!candidate %in% allowed)) add_finding(
            record_key = row$audit_id[[1L]], severity = "ERROR", issue_type = "unexpected_value",
            field = field_name, field_type = type, message = "A categorical response is outside its extracted control domain."
          )
        }
        if (field_name %in% .hpai_date_fields && is.na(.hpai_date(value))) add_finding(
          record_key = row$audit_id[[1L]], severity = "ERROR", issue_type = "unparseable_date",
          field = field_name, field_type = type, message = "A populated BCAP date cannot be normalized deterministically."
        )
      }
    }
    counts <- fields |> dplyr::count(audit_id, name = "n_fields")
    missing_records <- setdiff(successful_ids, counts$audit_id)
    if (length(missing_records) > 0L) add_finding(
      record_key = paste(missing_records, collapse = ","), severity = "ERROR",
      issue_type = "missing_required", message = "A successful extraction manifest row has no field records."
    )
  } else {
    fields <- products$fields
    inventory <- .hpai_schema_inventory(fields)
  }
  summary <- tibble::tibble(
    records_evaluated = dplyr::n_distinct(products$manifest$audit_id[products$manifest$status == "success"]),
    warning_count = sum(findings$severity == "WARNING"),
    error_count = sum(findings$severity == "ERROR"),
    review_required_count = sum(findings$issue_type == "review_required"),
    status = ifelse(sum(findings$severity == "ERROR") == 0L && (!isTRUE(strict) || sum(findings$severity == "WARNING") == 0L), "passed", "failed")
  )
  result <- list(
    status = summary$status[[1L]], findings = findings, summary = summary,
    schema_inventory = inventory, out_dir = products$out_dir
  )
  class(result) <- c("bcapture_hpai_validation", "list")
  if (isTRUE(write)) {
    validation_dir <- fs::path(products$out_dir, "validation")
    dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)
    write_csv_utf8(findings, fs::path(validation_dir, "validation_findings.csv"))
    write_csv_utf8(summary, fs::path(validation_dir, "validation_summary.csv"))
    write_csv_utf8(inventory, fs::path(validation_dir, "schema_inventory.csv"))
  }
  if (!isTRUE(quiet)) cli::cli_inform("Validated {summary$records_evaluated} supported BCAP record{?s}; status {summary$status}.")
  result
}

.hpai_abs_path <- function(path, argument) normalizePath(validate_scalar_path(path, argument), winslash = "/", mustWork = FALSE)

.hpai_path_within <- function(child, parent) {
  child <- normalizePath(child, winslash = "/", mustWork = FALSE)
  parent <- normalizePath(parent, winslash = "/", mustWork = FALSE)
  identical(child, parent) || startsWith(paste0(child, "/"), paste0(parent, "/"))
}

.hpai_git_root <- function(path) {
  current <- normalizePath(path, winslash = "/", mustWork = FALSE)
  while (nzchar(current)) {
    if (file.exists(fs::path(current, ".git")) || dir.exists(fs::path(current, ".git"))) return(current)
    parent <- fs::path_dir(current)
    if (identical(parent, current)) break
    current <- parent
  }
  NA_character_
}

.hpai_validate_paths <- function(out_dir, deidentified_dir, crosswalk_dir, overwrite) {
  out_abs <- .hpai_abs_path(out_dir, "out_dir")
  deid_abs <- .hpai_abs_path(deidentified_dir, "deidentified_dir")
  cross_abs <- .hpai_abs_path(crosswalk_dir, "crosswalk_dir")
  if (length(unique(c(out_abs, deid_abs, cross_abs))) != 3L) stop("BCAP output and crosswalk directories must be distinct.", call. = FALSE)
  if (.hpai_path_within(deid_abs, out_abs) || .hpai_path_within(cross_abs, out_abs)) stop("BCAP safe output and crosswalk must not be nested inside confidential extraction output.", call. = FALSE)
  if (.hpai_path_within(deid_abs, cross_abs) || .hpai_path_within(cross_abs, deid_abs)) stop("BCAP safe output and crosswalk must be physically separate and non-nested.", call. = FALSE)
  git_root <- .hpai_git_root(cross_abs)
  if (!is.na(git_root)) stop("Refusing to create a BCAP private crosswalk inside a Git working tree.", call. = FALSE)
  if (dir.exists(deid_abs) && !isTRUE(overwrite)) stop("deidentified_dir already exists; set overwrite = TRUE to replace it after a successful gate.", call. = FALSE)
  if (dir.exists(cross_abs) && !isTRUE(overwrite)) stop("crosswalk_dir already exists; set overwrite = TRUE to reuse or extend it safely.", call. = FALSE)
  list(out_dir = out_abs, deidentified_dir = deid_abs, crosswalk_dir = cross_abs)
}

.hpai_empty_record_crosswalk <- function() tibble::tibble(
  source_key = character(), record_id = character(), source_file = character(), source_md5 = character(),
  premises_key = character(), premises_pseudonym = character(), created_at = character()
)

.hpai_empty_entity_crosswalk <- function() tibble::tibble(
  pseudonym = character(), entity_type = character(), original_value = character(),
  normalized_value = character(), first_record_id = character(), first_raw_field = character(), created_at = character()
)

.hpai_validate_crosswalk <- function(record, entity) {
  record_required <- names(.hpai_empty_record_crosswalk())
  entity_required <- names(.hpai_empty_entity_crosswalk())
  if (!all(record_required %in% names(record)) || !all(entity_required %in% names(entity))) stop("Existing BCAP crosswalk has an unsupported schema.", call. = FALSE)
  if (anyDuplicated(record$source_key) || anyDuplicated(record$record_id)) stop("Existing BCAP record crosswalk is not one-to-one.", call. = FALSE)
  if (anyDuplicated(paste(entity$entity_type, entity$normalized_value, sep = "|"))) stop("Existing BCAP entity crosswalk contains contradictory normalized mappings.", call. = FALSE)
  duplicated_pseudonyms <- duplicated(entity$pseudonym) | duplicated(entity$pseudonym, fromLast = TRUE)
  if (any(duplicated_pseudonyms & entity$entity_type != "PREMISES")) stop("Existing BCAP entity crosswalk contains contradictory pseudonym mappings.", call. = FALSE)
  if (any(nzchar(entity$pseudonym) & !grepl("^(PREMISES|PERSON|ORG|CONTACT|ENTITY)-[0-9]{6}$", entity$pseudonym))) stop("Existing BCAP entity crosswalk has invalid pseudonym formats.", call. = FALSE)
  invisible(TRUE)
}

.hpai_load_crosswalk <- function(path) {
  if (!dir.exists(path)) return(list(record = .hpai_empty_record_crosswalk(), entity = .hpai_empty_entity_crosswalk(), existing = FALSE))
  paths <- fs::path(path, c("record_crosswalk.csv", "entity_crosswalk.csv", "crosswalk_manifest.csv"))
  if (!all(file.exists(paths))) stop("Existing BCAP crosswalk is incomplete; refusing to repair it.", call. = FALSE)
  record <- readr::read_csv(paths[[1L]], show_col_types = FALSE, na = c("", "NA"))
  entity <- readr::read_csv(paths[[2L]], show_col_types = FALSE, na = c("", "NA"))
  manifest <- readr::read_csv(paths[[3L]], show_col_types = FALSE, na = c("", "NA"))
  if ("created_at" %in% names(record)) record$created_at <- as.character(record$created_at)
  if ("created_at" %in% names(entity)) entity$created_at <- as.character(entity$created_at)
  .hpai_validate_crosswalk(record, entity)
  if (!all(c("crosswalk_schema_version", "form_family") %in% names(manifest))) stop("Existing BCAP crosswalk manifest has an unsupported schema.", call. = FALSE)
  list(record = record, entity = entity, manifest = manifest, existing = TRUE)
}

.hpai_normalize_identity <- function(value) {
  value <- trimws(as.character(value))
  if (is.na(value) || !nzchar(value)) return(NA_character_)
  toupper(gsub("\\s+", " ", value, perl = TRUE))
}

.hpai_next_id <- function(prefix, values) {
  numbers <- suppressWarnings(as.integer(sub(paste0("^", prefix), "", values)))
  next_number <- if (all(is.na(numbers))) 1L else max(numbers, na.rm = TRUE) + 1L
  paste0(prefix, sprintf("%06d", next_number))
}

.hpai_add_entity <- function(state, value, entity_type, record_id, raw_field, pseudonym = NULL) {
  normalized <- .hpai_normalize_identity(value)
  if (is.na(normalized)) return(NA_character_)
  entity_type <- toupper(entity_type)
  existing <- state$entities[state$entities$entity_type == entity_type & state$entities$normalized_value == normalized, , drop = FALSE]
  if (nrow(existing) > 0L) {
    if (nrow(existing) > 1L) stop("BCAP entity crosswalk has contradictory normalized mappings.", call. = FALSE)
    if (!is.null(pseudonym) && !identical(as.character(existing$pseudonym[[1L]]), as.character(pseudonym))) stop("BCAP premises linkage is contradictory.", call. = FALSE)
    return(as.character(existing$pseudonym[[1L]]))
  }
  if (is.null(pseudonym)) pseudonym <- .hpai_next_id(paste0(entity_type, "-"), state$entities$pseudonym)
  state$entities <- dplyr::bind_rows(state$entities, tibble::tibble(
    pseudonym = as.character(pseudonym), entity_type = entity_type,
    original_value = as.character(value), normalized_value = normalized,
    first_record_id = as.character(record_id), first_raw_field = as.character(raw_field), created_at = utc_now()
  ))
  as.character(pseudonym)
}

.hpai_assign_records <- function(manifest, fields, crosswalk, state) {
  successes <- manifest[manifest$status == "success", , drop = FALSE]
  successes <- successes[order(as.character(successes$audit_id)), , drop = FALSE]
  old <- crosswalk$record
  records <- vector("list", nrow(successes))
  for (i in seq_len(nrow(successes))) {
    audit_id <- as.character(successes$audit_id[[i]])
    prior <- old[old$source_key == audit_id, , drop = FALSE]
    prior_ids <- if (length(records) == 0L) character() else vapply(records, function(x) if (is.null(x)) NA_character_ else x$record_id, character(1))
    record_id <- if (nrow(prior) > 0L) as.character(prior$record_id[[1L]]) else .hpai_next_id("BCAP-CASE-", c(old$record_id, prior_ids))
    rows <- fields[fields$audit_id == audit_id & fields$field %in% .hpai_premises_fields, , drop = FALSE]
    raw_values <- if (nrow(rows) == 0L) character() else vapply(seq_len(nrow(rows)), function(j) {
      value <- as.character(rows$value[[j]])
      if (!.hpai_nonmissing(value)) value <- as.character(rows$value_raw[[j]])
      value
    }, character(1))
    raw_values <- raw_values[vapply(raw_values, .hpai_nonmissing, logical(1))]
    premise_key <- if (length(raw_values) == 0L) NA_character_ else .hpai_normalize_identity(raw_values[[1L]])
    premise_id <- if (nrow(prior) > 0L && .hpai_nonmissing(prior$premises_pseudonym[[1L]])) as.character(prior$premises_pseudonym[[1L]]) else NA_character_
    if (length(raw_values) > 0L) {
      existing_ids <- unique(vapply(raw_values, function(value) {
        hit <- state$entities[state$entities$entity_type == "PREMISES" & state$entities$normalized_value == .hpai_normalize_identity(value), , drop = FALSE]
        if (nrow(hit) == 0L) NA_character_ else as.character(hit$pseudonym[[1L]])
      }, character(1)))
      existing_ids <- existing_ids[!is.na(existing_ids)]
      if (length(existing_ids) > 1L) stop("BCAP premises identifiers resolve to contradictory pseudonyms.", call. = FALSE)
      if (is.na(premise_id) && length(existing_ids) == 1L) premise_id <- existing_ids[[1L]]
      if (is.na(premise_id)) premise_id <- .hpai_next_id("PREMISES-", state$entities$pseudonym)
      for (value in raw_values) .hpai_add_entity(state, value, "PREMISES", record_id, "premises_identifier", premise_id)
    }
    records[[i]] <- list(
      source_key = audit_id, record_id = record_id, source_file = as.character(successes$source_file[[i]]),
      source_md5 = as.character(successes$source_md5[[i]]), premises_key = premise_key,
      premises_pseudonym = premise_id,
      created_at = if (nrow(prior) > 0L) as.character(prior$created_at[[1L]]) else utc_now()
    )
  }
  current <- if (length(records) == 0L) .hpai_empty_record_crosswalk() else dplyr::bind_rows(records)
  old_only <- if (nrow(old) == 0L) old else old[!old$source_key %in% current$source_key, , drop = FALSE]
  list(records = dplyr::bind_rows(old_only, current), current = current)
}

.hpai_raw_value <- function(row) {
  value <- as.character(row$value[[1L]])
  if (!.hpai_nonmissing(value)) value <- as.character(row$value_raw[[1L]])
  if (!.hpai_nonmissing(value)) NA_character_ else value
}

.hpai_pseudonym_for_row <- function(row, record, state) {
  raw <- .hpai_raw_value(row)
  if (is.na(raw)) return(NA_character_)
  pclass <- as.character(row$pseudonym_class[[1L]])
  if (identical(pclass, "PREMISES")) {
    id <- record$premises_pseudonym[[1L]]
    if (!.hpai_nonmissing(id)) stop("A populated premises identifier has no record-level premises pseudonym.", call. = FALSE)
    return(as.character(id))
  }
  .hpai_add_entity(state, raw, pclass, record$record_id[[1L]], as.character(row$field[[1L]]))
}

.hpai_privacy_scan <- function(safe_fields, entities = .hpai_empty_entity_crosswalk()) {
  findings <- tibble::tibble(
    severity = character(), table_name = character(), field = character(),
    identifier_class = character(), source_field_class = character(), leak_type = character()
  )
  add <- function(severity, field, identifier_class, source_field_class, leak_type) {
    findings <<- dplyr::bind_rows(findings, tibble::tibble(
      severity = severity, table_name = "hpai_fields_long", field = field,
      identifier_class = identifier_class, source_field_class = source_field_class,
      leak_type = leak_type
    ))
  }
  if (nrow(safe_fields) == 0L) return(findings)
  values <- as.character(safe_fields$value)
  for (i in which(!is.na(values) & nzchar(trimws(values)))) {
    value <- values[[i]]
    field <- as.character(safe_fields$field[[i]])
    action <- as.character(safe_fields$privacy_action[[i]])
    identifier_class <- as.character(safe_fields$identifier_class[[i]])
    if (identical(action, "WITHHOLD")) add("ERROR", field, identifier_class, "withheld", "withheld_value_present")
    if (identical(action, "REVIEW")) add("ERROR", field, identifier_class, "review", "review_field_present")
    if (identical(action, "PSEUDONYMIZE")) {
      expected <- if ("pseudonym_class" %in% names(safe_fields)) as.character(safe_fields$pseudonym_class[[i]]) else NA_character_
      prefix <- if (.hpai_nonmissing(expected)) expected else "PREMISES|PERSON|ORG|CONTACT|ENTITY"
      if (!grepl(paste0("^(", prefix, ")-[0-9]{6}$"), value)) add("ERROR", field, identifier_class, "pseudonymized", "pseudonymization_failure")
    }
    if (grepl("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}", value, ignore.case = TRUE, perl = TRUE)) add("ERROR", field, identifier_class, "retained", "email_pattern")
    if (grepl("(?<![0-9])(?:\\+?1[ .-]?)?(?:[2-9][0-9]{2}[ .-]?[0-9]{3}[ .-]?[0-9]{4})(?![0-9])", value, perl = TRUE)) add("ERROR", field, identifier_class, "retained", "phone_pattern")
    if (grepl("ByteRange|Contents|Adobe\\.PPKLite|adbe\\.pkcs7|signer|certificate", value, ignore.case = TRUE, perl = TRUE)) add("ERROR", field, identifier_class, "signature", "signature_metadata_leakage")
    if (identical(as.character(safe_fields$field_type[[i]]), "Sig") && !identical(value, "signature_present")) add("ERROR", field, identifier_class, "signature", "signature_marker_failure")
    if (identical(action, "PSEUDONYMIZE") && nrow(entities) > 0L) {
      known <- entities$original_value[!is.na(entities$original_value) & nzchar(entities$original_value)]
      if (length(known) > 0L && value %in% known) add("ERROR", field, identifier_class, "crosswalk", "known_source_value")
    }
  }
  prohibited <- intersect(names(safe_fields), c("audit_id", "source_file", "source_relpath", "source_md5", "value_raw", "original_value", "normalized_value"))
  if (length(prohibited) > 0L) for (name in prohibited) add("ERROR", name, "serialization", "schema", "serialization_leakage")
  dplyr::distinct(findings)
}

.hpai_write_crosswalk <- function(path, records, entities) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  manifest <- tibble::tibble(
    crosswalk_schema_version = "bcap-1", form_family = "bcap", created_at = utc_now(), updated_at = utc_now(),
    n_records = nrow(records), n_entities = nrow(entities)
  )
  write_csv_utf8(records, fs::path(path, "record_crosswalk.csv"))
  write_csv_utf8(entities, fs::path(path, "entity_crosswalk.csv"))
  write_csv_utf8(manifest, fs::path(path, "crosswalk_manifest.csv"))
  writeLines(c("# BCAP private crosswalk", "", "This directory contains confidential re-identification mappings.", "It must remain separate from safe output and outside version control."), fs::path(path, "CROSSWALK_README.md"), useBytes = TRUE)
  try(Sys.chmod(path, mode = "0700"), silent = TRUE)
  invisible(path)
}

#' De-identify validated BCAP audit products
#'
#' deidentify_hpai applies the versioned BCAP field policy to successful
#' extracted records. It writes a safe output tree and a physically separate
#' private crosswalk only after semantic validation and strict privacy scanning
#' both pass with zero errors and warnings. Unsupported raw PDFs are never
#' accepted by this function.
#'
#' @param out_dir Existing extract_hpai output directory.
#' @param deidentified_dir Safe output destination.
#' @param crosswalk_dir Private crosswalk destination outside the repository.
#' @param overwrite Replace existing destinations only after a successful gate.
#' @param strict Require zero privacy warnings as well as zero errors.
#' @param quiet Suppress progress messages.
#' @return A list with finalization status, sanitized manifests, and output paths.
#' @export
deidentify_hpai <- function(out_dir, deidentified_dir, crosswalk_dir, overwrite = FALSE, strict = TRUE, quiet = FALSE) {
  if (!isTRUE(strict)) stop("BCAP safe-output finalization requires strict = TRUE.", call. = FALSE)
  paths <- .hpai_validate_paths(out_dir, deidentified_dir, crosswalk_dir, overwrite)
  validation <- validate_hpai(paths$out_dir, write = FALSE, strict = TRUE, quiet = TRUE)
  if (!identical(validation$status, "passed")) stop("BCAP semantic validation did not pass; no safe output was finalized.", call. = FALSE)
  products <- .hpai_read_extraction(paths$out_dir)
  crosswalk <- .hpai_load_crosswalk(paths$crosswalk_dir)
  state <- new.env(parent = emptyenv())
  state$entities <- crosswalk$entity
  record_map <- .hpai_assign_records(products$manifest, products$fields, crosswalk, state)
  records <- record_map$current
  successful_ids <- records$source_key
  fields <- products$fields[products$fields$audit_id %in% successful_ids, , drop = FALSE]
  inventory <- .hpai_schema_inventory(fields)
  policy <- inventory[inventory$observed_in_extraction, , drop = FALSE]
  fields <- dplyr::left_join(fields, policy, by = c("field", "field_type"), suffix = c("", "_policy"))
  if (any(is.na(fields$privacy_action))) stop("BCAP privacy policy did not classify every extracted field.", call. = FALSE)
  record_lookup <- stats::setNames(seq_len(nrow(records)), records$source_key)
  output_values <- character(nrow(fields))
  for (i in seq_len(nrow(fields))) {
    row <- fields[i, , drop = FALSE]
    raw <- .hpai_raw_value(row)
    action <- as.character(row$privacy_action[[1L]])
    if (is.na(raw)) {
      output_values[[i]] <- NA_character_
    } else if (identical(action, "SAFE_RETAIN")) {
      output_values[[i]] <- if (identical(as.character(row$field_type[[1L]]), "Sig")) "signature_present" else trimws(raw)
    } else if (identical(action, "SAFE_NORMALIZE")) {
      output_values[[i]] <- .hpai_date(raw)
    } else if (identical(action, "PSEUDONYMIZE")) {
      record <- records[record_lookup[[as.character(row$audit_id[[1L]])]], , drop = FALSE]
      output_values[[i]] <- .hpai_pseudonym_for_row(row, record, state)
    } else if (identical(action, "WITHHOLD")) {
      output_values[[i]] <- NA_character_
    } else {
      stop("BCAP privacy policy contains a field that cannot be finalized safely.", call. = FALSE)
    }
  }
  safe_fields <- fields |>
    dplyr::transmute(
      record_id = records$record_id[match(audit_id, records$source_key)],
      field_index = as.integer(field_index), page = as.integer(page), field = as.character(field),
      field_type = as.character(field_type), states = as.character(states), options = as.character(options),
      is_populated = !is.na(output_values) & nzchar(output_values), value = output_values,
      privacy_class = as.character(privacy_class), privacy_action = as.character(privacy_action),
      identifier_class = as.character(identifier_class), pseudonym_class = as.character(pseudonym_class)
    )
  safe_metadata <- products$manifest[products$manifest$audit_id %in% successful_ids, , drop = FALSE] |>
    dplyr::transmute(
      record_id = records$record_id[match(audit_id, records$source_key)], form_type = as.character(form_type),
      schema_group = as.character(schema_group), extraction_method = as.character(extraction_method),
      template_family = as.character(template_family), template_version = as.character(template_version),
      number_of_fields = as.integer(number_of_fields), safe_populated_fields = vapply(record_id, function(id) sum(safe_fields$record_id == id & safe_fields$is_populated), integer(1))
    )
  privacy_audit <- .hpai_privacy_scan(safe_fields, state$entities)
  if (nrow(privacy_audit) > 0L && (any(privacy_audit$severity == "ERROR") || isTRUE(strict) && any(privacy_audit$severity == "WARNING"))) stop("Strict BCAP privacy validation failed; no safe output or crosswalk was finalized.", call. = FALSE)
  temp_safe <- tempfile("bcap-safe-", tmpdir = fs::path_dir(paths$deidentified_dir))
  temp_cross <- tempfile("bcap-crosswalk-", tmpdir = fs::path_dir(paths$crosswalk_dir))
  dir.create(temp_safe, recursive = TRUE, showWarnings = FALSE)
  dir.create(temp_cross, recursive = TRUE, showWarnings = FALSE)
  committed <- FALSE
  on.exit({
    if (!committed) {
      if (dir.exists(temp_safe)) unlink(temp_safe, recursive = TRUE, force = TRUE)
      if (dir.exists(temp_cross)) unlink(temp_cross, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  dir.create(fs::path(temp_safe, "combined"), recursive = TRUE, showWarnings = FALSE)
  dir.create(fs::path(temp_safe, "privacy"), recursive = TRUE, showWarnings = FALSE)
  dir.create(fs::path(temp_safe, "validation"), recursive = TRUE, showWarnings = FALSE)
  write_csv_utf8(safe_fields, fs::path(temp_safe, "combined", "hpai_fields_long.csv"))
  write_csv_utf8(safe_metadata, fs::path(temp_safe, "combined", "hpai_metadata.csv"))
  safe_validation <- validation$findings |>
    dplyr::transmute(record_id = NA_character_, severity, issue_type, field, field_type, message)
  write_csv_utf8(safe_validation, fs::path(temp_safe, "validation", "validation_findings.csv"))
  write_csv_utf8(validation$summary, fs::path(temp_safe, "validation", "validation_summary.csv"))
  write_csv_utf8(policy, fs::path(temp_safe, "privacy", "policy_snapshot.csv"))
  audit <- safe_fields |>
    dplyr::count(field, privacy_class, privacy_action, identifier_class, name = "n_output_rows") |>
    dplyr::left_join(fields |> dplyr::filter(.hpai_nonmissing(value)) |> dplyr::count(field, name = "n_input_nonmissing"), by = "field") |>
    dplyr::mutate(n_input_nonmissing = dplyr::coalesce(n_input_nonmissing, 0L), n_output_nonmissing = n_output_rows)
  write_csv_utf8(audit, fs::path(temp_safe, "privacy", "deidentification_audit.csv"))
  write_csv_utf8(privacy_audit, fs::path(temp_safe, "privacy", "privacy_leak_audit.csv"))
  manifest <- tibble::tibble(
    form_family = "bcap", records_processed = nrow(records), fields_retained = sum(policy$privacy_class == "SAFE_RETAIN"),
    fields_normalized = sum(policy$privacy_class == "SAFE_NORMALIZE"), fields_pseudonymized = sum(policy$privacy_class == "PSEUDONYMIZE"),
    fields_withheld = sum(policy$privacy_class == "WITHHOLD"), fields_crosswalk_only = sum(policy$privacy_class == "CROSSWALK_ONLY"),
    fields_review = sum(policy$privacy_class == "REVIEW"), privacy_errors = sum(privacy_audit$severity == "ERROR"),
    privacy_warnings = sum(privacy_audit$severity == "WARNING"), status = "passed", created_at = utc_now()
  )
  write_csv_utf8(manifest, fs::path(temp_safe, "privacy", "deidentification_manifest.csv"))
  .hpai_write_crosswalk(temp_cross, record_map$records, state$entities)
  if (dir.exists(paths$deidentified_dir)) unlink(paths$deidentified_dir, recursive = TRUE, force = TRUE)
  if (dir.exists(paths$crosswalk_dir)) unlink(paths$crosswalk_dir, recursive = TRUE, force = TRUE)
  if (!file.rename(temp_safe, paths$deidentified_dir)) stop("Unable to finalize BCAP safe output directory.", call. = FALSE)
  if (!file.rename(temp_cross, paths$crosswalk_dir)) {
    unlink(paths$deidentified_dir, recursive = TRUE, force = TRUE)
    stop("Unable to finalize BCAP crosswalk directory; safe output was removed.", call. = FALSE)
  }
  committed <- TRUE
  result <- list(status = "passed", manifest = manifest, validation = validation, privacy_audit = privacy_audit, deidentified_dir = paths$deidentified_dir, crosswalk_dir = paths$crosswalk_dir)
  class(result) <- c("bcapture_hpai_deidentification", "list")
  if (!isTRUE(quiet)) cli::cli_inform("Finalized safe BCAP output for {nrow(records)} record{?s}; strict privacy audit passed.")
  result
}

#' Validate finalized BCAP safe output
#'
#' Runs the strict field-aware privacy scanner against an existing BCAP safe
#' output tree. The optional private crosswalk enables known-source matching
#' without placing source values in diagnostics.
#'
#' @param deidentified_dir Existing deidentify_hpai safe output.
#' @param crosswalk_dir Optional private crosswalk directory.
#' @param strict Require zero warnings as well as zero errors.
#' @param write Write the sanitized privacy audit below deidentified_dir.
#' @param quiet Suppress progress messages.
#' @return A list with status, findings, and summary.
#' @export
validate_hpai_privacy <- function(deidentified_dir, crosswalk_dir = NULL, strict = TRUE, write = TRUE, quiet = FALSE) {
  deid_abs <- .hpai_abs_path(deidentified_dir, "deidentified_dir")
  field_path <- fs::path(deid_abs, "combined", "hpai_fields_long.csv")
  if (!file.exists(field_path)) stop("BCAP safe output is missing combined/hpai_fields_long.csv.", call. = FALSE)
  safe_fields <- readr::read_csv(field_path, show_col_types = FALSE, na = c("", "NA"))
  entities <- .hpai_empty_entity_crosswalk()
  if (!is.null(crosswalk_dir)) entities <- .hpai_load_crosswalk(.hpai_abs_path(crosswalk_dir, "crosswalk_dir"))$entity
  findings <- .hpai_privacy_scan(safe_fields, entities)
  summary <- tibble::tibble(
    records_evaluated = if ("record_id" %in% names(safe_fields)) dplyr::n_distinct(safe_fields$record_id) else 0L,
    error_count = sum(findings$severity == "ERROR"), warning_count = sum(findings$severity == "WARNING"),
    status = ifelse(sum(findings$severity == "ERROR") == 0L && (!isTRUE(strict) || sum(findings$severity == "WARNING") == 0L), "passed", "failed")
  )
  result <- list(status = summary$status[[1L]], findings = findings, summary = summary)
  class(result) <- c("bcapture_hpai_privacy_audit", "list")
  if (isTRUE(write)) {
    privacy_dir <- fs::path(deid_abs, "privacy")
    dir.create(privacy_dir, recursive = TRUE, showWarnings = FALSE)
    write_csv_utf8(findings, fs::path(privacy_dir, "privacy_leak_audit.csv"))
    write_csv_utf8(summary, fs::path(privacy_dir, "privacy_audit_summary.csv"))
  }
  if (!isTRUE(quiet)) cli::cli_inform("BCAP privacy audit status: {summary$status}.")
  result
}
