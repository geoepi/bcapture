# Phase 3 form-family analysis products --------------------------------------

.phase3_system_epi <- c(
  "record_id", "form_type", "form_version", "dictionary_version",
  "dictionary_hash", "form_schema_hash", "source_schema_version",
  "validation_status", "semantic_status", "privacy_status",
  "source_hash_reference", "collation_version", "run_id"
)

.phase3_system_bcap <- c(
  "record_id", "form_type", "form_version", "source_schema_version",
  "extraction_method", "template_family", "template_version",
  "validation_status", "privacy_status", "source_hash_reference",
  "collation_version", "run_id"
)

.phase3_source_counts <- function(manifest, safe_n) {
  if (is.null(manifest)) {
    return(c(discovered = safe_n, supported = safe_n, unsupported = 0L, extraction_failure = 0L))
  }
  status <- tolower(as.character(manifest$status))
  c(
    discovered = nrow(manifest), supported = sum(status == "success"),
    unsupported = sum(status != "success"), extraction_failure = sum(status != "success")
  )
}
.phase3_read_source_manifest <- function(path, label) {
  if (is.null(path)) return(NULL)
  manifest <- .phase3_read_csv(path, label)
  .phase3_required_columns(manifest, "status", label)
  manifest
}

.phase3_hash_reference <- function(crosswalk, record_ids, form) {
  result <- stats::setNames(rep(NA_character_, length(record_ids)), record_ids)
  if (is.null(crosswalk)) return(unname(result))
  if (identical(form, "epi")) {
    result <- stats::setNames(as.character(crosswalk$source_sha256), crosswalk$case_id)
  } else {
    result <- stats::setNames(as.character(crosswalk$source_md5), crosswalk$record_id)
  }
  unname(result[record_ids])
}

.phase3_validation_status <- function(directory, record_ids) {
  summary <- .phase3_read_csv(fs::path(directory, "validation", "validation_form_summary.csv"), "Epi validation form summary")
  .phase3_required_columns(summary, c("case_id", "validation_status"), "Epi validation form summary")
  status <- as.character(summary$validation_status[match(record_ids, summary$case_id)])
  if (any(is.na(status))) stop("Epi validation form summary does not cover every safe record.", call. = FALSE)
  status
}

.phase3_normalize_epi_repeated <- function(epi) {
  lookup <- stats::setNames(epi$forms$record_id, epi$forms$record_id)
  repeated <- .phase3_repeated_items(epi, lookup)
  repeated$entity_id <- NULL
  repeated$event_id <- NULL
  names(repeated)[names(repeated) == "section_identity"] <- "table_name"
  repeated$table_name <- sub("^epi_", "", as.character(repeated$table_name))
  repeated <- repeated[, c("record_id", "table_name", "row_index", "item_id", "field_name", "value_character", "value_numeric", "value_date", "value_logical", "value_code", "value_type"), drop = FALSE]
  repeated[order(repeated$record_id, repeated$table_name, repeated$item_id, repeated$field_name), , drop = FALSE]
}

.phase3_epi_records <- function(epi, normalized, status, crosswalk, run_id) {
  forms <- normalized$data
  forms$schema_group <- NULL
  forms$source_schema_version <- as.character(epi$forms$schema_group)
  forms$validation_status <- status
  forms$semantic_status <- ifelse(status == "valid", "passed", "accepted_warning")
  forms$privacy_status <- "passed"
  forms$source_hash_reference <- .phase3_hash_reference(crosswalk, forms$record_id, "epi")
  forms$form_type <- "initial_epi"
  forms$collation_version <- .phase3_version
  forms$run_id <- run_id
  scalar <- setdiff(names(forms), .phase3_system_epi)
  forms <- forms[, c(.phase3_system_epi, scalar), drop = FALSE]
  forms[order(forms$record_id), , drop = FALSE]
}

.phase3_bcap_wide_records <- function(bcap, crosswalk, run_id) {
  fields <- .phase3_normalize_bcap(bcap$fields)
  duplicate_fields <- duplicated(fields[c("record_id", "field")])
  if (any(duplicate_fields)) stop("BCAP safe fields contain duplicate record/field keys.", call. = FALSE)
  safe_fields <- fields[fields$privacy_class %in% c("SAFE_RETAIN", "SAFE_NORMALIZE", "PSEUDONYMIZE"), , drop = FALSE]
  values <- safe_fields |>
    dplyr::transmute(
      record_id = as.character(record_id), field = as.character(field),
      value = dplyr::case_when(
        !is.na(value_date) ~ format(value_date, "%Y-%m-%d"),
        !is.na(value_logical) ~ ifelse(value_logical, "TRUE", "FALSE"),
        TRUE ~ value_character
      )
    )
  wide <- tidyr::pivot_wider(values, id_cols = "record_id", names_from = "field", values_from = "value", values_fill = NA_character_)
  metadata <- bcap$metadata
  metadata$form_version <- .phase3_bcap_versions
  metadata$source_schema_version <- as.character(metadata$schema_group)
  metadata$entity_id <- NULL
  metadata$event_id <- NULL
  metadata$validation_status <- "passed"
  metadata$privacy_status <- "passed"
  metadata$source_hash_reference <- .phase3_hash_reference(crosswalk, metadata$record_id, "bcap")
  metadata$form_type <- "bcap"
  metadata$template_version <- .phase3_bcap_versions
  metadata$collation_version <- .phase3_version
  metadata$run_id <- run_id
  keep <- c("record_id", "form_type", "form_version", "source_schema_version", "extraction_method", "template_family", "template_version", "validation_status", "privacy_status", "source_hash_reference", "collation_version", "run_id")
  records <- dplyr::left_join(metadata[, keep, drop = FALSE], wide, by = "record_id")
  observed_fields <- unique(as.character(safe_fields$field))
  observed_fields <- observed_fields[!is.na(observed_fields) & nzchar(observed_fields)]
  template <- .hpai_template_fields()
  template <- dplyr::bind_rows(template, .phase3_bcap_supplemental_fields())
  for (field in intersect(observed_fields, names(records))) {
    field_meta <- template[template$field == field, , drop = FALSE]
    if (nrow(field_meta) != 1L) stop("BCAP safe field is not uniquely defined in the versioned template: ", field, call. = FALSE)
    if (field %in% .hpai_date_fields) {
      records[[field]] <- .phase3_parse_date(records[[field]], paste0("BCAP ", field))
    } else if (identical(as.character(field_meta$field_type[[1L]]), "Btn")) {
      records[[field]] <- .phase3_parse_logical_if_boolean(records[[field]])
    }
  }
  records <- records[, c(keep, observed_fields), drop = FALSE]
  records[order(records$record_id), , drop = FALSE]
}

.phase3_empty_repeated <- function() {
  tibble::tibble(
    record_id = character(), table_name = character(), row_index = integer(), item_id = character(),
    field_name = character(), value_character = character(), value_numeric = double(),
    value_date = as.Date(character()), value_logical = logical(), value_code = character(), value_type = character()
  )
}

.phase3_provenance_family <- function(records, form, crosswalk, run_id) {
  if (identical(form, "epi")) {
    result <- tibble::tibble(
      record_id = records$record_id, form_type = records$form_type, form_version = records$form_version,
      source_schema_version = records$source_schema_version, parser_version = .phase3_package_version(),
      extraction_method = "validated_safe_initial_epi", template_version = NA_character_,
      deidentification_policy_version = .phase3_epi_version, collation_version = .phase3_version,
      source_hash_reference = records$source_hash_reference, run_id = run_id
    )
  } else {
    result <- tibble::tibble(
      record_id = records$record_id, form_type = records$form_type, form_version = records$form_version,
      source_schema_version = records$source_schema_version, parser_version = .phase3_package_version(),
      extraction_method = records$extraction_method, template_version = records$template_version,
      deidentification_policy_version = "bcap-policy-1", collation_version = .phase3_version,
      source_hash_reference = records$source_hash_reference, run_id = run_id
    )
  }
  result[order(result$record_id), , drop = FALSE]
}

.phase3_metadata_coverage <- function(form, records, repeated, metadata) {
  system <- if (identical(form, "epi")) .phase3_system_epi else .phase3_system_bcap
  main_fields <- setdiff(names(records), system)
  main_meta <- unique(as.character(metadata$field_name[is.na(metadata$table_name)]))
  missing_main <- setdiff(main_fields, main_meta)
  missing_repeated <- character()
  if (nrow(repeated) > 0L) {
    observed <- unique(repeated[, c("table_name", "field_name"), drop = FALSE])
    known <- unique(metadata[!is.na(metadata$table_name), c("table_name", "field_name"), drop = FALSE])
    observed_key <- paste(observed$table_name, observed$field_name, sep = "\r")
    known_key <- paste(known$table_name, known$field_name, sep = "\r")
    missing_repeated <- observed_key[!observed_key %in% known_key]
  }
  list(missing_main = missing_main, missing_repeated = missing_repeated, complete = length(c(missing_main, missing_repeated)) == 0L)
}

.phase3_family_qa <- function(form, records, repeated, metadata, coverage, status, source_manifest = NULL, type_findings = NULL, extra_findings = NULL) {
  findings <- tibble::tibble(rule_id = character(), severity = character(), message = character(), count = integer())
  add <- function(rule_id, severity, message, count) {
    if (length(count) == 1L && !is.na(count) && count > 0L) findings <<- dplyr::bind_rows(findings, tibble::tibble(rule_id = rule_id, severity = severity, message = message, count = as.integer(count)))
  }
  if (!is.null(type_findings) && nrow(type_findings) > 0L) findings <- dplyr::bind_rows(findings, type_findings)
  if (!is.null(extra_findings) && nrow(extra_findings) > 0L) findings <- dplyr::bind_rows(findings, extra_findings)
  add("duplicate_record_id", "ERROR", "Analysis-ready record IDs must be unique.", sum(duplicated(records$record_id)))
  add("missing_record_id", "ERROR", "Analysis-ready record IDs must be populated.", sum(!.phase3_nonmissing(records$record_id)))
  add("orphan_repeated_item", "ERROR", "Every repeated item must reference a parent record.", sum(!repeated$record_id %in% records$record_id))
  repeated_identity_duplicates <- if (nrow(repeated) == 0L) 0L else sum(duplicated(repeated[c("item_id", "field_name")]))
  add("duplicate_repeated_item_id", "ERROR", "Repeated item/value identities must be unique within a form family.", repeated_identity_duplicates)
  add("metadata_main_coverage", "ERROR", "Every analysis-ready record column must map to field metadata.", length(coverage$missing_main))
  add("metadata_repeated_coverage", "ERROR", "Every observed repeated field must map to field metadata.", length(coverage$missing_repeated))
  add("semantic_validation_error", "ERROR", "Upstream semantic validation errors block finalization.", status$semantic_errors)
  add("privacy_validation_error", "ERROR", "Upstream privacy validation errors block finalization.", status$privacy_errors)
  add("privacy_validation_warning", "ERROR", "Privacy warnings remain blocking under the strict safe-output policy.", status$privacy_warnings)
  add("semantic_validation_warning", "WARNING", "Known semantic warnings are accepted and documented as non-blocking upstream findings.", status$semantic_warnings)
  source <- .phase3_source_counts(source_manifest, nrow(records))
  type_warning_count <- if (is.null(type_findings) || nrow(type_findings) == 0L) 0L else sum(type_findings$count)
  metrics <- tibble::tibble(
    form_type = form,
    metric = c("safe_records", "repeated_items", "metadata_rows", "metadata_main_columns_missing", "metadata_repeated_fields_missing", "semantic_warnings", "semantic_errors", "privacy_warnings", "privacy_errors", "type_normalization_warnings", "source_files_discovered", "source_files_supported", "source_files_unsupported", "duplicate_record_ids", "orphan_repeated_items"),
    value = as.integer(c(nrow(records), nrow(repeated), nrow(metadata), length(coverage$missing_main), length(coverage$missing_repeated), status$semantic_warnings, status$semantic_errors, status$privacy_warnings, status$privacy_errors, type_warning_count, source[["discovered"]], source[["supported"]], source[["unsupported"]], sum(duplicated(records$record_id)), sum(!repeated$record_id %in% records$record_id)))
  )
  qa_status <- if (any(findings$severity == "ERROR")) "failed" else if (any(findings$severity == "WARNING")) "passed_with_warnings" else "passed"
  list(findings = findings, metrics = metrics, status = qa_status, source_counts = source)
}

.phase3_family_manifest <- function(form, records, repeated, metadata, qa, run_id, hashes) {
  tibble::tibble(
    run_id = run_id, form_type = form, timestamp = utc_now(), package_version = .phase3_package_version(),
    git_sha = .phase3_git_sha(), collation_version = .phase3_version,
    form_version = paste(sort(unique(as.character(records$form_version))), collapse = ";"),
    safe_records = nrow(records), repeated_items = nrow(repeated), metadata_rows = nrow(metadata),
    source_files_discovered = qa$source_counts[["discovered"]], source_files_supported = qa$source_counts[["supported"]],
    source_files_unsupported = qa$source_counts[["unsupported"]], semantic_status = if (qa$metrics$value[qa$metrics$metric == "semantic_warnings"] > 0L) "passed_with_warnings" else "passed",
    privacy_status = if (qa$metrics$value[qa$metrics$metric == "privacy_errors"] + qa$metrics$value[qa$metrics$metric == "privacy_warnings"] == 0L) "passed" else "failed",
    qa_status = qa$status, accepted_warning_policy = if (qa$status == "passed_with_warnings") "documented_non_blocking_semantic_or_type_warnings" else "none",
    output_hashes = paste(paste(hashes$product, hashes$format, hashes$sha256, sep = ":"), collapse = ";")
  )
}

.phase3_write_family <- function(output_dir, form, tables, qa, manifest) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  products <- tables[c("records", "repeated_items", "provenance", "field_metadata")]
  .phase3_write_products(output_dir, products)
  write_csv_utf8(qa$metrics, fs::path(output_dir, "qa_summary.csv"))
  saveRDS(qa$metrics, fs::path(output_dir, "qa_summary.rds"), version = 3)
  write_csv_utf8(qa$findings, fs::path(output_dir, "qa_findings.csv"))
  saveRDS(qa$findings, fs::path(output_dir, "qa_findings.rds"), version = 3)
  hashes <- .phase3_output_hashes(output_dir, products)
  write_csv_utf8(hashes, fs::path(output_dir, "output_hashes.csv"))
  write_csv_utf8(manifest, fs::path(output_dir, "run_manifest.csv"))
  list(products = products, hashes = hashes, manifest = manifest)
}

.phase3_finalize_family <- function(output_dir, form, tables, qa, run_id, overwrite) {
  output_dir <- .phase3_safe_path(output_dir, paste0(form, "_output_dir"))
  if (dir.exists(output_dir) && !isTRUE(overwrite)) stop(form, " analysis output already exists; set overwrite = TRUE to replace it.", call. = FALSE)
  parent <- fs::path_dir(output_dir)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile(paste0("phase3-", form, "-"), tmpdir = parent)
  dir.create(temp, recursive = TRUE, showWarnings = FALSE)
  committed <- FALSE
  on.exit(if (!committed && dir.exists(temp)) unlink(temp, recursive = TRUE, force = TRUE), add = TRUE)
  products <- tables[c("records", "repeated_items", "provenance", "field_metadata")]
  .phase3_write_products(temp, products)
  write_csv_utf8(qa$metrics, fs::path(temp, "qa_summary.csv"))
  saveRDS(qa$metrics, fs::path(temp, "qa_summary.rds"), version = 3)
  write_csv_utf8(qa$findings, fs::path(temp, "qa_findings.csv"))
  saveRDS(qa$findings, fs::path(temp, "qa_findings.rds"), version = 3)
  hashes <- .phase3_output_hashes(temp, products)
  write_csv_utf8(hashes, fs::path(temp, "output_hashes.csv"))
  manifest <- .phase3_family_manifest(form, products$records, products$repeated_items, products$field_metadata, qa, run_id, hashes)
  write_csv_utf8(manifest, fs::path(temp, "run_manifest.csv"))
  backup <- NULL
  if (dir.exists(output_dir)) {
    backup <- tempfile(paste0("phase3-", form, "-backup-"), tmpdir = parent)
    if (!file.rename(output_dir, backup)) stop("Unable to stage existing ", form, " analysis output for replacement.", call. = FALSE)
  }
  if (!file.rename(temp, output_dir)) {
    if (!is.null(backup)) file.rename(backup, output_dir)
    stop("Unable to finalize ", form, " analysis output directory.", call. = FALSE)
  }
  if (!is.null(backup) && dir.exists(backup)) unlink(backup, recursive = TRUE, force = TRUE)
  committed <- TRUE
  list(status = qa$status, output_dir = output_dir, tables = products, qa = qa, manifest = manifest, output_hashes = hashes)
}

.phase3_validate_analysis_paths <- function(input_dir, output_dir, crosswalk_dir, form) {
  input_dir <- .phase3_safe_path(input_dir, paste0(form, "_dir"))
  output_dir <- .phase3_safe_path(output_dir, paste0(form, "_output_dir"))
  if (!dir.exists(input_dir)) stop(form, " safe output directory does not exist.", call. = FALSE)
  if (.phase3_path_within(output_dir, input_dir)) stop(form, " analysis output must be separate from its safe input.", call. = FALSE)
  if (!is.null(crosswalk_dir) && .phase3_path_within(output_dir, .phase3_safe_path(crosswalk_dir, paste0(form, "_crosswalk_dir")))) stop(form, " analysis output must be separate from its private crosswalk.", call. = FALSE)
  list(input_dir = input_dir, output_dir = output_dir)
}

#' Collate validated deidentified Initial Epi products into separate analysis data
#'
#' This Phase 3 API is intentionally distinct from the existing raw extraction
#' `collate_epi()` API. It writes only Epi-family records, repeated items,
#' provenance, and field metadata; BCAP is not required.
#'
#' @param epi_dir Existing deidentified Initial Epi output directory.
#' @param output_dir Destination for Epi analysis products.
#' @param epi_crosswalk_dir Optional private Epi crosswalk used only for safe
#'   source-hash references.
#' @param epi_source_manifest Optional sanitized extraction manifest for counts.
#' @param run_id Optional stable run identifier.
#' @param version Epi dictionary version.
#' @param overwrite Replace an existing family output after all gates pass.
#' @param quiet Suppress progress messages.
#' @return A form-family-specific Phase 3 result.
#' @export
collate_epi_analysis <- function(epi_dir, output_dir, epi_crosswalk_dir = NULL, epi_source_manifest = NULL, run_id = NULL, version = .phase3_epi_version, overwrite = FALSE, quiet = FALSE) {
  paths <- .phase3_validate_analysis_paths(epi_dir, output_dir, epi_crosswalk_dir, "epi")
  if (!identical(as.character(version), .phase3_epi_version)) stop("Unsupported Initial Epi analysis version: ", version, call. = FALSE)
  if (is.null(run_id)) run_id <- paste0("phase3-epi-", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  if (length(run_id) != 1L || is.na(run_id) || !grepl("^[A-Za-z0-9_.-]+$", run_id)) stop("`run_id` must be one safe path component.", call. = FALSE)
  status <- .phase3_input_status(paths$input_dir, "epi")
  epi <- .phase3_epi_read(paths$input_dir)
  crosswalk <- .phase3_crosswalk(epi_crosswalk_dir, "epi", epi$forms$record_id)
  normalized <- .phase3_normalize_epi_forms(epi$forms, epi$dictionary, epi$responses)
  validation_status <- .phase3_validation_status(paths$input_dir, epi$forms$record_id)
  records <- .phase3_epi_records(epi, normalized, validation_status, crosswalk, run_id)
  repeated <- .phase3_normalize_epi_repeated(epi)
  provenance <- .phase3_provenance_family(records, "epi", crosswalk, run_id)
  metadata <- field_metadata("epi", version)
  coverage <- .phase3_metadata_coverage("epi", records, repeated, metadata)
  source_manifest <- .phase3_read_source_manifest(epi_source_manifest, "Epi source manifest")
  qa <- .phase3_family_qa("epi", records, repeated, metadata, coverage, status, source_manifest, normalized$findings)
  if (identical(qa$status, "failed")) stop("Epi Phase 3 QA failed; no analysis-ready Epi products were finalized.", call. = FALSE)
  tables <- list(records = records, repeated_items = repeated, provenance = provenance, field_metadata = metadata)
  result <- .phase3_finalize_family(paths$output_dir, "epi", tables, qa, run_id, overwrite)
  if (!isTRUE(quiet)) cli::cli_inform("Finalized separate Epi analysis products for {nrow(records)} records; QA status {qa$status}.")
  result
}

#' Collate validated deidentified BCAP products into separate analysis data
#'
#' BCAP analysis collation is independent of Initial Epi and does not require
#' cross-form linkage. The current BCAP template has no repeated relational
#' structure, so `repeated_items` is emitted as an explicit empty table.
#'
#' @param bcap_dir Existing deidentified BCAP output directory.
#' @param output_dir Destination for BCAP analysis products.
#' @param bcap_crosswalk_dir Optional private BCAP crosswalk used only for safe
#'   source-hash references.
#' @param bcap_source_manifest Optional sanitized extraction manifest for counts.
#' @param run_id Optional stable run identifier.
#' @param version BCAP template version.
#' @param overwrite Replace an existing family output after all gates pass.
#' @param quiet Suppress progress messages.
#' @return A form-family-specific Phase 3 result.
#' @export
collate_bcap <- function(bcap_dir, output_dir, bcap_crosswalk_dir = NULL, bcap_source_manifest = NULL, run_id = NULL, version = .phase3_bcap_versions, overwrite = FALSE, quiet = FALSE) {
  paths <- .phase3_validate_analysis_paths(bcap_dir, output_dir, bcap_crosswalk_dir, "bcap")
  if (!identical(as.character(version), .phase3_bcap_versions)) stop("Unsupported BCAP analysis version: ", version, call. = FALSE)
  if (is.null(run_id)) run_id <- paste0("phase3-bcap-", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  if (length(run_id) != 1L || is.na(run_id) || !grepl("^[A-Za-z0-9_.-]+$", run_id)) stop("`run_id` must be one safe path component.", call. = FALSE)
  status <- .phase3_input_status(paths$input_dir, "bcap")
  bcap <- .phase3_bcap_read(paths$input_dir)
  crosswalk <- .phase3_crosswalk(bcap_crosswalk_dir, "bcap", bcap$metadata$record_id)
  records <- .phase3_bcap_wide_records(bcap, crosswalk, run_id)
  repeated <- .phase3_empty_repeated()
  provenance <- .phase3_provenance_family(records, "bcap", crosswalk, run_id)
  metadata <- field_metadata("bcap", version)
  coverage <- .phase3_metadata_coverage("bcap", records, repeated, metadata)
  source_manifest <- .phase3_read_source_manifest(bcap_source_manifest, "BCAP source manifest")
  qa <- .phase3_family_qa("bcap", records, repeated, metadata, coverage, status, source_manifest)
  if (identical(qa$status, "failed")) stop("BCAP Phase 3 QA failed; no analysis-ready BCAP products were finalized.", call. = FALSE)
  tables <- list(records = records, repeated_items = repeated, provenance = provenance, field_metadata = metadata)
  result <- .phase3_finalize_family(paths$output_dir, "bcap", tables, qa, run_id, overwrite)
  if (!isTRUE(quiet)) cli::cli_inform("Finalized separate BCAP analysis products for {nrow(records)} records; QA status {qa$status}.")
  result
}

#' Cross-reference safe Epi and BCAP records using authoritative keys only
#'
#' This optional utility never infers relationships from dates, names,
#' geography, or response similarity. Zero links is a valid result.
#'
#' @param epi_dir Existing deidentified Initial Epi output directory.
#' @param bcap_dir Existing deidentified BCAP output directory.
#' @param epi_crosswalk_dir Optional private Epi crosswalk.
#' @param bcap_crosswalk_dir Optional private BCAP crosswalk.
#' @return A safe linkage table and counts.
#' @export
cross_reference_hpai <- function(epi_dir, bcap_dir, epi_crosswalk_dir = NULL, bcap_crosswalk_dir = NULL) {
  epi_dir <- .phase3_safe_path(epi_dir, "epi_dir")
  bcap_dir <- .phase3_safe_path(bcap_dir, "bcap_dir")
  epi <- .phase3_epi_read(epi_dir)
  bcap <- .phase3_bcap_read(bcap_dir)
  epi_crosswalk <- .phase3_crosswalk(epi_crosswalk_dir, "epi", epi$forms$record_id)
  bcap_crosswalk <- .phase3_crosswalk(bcap_crosswalk_dir, "bcap", bcap$metadata$record_id)
  entity_map <- .phase3_entity_map(epi, bcap, epi_crosswalk, bcap_crosswalk)
  records <- entity_map$records
  epi_records <- records[records$form_type == "initial_epi" & records$entity_scope == "shared_authoritative_key", , drop = FALSE]
  bcap_records <- records[records$form_type == "bcap" & records$entity_scope == "shared_authoritative_key", , drop = FALSE]
  linkage <- .phase3_linkage(entity_map, tibble::tibble())
  linkage <- linkage[, c("left_record_id", "right_record_id", "link_type", "link_basis", "link_confidence"), drop = FALSE]
  result <- list(
    status = "passed", linkage = linkage, authoritative_links = nrow(linkage),
    epi_records = nrow(epi$forms), bcap_records = nrow(bcap$metadata),
    linked_records = length(unique(c(linkage$left_record_id, linkage$right_record_id))),
    unlinked_records = nrow(epi$forms) + nrow(bcap$metadata) - length(unique(c(linkage$left_record_id, linkage$right_record_id))),
    epi_authoritative_candidates = nrow(epi_records), bcap_authoritative_candidates = nrow(bcap_records)
  )
  class(result) <- c("bcapture_hpai_cross_reference", "list")
  result
}

#' Write separate Epi and BCAP Phase 3 products below one analysis directory
#'
#' The wrapper is optional; it orchestrates independent family pipelines and
#' returns `out$epi` and `out$bcap`. It never creates a unified record/entity
#' table, and zero cross-form links do not block either family output.
#'
#' @param epi_dir Optional deidentified Initial Epi output directory.
#' @param bcap_dir Optional deidentified BCAP output directory.
#' @param output_dir Destination analysis directory.
#' @param epi_crosswalk_dir Optional private Epi crosswalk.
#' @param bcap_crosswalk_dir Optional private BCAP crosswalk.
#' @param epi_source_manifest Optional sanitized Epi extraction manifest.
#' @param bcap_source_manifest Optional sanitized BCAP extraction manifest.
#' @param run_id Optional stable run identifier.
#' @param overwrite Replace an existing analysis directory after all gates pass.
#' @param quiet Suppress progress messages.
#' @return A list containing separate family results and optional safe
#'   cross-reference output.
#' @export
collate_hpai <- function(epi_dir = NULL, bcap_dir = NULL, output_dir, epi_crosswalk_dir = NULL, bcap_crosswalk_dir = NULL, epi_source_manifest = NULL, bcap_source_manifest = NULL, run_id = NULL, overwrite = FALSE, quiet = FALSE) {
  if (is.null(epi_dir) && is.null(bcap_dir)) stop("Provide at least one safe Epi or BCAP input directory.", call. = FALSE)
  output_dir <- .phase3_safe_path(output_dir, "output_dir")
  input_dirs <- c(epi_dir, bcap_dir)
  input_dirs <- input_dirs[!vapply(input_dirs, is.null, logical(1))]
  if (any(vapply(input_dirs, function(path) .phase3_path_within(output_dir, .phase3_safe_path(path, "input_dir")), logical(1)))) stop("Analysis output must be separate from safe inputs.", call. = FALSE)
  crosswalks <- c(epi_crosswalk_dir, bcap_crosswalk_dir)
  crosswalks <- crosswalks[!vapply(crosswalks, is.null, logical(1))]
  if (length(crosswalks) > 0L && any(vapply(crosswalks, function(path) .phase3_path_within(output_dir, .phase3_safe_path(path, "crosswalk_dir")), logical(1)))) stop("Analysis output must be separate from private crosswalks.", call. = FALSE)
  if (dir.exists(output_dir) && !isTRUE(overwrite)) stop("Analysis output already exists; set overwrite = TRUE to replace it.", call. = FALSE)
  if (is.null(run_id)) run_id <- paste0("phase3-", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  if (length(run_id) != 1L || is.na(run_id) || !grepl("^[A-Za-z0-9_.-]+$", run_id)) stop("`run_id` must be one safe path component.", call. = FALSE)
  parent <- fs::path_dir(output_dir)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  temp_root <- tempfile("phase3-analysis-", tmpdir = parent)
  dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
  committed <- FALSE
  on.exit(if (!committed && dir.exists(temp_root)) unlink(temp_root, recursive = TRUE, force = TRUE), add = TRUE)
  result <- list(output_dir = output_dir, run_id = run_id)
  if (!is.null(epi_dir)) result$epi <- collate_epi_analysis(epi_dir, fs::path(temp_root, "epi"), epi_crosswalk_dir, epi_source_manifest, run_id, overwrite = FALSE, quiet = TRUE)
  if (!is.null(bcap_dir)) result$bcap <- collate_bcap(bcap_dir, fs::path(temp_root, "bcap"), bcap_crosswalk_dir, bcap_source_manifest, run_id, overwrite = FALSE, quiet = TRUE)
  if (!is.null(epi_dir) && !is.null(bcap_dir)) {
    cross <- cross_reference_hpai(epi_dir, bcap_dir, epi_crosswalk_dir, bcap_crosswalk_dir)
    dir.create(fs::path(temp_root, "qa"), recursive = TRUE, showWarnings = FALSE)
    write_csv_utf8(cross$linkage, fs::path(temp_root, "qa", "cross_reference.csv"))
    saveRDS(cross$linkage, fs::path(temp_root, "qa", "cross_reference.rds"), version = 3)
    result$cross_reference <- cross
  }
  dir.create(fs::path(temp_root, "metadata"), recursive = TRUE, showWarnings = FALSE)
  if (!is.null(result$epi)) {
    write_csv_utf8(result$epi$tables$field_metadata, fs::path(temp_root, "metadata", "epi_field_metadata.csv"))
    saveRDS(result$epi$tables$field_metadata, fs::path(temp_root, "metadata", "epi_field_metadata.rds"), version = 3)
  }
  if (!is.null(result$bcap)) {
    write_csv_utf8(result$bcap$tables$field_metadata, fs::path(temp_root, "metadata", "bcap_field_metadata.csv"))
    saveRDS(result$bcap$tables$field_metadata, fs::path(temp_root, "metadata", "bcap_field_metadata.rds"), version = 3)
  }
  dir.create(fs::path(temp_root, "manifests"), recursive = TRUE, showWarnings = FALSE)
  family_manifest <- dplyr::bind_rows(lapply(result[c("epi", "bcap")][!vapply(result[c("epi", "bcap")], is.null, logical(1))], function(x) x$manifest))
  write_csv_utf8(family_manifest, fs::path(temp_root, "manifests", "run_manifest.csv"))
  saveRDS(family_manifest, fs::path(temp_root, "manifests", "run_manifest.rds"), version = 3)
  family_hashes <- dplyr::bind_rows(lapply(names(result)[names(result) %in% c("epi", "bcap")], function(name) dplyr::mutate(result[[name]]$output_hashes, form_type = name)))
  if (!is.null(result$cross_reference)) family_hashes <- dplyr::bind_rows(family_hashes, tibble::tibble(product = "cross_reference", format = "csv", sha256 = source_sha256(fs::path(temp_root, "qa", "cross_reference.csv")), row_count = nrow(result$cross_reference$linkage), column_count = ncol(result$cross_reference$linkage), form_type = "cross_reference"))
  write_csv_utf8(family_hashes, fs::path(temp_root, "manifests", "output_hashes.csv"))
  backup <- NULL
  if (dir.exists(output_dir)) {
    backup <- tempfile("phase3-analysis-backup-", tmpdir = parent)
    if (!file.rename(output_dir, backup)) stop("Unable to stage existing analysis output for replacement.", call. = FALSE)
  }
  if (!file.rename(temp_root, output_dir)) {
    if (!is.null(backup)) file.rename(backup, output_dir)
    stop("Unable to finalize separate Phase 3 analysis output directory.", call. = FALSE)
  }
  if (!is.null(backup) && dir.exists(backup)) unlink(backup, recursive = TRUE, force = TRUE)
  committed <- TRUE
  statuses <- c(if (!is.null(result$epi)) result$epi$status, if (!is.null(result$bcap)) result$bcap$status)
  result$status <- if (any(statuses == "failed")) "failed" else if (any(statuses == "passed_with_warnings")) "passed_with_warnings" else "passed"
  class(result) <- c("bcapture_hpai_collation", "list")
  if (!isTRUE(quiet)) cli::cli_inform("Finalized separate Phase 3 Epi/BCAP outputs below {output_dir}; status {result$status}.")
  result
}

