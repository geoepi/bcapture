# Versioned analyst-facing field metadata ------------------------------------

.phase3_metadata_columns <- c(
  "field_name", "canonical_name", "form_type", "form_version", "section",
  "subsection", "question_number", "question_text", "response_type",
  "source_page", "source_location", "source_field_name", "units",
  "allowed_values", "privacy_class", "included_in_safe_output", "table_name",
  "row_semantics", "column_semantics", "notes"
)

.phase3_privacy_label <- function(action) {
  action <- as.character(action)
  result <- rep("WITHHOLD", length(action))
  result[action == "retain"] <- "SAFE_RETAIN"
  result[action == "coarsen"] <- "SAFE_NORMALIZE"
  result[action == "pseudonymize"] <- "PSEUDONYMIZE"
  result[action == "crosswalk_only"] <- "CROSSWALK_ONLY"
  result
}
.phase3_na_character <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(trimws(x))] <- NA_character_
  x
}

.phase3_epi_allowed_values <- function(codes, codebook_id) {
  if (is.na(codebook_id) || !nzchar(codebook_id)) return(NA_character_)
  values <- codes[as.character(codes$codebook_id) == as.character(codebook_id), , drop = FALSE]
  if (nrow(values) == 0L) return(NA_character_)
  paste(paste0(values$raw_code, " = ", values$response_label), collapse = " | ")
}

.phase3_epi_field_metadata <- function(version = .phase3_epi_version) {
  dictionary <- load_epi_dictionary(version)
  rules <- .epi_deid_load_rules(version, "analysis", dictionary)
  fields <- dictionary$fields
  policy <- rules[match(fields$raw_field, rules$raw_field), , drop = FALSE]
  if (any(is.na(policy$action))) stop("Epi metadata policy does not cover every dictionary field.", call. = FALSE)
  table_name <- .phase3_na_character(fields$table_name)
  row_semantics <- .phase3_na_character(fields$row_label)
  question_text <- .phase3_na_character(fields$question_text)
  source_page <- suppressWarnings(as.integer(fields$source_page))
  metadata <- tibble::tibble(
    field_name = ifelse(is.na(table_name), as.character(fields$canonical_name), as.character(fields$column_name)),
    canonical_name = as.character(fields$canonical_name),
    form_type = "initial_epi",
    form_version = as.character(version),
    section = as.character(fields$section_name),
    subsection = as.character(fields$section_id),
    question_number = as.character(fields$question_id),
    question_text = question_text,
    response_type = as.character(fields$response_type),
    source_page = source_page,
    source_location = ifelse(is.na(source_page), NA_character_, paste0("page ", source_page)),
    source_field_name = as.character(fields$raw_field),
    units = .phase3_na_character(fields$units),
    allowed_values = vapply(fields$codebook_id, function(x) .phase3_epi_allowed_values(dictionary$codes, x), character(1)),
    privacy_class = .phase3_privacy_label(policy$action),
    included_in_safe_output = policy$action %in% c("retain", "coarsen", "pseudonymize"),
    table_name = table_name,
    row_semantics = row_semantics,
    column_semantics = .phase3_na_character(fields$alternative_name),
    notes = .phase3_na_character(fields$notes)
  )
  fixed_specs <- tibble::tribble(
    ~table_name, ~field_name, ~canonical_name, ~question_text, ~allowed_values,
    "bird_movements", "direction", "bird_movements_direction", "Movement direction", "onto | off",
    "egg_movements", "direction", "egg_movements_direction", "Movement direction", "onto | off",
    "egg_movements", "material_type", "egg_movements_material_type", "Egg movement material type", "eggs | egg_products",
    "mortality_disposal", "method", "mortality_disposal_method", "Mortality or disposal method", NA_character_
  )
  fixed_metadata <- purrr::pmap_dfr(
    fixed_specs,
    function(table_name, field_name, canonical_name, question_text, allowed_values) tibble::tibble(
      field_name = field_name, canonical_name = canonical_name, form_type = "initial_epi", form_version = as.character(version),
      section = "Repeated table", subsection = table_name, question_number = NA_character_, question_text = question_text,
      response_type = "derived_categorical", source_page = NA_integer_, source_location = NA_character_, source_field_name = NA_character_,
      units = NA_character_, allowed_values = allowed_values, privacy_class = "SAFE_RETAIN", included_in_safe_output = TRUE,
      table_name = table_name, row_semantics = NA_character_, column_semantics = field_name,
      notes = "Derived from the versioned repeated-table row structure; no independent source widget exists."
    )
  )
  multiselect <- fields[as.character(fields$response_type) == "multiselect" | as.character(fields$field_role) %in% c("table_response", "table_cell"), , drop = FALSE]
  multiselect <- multiselect[!duplicated(multiselect$raw_field), , drop = FALSE]
  multi_policy <- rules[match(multiselect$raw_field, rules$raw_field), , drop = FALSE]
  multiselect_metadata <- tibble::tibble(
    field_name = as.character(multiselect$canonical_name), canonical_name = as.character(multiselect$canonical_name),
    form_type = "initial_epi", form_version = as.character(version), section = as.character(multiselect$section_name),
    subsection = as.character(multiselect$section_id), question_number = as.character(multiselect$question_id),
    question_text = as.character(multiselect$question_text), response_type = "categorical_item", source_page = suppressWarnings(as.integer(multiselect$source_page)),
    source_location = ifelse(is.na(multiselect$source_page), NA_character_, paste0("page ", multiselect$source_page)),
    source_field_name = as.character(multiselect$raw_field), units = .phase3_na_character(multiselect$units),
    allowed_values = vapply(multiselect$codebook_id, function(x) .phase3_epi_allowed_values(dictionary$codes, x), character(1)),
    privacy_class = .phase3_privacy_label(multi_policy$action), included_in_safe_output = multi_policy$action %in% c("retain", "coarsen", "pseudonymize"),
    table_name = "multiselect_responses", row_semantics = NA_character_, column_semantics = .phase3_na_character(multiselect$alternative_name),
    notes = .phase3_na_character(multiselect$notes)
  )
  coded <- fields[!is.na(fields$table_name) & nzchar(fields$table_name) & !is.na(fields$codebook_id) & nzchar(fields$codebook_id), , drop = FALSE]
  coded <- coded[!duplicated(paste(coded$table_name, coded$column_name, sep = "\r")), , drop = FALSE]
  coded_key <- paste(metadata$table_name, metadata$field_name, sep = "\r")
  code_base <- metadata[match(paste(coded$table_name, coded$column_name, sep = "\r"), coded_key), , drop = FALSE]
  code_base <- code_base[!is.na(code_base$field_name), , drop = FALSE]
  code_metadata <- code_base
  if (nrow(code_metadata) > 0L) {
    code_metadata$field_name <- paste0(code_metadata$field_name, "_code")
    code_metadata$canonical_name <- paste0(code_metadata$canonical_name, "_code")
    code_metadata$response_type <- "categorical_code"
    code_metadata$column_semantics <- paste0(code_metadata$column_semantics, " code")
    code_metadata$notes <- paste(code_metadata$notes, "Stable code representation emitted alongside the human-readable value.")
  }
  metadata <- dplyr::bind_rows(metadata, fixed_metadata, multiselect_metadata, code_metadata)
  metadata[order(is.na(metadata$table_name), metadata$table_name, metadata$field_name, metadata$source_field_name), , drop = FALSE]
}

.phase3_bcap_response_type <- function(field_type) {
  field_type <- as.character(field_type)
  result <- rep("unknown", length(field_type))
  result[field_type == "Tx"] <- "text"
  result[field_type == "Btn"] <- "boolean_control"
  result[field_type == "Ch"] <- "choice"
  result[field_type == "Sig"] <- "signature_presence"
  result
}

.phase3_bcap_supplemental_fields <- function() {
  tibble::tibble(
    field = c(
      "Q1_1_remediation", "Q1_2_remediation", "Q1_3_remediation", "Q1_3_17e",
      "Q1_4_remediation", "Q1_5_remediation", "Q1_6_remediation", "Q1_7_remediation",
      "Q2_1_remediation", "Q2_2_remediation", "Q2_3_remediation", "Q3_1_remediation",
      "Q3_2_remediation", "Q3_3_remediation", "Q3_4_remediation", "Q3_5_remediation"
    ),
    field_type = c("Tx", "Tx", "Tx", "Btn", rep("Tx", 12L)),
    alternative_name = NA_character_, page = NA_integer_, x1 = NA_real_, y1 = NA_real_, x2 = NA_real_, y2 = NA_real_,
    states = NA_character_, options = NA_character_, stringsAsFactors = FALSE
  )
}

.phase3_bcap_allowed_values <- function(states, options) {
  values <- unique(c(
    unlist(strsplit(.phase3_na_character(states), "\\|", fixed = FALSE), use.names = FALSE),
    unlist(strsplit(.phase3_na_character(options), "\\|", fixed = FALSE), use.names = FALSE)
  ))
  values <- values[!is.na(values) & nzchar(trimws(values))]
  if (length(values) == 0L) NA_character_ else paste(values, collapse = " | ")
}

.phase3_bcap_field_metadata <- function(version = .phase3_bcap_versions) {
  if (!identical(as.character(version), .phase3_bcap_versions)) stop("Unsupported BCAP template version: ", version, call. = FALSE)
  fields <- .hpai_template_fields()
  if (nrow(fields) == 0L) stop("BCAP template field metadata is unavailable.", call. = FALSE)
  policy <- purrr::pmap_dfr(
    list(fields$field, fields$field_type, fields$alternative_name),
    function(field, field_type, alternative_name) {
      p <- .hpai_field_policy(field, field_type, alternative_name)
      tibble::tibble(privacy_class = p$privacy_class, notes = p$notes)
    }
  )
  section <- vapply(fields$field, .hpai_section, character(1))
  bbox <- paste(fields$x1, fields$y1, fields$x2, fields$y2, sep = ",")
  bbox[is.na(fields$x1) | is.na(fields$y1) | is.na(fields$x2) | is.na(fields$y2)] <- NA_character_
  metadata <- tibble::tibble(
    field_name = as.character(fields$field),
    canonical_name = as.character(fields$field),
    form_type = "bcap",
    form_version = as.character(version),
    section = "BCAP audit form",
    subsection = section,
    question_number = section,
    question_text = sub(":\\s*$", "", as.character(fields$alternative_name)),
    response_type = .phase3_bcap_response_type(fields$field_type),
    source_page = suppressWarnings(as.integer(fields$page)),
    source_location = ifelse(is.na(bbox), NA_character_, paste0("page ", fields$page, "; bbox ", bbox)),
    source_field_name = as.character(fields$field),
    units = NA_character_,
    allowed_values = vapply(seq_len(nrow(fields)), function(i) .phase3_bcap_allowed_values(fields$states[[i]], fields$options[[i]]), character(1)),
    privacy_class = as.character(policy$privacy_class),
    included_in_safe_output = policy$privacy_class %in% c("SAFE_RETAIN", "SAFE_NORMALIZE", "PSEUDONYMIZE"),
    table_name = NA_character_,
    row_semantics = NA_character_,
    column_semantics = .phase3_na_character(fields$alternative_name),
    notes = as.character(policy$notes)
  )
  supplemental <- .phase3_bcap_supplemental_fields()
  supplemental_policy <- purrr::pmap_dfr(
    list(supplemental$field, supplemental$field_type, supplemental$alternative_name),
    function(field, field_type, alternative_name) {
      p <- .hpai_field_policy(field, field_type, alternative_name)
      tibble::tibble(privacy_class = p$privacy_class, notes = paste(p$notes, "Source wording and physical coordinates require metadata review."))
    }
  )
  supplemental_metadata <- tibble::tibble(
    field_name = as.character(supplemental$field), canonical_name = as.character(supplemental$field),
    form_type = "bcap", form_version = as.character(version), section = "BCAP audit form",
    subsection = vapply(supplemental$field, .hpai_section, character(1)), question_number = vapply(supplemental$field, .hpai_section, character(1)),
    question_text = NA_character_, response_type = .phase3_bcap_response_type(supplemental$field_type), source_page = supplemental$page,
    source_location = NA_character_, source_field_name = as.character(supplemental$field), units = NA_character_, allowed_values = NA_character_,
    privacy_class = supplemental_policy$privacy_class, included_in_safe_output = supplemental_policy$privacy_class %in% c("SAFE_RETAIN", "SAFE_NORMALIZE", "PSEUDONYMIZE"),
    table_name = NA_character_, row_semantics = NA_character_, column_semantics = NA_character_, notes = supplemental_policy$notes
  )
  metadata <- dplyr::bind_rows(metadata, supplemental_metadata)
  metadata[order(metadata$source_page, metadata$field_name), , drop = FALSE]
}

.phase3_validate_field_metadata <- function(metadata, form, version) {
  missing <- setdiff(.phase3_metadata_columns, names(metadata))
  if (length(missing) > 0L) stop(form, " field metadata is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  expected_form_type <- if (identical(form, "epi")) "initial_epi" else "bcap"
  if (!all(metadata$form_type == expected_form_type) || any(metadata$form_version != version)) stop(form, " field metadata has an unrecognized version.", call. = FALSE)
  allowed_privacy <- c("SAFE_RETAIN", "SAFE_NORMALIZE", "PSEUDONYMIZE", "WITHHOLD", "CROSSWALK_ONLY")
  if (any(is.na(metadata$privacy_class) | !metadata$privacy_class %in% allowed_privacy)) stop(form, " field metadata has an invalid privacy class.", call. = FALSE)
  if (any(is.na(metadata$response_type) | !nzchar(metadata$response_type))) stop(form, " field metadata has a missing response type.", call. = FALSE)
  if (any(!is.na(metadata$source_page) & (metadata$source_page < 1L | metadata$source_page > 100L))) stop(form, " field metadata has an implausible source page.", call. = FALSE)
  identity <- paste(metadata$form_version, metadata$table_name, metadata$row_semantics, metadata$column_semantics, metadata$source_field_name, sep = "\r")
  if (anyDuplicated(identity)) stop(form, " field metadata has duplicate field identities.", call. = FALSE)
  metadata
}

#' Return versioned analyst-facing field metadata
#'
#' The metadata is generated only from version-controlled dictionaries,
#' templates, and privacy policies. It never reads production values.
#'
#' @param form One of `"epi"` or `"bcap"`.
#' @param version Versioned dictionary or template identifier.
#' @return A tibble with one row per canonical/source field mapping.
#' @export
field_metadata <- function(form = c("epi", "bcap"), version = NULL) {
  form <- match.arg(form)
  if (is.null(version)) version <- if (identical(form, "epi")) .phase3_epi_version else .phase3_bcap_versions
  result <- if (identical(form, "epi")) .phase3_epi_field_metadata(version) else .phase3_bcap_field_metadata(version)
  .phase3_validate_field_metadata(result, form, as.character(version))
}

