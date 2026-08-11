utils::globalVariables(c(
  "audit_id", "field", "field_index", "field_type", "form_schema_hash",
  "is_populated", "n_audits_missing", "n_audits_present", "n_indexes",
  "n_state_sets", "n_types", "number_of_field_types", "number_of_fields",
  "number_of_populated_fields", "number_of_widgets", "page", "present",
  "schema_group", "source_file", "source_relpath", "states_normalized",
  "options", "options_normalized", "n_option_sets", "n_raw_option_sets",
  "number_of_choice_fields", "number_of_multiselect_fields", "severity",
  "form_id", "alternative_name", "states", "default_value", "is_multiselect",
  "raw_field", "canonical_name", "table_name", "row_index", "column_name",
  "source_sha256", "source_sha256_metadata", "source_page", "section_id", "section_name",
  "question_id", "subquestion_id", "field_role", "response_type", "raw_value", "value",
  "response_code", "response_label", "units", "n_rows", "status", "mapping_count",
  "mapped", "unknown_field_names", "raw_fields", "mapped_fields", "unknown_fields",
  "populated_fields", "populated_mapped_fields", "populated_unmapped_fields"
))

schema_hash_from_fields <- function(fields) {
  definitions <- purrr::map(seq_len(nrow(fields)), function(i) {
    list(
      field = as.character(fields$field[[i]]),
      field_type = as.character(fields$field_type[[i]]),
      states = normalize_state_set(fields$states[[i]]),
      options = normalize_option_set(if ("options" %in% names(fields)) fields$options[[i]] else NA_character_),
      field_flags = if (identical(as.character(fields$field_type[[i]]), "Ch")) as.integer(if ("field_flags" %in% names(fields)) fields$field_flags[[i]] %||% 0L else 0L) else 0L
    )
  })
  module <- ensure_hpai_python()
  as.character(module$schema_hash(definitions))
}

assign_schema_groups <- function(hashes) {
  unique_hashes <- sort(unique(as.character(stats::na.omit(hashes))))
  groups <- stats::setNames(sprintf("schema_%03d", seq_along(unique_hashes)), unique_hashes)
  unname(groups[as.character(hashes)])
}

schema_difference_counts <- function(diagnostic) {
  list(
    schemas = nrow(diagnostic$schema_groups),
    presence = nrow(diagnostic$field_presence_differences),
    types = nrow(diagnostic$field_type_differences),
    states = nrow(diagnostic$field_state_differences),
    options = nrow(diagnostic$field_option_differences),
    order = nrow(diagnostic$field_order_differences),
    widgets = nrow(diagnostic$widget_differences)
  )
}
