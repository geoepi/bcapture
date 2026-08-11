utils::globalVariables(c(
  "audit_id", "field", "field_index", "field_type", "form_schema_hash",
  "is_populated", "n_audits_missing", "n_audits_present", "n_indexes",
  "n_state_sets", "n_types", "number_of_field_types", "number_of_fields",
  "number_of_populated_fields", "number_of_widgets", "page", "present",
  "schema_group", "source_file", "source_relpath", "states_normalized"
))

schema_hash_from_fields <- function(fields) {
  definitions <- purrr::map(seq_len(nrow(fields)), function(i) {
    list(
      field = as.character(fields$field[[i]]),
      field_type = as.character(fields$field_type[[i]]),
      states = normalize_state_set(fields$states[[i]])
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
    order = nrow(diagnostic$field_order_differences),
    widgets = nrow(diagnostic$widget_differences)
  )
}
