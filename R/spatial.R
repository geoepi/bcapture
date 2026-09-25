spatial_result_tables <- function(parsed, record_id, form_type, source_file,
                                  source_relpath, source_checksum, id_col,
                                  checksum_col, epi = FALSE) {
  fields <- field_rows_to_tibble(parsed$fields, extraction_method = "spatial_template")
  if (isTRUE(epi)) fields <- .epi_fields(fields)
  fields <- dplyr::mutate(
    fields,
    record_id = record_id,
    form_type = form_type,
    source_file = source_file,
    source_relpath = as.character(source_relpath),
    source_checksum = source_checksum,
    .before = 1
  )
  names(fields)[names(fields) == "record_id"] <- id_col
  names(fields)[names(fields) == "source_checksum"] <- checksum_col
  populated <- fields[fields$is_populated %in% TRUE, , drop = FALSE]
  widgets <- widget_rows_to_tibble(parsed$widgets %||% list()) |>
    dplyr::mutate(
      record_id = record_id,
      form_type = form_type,
      source_file = source_file,
      source_relpath = as.character(source_relpath),
      source_checksum = source_checksum,
      .before = 1
    )
  names(widgets)[names(widgets) == "record_id"] <- id_col
  names(widgets)[names(widgets) == "source_checksum"] <- checksum_col
  metadata_values <- c(
    list(
      record_id = record_id,
      form_type = form_type,
      source_file = source_file,
      source_relpath = as.character(source_relpath),
      source_checksum = source_checksum,
      number_of_pages = as.integer(parsed$number_of_pages),
      number_of_fields = nrow(fields),
      number_of_widgets = nrow(widgets),
      number_of_source_widgets = as.integer(parsed$source_widget_count %||% 0L),
      number_of_canonical_fields = as.integer(parsed$number_of_canonical_fields %||% NA_integer_),
      number_of_canonical_widgets = as.integer(parsed$number_of_canonical_widgets %||% NA_integer_),
      number_of_populated_fields = nrow(populated),
      number_of_choice_fields = sum(fields$field_type == "Ch", na.rm = TRUE),
      number_of_multiselect_fields = sum(fields$is_multiselect %in% TRUE, na.rm = TRUE),
      form_schema_hash = as_optional_character(parsed$form_schema_hash),
      source_form_schema_hash = as_optional_character(parsed$source_form_schema_hash),
      canonical_template_schema_hash = as_optional_character(parsed$canonical_template_schema_hash),
      schema_identity = as_optional_character(parsed$canonical_template_schema_hash),
      template_family = as_optional_character(parsed$template_family),
      template_version = as_optional_character(parsed$template_version),
      registration_method = as_optional_character(parsed$registration_method),
      registration_quality = as_optional_character(parsed$registration_quality),
      registration_residual_pt = as.numeric(parsed$registration_residual_pt %||% NA_real_),
      anchor_fraction = as.numeric(parsed$anchor_fraction %||% NA_real_),
      anchor_matches = as.integer(parsed$anchor_matches %||% NA_integer_),
      anchor_eligible = as.integer(parsed$anchor_eligible %||% NA_integer_),
      control_threshold = as.numeric(parsed$control_threshold %||% NA_real_),
      extraction_method = "spatial_template",
      extraction_status = as_optional_character(parsed$extraction_status %||% "success"),
      extracted_at_utc = utc_now()
    ),
    parsed$pdf_metadata %||% list()
  )
  names(metadata_values)[names(metadata_values) == "record_id"] <- id_col
  names(metadata_values)[names(metadata_values) == "source_checksum"] <- checksum_col
  metadata <- as_single_row_tibble(metadata_values)
  wide_values <- c(
    list(
      record_id = record_id,
      form_type = form_type,
      source_file = source_file,
      source_relpath = as.character(source_relpath),
      source_checksum = source_checksum,
      form_schema_hash = as_optional_character(parsed$form_schema_hash),
      canonical_template_schema_hash = as_optional_character(parsed$canonical_template_schema_hash),
      schema_identity = as_optional_character(parsed$canonical_template_schema_hash)
    ),
    stats::setNames(as.list(fields$value), fields$field)
  )
  names(wide_values)[names(wide_values) == "record_id"] <- id_col
  names(wide_values)[names(wide_values) == "source_checksum"] <- checksum_col
  list(fields = fields, populated_fields = populated, widgets = widgets,
       metadata = metadata, wide = as_single_row_tibble(wide_values))
}

spatial_manifest_values <- function(parsed, record_id, form_type, source_file,
                                    source_relpath, source_checksum, id_col,
                                    checksum_col) {
  values <- list(
    record_id = record_id,
    form_type = form_type,
    source_file = source_file,
    source_relpath = as.character(source_relpath),
    source_checksum = source_checksum,
    status = "success",
    failure_type = NA_character_,
    error = NA_character_,
    number_of_pages = as.integer(parsed$number_of_pages),
    number_of_fields = as.integer(parsed$number_of_fields),
    number_of_widgets = as.integer(parsed$number_of_widgets),
    number_of_source_widgets = as.integer(parsed$source_widget_count %||% 0L),
    number_of_canonical_fields = as.integer(parsed$number_of_canonical_fields %||% NA_integer_),
    number_of_canonical_widgets = as.integer(parsed$number_of_canonical_widgets %||% NA_integer_),
    number_of_populated_fields = as.integer(parsed$number_of_populated_fields),
    form_schema_hash = as_optional_character(parsed$form_schema_hash),
    source_form_schema_hash = as_optional_character(parsed$source_form_schema_hash),
    canonical_template_schema_hash = as_optional_character(parsed$canonical_template_schema_hash),
    schema_identity = as_optional_character(parsed$canonical_template_schema_hash),
    schema_group = NA_character_,
    extraction_method = "spatial_template",
    template_family = as_optional_character(parsed$template_family),
    template_version = as_optional_character(parsed$template_version),
    registration_method = as_optional_character(parsed$registration_method),
    registration_quality = as_optional_character(parsed$registration_quality),
    registration_residual_pt = as.numeric(parsed$registration_residual_pt %||% NA_real_),
    anchor_fraction = as.numeric(parsed$anchor_fraction %||% NA_real_),
    control_threshold = as.numeric(parsed$control_threshold %||% NA_real_),
    pypdf_version = as_optional_character(parsed$pypdf_version),
    extracted_at_utc = utc_now()
  )
  names(values)[names(values) == "record_id"] <- id_col
  names(values)[names(values) == "source_checksum"] <- checksum_col
  as_single_row_tibble(values)
}

write_spatial_outputs <- function(tables, paths) {
  dir.create(paths$dir, recursive = TRUE, showWarnings = FALSE)
  write_csv_utf8(tables$fields, paths$fields_long)
  write_csv_utf8(tables$populated_fields, paths$populated_fields_long)
  write_csv_utf8(tables$wide, paths$fields_wide %||% paths$audits_wide)
  write_csv_utf8(tables$metadata, paths$metadata)
  write_csv_utf8(tables$widgets, paths$widgets)
  invisible(paths$dir)
}
