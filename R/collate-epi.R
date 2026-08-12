.epi_table_output_names <- c(
  ai_tests = "epi_ai_tests", houses = "epi_houses", mortality_disposal = "epi_mortality_disposal",
  manure_destinations = "epi_manure_destinations", imported_materials = "epi_imported_materials",
  worker_visits = "epi_worker_visits", crews = "epi_crews", visitors = "epi_visitors",
  shared_equipment = "epi_shared_equipment", bird_movements = "epi_bird_movements",
  egg_movements = "epi_egg_movements"
)

.epi_field_is_populated <- function(x) {
  if ("is_populated" %in% names(x) && !is.na(x$is_populated[[1L]])) {
    value <- x$is_populated[[1L]]
    return(isTRUE(value) || identical(as.character(value), "TRUE") || identical(as.character(value), "1"))
  }
  raw <- x$raw_value[[1L]] %||% x$value[[1L]]
  if (length(raw) == 0L || is.na(raw) || !nzchar(trimws(as.character(raw)))) return(FALSE)
  normalized <- sub("^/", "", trimws(as.character(raw)))
  !normalized %in% c("Off", epi_placeholder_values)
}

.epi_normalized_raw <- function(raw) {
  if (length(raw) == 0L || is.na(raw)) return(NA_character_)
  raw <- trimws(as.character(raw))
  if (!nzchar(raw)) return(NA_character_)
  sub("^/", "", raw)
}

.epi_raw_value <- function(x) {
  raw <- if ("value_raw" %in% names(x)) x$value_raw[[1L]] else NA_character_
  if (length(raw) == 0L || is.na(raw) || !nzchar(as.character(raw))) raw <- x$value[[1L]] %||% NA_character_
  as.character(raw)
}

.epi_value <- function(x) {
  value <- if ("value" %in% names(x)) x$value[[1L]] else NA_character_
  if (length(value) == 0L || is.na(value)) return(NA_character_)
  sub("^/", "", as.character(value))
}

.epi_first_nonmissing <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) NA_character_ else x[[1L]]
}

.epi_parse_date <- function(value) {
  value <- as.character(value)
  if (length(value) == 0L || is.na(value) || !nzchar(trimws(value))) return(as.Date(NA))
  for (format in c("%m/%d/%Y", "%m/%d/%y")) {
    parsed <- as.Date(value, format = format)
    if (!is.na(parsed)) return(parsed)
  }
  as.Date(NA)
}

.epi_parse_numeric <- function(value) {
  value <- as.character(value)
  if (length(value) == 0L || is.na(value) || !nzchar(trimws(value))) return(NA_real_)
  if (!grepl("^\\s*[+-]?[0-9]+([.][0-9]+)?\\s*$", value)) return(NA_real_)
  as.numeric(trimws(value))
}

.epi_parse_date_vector <- function(values) {
  parsed <- rep(as.Date(NA), length(values))
  for (i in seq_along(values)) parsed[[i]] <- .epi_parse_date(values[[i]])
  parsed
}

.epi_parse_numeric_vector <- function(values) {
  parsed <- rep(NA_real_, length(values))
  for (i in seq_along(values)) parsed[[i]] <- .epi_parse_numeric(values[[i]])
  parsed
}

.epi_table_column_metadata <- function(table_name, dictionary) {
  fields <- dictionary$fields[dictionary$fields$table_name == table_name, , drop = FALSE]
  columns <- unique(fields$column_name[!is.na(fields$column_name) & nzchar(fields$column_name)])
  purrr::map_dfr(columns, function(column) {
    current <- fields[fields$column_name == column, , drop = FALSE]
    types <- unique(as.character(current$data_type[!is.na(current$data_type) & nzchar(current$data_type)]))
    if (length(types) > 1L) stop("Initial Epi dictionary has incompatible data types for ", table_name, ".", column, call. = FALSE)
    tibble::tibble(
      column_name = column,
      data_type = if (length(types) == 0L) "character" else types[[1L]],
      has_code = any((!is.na(current$codebook_id) & nzchar(current$codebook_id)) | current$response_type %in% c("coded", "choice"))
    )
  })
}

.epi_table_type_contract <- function(table_name, dictionary) {
  fixed <- c(
    form_id = "character", form_version = "character", dictionary_version = "character",
    dictionary_hash = "character", row_index = "integer", row_label = "character",
    raw_fields = "character", source_pages = "character"
  )
  if (table_name %in% c("bird_movements", "egg_movements")) fixed <- c(fixed, direction = "character")
  if (table_name == "egg_movements") fixed <- c(fixed, material_type = "character")
  if (table_name == "mortality_disposal") fixed <- c(fixed, method = "character")
  metadata <- .epi_table_column_metadata(table_name, dictionary)
  cell_types <- unlist(purrr::map(metadata$column_name, function(column) {
    type <- metadata$data_type[metadata$column_name == column][[1L]]
    semantic_type <- if (identical(type, "date")) "Date" else if (identical(type, "numeric")) "double" else "character"
    values <- c(stats::setNames("character", paste0(column, "_raw")), stats::setNames(semantic_type, column))
    if (isTRUE(metadata$has_code[metadata$column_name == column][[1L]])) values <- c(values, stats::setNames("character", paste0(column, "_code")))
    values
  }), use.names = TRUE)
  c(fixed, cell_types)
}

validate_epi_table_types <- function(table_name, table, dictionary) {
  expected <- .epi_table_type_contract(table_name, dictionary)
  actual <- vapply(names(expected), function(column) {
    value <- table[[column]]
    if (inherits(value, "Date")) "Date" else if (is.integer(value)) "integer" else if (is.double(value)) "double" else if (is.logical(value)) "logical" else if (is.character(value)) "character" else paste(class(value), collapse = "/")
  }, character(1))
  mismatch <- names(expected)[actual != unname(expected)]
  if (length(mismatch) > 0L) {
    details <- paste0(table_name, ": ", mismatch, " expected ", unname(expected[mismatch]), " actual ", actual[mismatch])
    stop("Epi relational table type validation failed: ", paste(details, collapse = "; "), call. = FALSE)
  }
  invisible(TRUE)
}

.epi_empty_parse_diagnostics <- function() {
  tibble::tibble(
    form_id = character(), raw_field = character(), canonical_name = character(),
    table_name = character(), row_index = integer(), parse_type = character(), status = character()
  )
}

.epi_cast_repeated_table <- function(records, table_name, dictionary) {
  diagnostics <- .epi_empty_parse_diagnostics()
  metadata <- .epi_table_column_metadata(table_name, dictionary)
  for (i in seq_len(nrow(metadata))) {
    column <- metadata$column_name[[i]]
    raw_column <- paste0(column, "_raw")
    raw_field_column <- paste0(column, "_raw_field")
    if (!raw_column %in% names(records)) records[[raw_column]] <- rep(NA_character_, nrow(records))
    if (!column %in% names(records)) records[[column]] <- rep(NA_character_, nrow(records))
    if (!raw_field_column %in% names(records)) records[[raw_field_column]] <- rep(NA_character_, nrow(records))
    if (isTRUE(metadata$has_code[[i]]) && !paste0(column, "_code") %in% names(records)) records[[paste0(column, "_code")]] <- rep(NA_character_, nrow(records))
  }
  for (i in seq_len(nrow(metadata))) {
    column <- metadata$column_name[[i]]
    data_type <- metadata$data_type[[i]]
    raw_column <- paste0(column, "_raw")
    raw_values <- as.character(records[[raw_column]])
    if (identical(data_type, "date")) {
      parsed <- .epi_parse_date_vector(raw_values)
      bad <- which(!is.na(raw_values) & nzchar(trimws(raw_values)) & is.na(parsed))
      if (length(bad) > 0L) diagnostics <- dplyr::bind_rows(diagnostics, tibble::tibble(
        form_id = as.character(records$form_id[bad]), raw_field = as.character(records[[paste0(column, "_raw_field")]][bad]),
        canonical_name = as.character(dictionary$fields$canonical_name[match(records[[paste0(column, "_raw_field")]][bad], dictionary$fields$raw_field)]),
        table_name = table_name, row_index = as.integer(records$row_index[bad]), parse_type = "date", status = "failed"
      ))
      records[[column]] <- parsed
    } else if (identical(data_type, "numeric")) {
      parsed <- .epi_parse_numeric_vector(raw_values)
      bad <- which(!is.na(raw_values) & nzchar(trimws(raw_values)) & is.na(parsed))
      if (length(bad) > 0L) diagnostics <- dplyr::bind_rows(diagnostics, tibble::tibble(
        form_id = as.character(records$form_id[bad]), raw_field = as.character(records[[paste0(column, "_raw_field")]][bad]),
        canonical_name = as.character(dictionary$fields$canonical_name[match(records[[paste0(column, "_raw_field")]][bad], dictionary$fields$raw_field)]),
        table_name = table_name, row_index = as.integer(records$row_index[bad]), parse_type = "numeric", status = "failed"
      ))
      records[[column]] <- parsed
    } else {
      records[[column]] <- as.character(records[[column]])
    }
  }
  internal_columns <- grep("_raw_field$", names(records), value = TRUE)
  if (length(internal_columns) > 0L) records[internal_columns] <- NULL
  validate_epi_table_types(table_name, records, dictionary)
  list(data = records, diagnostics = diagnostics)
}

.epi_join_dictionary <- function(fields, dictionary) {
  if (!"field" %in% names(fields)) stop("`combined/epi_fields_long.csv` must contain a `field` column.", call. = FALSE)
  fields$raw_field <- as.character(fields$field)
  if (!"form_id" %in% names(fields)) stop("The raw Epi field product must contain `form_id`.", call. = FALSE)
  fields <- dplyr::left_join(fields, dictionary$fields, by = "raw_field", suffix = c("", "_dictionary"))
  fields$mapped <- !is.na(fields$canonical_name) & nzchar(fields$canonical_name)
  fields$populated <- vapply(seq_len(nrow(fields)), function(i) .epi_field_is_populated(fields[i, , drop = FALSE]), logical(1))
  fields
}

.epi_response_rows <- function(mapped, dictionary) {
  rows <- mapped[isTRUE(mapped$mapped) | mapped$mapped %in% TRUE, , drop = FALSE]
  rows <- rows[is.na(rows$table_name) | !nzchar(rows$table_name), , drop = FALSE]
  if (nrow(rows) == 0L) return(tibble::tibble())
  codes <- dictionary$codes
  result <- purrr::map_dfr(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    raw_value <- .epi_raw_value(row)
    normalized <- .epi_normalized_raw(raw_value)
    codebook <- row$codebook_id[[1L]]
    code <- if (row$response_type[[1L]] %in% c("coded", "choice") || (!is.na(codebook) && nzchar(codebook))) normalized else NA_character_
    label <- normalized
    if (!is.na(codebook) && nzchar(codebook) && !is.na(code)) {
      match <- codes[codes$codebook_id == codebook & as.character(codes$raw_code) == code, , drop = FALSE]
      if (nrow(match) > 0L) label <- as.character(match$response_label[[1L]])
    }
    tibble::tibble(
      form_id = as.character(row$form_id[[1L]]), form_version = "2024-05-28", dictionary_version = "1",
      dictionary_hash = dictionary$dictionary_hash, section_id = row$section_id[[1L]], section_name = row$section_name[[1L]],
      question_id = row$question_id[[1L]], subquestion_id = row$subquestion_id[[1L]], raw_field = row$raw_field[[1L]],
      canonical_name = row$canonical_name[[1L]], field_role = row$field_role[[1L]], response_type = row$response_type[[1L]],
      raw_value = raw_value, value = label, response_code = code, response_label = label, units = row$units[[1L]],
      source_page = as.integer(row$source_page[[1L]]), is_populated = row$populated[[1L]]
    )
  })
  result
}

.epi_multiselect_rows <- function(mapped, dictionary) {
  group_fields <- c(paste0("p0125", letters[1:7]), paste0("p0132", letters[1:7]))
  source <- mapped[mapped$mapped %in% TRUE & (mapped$is_multiselect %in% TRUE | mapped$raw_field %in% group_fields), , drop = FALSE]
  if (nrow(source) == 0L) return(tibble::tibble(form_id = character(), raw_field = character(), canonical_name = character(), item_code = character(), item_label = character()))
  purrr::map_dfr(seq_len(nrow(source)), function(i) {
    row <- source[i, , drop = FALSE]
    if (!isTRUE(row$populated[[1L]])) return(tibble::tibble())
    raw <- .epi_normalized_raw(.epi_raw_value(row))
    if (is.na(raw) || raw %in% c("Off", epi_placeholder_values)) return(tibble::tibble())
    group <- row$raw_field[[1L]] %in% group_fields
    items <- if (group) raw else unlist(strsplit(raw, "\\s*[|;,]\\s*"))
    items <- items[nzchar(items) & !items %in% epi_placeholder_values]
    if (length(items) == 0L) return(tibble::tibble())
    tibble::tibble(
      form_id = row$form_id[[1L]], raw_field = row$raw_field[[1L]], canonical_name = row$canonical_name[[1L]],
      item_code = if (group) row$raw_field[[1L]] else items, item_label = if (group) row$alternative_name[[1L]] else items
    )
  })
}

.epi_table_record <- function(mapped, form_id, table_name, row_index, dictionary) {
  cell <- mapped[mapped$form_id == form_id & mapped$table_name == table_name & as.character(mapped$row_index) == as.character(row_index), , drop = FALSE]
  if (nrow(cell) == 0L || !any(cell$populated %in% TRUE)) return(NULL)
  row <- list(
    form_id = form_id, form_version = "2024-05-28", dictionary_version = "1", dictionary_hash = dictionary$dictionary_hash,
    row_index = as.integer(row_index), row_label = .epi_first_nonmissing(cell$row_label),
    raw_fields = paste(cell$raw_field, collapse = "|"), source_pages = paste(sort(unique(cell$source_page)), collapse = "|")
  )
  if (table_name == "bird_movements") row$direction <- .epi_first_nonmissing(cell$row_label)
  if (table_name == "egg_movements") {
    row$direction <- .epi_first_nonmissing(cell$row_label)
    row$material_type <- if (any(grepl("^p01(82|83|84|85|92|93|94|95)", cell$raw_field))) "eggs" else "egg_products"
  }
  if (table_name == "mortality_disposal") row$method <- .epi_first_nonmissing(cell$row_label)
  columns <- unique(cell$column_name[!is.na(cell$column_name) & nzchar(cell$column_name)])
  for (column in columns) {
    candidates <- cell[cell$column_name == column, , drop = FALSE]
    selected <- candidates[which(candidates$populated %in% TRUE)[1L], , drop = FALSE]
    if (nrow(selected) == 0L || is.na(selected$raw_field[[1L]])) selected <- candidates[1L, , drop = FALSE]
    raw <- .epi_raw_value(selected)
    normalized <- .epi_normalized_raw(raw)
    codebook <- selected$codebook_id[[1L]]
    code <- if (selected$response_type[[1L]] %in% c("coded", "choice") || (!is.na(codebook) && nzchar(codebook))) normalized else NA_character_
    label <- normalized
    if (!is.na(codebook) && nzchar(codebook) && !is.na(code)) {
      match <- dictionary$codes[dictionary$codes$codebook_id == codebook & as.character(dictionary$codes$raw_code) == code, , drop = FALSE]
      if (nrow(match) > 0L) label <- as.character(match$response_label[[1L]])
    }
    row[[paste0(column, "_raw")]] <- as.character(raw)
    row[[column]] <- as.character(label)
    row[[paste0(column, "_raw_field")]] <- as.character(selected$raw_field[[1L]])
    has_code <- selected$response_type[[1L]] %in% c("coded", "choice") || (!is.na(codebook) && nzchar(codebook))
    if (has_code) row[[paste0(column, "_code")]] <- as.character(code)
  }
  tibble::as_tibble(row)
}

.epi_empty_table <- function(table_name, dictionary) {
  contract <- .epi_table_type_contract(table_name, dictionary)
  tibble::as_tibble(stats::setNames(lapply(unname(contract), function(type) {
    switch(type, Date = as.Date(character()), double = double(), integer = integer(), logical = logical(), character())
  }), names(contract)))
}

.epi_collate_tables <- function(mapped, dictionary, form_ids) {
  outputs <- list()
  diagnostics <- .epi_empty_parse_diagnostics()
  for (table_name in dictionary$tables$table_name) {
    records <- list()
    table_fields <- dictionary$fields[dictionary$fields$table_name == table_name, , drop = FALSE]
    row_indices <- sort(unique(as.integer(table_fields$row_index)))
    for (form_id in form_ids) {
      for (row_index in row_indices) {
        record <- .epi_table_record(mapped, form_id, table_name, row_index, dictionary)
        if (!is.null(record)) {
          records[[length(records) + 1L]] <- record
        }
      }
    }
    if (length(records) == 0L) {
      outputs[[table_name]] <- .epi_empty_table(table_name, dictionary)
      validate_epi_table_types(table_name, outputs[[table_name]], dictionary)
    } else {
      cast <- .epi_cast_repeated_table(dplyr::bind_rows(records), table_name, dictionary)
      outputs[[table_name]] <- cast$data
      diagnostics <- dplyr::bind_rows(diagnostics, cast$diagnostics)
    }
  }
  list(outputs = outputs, diagnostics = diagnostics, parse_counts = list(
    date = sum(diagnostics$parse_type == "date"), numeric = sum(diagnostics$parse_type == "numeric")
  ))
}

#' Collate raw Initial Epi extraction products into semantic relational data
#'
#' @param out_dir Existing successful `extract_epi()` output directory.
#' @param version Dictionary version, currently `"2024-05-28"`.
#' @param write Write products below `out_dir/collated`.
#' @param strict Stop on populated unmapped fields or invalid semantic mappings.
#' @param quiet Suppress progress messages.
#' @return A named list of collated tibbles.
#' @export
collate_epi <- function(out_dir, version = "2024-05-28", write = TRUE, strict = TRUE, quiet = FALSE) {
  out_dir <- validate_scalar_path(out_dir, "out_dir")
  if (!dir.exists(out_dir)) stop("`out_dir` does not exist or is not a directory.", call. = FALSE)
  paths <- epi_combined_output_paths(out_dir)
  required <- c(paths$fields_long, paths$metadata, paths$manifest)
  missing <- required[!file.exists(required)]
  if (length(missing) > 0L) stop("Successful Epi extraction products are missing: ", paste(fs::path_file(missing), collapse = ", "), call. = FALSE)
  dictionary <- load_epi_dictionary(version)
  fields <- readr::read_csv(paths$fields_long, show_col_types = FALSE)
  metadata <- readr::read_csv(paths$metadata, show_col_types = FALSE)
  extraction_manifest <- readr::read_csv(paths$manifest, show_col_types = FALSE)
  if (!"status" %in% names(extraction_manifest) || any(extraction_manifest$status != "success", na.rm = TRUE)) {
    stop("collate_epi() requires a successful Epi extraction; extraction_manifest.csv contains failed form(s).", call. = FALSE)
  }
  if (nrow(extraction_manifest) == 0L || any(extraction_manifest$form_type != "initial_epi", na.rm = TRUE)) stop("extraction_manifest.csv is not an Initial Epi extraction.", call. = FALSE)
  mapped <- .epi_join_dictionary(fields, dictionary)
  unknown_populated <- unique(mapped$raw_field[!mapped$mapped %in% TRUE & mapped$populated %in% TRUE])
  if (isTRUE(strict) && length(unknown_populated) > 0L) stop("Unmapped populated Epi field(s): ", paste(unknown_populated, collapse = ", "), call. = FALSE)
  forms_ids <- unique(as.character(extraction_manifest$form_id))
  provenance <- extraction_manifest |>
    dplyr::transmute(
      form_id = as.character(form_id), form_version = "2024-05-28", dictionary_version = "1",
      dictionary_hash = dictionary$dictionary_hash, source_file = as.character(source_file), source_sha256 = as.character(source_sha256),
      form_schema_hash = as.character(form_schema_hash), schema_group = as.character(schema_group)
    ) |>
    dplyr::filter(form_id %in% forms_ids)
  if (nrow(metadata) > 0L && "source_sha256" %in% names(metadata)) {
    provenance <- provenance |>
      dplyr::left_join(metadata |> dplyr::select(form_id, source_sha256_metadata = source_sha256), by = "form_id") |>
      dplyr::mutate(source_sha256 = dplyr::coalesce(source_sha256, source_sha256_metadata)) |>
      dplyr::select(-source_sha256_metadata)
  }
  responses <- .epi_response_rows(mapped, dictionary)
  multiselect <- .epi_multiselect_rows(mapped, dictionary)
  forms <- provenance
  scalar_names <- unique(dictionary$fields$canonical_name[is.na(dictionary$fields$table_name) | !nzchar(dictionary$fields$table_name)])
  for (canonical_name in scalar_names) forms[[canonical_name]] <- NA_character_
  for (form_id in forms_ids) {
    values <- responses[responses$form_id == form_id & responses$is_populated %in% TRUE, , drop = FALSE]
    for (i in seq_len(nrow(values))) forms[forms$form_id == form_id, values$canonical_name[[i]]] <- values$value[[i]]
  }
  table_result <- .epi_collate_tables(mapped, dictionary, forms_ids)
  coverage <- dplyr::bind_rows(
    dictionary$fields |> dplyr::transmute(raw_field, mapped = TRUE, mapping_count = 1L, canonical_name, table_name, row_index, column_name),
    mapped |> dplyr::filter(!mapped %in% TRUE) |> dplyr::distinct(raw_field) |> dplyr::transmute(raw_field, mapped = FALSE, mapping_count = 0L, canonical_name = NA_character_, table_name = NA_character_, row_index = NA_real_, column_name = NA_character_)
  ) |> dplyr::distinct(raw_field, .keep_all = TRUE)
  table_counts <- tidyr::expand_grid(form_id = forms_ids, table_name = dictionary$tables$table_name)
  actual_counts <- purrr::map_dfr(names(table_result$outputs), function(table_name) {
    table <- table_result$outputs[[table_name]]
    if (nrow(table) == 0L) tibble::tibble(form_id = character(), table_name = character(), n_rows = integer()) else table |> dplyr::count(form_id, name = "n_rows") |> dplyr::mutate(table_name = table_name) |> dplyr::select(form_id, table_name, n_rows)
  })
  table_counts <- table_counts |> dplyr::left_join(actual_counts, by = c("form_id", "table_name")) |> dplyr::mutate(n_rows = dplyr::coalesce(n_rows, 0L))
  collation_manifest <- purrr::map_dfr(forms_ids, function(form_id) {
    current <- mapped[mapped$form_id == form_id, , drop = FALSE]
    unknown <- unique(current$raw_field[!current$mapped %in% TRUE])
    tibble::tibble(
      form_id = form_id, form_version = "2024-05-28", dictionary_version = "1", dictionary_hash = dictionary$dictionary_hash,
      raw_fields = nrow(current), mapped_fields = sum(current$mapped %in% TRUE), unknown_fields = length(unknown),
      unknown_field_names = paste(unknown, collapse = "|"), populated_fields = sum(current$populated %in% TRUE),
      populated_mapped_fields = sum(current$populated %in% TRUE & current$mapped %in% TRUE),
      populated_unmapped_fields = sum(current$populated %in% TRUE & !current$mapped %in% TRUE),
      status = if (sum(current$populated %in% TRUE & !current$mapped %in% TRUE) > 0L) "warning" else "success"
    )
  })
  diagnostics <- paste(
    "# Initial Epi Collation Diagnostics", "",
    paste0("Dictionary version: ", version), paste0("Forms processed: ", length(forms_ids)), "",
    "Dictionary coverage:", paste0("- Expected fields: ", nrow(dictionary$fields)), paste0("- Mapped fields: ", sum(coverage$mapped)), paste0("- Unmapped fields: ", sum(!coverage$mapped)), "",
    "Populated-field coverage:", paste0("- Populated responses: ", sum(mapped$populated %in% TRUE)), paste0("- Successfully mapped: ", sum(mapped$populated %in% TRUE & mapped$mapped %in% TRUE)), paste0("- Unmapped: ", sum(mapped$populated %in% TRUE & !mapped$mapped %in% TRUE)), "",
    "Repeated records:", paste0("- ", names(table_result$outputs), ": ", vapply(table_result$outputs, nrow, integer(1))), "",
    "Parsing warnings:", paste0("- Date parsing failures: ", table_result$parse_counts$date), paste0("- Numeric parsing failures: ", table_result$parse_counts$numeric), sep = "\n"
  )
  result <- list(
    forms = forms, responses = responses |> dplyr::select(-is_populated), multiselect = multiselect,
    ai_tests = table_result$outputs$ai_tests, houses = table_result$outputs$houses,
    mortality_disposal = table_result$outputs$mortality_disposal, manure_destinations = table_result$outputs$manure_destinations,
    imported_materials = table_result$outputs$imported_materials, worker_visits = table_result$outputs$worker_visits,
    crews = table_result$outputs$crews, visitors = table_result$outputs$visitors, shared_equipment = table_result$outputs$shared_equipment,
    bird_movements = table_result$outputs$bird_movements, egg_movements = table_result$outputs$egg_movements,
    coverage = coverage, manifest = collation_manifest, parse_diagnostics = table_result$diagnostics
  )
  if (isTRUE(write)) {
    collated_dir <- fs::path(out_dir, "collated")
    dir.create(collated_dir, recursive = TRUE, showWarnings = FALSE)
    write_csv_utf8(result$forms, fs::path(collated_dir, "epi_forms.csv"))
    write_csv_utf8(result$responses, fs::path(collated_dir, "epi_responses_long.csv"))
    write_csv_utf8(result$multiselect, fs::path(collated_dir, "epi_multiselect_responses.csv"))
    purrr::iwalk(table_result$outputs, ~ write_csv_utf8(.x, fs::path(collated_dir, paste0(.epi_table_output_names[[.y]], ".csv"))))
    write_csv_utf8(coverage, fs::path(collated_dir, "dictionary_coverage.csv"))
    write_csv_utf8(collation_manifest, fs::path(collated_dir, "collation_manifest.csv"))
    write_csv_utf8(table_counts, fs::path(collated_dir, "collation_table_counts.csv"))
    write_csv_utf8(result$parse_diagnostics, fs::path(collated_dir, "collation_parse_diagnostics.csv"))
    writeLines(diagnostics, fs::path(collated_dir, "collation_diagnostics.md"), useBytes = TRUE)
  }
  if (!isTRUE(quiet)) cli::cli_inform("Collated {length(forms_ids)} Initial Epi form{?s}.")
  result
}
