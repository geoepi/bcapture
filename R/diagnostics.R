#' Diagnose HPAI PDF form schemas
#'
#' Compares logical AcroForm fields and available widget diagnostics in an
#' existing bcapture extraction directory.
#'
#' @param out_dir An existing bcapture extraction directory.
#' @param write Write diagnostic CSV products and a Markdown report.
#' @param quiet Suppress concise diagnostic messages.
#' @return A named list of diagnostic tibbles.
#' @export
diagnose_hpai <- function(out_dir, write = TRUE, quiet = FALSE) {
  out_dir <- validate_scalar_path(out_dir, "out_dir")
  if (!dir.exists(out_dir)) stop("`out_dir` does not exist or is not a directory.", call. = FALSE)
  fields <- .read_diagnostic_fields(out_dir)
  metadata <- .read_diagnostic_metadata(out_dir)
  widgets <- .read_diagnostic_widgets(out_dir)
  if (nrow(fields) == 0L) stop("No canonical logical field table was found in `out_dir`.", call. = FALSE)

  fields$field_type <- sub("^/", "", fields$field_type)
  fields$states_normalized <- vapply(fields$states, state_set_to_string, character(1))
  audit_ids <- sort(unique(fields$audit_id))
  metadata <- .complete_diagnostic_metadata(metadata, fields, audit_ids)
  metadata$schema_group <- assign_schema_groups(metadata$form_schema_hash)
  fields <- dplyr::left_join(fields, metadata[, c("audit_id", "schema_group")], by = "audit_id")
  if (nrow(widgets) > 0L) {
    widgets$field_type <- sub("^/", "", widgets$field_type)
    widgets <- dplyr::left_join(widgets, metadata[, c("audit_id", "schema_group")], by = "audit_id")
  }

  schema_summary <- .schema_summary(fields, metadata)
  schema_groups <- .schema_groups(schema_summary)
  field_presence <- .field_presence(fields, audit_ids)
  field_presence_differences <- .field_presence_differences(field_presence)
  field_type_differences <- .field_type_differences(fields)
  field_state_differences <- .field_state_differences(fields)
  field_order_differences <- .field_order_differences(fields)
  pairwise <- .schema_pairwise(fields, audit_ids)
  widget_differences <- .widget_encoding_candidates(fields, widgets)

  result <- list(
    summary = schema_summary,
    schema_groups = schema_groups,
    pairwise = pairwise,
    field_presence = field_presence,
    field_presence_differences = field_presence_differences,
    field_type_differences = field_type_differences,
    field_state_differences = field_state_differences,
    field_order_differences = field_order_differences,
    widget_differences = widget_differences
  )
  if (isTRUE(write)) .write_diagnostics(result, out_dir)

  counts <- schema_difference_counts(result)
  if (!quiet) {
    cli::cli_inform(paste0(
      counts$schemas, " normalized PDF schema", if (counts$schemas == 1L) "" else "s",
      " detected across ", nrow(schema_summary), " audit", if (nrow(schema_summary) == 1L) "" else "s", "."
    ))
    if (counts$schemas > 1L) {
      cli::cli_alert_warning(paste0(
        "Differences: field presence = ", counts$presence,
        "; field types = ", counts$types,
        "; response-state sets = ", counts$states,
        "; field order = ", counts$order, "."
      ))
    }
    if (counts$widgets > 0L) cli::cli_alert_warning("Possible alternate button encoding detected.")
    if (isTRUE(write)) cli::cli_inform("See {fs::path(out_dir, 'diagnostics', 'schema_diagnostics.md')}.")
  }
  result
}

.read_diagnostic_fields <- function(out_dir) {
  combined <- fs::path(out_dir, "combined", "hpai_fields_long.csv")
  if (file.exists(combined)) return(readr::read_csv(combined, show_col_types = FALSE))
  audit_dirs <- list.dirs(fs::path(out_dir, "audits"), recursive = FALSE, full.names = TRUE)
  files <- unlist(lapply(audit_dirs, function(dir) {
    candidates <- list.files(dir, pattern = "_fields_long\\.csv$", full.names = TRUE)
    candidates[!grepl("_populated_fields_long\\.csv$", basename(candidates))]
  }), use.names = FALSE)
  if (length(files) == 0L) stop("The combined canonical fields file is unavailable and no per-audit fields_long files were found.", call. = FALSE)
  dplyr::bind_rows(purrr::map(files, ~ readr::read_csv(.x, show_col_types = FALSE)))
}

.read_diagnostic_metadata <- function(out_dir) {
  combined <- fs::path(out_dir, "combined", "hpai_metadata.csv")
  if (file.exists(combined)) return(readr::read_csv(combined, show_col_types = FALSE))
  audit_dirs <- list.dirs(fs::path(out_dir, "audits"), recursive = FALSE, full.names = TRUE)
  files <- unlist(lapply(audit_dirs, function(dir) list.files(dir, pattern = "_metadata\\.csv$", full.names = TRUE)), use.names = FALSE)
  if (length(files) == 0L) return(tibble::tibble())
  dplyr::bind_rows(purrr::map(files, ~ readr::read_csv(.x, show_col_types = FALSE)))
}

.read_diagnostic_widgets <- function(out_dir) {
  combined <- fs::path(out_dir, "combined", "hpai_widgets.csv")
  if (file.exists(combined)) return(readr::read_csv(combined, show_col_types = FALSE))
  audit_dirs <- list.dirs(fs::path(out_dir, "audits"), recursive = FALSE, full.names = TRUE)
  files <- unlist(lapply(audit_dirs, function(dir) list.files(dir, pattern = "_widgets\\.csv$", full.names = TRUE)), use.names = FALSE)
  if (length(files) == 0L) return(empty_widget_table())
  dplyr::bind_rows(purrr::map(files, ~ readr::read_csv(.x, show_col_types = FALSE)))
}

.complete_diagnostic_metadata <- function(metadata, fields, audit_ids) {
  fallback <- fields |>
    dplyr::group_by(audit_id) |>
    dplyr::summarise(
      source_file = dplyr::first(source_file),
      source_relpath = dplyr::first(source_relpath),
      number_of_fields = dplyr::n_distinct(field),
      number_of_populated_fields = sum(is_populated %in% TRUE),
      .groups = "drop"
    )
  if (nrow(metadata) > 0L) {
    metadata <- dplyr::right_join(metadata, fallback, by = "audit_id", suffix = c("", "_fallback"))
    for (column in c("source_file", "source_relpath", "number_of_fields", "number_of_populated_fields")) {
      fallback_column <- paste0(column, "_fallback")
      if (fallback_column %in% names(metadata)) {
        metadata[[column]] <- ifelse(is.na(metadata[[column]]), metadata[[fallback_column]], metadata[[column]])
        metadata[[fallback_column]] <- NULL
      }
    }
  } else {
    metadata <- fallback
  }
  if (!"form_schema_hash" %in% names(metadata)) metadata$form_schema_hash <- NA_character_
  if (anyNA(metadata$form_schema_hash)) {
    for (audit_id in metadata$audit_id[is.na(metadata$form_schema_hash)]) {
      audit_fields <- fields[fields$audit_id == audit_id, , drop = FALSE]
      metadata$form_schema_hash[metadata$audit_id == audit_id] <- tryCatch(
        schema_hash_from_fields(audit_fields),
        error = function(error) paste0("unavailable_", audit_id)
      )
    }
  }
  if (!"number_of_widgets" %in% names(metadata)) metadata$number_of_widgets <- NA_integer_
  if (!"schema_group" %in% names(metadata)) metadata$schema_group <- NA_character_
  metadata[match(audit_ids, metadata$audit_id), , drop = FALSE]
}

.schema_summary <- function(fields, metadata) {
  type_counts <- fields |>
    dplyr::group_by(audit_id) |>
    dplyr::summarise(number_of_field_types = dplyr::n_distinct(field_type), .groups = "drop")
  dplyr::left_join(metadata, type_counts, by = "audit_id") |>
    dplyr::select(audit_id, source_file, schema_group, form_schema_hash,
      number_of_fields, number_of_widgets, number_of_populated_fields,
      number_of_field_types)
}

.schema_groups <- function(summary) {
  summary |>
    dplyr::group_by(schema_group, form_schema_hash) |>
    dplyr::summarise(
      number_of_audits = dplyr::n(),
      audit_ids = paste(sort(audit_id), collapse = "|"),
      number_of_fields = dplyr::first(number_of_fields),
      number_of_field_types = dplyr::first(number_of_field_types),
      .groups = "drop"
    ) |>
    dplyr::arrange(schema_group)
}

.field_presence <- function(fields, audit_ids) {
  field_ids <- sort(unique(fields$field))
  purrr::map_dfr(field_ids, function(field) {
    present <- audit_ids %in% unique(fields$audit_id[fields$field == field])
    tibble::tibble(field = field, audit_id = audit_ids, present = present,
      schema_group = fields$schema_group[match(audit_ids, fields$audit_id)])
  })
}

.field_presence_differences <- function(presence) {
  presence |>
    dplyr::group_by(field) |>
    dplyr::summarise(
      n_audits_present = sum(present), n_audits_missing = sum(!present),
      present_in = paste(audit_id[present], collapse = "|"),
      missing_from = paste(audit_id[!present], collapse = "|"),
      severity = "WARNING",
      .groups = "drop"
    ) |>
    dplyr::filter(n_audits_missing > 0L, n_audits_present > 0L)
}

.field_type_differences <- function(fields) {
  typed <- dplyr::distinct(fields, audit_id, field, field_type, schema_group)
  variable <- typed |>
    dplyr::group_by(field) |>
    dplyr::summarise(n_types = dplyr::n_distinct(field_type), .groups = "drop") |>
    dplyr::filter(n_types > 1L)
  dplyr::inner_join(typed, variable, by = "field") |>
    dplyr::mutate(severity = "ERROR") |>
    dplyr::arrange(field, audit_id)
}

.field_state_differences <- function(fields) {
  states <- dplyr::distinct(fields, audit_id, field, field_type, states, states_normalized, schema_group)
  variable <- states |>
    dplyr::group_by(field, field_type) |>
    dplyr::summarise(n_state_sets = dplyr::n_distinct(states_normalized), .groups = "drop") |>
    dplyr::filter(n_state_sets > 1L)
  dplyr::inner_join(states, variable, by = c("field", "field_type")) |>
    dplyr::select(field, field_type, audit_id, schema_group, states_raw = states, states_normalized) |>
    dplyr::mutate(severity = "WARNING") |>
    dplyr::arrange(field, audit_id)
}

.field_order_differences <- function(fields) {
  variable <- fields |>
    dplyr::group_by(field) |>
    dplyr::summarise(n_indexes = dplyr::n_distinct(field_index), .groups = "drop") |>
    dplyr::filter(n_indexes > 1L)
  dplyr::inner_join(fields, variable, by = "field") |>
    dplyr::select(field, audit_id, schema_group, field_index, page) |>
    dplyr::mutate(severity = "INFO")
}

.schema_pairwise <- function(fields, audit_ids) {
  if (length(audit_ids) < 2L) return(tibble::tibble())
  pairs <- utils::combn(audit_ids, 2L, simplify = FALSE)
  collapse_values <- function(values) if (length(values) == 0L) NA_character_ else paste(sort(values), collapse = "|")
  purrr::map_dfr(pairs, function(pair) {
    a <- fields[fields$audit_id == pair[[1L]], , drop = FALSE]
    b <- fields[fields$audit_id == pair[[2L]], , drop = FALSE]
    common <- intersect(a$field, b$field)
    only_a <- setdiff(a$field, b$field)
    only_b <- setdiff(b$field, a$field)
    type_diff <- sum(a$field_type[match(common, a$field)] != b$field_type[match(common, b$field)], na.rm = TRUE)
    state_diff <- sum(a$states_normalized[match(common, a$field)] != b$states_normalized[match(common, b$field)], na.rm = TRUE)
    order_diff <- sum(a$field_index[match(common, a$field)] != b$field_index[match(common, b$field)], na.rm = TRUE)
    tibble::tibble(
      audit_a = pair[[1L]], audit_b = pair[[2L]],
      same_schema_hash = identical(unique(a$schema_group), unique(b$schema_group)),
      fields_a = dplyr::n_distinct(a$field), fields_b = dplyr::n_distinct(b$field),
      common_fields = length(common), fields_only_a = collapse_values(only_a),
      fields_only_b = collapse_values(only_b), field_type_differences = type_diff,
      state_set_differences = state_diff, field_order_differences = order_diff,
      field_name_jaccard = length(common) / length(union(a$field, b$field))
    )
  })
}

.widget_encoding_candidates <- function(fields, widgets) {
  button_fields <- dplyr::filter(fields, field_type == "Btn")
  if (nrow(button_fields) < 2L) return(tibble::tibble())
  button_fields <- dplyr::mutate(button_fields, suffix = sub("^[^_]+_[^_]+_", "", field))
  candidates <- list()
  for (i in seq_len(nrow(button_fields))) {
    for (j in seq_len(nrow(button_fields))) {
      if (i == j || button_fields$suffix[[i]] != button_fields$suffix[[j]]) next
      states_i <- normalize_state_set(button_fields$states[[i]])
      states_j <- normalize_state_set(button_fields$states[[j]])
      split_i <- "Off" %in% states_i && any(c("Yes", "No") %in% states_i)
      split_j <- "Off" %in% states_j && any(c("Yes", "No") %in% states_j)
      complete_i <- all(c("Yes", "No") %in% states_i)
      complete_j <- all(c("Yes", "No") %in% states_j)
      complementary_split <- split_i && split_j &&
        length(intersect(states_i, c("Yes", "No"))) > 0L &&
        length(intersect(states_j, c("Yes", "No"))) > 0L &&
        !identical(intersect(states_i, c("Yes", "No")), intersect(states_j, c("Yes", "No")))
      if (!((split_i && complete_j) || (split_j && complete_i) || complementary_split)) next
      first <- button_fields[i, , drop = FALSE]
      second <- button_fields[j, , drop = FALSE]
      page <- first$page
      geometry_note <- "Widget geometry unavailable."
      if (nrow(widgets) > 0L) {
        first_widget <- widgets[match(first$field, widgets$full_field_name), , drop = FALSE]
        second_widget <- widgets[match(second$field, widgets$full_field_name), , drop = FALSE]
        if (nrow(first_widget) > 0L && nrow(second_widget) > 0L && !is.na(first_widget$page[[1L]])) {
          page <- first_widget$page[[1L]]
          same_page <- identical(first_widget$page[[1L]], second_widget$page[[1L]])
          first_x <- mean(c(first_widget$rect_x1[[1L]], first_widget$rect_x2[[1L]]), na.rm = TRUE)
          second_x <- mean(c(second_widget$rect_x1[[1L]], second_widget$rect_x2[[1L]]), na.rm = TRUE)
          geometry_note <- if (same_page && is.finite(first_x) && is.finite(second_x) && abs(first_x - second_x) <= 150) "Widgets are on the same page and geographically nearby." else "Widgets did not meet the nearby-geometry heuristic."
        }
      }
      candidates[[length(candidates) + 1L]] <- tibble::tibble(
        candidate_id = paste(sort(c(first$field, second$field)), collapse = "__"),
        audit_id = first$audit_id, schema_group = first$schema_group, page = page,
        field = first$field, companion_field = second$field,
        field_states = first$states, companion_states = second$states,
        diagnostic_type = "possible_split_radio_encoding", confidence = "low",
        notes = paste("Possible alternate AcroForm encoding of a multi-widget button control.", geometry_note, "No automatic reconciliation was performed.")
      )
    }
  }
  if (length(candidates) == 0L) tibble::tibble() else dplyr::distinct(dplyr::bind_rows(candidates))
}

.write_diagnostics <- function(result, out_dir) {
  diagnostics_dir <- fs::path(out_dir, "diagnostics")
  dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv_utf8(result$summary, fs::path(diagnostics_dir, "schema_summary.csv"))
  write_csv_utf8(result$schema_groups, fs::path(diagnostics_dir, "schema_groups.csv"))
  write_csv_utf8(result$pairwise, fs::path(diagnostics_dir, "schema_pairwise_comparison.csv"))
  write_csv_utf8(result$field_presence_differences, fs::path(diagnostics_dir, "field_presence_differences.csv"))
  write_csv_utf8(result$field_type_differences, fs::path(diagnostics_dir, "field_type_differences.csv"))
  write_csv_utf8(result$field_state_differences, fs::path(diagnostics_dir, "field_state_differences.csv"))
  write_csv_utf8(result$field_order_differences, fs::path(diagnostics_dir, "field_order_differences.csv"))
  write_csv_utf8(result$widget_differences, fs::path(diagnostics_dir, "widget_encoding_differences.csv"))
  .write_diagnostic_markdown(result, fs::path(diagnostics_dir, "schema_diagnostics.md"))
}

.write_diagnostic_markdown <- function(result, path) {
  counts <- schema_difference_counts(result)
  lines <- c(
    "# HPAI PDF Schema Diagnostics", "", "## Summary", "",
    paste0("- PDFs examined: ", nrow(result$summary)),
    paste0("- Successfully extracted: ", nrow(result$summary)),
    paste0("- Normalized schemas: ", counts$schemas),
    paste0("- Field-presence differences: ", counts$presence),
    paste0("- Field-type differences: ", counts$types),
    paste0("- State-set differences: ", counts$states),
    paste0("- Field-order differences: ", counts$order), "", "## Schema groups", ""
  )
  for (i in seq_len(nrow(result$schema_groups))) {
    group <- result$schema_groups[i, ]
    lines <- c(lines, paste0("### ", group$schema_group), "",
      paste0("Audits: ", group$audit_ids), "",
      paste0("Logical fields: ", group$number_of_fields), "")
  }
  lines <- c(lines, "## Potentially consequential differences", "")
  if (nrow(result$field_presence_differences) == 0L && nrow(result$field_state_differences) == 0L) {
    lines <- c(lines, "No field-presence or normalized state-set differences were detected.", "")
  } else {
    for (i in seq_len(nrow(result$field_presence_differences))) {
      row <- result$field_presence_differences[i, ]
      lines <- c(lines, paste0("### ", row$field), "", paste0("Present in: ", row$present_in), "", paste0("Missing from: ", row$missing_from), "")
    }
    for (i in seq_len(nrow(result$field_state_differences))) {
      row <- result$field_state_differences[i, ]
      lines <- c(lines, paste0("### ", row$field), "", paste0("Normalized state set: ", row$states_normalized), "", paste0("Observed raw states: ", row$states_raw, " (", row$audit_id, ")"), "")
    }
  }
  if (nrow(result$widget_differences) > 0L) {
    lines <- c(lines, "A possible alternate multi-widget button encoding was detected involving:", "")
    lines <- c(lines, paste0("- ", unique(c(result$widget_differences$field, result$widget_differences$companion_field)), collapse = "\n"), "")
  }
  lines <- c(lines,
    "No automatic semantic reconciliation was performed.", "", "## Informational differences", "",
    "Field ordering differs across PDFs. Field order does not contribute to the normalized form schema hash.")
  writeLines(lines, path, useBytes = TRUE)
}
