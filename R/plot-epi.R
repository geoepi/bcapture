# Internal helpers and exported plots for de-identified Initial Epi summaries.

.epi_visualization_product_files <- c(
  overview = "dataset_overview.csv",
  field_inventory = "summary_field_inventory.csv",
  scalar_frequencies = "scalar_frequencies.csv",
  multiselect_frequencies = "multiselect_frequencies.csv",
  repeated_categorical_frequencies = "repeated_categorical_frequencies.csv",
  numeric_summaries = "numeric_summaries.csv",
  date_summaries = "date_summaries.csv",
  repeated_table_summaries = "repeated_table_summaries.csv",
  validation_rule_summary = "validation_rule_summary.csv",
  validation_status_summary = "validation_status_summary.csv",
  manifest = "summary_manifest.csv"
)

.epi_or <- function(x, y) if (is.null(x)) y else x

.epi_visualization_require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop(
    "ggplot2 is required for Initial Epi visualization functions.", call. = FALSE)
  invisible(TRUE)
}

.epi_visualization_missing <- function() {
  stop("Summary products are missing; run summarize_epi() first.", call. = FALSE)
}

.epi_visualization_read_products <- function(deidentified_dir) {
  if (!is.character(deidentified_dir) || length(deidentified_dir) != 1L ||
      is.na(deidentified_dir) || !nzchar(deidentified_dir)) stop(
    "deidentified_dir must be a single non-empty path.", call. = FALSE)
  summary_dir <- fs::path(fs::path_norm(deidentified_dir), "summary")
  if (!dir.exists(summary_dir)) .epi_visualization_missing()
  paths <- fs::path(summary_dir, unname(.epi_visualization_product_files))
  if (any(!file.exists(paths))) .epi_visualization_missing()
  products <- lapply(seq_along(paths), function(i) {
    if (!file.exists(paths[[i]])) return(NULL)
    readr::read_csv(paths[[i]], show_col_types = FALSE)
  })
  names(products) <- names(.epi_visualization_product_files)
  products
}

.epi_visualization_input <- function(x) {
  products <- if (is.character(x) && length(x) == 1L) {
    .epi_visualization_read_products(x)
  } else if (is.list(x)) {
    x
  } else {
    stop("x must be a summarize_epi() result or a deidentified_dir path.", call. = FALSE)
  }
  aliases <- list(
    overview = c("overview", "dataset_overview"),
    field_inventory = c("field_inventory"),
    scalar_frequencies = c("scalar_frequencies"),
    multiselect_frequencies = c("multiselect_frequencies"),
    repeated_categorical_frequencies = c("repeated_categorical_frequencies"),
    numeric_summaries = c("numeric_summaries"),
    date_summaries = c("date_summaries"),
    repeated_table_summaries = c("repeated_table_summaries"),
    validation_rule_summary = c("validation_rule_summary"),
    validation_status_summary = c("validation_status_summary"),
    manifest = c("manifest", "summary_manifest")
  )
  out <- lapply(aliases, function(candidates) {
    hit <- candidates[candidates %in% names(products)]
    if (length(hit) == 0L) NULL else products[[hit[[1L]]]]
  })
  names(out) <- names(aliases)
  if (is.null(out$manifest) || !is.data.frame(out$manifest) || nrow(out$manifest) != 1L) {
    stop("Summary manifest is missing or invalid.", call. = FALSE)
  }
  if (!"summary_schema_version" %in% names(out$manifest) ||
      as.integer(out$manifest$summary_schema_version[[1L]]) != 1L) {
    stop("Unsupported summary schema version; expected summary_schema_version == 1.",
      call. = FALSE)
  }
  status <- as.character(.epi_or(out$manifest$status[[1L]], NA_character_))
  if (is.na(status) || !status %in% c("passed", "review")) stop(
    "Summary manifest status must be passed or review before plotting.", call. = FALSE)
  out
}

.epi_visualization_require_product <- function(products, name, columns = character()) {
  data <- products[[name]]
  if (is.null(data) || !is.data.frame(data)) stop(
    "Summary product ", name, " is missing; run summarize_epi() first.", call. = FALSE)
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) stop(
    "Summary product ", name, " is missing required columns: ",
    paste(missing, collapse = ", "), call. = FALSE)
  data
}

.epi_display_label <- function(data, max_chars = 96L) {
  question <- if ("question_text" %in% names(data)) as.character(data$question_text) else
    rep(NA_character_, nrow(data))
  canonical <- if ("canonical_name" %in% names(data)) as.character(data$canonical_name) else
    if ("column_name" %in% names(data)) as.character(data$column_name) else
      if ("table_name" %in% names(data)) as.character(data$table_name) else rep("Variable", nrow(data))
  fallback <- gsub("_+", " ", canonical)
  fallback <- tools::toTitleCase(fallback)
  use_question <- !is.na(question) & nzchar(trimws(question)) & nchar(question) <= max_chars
  out <- fallback
  out[use_question] <- question[use_question]
  out[is.na(out) | !nzchar(trimws(out))] <- "Variable"
  out
}

.epi_wrap_labels <- function(x, width = 42L) {
  vapply(as.character(x), function(value) {
    paste(strwrap(value, width = width), collapse = "\n")
  }, character(1))
}

.epi_resolve_variable <- function(data, variable, context = "summary variable") {
  if (is.null(variable) || length(variable) != 1L || is.na(variable) || !nzchar(variable)) {
    stop("A single ", context, " must be supplied.", call. = FALSE)
  }
  variable <- as.character(variable)
  canonical_hits <- if ("canonical_name" %in% names(data))
    which(as.character(data$canonical_name) == variable) else integer()
  if (length(canonical_hits) > 0L) return(canonical_hits)
  question_hits <- if ("question_id" %in% names(data))
    which(as.character(data$question_id) == variable) else integer()
  if (length(question_hits) > 1L) {
    candidates <- if ("canonical_name" %in% names(data))
      unique(as.character(data$canonical_name[question_hits])) else as.character(question_hits)
    if (length(candidates) == 1L) return(question_hits)
    stop("Ambiguous ", context, " '", variable, "'. Candidates: ",
      paste(candidates, collapse = ", "), call. = FALSE)
  }
  if (length(question_hits) == 1L) return(question_hits)
  stop("No ", context, " matched '", variable, "'.", call. = FALSE)
}

.epi_resolve_section <- function(data, section) {
  if (is.null(section)) return(seq_len(nrow(data)))
  section <- as.character(section[[1L]])
  hits <- integer()
  if ("section_id" %in% names(data)) hits <- which(as.character(data$section_id) == section)
  if (length(hits) == 0L && "section_name" %in% names(data))
    hits <- which(as.character(data$section_name) == section)
  if (length(hits) == 0L) stop("No summary section matched '", section, "'.", call. = FALSE)
  hits
}

.epi_empty_plot <- function(title, message, x = NULL) {
  .epi_visualization_require_ggplot2()
  data <- tibble::tibble(label = message, value = 0)
  ggplot2::ggplot(data, ggplot2::aes(x = label, y = value)) +
    ggplot2::geom_blank() +
    ggplot2::annotate("text", x = 1, y = 0, label = message, size = 4) +
    ggplot2::labs(title = title, x = .epi_or(x, NULL), y = NULL) +
    .theme_bcapture() + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
}

.theme_bcapture <- function() {
  .epi_visualization_require_ggplot2()
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0, colour = "#52616b"),
      axis.text.y = ggplot2::element_text(lineheight = 0.9)
    )
}

.epi_caption_counts <- function(data, show_counts = TRUE, show_missing = TRUE) {
  parts <- character()
  if (isTRUE(show_counts) && all(c("n_answered", "n_cases_total") %in% names(data))) {
    parts <- c(parts, paste0("Answered: ", data$n_answered[[1L]], " of ",
      data$n_cases_total[[1L]], " cases"))
  }
  if (isTRUE(show_missing) && "n_missing" %in% names(data))
    parts <- c(parts, paste0("missing: ", data$n_missing[[1L]]))
  paste(parts, collapse = "; ")
}

#' Plot scalar categorical Initial Epi responses
#'
#' @param x A summarize_epi() result or a deidentified directory containing
#'   summary products.
#' @param variable Canonical variable name or unique question ID.
#' @param denominator Use the supplied answered-case or all-case percentage.
#' @param show_counts Include supplied answered and total case counts.
#' @param show_missing Include the supplied missing count.
#' @param title Optional plot title. Defaults to the question label.
#' @return A ggplot object. No files are created.
#' @export
plot_epi_categorical <- function(x, variable,
                                 denominator = c("answered", "all_cases"),
                                 show_counts = TRUE, show_missing = TRUE,
                                 title = NULL) {
  .epi_visualization_require_ggplot2()
  denominator <- match.arg(denominator)
  products <- .epi_visualization_input(x)
  data <- .epi_visualization_require_product(products, "scalar_frequencies",
    c("canonical_name", "question_id", "question_text", "response_label", "response_order",
      "n_cases_total", "n_answered", "n_missing", "n_response", "percent_of_answered"))
  i <- .epi_resolve_variable(data, variable, "categorical variable")
  data <- data[i, , drop = FALSE]
  label <- .epi_display_label(data)[[1L]]
  if (all(is.na(data$n_answered) | data$n_answered == 0L))
    return(.epi_empty_plot(.epi_or(title, label), "No responses recorded", "Response"))
  value_col <- if (denominator == "answered") "percent_of_answered" else "percent_of_all_cases"
  if (!value_col %in% names(data)) stop(
    "Summary product scalar_frequencies is missing required column: ", value_col, call. = FALSE)
  response <- ifelse(is.na(data$response_label), "(Missing label)", as.character(data$response_label))
  order <- if ("response_order" %in% names(data)) data$response_order else seq_along(response)
  order <- order[order %in% seq_along(response)]
  if (length(order) != length(response)) order <- seq_along(response)
  levels <- response[order]
  data$display_response <- factor(response, levels = rev(levels))
  data$display_percent <- as.numeric(data[[value_col]])
  caption <- .epi_caption_counts(data, show_counts, show_missing)
  ggplot2::ggplot(data, ggplot2::aes(x = display_response, y = display_percent)) +
    ggplot2::geom_col(fill = "#2c6e8f", show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(labels = function(x) .epi_wrap_labels(x)) +
    ggplot2::labs(title = .epi_or(title, label), x = NULL, y = if (denominator == "answered")
      "Percent of answered cases" else "Percent of all cases", caption = caption) +
    .theme_bcapture()
}

#' Plot Initial Epi multiselect responses
#'
#' @param x A summarize_epi() result or a deidentified directory containing
#'   summary products.
#' @param variable Canonical variable name or unique question ID.
#' @param denominator Use the supplied all-case or selecting-case percentage.
#' @param show_counts Include supplied case counts in the caption.
#' @param title Optional plot title. Defaults to the question label.
#' @return A ggplot object. No files are created.
#' @export
plot_epi_multiselect <- function(x, variable,
                                 denominator = c("all_cases", "selecting_cases"),
                                 show_counts = TRUE, title = NULL) {
  .epi_visualization_require_ggplot2()
  denominator <- match.arg(denominator)
  products <- .epi_visualization_input(x)
  data <- .epi_visualization_require_product(products, "multiselect_frequencies",
    c("question_id", "question_text", "item_label", "n_cases_total",
      "n_cases_with_any_selection", "n_cases_selecting_item",
      "percent_of_selecting_cases", "percent_of_all_cases"))
  i <- .epi_resolve_variable(data, variable, "multiselect variable")
  data <- data[i, , drop = FALSE]
  label <- .epi_display_label(data)[[1L]]
  value_col <- if (denominator == "all_cases") "percent_of_all_cases" else
    "percent_of_selecting_cases"
  data$display_percent <- as.numeric(data[[value_col]])
  data$display_item <- factor(as.character(data$item_label),
    levels = rev(unique(as.character(data$item_label))))
  caption <- if (isTRUE(show_counts)) paste0(
    "Cases with any selection: ", data$n_cases_with_any_selection[[1L]], " of ",
    data$n_cases_total[[1L]], ". Percentages within a multiselect question need not sum to 100%.") else
    "Percentages within a multiselect question need not sum to 100%."
  if (nrow(data) == 0L || all(is.na(data$display_percent)))
    return(.epi_empty_plot(.epi_or(title, label), "No responses recorded", "Selected item"))
  ggplot2::ggplot(data, ggplot2::aes(x = display_item, y = display_percent)) +
    ggplot2::geom_col(fill = "#6b8e9e", show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(labels = function(x) .epi_wrap_labels(x)) +
    ggplot2::labs(title = .epi_or(title, label), x = "Selected item", y = if (denominator == "all_cases")
      "Percent of all cases" else "Percent of selecting cases", caption = caption) +
    .theme_bcapture()
}

#' Plot aggregate numeric Initial Epi summaries
#'
#' @param x A summarize_epi() result or a deidentified directory containing
#'   summary products.
#' @param variable Canonical variable name or unique question ID. Required
#'   when more than one numeric variable is available.
#' @param section Optional exact section ID or section name filter.
#' @param show_range Display the supplied minimum-to-maximum interval.
#' @param title Optional plot title.
#' @return A ggplot object. No raw values or files are created.
#' @export
plot_epi_numeric <- function(x, variable = NULL, section = NULL,
                             show_range = TRUE, title = NULL) {
  .epi_visualization_require_ggplot2()
  products <- .epi_visualization_input(x)
  data <- .epi_visualization_require_product(products, "numeric_summaries",
    c("canonical_name", "question_id", "units", "n_parsed", "n_unparsed", "n_missing",
      "median", "q25", "q75", "min", "max"))
  data <- data[.epi_resolve_section(data, section), , drop = FALSE]
  if (!is.null(variable)) data <- data[.epi_resolve_variable(data, variable, "numeric variable"), , drop = FALSE]
  if (nrow(data) == 0L) return(.epi_empty_plot(.epi_or(title, "Numeric summaries"), "No numeric summaries recorded"))
  if (is.null(variable) && nrow(data) > 1L) stop(
    "Multiple numeric variables matched; supply variable for a single plot.", call. = FALSE)
  if (all(is.na(data$n_parsed) | data$n_parsed == 0L))
    return(.epi_empty_plot(.epi_or(title, .epi_display_label(data)[[1L]]),
      "No parsed numeric responses", "Variable"))
  data$display_label <- .epi_display_label(data)
  data$display_units <- as.character(.epi_or(data$units, ""))
  data$display_label <- ifelse(nzchar(data$display_units),
    paste0(data$display_label, " (", data$display_units, ")"), data$display_label)
  caption <- paste0("Parsed: ", data$n_parsed[[1L]], "; unparsed: ",
    data$n_unparsed[[1L]], "; missing: ", data$n_missing[[1L]])
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = display_label, y = median))
  if (isTRUE(show_range)) plot <- plot + ggplot2::geom_linerange(
    ggplot2::aes(ymin = min, ymax = max), colour = "#9aaab2", linewidth = 1)
  plot + ggplot2::geom_errorbar(ggplot2::aes(ymin = q25, ymax = q75),
    width = .2, colour = "#2c6e8f", linewidth = 2) +
    ggplot2::geom_point(size = 2.5, colour = "#24313a") + ggplot2::coord_flip() +
    ggplot2::labs(title = .epi_or(title, "Numeric summary"), x = NULL, y = "Value", caption = caption) +
    .theme_bcapture()
}

#' Plot repeated-table categorical responses or record counts
#'
#' @param x A summarize_epi() result or a deidentified directory containing
#'   summary products.
#' @param table Optional exact repeated table name.
#' @param variable Optional canonical name, column name, or unique question ID
#'   for categorical mode.
#' @param type Either "categorical" or "counts".
#' @param title Optional plot title.
#' @return A ggplot object with record-level semantics where applicable.
#' @export
plot_epi_repeated <- function(x, table = NULL, variable = NULL,
                              type = c("categorical", "counts"), title = NULL) {
  .epi_visualization_require_ggplot2()
  type <- match.arg(type)
  products <- .epi_visualization_input(x)
  if (type == "categorical") {
    data <- .epi_visualization_require_product(products, "repeated_categorical_frequencies",
      c("table_name", "column_name", "question_id", "question_text", "response_label",
        "n_records_total", "n_nonmissing", "percent_of_nonmissing"))
    if (!is.null(table)) data <- data[as.character(data$table_name) == as.character(table), , drop = FALSE]
    if (!is.null(table) && nrow(data) == 0L) stop("No repeated table matched '", table, "'.", call. = FALSE)
    if (!is.null(variable)) {
      if ("canonical_name" %in% names(data)) i <- .epi_resolve_variable(data, variable, "repeated categorical variable") else {
        hits <- which(as.character(data$column_name) == as.character(variable))
        if (length(hits) != 1L) stop("No unique repeated categorical variable matched '", variable, "'.", call. = FALSE)
        i <- hits
      }
      data <- data[i, , drop = FALSE]
    }
    if (nrow(data) == 0L) return(.epi_empty_plot(.epi_or(title, "Repeated categorical responses"), "No responses recorded", "Response"))
    if (is.null(variable) && length(unique(data$question_id)) > 1L) stop(
      "Multiple repeated categorical variables matched; supply variable.", call. = FALSE)
    label <- .epi_display_label(data)[[1L]]
    data$display_response <- factor(as.character(data$response_label),
      levels = rev(unique(as.character(data$response_label))))
    data$display_percent <- as.numeric(data$percent_of_nonmissing)
    caption <- paste0("Percent among records with a nonmissing response. Records with a nonmissing response: ",
      data$n_nonmissing[[1L]], " of ", data$n_records_total[[1L]], ".")
    return(ggplot2::ggplot(data, ggplot2::aes(x = display_response, y = display_percent)) +
      ggplot2::geom_col(fill = "#557d8b", show.legend = FALSE) + ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(labels = function(x) .epi_wrap_labels(x)) +
      ggplot2::labs(title = .epi_or(title, label), x = "Response (records)",
        y = "Percent among records with a nonmissing response", caption = caption) +
      .theme_bcapture())
  }
  data <- .epi_visualization_require_product(products, "repeated_table_summaries",
    c("table_name", "median_records_per_case", "max_records_per_case"))
  if (!is.null(table)) data <- data[as.character(data$table_name) == as.character(table), , drop = FALSE]
  if (nrow(data) == 0L) return(.epi_empty_plot(.epi_or(title, "Repeated-table counts"), "No repeated tables recorded", "Table"))
  data$display_table <- factor(gsub("_+", " ", as.character(data$table_name)),
    levels = rev(gsub("_+", " ", as.character(data$table_name))))
  ggplot2::ggplot(data, ggplot2::aes(x = display_table, y = median_records_per_case)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = median_records_per_case,
      ymax = max_records_per_case), colour = "#9aaab2", linewidth = 1) +
    ggplot2::geom_point(size = 2.5, colour = "#2c6e8f") + ggplot2::coord_flip() +
    ggplot2::labs(title = .epi_or(title, "Repeated-table counts"), x = NULL,
      y = "Median records per case", caption = "Point and upper interval use supplied aggregate counts.") +
    .theme_bcapture()
}

#' Plot Initial Epi validation status or validation-rule findings
#'
#' @param x A summarize_epi() result or a deidentified directory containing
#'   summary products.
#' @param type Either "status" or "rules".
#' @param title Optional plot title.
#' @return A ggplot object. Validation counts are taken from summary products.
#' @export
plot_epi_validation <- function(x, type = c("status", "rules"), title = NULL) {
  .epi_visualization_require_ggplot2()
  type <- match.arg(type)
  products <- .epi_visualization_input(x)
  if (type == "status") {
    data <- .epi_visualization_require_product(products, "validation_status_summary",
      c("validation_status", "n_cases", "percent_of_cases"))
    statuses <- c("valid", "review", "error")
    data$validation_status <- factor(tolower(as.character(data$validation_status)), levels = statuses)
    data <- data[!is.na(data$validation_status), , drop = FALSE]
    if (nrow(data) == 0L) return(.epi_empty_plot(.epi_or(title, "Validation status"), "No validation statuses recorded", "Status"))
    return(ggplot2::ggplot(data, ggplot2::aes(x = validation_status, y = n_cases, fill = validation_status)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = c(valid = "#3b7d5a", review = "#9a6b20", error = "#a94a4a"), drop = FALSE) +
      ggplot2::labs(title = .epi_or(title, "Validation status"), x = "Status", y = "Cases",
        caption = "Case counts and percentages are supplied by validation_status_summary.csv.") +
      .theme_bcapture())
  }
  data <- .epi_visualization_require_product(products, "validation_rule_summary",
    c("severity", "validation_type", "rule_id", "n_findings", "n_cases"))
  if (nrow(data) == 0L) return(.epi_empty_plot(.epi_or(title, "Validation findings"), "No validation findings recorded", "Rule"))
  severity_order <- c("ERROR", "WARNING", "INFO")
  data$severity <- toupper(as.character(data$severity))
  data$severity <- factor(data$severity, levels = severity_order)
  data <- data[order(data$severity, -as.numeric(data$n_findings), na.last = TRUE), , drop = FALSE]
  data$display_rule <- .epi_wrap_labels(as.character(data$rule_id), 48L)
  ggplot2::ggplot(data, ggplot2::aes(x = display_rule, y = n_findings, fill = severity)) +
    ggplot2::geom_col(show.legend = TRUE) + ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c(ERROR = "#a94a4a", WARNING = "#9a6b20", INFO = "#557d8b"),
      drop = FALSE) + ggplot2::labs(title = .epi_or(title, "Validation findings"), x = "Rule",
      y = "Findings", fill = "Severity", caption = "Findings and affected case counts are supplied by validation_rule_summary.csv.") +
    .theme_bcapture()
}
