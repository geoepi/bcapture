.epi_report_required_products <- function() {
  c(
    dataset_overview = "dataset_overview.csv",
    field_inventory = "summary_field_inventory.csv",
    scalar_frequencies = "scalar_frequencies.csv",
    multiselect_frequencies = "multiselect_frequencies.csv",
    repeated_categorical_frequencies = "repeated_categorical_frequencies.csv",
    numeric_summaries = "numeric_summaries.csv",
    date_summaries = "date_summaries.csv",
    case_table_counts = "case_table_counts.csv",
    repeated_table_summaries = "repeated_table_summaries.csv",
    validation_rule_summary = "validation_rule_summary.csv",
    validation_status_summary = "validation_status_summary.csv",
    summary_manifest = "summary_manifest.csv"
  )
}

.epi_report_read_csv <- function(path, label) {
  if (!file.exists(path)) stop(
    "Summary products are missing; run summarize_epi() first.", call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE)
}

.epi_report_require_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) stop(
    "Summary product ", label, " is missing required columns: ",
    paste(missing, collapse = ", "), call. = FALSE)
  invisible(data)
}

.epi_report_read_products <- function(deidentified_dir, report) {
  if (identical(report, "features")) {
    summary_dir <- fs::path(deidentified_dir, "features", "summary")
    required <- .epi_feature_summary_files
    if (!dir.exists(summary_dir) || any(!file.exists(fs::path(summary_dir, required)))) {
      stop("Feature summary products are missing; run summarize_epi_features() first.",
           call. = FALSE)
    }
    products <- purrr::map(required, ~ .epi_report_read_csv(
      fs::path(summary_dir, .x), .x))
    names(products) <- names(required)
    manifest <- products$manifest
    .epi_report_require_columns(manifest, c(
      "feature_summary_schema_version", "form_version", "profile", "status",
      "n_cases", "package_version", "created_at"
    ), "feature_summary_manifest.csv")
    if (nrow(manifest) != 1L ||
        as.integer(manifest$feature_summary_schema_version[[1L]]) != 1L) stop(
      "Unsupported feature summary schema version; expected feature_summary_schema_version == 1.",
      call. = FALSE
    )
    if (!as.character(manifest$status[[1L]]) %in% c("passed", "review")) stop(
      "Feature summary manifest status must be passed or review before rendering.",
      call. = FALSE
    )
    dictionary_path <- fs::path(deidentified_dir, "features", "feature_dictionary.csv")
    long_path <- fs::path(deidentified_dir, "features", "feature_long.csv")
    if (!file.exists(dictionary_path) || !file.exists(long_path)) stop(
      "Feature products are missing; run derive_epi_features() first.", call. = FALSE
    )
    dictionary <- .epi_report_read_csv(dictionary_path, "feature_dictionary.csv")
    long <- .epi_report_read_csv(long_path, "feature_long.csv")
    .epi_report_require_columns(dictionary, c(
      "feature_order", "feature_name", "feature_label", "feature_type",
      "domain_id", "domain_label", "source_question_id", "source_table", "description"
    ), "feature_dictionary.csv")
    .epi_report_require_columns(long, c(
      "feature_name", "feature_label", "feature_type", "domain_id", "domain_label",
      "value_status", "numeric_value"
    ), "feature_long.csv")
    distribution_data <- long[
      as.character(long$feature_type) %in% c("numeric", "count") &
        as.character(long$value_status) == "known" & !is.na(long$numeric_value),
      c("feature_name", "feature_label", "feature_type", "domain_id",
        "domain_label", "numeric_value"), drop = FALSE
    ]
    products$feature_summary_manifest <- products$manifest
    products$manifest <- NULL
    products$feature_dictionary <- dictionary[order(
      as.integer(dictionary$feature_order)), , drop = FALSE]
    products$distribution_data <- tibble::as_tibble(distribution_data)
    return(list(products = products, privacy = list(
      manifest = NULL, review = NULL, leak_audit = NULL
    )))
  }
  summary_dir <- fs::path(deidentified_dir, "summary")
  required <- .epi_report_required_products()
  if (!dir.exists(summary_dir) || any(!file.exists(fs::path(summary_dir, required)))) {
    stop("Summary products are missing; run summarize_epi() first.", call. = FALSE)
  }
  products <- purrr::map(required, ~ .epi_report_read_csv(
    fs::path(summary_dir, .x), .x))
  names(products) <- names(required)
  manifest <- products$summary_manifest
  .epi_report_require_columns(manifest,
    c("summary_schema_version", "form_version", "profile", "status", "n_cases",
      "package_version", "created_at"), "summary_manifest.csv")
  if (nrow(manifest) != 1L || as.integer(manifest$summary_schema_version[[1L]]) != 1L) {
    stop("Unsupported summary schema version; expected summary_schema_version == 1.",
      call. = FALSE)
  }
  summary_status <- as.character(manifest$status[[1L]])
  if (is.na(summary_status) || !summary_status %in% c("passed", "review")) {
    stop("Summary manifest status must be passed or review before rendering.", call. = FALSE)
  }
  .epi_report_require_columns(products$dataset_overview,
    c("form_version", "profile", "n_cases", "n_counties", "first_interview_date",
      "last_interview_date", "n_validation_valid", "n_validation_review",
      "n_validation_error", "n_repeated_tables", "n_repeated_records"),
    "dataset_overview.csv")
  .epi_report_require_columns(products$validation_status_summary,
    c("validation_status", "n_cases", "percent_of_cases"),
    "validation_status_summary.csv")
  .epi_report_require_columns(products$scalar_frequencies,
    c("section_id", "section_name", "question_id", "question_text", "canonical_name",
      "response_label", "response_order", "n_cases_total", "n_answered", "n_missing",
      "n_response", "percent_of_answered"), "scalar_frequencies.csv")
  .epi_report_require_columns(products$multiselect_frequencies,
    c("section_id", "section_name", "question_id", "question_text", "item_label",
      "n_cases_total", "n_cases_with_any_selection", "n_cases_selecting_item",
      "percent_of_selecting_cases", "percent_of_all_cases"),
    "multiselect_frequencies.csv")
  .epi_report_require_columns(products$repeated_categorical_frequencies,
    c("section_id", "section_name", "question_id", "question_text", "canonical_name",
      "table_name", "column_name", "response_label", "n_records_total", "n_nonmissing",
      "n_value", "percent_of_nonmissing"), "repeated_categorical_frequencies.csv")
  .epi_report_require_columns(products$numeric_summaries,
    c("section_id", "section_name", "question_id", "canonical_name", "units",
      "n_total", "n_populated", "n_parsed", "n_unparsed", "n_missing", "mean", "sd",
      "median", "q25", "q75", "min", "max"), "numeric_summaries.csv")
  .epi_report_require_columns(products$date_summaries,
    c("section_id", "section_name", "question_id", "canonical_name", "n_total",
      "n_populated", "n_parsed", "n_unparsed", "n_missing", "min_date", "median_date",
      "max_date"), "date_summaries.csv")
  .epi_report_require_columns(products$repeated_table_summaries,
    c("table_name", "n_cases_total", "n_cases_with_records", "n_records_total",
      "median_records_per_case", "max_records_per_case"),
    "repeated_table_summaries.csv")
  .epi_report_require_columns(products$validation_rule_summary,
    c("severity", "validation_type", "rule_id", "n_findings", "n_cases"),
    "validation_rule_summary.csv")
  products <- products[setdiff(names(products), "case_table_counts")]

  privacy <- list(manifest = NULL, review = NULL, leak_audit = NULL)
  if (identical(report, "quality")) {
    privacy_dir <- fs::path(deidentified_dir, "privacy")
    manifest_path <- fs::path(privacy_dir, "deidentification_manifest.csv")
    if (file.exists(manifest_path)) privacy$manifest <- readr::read_csv(
      manifest_path, show_col_types = FALSE)
    review_path <- fs::path(privacy_dir, "deidentification_review.csv")
    if (file.exists(review_path)) privacy$review <- readr::read_csv(
      review_path, show_col_types = FALSE)
    audit_path <- fs::path(privacy_dir, "privacy_leak_audit.csv")
    if (file.exists(audit_path)) privacy$leak_audit <- readr::read_csv(
      audit_path, show_col_types = FALSE)
  }
  list(products = products, privacy = privacy)
}

.epi_report_safe_privacy <- function(privacy) {
  manifest <- privacy$manifest
  review <- privacy$review
  audit <- privacy$leak_audit
  manifest_summary <- if (is.null(manifest) || nrow(manifest) == 0L) {
    tibble::tibble()
  } else {
    first <- manifest[1L, , drop = FALSE]
    tibble::tibble(
      status = as.character(first$status[[1L]]),
      privacy_warnings = as.integer(first$privacy_warnings[[1L]]),
      privacy_errors = as.integer(first$privacy_errors[[1L]]),
      free_text_fields_withheld = as.integer(first$free_text_fields_withheld[[1L]])
    )
  }
  review_summary <- if (is.null(review) || nrow(review) == 0L) {
    tibble::tibble(n_review_rows = 0L, n_cases_affected = 0L, n_fields_affected = 0L)
  } else {
    tibble::tibble(
      n_review_rows = nrow(review),
      n_cases_affected = if ("case_id" %in% names(review))
        dplyr::n_distinct(review$case_id) else NA_integer_,
      n_fields_affected = if ("canonical_name" %in% names(review))
        dplyr::n_distinct(review$canonical_name) else NA_integer_
    )
  }
  leak_summary <- if (is.null(audit) || nrow(audit) == 0L) {
    tibble::tibble(n_audit_rows = 0L, n_errors = 0L, n_warnings = 0L, status = "No identifier leaks detected")
  } else {
    severity <- toupper(as.character(audit$severity %||% NA_character_))
    tibble::tibble(
      n_audit_rows = nrow(audit),
      n_errors = sum(severity %in% c("ERROR", "CONFIRMED_ERROR"), na.rm = TRUE),
      n_warnings = sum(severity %in% c("WARNING", "REVIEW"), na.rm = TRUE),
      status = if (any(severity %in% c("ERROR", "CONFIRMED_ERROR"), na.rm = TRUE))
        "Identifier leak findings require review" else "No identifier leaks detected"
    )
  }
  list(manifest = manifest_summary, review = review_summary, leak_audit = leak_summary)
}

.epi_report_quarto_args <- function(template, output_dir, output_file, params_file) {
  template <- normalizePath(template, winslash = "/", mustWork = FALSE)
  params_file <- normalizePath(params_file, winslash = "/", mustWork = FALSE)
  c(
    "render", template, "--to", "html", "--output", output_file,
    "--execute-params", params_file
  )
}

.epi_report_quarto_env <- function() {
  r_exe <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  paste0("QUARTO_R=", normalizePath(r_exe, winslash = "/", mustWork = FALSE))
}

.epi_report_yaml_value <- function(value) {
  value <- gsub("\\\\", "/", as.character(value), fixed = FALSE)
  value <- gsub('"', '\\\"', value, fixed = TRUE)
  paste0('"', value, '"')
}

.epi_report_quarto_version <- function(quarto) {
  output <- tempfile("bcapture-quarto-version-")
  error <- tempfile("bcapture-quarto-version-error-")
  on.exit(unlink(c(output, error), force = TRUE), add = TRUE)
  status <- system2(quarto, "--version", stdout = output, stderr = error)
  if (!identical(as.integer(status), 0L)) return(NA_character_)
  trimws(paste(readLines(output, warn = FALSE), collapse = " "))
}

.epi_report_find_quarto <- function(quarto = Sys.which("quarto")) {
  if (!nzchar(quarto)) stop(
    "Quarto is required to render bcapture HTML reports but was not found on PATH.",
    call. = FALSE)
  quarto
}

.epi_report_run_quarto <- function(quarto, args, stdout, stderr, cache_dir = NULL) {
  previous <- Sys.getenv("QUARTO_R", unset = NA_character_)
  previous_deno <- Sys.getenv("DENO_DIR", unset = NA_character_)
  previous_localappdata <- Sys.getenv("LOCALAPPDATA", unset = NA_character_)
  Sys.setenv(QUARTO_R = sub("^QUARTO_R=", "", .epi_report_quarto_env()))
  if (!is.null(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    Sys.setenv(DENO_DIR = normalizePath(cache_dir, winslash = "/", mustWork = FALSE))
    Sys.setenv(LOCALAPPDATA = normalizePath(cache_dir, winslash = "/", mustWork = FALSE))
  }
  on.exit(if (is.na(previous)) Sys.unsetenv("QUARTO_R") else
    Sys.setenv(QUARTO_R = previous), add = TRUE)
  on.exit(if (is.na(previous_deno)) Sys.unsetenv("DENO_DIR") else
    Sys.setenv(DENO_DIR = previous_deno), add = TRUE)
  on.exit(if (is.na(previous_localappdata)) Sys.unsetenv("LOCALAPPDATA") else
    Sys.setenv(LOCALAPPDATA = previous_localappdata), add = TRUE)
  system2(quarto, args = args, stdout = stdout, stderr = stderr,
    env = character())
}

.epi_report_path_equal <- function(x, y) {
  normalize <- function(path) {
    path <- fs::path_norm(path)
    if (file.exists(path) || dir.exists(path)) fs::path_real(path) else path
  }
  identical(normalize(x), normalize(y))
}

.epi_report_validate_output <- function(deidentified_dir, output_dir, output_file) {
  protected <- c("collated", "validation", "privacy", "summary", "features")
  protected_paths <- fs::path(deidentified_dir, protected)
  if (any(vapply(protected_paths, .epi_report_path_equal, logical(1), output_dir))) stop(
    "Report output_dir cannot be collated/, validation/, privacy/, summary/, or features/.",
    call. = FALSE)
  if (any(vapply(protected_paths, function(path) .epi_report_path_equal(
    path, fs::path(output_dir, output_file)), logical(1)))) stop(
    "Report output cannot overwrite an analytical product.", call. = FALSE)
  invisible(TRUE)
}

.epi_report_manifest_row <- function(report, output_file, manifest, quarto_version) {
  schema_version <- if (identical(report, "features"))
    manifest$feature_summary_schema_version[[1L]] else
    manifest$summary_schema_version[[1L]]
  tibble::tibble(
    report_schema_version = 1L,
    report_type = report,
    report_file = fs::path_file(output_file),
    form_version = as.character(manifest$form_version[[1L]]),
    profile = as.character(manifest$profile[[1L]]),
    summary_schema_version = as.integer(schema_version),
    summary_status = as.character(manifest$status[[1L]]),
    n_cases = as.integer(manifest$n_cases[[1L]]),
    bcapture_version = as.character(manifest$package_version[[1L]]),
    quarto_version = quarto_version,
    created_at = utc_now()
  )
}

.epi_report_write_manifest <- function(path, row) {
  row <- tibble::as_tibble(lapply(row, as.character))
  existing <- if (file.exists(path)) readr::read_csv(path,
    col_types = readr::cols(.default = readr::col_character())) else
    tibble::tibble()
  if (nrow(existing) > 0L && "report_file" %in% names(existing)) {
    existing <- existing[existing$report_file != row$report_file[[1L]], , drop = FALSE]
  }
  out <- dplyr::bind_rows(existing, row)
  out <- out[order(as.character(out$report_file)), , drop = FALSE]
  stage <- paste0(path, ".tmp-", paste(sample(c(letters, 0:9), 10L, replace = TRUE), collapse = ""))
  on.exit(unlink(stage, force = TRUE), add = TRUE)
  write_csv_utf8(out, stage)
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(stage, path)) stop("Unable to finalize report_manifest.csv.", call. = FALSE)
  invisible(path)
}

#' Render a self-contained Initial Epi HTML report
#'
#' Reports consume the immutable products created by [summarize_epi()] or, for
#' the feature report, [summarize_epi_features()]. They never invoke either
#' summary function, read a re-identification crosswalk, or access identifiable
#' source products.
#'
#' @param deidentified_dir De-identified Initial Epi output containing summary/.
#' @param report Report type: "summary", "quality", or "features".
#' @param output_dir Directory for human-readable reports. Defaults to reports/.
#' @param output_file Optional HTML basename. Defaults by report type.
#' @param overwrite Replace the requested existing report when TRUE.
#' @param quiet Suppress the concise success message.
#' @return A small list containing report status, output file, summary status,
#'   case count, and Quarto version.
#' @export
render_epi_report <- function(deidentified_dir,
                              report = c("summary", "quality", "features"),
                              output_dir = NULL, output_file = NULL,
                              overwrite = FALSE, quiet = FALSE) {
  deidentified_dir <- validate_scalar_path(deidentified_dir, "deidentified_dir")
  report <- match.arg(report, c("summary", "quality", "features"))
  if (!is.null(output_dir)) output_dir <- validate_scalar_path(output_dir, "output_dir")
  output_dir <- output_dir %||% fs::path(deidentified_dir, "reports")
  output_dir <- fs::path_norm(output_dir)
  default_file <- switch(report,
    summary = "initial_epi_summary.html",
    quality = "initial_epi_quality.html",
    features = "initial_epi_features.html"
  )
  if (is.null(output_file)) output_file <- default_file
  output_file <- validate_scalar_path(output_file, "output_file")
  if (!identical(fs::path_file(output_file), output_file) ||
      !grepl("\\.html$", output_file, ignore.case = TRUE)) stop(
    "output_file must be an .html basename below output_dir.", call. = FALSE)
  .epi_report_validate_output(deidentified_dir, output_dir, output_file)
  target <- fs::path(output_dir, output_file)
  if (file.exists(target) && !isTRUE(overwrite)) stop(
    "Report output already exists; use overwrite = TRUE to replace it.", call. = FALSE)
  quarto <- .epi_report_find_quarto()
  template_name <- switch(report,
    summary = "initial-epi-summary.qmd",
    quality = "initial-epi-quality.qmd",
    features = "initial-epi-features.qmd"
  )
  template <- system.file("quarto", template_name, package = "bcapture")
  if (!nzchar(template) || !file.exists(template)) stop(
    "Installed Quarto report template is missing: ", template_name, call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop(
    "ggplot2 is required to render bcapture HTML reports.", call. = FALSE)

  input <- .epi_report_read_products(deidentified_dir, report)
  manifest <- if (report == "features") input$products$feature_summary_manifest else
    input$products$summary_manifest
  safe_data <- c(input$products, list(
    privacy = .epi_report_safe_privacy(input$privacy),
    report = report
  ))
  stage <- tempfile("bcapture-report-stage-")
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  on.exit(if (dir.exists(stage)) fs::dir_delete(stage), add = TRUE)
  data_file <- fs::path(stage, "report-data.rds")
  saveRDS(safe_data, data_file)
  params_file <- fs::path(stage, "report-params.yml")
  render_stage <- fs::path(stage, "rendered")
  dir.create(render_stage, recursive = TRUE, showWarnings = FALSE)
  staged_template <- fs::path(render_stage, "report.qmd")
  if (!file.copy(template, staged_template, overwrite = TRUE)) stop(
    "Unable to stage the installed Quarto report template.", call. = FALSE)
  staged_assets <- fs::path(render_stage, "assets")
  dir.create(staged_assets, recursive = TRUE, showWarnings = FALSE)
  template_assets <- fs::path(fs::path_dir(template), "assets", "epi-report.css")
  if (!file.copy(template_assets, fs::path(staged_assets, "epi-report.css"), overwrite = TRUE)) stop(
    "Unable to stage the Quarto report stylesheet.", call. = FALSE)
  generated_at <- utc_now()
  title <- switch(report,
    summary = "Initial Epi Descriptive Summary",
    quality = "Initial Epi Data Quality Review",
    features = "Initial Epi Analytical Feature Review"
  )
  writeLines(c(
    paste0("data_file: ", .epi_report_yaml_value(data_file)),
    if (file.exists(fs::path(fs::path_dir(template), "..", "..", "R", "plot-epi.R")))
      paste0("plot_helpers: ", .epi_report_yaml_value(fs::path_norm(
        fs::path(fs::path_dir(template), "..", "..", "R", "plot-epi.R")))) else
      "plot_helpers: null",
    paste0("report_title: ", .epi_report_yaml_value(title)),
    paste0("generated_at: ", .epi_report_yaml_value(generated_at))
  ), params_file, useBytes = TRUE)
  args <- .epi_report_quarto_args(staged_template, render_stage, output_file, params_file)
  stdout <- fs::path(stage, "quarto-stdout.txt")
  stderr <- fs::path(stage, "quarto-stderr.txt")
  deno_cache <- fs::path(stage, "deno-cache")
  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(render_stage)
  status <- .epi_report_run_quarto(quarto, args, stdout, stderr, deno_cache)
  setwd(old_dir)
  if (!identical(as.integer(status), 0L)) {
    diagnostics <- c(
      if (file.exists(stderr)) readLines(stderr, warn = FALSE) else character(),
      if (file.exists(stdout)) readLines(stdout, warn = FALSE) else character()
    )
    diagnostics <- diagnostics[nzchar(diagnostics)]
    diagnostics <- paste(utils::tail(diagnostics, 40L), collapse = "\n")
    stop(
      "Quarto failed for report '", report, "' with exit status ", status, ".",
      if (nzchar(diagnostics)) paste0("\n", diagnostics) else "", call. = FALSE)
  }
  rendered <- fs::path(render_stage, output_file)
  if (!file.exists(rendered) || file.info(rendered)$size <= 0) stop(
    "Quarto completed but the expected HTML report was not created.", call. = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  publish <- fs::path(output_dir, paste0(".", output_file, ".tmp-",
    paste(sample(c(letters, 0:9), 10L, replace = TRUE), collapse = "")))
  on.exit(unlink(publish, force = TRUE), add = TRUE)
  if (!file.copy(rendered, publish, overwrite = TRUE)) stop(
    "Unable to stage the rendered HTML report for publication.", call. = FALSE)
  if (file.exists(target)) unlink(target, force = TRUE)
  if (file.exists(target)) stop("Unable to replace the existing HTML report.", call. = FALSE)
  if (!file.rename(publish, target)) stop(
    "Unable to finalize the HTML report.", call. = FALSE)
  quarto_version <- .epi_report_quarto_version(quarto)
  .epi_report_write_manifest(fs::path(output_dir, "report_manifest.csv"),
    .epi_report_manifest_row(report, output_file, manifest, quarto_version))
  result <- list(
    report = report, status = "rendered", output_file = target,
    summary_status = as.character(manifest$status[[1L]]),
    n_cases = as.integer(manifest$n_cases[[1L]]), quarto_version = quarto_version
  )
  if (!isTRUE(quiet)) cli::cli_inform(
    "Rendered Initial Epi {report} report for {result$n_cases} cases: {target}")
  result
}
