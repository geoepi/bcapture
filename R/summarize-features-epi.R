.epi_feature_summary_files <- c(
  overview = "feature_overview.csv",
  binary = "binary_feature_summary.csv",
  categorical = "categorical_feature_summary.csv",
  numeric = "numeric_feature_summary.csv",
  count = "count_feature_summary.csv",
  missingness = "feature_missingness_summary.csv",
  domain_coverage = "domain_coverage_summary.csv",
  consistency = "consistency_summary.csv",
  manifest = "feature_summary_manifest.csv"
)

.epi_feature_product_files <- c(
  features = "epi_features.csv",
  feature_long = "feature_long.csv",
  feature_dictionary = "feature_dictionary.csv",
  feature_summary = "feature_summary.csv",
  domain_summary = "feature_domain_summary.csv",
  consistency_findings = "feature_consistency_findings.csv",
  audit = "feature_derivation_audit.csv",
  manifest = "feature_manifest.csv"
)

.epi_feature_summary_read_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
}

.epi_feature_summary_input <- function(x) {
  if (is.character(x)) {
    deidentified_dir <- validate_scalar_path(x, "x")
    feature_dir <- fs::path(deidentified_dir, "features")
    paths <- fs::path(feature_dir, unname(.epi_feature_product_files))
    if (!dir.exists(feature_dir) || !all(file.exists(paths))) stop(
      "Feature products are missing; run derive_epi_features() first.", call. = FALSE
    )
    products <- lapply(paths, .epi_feature_summary_read_csv)
    names(products) <- names(.epi_feature_product_files)
    return(c(products, list(
      deidentified_dir = deidentified_dir, feature_dir = feature_dir
    )))
  }
  if (!is.list(x) || !all(names(.epi_feature_product_files) %in% names(x))) stop(
    "`x` must be a de-identified output directory or the result of derive_epi_features().",
    call. = FALSE
  )
  feature_dir <- if (length(x$output_dir) == 1L && !is.na(x$output_dir) &&
                     nzchar(as.character(x$output_dir))) {
    validate_scalar_path(as.character(x$output_dir), "x$output_dir")
  } else {
    NA_character_
  }
  c(x[names(.epi_feature_product_files)], list(
    deidentified_dir = if (is.na(feature_dir)) NA_character_ else dirname(feature_dir),
    feature_dir = feature_dir
  ))
}

.epi_feature_summary_require_columns <- function(x, required, label) {
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) stop(
    label, " is missing required columns: ", paste(missing, collapse = ", "), ".",
    call. = FALSE
  )
  invisible(x)
}

.epi_feature_summary_validate <- function(input) {
  manifest <- input$manifest
  dictionary <- input$feature_dictionary
  long <- input$feature_long
  findings <- input$consistency_findings
  .epi_feature_summary_require_columns(manifest, c(
    "feature_schema_version", "feature_version", "form_version",
    "feature_registry_hash", "n_cases", "n_features", "n_domains", "status"
  ), "feature_manifest.csv")
  if (nrow(manifest) != 1L) stop(
    "feature_manifest.csv must contain exactly one row.", call. = FALSE
  )
  if (!identical(as.integer(manifest$feature_schema_version[[1L]]), 1L)) stop(
    "Unsupported feature schema version.", call. = FALSE
  )
  .epi_feature_summary_require_columns(dictionary, c(
    "feature_order", "feature_version", "form_version", "feature_name",
    "feature_label", "domain_id", "domain_label", "domain_order",
    "feature_type", "unit", "description", "source_question_id",
    "source_table", "source_variable", "source_codebook_id"
  ), "feature_dictionary.csv")
  .epi_feature_summary_require_columns(long, c(
    "case_id", "domain_id", "domain_label", "feature_name", "feature_label",
    "feature_type", "value_status", "logical_value", "numeric_value",
    "character_value"
  ), "feature_long.csv")
  .epi_feature_summary_require_columns(findings, c(
    "case_id", "feature_name", "severity", "finding_type"
  ), "feature_consistency_findings.csv")
  if (anyDuplicated(as.character(dictionary$feature_name))) stop(
    "feature_dictionary.csv contains duplicate features.", call. = FALSE
  )
  if (!all(as.character(dictionary$feature_type) %in%
           c("binary", "categorical", "numeric", "count"))) stop(
    "feature_dictionary.csv contains unsupported feature types.", call. = FALSE
  )
  if (anyDuplicated(paste(long$case_id, long$feature_name, sep = "\r"))) stop(
    "feature_long.csv contains duplicate case-feature rows.", call. = FALSE
  )
  feature_names <- as.character(dictionary$feature_name)
  if (!setequal(unique(as.character(long$feature_name)), feature_names)) stop(
    "feature_long.csv does not contain exactly the registered features.", call. = FALSE
  )
  cases <- sort(unique(as.character(long$case_id)))
  expected_rows <- length(cases) * length(feature_names)
  if (nrow(long) != expected_rows ||
      !identical(as.integer(manifest$n_cases[[1L]]), as.integer(length(cases))) ||
      !identical(as.integer(manifest$n_features[[1L]]), as.integer(nrow(dictionary)))) stop(
    "Feature products do not contain one row per case and registered feature.",
    call. = FALSE
  )
  dictionary <- dictionary[order(as.integer(dictionary$feature_order)), , drop = FALSE]
  list(manifest = manifest, dictionary = dictionary, long = long,
       findings = findings, cases = cases)
}

.epi_feature_is_known <- function(current, feature_type) {
  status <- as.character(current$value_status)
  if (identical(feature_type, "binary")) {
    status == "known" & !is.na(current$logical_value)
  } else if (identical(feature_type, "categorical")) {
    status %in% c("known", "unknown", "not_applicable") &
      !is.na(current$character_value) & nzchar(as.character(current$character_value))
  } else {
    status == "known" & !is.na(current$numeric_value)
  }
}

.epi_feature_numeric_statistics <- function(values) {
  values <- as.numeric(values[!is.na(values)])
  c(
    mean = if (length(values) == 0L) NA_real_ else mean(values),
    sd = if (length(values) < 2L) NA_real_ else stats::sd(values),
    median = if (length(values) == 0L) NA_real_ else stats::median(values),
    q25 = if (length(values) == 0L) NA_real_ else
      as.numeric(stats::quantile(values, 0.25, names = FALSE)),
    q75 = if (length(values) == 0L) NA_real_ else
      as.numeric(stats::quantile(values, 0.75, names = FALSE)),
    min = if (length(values) == 0L) NA_real_ else min(values),
    max = if (length(values) == 0L) NA_real_ else max(values)
  )
}

.epi_feature_categorical_levels <- function(meta, observed, form_version) {
  codebook_id <- as.character(meta$source_codebook_id[[1L]])
  defined <- character()
  if (!is.na(codebook_id) && nzchar(codebook_id)) {
    defined <- tryCatch({
      codes <- load_epi_codebook(form_version)
      current <- codes[as.character(codes$codebook_id) == codebook_id, , drop = FALSE]
      current <- current[order(as.integer(current$response_order)), , drop = FALSE]
      as.character(current$response_label)
    }, error = function(e) character())
  }
  unique(c(defined[!is.na(defined) & nzchar(defined)],
           observed[!is.na(observed) & nzchar(observed)]))
}

.epi_feature_descriptive_products <- function(validated) {
  manifest <- validated$manifest
  dictionary <- validated$dictionary
  long <- validated$long
  findings <- validated$findings
  cases <- validated$cases
  n_cases <- length(cases)
  binary_rows <- list()
  categorical_rows <- list()
  numeric_rows <- list()
  count_rows <- list()
  missing_rows <- list()

  for (i in seq_len(nrow(dictionary))) {
    meta <- dictionary[i, , drop = FALSE]
    name <- as.character(meta$feature_name[[1L]])
    type <- as.character(meta$feature_type[[1L]])
    current <- long[as.character(long$feature_name) == name, , drop = FALSE]
    current <- current[match(cases, as.character(current$case_id)), , drop = FALSE]
    known <- .epi_feature_is_known(current, type)
    common <- list(
      feature_name = name,
      feature_label = as.character(meta$feature_label[[1L]]),
      domain_id = as.character(meta$domain_id[[1L]]),
      domain_label = as.character(meta$domain_label[[1L]])
    )
    missing_rows[[i]] <- tibble::as_tibble(c(common, list(
      feature_type = type, n_cases = as.integer(n_cases),
      n_known = as.integer(sum(known)), n_missing = as.integer(sum(!known)),
      percent_missing = if (n_cases == 0L) NA_real_ else 100 * sum(!known) / n_cases
    )))
    if (type == "binary") {
      n_true <- sum(known & current$logical_value %in% TRUE)
      n_false <- sum(known & current$logical_value %in% FALSE)
      binary_rows[[length(binary_rows) + 1L]] <- tibble::as_tibble(c(common, list(
        n_cases = as.integer(n_cases), n_known = as.integer(sum(known)),
        n_true = as.integer(n_true), n_false = as.integer(n_false),
        n_missing = as.integer(sum(!known)),
        percent_true_of_known = if (sum(known) == 0L) NA_real_ else
          100 * n_true / sum(known),
        percent_true_of_all_cases = if (n_cases == 0L) NA_real_ else
          100 * n_true / n_cases
      )))
    } else if (type == "categorical") {
      observed <- as.character(current$character_value[known])
      levels <- .epi_feature_categorical_levels(
        meta, observed, as.character(manifest$form_version[[1L]])
      )
      for (category in levels) {
        n <- sum(observed == category, na.rm = TRUE)
        categorical_rows[[length(categorical_rows) + 1L]] <- tibble::as_tibble(c(
          common, list(
            category = category, n = as.integer(n), n_known = as.integer(sum(known)),
            n_missing = as.integer(sum(!known)),
            percent_of_known = if (sum(known) == 0L) NA_real_ else 100 * n / sum(known),
            percent_of_all_cases = if (n_cases == 0L) NA_real_ else 100 * n / n_cases
          )
        ))
      }
    } else if (type %in% c("numeric", "count")) {
      values <- as.numeric(current$numeric_value[known])
      statistics <- as.list(.epi_feature_numeric_statistics(values))
      base <- c(common, list(
        n_cases = as.integer(n_cases), n_nonmissing = as.integer(length(values))
      ))
      if (type == "numeric") {
        numeric_rows[[length(numeric_rows) + 1L]] <- tibble::as_tibble(c(
          common, list(unit = as.character(meta$unit[[1L]])),
          list(n_cases = as.integer(n_cases), n_nonmissing = as.integer(length(values)),
               n_missing = as.integer(n_cases - length(values))), statistics
        ))
      } else {
        count_rows[[length(count_rows) + 1L]] <- tibble::as_tibble(c(
          base, list(n_zero = as.integer(sum(values == 0)),
                     n_positive = as.integer(sum(values > 0))), statistics
        ))
      }
    }
  }

  missingness <- dplyr::bind_rows(missing_rows)
  domains <- unique(dictionary[c("domain_id", "domain_label", "domain_order")])
  domains <- domains[order(as.integer(domains$domain_order)), , drop = FALSE]
  domain_rows <- lapply(seq_len(nrow(domains)), function(i) {
    domain <- domains[i, , drop = FALSE]
    names <- as.character(dictionary$feature_name[
      as.character(dictionary$domain_id) == as.character(domain$domain_id[[1L]])])
    current <- long[as.character(long$feature_name) %in% names, , drop = FALSE]
    known <- mapply(function(status, logical, numeric, character, feature_name) {
      row <- data.frame(value_status = status, logical_value = logical,
                        numeric_value = numeric, character_value = character,
                        stringsAsFactors = FALSE)
      type <- as.character(dictionary$feature_type[
        match(feature_name, dictionary$feature_name)])
      .epi_feature_is_known(row, type)
    }, current$value_status, current$logical_value, current$numeric_value,
       current$character_value, current$feature_name, USE.NAMES = FALSE)
    known_by_case <- stats::setNames(integer(n_cases), cases)
    if (nrow(current) > 0L) {
      known_by_case <- as.integer(rowsum(as.integer(known), current$case_id,
                                         reorder = FALSE)[cases, 1L])
      known_by_case[is.na(known_by_case)] <- 0L
    }
    n_features <- length(names)
    n_cells <- n_cases * n_features
    n_known <- sum(known)
    tibble::tibble(
      domain_id = as.character(domain$domain_id[[1L]]),
      domain_label = as.character(domain$domain_label[[1L]]),
      n_features = as.integer(n_features), n_cases = as.integer(n_cases),
      n_case_feature_cells = as.integer(n_cells), n_known_cells = as.integer(n_known),
      n_missing_cells = as.integer(n_cells - n_known),
      percent_known_cells = if (n_cells == 0L) NA_real_ else 100 * n_known / n_cells,
      n_cases_with_any_known_feature = as.integer(sum(known_by_case > 0L)),
      percent_cases_with_any_known_feature = if (n_cases == 0L) NA_real_ else
        100 * sum(known_by_case > 0L) / n_cases,
      n_cases_with_all_features_known = as.integer(sum(known_by_case == n_features)),
      percent_cases_with_all_features_known = if (n_cases == 0L) NA_real_ else
        100 * sum(known_by_case == n_features) / n_cases
    )
  })
  domain_coverage <- dplyr::bind_rows(domain_rows)

  finding_rows <- if (nrow(findings) == 0L) tibble::tibble(
    summary_level = character(), severity = character(), finding_type = character(),
    feature_name = character(), feature_label = character(), domain_id = character(),
    domain_label = character(), n_findings = integer(), n_cases = integer()
  ) else {
    finding_meta <- dplyr::left_join(
      findings, dictionary[c("feature_name", "feature_label", "domain_id", "domain_label")],
      by = "feature_name"
    )
    grouped <- dplyr::group_by(finding_meta, severity, finding_type, feature_name,
                               feature_label, domain_id, domain_label)
    dplyr::ungroup(dplyr::summarise(
      grouped, n_findings = dplyr::n(), n_cases = dplyr::n_distinct(case_id),
      .groups = "drop"
    )) |>
      dplyr::mutate(summary_level = "finding", .before = 1L)
  }
  severity_levels <- c("ERROR", "WARNING", "INFO")
  severity_total <- lapply(severity_levels, function(level) {
    current <- findings[as.character(findings$severity) == level, , drop = FALSE]
    tibble::tibble(
      summary_level = "severity_total", severity = level,
      finding_type = NA_character_, feature_name = NA_character_,
      feature_label = NA_character_, domain_id = NA_character_,
      domain_label = NA_character_, n_findings = as.integer(nrow(current)),
      n_cases = as.integer(length(unique(as.character(current$case_id))))
    )
  })
  consistency <- dplyr::bind_rows(finding_rows, dplyr::bind_rows(severity_total))

  counts <- table(factor(as.character(dictionary$feature_type),
                         levels = c("binary", "count", "numeric", "categorical")))
  n_warnings <- sum(as.character(findings$severity) == "WARNING")
  n_errors <- sum(as.character(findings$severity) == "ERROR")
  overview <- tibble::tibble(
    feature_version = as.character(manifest$feature_version[[1L]]),
    feature_registry_hash = as.character(manifest$feature_registry_hash[[1L]]),
    n_cases = as.integer(n_cases), n_features = as.integer(nrow(dictionary)),
    n_domains = as.integer(nrow(domains)), n_binary = as.integer(counts[["binary"]]),
    n_count = as.integer(counts[["count"]]), n_numeric = as.integer(counts[["numeric"]]),
    n_categorical = as.integer(counts[["categorical"]]),
    n_consistency_findings = as.integer(nrow(findings)),
    n_consistency_warnings = as.integer(n_warnings),
    n_consistency_errors = as.integer(n_errors),
    source_feature_status = as.character(manifest$status[[1L]])
  )
  status <- if (identical(as.character(manifest$status[[1L]]), "passed") &&
                n_warnings == 0L && n_errors == 0L) "passed" else "review"
  package_version <- tryCatch(as.character(utils::packageVersion("bcapture")),
                              error = function(e) "0.0.0.9000")
  summary_manifest <- tibble::tibble(
    feature_summary_schema_version = 1L,
    feature_version = as.character(manifest$feature_version[[1L]]),
    form_version = as.character(manifest$form_version[[1L]]),
    feature_registry_hash = as.character(manifest$feature_registry_hash[[1L]]),
    profile = if ("profile" %in% names(manifest))
      as.character(manifest$profile[[1L]]) else NA_character_,
    source_feature_manifest_status = as.character(manifest$status[[1L]]),
    n_cases = as.integer(n_cases), n_features = as.integer(nrow(dictionary)),
    n_domains = as.integer(nrow(domains)), status = status,
    package_version = package_version, created_at = utc_now()
  )
  distribution_data <- long[
    as.character(long$feature_type) %in% c("numeric", "count") &
      as.character(long$value_status) == "known" & !is.na(long$numeric_value),
    c("feature_name", "feature_label", "domain_id", "domain_label",
      "feature_type", "numeric_value"), drop = FALSE
  ]
  list(
    overview = overview, binary = dplyr::bind_rows(binary_rows),
    categorical = dplyr::bind_rows(categorical_rows),
    numeric = dplyr::bind_rows(numeric_rows), count = dplyr::bind_rows(count_rows),
    missingness = missingness, domain_coverage = domain_coverage,
    consistency = consistency, manifest = summary_manifest,
    distribution_data = tibble::as_tibble(distribution_data)
  )
}

.epi_feature_summary_write <- function(products, output_dir, overwrite) {
  if (dir.exists(output_dir) && !isTRUE(overwrite)) stop(
    "Feature summary output already exists; use overwrite = TRUE to replace it.",
    call. = FALSE
  )
  parent <- dirname(output_dir)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(pattern = "summary.tmp-", tmpdir = parent)
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  on.exit(if (dir.exists(stage)) fs::dir_delete(stage), add = TRUE)
  for (name in names(.epi_feature_summary_files)) {
    write_csv_utf8(products[[name]], fs::path(stage, .epi_feature_summary_files[[name]]))
  }
  backup <- NA_character_
  if (dir.exists(output_dir)) {
    backup <- tempfile(pattern = "summary.backup-", tmpdir = parent)
    if (!file.rename(output_dir, backup)) stop(
      "Unable to stage the existing feature summary output for replacement.",
      call. = FALSE
    )
  }
  if (!file.rename(stage, output_dir)) {
    if (!is.na(backup) && dir.exists(backup)) file.rename(backup, output_dir)
    stop("Unable to finalize feature summary output directory.", call. = FALSE)
  }
  if (!is.na(backup) && dir.exists(backup)) fs::dir_delete(backup)
  on.exit(NULL, add = TRUE)
  invisible(output_dir)
}

#' Summarize case-level Initial Epi analytical features
#'
#' `summarize_epi_features()` describes the de-identified case-level products
#' created by [derive_epi_features()]. It does not rerun feature derivations and
#' never reads identifiable source data or private crosswalks. Results are
#' descriptive: feature prevalence and information coverage are not measures of
#' risk and do not imply causation.
#'
#' @param x A de-identified output directory containing `features`, or the
#'   in-memory result returned by [derive_epi_features()].
#' @param write Write CSV products below `features/summary`.
#' @param overwrite Atomically replace only an existing `features/summary`
#'   directory.
#' @param quiet Suppress progress messages.
#' @return A named list of aggregate feature summaries, a minimized numeric and
#'   count distribution data set without case identifiers, the manifest, and
#'   output directory.
#' @export
summarize_epi_features <- function(x, write = TRUE, overwrite = FALSE,
                                   quiet = FALSE) {
  input <- .epi_feature_summary_input(x)
  validated <- .epi_feature_summary_validate(input)
  products <- .epi_feature_descriptive_products(validated)
  output_dir <- if (is.na(input$feature_dir)) NA_character_ else
    fs::path(input$feature_dir, "summary")
  if (isTRUE(write)) {
    if (is.na(output_dir)) stop(
      "`write = TRUE` requires feature products with a known output directory.",
      call. = FALSE
    )
    .epi_feature_summary_write(products, output_dir, overwrite)
  }
  if (!isTRUE(quiet)) cli::cli_inform(
    "Summarized {nrow(validated$dictionary)} Initial Epi feature{?s} across {length(validated$cases)} de-identified case{?s}; summary status {products$manifest$status[[1L]]}."
  )
  c(products, list(output_dir = if (isTRUE(write)) output_dir else NA_character_))
}
