.epi_feature_plot_missing <- function() {
  stop("Feature summary products are missing; run summarize_epi_features() first.",
       call. = FALSE)
}

.epi_feature_plot_read <- function(deidentified_dir) {
  deidentified_dir <- validate_scalar_path(deidentified_dir, "x")
  summary_dir <- fs::path(deidentified_dir, "features", "summary")
  paths <- fs::path(summary_dir, unname(.epi_feature_summary_files))
  if (!dir.exists(summary_dir) || !all(file.exists(paths))) .epi_feature_plot_missing()
  products <- lapply(paths, .epi_feature_summary_read_csv)
  names(products) <- names(.epi_feature_summary_files)
  long_path <- fs::path(deidentified_dir, "features", "feature_long.csv")
  if (!file.exists(long_path)) .epi_feature_plot_missing()
  long <- .epi_feature_summary_read_csv(long_path)
  .epi_feature_summary_require_columns(long, c(
    "feature_name", "feature_label", "domain_id", "domain_label",
    "feature_type", "value_status", "numeric_value"
  ), "feature_long.csv")
  products$distribution_data <- tibble::as_tibble(long[
    as.character(long$feature_type) %in% c("numeric", "count") &
      as.character(long$value_status) == "known" & !is.na(long$numeric_value),
    c("feature_name", "feature_label", "domain_id", "domain_label",
      "feature_type", "numeric_value"), drop = FALSE
  ])
  products
}

.epi_feature_plot_input <- function(x) {
  products <- if (is.character(x)) .epi_feature_plot_read(x) else if (is.list(x)) x else
    stop("`x` must be a summarize_epi_features() result or a de-identified output directory.",
         call. = FALSE)
  if (!all(c(names(.epi_feature_summary_files), "distribution_data") %in% names(products))) {
    .epi_feature_plot_missing()
  }
  manifest <- products$manifest
  if (!is.data.frame(manifest) || nrow(manifest) != 1L ||
      !"feature_summary_schema_version" %in% names(manifest) ||
      as.integer(manifest$feature_summary_schema_version[[1L]]) != 1L) stop(
    "Unsupported feature summary schema version; expected feature_summary_schema_version == 1.",
    call. = FALSE
  )
  status <- as.character(manifest$status[[1L]])
  if (!status %in% c("passed", "review")) stop(
    "Feature summary manifest status must be passed or review before plotting.",
    call. = FALSE
  )
  products
}

.epi_feature_plot_filter <- function(data, domain = NULL, feature = NULL) {
  if (!is.null(domain)) {
    if (length(domain) != 1L || is.na(domain) || !nzchar(as.character(domain))) stop(
      "`domain` must be NULL or one non-empty domain ID or label.", call. = FALSE
    )
    keep <- as.character(data$domain_id) == as.character(domain) |
      as.character(data$domain_label) == as.character(domain)
    if (!any(keep)) stop("No feature domain matched '", domain, "'.", call. = FALSE)
    data <- data[keep, , drop = FALSE]
  }
  if (!is.null(feature)) {
    if (length(feature) != 1L || is.na(feature) || !nzchar(as.character(feature))) stop(
      "`feature` must be NULL or one non-empty feature name or label.", call. = FALSE
    )
    keep <- as.character(data$feature_name) == as.character(feature) |
      as.character(data$feature_label) == as.character(feature)
    if (!any(keep)) stop("No feature matched '", feature, "'.", call. = FALSE)
    data <- data[keep, , drop = FALSE]
  }
  data
}

.epi_feature_plot_prevalence <- function(products, domain, feature) {
  binary <- products$binary
  categorical <- products$categorical
  if (!is.null(domain)) {
    .epi_feature_plot_filter(products$missingness, domain, NULL)
    binary <- binary[as.character(binary$domain_id) == as.character(domain) |
      as.character(binary$domain_label) == as.character(domain), , drop = FALSE]
    categorical <- categorical[
      as.character(categorical$domain_id) == as.character(domain) |
        as.character(categorical$domain_label) == as.character(domain), , drop = FALSE]
  }
  if (!is.null(feature)) {
    binary_hit <- as.character(binary$feature_name) == as.character(feature) |
      as.character(binary$feature_label) == as.character(feature)
    categorical_hit <- as.character(categorical$feature_name) == as.character(feature) |
      as.character(categorical$feature_label) == as.character(feature)
    if (any(categorical_hit)) {
      data <- categorical[categorical_hit, , drop = FALSE]
      data$display_category <- factor(as.character(data$category),
        levels = rev(as.character(data$category)))
      return(ggplot2::ggplot(data, ggplot2::aes(x = display_category,
                                                y = percent_of_known)) +
        ggplot2::geom_col(fill = "#557d8b", show.legend = FALSE) +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(labels = function(x) .epi_wrap_labels(x)) +
        ggplot2::labs(title = as.character(data$feature_label[[1L]]), x = NULL,
          y = "Percent of known questionnaire states",
          caption = paste0("Known questionnaire states: ", data$n_known[[1L]],
            "; missing: ", data$n_missing[[1L]],
            ". Don't know and Not applicable remain substantive categories.")) +
        .theme_bcapture())
    }
    if (!any(binary_hit)) stop(
      "Prevalence plots require a binary or categorical feature; no feature matched '",
      feature, "'.", call. = FALSE
    )
    binary <- binary[binary_hit, , drop = FALSE]
  }
  if (nrow(binary) == 0L) return(.epi_empty_plot(
    "Binary feature prevalence", "No binary features matched", "Feature"
  ))
  binary <- binary[order(binary$percent_true_of_known, na.last = TRUE), , drop = FALSE]
  binary$display_feature <- factor(as.character(binary$feature_label),
    levels = unique(as.character(binary$feature_label)))
  ggplot2::ggplot(binary, ggplot2::aes(x = display_feature,
                                       y = percent_true_of_known)) +
    ggplot2::geom_col(fill = "#2c6e8f", show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(labels = function(x) .epi_wrap_labels(x, 46L)) +
    ggplot2::labs(title = "Recorded binary feature prevalence", x = NULL,
      y = "Percent TRUE among known responses",
      caption = "Denominator: cases with an explicit TRUE or FALSE feature value. Missing values are not treated as FALSE.") +
    .theme_bcapture()
}

.epi_feature_plot_missingness <- function(products, domain, feature) {
  data <- .epi_feature_plot_filter(products$missingness, domain, feature)
  data <- data[order(data$percent_missing, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  data$display_feature <- factor(as.character(data$feature_label),
    levels = rev(unique(as.character(data$feature_label))))
  ggplot2::ggplot(data, ggplot2::aes(x = display_feature, y = percent_missing)) +
    ggplot2::geom_col(fill = "#7b8790", show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(labels = function(x) .epi_wrap_labels(x, 46L)) +
    ggplot2::labs(title = "Feature missingness", x = NULL, y = "Percent missing",
      caption = "Don't know and Not applicable are counted as recorded categorical states, not missing values.") +
    .theme_bcapture()
}

.epi_feature_plot_distribution <- function(products, domain, feature) {
  data <- .epi_feature_plot_filter(products$distribution_data, domain, feature)
  if (is.null(feature)) {
    matched <- unique(as.character(data$feature_name))
    if (length(matched) != 1L) stop(
      "Supply one numeric or count `feature` for a distribution plot.", call. = FALSE
    )
  }
  if (nrow(data) == 0L) return(.epi_empty_plot(
    "Feature distribution", "No recorded numeric or count values", "Feature"
  ))
  if (length(unique(as.character(data$feature_name))) != 1L) stop(
    "Distribution plots require exactly one numeric or count feature.", call. = FALSE
  )
  data$display_feature <- factor(.epi_wrap_labels(data$feature_label, 46L))
  type <- as.character(data$feature_type[[1L]])
  ggplot2::ggplot(data, ggplot2::aes(x = display_feature, y = numeric_value)) +
    ggplot2::geom_boxplot(width = 0.28, outlier.shape = NA, fill = "#d8e4e9",
      colour = "#2c6e8f") +
    ggplot2::geom_point(position = ggplot2::position_jitter(
      width = 0.09, height = 0, seed = 1729L), alpha = 0.55, size = 1.8,
      colour = "#24313a") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = as.character(data$feature_label[[1L]]), x = NULL,
      y = if (type == "count") "Recorded qualifying rows" else "Recorded value",
      caption = if (type == "count")
        "Points are de-identified case-level counts. Zero means zero qualifying retained-content records; it does not confirm absence of the underlying event."
      else "Points are de-identified case-level values; the box shows the median and interquartile range.") +
    .theme_bcapture()
}

.epi_feature_plot_coverage <- function(products, domain) {
  data <- .epi_feature_plot_filter(products$domain_coverage, domain, NULL)
  data <- data[order(data$percent_known_cells, na.last = TRUE), , drop = FALSE]
  data$display_domain <- factor(as.character(data$domain_label),
    levels = unique(as.character(data$domain_label)))
  ggplot2::ggplot(data, ggplot2::aes(x = display_domain, y = percent_known_cells)) +
    ggplot2::geom_col(fill = "#6b8e9e", show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(labels = function(x) .epi_wrap_labels(x, 42L)) +
    ggplot2::labs(title = "Feature information coverage by domain", x = NULL,
      y = "Percent of case-feature cells known",
      caption = "Coverage describes recorded information completeness; it is not domain importance or risk.") +
    .theme_bcapture()
}

.epi_feature_plot_consistency <- function(products, domain, feature) {
  data <- products$consistency[products$consistency$summary_level == "finding", , drop = FALSE]
  if (nrow(data) > 0L) data <- .epi_feature_plot_filter(data, domain, feature)
  if (nrow(data) == 0L) return(.epi_empty_plot(
    "Feature consistency findings", "No consistency findings recorded", "Finding"
  ))
  key <- paste(data$domain_label, data$finding_type, data$severity, sep = "\r")
  sums <- rowsum(as.numeric(data$n_findings), key, reorder = FALSE)
  parts <- strsplit(rownames(sums), "\r", fixed = TRUE)
  plotted <- tibble::tibble(
    domain_label = vapply(parts, `[[`, character(1), 1L),
    finding_type = vapply(parts, `[[`, character(1), 2L),
    severity = vapply(parts, `[[`, character(1), 3L),
    n_findings = as.numeric(sums[, 1L])
  )
  plotted$display_finding <- .epi_wrap_labels(paste0(
    plotted$domain_label, ": ", gsub("_", " ", plotted$finding_type)), 52L)
  plotted$severity <- factor(plotted$severity, levels = c("ERROR", "WARNING", "INFO"))
  ggplot2::ggplot(plotted, ggplot2::aes(x = display_finding, y = n_findings,
                                        fill = severity)) +
    ggplot2::geom_col(show.legend = TRUE) + ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c(
      ERROR = "#a94a4a", WARNING = "#9a6b20", INFO = "#557d8b"
    ), drop = FALSE) +
    ggplot2::labs(title = "Feature consistency findings", x = "Domain and finding type",
      y = "Findings", fill = "Severity",
      caption = "Counts describe retained consistency findings; no case identifiers or source values are shown.") +
    .theme_bcapture()
}

#' Plot descriptive Initial Epi analytical feature summaries
#'
#' @param x A [summarize_epi_features()] result or a de-identified output
#'   directory containing `features/summary`.
#' @param domain Optional exact feature domain ID or label.
#' @param feature Optional exact feature name or label. A single numeric or
#'   count feature is required for distribution plots.
#' @param type Plot type: binary/categorical prevalence, feature missingness,
#'   numeric/count distribution, domain coverage, or consistency findings.
#' @return A standard ggplot object. No files are created.
#' @export
plot_epi_features <- function(x, domain = NULL, feature = NULL,
                              type = c("prevalence", "missingness", "distribution",
                                       "coverage", "consistency")) {
  .epi_visualization_require_ggplot2()
  type <- match.arg(type)
  products <- .epi_feature_plot_input(x)
  switch(type,
    prevalence = .epi_feature_plot_prevalence(products, domain, feature),
    missingness = .epi_feature_plot_missingness(products, domain, feature),
    distribution = .epi_feature_plot_distribution(products, domain, feature),
    coverage = {
      if (!is.null(feature)) stop(
        "`feature` is not used for domain coverage plots.", call. = FALSE
      )
      .epi_feature_plot_coverage(products, domain)
    },
    consistency = .epi_feature_plot_consistency(products, domain, feature)
  )
}
