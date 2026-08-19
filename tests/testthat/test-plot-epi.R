.plot_summary_fixture <- function() {
  parsed <- parse(testthat::test_path("test-summarize-epi.R"))
  helper_env <- new.env(parent = globalenv())
  eval(parsed[[1L]], helper_env)
  deidentified_dir <- helper_env$.summary_fixture()
  bcapture::summarize_epi(deidentified_dir, quiet = TRUE)
  deidentified_dir
}

.plot_summary_with <- function(summary, product, data) {
  summary[[product]] <- data
  summary
}

test_that("visualization input accepts summary results and paths without reading source products", {
  deidentified_dir <- .plot_summary_fixture()
  summary <- bcapture::summarize_epi(deidentified_dir, write = FALSE, quiet = TRUE)
  expect_true(is.list(bcapture:::.epi_visualization_input(summary)))
  from_path <- bcapture:::.epi_visualization_input(deidentified_dir)
  expect_equal(nrow(from_path$scalar_frequencies), nrow(summary$scalar_frequencies))
  unlink(file.path(deidentified_dir, "collated"), recursive = TRUE, force = TRUE)
  unlink(file.path(deidentified_dir, "privacy"), recursive = TRUE, force = TRUE)
  expect_s3_class(bcapture::plot_epi_validation(deidentified_dir, type = "status"), "ggplot")
  expect_false(any(file.exists(fs::path(deidentified_dir, "record_crosswalk.csv"))))
})

test_that("categorical plots preserve supplied percentages and response semantics", {
  summary <- bcapture::summarize_epi(.plot_summary_fixture(), write = FALSE, quiet = TRUE)
  coded <- summary$scalar_frequencies[summary$scalar_frequencies$canonical_name ==
    "closest_field_tilled_last_fall", , drop = FALSE]
  coded$percent_of_answered <- c(42, 17, 0)
  summary <- .plot_summary_with(summary, "scalar_frequencies", rbind(
    coded, summary$scalar_frequencies[summary$scalar_frequencies$canonical_name ==
      "supplemental_feed", , drop = FALSE]))
  plot <- bcapture::plot_epi_categorical(summary, "closest_field_tilled_last_fall")
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$data$display_percent[seq_len(nrow(coded))], c(42, 17, 0))
  expect_true(all(c("Yes", "No", "Don't know") %in% as.character(plot$data$display_response)))
  not_applicable <- bcapture::plot_epi_categorical(summary, "supplemental_feed")
  expect_true("Not applicable" %in% as.character(not_applicable$data$display_response))
  expect_match(plot$labels$caption, "Answered: 2 of 3 cases; missing: 1")
  all_cases <- bcapture::plot_epi_categorical(summary, "closest_field_tilled_last_fall",
    denominator = "all_cases")
  expect_equal(all_cases$data$display_percent[1L], coded$percent_of_all_cases[1L])
})

test_that("categorical resolution is exact and ambiguous requests are rejected", {
  summary <- bcapture::summarize_epi(.plot_summary_fixture(), write = FALSE, quiet = TRUE)
  expect_error(bcapture::plot_epi_categorical(summary, "does_not_exist"), "No categorical variable matched")
  duplicate <- summary$scalar_frequencies[summary$scalar_frequencies$question_id == "A1", , drop = FALSE]
  duplicate$canonical_name <- paste0(duplicate$canonical_name, "_copy")
  summary$scalar_frequencies <- rbind(summary$scalar_frequencies, duplicate)
  expect_error(bcapture::plot_epi_categorical(summary, "A1"), "Ambiguous categorical variable.*Candidates")
})

test_that("multiselect plots use supplied denominator columns and annotate selection semantics", {
  summary <- bcapture::summarize_epi(.plot_summary_fixture(), write = FALSE, quiet = TRUE)
  multi <- summary$multiselect_frequencies[summary$multiselect_frequencies$question_id == "C3", , drop = FALSE]
  multi$percent_of_all_cases <- seq_len(nrow(multi)) * 13
  summary$multiselect_frequencies <- multi
  plot <- bcapture::plot_epi_multiselect(summary, "C3")
  expect_equal(plot$data$display_percent, multi$percent_of_all_cases)
  expect_match(plot$labels$caption, "Cases with any selection")
  expect_match(plot$labels$caption, "need not sum to 100%")
  selecting <- bcapture::plot_epi_multiselect(summary, "C3", denominator = "selecting_cases")
  expect_equal(selecting$data$display_percent, multi$percent_of_selecting_cases)
})

test_that("numeric plots use aggregate intervals and handle no parsed values", {
  summary <- bcapture::summarize_epi(.plot_summary_fixture(), write = FALSE, quiet = TRUE)
  plot <- bcapture::plot_epi_numeric(summary, "baseline_mortality")
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$data$median, 1.5)
  expect_equal(plot$data$q25, 1.25)
  expect_equal(plot$data$q75, 1.75)
  expect_equal(plot$data$min, 1)
  expect_equal(plot$data$max, 2)
  expect_match(plot$labels$caption, "Parsed: 2; unparsed: 1; missing: 0")
  no_data <- summary$numeric_summaries
  no_data$n_parsed[no_data$canonical_name == "baseline_mortality"] <- 0L
  summary$numeric_summaries <- no_data
  empty <- bcapture::plot_epi_numeric(summary, "baseline_mortality")
  expect_s3_class(empty, "ggplot")
  expect_match(empty$data$label, "No parsed numeric responses")
  expect_error(bcapture::plot_epi_numeric(summary), "Multiple numeric variables")
})

test_that("repeated plots distinguish records from cases and retain zero tables", {
  summary <- bcapture::summarize_epi(.plot_summary_fixture(), write = FALSE, quiet = TRUE)
  repeated <- summary$repeated_categorical_frequencies
  expect_gt(nrow(repeated), 0L)
  variable <- as.character(repeated$canonical_name[[1L]])
  plot <- bcapture::plot_epi_repeated(summary, variable = variable, type = "categorical")
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$data$display_percent, repeated$percent_of_nonmissing[
    repeated$canonical_name == variable])
  expect_match(plot$labels$y, "records")
  expect_match(plot$labels$caption, "records with a nonmissing response")
  counts <- bcapture::plot_epi_repeated(summary, type = "counts")
  expect_equal(nrow(counts$data), 11L)
  expect_true(any(counts$data$n_records_total == 0L))
})

test_that("validation plots preserve supplied levels and ordering", {
  summary <- bcapture::summarize_epi(.plot_summary_fixture(), write = FALSE, quiet = TRUE)
  status <- bcapture::plot_epi_validation(summary, type = "status")
  expect_equal(as.character(status$data$validation_status), c("valid", "review", "error"))
  expect_equal(status$data$n_cases, c(1L, 1L, 1L))
  rules <- bcapture::plot_epi_validation(summary, type = "rules")
  expect_s3_class(rules, "ggplot")
  expect_true(all(c("n_findings", "n_cases") %in% names(rules$data)))
})

test_that("all exported plot functions are read-only and create no files", {
  deidentified_dir <- .plot_summary_fixture()
  summary <- bcapture::summarize_epi(deidentified_dir, write = FALSE, quiet = TRUE)
  before <- fs::dir_ls(deidentified_dir, recurse = TRUE, type = "file")
  invisible(bcapture::plot_epi_categorical(summary, "closest_field_tilled_last_fall"))
  invisible(bcapture::plot_epi_multiselect(summary, "C3"))
  invisible(bcapture::plot_epi_numeric(summary, "baseline_mortality"))
  invisible(bcapture::plot_epi_repeated(summary, type = "counts"))
  invisible(bcapture::plot_epi_validation(summary, type = "rules"))
  after <- fs::dir_ls(deidentified_dir, recurse = TRUE, type = "file")
  expect_identical(before, after)
})
