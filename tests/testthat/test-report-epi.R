.report_fixture <- function() {
  parsed <- parse(testthat::test_path("test-summarize-epi.R"))
  helper_env <- new.env(parent = globalenv())
  eval(parsed[[1L]], helper_env)
  deidentified_dir <- helper_env$.summary_fixture()
  bcapture::summarize_epi(deidentified_dir, quiet = TRUE)
  deidentified_dir
}

.report_checksums <- function(path) {
  dirs <- fs::path(path, c("collated", "validation", "privacy", "summary"))
  files <- unlist(lapply(dirs, function(dir) {
    if (dir.exists(dir)) fs::dir_ls(dir, recurse = TRUE, type = "file") else character()
  }), use.names = FALSE)
  stats::setNames(as.character(tools::md5sum(files)), fs::path_rel(files, path))
}

test_that("installed report templates and command construction are stable", {
  expect_true(file.exists(system.file("quarto", "initial-epi-summary.qmd", package = "bcapture")))
  expect_true(file.exists(system.file("quarto", "initial-epi-quality.qmd", package = "bcapture")))
  args <- bcapture:::.epi_report_quarto_args(
    "C:/path with spaces/template.qmd", "C:/stage with spaces", "report.html",
    "C:/data with spaces/report-params.yml"
  )
  expect_equal(args[c(1:6)], c(
    "render", "C:/path with spaces/template.qmd", "--to", "html", "--output", "report.html"
  ))
  expect_true(any(grepl("--execute-params", args, fixed = TRUE)))
  expect_false(any(grepl("^TMPDIR=", args)))
  expect_false(any(grepl("^QUARTO_R=", args)))
  expect_match(bcapture:::.epi_report_quarto_env(), "^QUARTO_R=")
  expect_error(bcapture:::.epi_report_find_quarto(""),
    "Quarto is required to render bcapture HTML reports but was not found on PATH.")
})

test_that("report input gates require complete, compatible summary products", {
  deidentified_dir <- tempfile("epi-report-missing-")
  dir.create(deidentified_dir)
  expect_error(bcapture::render_epi_report(deidentified_dir),
    "Summary products are missing; run summarize_epi\\(\\) first\\.")
  expect_error(bcapture::render_epi_report(deidentified_dir, report = "all"),
    "arg.*one of")

  deidentified_dir <- .report_fixture()
  manifest_path <- file.path(deidentified_dir, "summary", "summary_manifest.csv")
  manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
  manifest$summary_schema_version <- 2L
  readr::write_csv(manifest, manifest_path)
  expect_error(bcapture::render_epi_report(deidentified_dir), "Unsupported summary schema version")
  manifest$summary_schema_version <- 1L
  manifest$status <- "failed"
  readr::write_csv(manifest, manifest_path)
  expect_error(bcapture::render_epi_report(deidentified_dir),
    "Summary manifest status must be passed or review")
})

test_that("report preparation preserves summary products as the authority", {
  deidentified_dir <- .report_fixture()
  scalar_path <- file.path(deidentified_dir, "summary", "scalar_frequencies.csv")
  scalar <- readr::read_csv(scalar_path, show_col_types = FALSE)
  scalar$percent_of_answered[[1L]] <- 77
  readr::write_csv(scalar, scalar_path)
  products <- bcapture:::.epi_report_read_products(deidentified_dir, "summary")
  expect_equal(products$products$scalar_frequencies$percent_of_answered[[1L]], 77)
  expect_false("case_table_counts" %in% names(products$products))
})

test_that("synthetic summary and quality reports render atomically", {
  skip_if(Sys.which("quarto") == "", "Quarto unavailable")
  skip_if_not_installed("ggplot2")
  deidentified_dir <- .report_fixture()
  before <- .report_checksums(deidentified_dir)
  output_dir <- file.path(tempdir(), "epi reports with spaces")
  unlink(output_dir, recursive = TRUE, force = TRUE)
  summary_result <- bcapture::render_epi_report(
    deidentified_dir, report = "summary", output_dir = output_dir, quiet = TRUE
  )
  quality_result <- bcapture::render_epi_report(
    deidentified_dir, report = "quality", output_dir = output_dir, quiet = TRUE
  )
  expect_equal(summary_result$status, "rendered")
  expect_equal(quality_result$status, "rendered")
  summary_file <- file.path(output_dir, "initial_epi_summary.html")
  quality_file <- file.path(output_dir, "initial_epi_quality.html")
  expect_true(file.exists(summary_file))
  expect_true(file.exists(quality_file))
  expect_gt(file.info(summary_file)$size, 1000)
  expect_gt(file.info(quality_file)$size, 1000)
  expect_false(file.exists(paste0(summary_file, "_files")))
  expect_false(file.exists(paste0(quality_file, "_files")))
  summary_html <- paste(readLines(summary_file, warn = FALSE), collapse = "\n")
  quality_html <- paste(readLines(quality_file, warn = FALSE), collapse = "\n")
  expect_match(summary_html, "Initial Epi Descriptive Summary")
  expect_match(summary_html, "controlled analytical use")
  expect_match(summary_html, "Dataset overview")
  expect_match(summary_html, "Response categories")
  expect_match(summary_html, "Not applicable")
  expect_match(summary_html, "Numeric summaries")
  expect_match(summary_html, "Date summaries")
  expect_match(summary_html, "Percentages within a multiselect question need not sum to 100%")
  repeated_tables <- readr::read_csv(
    file.path(deidentified_dir, "summary", "repeated_table_summaries.csv"),
    show_col_types = FALSE
  )
  expect_equal(nrow(repeated_tables), 11L)
  expect_true(all(vapply(repeated_tables$table_name, grepl, logical(1),
    x = summary_html, fixed = TRUE)))
  expect_match(quality_html, "Initial Epi Data Quality Review")
  expect_match(quality_html, "controlled analytical use")
  expect_match(quality_html, "validation")
  expect_match(quality_html, "Numeric parsing quality")
  expect_match(quality_html, "Date parsing quality")
  expect_match(quality_html, "Missingness overview")
  expect_match(quality_html, "Privacy review summary")
  expect_match(quality_html, "No identifier leaks detected")
  known <- c("SYNTHETIC-PREMISES-", "PERSON-", "ORG-", "ENTITY-", "CONTACT-",
    "LOCATION-", "Owner One", "Second Owner", "Third Owner", "Visitor A",
    "Visitor B", "Visitor C")
  expect_false(any(vapply(known, grepl, logical(1), x = summary_html, fixed = TRUE)))
  expect_false(any(vapply(known, grepl, logical(1), x = quality_html, fixed = TRUE)))
  expect_false(grepl(normalizePath(deidentified_dir, winslash = "/", mustWork = TRUE),
    summary_html, fixed = TRUE))
  expect_false(grepl(normalizePath(deidentified_dir, winslash = "/", mustWork = TRUE),
    quality_html, fixed = TRUE))
  expect_identical(before, .report_checksums(deidentified_dir))
  manifest <- readr::read_csv(file.path(output_dir, "report_manifest.csv"), show_col_types = FALSE)
  expect_equal(nrow(manifest), 2L)
  expect_setequal(manifest$report_file, c("initial_epi_summary.html", "initial_epi_quality.html"))
  expect_error(bcapture::render_epi_report(
    deidentified_dir, report = "summary", output_dir = output_dir, quiet = TRUE
  ), "Report output already exists")
  expect_silent(bcapture::render_epi_report(
    deidentified_dir, report = "summary", output_dir = output_dir,
    overwrite = TRUE, quiet = TRUE
  ))
})
