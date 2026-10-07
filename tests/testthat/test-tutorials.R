test_that("tutorials use package-safe synthetic assets and current APIs", {
  repo_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
  source_paths <- file.path(repo_root, "vignettes", c("initial-epi-workflow.Rmd", "bcap-workflow.Rmd"))
  installed_paths <- file.path(system.file("doc", package = "bcapture"), c("initial-epi-workflow.html", "bcap-workflow.html"))
  paths <- if (all(file.exists(source_paths))) source_paths else installed_paths
  expect_true(all(file.exists(paths)))
  expect_true(nzchar(system.file("extdata", "examples", "synthetic_initial_epi_print.pdf", package = "bcapture")))
  expect_true(nzchar(system.file("extdata", "examples", "synthetic_bcap_audit_print.pdf", package = "bcapture")))

  text <- vapply(paths, function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), character(1))
  expect_false(any(grepl("D:/HPAI_Data|D:\\\\HPAI_Data", text, perl = TRUE)))
  for (api in c("extract_epi", "collate_epi", "validate_epi", "deidentify_epi", "collate_epi_analysis", "extract_hpai", "validate_hpai", "deidentify_hpai", "collate_bcap", "field_metadata", "cross_reference_hpai")) {
    expect_true(any(grepl(api, text, fixed = TRUE)), info = api)
  }
  expect_true(all(vapply(text, function(x) grepl("private crosswalk", x, fixed = TRUE) && grepl("WITHHOLD|withheld", x, perl = TRUE), logical(1))))
})
