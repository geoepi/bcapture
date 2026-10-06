test_that("synthetic OCR fixtures are present and production-independent", {
  fixture_dir <- testthat::test_path("fixtures", "ocr")
  expected <- file.path(
    fixture_dir,
    paste0("initial_epi_", rep(c("clean", "moderate", "borderline"), each = 2), "_page_", rep(c("01", "06"), 3), ".jpg")
  )
  expect_true(all(file.exists(expected)))
  expect_true(file.exists(file.path(fixture_dir, "README.md")))
})

test_that("optional Tesseract synthetic evaluator is fail-safe", {
  tesseract <- Sys.getenv("BCAPTURE_TESSERACT", unset = Sys.which("tesseract"))
  python <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  testthat::skip_if(!nzchar(tesseract) || !file.exists(tesseract), "Tesseract is unavailable")
  testthat::skip_if(!nzchar(python) || !file.exists(python), "Configured Python runtime is unavailable")

  repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  fixture_dir <- file.path(repo_root, "tests", "testthat", "fixtures", "ocr")
  script <- file.path(repo_root, "scripts", "evaluate_tesseract_synthetic.py")
  template <- file.path(repo_root, "inst", "extdata", "templates", "initial_epi", "2024-05-28", "blank_printed.pdf")
  testthat::skip_if_not(file.exists(script), "Source-only OCR evaluator is unavailable in installed package")
  output <- tempfile("bcapture-tesseract-synthetic-", fileext = ".json")
  status <- system2(
    python,
    c(
      script,
      "--fixtures", fixture_dir,
      "--template", template,
      "--tesseract", tesseract,
      "--output", output
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  if (!is.null(attr(status, "status"))) {
    testthat::fail("Optional Tesseract synthetic evaluator failed to run")
  }
  result <- jsonlite::read_json(output, simplifyVector = TRUE)
  expect_equal(nrow(result$levels), 3L)
  expect_true(all(vapply(result$levels$pages, function(page) all(page$tokens > 0), logical(1))))
  anchor_rates <- unlist(lapply(result$levels$pages, function(page) page$anchors_recovered / page$anchors_expected))
  expect_true(all(anchor_rates >= 0.5))
})
