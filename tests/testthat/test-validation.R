test_that("argument validation catches missing inputs", {
  expect_error(extract_hpai(file.path(tempdir(), "missing"), tempdir()), "in_dir")
  input <- tempfile("bcapture-empty-")
  dir.create(input)
  expect_error(extract_hpai(input, tempfile("bcapture-out-")), "No PDF")
})

test_that("invalid output directories are rejected", {
  input <- tempfile("bcapture-input-")
  dir.create(input)
  file.create(file.path(input, "test.pdf"))
  invalid_out <- tempfile("bcapture-file-")
  file.create(invalid_out)
  expect_error(extract_hpai(input, invalid_out), "existing file")
})

test_that("audit IDs sanitize invalid filesystem characters and detect collisions", {
  expect_equal(bcapture:::sanitize_audit_id("Farm:2026"), "Farm_2026")
  input <- tempfile("bcapture-collision-")
  dir.create(input)
  dir.create(file.path(input, "one"))
  dir.create(file.path(input, "two"))
  file.create(file.path(input, "one", "Farm.pdf"))
  file.create(file.path(input, "two", "Farm.pdf"))
  expect_error(extract_hpai(input, tempfile("bcapture-out-"), recursive = TRUE), "duplicate audit IDs")
})

test_that("overwrite protection runs before extraction", {
  input <- tempfile("bcapture-input-")
  out <- tempfile("bcapture-output-")
  dir.create(input)
  dir.create(out)
  file.create(file.path(input, "test.pdf"))
  dir.create(file.path(out, "audits", "test"), recursive = TRUE)
  expect_error(extract_hpai(input, out), "Output already exists")
})

test_that("field normalization preserves /Off semantics", {
  rows <- bcapture:::field_rows_to_tibble(list(list(
    field_index = 1, page = 1, field = "x", value_raw = "/Off",
    value = "Off", is_populated = FALSE
  )))
  expect_equal(rows$value, "Off")
  expect_false(rows$is_populated)
})
