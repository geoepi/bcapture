test_that("output paths use the audits layer", {
  paths <- bcapture:::audit_output_paths("pdf1", "out")
  expect_equal(paths$dir, fs::path("out", "audits", "pdf1"))
  expect_equal(fs::path_file(paths$fields_long), "pdf1_fields_long.csv")
  expect_equal(fs::path_file(paths$metadata), "pdf1_metadata.csv")
})

test_that("combined paths are stable", {
  paths <- bcapture:::combined_output_paths("out")
  expect_equal(paths$audits_wide, fs::path("out", "combined", "hpai_audits_wide.csv"))
  expect_equal(paths$manifest, fs::path("out", "extraction_manifest.csv"))
})
