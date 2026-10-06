.feature_analysis_fixture <- function() {
  expressions <- parse(testthat::test_path("test-features-epi.R"))
  fixture_env <- new.env(parent = globalenv())
  eval(expressions[[1L]], fixture_env)
  fixture_env$.feature_fixture()
}

.feature_analysis_checksums <- function(path, include_summary = FALSE) {
  roots <- c("collated", "validation", "privacy", "summary", "reports", "features")
  files <- unlist(lapply(file.path(path, roots), function(root) {
    if (!dir.exists(root)) return(character())
    found <- fs::dir_ls(root, recurse = TRUE, type = "file")
    if (!include_summary) found <- found[!grepl(
      "[/\\\\]features[/\\\\]summary[/\\\\]", found
    )]
    found
  }), use.names = FALSE)
  stats::setNames(as.character(tools::md5sum(files)), fs::path_rel(files, path))
}
