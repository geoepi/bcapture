hpai_state <- new.env(parent = emptyenv())

ensure_acroform_python <- function() {
  if (exists("module", envir = hpai_state, inherits = FALSE)) return(hpai_state$module)
  tryCatch(
    reticulate::py_require("pypdf"),
    error = function(error) stop(
      "bcapture needs Python and the `pypdf` package for PDF extraction. ",
      "Install/configure Python for reticulate, then retry. Original error: ",
      conditionMessage(error), call. = FALSE
    )
  )
  module_path <- system.file("python", "acroform.py", package = "bcapture")
  if (!nzchar(module_path) || !fs::file_exists(module_path)) {
    stop("The packaged Python module `inst/python/acroform.py` could not be found.", call. = FALSE)
  }
  hpai_state$module <- tryCatch(
    reticulate::import_from_path("acroform", path = dirname(module_path), delay_load = FALSE),
    error = function(error) stop(
      "bcapture could not initialize Python or import `pypdf`. ",
      "Check reticulate's Python configuration and install pypdf. Original error: ",
      conditionMessage(error), call. = FALSE
    )
  )
  hpai_state$module
}

ensure_hpai_python <- function() {
  ensure_acroform_python()
}

hpai_python_version <- function(module) {
  tryCatch(as.character(module$pypdf_version()), error = function(...) NA_character_)
}
