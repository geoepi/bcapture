hpai_state <- new.env(parent = emptyenv())
spatial_state <- new.env(parent = emptyenv())

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

ensure_spatial_python <- function() {
  if (exists("module", envir = spatial_state, inherits = FALSE)) return(spatial_state$module)
  tryCatch(
    reticulate::py_require(c("pypdf", "pdfplumber", "pypdfium2")),
    error = function(error) stop(
      "Flattened PDF extraction needs Python packages `pypdf`, `pdfplumber`, and `pypdfium2`. ",
      "Install/configure Python for reticulate, then retry. Original error: ",
      conditionMessage(error), call. = FALSE
    )
  )
  module_path <- system.file("python", "spatial_pdf.py", package = "bcapture")
  if (!nzchar(module_path) || !fs::file_exists(module_path)) {
    stop("The packaged Python module `inst/python/spatial_pdf.py` could not be found.", call. = FALSE)
  }
  spatial_state$module <- tryCatch(
    reticulate::import_from_path("spatial_pdf", path = dirname(module_path), delay_load = FALSE),
    error = function(error) stop(
      "bcapture could not initialize the spatial PDF Python dependencies. ",
      "Check reticulate's Python configuration and install pypdf, pdfplumber, and pypdfium2. Original error: ",
      conditionMessage(error), call. = FALSE
    )
  )
  spatial_state$module
}

spatial_template_dir <- function(form_type) {
  location <- switch(
    form_type,
    initial_epi = file.path("initial_epi", "2024-05-28"),
    bcap = file.path("bcap", "2025-12-08"),
    stop("Unsupported spatial template family: ", form_type, call. = FALSE)
  )
  path <- system.file("extdata", "templates", location, package = "bcapture")
  if (!nzchar(path)) path <- file.path("inst", "extdata", "templates", location)
  if (!dir.exists(path)) stop("The canonical spatial template is unavailable: ", path, call. = FALSE)
  path
}

extract_spatial_pdf <- function(pdf_file, form_type) {
  module <- ensure_spatial_python()
  parsed <- tryCatch(
    reticulate::py_to_r(module$extract_spatial(pdf_file, spatial_template_dir(form_type))),
    error = function(error) stop(error)
  )
  parsed
}

hpai_python_version <- function(module) {
  tryCatch(as.character(module$pypdf_version()), error = function(...) NA_character_)
}
