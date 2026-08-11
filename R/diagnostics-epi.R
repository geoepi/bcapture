#' Diagnose Initial Epidemiological Interview schemas
#'
#' @param out_dir An existing Epi extraction directory.
#' @param write Write diagnostic CSV products and a Markdown report.
#' @param quiet Suppress concise diagnostic messages.
#' @return A named list of diagnostic tibbles.
#' @export
diagnose_epi <- function(out_dir, write = TRUE, quiet = FALSE) {
  diagnose_acroform_batch(out_dir, id_col = "form_id", form_label = "Initial Epi", prefix = "epi", container = "forms", write = write, quiet = quiet)
}
