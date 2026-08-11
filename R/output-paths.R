audit_output_paths <- function(audit_id, out_dir) {
  audit_dir <- fs::path(out_dir, "audits", audit_id)
  list(
    dir = audit_dir,
    fields_long = fs::path(audit_dir, paste0(audit_id, "_fields_long.csv")),
    populated_fields_long = fs::path(audit_dir, paste0(audit_id, "_populated_fields_long.csv")),
    fields_wide = fs::path(audit_dir, paste0(audit_id, "_fields_wide.csv")),
    metadata = fs::path(audit_dir, paste0(audit_id, "_metadata.csv"))
  )
}

combined_output_paths <- function(out_dir) {
  combined_dir <- fs::path(out_dir, "combined")
  list(
    dir = combined_dir,
    fields_long = fs::path(combined_dir, "hpai_fields_long.csv"),
    populated_fields_long = fs::path(combined_dir, "hpai_populated_fields_long.csv"),
    audits_wide = fs::path(combined_dir, "hpai_audits_wide.csv"),
    metadata = fs::path(combined_dir, "hpai_metadata.csv"),
    manifest = fs::path(out_dir, "extraction_manifest.csv")
  )
}
