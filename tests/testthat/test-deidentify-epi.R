.deid_synthetic_output <- function(values = list(), schemas = c("schema_a")) {
  dictionary <- bcapture:::load_epi_dictionary()
  out_dir <- tempfile("epi-deid-source-")
  dir.create(file.path(out_dir, "combined"), recursive = TRUE)
  form_ids <- paste0("synthetic_", seq_along(schemas))
  field_rows <- purrr::map2_dfr(form_ids, schemas, function(form_id, schema) {
    fields <- dictionary$fields |>
      dplyr::transmute(
        form_id = form_id, form_type = "initial_epi", source_file = paste0(form_id, ".pdf"),
        source_relpath = paste0(form_id, ".pdf"), source_sha256 = paste0("sha-", form_id),
        field = raw_field, alternative_name, field_type = ifelse(response_type %in% c("coded", "choice"), "Btn", "Tx"),
        field_flags = NA_integer_, value_raw = NA_character_, value = NA_character_,
        default_value_raw = NA_character_, default_value = NA_character_, is_default_value = FALSE,
        states = NA_character_, options = NA_character_, is_multiselect = response_type == "multiselect",
        is_populated = FALSE, extraction_method = "synthetic", page = as.integer(source_page),
        form_schema_hash = schema, schema_group = schema
      )
    supplied <- values[[form_id]]
    if (is.null(supplied)) supplied <- list()
    for (raw_field in names(supplied)) {
      index <- match(raw_field, fields$field)
      if (is.na(index)) next
      fields$value_raw[[index]] <- as.character(supplied[[raw_field]])
      fields$value[[index]] <- sub("^/", "", as.character(supplied[[raw_field]]))
      fields$is_populated[[index]] <- TRUE
    }
    fields
  })
  manifest <- purrr::map2_dfr(form_ids, schemas, function(form_id, schema) tibble::tibble(
    form_id = form_id, form_type = "initial_epi", source_file = paste0(form_id, ".pdf"),
    source_relpath = paste0(form_id, ".pdf"), source_sha256 = paste0("sha-", form_id), status = "success",
    failure_type = NA_character_, error = NA_character_, number_of_pages = 12L,
    number_of_fields = sum(field_rows$form_id == form_id), number_of_widgets = 0L,
    number_of_populated_fields = sum(field_rows$form_id == form_id & field_rows$is_populated),
    form_schema_hash = schema, schema_group = schema, extraction_method = "synthetic",
    pypdf_version = NA_character_, extracted_at_utc = "2024-05-28T00:00:00Z"
  ))
  readr::write_csv(field_rows, file.path(out_dir, "combined", "epi_fields_long.csv"), na = "")
  readr::write_csv(dplyr::select(manifest, form_id, form_type, source_file, source_relpath, source_sha256, form_schema_hash, schema_group), file.path(out_dir, "combined", "epi_metadata.csv"), na = "")
  readr::write_csv(manifest, file.path(out_dir, "extraction_manifest.csv"), na = "")
  bcapture::collate_epi(out_dir, quiet = TRUE)
  out_dir
}

.deid_values <- function(owner = "Owner Person", premises = "Smith Farms") list(
  premid = "SYNTHETIC-PREMISES-001", premname = premises, premadd = "123 Main Street",
  premcnty = "Example County", ownname = owner, ownph = "555-222-3333", owneml = "owner@example.org",
  premlat = "30.123", premlong = "-84.123", p0004 = "Owner Person is mentioned in notes.",
  p0001 = "05/28/2024", p00010a = "House 1", p00010c = "100", p00010e = "10", p00010g = "05/20/2024",
  p00010h = "House 1 onset", p0133b = "Visitor Company / 555-222-3333", p0179oth = "Example Company Crew",
  weename = "Interviewer Person"
)

.deid_scan_reference <- function(outputs, sensitive_values, sensitive_context = list()) {
  normalize <- function(value) {
    value <- as.character(value)
    tolower(trimws(gsub("[[:space:]]+", " ", value)))
  }
  scalar_nonempty <- function(value) length(value) > 0L && !is.na(value[[1L]]) && nzchar(trimws(as.character(value[[1L]])))
  sensitive_values <- as.character(sensitive_values)
  sensitive_values <- unique(sensitive_values[!is.na(sensitive_values) & nzchar(trimws(sensitive_values))])
  global <- unique(normalize(sensitive_values))
  context <- list()
  context_names <- names(sensitive_context)
  if (is.null(context_names)) context_names <- rep(NA_character_, length(sensitive_context))
  if (length(sensitive_context) > 0L) for (j in seq_along(sensitive_context)) {
    key <- context_names[[j]]
    if (!scalar_nonempty(key)) next
    pieces <- strsplit(as.character(key[[1L]]), "\r", fixed = TRUE)[[1L]]
    raw_field <- if (length(pieces) >= 3L) paste(pieces[-c(1L, 2L)], collapse = "\r") else NA_character_
    if (!scalar_nonempty(raw_field)) raw_field <- NA_character_
    source <- unique(normalize(sensitive_context[[j]]))
    source <- source[!is.na(source)]
    if (length(source) == 0L) next
    context[[length(context) + 1L]] <- list(
      table_name = pieces[[1L]], column_name = if (length(pieces) >= 2L) pieces[[2L]] else NA_character_,
      raw_field = raw_field, source = source, numeric = grepl("^[0-9+(). -]+$", source)
    )
  }
  scoped <- function(table_name, column_name, raw_field, has_raw_field) {
    entries <- context[vapply(context, function(entry) identical(entry$table_name, table_name) && identical(entry$column_name, column_name), logical(1))]
    if (length(entries) == 0L) return(global)
    legacy <- unlist(lapply(entries, function(entry) if (is.na(entry$raw_field)) entry$source else character()), use.names = FALSE)
    fields <- entries[vapply(entries, function(entry) !is.na(entry$raw_field), logical(1))]
    if (!has_raw_field) return(unique(c(global, legacy, unlist(lapply(fields, `[[`, "source"), use.names = FALSE))))
    same <- fields[vapply(fields, function(entry) identical(entry$raw_field, raw_field), logical(1))]
    cross <- fields[vapply(fields, function(entry) !identical(entry$raw_field, raw_field), logical(1))]
    cross <- unlist(lapply(cross, function(entry) entry$source[!entry$numeric]), use.names = FALSE)
    unique(c(global, legacy, unlist(lapply(same, `[[`, "source"), use.names = FALSE), cross))
  }
  known_hit <- function(cell, sources) {
    sources <- unique(sources[!is.na(sources)])
    for (source in sources) {
      numeric_source <- grepl("^[0-9+(). -]+$", source)
      hit <- if (numeric_source || nchar(source) < 8L) identical(cell, source) else identical(cell, source) || grepl(source, cell, fixed = TRUE)
      if (isTRUE(hit)) return(TRUE)
    }
    FALSE
  }
  flags <- list()
  add <- function(table_name, column_name, x, i, leak_type, severity) {
    flags[[length(flags) + 1L]] <<- tibble::tibble(
      severity = severity, table_name = table_name, column_name = column_name,
      case_id = if ("case_id" %in% names(x)) as.character(x$case_id[[i]]) else NA_character_,
      raw_field = if ("raw_field" %in% names(x)) as.character(x$raw_field[[i]]) else NA_character_,
      canonical_name = if ("canonical_name" %in% names(x)) as.character(x$canonical_name[[i]]) else NA_character_,
      leak_type = leak_type
    )
  }
  for (table_name in names(outputs)) {
    x <- outputs[[table_name]]
    if (!is.data.frame(x)) next
    for (column in names(x)) {
      if (inherits(x[[column]], "Date") || (!is.character(x[[column]]) && !is.numeric(x[[column]]))) next
      values <- as.character(x[[column]])
      normalized <- normalize(values)
      for (i in which(!is.na(normalized))) {
        raw_field <- if ("raw_field" %in% names(x)) as.character(x$raw_field[[i]]) else NA_character_
        has_raw_field <- !is.na(raw_field) && nzchar(trimws(raw_field))
        if (known_hit(normalized[[i]], scoped(table_name, column, raw_field, has_raw_field))) add(table_name, column, x, i, "known_source_value", "ERROR")
        if (is.character(x[[column]]) && grepl("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}", values[[i]], ignore.case = TRUE, perl = TRUE)) add(table_name, column, x, i, "email_pattern", "WARNING")
        if (is.character(x[[column]]) && grepl("(?<![0-9])(?:\\+?1[ .-]?)?(?:[2-9][0-9]{2}[ .-]?[0-9]{3}[ .-]?[0-9]{4})(?![0-9])", values[[i]], perl = TRUE)) add(table_name, column, x, i, "phone_pattern", "WARNING")
      }
    }
  }
  if (length(flags) == 0L) tibble::tibble(severity = character(), table_name = character(), column_name = character(), case_id = character(), raw_field = character(), canonical_name = character(), leak_type = character()) else dplyr::bind_rows(flags) |> dplyr::distinct()
}

test_that("the analysis privacy policy covers every logical field", {
  dictionary <- bcapture:::load_epi_dictionary()
  rules <- bcapture:::.epi_deid_load_rules("2024-05-28", "analysis", dictionary)
  expect_equal(nrow(rules), 497L)
  expect_equal(length(unique(rules$raw_field)), 497L)
  expect_setequal(rules$raw_field, dictionary$fields$raw_field)
  expect_true(all(rules$action %in% c("retain", "drop", "pseudonymize", "coarsen", "review_remove")))
  expect_true(all(rules$privacy_class %in% c("direct_identifier", "quasi_identifier", "sensitive_free_text", "provenance_link", "non_identifier")))
  expect_true(all(c("dictionary_version", "schema_group", "raw_field", "canonical_name", "row_index") %in% bcapture:::.epi_deid_allowed_metadata))
})

test_that("deidentify_epi protects direct identifiers and preserves analytical types", {
  source_dir <- .deid_synthetic_output(list(synthetic_1 = .deid_values()))
  deid_dir <- tempfile("epi-deid-output-")
  crosswalk_dir <- tempfile("epi-deid-crosswalk-")
  source_forms <- readr::read_csv(file.path(source_dir, "collated", "epi_forms.csv"), show_col_types = FALSE)
  expect_message(
    result <- bcapture::deidentify_epi(source_dir, deid_dir, crosswalk_dir),
    "privacy audit passed"
  )
  expect_equal(result$status, "passed")
  expect_false(any(c("record_crosswalk", "entity_crosswalk", "crosswalk") %in% names(result)))
  output_forms <- readr::read_csv(file.path(deid_dir, "collated", "epi_forms.csv"), show_col_types = FALSE)
  output_responses <- readr::read_csv(file.path(deid_dir, "collated", "epi_responses_long.csv"), show_col_types = FALSE)
  output_houses <- readr::read_csv(file.path(deid_dir, "collated", "epi_houses.csv"), show_col_types = FALSE)
  expect_true(all(grepl("^CASE-[0-9]{6}$", output_forms$case_id)))
  expect_false("form_id" %in% names(output_forms))
  expect_false(any(c("source_file", "source_relpath", "source_sha256") %in% names(output_forms)))
  expect_equal(output_forms$premises_id, "PREMISES-000001")
  expect_equal(output_forms$premises_name, "PREMISES-000001")
  expect_true(is.na(output_forms$premises_address[[1L]]))
  expect_true(is.na(output_forms$premises_latitude[[1L]]))
  expect_true(is.na(output_forms$premises_longitude[[1L]]))
  expect_equal(output_forms$premises_county, "Example County")
  expect_true(is.na(output_forms$premises_owner_phone[[1L]]))
  expect_true(is.na(output_forms$premises_owner_email[[1L]]))
  expect_true(is.numeric(output_houses$birds_today))
  expect_true(inherits(output_houses$clinical_onset_date, "Date"))
  expect_equal(output_houses$clinical_onset_location, "LOCATION-000001")
  expect_equal(output_forms$company_crew_name, "ORG-000001")
  expect_equal(output_forms$interviewee_name, "PERSON-000001")
  expect_true(any(output_responses$raw_field == "p0004" & is.na(output_responses$raw_value)))
  expect_true(any(output_responses$raw_field == "premname" & grepl("^PREMISES-", output_responses$value)))
  expect_false(any(output_responses$raw_value == "Owner Person", na.rm = TRUE))
  expect_true(file.exists(file.path(deid_dir, "privacy", "deidentification_review.csv")))
  expect_true(nrow(readr::read_csv(file.path(deid_dir, "privacy", "deidentification_review.csv"), show_col_types = FALSE)) > 0L)
  expect_true(file.exists(file.path(crosswalk_dir, "record_crosswalk.csv")))
  expect_true(file.exists(file.path(crosswalk_dir, "entity_crosswalk.csv")))
  expect_false(dir.exists(file.path(deid_dir, "crosswalk")))
  expect_equal(source_forms, readr::read_csv(file.path(source_dir, "collated", "epi_forms.csv"), show_col_types = FALSE))
})

test_that("crosswalk reuse is stable and extension does not renumber", {
  crosswalk_dir <- tempfile("epi-deid-reuse-crosswalk-")
  source_one <- .deid_synthetic_output(list(synthetic_1 = .deid_values()))
  first_dir <- tempfile("epi-deid-reuse-one-")
  first <- bcapture::deidentify_epi(source_one, first_dir, crosswalk_dir, quiet = TRUE)
  first_forms <- readr::read_csv(file.path(first_dir, "collated", "epi_forms.csv"), show_col_types = FALSE)
  second_values <- .deid_values(owner = "Second Owner", premises = "Second Farm")
  second_values$premid <- "SYNTHETIC-PREMISES-002"
  source_two <- .deid_synthetic_output(list(synthetic_1 = .deid_values(), synthetic_2 = second_values), schemas = c("schema_a", "schema_a"))
  second_dir <- tempfile("epi-deid-reuse-two-")
  second <- bcapture::deidentify_epi(source_two, second_dir, crosswalk_dir, quiet = TRUE)
  second_forms <- readr::read_csv(file.path(second_dir, "collated", "epi_forms.csv"), show_col_types = FALSE)
  expect_equal(second_forms$case_id[[1L]], first_forms$case_id[[1L]])
  expect_equal(second_forms$premises_id[[1L]], first_forms$premises_id[[1L]])
  expect_equal(second_forms$case_id[[2L]], "CASE-000002")
  expect_equal(second_forms$premises_id[[2L]], "PREMISES-000002")
  record_crosswalk <- readr::read_csv(file.path(crosswalk_dir, "record_crosswalk.csv"), show_col_types = FALSE)
  expect_equal(nrow(record_crosswalk), 2L)
  expect_true(all(grepl("^CASE-[0-9]{6}$", record_crosswalk$case_id)))
  expect_equal(first$status, "passed")
  expect_equal(second$status, "passed")
})

test_that("path safety rejects nested and version-controlled crosswalks", {
  source_dir <- tempfile("epi-deid-path-source-"); dir.create(source_dir, recursive = TRUE)
  deid_dir <- tempfile("epi-deid-path-output-")
  expect_error(bcapture::deidentify_epi(source_dir, deid_dir, file.path(deid_dir, "crosswalk")), "physically separate")
  existing_output <- tempfile("epi-deid-existing-output-")
  dir.create(file.path(existing_output, "privacy"), recursive = TRUE)
  file.create(file.path(existing_output, "privacy", "deidentification_manifest.csv"))
  expect_error(
    bcapture::deidentify_epi(source_dir, deid_dir, file.path(existing_output, "crosswalk")),
    "existing de-identified output"
  )
  git_crosswalk <- file.path(getwd(), "private-crosswalk-test")
  if (is.na(bcapture:::.epi_deid_git_root(normalizePath(git_crosswalk, winslash = "/", mustWork = FALSE)))) skip("Git worktree is not present in the installed package check copy")
  expect_error(bcapture::deidentify_epi(source_dir, deid_dir, git_crosswalk), "Git working tree")
})

test_that("known source values are confirmed leaks and pseudonymized output is clean", {
  leaked <- bcapture:::.epi_deid_scan(list(epi_forms = tibble::tibble(case_id = "CASE-000001", premises_name = "Secret Farm")), "Secret Farm")
  clean <- bcapture:::.epi_deid_scan(list(epi_forms = tibble::tibble(case_id = "CASE-000001", premises_name = "PREMISES-000001")), "Secret Farm")
  expect_true(any(leaked$severity == "ERROR"))
  expect_equal(nrow(clean), 0L)
})

test_that("EAV known-value scanning scopes numeric sources by raw field", {
  sep <- intToUtf8(13)
  context <- list()
  context[[paste("epi_responses_long", "raw_value", "numeric_field", sep = sep)]] <- c("100", "100")
  context[[paste("epi_responses_long", "raw_value", "short_field", sep = sep)]] <- "Secret"
  context[[paste("epi_responses_long", "raw_value", "long_field", sep = sep)]] <- "Sensitive Farm"
  context[[paste("epi_responses_long", "raw_value", "whitespace_field", sep = sep)]] <- paste0("Mixed", intToUtf8(11L), "Whitespace")
  context[[paste("epi_repeated", "value", sep = sep)]] <- "100"
  outputs <- list(
    epi_responses_long = tibble::tibble(
      case_id = c("same-numeric", "cross-numeric", "cross-short", "cross-long", "cross-whitespace", "missing-field", "global-text", "pattern"),
      raw_field = c("numeric_field", "other_field", "other_field", "other_field", "other_field", NA_character_, "other_field", "other_field"),
      canonical_name = "synthetic_value",
      raw_value = c("100", "100", "Secret", "Sensitive Farm note", paste0(" Mixed", intToUtf8(12L), "Whitespace "), "100", "Global Identity", "safe"),
      date_value = as.Date(rep("2024-05-28", 8L)),
      numeric_value = c(1, 2, 3, 4, 5, 6, 7, 8),
      free_text = c(NA_character_, "", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, "owner@example.org / 555-222-3333")
    ),
    epi_repeated = tibble::tibble(case_id = c("legacy-a", "legacy-b"), raw_field = c("a", "b"), value = c("100", "100"))
  )
  sensitive_values <- c("Global Identity", as.character(as.Date("2024-05-28")))
  actual <- bcapture:::.epi_deid_scan(outputs, sensitive_values, context)
  expected <- .deid_scan_reference(outputs, sensitive_values, context)
  expect_equal(actual, expected)
  known <- actual[actual$leak_type == "known_source_value", , drop = FALSE]
  expect_true(any(known$case_id == "same-numeric" & known$raw_field == "numeric_field"))
  expect_false(any(known$case_id == "cross-numeric"))
  expect_true(any(known$case_id == "cross-short"))
  expect_true(any(known$case_id == "cross-long"))
  expect_true(any(known$case_id == "cross-whitespace"))
  expect_true(any(known$case_id == "missing-field"))
  expect_true(any(known$case_id == "global-text"))
  expect_equal(sum(actual$leak_type == "email_pattern"), 1L)
  expect_equal(sum(actual$leak_type == "phone_pattern"), 1L)
  expect_false(any(actual$case_id == "pattern" & actual$column_name == "date_value"))
  expect_equal(sum(actual$case_id %in% c("legacy-a", "legacy-b") & actual$leak_type == "known_source_value"), 2L)
})

test_that("malformed or unnamed contexts are NA-safe and retain global matching", {
  sep <- intToUtf8(13L)
  malformed <- list("100", NA_character_, "")
  names(malformed) <- c("epi_responses_long", NA_character_, paste("epi_responses_long", "raw_value", "other_field", sep = sep))
  output <- tibble::tibble(raw_field = c(NA_character_, "other_field"), raw_value = c("Global Identity", "100"))
  actual <- bcapture:::.epi_deid_scan(list(epi_responses_long = output), "Global Identity", malformed)
  expect_true(any(actual$leak_type == "known_source_value" & is.na(actual$raw_field)))
  expect_false(any(actual$leak_type == "known_source_value" & actual$raw_field == "other_field", na.rm = TRUE))
  expect_equal(actual, .deid_scan_reference(list(epi_responses_long = output), "Global Identity", malformed))
})

test_that("strict mode refuses finalization after an injected retained identifier", {
  source_dir <- .deid_synthetic_output(list(synthetic_1 = .deid_values()))
  deid_dir <- tempfile("epi-deid-injected-output-")
  crosswalk_dir <- tempfile("epi-deid-injected-crosswalk-")
  original_scan <- bcapture:::.epi_deid_scan
  injected_value <- "Synthetic retained free identifier"
  injected_scan <- function(outputs, sensitive_values, sensitive_context = list()) {
    outputs[[1L]]$synthetic_retained_identifier <- injected_value
    original_scan(outputs, c(sensitive_values, injected_value), sensitive_context)
  }
  testthat::local_mocked_bindings(.epi_deid_scan = injected_scan, .package = "bcapture")
  expect_error(
    bcapture::deidentify_epi(source_dir, deid_dir, crosswalk_dir, quiet = TRUE),
    "confirmed direct-identifier leaks"
  )
  expect_false(dir.exists(deid_dir))
  expect_false(dir.exists(crosswalk_dir))
})
