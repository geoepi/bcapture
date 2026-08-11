dir.create(
  "D:/HPAI_Data/bcapture_test/input",
  recursive = TRUE,
  showWarnings = FALSE
)

result <- extract_hpai(
  in_dir  = "D:/HPAI_Data/bcapture_test/input",
  out_dir = "D:/HPAI_Data/bcapture_test/output"
)

result

manifest <- read.csv(
  "D:/HPAI_Data/bcapture_test/output/extraction_manifest.csv"
)

manifest

extract_hpai(
  in_dir  = "D:/HPAI_Data/bcapture_test/batch_input",
  out_dir = "D:/HPAI_Data/bcapture_test/batch_output"
)



library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

out_dir <- "D:/HPAI_Data/bcapture_test/batch_output"

field_files <- list.files(
  file.path(out_dir, "audits"),
  pattern = "_fields_long\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

fields <- map_dfr(
  field_files,
  ~ read_csv(.x, show_col_types = FALSE)
)

fields |>
  group_by(audit_id) |>
  summarise(
    n_fields = n(),
    n_unique_fields = n_distinct(field),
    n_field_types = n_distinct(field_type),
    .groups = "drop"
  )

field_presence <- fields |>
  distinct(audit_id, field) |>
  mutate(present = TRUE) |>
  pivot_wider(
    names_from = audit_id,
    values_from = present,
    values_fill = FALSE
  )

field_presence |>
  filter(if_any(-field, ~ !.x))



field_types <- fields |>
  distinct(audit_id, field, field_type) |>
  pivot_wider(
    names_from = audit_id,
    values_from = field_type
  )

field_types |>
  filter(
    if_any(
      -field,
      ~ .x != first(c_across(-field))
    )
  )

type_differences <- fields |>
  distinct(audit_id, field, field_type) |>
  group_by(field) |>
  summarise(
    n_types = n_distinct(field_type),
    types = paste(
      sort(unique(field_type)),
      collapse = " | "
    ),
    .groups = "drop"
  ) |>
  filter(n_types > 1)

type_differences

state_differences <- fields |>
  distinct(
    audit_id,
    field,
    field_type,
    states
  ) |>
  group_by(field, field_type) |>
  summarise(
    n_state_versions = n_distinct(states, na.rm = FALSE),
    state_versions = paste(
      unique(states),
      collapse = " || "
    ),
    .groups = "drop"
  ) |>
  filter(n_state_versions > 1)

state_differences

fields |>
  filter(field_type == "/Btn") |>
  distinct(
    audit_id,
    field,
    states
  ) |>
  arrange(field, audit_id) |>
  print(n = 200)


field_order <- fields |>
  select(
    audit_id,
    field_index,
    field
  ) |>
  arrange(
    audit_id,
    field_index
  )

field_order |>
  group_by(field) |>
  summarise(
    n_positions = n_distinct(field_index),
    positions = paste(
      sort(unique(field_index)),
      collapse = ", "
    ),
    .groups = "drop"
  ) |>
  filter(n_positions > 1)



library(dplyr)
library(purrr)
library(readr)
library(tidyr)

out_dir <- "D:/HPAI_Data/bcapture_test/batch_output"

field_files <- list.files(
  file.path(out_dir, "audits"),
  pattern = "_fields_long\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# IMPORTANT: exclude populated-field derivatives
field_files <- field_files[
  !grepl(
    "_populated_fields_long\\.csv$",
    field_files
  )
]

basename(field_files)

fields <- map_dfr(
  field_files,
  ~ read_csv(.x, show_col_types = FALSE)
)

fields |>
  group_by(audit_id) |>
  summarise(
    n_fields = n(),
    n_unique_fields = n_distinct(field),
    n_field_types = n_distinct(field_type),
    .groups = "drop"
  )

field_presence <- fields |>
  distinct(audit_id, field) |>
  mutate(present = TRUE) |>
  pivot_wider(
    names_from = audit_id,
    values_from = present,
    values_fill = FALSE
  )

field_presence_differences <- field_presence |>
  filter(if_any(-field, ~ !.x))

field_presence_differences

fields |>
  filter(
    grepl("17e", field)
  ) |>
  select(
    audit_id,
    field_index,
    page,
    field,
    field_type,
    value_raw,
    value,
    states,
    is_populated
  ) |>
  arrange(
    audit_id,
    field
  ) |>
  print(n = Inf)




######################################################## DIAG 2
library(readr)
library(dplyr)

diag_dir <- "D:/HPAI_Data/bcapture_test/batch_output_v2/diagnostics"

schema_summary <- read_csv(
  file.path(diag_dir, "schema_summary.csv"),
  show_col_types = FALSE
)

schema_summary


schema_summary |>
  select(
    audit_id,
    schema_group,
    form_schema_hash,
    number_of_fields,
    number_of_widgets,
    number_of_populated_fields,
    number_of_field_types
  )

schema_groups <- read_csv(
  file.path(diag_dir, "schema_groups.csv"),
  show_col_types = FALSE
)

schema_groups

presence <- read_csv(
  file.path(
    diag_dir,
    "field_presence_differences.csv"
  ),
  show_col_types = FALSE
)

presence


types <- read_csv(
  file.path(
    diag_dir,
    "field_type_differences.csv"
  ),
  show_col_types = FALSE
)

types


states <- read_csv(
  file.path(
    diag_dir,
    "field_state_differences.csv"
  ),
  show_col_types = FALSE
)

states

widgets <- read_csv(
  file.path(
    diag_dir,
    "widget_encoding_differences.csv"
  ),
  show_col_types = FALSE
)

widgets


d <- diagnose_hpai(
  out_dir = "D:/HPAI_Data/bcapture_test/batch_output_v2"
)

names(d)

d$field_presence_differences
d$field_type_differences
d$field_state_differences
d$widget_differences


manifest <- read_csv(
  "D:/HPAI_Data/bcapture_test/batch_output_v2/extraction_manifest.csv",
  show_col_types = FALSE
)

manifest |>
  select(
    audit_id,
    status,
    number_of_fields,
    number_of_widgets,
    number_of_populated_fields,
    form_schema_hash,
    schema_group
  )


















