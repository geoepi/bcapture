# Initial Epidemiological Interview form support

`bcapture` supports raw extraction from the interactive `HPAI Response / Initial
Epidemiological (Epi) Interview` form family developed against the May 28,
2024 template.

The form has four high-level sections:

- A — Premises Information
- B — Flock Information
- C — Trace-in and Trace-out
- D — Wild Bird and Environmental Information

This layer is an AcroForm extraction and provenance layer only. It does not
perform epidemiological semantic response mapping, question labeling,
validation, date or numeric parsing, or relational collation. Numeric button
export values remain raw because their meanings are field-specific. PDF choice
defaults and known prompts such as `Select or Type` and `Select (Ctrl for
multi)` are preserved in the canonical long table, but are excluded from
populated-only products.

Only interactive AcroForm PDFs are supported. Scanned and handwritten forms
remain `no_acroform_fields`; OCR and handwriting recognition are not part of
this implementation.

Repeated fields retain their original logical names and are not interpreted as
house, movement, worker, visitor, or environmental rows. A future versioned
dictionary and `collate_epi()` layer is expected to produce relational tables
such as `epi_forms`, `epi_houses`, `epi_ai_tests`, `epi_worker_visits`,
`epi_bird_movements`, `epi_egg_movements`, and `epi_environment`.

Never commit populated Initial Epi Interview forms or extracted respondent data.
Use synthetic test values only; local test inputs belong under ignored private
directories such as `local/`, `output/`, or `data-private/`.
