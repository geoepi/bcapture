# Initial Epidemiological Interview form support

`bcapture` supports raw extraction from the interactive `HPAI Response / Initial
Epidemiological (Epi) Interview` form family developed against the May 28,
2024 template.

The form has four high-level sections:

- A — Premises Information
- B — Flock Information
- C — Trace-in and Trace-out
- D — Wild Bird and Environmental Information

The extraction layer is an AcroForm and provenance layer. Semantic response
mapping and relational collation are provided separately by `collate_epi()`
using the versioned 2024-05-28 dictionary. Numeric button export values remain
raw in the extraction products because their meanings are field-specific. PDF
choice defaults and known prompts such as `Select or Type` and `Select (Ctrl
for multi)` are preserved in the canonical long table, but are excluded from
populated-only products.

Only interactive AcroForm PDFs are supported. Scanned and handwritten forms
remain `no_acroform_fields`; OCR and handwriting recognition are not part of
this implementation.

Repeated fields retain their original logical names in raw extraction. The
versioned dictionary and `collate_epi()` layer interprets them as house,
movement, worker, visitor, equipment, and environmental structures, writing
relational tables such as `epi_forms`, `epi_houses`, `epi_ai_tests`,
`epi_worker_visits`, `epi_bird_movements`, and `epi_egg_movements`.

Never commit populated Initial Epi Interview forms or extracted respondent data.
Use synthetic test values only; local test inputs belong under ignored private
directories such as `local/`, `output/`, or `data-private/`.
