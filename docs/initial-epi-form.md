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

Interactive AcroForm PDFs are preferred. The versioned May 28, 2024 template
also supports selectable-text flattened PDFs through a spatial fallback that
uses page registration, blank-template subtraction, and rendered control-mark
comparison. Scanned, OCR-only, handwritten, and unrecognized flattened forms
remain unsupported and fail with an explicit classified failure.

Repeated fields retain their original logical names in raw extraction. The
versioned dictionary and `collate_epi()` layer interprets them as house,
movement, worker, visitor, equipment, and environmental structures, writing
relational tables such as `epi_forms`, `epi_houses`, `epi_ai_tests`,
`epi_worker_visits`, `epi_bird_movements`, and `epi_egg_movements`.

Never commit populated Initial Epi Interview forms or extracted respondent data.
Use synthetic test values only; local test inputs belong under ignored private
directories such as `local/`, `output/`, or `data-private/`.
