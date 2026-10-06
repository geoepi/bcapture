# Initial Epi semantic-validation and privacy audit

Audit date: 2026-10-05. This report contains counts, public schema names, and
structural descriptions. Production values, identifiers, filenames, paths,
candidate records, and crosswalk contents remain outside the repository.

## Governance and repository

`D:\Github\.codex-local\EXECUTION_GOVERNANCE.md` was read and followed under
SAE. A designated GPT-5.6 Sol supervisor reviewed Level-2 decisions; delegated
implementation used GPT-5.6 Luna at its highest available reasoning effort.
The supervisor approved strict multiselect container decoding, closed numeric
normalization, and the bounded logical-field privacy-scoping correction.
Final SAE disposition: **ACCEPTED**. The Level-2 decisions were escalated to
the designated supervisor and accepted after regression and production evidence
was complete. No Level-3 governing change or human escalation was required.
No privacy policy, field action, canonical schema, public API, warning severity,
acceptance threshold, or scientific interpretation was changed. Ambiguous
dates, coordinate notation, and source entries were retained for review.

Branch: `feature/epi-semantic-deid-audit`, based on completed extraction commit
`b1f6a9e`. The extraction commits `71cb224`, `937c5c2`, and `b1f6a9e` were
preserved. The original checkout's unrelated script edit was preserved.
Implementation commits: `f087a22` (semantic normalization/provenance) and
`a3f06ae` (privacy scoping/performance). This sanitized report is committed
separately. The feature checkout was verified clean at task close; the original
checkout still contains its pre-existing script edit. No unrelated local work
was staged, and nothing is merged to main.

## Reproduced semantic baseline

The existing successful-extraction subset contains 40 of the 46 inputs. The
three image-only and three incompatible-signature inputs remain excluded.
The local development package reproduced 172 findings: **170 WARNING and two
ERROR findings in one form**. Findings match the preceding run after accounting
for CSV missing-value encoding and R storage types. Form statuses were 20 valid,
19 review, and one error. All 20 forms with findings are represented in the
redacted case/rule inventory. The count-only appendix below summarizes public
fields and opaque audit case labels; the private linkage is not committed.

| Baseline rule | Warnings | Affected forms | Diagnosis and disposition |
| --- | ---: | ---: | --- |
| `no_with_followup_data` | 60 | 15 | KEEP/DOCUMENT. Twelve scalar follow-ups (three explicit missingness; nine other entries) and 48 table follow-ups accompany No responses. All 48 tables contain only explicit missingness entries; 21 warnings overlap an incomplete-table finding. These are separate constraints, with cascading rather than duplicate evidence. |
| `yes_followup_missing` | 5 | 4 | KEEP / SOURCE DATA. A Yes response lacks a populated declared follow-up. |
| `expected_date_unparseable` | 7 | 5 | KEEP / REVIEW. Three dates omit the year, one uses a dash-delimited representation, and three contain mixed content. No date, year, or chronology was inferred. |
| `expected_numeric_unparseable` | 31 | 7 | FIX NORMALIZATION for 13 exactly grouped numbers and four leading decimals. KEEP / REVIEW for 14 remaining mixed entries, including two degree-bearing coordinate strings. Units and coordinate conversion were not inferred. |
| `incomplete_table_record` | 67 | 3 | KEEP/DOCUMENT. Sixty-six records have explicit missingness text in noncore cells; one has location/context text without required core evidence. The registered population/completeness semantics remain unchanged. |

The 67 incomplete-record findings cover shared equipment (37), egg movements
(nine), bird movements (six), crews/imported materials/manure destinations/
worker visits (three each), mortality disposal (two), and houses (one).
They arose from AcroForm inputs rather than the spatial route. The large count
is repeated application of a correct registered rule to sparse records.

| Error field | Error class | Root cause | Package action |
| --- | --- | --- | --- |
| `p0139d` / `visitors_other_livestock_premises_type` | `unknown_multiselect_option` | The extracted raw and normalized representations were identical quoted list literals containing one registered string option. Collation emitted the brackets/quotes as part of the item label. | FIX NORMALIZATION: decode the string-list representation at collation; preserve raw extraction and source values. |
| `p0140d` / `visitors_other_livestock_premises_type` | `unknown_multiselect_option` | The same representation defect occurred in another visitor row of the same form. The underlying decoded choice is already in the versioned registry. | Apply the same generic decoder; do not extend the option registry. |

For each error, the constraint is that every derived selection must match the
versioned option registry. The observed raw extraction and normalized scalar
were a one-element quoted list, structurally `['<registered choice>']`;
before correction the derived item still contained the container punctuation.
After correction the item is the registered choice, with its raw representation
unchanged. Both errors are resolved. Neither is an extraction or source-data
error, and neither requires a severity change.

## General normalization fixes

The multiselect decoder recognizes only bounded, fully consumed top-level
string lists/arrays, without evaluating expressions. Quoted strings and escapes
are decoded; nested arrays, nonstrings, malformed syntax, and trailing content
remain unknown options and still fail validation. Ordinary delimited values
retain their existing behavior. Raw extracted values are never rewritten.

Numeric normalization accepts the existing signed integer/decimal grammar plus
exact groups of three comma-separated digits and signed decimals without a
leading zero. Commas are removed only after the complete input matches the
closed grammar. Malformed grouping, decimal commas, trailing decimal points,
exponents, units, percentages, and degree-bearing coordinates remain
unparseable. Existing range and negative-value rules still apply.

Validation provenance lookup also excludes missing dictionary indexes before
matching a repeated-table field. This restores the correct raw-field annotation
on existing findings without changing any rule, severity, or data value.

## Strict privacy failure and analogous risk

The original strict public workflow was rerun with its unchanged scanner and
unchanged finalization gate. It reproduced the confirmed-leak error and blocked
finalization: zero safe files, zero crosswalk files, and zero temporary output
siblings remained. A reversible diagnostic wrapper retained the candidate only in private
storage and allowed the original scanner and failure path to execute normally.

The diagnostic trace identified **165 `known_source_value` flags**: 55 logical
quantity cells in 30 cases, repeated in `raw_value`, `value`, and
`response_label`. They cover six existing retained quasi-identifier fields:
eggs present, eggs laid last week, baseline mortality, initial sampling
mortality, carcass-bin distance, and manure distance. No global source identity
matched these cells; email and phone heuristic diagnostics returned zero flags.

The origin is two sensitive-free-text fields, `p0107` (`broken_eggs_disposal`)
and `p0108` (`wash_water_disposal`), in one case. Their existing `review_remove`
rules correctly withhold their contents. A single numeric-like token entered
the shared long-table sensitivity pool and collided with unrelated retained
quantities. The scanner lost logical field identity when it treated all
`raw_value` rows as one output field. This is a false-positive matching defect;
the traced numeric contents did not survive in their withheld source fields.

The corpus audit checked all populated retained long-table entries: 76 logical
fields and 2,221 cells. The observed collision mechanism affected six fields,
55 cells, and 30 records. The two originating cells belong to one source
record. This is a reusable structural defect rather than an identifier-specific
exception.

The identifier classification is a **numeric-like token in withheld sensitive
free text**, rather than a surviving person, premises, address, telephone,
email, or case identifier. The failure-mode classification is loss of logical
field context during privacy matching. The field removal rules, their ordering,
serialization, and crosswalk mapping were already correct. No analogous
non-numeric or global identifier survival was found by the strict scan.

The correction scopes long-table context by output table, output column, and
raw field. Same-field numeric survivors remain errors. Global values and
cross-field nonnumeric text remain detectable using the existing matching
criteria. Missing field identity falls back to broader scanning; legacy scoped
callers remain supported. Email/phone checks and the strict finalization gate
are unchanged. No production token is whitelisted.

Repeated normalization and cell/source comparisons were also replaced by
cached, vectorized comparisons. A scalar test oracle checks equivalent flag
sets and detection ordering across legacy and corrected scopes, all matching
classes, metadata gaps, missing values, dates, and heuristic patterns.
Synthetic regressions cover the formerly flagged cross-field numeric collision,
same-field numeric survival, legacy scopes, nonnumeric cross-field survival,
missing metadata, whitespace normalization, heuristic detection, and strict
failure before finalization. All examples were independently constructed;
none reproduces a production identifier or record.
The optimized legacy-scope scan reproduced exactly the original 165 confirmed
and zero potential flags. Corrected logical scoping produced zero errors and
zero warnings. The original scalar scan took 1,822.70 seconds; the optimized
legacy scan took 7.97 seconds and corrected scan 7.90 seconds in local runs.
These are observed timings, not a controlled benchmark. The complete final
deidentification run took 42.89 seconds, including a 7.85-second privacy scan.
The change improves this step without changing extraction execution.

## Validation and final output status

Combined focused validation passed 36 tests and 283 expectations. The complete
package suite passed 101 tests and 781 expectations, including report rendering
and extraction regressions. Both runs had zero failures, errors, warnings, or
skips. Four host locale warnings occurred during R startup; these are distinct
from captured package-test warnings.
Final semantic validation has **153 warnings and zero errors** across 40 forms:
20 valid and 20 review. The remaining counts are 60 No/follow-up, five
Yes/missing-follow-up, seven date-parse, 14 numeric-parse, and 67 incomplete-table
warnings. Seventeen numeric warnings were resolved by normalization. Both
multiselect errors resolved; no warning was suppressed or reclassified.

The public strict deidentification workflow processed all 40 successful forms
and reported **zero privacy errors and zero privacy warnings before
finalization**. `epi_safe` was populated with 23 files, including 14 analytical
CSVs; `epi_crosswalk` was populated separately with four files. An independent
boundary check audited all 23 safe files and found zero crosswalk files,
forbidden provenance columns, raw-provenance matches, withheld source cells,
or malformed pseudonyms in safe output. Confidential identifiers represented
by the registered policy were removed or pseudonymized; crosswalk content
remains outside safe output.

The independent extraction audit has already preserved Initial Epi 457/457
values and 148/148 buttons, BCAP 218/218 values and 155/155 buttons, and zero
blank-template false positives. No extraction implementation was changed.
All 46/46 source PDF byte hashes, sizes, and modification times remained
unchanged. All 39/39 previously stable extracted field-table fingerprints
remained unchanged. The 40th table is the already recovered Portfolio form;
there was no new extraction correction or reopened structural failure.

The existing record-map precheck found 40 cases, 40 premises keys, and 40
premises pseudonyms, with no conflicting linkage. Reuse preserves both case and
premises assignment. Crosswalk coverage, pseudonym formats, and case/premises
linkage passed independent validation. An isolated strict reuse run preserved
record/entity mappings, matched the baseline crosswalk, and reproduced the
hashes of all 14 analytical CSVs. Live safe-file hashes remained unchanged.
Safe output was permitted only after zero blocking semantic errors and a
successful strict privacy audit with zero errors/warnings. Synthetic leak
tests also confirm that neither safe output nor crosswalk is finalized on
strict failure.

Automatic approval review rejected a direct overwrite reuse test against the
finalized safe directory because it could remove and rename finalized output.
The isolated reuse check passed instead, without mutating the live safe
directory or crosswalk. No further live overwrite was attempted.

## Data boundary

Source PDFs are read in place. No source PDFs, confidential extracted records,
production safe outputs, production crosswalks, or real identifiers entered Git
or synthetic tests. All newly added fixtures are synthetic. Private candidate
retention was limited to deriving redacted diagnostics and regression evidence.
The private candidate snapshot was removed, and no isolated reuse copies
remain. Count-only diagnostic evidence remains in confidential local storage.
No OCR, unsupported-template interpretation,
manual production correction, or schema redesign was performed.

## Count-only baseline inventory

Field-level counts use public dictionary names. Audit labels below are opaque
diagnostic ordinals; source case identifiers and the private linkage are excluded.
Counts describe findings, without any source values or production record content.

| Rule | Raw field | Canonical field | Count | Forms |
| --- | --- | --- | ---: | ---: |
| `unknown_multiselect_option` | `p0139d` | `visitors_other_livestock_premises_type` | 1 | 1 |
| `unknown_multiselect_option` | `p0140d` | `visitors_other_livestock_premises_type` | 1 | 1 |
| `no_with_followup_data` | `p0021` | `depopulation_plan` | 1 | 1 |
| `no_with_followup_data` | `p0106` | `carcass_bin_available` | 8 | 8 |
| `no_with_followup_data` | `p0120` | `contract_workers_present` | 3 | 3 |
| `no_with_followup_data` | `p0122` | `visited_other_premises` | 7 | 7 |
| `no_with_followup_data` | `p0127` | `crews_enter_premises` | 5 | 5 |
| `no_with_followup_data` | `p0166` | `birds_introduced` | 5 | 5 |
| `no_with_followup_data` | `p0172` | `birds_moved_off_premises` | 5 | 5 |
| `no_with_followup_data` | `p0181` | `eggs_moved_onto_premises` | 7 | 7 |
| `no_with_followup_data` | `p0186` | `egg_products_moved_onto_premises` | 7 | 7 |
| `no_with_followup_data` | `p0191` | `eggs_moved_off_premises` | 4 | 4 |
| `no_with_followup_data` | `p0196` | `egg_products_moved_off_premises` | 8 | 8 |
| `yes_followup_missing` | `p0020` | `veterinarian_available` | 2 | 2 |
| `yes_followup_missing` | `p0021` | `depopulation_plan` | 2 | 2 |
| `yes_followup_missing` | `p0181` | `eggs_moved_onto_premises` | 1 | 1 |
| `expected_date_unparseable` | `p00010g` | `houses_clinical_onset_date` | 2 | 2 |
| `expected_date_unparseable` | `p00013g` | `houses_clinical_onset_date` | 1 | 1 |
| `expected_date_unparseable` | `p0002` | `clinical_signs_first_observed_date` | 1 | 1 |
| `expected_date_unparseable` | `p0008a` | `ai_tests_date` | 2 | 2 |
| `expected_date_unparseable` | `p0009a` | `ai_tests_date` | 1 | 1 |
| `expected_numeric_unparseable` | `p00010c` | `houses_birds_today` | 2 | 2 |
| `expected_numeric_unparseable` | `p00010d` | `houses_birds_placed` | 3 | 3 |
| `expected_numeric_unparseable` | `p00011c` | `houses_birds_today` | 2 | 2 |
| `expected_numeric_unparseable` | `p00011d` | `houses_birds_placed` | 2 | 2 |
| `expected_numeric_unparseable` | `p00012c` | `houses_birds_today` | 2 | 2 |
| `expected_numeric_unparseable` | `p00012d` | `houses_birds_placed` | 2 | 2 |
| `expected_numeric_unparseable` | `p00013c` | `houses_birds_today` | 1 | 1 |
| `expected_numeric_unparseable` | `p00013d` | `houses_birds_placed` | 1 | 1 |
| `expected_numeric_unparseable` | `p00013e` | `houses_age_weeks` | 1 | 1 |
| `expected_numeric_unparseable` | `p0005` | `baseline_mortality` | 3 | 3 |
| `expected_numeric_unparseable` | `p0006` | `initial_sampling_mortality` | 3 | 3 |
| `expected_numeric_unparseable` | `p0100oth` | `mortality_disposal_distance_yards` | 3 | 3 |
| `expected_numeric_unparseable` | `p0106b` | `carcass_bin_distance_yards` | 1 | 1 |
| `expected_numeric_unparseable` | `p0307` | `closest_field_distance_yards` | 3 | 3 |
| `expected_numeric_unparseable` | `premlat` | `premises_latitude` | 1 | 1 |
| `expected_numeric_unparseable` | `premlong` | `premises_longitude` | 1 | 1 |
| `incomplete_table_record` | `p00011a` | `houses_house_id` | 1 | 1 |
| `incomplete_table_record` | `p0105` | `mortality_disposal_other_description` | 2 | 2 |
| `incomplete_table_record` | `p0109` | `manure_destinations_company_name_location` | 3 | 3 |
| `incomplete_table_record` | `p0116` | `imported_materials_product_species_origin` | 3 | 3 |
| `incomplete_table_record` | `p0123` | `worker_visits_premises_processor` | 3 | 3 |
| `incomplete_table_record` | `p0128` | `crews_date` | 3 | 3 |
| `incomplete_table_record` | `p0151a` | `shared_equipment_received_or_loaned` | 3 | 3 |
| `incomplete_table_record` | `p0152a` | `shared_equipment_received_or_loaned` | 3 | 3 |
| `incomplete_table_record` | `p0153a` | `shared_equipment_received_or_loaned` | 3 | 3 |
| `incomplete_table_record` | `p0154a` | `shared_equipment_received_or_loaned` | 3 | 3 |
| `incomplete_table_record` | `p0155a` | `shared_equipment_received_or_loaned` | 1 | 1 |
| `incomplete_table_record` | `p0156a` | `shared_equipment_received_or_loaned` | 1 | 1 |
| `incomplete_table_record` | `p0157a` | `shared_equipment_received_or_loaned` | 1 | 1 |
| `incomplete_table_record` | `p0158a` | `shared_equipment_received_or_loaned` | 2 | 2 |
| `incomplete_table_record` | `p0159a` | `shared_equipment_received_or_loaned` | 2 | 2 |
| `incomplete_table_record` | `p0160a` | `shared_equipment_received_or_loaned` | 3 | 3 |
| `incomplete_table_record` | `p0161a` | `shared_equipment_received_or_loaned` | 3 | 3 |
| `incomplete_table_record` | `p0162a` | `shared_equipment_received_or_loaned` | 3 | 3 |
| `incomplete_table_record` | `p0163a` | `shared_equipment_received_or_loaned` | 3 | 3 |
| `incomplete_table_record` | `p0164a` | `shared_equipment_received_or_loaned` | 3 | 3 |
| `incomplete_table_record` | `p0165a` | `shared_equipment_received_or_loaned` | 3 | 3 |
| `incomplete_table_record` | `p0167` | `bird_movements_date` | 3 | 3 |
| `incomplete_table_record` | `p0173` | `bird_movements_date` | 3 | 3 |
| `incomplete_table_record` | `p0182` | `egg_movements_source_destination` | 3 | 3 |
| `incomplete_table_record` | `p0187` | `egg_movements_source_destination` | 3 | 3 |
| `incomplete_table_record` | `p0197` | `egg_movements_source_destination` | 3 | 3 |

The following per-case counts reconcile all 170 warnings and two errors.
N/F = No with follow-up, Y/M = Yes missing follow-up, date/number = parse
warnings, incomplete = incomplete table, and option = unknown-option errors.

| Audit case | N/F | Y/M | Date | Number | Incomplete | Option errors |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| AUDIT-001 | 7 | 0 | 0 | 0 | 25 | 0 |
| AUDIT-002 | 0 | 1 | 0 | 0 | 0 | 0 |
| AUDIT-003 | 7 | 0 | 0 | 2 | 22 | 0 |
| AUDIT-006 | 1 | 0 | 1 | 12 | 0 | 0 |
| AUDIT-008 | 10 | 0 | 1 | 2 | 0 | 0 |
| AUDIT-009 | 4 | 0 | 0 | 0 | 0 | 0 |
| AUDIT-010 | 1 | 0 | 0 | 0 | 0 | 0 |
| AUDIT-011 | 1 | 0 | 0 | 0 | 0 | 0 |
| AUDIT-012 | 9 | 0 | 2 | 0 | 0 | 0 |
| AUDIT-014 | 1 | 0 | 0 | 0 | 0 | 0 |
| AUDIT-017 | 1 | 0 | 0 | 0 | 0 | 2 |
| AUDIT-018 | 4 | 0 | 0 | 0 | 0 | 0 |
| AUDIT-020 | 2 | 0 | 0 | 0 | 0 | 0 |
| AUDIT-024 | 0 | 1 | 1 | 0 | 0 | 0 |
| AUDIT-025 | 4 | 1 | 0 | 1 | 0 | 0 |
| AUDIT-026 | 0 | 0 | 0 | 8 | 0 | 0 |
| AUDIT-027 | 7 | 0 | 0 | 0 | 20 | 0 |
| AUDIT-032 | 0 | 2 | 0 | 0 | 0 | 0 |
| AUDIT-034 | 1 | 0 | 0 | 1 | 0 | 0 |
| AUDIT-037 | 0 | 0 | 2 | 5 | 0 | 0 |
