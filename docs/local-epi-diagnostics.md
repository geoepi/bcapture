# Local Initial Epi corpus diagnostics

> Historical phase report: this document records the extraction-diagnostics
> stage before the later semantic normalization and privacy-scoping audit. The
> final package behavior and final validation state are documented in
> [the semantic/de-identification audit](epi-semantic-deid-audit.md). This
> report is retained as an audit trail and should not be read as the final
> privacy outcome.

Validation date: 2026-10-05. This report contains aggregate structural findings
only. Source paths, filenames, extracted values, and re-identification mappings
remain outside the repository.

## Baseline and structural decisions

The unmodified development package discovered 46 PDFs: 39 succeeded (36
AcroForm, three spatial) and seven failed. No partial-success records or R
extraction warnings were returned. Initial failures comprised two
`no_usable_digital_content`, three `unexpected_form_type`, one
`unrecognized_flattened_form`, and one `unknown_error`.

| Structural population | Count | Decision and evidence |
| --- | ---: | --- |
| PDF Portfolio with one canonical interactive Epi member | 1 | EXTEND. Seven embedded PDFs: one 12-page member with 497 fields, 667 widgets, and the exact registered schema; six field-free one-page members and a one-page outer container. Inspect and extract in memory. |
| Scanned forms with no usable selectable text | 3 | CLASSIFY. Two have no text; one has sparse first-page text below the existing usability threshold. No OCR added. |
| Incompatible interactive form signatures | 3 | CLASSIFY. Two share a 442-field architecture; one has 358 fields and A4 pages. All lack the distributed Epi signature. Their scientific mapping requires separate template/version decisions. |
| Supplemental raster pages on an incompatible form | 1, included above | CLASSIFY with the incompatible form. Eleven supplemental pages do not establish a supported Epi page range. |
| Unrepresentable PDF metadata | 1, included among scans | FIX. An embedded NUL in metadata caused a Python-to-R transport exception. Omit only the metadata item and record omission evidence; field values are never repaired. |

All 46 PDFs were structurally readable and unencrypted. No additional real
population of concatenated Epi forms, malformed PDFs, or usable-text registration
failures was observed. The three existing spatial successes retained strong
anchor registration and zero ambiguous fields. Each has one unsupported
administrative reset control; this is an existing analytical-coverage limitation.

## Final extraction result

| Measure | Initial | Final |
| --- | ---: | ---: |
| PDFs discovered | 46 | 46 |
| Successful | 39 | 40 |
| Failed | 7 | 6 |
| Successful AcroForm extractions | 36 | 37 |
| Successful spatial extractions | 3 | 3 |
| Captured extraction warnings | 0 | 0 |

Net recovery: one PDF. No prior successful record was lost. Exact fingerprints of
all 39 prior field tables match after rerun. Final failures are three
`no_usable_digital_content` scans and three `unexpected_form_type` inputs, with
attempted routes correctly reported as spatial and AcroForm, respectively.
No partial-success status or unknown error remains; the existing reset-control
coverage limitation described above still applies.

## Package changes and safeguards

Initial Epi retains AcroForm precedence and the existing selectable-text fallback.
The added container route requires exactly one embedded field-bearing PDF with
the registered interactive page count, canonical schema, logical-field count,
and widget count. Other PDFs must be readable, field-free, and together with the
outer pages shorter than the registered printed form. This is a conservative
ambiguity bound, not semantic proof that shorter supplemental documents cannot
contain form fragments. Unknown non-PDF attachments, nested containers, XFA,
encrypted/unreadable members, and multiple form candidates fail explicitly.
Standalone signature-valid Epi AcroForms with extra pages also fail explicitly.
BCAP container extraction remains unsupported.

C2PA provenance is distinguished structurally by its associated-file
relationship, media type, and absence of a PDF header; filenames play no role.
This preserves existing signed flattened fixtures. The PDF embedding relationship
is described in the [official C2PA specification](https://spec.c2pa.org/specifications/specifications/2.2/specs/C2PA_Specification.html).

A raw AcroForm tree audit detects duplicate qualified names and mapping keys
before a parsed dictionary can collapse them. Unnamed radio widgets and unnamed
ancestors remain supported. Selectable-text usability is checked before spatial
page count, typed Python failure classes survive into R, and failed manifests
record the attempted extraction route. NUL-bearing metadata keys/values receive
omission indicators; NUL-bearing logical strings fail explicitly.

Raw provenance distinguishes the submitted container hash from the selected
member hash and counts. All-failure Epi manifests retain the same provenance
columns with unavailable values marked missing. No embedded PDFs are written to
disk. Button-diagnostic grouping avoids repeated normalization and irrelevant
pair comparisons while preserving exact ordered rows and geometric evidence.

| Change | Root cause and regression coverage | Real-corpus verification |
| --- | --- | --- |
| Embedded canonical AcroForm route | A Portfolio cover has no Epi fields. Synthetic Portfolio, EmbeddedFiles, AF-only, duplicate-filename, canonical-member, partial-member, and multi-candidate cases exercise selection/provenance. | Affected-file rerun recovered the Portfolio with 497 fields and 667 widgets. |
| Metadata transport and scan classification | NUL metadata cannot cross the Python-to-R boundary; page count previously preceded text usability. Synthetic metadata key/value NUL, logical-value NUL, and raster-only PDFs test omission versus controlled failure. | All three scans return controlled `no_usable_digital_content`, including the previous metadata exception. |
| Failure route and type retention | Handler scope and message-only wrapping lost attempted route/typed failure evidence. Synthetic Epi and BCAP scan batches verify attempted spatial routing and exact failure classes. | All six failed manifest rows retain the appropriate attempted route and controlled class. |
| Ambiguity and feature preflight | Parsed dictionaries can hide duplicate fields, and aggregate/XFA content can be silently omitted. Synthetic duplicate/mapped names, radio widgets, unnamed ancestors, concatenation, supplements, nested/unknown/malformed/encrypted attachments, XFA, and C2PA cases exercise the guard. | No observed corpus ambiguity is promoted to success; unfamiliar signatures remain unsupported. |
| Button diagnostic grouping | Full pair loops repeatedly normalize the same state sets and compare unrelated suffixes. Synthetic ordering/geometry assertions and exact baseline comparison preserve behavior. | Full 46-input invocation completed successfully with `diagnostics = TRUE`. |

## Validation

The complete pre-change package suite passed, including Quarto reporting tests.
The independent pre-change synthetic audit recovered Initial Epi 457/457 values
and 148/148 buttons, BCAP 218/218 values and 155/155 buttons, with zero blank
false positives for both families. An exact old/new diagnostic comparison
preserved all eight ordered synthetic candidate rows.

Post-change focused validation passed 32 tests and 167 expectations with no
failures, errors, warnings, or skips. The independent post-change audit preserved
Epi 457/457 values and 148/148 buttons, BCAP 218/218 values and 155/155 buttons,
and zero blank false positives. The complete post-change package suite passed
91 tests and 715 expectations, with zero failures, errors, warnings, or skips;
Quarto report checks ran successfully. Both affected-file reruns produced the
expected dispositions. The complete corpus rerun confirmed 40 successes and six
controlled failures, with all 39 prior field tables unchanged. All 46 source
PDFs retain identical SHA-256 hashes, byte sizes, and modification times
(120,536,514 bytes total).

The established `collate_epi()` API requires an all-success extraction manifest.
For integration, a derived subset was created inside the authorized confidential
output root, strictly retaining the 40 successful IDs and excluding the six
failures. Required field and metadata coverage was verified; canonical extraction
products remained byte-identical. A private derivation manifest records retained
and excluded IDs/counts, the source-manifest hash, and derived-product hashes.
No PDFs were copied. The existing collation, semantic-validation, and
de-identification APIs use the established dictionary and privacy rules.
Collation completed in strict mode. Semantic validation retained 172 findings:
170 warnings and two errors in one form; form statuses are 20 valid, 19 review,
and one error. The
validator's established default records findings rather than gating subsequent
processing. These are data-quality findings requiring review, distinct from
extraction failures.

Strict `deidentify_epi()` reported confirmed direct-identifier leakage and
refused to finalize output. The integration therefore **failed its privacy
acceptance check**. Both the safe-output and crosswalk directories contain zero
finalized files, and the temporary safe-output sibling was removed. There is no
deidentified dataset to publish or treat as privacy-cleared. The audit result
requires separate diagnosis; no identifier policy, mapping, strictness, or
acceptance criterion was changed. Container/member provenance is excluded by
collation's explicit output selection and remains in confidential extraction
products.

## Governance and data boundary

Work proceeded under SAE. The supervisor approved bounded container selection,
canonical embedded-form checks, ancillary C2PA handling, and rejection of
ambiguous standalone page counts. Unsupported form-version interpretation and
OCR remain outside this task. Final source review identified no further actionable
defect. All source-acceptance conditions passed: full suite, exact-value/button
audit, and unchanged field tables for the 39 previous successes.
Work is isolated on `feature/local-epi-diagnostics`. The feature checkout is
clean after committing this report. The original checkout retains its
pre-existing user modification to `scripts/bcapture_test.R`. No merge to main
was performed.

The supervisor accepted the extraction feature and directed separate reporting
of the privacy integration failure under task section 17. Leakage diagnosis may
proceed as separately authorized diagnostic work; privacy-rule, mapping, output
policy, or acceptance changes require Level-3 human review.

Implementation commits: `71cb224` (structure routing, failure/provenance fixes,
synthetic regressions, and API documentation) and `937c5c2` (diagnostic
optimization). This report is recorded separately on the same branch.

Only package code, synthetic structural fixtures/tests, public template
references, documentation, and this sanitized report are intended for Git.
Authoritative PDFs are read in place; confidential outputs, local inventories,
hash comparisons, and any crosswalk remain in the authorized external locations.
No production PDFs, confidential extracted data, or crosswalks entered Git; no
real-data fixtures were created. Newly added regression fixtures are generated
from synthetic structures and the packaged public blank template.

Runtime: local development package `0.0.0.9000`, R 4.5.0, Python 3.12.14,
`pypdf` 6.10.0, `pdfplumber` 0.11.9, and `pypdfium2` 5.13.0. The registered
Initial Epi template version is 2024-05-28. Both test runs emitted four R startup
locale warnings from the host environment; these are distinct from captured
package-test and extraction warnings.
