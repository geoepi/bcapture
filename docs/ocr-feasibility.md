# Initial Epi Tesseract OCR feasibility

Validation date: 2026-10-06. This is a sanitized architecture report. It
contains aggregate structural and image-quality metrics only; source filenames,
recognized text, identifiers, and field values remain outside the repository.

## Decision

**HOLD**

Tesseract is technically viable as a future local OCR component for the
machine-printed scan class, and the canonical page geometry supports guarded
affine registration. The evidence is not sufficient to enable a production OCR
route: two scans are handwriting-heavy, production field values have no safe
ground truth for accuracy scoring, and image-based control scores remain
ambiguous without a verified checked/unchecked label. Existing AcroForm and
selectable-text routes remain unchanged; image-only inputs remain controlled
`no_usable_digital_content` failures.

## Governance and scope

`D:/Github/.codex-local/EXECUTION_GOVERNANCE.md` was read and followed under
SAE. This work stayed within Level 1 execution authority. No Level 2 OCR route
or public API design was implemented because the evidence remained mixed; no
Level 3 escalation was required. The benchmark used only a local Tesseract
process, English language data, and ephemeral local render/OCR products.

## T1 structural characterization

The three known image-only Initial Epi inputs were read in place and assigned
sanitized case IDs in sorted input order.

| Case | Pages | Page box | Rotation/orientation | Raster content | Text layer | Form compatibility |
| --- | ---: | --- | --- | --- | --- | --- |
| case_001 | 12/12 | 612 x 792 pt | 0 degrees, portrait | 12 grayscale JPEG images; approximately 168-207 x 179-231 dpi | 0 characters / 0 words | B: canonical form with modest scan/crop variation |
| case_002 | 12/12 | 612 x 792 pt | 0 degrees, portrait | 12 CMYK JPEG images; approximately 161-201 x 173-226 dpi | 33 characters / 3 words on one page only; not usable as a form text layer | B: canonical form with modest scan/crop variation |
| case_003 | 12/12 | 612 x 792 pt | 0 degrees, portrait | 12 RGB JPEG images at 200 x 200 dpi | 0 characters / 0 words | A: canonical form scan |

All 36 page boxes match the packaged 2024-05-28 canonical page geometry. No
missing-page signal was observed. Cases 001 and 002 use full-height raster
pages with variable side margins; case 003 is full-bleed within the page box.
Visual checks found no gross rotation or perspective failure. Cases 001 and
002 contain substantial handwritten responses and handwritten checkbox marks.
Case 003 contains machine-printed response text and image-based checkbox
marks in the inspected pages. Handwritten narrative and signatures were not
OCR targets.

## Tesseract setup

Tesseract was absent initially. The current 5.x Windows release was installed
in a user-local directory, without changing the system `PATH` or repository.
The verified engine is `5.5.3.20260724`; available language data are `eng` and
`osd`. The diagnostic utility accepts an explicit executable path and runs
Tesseract TSV through a local subprocess. `Sys.which("tesseract")` remains an
appropriate capability check for package use; absence must remain a controlled
optional-capability outcome. No R wrapper or package dependency was added.

## T2 OCR, anchors, and registration

Each page was rendered at 200 dpi and evaluated with Tesseract `eng`, page
segmentation mode 6, using raw RGB and grayscale/autocontrast inputs. The
reported configuration is the better of those two bounded preprocessing
choices for each case. Anchor matching used conservative exact or normalized
matching against OCR of the canonical blank page. Registration used a guarded
affine transform with at least 6 inliers, at least 300 pixels of anchor spread
in both axes, and a maximum inlier residual of 22 pixels.

| Case | Selected input | Anchors recovered | Pages registered | Registration RMSE, median/max | Non-baseline words assigned to canonical regions |
| --- | --- | ---: | ---: | --- | ---: |
| case_001 | raw | 519 / 813 (63.8%) | 7 / 12 | 3.036 / 4.155 pt | 151 across 7 pages |
| case_002 | grayscale/autocontrast | 575 / 804 (71.5%) | 6 / 12 | 2.833 / 4.495 pt | 153 across 6 pages |
| case_003 | raw | 717 / 813 (88.2%) | 12 / 12 | 0.452 / 0.792 pt | 256 across 12 pages |

The machine-printed case therefore gives strong registration evidence, but
registration is incomplete for the handwriting-heavy cases. Field-region
assignment was measured only as spatial token assignment. Normalization and
semantic parsing were not claimed because production values lack a safe
ground-truth comparison in this task.

## Controls and preprocessing

Controls were evaluated with canonical widget locations and image difference
against the blank printed template; Tesseract symbols were not used as control
states. The existing synthetic-derived threshold was retained for diagnostic
reporting only. It is not a production accuracy label.

| Case | Compared widgets | Above threshold | Ambiguous within +/-2 score units | Difference score range; median |
| --- | ---: | ---: | ---: | --- |
| case_001 | 451 | 383 | 85 | 3.889-42.960; 21.417 |
| case_002 | 392 | 339 | 82 | 5.065-41.033; 15.633 |
| case_003 | 667 | 132 | 162 | 0.044-30.740; 6.429 |

The image-comparison route is structurally plausible, but these production
scores do not establish checked/unchecked separation. The ambiguity counts and
the absence of verified production labels require a fail-closed design before
any control state is exported.

## Synthetic regression fixtures

Because page registration was promising for case 003, deterministic synthetic
fixtures were created from the packaged fictional `synthetic_epi_print.pdf`
only. They contain clean, moderate, and borderline raster conditions on pages
1 and 6. The optional aggregate evaluator recovered the following canonical
anchors:

| Synthetic level | Page 1 | Page 6 |
| --- | ---: | ---: |
| clean | 101 / 103 | 89 / 91 |
| moderate | 101 / 103 | 84 / 91 |
| borderline | 101 / 103 | 64 / 91 |

These fixtures support future OCR development and fail-closed testing. They do
not justify enabling production extraction: they are synthetic, and the
current test only checks optional capability behavior and aggregate anchor
recovery, not an OCR extraction contract.

## Handwriting and support boundary

Machine-printed text was evaluated fully. Short handwriting was observed but
not interpreted. Handwritten narrative and signatures remain unsupported and
must never be sent through a general OCR route. The handwriting-heavy cases
therefore block broad production support even though the machine-printed case
is a plausible future target.

## Implementation status

This remains a feasibility implementation only. Added artifacts are the
sanitized production benchmark utility, deterministic synthetic fixtures, an
optional synthetic evaluator, and focused optional-capability tests. No OCR
extraction route, failure classes, public provenance fields, or schema changes
were added. The intended future boundary remains:

```text
1. AcroForm
2. selectable-text spatial
3. guarded embedded/container extraction
4. guarded OCR spatial for canonical machine-printed scans only
5. controlled unsupported/failure
```

Any future OCR route should carry local engine/version, template version,
registration method/score, and field-level confidence/BBox evidence without
emitting a whole-page transcript. It must reuse existing normalization,
semantic validation, de-identification, and privacy validation, and fail closed
for insufficient anchors, registration failure, template mismatch, and low
confidence.

## Reproducibility and privacy

The production PDFs were read in place and not modified, renamed, moved,
deleted, copied into the repository, or uploaded. Rendered pages and OCR TSV
were temporary local products and were removed after analysis. No production
PDF, rendered production page, OCR transcript, identifier, de-identified
output, or crosswalk entered Git. All committed OCR fixtures are synthetic and
contain fictional values only.
