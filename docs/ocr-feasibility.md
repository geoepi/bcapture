# Initial Epi scanned-form OCR feasibility

Validation date: 2026-10-06. This is a sanitized architecture report. It
contains aggregate structural and image-quality metrics only; source filenames,
recognized text, identifiers, and field values remain outside the repository.

## Decision

**HOLD**

The canonical Initial Epi page geometry is compatible with a future bounded
OCR-spatial route, but the evidence required for safe extraction is incomplete.
No OCR engine was available in the local execution environment, so anchor
recovery, OCR registration, typed-field assignment, and confidence-gated
control classification could not be measured. Two of the three scans contain
substantial handwriting, which remains explicitly out of scope. The existing
AcroForm and selectable-text routes are unchanged, and image-only inputs remain
controlled `no_usable_digital_content` failures.

## O1 structural characterization

The three known image-only Initial Epi inputs were read in place and assigned
sanitized case IDs in sorted input order.

| Case | Pages | Page box | Rotation/orientation | Raster content | Text layer | Form compatibility |
| --- | ---: | --- | --- | --- | --- | --- |
| case_001 | 12/12 | 612 x 792 pt | 0°, portrait | 12 grayscale JPEG images; approximately 168–207 x 179–231 dpi | 0 characters / 0 words | B: canonical form with modest scan/crop variation |
| case_002 | 12/12 | 612 x 792 pt | 0°, portrait | 12 CMYK JPEG images; approximately 161–201 x 173–226 dpi | 33 characters / 3 words on one page only; not usable as a form text layer | B: canonical form with modest scan/crop variation |
| case_003 | 12/12 | 612 x 792 pt | 0°, portrait | 12 RGB JPEG images at 200 x 200 dpi | 0 characters / 0 words | A: canonical form scan |

All 36 page boxes match the packaged 2024-05-28 canonical page geometry, all
pages are portrait, and no missing-page signal was observed. Cases 001 and 002
place the raster page at full height with variable side margins; case 003 is
full-bleed within the page box. The images use JPEG/DCT compression. Rendered
page summaries showed predominantly light backgrounds and low dark-pixel
fractions; no gross rotation or perspective failure was observed in the visual
spot checks.

Cases 001 and 002 contain handwritten responses and handwritten checkbox marks.
Case 003 contains machine-printed response text and image-based checkbox marks
in the inspected pages. Handwritten narrative and signatures were not treated
as OCR targets.

## O2–O3 feasibility evidence

The bundled local runtime provided Python 3.12.14, `pypdf` 6.10.0,
`pdfplumber` 0.11.9, `pypdfium2` 5.13.0, Pillow, and NumPy. Tesseract and
alternative OCR engines/wrappers were not installed. No cloud OCR or external
transmission was used.

| Measure | Result |
| --- | --- |
| OCR engine/version | Not available; experiment not run |
| Expected-anchor recovery | Not measured |
| OCR registration transform/residual | Not measured; only page-box/placement compatibility was checked |
| Field-region assignment | Not measured |
| Typed-field feasibility | Visually plausible for case 003 only; not validated by OCR |
| Checkbox/control feasibility | Image marks are present and canonical control regions are available; threshold separability not measured |
| Handwriting | Present in cases 001–002; excluded from any future bounded route |
| Characterization runtime | Approximately 9 seconds for the three-form structural/text-layer pass; approximately 8 seconds for low-resolution render summaries |

The existing architecture remains the correct candidate boundary for a future
experiment:

```text
local OCR word boxes -> guarded page registration -> canonical field regions
-> existing normalization/semantic validation -> privacy/de-identification
```

That experiment should use a deterministic local OCR engine, begin with
synthetic rasterized templates and the machine-printed scan class, and fail
closed when anchor recovery, registration, or field confidence is inadequate.
Checkboxes should continue to use image comparison against canonical control
regions rather than OCR symbols. No OCR extraction route or public provenance
fields were added in this task.

## Reproducibility and privacy

`scripts/characterize_initial_epi_scans.py` performs sanitized O1 diagnostics.
It reports case IDs, counts, geometry, image metadata, and aggregate render
statistics; it does not print source filenames, PDF metadata values, OCR text,
or field values. Temporary renders and metrics were kept outside the
repository.

The source PDFs were read in place and not modified, renamed, moved, deleted,
copied into the repository, uploaded, or committed. No production PDF,
rendered production page, OCR transcript, identifier, de-identified output, or
crosswalk entered Git. No OCR regression fixtures were added; any future OCR
fixtures must be synthetic.
