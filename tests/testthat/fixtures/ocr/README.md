# Synthetic OCR fixtures

These deterministic JPEG fixtures are generated from the packaged synthetic
Initial Epi print fixture, which contains fictional values only. They represent
clean, moderately degraded, and borderline-but-supported raster conditions on
pages 1 and 6. The fixtures do not contain production scans, identifiers, OCR
transcripts, or de-identified records.

The generator is:

```text
scripts/create_tesseract_synthetic_fixtures.py
```

Tesseract evaluation is optional and remains outside the primary package
extraction route. The aggregate evaluator is:

```text
scripts/evaluate_tesseract_synthetic.py
```
