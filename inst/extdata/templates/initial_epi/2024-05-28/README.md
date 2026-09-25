# Initial Epi 2024-05-28 canonical spatial template

The two PDF files in this directory are immutable canonical source templates.
`blank_interactive.pdf` is the AcroForm source and `blank_printed.pdf` is the
selectable-text printed-space source. The CSV files are deterministic derived
metadata and may be rebuilt with:

```text
python scripts/derive_spatial_templates.py --evaluation-dir local/synthetic_form_testset_20260924
```

The PDFs must not be optimized, recompressed, flattened, rewritten, or
regenerated. Their SHA-256 hashes are recorded in `manifest.csv`.
