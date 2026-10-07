# Phase 3 separate-form analysis outputs

Phase 3 is the production boundary between validated, deidentified form
products and analysis-ready data. It accepts safe output directories only. It
does not read PDFs, rerun extraction, repair validation findings, or copy
private crosswalk contents.

## API boundary

The package already exposes `collate_epi()` for the upstream
extraction-to-within-Epi semantic collation workflow. That API remains
unchanged for compatibility. The safe-output Phase 3 API is
`collate_epi_analysis()`. BCAP has the independent `collate_bcap()` API, and
`collate_hpai()` is an optional wrapper that runs either family or both.

```r
epi <- collate_epi_analysis(
  epi_dir = "D:/HPAI_Data/HPAI/HPAI/extraction/epi_deid",
  output_dir = "analysis/epi",
  run_id = "phase3-epi-production"
)

bcap <- collate_bcap(
  bcap_dir = "D:/HPAI_Data/HPAI/HPAI/extraction/a_safe",
  output_dir = "analysis/bcap",
  run_id = "phase3-bcap-production"
)

all_forms <- collate_hpai(
  epi_dir = "D:/HPAI_Data/HPAI/HPAI/extraction/epi_deid",
  bcap_dir = "D:/HPAI_Data/HPAI/HPAI/extraction/a_safe",
  output_dir = "D:/HPAI_Data/HPAI/HPAI/extraction/analysis",
  epi_crosswalk_dir = "D:/HPAI_Data/HPAI/HPAI/extraction/epi_crosswalk",
  bcap_crosswalk_dir = "D:/HPAI_Data/HPAI/HPAI/extraction/a_xwalk"
)
```

The wrapper returns `$epi` and `$bcap` separately. It never creates a
unified `records` or `entities` table. Each family can be produced and
validated independently, and zero cross-form links is a valid successful
result.

## Output contract

Each family directory contains the same four core products as CSV and RDS:

| Product | Grain | Contract |
|---|---|---|
| `records` | one row per safe form record | family-scoped `record_id`; no `entity_id` |
| `repeated_items` | one row per repeated Epi value | typed value slots and stable `item_id`; explicit empty BCAP table when no relational repeated structure exists |
| `provenance` | one row per safe form record | version, validation/privacy status, run, and safe source-hash reference |
| `field_metadata` | one row per versioned field mapping | source/question/response/units/privacy/output/table semantics |

BCAP `records` is a wide one-row-per-record table containing only fields
permitted by the versioned safe-output policy. Withheld fields are absent from
that table but remain present in `field_metadata`. Epi repeated values remain
long and are not flattened into the parent record table.

The wrapper also writes:

* `metadata/epi_field_metadata.*` and `metadata/bcap_field_metadata.*`;
* `qa/cross_reference.*`, when both families are supplied;
* `manifests/run_manifest.*` and `manifests/output_hashes.csv`.

## Field metadata and privacy

`field_metadata("epi")` and `field_metadata("bcap")` are generated only from
version-controlled dictionaries, templates, and privacy policies. They never
read production values. Every row includes the required field name, canonical
name, form/version, question and source location, response type, units,
allowed values, privacy class, safe-output inclusion, and table semantics.

The allowed privacy classes are `SAFE_RETAIN`, `SAFE_NORMALIZE`,
`PSEUDONYMIZE`, `WITHHOLD`, and `CROSSWALK_ONLY`. Withheld metadata is
deliberately retained for auditability and downstream schema planning.

## Optional cross-reference

`cross_reference_hpai()` is fail-closed and uses only exact authoritative
`premises_key` equality supplied by both private crosswalks. It emits safe
record IDs, linkage basis, and confidence; private keys themselves are never
written to an analysis product. It does not infer links from names, addresses,
geography, dates, response similarity, or other derived features. If either
crosswalk is absent, or if no keys are shared, the linkage table is empty and
the family products remain valid.

## QA, provenance, and determinism

The finalization gate checks upstream semantic/privacy status, exact schema
membership, required keys, duplicate IDs, repeated-row referential integrity,
metadata coverage, and type normalization. Unknown columns, missing canonical
columns, unsupported versions, ambiguous crosswalk mappings, privacy errors,
and orphan rows are blocking errors. Known non-blocking Epi semantic/type
warnings are retained in `qa_findings.csv` and reflected in the family status.

`run_manifest.csv` records the run ID, timestamp, package version, Git SHA,
schema versions, source counts, semantic/privacy status, QA status, and
product hashes. `output_hashes.csv` records one hash per product and format.
The products are deterministic for fixed inputs and run ID; timestamps are
confined to manifests.

Keep PDFs, confidential extraction tables, safe production inputs, and private
crosswalks outside Git. The production output directory is not package source
data.
