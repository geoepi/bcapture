# Initial Epi case-level analytical features

`derive_epi_features()` creates a versioned case-level feature layer from a
successful `deidentify_epi()` output directory. Analytical features are
structured representations of questionnaire observations. They are not risk
scores and do not imply causation.

## Architectural role

The summary and feature branches answer different questions:

- `summarize_epi()` asks what a collection of cases looks like descriptively;
- `derive_epi_features()` asks what analysis-ready structured attributes can
  be assigned to each de-identified case.

```text
PDF -> extract -> collate -> validate -> de-identify
                                      |-> summarize -> visualize -> report
                                      `-> derive features -> future epidemiological analysis
```

Neither branch is a prerequisite for the other.

## Privacy boundary

Feature derivation reads only `collated/`, `validation/`, and `privacy/` below
the de-identified directory. It rejects identifiable provenance columns,
requires the analysis privacy profile, always rejects `privacy_errors > 0`,
and never searches for or reads the private re-identification crosswalk.

The registry may reference only fields whose privacy action is `retain`.
Repeated record counts name the exact retained analytical columns used to
establish record content. Direct identifiers, pseudonyms, sensitive free text,
and review-withheld values cannot become features.

`epi_features.csv` remains a controlled-use de-identified analytical dataset.
It contains case-level information and is not automatically suitable for
unrestricted public release. It is pseudonymized/de-identified, not anonymous.

## Registry and provenance

Version 1 is stored under
`inst/extdata/features/initial_epi/2024-05-28/`. `features.csv` defines source
questions and tables, types, mappings, missingness, derivations, order, and
parent-child rules. `domains.csv` defines neutral organizational domains. The
normalized SHA-256 registry hash and semantic dictionary hash are recorded in
every `feature_manifest.csv`.

The development audit in `design_audit.csv` records candidates that were
implemented, deferred, or rejected and the reason. The semantic review covered
all 497 logical dictionary fields, representing 150 distinct semantic
variables after repeated-field grouping. Forty compact features were selected;
the layer intentionally does not reproduce every retained questionnaire field.

## Feature inventory

- Flock and clinical (6): `baseline_mortality`,
  `initial_sampling_mortality`, `eggs_present`, `eggs_laid_last_week`,
  `ai_test_records`, and `house_records`.
- Mortality disposal (4): three explicit carcass-bin responses and
  `mortality_disposal_records`.
- Manure and imported materials (3):
  `manure_from_another_premises_reported`, `manure_destination_records`, and
  `imported_material_records`.
- Personnel and visitor contact (6): explicit worker/crew parent responses and
  worker, crew, and visitor record counts.
- Shared equipment (2): within-premises farm-equipment movement and retained
  shared-equipment records.
- Bird movement (3): introductions, movements off premises, and bird-movement
  records.
- Egg movement (5): four directional/material parent responses and
  egg-movement records.
- Environment and wildlife (9): explicit outdoor-access, wildlife, and water
  responses plus categorical supplemental-feed and wildlife-plan responses.
- Management and veterinary (2): veterinarian availability and depopulation
  plan responses.

Direct numeric features copy parsed semantic values without unit conversion.
Coded categorical features copy only dictionary-defined labels. Logical
features use registry-defined response codes. Count features count rows with at
least one populated retained analytical field named by the registry.

## Missingness and consistency

Binary features use `TRUE`, `FALSE`, and `NA` only because their source
codebooks contain explicit Yes and No states; blank remains missing. The two
codebooks with `Don't know` or `Not applicable` are represented as categorical
features, and `feature_long.csv` retains `unknown` and `not_applicable` status.
No blank, unknown, or not-applicable response is converted to No.

Zero repeated rows means zero qualifying retained-content records. It does not
mean that the parent event was confirmed absent. Parent Yes/No/missing values
are preserved independently of child records. Contradictions are never
repaired; they are emitted in `feature_consistency_findings.csv` as INFO or
WARNING findings. Duplicate stable row identities are counted rather than
silently deduplicated and are flagged for review.

## Output contract

The atomic `features/` directory contains:

- `epi_features.csv`: one row per case with validation status and registry-order
  feature columns;
- `feature_long.csv`: typed-long values and explicit value status;
- `feature_dictionary.csv`: a self-contained derivation dictionary;
- `feature_summary.csv`: descriptive binary, categorical, count, and numeric
  quality summaries;
- `feature_domain_summary.csv`: domain organization and observed-value coverage;
- `feature_consistency_findings.csv`: parent-child and duplicate-row findings;
- `feature_derivation_audit.csv`: per-feature missingness, range, and status;
- `feature_manifest.csv`: dictionary, registry, privacy, validation, and package
  provenance.

Only `features/` may be created or atomically replaced. Source products remain
unchanged. `write = FALSE` returns every product without writing files.

## Boundaries and future work

This layer does not perform feature weighting, scoring, causal classification,
outcome construction, feature selection by observed association, or inferential
analysis. It does not join geography, weather, land cover, wildlife abundance,
movement databases, or other external sources.

The controlled feature products may later support descriptive comparisons,
case-control analyses, regression, machine learning, and source/pathway
hypotheses after appropriate scientific review. Separate controlled modules may
later add spatial/environmental covariates, movement-network covariates,
weather context, or production-system context through explicit `case_id`
linkage.
