# bCAPture Tutorials

These tutorials show the current bCAPture user workflows with fictional
information only. They are intended for scientifically literate R users who
are new to the package and for future manual smoke testing.

Do not copy controlled or operational data into tutorial directories merely to
reproduce an example. Use working copies of interactive AcroForm PDFs and
obviously fictional values.

## Workflows

- [BCAP Form Extraction Workflow](bcap-workflow.md): extract interactive BCAP
  PDFs, inspect fields, and review schema diagnostics.
- [Initial Epi Extraction, Collation, and Validation Workflow](initial-epi-workflow.md):
  extract Initial Epi forms, create semantic relational tables, and review
  data-quality findings.
- [Initial Epi De-identification Workflow](initial-epi-deidentification.md):
  create a controlled-use pseudonymized analysis dataset and keep its private
  crosswalk separate.
- [Initial Epi descriptive summaries](../initial-epi-summaries.md): create
  metadata-driven descriptive products from the de-identified dataset.
- [Initial Epi analytical features](initial-epi-analytic-features.md): derive
  versioned case-level analytical attributes from de-identified data.
- [Initial Epi Visualization](initial-epi-visualization.md): create reusable
  ggplot objects from the summary products.
- [Initial Epi HTML Reporting](initial-epi-reporting.md): render self-contained
  descriptive and data-quality reports from summary products.

The current package boundary is:

~~~
BCAP:
extraction + schema diagnostics

Initial Epi:
extraction + schema diagnostics + semantic collation
+ validation + de-identification + descriptive summaries
+ auditable case-level analytical features
~~~

BCAP does not currently provide semantic collation, validation, or
de-identification functions.

## Important form requirement

The current extractors read interactive PDF AcroForm fields. Do not use
Print to PDF, flatten the form, or scan a completed form for this workflow.
Those operations can remove the fields required by `bcapture`.
