# Initial Epi analytical feature registry: 2024-05-28

This bundle is the authoritative version 1 definition of case-level analytical
features derived from the May 28 2024 Initial Epi semantic dictionary.

`features.csv` defines feature order, type, source, response mapping,
missingness, and any parent-child consistency relationship. `domains.csv`
defines neutral organizational domains. `manifest.csv` records the schema and a
normalized SHA-256 hash computed from the feature and domain registries.
`design_audit.csv` records the semantic inclusion, deferral, and rejection
rationale used to construct version 1; it is review metadata and is not part of
the authoritative derivation hash.

Analytical features are structured representations of questionnaire
observations. They are not risk scores and do not imply causation. Binary
features map only explicit Yes and No codes. Missing responses are never
treated as No. Categorical features retain Don't know and Not applicable as
distinct states. Repeated-table counts include only rows with content in the
retained analytical columns named by the registry; zero qualifying rows does
not establish that an event did not occur.

The registry never references direct identifiers, pseudonym identifiers,
sensitive free text, or fields withheld by the analysis privacy policy.
`epi_features.csv` remains a controlled-use de-identified analytical dataset.
It contains case-level information and is not automatically suitable for
unrestricted public release.
