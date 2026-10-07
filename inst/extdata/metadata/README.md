# Analyst-facing field metadata

The Phase 3 `field_metadata()` accessor derives its dictionaries at runtime
from the version-controlled Initial Epi dictionaries, BCAP template fields,
and deidentification policies in this package. No production records or
private crosswalk values are stored here.

The metadata contract includes source/question location, response type,
allowed values, privacy classification, safe-output inclusion, and repeated
table semantics. Fields classified as `WITHHOLD` remain in metadata for
governance and schema audit but are excluded from analysis-ready records.

The current BCAP safe schema includes a small set of observed remediation
fields that are not part of the original template dictionary. These are
represented by curated supplemental metadata in `R/field-metadata.R`; their
wording and physical coordinates remain explicitly marked for review.
