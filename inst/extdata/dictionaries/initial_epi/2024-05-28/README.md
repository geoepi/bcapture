# Initial Epi semantic dictionary: 2024-05-28

This bundle describes the public USDA/APHIS **HPAI Response / Initial
Epidemiological (Epi) Interview**, May 28, 2024 responder template. It is a
versioned dictionary for this source form only; it is not a universal Initial
Epi dictionary and should not be silently reused for later form revisions.

`fields.csv` was bootstrapped from the blank responder template with the
package's AcroForm inventory and then curated into semantic fields. It has one
row for each of the 497 logical PDF fields, preserving the exact raw field
name. The inventory includes the form anchors `premid`, `p0001`, `p0100`, and
`p0300`; these are also the signature anchors used by `extract_epi()`.

The dictionary hash is computed by `load_epi_dictionary()` from the complete
manifest, field mapping, codebook, and table registry at load time. It is
carried into every collated product as `dictionary_hash`; no hand-maintained
checksum is stored here.

`codes.csv` contains field-specific response codebooks. Numeric button values
are never interpreted globally: the `codebook_id` in `fields.csv` selects the
meaning. `tables.csv` registers repeated structures; the individual raw-field
mapping remains in `fields.csv`.

`fields.csv` uses `date` only when the questionnaire expects one scalar
calendar date. It uses `date_text` when the printed form permits date(s),
multiple dates, ranges, recurrence, or other temporal expressions. The
versioned validation registry uses `child_filter` metadata for directional or
material subsets of repeated child tables; the validation engine applies those
filters within the current form only.

`deidentification_rules.csv` is the complete 497-field privacy policy for the
controlled-use `analysis` profile. It is data-driven and versioned with this
dictionary. It explicitly retains approved analytical values, removes direct
contact and exact-location values, pseudonymizes identity-bearing values, and
withholds unconstrained free text for review. This profile creates
pseudonymized/de-identified analytical data, not irreversibly anonymous data;
the separately stored crosswalk remains sensitive and is required for
authorized re-identification.

The official form states that study data are Confidential Business Information.
This bundle contains field names, question metadata, and response vocabulary
only. It contains no populated PDF, respondent response, premises identifier,
contact information, or other CBI.
