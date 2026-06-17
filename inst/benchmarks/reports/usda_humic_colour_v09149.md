# Humic colour-value USDA predicate + 11 subgroups (v0.9.149)

The v0.9.147 coverage slice excluded ~13 "Humic" Udept/Xerept/Ustept subgroups
because their differentia — a dark colour value — had **no predicate**
(`humic_inceptisol_usda` is epipedon-based, `humic_oxisol_usda` is OC-based;
neither matches). This release writes that predicate and unblocks them.
Completeness, not accuracy.

## The verbatim criterion (carefully verified)

KST-13 Ch. 11 "Humic" Inceptisol intergrade (confirmed via SoilTaxonomy's
`getTaxonCriteria`, e.g. Humic Eutrudepts):

> a colour value, moist, of 3 or less and a colour value, dry, of 5 or less
> (crushed and smoothed sample) **throughout the upper 18 cm** of the mineral soil.

**Verification hazard encountered & avoided:** SoilTaxonomy's fuzzy name lookup
*mis-resolves* these names (e.g. `getTaxonCriteria("Humic Haploxerepts")` returned
the *Calcic* criterion; `"Humic Dystroxerepts"` returned the *Fluventic* one —
off-by-one adjacent codes), and soilKey's `kst13_codes()` uses a **different code
scheme** from SoilTaxonomy (soilKey KEEN = Humic Dystroxerepts; SoilTaxonomy
KEEN = Typic Dystroxerepts). So the criterion was taken from the one clean,
self-consistent reading + the well-established KST definition — not from a fuzzy
lookup.

## `humic_colour_usda()`

Reads the schema's `munsell_value_moist` / `munsell_value_dry` (the fields
`mollic_epipedon_usda` already uses). Every layer overlapping the upper 18 cm
must be dark in **both** moist (≤3) and dry (≤5) value, with both **recorded** —
a missing dry value cannot confirm the criterion. This conservative design means
it never over-fires on a dark A horizon alone.

## The 11 subgroups (coverage 2038 → 2049 / 2715, 75.5%)

- **7 single-modifier** (`all_of [humic_colour_usda]`): Humic Densiudepts,
  Dystroxerepts, Dystrustepts, Eutrudepts, Fragiudepts, Fragixerepts,
  Haploxerepts.
- **4 Humic Lithic** (`all_of [humic_colour_usda, lithic_contact_usda ≤50 cm]`):
  Humic Lithic Dystrudepts, Eutrudepts, Dystroxerepts, Haploxerepts.

Inserted append-before-default + first-match, so they only ever refine a former
Typic. soilKey codes diverge from kst13, so fresh non-colliding codes were minted
per great-group block (with intra-batch reservation so the single + Lithic
siblings in the same block don't collide).

**Excluded (honest):** the aquept Humic subgroups (Endo-/Epiaquepts — compound
with base saturation, or not a valid KST subgroup) and every multi-modifier
Humic compound (Aeric/Alfic/Aquic/Fluventic/Humic-Inceptic/Psammentic/Xeric),
which need additional predicates.

## Gate

- **44 canonical fixtures byte-identical**; full suite green (coverage-count
  assertions updated 2038 → 2049 in test-v09147 and test-usda-intergrade).
- **KSSL+NASIS n=3860 = 0 worsened** — but **vacuous**: the loaded KSSL carries
  **no `munsell_value_dry` in the upper 18 cm** (0 of 3860 pedons), so the gate
  cannot exercise this predicate (the same "weak-test" situation as the v0.9.123
  Oxisol subgroups, here for a different reason). Safety therefore rests on the
  verbatim-exact criterion + the conservative require-both-recorded design +
  append-before-default, NOT on the dataset.
- `R CMD check --as-cran` = 1 NOTE (CRAN-incoming). +6 unit tests (test-v09149).

## Note (out of scope, flagged)

The pre-existing duplicate subgroup code `KFGN` in `inceptisols.yaml` (2
occurrences, present on `main` before this change) is a latent data wart,
unrelated to this slice.
