# Honest WRB qualifier coverage: 229 -> 233/234 (v0.9.145)

A roadmap gap audit (6 grounded survey agents + synthesis) concluded that the
soilKey accuracy ceiling is missing reference data, not key bugs, and that almost
nothing remains code-implementable for accuracy. The one genuine *correctness*
defect it surfaced — plus three zero-risk completeness wrappers — is shipped here.
No classification behaviour changes (44 canonical fixtures byte-identical).

## The defect: a vendored upstream typo under-counted coverage

`coverage_report("wrb_qualifiers")` listed **Petrosalic** as missing even though
`qual_petrosalic()` (R/qualifiers-wrb2022-v0964.R) is a complete, correct
implementation. Cause: the vendored WRB 2022 canonical table
(`WRB_4th_2022`, from ncss-tech/SoilTaxonomy) stores the name with its leading
**P dropped** — `"etrosalic"`. The detector `.qualifier_is_implemented()` looked
up `qual_etrosalic` (absent) and reported a false gap.

**Fix:** normalise the lookup key `"etrosalic" -> "petrosalic"` in
`.qualifier_is_implemented()` (R/coverage.R). Petrosalic is in **no RSG
applicable list**, so this is a pure coverage-count correction with zero
classification effect. 229 -> 230/234.

## Three thin wrappers over already-complete diagnostics

Three canonical qualifiers had a fully-implemented backing diagnostic but no
`qual_*` entry point to expose or count them:

| qualifier | backing diagnostic (pre-existing) |
|---|---|
| `qual_sideralic` | `sideralic_properties()` (WRB 3.2.13, both criteria; Fix B v0.9.125) |
| `qual_panpaic` | `panpaic()` |
| `qual_claric` | `claric_material()` |

Each is a one-line `.q_presence("Name", <diag>(pedon), 100, pedon)` wrapper — the
exact pattern of the ~50 existing qualifiers. **None appears in any RSG
applicable list**, so classification is untouched; the data to fire them is
sparse in real pedons (they resolve to NA absent the relevant morphology).

## Result

WRB qualifier coverage **229 -> 233/234 (99.6%)**:

| group | covered | n | pct |
|---|---|---|---|
| principal | 131 | 131 | 100% |
| supplementary | 102 | 103 | 99% |
| **overall** | **233** | **234** | **99.6%** |

The lone remaining gap is **Novic**, which is genuinely **schema-blocked**: it
needs a deposition-age / fresh-deposit metadata field that no PedonRecord schema
(and no dataset) records. Designation-based buried-soil detection alone is
insufficient. Not implementable without new data.

## Gate

- **44 canonical fixtures byte-identical** (no RSG list references the four
  qualifiers; the keys are untouched).
- Coverage tests updated: `test-coverage.R` and `test-wrb-decomp-qualifiers.R`
  (229L -> 233L; the "non-existent function returns NA" example moved from the
  now-implemented Claric to the still-open Novic).
- Full suite green; `R CMD check --as-cran` = 1 NOTE (CRAN-incoming, benign).
- 3 new exports (`qual_sideralic/panpaic/claric`, `@keywords internal`).
