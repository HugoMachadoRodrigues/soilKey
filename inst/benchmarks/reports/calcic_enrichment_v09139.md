# Calcic secondary-carbonate enrichment — v0.9.139

Implements the verbatim calcic-horizon enrichment clause — but, after a measured
KSSL gate, **scoped to SiBCS only**, because the shared WRB/USDA core relies on a
morphological OR-alternative the schema cannot measure.

## The verbatim clause

All three systems require, beyond CaCO3-equiv ≥ 15%, an enrichment signature that
separates a pedogenic calcic horizon from inherited calcareous parent material:

- **WRB 2022 (3.1.4, crit 2)** — *one or both of*: (2a) **protocalcic properties**
  (a morphological observation — visible discrete secondary-carbonate
  accumulations) **OR** (2b) CaCO3-equiv ≥ 5% (absolute) higher than an underlying
  layer.
- **USDA KST** — 5% (absolute) more than an underlying horizon **OR** ≥ 5%
  by-volume identifiable secondary carbonates.
- **SiBCS Cap 2 p.71** — ≥ 50 g/kg more CaCO3 than the subjacent layer (measured by
  volume if the secondary carbonate is gravelly/concretionary/powdery — still the
  same +50, just a different measurement). **No protocalcic / morphological OR.**

The measurable part is the **+5% (50 g/kg) enrichment vs an underlying layer**. The
OR-alternative (protocalcic / by-volume secondary carbonates) is **morphological
and absent from the schema** (only `caco3_pct` exists; no
secondary-carbonate-by-volume or pseudomycelia/pendent field).

## Measured KSSL gate — why the shared core was NOT changed

A first implementation added the enrichment to the shared `calcic()` core and was
measured on the full KSSL gpkg (**n = 34,755** pedons), comparing classify_usda
before/after:

| | n |
|---|---|
| pedons losing the calcic horizon (TRUE→FALSE) | 1,775 |
| of those, predicted **order** changed (all aridisols→entisols) | 78 |
| **order-level WORSENED** (ref Aridisol, was correct, now Entisol) | **10** |
| order-level improved (false Aridisol → correctly Entisol) | 20 |
| order-level neutral (wrong→wrong) | 48 |

The change is net **+10** order accuracy on the flips, but it **drops 10 genuine
Aridisols** (references: `typic calciargids`, `argic petrocalcids`, `typic
camborthids`, `natric camborthids`, …) to Entisols — real calcic horizons that
qualify via the **unmeasured protocalcic / by-volume path**, where the +5% mass
enrichment is not shown (carbonate uniform or non-decreasing, or the calcic is the
deepest sampled layer). This **violates the project's strict 0-worsened discipline**
for KSSL-gated core changes.

## Decision — SiBCS-only, shared core byte-identical

- **`calcic()` core stays absolute-only (byte-identical)** for its WRB/USDA
  consumers (`calcic_horizon_usda`, `qual_calcic`, Calcisol RSG, USDA calcic
  subgroups). Their verbatim criterion has the protocalcic OR, which a caco3-only
  test cannot honour. → KSSL byte-identical, the 78 flips eliminated.
- **`horizonte_calcico` (SiBCS) enforces the +50 enrichment** via the new
  `test_caco3_enrichment`, because SiBCS p.71 has **no** protocalcic alternative.
  Refine-when-present: a candidate layer is dropped only when its CaCO3 fails to
  exceed every deeper measured layer by 5% **and** it is not over a ≥ 40%
  calcareous substrate; an NA-subjacent layer or absent CaCO3 leaves the result
  unchanged.

## `test_caco3_enrichment` (new internal helper)

For each candidate (already ≥ 15% CaCO3): passes if a deeper measured layer is
≥ 40% (marble/marl substrate exemption) or if the candidate exceeds the **minimum**
deeper measured CaCO3 by ≥ 5% absolute; a candidate with **no** underlying measured
layer is dropped (WRB crit 2b inapplicable; only the unmeasured protocalcic path
could qualify it). Empty candidates → NA (preserves the no-data semantics).

## Gate results

- **`calcic()` core byte-identical** → WRB/USDA/KSSL **unchanged** (no re-gate
  needed; the 1,775 flips / 10 worsenings are gone).
- **SiBCS** `horizonte_calcico` enrichment: 44 canonical fixtures byte-identical
  (the Calcisol fixture's Bk1=35/Bk2=40 over C=30 stay enriched); Redape order
  63.8% + BDsolos RJ confusion + the Chernossolos structural tests unchanged.
- Full suite green; +8 lock-in tests (test-v09139); `--as-cran` codoc OK.

## Deferred (honest, schema-blocked)

The WRB/USDA shared-core enrichment needs a **secondary-carbonate morphology
field** (protocalcic properties / ≥ 5% by-volume identifiable secondary carbonates)
to be applied without dropping protocalcic-qualified Aridisols. Until that field
exists, the shared core stays absolute-only. This is a Fix-C-style schema addition
for a future slice.
