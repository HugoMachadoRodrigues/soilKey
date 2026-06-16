# Schema morphology field → calcic enrichment (WRB/USDA) + Raptic/Urbic (v0.9.142)

Unblocks three deferred clauses that needed a morphology field or a thickness
gate, in the refine-when-present pattern (byte-identical until the data exists).

## New schema field

`secondary_carbonates_pct` — identifiable SECONDARY carbonates by volume (soft
masses / pseudomycelia / pendents / nodules). This is the morphological OR-path of
the calcic horizon (WRB 2022 3.1.4 *protocalcic properties* / USDA KST ≥5%
by-volume secondary carbonates) that v0.9.139 found essential but could not encode.
`pedon-schema.json` regenerated.

`layer_origin` (aeolic/fluvic/solimovic/tephric/...) already existed — no new field
needed for Raptic.

## Wired (all refine-when-present)

### calcic() core — WRB/USDA enrichment now reachable
The shared `calcic()` core now enforces the WRB crit-2 enrichment, but at the
criterion level: a ≥15% layer is dropped ONLY when BOTH (2b) the +5%-vs-underlying
CaCO3 test fails AND (2a) `secondary_carbonates_pct` is RECORDED and <5% (both the
enrichment and the protocalcic alternative disproven). When the morphology field is
absent — all current KSSL / FEBR / fixtures — criterion 2a is indeterminate, so
nothing is dropped → **byte-identical**. This resolves the v0.9.139 tension (the
caco3-only core dropped 10 protocalcic Aridisols): with the field, a protocalcic
Aridisol (`secondary_carbonates_pct ≥ 5`) is kept, and a uniform calcareous parent
(secondary <5, no +5%) is correctly excluded.

### SiBCS horizonte_calcico — by-volume alternative
SiBCS Cap 2 p.71 allows the +50 enrichment to be shown "expresso em volume" when
the secondary carbonate is gravelly/concretionary/powdery. `horizonte_calcico` now
passes a layer via the CaCO3 +50 enrichment OR `secondary_carbonates_pct ≥ 5`.

### Raptic (rp) — material-origin exclusion (WRB Ch 5 p.144)
"a lithic discontinuity ≤100 cm **not related to aeolic/fluvic/solimovic/tephric
material**". `qual_raptic` now excludes a discontinuity layer whose recorded
`layer_origin` matches those origins (absent → counts, as before).

### Urbic (ub) — ≥20 cm thickness (WRB Ch 5 p.150)
"a layer **≥20 cm thick** within 100 cm with ≥20% artefacts, ≥35% rubble". The
qualifying layers must now total ≥20 cm.

## Gate

- 44 canonical fixtures byte-identical (`secondary_carbonates_pct` /
  excluding-`layer_origin` absent everywhere; the Technosol/Urbic and Calcisol
  fixtures keep their results).
- `test-v0943` schema-up-to-date passes (pedon-schema.json regenerated).
- WRB end-to-end, calcic, Chernossolos, qualifier-bloco suites green.
- KSSL byte-identical (the calcic core drops nothing without
  `secondary_carbonates_pct`) — no KSSL re-gate needed.
- +7 lock-in tests (test-v09142). `R CMD check --as-cran` tests OK + examples OK +
  Status 1 NOTE.

## Honest note

Like the other Fix-C schema additions (v0.9.128/133), these are **verbatim-
completing, zero-effect-on-current-data** changes: no local dataset records
`secondary_carbonates_pct` or the excluding `layer_origin`, and the Urbic ≥20 cm
gate only tightens future thin urbic layers. They make the calcic / Raptic / Urbic
predicates correct when the morphology data is supplied, without any regression.
