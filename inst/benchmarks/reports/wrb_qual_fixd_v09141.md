# Fix D residue — WRB 2022 qualifier refinements (v0.9.141)

Closes the WRB qualifier-audit backlog (Fix D) by resolving the 7 items deferred
in v0.9.132 as "proxy / uncertain", each re-read against the verbatim WRB 2022
Ch 5 (p.133-150). Two are real, fixable threshold/clause bugs; the rest are
documented as non-2022 or schema/proxy-limited (the gate is the 44 fixtures + the
qualifier suite — KSSL is USDA-labelled, blind to WRB).

## Fixed (verbatim-confirmed)

### Mazic (mz) — p.140
Verbatim: "having a **massive structure AND a rupture-resistance class of at least
hard** in the upper 20 cm of the mineral soil (in Vertisols only)." The prior test
checked only the massive structure → over-fired on a *soft* massive (slaked, not
hardsetting) surface. Now also requires `rupture_resistance %in% {hard, very hard,
extremely hard}` (NOT "slightly hard"), **refine-when-present** — absent rupture
data leaves the massive-only result byte-identical.

### Grumic (gm) — p.136
Verbatim: "a layer ≥1 cm at the mineral soil surface with **STRONG granular OR
strong angular/subangular blocky** structure with an **aggregate size ≤1 cm**
(self-mulching; Vertisols only)." Two prior divergences:
- admitted a **"moderate"** grade → the verbatim is **strong only** (over-fire);
- required **granular** type → missed the **strong-blocky** self-mulching form
  (under-fire).
Fixed both, with the ≤1 cm aggregate limit applied **structure-class-dependently**:
granular up to "medium" stays ≤1 cm, but "medium" blocky peds are 10-20 mm (>1 cm),
so for blocky only "very fine"/"fine" qualify. The canonical Vertisol fixture
(strong subangular-blocky **medium**) is therefore correctly **not** Grumic.

## Documented, not changed

- **Hyposalic** — NOT a WRB 2022 qualifier. The 2022 salinity qualifier for
  EC ≥ 4 dS/m (no salic horizon ≤100 cm) is **Protosalic**; `qual_hyposalic`
  (EC 4-<15) is a package extension, kept as-is.
- **Hyperskeletic** — NOT a WRB 2022 qualifier. The 2022 skeletal qualifier is
  **Skeletic** (≥40% coarse fragments over 100 cm); the ≥90% threshold is a
  WRB-2014 holdover, kept as a coverage extension.
- **Raptic (rp)** — p.144: "a lithic discontinuity ≤100 cm **not related to aeolic,
  fluvic, solimovic or tephric material**." The code's stratification-pattern proxy
  omits the material-origin exclusion (it cannot read material origin from the
  schema); a proxy, not exactly the verbatim. Left as documented proxy.
- **Urbic (ub)** — p.150: ≥20 cm thick, ≥20% artefacts of which ≥35% rubble
  (Technosols only). The code's `artefacts_urbic_pct ≥ 20` + designation proxy
  omits the ≥20 cm thickness and the two-stage artefact fraction; Technosol-only
  and rare, left as approximate proxy.
- **Evapocrustic (ev)** — p.133: "a saline crust ≤2 cm thick on the soil surface."
  The code's `surface_crust_type` regex + `top_cm < 5` is a reasonable surface-crust
  proxy; the ≤2 cm thickness is not separately enforced. Left as proxy.

## Gate

- 44 canonical fixtures: byte-identical (the Vertisol fixture stays non-Grumic /
  non-Mazic — its medium blocky surface correctly fails the ≤1 cm Grumic limit and
  its absent rupture_resistance keeps Mazic massive-only).
- Qualifier suite (bloco-b, v092a, decomp, closure), USDA/SiBCS Vertisol routing,
  WRB end-to-end: all green. WRB qualifier coverage unchanged (229/234).
- +5 lock-in tests (test-v09141). No KSSL gate (KSSL is WRB-blind).

This completes the Fix D WRB qualifier audit (base-status v0.9.129, texture
v0.9.130, colour v0.9.131, batch-11 v0.9.132, schema-unblock v0.9.133, and this
residue v0.9.141).
