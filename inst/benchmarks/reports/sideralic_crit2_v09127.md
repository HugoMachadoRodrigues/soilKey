# Sideralic properties — criterion 2 implementation (v0.9.127, Fix B)

WRB 2022 (Ch 3.2.13, p81) defines sideralic properties as requiring **both**:

1. one or both of: ≥ 8 % clay **and** CEC (1 M NH₄OAc, pH 7) < 24 cmol_c kg⁻¹
   clay; **or** CEC < 2 cmol_c kg⁻¹ soil;
2. **evidence of soil formation as defined in criterion 3 of the cambic
   horizon.**

The package implemented only criterion 1; criterion 2 was the confirmed-but-
deferred item from the v0.9.125 WRB predicate audit.

## A correction the verbatim PDF forced

The deferral note assumed criterion 2 could reuse the package's existing cambic
soil-formation check (`cand_str` / `structure_development`). Reading the
verbatim cambic-horizon definition (Ch 3.1.5, p39) showed that check actually
maps to cambic **criterion 2** ("soil aggregate structure in ≥ 50 % by volume"),
**not criterion 3**. Cambic **criterion 3** is a distinct *pedogenic-contrast*
test against adjacent layers:

- **3.a** vs the directly underlying layer (no lithic discontinuity): hue ≥ 2.5
  units redder (or yellower if the underlying hue is 5YR or redder); OR chroma
  ≥ 1 unit higher; OR clay ≥ 4 % (absolute) higher.
- **3.b** vs an overlying mineral layer ≥ 5 cm thick: hue ≥ 2.5 units redder; OR
  value ≥ 1 unit higher; OR chroma ≥ 1 unit higher.
- **3.c** vs the directly underlying layer: ≥ 5 % (absolute) less calcium
  carbonate equivalent (or gypsum) — carbonate removal.
- **3.d** Fe_dith ≥ 0.1 %, Fe_ox/Fe_dith ≥ 0.1, hue 2.5YR–2.5Y, chroma > 3.

So a faithful criterion 2 is a *new* helper, not a refactor of the structure
check. Implemented as `test_cambic_soil_formation()` (+ `.munsell_hue_units()`
for exact "2.5 units redder/yellower" arithmetic), required on the same layer as
criterion 1.

## Schema-driven simplifications (documented, not silent)

| sub-criterion | status | reason |
|---|---|---|
| 3.a hue/chroma/clay | full | `munsell_*_moist`, `clay_pct` present |
| 3.b hue/value/chroma | full | overlying-mineral guard via designation/OC |
| 3.c carbonate | partial | `caco3_pct` present; **gypsum omitted** (no column) |
| 3.d Fe | full | `fe_dcb_pct` (Fe_dith), `fe_ox_pct` present |
| ≥ 90 % exposed area | taken as met | no per-layer area-fraction field |
| lithic discontinuity | proxy | leading-integer designation convention (e.g. `2C`) |

## Honest missing-data behaviour

Where criterion 1 holds but criterion 2 cannot be assessed (no Munsell / clay /
Fe / carbonate adjacency data), `sideralic_properties` returns **NA**, not a
false positive. Previously, criterion-1-only made the property over-fire on
low-CEC parent material with no pedogenic development.

## Verification

- `sideralic_properties` is **not wired into any classification key** (grep of
  `inst/rules/`), so the change has **zero classification impact**: all 44
  canonical fixtures are byte-identical.
- +14 focused unit tests (`test-v09127-sideralic-crit2.R`): hue scale, each
  criterion-3 path, lithic-discontinuity blocking, NA-on-no-data, and the
  TRUE/NA/FALSE sideralic outcomes.
- Full suite: 5590 pass / 0 fail. `R CMD check --as-cran`: codoc OK, Status
  unchanged.
