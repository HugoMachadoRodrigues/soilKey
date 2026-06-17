# KSSL subgroup before/after gate -- v0.9.147 (Humic colour-value differentia)

Validation of the **Humic Dystrudepts** predicate re-point on the bundled
**KSSL+NASIS** sample (the public `inst/extdata/kssl_nasis_sample.rds`, n=99,
99 carrying a `reference_usda_subgroup`). The 2895-pedon local cache used by the
earlier gates is not reachable in this environment; this run is the in-package
floor.

## What changed

The registered subgroup `Humic Dystrudepts` (soilKey code `KFGD`) was mapped to
`humic_inceptisol_usda` -- a mollic-OR-umbric **epipedon** test. The
authoritative KST 13ed differentia (`ST_criteria_13th` code `KFGY`) is instead a
pure dark **colour-value** test: "a colour value, moist, of 3 or less and a
colour value, dry, of 5 or less ... throughout the upper 18 cm of the mineral
soil." The subgroup is now mapped to the new `dark_color_value_usda` predicate
and moved to its KST position -- the residual subgroup immediately before
`Typic` (it was wrongly placed 4th, where a broad predicate could have stolen
profiles from the Andic / Aquandic / Vitrandic / Fluvaquentic / Oxyaquic /
Fragic / Lamellic intergrades listed below it).

The great-group `humic_inceptisol_usda` usages (Humudepts, Humustepts,
Humixerepts, Humicryepts, Humigelepts, Humaquepts) are **unchanged** -- at the
great-group level the epipedon test is correct ("Other [suborder] that have an
umbric or mollic epipedon", verified against `ST_criteria_13th`).

## Decision rule

As in the v0.9.113 / v0.9.121 / v0.9.123 gates: subgroup exact-match accuracy is
intrinsically low, so the decisive signal is **safety** -- any predicate that
turns a previously-correct prediction into a wrong one is a regression.

## Result

- changed predictions: **5**
- improved (now matches reference): **0**
- **worsened (was-correct -> now-wrong): 0**

All 5 changes are `Typic Dystrudepts -> Humic Dystrudepts`:

| reference subgroup    | before            | after             |
|-----------------------|-------------------|-------------------|
| aeric calciaquolls    | Typic Dystrudepts | Humic Dystrudepts |
| oxyaquic haplorthods  | Typic Dystrudepts | Humic Dystrudepts |
| eutropeptic rendolls  | Typic Dystrudepts | Humic Dystrudepts |
| cryic fragiorthods    | Typic Dystrudepts | Humic Dystrudepts |
| typic placaquods      | Typic Dystrudepts | Humic Dystrudepts |

## Honest interpretation

Unlike the Oxisol/Gypsid gates, this change **is** exercised by the bundled
sample (10 pedons key into Dystrudepts; 5 now refine to Humic). None of the 5
have a Dystrudepts reference -- they are dark-surface Mollisols/Spodosols/
Rendolls that the soilKey **great-group** key already misfiles as Dystrudepts (a
pre-existing recall gap driven by missing diagnostic data, out of scope here).
So all 5 were wrong before and remain wrong after: the subgroup re-point cannot
have flipped a correct prediction.

The change is nonetheless visibly **correct**: these dark-surface soils pass the
colour-value test but were *missed* by the full epipedon test (the soilKey
`mollic_epipedon_usda` also gates on base saturation, organic carbon, thickness
and structure, which fail or go undetermined on sparse KSSL rows). That is
precisely the "under-matches colour-only soils" defect the re-point fixes.

## Other safety evidence

1. **Criteria-exact predicate, audited against `ST_criteria_13th`.** Every
   Inceptisol "Humic" subgroup was read one at a time: the udept/ustept/xerept
   ones (Humic Dystrudepts KFGY, Eutrudepts KFFV, Dystrustepts KDDK,
   Dystroxerepts KEEN, Haploxerepts KEFP) are the colour-value test; the aquept
   ones are epipedon-defined (Fragiaquepts KADB / Densiaquepts KAEB =
   histic/mollic/umbric; Gelaquepts KAFF / Cryaquepts KAGL = mollic/umbric) or
   colour-plus-base-saturation (Endoaquepts KAKJ / Epiaquepts KAJG). Of these,
   only Humic Dystrudepts is registered in soilKey today.
2. **44/44 canonical fixtures byte-identical.** The two Dystrudepts fixtures
   (`make_plinthosol_canonical` / `make_plintossolo_canonical`) have an A
   horizon at colour value moist 4 (> 3), so they correctly fail the new test
   and stay `Typic Dystrudepts`. `make_umbrisol_canonical` stays
   `Typic Humudepts` (great-group epipedon path untouched).
3. **Full test suite green** (200 files / 1904 tests / 5838 checks / 0 failures),
   plus 18 new unit + end-to-end tests in `test-usda-humic-colour-value.R`.

## Deferred

- The four other colour-value "Humic" subgroups (Humic Eutrudepts,
  Dystrustepts, Dystroxerepts, Haploxerepts) are **not yet registered** in
  soilKey; `dark_color_value_usda` is ready to wire them if/when those
  great-group subgroup lists are expanded.
- The "after mixing" depth-weighted-mean colour path (the alternative to the
  "throughout, unmixed" path implemented here) is a deferred refinement.
