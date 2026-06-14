# USDA predicate correctness audit -- Phase 1 (v0.9.124)

A systematic review of the **102 USDA core diagnostic predicates** against their
verbatim KST 13th-edition criteria (`SoilTaxonomy::ST_criteria_13th` for key
taxa; KST Ch. 3 for diagnostic horizons), each flagged divergence then
**adversarially verified** by an independent skeptic to refute false positives.

## Review outcome (102 predicates)

| Verdict | n |
|---|---:|
| Correct | 58 |
| Defensible simplification (reasonable proxy for the available schema) | 17 |
| Missing-data-only (correct when the field is present) | 8 |
| Cannot verify | 1 |
| **Flagged divergent** | **18** |

Of the 18 flagged, adversarial verification **confirmed 8** and **refuted 10**
(misreadings or defensible choices -- e.g. `humic_oxisol_usda`'s 16 kg/m2,
`sombric_subgroup_usda`'s value <= 4, an alleged NA-logic bug in
`mollisol_qualifying_usda` -- all correctly left untouched).

## Confirmed divergences (8)

### Fixed in v0.9.124 (4) -- KSSL n=2895 gate: 0 changed, 0 worsened

| Predicate | Canonical | Was | Now |
|---|---|---|---|
| `rendoll_qualifying_usda` | lithic/paralithic contact within **50 cm** | 100 cm | 50 cm |
| `hydraquent_qualifying_usda` | in **all** horizons **20-50 cm**: n>0.7 **and clay >= 8%** | 0-50 cm, any, no clay | 20-50, all, clay >= 8% |
| `aeric_oxisol_usda` | chroma-3 horizon **directly below the epipedon** | counted A horizons | excludes A*/O* horizons |
| `duric_subgroup_usda` | cemented in **>= 90% of the pedon** | any single cemented layer | 90% cumulative thickness |

All four are *stricter* (remove false positives); the gate confirms none turns a
previously-correct classification into a wrong one.

### Deferred -- schema-limited (3): fixing with the data we have would be guessing

| Predicate | Why deferred |
|---|---|
| `vertic_subgroup_usda` | "cracks ... within 125 cm" needs a crack-*position* field; only crack thickness (`cracks_depth_cm`) exists |
| `vitrand_qualifying_usda` | needs *separate* air-dried vs undried 1500-kPa water fields; only one exists |
| `vitrandic_subgroup_usda` | needs a 0.02-2 mm particle-size field as the prerequisite to the glass >= 5% branch |

### Deferred -- coupled to a second bug the gate surfaced (1): `humult_qualifying_usda`

The Humults suborder qualifies via **either** (1) >= 0.9% weighted-average OC in
the upper 15 cm of the argillic/kandic horizon **or** (2) >= 12 kg/m2 OC in
0-100 cm. The code implemented only (2); criterion (1) was genuinely missing.
Wiring (1) is implemented but **held**, because the gate showed it inherits a
**top-detection error from `argillic_within_usda`**: on KSSL pedon 1828
(E clay 16.6 -> B clay 15.8, i.e. *no* clay increase at the "B"), that predicate
includes the transitional B in the argillic, so the upper-15-cm OC window starts
8 cm too shallow and the criterion fires on a soil that is `Typic Hapludults`.
Criterion (1) waits until `argillic_within_usda`'s top is corrected -- a NEW
finding the integration gate caught that the static review did not.

## Audit backlog (next phases)

- **`argillic_within_usda` top-detection** (couple with `humult` criterion 1):
  do not include a transitional horizon with no clay increase in the argillic.
- **Schema additions** that would unblock 3 deferrals: crack position; air-dried
  vs undried 1500-kPa water; 0.02-2 mm particle-size fraction.
- **Phase 2**: WRB cores (236) vs WRB 2022. **Phase 3**: SiBCS vs Embrapa 2018.
