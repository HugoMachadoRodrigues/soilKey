# KSSL subgroup before/after gate -- v0.9.123 (intergrade subgroups)

Validation of the **+25** USDA intergrade subgroup additions (24 Humic
Rhodic/Xanthic Oxisols + Leptic Haplogypsids) on **2895** real KSSL+NASIS
pedons carrying a `reference_usda_subgroup`.

## Decision rule

As in the v0.9.113 / v0.9.121 gates: any modifier whose predicate turns a
previously-correct `Typic` into a wrong specific subgroup is excluded.

## Result

- changed predictions: **0**
- improved: 0
- **worsened (was-correct -> now-wrong): 0**

## Honest interpretation

The gate fires on **0** KSSL pedons -- KSSL+NASIS is overwhelmingly a
continental-US sample, which contains very few **Oxisols** (the 24 Humic
Rhodic/Xanthic subgroups are Oxisols) and few profiles that reach the
**Gypsids** great group via the soilKey key. So this dataset does **not
exercise** these subgroups, and "0 worsened" here is a safety floor, not a
strong test.

The real safety of this front rests on two things instead:

1. **Criteria-exact predicates, verified per subgroup against
   `ST_criteria_13th`.** Every modifier maps to an existing predicate whose
   match to the canonical differentia was checked one subgroup at a time:
   `humic_oxisol_usda` = the "16 kg/m2 OC within 100 cm" clause;
   `rhodic_subgroup_usda` / `xanthic_subgroup_usda` = the exact hue/value
   clauses; `gypsic_horizon_usda(max_top_cm = 18)` = "a gypsic horizon within
   18 cm". This is what excluded the mismatches (Natr- "Leptic" = soluble
   salts, not contact; Alfisol "Chromic" = chroma >= 4 within 18 cm, not the
   Vertisol chroma >= 3 within 30 cm).
2. **Append-before-default + first-match**, which makes it provably impossible
   for any profile that already matched a specific subgroup to change -- only
   `Typic` fall-throughs can refine.

## Canonical-fixture impact

Of the 44 canonical fixtures, **1** refines `Typic Haplogypsids -> Leptic
Haplogypsids` (`make_gypsisol_canonical`) -- a validated, same-great-group
refinement: its gypsic horizon (CaSO4 8 %, 35 cm thick) begins at 15 cm, within
the 18 cm window. The other 43 are byte-identical.

## Deferred (out of safe reach here)

Salts-based Leptic Natr- subgroups (need a visible-salt-crystal morphology
field soilKey does not carry; an EC proxy would be incorrect); soil-moisture-
regime intergrades (Aridic / Udic / Torrertic; need climate data); the Alfisol
Chromic-Vertic intergrades (need a distinct chroma >= 4 / 18 cm "after mixing"
predicate); Anthropic / Aquertic (compound predicates).
