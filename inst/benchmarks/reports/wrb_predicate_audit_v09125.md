# WRB predicate correctness audit -- Phase 2 (v0.9.125)

A review of the **79 WRB diagnostic-horizon/property/material predicates** (the
drivers of RSG assignment) against WRB 2022 (4th ed). A fan-out Workflow flagged
divergences, but -- unlike the USDA audit, where `SoilTaxonomy::ST_criteria_13th`
gave **machine-verifiable** ground truth -- WRB has no such resource in the
package, so the agent flags rested on model knowledge. **Every flag was then
checked against the authoritative WRB 2022 PDF** (provided by the maintainer).
That cross-check was decisive: it refuted 4 agent flags AND found a real bug the
agents missed.

## Workflow flags vs the PDF

| Predicate | Agent flag | PDF verdict (WRB 2022) |
|---|---|---|
| `tephric_material` | glass 30%->5% | **REFUTED** -- 3.3.19: >= 30% (by grain count) is correct; 5% is the *andic/vitric* glass threshold |
| `histic_horizon` | surface 0->30 cm | **REFUTED** -- 3.1.15 has NO depth-from-surface criterion (organic material + saturated >=30 d or drained + >=10 cm); the "30 cm" is the USDA epipedon / Histosol-key rule |
| `plaggic` (depth) | add <=50 cm | **REFUTED** -- 3.1.29 is a surface horizon with no top-depth limit (can exceed 100 cm) |
| `shrink_swell_cracks` | width >=1 cm | **REFUTED** (by the workflow itself) -- 3.2.12: >= 0.5 cm is correct |
| `ornithogenic_material` | OR -> AND | **CONFIRMED** -- 3.3.15 requires BOTH bird remnants AND >= 750 mg/kg Mehlich-3 P |
| `sideralic_properties` | add cambic evidence | **CONFIRMED** -- 3.2.13 criterion 2 requires "evidence of soil formation as defined in cambic criterion 3" |
| `hypersulfidic_material` | add incubation test | **CONFIRMED but SCHEMA-BLOCKED** -- 3.3.8 criterion 3 is an 8-week aerobic incubation; no field records the result |
| `hyposulfidic_material` | cascade | **CONFIRMED but SCHEMA-BLOCKED** -- 3.3.9 = sulfidic S + pH + NOT hypersulfidic |
| `plaggic` (P) | (not flagged) | **BUG the agents missed** -- 3.1.29 criterion 2b is >= 100 mg/kg P (Mehlich-3); code used 50 |

## Fixed in v0.9.125 (2)

- **`ornithogenic_material`**: OR -> AND. Now requires both bird-activity
  evidence (designation) AND >= 750 mg/kg Mehlich-3 P (3.3.15). Stricter -- a
  high-P subsurface layer can no longer be ornithogenic without bird evidence.
- **`plaggic`**: Mehlich-3 P threshold 50 -> **100** mg/kg (3.1.29 crit 2b).

## Deferred (verified-but-not-yet-fixed)

- **`sideralic_properties`** criterion 2 (cambic soil-formation evidence): real,
  but needs the cambic criterion-3 structure-evidence check factored out -- a
  careful refactor, done separately.
- **`hypersulfidic` / `hyposulfidic`**: schema-blocked (no 8-week-incubation
  result field). Correct as far as the available data allow.
- **`plaggic`** is still an incomplete proxy vs the full 3.1.29 (it omits the
  texture, Munsell-colour, base-saturation and surface-raised criteria); only the
  clear P-threshold bug is fixed here.

## Method note

The headline lesson: the adversarial-verify pattern that made the USDA audit
trustworthy degrades without machine-readable ground truth -- the WRB agents
produced **4 false positives** (and missed the real `plaggic` P bug). Reading
the authoritative WRB 2022 text per predicate is what made this reliable. Gate:
44 canonical fixtures byte-identical + full suite (KSSL is USDA-labelled and
blind to WRB diagnostic changes, so it is not used here).
