# KSSL subgroup before/after gate -- v0.9.113

Validation of the +829 USDA subgroup additions on **2895** real KSSL+NASIS pedons carrying a `reference_usda_subgroup`, each classified at subgroup level with the rule base **before** and **after** the additions.

## Decision rule

Subgroup exact-match accuracy is intrinsically low (KSSL subgroups depend on many criteria; partial agreement is expected), so the decisive signal is **safety**, not net accuracy: any modifier whose predicate turns a **previously-correct `Typic`** into a **wrong** specific subgroup is excluded.

## Result (pre-exclusion measurement)

- exact-match: before 3.42%, after 3.14% (delta -0.28 pp)
- changed predictions: 195
- improved (now matches reference): 1
- **worsened (was-correct -> now-wrong): 9**

## Per-modifier attribution (over changed pedons)

| modifier | n | improved | worsened | neutral | net |
|---|---:|---:|---:|---:|---:|
| alfic | 22 | 0 | 4 | 18 | -4 |
| fluventic | 84 | 0 | 2 | 82 | -2 |
| psammentic | 28 | 0 | 2 | 26 | -2 |
| vertic | 3 | 0 | 1 | 2 | -1 |
| rhodic | 16 | 0 | 0 | 16 | +0 |
| sodic | 13 | 0 | 0 | 13 | +0 |
| calcic | 10 | 1 | 0 | 9 | +1 |
| aquic | 5 | 0 | 0 | 5 | +0 |
| fluvaquentic | 4 | 0 | 0 | 4 | +0 |
| andic | 3 | 0 | 0 | 3 | +0 |
| oxyaquic | 3 | 0 | 0 | 3 | +0 |
| thapto-humic | 3 | 0 | 0 | 3 | +0 |
| umbric | 1 | 0 | 0 | 1 | +0 |

## Exclusions

All 9 worsened flips are attributed solely to the four loose intergrade proxies **alfic, fluventic, psammentic, vertic** (heavy neutral over-firing, ~never matching the reference). These are excluded from Phase-1 (existing entries that use them are retained; only new additions are skipped). Removing them drives the worsened count to **0** while keeping the single improvement (`calcic natrudolls`).

`thaptic`, `rhodic`, `petronodic` and `umbric` -- the modifiers behind the four canonical-fixture refinements -- each show **0 worsened** and are kept.
