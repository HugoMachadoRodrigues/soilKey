# KSSL subgroup before/after gate -- v0.9.121 (colour / contact subgroups)

Validation of the **+57** USDA colour/contact subgroup additions (20 chromic,
12 xanthic, 9 calcic, 16 leptic) on **2895** real KSSL+NASIS pedons carrying a
`reference_usda_subgroup`. By append-before-default insertion plus the
first-match rule engine, the only classifications that can change are
former-`Typic` fall-throughs, whose "before" name is provably `Typic <same
great group>`; one new-rules pass therefore yields both before and after.

## Decision rule

Subgroup exact-match accuracy is intrinsically low (KSSL subgroups depend on
many criteria; partial agreement is expected), so the decisive signal is
**safety**, not net accuracy: any modifier whose predicate turns a
**previously-correct `Typic`** into a **wrong** specific subgroup is excluded.

## Result

- changed predictions: **83**
- improved (now matches reference): 0
- **worsened (was-correct -> now-wrong): 0**

## Per-modifier attribution (over changed pedons)

| modifier | n fired | improved | worsened |
|---|---:|---:|---:|
| chromic | 73 | 0 | 0 |
| xanthic | 1 | 0 | 0 |
| calcic | 0 | 0 | 0 |
| leptic | 9 | 0 | 0 |

## Exclusions

**None.** All four modifiers show **0 worsened** flips, so all 57 additions are
kept. This is cleaner than the v0.9.113 gate (which excluded four loose
intergrade proxies) because each colour/contact predicate is grounded directly
in the canonical KST 13th-edition differentia (`ST_criteria_13th`): chromic is
the Vertisol value/chroma "not-dark" test (not the WRB red-hue qualifier), with
the Aquerts variant dropping the chroma clause; leptic is the USDA shallow
densic/lithic/paralithic contact within 100 cm (not the WRB coarse-fragment
sense); xanthic and calcic reuse the existing predicates with the per-subgroup
depth window.

## Canonical-fixture impact

Of the 44 canonical fixtures, **2** refine from `Typic Hapluderts` to **Chromic
Hapluderts** (`make_vertisol_canonical`, `make_vertissolo_canonical`) -- a
validated, more-specific same-great-group refinement: their upper-30 cm colours
(chroma 4 / value moist 4) meet the Chromic criterion. The other 42 are
byte-identical.

## Deferred (distinct concepts, not implemented here)

Five `Leptic` subgroups whose differentia is NOT a contact were deferred to
avoid a wrong predicate: `Leptic Haplogypsids` (gypsic horizon within 18 cm)
and `Leptic Natralbolls / Natrudolls / Natrustolls / Natrustalfs` (visible
crystals of gypsum / more soluble salts within 40 cm).
