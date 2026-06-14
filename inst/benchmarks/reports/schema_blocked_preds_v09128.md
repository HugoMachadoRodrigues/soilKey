# Schema-blocked predicates unlocked (v0.9.128, Fix C)

Four new horizon-schema fields let five predicates enforce their **verbatim**
criteria instead of an air-dried-only / proxy approximation. Each grounded in
the authoritative source (`SoilTaxonomy::ST_criteria_13th` for USDA, the WRB
2022 PDF for the sulfidic materials).

## The contract: refine-when-present, byte-identical-when-absent

Every refinement is gated on the new field being measured. When the column is
absent (all existing fixtures, KSSL, FEBR, etc.), the predicate falls back to
its prior logic, so **no classification changes on existing data**. The field
only tightens the result when richer data is supplied.

## The four fields and five predicates

| field | predicate | verbatim criterion | source |
|---|---|---|---|
| `water_content_1500kpa_undried` | `vitrand_qualifying_usda` | 1500 kPa water < 15 % air-dried **and** < 30 % undried | Vitrands, KST 13ed Ch 6 |
| `particles_002_2mm_pct` | `vitrandic_subgroup_usda` | fine earth ≥ 30 % in 0.02–2.0 mm (beside ≥ 5 % glass) | Vitrandic*, KST 13ed Ch 9 |
| `cracks_top_cm` | `vertic_subgroup_usda` | cracks within 125 cm of the surface | Vertic*, KST 13ed |
| `incubation_ph` | `hypersulfidic_material`, `hyposulfidic_material` | incubation pH < 4 (hyper) / ≥ 4 (hypo) | WRB 2022 Ch 3.3.8 / 3.3.9 |

## `hyposulfidic_material` was unreachable

A notable fix beyond tightening: without the incubation test,
`hypersulfidic_material` = (S ≥ 0.01 % AND field pH ≥ 4), so
`hyposulfidic_material` = (S + pH) AND NOT(S + pH) = **always empty** — it could
never fire. With `incubation_ph`, the refined `hypersulfidic` excludes layers
that stay ≥ 4 on incubation, and `hyposulfidic` (its set-complement among
sulfidic + pH≥4 layers) becomes reachable. Verified:

| incubation_ph | hypersulfidic | hyposulfidic |
|---|---|---|
| absent | TRUE (potential) | FALSE |
| 3.5 (acidifies) | TRUE | FALSE |
| 6.0 (stays ≥ 4) | FALSE | **TRUE** |

## Verification

- 44 canonical fixtures **byte-identical** (full suite 5604 pass / 0 fail).
- KSSL n=2895 before/after (`vitrand`/`vitrandic`/`vertic` are wired into the
  USDA key): **0 changed** — the new fields are absent in that data.
- +16 unit tests (`test-v09128-schema-blocked-preds.R`): each predicate's
  absent-field (unchanged) and present-field (refined) behaviour.
- `inst/schemas/pedon-schema.json` regenerated; its sync test passes.
- `R CMD check --as-cran`: codoc OK, Status unchanged.
