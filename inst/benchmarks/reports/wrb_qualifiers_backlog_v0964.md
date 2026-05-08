# WRB 2022 qualifiers backlog (v0.9.64 and beyond)

After v0.9.63's Tier-1 batch (43 new qualifier functions), the
canonical NCSS-tech audit reports:

```
| Element                | Canonical | Implemented | Missing |
|------------------------|----------:|------------:|--------:|
| Reference Soil Groups  |        32 |          32 |       0 |
| Principal qualifiers   |       131 |         123 |       8 |
| Supplementary qualif.  |       170 |         127 |      43 |
```

**Date**: 2026-05-08
**soilKey version**: 0.9.63

This document tracks the remaining 8 PQ + 43 SQ qualifiers that
were not covered by v0.9.63, classified by complexity tier so
work can be prioritised.

---

## Tier-2 (composite of existing primitives)

These can be implemented quickly by composing existing diagnostics.
Estimated effort: ~30 minutes per qualifier; batch of ~10 can ship
in v0.9.64.

### Principal qualifiers
- **Entic** (Podzols): "having a bleached/eluvial horizon between
  0-50 cm but lacking spodic." Compose: `albic` ∧ ¬`spodic`.
- **Tonguic** (Chernozems / Kastanozems / Phaeozems / Umbrisols):
  "tonguing of A horizon into B (irregular boundary, residual A
  pockets at >= 50 cm)." Needs designation pattern `^A.*\\+|^B/A`
  + lower boundary check.
- **Nudiargic** (Acrisols / Lixisols / etc.): "argic horizon at
  the surface (no overlying eluvial layer)." Compose: `argic` AND
  shallowest passing argic layer top_cm <= 5 cm.
- **Nudinatric** (Solonetz): same logic as Nudiargic but for
  natric horizon.
- **Someric** (Phaeozems / Chernozems): "having anthrostagnic /
  irrigation-derived dark surface." Compose: anthric horizon +
  mollic colour criteria.
- **Neobrunic / Neocambic** (Retisols): "brunic / cambic that has
  formed in last few centuries." Compose: cambic + recent-age
  marker (no canonical age column in soilKey, so this needs
  layer_origin + designation patterns matching young-soil
  signatures).

### Supplementary qualifiers
Many SQs are Endo- / Epi- / Bathy- / Hyper- / Hypo- variants of
existing qualifiers but for diagnostics we don't yet wrap as
`qual_*`. The pattern is mechanical:
- **Endothionic / Endoabruptic / Endocalcic / Endogypsic / etc.**
  -- modify existing diagnostic to depth window 50-200 cm via
  `.q_within_depth()` (already implemented in v0.9.63, just need
  to wire 1-line qualifiers).
- **Bathy-X** -- 100-200 cm depth window via the same helper.
- **Hyper-X / Hypo-X** -- threshold-shifted versions of base
  diagnostics (e.g. Hypereutric = BS >= 80%; Hypereutric is
  already implemented; missing variants follow the same pattern).

The backlog list of 43 SQs probably reduces to ~15 after
discounting the duplicate Endo-/Bathy- variants of qualifiers
already covered.

## Tier-3 (new primitive required)

These need new horizon-attribute schema or new diagnostic
mechanics that soilKey doesn't yet implement. Estimated effort:
~2-4 hours per qualifier, often requiring data-loader patches.

### Principal qualifiers (Tier-3)
- **Anofluvic / Pantofluvic / Orthofluvic** -- already implemented
  in v0.9.63 as depth-window modifiers, but the underlying
  `fluvic_material` test is itself simplified (lacking the WRB
  2022 stratification + texture-contrast requirements). True
  full-fidelity implementation needs the canonical
  fluvic-material clause text from `wrb2022_canonical()$rsg`.

### Supplementary qualifiers (Tier-3)
- **Activic** -- "active aluminium" -- needs `al_kcl_cmol` schema
  field (KCl-extractable Al, not yet on the loader path for
  BDsolos / FEBR).
- **Bryic** -- "bryophyte cover at surface" -- needs vegetation
  / surface-cover field (not in soilKey schema yet).
- **Capillaric** -- "capillary rise zone" -- needs water-table
  depth + texture-derived capillary fringe estimate.
- **Cordic** -- "having a cordic horizon" -- new diagnostic;
  cordic horizon is a hardened layer not currently in soilKey.
- **Differentic** -- requires comparison of upper-vs-lower
  texture / clay strata (we have `abrupt_textural_difference`
  but not a permissive Differentic version).
- **Gilgaic** -- "gilgai microrelief" -- site-level field
  (`forma_relevo` regex match for "gilgai") needed.
- **Mahic** -- "manure-derived black surface" -- requires
  organic-content + cultural marker; rough.
- **Mineralic** -- "predominantly mineral over organic" -- could
  be implemented as inverse of Histosol qualifying logic.
- **Naramic / Lapiadic / Litholinic / Saprolithic** -- specific
  parent-material / weathering-stage qualifiers; need parent-rock
  type schema (only loosely populated).
- **Pelocrustic / Biocrustic / Evapocrustic / Puffic** -- surface
  crust morphology qualifiers; need surface-crust description
  field.
- **Protospodic / Protoargic / Protoandic** -- "early-stage"
  modifiers of spodic / argic / andic; need quantitative
  thresholds at lower limits.
- **Thixotropic** -- "shows thixotropic behaviour when worked";
  need slurry / consistency lab field.

## Recommended path to v1.0 closure

1. **v0.9.64**: implement remaining Tier-2 PQs + SQs (~25 functions,
   1-2 days). Should bring WRB PQ coverage to >= 95% and SQ to
   >= 85%.
2. **v0.9.65**: Tier-3 qualifiers that only need new schema fields
   (Activic, Differentic, Gilgaic). Add the schema fields to
   `class-PedonRecord.R` + loader patches.
3. **v0.9.66+**: Tier-3 qualifiers requiring real morphological /
   weathering-stage schema (Pelocrustic, Saprolithic, etc.).
   These are deferred until a compelling use case appears.

Of the 43 missing SQs, only ~10 require Tier-3 work; the other
~33 are mechanical Endo-/Bathy-/Hyper- patterns that v0.9.64 can
batch-ship in one focused sprint.
