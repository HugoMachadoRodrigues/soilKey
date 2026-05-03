# WoSIS benchmark report (GraphQL) -- 2026-05-03

**Endpoint:** https://graphql.isric.org/wosis/graphql
**Continent filter:** South America
**WRB RSG filter:** (none)
**Country filter:** (none)
**Profiles pulled:** 30
**Profiles classified:** 30

## Top-1 agreement

- **Overall top-1: 0.167** (no stratification)
- Indeterminate (NA assignments): 0.000

## Top-1 stratified by data-coverage tier

Different profiles in WoSIS carry very different attribute sets.
soilKey reports `coverage_tier` per profile based on what was
actually present (not on the WoSIS schema):

- **full**: texture + (pH H2O or KCl) + CEC + OC.
- **partial**: texture + OC + (pH OR CEC).
- **minimal**: texture only or no chemistry.
- **empty**: no horizons.

| Coverage tier | Profiles | Top-1 |
|:--------------|---------:|:------|
| partial      |       30 | 5/30 (16.7%) |

Profiles below the **full** tier face a hard data ceiling:
many WRB RSGs (Vertisols, Nitisols, Andosols, Ferralsols) require
attributes (cracks, slickensides, Fe-DCB, Munsell, allophane
indicators) that WoSIS does not store at all. The honest
interpretation: top-1 in the **full** tier reflects soilKey
performance; top-1 in the **partial / minimal / empty** tiers
reflects the unrecoverable WoSIS data ceiling.

## Per-RSG agreement

| Target RSG | Match |
|:-----------|:------|
| Arenosol | 3/3 (100.0%) |
| Calcisol | 0/3 (0.0%) |
| Fluvisol | 0/3 (0.0%) |
| Gypsisol | 0/1 (0.0%) |
| Kastanozem | 0/3 (0.0%) |
| Luvisol | 0/3 (0.0%) |
| Phaeozem | 0/5 (0.0%) |
| Planosol | 0/1 (0.0%) |
| Regosol | 2/2 (100.0%) |
| Solonchak | 0/1 (0.0%) |
| Solonetz | 0/4 (0.0%) |
| Vertisol | 0/1 (0.0%) |

## Confusion matrix

```
            assigned
target       Arenosol Calcisol Fluvisol Regosol Solonchak
  Arenosol          3        0        0       0         0
  Calcisol          0        0        1       2         0
  Fluvisol          1        0        0       2         0
  Gypsisol          0        0        0       1         0
  Kastanozem        0        0        1       2         0
  Luvisol           0        1        0       2         0
  Phaeozem          0        1        0       4         0
  Planosol          0        0        0       1         0
  Regosol           0        0        0       2         0
  Solonchak         0        0        0       1         0
  Solonetz          0        0        0       2         2
  Vertisol          0        1        0       0         0
```

## Evidence-grade distribution

```
grade
 A 
30 
```

_Report emitted by `run_wosis_benchmark_graphql()` -- soilKey v0.9.23_
