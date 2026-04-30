# WoSIS benchmark report (GraphQL) -- 2026-04-30

**Endpoint:** https://graphql.isric.org/wosis/graphql
**Continent filter:** South America
**WRB RSG filter:** (none)
**Country filter:** (none)
**Profiles pulled:** 100
**Profiles classified:** 100

## Top-1 agreement

- **Overall top-1: 0.130** (no stratification)
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
| partial      |      100 | 13/100 (13.0%) |

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
| Arenosol | 6/7 (85.7%) |
| Calcisol | 0/8 (0.0%) |
| Cambisol | 0/6 (0.0%) |
| Chernozem | 0/1 (0.0%) |
| Cryosol | 0/1 (0.0%) |
| Fluvisol | 2/7 (28.6%) |
| Gleysol | 0/4 (0.0%) |
| Gypsisol | 0/1 (0.0%) |
| Histosol | 1/1 (100.0%) |
| Kastanozem | 0/9 (0.0%) |
| Leptosol | 0/3 (0.0%) |
| Luvisol | 0/7 (0.0%) |
| Phaeozem | 0/15 (0.0%) |
| Planosol | 0/1 (0.0%) |
| Regosol | 3/9 (33.3%) |
| Solonchak | 1/5 (20.0%) |
| Solonetz | 0/13 (0.0%) |
| Vertisol | 0/2 (0.0%) |

## Confusion matrix

```
            assigned
target       Arenosol Calcisol Fluvisol Histosol Regosol Solonchak
  Arenosol          6        1        0        0       0         0
  Calcisol          1        0        2        0       5         0
  Cambisol          3        0        0        0       3         0
  Chernozem         0        0        0        0       1         0
  Cryosol           1        0        0        0       0         0
  Fluvisol          1        0        2        0       4         0
  Gleysol           0        0        0        0       4         0
  Gypsisol          0        0        0        0       1         0
  Histosol          0        0        0        1       0         0
  Kastanozem        1        0        1        0       7         0
  Leptosol          2        0        0        0       1         0
  Luvisol           0        1        0        0       6         0
  Phaeozem          1        2        2        0      10         0
  Planosol          0        0        0        0       1         0
  Regosol           5        0        1        0       3         0
  Solonchak         0        0        0        0       4         1
  Solonetz          0        0        6        0       5         2
  Vertisol          0        1        0        0       1         0
```

## Evidence-grade distribution

```
grade
  A 
100 
```

_Report emitted by `run_wosis_benchmark_graphql()` -- soilKey v0.9.11_
