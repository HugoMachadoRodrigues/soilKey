# WoSIS benchmark report (GraphQL) -- 2026-04-30

**Endpoint:** https://graphql.isric.org/wosis/graphql
**Continent filter:** South America
**WRB RSG filter:** (none)
**Country filter:** (none)
**Profiles pulled:** 100
**Profiles classified:** 100

## Top-1 agreement

- Top-1: **0.120**
- Indeterminate (NA assignments): 0.000

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
| Solonchak | 0/5 (0.0%) |
| Solonetz | 0/13 (0.0%) |
| Vertisol | 0/2 (0.0%) |

## Confusion matrix

```
            assigned
target       Arenosol Calcisol Fluvisol Histosol Regosol
  Arenosol          6        1        0        0       0
  Calcisol          1        0        2        0       5
  Cambisol          3        0        0        0       3
  Chernozem         0        0        0        0       1
  Cryosol           1        0        0        0       0
  Fluvisol          1        0        2        0       4
  Gleysol           0        0        0        0       4
  Gypsisol          0        0        0        0       1
  Histosol          0        0        0        1       0
  Kastanozem        1        0        1        0       7
  Leptosol          2        0        0        0       1
  Luvisol           0        1        0        0       6
  Phaeozem          1        2        2        0      10
  Planosol          0        0        0        0       1
  Regosol           5        0        1        0       3
  Solonchak         0        0        0        0       5
  Solonetz          0        1        6        0       6
  Vertisol          0        1        0        0       1
```

## Evidence-grade distribution

```
grade
  A 
100 
```

_Report emitted by `run_wosis_benchmark_graphql()` -- soilKey v0.9.10_
