# Carater carbonatico (SiBCS Cap 1, p 33)

\>= 150 g/kg (15%) de CaCO3 equivalente em qualquer forma de segregacao
(incl. nodulos, concrecoes). Excludente: nao satisfaz aos requisitos de
horizonte calcico.

## Usage

``` r
carater_carbonatico(pedon, min_caco3_pct = 15, max_depth_cm = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_caco3_pct:

  Limite de CaCO3 (default 15%).

- max_depth_cm:

  Profundidade maxima (`top_cm`) em que camadas qualificam (default
  `NULL` = sem restricao). SiBCS Cap 14 Subgrupos usam
  `max_depth_cm = 150`.
