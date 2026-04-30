# Carater salino (SiBCS Cap 1, p 39): 4 \<= CE \< 7 dS/m.

Carater salino (SiBCS Cap 1, p 39): 4 \<= CE \< 7 dS/m.

## Usage

``` r
carater_salino(pedon, min_ec = 4, max_ec = 7, max_depth_cm = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_ec:

  Limite inferior de CE em dS/m (default 4).

- max_ec:

  Limite superior (exclusivo) (default 7).

- max_depth_cm:

  Profundidade maxima em que camadas qualificam (default `NULL`). SiBCS
  Cap 14 Subgrupos usam 150.
