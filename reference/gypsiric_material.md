# Gypsiric material (WRB 2022 Ch 3.3.7): \\= 5% gypsum that is primary (not secondary). Without a "secondary fraction" schema column, v0.3.3 treats any layer with caso4_pct \>= 5 as gypsiric unless it explicitly carries gypsic-horizon designation.

Gypsiric material (WRB 2022 Ch 3.3.7): \\= 5% gypsum that is primary
(not secondary). Without a "secondary fraction" schema column, v0.3.3
treats any layer with caso4_pct \>= 5 as gypsiric unless it explicitly
carries gypsic-horizon designation.

## Usage

``` r
gypsiric_material(pedon, min_caso4_pct = 5)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_caso4_pct:

  Numeric threshold or option (see Details).
