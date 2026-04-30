# Protocalcic properties (WRB 2022 Ch 3.2.8)

Visible secondary carbonate accumulations, less than the calcic gate.
Detects via caco3_pct between 0.5 and the calcic threshold (15) AND
designation effervescence pattern (`k`).

## Usage

``` r
protocalcic_properties(pedon, min_caco3_pct = 0.5, max_caco3_pct = 15)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_caco3_pct:

  Numeric threshold or option (see Details).

- max_caco3_pct:

  Numeric threshold or option (see Details).
