# Nitric Subgroup helper (Anhyturbels / Anhyorthels)

Pass when a horizon \>= 15 cm thick has nitrate concentration \>= 118
mmol(-)/L AND (thickness \* concentration) \>= 3500. (Nitrate is not in
the schema; v0.8 returns NA with missing flag.)

## Usage

``` r
nitric_subgroup_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
