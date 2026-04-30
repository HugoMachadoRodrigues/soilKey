# Cumulic Subgroup helper (Mollorthels / Umbrorthels)

Pass when:

- Mollic or umbric epipedon \>= 40 cm thick with texture finer than
  loamy fine sand; AND

- Slope \< 25 percent.

## Usage

``` r
cumulic_subgroup_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Slope is taken from `site$slope_pct` when available; if NA, assumed to
satisfy (TRUE).
