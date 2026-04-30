# Oxyaquic Subgroup helper (Spodosols, Mollisols, etc.)

Pass when the soil is saturated with water in one or more layers within
100 cm of the mineral soil surface for either or both:

- 20+ consecutive days; OR

- 30+ cumulative days.

## Usage

``` r
oxyaquic_subgroup_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

v0.8 proxy: pass when redoximorphic features OR low chroma in any layer
within 100 cm (subset of full aquic conditions).
