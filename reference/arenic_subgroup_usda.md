# Arenic / Grossarenic Subgroup helper (Spodosols)

Pass when texture class (fine-earth fraction) is sandy throughout from
the surface to the top of the spodic horizon AND the spodic top depth
falls in `[min_spodic_top, max_spodic_top]`.

## Usage

``` r
arenic_subgroup_usda(pedon, min_spodic_top = 75, max_spodic_top = 125)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_spodic_top:

  Default 75.

- max_spodic_top:

  Default 125.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Standard cuts: - "Arenic": 75-125 cm - "Grossarenic": 125+ cm (use
min_spodic_top=125, max=Inf)
