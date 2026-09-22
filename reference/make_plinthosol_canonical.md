# Build the canonical Plinthosol fixture

Synthetic seasonally-saturated tropical Plinthosol: A horizon with
typical Cerrado SOC; Btv with diagnostic plinthite (25% by volume over
60 cm); persistent plinthite at depth. By construction:

- [`plinthic`](https://hugomachadorodrigues.github.io/soilKey/reference/plinthic.md):
  PASSES on Btv and Cv.

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md),
  [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md),
  [`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md),
  [`spodic`](https://hugomachadorodrigues.github.io/soilKey/reference/spodic.md),
  [`calcic`](https://hugomachadorodrigues.github.io/soilKey/reference/calcic.md),
  [`gypsic`](https://hugomachadorodrigues.github.io/soilKey/reference/gypsic.md),
  [`salic`](https://hugomachadorodrigues.github.io/soilKey/reference/salic.md):
  FAIL.

## Usage

``` r
make_plinthosol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
