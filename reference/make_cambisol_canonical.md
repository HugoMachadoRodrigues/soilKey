# Build the canonical Cambisol fixture

Synthetic temperate-zone Cambisol on weathered colluvium: modest
subsurface alteration in Bw without meeting argic clay-increase or
ferralic CEC criteria. By construction:

- [`cambic`](https://hugomachadorodrigues.github.io/soilKey/reference/cambic.md):
  PASSES on Bw (thickness 35 cm, sandy clay loam, no argic / no
  ferralic).

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md),
  [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md),
  [`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md),
  [`calcic`](https://hugomachadorodrigues.github.io/soilKey/reference/calcic.md),
  [`gypsic`](https://hugomachadorodrigues.github.io/soilKey/reference/gypsic.md),
  [`salic`](https://hugomachadorodrigues.github.io/soilKey/reference/salic.md):
  FAIL.

## Usage

``` r
make_cambisol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
