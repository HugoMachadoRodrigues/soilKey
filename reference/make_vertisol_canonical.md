# Build the canonical Vertisol fixture

Synthetic Vertisol from a smectite-rich plain: deep clay (50-55%) with
strong slickensides in the Bss horizon. Surface chroma 4 (above the
mollic cap) so that vertic_properties is the only v0.2 diagnostic that
passes. By construction:

- [`vertic_properties`](https://hugomachadorodrigues.github.io/soilKey/reference/vertic_properties.md):
  PASSES on Bss and BC.

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md),
  [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md),
  [`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md),
  [`cambic`](https://hugomachadorodrigues.github.io/soilKey/reference/cambic.md),
  [`plinthic`](https://hugomachadorodrigues.github.io/soilKey/reference/plinthic.md),
  [`spodic`](https://hugomachadorodrigues.github.io/soilKey/reference/spodic.md),
  [`calcic`](https://hugomachadorodrigues.github.io/soilKey/reference/calcic.md),
  [`gypsic`](https://hugomachadorodrigues.github.io/soilKey/reference/gypsic.md),
  [`salic`](https://hugomachadorodrigues.github.io/soilKey/reference/salic.md):
  FAIL.

## Usage

``` r
make_vertisol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
