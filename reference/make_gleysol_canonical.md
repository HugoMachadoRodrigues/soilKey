# Build the canonical Gleysol fixture

Synthetic Gleysol from a high-water-table floodplain: A with low chroma
but no explicit redox features (so gleyic test is anchored on Bg); Bg
with diagnostic redoximorphic features (35% by volume) within the upper
50 cm. By construction:

- [`gleyic_properties`](https://hugomachadorodrigues.github.io/soilKey/reference/gleyic_properties.md):
  PASSES on Bg.

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
make_gleysol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
