# Build the canonical Gypsisol fixture

Synthetic Gypsisol on gypsiferous parent material: shallow A; gypsum
accumulation rising sharply in the By1 horizon (35% gypsum over 50 cm) –
the diagnostic gypsic horizon. By construction:

- [`gypsic`](https://hugomachadorodrigues.github.io/soilKey/reference/gypsic.md):
  PASSES on By1 and By2.

- [`calcic`](https://hugomachadorodrigues.github.io/soilKey/reference/calcic.md),
  [`salic`](https://hugomachadorodrigues.github.io/soilKey/reference/salic.md):
  FAIL.

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md),
  [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md),
  [`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md):
  FAIL.

## Usage

``` r
make_gypsisol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
