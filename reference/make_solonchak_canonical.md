# Build the canonical Solonchak fixture

Synthetic Solonchak from a coastal-arid setting: surface salt
accumulation gives the diagnostic salic horizon (EC 25 dS/m over 25 cm);
EC declines but stays elevated in the Bz; non-saline C below. By
construction:

- [`salic`](https://hugomachadorodrigues.github.io/soilKey/reference/salic.md):
  PASSES on Az.

- [`gypsic`](https://hugomachadorodrigues.github.io/soilKey/reference/gypsic.md),
  [`calcic`](https://hugomachadorodrigues.github.io/soilKey/reference/calcic.md):
  FAIL.

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md),
  [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md),
  [`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md):
  FAIL.

## Usage

``` r
make_solonchak_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
