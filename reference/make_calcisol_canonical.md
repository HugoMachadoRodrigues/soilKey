# Build the canonical Calcisol fixture

Synthetic semi-arid Calcisol on calcareous loess: A horizon with modest
secondary carbonate; a thick Bk1 with the diagnostic calcic horizon (35%
CaCO3 over 40 cm); deepening accumulation in Bk2. By construction:

- [`calcic`](https://hugomachadorodrigues.github.io/soilKey/reference/calcic.md):
  PASSES on Bk1 and Bk2.

- [`gypsic`](https://hugomachadorodrigues.github.io/soilKey/reference/gypsic.md),
  [`salic`](https://hugomachadorodrigues.github.io/soilKey/reference/salic.md):
  FAIL.

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md),
  [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md),
  [`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md):
  FAIL.

## Usage

``` r
make_calcisol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
