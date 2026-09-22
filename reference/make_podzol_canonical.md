# Build the canonical Podzol fixture

Synthetic boreal / temperate-coniferous Podzol: bleached E (low clay,
low CEC), illuvial Bs with diagnostic Al/Fe oxalate accumulation,
weathered C. By construction:

- [`spodic`](https://hugomachadorodrigues.github.io/soilKey/reference/spodic.md):
  PASSES on Bs (Al_ox + 0.5\*Fe_ox = 0.6, pH 4.5, 40 cm thick).

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md),
  [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md),
  [`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md),
  [`cambic`](https://hugomachadorodrigues.github.io/soilKey/reference/cambic.md),
  [`plinthic`](https://hugomachadorodrigues.github.io/soilKey/reference/plinthic.md),
  [`calcic`](https://hugomachadorodrigues.github.io/soilKey/reference/calcic.md),
  [`gypsic`](https://hugomachadorodrigues.github.io/soilKey/reference/gypsic.md),
  [`salic`](https://hugomachadorodrigues.github.io/soilKey/reference/salic.md):
  FAIL.

## Usage

``` r
make_podzol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

E horizon Munsell is set to chroma 3 (rather than canonical 1-2 of a
true albic) to keep `gleyic_properties` clearly negative under the
conservative v0.2 criterion.
