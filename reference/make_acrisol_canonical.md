# Build the canonical Acrisol fixture

Synthetic tropical-humid Acrisol on weathered gneiss: argic horizon at
Bt1 with low-activity clay (CEC/clay ~ 17 cmol_c/kg clay) and low base
saturation (BS ~ 25%). By construction:

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md):
  PASSES on Bt1.

- [`acrisol`](https://hugomachadorodrigues.github.io/soilKey/reference/acrisol.md):
  PASSES (CEC low, BS low).

- [`lixisol`](https://hugomachadorodrigues.github.io/soilKey/reference/lixisol.md),
  [`alisol`](https://hugomachadorodrigues.github.io/soilKey/reference/alisol.md),
  [`luvisol`](https://hugomachadorodrigues.github.io/soilKey/reference/luvisol.md):
  FAIL.

- Other diagnostics: FAIL.

## Usage

``` r
make_acrisol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
