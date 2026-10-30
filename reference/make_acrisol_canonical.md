# Build the canonical Acrisol fixture

Synthetic tropical-humid Acrisol on weathered gneiss: argic horizon at
Bt1 with low-activity clay (CEC/clay ~ 17 cmol_c/kg clay) and low base
saturation (BS ~ 25%). v0.9.310: in Bt1 and Bt2 the exchangeable Al (0.4
until then) exceeds the exchangeable bases (lowered from 1.15 to 0.72
cmolc/kg, so that the ECEC stays below 6 cmolc/kg clay, Geric), as the
WRB 2022 Acrisols ask for in 50-100 cm (Ch 4, p. 119); with Al below the
bases the profile is a Lixisol. By construction:

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md):
  PASSES on Bt1.

- [`acrisol`](https://hugomachadorodrigues.github.io/soilKey/reference/acrisol.md):
  PASSES (CEC low, exchangeable Al \> bases).

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
