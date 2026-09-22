# Build the canonical Luvisol fixture

Synthetic temperate-zone Luvisol on loess: clear textural
differentiation, Bt with clay coatings, high base saturation, high-
activity clay. By construction:

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md):
  PASSES on horizon Bt1 (clay increase from E (18%) to Bt1 (35%) gives
  ratio 1.94 in the 15-40% band; thickness 25 cm; texture clay loam; no
  glossic features).

- [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md):
  FAILS (CEC/clay ~ 45 cmol_c/kg clay in the Bt – well above the 16
  cmol_c/kg threshold).

- [`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md):
  FAILS (A horizon: moist value 4 \> 3, thickness 10 cm \< 20 cm).

## Usage

``` r
make_luvisol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
