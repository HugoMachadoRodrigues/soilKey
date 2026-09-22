# Build the canonical Chernozem fixture

Synthetic Ukrainian / Russian steppe Chernozem on loess: thick dark Ah,
granular structure, secondary carbonates accumulating in the Bk. By
construction:

- [`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md):
  PASSES on horizon Ah1 (moist value 2, chroma 1, dry value 3; SOC 4%;
  BS 89%; thickness 30 cm; strong granular structure).

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md):
  FAILS (essentially no clay differentiation; ratios all close to 1).

- [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md):
  FAILS (CEC/clay ~ 120 cmol_c/kg clay – high-activity 2:1 clay).

## Usage

``` r
make_chernozem_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
