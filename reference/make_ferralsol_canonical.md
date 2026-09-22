# Build the canonical Ferralsol fixture

Synthetic but realistic Brazilian Latossolo Vermelho (Ferralsol per WRB
2022): deeply weathered, kaolinitic / oxidic, with the canonical
"low-activity clay" signature. Diagnostic outcomes are deterministic by
construction:

- [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md):
  PASSES on horizons Bw1 and Bw2 (CEC/clay = 8.3 cmol_c/kg clay;
  ECEC/clay = 3.6 cmol_c/kg clay; texture sandy clay / clay; thickness
  \>= 30 cm).

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md):
  FAILS (gradual clay increase, all pairwise ratios \< 1.2; absolute
  increment too small for the \>= 40% rule).

- [`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md):
  FAILS (chroma \> 3, BS \< 50%, A horizon \< 20 cm thick).

## Usage

``` r
make_ferralsol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
