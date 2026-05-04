# Toxic qualifier (tx): toxic concentration of organic or inorganic constituents.

WRB 2022 Ch 5 / Ch 4 Histosols + Cryosols + Technosols (variable pages):
substances at concentrations toxic to plant roots. Practical proxy: very
low pH (\<= 3.5, sulfuric / hyperacidic) OR very high electrical
conductivity (\>= 16 dS/m, equivalent to Salic) OR specific
contamination fields (heavy metals, hydrocarbons) which the soilKey
schema does not yet model. v0.9.33 v0 implementation uses pH \<= 3.5 OR
EC \>= 16 dS/m.

## Usage

``` r
qual_toxic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
