# Calcaric material (WRB 2022 Ch 3.3.3): \\= 2% CaCO3 throughout the fine earth, primary carbonates from the parent material.

v0.9.315: WRB 2022 Ch 3.3.3 (p. 86), the material "contains \>= 2
carbonate equivalent" and "shows visible effervescence with 1 M HCl
throughout the fine earth". Each layer is read as the Calcaric qualifier
reads it (since v0.9.311): the CaCO3 equivalent decides where it is
recorded (`caco3_pct >= min_caco3_pct`), otherwise the field reaction
(`effervescence_class`: moderate or stronger is visible effervescence,
none and slight are not). Before v0.9.315 only `caco3_pct` was read, so
a profile described with the HCl reaction alone was never calcaric. TRUE
when some layer is calcaric material, FALSE when every layer is not, NA
otherwise.

## Usage

``` r
calcaric_material(pedon, min_caco3_pct = 2)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_caco3_pct:

  Numeric threshold or option (see Details).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
