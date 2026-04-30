# Andic properties (WRB 2022)

Tests for the andic property complex – volcanic-ash-derived allophanic /
imogolitic / Al-humus material with low bulk density, high active Al +
Fe. Diagnostic of Andosols.

## Usage

``` r
andic_properties(pedon, min_alfe = 2, max_bd = 0.9)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_alfe:

  Minimum (Al_ox + 0.5\*Fe_ox) percent (default 2.0).

- max_bd:

  Maximum bulk density g/cm^3 (default 0.9).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

v0.3 simplified criteria:

- (Al_ox + 0.5 \* Fe_ox) \>= 2.0%

- bulk_density \<= 0.9 g/cm^3

Both must hold on the same layer.

v0.3 limitations: WRB 2022 also accepts phosphate retention \>= 70%,
glass content \>= 30% in the sand fraction, or specific Si-oxalate
alternatives. None of these are in the canonical schema; v0.4 will
extend.

## References

IUSS Working Group WRB (2022), Chapter 3, Andic properties.
