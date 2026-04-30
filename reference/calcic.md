# Calcic horizon (WRB 2022)

Tests whether any horizon meets the calcic horizon criteria. The calcic
horizon is a horizon of secondary carbonate accumulation, diagnostic for
Calcisols and qualifying many other RSGs.

## Usage

``` r
calcic(pedon, min_thickness = 15, min_caco3_pct = 15)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 15).

- min_caco3_pct:

  Minimum CaCO3 percent in fine earth (default 15).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests called:

- [`test_caco3_concentration`](https://hugomachadorodrigues.github.io/soilKey/reference/test_caco3_concentration.md)
  – CaCO3 \>= 15%.

- [`test_minimum_thickness`](https://hugomachadorodrigues.github.io/soilKey/reference/test_minimum_thickness.md)
  – thickness \>= 15 cm.

v0.2 limitations: the WRB criterion of "5% absolute or relative more
CaCO3 than the underlying horizon" is not enforced; this captures true
calcic horizons but may also mark uniformly carbonate-rich substrates
that are not pedologically calcic. Cementation (petrocalcic) is not yet
detected. Both refinements are scheduled for v0.3.

## References

IUSS Working Group WRB (2022). *World Reference Base for Soil
Resources*, 4th edition. International Union of Soil Sciences, Vienna.
Chapter 3 – Calcic horizon.
