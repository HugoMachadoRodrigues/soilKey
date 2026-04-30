# Gypsic horizon (WRB 2022)

Tests whether any horizon meets the gypsic horizon criteria. The gypsic
horizon is a horizon of secondary gypsum accumulation, diagnostic for
Gypsisols.

## Usage

``` r
gypsic(pedon, min_thickness = 15, min_gypsum_pct = 5)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 15).

- min_gypsum_pct:

  Minimum gypsum percent in fine earth (default 5).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests called:

- [`test_caso4_concentration`](https://hugomachadorodrigues.github.io/soilKey/reference/test_caso4_concentration.md)
  – gypsum \>= 5%.

- [`test_minimum_thickness`](https://hugomachadorodrigues.github.io/soilKey/reference/test_minimum_thickness.md)
  – thickness \>= 15 cm.

v0.2 limitations: the WRB rule that gypsum content must exceed the
underlying horizon by 1% (absolute) is not enforced. Petrogypsic
(cemented) horizons are not yet detected. Both deferred to v0.3.

## References

IUSS Working Group WRB (2022). *World Reference Base for Soil
Resources*, 4th edition. International Union of Soil Sciences, Vienna.
Chapter 3 – Gypsic horizon.
