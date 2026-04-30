# Cambic horizon (WRB 2022)

Tests whether any horizon meets the cambic horizon criteria. The cambic
horizon is a subsurface horizon with evidence of pedological alteration
that does not meet the criteria for any stronger diagnostic horizon. It
is the diagnostic of Cambisols.

## Usage

``` r
cambic(pedon, min_thickness = 15, min_top_cm = 5)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 15).

- min_top_cm:

  Numeric threshold or option (see Details).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

v0.2 implementation tests three conditions:

- thickness \>= 15 cm
  ([`test_minimum_thickness`](https://hugomachadorodrigues.github.io/soilKey/reference/test_minimum_thickness.md))

- texture sandy loam or finer
  ([`test_texture_argic`](https://hugomachadorodrigues.github.io/soilKey/reference/test_texture_argic.md))

- NOT
  [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
  AND NOT
  [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md)

v0.2 limitations: WRB 2022 also excludes profiles with spodic, calcic,
gypsic, plinthic, vertic, and several other diagnostic horizons. Those
exclusions, plus the WRB criteria of "evidence of alteration"
(color/structure differences from parent material, carbonate removal),
are scheduled for v0.3.

## References

IUSS Working Group WRB (2022), Chapter 3, Cambic horizon.
