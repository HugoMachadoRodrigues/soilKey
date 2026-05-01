# Spodic horizon (WRB 2022)

Tests whether any horizon meets the spodic horizon criteria. The spodic
horizon is an illuvial horizon with active Al + Fe oxalate- extractable
material plus organic matter; diagnostic of Podzols.

## Usage

``` r
spodic(
  pedon,
  min_thickness = 2.5,
  min_alfe = 0.5,
  max_ph = 5.9,
  min_oc_in_b = 0.5
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 2.5).

- min_alfe:

  Minimum (Al_ox + 0.5 \* Fe_ox) percent (default 0.5).

- max_ph:

  Maximum ph_h2o (default 5.9).

- min_oc_in_b:

  Minimum OC % in the candidate Bh / Bs layer for the v0.9.19
  morphological inference path when Al / Fe oxalate are missing (default
  0.5).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests:

- [`test_spodic_aluminum_iron`](https://hugomachadorodrigues.github.io/soilKey/reference/test_spodic_aluminum_iron.md)
  – (Al_ox + 0.5\*Fe_ox) \>= 0.5%

- [`test_ph_below`](https://hugomachadorodrigues.github.io/soilKey/reference/test_ph_below.md)
  – ph_h2o \<= 5.9

- [`test_minimum_thickness`](https://hugomachadorodrigues.github.io/soilKey/reference/test_minimum_thickness.md)
  – thickness \>= 2.5 cm

v0.2 limitations: the WRB color criterion (hue 5YR or yellower with
chroma \<= 5, or specific dark colors) is not enforced. The (Al_ox +
Fe_ox)/clay \>= 0.05 alternative ratio test is not yet wired. Both
deferred to v0.3.

## References

IUSS Working Group WRB (2022), Chapter 3, Spodic horizon.
