# Ferralic horizon (WRB 2022)

Tests whether any horizon meets the ferralic horizon criteria. The
ferralic horizon is a subsurface horizon resulting from long and intense
weathering, characterized by very low cation exchange capacity per unit
clay – the canonical "low-activity clay" signal that defines the
Ferralsol RSG.

## Usage

``` r
ferralic(pedon, min_thickness = 30, max_cec = 16)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 30).

- max_cec:

  Maximum CEC (1M NH4OAc, pH 7) per kg clay (default 16).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests called:

- [`test_ferralic_texture`](https://hugomachadorodrigues.github.io/soilKey/reference/test_ferralic_texture.md)
  – texture sandy loam or finer.

- [`test_cec_per_clay`](https://hugomachadorodrigues.github.io/soilKey/reference/test_cec_per_clay.md)
  – CEC / clay \<= 16 cmol_c/kg clay.

- [`test_ferralic_thickness`](https://hugomachadorodrigues.github.io/soilKey/reference/test_ferralic_thickness.md)
  – thickness \>= 30 cm.

v0.3.1 alignment with WRB 2022 Ch 3.1.10 (p. 44): the older "ECEC \<= 12
cmol_c/kg clay" gate was removed because it is not in the canonical text
– only CEC (1M NH4OAc, pH 7) \<= 16 is required. The weatherable-mineral
test (\<= 10% by volume), water-dispersible-clay test, and
stratification / rock-structure exclusions remain deferred (they need
mineralogical data outside the canonical horizon schema) and are
refinements rather than gates.

## References

IUSS Working Group WRB (2022). *World Reference Base for Soil
Resources*, 4th edition. International Union of Soil Sciences, Vienna.
Chapter 3.1.10 – Ferralic horizon (p. 44).
