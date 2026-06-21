# Ferralic horizon (WRB 2022)

Tests whether any horizon meets the ferralic horizon criteria. The
ferralic horizon is a subsurface horizon resulting from long and intense
weathering, characterized by very low cation exchange capacity per unit
clay – the canonical "low-activity clay" signal that defines the
Ferralsol RSG.

## Usage

``` r
ferralic(pedon, min_thickness = 30, max_cec = NULL, engine = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 30).

- max_cec:

  Maximum CEC (1M NH4OAc, pH 7) per kg clay (default `NULL` = 16 in
  soilkey engine, 20 in aqp engine; see `engine`).

- engine:

  One of `"soilkey"` (default; strict 16 cmol_c/kg-clay threshold per
  WRB 2022) or `"aqp"` (relaxed 20 cmol_c/kg-clay – a regional tolerance
  that accommodates Brazilian / SOTERLAC Latossolos data, where
  Embrapa-style Mehlich/Ca+Mg+K+Al sum often reads ~17-20 on profiles
  that the canonical NRCS / WRB definition would accept as ferralic).
  `NULL` reads `getOption("soilKey.diagnostic_engine")`. The numeric
  threshold can also be overridden directly via
  `options(soilKey.ferralic_max_cec = ...)`.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests called:

- `test_ferralic_texture` – texture sandy loam or finer.

- `test_cec_per_clay` – CEC / clay \<= 16 (or 20 under `engine = "aqp"`)
  cmol_c/kg clay.

- `test_ferralic_thickness` – thickness \>= 30 cm.

v0.3.1 alignment with WRB 2022 Ch 3.1.10 (p. 44): the older "ECEC \<= 12
cmol_c/kg clay" gate was removed because it is not in the canonical text
– only CEC (1M NH4OAc, pH 7) \<= 16 is required.

v0.9.67 regional tolerance: BDsolos RJ benchmark (n=722 perfis) showed
88/115 Latossolos failing the strict 16-cmol gate because Embrapa lab
methodology often reads CEC at 17-20 on profiles that are unambiguously
Latossolos by every other criterion. The `engine = "aqp"` threshold of
20 closes that gap without redefining the WRB threshold itself; users
targeting strict WRB 2022 fidelity should keep `engine = "soilkey"`.

The weatherable-mineral test (\<= 10% by volume), water-dispersible-clay
test, and stratification / rock-structure exclusions remain deferred
(they need mineralogical data outside the canonical horizon schema) and
are refinements rather than gates.

## References

IUSS Working Group WRB (2022). *World Reference Base for Soil
Resources*, 4th edition. International Union of Soil Sciences, Vienna.
Chapter 3.1.10 – Ferralic horizon (p. 44).
