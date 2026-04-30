# Mollic horizon (WRB 2022)

Tests whether any near-surface horizon meets the mollic horizon
criteria. The mollic horizon is the diagnostic surface horizon of
Chernozems, Phaeozems, Kastanozems, and several other RSGs; it indicates
a thick, dark, base-rich, organic-matter-enriched topsoil formed under
steppe or comparable vegetation.

## Usage

``` r
mollic(
  pedon,
  min_thickness = 20,
  min_oc = 0.6,
  min_bs = 50,
  surface_top_cm = 5
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 20).

- min_oc:

  Minimum SOC % (default 0.6).

- min_bs:

  Minimum base saturation % (default 50).

- surface_top_cm:

  Maximum top depth (cm) for a horizon to be considered
  "surface-related" (default 5). v0.1 uses this as a proxy for the WRB
  rule that mollic must form continuously from the soil surface (after
  mixing of upper 20 cm if required).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests called:

- [`test_mollic_color`](https://hugomachadorodrigues.github.io/soilKey/reference/test_mollic_color.md)
  – moist value \<= 3, moist chroma \<= 3, dry value \<= 5.

- [`test_mollic_organic_carbon`](https://hugomachadorodrigues.github.io/soilKey/reference/test_mollic_organic_carbon.md)
  – SOC \>= 0.6%.

- [`test_mollic_base_saturation`](https://hugomachadorodrigues.github.io/soilKey/reference/test_mollic_base_saturation.md)
  – BS (NH4OAc, pH 7) \>= 50%.

- [`test_mollic_thickness`](https://hugomachadorodrigues.github.io/soilKey/reference/test_mollic_thickness.md)
  – horizon thickness \>= 20 cm.

- [`test_mollic_structure`](https://hugomachadorodrigues.github.io/soilKey/reference/test_mollic_structure.md)
  – not simultaneously massive AND very hard when dry.

v0.1 limitations: cumulative thickness across contiguous mollic-
qualifying horizons is not yet supported – this matters for profiles
where mollic criteria are met by an A1+A2 sequence but no single horizon
is \>= 20 cm thick. Mixing of upper 20 cm before the test (per WRB) is
also deferred to v0.2.

## References

IUSS Working Group WRB (2022). *World Reference Base for Soil
Resources*, 4th edition. International Union of Soil Sciences, Vienna.
Chapter 3 – Mollic horizon.
