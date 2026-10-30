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

Since v0.9.314 the criteria are read as WRB 2022 Ch 2.1 (p. 23) sets
them for a horizon of several subhorizons: every criterion except the
thickness in every layer, unless the text says otherwise. Criteria 1
(structure; structure not recorded does not block it), 2 (\>= 0.6% SOC)
and 3 (colour, items a-c; a plough layer takes the colour of the
subhorizon below it) are read in each layer, and the horizon is the run
of such layers from the mineral soil surface. Criterion 5, the base
saturation, is the weighted average over the run (by pH where it is not
recorded, Annex 2 Table 9.2), and criterion 6 its thickness: \>= 20 cm
(6b), or since v0.9.315 \>= 10 cm directly over continuous rock, technic
hard material or a cryic, petrocalcic, petroduric, petrogypsic or
petroplinthic horizon (6a, p. 53; the umbric horizon names cryic,
petroduric and petroplinthic, p. 70). A moist value with no dry value
recorded reads the dry value as the moist value + 1; a layer with no
colour recorded at all and \>= 1.5% SOC is read as dark (Hugo,
2026-10-29). A dry colour alone leaves criterion 3 undecided unless its
value is above 5. The colour subtest records where each layer's reading
came from.

## References

IUSS Working Group WRB (2022). *World Reference Base for Soil
Resources*, 4th edition. International Union of Soil Sciences, Vienna.
Chapter 3 – Mollic horizon.
