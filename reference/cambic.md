# Cambic horizon (WRB 2022)

Tests whether any horizon meets the cambic horizon criteria. The cambic
horizon is a subsurface horizon with evidence of pedological alteration
that does not meet the criteria for any stronger diagnostic horizon. It
is the diagnostic of Cambisols.

## Usage

``` r
cambic(pedon, min_thickness = 15, min_top_cm = 5, engine = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 15).

- min_top_cm:

  Minimum top depth (cm) for a horizon to be considered cambic-eligible
  (default 5). Anchors the candidate set to subsurface layers. Since
  v0.9.314 the WRB reading asks it only when it is given.

- engine:

  v0.9.63+. One of `"soilkey"` (hand-coded path, default for
  back-compat) or `"aqp"` (canonical NRCS dispatch via
  [`aqp::getCambicBounds`](https://ncss-tech.github.io/aqp/reference/getCambicBounds.html)).
  When `NULL` (the new default) the function reads
  `getOption("soilKey.diagnostic_engine", "soilkey")`, so a global
  `options(soilKey.diagnostic_engine = "aqp")` flips every `cambic()`
  call without modifying call sites. The aqp engine fired 40.6 soilkey 0
  v0.9.50 LUCAS WRB benchmark from 0 100
  [`cambic_aqp`](https://hugomachadorodrigues.github.io/soilKey/reference/cambic_aqp.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

v0.2 implementation tests three conditions:

- thickness \>= 15 cm (`test_minimum_thickness`)

- texture sandy loam or finer (`test_texture_argic`)

- NOT
  [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
  AND NOT
  [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md)

v0.2 limitations: WRB 2022 also excludes profiles with spodic, calcic,
gypsic, plinthic, vertic, and several other diagnostic horizons. Those
exclusions, plus the WRB criteria of "evidence of alteration"
(color/structure differences from parent material, carbonate removal),
are scheduled for v0.3.

## v0.9.314

The WRB cambic horizon follows WRB 2022 Ch 3.1.5 (p. 39-40): criterion 4
is read layer by layer, so a layer that forms part of an argic,
ferralic, calcic, mollic or other listed horizon (or of a layer with
andic properties) is set aside, not the whole profile; the subsurface
layers are those below the layer at the mineral soil surface that are
not A horizons; the 15 cm are those of the adjoining layers; a layer
whose texture or structure is not recorded leaves the result undecided.
Criterion 3 is not tested. The USDA keys keep the earlier reading.

## References

IUSS Working Group WRB (2022), Chapter 3, Cambic horizon.
