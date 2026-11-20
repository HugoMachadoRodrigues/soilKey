# Histic horizon (WRB 2022)

A horizon of organic material (WRB 2022 Ch 3.1.15): organic material
(`oc_pct >= min_oc`, default 20, Ch 3.3.13) that is at least
`min_thickness` cm thick (default 10). The thickness is that of the
adjoining organic layers that run from the surface, taken together. A
layer whose organic carbon is not recorded may be organic material and
leaves the answer undecided (`NA`) where it could change it.

## Usage

``` r
histic_horizon(
  pedon,
  min_thickness = 10,
  min_oc = 20,
  surface_top_cm = 0,
  saturation = FALSE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness (cm), default 10.

- min_oc:

  Minimum soil organic carbon (%) of organic material, default 20 (WRB
  2022 Ch 3.3.13).

- surface_top_cm:

  Maximum top depth (cm) of the uppermost layer of the horizon (default
  0, the soil surface).

- saturation:

  If `TRUE`, also read criterion 1 (saturated for \>= 30 consecutive
  days in most years, or drained); default `FALSE`.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Criterion 1 of Ch 3.1.15 (p. 48), saturated with water for 30 or more
consecutive days in most years or drained, is read with
`saturation = TRUE`, as the WRB qualifiers do since v0.9.414: a site
recorded as artificially drained (`artificially_drained`, or a drainage
marker in `drainage_class` or `land_use`, as the Drainic qualifier reads
them) meets it, otherwise a layer's `water_saturation_days` of 30 or
more does, under 30 does not (the layer is then folic, Ch 3.1.12, p.
46), and a layer with neither recorded leaves the horizon undecided. The
default (`FALSE`) does not read it, as before, for the FAO 1974 stand-in
and other direct callers.

Since v0.9.313 this is the horizon only: the Histosols are keyed by
`histosol()`, which reads the Chapter 4 key as worded. Until then this
function was also the Histosols gate, with organic material read at 12%
organic carbon and a second path of 40 cm of organic material within the
upper 80 cm.

## References

IUSS Working Group WRB (2022), Chapter 3.1.15, Histic horizon, and
Chapter 3.3.13, Organic material.
