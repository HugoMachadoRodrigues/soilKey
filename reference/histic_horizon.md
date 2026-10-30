# Histic horizon (WRB 2022)

A horizon of organic material (WRB 2022 Ch 3.1.15): organic material
(`oc_pct >= min_oc`, default 20, Ch 3.3.13) that is at least
`min_thickness` cm thick (default 10). The thickness is that of the
adjoining organic layers that run from the surface, taken together. A
layer whose organic carbon is not recorded may be organic material and
leaves the answer undecided (`NA`) where it could change it. The
water-saturation criterion (saturated for 30 or more consecutive days,
or drained) is not read.

## Usage

``` r
histic_horizon(pedon, min_thickness = 10, min_oc = 20, surface_top_cm = 0)
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

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Since v0.9.313 this is the horizon only: the Histosols are keyed by
`histosol()`, which reads the Chapter 4 key as worded. Until then this
function was also the Histosols gate, with organic material read at 12%
organic carbon and a second path of 40 cm of organic material within the
upper 80 cm.

## References

IUSS Working Group WRB (2022), Chapter 3.1.15, Histic horizon, and
Chapter 3.3.13, Organic material.
