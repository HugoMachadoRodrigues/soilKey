# Vertisol RSG gate (WRB 2022 Ch 4, p 101)

WRB 2022 Ch 4 (p. 101), Vertisols: other soils having

1.  a vertic horizon starting \\= 100 cm from the mineral soil surface;

2.  \\= 30% clay between the mineral soil surface and the vertic horizon
    throughout;

3.  shrink-swell cracks that start (a) at the mineral soil surface, (b)
    at the base of a plough layer, (c) directly below a self-mulching
    surface (a layer with strong granular, or strong angular or
    subangular blocky, structure of aggregates \\= 1 cm) or (d) directly
    below a surface crust, and extend to the vertic horizon.

Since v0.9.311 each item is TRUE, FALSE or NA (not recorded), and the
gate is NA when an item is undecided and none fails. Only a vertic
horizon that can start within 100 cm decides item 1, so unrecorded
layers below it do not leave the gate undecided. Item 3 reads the cracks
of each layer as
[`vertic_horizon`](https://hugomachadorodrigues.github.io/soilKey/reference/vertic_horizon.md)
does (`cracks_width_cm`, or slickensides or an `ss` designation as
evidence of them): every layer between the start and the vertic horizon
must have them, and a layer whose cracks are not recorded leaves the
item undecided. Until v0.9.310 item 3 asked for a layer with cracks
starting within 20 cm and read missing cracks as absent.

## Usage

``` r
vertisol(pedon, strict = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- strict:

  Logical or `NULL`. When `NULL` (default) it resolves via
  `getOption("soilKey.rsg_strict", FALSE)`. `TRUE` applies the Tier-3
  strengthened threshold.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Tier-3 strict mode (v0.9.98)

With `strict = TRUE` the overlying-clay threshold is raised from 30% to
35%, tightening the gate against marginally clayey profiles that satisfy
the vertic horizon but sit close to the Vertisol cut-off.
