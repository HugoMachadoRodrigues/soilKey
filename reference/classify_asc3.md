# Classify a pedon under the Australian Soil Classification

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
through the Australian Soil Classification, 3rd edition (Isbell and the
National Committee on Soil and Terrain 2021), following the online
version with its errata. **Experimental**: this release keys the 15
orders (the key to soil orders); suborders, great groups, subgroups and
families follow.

## Usage

``` r
classify_asc3(
  pedon,
  rules = NULL,
  on_missing = c("warn", "silent", "error"),
  gapfill = FALSE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- rules:

  Optional pre-loaded rule set (`load_rules("asc3")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"asc3"`, the order in `name` and `rsg_or_order`, the
key trace, and `provisional_because` (orders before the assigned one
that could not be decided).

## Details

The ASC is largely morphological. The diagnostics read field texture
grades (`field_texture`, NCST Field Handbook codes; lab clay stands in
and is undecided near the limits), the Australian horizon notation (A1,
A2, B21t, Bhs, Bk ...; FAO/USDA designations are read by the role of
each horizon), the boundary between the A and the B, pH (strongly acid:
pHw below 5.5), ESP (sodic: 6 or more), subplasticity, free iron, grade
of pedality, carbonates and the moisture of the horizons when described
(`soil_water_status`: the Vertosols' cracks are not required in a solum
described moist). From the site it reads `drainage_class` and
`tidal_inundation` (Hydrosols, organic materials), `surface_condition`
(a cracking surface, Vertosols) and `designation_system` (`"asc"` for
the Australian notation). The choices where the text is silent are
listed in
`system.file("rules", "asc3", "readings.yaml", package = "soilKey")`.

## References

Isbell, R.F. and the National Committee on Soil and Terrain (2021). The
Australian Soil Classification, 3rd ed. CSIRO Publishing, Melbourne.
Online version: www.soilscienceaustralia.org.au/asc.

## See also

[`make_asc3_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_asc3_example.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md).

## Examples

``` r
res <- classify_asc3(make_asc3_example("chromosols"), on_missing = "silent")
res$name
#> [1] "Chromosol"
```
