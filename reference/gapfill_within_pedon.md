# Fill interior missing horizon attributes by within-pedon depth interpolation

For each requested attribute, builds a depth profile from the horizons
in which that attribute is *measured* (non-`NA`) and linearly
interpolates the value at the mid-depth of every horizon where it is
missing – but only for horizons whose mid-depth falls strictly between
the shallowest and deepest measured layer. Cells above the top or below
the bottom measured layer are left `NA`: the function interpolates, it
never extrapolates. Each fill is written with
`source = "inferred_prior"`, so the
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
authority order keeps it from displacing a measured, spectra-predicted
or VLM-extracted value, and any downstream `compute_evidence_grade` call
reports grade `"C"`.

## Usage

``` r
gapfill_within_pedon(pedon, attrs = NULL, confidence = 0.6, overwrite = FALSE)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  with at least two horizons.

- attrs:

  Character vector of horizon columns to fill. Defaults to the
  continuous depth-trending attributes a linear interpolation can
  reasonably estimate (clay/silt/sand, pH, organic carbon, CEC/ECEC,
  base/aluminium saturation, bulk density).

- confidence:

  Numeric in \\0, 1\\ recorded as the provenance confidence of each
  interpolated cell. Defaults to `0.6` – below a measured value but
  anchored on the profile's own data, hence above the `0.5` used for an
  external SoilGrids prior.

- overwrite:

  If `FALSE` (default) only `NA` cells are filled. If `TRUE`,
  non-measured cells are re-interpolated (measured cells are still never
  overwritten, and the provenance authority order is always respected).

## Value

Invisibly, the mutated `pedon`. An attribute `"gapfill_within_pedon"` on
the return value records how many cells were filled and for which
attributes.

## Details

This is the within-pedon companion to
[`apply_soilgrids_depth_prior`](https://hugomachadorodrigues.github.io/soilKey/reference/apply_soilgrids_depth_prior.md)
(which fills from an external SoilGrids profile rather than from the
profile's own measured layers). It is the mechanism behind the opt-in
`gapfill` argument of
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md),
[`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md),
[`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md)
and
[`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md).

Note that this mutates `pedon` in place (as
`apply_soilgrids_depth_prior` does). The `gapfill` argument of the
classifiers operates on a deep copy instead, so a classification call
never alters the caller's pedon.

## See also

[`apply_soilgrids_depth_prior`](https://hugomachadorodrigues.github.io/soilKey/reference/apply_soilgrids_depth_prior.md),
[`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md)

## Examples

``` r
h <- data.frame(
  top_cm    = c(0, 20, 40, 60),
  bottom_cm = c(20, 40, 60, 90),
  clay_pct  = c(15, NA, 35, 40)
)
p <- PedonRecord$new(horizons = h)
gapfill_within_pedon(p, attrs = "clay_pct")
p$horizons$clay_pct   # second horizon filled to ~25 by interpolation
#> [1] 15 25 35 40
```
