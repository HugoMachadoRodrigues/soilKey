# Fill missing horizon attributes from a SoilGrids depth prior

For each horizon and each requested attribute, interpolates the value at
the horizon's mid-depth from the six standard SoilGrids 2.0 depth slices
(0-5, 5-15, 15-30, 30-60, 60-100, 100-200 cm) and writes it into the
pedon with `source = "inferred_prior"`. Existing values are preserved
unless `overwrite = TRUE`; the
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
authority order means a SoilGrids prior can never silently displace a
measured, spectra-predicted or VLM-extracted value.

## Usage

``` r
apply_soilgrids_depth_prior(
  pedon,
  attrs = NULL,
  depth_profiles = NULL,
  overwrite = FALSE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  with at least one horizon. For the live fetch it must also carry
  `site$lat` and `site$lon`.

- attrs:

  Character vector of horizon columns to fill. Defaults to all
  SoilGrids-backed attributes: `clay_pct`, `sand_pct`, `silt_pct`,
  `ph_h2o`, `oc_pct`, `cec_cmol`.

- depth_profiles:

  Optional named list mapping an attribute to a numeric vector of six
  slice values (0-5 ... 100-200 cm). When supplied the SoilGrids network
  call is skipped entirely – this is the path the test suite and offline
  users take.

- overwrite:

  If `FALSE` (default) only `NA` cells are filled. If `TRUE`, every
  requested cell is overwritten (subject to the provenance authority
  order).

## Value

Invisibly, the mutated `pedon`. An attribute `"soilgrids_depth_fill"` on
the return value records how many cells were filled.

## Details

This is the depth-resolved companion to
[`spatial_prior_soilgrids`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior_soilgrids.md)
(which returns a site-level RSG probability vector, not horizon
attributes), and the attribute-fill stage of
[`classify_from_photos`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_from_photos.md).

## Examples

``` r
if (FALSE) { # \dontrun{
p <- make_cambisol_canonical()
p$horizons$clay_pct <- NA_real_
# Offline: supply the six-slice profiles directly.
apply_soilgrids_depth_prior(
  p, attrs = "clay_pct",
  depth_profiles = list(clay_pct = c(18, 20, 24, 28, 30, 30)))
} # }
```
