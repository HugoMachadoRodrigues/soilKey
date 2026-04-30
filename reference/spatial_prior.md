# Spatial prior over RSGs (or Orders) at a pedon's location

Top-level dispatcher. Reads a categorical raster of soil classes
(SoilGrids globally, Embrapa for Brazil), buffers the pedon's
coordinates, tallies pixel classes within the buffer, and returns the
empirical class frequency as a probability distribution.

## Usage

``` r
spatial_prior(
  pedon,
  source = c("soilgrids", "embrapa"),
  system = c("wrb2022", "usda"),
  ...
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  with non-NULL `site$lat` / `site$lon`.

- source:

  Backend to query: `"soilgrids"` (default) or `"embrapa"`.

- system:

  Classification system: `"wrb2022"` (default) or `"usda"`. Embrapa
  source forces `"sibcs5"` internally regardless of this argument.

- ...:

  Passed through to the backend
  ([`spatial_prior_soilgrids`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior_soilgrids.md)
  or
  [`spatial_prior_embrapa`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior_embrapa.md)).

## Value

A `data.table` with columns `rsg_code` (character) and `probability`
(numeric, summing to 1). Empty if the buffer extracts no valid pixels –
callers should check [`nrow()`](https://rdrr.io/r/base/nrow.html).

## Details

The prior is intentionally separate from the deterministic key. Pass the
returned data.table to
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
via the `prior` argument; the result will then carry a `prior_check`
entry (consistent / inconsistent / not_run).
