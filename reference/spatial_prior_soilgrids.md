# SoilGrids spatial prior

Reads a categorical raster of dominant Reference Soil Groups around the
pedon's site, buffers the point in metric coordinates, extracts all
pixel values within the buffer, and returns the empirical class
frequency as a probability distribution over RSG codes.

## Usage

``` r
spatial_prior_soilgrids(
  pedon,
  system = c("wrb2022", "usda"),
  buffer_m = 250,
  source_url = NULL,
  n_classes_top = 10,
  lut = NULL,
  ...
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  with non-NULL `site$lat` and `site$lon`.

- system:

  Classification system; `"wrb2022"` (default) maps SoilGrids integer
  codes through the WRB lookup table. `"usda"` is reserved for a future
  SoilGrids-USDA layer.

- buffer_m:

  Buffer radius in metres around the point (default 250 m, i.e. one
  SoilGrids pixel).

- source_url:

  Optional. A path or URL accepted by
  [`terra::rast`](https://rspatial.github.io/terra/reference/rast.html).
  If NULL, falls back to `getOption("soilKey.test_raster")`.

- n_classes_top:

  Keep only the top N classes by frequency (default 10). Set to `Inf` to
  keep all.

- lut:

  Optional named integer vector mapping raster values to RSG codes.
  Default is
  [`soilgrids_wrb_lut`](https://hugomachadorodrigues.github.io/soilKey/reference/soilgrids_wrb_lut.md);
  pass a custom one if your raster uses different codes.

- ...:

  Reserved for future use.

## Value

A `data.table` with columns `rsg_code`, `probability`.

## Data source

For real use, pass `source_url` pointing at a SoilGrids "MostProbable
WRB" GeoTIFF / COG, e.g. one of the regional cuts published at
`https://files.isric.org/soilgrids/latest/data/wrb/`. For tests, set
`options(soilKey.test_raster = "/path/to/syn.tif")` to point at a local
synthetic raster – this avoids network access in CI.

## Coordinate handling

We use
[`sf::st_transform`](https://r-spatial.github.io/sf/reference/st_transform.html)
when sf is available; otherwise we fall back to
[`terra::project`](https://rspatial.github.io/terra/reference/project.html)
on a single-point SpatVector. The buffer is constructed in metric (UTM)
coordinates so `buffer_m` is in metres regardless of the pedon CRS. The
raster itself is queried in its native CRS via terra's automatic
reprojection.

## See also

[`spatial_prior`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior.md),
[`soilgrids_wrb_lut`](https://hugomachadorodrigues.github.io/soilKey/reference/soilgrids_wrb_lut.md).
