# Build a metric-buffered SpatVector around a pedon's site

Internal: prefers `sf` for the geographic-to-UTM transform if available;
otherwise uses terra's own projection machinery. The returned SpatVector
is in lon/lat (EPSG:4326) so it can be passed to terra::extract
regardless of the raster CRS.

## Usage

``` r
soilgrids_buffer_vect(pedon, buffer_m = 250)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
