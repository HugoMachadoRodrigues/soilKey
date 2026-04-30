# Embrapa national soil-class spatial prior (Brazil only)

v0.5 stub. Reads a user-provided categorical raster of SiBCS orders /
suborders, buffers the pedon's site, tallies pixel classes, and returns
a probability distribution over SiBCS codes (or, with a user-provided
LUT, over WRB equivalents).

## Usage

``` r
spatial_prior_embrapa(
  pedon,
  raster_path = NULL,
  buffer_m = 3750,
  lut = NULL,
  n_classes_top = 10,
  ...
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- raster_path:

  Required. Path to a local categorical raster (GeoTIFF) of Embrapa
  SiBCS classes. There is no built-in file in v0.5 – download the
  polygon map from <https://www.embrapa.br/solos/sibcs> and rasterise
  it.

- buffer_m:

  Buffer radius in metres (default 3750, i.e. ~15-cell neighbourhood at
  250 m resolution).

- lut:

  Optional named character vector mapping raster integer values to
  soil-class codes. If NULL, raster categories are used as-is
  (terra::levels).

- n_classes_top:

  Keep only the top N classes (default 10).

- ...:

  Reserved.

## Value

A `data.table` with columns `rsg_code`, `probability`.

## Details

Unlike SoilGrids, Embrapa does not publish per-pixel probabilities, so
the empirical frequency over a neighbourhood window (default 15 x 15
cells = ~3.75 km radius at 250 m resolution) is used as an
approximation.
