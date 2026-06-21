# Look up a SoilGrids 250m soil property at WGS84 coordinates

Reads ISRIC SoilGrids 250m (Hengl et al. 2017, 2021) directly from the
ISRIC Cloud-Optimized GeoTIFF (COG) endpoint at
`https://files.isric.org/soilgrids/latest/data/` – no download required,
only the pixel under each query coordinate is transferred over HTTPS.

## Usage

``` r
lookup_soilgrids(
  coords,
  property = c("clay", "sand", "silt", "phh2o", "soc", "cec", "bdod", "nitrogen", "ocd",
    "ocs", "cfvo"),
  depth = c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"),
  quantile = c("mean", "Q0.05", "Q0.5", "Q0.95", "uncertainty"),
  baseurl = "https://files.isric.org/soilgrids/latest/data",
  raw = FALSE
)
```

## Arguments

- coords:

  A 2-column matrix or data.frame with `lon`, `lat` (WGS84 decimal
  degrees), or a length-2 numeric vector for a single query.

- property:

  One of the SoilGrids 250m predicted properties: `"clay"`, `"sand"`,
  `"silt"`, `"phh2o"`, `"soc"`, `"cec"`, `"bdod"`, `"nitrogen"`,
  `"ocd"`, `"ocs"`, `"cfvo"`.

- depth:

  Depth interval. One of `"0-5cm"`, `"5-15cm"`, `"15-30cm"`,
  `"30-60cm"`, `"60-100cm"`, `"100-200cm"`.

- quantile:

  Output quantile. One of `"mean"` (default), `"Q0.05"`, `"Q0.5"`,
  `"Q0.95"`, `"uncertainty"`.

- baseurl:

  Base URL of the SoilGrids COG endpoint. Default is the canonical ISRIC
  location; override only for a local mirror.

- raw:

  If `TRUE`, returns the integer raster value without scaling. Default
  `FALSE` (returns the value in conventional units).

## Value

Numeric vector of length `nrow(coords)`. `NA` outside the SoilGrids
footprint or on network errors.

## Details

SoilGrids stores integer rasters scaled per property; this helper
applies the canonical conversion factor so the returned value is in
conventional soil units (%, pH, g/kg, cmol(c)/kg, g/cm^3).

## See also

[`lookup_esdb`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_esdb.md),
[`lookup_mapbiomas_solos`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_mapbiomas_solos.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Single point
lookup_soilgrids(c(-43.0, -22.0),
                  property = "phh2o",
                  depth = "0-5cm",
                  quantile = "mean")

# Vector + multiple properties
coords <- rbind(c(-43.0, -22.0), c( -9.14, 38.72))
lookup_soilgrids(coords, "clay",  "0-5cm", "mean")
lookup_soilgrids(coords, "phh2o", "0-5cm", "mean")
} # }
```
