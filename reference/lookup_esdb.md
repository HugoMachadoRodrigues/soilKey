# Look up an ESDB raster value at WGS84 coordinates

Loads the requested attribute raster, reprojects WGS84 lat/lon input to
the raster's native CRS (typically LAEA Europe, EPSG:3035), and extracts
the value(s). When a Value Attribute Table (\`.vat.dbf\`) is available,
the integer raster value is decoded to its coded string (e.g. \`21\` -\>
\`"LV"\` -\> Luvisol).

## Usage

``` r
lookup_esdb(coords, attribute, raster_root, decode = TRUE)
```

## Arguments

- coords:

  A two-column matrix or data.frame with \`lon\` and \`lat\` (WGS84
  decimal degrees) – in that order. A single `c(lon, lat)` vector is
  also accepted.

- attribute:

  Name of the ESDB attribute folder, e.g. `"WRBLV1"` or `"WRBFU"`. See
  [`available_esdb_attributes`](https://hugomachadorodrigues.github.io/soilKey/reference/available_esdb_attributes.md).

- raster_root:

  Path to the unpacked ESDB raster directory.

- decode:

  If `TRUE` (default), decode the integer raster value to the VAT-coded
  string (e.g. `"21"` -\> `"LV"`). If `FALSE`, return the raw integer.

## Value

Character vector (decoded codes) or numeric vector (raw values) of the
same length as `nrow(coords)`. `NA` for points outside the raster
footprint.

## Details

Coordinates outside the European raster footprint return \`NA\` silently
(rather than erroring) so vectorised calls degrade gracefully.

## See also

[`available_esdb_attributes`](https://hugomachadorodrigues.github.io/soilKey/reference/available_esdb_attributes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
root <- "~/data/ESDB-Raster-Library-1k-GeoTIFF-20240507"

# Single point: Wageningen, Netherlands (5.66 E, 51.97 N)
lookup_esdb(c(5.66, 51.97), "WRBLV1", root)
#> [1] "GL"   # Gleysol per the ESDB 1km raster

# Vector: Lisbon + Berlin + Helsinki
coords <- rbind(c(-9.14, 38.72), c(13.40, 52.52), c(24.94, 60.17))
lookup_esdb(coords, "WRBLV1", root)
#> [1] "CM" "LV" "PZ"   # Cambisol, Luvisol, Podzol
} # }
```
