# Look up a MapBiomas Solos raster value at WGS84 coordinates

MapBiomas Solos (Project MapBiomas, Brazil) distributes a national
raster of SiBCS classes at 30 m, downloadable from
<https://mapbiomas.org/en/produtos>. This helper mirrors the shape of
[`lookup_esdb`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_esdb.md)
but is local-file only: pass the path of the unpacked GeoTIFF and the
function reprojects the user's WGS84 lat/lon to the raster's native CRS,
extracts the pixel and (optionally) decodes the integer class code via a
user-supplied legend.

## Usage

``` r
lookup_mapbiomas_solos(coords, raster_path, legend = NULL)
```

## Arguments

- coords:

  A 2-column matrix or data.frame with `lon`, `lat` (WGS84 decimal
  degrees), or a length-2 numeric vector for a single query.

- raster_path:

  Path to the unpacked MapBiomas Solos GeoTIFF.

- legend:

  Optional two-column data.frame (first column = numeric value, second =
  SiBCS class name). When provided, the integer raster value is decoded;
  when `NULL`, the raw integer is returned.

## Value

Character vector of decoded class names (when `legend` is supplied) or
numeric vector of raster values. Same length as `nrow(coords)`. `NA` for
points outside the raster footprint.

## Details

MapBiomas does not bundle a \`.vat.dbf\`; the canonical legend is
published as a CSV / dictionary on their website. Pass it via `legend`
as a two-column data.frame (`value, class_name`) to enable decoding.

## See also

[`lookup_esdb`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_esdb.md),
[`lookup_soilgrids`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_soilgrids.md).

## Examples

``` r
if (FALSE) { # \dontrun{
tif <- "~/data/mapbiomas_solos_collection2_2023.tif"
legend <- data.frame(
  value = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L),
  class_name = c("Latossolo Vermelho-Amarelo",
                   "Latossolo Amarelo",
                   "Argissolo Vermelho-Amarelo",
                   "Argissolo Amarelo",
                   "Neossolo Quartzarenico",
                   "Cambissolo Haplico",
                   "Espodossolo",
                   "Gleissolo",
                   "Nitossolo",
                   "Planossolo",
                   "Plintossolo",
                   "Vertisolo",
                   "Outros")
)
lookup_mapbiomas_solos(c(-43.0, -22.0), tif, legend)
} # }
```
