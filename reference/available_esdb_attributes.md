# List ESDB Raster Library attributes available at a given root

Walks \`raster_root\` and returns the folder names that contain a valid
\`\<NAME\>.tif\` raster. Useful for discovery before calling
[`lookup_esdb`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_esdb.md).

## Usage

``` r
available_esdb_attributes(raster_root)
```

## Arguments

- raster_root:

  Path to the unpacked ESDB raster directory (typically
  \`\<some\>/ESDB-Raster-Library-1k-GeoTIFF-...\`).

## Value

A character vector of attribute names (sorted).

## Examples

``` r
if (FALSE) { # \dontrun{
available_esdb_attributes("~/data/ESDB-Raster-Library-1k-GeoTIFF-20240507")
#> [1] "AGLI1NNI" "AGLI2NNI" "AGLIM1" "AGLIM2" "ALT" "ATC" "AWC_SUB" ...
#>     [continued: 71 attributes]
} # }
```
