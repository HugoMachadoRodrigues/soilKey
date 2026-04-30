# Likely soil classes at a geographic location (spatial classification aid)

Returns a ranked list of the soil Reference Soil Groups (or SiBCS
ordens, or USDA orders) most likely to occur at the given point, based
on a global or regional dominant-soil raster (SoilGrids 2.0 by default).
This is the \*\*before-you-have-a-pedon helper\*\*: a pedologist
arriving in the field can call it with the GPS coordinates of the
planned profile pit and see which classes are expected, plus what
attributes typically distinguish them.

## Usage

``` r
soil_classes_at_location(
  lat,
  lon,
  system = c("wrb2022", "sibcs", "usda"),
  buffer_m = 1000,
  source_url = NULL,
  top_n = 5,
  verbose = TRUE
)
```

## Arguments

- lat, lon:

  Numeric WGS-84 coordinates.

- system:

  Classification system. One of `"wrb2022"` (default), `"sibcs"`,
  `"usda"`.

- buffer_m:

  Radius in metres around the point used to gather raster pixels
  (default 1000 m, i.e. roughly 4 SoilGrids pixels).

- source_url:

  Path / URL of the dominant-soil raster.

- top_n:

  Keep the top N classes by probability (default 5).

- verbose:

  Emit a `cli` summary.

## Value

A list as described under **Output**.

## Details

This function does **not** classify a profile. The deterministic key in
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
/
[`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md)
/
[`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md)
remains the only thing that assigns a class from horizon data. The
output here is purely informational – a "shopping list" of what to
confirm.

## Data source

For real use, point `source_url` at a regional SoilGrids "MostProbable
WRB" GeoTIFF / COG (one of the cuts at
<https://files.isric.org/soilgrids/latest/data/wrb/>). For tests,
`options(soilKey.test_raster = "/tmp/syn.tif")` is honoured. When no
source is given, the function emits a `cli_alert_warning()` and returns
an empty result – it does **not** pretend to know.

## Output

A list with three elements:

- `distribution`:

  A `data.table` with columns `rsg_code`, `rsg_name`, `probability`,
  sorted by descending probability.

- `typical_attributes`:

  A `data.table` keyed by `rsg_code` with the canonical attribute ranges
  that distinguish each class (clay range, CEC range, BS range, etc.).
  The values come from the WRB 2022 / SiBCS 5 / KST 13ed canonical
  thresholds, NOT from the raster.

- `site`:

  The site list passed in, plus the buffer radius and the source URL.

## See also

[`spatial_prior_soilgrids`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior_soilgrids.md)
for the post-classification consistency check.

## Examples

``` r
if (FALSE) { # \dontrun{
# Mata Atlântica, Rio de Janeiro state.
res <- soil_classes_at_location(
  lat        = -22.7,
  lon        = -43.7,
  system     = "wrb2022",
  source_url = "https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.vrt"
)
res$distribution         # ranked list of likely RSGs
res$typical_attributes   # canonical thresholds per RSG to confirm
} # }
```
