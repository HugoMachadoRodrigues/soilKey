# Read one WoSIS profile's layers live from ISRIC

Fetches the layers of a single profile from ISRIC's WoSIS GraphQL
endpoint and returns them in the soilKey horizon schema. Nothing is
written to disk.

## Usage

``` r
read_wosis_layers_graphql(
  profile_id,
  timeout = 30,
  max_layers = 40L,
  page_size = 10L,
  progress = NULL
)
```

## Arguments

- profile_id:

  A WoSIS `profile_id`, as returned by
  [`read_wosis_profiles_graphql`](https://hugomachadorodrigues.github.io/soilKey/reference/read_wosis_profiles_graphql.md).

- timeout:

  Seconds to wait for each request (default 30).

- max_layers:

  Largest profile, in layers, that is read (default 40).

- page_size:

  Layers per request (default 10).

- progress:

  Optional `function(done, total)` called after each page, for a
  progress bar.

## Value

A data.frame of horizons in the soilKey schema, top to bottom, with
attributes `"provenance"` (one row per value: horizon, column, WoSIS
code, unit and method), `"licence"` (the most restrictive licence among
the layers), `"dataset"`, `"n_layers"` and `"depth_shift_cm"` (non-zero
when depths were shifted so an organic surface layer starts at 0 cm).
Zero rows, with a `"wosis_error"` attribute, when ISRIC cannot be
reached, the profile has no layers, or it has more than `max_layers`.

## Details

Units are converted from ISRIC's own catalogue (organic carbon, total
nitrogen and carbonate equivalent arrive in g/kg and become %), and only
properties whose method matches the soilKey column are mapped:
volumetric coarse fragments, saturated-paste EC, gravimetric water
retention and CEC at pH 7. WoSIS serves no exchangeable bases, base
saturation or Fe/Al oxides, so those columns stay empty and the
classifier reports them as missing.

## Load on ISRIC

The endpoint is a free public service whose database cancels any
statement that runs longer than about 30 seconds, and the cost of a
query grows with the number of layers times the number of properties.
The layers are therefore counted first with a cheap query, then read in
pages of `page_size` in a stable order. Profiles recorded as many fine
depth increments (some have 90 one-centimetre layers) exceed
`max_layers` and are not read unless you raise it deliberately.

## See also

[`wosis_profile_to_pedon`](https://hugomachadorodrigues.github.io/soilKey/reference/wosis_profile_to_pedon.md),
[`wosis_citation`](https://hugomachadorodrigues.github.io/soilKey/reference/wosis_citation.md).

## Examples

``` r
# \donttest{
# Requires network access to ISRIC.
p <- read_wosis_profiles_graphql(country = "Argentina", n_max = 1)
if (nrow(p)) {
  h <- read_wosis_layers_graphql(p$profile_id[1])
  h[, c("top_cm", "bottom_cm", "designation", "clay_pct", "oc_pct")]
}
#>   top_cm bottom_cm designation clay_pct oc_pct
#> 1      0         7           A       20   0.97
#> 2      7        23          2B       53   1.19
#> 3     23        45         3C1       54   1.40
#> 4     45       100         3C2       39   0.68
# }
```
