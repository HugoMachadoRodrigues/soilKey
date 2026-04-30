# Download an OSSL subset and attach WRB / SiBCS / USDA labels

Fetches a region-filtered slice of the Open Soil Spectral Library via
[`download_ossl_subset`](https://hugomachadorodrigues.github.io/soilKey/reference/download_ossl_subset.md)
and post-joins WRB Reference Soil Group labels from WoSIS GraphQL by
spatial nearest-neighbour. The resulting artefact has the canonical
`list(Xr, Yr, metadata)` shape – with extra columns in `Yr`: `wrb_rsg`,
`wrb_label_source`, `wrb_label_distance_km`, plus optionally
`sibcs_ordem` and `usda_order` when `translate_systems = TRUE`.

## Usage

``` r
download_ossl_subset_with_labels(
  region = c("global", "south_america", "north_america", "europe", "africa", "asia",
    "oceania"),
  max_distance_km = 5,
  wosis_endpoint = NULL,
  translate_systems = TRUE,
  max_to_label = Inf,
  verbose = TRUE,
  query_fn = NULL,
  ...
)
```

## Arguments

- region:

  OSSL region filter; one of `"global"`, `"south_america"`,
  `"north_america"`, `"europe"`, `"africa"`, `"asia"`, `"oceania"`.

- max_distance_km:

  WoSIS spatial-join tolerance in kilometres (default 5). Profiles whose
  nearest WRB-labeled WoSIS neighbour is farther than this are left
  unlabeled.

- wosis_endpoint:

  Override for the WoSIS GraphQL endpoint (default
  `getOption("soilKey.wosis_graphql")`). The canonical value is
  `"https://graphql.isric.org/wosis/graphql"`.

- translate_systems:

  If `TRUE` (default), also adds `sibcs_ordem` and `usda_order` columns
  derived from the WRB label via the Schad (2023) Annex Table 1 / SiBCS
  5ª ed. Annex A correspondence. Those translations are 1:N for some
  classes; we pick the most-common partner and tag rows where the
  translation is genuinely ambiguous.

- max_to_label:

  Maximum number of profiles to query against WoSIS (default `Inf`).
  WoSIS throttles aggressive queries; cap this when running interactive
  demos.

- verbose:

  Emit `cli` progress messages.

- query_fn:

  Optional injection of the per-coordinate WoSIS query function. Default
  uses
  [`.query_nearest_wosis_wrb`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-query_nearest_wosis_wrb.md).
  Tests pass a stub here to exercise the join logic without network.

- ...:

  Forwarded to
  [`download_ossl_subset`](https://hugomachadorodrigues.github.io/soilKey/reference/download_ossl_subset.md).

## Value

A list with `Xr` (numeric matrix), `Yr` (data frame with the labels
attached), and `metadata` (list with the OSSL fetch metadata + the join
statistics: number of profiles labeled, average / max distance, WoSIS
endpoint, snapshot date).

## Why this function exists

OSSL stores Vis-NIR / MIR spectra and lab data but typically lacks WRB
Reference Soil Group labels on most profiles (KSSL data is
USDA-flavoured; non-US contributions are inconsistent). WoSIS, by
contrast, archives ~228 000 profiles with WRB labels but no spectra.
This function bridges the two so the user can run
[`classify_by_spectral_neighbours`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_by_spectral_neighbours.md)
on a real-data OSSL library without having to do the spatial join
themselves.

## Caveats and provenance

WRB labels obtained via spatial join are **weak labels**. The same
physical location may have been classified differently across surveys
(different WRB editions, different interpretations). Each row carries:

- `wrb_label_source = "wosis_spatial_join"`: label inherited from a
  WoSIS neighbour within `max_distance_km`.

- `wrb_label_distance_km`: the distance to that neighbour (NA when no
  neighbour was found within tolerance).

- `wrb_label_source = "ossl_native"`: label was already present in OSSL
  Yr (rare; preserved verbatim).

- `wrb_label_source = "missing"`: no neighbour within tolerance; the row
  stays unlabeled and will be skipped downstream.

Treat the labels as priors, not ground truth.

## See also

[`download_ossl_subset`](https://hugomachadorodrigues.github.io/soilKey/reference/download_ossl_subset.md),
[`classify_by_spectral_neighbours`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_by_spectral_neighbours.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Real OSSL South-America subset with WRB labels:
lib <- download_ossl_subset_with_labels(
  region          = "south_america",
  max_distance_km = 10
)
table(lib$Yr$wrb_rsg, useNA = "always")
table(lib$Yr$wrb_label_source)

# Drop into the spectral analogy classifier:
res <- classify_by_spectral_neighbours(
  spectrum     = my_query_spectrum,
  ossl_library = lib,
  k            = 25,
  region       = list(lat = -22.7, lon = -43.7,
                      radius_km = 500)
)
} # }
```
