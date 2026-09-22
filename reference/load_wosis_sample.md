# Load the bundled WoSIS South-America sample

Returns a 40-profile snapshot of WoSIS GraphQL data pulled on 2026-05-03
with `continent = "South America"`. The data is a frozen artefact – do
NOT use it for current paper-grade benchmarks (the WoSIS database is
updated periodically; the bundled snapshot is for reproducible tests and
offline development only).

## Usage

``` r
load_wosis_sample()
```

## Value

A list as described above.

## Details

For up-to-date benchmarks, call `run_wosis_benchmark_graphql()` directly
against the live ISRIC GraphQL endpoint.

## Returned data

A list with elements:

- `profiles_raw` – the parsed GraphQL response (one element per profile;
  nested layer arrays).

- `pedons` – `PedonRecord` objects ready for classification (one per
  profile).

- `pulled_on` – `Date` of the snapshot.

- `endpoint`, `filter`, `n_pulled` – metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
sample <- load_wosis_sample()
length(sample$pedons)
#> 40
classify_wrb2022(sample$pedons[[1]])$rsg_or_order
} # }
```
