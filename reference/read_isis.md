# Read profiles of the ISRIC World Soil Reference Collection (ISIS)

Reads profiles live from ISRIC's ISIS GraphQL service and returns them
as
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)s,
ready for
[`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md)
or
[`benchmark_isis`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_isis.md).
Nothing is written to disk. Each pedon carries the names ISRIC gives it,
the licence and the citation
([`isis_citation`](https://hugomachadorodrigues.github.io/soilKey/reference/isis_citation.md));
see
[`isis_profile_to_pedon`](https://hugomachadorodrigues.github.io/soilKey/reference/isis_profile_to_pedon.md)
for what the translation reads.

## Usage

``` r
read_isis(
  profile_codes = NULL,
  country = NULL,
  max_profiles = 100L,
  absent_unless_recorded = TRUE,
  timeout = 60,
  progress = NULL,
  all = FALSE
)
```

## Arguments

- profile_codes:

  Profile codes, e.g. `c("KE009", "ZW005")`.

- country:

  ISO 3166 two-letter country code, e.g. `"KE"`, read when
  `profile_codes` is `NULL`.

- max_profiles:

  The most profiles to read (default 100). Ignored with `all = TRUE`.

- absent_unless_recorded:

  Passed to
  [`isis_profile_to_pedon`](https://hugomachadorodrigues.github.io/soilKey/reference/isis_profile_to_pedon.md).

- timeout:

  Seconds to wait for each request (default 60).

- progress:

  Optional `function(done, total)`; `total` is `NA` with `all = TRUE`.

- all:

  If `TRUE`, reads the whole collection, including the profiles ISIS
  records with no country. It takes neither `profile_codes` nor
  `country`. A warning names the profiles that came with no horizons to
  classify; they stay in the result.

## Value

A named list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)s,
one per profile read. On a failed request the profiles read until then
are returned with a warning. Each pedon gives ISRIC's latest WRB name in
`site$reference_wrb_full`, its year in `site$reference_wrb_edition`,
both together in `site$reference_wrb_cited`, and every edition in
`site$reference_wrb_all`. ISRIC's FAO-Unesco labels are in
`site$reference_fao74` (Legend of 1974) and `site$reference_fao88`
(Revised Legend, stored by ISIS under 1997).

## Examples

``` r
if (FALSE) { # \dontrun{
p <- read_isis(c("KE009", "ZW005"))
classify_all(p$KE009)
p$KE009$site$reference_wrb_cited
everything <- read_isis(all = TRUE)
} # }
```
