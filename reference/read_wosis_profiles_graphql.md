# Query WoSIS profiles live from ISRIC

Reads soil profiles straight from ISRIC's WoSIS GraphQL endpoint.
Nothing is cached to disk: soilKey distributes no soil observations, and
each call is made by you, to ISRIC, under ISRIC's terms.

## Usage

``` r
read_wosis_profiles_graphql(
  country = NULL,
  wrb_rsg = NULL,
  n_max = 25L,
  licence_filter = c("permissive", "any"),
  timeout = 30
)
```

## Arguments

- country:

  Optional country name, e.g. `"Brazil"`.

- wrb_rsg:

  Optional WRB Reference Soil Group, e.g. `"Ferralsols"`.

- n_max:

  Maximum profiles to return (default 25).

- licence_filter:

  `"permissive"` (default) or `"any"`.

- timeout:

  Seconds to wait for ISRIC (default 30).

## Value

A data.frame, one row per profile, with `profile_id`, `profile_code`,
`country`, `lat`, `lon`, `wrb_rsg`, `usda_order`, `dataset`, `licence`
and `licence_short`. Zero rows if nothing matched. Never throws on a
network failure: it returns zero rows with a `"wosis_error"` attribute,
since an unreachable server is an expected condition.

## Licence

WoSIS is licensed *per profile* as each data provider specified, and
about half of it carries a NonCommercial restriction.
`licence_filter = "permissive"` (the default) returns only profiles free
of that restriction (CC BY, or public domain); `"any"` returns
everything. Either way the licence and the source dataset travel with
every profile, because CC BY requires attribution when you reuse the
data.

## See also

[`wosis_profile_to_pedon`](https://hugomachadorodrigues.github.io/soilKey/reference/wosis_profile_to_pedon.md)
to turn a row into a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
# \donttest{
# Requires network access to ISRIC.
p <- read_wosis_profiles_graphql(country = "Brazil", n_max = 5)
if (nrow(p)) p[, c("profile_code", "wrb_rsg", "licence_short")]
# }
```
