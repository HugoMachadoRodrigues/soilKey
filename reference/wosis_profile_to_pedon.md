# Turn one WoSIS profile into a PedonRecord, with its horizons

Fetches the profile's layers live from ISRIC
([`read_wosis_layers_graphql`](https://hugomachadorodrigues.github.io/soilKey/reference/read_wosis_layers_graphql.md))
and builds a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
Every value carries a provenance note naming its WoSIS code, unit and
method. The site metadata carries the licence, the source dataset, the
date the data were read and the citation ISRIC asks for
([`wosis_citation`](https://hugomachadorodrigues.github.io/soilKey/reference/wosis_citation.md)),
so attribution survives into anything the user later exports. CC BY
requires it, and an extract that drops it turns a licensed use into an
unattributed one.

## Usage

``` r
wosis_profile_to_pedon(row, fetch_layers = TRUE, timeout = 30, progress = NULL)
```

## Arguments

- row:

  One row of
  [`read_wosis_profiles_graphql`](https://hugomachadorodrigues.github.io/soilKey/reference/read_wosis_profiles_graphql.md).

- fetch_layers:

  If `TRUE` (default) the layers are read from ISRIC; `FALSE` builds the
  site metadata only, with no network call.

- timeout:

  Seconds to wait for ISRIC (default 30).

- progress:

  Optional `function(done, total)` passed to
  [`read_wosis_layers_graphql`](https://hugomachadorodrigues.github.io/soilKey/reference/read_wosis_layers_graphql.md).

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
If the layers could not be read, it has no horizons and
`site$wosis_error` says why.

## Details

When the layers carry a more restrictive licence than the profile
listing reported, the more restrictive one is kept.
