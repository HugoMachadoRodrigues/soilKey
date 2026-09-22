# Audit the strong-clay-films exclusion across a list of pedons

Applies
[`argic_with_strong_clay_films()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic_with_strong_clay_films.md)
to every pedon in `pedons` and returns a per-pedon table summarising how
the v0.9.61
[`B_latossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_latossolico.md)
latossolic-vs-argic rule resolves on the benchmark sample.

## Usage

``` r
audit_argic_strong_films(pedons, reference_filter = NULL)
```

## Arguments

- pedons:

  List of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  objects.

- reference_filter:

  Optional regex applied to `p$site$reference_sibcs` to keep only pedons
  whose reference matches (case-sensitive, ICU). Default `NULL` keeps
  every pedon.

## Value

A `data.frame` with columns `id`, `reference_sibcs`, `argic_passed`,
`has_films_at_argic`, `strong_films_at_argic`, and
`would_exclude_from_latossolo`.

## Details

Useful for empirical validation of the SiBCS Cap 18 precedence rule on
field-described datasets such as BDsolos and Redape, where clay-film
qualifiers are recorded in mixed Portuguese / English tokenisation. The
audit is read-only and never invokes
[`classify_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md).

## Examples

``` r
if (FALSE) { # \dontrun{
peds <- load_bdsolos_csv("RJ.csv")
a <- audit_argic_strong_films(peds, reference_filter = "LATOSSOLO")
table(a$would_exclude_from_latossolo)
} # }
```
