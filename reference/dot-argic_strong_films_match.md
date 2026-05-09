# Detect strong clay-film qualifier strings (Portuguese / English)

Internal helper used by
[`argic_with_strong_clay_films()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic_with_strong_clay_films.md)
and the v0.9.61
[`B_latossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_latossolico.md)
latossolic-vs-argic precedence rule. Strips Portuguese accents and
matches the standard SiBCS Cap 18 "strong" terminology: *comum*,
*abundante*, *common*, *abundant*.

## Usage

``` r
.argic_strong_films_match(films_chr)
```

## Arguments

- films_chr:

  Character vector of `clay_films_amount` values (Portuguese or
  English).

## Value

Logical scalar: `TRUE` if any element matches a strong qualifier;
`FALSE` for empty input or weak-only qualifiers.

## Details

"Pouca", "fraca", "few", "weak" do NOT count as strong (they are the
weak end of the SiBCS clay-film scale that allows latossolic features to
dominate).
