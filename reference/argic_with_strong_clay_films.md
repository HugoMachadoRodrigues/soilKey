# Test whether a pedon's argic horizon has strong clay films

Wraps
[`argic()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
and inspects the `clay_films_amount` field at the argic-passing layers.
Returns a structured result that
[`B_latossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_latossolico.md)
used, from v0.9.61 to v0.9.240, to decide whether a B textural excluded
it.

## Usage

``` r
argic_with_strong_clay_films(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A list with:

- `passed` – logical, `TRUE` only when argic passes AND at least one
  argic-passing layer has a strong (*comum* / *abundante*) film
  qualifier.

- `layers` – integer vector of argic-passing layer indices (empty when
  `passed` is `FALSE`).

- `argic` – the underlying
  [`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
  from
  [`argic()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md).

- `films` – character vector of the `clay_films_amount` values at the
  argic-passing layers.

## Details

Since v0.9.241 the SiBCS key does not use this rule: the B textural is
tested by the items of the book and excludes the B latossolico, whose
cerosidade is tested layer by layer (see
[`B_textural`](https://hugomachadorodrigues.github.io/soilKey/reference/B_textural.md)
and
[`B_latossolico`](https://hugomachadorodrigues.github.io/soilKey/reference/B_latossolico.md)).
The function is kept to inspect a dataset.
