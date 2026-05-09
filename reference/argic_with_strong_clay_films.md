# Test whether a pedon's argic horizon has strong clay films

Wraps
[`argic()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
and inspects the `clay_films_amount` field at the argic-passing layers.
Returns a structured result that
[`B_latossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_latossolico.md)
uses to decide whether the SiBCS Cap 18 strong-films exclusion fires.

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
