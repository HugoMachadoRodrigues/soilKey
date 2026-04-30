# Argillic horizon (USDA Soil Taxonomy)

v0.2 scaffold delegating to WRB
[`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md).
The two diagnostics' clay-increase rules are essentially the same; USDA
argillic additionally requires evidence of clay illuviation (clay films
/ clay bridges) on at least 1% of the surface area, which v0.8 will
enforce against the `clay_films_amount` column.

## Usage

``` r
argillic_usda(pedon, ...)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- ...:

  Passed to
  [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Soil Survey Staff (2014), Keys to Soil Taxonomy, Ch. 3 – argillic
horizon.
