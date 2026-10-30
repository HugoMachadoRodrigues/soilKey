# Build the canonical Retisol fixture

Synthetic temperate Retisol on loess over clay-rich substrate: bleached
E with glossic tongues penetrating the underlying argic Bt. By
construction
[`retic_properties`](https://hugomachadorodrigues.github.io/soilKey/reference/retic_properties.md)
passes via the "glossic" designation pattern;
[`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
also passes (this is correct – Retisols are argic + retic features, and
the WRB key tests RT before AC/LX/AL/LV). The Bt is below 1.5 kg/dm3, so
that the claric E has no stagnic properties (WRB 2022 3.2.14 criterion
4) and the profile does not key as a Stagnosol.

## Usage

``` r
make_retisol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
