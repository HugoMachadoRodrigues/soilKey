# Apply the NASIS surveyor tie-breaker to a DiagnosticResult

When `result$passed` is `NA` (insufficient data) AND the surveyor
identified the matching diagnostic in NASIS, flips the result to `TRUE`
with a provenance note. `TRUE` or `FALSE` canonical results are NOT
overridden – the function returns the input unchanged in those cases.

## Usage

``` r
.apply_nasis_tiebreaker(result, pedon, pattern, feature_label)
```

## Arguments

- result:

  A
  [`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
  (from a canonical gate like
  [`mollic_epipedon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic_epipedon_usda.md)).

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- pattern:

  Regex pattern matched against `pediagfeatures.featkind`.

- feature_label:

  Short label for the provenance note.

## Value

The (possibly modified) DiagnosticResult.
