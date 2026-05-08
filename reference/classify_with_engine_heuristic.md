# Classify a pedon with the engine chosen by \`pick_engine()\`

Convenience wrapper that routes
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
/
[`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md)
/
[`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md)
through whichever engine the heuristic recommends for the specific
pedon.

## Usage

``` r
classify_with_engine_heuristic(
  pedon,
  system = c("wrb2022", "sibcs", "usda"),
  min_score = 3L,
  ...
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- system:

  One of `"wrb2022"`, `"sibcs"`, `"usda"`.

- min_score:

  Forwarded to `pick_engine`.

- ...:

  Forwarded to the underlying classifier.

## Value

The result of the chosen classifier (a
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)).
The chosen engine is captured in `$trace$engine_used`.
