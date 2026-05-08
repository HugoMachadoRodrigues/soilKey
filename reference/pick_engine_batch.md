# Per-pedon batch engine recommendation

Vectorised version of
[`pick_engine`](https://hugomachadorodrigues.github.io/soilKey/reference/pick_engine.md)
returning the recommended engine for each pedon in a list.

## Usage

``` r
pick_engine_batch(pedons, min_score = 3L)
```

## Arguments

- pedons:

  A list of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  objects.

- min_score:

  Integer; forwarded to `pick_engine`.

## Value

Character vector of length(pedons) with values "aqp" or "soilkey".
