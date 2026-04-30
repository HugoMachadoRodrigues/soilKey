# Map a 95% prediction interval to a \[0, 1\] confidence score

Tightens confidence as the prediction interval narrows relative to the
predicted value: `confidence = 1 - (PI95_width / |value|) / 4`, floored
at 0 and capped at 1. When `value` is near zero we fall back to an
absolute-width heuristic so we never blow up.

## Usage

``` r
pi_to_confidence(pi95_low, pi95_high, value = NULL)
```

## Arguments

- pi95_low:

  Lower 2.5% quantile of the prediction.

- pi95_high:

  Upper 97.5% quantile of the prediction.

- value:

  Optional point prediction. When supplied, normalisation is by
  `|value|`; otherwise by `|midpoint|`.

## Value

Numeric in `[0, 1]`.

## Details

Properties of the mapping:

- Zero-width interval -\> confidence = 1.

- Interval whose width equals `|value| * 4` -\> confidence = 0.

- NA value or NA bounds -\> confidence = 0.5 (neutral).
