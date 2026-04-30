# Test that a candidate layer starts at or above a top_cm threshold

Used to require surface contact (default top_cm \<= 0, i.e., layer must
reach the surface) or near-surface presence.

## Usage

``` r
test_top_at_or_above(h, max_top_cm = 0, candidate_layers = NULL)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- max_top_cm:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).
