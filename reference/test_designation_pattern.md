# Test that a horizon designation matches a regex pattern

Useful for diagnostics that key on field-described features (e.g.,
glossic tongues for retic, R / Cr for leptic, "f" suffix for cryic /
frozen, hortic / irragric / plaggic / pretic / terric for anthric).

## Usage

``` r
test_designation_pattern(h, pattern, candidate_layers = NULL)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- pattern:

  A regex (case-insensitive).

- candidate_layers:

  Numeric threshold or option (see Details).
