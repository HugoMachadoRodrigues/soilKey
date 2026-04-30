# Test for coarse texture throughout the upper part of the profile

Default predicate: `silt + 2 * clay < 15` (loamy sand or coarser) in
EVERY layer that intersects the upper `max_top_cm` (default 100).
Diagnostic for Arenosols.

## Usage

``` r
test_coarse_texture_throughout(h, max_top_cm = 100, candidate_layers = NULL)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- max_top_cm:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).
