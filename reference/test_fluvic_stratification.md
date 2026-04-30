# Test for fluvic stratification: irregular OC pattern + texture variability across consecutive horizons

v0.3 simplified: returns TRUE when (a) at least 3 layers within the
upper 100 cm exist, AND (b) clay_pct varies by \>= 8 percentage points
across consecutive layers (indicating depositional alternation), AND (c)
OC does not decrease monotonically with depth.

## Usage

``` r
test_fluvic_stratification(h, max_top_cm = 100, min_clay_swing = 8)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- max_top_cm:

  Numeric threshold or option (see Details).

- min_clay_swing:

  Numeric threshold or option (see Details).
