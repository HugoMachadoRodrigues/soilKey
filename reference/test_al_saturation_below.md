# Test that aluminium saturation is below a threshold

Default 50% (Luvisol RSG criterion). Uses `al_sat_pct` when reported;
otherwise falls back to computation from exchangeable bases and Al.

## Usage

``` r
test_al_saturation_below(h, max_pct = 50, candidate_layers = NULL)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- max_pct:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).
