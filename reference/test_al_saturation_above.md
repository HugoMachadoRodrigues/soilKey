# Test that aluminium saturation is at or above a threshold

Default 50% (Alisol RSG criterion). Uses `al_sat_pct` when reported;
otherwise falls back to `al_cmol / (ca+mg+k+na+al)_cmol * 100`.

## Usage

``` r
test_al_saturation_above(h, min_pct = 50, candidate_layers = NULL)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- min_pct:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).
