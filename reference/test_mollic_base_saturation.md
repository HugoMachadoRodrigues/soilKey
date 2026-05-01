# Mollic base-saturation test (NH4OAc, pH 7, default \>= 50%)

Mollic base-saturation test (NH4OAc, pH 7, default \>= 50%)

## Usage

``` r
test_mollic_base_saturation(
  h,
  min_pct = 50,
  candidate_layers = NULL,
  allow_inference = TRUE
)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- min_pct:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).

- allow_inference:

  If `TRUE` (default), fall back to sum-of-cations / CEC arithmetic OR
  `al_sat_pct < 20` OR `ph_h2o >= 5.8` when `bs_pct` is missing.
