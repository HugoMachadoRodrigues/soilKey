# Mollic Munsell color test (WRB 2022)

Moist value \<= 3 AND moist chroma \<= 3 AND dry value \<= 5. If
`munsell_value_dry` is missing, uses the conservative substitute
`munsell_value_moist + 1`.

## Usage

``` r
test_mollic_color(
  h,
  max_value_moist = 3,
  max_chroma_moist = 3,
  max_value_dry = 5,
  candidate_layers = NULL,
  allow_oc_inference = TRUE
)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- max_value_moist:

  Numeric threshold or option (see Details).

- max_chroma_moist:

  Numeric threshold or option (see Details).

- max_value_dry:

  Numeric threshold or option (see Details).

- candidate_layers:

  Optional restriction.

- allow_oc_inference:

  If `TRUE` (default), accept OC \\= 1.5 % in a surface A horizon as
  evidence of dark colour when both moist and dry Munsell are missing.
