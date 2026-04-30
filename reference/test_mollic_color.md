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
  candidate_layers = NULL
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
