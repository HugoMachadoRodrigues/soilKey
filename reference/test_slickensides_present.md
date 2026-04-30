# Test for slickensides at or above a presence level

Default accepted levels are `c("common", "many", "continuous")` (vertic
features, WRB 2022). The `slickensides` column accepts
`c("absent", "few", "common", "many", "continuous")`.

## Usage

``` r
test_slickensides_present(
  h,
  levels = c("common", "many", "continuous"),
  candidate_layers = NULL
)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- levels:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).
