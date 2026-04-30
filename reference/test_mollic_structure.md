# Mollic structure test (WRB 2022)

Excludes horizons that are simultaneously massive AND very hard when
dry. v0.1 implementation reads `structure_grade` and `consistence_moist`
as text and looks for the keyword pair.

## Usage

``` r
test_mollic_structure(h, candidate_layers = NULL)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).
