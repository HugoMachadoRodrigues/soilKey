# Test that duripan_pct \>= threshold (Si-cemented nodules)

Default 10% per WRB 2022 Ch 3.1.7 (Duric horizon, p. 41). v0.3.1 reduced
default from 15% to 10% to match the canonical text.

## Usage

``` r
test_duripan_concentration(h, min_pct = 10, candidate_layers = NULL)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- min_pct:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).
