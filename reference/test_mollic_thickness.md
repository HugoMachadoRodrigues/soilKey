# Mollic thickness test (default \>= 20 cm in v0.1)

WRB 2022 has more nuanced thickness criteria depending on whether the
soil overlies continuous rock at \<75 cm, but the simple absolute
threshold is the predominant case for non-shallow soils. Cumulative
thickness across multiple contiguous mollic-qualifying horizons is a
v0.2 refinement.

## Usage

``` r
test_mollic_thickness(h, min_cm = 20, candidate_layers = NULL)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- min_cm:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).
