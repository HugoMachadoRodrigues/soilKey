# Test for any layer with caco3_pct above a (low) threshold

Default threshold is 0.01% – effectively "any measurable secondary
carbonate". Used to distinguish Phaeozems (no carbonates within 100 cm)
from Chernozems and Kastanozems.

## Usage

``` r
test_carbonates_present(h, min_pct = 0.01, candidate_layers = NULL)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- min_pct:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).
