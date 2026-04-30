# Test minimum horizon thickness

For each candidate layer, checks `bottom_cm - top_cm >= min_cm`. Used by
argic (default 7.5), ferralic (30), mollic (20), and others.

## Usage

``` r
test_minimum_thickness(h, min_cm = 7.5, candidate_layers = NULL)
```

## Arguments

- h:

  Horizons data.table.

- min_cm:

  Minimum thickness in cm.

- candidate_layers:

  Integer vector of horizon indices to test. If NULL, all layers are
  tested.
