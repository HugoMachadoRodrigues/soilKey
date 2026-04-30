# Test that an arbitrary numeric column exceeds a threshold per layer

Generic helper: returns the layers where `h[[column]] >= threshold`.
Used by many of the v0.3.3 diagnostics that boil down to "layer with
attribute X above value V".

## Usage

``` r
test_numeric_above(h, column, threshold, candidate_layers = NULL)
```

## Arguments

- h:

  Horizons table.

- column:

  Name of the numeric column to test.

- threshold:

  Minimum value (inclusive).

- candidate_layers:

  Optional layer index restriction.

## Value

Sub-test result list.
