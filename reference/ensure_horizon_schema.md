# Coerce a horizons-like data.frame to the canonical schema

Adds any missing canonical columns as NAs of the right type and reorders
canonical columns first. Extra user-supplied columns are preserved at
the end. Coerces character values to numeric where the schema requires
it.

## Usage

``` r
ensure_horizon_schema(h)
```

## Arguments

- h:

  Input data.frame or data.table.

## Value

A `data.table`.
