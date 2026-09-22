# Build an empty horizons data.table with the canonical schema

Build an empty horizons data.table with the canonical schema

## Usage

``` r
make_empty_horizons(n = 0L)
```

## Arguments

- n:

  Number of rows (default 0).

## Value

A `data.table` with all canonical horizon columns filled with NAs of the
correct type.

## Examples

``` r
h <- make_empty_horizons(3)
nrow(h)
#> [1] 3
```
