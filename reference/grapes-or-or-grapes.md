# Default-value-for-NULL operator

Returns the left-hand side if it is non-NULL, otherwise the right-hand
side. Re-exported so that downstream code can use the same idiom soilKey
itself uses internally.

## Usage

``` r
a %||% b
```

## Arguments

- a:

  The candidate value.

- b:

  The fallback used when `a` is NULL.

## Value

Either `a` or `b`.
