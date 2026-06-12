# Clear the in-memory KST13 cache

Useful when the vendored JSON files are updated mid-session. Frees ~3.1
MB.

## Usage

``` r
clear_kst13_cache()
```

## Value

`NULL`, invisibly. Called for its side effect of emptying the KST
13th-edition lookup cache.
