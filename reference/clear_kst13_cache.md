# Clear the in-memory KST13 cache

Useful after pointing `options(soilKey.kst13_dir)` at other copies of
the files mid-session. Frees ~3.1 MB.

## Usage

``` r
clear_kst13_cache()
```

## Value

`NULL`, invisibly. Called for its side effect of emptying the KST
13th-edition lookup cache.
