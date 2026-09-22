# Clear the soilKey OSSL cache

Removes the per-region cache files written by
[`download_ossl_subset`](https://hugomachadorodrigues.github.io/soilKey/reference/download_ossl_subset.md).
Useful when a stale cache is suspected or when disk space is tight.

## Usage

``` r
clear_ossl_cache(region = NULL, cache_dir = NULL, verbose = TRUE)
```

## Arguments

- region:

  Optional character vector of regions to clear; the default `NULL`
  clears every cached file under \`tools::R_user_dir("soilKey",
  "cache")\`.

- cache_dir:

  Cache directory (defaults to the soilKey user-cache dir).

- verbose:

  If `TRUE`, prints which files were removed.

## Value

Invisibly, the character vector of files that were removed.
