# Read a lazy-fetch cache, downloading on first call if needed

Internal entry point used by every lazy-fetch loader. Encapsulates the
three-step resolution (bundled / user-cache / on-demand download with
interactive prompt).

## Usage

``` r
.lazy_fetch_readRDS(name)
```
