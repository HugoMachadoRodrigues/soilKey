# Resolve the local path of a v0.9.94 lazy-fetch cache file

Internal helper used by every lazy-fetch loader (`load_kssl_sample`,
`load_kssl_nasis_sample`, `load_afsp_sample`,
`load_wosis_stratified_sample`). Looks in three places:

1.  Bundled `inst/extdata/<name>.rds` (back-compat for developer
    checkouts and pre-v0.9.94 install paths).

2.  User cache at `tools::R_user_dir("soilKey", "data")`.

3.  Returns `NULL` if neither exists – the caller then decides whether
    to prompt the user for an on-demand download.

## Usage

``` r
.lazy_fetch_local_path(name)
```

## Arguments

- name:

  Base name without `.rds` extension. Must be one of
  `.SOILKEY_LAZY_FETCH_CACHES`.

## Value

Character path to a readable .rds file, or `NULL` if the cache is not
yet present locally.
