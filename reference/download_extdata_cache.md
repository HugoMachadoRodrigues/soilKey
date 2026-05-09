# Download one or more soilKey lazy-fetch caches from GitHub Release

soilKey ships four large benchmark caches (KSSL, KSSL+NASIS, AfSP, WoSIS
stratified) that are too large to embed in the CRAN source tarball.
Since v0.9.94 they are pinned to a versioned GitHub Release and
downloaded on demand into the user cache directory at
`tools::R_user_dir("soilKey", "data")`.

## Usage

``` r
download_extdata_cache(
  which = "all",
  release = .SOILKEY_LAZY_FETCH_RELEASE,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- which:

  Character vector of cache names to download. `"all"` (default)
  downloads every lazy-fetch cache. Valid names: `"afsp_sample"`,
  `"kssl_sample"`, `"kssl_nasis_sample"`, `"wosis_stratified_sample"`.

- release:

  GitHub Release tag to pull from (default `"v0.9.94-data"`). Override
  only if you maintain a local mirror.

- overwrite:

  If `TRUE`, redownload even if the file is already present in the user
  cache (default `FALSE`).

- verbose:

  Print progress (default `TRUE`).

## Value

Invisibly, a named character vector of local paths to the downloaded
files.

## Details

On first call to any of
[`load_kssl_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_sample.md),
[`load_kssl_nasis_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_nasis_sample.md),
[`load_afsp_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_afsp_sample.md),
or
[`load_wosis_stratified_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_wosis_stratified_sample.md),
soilKey checks for the file in the user cache. If missing, the loader
prompts (interactive sessions only) to download. Use
`download_extdata_cache()` to eagerly populate the cache without
prompting.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download every lazy-fetch cache once, ahead of any benchmark run:
download_extdata_cache()

# Or just the WRB AfSP sample:
download_extdata_cache("afsp_sample")
} # }
```
