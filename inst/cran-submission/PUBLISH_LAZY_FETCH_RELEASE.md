# Publishing the v0.9.94 lazy-fetch GitHub Release

soilKey v0.9.94 moved the four large benchmark caches out of the
CRAN source tarball. They are now distributed via GitHub Release
attachments, downloaded on demand by `load_*_sample()` functions
and the `download_extdata_cache()` helper.

This document is the **release maintainer checklist** for keeping
those .rds files in sync with code changes.

## When to publish a new lazy-fetch release

You need to publish a new GitHub Release whenever any of the four
.rds files changes content (re-pulled from upstream, schema migration,
field-name addition, etc.). The default release tag the loaders
target is `v0.9.94-data` -- that tag should always exist and always
point to the latest valid versions of all four files.

## How to publish

```bash
# 1. Make sure the four .rds files in inst/extdata/ are the ones you want
ls -lh inst/extdata/{afsp,kssl,kssl_nasis,wosis_stratified}_sample.rds

# 2. Create the release with all four .rds files attached
gh release create v0.9.94-data \
  --title "soilKey lazy-fetch data (v0.9.94)" \
  --notes "Benchmark caches: AfSP (n=120), KSSL (n=99), KSSL+NASIS (n=99), WoSIS stratified (n=130)" \
  inst/extdata/afsp_sample.rds \
  inst/extdata/kssl_sample.rds \
  inst/extdata/kssl_nasis_sample.rds \
  inst/extdata/wosis_stratified_sample.rds

# 3. Verify the release URL pattern matches what the loaders expect:
# https://github.com/HugoMachadoRodrigues/soilKey/releases/download/v0.9.94-data/<file>.rds
```

## Updating the release in place

If you need to refresh ONE cache (e.g., after pulling new WoSIS
data), use `gh release upload --clobber`:

```bash
gh release upload v0.9.94-data inst/extdata/wosis_stratified_sample.rds --clobber
```

## Bumping the release tag

If the schema of the .rds files changes incompatibly (e.g., a field
gets renamed and the code in v0.9.95+ expects the new name), bump
both the release tag AND the `.SOILKEY_LAZY_FETCH_RELEASE` constant
in `R/extdata-lazy-fetch.R`.

```r
# R/extdata-lazy-fetch.R
.SOILKEY_LAZY_FETCH_RELEASE <- "v0.9.95-data"   # update here
```

Then create the new release as above and document the breaking
change in NEWS.md.

## Test on a clean machine

After publishing, sanity-check on a clean R install:

```r
remove.packages("soilKey")
unlink(tools::R_user_dir("soilKey", "data"), recursive = TRUE)
remotes::install_github("HugoMachadoRodrigues/soilKey")
library(soilKey)
download_extdata_cache()                   # downloads all 4 to user cache
length(load_afsp_sample()$pedons)          # 120 (verifies AfSP loads)
length(load_kssl_sample()$pedons)          # 99
length(load_kssl_nasis_sample()$pedons)    # 99
length(load_wosis_stratified_sample()$pedons)  # 130
```

If any of those calls fail, the release attachment URL is wrong
or the file content is stale.
