# Auto-detect PROJ_LIB and GDAL_DATA directories

Probes the common system locations for PROJ `proj.db` and GDAL data
directories, on macOS Homebrew (Apple silicon and Intel), Linuxbrew,
conda / mamba environments, and Debian / Ubuntu / Fedora apt or dnf
installs. Sets the corresponding environment variables only when they
are not already set, so a user-provided value always wins. Idempotent:
safe to call repeatedly.

## Usage

``` r
auto_set_proj_env(verbose = FALSE)
```

## Arguments

- verbose:

  If `TRUE`, emits a `cli` message confirming what was detected.

## Value

Invisibly, a named list with `PROJ_LIB` and `GDAL_DATA` (the values that
were set, or `NA_character_` if a value was already present or no
candidate was found).

## Details

Called automatically from `.onLoad`; call manually after installing PROJ
/ GDAL via Homebrew if you want to refresh the env without restarting R.
