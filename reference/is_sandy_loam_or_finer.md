# Texture predicate: "sandy loam or finer"

WRB 2022 (Annex 1) and the USDA texture triangle agree on
`silt + 2 * clay >= 30` as the boundary between loamy sand and sandy
loam. Returns `TRUE`/`FALSE`/`NA`.

## Usage

``` r
is_sandy_loam_or_finer(sand, silt, clay)
```

## Arguments

- sand, silt, clay:

  Numeric percentages.
