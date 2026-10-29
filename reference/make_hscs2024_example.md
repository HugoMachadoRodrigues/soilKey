# Example pedons for the Hungarian soil types

Small, plausible profiles written for the tests and examples, one per
type of the Hungarian diagnostic soil classification (Micheli et al.
2024), with designations in the FAO (2006) notation the text adopts and
the properties the key reads, including a total water-soluble salt
content (`soluble_salts_pct`) and exchangeable cations, so that the
types before each one are decided. They are not real profiles.

## Usage

``` r
make_hscs2024_example(
  type = c("peat", "anthropogenic", "rocky_skeletal", "rock_influenced", "solonetz",
    "solonchak", "swelling_clay", "meadow", "carbonate", "steppe", "clay_illuvial",
    "sandy", "brown_earth", "deposit", "weakly_developed")
)
```

## Arguments

- type:

  One of `"peat"`, `"anthropogenic"`, `"rocky_skeletal"`,
  `"rock_influenced"`, `"solonetz"`, `"solonchak"`, `"swelling_clay"`,
  `"meadow"`, `"carbonate"`, `"steppe"`, `"clay_illuvial"`, `"sandy"`,
  `"brown_earth"`, `"deposit"`, `"weakly_developed"`.

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
p <- make_hscs2024_example("meadow")
p$horizons[, c("designation", "top_cm", "bottom_cm")]
#>    designation top_cm bottom_cm
#>         <char>  <num>     <num>
#> 1:           A      0        30
#> 2:          Bl     30        60
#> 3:          Cr     60       120
```
