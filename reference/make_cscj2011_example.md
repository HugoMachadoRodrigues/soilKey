# Example pedons for the CSCJ great groups

Small, plausible profiles written for the tests and examples, one per
great group of the Comprehensive Soil Classification System of Japan
(2011), with designations in the Japanese notation. Most are the
[`make_scsj2017_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_scsj2017_example.md)
profiles of the great group of the same concept; the Dark Red soil (from
limestone) is the CSCJ's own. They are not real profiles.

## Usage

``` r
make_cscj2011_example(
  great_group = c("man_made", "organic", "podzol", "andosol", "dark_red", "lowland",
    "red_yellow", "stagnic", "brown_forest", "regosol")
)
```

## Arguments

- great_group:

  One of `"man_made"`, `"organic"`, `"podzol"`, `"andosol"`,
  `"dark_red"`, `"lowland"`, `"red_yellow"`, `"stagnic"`,
  `"brown_forest"`, `"regosol"`.

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
p <- make_cscj2011_example("dark_red")
p$horizons[, c("designation", "top_cm", "bottom_cm")]
#>    designation top_cm bottom_cm
#>         <char>  <num>     <num>
#> 1:           A      0        12
#> 2:         Bw1     12        40
#> 3:         Bw2     40        70
#> 4:           C     70        95
#> 5:           R     95       120
```
