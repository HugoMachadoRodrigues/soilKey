# Example pedons for the Japanese great groups

Small, plausible profiles written for the tests and examples, one per
great group of the Soil Classification System of Japan (2017), with
designations in the Japanese notation and the properties the key reads.
They are not real profiles.

## Usage

``` r
make_scsj2017_example(
  great_group = c("man_made", "organic", "andosol", "podzol", "fluvic", "red_yellow",
    "stagnic", "eutrosol", "brown_forest", "regosol")
)
```

## Arguments

- great_group:

  One of `"man_made"`, `"organic"`, `"andosol"`, `"podzol"`, `"fluvic"`,
  `"red_yellow"`, `"stagnic"`, `"eutrosol"`, `"brown_forest"`,
  `"regosol"`.

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
p <- make_scsj2017_example("podzol")
p$horizons[, c("designation", "top_cm", "bottom_cm")]
#>    designation top_cm bottom_cm
#>         <char>  <num>     <num>
#> 1:           O     -6         0
#> 2:           E      0        10
#> 3:         Bhs     10        18
#> 4:          Bs     18        45
#> 5:           C     45        90
```
