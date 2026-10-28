# Example pedons for the New Zealand orders

Small, plausible profiles written for the tests and examples, one per
order of the New Zealand Soil Classification, with designations in the
New Zealand notation and the properties the order key reads. Most key to
the last (Orthic or default) group of their order. They are not real
profiles.

## Usage

``` r
make_nzsc4_example(
  order = c("organic", "gley", "ultic", "podzol", "allophanic", "pumice", "melanic",
    "semiarid", "oxidic", "granular", "pallic", "brown", "anthropic", "recent", "raw")
)
```

## Arguments

- order:

  One of `"organic"`, `"gley"`, `"ultic"`, `"podzol"`, `"allophanic"`,
  `"pumice"`, `"melanic"`, `"semiarid"`, `"oxidic"`, `"granular"`,
  `"pallic"`, `"brown"`, `"anthropic"`, `"recent"`, `"raw"`.

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
p <- make_nzsc4_example("pallic")
p$horizons[, c("designation", "top_cm", "bottom_cm")]
#>    designation top_cm bottom_cm
#>         <char>  <num>     <num>
#> 1:          Ap      0        20
#> 2:          Bw     20        45
#> 3:          Bx     45        90
#> 4:           C     90       120
```
