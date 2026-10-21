# Example pedons for the Canadian orders

Small, plausible profiles written for the tests and examples, one per
order of the Canadian System of Soil Classification, with designations
in the Canadian notation and the properties the order key reads. They
are not records of real pedons.

## Usage

``` r
make_cssc3_example(
  order = c("cryosolic", "organic", "vertisolic", "podzolic", "gleysolic", "solonetzic",
    "chernozemic", "luvisolic", "brunisolic", "regosolic")
)
```

## Arguments

- order:

  One of `"cryosolic"`, `"organic"`, `"vertisolic"`, `"podzolic"`,
  `"gleysolic"`, `"solonetzic"`, `"chernozemic"`, `"luvisolic"`,
  `"brunisolic"`, `"regosolic"`.

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
p <- make_cssc3_example("chernozemic")
p$horizons[, c("designation", "top_cm", "bottom_cm", "oc_pct")]
#>    designation top_cm bottom_cm oc_pct
#>         <char>  <num>     <num>  <num>
#> 1:          Ah      0        18    3.0
#> 2:          Bm     18        40    1.0
#> 3:         Cca     40        90    0.4
```
