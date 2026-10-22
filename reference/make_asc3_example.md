# Example pedons for the Australian orders

Small, plausible profiles written for the tests and examples, one per
order of the Australian Soil Classification, with designations in the
Australian notation (Field Handbook), field textures and the properties
the order key reads. They are not real profiles.

## Usage

``` r
make_asc3_example(
  order = c("anthroposols", "organosols", "podosols", "vertosols", "hydrosols",
    "kurosols", "sodosols", "chromosols", "calcarosols", "ferrosols", "dermosols",
    "kandosols", "arenosols", "rudosols", "tenosols")
)
```

## Arguments

- order:

  One of the 15 orders, in lower case and plural: `"anthroposols"`,
  `"organosols"`, `"podosols"`, `"vertosols"`, `"hydrosols"`,
  `"kurosols"`, `"sodosols"`, `"chromosols"`, `"calcarosols"`,
  `"ferrosols"`, `"dermosols"`, `"kandosols"`, `"arenosols"`,
  `"rudosols"`, `"tenosols"`.

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
p <- make_asc3_example("kandosols")
p$horizons[, c("designation", "top_cm", "bottom_cm", "field_texture")]
#>    designation top_cm bottom_cm field_texture
#>         <char>  <num>     <num>        <char>
#> 1:          A1      0        15            SL
#> 2:          A3     15        30            SL
#> 3:         B21     30        70           SCL
#> 4:         B22     70       130           SCL
```
