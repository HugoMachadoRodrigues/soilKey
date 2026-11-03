# Example pedons for the divisions of the Russian soil classification

Small, plausible profiles written for the tests and examples, one per
division of the Russian soil classification (Field Guide of 2008), with
designations in the Russian notation (declared in
`site$designation_system`) and a few properties. They are not real
profiles.

## Usage

``` r
make_kidpr2008_example(
  division = c("AA", "AB", "BA", "BB", "BC", "DA", "CA", "CB", "CC", "CD", "CE", "CF",
    "CG", "CH", "CI", "CJ", "CK", "CL", "CM", "CN", "CO", "CP", "CQ", "CR", "CS", "CT",
    "CU")
)
```

## Arguments

- division:

  The code of a division: `"AA"`, `"AB"` (trunk A), `"BA"`, `"BB"`,
  `"BC"` (trunk B), `"DA"` (trunk D, the division soilKey codes \[DA\]),
  `"CA"` to `"CU"` (trunk C).

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Examples

``` r
classify_kidpr2008(make_kidpr2008_example("CI"), on_missing = "silent")$name
#> [1] "Chernozems [typical]"
```
