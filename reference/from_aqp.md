# Convert an aqp SoilProfileCollection back to a list of PedonRecord

Inverse of
[`as_aqp`](https://hugomachadorodrigues.github.io/soilKey/reference/as_aqp.md).
Walks each profile in the SPC, renames aqp's canonical horizon column
names back to soilKey's (`top` -\> `top_cm`, `name` -\> `designation`,
`clay` -\> `clay_pct`, ...), assembles a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
per profile, and returns the list.

## Usage

``` r
from_aqp(spc)
```

## Arguments

- spc:

  A
  [`aqp::SoilProfileCollection`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-class.html).

## Value

A list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects (length = `length(spc)`).

## Details

Round-trip property: `from_aqp(as_aqp(pedon))` reproduces `pedon` modulo
column ordering.

## See also

[`as_aqp`](https://hugomachadorodrigues.github.io/soilKey/reference/as_aqp.md),
the forward conversion.

## Examples

``` r
if (FALSE) { # \dontrun{
pedons <- list(make_ferralsol_canonical(), make_luvisol_canonical())
spc <- as_aqp(pedons)
pedons2 <- from_aqp(spc)
identical(pedons[[1]]$horizons$clay_pct, pedons2[[1]]$horizons$clay_pct)
#> [1] TRUE
} # }
```
