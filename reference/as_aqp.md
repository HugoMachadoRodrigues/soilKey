# Convert one or more PedonRecord objects to an aqp SoilProfileCollection

Builds a
[`aqp::SoilProfileCollection`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-class.html)
from one `PedonRecord` or a list of them. Standard soilKey columns
(`top_cm`, `bottom_cm`, `designation`, `clay_pct`, `sand_pct`,
`silt_pct`) are renamed to aqp's canonical convention (`top`, `bottom`,
`name`, `clay`, `sand`, `silt`). All other columns are passed through
unchanged. Site-level slots (`lat`, `lon`, `country`, `parent_material`,
`reference_*`, `nasis_diagnostic_features`, etc.) are attached to the
SPC's site table.

## Usage

``` r
as_aqp(x)
```

## Arguments

- x:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  or a list of them.

## Value

A
[`aqp::SoilProfileCollection`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-class.html).

## Details

Requires the `aqp` package, listed in Suggests; the function raises a
clear error if aqp is not installed.

## See also

[`from_aqp`](https://hugomachadorodrigues.github.io/soilKey/reference/from_aqp.md),
the inverse conversion.

## Examples

``` r
if (FALSE) { # \dontrun{
library(soilKey)
library(aqp)

pedons <- list(make_ferralsol_canonical(), make_luvisol_canonical())
spc <- as_aqp(pedons)
length(spc)         # 2 profiles
aqp::horizons(spc)  # one row per horizon, aqp-named columns
} # }
```
