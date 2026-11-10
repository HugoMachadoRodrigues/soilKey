# Example pedons for the FAO-Unesco legend

Small synthetic profiles written for the tests and examples, one per
major soil grouping and one per soil unit of the Revised Legend (FAO
1988), built from the key's numbers (no profile of the book is copied).
A grouping's example keys to its own grouping without an undecided gate
before it; a unit's example (built on its grouping's) keys to its own
unit without an undecided grouping or unit before it. With
`near_miss = TRUE`, the same profile with one printed number of its gate
crossed (a Luvisol with a base saturation of 45 % in its lower B, a
Histosol with 35 cm of peat, a Lithic Leptosol with its rock at 11 cm),
which keys elsewhere; a unit whose gate has no printed number has no
near miss. They are not real profiles.

## Usage

``` r
make_fao_example(code = "HS", edition = c("1988", "1974"), near_miss = FALSE)
```

## Arguments

- code:

  The code of a major soil grouping of 1988 ("HS", "AT", "LP", "VR",
  "FL", "SC", "GL", "AN", "AR", "RG", "PZ", "PT", "FR", "PL", "SN",
  "GR", "CH", "KS", "PH", "PD", "GY", "CL", "NT", "AL", "AC", "LV",
  "LX", "CM"), or of one of its 153 soil units ("HSi", "LVx", "CMe"
  ...); with `edition = "1974"`, of a major soil group of 1974 ("O",
  "I", "V", "J", "Z", "G", "T", "Q", "R", "U", "E", "P", "F", "W", "S",
  "M", "C", "K", "H", "D", "X", "Y", "N", "A", "L", "B") or of one of
  its 106 soil units ("Ox", "Lc", "Be" ...).

- edition:

  `"1988"` (default) or `"1974"` (since v0.9.389).

- near_miss:

  `TRUE` for the near miss of the grouping.

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## See also

[`classify_fao`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_fao.md).

## Examples

``` r
p <- make_fao_example("PH")
p$horizons[, c("designation", "top_cm", "bottom_cm", "bs_pct")]
#>    designation top_cm bottom_cm bs_pct
#>         <char>  <num>     <num>  <num>
#> 1:         Ah1      0        20     70
#> 2:         Ah2     20        40     65
#> 3:          Bw     40        90     60
#> 4:           C     90       140     60
classify_fao(make_fao_example("LVx"), on_missing = "silent")$name
#> [1] "Chromic Luvisols"
classify_fao(make_fao_example("Fx", edition = "1974"), edition = "1974",
             on_missing = "silent")$name
#> [1] "Xanthic Ferralsols"
```
