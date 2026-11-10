# The correspondence between the FAO-Unesco legends of 1974 and 1988

The official correspondence of Appendix 4 of the Revised Legend (FAO
1988, "Main changes in the revised legend", pp. 113-126) between the 26
major soil groups and 106 soil units of the Legend of 1974 and the 28
major soil groupings and 153 soil units of 1988, with the three changes
the appendix does not flag added in brackets (Calcaric Gleysols to
Calcic Gleysols is a change of definition; the Plinthic Luvisols have no
1988 row; the Thionic Histosols are new). For display and for reading
labelled data only: it never classifies
([`classify_fao`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_fao.md)
keys each edition by its own text).

## Usage

``` r
fao_crosswalk(code = NULL, from = c("1974", "1988"))
```

## Arguments

- code:

  Optional character vector of codes to look up, of the edition `from`
  (a group, grouping or unit: `"Lo"`, `"L"`; `"LXh"`, `"LX"`). `NULL`
  (default) for the whole table.

- from:

  `"1974"` (default): one row per 1974 class; the 1988 codes it goes to.
  `"1988"`: the rows whose 1988 side names the code (as a target or as
  one of soilKey's alternatives), and the rows of the 1988 classes with
  no 1974 class (relation `"new"`).

## Value

A `data.frame` with `level` (`"group"` or `"unit"`), `code_1974`,
`name_1974`, `code_1988` and `name_1988` (several joined by `", "`;
empty when deleted), `relation` (`"same"`, `"renamed"`, `"amended"`,
`"renamed_amended"`, `"merged"`, `"split"`, `"deleted"`, `"new"`),
`also` and `also_when` (soilKey's alternative 1988 targets read from a
group comment of the appendix, such as the Lixisols for a low activity
Luvisol, and when they apply), `page` (printed page of 1988),
`bracketed` (`TRUE` for a row the appendix does not print) and `note`
(the comment, paraphrased).

## References

FAO (1988). FAO/Unesco Soil Map of the World, Revised Legend. World Soil
Resources Report 60, FAO, Rome; ISRIC reprint 1997, Appendix 4.

## See also

[`classify_fao`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_fao.md).

## Examples

``` r
fao_crosswalk(c("Lo", "Gc", "Vp", "Y"))[, c("code_1974", "code_1988", "relation", "also")]
#>   code_1974          code_1988        relation                       also
#> 1         Y                            deleted CL, GY, CM, LV, LX, RG, AR
#> 2        Gc                GLk renamed_amended                           
#> 3        Vp VRe, VRd, VRk, VRy           split                           
#> 4        Lo                LVh renamed_amended                        LXh
fao_crosswalk("LXh", from = "1988")[, c("code_1974", "code_1988", "relation")]
#>   code_1974 code_1988        relation
#> 1        Lo       LVh renamed_amended
#> 2        Lc       LVx            same
#> 3        Lk       LVk            same
#> 4        Lv       LVv            same
#> 5      <NA>       LXh             new
```
