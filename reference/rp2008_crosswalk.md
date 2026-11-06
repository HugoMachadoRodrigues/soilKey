# Correspondence tables of the Referentiel pedologique 2008

The correspondence tables of the Referentiel pedologique 2008 (AFES,
2009) as data frames: codes and names as the book prints them, with the
printed page, its doubts ("?") kept as a flag and its slips noted. They
are notes for display and benchmarks:
[`classify_rp2008`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_rp2008.md)
and
[`classify_cpcs1967`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_cpcs1967.md)
never read them.

## Usage

``` r
rp2008_crosswalk(
  table = c("wrb2006", "wrb2006_horizons", "wrb2006_qualifiers", "wrb2006_faux_amis",
    "cpcs_horizons", "cpcs_classes", "cpcs_new_in_rp", "legacy_rp1995")
)
```

## Arguments

- table:

  Which table (see Details).

## Value

A data frame, one row per pair of the table.

## Details

- `"wrb2006"`:

  Annex 5 (pp. 379-390): one row per reference of the 110 (two for the
  brunisols eutriques, which the annex splits by their saturation, and
  one more for the lithosols stricts), with its WRB 2006 name. The WRB
  2006 is the edition the book cites, not soilKey's WRB 2022.

- `"wrb2006_horizons"`:

  Annex 5: the reference horizons the tables put beside a WRB 2006
  diagnostic horizon (pp. 380, 383, 384).

- `"wrb2006_qualifiers"`:

  Annex 5: the RP qualifiers and their WRB 2006 qualifiers, "most often
  imperfect" (pp. 379, 380, 385, 391-392); `scope` is the group the book
  restricts a row to.

- `"wrb2006_faux_amis"`:

  Annex 5: the WRB 2006 qualifiers spelled like an RP qualifier but
  meaning something else (p. 392).

- `"cpcs_horizons"`:

  Annex 6 (p. 393): one row per CPCS 1967 horizon symbol and RP code;
  `intertropical` for the codes of the intertropical domain, `slip` for
  the codes that are not RP codes, `after_1967` for the symbols not in
  the CPCS 1967.

- `"cpcs_classes"`:

  Annex 7 (pp. 394-398): one row per CPCS category (keyed by its
  `cpcs1967` code) and RP entry: the entry's name, the reference codes
  or the group (GER) it names, its qualifiers, `double` for a double
  attachment, `none` where the book prints "???".

- `"cpcs_new_in_rp"`:

  Annex 7: the RP categories the book calls completely new with respect
  to the CPCS (p. 398).

- `"legacy_rp1995"`:

  The references of the RP of 1995 removed in 2008 (p. VII) and the
  names of 2008 the book gives for such solums; `reading` where the
  destination is soilKey's reading. Used by
  [`rp2008_from_label`](https://hugomachadorodrigues.github.io/soilKey/reference/rp2008_from_label.md).

## See also

[`rp2008_from_label`](https://hugomachadorodrigues.github.io/soilKey/reference/rp2008_from_label.md),
[`classify_rp2008`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_rp2008.md).

## Examples

``` r
head(rp2008_crosswalk("wrb2006"))
#>                       code        ger                       rp
#> 1      alocrisols_typiques alocrisols      alocrisols typiques
#> 2      alocrisols_humiques alocrisols      alocrisols humiques
#> 3             vitrandosols   andosols             vitrandosols
#> 4    silandosols_eutriques   andosols    silandosols eutriques
#> 5   silandosols_dystriques   andosols   silandosols dystriques
#> 6 silandosols_perhydriques   andosols silandosols perhydriques
#>                           wrb2006 doubtful note page
#> 1        Cambisols (Hyperdystric)    FALSE <NA>  379
#> 2 Cambic Umbrisols (Hyperdystric)    FALSE <NA>  379
#> 3                 Vitric Andosols    FALSE <NA>  379
#> 4      Silandic Andosols (Eutric)    FALSE <NA>  379
#> 5     Silandic Andosols (Dystric)    FALSE <NA>  379
#> 6        Hydric Silandic Andosols    FALSE <NA>  379
rp2008_crosswalk("cpcs_horizons")[1:6, ]
#>   cpcs rp intertropical  slip after_1967 note page
#> 1  A00 OL         FALSE FALSE      FALSE <NA>  393
#> 2  A0F OF         FALSE FALSE      FALSE <NA>  393
#> 3  A0F Hf         FALSE FALSE      FALSE <NA>  393
#> 4  A0F Hm         FALSE FALSE      FALSE <NA>  393
#> 5  A0H OH         FALSE FALSE      FALSE <NA>  393
#> 6  A0H Hs         FALSE FALSE      FALSE <NA>  393
subset(rp2008_crosswalk("cpcs_classes"), cpcs == "V/111")
#>     cpcs cpcs_level                cpcs_name item            rp_name references
#> 43 V/111  subgroups Rendzines très humifères    1 rendosols humiques  rendosols
#>     ger qualifiers double any_reference scope doubtful  none  open page
#> 43 <NA>   humiques  FALSE         FALSE  <NA>    FALSE FALSE FALSE  395
```
