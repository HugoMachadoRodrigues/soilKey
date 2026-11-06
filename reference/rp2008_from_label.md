# Read French soil labels as references of the Referentiel pedologique 2008

Reads labels of French datasets (a reference, a type with its
qualifiers, a double attachment written with a hyphen) as the references
of the RP 2008, for benchmarks. A label of the RP of 1995 whose
reference was removed in 2008 (p. VII: the four old brunisols, the
pseudo-luvisols, the reductisols dupliques ...) is read through
`rp2008_crosswalk("legacy_rp1995")`; a reference of 1995 that keeps its
name in 2008 is read as the 2008 one. Case, accents, the singular or
plural and the qualifiers after the name do not matter: the longest name
the label starts with decides.

## Usage

``` r
rp2008_from_label(labels)
```

## Arguments

- labels:

  Character vector of labels.

## Value

A data frame, one row per label: `label`; `references` (the codes,
comma-separated: several when the label leaves the choice open, any of
them); `ger` (the group of references); `source` (`"rp2008"` a reference
of 2008, `"rp1995"` a label of 1995 removed in 2008, `"group"` a group
named alone, `NA` not read); `double_with` (the codes of the second part
of a double attachment, as in `luvisol typique-redoxisol`); `note`.

## See also

[`rp2008_crosswalk`](https://hugomachadorodrigues.github.io/soilKey/reference/rp2008_crosswalk.md).

## Examples

``` r
rp2008_from_label(c("Brunisol eutrique resature, limoneux",
                    "BRUNISOL SATURE", "luvisol typique-redoxisol",
                    "Sol brun lessive"))
#>                                  label          references       ger source
#> 1 Brunisol eutrique resature, limoneux brunisols_eutriques brunisols rp2008
#> 2                      BRUNISOL SATURE brunisols_eutriques brunisols rp1995
#> 3            luvisol typique-redoxisol   luvisols_typiques  luvisols rp2008
#> 4                     Sol brun lessive                <NA>      <NA>   <NA>
#>   double_with                        note
#> 1        <NA>                        <NA>
#> 2        <NA> brunisols eutriques saturés
#> 3  redoxisols                        <NA>
#> 4        <NA>                        <NA>
```
