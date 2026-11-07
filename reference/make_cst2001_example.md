# Example pedons for the suborders and groups of the Chinese Soil Taxonomy (2001)

Small, plausible profiles written for the tests and examples, one per
suborder of the Chinese Soil Taxonomy (2001), with FAO designations, the
regimes on the site and the data the key reads; each keys to its own
suborder, not provisionally (with the group it reaches). Since v0.9.374
a group code (`"A1.1"` ... `"N4.7"`, and soilKey's `"[A1.4]"` and
`"[A2.5]"`) gives one profile per group, 140 in all, each keying to its
own group, not provisionally. They are not real profiles. With
`near_miss = TRUE`, fourteen of them (one per order) change one property
so that the profile misses its class and lands in another: A1 without
permafrost (Orthic Histosols), B2 with a cumulic layer of 35 cm (cumulic
evidence only: Orthic Primosols), C1 with 40 g/kg carbon in the Bhs
(Orthic Spodosols), D1 frigid (Udic Andosols), E1 with a CEC of 18
cmol/kg clay (a LAC-ferric horizon: Udic Ferrosols), F1 with rock at 45
cm (Aquic Cambosols), G1 without the crust and pavement (Udic Cambosols,
with the note on the residual's name), H1 with an ESP of 22 (alkalic
evidence: Ustic Argosols), I1 without permafrost (Orthic Gleyosols), J2
with its humus near the surface (no isohumic property: Ustic Cambosols),
K1 with a CEC of 26 cmol/kg clay (Ustic Argosols), L1 gelic (Udic
Argosols), M3 aridic and not irrigated (Udic Cambosols, with the note)
and N2 with 2 g/kg carbon (Orthic Primosols).

## Usage

``` r
make_cst2001_example(suborder = .CST_EXAMPLE_CODES, near_miss = FALSE)
```

## Arguments

- suborder:

  The code of the suborder, as printed (`"A1"` ... `"N4"`), or of a
  group (`"A1.1"` ... `"N4.7"`).

- near_miss:

  Give the near miss of the suborder (one per order: `"A1"`, `"B2"`,
  `"C1"`, `"D1"`, `"E1"`, `"F1"`, `"G1"`, `"H1"`, `"I1"`, `"J2"`,
  `"K1"`, `"L1"`, `"M3"`, `"N2"`).

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## See also

[`classify_cst2001`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_cst2001.md).

## Examples

``` r
p <- make_cst2001_example("E1")
p$horizons[, c("designation", "top_cm", "bottom_cm", "clay_pct", "cec_cmol")]
#>    designation top_cm bottom_cm clay_pct cec_cmol
#>         <char>  <num>     <num>    <num>    <num>
#> 1:           A      0        15       45        9
#> 2:         Bo1     15        80       55        6
#> 3:         Bo2     80       200       55        6
classify_cst2001(make_cst2001_example("E1", near_miss = TRUE), on_missing = "silent")$name
#> [1] "Argi-Udic Ferrosols"
classify_cst2001(make_cst2001_example("K3.2"), on_missing = "silent")$levels
#>       level code                      name
#> 1    orders    K                 Ferrosols
#> 2 suborders   K3            Udic Ferrosols
#> 3    groups K3.2 Hiweatheri-Udic Ferrosols
#>                                   name_pt book_provisional
#> 1                 Ferrossolos (Ferrosols)            FALSE
#> 2                      Ferrossolos údicos            FALSE
#> 3 Ferrossolos údicos muito intemperizados            FALSE
```
