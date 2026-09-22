# Plinthic horizon (WRB 2022)

Tests whether any horizon meets the plinthic horizon criteria. Plinthite
is Fe-rich material that hardens irreversibly on repeated wetting and
drying; the plinthic horizon is the diagnostic of Plinthosols.

## Usage

``` r
plinthic(pedon, min_thickness = 15, min_plinthite_pct = 15)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 15).

- min_plinthite_pct:

  Minimum volume % plinthite (default 15).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests:

- `test_plinthite_concentration` – plinthite volume % \>= 15

- `test_minimum_thickness` – thickness \>= 15 cm

v0.2 limitations: WRB 2022 also accepts profiles with \>= 40% red
Fe-rich mottles as alternative criterion – not yet wired. The
"irreversibly hardens" criterion is conceptual and requires field
observation; v0.2 takes `plinthite_pct` as already representing true
plinthite (as opposed to soft mottles).

## v0.9.72 designation morphological inference (opt-in)

Field-described Brazilian Plintossolos profiles (e.g.\\ the Embrapa
Redape curated dataset) routinely encode plinthite via the designation
suffix `f` in the master letter sequence (e.g.\\ `Btf`, `2Btf`, `Cf`) –
the curator's direct assertion that plinthite is present – without
recording `plinthite_pct` as a numeric volume percent.

With `options(soilKey.plinthic_designation_inference = TRUE)` the
function accepts a layer as plinthic when:

1.  the canonical `plinthite_pct` test is `NA` for that layer, AND

2.  the designation matches `[A-Z]+[A-Za-z]*f[0-9]?` (a `f`
    master-letter modifier in any sub-position).

Default is `FALSE` (canonical behaviour preserved).

## References

IUSS Working Group WRB (2022), Chapter 3, Plinthic horizon.
