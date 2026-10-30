# Plinthic horizon (WRB 2022)

Tests whether any horizon meets the plinthic horizon criteria (WRB 2022
Ch 3.1.30, p. 62), the diagnostic of Plinthosols with the pisoplinthic
and petroplinthic horizons. The depth limit belongs to the Plinthosols
key (`plinthosol()`), not to the horizon.

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

Criteria read (v0.9.312):

- 1: `plinthite_pct` \\= 15 (the oximorphic features inside former
  aggregates of criterion 1, as `plinthite_pct` records them); a layer
  where it is not recorded is undecided;

- 2: Fe-dithionite \\= 2.5% (2.a) when `fe_dcb_pct` is measured; below
  it the layer is undecided, since 2.b (10% Fe in the oximorphic
  features) and 2.c (hardening after drying and wetting) are not
  recorded; not measured, the recorded plinthite is taken for 2.c;

- 3: Feox/Fedith \< 0.1 when both are measured;

- 4: not part of a petroplinthic or pisoplinthic horizon;

- 5: \\= 15 cm thick; adjoining sublayers (Btv1 over Btv2) count
  together.

The result is `NA` when undecided layers could make the horizon.

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

IUSS Working Group WRB (2022), Chapter 3.1.30, Plinthic horizon.
