# Petroplinthic horizon (WRB 2022)

Continuous or fractured layer cemented by Fe (WRB 2022 Ch 3.1.27, p.
59). Since v0.9.312 a layer qualifies when its Fe cement is recorded (an
iron pan, `pan_type` ferricrete or ironpan, or plinthite \\=
`min_plinthite_pct`) and its cementation class is at least strongly
cemented (criterion 4; until v0.9.311 moderately was enough), over
`min_thickness` cm of adjoining layers (criterion 7). A layer whose Fe
cement or cementation is not recorded is undecided.

## Usage

``` r
petroplinthic(pedon, min_thickness = 10, min_plinthite_pct = 15)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

- min_plinthite_pct:

  Numeric threshold or option (see Details).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
