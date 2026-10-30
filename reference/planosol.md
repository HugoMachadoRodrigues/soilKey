# Planosol RSG gate (WRB 2022 Ch 4, p 107)

WRB 2022: an abrupt textural difference \\= 75 cm from the mineral soil
surface and, within 5 cm directly above or below it, (1) stagnic
properties in which the area of reductimorphic plus oximorphic features
is \\= 50% and (2) reducing conditions in some parts of the volume with
the reductimorphic features.

## Usage

``` r
planosol(pedon, strict = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- strict:

  Logical or `NULL`; recorded in the evidence. Since v0.9.312 it changes
  nothing (the fallback it disabled is gone).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Details

The two layers that meet at the abrupt textural difference stand for the
5 cm above and below it. Stagnic properties are those of
[`stagnic_properties`](https://hugomachadorodrigues.github.io/soilKey/reference/stagnic_properties.md);
a claric layer directly above the abrupt textural difference is stagnic
(Ch 3.2.14, criterion 4) and its whole area counts as reductimorphic.
Since v0.9.312 the abrupt textural difference has its transition
criterion (`boundary_distinctness`), the depth limit of 75 cm is
applied, and the v0.2 `planic_features` fallback (clay doubling at an
abrupt boundary, with no redox evidence), which is not in WRB 2022, is
gone.
