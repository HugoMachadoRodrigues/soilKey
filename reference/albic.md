# Albic horizon (WRB 2022)

A bleached eluvial horizon – claric material that has lost iron oxides
and/or organic matter due to clay migration, podzolization, or redox
under stagnant water. Diagnostic for parts of Podzols, Retisols and
Planosols qualifiers.

## Usage

``` r
albic(pedon, min_thickness = 1)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 1, per WRB 2022). The albic horizon
  has no canonical thickness gate; we keep a token min so that fully-NA
  layers don't pass.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests:

- [`test_claric_munsell`](https://hugomachadorodrigues.github.io/soilKey/reference/test_claric_munsell.md)
  – Munsell criteria of claric material (Ch 3.3.4).

Designation pattern `E` or `Eg` also serves as positive evidence when
Munsell columns are missing (proxy path).

## References

IUSS Working Group WRB (2022), Ch 3.1 – Albic horizon.
