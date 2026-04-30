# Cryic conditions (WRB 2022)

Tests whether continuous frozen / permafrost material occurs within the
upper 100 cm. v0.3 detects via designation pattern: any layer with
designation containing the suffix `"f"` (frozen) within the top 100 cm,
or the explicit pattern `"^Cf"` / `"perma"`. Diagnostic of Cryosols.

## Usage

``` r
cryic_conditions(pedon, max_top_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Maximum top depth (cm) (default 100).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 5, Cryosols.
