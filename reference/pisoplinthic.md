# Pisoplinthic horizon (WRB 2022): pisolitic plinthic. v0.3.3 detects via designation pattern `Bspl` / `Bvpi` or via plinthite \\= 15% AND structure_type containing 'pisol'.

Pisoplinthic horizon (WRB 2022): pisolitic plinthic. v0.3.3 detects via
designation pattern `Bspl` / `Bvpi` or via plinthite \\= 15% AND
structure_type containing 'pisol'.

## Usage

``` r
pisoplinthic(pedon, min_thickness = 15, min_plinthite_pct = 15)
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
