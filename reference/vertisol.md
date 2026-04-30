# Vertisol RSG gate (WRB 2022 Ch 4, p 101)

WRB-canonical: vertic horizon \\= 100 cm AND \\= 30% clay between the
surface and the vertic horizon throughout AND shrink-swell cracks that
start at the surface (or below a plough layer / below a self- mulching
surface / below a surface crust) and extend to the vertic horizon.

## Usage

``` r
vertisol(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

v0.3.4 enforces (1) vertic horizon, (2) all overlying layers \\= 30%
clay, and (3) shrink-swell cracks that start within the upper 20 cm.
"Cracks extending to the vertic horizon" is enforced indirectly by the
test_shrink_swell_cracks test that already requires an explicit
`cracks_width_cm` value.
