# Vertic Subgroup helper (USDA, KST 13ed)

Pass when EITHER:

- Cracks within 125 cm of the mineral soil surface that are \>= 5 mm
  wide through a thickness \>= 30 cm AND slickensides or wedge-shaped
  peds in a layer \>= 15 cm thick within 125 cm; OR

- Linear extensibility (LE) \>= 6.0 cm between surface and 100 cm (or to
  a densic/lithic/paralithic contact).

## Usage

``` r
vertic_subgroup_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementation: tests cracks_width_cm \>= 0.5 AND cracks_depth_cm \>= 30
AND slickensides present, OR sum(thickness \* cole_value) \>= 6 cm.
