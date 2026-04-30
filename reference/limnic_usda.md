# Limnic Subgroup helper (Histels)

Pass when one or more limnic layers (coprogenous earth / diatomaceous
earth / marl) with cumulative thickness \>= 5 cm occur within the
control section (KST 13ed, p 190).

## Usage

``` r
limnic_usda(pedon, min_thickness_cm = 5, max_top_cm = 130)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness_cm:

  Default 5.

- max_top_cm:

  Default 130 cm (control section).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementation: detects designation containing 'L' (KST notation for
limnic) OR `layer_origin == "lacustrine"`.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 3, p 45; Ch. 9 Hemistels /
Sapristels.
