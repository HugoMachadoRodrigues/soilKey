# Fractic qualifier (fc): fractures (cracks) within 100 cm

WRB 2022 Ch 5 (Durisols / Gypsisols / Calcisols): "Showing fractures
within 100 cm of the soil surface" (a duripan, gypsic, or calcic horizon
that has cracked / fractured).

## Usage

``` r
qual_fractic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

Implementation: positive `cracks_width_cm` or `cracks_depth_cm` on any
layer with top \<= 100 cm.
