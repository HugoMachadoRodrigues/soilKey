# Fragic horizon (WRB 2022): a high-bulk-density horizon with restricted rooting. v0.3.3: detects via bulk_density_g_cm3 \>= 1.65 AND structure grade massive/very firm OR designation pattern `x`/`Bx`.

Fragic horizon (WRB 2022): a high-bulk-density horizon with restricted
rooting. v0.3.3: detects via bulk_density_g_cm3 \>= 1.65 AND structure
grade massive/very firm OR designation pattern `x`/`Bx`.

## Usage

``` r
fragic(pedon, min_thickness = 15, min_bd = 1.65)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

- min_bd:

  Numeric threshold or option (see Details).
