# Hydric qualifier (hy): water content at 1500 kPa \>= 100% (undried fine earth, WRB 2022). v0.9.1 accepts the air-dried equivalent (\>= 70%) when the lab protocol pre-dries; the result is flagged as "potentially over-permissive" via the `notes` field when the value falls in the 70-100% band.

Hydric qualifier (hy): water content at 1500 kPa \>= 100% (undried fine
earth, WRB 2022). v0.9.1 accepts the air-dried equivalent (\>= 70%) when
the lab protocol pre-dries; the result is flagged as "potentially
over-permissive" via the `notes` field when the value falls in the
70-100% band.

## Usage

``` r
qual_hydric(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
