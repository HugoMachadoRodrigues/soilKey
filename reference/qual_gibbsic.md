# Gibbsic qualifier (gi): high gibbsite (\>= 25%) in fine earth

WRB 2022 Ch 5 (Plinthosols / Ferralsols): "Containing layers with \>=
25% gibbsite by mass averaged over a depth of 100 cm".

## Usage

``` r
qual_gibbsic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

soilKey schema does not currently carry direct gibbsite percent. The
closest proxy is `al_ox_pct` (oxalate-extractable Al, %), but gibbsite
is poorly extracted by oxalate. The sulfuric attack `al2o3_sulfuric_pct`
captures crystalline Al-oxides (gibbsite + boehmite + diaspore +
Al-substitution in goethite). This implementation uses Al2O3 by sulfuric
attack \>= 25% as a proxy (slight over-estimate, since not all
crystalline Al is gibbsite).
