# Oxisol (USDA Cap 13): oxic horizon, excluding profiles with an argillic horizon overlying the oxic.

v0.9.17 fix: KST 13ed Ch 13 (p 295) excludes from Oxisols any profile
whose argillic horizon's upper boundary lies within 100 cm of the
surface AND whose argillic base lies within 30 cm of the upper boundary
of the oxic. Operationally we use the simpler and more defensible
"argillic above oxic" check: if argillic exists and starts strictly
shallower than the oxic, the profile is NOT an Oxisol (route to Ultisols
/ Alfisols instead). The previous v0.8 implementation lacked this
exclusion and was responsible for misclassifying 144 Embrapa FEBR
Ultisols as Oxisols in the v0.9.16 benchmark.

## Usage

``` r
oxisol_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
