# USDA family: mineralogy class (KST Ch. 17)

Priority key from the available chemistry: carbonatic (CaCO3 \>= 40
percent) -\> oxidic (Kr \< 0.75) -\> micaceous (sand mica dominant) -\>
smectitic (high CEC/clay activity) -\> kaolinitic (Ki \<= 2.2) -\>
siliceous (quartzose sand) -\> mixed (default).

## Usage

``` r
family_mineralogy_usda(pedon, min_cm = 25, max_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_cm, max_cm:

  Control-section depth window (cm).

## Value

A
[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## References

Soil Survey Staff (2022), KST 13th ed., Ch. 17.
