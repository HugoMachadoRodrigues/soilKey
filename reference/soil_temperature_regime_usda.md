# Soil temperature regime helper (USDA, KST 13ed Ch 3, pp 53-58)

Returns TRUE when `pedon$site$soil_temperature_regime` matches `target`.
Temperature regimes:

- "gelic": MAST \< 0 C (and permafrost present)

- "cryic": MAST 0-8 C, summer \< 15 C

- "frigid": MAST \< 8 C, summer \>= 15 C

- "mesic": MAST 8-15 C

- "thermic": MAST 15-22 C

- "hyperthermic": MAST \>= 22 C

- Plus iso- variants (low summer-winter difference)

## Usage

``` r
soil_temperature_regime_usda(
  pedon,
  target = c("gelic", "cryic", "frigid", "mesic", "thermic", "hyperthermic", "isofrigid",
    "isomesic", "isothermic", "isohyperthermic")
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- target:

  Character, one of the recognized regimes.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
