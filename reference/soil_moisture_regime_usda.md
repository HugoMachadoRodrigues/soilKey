# Soil moisture regime helper (USDA, KST 13ed Ch 3, pp 50-52)

Returns TRUE when `pedon$site$soil_moisture_regime` matches `target`.
Climatic data is required; in v0.8.x the regime is read directly from
site metadata (a v0.9 helper will derive it from monthly
precipitation+ETP).

## Usage

``` r
soil_moisture_regime_usda(
  pedon,
  target = c("aquic", "aridic", "torric", "udic", "perudic", "ustic", "xeric")
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

## Details

Recognized targets (KST 13ed Ch 3): "aquic", "aridic", "torric", "udic",
"perudic", "ustic", "xeric".
