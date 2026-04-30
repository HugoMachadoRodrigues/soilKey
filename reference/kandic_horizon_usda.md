# Kandic horizon (USDA, KST 13ed Ch 3, p 45)

Subsurface horizon with low-activity clays (CEC \<= 16 cmol/kg clay,
ECEC \<= 12) and clay increase. Implementation: delegates to argic with
additional CEC/clay check.

## Usage

``` r
kandic_horizon_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
