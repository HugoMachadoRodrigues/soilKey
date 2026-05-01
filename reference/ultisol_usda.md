# Ultisols (USDA Cap 15): argillic/kandic horizon + base saturation \< 35%.

v0.9.17 graceful BS handling: when `bs_pct` is missing in the argillic
layers, the diagnostic falls back to two equivalent indirect criteria
before failing:

- `al_sat_pct >= 50` (high Al saturation mathematically forces BS \< 50,
  and BS \< 35 in essentially all tropical soils with this profile);

- `ph_h2o < 5.0` (the empirical threshold below which BS exceeds 35 in
  fewer than 5

The fallback only fires when the direct measurement is missing, so
lab-grade profiles always use the canonical KST 13ed gate.

## Usage

``` r
ultisol_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
