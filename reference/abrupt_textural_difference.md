# Abrupt textural difference (WRB 2022 Ch 3.2.1)

Sharp clay-content increase between two superimposed mineral layers
meeting all of:

- underlying clay \\= 15% AND thickness \\= 7.5 cm;

- underlying starts \\= 10 cm below mineral soil surface;

- underlying has, vs overlying: 2x clay if overlying \< 20%, OR \\= 20pp
  (absolute) more clay if overlying \\= 20%;

- transitional layer, if any, \\= 2 cm.

v0.3.3 enforces criteria 1, 2, 3. The transitional-layer check is
deferred (the canonical horizon schema does not carry a "transitional"
marker; it can be added later via boundary_distinctness inspection).

## Usage

``` r
abrupt_textural_difference(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
