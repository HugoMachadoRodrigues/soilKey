# Abrupt textural difference (WRB 2022 Ch 3.2.1)

Sharp clay-content increase between two superimposed mineral layers
meeting all of (WRB 2022 3.2.1, p. 72):

- underlying clay \\= 15% AND thickness \\= 7.5 cm;

- underlying starts \\= 10 cm below the mineral soil surface;

- underlying has, vs overlying: 2x clay if overlying \< 20%, OR \\= 20pp
  (absolute) more clay if overlying \\= 20%;

- transitional layer, if any, \\= 2 cm.

Since v0.9.312 criterion 5 is read from the distinctness of the lower
boundary of the overlying layer (`boundary_distinctness`): abrupt (2 cm
or less) or very abrupt passes, clear, gradual or diffuse fails, and a
boundary not recorded leaves the pair undecided (`NA`). Criterion 2 is
counted from the mineral soil surface (below organic layers).

## Usage

``` r
abrupt_textural_difference(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
