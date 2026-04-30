# Ferralsol RSG gate (WRB 2022 Ch 4, p 110)

WRB-canonical: ferralic horizon \\= 150 cm AND no argic horizon starting
above (or at the upper limit of) the ferralic, UNLESS the argic in its
upper 30 cm or throughout has one or more of:

- \< 10% water-dispersible clay; OR

- DeltapH (pH_KCl - pH_water) \\= 0; OR

- \\= 1.4% soil organic carbon.

v0.3.4 enforces all three exception paths. The DeltapH check uses
`ph_kcl` and `ph_h2o`; the WDC check uses `water_dispersible_clay_pct`
(introduced in v0.3.3 schema).

## Usage

``` r
ferralsol(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
