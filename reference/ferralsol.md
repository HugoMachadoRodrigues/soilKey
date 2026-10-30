# Ferralsol RSG gate (WRB 2022 Ch 4, p 110)

WRB-canonical: ferralic horizon \\= 150 cm AND no argic horizon starting
above (or at the upper limit of) the ferralic, UNLESS the argic in its
upper 30 cm or throughout has one or more of:

- \< 10% water-dispersible clay; OR

- DeltapH (pH_KCl - pH_water) \\= 0; OR

- \\= 1.4% soil organic carbon.

v0.3.4 enforces all three exception paths. The DeltapH check uses
`ph_kcl` and `ph_h2o`; the WDC check uses `water_dispersible_clay_pct`
(introduced in v0.3.3 schema), a clay content in % of the fine earth.

## Usage

``` r
ferralsol(pedon, strict = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- strict:

  Logical or `NULL`. When `NULL` (default) it resolves via
  `getOption("soilKey.rsg_strict", FALSE)`. `TRUE` requires two of the
  three argic exception paths.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Details

v0.9.310: the ferralic horizon must start \<= 150 cm from the mineral
soil surface; an argic horizon counts when it starts above or at the
upper limit of the ferralic horizon (not only when it lies wholly above
it); and an exception must hold throughout the upper 30 cm of the argic
horizon (or all of it when it is thinner). Data that do not decide an
exception leave the gate undecided (NA).

## Tier-3 strict mode (v0.9.98)

When an argic horizon sits above the ferralic, the default gate keeps
the profile as a Ferralsol if *any one* of the three exception paths
(WDC \\ 10%, DeltapH \\= 0, SOC \\= 1.4%) holds. With `strict = TRUE`
the gate requires *at least two* of the three – a single weak indicator
no longer rescues a profile with a translocated-clay argic from being
keyed out of Ferralsols.
