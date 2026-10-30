# Pisoplinthic horizon (WRB 2022)

WRB 2022 Ch 3.1.28 (p. 60): \\= 40% of the volume (whole soil)
yellowish, reddish or blackish concretions or nodules over 2 mm, at
least moderately cemented, not part of a petroplinthic horizon, \\= 15
cm thick. Since v0.9.312 the concretions are read from
`ferric_nodules_pct` (Fe/Mn nodules or concretions; the ISIS reader
stores the hard ones over 2 mm), adjoining sublayers counting together;
a layer where they are not recorded is undecided. Until v0.9.311 the
test was plinthite \\= 15% with a "pisolitic" structure, which is not
the WRB 2022 definition.

## Usage

``` r
pisoplinthic(
  pedon,
  min_thickness = 15,
  min_plinthite_pct = 15,
  min_nodules_pct = 40
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

- min_plinthite_pct:

  Not used since v0.9.312; kept so that existing calls still work.

- min_nodules_pct:

  Minimum volume % of concretions and nodules (default 40).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
