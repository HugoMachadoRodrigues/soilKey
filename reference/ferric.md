# Ferric horizon (WRB 2022)

v0.9.309: the criteria of WRB 2022 Ch 3.1.11 (p. 45). The horizon
consists of mineral material, is \>= 15 cm thick and is not part of a
petroplinthic, pisoplinthic or plinthic horizon, and has, in one or more
subhorizons, (a) \>= 15% of its area as coarse (\> 20 mm) masses inside
aggregates that are black or have a hue redder than 7.5YR and a chroma
\>= 5, moist, or (b) \>= 5% concretions or nodules, at least weakly
cemented, reddish or blackish, \> 2 mm (`ferric_nodules_pct`). Until
v0.9.308 soilKey read it from Fe-dithionite \>= 5%, which is not a
criterion of the text: a red Ferralsol with 6% Fe-dithionite and no
nodules became Ferric (ISRIC's KE009). soilKey records no size for
mottles, so criterion (a) is met only by nodules; mottles of the right
colours leave it undecided, and a layer with no mottles and no nodules
recorded as such does not meet it.

## Usage

``` r
ferric(pedon, min_thickness = 15, min_nodules_pct = 5)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness (cm; default 15).

- min_nodules_pct:

  Minimum nodules, % (default 5).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 3.1.11, Ferric horizon.
