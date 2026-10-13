# Horizonte B textural (SiBCS Cap 2, p 54-57)

The B textural as the book states it: a subsurface horizon of texture
francoarenosa or finer, at least 7.5 cm thick (thickness items a to e),
that meets one or more of the items below (v0.9.241).

- \(f\) an E horizon above the B, the B having more clay than the
  horizons above it and being no B espodico, plintico or planico;

- \(g\) a mudanca textural abrupta from the A or E to the B (Cap 1, p
  30-31): at least twice the clay when the A or E has less than 200
  g/kg, 200 g/kg more otherwise, within 7.5 cm. The distance is read
  from the distinctness of the transition (`boundary_distinctness`:
  abrupt or clear); where it is not recorded the two layers are
  compared;

- \(h\) the relacao textural B/A of footnote 4, above 1.50, 1.70 or 1.80
  according to the clay of the A (`test_ratio_textural_sibcs`);

- \(i\) where the increase is below item (h), the cerosidade of one or
  more sub-horizons of the upper part of the B (read as its upper
  half): (1) texture media with no macroaggregates and cerosidade of
  moderate grade; (2) texture media with prismatic or blocky structure
  of moderate or strong grade and cerosidade of at least moderate
  grade; (3) texture argilosa or muito argilosa with that structure and
  cerosidade at least common and weak or few and moderate; (4) relacao
  textural of 1.4 or more with a fragipa within 200 cm, the B being no B
  espodico;

- \(j\) with a discontinuity of parent material between the A or E and
  the B, or with only a ploughed layer above the B, items (h) or (i) are
  needed.

Amounts and grades written as ranges are read by their lower end, the
requirements being minima. Lamellae (item d) are not tested.

## Usage

``` r
B_textural(pedon, ...)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- ...:

  Passed to
  [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
  for a profile with no horizon designation; ignored otherwise.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Details

Until v0.9.240 this function was the WRB
[`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
horizon, whose clay increase (6 percentage points, a ratio of 1.4 or 20
points, between a layer and the least clayey layer anywhere above it) is
not the one of the book, joined to item (h); items (f), (g) and (i) were
not tested.

The layers returned are the B sub-horizons (BC excluded) of texture
francoarenosa or finer; `evidence$upper_layers` are those that carry the
evidence, from the top of the B down.

The items are stated for the A, E and B horizons. A profile with no
horizon designation at all (depth slices, for instance) cannot be read
by them and is tested as before, by the clay increase between layers.

In the morphological mode
(`options(soilKey.morphological_inference = TRUE)`) a "t" in the
designation of a B horizon stands for the B textural when the items do
not show it. The diagnostic engine
(`options(soilKey.diagnostic_engine = "aqp")`) does not change this
test: it changed the argic horizon that this function used to be.
