# Horizonte B latossolico (SiBCS Cap 2, p 57-59)

The requirements of the book that the data of a profile can show
(v0.9.241):

- \(d\) texture francoarenosa or finer;

- \(g\) cation exchange capacity below 17 cmolc per kg of clay, with no
  correction for carbon;

- \(h\) cerosidade, if present, at most few and weak: a layer with a
  larger amount or a higher grade is no B latossolico (a range counts by
  its upper end, the requirement being a maximum);

- \(b\) a thickness of 50 cm or more, of the horizon: of the adjoining
  sub-horizons that meet the other requirements, taken together;

- it "nao apresenta caracteristicas diagnosticas de horizontes glei, B
  textural, B nitico e plintico": the layers of a
  [`B_textural`](https://hugomachadorodrigues.github.io/soilKey/reference/B_textural.md)
  are no B latossolico, and a plinthic or gleyic horizon or a
  [`B_nitico`](https://hugomachadorodrigues.github.io/soilKey/reference/B_nitico.md)
  in the profile excludes it, as before.

Where the designations are recorded, only B horizons are candidates: the
B latossolico is a subsurface horizon below the A. Items (a) structure,
(c) rock structure, (e) Ki and (f) weatherable minerals are not tested.

## Usage

``` r
B_latossolico(
  pedon,
  min_thickness = 50,
  max_cec_per_clay = NULL,
  engine = NULL,
  ...
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness of the horizon, in cm (50).

- max_cec_per_clay:

  CEC per kg clay the horizon must stay below. Defaults to `NULL`
  (engine-aware): 17 in soilkey engine (SiBCS Cap 2: "menor que 17 cmolc
  kg-1 de argila, sem correcao para carbono") or 20 in aqp engine
  (v0.9.68 regional tolerance for Embrapa lab methodology offset).
  Strict since v0.9.212: a value of exactly 17 is not latossolic, it is
  the B incipiente's "17 ou maior".

- engine:

  One of `"soilkey"` (default) or `"aqp"`; `NULL` reads
  `getOption("soilKey.diagnostic_engine")`.

- ...:

  Reserved for future arguments.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Details

Until v0.9.240 the 50 cm were asked of each layer on its own, so that a
B latossolico described as B1, B21 and B22 of 35 cm each was none; and
since v0.9.61 a B textural did not exclude it unless its clay films were
common or more, on a rule the comments attributed to Cap 18 of the book,
which deals with families and series.

In the morphological mode a "Bw" or "Bo" of a tropical profile stands
for the B latossolico when the data do not show it.
