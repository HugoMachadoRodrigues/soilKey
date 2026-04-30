# Tsitelic horizon (WRB 2022 Ch 3.1)

From Georgian *tsiteli* = red. A red colour-defined horizon formed on
weathered basalt or similar Fe-rich parent material in Caucasian /
Mediterranean settings. Used by the Cambisols key (Ch 4 p 123, criterion
4) and by the Tsitelic qualifier.

## Usage

``` r
tsitelic(pedon, min_thickness = 10)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

## Details

Diagnostic criteria (v0.3.5 simplification):

- Munsell hue \\= 2.5YR (i.e. 2.5YR, 10R, 7.5R, 5R, 2.5R) AND value \\=
  4 (moist) AND chroma \\= 4 (moist);

- evidence of soil formation (cambic-style criterion 3) proxied by clay
  \\= 8% AND `structure_grade` not "single grain" / "massive";

- thickness \\= 10 cm.
