# Greyzemic qualifier (gz): mollic / umbric overlain by albic-like layer

WRB 2022 Ch 5 (Chernozems / Phaeozems / Umbrisols): "Having a mollic /
umbric horizon overlain by a thin (\<= 10 cm) albic-like layer with low
chroma and high value (Munsell value \>= 4 moist AND chroma \<= 2)."

## Usage

``` r
qual_greyzemic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

Implementation: presence of mollic OR umbric (we have
[`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md)
but not yet `umbric`) AND an overlying bleached layer
(`munsell_value_moist >= 4` and `munsell_chroma_moist <= 2`, thickness
\<= 10 cm).
