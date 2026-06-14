# Test cambic-horizon criterion 3 (evidence of soil formation), WRB 2022 Ch 3.1.5.

A layer shows evidence of soil formation if ANY of the following holds,
compared with an adjacent layer not separated from it by a lithic
discontinuity:

- **3.a** vs the directly underlying layer: hue \\\ge\\ 2.5 units redder
  (or yellower if the underlying hue is 5YR or redder); OR chroma
  \\\ge\\ 1 unit higher; OR clay \\\ge\\ 4% (absolute) higher;

- **3.b** vs an overlying mineral layer \\\ge\\ 5 cm thick: hue \\\ge\\
  2.5 units redder; OR value \\\ge\\ 1 unit higher; OR chroma \\\ge\\ 1
  unit higher;

- **3.c** vs the directly underlying layer: \\\ge\\ 5% (absolute) less
  calcium carbonate equivalent (carbonate removal);

- **3.d** Fe-dith \\\ge\\ 0.1%, Fe-ox/Fe-dith \\\ge\\ 0.1, and hue
  2.5YR-2.5Y with chroma \> 3.

Simplifications relative to the verbatim text (documented; no schema
support): the \\\ge\\ 90% exposed-area Munsell qualifier is treated as
met by the recorded layer colour; gypsum removal in 3.c is omitted (no
gypsum column); the lithic-discontinuity check uses the leading-integer
designation convention (e.g. `2C`); 3.b's "overlying MINERAL layer"
excludes O-designated layers. A layer with no assessable evidence (all
relevant adjacency data absent) contributes `NA`, not a pass.

## Usage

``` r
test_cambic_soil_formation(h, candidate_layers = NULL)
```

## Arguments

- h:

  Horizons table.

- candidate_layers:

  Optional integer layer indices to restrict to.
