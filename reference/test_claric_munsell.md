# Test for "claric" Munsell colour per layer (WRB 2022 Ch 3.3.4)

Claric material is light-coloured fine earth meeting one of the WRB
Munsell criteria:

- dry: value \\= 7 with chroma \\= 3, OR value \\= 5 with chroma \\= 2;

- moist: value \\= 6 with chroma \\= 4, OR value \\= 5 with chroma \\=
  3, OR value \\= 4 with chroma \\= 2, OR (hue 5YR or redder AND value
  \\= 4 AND chroma \\= 3 AND \\= 25% of sand / coarse silt grains
  uncoated).

v0.3.3 implementation: requires moist Munsell value/chroma to satisfy
the four moist alternatives (the dry alternatives are checked when dry
Munsell is present); the uncoated-grain check is deferred (treated as
satisfied when the colour passes).

## Usage

``` r
test_claric_munsell(h, candidate_layers = NULL)
```
