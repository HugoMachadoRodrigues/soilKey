# Build the canonical Fluvisol fixture

Synthetic floodplain Fluvisol: stratified textures across consecutive C
horizons, OC pattern non-monotone with depth (because C2 is more
recently deposited, OC-richer than C1). By construction
[`fluvic_material`](https://hugomachadorodrigues.github.io/soilKey/reference/fluvic_material.md)
passes.

## Usage

``` r
make_fluvisol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

v0.9.310: the strata are numbered as deposits of their own (C, 2C, 3C;
FAO 2006 Guidelines), the lithic discontinuities that their
particle-size distributions show (WRB 2022 Ch 3.2.7, criterion 2.a.iv),
and the first two are massive, as unaltered alluvium is. Written C1, C2,
C3, the clay-richer second stratum met the argic horizon (WRB 2022 Ch
3.1.3), and the profile keyed out as a Luvisol once the Luvisols asked
only for an argic horizon starting \<= 100 cm (Ch 4, p. 122); with the
weak subangular blocky structure they had, the C below the Ah (chroma 1
unit higher) was a cambic horizon (Ch 3.1.5, criterion 3.b.iii) and the
profile a Fluvic Cambisol.
