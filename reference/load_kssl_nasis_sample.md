# Load the bundled KSSL + NASIS morphological-enriched sample (v0.9.75)

Returns a 99-profile snapshot built by joining the NCSS Lab Data Mart
(`ncss_labdata.gpkg`) with the companion NASIS Morphological sqlite
(`NASIS_Morphological_*.sqlite`) via
[`load_kssl_pedons_with_nasis`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_pedons_with_nasis.md).
Pre-annotated with derived WRB Reference Soil Group via
[`usda_to_wrb_rsg`](https://hugomachadorodrigues.github.io/soilKey/reference/usda_to_wrb_rsg.md).

## Usage

``` r
load_kssl_nasis_sample()
```

## Value

A list with `pedons`, `pulled_on`, `source`, `join_helper`,
`cross_walk`.

## Details

Compared to
[`load_kssl_sample`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_sample.md)
(KSSL lab tables only), this sample carries the morphological evidence
that several WRB diagnostic horizons need:

\| Field \| KSSL-only \| KSSL + NASIS \| \|——-\|———-:\|————-:\| \|
munsell_hue_moist \| 0 \| munsell_value_moist \| 0 \|
munsell_chroma_moist \| 0 \| munsell_hue_dry \| 0 \| structure_grade \|
0 \| structure_type \| 0 \| clay_films_amount \| 0 \| slickensides \| 0

First-ever benchmark on this enriched sample (soilKey v0.9.75, full
v0.9.69-72 fallback stack):

- Top-1 baseline: 19.1\\ \*\*+3.5pp lift purely from NASIS
  morphology\*\*)

- Top-1 full stack: 20.6\\

- Phaeozem: 1/33 -\> 2/33 (Munsell-driven mollic detection)

- Podzol: 0/15 -\> 1/15

Remaining ceiling driven by attributes neither dataset preserves:
Solonetz needs Na/ESP, Vertisols need slickensides + cracks (NASIS
records 1.7 on subsoil samples NASIS often lacks.

## Reference

Beaudette, D., Skovlin, J., Roecker, S., Brown, A. (2024). aqp:
Algorithms for Quantitative Pedology. R package version 2.x.
`https://github.com/ncss-tech/aqp`.

## Examples

``` r
if (FALSE) { # \dontrun{
s <- load_kssl_nasis_sample()
length(s$pedons)
#> 99
# Munsell now populated (KSSL-only sample had 0%):
mean(vapply(s$pedons,
            function(p) any(!is.na(p$horizons$munsell_hue_moist)),
            logical(1)))
#> 0.99
} # }
```
