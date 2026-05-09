# Load the bundled KSSL/NCSS lab-data sample (v0.9.74)

Returns a 100-profile snapshot from the NCSS Lab Data Mart (KSSL gpkg,
`head = 100`) pre-annotated with derived WRB Reference Soil Group via
[`usda_to_wrb_rsg`](https://hugomachadorodrigues.github.io/soilKey/reference/usda_to_wrb_rsg.md).

## Usage

``` r
load_kssl_sample()
```

## Value

A list with `pedons`, `pulled_on`, `source`, `cross_walk`.

## Details

This is the bundled offline counterpart to
[`load_kssl_pedons_gpkg`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_pedons_gpkg.md)
– use this for tests and demos when the 5.5 GB gpkg is not available
locally.

Each pedon has BOTH:

- `site$reference_usda` (Order, Suborder, Greatgroup, Subgroup) – the
  canonical KSSL classification.

- `site$reference_wrb_from_usda` – the derived WRB RSG via the IUSS WRB
  2022 Annex 6 cross-walk.

First-ever KSSL WRB benchmark (soilKey v0.9.74, full v0.9.69-72 fallback
stack):

- Top-1 accuracy: 20.1\\

- Calcisol 69\\

- Phaeozem / Kastanozem / Solonetz 0\\ data not in KSSL lab tables (in
  companion NASIS).

## Reference

Beaudette, D., Skovlin, J., Roecker, S., Brown, A. (2024). aqp:
Algorithms for Quantitative Pedology. R package version 2.x.
<https://github.com/ncss-tech/aqp>.

## Examples

``` r
if (FALSE) { # \dontrun{
s <- load_kssl_sample()
length(s$pedons)
#> 100
table(vapply(s$pedons, function(p) p$site$reference_wrb_from_usda,
             character(1)))
} # }
```
