# Control section for particle-size classes and their substitutes (USDA)

The key to the control section for particle-size classes and their
substitutes in mineral soils of the Keys to Soil Taxonomy (13th ed.,
2022, ch. 17, pp. 357-358), cases A to G: a root-limiting layer within
36 cm (A); Andisols (B); Alfisols, Ultisols and the Aridisols and
Mollisols with an argillic, kandic or natric horizon starting within 100
cm and ending at 25 cm or deeper, or in Arenic or Grossarenic subgroups
(C, items 1 to 4); the same orders with that horizon at 100 cm or deeper
or in Lamellic subgroups (D); an argillic or natric horizon ending above
25 cm (E); Wassents (F); and all other mineral soils, from the Ap
horizon or 25 cm to 100 cm or a root-limiting layer (G). Depths are from
the mineral soil surface (or the top of organic material with andic soil
properties, when shallower).

## Usage

``` r
family_control_section_usda(
  pedon,
  order_code = NULL,
  great_group = NULL,
  subgroup = NULL
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- order_code:

  USDA order code (e.g. `"AF"`), or NULL.

- great_group:

  Great group name (e.g. `"Hapludalfs"`), or NULL.

- subgroup:

  Subgroup name (e.g. `"Arenic Hapludalfs"`), or NULL.

## Value

A list: `top`, `bottom` (cm), `case` (A to G, with the item of case C,
e.g. `"C4"`), `evidence` and `missing`.

## Details

Root-limiting layers (p. 357) are field observations: one not recorded
is absent. When a test that decides the case is undecided, the bounds
are NA and the missing columns are listed.

## References

Soil Survey Staff (2022). *Keys to Soil Taxonomy*, 13th ed. USDA-NRCS.
Chapter 17, pp. 357-358.

## Examples

``` r
p <- make_luvisol_canonical()
family_control_section_usda(p, order_code = "AF",
                            great_group = "Hapludalfs",
                            subgroup = "Typic Hapludalfs")[c("top", "bottom", "case")]
#> $top
#> [1] 25
#> 
#> $bottom
#> [1] 75
#> 
#> $case
#> [1] "C4"
#> 
```
