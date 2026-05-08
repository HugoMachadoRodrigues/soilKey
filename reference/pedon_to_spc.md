# Convert a soilKey PedonRecord to an aqp SoilProfileCollection

The mapping respects aqp's expected column conventions and sets the
metadata required by
[`getArgillicBounds()`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.html),
[`getCambicBounds()`](https://ncss-tech.github.io/aqp/reference/getCambicBounds.html),
and `mollicEpipedon()`:

## Usage

``` r
pedon_to_spc(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`aqp::SoilProfileCollection`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-class.html)
with one site (the pedon) and one row per horizon.

## Details

- `id` from `pedon$site$id`

- `top` / `bottom` from `top_cm` / `bottom_cm`

- `name` (designation) from `designation`

- `texcl` (texture class) derived via
  [`texture_class_from_pct`](https://hugomachadorodrigues.github.io/soilKey/reference/texture_class_from_pct.md)

- `clay`, `silt`, `sand` from `clay_pct` / `silt_pct` / `sand_pct`

- `m_hue`, `m_value`, `m_chroma`, `d_value`, `d_chroma` from
  `munsell_*_moist` and `munsell_*_dry`

Internal use; the soilKey diagnostics call this on the fly when
`engine = "aqp"`. Direct use is supported for users who want to plug
additional aqp algorithms (`slab`, `slice`, `glom`) into a soilKey
workflow.
