# Chromic Subgroup helper (Vertisols, KST 13ed Ch 16)

The Vertisol *Chromic* subgroups are the “not dark” ones: per KST 13ed,
those that have, in one or more horizons within `max_top_cm` of the
mineral soil surface, 50 percent or more of the colours with a moist
value of 4 or more, a dry value of 6 or more, or (when `use_chroma`) a
moist chroma of 3 or more. This is a value/chroma test, **not** the
red-hue `chromic` qualifier of WRB. The Aquerts great groups
(Dur-/Dystr-/Endo-/Epiaquerts) drop the chroma clause – gleyed material
is low-chroma by definition – so they are wired with
`use_chroma = FALSE`.

## Usage

``` r
chromic_subgroup_usda(pedon, use_chroma = TRUE, max_top_cm = 30)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- use_chroma:

  Logical; include the moist-chroma \>= 3 clause (default `TRUE`). Set
  `FALSE` for Aquerts.

- max_top_cm:

  Upper-depth window in cm (default 30).
