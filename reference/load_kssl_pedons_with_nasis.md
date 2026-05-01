# Load KSSL pedons enriched with NASIS morphology

Joins the NCSS Lab Data Mart GeoPackage with the NASIS Morphological
SQLite to produce PedonRecord objects whose horizons table has BOTH lab
chemistry + physics AND field morphology (Munsell, structure, clay
films, slickensides, cracks). Required for the morphological-evidence
diagnostics
([`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
clay-films,
[`vertic_horizon`](https://hugomachadorodrigues.github.io/soilKey/reference/vertic_horizon.md)
slickensides,
[`mollic_epipedon_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic_epipedon_usda.md)
Munsell, etc.) to fire on KSSL profiles – the lab gpkg alone has none of
those.

## Usage

``` r
load_kssl_pedons_with_nasis(
  gpkg,
  sqlite,
  head = NULL,
  require_b_horizon = TRUE,
  verbose = TRUE
)
```

## Arguments

- gpkg:

  Path to `ncss_labdata.gpkg`.

- sqlite:

  Path to `NASIS_Morphological_*.sqlite`.

- head:

  Optional integer; load only the first N classified pedons. Useful for
  parser validation / scaling.

- require_b_horizon:

  If `TRUE` (default), drops pedons whose deepest horizon's bottom_cm \<
  30.

- verbose:

  If `TRUE` (default), emits progress messages.

## Value

A list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects.
