# Load KSSL / NCSS pedons from the ncss_labdata GeoPackage

Reads the \`lab_combine_nasis_ncss\` / \`lab_site\` / \`lab_layer\` /
\`lab_chemical_properties\` / \`lab_physical_properties\` views from the
NCSS Lab Data Mart GeoPackage and assembles a list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects. Each pedon has its USDA Soil Taxonomy Order attached as
`site$reference_usda`, normalised to match \`classify_usda()\` output
("Mollisols", "Alfisols", ...).

## Usage

``` r
load_kssl_pedons_gpkg(
  gpkg,
  head = NULL,
  require_b_horizon = TRUE,
  verbose = TRUE
)
```

## Arguments

- gpkg:

  Path to `ncss_labdata.gpkg`.

- head:

  Optional integer; load only the first N classified pedons. Useful for
  parser validation.

- require_b_horizon:

  If `TRUE` (default), drops pedons whose deepest horizon's bottom_cm
  \< 30. Most non-Entisol Order gates need a B horizon.

- verbose:

  If `TRUE` (default), emits progress messages.

## Value

A list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects.
