# Export a classification result + pedon to a QGIS GeoPackage

Writes a single GeoPackage (`.gpkg`) that QGIS reads natively,
containing one POINT layer (the profile location with all classification
metadata as attributes) plus two attribute-only tables (the horizons
schema and the provenance log). Lets a pedologist overlay the soilKey
result on a soil-survey base map or join it with field-campaign vector
data without writing R or SQL.

## Usage

``` r
report_to_qgis(
  pedon,
  classifications,
  file,
  report_html = NULL,
  overwrite = TRUE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- classifications:

  A list of one to three
  [`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
  objects, named `wrb` / `sibcs` / `usda`. Pass the output of
  [`classify_from_documents`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_from_documents.md)
  verbatim, or build the list manually.

- file:

  Output path (`.gpkg`). Created with parents.

- report_html:

  Optional path to a sibling HTML report (rendered via
  [`report_html`](https://hugomachadorodrigues.github.io/soilKey/reference/report_html.md))
  – stored in the `report_html` attribute of `pedon_point` so QGIS users
  can launch the report from the feature pop-up.

- overwrite:

  If `TRUE` (default), an existing `file` is replaced; otherwise an
  error is thrown.

## Value

The output `file` path, invisibly. Side-effect: writes a multi-layer
GeoPackage.

## Geometry handling

The point geometry uses the pedon's site CRS (`pedon$site$crs`, default
EPSG:4326). When the site has no coordinates, the function still writes
the two attribute tables but skips the point layer and emits a warning.

## Layer schema

- `pedon_point`:

  site_id, country, year, lat, lon, crs, wrb_name, wrb_rsg, wrb_grade,
  wrb_principal, wrb_supplementary, sibcs_name, sibcs_ordem,
  sibcs_grade, usda_name, usda_order, usda_grade, n_horizons,
  report_html (relative path), generated_at.

- `horizons_table`:

  site_id, horizon_idx, top_cm, bottom_cm, designation, plus the
  canonical
  [`horizon_column_spec()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizon_column_spec.md)
  attributes when present.

- `provenance_log`:

  site_id, horizon_idx, attribute, source, confidence, notes.

## See also

[`report`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md)
for HTML / PDF reports;
[`classify_from_documents`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_from_documents.md)
for the high-level one-liner that produces compatible `classifications`.

## Examples

``` r
if (FALSE) { # \dontrun{
pedon <- make_ferralsol_canonical()
results <- list(
  wrb   = classify_wrb2022(pedon, on_missing = "silent"),
  sibcs = classify_sibcs(pedon, include_familia = TRUE),
  usda  = classify_usda(pedon)
)
report_to_qgis(pedon, results,
               file        = "perfil_042.gpkg",
               report_html = "perfil_042.html")
# In QGIS: Layer -> Add Layer -> Add Vector Layer -> perfil_042.gpkg
} # }
```
