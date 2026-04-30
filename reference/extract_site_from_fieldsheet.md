# Extract site metadata from a field-sheet image

Sends a photographed / scanned field sheet to a multimodal VLM and
merges the extracted site-level metadata (lat, lon, elevation, parent
material, land use, etc.) into `pedon$site`. Existing fields are
preserved unless `overwrite = TRUE`; only NULL fields are filled.

## Usage

``` r
extract_site_from_fieldsheet(
  pedon,
  image_path,
  provider,
  max_retries = 3L,
  overwrite = FALSE,
  prompt_name = "extract_site_metadata",
  schema_name = "site"
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- image_path:

  Path to the field-sheet image.

- provider:

  A chat provider from
  [`vlm_provider`](https://hugomachadorodrigues.github.io/soilKey/reference/vlm_provider.md)
  (or a
  [`MockVLMProvider`](https://hugomachadorodrigues.github.io/soilKey/reference/MockVLMProvider.md)
  for testing).

- max_retries:

  Integer; how many times to re-prompt on validation failure. Default 3.

- overwrite:

  If `TRUE`, lower-authority values are allowed to clobber
  higher-authority ones. Default `FALSE`.

- prompt_name:

  Override the default prompt template (`"extract_horizons"`).

- schema_name:

  Override the default schema (`"horizon"`).

## Value

Invisibly, the mutated `pedon`.
