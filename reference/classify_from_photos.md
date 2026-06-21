# Classify a soil profile from field photographs alone

A no-lab-data pipeline: profile photographs are sent to a
vision-language model for Munsell-colour and (optionally) site-metadata
extraction; the missing horizon attributes are back-filled from a
SoilGrids depth prior; and the WRB 2022, SiBCS 5 and USDA Soil Taxonomy
keys are run on the assembled
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Usage

``` r
classify_from_photos(
  images,
  lat = NULL,
  lon = NULL,
  country = NULL,
  provider = NULL,
  systems = c("wrb", "sibcs", "usda"),
  soilgrids = TRUE,
  depth_profiles = NULL,
  on_missing = "silent"
)
```

## Arguments

- images:

  Either a character vector of profile-photo paths, or a named list with
  elements `profile` (character vector, required) and `fieldsheet`
  (character vector, optional).

- lat, lon:

  Optional decimal-degree coordinates. When supplied they seed
  `pedon$site` and are used for the SoilGrids fetch; a field sheet can
  also supply them through extraction.

- country:

  Optional ISO-2 country code; passed through to the constructed pedon's
  site metadata.

- provider:

  A vision-language provider: an ellmer chat object for live use, or a
  `MockVLMProvider` for testing and offline demos. Required – there is
  no default, so a real classification is never produced from canned
  data by accident.

- systems:

  Character vector, any subset of `c("wrb", "sibcs", "usda")`.

- soilgrids:

  If `TRUE` (default) missing horizon attributes are back-filled from a
  SoilGrids depth prior via
  [`apply_soilgrids_depth_prior`](https://hugomachadorodrigues.github.io/soilKey/reference/apply_soilgrids_depth_prior.md).

- depth_profiles:

  Optional named list of six-slice SoilGrids depth profiles, forwarded
  to
  [`apply_soilgrids_depth_prior`](https://hugomachadorodrigues.github.io/soilKey/reference/apply_soilgrids_depth_prior.md).
  Supplying it skips the network call.

- on_missing:

  Forwarded to the classifiers; default `"silent"`.

## Value

A named list with one
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
per requested system (`$wrb`, `$sibcs`, `$usda`), the constructed
`$pedon`, its `$provenance` ledger, and a one-row `$summary` data frame.
If extraction yields no horizons the list instead carries `$error` and a
`NULL` pedon.

## Details

Because every value originates from a photograph or a spatial prior, the
classification's evidence grade is low by construction (`D` for
VLM-extracted attributes, `C` where a SoilGrids prior contributed). The
result is a screening estimate, not a substitute for a described and
sampled profile.

## See also

[`extract_munsell_from_photo`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_munsell_from_photo.md),
[`apply_soilgrids_depth_prior`](https://hugomachadorodrigues.github.io/soilKey/reference/apply_soilgrids_depth_prior.md),
[`compute_per_attribute_evidence_grade`](https://hugomachadorodrigues.github.io/soilKey/reference/compute_per_attribute_evidence_grade.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Live use with an ellmer chat:
res <- classify_from_photos(
  images   = list(profile = "profile.jpg", fieldsheet = "sheet.jpg"),
  lat = -22.7, lon = -43.6, country = "BR",
  provider = ellmer::chat_anthropic())
res$wrb$name
res$wrb$evidence_grade   # "D" or "C"
} # }
```
