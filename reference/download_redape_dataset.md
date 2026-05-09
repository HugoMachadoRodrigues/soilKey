# Download the curated Redape GeoTab dataset (Vaz et al 2023)

Enumerates the dataset via the Dataverse API and downloads all JSON
profile files (the structured / interoperable format used by the
curators) into `dest_dir`. Skips files already present unless
`overwrite = TRUE`.

## Usage

``` r
download_redape_dataset(
  dest_dir,
  dataset_doi = .REDAPE_GEOTAB_DOI,
  include_rtf = FALSE,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- dest_dir:

  Destination directory for the JSON files.

- dataset_doi:

  DOI of the dataset (default: the Vaz 2023 dataset).

- include_rtf:

  If `TRUE`, also download the original RTF profile sheets (default
  `FALSE`; the JSON files alone are enough for classification).

- overwrite:

  If `TRUE`, re-download files that already exist locally.

- verbose:

  Print progress (default `TRUE`).

## Value

Character vector of paths to the downloaded files.

## References

Vaz, G. J., Silva Jr, A. F., & Silva Neto, L. de F. da (2023). Brazilian
soil data for taxonomic classification. Redape, V1.
<https://doi.org/10.48432/PYKKA7>.
