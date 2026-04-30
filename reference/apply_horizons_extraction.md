# Apply a parsed horizons-extraction result to a pedon

Walks the `horizons` array of a parsed extraction response, creating /
matching horizon rows and recording each non-null attribute via
`pedon$add_measurement(... source = "extracted_vlm")`.

## Usage

``` r
apply_horizons_extraction(
  pedon,
  parsed,
  overwrite = FALSE,
  source_label = "extracted_vlm"
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

Returns the count of provenance entries added.
