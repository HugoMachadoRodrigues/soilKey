# Compute the provenance-aware evidence grade

v0.1 rule: A if every recorded provenance is `"measured"`, B if any
`"predicted_spectra"`, C if any `"inferred_prior"`, D if any
`"extracted_vlm"` or `"user_assumed"`. If no provenance is recorded,
defaults to A (assume measured).

## Usage

``` r
compute_evidence_grade(pedon, trace)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
