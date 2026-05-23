# Compute the provenance-aware evidence grade

Returns the weakest grade present across the pedon's provenance ledger:
A if every recorded provenance is `"measured"`, B if any
`"predicted_spectra"`, C if any `"inferred_prior"`, D if any
`"extracted_vlm"`, E if any `"user_assumed"`. If no provenance is
recorded, defaults to A (assume measured).

## Usage

``` r
compute_evidence_grade(pedon, trace)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

Grade E was split out from D in v0.9.99 so that a wholly assumed value
is distinguishable from a VLM-extracted one; see
[`compute_per_attribute_evidence_grade`](https://hugomachadorodrigues.github.io/soilKey/reference/compute_per_attribute_evidence_grade.md)
for the cell-by-cell breakdown.
