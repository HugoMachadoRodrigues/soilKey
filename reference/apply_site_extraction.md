# Apply a parsed site-extraction result to a pedon

Site metadata is not under provenance control (PedonRecord\$site is a
free-form list, not a column with an authority-ranked log). We therefore
set the missing fields directly and emit a provenance entry against
horizon_idx 0 (sentinel for "site-level") only when there is at least
one horizon to anchor it.

## Usage

``` r
apply_site_extraction(pedon, parsed, overwrite = FALSE)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

For attributes that already exist in `pedon$site`, we leave them alone
unless `overwrite = TRUE`. The VLM contract (extracted_vlm \< measured)
is preserved by attribute origin: a user-built site list is treated as
authoritative; an empty / NULL field can be filled by the VLM.
