# Per-attribute provenance-aware evidence grade

Resolves the evidence grade of every `(horizon, attribute)` cell that
carries a provenance entry. Where a cell has more than one entry (a
value re-sourced over the profile's lifetime) the most authoritative
source wins, mirroring
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)'s
own authority order.

## Usage

``` r
compute_per_attribute_evidence_grade(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A `data.table` with columns `horizon_idx`, `attribute` and `grade`,
sorted by horizon then attribute. A pedon with no provenance entries
yields a zero-row table.

## Details

Grades: `A` measured, `B` predicted from spectra, `C` inferred from a
spatial prior, `D` extracted by a vision-language model, `E`
user-assumed.

## See also

[`classify_from_photos`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_from_photos.md),
the global evidence grade reported on every
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md).

## Examples

``` r
p <- make_ferralsol_canonical()
compute_per_attribute_evidence_grade(p)   # all-measured -> all grade A
#> Empty data.table (0 rows and 3 cols): horizon_idx,attribute,grade
```
