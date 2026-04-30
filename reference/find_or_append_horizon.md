# Find or create a horizon row matching the given (top, bottom)

For PDF-extracted horizons we may be merging into a pedon that already
has the horizons table from another source, or starting from scratch.
Strategy:

## Usage

``` r
find_or_append_horizon(pedon, top_cm, bottom_cm)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

1.  If the pedon already has horizons, find an existing row whose
    `(top_cm, bottom_cm)` match within 1 cm.

2.  If none matches, append a new row with the canonical schema and
    return the new index.
