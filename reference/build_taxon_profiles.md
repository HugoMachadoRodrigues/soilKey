# Build per-taxon mean depth profiles for predicted-taxon gap-fill

For each taxon (the first word of the reference label at the requested
level), averages each attribute across the calibration pedons into the
six standard depth slices (0-5 ... 100-200 cm). The result feeds
[`gapfill_by_predicted_taxon`](https://hugomachadorodrigues.github.io/soilKey/reference/gapfill_by_predicted_taxon.md).
Calibrate on a set DISJOINT from the pedons you will fill (e.g. a train
split) to keep the fill non-circular.

## Usage

``` r
build_taxon_profiles(pedons, ref_field = "reference_sibcs", attrs = NULL)
```

## Arguments

- pedons:

  A list of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  with a reference label.

- ref_field:

  Site field holding the reference label (default `"reference_sibcs"`;
  e.g. `"reference_usda"` / `"reference_wrb"`).

- attrs:

  Attributes to profile (default the continuous gap-fill set).

## Value

A named list `taxon -> attr -> numeric(6)` (NA where a taxon has no
measured value in a slice).

## See also

[`gapfill_by_predicted_taxon`](https://hugomachadorodrigues.github.io/soilKey/reference/gapfill_by_predicted_taxon.md)
