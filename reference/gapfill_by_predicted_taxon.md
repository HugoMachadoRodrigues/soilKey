# Fill missing horizon attributes from the predicted taxon's mean profile

Classifies `pedon` with NO fill to get a provisional taxon, then fills
its missing cells from `taxon_profiles[[<that taxon>]]` (built by
[`build_taxon_profiles`](https://hugomachadorodrigues.github.io/soilKey/reference/build_taxon_profiles.md)).
Non-circular: the fill is keyed on the model's own prediction, not the
reference. Each fill is written with `source = "inferred_prior"` (grade
C). Reachable via
`gapfill = list(method = "taxon", taxon_profiles = <...>)`.

## Usage

``` r
gapfill_by_predicted_taxon(
  pedon,
  taxon_profiles,
  system = c("sibcs", "wrb2022", "usda"),
  attrs = NULL,
  confidence = 0.55
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- taxon_profiles:

  Output of
  [`build_taxon_profiles`](https://hugomachadorodrigues.github.io/soilKey/reference/build_taxon_profiles.md).

- system:

  One of `"sibcs"` (default), `"wrb2022"`, `"usda"`.

- attrs:

  Attributes to fill (default: those present in the matched profile).

- confidence:

  Provenance confidence (default 0.55, below a coordinate prior).

## Value

Invisibly, the mutated `pedon`; attribute `"gapfill_by_predicted_taxon"`
records the taxon + cells filled.

## See also

[`build_taxon_profiles`](https://hugomachadorodrigues.github.io/soilKey/reference/build_taxon_profiles.md),
[`apply_soilgrids_depth_prior`](https://hugomachadorodrigues.github.io/soilKey/reference/apply_soilgrids_depth_prior.md)
