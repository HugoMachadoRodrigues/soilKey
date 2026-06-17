# Fill horizon attributes derivable BY DEFINITION from the same horizon

Recovers cells that are exact closures of other measured columns in the
same horizon (not statistical estimates): the texture third
(clay/silt/sand) when the other two are present and sum to \\ 100;
effective CEC as `sum(bases) + al`; aluminium saturation as
`100 * al / ecec`; and base saturation as `100 * sum(bases) / cec`.
Every fill is written with `source = "inferred_prior"` so the
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
authority order keeps it from displacing a measured value and the
evidence grade drops to `"C"`. Companion to
[`gapfill_within_pedon`](https://hugomachadorodrigues.github.io/soilKey/reference/gapfill_within_pedon.md)
(depth interpolation) and
[`apply_soilgrids_depth_prior`](https://hugomachadorodrigues.github.io/soilKey/reference/apply_soilgrids_depth_prior.md)
(external prior); reachable via the `gapfill = list(method = "derive")`
argument of the classifiers.

## Usage

``` r
gapfill_derive_horizon(pedon, overwrite = FALSE)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- overwrite:

  If `FALSE` (default) only `NA` target cells are filled.

## Value

Invisibly, the mutated `pedon`; attribute `"gapfill_derive_horizon"`
records the count filled.

## See also

[`gapfill_within_pedon`](https://hugomachadorodrigues.github.io/soilKey/reference/gapfill_within_pedon.md),
[`apply_soilgrids_depth_prior`](https://hugomachadorodrigues.github.io/soilKey/reference/apply_soilgrids_depth_prior.md)
