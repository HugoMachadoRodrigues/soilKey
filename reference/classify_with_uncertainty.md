# Posterior distribution over classification outcomes

Runs `n` Monte-Carlo perturbations of a pedon and tallies the resulting
classes into an empirical posterior. Unlike
[`classification_robustness`](https://hugomachadorodrigues.github.io/soilKey/reference/classification_robustness.md),
the perturbation magnitude of every `(horizon, attribute)` cell is
scaled by its provenance evidence grade (see
[`get_perturbation_scale`](https://hugomachadorodrigues.github.io/soilKey/reference/get_perturbation_scale.md)):
an A-grade measurement is nudged by a few percent, an E-grade assumption
by a third of its value. The posterior therefore reflects not just how
close the profile sits to a key boundary, but how trustworthy the inputs
that placed it there actually are.

## Usage

``` r
classify_with_uncertainty(
  pedon,
  n = 200L,
  system = c("wrb2022", "sibcs", "usda"),
  level = c("rsg", "name"),
  scales = NULL,
  sensitivity = TRUE,
  seed = 42L
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- n:

  Number of Monte-Carlo draws (default 200).

- system:

  One of `"wrb2022"`, `"sibcs"`, `"usda"`.

- level:

  `"rsg"` (default; compare the RSG / order) or `"name"` (compare the
  full classification name, qualifiers included – strictly more
  uncertain).

- scales:

  Optional named list overriding the default per-grade magnitudes; each
  element has the shape returned by
  [`get_perturbation_scale`](https://hugomachadorodrigues.github.io/soilKey/reference/get_perturbation_scale.md),
  keyed by grade letter.

- sensitivity:

  If `TRUE` (default) also computes a leave-one-attribute-out
  sensitivity ranking. Set `FALSE` to skip that extra pass when only the
  posterior is needed.

- seed:

  Random seed for reproducibility.

## Value

A list of class `"soilkey_uncertainty"` with elements: `posterior`
(named numeric vector summing to 1, sorted descending), `top1` (the
modal class), `entropy` (Shannon entropy of the posterior, natural log),
`sensitivity` (a `data.table` of `attribute` / `importance`, or `NULL`),
`n_runs`, `n_success`, `baseline`, `system` and `level`.

## See also

[`classification_robustness`](https://hugomachadorodrigues.github.io/soilKey/reference/classification_robustness.md),
[`get_perturbation_scale`](https://hugomachadorodrigues.github.io/soilKey/reference/get_perturbation_scale.md),
[`compute_per_attribute_evidence_grade`](https://hugomachadorodrigues.github.io/soilKey/reference/compute_per_attribute_evidence_grade.md).

## Examples

``` r
# \donttest{
p <- make_ferralsol_canonical()
u <- classify_with_uncertainty(p, n = 50, system = "wrb2022")
u$posterior   # P(RSG = x)
#> Ferralsols 
#>          1 
u$entropy     # near 0 for a robust profile
#> [1] 0
# }
```
