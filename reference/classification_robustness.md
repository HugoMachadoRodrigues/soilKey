# Robustness of classification under input perturbation

For a given
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md),
perturb a chosen list of horizon attributes by a configured fractional
amount, re-classify under the requested system, and report how often the
classification `$rsg_or_order` (or full `$name`) matches the unperturbed
baseline.

## Usage

``` r
classification_robustness(
  pedon,
  system = c("wrb2022", "sibcs", "usda"),
  level = c("order", "name"),
  n = 50L,
  perturbations = NULL,
  seed = 42L
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- system:

  One of `"wrb2022"`, `"sibcs"`, `"usda"`.

- level:

  Either `"order"` (compare `$rsg_or_order`) or `"name"` (compare full
  classification name).

- n:

  Number of Monte-Carlo perturbed runs (default 50).

- perturbations:

  Named list. Each name is a horizon column; each element is a function
  taking the original value and returning a perturbed value.
  NA-tolerant.

- seed:

  Random seed for reproducibility.

## Value

A list with elements `baseline` (the unperturbed classification name),
`n` (number of MC runs), `robustness` (fraction of perturbed runs
matching baseline), `flipped_to` (table of alternative classifications
when the perturbation flipped the result).

## Details

Default perturbation panel:

- `clay_pct`: ±5

- `sand_pct`: ±5

- `silt_pct`: ±5

- `ph_h2o`: ±0.2 absolute

- `oc_pct`: ±10

## Examples

``` r
if (FALSE) { # \dontrun{
p <- make_ferralsol_canonical()
classification_robustness(p, system = "wrb2022", n = 50)
#> $baseline    : "Ferralsols"
#> $robustness  : 0.96  (48 / 50 perturbed runs landed on Ferralsols)
#> $flipped_to  : table(c("Cambisols" = 1, "Acrisols" = 1))
} # }
```
