# Batch robustness across many pedons

Runs
[`classification_robustness`](https://hugomachadorodrigues.github.io/soilKey/reference/classification_robustness.md)
on each pedon in a list and returns a tidy data.frame with one row per
pedon. Useful for paper-grade claims like "85 to a 5

## Usage

``` r
batch_robustness(pedons, ...)
```

## Arguments

- pedons:

  List of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  objects.

- ...:

  Passed to
  [`classification_robustness`](https://hugomachadorodrigues.github.io/soilKey/reference/classification_robustness.md).

## Value

A data.frame with columns `id`, `baseline`, `robustness`, `n_flipped`.

## Examples

``` r
if (FALSE) { # \dontrun{
pedons <- list(make_ferralsol_canonical(),
                 make_luvisol_canonical(),
                 make_chernozem_canonical())
batch_robustness(pedons, system = "wrb2022", n = 50)
#>            id   baseline robustness n_flipped
#> 1 FR-canon-01 Ferralsols       0.96         2
#> 2 LV-canon-01   Luvisols       1.00         0
#> 3 CH-canon-01 Chernozems       0.94         3
} # }
```
