# Comprehensive classification metrics from a pooled confusion matrix

All metrics are computed closed-form from `cm` (a `table` with
`reference` rows / `predicted` columns, as produced by
`.merge_confusion`). The matrix is first padded to the union of its row
and column labels, so a class that is only ever predicted (or only ever
referenced) is handled, and every division is guarded so an absent class
never yields `NaN`. `balanced_accuracy` and `macro_f1` average only over
classes that have reference support (a zero-support class neither
inflates nor deflates them). `nir` is the no-information rate (the
accuracy of always predicting the majority *reference* class) – the
baseline an accuracy figure must beat to be meaningful.

## Usage

``` r
.benchmark_metrics_from_confusion(cm)
```
