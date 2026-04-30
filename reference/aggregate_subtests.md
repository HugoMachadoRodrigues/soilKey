# Aggregate sub-test results into a passed/missing summary

Used by every diagnostic-level function. `layers_passing` is the
intersection of `layers` across the listed sub-tests; `passed` is `TRUE`
if that intersection is non-empty, `NA` if no test could be evaluated
and missing attributes were reported, and `FALSE` otherwise.

## Usage

``` r
aggregate_subtests(tests, layer_tests = NULL, exclusions = character(0))
```
