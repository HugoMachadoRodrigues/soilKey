# Collect distinct missing soil attributes from the trace

Filters out the `diagnostic_X` markers that record stubbed RSG tests;
the user can already see those in the trace and the classification name.

## Usage

``` r
collect_missing_attributes(trace)
```
