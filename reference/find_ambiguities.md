# Collect ambiguous RSG candidates from the trace

v0.1 rule: an entry is ambiguous if its result is NA and at least one
attribute (not just a stubbed diagnostic) was reported missing.

## Usage

``` r
find_ambiguities(trace, current, diags = NULL)
```
