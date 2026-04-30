# Three-valued ALL across a logical vector, NA-aware

Returns FALSE if any element is exactly FALSE; TRUE if every element is
exactly TRUE; NA if no FALSE but at least one NA. Used inside SiBCS
pendente diagnostics that combine per-layer tests with proper
propagation.

## Usage

``` r
.three_valued_all(x)
```
