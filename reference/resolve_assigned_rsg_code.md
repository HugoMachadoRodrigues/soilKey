# Resolve the assigned RSG code from a ClassificationResult

Walks the trace looking for the entry whose name matches `rsg_or_order`
and returns its `code`. Used internally by
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
to wire the prior check.

## Usage

``` r
resolve_assigned_rsg_code(result)
```
