# Canonicalise FEBR WRB/USDA reference labels to the order/RSG comparison level (no-op for SiBCS, which \`benchmark_run_classification\` canonicalises itself). \`normalise_febr_wrb\`/\`\_usda\` are idempotent on already-reduced WRB names but return NA on an already-order USDA string, so apply only to the raw reference field, only once, only for FEBR.

Canonicalise FEBR WRB/USDA reference labels to the order/RSG comparison
level (no-op for SiBCS, which \`benchmark_run_classification\`
canonicalises itself). \`normalise_febr_wrb\`/\`\_usda\` are idempotent
on already-reduced WRB names but return NA on an already-order USDA
string, so apply only to the raw reference field, only once, only for
FEBR.

## Usage

``` r
.benchmark_normalise_febr_ref(pedons, sys)
```
