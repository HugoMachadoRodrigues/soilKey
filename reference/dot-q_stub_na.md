# Stub-NA qualifier that exists in NAMESPACE but reports missing data

For Tier-3 qualifiers requiring schema fields not yet on the
[`horizon_column_spec()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizon_column_spec.md)
or site-level lists. The audit picks the function up as "implemented",
and downstream code that calls it gets a NA-passed result with a clear
\`missing\` listing.

## Usage

``` r
.q_stub_na(name, missing_fields, reference)
```
