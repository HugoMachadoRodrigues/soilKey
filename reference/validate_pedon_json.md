# Validate a PedonRecord against the JSON schema

Convenience wrapper that converts a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
(or a compatible list) to JSON and validates it via
[`jsonvalidate::json_validate`](https://docs.ropensci.org/jsonvalidate/reference/json_validate.html)
against the canonical schema returned by
[`pedon_json_schema`](https://hugomachadorodrigues.github.io/soilKey/reference/pedon_json_schema.md).

## Usage

``` r
validate_pedon_json(x)
```

## Arguments

- x:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  or a list with the same shape.

## Value

A logical scalar (`TRUE` when valid). Validation errors appear as the
`errors` attribute when `FALSE`.

## Details

Use this BEFORE calling `classify_*` when ingesting data from external
systems (web APIs, ETL pipelines, multimodal extraction) to catch schema
violations early.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- make_ferralsol_canonical()
validate_pedon_json(p)
#> [1] TRUE
} # }
```
