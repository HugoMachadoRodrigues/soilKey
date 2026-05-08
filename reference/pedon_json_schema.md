# JSON Schema for a soilKey PedonRecord

Returns a Draft-2020-12 JSON Schema describing the canonical
`PedonRecord` structure: a `site` object with site-level metadata plus a
`horizons` array where each element matches the canonical horizon schema
documented by
[`horizon_column_spec`](https://hugomachadorodrigues.github.io/soilKey/reference/horizon_column_spec.md).

## Usage

``` r
pedon_json_schema(as = c("list", "json"), pretty = TRUE)
```

## Arguments

- as:

  One of `"list"` (default; returns a structured R list ready to
  serialise) or `"json"` (returns a JSON string; requires the `jsonlite`
  package).

- pretty:

  Logical, only used for `as = "json"`.

## Value

A list (default) or a JSON string.

## Examples

``` r
if (FALSE) { # \dontrun{
schema <- pedon_json_schema()
names(schema)
#> [1] "$schema"     "$id"         "title"       "type"        "required"   "properties"

# Validate a JSON profile against the schema:
if (requireNamespace("jsonvalidate", quietly = TRUE)) {
  schema_json <- pedon_json_schema(as = "json")
  jsonvalidate::json_validate('{"site":{...},"horizons":[...]}',
                                 schema_json, engine = "ajv")
}
} # }
```
