# Validate a JSON string against a packaged schema

Thin wrapper around
[`jsonvalidate::json_validate`](https://docs.ropensci.org/jsonvalidate/reference/json_validate.html)
that resolves a schema by short name (`"horizon"`, `"site"`) and returns
a normalized result list with `valid` (logical) and `errors` (character
vector, possibly empty).

## Usage

``` r
validate_against_schema(json_string, schema_name, engine = "ajv")
```

## Arguments

- json_string:

  A character scalar holding the JSON document to validate (e.g. the raw
  string returned by a VLM call).

- schema_name:

  Short schema name as accepted by
  [`load_schema`](https://hugomachadorodrigues.github.io/soilKey/reference/load_schema.md).

- engine:

  Validation engine to use; passed through to
  [`jsonvalidate::json_validate`](https://docs.ropensci.org/jsonvalidate/reference/json_validate.html).
  Default `"ajv"` supports draft-07.

## Value

A list with elements:

- `valid`: `TRUE` / `FALSE`.

- `errors`: character vector of validation error messages (empty if
  `valid`).
