# Load a packaged JSON schema as a string

Reads `inst/schemas/<name>.json` and returns its contents as a single
character scalar. The JSON is not parsed – callers either pass the
string straight to
[`validate_against_schema`](https://hugomachadorodrigues.github.io/soilKey/reference/validate_against_schema.md)
or substitute it into a prompt template via
[`load_prompt`](https://hugomachadorodrigues.github.io/soilKey/reference/load_prompt.md).

## Usage

``` r
load_schema(name)
```

## Arguments

- name:

  Schema base name, e.g. `"horizon"`, `"site"`.

## Value

Character scalar containing the schema JSON.
