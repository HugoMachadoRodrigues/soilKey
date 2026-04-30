# Path to a packaged JSON schema file

Resolves `name` to an absolute file path under the package's
`inst/schemas/` directory (which becomes `schemas/` in the installed
package). The `.json` extension is added if missing.

## Usage

``` r
schema_path(name)
```

## Arguments

- name:

  Schema base name, e.g. `"horizon"` or `"site"`. Either with or without
  the `.json` suffix.

## Value

Absolute file path. Errors if the schema is not found.
