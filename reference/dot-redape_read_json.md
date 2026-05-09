# Read a single Redape GeoTab JSON file

The Redape JSON format wraps profiles in `{"items": [{...}]}`. Some
files in the published dataset have a stray trailing brace that breaks
strict JSON parsers; this helper tolerates it.

## Usage

``` r
.redape_read_json(path)
```

## Arguments

- path:

  Path to a JSON file.

## Value

List of items (typically length 1).
