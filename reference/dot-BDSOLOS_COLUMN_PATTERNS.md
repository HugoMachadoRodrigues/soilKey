# Canonical mapping from BDsolos column-name variants to soilKey schema

BDsolos exports use Portuguese column names with variable casing and
diacritic handling. This table records the regex patterns that identify
each soilKey horizon column. Patterns are matched case-insensitively,
after stripping diacritics and the underscore between word fragments.

## Usage

``` r
.BDSOLOS_COLUMN_PATTERNS
```

## Format

An object of class `list` of length 41.
