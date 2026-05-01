# Has the field surveyor identified this diagnostic in NASIS?

Looks at `pedon$site$nasis_diagnostic_features` for a `featkind` value
matching `pattern` (case-insensitive regex). Returns `FALSE` when the
slot is missing entirely (e.g. lab-only loaders, non-KSSL pedons).

## Usage

``` r
.has_nasis_feature(pedon, pattern)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- pattern:

  Regex pattern matched case-insensitively against each featkind string.

## Value

Logical scalar.
