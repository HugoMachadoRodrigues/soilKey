# Resolve and run a single named diagnostic test

Looks up `names(test_spec)[1]` as a function exported by soilKey, calls
it with the pedon and the parameters listed in `test_spec[[1]]`, and
normalises the return value.

## Usage

``` r
run_single_test(pedon, test_spec)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- test_spec:

  A one-entry named list parsed from YAML, e.g.
  `list(ferralic = list(min_thickness = 30))`.

## Value

A list with `test_name`, `passed`, `layers`, `missing`, plus optional
`evidence`, `reference`, `notes`.
