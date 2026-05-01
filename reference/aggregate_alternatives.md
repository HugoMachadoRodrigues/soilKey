# Aggregate alternative-path subtests with OR semantics

Each "path" is a named list of subtests that combine with AND (intersect
their layers). Paths combine with OR: the diagnostic passes if any path
passes; passing layers are the union across passing paths; missing
attributes are unioned across all paths that did not pass and reported
NA. Used by diagnostics where WRB specifies several alternative
qualifying conditions.

## Usage

``` r
aggregate_alternatives(paths)
```

## Arguments

- paths:

  Named list of named subtest lists. Each inner list is a set of
  subtests that combine with AND.

## Value

A list with elements `passed`, `layers`, `missing`, and `passing_path`
(the name of the first path that passed, or `NA_character_`).
