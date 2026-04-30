# Evaluate the test block of a single RSG

Given a parsed `tests` block from a YAML key entry, evaluates the
appropriate combinator and returns a list with `passed`, `evidence`,
`missing`, and (optionally) `notes`.

## Usage

``` r
evaluate_rsg_tests(pedon, tests)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- tests:

  A `tests` block from the YAML.

## Value

A list summarising the test outcome.
