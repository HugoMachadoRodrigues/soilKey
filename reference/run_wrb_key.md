# Run the WRB 2022 key over a pedon

Iterates over the RSGs in canonical key order; the first RSG whose tests
pass is assigned. RSGs whose tests return NA (stubbed diagnostics or
insufficient data) are skipped and recorded in the trace.

## Usage

``` r
run_wrb_key(pedon, rules = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- rules:

  Optional pre-loaded rule set; if NULL, reads
  `inst/rules/wrb2022/key.yaml`.

## Value

A list with `assigned` (the YAML entry for the assigned RSG) and `trace`
(one entry per RSG tested, in order).
