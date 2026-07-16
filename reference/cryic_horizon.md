# Cryic horizon (WRB 2022, Chapter 3.1.8)

A perennially frozen horizon in mineral or organic material
(permafrost). The permafrost logic is shared with
[`cryic_conditions`](https://hugomachadorodrigues.github.io/soilKey/reference/cryic_conditions.md);
this exposes it under the canonical WRB 2022 horizon name so that
`coverage_report("wrb_horizons")` reports it, and returns a
`DiagnosticResult` named `"cryic"`.

## Usage

``` r
cryic_horizon(pedon, ...)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- ...:

  Passed to
  [`cryic_conditions`](https://hugomachadorodrigues.github.io/soilKey/reference/cryic_conditions.md)
  (e.g. `max_top_cm`, `max_temp_C`).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 3.1.8, Cryic horizon.
