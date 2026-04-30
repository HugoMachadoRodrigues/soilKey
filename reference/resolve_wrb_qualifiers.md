# Resolve WRB 2022 qualifiers for a Reference Soil Group

Walks the YAML qualifier list for a given RSG code and tests every
principal / supplementary qualifier against the pedon. Returns the
resolved canonical name pieces (principal + supplementary) plus a
per-qualifier trace.

## Usage

``` r
resolve_wrb_qualifiers(pedon, rsg_code, rules = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- rsg_code:

  Two-letter RSG code (e.g. `"FR"` for Ferralsols).

- rules:

  Optional pre-loaded rules list (saves I/O when many RSGs are tested).

## Value

A list with `principal` (character vector), `supplementary` (character
vector), `trace`, and `trace_supplementary`.
