# Kastanozem RSG gate (strengthened, WRB 2022 Ch 4, p 112)

Same structure as
[`chernozem_strict`](https://hugomachadorodrigues.github.io/soilKey/reference/chernozem_strict.md)
but using the mollic horizon (no chernic gate) and starting \\= 70 cm of
mineral soil surface.

## Usage

``` r
kastanozem_strict(pedon, min_bs = 50, max_top_cm = 70, strict = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_bs:

  Minimum base saturation % above the carbonate layer (default 50).

- max_top_cm:

  Maximum depth of the carbonate layer below the mineral soil surface,
  cm (default 70).

- strict:

  Logical or `NULL`. When `NULL` (default) it resolves via
  `getOption("soilKey.rsg_strict", FALSE)`. `TRUE` raises the
  base-saturation floor to 75%.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Tier-3 strict mode (v0.9.98)

With `strict = TRUE` the base-saturation floor above the
carbonate-bearing layer is raised from 50% to 75%. The 70 cm
carbonate-depth window is left unchanged.
