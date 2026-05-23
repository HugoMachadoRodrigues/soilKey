# Planosol RSG gate (WRB 2022 Ch 4, p 107)

WRB-canonical: abrupt textural difference \\= 75 cm AND, in 5 cm
directly above or below the abrupt textural difference, stagnic
properties (\>= 50% redoximorphic features) AND reducing conditions.

## Usage

``` r
planosol(pedon, strict = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- strict:

  Logical or `NULL`. When `NULL` (default) it resolves via
  `getOption("soilKey.rsg_strict", FALSE)`. `TRUE` disables the
  planic-features fallback.

## Details

v0.3.4 enforces all three components. The 5-cm-window restriction is
relaxed to "the layer immediately above or below the abrupt textural
difference satisfies stagnic + reducing".

## Tier-3 strict mode (v0.9.98)

With `strict = TRUE` the `planic_features` fallback path is disabled.
Strict mode requires the canonical evidence – an abrupt textural
difference *plus* measured stagnic and reducing conditions in the
bracketing layer – and will not accept the simpler clay-doubling proxy
on its own.
