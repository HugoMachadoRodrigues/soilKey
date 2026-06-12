# Chernozem RSG gate (strengthened, WRB 2022 Ch 4, p 111)

WRB-canonical: chernic horizon AND, starting \\= 50 cm below the lower
limit of the mollic horizon and (if a petrocalcic horizon is present)
above it, a layer with protocalcic properties \\= 5 cm thick OR a calcic
horizon AND base saturation \\= 50% from the surface to the protocalcic
/ calcic layer throughout.

## Usage

``` r
chernozem_strict(pedon, min_bs = 50, max_top_cm = 50, strict = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_bs:

  Numeric threshold or option (see Details).

- max_top_cm:

  Numeric threshold or option (see Details).

- strict:

  Logical or `NULL`. When `NULL` (default) it resolves via
  `getOption("soilKey.rsg_strict", FALSE)`. `TRUE` raises the
  base-saturation floor to 80%.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Details

v0.3.4 strengthens the previous v0.2 chernozem (which only required
mollic + chernic_color) by adding the protocalcic / calcic gate and the
BS \\= 50% requirement.

Note: the v0.2
[`chernozem()`](https://hugomachadorodrigues.github.io/soilKey/reference/chernozem.md)
diagnostic remains available as a less-strict variant;
`chernozem_strict()` is what the v0.3.4 key.yaml uses for the CH RSG.

## Tier-3 strict mode (v0.9.98)

With `strict = TRUE` the base-saturation floor above the
carbonate-bearing layer is raised from 50% to 80%, in line with the very
high base status expected of a textbook Chernozem.
