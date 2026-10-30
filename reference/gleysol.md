# Gleysol RSG gate (WRB 2022 Ch 4, p 103)

WRB-canonical (multi-path):

1.  Layer \\= 25 cm starting \\= 40 cm with gleyic properties throughout
    AND reducing conditions in some parts of every sublayer; OR

2.  Mollic/umbric \> 40 cm thick with reducing conditions some parts of
    every sublayer 40 cm below mineral surface to lower limit, AND
    directly underneath a layer \\= 10 cm with lower limit \\= 65 cm
    having gleyic properties + reducing conditions; OR

3.  Permanent saturation by water \\= 40 cm.

Path 1 reads the layers of
[`gleyic_properties`](https://hugomachadorodrigues.github.io/soilKey/reference/gleyic_properties.md)
(since v0.9.312: the matrix colour, the layer underneath and the site's
groundwater, without the old decay-with-depth test) and of
[`reducing_conditions`](https://hugomachadorodrigues.github.io/soilKey/reference/reducing_conditions.md);
since v0.9.312 the 25 cm may be made of adjoining sublayers (Bg1 over
Bg2), counted from the mineral soil surface, and a layer whose redox
reading is undecided leaves the gate `NA` when it could complete the 25
cm. Path 3 is read from the designation (W / saturated marker). Path 2
is not implemented.

## Usage

``` r
gleysol(pedon, strict = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- strict:

  Logical or `NULL`. When `NULL` (default) it resolves via
  `getOption("soilKey.rsg_strict", FALSE)`. `TRUE` tightens path 1 and
  disables path 3.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Tier-3 strict mode (v0.9.98)

With `strict = TRUE` the path-1 gleyic+reducing layer must start within
the upper 25 cm (instead of 40 cm), and the path-3 designation-only
fallback (a “W” / aquic marker) is disabled: strict mode requires
measured gleyic and reducing evidence.
