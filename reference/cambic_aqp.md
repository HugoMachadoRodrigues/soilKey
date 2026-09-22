# Cambic horizon via aqp::getCambicBounds()

Wraps
[`aqp::getCambicBounds()`](https://ncss-tech.github.io/aqp/reference/getCambicBounds.html)
in soilKey's
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
contract. The aqp test enforces the KST 13ed cambic criteria:

- Texture finer than loamy fine sand (i.e. NOT in the sandy-texture
  pattern).

- Soil structure or absence of rock structure.

- Evidence of pedogenic alteration (chroma / value / clay).

- NOT meeting argic / oxic / spodic / mollic criteria.

soilKey's
[`cambic`](https://hugomachadorodrigues.github.io/soilKey/reference/cambic.md)
(and the SiBCS proxy
[`B_incipiente`](https://hugomachadorodrigues.github.io/soilKey/reference/B_incipiente.md))
implements similar logic but with SiBCS / WRB-flavoured exclusions; the
aqp engine here is an independent canonical reference.

## Usage

``` r
cambic_aqp(pedon, argi_bounds = NULL, ...)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- argi_bounds:

  Optional `c(ubound, lbound)` for argillic bounds (forwarded to aqp).
  `NULL` (default) means the aqp internals re-detect.

- ...:

  Reserved for future arguments.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
with `name = "cambic_aqp"`.

## See also

[`cambic`](https://hugomachadorodrigues.github.io/soilKey/reference/cambic.md)
(soilKey hand-coded),
[`aqp::getCambicBounds`](https://ncss-tech.github.io/aqp/reference/getCambicBounds.html).
