# Argic / argillic horizon via aqp::getArgillicBounds()

Wraps
[`aqp::getArgillicBounds()`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.html)
(Beaudette et al.) in soilKey's
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
contract. The aqp implementation is the canonical NRCS R port and uses
the tiered USDA-NRCS clay-increase thresholds:

- Eluvial clay \< 15\\

- Eluvial clay 15-40\\

- Eluvial clay \\= 40\\

(vs. soilKey's hand-coded
[`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
which uses the WRB 6/1.4/20 thresholds). For BDsolos / FEBR / KSSL
profiles the aqp rule is closer to KST 13ed and BDsolos field practice.

## Usage

``` r
argic_aqp(pedon, require_t = FALSE, ...)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- require_t:

  Whether to require an explicit "t" suffix in the horizon designation
  (default `FALSE` for BDsolos / FEBR; `TRUE` matches the strict KST
  13ed text).

- ...:

  Reserved for future arguments.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
with `name = "argic_aqp"`. `$layers` are the row indices of horizons in
the argillic / argic depth interval. `$evidence` carries the raw aqp
`c(ubound, lbound)` bounds for traceability.

## Details

By default aqp requires a "t" suffix in the horizon designation
(`require_t = TRUE`); we expose this so callers can be permissive on
datasets where designation is missing or non-conforming (BDsolos exports
often drop the "t").

## See also

[`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
(soilKey hand-coded; WRB 6/1.4/20),
[`aqp::getArgillicBounds`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.html).
