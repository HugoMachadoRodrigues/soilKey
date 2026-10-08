# Oxic horizon (USDA Soil Taxonomy)

The USDA oxic horizon is the diagnostic of Oxisols. Its central criteria
match the WRB 2022 ferralic horizon closely enough that v0.2 simply
delegates: every fixture that classifies as Oxisol via USDA also
classifies as Ferralsol via WRB and vice-versa. The fine-grained
differences (USDA's water-dispersible-clay test, the sand-fraction
weatherable-mineral cut-offs) are tracked in the diagnostics.yaml for
v0.8 refinement.

## Usage

``` r
oxic_usda(pedon, ...)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- ...:

  Passed to
  [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
(with `name = "oxic_usda"`).

## Details

The CEC limit is inclusive here: KST 13 (Ch. 3, oxic horizon, criterion
6) asks for "an apparent CEC of 16 cmol(+) or less per kg clay", where
WRB 2022's ferralic horizon asks for "\< 16". Since v0.9.212
[`ferralic()`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md)
follows WRB and this function passes `cec_inclusive = TRUE`, so a
horizon at exactly 16 is oxic but not ferralic.

## References

Soil Survey Staff (2014). *Keys to Soil Taxonomy*, 12th edition.
USDA-NRCS, Washington DC. Chapter 3 – Diagnostic Horizons; oxic.
