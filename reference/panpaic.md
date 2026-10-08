# Panpaic horizon (WRB 2022 Ch 3.1)

From Quechua *p'anpay* = "to bury". A buried diagnostic horizon (any
horizon whose original surface was subsequently overlain by younger
material). Used by the Panpaic qualifier and by the Cambisols /
Anthrosols branches.

## Usage

``` r
panpaic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Details

Since v0.9.220 the four criteria of WRB 2022 Ch 3.1.23 on a buried
surface horizon (an A designation with a `b` suffix, or below a lithic
discontinuity, `2A`): \\\ge\\ 0.2% SOC; SOC \\\ge\\ 25% (relative) and
\\\ge\\ 0.2% (absolute) higher than in the overlying layer; a lithic
discontinuity at its upper limit; \\\ge\\ 5 cm thick. Until v0.9.219 any
designation containing a `b` passed, `AB` included.
