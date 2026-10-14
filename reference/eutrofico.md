# Solo eutrofico (SiBCS Cap 1, p 30)

Returns TRUE se a saturacao por bases (V%) \>= 50% no horizonte
diagnostico subsuperficial (B ou C). 65% para A chernozemico.

## Usage

``` r
eutrofico(pedon, min_v = 50)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_v:

  Numeric threshold or option (see Details).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Details

Since v0.9.257, in a profile with a B horizon the base saturation is
judged as the key and the definitions of the classes ask for it, "na
maior parte dos primeiros 100 cm do horizonte B (inclusive BA)": it is
high when the B layers with V of `min_v` or more make more than half of
the thickness of that section, among the layers where V is recorded. The
BC is not part of the section. Until v0.9.256 one layer of B or of C
with a high V was enough. A profile with no B is read as before, in its
C layers, or in all of its layers when it has no C either.
