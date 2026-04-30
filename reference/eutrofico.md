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
