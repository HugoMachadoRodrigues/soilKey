# Carater espessarenico (SiBCS Cap 5)

Textura arenosa (clay% \< `max_clay_pct`) da superficie ate boundary em
\[100, 200\] cm. Variante "espessa" do
[`carater_arenico`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_arenico.md).

## Usage

``` r
carater_espessarenico(
  pedon,
  max_clay_pct = 15,
  min_depth_cm = 100,
  max_depth_cm = 200
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_clay_pct:

  Limite superior de % argila (default 15).

- min_depth_cm:

  Profundidade minima do boundary (default 100).

- max_depth_cm:

  Profundidade maxima do boundary (default 200).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Embrapa (2018), SiBCS 5a ed., Cap 5, pp 130-131.
