# Carater perferrico (SiBCS Cap 1; Cap 6 CX Perferricos)

Teor de Fe2O3 (pelo ataque sulfurico-NaOH) \>= 360 g/kg de solo (= 36%)
na maior parte dos primeiros 100 cm do horizonte B. Discrimina os
Grandes Grupos Perferricos (acima do range "ferrico" 180-360 g/kg). Cap
6 CX 4.3 e Cap 10 (Latossolos Perferricos).

## Usage

``` r
carater_perferrico(pedon, min_fe2o3_pct = 36, max_depth_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_fe2o3_pct:

  Limite inferior em % mass (default 36 = 360 g/kg).

- max_depth_cm:

  Profundidade maxima de B avaliado (default 100).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Embrapa (2018), SiBCS 5a ed., Cap 1; Cap 6, p 142; Cap 10 (Latossolos
Perferricos).
