# Carater hidromorfico (SiBCS Cap 8)

Solos saturados com agua em camada(s) dentro de `max_depth_cm` (default
100 cm), evidenciado por horizonte glei
([`horizonte_glei`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_glei.md))
OU caracter redoxico
([`carater_redoxico`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_redoxico.md))
OU horizonte Eg na designation OU acumulacao de Mn em horizonte E ou B
espodico. Discrimina os Grandes Grupos Hidromorficos /
Hidro-hiperespessos de Espodossolos (Cap 8 1.1, 1.2, 2.1, 2.2, 3.1,
3.2).

## Usage

``` r
carater_hidromorfico(pedon, max_depth_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Default 100 cm.

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao v0.7.5 (aproximacao):

- [`horizonte_glei`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_glei.md)
  dentro de max_depth_cm, OR

- [`carater_redoxico`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_redoxico.md)
  ate max_depth_cm, OR

- designation pattern `Eg` dentro de max_depth_cm.

## References

Embrapa (2018), SiBCS 5a ed., Cap 8, pp 165-168.
