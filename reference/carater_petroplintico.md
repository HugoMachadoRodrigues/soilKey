# Carater petroplintico (SiBCS Cap 5)

Caracteres concrecionario e/ou litoplintico ou horizontes concrecionario
/ litoplintico em posicao NAO diagnostica para Plintossolos Petricos,
dentro de `max_depth_cm` (default 150). Discrimina os Subgrupos
petroplinticos de Argissolos (Cap 5: PA, PVA, PV).

## Usage

``` r
carater_petroplintico(pedon, max_depth_cm = 150)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Default 150 cm.

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao: passa se
[`horizonte_concrecionario`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_concrecionario.md)
OU
[`horizonte_litoplintico`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_litoplintico.md)
retornarem TRUE em ao menos uma camada com top \< max_depth_cm.

## References

Embrapa (2018), SiBCS 5a ed., Cap 5; Cap 16 (Plintossolos).
