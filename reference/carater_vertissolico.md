# Carater vertissolico (SiBCS Cap 6)

Solos com horizonte vertico OU caracter vertico em posicao nao
diagnostica para Vertissolos, dentro de `max_depth_cm` (default 150).
Discrimina os Subgrupos vertissolicos de Cambissolos Carbonaticos /
Eutroficos / Tb Eutroferricos (Cap 6 CY 3.1.3, 3.6.2, CX 4.1.5, 4.7.7,
4.11.4).

## Usage

``` r
carater_vertissolico(pedon, max_depth_cm = 150)
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
[`horizonte_vertico`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_vertico.md)
retornar TRUE em ao menos uma camada com `top_cm` \\\<\\ `max_depth_cm`.
SiBCS estrito requer "posicao nao diagnostica para Vertissolos" –
aproximamos isso confiando no dispatcher (apenas chamamos quando ja
sabemos que nao e Vertissolo).

## References

Embrapa (2018), SiBCS 5a ed., Cap 6, pp 146-153; Cap 17 (Vertissolos).
