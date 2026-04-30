# Carater terrico (SiBCS Cap 14)

Solos com horizontes ou camadas constituidos por materiais minerais
(horizonte A, Ag, Big e/ou Cg), com espessura cumulativa \\\ge\\
`min_thickness_cm` dentro de `within_depth_cm` da superficie do solo.
Discrimina os Subgrupos terricos de Organossolos (Cap 14, pp 245-250) e
Cambissolos terricos (Cap 6).

## Usage

``` r
carater_terrico(pedon, min_thickness_cm = 30, within_depth_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness_cm:

  Espessura cumulativa minima de material mineral (default 30 cm).

- within_depth_cm:

  Profundidade de busca (default 100 cm).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md);
`passed = TRUE` se a soma da espessura dos horizontes minerais (truncada
em `within_depth_cm`) for \\\ge\\ `min_thickness_cm`.

## Details

Padroes de designacao reconhecidos para horizonte mineral:

- `A`, `Ap`, `An` (mineral superficial)

- `Ag` (mineral hidromorfico)

- `Big`, `Bg` (B mineral hidromorfico)

- `Cg` (C mineral hidromorfico)

- `C`, `Cr`, `Crf` (mineral subsuperficial)

Excluidos do somatorio: horizontes histicos (`H*`, `O*`) e horizontes
cementados puros sem material mineral.

## References

Embrapa (2018), SiBCS 5a ed., Cap 14, p 246 (subgrupos terricos de
Organossolos).
