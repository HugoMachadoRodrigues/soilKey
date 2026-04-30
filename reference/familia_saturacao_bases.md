# Familia: saturacao por bases (Cap 18, p 285)

Retorna `"eutrofico"` (V \>= 50%) ou `"distrofico"` (V \< 50%) baseado
na media ponderada de `bs_pct` na secao de controle. Pode ser combinado
com prefixos epi-/meso-/endo- via `familia_prefixo_profundidade`.

## Usage

``` r
familia_saturacao_bases(pedon, max_depth_cm = 150, threshold = 50)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Profundidade da secao de controle (default 150 cm; p. 31 do SiBCS
  define a secao de controle dos Argissolos / Latossolos como 0-150 cm
  de B).

- threshold:

  Limiar de eutrofico (default 50%).

## Value

[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## Details

Aplicavel a todas as classes que ainda nao consideram saturacao por
bases em nivel categorico mais alto (p.ex. Latossolos Eutroficos /
Distroficos ja a consideram).

## References

Embrapa (2018), SiBCS 5a ed., Cap 1, p 31; Cap 18, p 285.
