# Familia: mineralogia da fracao areia (Cap 18, p 286)

Identifica predominio de minerais facilmente alteraveis na fracao areia
(\>= 0,05 mm) na secao de controle. Classes:

- `micacea`: `sand_mica_pct >= 15` (% volume).

- `anfibolitica`: `sand_amphibole_pct >= 15`.

- `feldspatica`: `sand_feldspar_pct >= 15`.

## Usage

``` r
familia_mineralogia_areia(pedon, max_depth_cm = 200, threshold = 15)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Profundidade da secao de controle (default 200).

- threshold:

  Limiar de % volume (default 15).

## Value

[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## Details

Quando os percentuais especificos estao ausentes, busca a coluna
`sand_mineralogy` (atalho qualitativo, valores aceitos: "micacea",
"anfibolitica", "feldspatica").

Aplicavel a Cambissolos, Chernossolos, Gleissolos, Luvissolos, Neossolos
(excepto Quartzarenicos), Nitossolos, Planossolos, Plintossolos e
Vertissolos.

## References

Embrapa (2018), SiBCS 5a ed., Cap 18, p 286.
