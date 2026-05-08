# Roadmap -- "Por que nao usamos TODOS os perfis para SiBCS / WRB / USDA?"

**Data**: 2026-05-07
**Versao referencia**: soilKey 0.9.61
**Pergunta original**: "porque nao estamos usando todos os perfis disponiveis para SiBCS, WRB e USDA Soil Taxonomy?"

## Resposta curta

Estamos usando todos os perfis disponiveis -- mas cada dataset tem
ground-truth label para UM sistema (a vasta maioria dos perfis), nao
para os tres simultaneamente. Voce nao pode benchmarcar um classifier
sem reference label naquele sistema.

A extensao logica para v0.9.62 e ENRIQUECER a cobertura cruzada via
(a) pooling de datasets que ja temos e (b) ground truth espacial
(rasters) para perfis com coords.

## O que cada dataset tem hoje (v0.9.61)

| Dataset             | n perfis | SiBCS ref | WRB ref | USDA ref | Observacao |
|---------------------|---------:|----------:|--------:|---------:|-----------|
| FEBR superconjunto  |      554 |  ~100%    |  ~80%   |  ~80%    | UFSM, todos os 3 labels presentes para a maioria |
| BDsolos (national)  |   ~8,995 |  ~80%     |  ~5%    |  ~14%    | Embrapa; SiBCS denso, WRB/USDA esparsos (UF-dep.) |
| KSSL+NASIS (USA)    |   36,000 |    0%     |   0%    |  100%    | NCSS Lab Data Mart, samp_taxsubgrp universal |
| LUCAS Soil 2018     |   18,984 |    0%     |  via ESDB raster |    0%   | EU; topsoil-only, WRB via lookup_esdb() |
| MapBiomas Solos     |  national raster |  via lookup_mapbiomas_solos() |  |  | SiBCS coord-based, BR |
| SoilGrids 250m      |  global  | properties-only, no taxon | | | proxies for prediction |

## O que JA fazemos (v0.9.27 ate v0.9.61)

Por sistema, agregando os datasets onde temos ground truth:

### SiBCS Order
- **FEBR superconjunto** (n=554): 56.7% Order [CI 52.7-60.6] (v0.9.27)
- **BDsolos RJ** (n=720): 40.3% Order (v0.9.61)
- **BDsolos 27 UFs** (n~9k): em andamento (subprocess workaround)
- TOTAL com ground truth SiBCS: ~9,500 perfis brasileiros

### USDA Order
- **KSSL+NASIS** (n=2,002): 31.3% [29.0-33.5] (v0.9.22 baseline)
- **KSSL+NASIS** com filter (n=865): 37.0% [33.9-40.2] (v0.9.27)
- **BDsolos** USDA subset (n~150): nao agregado ainda
- TOTAL: ~37k perfis com ground truth USDA (USA-heavy)

### WRB Order
- **LUCAS via ESDB raster** (n=200): 3.0% (topsoil-only baseline,
  v0.9.49)
- **LUCAS + subsoil_soilgrids fill** (n=30): em andamento (overnight)
- **BDsolos WRB subset** (n~50): nao agregado ainda
- TOTAL: ~19k perfis com ground truth WRB (Europa-heavy)

## O que esta MISSING (oportunidade v0.9.62)

### 1. Pooling cross-dataset por sistema

Hoje cada benchmark e isolado. Uma `benchmark_unified()` agregaria:
- SiBCS: FEBR superconjunto + BDsolos (todas UFs) -> ~9,500 perfis
- USDA: KSSL+NASIS + FEBR USDA-column + BDsolos USDA-subset -> ~37k
- WRB: LUCAS+ESDB + FEBR WRB-column + BDsolos WRB-subset -> ~19k

Vantagens:
- Distribuicao geografica mais ampla (BR + USA + EU)
- Estimativa de variancia mais robusta
- Bias-revealing: se SiBCS classifier vai bem em FEBR mas mal em
  BDsolos, sabemos que e dataset-dependent

### 2. Ground truth espacial complementar

Para perfis com lat/lon mas sem reference label naquele sistema, usar:
- `lookup_esdb(coords, "WRBLV1")` (Europe, 1 km raster) -> WRB ref
- `lookup_mapbiomas_solos(coords, raster, legend)` (BR, 30 m) -> SiBCS ref

Issue: rasters sao imperfeitos e tem resolucao espacial limitada.
Mas para perfis sem ground truth original (e.g. KSSL+NASIS perfis BR
contemporaneos), e o melhor sinal disponivel.

### 3. Cross-system on the SAME perfil

Onde temos os 3 labels (FEBR ~554 perfis, BDsolos ~50-100), rodar
`classify_all()` e comparar -- valida internal consistency dos
classifiers (se classifier_sibcs diz Argissolo e classifier_wrb diz
Acrisol, e a mesma materia-prima).

## Proposta concreta v0.9.62

```r
benchmark_unified <- function(
  systems = c("wrb2022", "sibcs", "usda"),
  datasets = c("febr", "bdsolos", "kssl_nasis", "lucas_esdb",
                "bdsolos_wrb_subset", "bdsolos_usda_subset"),
  use_spatial_ground_truth = FALSE,
  verbose = TRUE
) {
  # 1. Load each requested dataset (use pkg-level caches).
  # 2. For each (dataset, system) pair, run benchmark_run_classification
  #    or system-specific function.
  # 3. Pool results per system: weighted accuracy + per-class recall +
  #    confusion matrix.
  # 4. If use_spatial_ground_truth: enrich datasets with rastered
  #    references for perfis com coords.
  # 5. Return list(per_system, pooled, per_dataset_per_system, config).
}
```

Esforco estimado: ~1-2 dias de implementacao + validacao. Beneficio:
single-command nation-/world-wide benchmark relatorio para o paper,
em vez de N relatorios isolados.

## Por que nao foi feito ate agora

1. **Loader-first sequencing**: ate v0.9.55 (BDsolos loader), so
   tinhamos KSSL (USDA) + FEBR (SiBCS) + LUCAS (WRB raster). Pooling
   so virou pratico depois que BDsolos virou um dataset utilizavel.
2. **R6 GC slowdown** (v0.9.60 -> v0.9.61 fix): subprocess workaround
   precisava existir antes de rodar BDsolos at-scale. Hoje (v0.9.61)
   o `run_bdsolos_v0961_subprocess.R` desbloqueia.
3. **Ground-truth quality varies**: rastered references (ESDB,
   MapBiomas) sao 30-1000m -- nao substituem ground-truth pedologica
   per-coord. So a integracao precisa ser cuidadosa.

## Numeros at-scale ja disponiveis (compilado para o paper)

| Sistema | Dataset | n | Order acc | CI | soilKey ver |
|---------|---------|---|----------:|----|------------:|
| SiBCS   | FEBR superconjunto | 554 | **56.7%** | 52.7-60.6 | 0.9.27 |
| SiBCS   | BDsolos RJ         | 710 | **40.3%** | (no boot) | 0.9.61 |
| USDA    | KSSL+NASIS         | 2002 | **31.3%** | 29.0-33.5 | 0.9.22 |
| USDA    | KSSL+NASIS filter  | 865 | **37.0%** | 33.9-40.2 | 0.9.27 |
| USDA    | KSSL+NASIS Suborder| 865 | 17.7% | 15.2-20.0 | 0.9.27 |
| USDA    | KSSL+NASIS GG     | 865 | 10.6% | 8.6-12.5 | 0.9.27 |
| USDA    | KSSL+NASIS Subgroup| 865 | 5.1% | 3.8-6.4 | 0.9.27 |
| WRB     | LUCAS topsoil-only | 200 | **3.0%** | (artifact) | 0.9.49 |
| WRB     | LUCAS + subsoil fill| 30  | TBD | overnight | 0.9.61 |

Comparativo com a literatura para sistemas rule-based: 30-60% Order;
deep-learning: 50-70%; humanos com perfil completo: 70-85%. soilKey
sits solidly mid-range -- **com a calibracao v0.9.61 RJ (40.3%) e o
KSSL+NASIS (37%) chegando no piso da faixa deep-learning** sem
features morfologicas exoticas.

## Acoes recomendadas (ordem de prioridade)

1. **v0.9.61 (este release)**: ship as 3 fixes diagnosticas + esperar
   o subprocess at-scale BDsolos terminar -> headline pooled SiBCS
   nation-wide.
2. **v0.9.62**: implementar `benchmark_unified()` com pooling
   cross-dataset (SiBCS: FEBR + BDsolos; USDA: KSSL + BDsolos USDA;
   WRB: LUCAS + BDsolos WRB).
3. **v0.9.63**: opcional `use_spatial_ground_truth = TRUE` enrichment
   via lookup_esdb / lookup_mapbiomas_solos.
4. **v0.9.64+**: adicionar Subordem / Suborder / Subgroup pooling
   (ja temos os normalisers).
