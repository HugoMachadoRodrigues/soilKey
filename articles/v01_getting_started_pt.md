# Começando com soilKey (PT-BR)

`soilKey` faz classificação automática de perfis de solo nos três
sistemas canônicos: **WRB 2022** (4ª edição), **SiBCS 5ª ed.** (Embrapa,
2018) e **USDA Soil Taxonomy 13ª ed.** (USDA-NRCS, 2022). A chave
taxonômica em si é implementada como código R determinístico orientado
por regras YAML versionadas; extração via *vision-language model*,
priors espaciais (SoilGrids) e predição de atributos via OSSL ficam ao
lado da chave como módulos separados, **nunca dentro dela**.

Esta vinheta é uma adaptação para PT-BR de `v01_getting_started`. O
conteúdo cobre o mesmo material; preferimos PT-BR aqui porque a
comunidade brasileira de pedologia é grande e quase toda a literatura
SiBCS é escrita em português.

## 0. O começo de 30 segundos

Se você só quer ver `soilKey` funcionando ponta-a-ponta em um perfil
real, sem escrever código R, há dois caminhos.

### A. Interface Shiny (sem código)

``` r

library(soilKey)
run_classify_app()   # abre um app Shiny em uma única tela no navegador
```

Faça upload de um CSV (uma linha por horizonte) ou edite manualmente,
clique em **Classify**, e leia os nomes WRB / SiBCS / USDA junto com o
*key trace* determinístico e a *evidence grade*.

### B. Uma chamada R, uma fixture

``` r

library(soilKey)
#> 
#> Attaching package: 'soilKey'
#> The following object is masked from 'package:base':
#> 
#>     %||%

pedon <- make_ferralsol_canonical()   # Latossolo Vermelho canônico
classify_wrb2022(pedon, on_missing = "silent")$name
#> [1] "Geric Rhodic Ferralsol (Clayic, Epic, Eutric, Humic)"
classify_sibcs(pedon)$name
#> [1] "Latossolos Vermelhos Distróficos típicos"
classify_usda(pedon, on_missing = "silent")$name
#> [1] "Rhodic Hapludox"
```

É o pacote inteiro: `PedonRecord` entra, classificação sai. As seções
seguintes mostram como construir o seu próprio pedon e como os módulos
laterais (VLM, espacial, espectral) se conectam.

## 1. Construindo um PedonRecord do zero

`PedonRecord` é a estrutura central de dados. Ele agrupa:

- **Metadados de sítio** (id, lat/lon, país, material de origem, …)
- **Tabela de horizontes** com o esquema canônico fixo (veja
  [`horizon_column_spec()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizon_column_spec.md)
  para a lista completa de colunas)
- **Espectros** opcionais (Vis-NIR / SWIR / MIR)
- **Imagens** opcionais (foto de perfil)
- **Documentos** opcionais (PDF / *field report*)
- **Log de proveniência por atributo**: cada valor numérico ou
  categórico carrega uma tag `source` ∈ {`measured`,
  `predicted_spectra`, `extracted_vlm`, `inferred_prior`,
  `user_assumed`}

``` r

meu_pedon <- PedonRecord$new(
  site = list(
    id              = "exemplo-001",
    lat             = -22.5,
    lon             = -43.7,
    country         = "BR",
    parent_material = "gnaisse"
  ),
  horizons = data.frame(
    top_cm      = c(0,  15, 65, 130),
    bottom_cm   = c(15, 65, 130, 200),
    designation = c("A", "AB", "Bw1", "Bw2"),
    munsell_hue_moist    = rep("2.5YR", 4),
    munsell_value_moist  = c(3, 3, 4, 4),
    munsell_chroma_moist = c(4, 6, 6, 6),
    clay_pct = c(50, 55, 60, 60),
    silt_pct = c(15, 10, 8,  8),
    sand_pct = c(35, 35, 32, 32),
    cec_cmol = c(8, 5.5, 5.0, 4.8),
    bs_pct   = c(24, 14, 13, 13),
    ph_h2o   = c(4.8, 4.7, 4.8, 4.9),
    oc_pct   = c(2.0, 0.6, 0.3, 0.2)
  )
)
```

Ao chamar qualquer `classify_*()` em cima desse pedon, soilKey lê os
dados, anda a chave taxonômica, e retorna um objeto
`ClassificationResult` com:

- O **nome** atribuído (`Latossolos Vermelhos Distroficos tipicos` na
  SiBCS, por exemplo)
- O **trace da chave** (quais RSGs / Ordens foram testadas, quais
  diagnósticos passaram)
- O **evidence grade** A-D resumindo a qualidade dos dados que
  produziram a decisão
- A lista de **atributos faltantes** que, se medidos, mudariam ou
  refinariam a classificação

## 2. Classificando nos três sistemas

A interface é simétrica: um `PedonRecord` entra, um
`ClassificationResult` sai. As três funções aceitam um argumento
`on_missing` que controla o que fazer quando atributos críticos não
estão presentes:

- `"warn"` (padrão): avisa, classifica com base no que tem
- `"silent"`: classifica silenciosamente
- `"error"`: para com erro

``` r

res_wrb   <- classify_wrb2022(meu_pedon, on_missing = "silent")
res_sibcs <- classify_sibcs(meu_pedon, include_familia = TRUE)
res_usda  <- classify_usda(meu_pedon,  on_missing = "silent")

res_wrb$name
#> [1] "Haplic Ferralsol (Clayic, Epic, Eutric, Humic)"
res_sibcs$name
#> [1] "Latossolos Vermelhos Distróficos típicos, textura argilosa, A moderado"
res_usda$name
#> [1] "Typic Hapludox"
```

### Atalho: classificar nos três sistemas em uma chamada

``` r

todos <- classify_all(meu_pedon, on_missing = "silent")
todos$summary
#>                                              wrb
#> 1 Haplic Ferralsol (Clayic, Epic, Eutric, Humic)
#>                                                                    sibcs
#> 1 Latossolos Vermelhos Distróficos típicos, textura argilosa, A moderado
#>             usda
#> 1 Typic Hapludox
```

[`classify_all()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md)
retorna uma lista nomeada com `wrb`, `sibcs`, `usda` (cada uma um
`ClassificationResult` completo) e um resumo `summary` em `data.frame`
de uma linha — útil para tabular muitos pedons de uma vez só.

## 3. Inspecionando o key trace

O *trace* da chave é o histórico determinístico da decisão. Cada RSG /
Ordem / Sistema testado aparece com o resultado de cada diagnóstico
envolvido:

``` r

res_wrb$trace
#> $HS
#> $HS$code
#> [1] "HS"
#> 
#> $HS$name
#> [1] "Histosols"
#> 
#> $HS$passed
#> [1] FALSE
#> 
#> $HS$evidence
#> $HS$evidence[[1]]
#> $HS$evidence[[1]]$test_name
#> [1] "histosol"
#> 
#> $HS$evidence[[1]]$passed
#> [1] FALSE
#> 
#> $HS$evidence[[1]]$layers
#> integer(0)
#> 
#> $HS$evidence[[1]]$missing
#> character(0)
#> 
#> $HS$evidence[[1]]$evidence
#> $HS$evidence[[1]]$evidence$items
#> $HS$evidence[[1]]$evidence$items$item1
#> [1] FALSE
#> 
#> $HS$evidence[[1]]$evidence$items$item2
#> [1] FALSE
#> 
#> $HS$evidence[[1]]$evidence$items$item3
#> [1] FALSE
#> 
#> 
#> $HS$evidence[[1]]$evidence$organic_thickness_100cm
#> [1] 0
#> 
#> $HS$evidence[[1]]$evidence$possible_organic_thickness_100cm
#> [1] 0
#> 
#> $HS$evidence[[1]]$evidence$organic_from_surface
#> integer(0)
#> 
#> $HS$evidence[[1]]$evidence$possible_organic_from_surface
#> integer(0)
#> 
#> 
#> $HS$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Histosols (p. 95)"
#> 
#> $HS$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $HS$missing
#> character(0)
#> 
#> $HS$notes
#> NULL
#> 
#> 
#> $AT
#> $AT$code
#> [1] "AT"
#> 
#> $AT$name
#> [1] "Anthrosols"
#> 
#> $AT$passed
#> [1] FALSE
#> 
#> $AT$evidence
#> $AT$evidence[[1]]
#> $AT$evidence[[1]]$test_name
#> [1] "anthric_horizons"
#> 
#> $AT$evidence[[1]]$passed
#> [1] FALSE
#> 
#> $AT$evidence[[1]]$layers
#> integer(0)
#> 
#> $AT$evidence[[1]]$missing
#> character(0)
#> 
#> $AT$evidence[[1]]$evidence
#> $AT$evidence[[1]]$evidence$designation
#> $AT$evidence[[1]]$evidence$designation$anthric_designation
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$passed
#> [1] FALSE
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$layers
#> integer(0)
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$missing
#> character(0)
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`1`
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`1`$idx
#> [1] 1
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`1`$designation
#> [1] "A"
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`1`$pattern
#> [1] "hortic|irragric|plaggic|pretic|terric"
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`1`$passed
#> [1] FALSE
#> 
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`2`
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`2`$idx
#> [1] 2
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`2`$designation
#> [1] "AB"
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`2`$pattern
#> [1] "hortic|irragric|plaggic|pretic|terric"
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`2`$passed
#> [1] FALSE
#> 
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`3`
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`3`$idx
#> [1] 3
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`3`$designation
#> [1] "Bw1"
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`3`$pattern
#> [1] "hortic|irragric|plaggic|pretic|terric"
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`3`$passed
#> [1] FALSE
#> 
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`4`
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`4`$idx
#> [1] 4
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`4`$designation
#> [1] "Bw2"
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`4`$pattern
#> [1] "hortic|irragric|plaggic|pretic|terric"
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$details$`4`$passed
#> [1] FALSE
#> 
#> 
#> 
#> $AT$evidence[[1]]$evidence$designation$anthric_designation$notes
#> [1] NA
#> 
#> 
#> 
#> $AT$evidence[[1]]$evidence$property_based
#> $AT$evidence[[1]]$evidence$property_based$anthric_props
#> $AT$evidence[[1]]$evidence$property_based$anthric_props$passed
#> [1] FALSE
#> 
#> $AT$evidence[[1]]$evidence$property_based$anthric_props$layers
#> integer(0)
#> 
#> $AT$evidence[[1]]$evidence$property_based$anthric_props$missing
#> character(0)
#> 
#> $AT$evidence[[1]]$evidence$property_based$anthric_props$details
#> NULL
#> 
#> $AT$evidence[[1]]$evidence$property_based$anthric_props$notes
#> [1] NA
#> 
#> 
#> 
#> 
#> $AT$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 5, Anthrosols"
#> 
#> $AT$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $AT$missing
#> character(0)
#> 
#> $AT$notes
#> NULL
#> 
#> 
#> $TC
#> $TC$code
#> [1] "TC"
#> 
#> $TC$name
#> [1] "Technosols"
#> 
#> $TC$passed
#> [1] NA
#> 
#> $TC$evidence
#> $TC$evidence[[1]]
#> $TC$evidence[[1]]$test_name
#> [1] "technic_features"
#> 
#> $TC$evidence[[1]]$passed
#> [1] NA
#> 
#> $TC$evidence[[1]]$layers
#> integer(0)
#> 
#> $TC$evidence[[1]]$missing
#> [1] "artefacts_pct"            "geomembrane_present"     
#> [3] "technic_hardmaterial_pct"
#> 
#> $TC$evidence[[1]]$evidence
#> $TC$evidence[[1]]$evidence$artefacts
#> $TC$evidence[[1]]$evidence$artefacts$artefacts
#> $TC$evidence[[1]]$evidence$artefacts$artefacts$passed
#> [1] NA
#> 
#> $TC$evidence[[1]]$evidence$artefacts$artefacts$layers
#> integer(0)
#> 
#> $TC$evidence[[1]]$evidence$artefacts$artefacts$missing
#> [1] "artefacts_pct"
#> 
#> $TC$evidence[[1]]$evidence$artefacts$artefacts$details
#> $TC$evidence[[1]]$evidence$artefacts$artefacts$details$summary
#> $TC$evidence[[1]]$evidence$artefacts$artefacts$details$summary$weighted_mean_pct
#> [1] NA
#> 
#> $TC$evidence[[1]]$evidence$artefacts$artefacts$details$summary$lower_bound_pct
#> [1] 0
#> 
#> $TC$evidence[[1]]$evidence$artefacts$artefacts$details$summary$upper_bound_pct
#> [1] 100
#> 
#> $TC$evidence[[1]]$evidence$artefacts$artefacts$details$summary$window_cm
#> [1]   0 100
#> 
#> $TC$evidence[[1]]$evidence$artefacts$artefacts$details$summary$thickness_evaluated_cm
#> [1] 0
#> 
#> $TC$evidence[[1]]$evidence$artefacts$artefacts$details$summary$thickness_unmeasured_cm
#> [1] 100
#> 
#> $TC$evidence[[1]]$evidence$artefacts$artefacts$details$summary$threshold
#> [1] 20
#> 
#> 
#> 
#> $TC$evidence[[1]]$evidence$artefacts$artefacts$notes
#> [1] NA
#> 
#> 
#> 
#> $TC$evidence[[1]]$evidence$geomembrane
#> $TC$evidence[[1]]$evidence$geomembrane$geomembrane
#> $TC$evidence[[1]]$evidence$geomembrane$geomembrane$passed
#> [1] NA
#> 
#> $TC$evidence[[1]]$evidence$geomembrane$geomembrane$layers
#> integer(0)
#> 
#> $TC$evidence[[1]]$evidence$geomembrane$geomembrane$missing
#> [1] "geomembrane_present"
#> 
#> $TC$evidence[[1]]$evidence$geomembrane$geomembrane$details
#> NULL
#> 
#> $TC$evidence[[1]]$evidence$geomembrane$geomembrane$notes
#> [1] NA
#> 
#> 
#> 
#> $TC$evidence[[1]]$evidence$hardmaterial
#> $TC$evidence[[1]]$evidence$hardmaterial$hardmaterial
#> $TC$evidence[[1]]$evidence$hardmaterial$hardmaterial$passed
#> [1] NA
#> 
#> $TC$evidence[[1]]$evidence$hardmaterial$hardmaterial$layers
#> integer(0)
#> 
#> $TC$evidence[[1]]$evidence$hardmaterial$hardmaterial$missing
#> [1] "technic_hardmaterial_pct"
#> 
#> $TC$evidence[[1]]$evidence$hardmaterial$hardmaterial$details
#> NULL
#> 
#> $TC$evidence[[1]]$evidence$hardmaterial$hardmaterial$notes
#> [1] NA
#> 
#> 
#> 
#> 
#> $TC$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 5, Technosols"
#> 
#> $TC$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $TC$missing
#> [1] "artefacts_pct"            "geomembrane_present"     
#> [3] "technic_hardmaterial_pct"
#> 
#> $TC$notes
#> NULL
#> 
#> 
#> $CR
#> $CR$code
#> [1] "CR"
#> 
#> $CR$name
#> [1] "Cryosols"
#> 
#> $CR$passed
#> [1] NA
#> 
#> $CR$evidence
#> $CR$evidence[[1]]
#> $CR$evidence[[1]]$test_name
#> [1] "cryosol"
#> 
#> $CR$evidence[[1]]$passed
#> [1] NA
#> 
#> $CR$evidence[[1]]$layers
#> integer(0)
#> 
#> $CR$evidence[[1]]$missing
#> [1] "permafrost_temp_C"
#> 
#> $CR$evidence[[1]]$evidence
#> $CR$evidence[[1]]$evidence$item1
#> [1] NA
#> 
#> $CR$evidence[[1]]$evidence$cryic_100cm
#> 
#> ── DiagnosticResult: cryic_conditions
#> Status: NA (insufficient data)
#> Missing attributes (1): permafrost_temp_C
#> Sub-tests:
#> Reference: IUSS Working Group WRB (2022), Chapter 5, Cryosols
#> 
#> $CR$evidence[[1]]$evidence$item2
#> $CR$evidence[[1]]$evidence$item2$passed
#> [1] FALSE
#> 
#> $CR$evidence[[1]]$evidence$item2$cryic_200cm
#> 
#> ── DiagnosticResult: cryic_conditions
#> Status: NA (insufficient data)
#> Missing attributes (1): permafrost_temp_C
#> Sub-tests:
#> Reference: IUSS Working Group WRB (2022), Chapter 5, Cryosols
#> 
#> $CR$evidence[[1]]$evidence$item2$cryogenic_alteration_layers
#> integer(0)
#> 
#> 
#> 
#> $CR$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Cryosols (p. 98)"
#> 
#> $CR$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $CR$missing
#> [1] "permafrost_temp_C"
#> 
#> $CR$notes
#> NULL
#> 
#> 
#> $LP
#> $LP$code
#> [1] "LP"
#> 
#> $LP$name
#> [1] "Leptosols"
#> 
#> $LP$passed
#> [1] NA
#> 
#> $LP$evidence
#> $LP$evidence[[1]]
#> $LP$evidence[[1]]$test_name
#> [1] "leptic_features"
#> 
#> $LP$evidence[[1]]$passed
#> [1] NA
#> 
#> $LP$evidence[[1]]$layers
#> integer(0)
#> 
#> $LP$evidence[[1]]$missing
#> [1] "coarse_fragments_pct"
#> 
#> $LP$evidence[[1]]$evidence
#> $LP$evidence[[1]]$evidence$designation
#> $LP$evidence[[1]]$evidence$designation$rock_designation
#> $LP$evidence[[1]]$evidence$designation$rock_designation$passed
#> [1] FALSE
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$layers
#> integer(0)
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$missing
#> character(0)
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`1`
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`1`$idx
#> [1] 1
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`1`$designation
#> [1] "A"
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`1`$pattern
#> [1] "^[0-9]*R($|[a-z0-9]|/?Cr)"
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`1`$passed
#> [1] FALSE
#> 
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`2`
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`2`$idx
#> [1] 2
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`2`$designation
#> [1] "AB"
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`2`$pattern
#> [1] "^[0-9]*R($|[a-z0-9]|/?Cr)"
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`2`$passed
#> [1] FALSE
#> 
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`3`
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`3`$idx
#> [1] 3
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`3`$designation
#> [1] "Bw1"
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`3`$pattern
#> [1] "^[0-9]*R($|[a-z0-9]|/?Cr)"
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`3`$passed
#> [1] FALSE
#> 
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`4`
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`4`$idx
#> [1] 4
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`4`$designation
#> [1] "Bw2"
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`4`$pattern
#> [1] "^[0-9]*R($|[a-z0-9]|/?Cr)"
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$details$`4`$passed
#> [1] FALSE
#> 
#> 
#> 
#> $LP$evidence[[1]]$evidence$designation$rock_designation$notes
#> [1] NA
#> 
#> 
#> $LP$evidence[[1]]$evidence$designation$shallow
#> $LP$evidence[[1]]$evidence$designation$shallow$passed
#> [1] FALSE
#> 
#> $LP$evidence[[1]]$evidence$designation$shallow$layers
#> integer(0)
#> 
#> $LP$evidence[[1]]$evidence$designation$shallow$missing
#> character(0)
#> 
#> $LP$evidence[[1]]$evidence$designation$shallow$details
#> list()
#> 
#> $LP$evidence[[1]]$evidence$designation$shallow$notes
#> [1] NA
#> 
#> 
#> 
#> $LP$evidence[[1]]$evidence$coarse_fragments
#> $LP$evidence[[1]]$evidence$coarse_fragments$coarse_at_surface
#> $LP$evidence[[1]]$evidence$coarse_fragments$coarse_at_surface$passed
#> [1] NA
#> 
#> $LP$evidence[[1]]$evidence$coarse_fragments$coarse_at_surface$layers
#> integer(0)
#> 
#> $LP$evidence[[1]]$evidence$coarse_fragments$coarse_at_surface$missing
#> [1] "coarse_fragments_pct"
#> 
#> $LP$evidence[[1]]$evidence$coarse_fragments$coarse_at_surface$details
#> NULL
#> 
#> $LP$evidence[[1]]$evidence$coarse_fragments$coarse_at_surface$notes
#> [1] NA
#> 
#> 
#> 
#> $LP$evidence[[1]]$evidence$engine
#> [1] "soilkey"
#> 
#> 
#> $LP$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 5, Leptosols "
#> 
#> $LP$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $LP$missing
#> [1] "coarse_fragments_pct"
#> 
#> $LP$notes
#> NULL
#> 
#> 
#> $SN
#> $SN$code
#> [1] "SN"
#> 
#> $SN$name
#> [1] "Solonetz"
#> 
#> $SN$passed
#> [1] FALSE
#> 
#> $SN$evidence
#> $SN$evidence[[1]]
#> $SN$evidence[[1]]$test_name
#> [1] "solonetz"
#> 
#> $SN$evidence[[1]]$passed
#> [1] FALSE
#> 
#> $SN$evidence[[1]]$layers
#> integer(0)
#> 
#> $SN$evidence[[1]]$missing
#> character(0)
#> 
#> $SN$evidence[[1]]$evidence
#> $SN$evidence[[1]]$evidence$natric_horizon
#> 
#> ── DiagnosticResult: natric_horizon
#> Status: failed
#> Sub-tests:
#> Reference: IUSS Working Group WRB (2022), Chapter 3, Natric horizon
#> Notes: Profile lacks an argic horizon -- natric inapplicable
#> 
#> $SN$evidence[[1]]$evidence$mineral_surface_cm
#> [1] 0
#> 
#> 
#> $SN$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Solonetz (p. 100)"
#> 
#> $SN$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $SN$missing
#> character(0)
#> 
#> $SN$notes
#> NULL
#> 
#> 
#> $VR
#> $VR$code
#> [1] "VR"
#> 
#> $VR$name
#> [1] "Vertisols"
#> 
#> $VR$passed
#> [1] NA
#> 
#> $VR$evidence
#> $VR$evidence[[1]]
#> $VR$evidence[[1]]$test_name
#> [1] "vertisol"
#> 
#> $VR$evidence[[1]]$passed
#> [1] NA
#> 
#> $VR$evidence[[1]]$layers
#> integer(0)
#> 
#> $VR$evidence[[1]]$missing
#> [1] "slickensides"    "structure_type"  "cracks_width_cm"
#> 
#> $VR$evidence[[1]]$evidence
#> $VR$evidence[[1]]$evidence$vertic_horizon
#> 
#> ── DiagnosticResult: vertic_horizon
#> Status: NA (insufficient data)
#> Missing attributes (3): slickensides, structure_type, cracks_width_cm
#> Sub-tests:
#> [ NA ] designation_inference
#> [ NA ] chroma_clay_inference
#> Reference: IUSS Working Group WRB (2022), Chapter 3.1.40, Vertic horizon (p.
#> 70)
#> 
#> $VR$evidence[[1]]$evidence$mineral_surface_cm
#> [1] 0
#> 
#> $VR$evidence[[1]]$evidence$vertic_top_cm
#> [1] 0
#> 
#> $VR$evidence[[1]]$evidence$clay_above_30_throughout
#> [1] TRUE
#> 
#> $VR$evidence[[1]]$evidence$clay_threshold_pct
#> [1] 30
#> 
#> $VR$evidence[[1]]$evidence$strict_mode
#> [1] FALSE
#> 
#> $VR$evidence[[1]]$evidence$cracks_at_surface
#> [1] TRUE
#> 
#> $VR$evidence[[1]]$evidence$cracks
#> $VR$evidence[[1]]$evidence$cracks$passed
#> [1] TRUE
#> 
#> $VR$evidence[[1]]$evidence$cracks$starts
#> $VR$evidence[[1]]$evidence$cracks$starts$mineral_surface
#> [1] TRUE
#> 
#> $VR$evidence[[1]]$evidence$cracks$starts$plough_layer_base
#> [1] FALSE
#> 
#> $VR$evidence[[1]]$evidence$cracks$starts$below_self_mulching_surface
#> [1] NA
#> 
#> 
#> $VR$evidence[[1]]$evidence$cracks$plough_base_cm
#> [1] NA
#> 
#> $VR$evidence[[1]]$evidence$cracks$self_mulching_base_cm
#> [1] NA
#> 
#> $VR$evidence[[1]]$evidence$cracks$crack_status
#> [1] NA NA NA NA
#> 
#> 
#> $VR$evidence[[1]]$evidence$morphological_inference_fired
#> [1] FALSE
#> 
#> 
#> $VR$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Vertisols (p. 101)"
#> 
#> $VR$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $VR$missing
#> [1] "slickensides"    "structure_type"  "cracks_width_cm"
#> 
#> $VR$notes
#> NULL
#> 
#> 
#> $SC
#> $SC$code
#> [1] "SC"
#> 
#> $SC$name
#> [1] "Solonchaks"
#> 
#> $SC$passed
#> [1] NA
#> 
#> $SC$evidence
#> $SC$evidence[[1]]
#> $SC$evidence[[1]]$test_name
#> [1] "salic"
#> 
#> $SC$evidence[[1]]$passed
#> [1] NA
#> 
#> $SC$evidence[[1]]$layers
#> integer(0)
#> 
#> $SC$evidence[[1]]$missing
#> [1] "ec_dS_m"
#> 
#> $SC$evidence[[1]]$evidence
#> $SC$evidence[[1]]$evidence$ec
#> $SC$evidence[[1]]$evidence$ec$passed
#> [1] NA
#> 
#> $SC$evidence[[1]]$evidence$ec$layers
#> integer(0)
#> 
#> $SC$evidence[[1]]$evidence$ec$missing
#> [1] "ec_dS_m"
#> 
#> $SC$evidence[[1]]$evidence$ec$details
#> list()
#> 
#> $SC$evidence[[1]]$evidence$ec$notes
#> [1] NA
#> 
#> 
#> $SC$evidence[[1]]$evidence$thickness
#> $SC$evidence[[1]]$evidence$thickness$passed
#> [1] FALSE
#> 
#> $SC$evidence[[1]]$evidence$thickness$layers
#> integer(0)
#> 
#> $SC$evidence[[1]]$evidence$thickness$missing
#> character(0)
#> 
#> $SC$evidence[[1]]$evidence$thickness$details
#> NULL
#> 
#> $SC$evidence[[1]]$evidence$thickness$notes
#> [1] NA
#> 
#> 
#> $SC$evidence[[1]]$evidence$product
#> $SC$evidence[[1]]$evidence$product$passed
#> [1] FALSE
#> 
#> $SC$evidence[[1]]$evidence$product$layers
#> integer(0)
#> 
#> $SC$evidence[[1]]$evidence$product$missing
#> character(0)
#> 
#> $SC$evidence[[1]]$evidence$product$details
#> list()
#> 
#> $SC$evidence[[1]]$evidence$product$notes
#> [1] NA
#> 
#> 
#> 
#> $SC$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 3.1.20, Salic horizon (p. 49)"
#> 
#> $SC$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $SC$missing
#> [1] "ec_dS_m"
#> 
#> $SC$notes
#> NULL
#> 
#> 
#> $GL
#> $GL$code
#> [1] "GL"
#> 
#> $GL$name
#> [1] "Gleysols"
#> 
#> $GL$passed
#> [1] FALSE
#> 
#> $GL$evidence
#> $GL$evidence[[1]]
#> $GL$evidence[[1]]$test_name
#> [1] "gleysol"
#> 
#> $GL$evidence[[1]]$passed
#> [1] FALSE
#> 
#> $GL$evidence[[1]]$layers
#> integer(0)
#> 
#> $GL$evidence[[1]]$missing
#> [1] "redoximorphic_features_pct"
#> 
#> $GL$evidence[[1]]$evidence
#> $GL$evidence[[1]]$evidence$gleyic_properties
#> 
#> ── DiagnosticResult: gleyic_properties
#> Status: NA (insufficient data)
#> Missing attributes (1): redoximorphic_features_pct
#> Sub-tests:
#> Reference: IUSS Working Group WRB (2022), Chapter 3.2.6, Gleyic properties (p.
#> 75)
#> 
#> $GL$evidence[[1]]$evidence$reducing_conditions
#> 
#> ── DiagnosticResult: reducing_conditions
#> Status: failed
#> Sub-tests:
#> [fail] redox
#> Reference: IUSS Working Group WRB (2022), Chapter 3.2.10
#> 
#> $GL$evidence[[1]]$evidence$path1_layers
#> integer(0)
#> 
#> $GL$evidence[[1]]$evidence$path1_undecided
#> integer(0)
#> 
#> $GL$evidence[[1]]$evidence$path1_ok
#> [1] FALSE
#> 
#> $GL$evidence[[1]]$evidence$path3_ok
#> [1] FALSE
#> 
#> $GL$evidence[[1]]$evidence$strict_mode
#> [1] FALSE
#> 
#> 
#> $GL$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Gleysols (p. 103)"
#> 
#> $GL$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $GL$missing
#> [1] "redoximorphic_features_pct"
#> 
#> $GL$notes
#> NULL
#> 
#> 
#> $AN
#> $AN$code
#> [1] "AN"
#> 
#> $AN$name
#> [1] "Andosols"
#> 
#> $AN$passed
#> [1] FALSE
#> 
#> $AN$evidence
#> $AN$evidence[[1]]
#> $AN$evidence[[1]]$test_name
#> [1] "andosol"
#> 
#> $AN$evidence[[1]]$passed
#> [1] FALSE
#> 
#> $AN$evidence[[1]]$layers
#> integer(0)
#> 
#> $AN$evidence[[1]]$missing
#> character(0)
#> 
#> $AN$evidence[[1]]$evidence
#> $AN$evidence[[1]]$evidence$andic
#> 
#> ── DiagnosticResult: andic_properties
#> Status: NA (insufficient data)
#> Missing attributes (5): al_ox_pct, fe_ox_pct, bulk_density_g_cm3, ph_naf,
#> phosphate_retention_pct
#> Sub-tests:
#> [ NA ] oc_bd_proxy
#> [fail] naf_field_test
#> Reference: IUSS Working Group WRB (2022), Chapter 3, Andic properties
#> 
#> $AN$evidence[[1]]$evidence$vitric
#> 
#> ── DiagnosticResult: vitric_properties
#> Status: NA (insufficient data)
#> Missing attributes (1): volcanic_glass_pct
#> Sub-tests:
#> [ NA ] volcanic_glass
#> [fail] alfe_ox
#> [fail] phosphate_ret
#> Reference: IUSS Working Group WRB (2022), Chapter 3.2.16
#> 
#> $AN$evidence[[1]]$evidence$layer_status
#> [1] NA NA NA NA
#> 
#> $AN$evidence[[1]]$evidence$combined_thickness_cm
#> [1] 0
#> 
#> $AN$evidence[[1]]$evidence$possible_thickness_cm
#> [1] 100
#> 
#> $AN$evidence[[1]]$evidence$item1a
#> [1] NA
#> 
#> $AN$evidence[[1]]$evidence$item1b
#> [1] FALSE
#> 
#> $AN$evidence[[1]]$evidence$limiting_layer_top_cm
#> [1] NA
#> 
#> $AN$evidence[[1]]$evidence$strict_mode
#> [1] FALSE
#> 
#> $AN$evidence[[1]]$evidence$exclusion_failed
#> $AN$evidence[[1]]$evidence$exclusion_failed$argic
#> 
#> ── DiagnosticResult: argic
#> Status: failed
#> Sub-tests:
#> [fail] clay_increase
#> [fail] illuvial_clay
#> [fail] criterion_2
#> [fail] clay_min
#> [fail] thickness
#> [fail] texture
#> Reference: IUSS Working Group WRB (2022), Chapter 3, Argic horizon
#> Notes: No layer satisfied the clay-increase precondition
#> 
#> $AN$evidence[[1]]$evidence$exclusion_failed$ferralic
#> 
#> ── DiagnosticResult: ferralic
#> Status: PASSED
#> Layers satisfying: 2, 3, 4
#> Sub-tests:
#> [PASS] texture
#> [PASS] cec_per_clay
#> [PASS] thickness
#> Reference: IUSS Working Group WRB (2022), Chapter 3.1.10, Ferralic horizon (p.
#> 44)
#> Notes: v0.3.1: ECEC/clay <= 12 test removed; not part of WRB 2022 ferralic.
#> v0.9.67 engine=soilkey threshold = 16 cmol_c/kg clay.
#> 
#> $AN$evidence[[1]]$evidence$exclusion_failed$petroplinthic
#> 
#> ── DiagnosticResult: petroplinthic
#> Status: NA (insufficient data)
#> Missing attributes (3): pan_type, plinthite_pct, cementation_class
#> Sub-tests:
#> Reference: IUSS Working Group WRB (2022), Chapter 3.1.27, Petroplinthic horizon
#> (p. 59)
#> 
#> $AN$evidence[[1]]$evidence$exclusion_failed$pisoplinthic
#> 
#> ── DiagnosticResult: pisoplinthic
#> Status: NA (insufficient data)
#> Missing attributes (1): ferric_nodules_pct
#> Sub-tests:
#> Reference: IUSS Working Group WRB (2022), Chapter 3.1.28, Pisoplinthic horizon
#> (p. 60)
#> 
#> $AN$evidence[[1]]$evidence$exclusion_failed$plinthic
#> 
#> ── DiagnosticResult: plinthic
#> Status: NA (insufficient data)
#> Missing attributes (1): plinthite_pct
#> Sub-tests:
#> [ NA ] plinthite
#> [ NA ] thickness
#> [ NA ] designation_inference
#> Reference: IUSS Working Group WRB (2022), Chapter 3.1.30, Plinthic horizon (p.
#> 62)
#> 
#> $AN$evidence[[1]]$evidence$exclusion_failed$spodic
#> 
#> ── DiagnosticResult: spodic
#> Status: NA (insufficient data)
#> Missing attributes (2): al_ox_pct, fe_ox_pct
#> Sub-tests:
#> [ NA ] alfe_oxalate
#> [fail] ph
#> [fail] illuvial_signature
#> [fail] thickness
#> Reference: IUSS Working Group WRB (2022), Chapter 3, Spodic horizon
#> Notes: v0.9.19: + morphological inference path when Al/Fe oxalate missing
#> 
#> 
#> $AN$evidence[[1]]$evidence$exclusion_buried
#> $AN$evidence[[1]]$evidence$exclusion_buried$argic
#> [1] FALSE
#> 
#> $AN$evidence[[1]]$evidence$exclusion_buried$ferralic
#> [1] FALSE
#> 
#> $AN$evidence[[1]]$evidence$exclusion_buried$petroplinthic
#> [1] FALSE
#> 
#> $AN$evidence[[1]]$evidence$exclusion_buried$pisoplinthic
#> [1] FALSE
#> 
#> $AN$evidence[[1]]$evidence$exclusion_buried$plinthic
#> [1] FALSE
#> 
#> $AN$evidence[[1]]$evidence$exclusion_buried$spodic
#> [1] FALSE
#> 
#> 
#> $AN$evidence[[1]]$evidence$argic_of_andic_layers
#> [1] FALSE
#> 
#> $AN$evidence[[1]]$evidence$exclusion_active
#> $AN$evidence[[1]]$evidence$exclusion_active$argic
#> [1] FALSE
#> 
#> $AN$evidence[[1]]$evidence$exclusion_active$ferralic
#> [1] TRUE
#> 
#> $AN$evidence[[1]]$evidence$exclusion_active$petroplinthic
#> [1] FALSE
#> 
#> $AN$evidence[[1]]$evidence$exclusion_active$pisoplinthic
#> [1] FALSE
#> 
#> $AN$evidence[[1]]$evidence$exclusion_active$plinthic
#> [1] FALSE
#> 
#> $AN$evidence[[1]]$evidence$exclusion_active$spodic
#> [1] FALSE
#> 
#> 
#> 
#> $AN$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Andosols (p. 104)"
#> 
#> $AN$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $AN$missing
#> character(0)
#> 
#> $AN$notes
#> NULL
#> 
#> 
#> $PZ
#> $PZ$code
#> [1] "PZ"
#> 
#> $PZ$name
#> [1] "Podzols"
#> 
#> $PZ$passed
#> [1] NA
#> 
#> $PZ$evidence
#> $PZ$evidence[[1]]
#> $PZ$evidence[[1]]$test_name
#> [1] "spodic"
#> 
#> $PZ$evidence[[1]]$passed
#> [1] NA
#> 
#> $PZ$evidence[[1]]$layers
#> integer(0)
#> 
#> $PZ$evidence[[1]]$missing
#> [1] "al_ox_pct" "fe_ox_pct"
#> 
#> $PZ$evidence[[1]]$evidence
#> $PZ$evidence[[1]]$evidence$alfe_oxalate
#> $PZ$evidence[[1]]$evidence$alfe_oxalate$passed
#> [1] NA
#> 
#> $PZ$evidence[[1]]$evidence$alfe_oxalate$layers
#> integer(0)
#> 
#> $PZ$evidence[[1]]$evidence$alfe_oxalate$missing
#> [1] "al_ox_pct" "fe_ox_pct"
#> 
#> $PZ$evidence[[1]]$evidence$alfe_oxalate$details
#> list()
#> 
#> $PZ$evidence[[1]]$evidence$alfe_oxalate$notes
#> [1] NA
#> 
#> 
#> $PZ$evidence[[1]]$evidence$ph
#> $PZ$evidence[[1]]$evidence$ph$passed
#> [1] FALSE
#> 
#> $PZ$evidence[[1]]$evidence$ph$layers
#> integer(0)
#> 
#> $PZ$evidence[[1]]$evidence$ph$missing
#> character(0)
#> 
#> $PZ$evidence[[1]]$evidence$ph$details
#> list()
#> 
#> $PZ$evidence[[1]]$evidence$ph$notes
#> [1] NA
#> 
#> 
#> $PZ$evidence[[1]]$evidence$illuvial_signature
#> $PZ$evidence[[1]]$evidence$illuvial_signature$passed
#> [1] FALSE
#> 
#> $PZ$evidence[[1]]$evidence$illuvial_signature$layers
#> integer(0)
#> 
#> $PZ$evidence[[1]]$evidence$illuvial_signature$missing
#> character(0)
#> 
#> $PZ$evidence[[1]]$evidence$illuvial_signature$details
#> $PZ$evidence[[1]]$evidence$illuvial_signature$details$designation_match
#> integer(0)
#> 
#> $PZ$evidence[[1]]$evidence$illuvial_signature$details$albic_above_match
#> integer(0)
#> 
#> 
#> $PZ$evidence[[1]]$evidence$illuvial_signature$notes
#> [1] NA
#> 
#> 
#> $PZ$evidence[[1]]$evidence$thickness
#> $PZ$evidence[[1]]$evidence$thickness$passed
#> [1] FALSE
#> 
#> $PZ$evidence[[1]]$evidence$thickness$layers
#> integer(0)
#> 
#> $PZ$evidence[[1]]$evidence$thickness$missing
#> character(0)
#> 
#> $PZ$evidence[[1]]$evidence$thickness$details
#> NULL
#> 
#> $PZ$evidence[[1]]$evidence$thickness$notes
#> [1] NA
#> 
#> 
#> 
#> $PZ$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 3, Spodic horizon"
#> 
#> $PZ$evidence[[1]]$notes
#> [1] "v0.9.19: + morphological inference path when Al/Fe oxalate missing"
#> 
#> 
#> 
#> $PZ$missing
#> [1] "al_ox_pct" "fe_ox_pct"
#> 
#> $PZ$notes
#> NULL
#> 
#> 
#> $PT
#> $PT$code
#> [1] "PT"
#> 
#> $PT$name
#> [1] "Plinthosols"
#> 
#> $PT$passed
#> [1] NA
#> 
#> $PT$evidence
#> $PT$evidence[[1]]
#> $PT$evidence[[1]]$test_name
#> [1] "plinthosol"
#> 
#> $PT$evidence[[1]]$passed
#> [1] NA
#> 
#> $PT$evidence[[1]]$layers
#> integer(0)
#> 
#> $PT$evidence[[1]]$missing
#> [1] "plinthite_pct"      "ferric_nodules_pct" "pan_type"          
#> [4] "cementation_class" 
#> 
#> $PT$evidence[[1]]$evidence
#> $PT$evidence[[1]]$evidence$plinthic
#> 
#> ── DiagnosticResult: plinthic
#> Status: NA (insufficient data)
#> Missing attributes (1): plinthite_pct
#> Sub-tests:
#> [ NA ] plinthite
#> [ NA ] thickness
#> [ NA ] designation_inference
#> Reference: IUSS Working Group WRB (2022), Chapter 3.1.30, Plinthic horizon (p.
#> 62)
#> 
#> $PT$evidence[[1]]$evidence$pisoplinthic
#> 
#> ── DiagnosticResult: pisoplinthic
#> Status: NA (insufficient data)
#> Missing attributes (1): ferric_nodules_pct
#> Sub-tests:
#> Reference: IUSS Working Group WRB (2022), Chapter 3.1.28, Pisoplinthic horizon
#> (p. 60)
#> 
#> $PT$evidence[[1]]$evidence$petroplinthic
#> 
#> ── DiagnosticResult: petroplinthic
#> Status: NA (insufficient data)
#> Missing attributes (3): pan_type, plinthite_pct, cementation_class
#> Sub-tests:
#> Reference: IUSS Working Group WRB (2022), Chapter 3.1.27, Petroplinthic horizon
#> (p. 59)
#> 
#> $PT$evidence[[1]]$evidence$starts_cm
#>      plinthic  pisoplinthic petroplinthic 
#>             0             0             0 
#> 
#> $PT$evidence[[1]]$evidence$mineral_surface_cm
#> [1] 0
#> 
#> 
#> $PT$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Plinthosols (p. 106)"
#> 
#> $PT$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $PT$missing
#> [1] "plinthite_pct"      "ferric_nodules_pct" "pan_type"          
#> [4] "cementation_class" 
#> 
#> $PT$notes
#> NULL
#> 
#> 
#> $PL
#> $PL$code
#> [1] "PL"
#> 
#> $PL$name
#> [1] "Planosols"
#> 
#> $PL$passed
#> [1] FALSE
#> 
#> $PL$evidence
#> $PL$evidence[[1]]
#> $PL$evidence[[1]]$test_name
#> [1] "planosol"
#> 
#> $PL$evidence[[1]]$passed
#> [1] FALSE
#> 
#> $PL$evidence[[1]]$layers
#> integer(0)
#> 
#> $PL$evidence[[1]]$missing
#> character(0)
#> 
#> $PL$evidence[[1]]$evidence
#> $PL$evidence[[1]]$evidence$abrupt_textural_difference
#> 
#> ── DiagnosticResult: abrupt_textural_difference
#> Status: failed
#> Sub-tests:
#> Reference: IUSS Working Group WRB (2022), Chapter 3.2.1 (p. 72)
#> 
#> $PL$evidence[[1]]$evidence$strict_mode
#> [1] FALSE
#> 
#> 
#> $PL$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Planosols (p. 107)"
#> 
#> $PL$evidence[[1]]$notes
#> [1] "No abrupt textural difference within 75 cm of the mineral soil surface"
#> 
#> 
#> 
#> $PL$missing
#> character(0)
#> 
#> $PL$notes
#> NULL
#> 
#> 
#> $ST
#> $ST$code
#> [1] "ST"
#> 
#> $ST$name
#> [1] "Stagnosols"
#> 
#> $ST$passed
#> [1] FALSE
#> 
#> $ST$evidence
#> $ST$evidence[[1]]
#> $ST$evidence[[1]]$test_name
#> [1] "stagnosol"
#> 
#> $ST$evidence[[1]]$passed
#> [1] FALSE
#> 
#> $ST$evidence[[1]]$layers
#> integer(0)
#> 
#> $ST$evidence[[1]]$missing
#> [1] "redoximorphic_features_pct"
#> 
#> $ST$evidence[[1]]$evidence
#> $ST$evidence[[1]]$evidence$redox_layers
#> $ST$evidence[[1]]$evidence$redox_layers$reductimorphic
#> [1] FALSE FALSE FALSE FALSE
#> 
#> $ST$evidence[[1]]$evidence$redox_layers$oximorphic
#> [1] NA NA NA NA
#> 
#> $ST$evidence[[1]]$evidence$redox_layers$claric
#> [1] FALSE FALSE FALSE FALSE
#> 
#> $ST$evidence[[1]]$evidence$redox_layers$crit1
#> [1] FALSE FALSE FALSE FALSE
#> 
#> $ST$evidence[[1]]$evidence$redox_layers$gleyic
#> [1] NA NA NA NA
#> 
#> $ST$evidence[[1]]$evidence$redox_layers$stagnic
#> [1] NA NA NA NA
#> 
#> $ST$evidence[[1]]$evidence$redox_layers$stagnic_oximorphic
#> [1] NA NA NA NA
#> 
#> $ST$evidence[[1]]$evidence$redox_layers$area
#> [1] 100 100 100 100
#> 
#> $ST$evidence[[1]]$evidence$redox_layers$reducing
#> [1] FALSE FALSE FALSE FALSE
#> 
#> $ST$evidence[[1]]$evidence$redox_layers$groundwater
#> [1] NA
#> 
#> $ST$evidence[[1]]$evidence$redox_layers$rock
#> [1] FALSE FALSE FALSE FALSE
#> 
#> 
#> $ST$evidence[[1]]$evidence$depth_cm
#> from   to 
#>    0   60 
#> 
#> $ST$evidence[[1]]$evidence$area_fraction
#>  low high 
#>    0    1 
#> 
#> $ST$evidence[[1]]$evidence$area_ok
#> [1] NA
#> 
#> $ST$evidence[[1]]$evidence$reducing_ok
#> [1] FALSE
#> 
#> 
#> $ST$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Stagnosols (p. 108)"
#> 
#> $ST$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $ST$missing
#> [1] "redoximorphic_features_pct"
#> 
#> $ST$notes
#> NULL
#> 
#> 
#> $NT
#> $NT$code
#> [1] "NT"
#> 
#> $NT$name
#> [1] "Nitisols"
#> 
#> $NT$passed
#> [1] NA
#> 
#> $NT$evidence
#> $NT$evidence[[1]]
#> $NT$evidence[[1]]$test_name
#> [1] "nitisol"
#> 
#> $NT$evidence[[1]]$passed
#> [1] NA
#> 
#> $NT$evidence[[1]]$layers
#> integer(0)
#> 
#> $NT$evidence[[1]]$missing
#> [1] "structure_type"           "structure_grade"         
#> [3] "structure_type_secondary" "pressure_faces"          
#> [5] "fe_dcb_pct"               "fe_ox_pct"               
#> 
#> $NT$evidence[[1]]$evidence
#> $NT$evidence[[1]]$evidence$nitic_horizon
#> 
#> ── DiagnosticResult: nitic_horizon
#> Status: NA (insufficient data)
#> Missing attributes (6): structure_type, structure_grade,
#> structure_type_secondary, pressure_faces, fe_dcb_pct, fe_ox_pct
#> Sub-tests:
#> Reference: IUSS Working Group WRB (2022), Chapter 3.1.22, Nitic horizon
#> 
#> 
#> $NT$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Nitisols"
#> 
#> $NT$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $NT$missing
#> [1] "structure_type"           "structure_grade"         
#> [3] "structure_type_secondary" "pressure_faces"          
#> [5] "fe_dcb_pct"               "fe_ox_pct"               
#> 
#> $NT$notes
#> NULL
#> 
#> 
#> $FR
#> $FR$code
#> [1] "FR"
#> 
#> $FR$name
#> [1] "Ferralsols"
#> 
#> $FR$passed
#> [1] TRUE
#> 
#> $FR$evidence
#> $FR$evidence[[1]]
#> $FR$evidence[[1]]$test_name
#> [1] "ferralsol"
#> 
#> $FR$evidence[[1]]$passed
#> [1] TRUE
#> 
#> $FR$evidence[[1]]$layers
#> [1] 2 3 4
#> 
#> $FR$evidence[[1]]$missing
#> character(0)
#> 
#> $FR$evidence[[1]]$evidence
#> $FR$evidence[[1]]$evidence$ferralic
#> 
#> ── DiagnosticResult: ferralic
#> Status: PASSED
#> Layers satisfying: 2, 3, 4
#> Sub-tests:
#> [PASS] texture
#> [PASS] cec_per_clay
#> [PASS] thickness
#> Reference: IUSS Working Group WRB (2022), Chapter 3.1.10, Ferralic horizon (p.
#> 44)
#> Notes: v0.3.1: ECEC/clay <= 12 test removed; not part of WRB 2022 ferralic.
#> v0.9.67 engine=soilkey threshold = 16 cmol_c/kg clay.
#> 
#> $FR$evidence[[1]]$evidence$argic
#> 
#> ── DiagnosticResult: argic
#> Status: failed
#> Sub-tests:
#> [fail] clay_increase
#> [fail] illuvial_clay
#> [fail] criterion_2
#> [fail] clay_min
#> [fail] thickness
#> [fail] texture
#> Reference: IUSS Working Group WRB (2022), Chapter 3, Argic horizon
#> Notes: No layer satisfied the clay-increase precondition
#> 
#> $FR$evidence[[1]]$evidence$strict_mode
#> [1] FALSE
#> 
#> 
#> $FR$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Ferralsols (p. 110)"
#> 
#> $FR$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $FR$missing
#> character(0)
#> 
#> $FR$notes
#> NULL
```

Isso é a feature distintiva do soilKey vs. um classificador
“caixa-preta” baseado em LLM: cada classificação carrega o motivo. Se
você discorda do resultado, o trace mostra exatamente em qual
diagnóstico a chave se desviou da sua expectativa.

## 4. Provenance + evidence grade

Cada valor que entra na chave tem uma tag de proveniência:

``` r

meu_pedon$add_measurement(
  horizon_idx = 4,
  attribute   = "clay_pct",
  value       = 60,
  source      = "predicted_spectra",
  confidence  = 0.85,
  notes       = "Vis-NIR PLSR-local, OSSL South-America library",
  overwrite   = TRUE
)
```

O `evidence_grade` do `ClassificationResult` é a regra do “pior
provenance entre os atributos que foram decisivos para a classificação”:

- **A**: tudo `measured` (laboratório)
- **B**: ao menos um `predicted_spectra` decisivo
- **C**: ao menos um `extracted_vlm` decisivo
- **D**: ao menos um `inferred_prior` ou `user_assumed` decisivo

Isso garante que um perfil classificado a partir de chemistry de
laboratório recebe **A**, mas o mesmo perfil com um valor de argila
chave preenchido por VLM ou predição espectral é honestamente **B** ou
**C**, **mesmo que o nome final seja idêntico**.

## 5. Cross-system: o mesmo perfil em três taxonomias

``` r

todos <- classify_all(meu_pedon, on_missing = "silent")
data.frame(
  Sistema = c("WRB 2022", "SiBCS 5ª ed.", "USDA ST 13"),
  Nome    = c(todos$wrb$name, todos$sibcs$name, todos$usda$name)
)
#>        Sistema
#> 1     WRB 2022
#> 2 SiBCS 5ª ed.
#> 3   USDA ST 13
#>                                                                     Nome
#> 1                         Haplic Ferralsol (Clayic, Epic, Eutric, Humic)
#> 2 Latossolos Vermelhos Distróficos típicos, textura argilosa, A moderado
#> 3                                                         Typic Hapludox
```

Para um Latossolo Vermelho típico do Cerrado / Mata Atlântica
brasileira, esperamos:

- WRB: `Geric Ferric Rhodic Chromic Ferralsol (...)`
- SiBCS: `Latossolos Vermelhos Distroficos tipicos`
- USDA: `Rhodic Hapludox`

A correspondência entre os três sistemas (Schad 2023, Annex Table 1)
está documentada no pacote — veja
[`vignette("v03_cross_system_correlation", package = "soilKey")`](https://hugomachadorodrigues.github.io/soilKey/articles/v03_cross_system_correlation.md).

## 6. Onde ir a partir daqui

| Vinheta | Tópico |
|----|----|
| `v02_classify_wrb_end_to_end` | WRB 2022 com nome canônico do Cap. 6 (qualifiers + suplementares + specifiers) |
| `v03_cross_system_correlation` | Correspondência WRB ↔︎ SiBCS ↔︎ USDA |
| `v04_vlm_extraction` | Extração multimodal (PDF, foto) via VLM com schema validation |
| `v05_spatial_spectra_pipeline` | Prior espacial (SoilGrids) + gap-filling com OSSL |
| `v06_wosis_benchmark` | Protocolo de validação contra a base WoSIS (ISRIC) |
| `v07_end_to_end_pipeline` | Pipeline completo (Gemma 4 + espacial + espectral + chave + relatório) |
| `v08_kssl_nasis_multilevel` | Benchmark USDA Soil Taxonomy 13ed em quatro níveis (Order/Suborder/Great Group/Subgroup) na base KSSL+NASIS |

Para a comunidade brasileira: o caminho mais natural é começar aqui
(`v01_getting_started_pt`), depois ir para
`v03_cross_system_correlation` para entender a correspondência SiBCS ↔︎
WRB ↔︎ USDA, e finalmente `v06_wosis_benchmark` se você quiser validar
contra dados reais.

## 7. Onde reportar bugs / sugerir features / pedir ajuda

- **Bugs reproduzíveis**: abra uma [issue no
  GitHub](https://github.com/HugoMachadoRodrigues/soilKey/issues/new?template=bug_report.yml)
  usando o template “🐛 Bug report”.
- **Pedido de feature** (novo diagnóstico, qualifier, loader): use o
  template “💡 Feature request”.
- **Discordância sobre classificação de um perfil real**: use o template
  “🌱 Soil profile classification help” — colamos juntos o trace da
  chave e identificamos exatamente onde soilKey divergiu da sua
  expectativa.
- **Perguntas gerais**: abra uma issue com o template “🌱 Soil profile
  classification help” ou veja a [documentação
  completa](https://hugomachadorodrigues.github.io/soilKey/).

## Referências canônicas

- IUSS Working Group WRB (2022). *World Reference Base for Soil
  Resources*, 4ª ed. International Union of Soil Sciences, Vienna. PDF
  no ISRIC
  (`https://files.isric.org/public/documents/WRB_fourth_edition_2022-12-18.pdf`)
- Soil Survey Staff (2022). *Keys to Soil Taxonomy*, 13ª ed. USDA-NRCS,
  Washington, DC.
- Santos, H.G. *et al.* (2018). *Sistema Brasileiro de Classificação de
  Solos*, 5ª ed. revista e ampliada. Embrapa, Brasília.
