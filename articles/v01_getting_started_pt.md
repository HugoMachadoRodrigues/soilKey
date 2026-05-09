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
#> [1] "Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)"
classify_sibcs(pedon)$name
#> [1] "Latossolos Vermelhos Distroficos tipicos"
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
#> [1] "Chromic Ferralsol (Clayic, Dystric, Ochric, Rubic)"
res_sibcs$name
#> [1] "Latossolos Vermelhos Distroficos tipicos, argilosa, moderado"
res_usda$name
#> [1] "Typic Hapludox"
```

### Atalho: classificar nos três sistemas em uma chamada

``` r

todos <- classify_all(meu_pedon, on_missing = "silent")
todos$summary
#>                                                  wrb
#> 1 Chromic Ferralsol (Clayic, Dystric, Ochric, Rubic)
#>                                                          sibcs           usda
#> 1 Latossolos Vermelhos Distroficos tipicos, argilosa, moderado Typic Hapludox
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
#> [1] "histic_horizon"
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
#> $HS$evidence[[1]]$evidence$contiguous
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$passed
#> [1] FALSE
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$layers
#> integer(0)
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$missing
#> character(0)
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`1`
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`1`$idx
#> [1] 1
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`1`$oc_pct
#> [1] 2
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`1`$threshold
#> [1] 12
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`1`$passed
#> [1] FALSE
#> 
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`2`
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`2`$idx
#> [1] 2
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`2`$oc_pct
#> [1] 0.6
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`2`$threshold
#> [1] 12
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`2`$passed
#> [1] FALSE
#> 
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`3`
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`3`$idx
#> [1] 3
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`3`$oc_pct
#> [1] 0.3
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`3`$threshold
#> [1] 12
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`3`$passed
#> [1] FALSE
#> 
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`4`
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`4`$idx
#> [1] 4
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`4`$oc_pct
#> [1] 0.2
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`4`$threshold
#> [1] 12
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$details$`4`$passed
#> [1] FALSE
#> 
#> 
#> 
#> $HS$evidence[[1]]$evidence$contiguous$organic_carbon$notes
#> [1] NA
#> 
#> 
#> $HS$evidence[[1]]$evidence$contiguous$at_surface
#> $HS$evidence[[1]]$evidence$contiguous$at_surface$passed
#> [1] FALSE
#> 
#> $HS$evidence[[1]]$evidence$contiguous$at_surface$layers
#> integer(0)
#> 
#> $HS$evidence[[1]]$evidence$contiguous$at_surface$missing
#> character(0)
#> 
#> $HS$evidence[[1]]$evidence$contiguous$at_surface$details
#> list()
#> 
#> $HS$evidence[[1]]$evidence$contiguous$at_surface$notes
#> [1] NA
#> 
#> 
#> $HS$evidence[[1]]$evidence$contiguous$thickness
#> $HS$evidence[[1]]$evidence$contiguous$thickness$passed
#> [1] FALSE
#> 
#> $HS$evidence[[1]]$evidence$contiguous$thickness$layers
#> integer(0)
#> 
#> $HS$evidence[[1]]$evidence$contiguous$thickness$missing
#> character(0)
#> 
#> $HS$evidence[[1]]$evidence$contiguous$thickness$details
#> NULL
#> 
#> $HS$evidence[[1]]$evidence$contiguous$thickness$notes
#> [1] NA
#> 
#> 
#> 
#> $HS$evidence[[1]]$evidence$cumulative
#> $HS$evidence[[1]]$evidence$cumulative$cumulative_oc
#> $HS$evidence[[1]]$evidence$cumulative$cumulative_oc$passed
#> [1] FALSE
#> 
#> $HS$evidence[[1]]$evidence$cumulative$cumulative_oc$layers
#> integer(0)
#> 
#> $HS$evidence[[1]]$evidence$cumulative$cumulative_oc$missing
#> character(0)
#> 
#> $HS$evidence[[1]]$evidence$cumulative$cumulative_oc$details
#> $HS$evidence[[1]]$evidence$cumulative$cumulative_oc$details$cumulative_thickness_cm
#> [1] 0
#> 
#> $HS$evidence[[1]]$evidence$cumulative$cumulative_oc$details$min_thickness_cm
#> [1] 40
#> 
#> $HS$evidence[[1]]$evidence$cumulative$cumulative_oc$details$max_depth_cm
#> [1] 80
#> 
#> 
#> $HS$evidence[[1]]$evidence$cumulative$cumulative_oc$notes
#> [1] NA
#> 
#> 
#> 
#> 
#> $HS$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 3, Histic horizon"
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
#> list()
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
#> [1] "cryic_conditions"
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
#> $CR$evidence[[1]]$evidence$permafrost_temp
#> $CR$evidence[[1]]$evidence$permafrost_temp$permafrost
#> $CR$evidence[[1]]$evidence$permafrost_temp$permafrost$passed
#> [1] NA
#> 
#> $CR$evidence[[1]]$evidence$permafrost_temp$permafrost$layers
#> integer(0)
#> 
#> $CR$evidence[[1]]$evidence$permafrost_temp$permafrost$missing
#> [1] "permafrost_temp_C"
#> 
#> $CR$evidence[[1]]$evidence$permafrost_temp$permafrost$details
#> NULL
#> 
#> $CR$evidence[[1]]$evidence$permafrost_temp$permafrost$notes
#> [1] NA
#> 
#> 
#> 
#> $CR$evidence[[1]]$evidence$designation
#> $CR$evidence[[1]]$evidence$designation$frozen_designation
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$passed
#> [1] FALSE
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$layers
#> integer(0)
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$missing
#> character(0)
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`1`
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`1`$idx
#> [1] 1
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`1`$designation
#> [1] "A"
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`1`$pattern
#> [1] "^[A-Z][a-z]*f($|[0-9])|^Cf|perma"
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`1`$passed
#> [1] FALSE
#> 
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`2`
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`2`$idx
#> [1] 2
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`2`$designation
#> [1] "AB"
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`2`$pattern
#> [1] "^[A-Z][a-z]*f($|[0-9])|^Cf|perma"
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`2`$passed
#> [1] FALSE
#> 
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`3`
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`3`$idx
#> [1] 3
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`3`$designation
#> [1] "Bw1"
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`3`$pattern
#> [1] "^[A-Z][a-z]*f($|[0-9])|^Cf|perma"
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`3`$passed
#> [1] FALSE
#> 
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`4`
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`4`$idx
#> [1] 4
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`4`$designation
#> [1] "Bw2"
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`4`$pattern
#> [1] "^[A-Z][a-z]*f($|[0-9])|^Cf|perma"
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$details$`4`$passed
#> [1] FALSE
#> 
#> 
#> 
#> $CR$evidence[[1]]$evidence$designation$frozen_designation$notes
#> [1] NA
#> 
#> 
#> $CR$evidence[[1]]$evidence$designation$within_depth
#> $CR$evidence[[1]]$evidence$designation$within_depth$passed
#> [1] FALSE
#> 
#> $CR$evidence[[1]]$evidence$designation$within_depth$layers
#> integer(0)
#> 
#> $CR$evidence[[1]]$evidence$designation$within_depth$missing
#> character(0)
#> 
#> $CR$evidence[[1]]$evidence$designation$within_depth$details
#> list()
#> 
#> $CR$evidence[[1]]$evidence$designation$within_depth$notes
#> [1] NA
#> 
#> 
#> 
#> 
#> $CR$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 5, Cryosols"
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
#> [1] "^R$|^Cr|^R[a-z]"
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
#> [1] "^R$|^Cr|^R[a-z]"
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
#> [1] "^R$|^Cr|^R[a-z]"
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
#> [1] "^R$|^Cr|^R[a-z]"
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
#> [1] "natric_horizon"
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
#> $SN$evidence[[1]]$evidence$argic
#> 
#> ── DiagnosticResult: argic
#> Status: failed
#> Sub-tests:
#> [fail] clay_increase
#> [fail] thickness
#> [fail] texture
#> [PASS] not_albeluvic
#> Reference: IUSS Working Group WRB (2022), Chapter 3, Argic horizon
#> Notes: No layer satisfied the clay-increase precondition
#> 
#> 
#> $SN$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 3, Natric horizon"
#> 
#> $SN$evidence[[1]]$notes
#> [1] "Profile lacks an argic horizon -- natric inapplicable"
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
#> [1] "slickensides"
#> 
#> $VR$evidence[[1]]$evidence
#> $VR$evidence[[1]]$evidence$vertic_horizon
#> 
#> ── DiagnosticResult: vertic_horizon
#> Status: NA (insufficient data)
#> Missing attributes (1): slickensides
#> Sub-tests:
#> [PASS] clay
#> [ NA ] slickensides
#> [fail] cracks
#> [fail] thickness
#> [ NA ] cole_le_path
#> [ NA ] designation_inference
#> Reference: IUSS Working Group WRB (2022), Chapter 3.1, Vertic horizon; KST 13ed
#> Ch 16 LE alternative
#> 
#> 
#> $VR$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Vertisols (p. 101)"
#> 
#> $VR$evidence[[1]]$notes
#> [1] "Failed/NA because vertic_horizon test did not pass"
#> 
#> 
#> 
#> $VR$missing
#> [1] "slickensides"
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
#> [fail] gleyic_features
#> [ NA ] stagnic_pattern
#> [ NA ] designation_inference
#> Reference: IUSS Working Group WRB (2022), Chapter 3, Gleyic properties
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
#> $GL$evidence[[1]]$evidence$path1_ok
#> [1] FALSE
#> 
#> $GL$evidence[[1]]$evidence$path3_ok
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
#> [1] NA
#> 
#> $AN$evidence
#> $AN$evidence[[1]]
#> $AN$evidence[[1]]$test_name
#> [1] "andosol"
#> 
#> $AN$evidence[[1]]$passed
#> [1] NA
#> 
#> $AN$evidence[[1]]$layers
#> integer(0)
#> 
#> $AN$evidence[[1]]$missing
#> [1] "al_ox_pct"               "fe_ox_pct"              
#> [3] "phosphate_retention_pct" "volcanic_glass_pct"     
#> 
#> $AN$evidence[[1]]$evidence
#> $AN$evidence[[1]]$evidence$andic
#> 
#> ── DiagnosticResult: andic_properties
#> Status: NA (insufficient data)
#> Missing attributes (3): al_ox_pct, fe_ox_pct, phosphate_retention_pct
#> Sub-tests:
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
#> 
#> $AN$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Andosols (p. 104)"
#> 
#> $AN$evidence[[1]]$notes
#> [1] "No andic or vitric layers"
#> 
#> 
#> 
#> $AN$missing
#> [1] "al_ox_pct"               "fe_ox_pct"              
#> [3] "phosphate_retention_pct" "volcanic_glass_pct"     
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
#> [1] "plinthic"
#> 
#> $PT$evidence[[1]]$passed
#> [1] NA
#> 
#> $PT$evidence[[1]]$layers
#> integer(0)
#> 
#> $PT$evidence[[1]]$missing
#> [1] "plinthite_pct"
#> 
#> $PT$evidence[[1]]$evidence
#> $PT$evidence[[1]]$evidence$plinthite
#> $PT$evidence[[1]]$evidence$plinthite$passed
#> [1] NA
#> 
#> $PT$evidence[[1]]$evidence$plinthite$layers
#> integer(0)
#> 
#> $PT$evidence[[1]]$evidence$plinthite$missing
#> [1] "plinthite_pct"
#> 
#> $PT$evidence[[1]]$evidence$plinthite$details
#> list()
#> 
#> $PT$evidence[[1]]$evidence$plinthite$notes
#> [1] NA
#> 
#> 
#> $PT$evidence[[1]]$evidence$thickness
#> $PT$evidence[[1]]$evidence$thickness$passed
#> [1] FALSE
#> 
#> $PT$evidence[[1]]$evidence$thickness$layers
#> integer(0)
#> 
#> $PT$evidence[[1]]$evidence$thickness$missing
#> character(0)
#> 
#> $PT$evidence[[1]]$evidence$thickness$details
#> NULL
#> 
#> $PT$evidence[[1]]$evidence$thickness$notes
#> [1] NA
#> 
#> 
#> $PT$evidence[[1]]$evidence$designation_inference
#> $PT$evidence[[1]]$evidence$designation_inference$passed
#> [1] NA
#> 
#> $PT$evidence[[1]]$evidence$designation_inference$layers
#> integer(0)
#> 
#> $PT$evidence[[1]]$evidence$designation_inference$source
#> [1] "off"
#> 
#> 
#> 
#> $PT$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 3, Plinthic horizon "
#> 
#> $PT$evidence[[1]]$notes
#> [1] NA
#> 
#> 
#> 
#> $PT$missing
#> [1] "plinthite_pct"
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
#> Reference: IUSS Working Group WRB (2022), Chapter 3.2.1
#> 
#> 
#> $PL$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 4, Planosols (p. 107)"
#> 
#> $PL$evidence[[1]]$notes
#> [1] "No abrupt textural difference within profile"
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
#> [1] NA
#> 
#> $ST$evidence
#> $ST$evidence[[1]]
#> $ST$evidence[[1]]$test_name
#> [1] "stagnic_properties"
#> 
#> $ST$evidence[[1]]$passed
#> [1] NA
#> 
#> $ST$evidence[[1]]$layers
#> integer(0)
#> 
#> $ST$evidence[[1]]$missing
#> [1] "redoximorphic_features_pct"
#> 
#> $ST$evidence[[1]]$evidence
#> $ST$evidence[[1]]$evidence$stagnic_pattern
#> $ST$evidence[[1]]$evidence$stagnic_pattern$passed
#> [1] NA
#> 
#> $ST$evidence[[1]]$evidence$stagnic_pattern$layers
#> integer(0)
#> 
#> $ST$evidence[[1]]$evidence$stagnic_pattern$missing
#> [1] "redoximorphic_features_pct"
#> 
#> $ST$evidence[[1]]$evidence$stagnic_pattern$details
#> NULL
#> 
#> $ST$evidence[[1]]$evidence$stagnic_pattern$notes
#> [1] NA
#> 
#> 
#> 
#> $ST$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 3, Stagnic properties"
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
#> [1] FALSE
#> 
#> $NT$evidence
#> $NT$evidence[[1]]
#> $NT$evidence[[1]]$test_name
#> [1] "nitic_horizon"
#> 
#> $NT$evidence[[1]]$passed
#> [1] FALSE
#> 
#> $NT$evidence[[1]]$layers
#> integer(0)
#> 
#> $NT$evidence[[1]]$missing
#> character(0)
#> 
#> $NT$evidence[[1]]$evidence
#> $NT$evidence[[1]]$evidence$ferralic
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
#> 
#> $NT$evidence[[1]]$reference
#> [1] "IUSS Working Group WRB (2022), Chapter 3, Nitic horizon"
#> 
#> $NT$evidence[[1]]$notes
#> [1] "Excluded -- profile has a ferralic horizon (Ferralsol path)"
#> 
#> 
#> 
#> $NT$missing
#> character(0)
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
#> [fail] thickness
#> [fail] texture
#> [PASS] not_albeluvic
#> Reference: IUSS Working Group WRB (2022), Chapter 3, Argic horizon
#> Notes: No layer satisfied the clay-increase precondition
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
#>        Sistema                                                         Nome
#> 1     WRB 2022           Chromic Ferralsol (Clayic, Dystric, Ochric, Rubic)
#> 2 SiBCS 5ª ed. Latossolos Vermelhos Distroficos tipicos, argilosa, moderado
#> 3   USDA ST 13                                               Typic Hapludox
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
- **Perguntas gerais**: [GitHub
  Discussions](https://github.com/HugoMachadoRodrigues/soilKey/discussions).

## Referências canônicas

- IUSS Working Group WRB (2022). *World Reference Base for Soil
  Resources*, 4ª ed. International Union of Soil Sciences, Vienna.
  [PDF](https://openknowledge.fao.org/server/api/core/bitstreams/bcdecec7-f45f-4dc5-beb1-97022d29fab4/content)
- Soil Survey Staff (2022). *Keys to Soil Taxonomy*, 13ª ed. USDA-NRCS,
  Washington, DC.
- Santos, H.G. *et al.* (2018). *Sistema Brasileiro de Classificação de
  Solos*, 5ª ed. revista e ampliada. Embrapa, Brasília.
