# soilKey — Arquitetura do pacote

**Um pacote R para classificação automatizada de perfis de solo segundo
WRB 2022 (4ª ed.) e SiBCS 5ª ed., com extração multimodal, priors
espaciais (SoilGrids) e predição de atributos via espectroscopia
(OSSL).**

> Migrado de `pedokey_architecture_1.md` (rascunho original) em
> 2026-04-25 após decisão sobre o nome do pacote. Este é o documento
> vivo da arquitetura — atualize-o conforme decisões mudarem ao longo da
> implementação.

------------------------------------------------------------------------

## 1. Nome e identidade

- **Nome:** `soilKey`. Decidido em 2026-04-25, após avaliação de
  alternativas (`pedokey`, `pedorules`, `autopedo`).
- **Licença:** MIT (ver `LICENSE` / `LICENSE.md`). Escolha reconciliada
  com `DESCRIPTION` em 2026-04-30. MIT (em vez de GPL-3, considerado nos
  rascunhos iniciais) maximiza interoperabilidade com pipelines
  acadêmicos e governamentais — sem viralidade copyleft que limite uso
  embarcado em ferramentas internas da Embrapa, FAO ou IUSS-WRB.
- **Público-alvo:** pedólogos, agrônomos, consultores ambientais,
  instituições de pesquisa. Não requer expertise em ML.

------------------------------------------------------------------------

## 2. Princípios de design

1.  **Separação estrita de responsabilidades.** Extração (VLM),
    inferência de atributos (espectros, prior), e classificação (chave)
    são etapas independentes. **A chave nunca é delegada a um LLM.** LLM
    serve apenas para transformar dados não-estruturados em dados
    estruturados validados por schema.
2.  **Rastreabilidade total.** Cada classe atribuída carrega o traço
    completo: quais diagnósticos foram testados, quais atributos foram
    usados, a origem de cada atributo (medido / predito / extraído /
    assumido), e a cadeia de decisão que levou ao resultado.
3.  **Declarativo onde possível.** As regras da chave ficam em YAML
    versionado, não código espalhado. Isso permite auditoria por
    pedólogos que não programam, e torna atualizações (ex.: WRB 5ª
    ed. futura) uma questão de editar YAML + ajustar funções de
    diagnóstico.
4.  **Testes como documentação científica.** Cada diagnóstico tem
    fixtures baseadas em perfis canônicos publicados (exemplos didáticos
    da WRB, RADAMBRASIL, Soil Atlas of Europe, WoSIS).
5.  **Interoperabilidade com o ecossistema existente.** `PedonRecord`
    converte de/para
    [`aqp::SoilProfileCollection`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-class.html);
    espectros seguem convenções do `prospectr`/`resemble`/`ossl`;
    espacial segue `terra`/`sf`.
6.  **Graus de evidência explícitos.** O resultado final inclui um
    `evidence_grade` (A/B/C/D) baseado na proveniência dos atributos
    críticos. Isso é mais importante do que uma “confiança” numérica
    opaca.

------------------------------------------------------------------------

## 3. Dependências

| Domínio        | Pacotes                                |
|----------------|----------------------------------------|
| Pedologia      | aqp, SoilTaxonomy, mpspline2           |
| Espacial       | terra, sf, gdalcubes                   |
| Espectros      | prospectr, resemble, ossl              |
| VLM/LLM        | ellmer, httr2, jsonlite, jsonvalidate  |
| E/S documentos | pdftools, magick, tesseract (opcional) |
| Core           | R6, cli, rlang, yaml, data.table       |
| Testes/docs    | testthat, roxygen2, rmarkdown, knitr   |

------------------------------------------------------------------------

## 4. Estrutura do pacote

    soilKey/
    ├── DESCRIPTION
    ├── NAMESPACE
    ├── R/
    │   ├── soilKey-package.R
    │   ├── class-PedonRecord.R
    │   ├── class-DiagnosticResult.R
    │   ├── class-ClassificationResult.R
    │   │
    │   │   # ───── Módulo 1: Chaves determinísticas ─────
    │   ├── diagnostics-horizons-wrb.R      # argic(), ferralic(), mollic(), nitic()...
    │   ├── diagnostics-properties-wrb.R    # stagnic(), gleyic(), andic(), vertic()...
    │   ├── diagnostics-materials-wrb.R     # fluvic(), tephric(), sulfidic(), organic()...
    │   ├── diagnostics-horizons-sibcs.R    # B_latossolico(), B_textural(), B_nitico()...
    │   ├── key-wrb2022.R                   # classify_wrb2022()
    │   ├── key-sibcs.R                     # classify_sibcs()
    │   ├── qualifiers-wrb2022.R
    │   ├── subgroups-sibcs.R
    │   ├── rule-engine.R                   # consome YAML
    │   │
    │   │   # ───── Módulo 2: VLM ─────
    │   ├── vlm-extract.R
    │   ├── vlm-providers.R                 # adapters via ellmer
    │   ├── vlm-schemas.R
    │   ├── vlm-prompts.R
    │   ├── vlm-validate.R
    │   │
    │   │   # ───── Módulo 3: Prior espacial ─────
    │   ├── spatial-prior.R
    │   ├── spatial-soilgrids.R
    │   ├── spatial-embrapa.R
    │   ├── spatial-combine.R
    │   │
    │   │   # ───── Módulo 4: Espectroscopia ─────
    │   ├── spectra-ossl.R
    │   ├── spectra-preprocess.R
    │   ├── spectra-predict-mbl.R
    │   ├── spectra-predict-pretrained.R
    │   ├── spectra-fill-attributes.R
    │   │
    │   │   # ───── Orquestração e relatórios ─────
    │   ├── classify.R                      # wrapper de alto nível
    │   ├── report-html.R
    │   ├── report-pdf.R
    │   └── utils-*.R
    │
    ├── inst/
    │   ├── rules/
    │   │   ├── wrb2022/
    │   │   │   ├── key.yaml                # ordem dos 32 RSGs
    │   │   │   ├── diagnostics.yaml        # definições dos diagnósticos
    │   │   │   └── qualifiers.yaml         # 202 qualifiers por RSG
    │   │   └── sibcs5/
    │   │       ├── key.yaml                # ordem das 13 ordens
    │   │       ├── diagnostics.yaml
    │   │       └── subgroups.yaml
    │   ├── prompts/
    │   │   ├── extract_horizons.md
    │   │   ├── extract_munsell_from_photo.md
    │   │   ├── extract_structure.md
    │   │   └── extract_site_metadata.md
    │   ├── schemas/
    │   │   ├── horizon.json
    │   │   ├── site.json
    │   │   └── pedon.json
    │   └── examples/
    │       ├── profiles_wrb/               # 32 perfis canônicos, um por RSG
    │       └── profiles_sibcs/             # 13 perfis canônicos, um por ordem
    │
    ├── data/                               # tabelas internas (.rda)
    │   ├── wrb_rsg_codes.rda
    │   ├── wrb_qualifier_codes.rda
    │   ├── sibcs_classes.rda
    │   └── munsell_lookup.rda
    │
    ├── data-raw/                           # scripts reproduzíveis que geram data/
    │
    ├── tests/testthat/
    │   ├── test-diagnostics-wrb-argic.R
    │   ├── test-diagnostics-wrb-ferralic.R
    │   ├── test-diagnostics-wrb-mollic.R
    │   ├── ...                             # um teste por diagnóstico
    │   ├── test-key-wrb-ferralsol.R        # end-to-end por RSG
    │   ├── ...
    │   ├── test-key-sibcs-latossolo.R
    │   ├── ...
    │   ├── test-vlm-schema.R
    │   ├── test-spatial-soilgrids.R
    │   └── test-spectra-ossl.R
    │
    ├── vignettes/
    │   ├── 01-getting-started.Rmd
    │   ├── 02-wrb-classification.Rmd
    │   ├── 03-sibcs-classification.Rmd
    │   ├── 04-extraction-from-reports.Rmd
    │   ├── 05-spatial-prior.Rmd
    │   ├── 06-spectra-ossl.Rmd
    │   ├── 07-cross-system-correlation.Rmd
    │   ├── 08-custom-rules.Rmd
    │   └── 09-wosis-benchmark.Rmd
    │
    └── README.md

------------------------------------------------------------------------

## 5. Modelo de dados

### 5.1 `PedonRecord` (classe R6)

A estrutura central carrega tudo sobre um perfil — site, horizontes,
espectros, imagens, documentos, e a proveniência de cada atributo.

``` r

PedonRecord <- R6::R6Class("PedonRecord",
  public = list(

    # ---- dados ----
    site = NULL,        # list:
                        #   lat, lon, crs (default 4326), date, country,
                        #   elevation_m, slope_pct, aspect_deg,
                        #   landform, parent_material,
                        #   land_use, vegetation, drainage_class

    horizons = NULL,    # data.table com colunas (schema fixo):
                        #   top_cm, bottom_cm, designation,
                        #   boundary_distinctness, boundary_topography,
                        #   munsell_hue_moist, munsell_value_moist, munsell_chroma_moist,
                        #   munsell_hue_dry, munsell_value_dry, munsell_chroma_dry,
                        #   structure_grade, structure_size, structure_type,
                        #   consistence_moist, consistence_wet, clay_films,
                        #   coarse_fragments_pct,
                        #   clay_pct, silt_pct, sand_pct,
                        #   ph_h2o, ph_kcl, ph_cacl2,
                        #   oc_pct, n_total_pct,
                        #   cec_cmol, ecec_cmol, bs_pct, al_sat_pct,
                        #   ca_cmol, mg_cmol, k_cmol, na_cmol, al_cmol,
                        #   caco3_pct, caso4_pct,
                        #   fe_dcb_pct, fe_ox_pct, al_ox_pct, si_ox_pct,
                        #   bulk_density_g_cm3, water_content_33kpa, water_content_1500kpa

    spectra = NULL,     # list:
                        #   vnir = matrix (rows = horizons, cols = 350:2500 nm),
                        #   mir  = matrix (rows = horizons, cols = 4000:600 cm^-1),
                        #   metadata = list(instrument, date, preprocess_applied)

    images = NULL,      # list of:
                        #   list(type="profile_wall", path=..., reference_card=TRUE, annotations=...)

    documents = NULL,   # list of:
                        #   list(type="field_sheet"|"lab_report"|"survey_report", path=..., text=...)

    # ---- proveniência ----
    provenance = NULL,  # data.table: horizon_idx, attribute, source, confidence, notes
                        # source ∈ {measured, extracted_vlm, predicted_spectra,
                        #           inferred_prior, user_assumed}

    # ---- métodos ----
    initialize = function(site = NULL, horizons = NULL,
                          spectra = NULL, images = NULL, documents = NULL) { ... },
    validate   = function() { ... },   # checa sanidade: top<bottom, %areia+silte+argila≈100,
                                        # pH em range plausível, CEC >= soma das bases, etc.
    to_aqp     = function() { ... },   # coerce para SoilProfileCollection
    from_aqp   = function(spc) { ... },
    add_measurement = function(horizon_idx, attribute, value, source, confidence = 1.0) { ... },
    summary    = function() { ... },
    print      = function() { ... }
  )
)
```

### 5.2 `DiagnosticResult`

Retornado por cada função de diagnóstico. Nunca é apenas `TRUE`/`FALSE`
— sempre carrega evidência.

``` r

DiagnosticResult <- R6::R6Class("DiagnosticResult",
  public = list(
    name      = NULL,   # "argic"
    passed    = NULL,   # TRUE/FALSE/NA (NA = atributos faltantes impediram teste)
    layers    = NULL,   # quais horizontes (indices) satisfazem
    evidence  = NULL,   # list com sub-testes e seus resultados
    missing   = NULL,   # quais atributos seriam necessários e faltam
    reference = NULL,   # citação literária
    notes     = NULL
  )
)
```

### 5.3 `ClassificationResult`

Saída final de
[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
ou
[`classify_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md).

``` r

ClassificationResult <- R6::R6Class("ClassificationResult",
  public = list(
    system         = NULL,   # "WRB 2022" | "SiBCS 5"
    name           = NULL,   # "Rhodic Ferralsol (Clayic, Humic, Dystric)"
    rsg_or_order   = NULL,   # "Ferralsols"
    qualifiers     = NULL,   # ordered list principal + supplementary
    trace          = NULL,   # lista de DiagnosticResult em ordem de teste
    ambiguities    = NULL,   # RSGs alternativas que quase passaram (com delta)
    missing_data   = NULL,   # atributos cuja medição refinaria o resultado
    evidence_grade = NULL,   # A/B/C/D
    prior_check    = NULL,   # consistência com prior espacial
    warnings       = NULL,

    print   = function() { ... },
    summary = function() { ... },
    report  = function(file, format = c("html","pdf","md")) { ... }
  )
)
```

**Graus de evidência:**

| Grau | Critério |
|----|----|
| A | Todos atributos críticos vêm de laboratório, diagnósticos passaram sem ambiguidade. |
| B | Atributos críticos medidos; atributos secundários preditos por espectros com PI95% estreito. |
| C | Alguns atributos críticos preditos por espectros; prior espacial consistente com resultado. |
| D | Múltiplos atributos críticos extraídos por VLM ou inferidos de prior; resultado tentativo. |

------------------------------------------------------------------------

## 6. Módulo 1 — Chaves determinísticas

### 6.1 Diagnósticos como funções puras

Cada diagnóstico é uma função que recebe um `PedonRecord` e retorna um
`DiagnosticResult`. Exemplo:

``` r

#' Argic horizon (WRB 2022)
#'
#' Tests whether any horizon or combination meets the argic horizon criteria
#' per Chapter 3 of WRB 2022 (IUSS Working Group WRB, 2022).
#'
#' @param pedon A PedonRecord.
#' @param min_thickness Minimum thickness in cm (default 7.5).
#' @return A DiagnosticResult.
#' @references IUSS Working Group WRB (2022), Chapter 3, Argic horizon.
#' @export
argic <- function(pedon, min_thickness = 7.5) {

  h <- pedon$horizons
  tests <- list()

  # Teste 1: incremento textural qualificado (uma de três condições)
  tests$clay_increase <- test_clay_increase_argic(h)

  # Teste 2: espessura mínima do horizonte argic candidato
  tests$thickness <- test_minimum_thickness(h, min_thickness,
                                             candidate_layers = tests$clay_increase$layers)

  # Teste 3: textura mínima (não pode ser areia)
  tests$texture <- test_texture_argic(h, candidate_layers = tests$clay_increase$layers)

  # Teste 4: exclusões (albeluvic glossic -> Retisol em vez de Luvisol/Acrisol)
  tests$not_albeluvic <- test_not_albeluvic(h)

  # Atributos necessários que faltam
  missing <- attributes_missing_for_tests(tests, h)

  all_ok <- all(vapply(tests, function(t) isTRUE(t$passed), logical(1)))

  DiagnosticResult$new(
    name      = "argic",
    passed    = if (length(missing) > 0) NA else all_ok,
    layers    = if (isTRUE(all_ok)) tests$clay_increase$layers else integer(0),
    evidence  = tests,
    missing   = missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Argic horizon"
  )
}
```

Sub-testes (`test_clay_increase_argic` etc.) ficam em
`R/utils-diagnostic-tests.R` e também são exportados — pedólogos podem
chamá-los isoladamente para auditoria.

### 6.2 A chave WRB 2022 em YAML

A ordem de teste dos 32 RSGs é explícita em
`inst/rules/wrb2022/key.yaml`:

``` yaml
version: "WRB 2022 (4th edition)"
source: "IUSS Working Group WRB (2022). World Reference Base for Soil Resources.
        International soil classification system for naming soils and creating
        legends for soil maps. 4th edition. IUSS, Vienna."
checksum: "sha256:..."  # verificável contra a edição

# Ordem canônica do chaveamento; excludes_previous é implícito (se não
# satisfaz aqui, passa para o próximo RSG).

rsgs:
  - code: HS
    name: Histosols
    tests:
      any_of:
        - organic_material: {thickness_gte: 10, from_surface: true}
        - organic_material: {thickness_gte: 40, within_top_cm: 80, cumulative: true}

  - code: AT
    name: Anthrosols
    tests:
      any_of:
        - hortic_horizon: {}
        - irragric_horizon: {}
        - plaggic_horizon: {}
        - pretic_horizon: {}
        - terric_horizon: {}

  - code: TC
    name: Technosols
    tests:
      any_of:
        - artefacts: {volume_pct_gte: 20, within_top_cm: 100}
        - geomembrane: {within_top_cm: 100}
        - technic_hard_material: {depth_lte: 5, coverage_pct_gte: 95}

  - code: CR
    name: Cryosols
    tests:
      all_of:
        - cryic_conditions: {within_top_cm: 100}

  - code: LP
    name: Leptosols
    tests:
      any_of:
        - continuous_rock: {within_cm: 25}
        - coarse_fragments: {pct_gte: 90, within_top_cm: 75}

  # ... (continua pelos 32 RSGs em ordem: SN, VR, SC, GL, AN, PZ, PT, NT, FR,
  #      PL, ST, CH, KS, PH, UM, DU, GY, CL, RT, AC, LX, AL, LV, CM, AR, FL, RG)

  - code: RG
    name: Regosols
    tests:
      default: true   # catch-all — todos os solos minerais que não keyaram antes
```

### 6.3 O rule engine

`R/rule-engine.R` lê o YAML, resolve as referências a funções de
diagnóstico, e executa:

``` r

run_wrb_key <- function(pedon, rules = NULL) {
  rules <- rules %||% load_rules("wrb2022")
  trace <- list()

  for (rsg in rules$rsgs) {
    result <- evaluate_rsg_tests(pedon, rsg$tests)
    trace[[rsg$code]] <- list(
      code = rsg$code, name = rsg$name,
      passed = result$passed,
      evidence = result$evidence,
      missing = result$missing
    )
    if (isTRUE(result$passed)) {
      return(list(assigned = rsg, trace = trace))
    }
    # NA (atributos insuficientes) — anota mas continua; o resultado final
    # aponta a ambiguidade.
  }
  # default: Regosols (catch-all)
  list(assigned = rules$rsgs[[length(rules$rsgs)]], trace = trace)
}

evaluate_rsg_tests <- function(pedon, tests) {
  if (!is.null(tests$all_of)) {
    results <- lapply(tests$all_of, run_single_test, pedon = pedon)
    passed <- all(vapply(results, `[[`, logical(1), "passed"))
  } else if (!is.null(tests$any_of)) {
    results <- lapply(tests$any_of, run_single_test, pedon = pedon)
    passed <- any(vapply(results, `[[`, logical(1), "passed"))
  } else if (isTRUE(tests$default)) {
    return(list(passed = TRUE, evidence = list(), missing = character(0)))
  }
  missing <- unique(unlist(lapply(results, `[[`, "missing")))
  list(passed = passed, evidence = results, missing = missing)
}
```

### 6.4 Qualifiers (WRB)

Após o RSG ser atribuído, `qualifiers-wrb2022.R` roda os ~202
qualifiers, filtrando aqueles disponíveis para aquele RSG (Chapter 4 da
WRB 2022). Cada qualifier é também uma função pura com mesmo padrão.
Principal qualifiers são ordenados conforme a especificação;
supplementary são listados em ordem alfabética.

Output final do nome: `"Rhodic Ferralsol (Clayic, Humic, Dystric)"`.

#### 6.4.1 Inventário canônico para v0.9 (WRB 2022 Ch 5, pp 127-156)

A tabela completa de códigos está em **Ch 6 (pp 152-153)** e contém 4
grupos:

| Grupo | Conteúdo | n |
|----|----|----|
| **Reference Soil Groups** | 32 RSGs com seus códigos de 2 letras (HS, AT, …, RG) | 32 |
| **Qualifiers** | Qualifiers principais e suplementares + sub-qualifiers (Hyper-, Hypo-, Proto-, Neo-, Orto-, Endoab-, Subaq-, etc.) | ~190 |
| **Specifiers** | Modificadores de profundidade/posição: `..a` Ano- (≤50 cm), `..n` Endo- (50-100 cm), `..p` Epi-, `..k` Kato- (parte inferior), `..e` Panto- (todo o perfil), `..y` Poly-, `..d` Bathy- (\>100 cm), `..s` Supra- (acima de barreira), `..b` Thapto- (em soil sepultado), `..m` Amphi- | 10 |
| **Combinações Novic** | Material novo sobre solo sepultado, e.g. `nva` Aeoli-Novic, `nvf` Fluvi-Novic | 5+ |

**Estratégia de implementação para v0.9:**

A maioria dos qualifiers se reduz a 4 padrões estruturais:

1.  **“Having X horizon at depth ≤ Y cm”** (\>= 60% dos qualifiers):
    `Calcic`, `Petrocalcic`, `Argic` (já indireto via RSG), `Spodic`,
    `Histic`, `Plinthic`, `Andic`, etc. Implementação: thin wrapper
    `qualifier_has_horizon(horizon_diagnostic_fn, max_top_cm)`.
2.  **“Having layer with Z properties at depth ≤ Y cm”** (~25%):
    `Stagnic`, `Gleyic`, `Vitric`, `Vertic`, `Sodic`, `Reducing`, etc.
    Wrapper análogo com `property_diagnostic_fn`.
3.  **“Material gating”** (~10%): `Calcaric`, `Dolomitic`, `Tephric`,
    `Fluvic`, `Sulfidic` etc. Wrapper sobre `material_diagnostic_fn`.
4.  **“Composição química/física específica”** (~5%): `Dystric`,
    `Eutric`, `Hyperdystric`, `Hypereutric` (BS thresholds), `Magnesic`,
    `Hyperalic`, `Geric`, `Pellic`, `Rubic`, `Xanthic` (Munsell),
    `Vermic` (worm features), `Skeletic` (coarse fragments), etc.
    Implementação caso a caso.

Sub-qualifiers (`Hyper-`, `Hypo-`, `Proto-`, `Neo-`) são variantes
paramétricas do qualifier base — são naturais como sufixos no mesmo
arquivo YAML, com thresholds modificados.

**Specifiers** se aplicam compositorialmente sobre qualifiers
(e.g. `Epileptic` = leptic na faixa 0-50 cm; `Endoleptic` = leptic na
faixa 50-100 cm) — implementação via decorator.

**YAML schema proposto** para `inst/rules/wrb2022/qualifiers.yaml`:

``` yaml
qualifiers:
  - code: ap
    name: Abruptic
    type: principal
    applicable_rsgs: [PL, ST, LV, AC, LX, AL, RT]  # do Ch 4
    test:
      diagnostic: abrupt_textural_difference
      max_top_cm: 100
    references: "Ch 5, p 127"
    sub_qualifiers:
      - code: go
        name: Geoabruptic
        condition: "abrupt_textural_difference NOT associated with argic/natric/spodic upper limit"
```

Esse formato torna a expansão dos 200+ qualifiers uma questão de gerar
YAML programaticamente a partir do texto canônico (que já tenho
indexado).

**Riscos do v0.9:** - Vários qualifiers requerem diagnósticos de Ch 3
ainda não implementados (chernic, hortic, irragric, plaggic, pretic,
terric, anthraquic, hydragric, hortic, panpaic, etc.). v0.9 deve incluir
esses ~12 diagnósticos faltantes antes da resolução de qualifiers. - A
ordem dos principal qualifiers no nome do solo importa: a WRB lista os
qualifiers principais “according to the list from top to bottom” do Ch 4
— ou seja, a ordem listada por RSG é canônica e precisa ser preservada
no YAML.

### 6.5 SiBCS (chave paralela, 13 ordens)

Implementação estrutural idêntica, com diagnósticos específicos. Exemplo
— Latossolo:

``` r

#' Horizonte B latossólico (SiBCS)
#'
#' @references SiBCS 5ª ed. (Embrapa, 2018), Capítulo ...
#' @export
B_latossolico <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  tests$minimum_thickness <- test_thickness(h, min_cm = 50)
  tests$ki_lte_22 <- test_ki(h, max = 2.2)
  tests$kr_lte_17 <- test_kr(h, max = 1.7)
  tests$cec_by_clay <- test_cec_by_clay(h, max_cmol_per_kg_clay = 17)
  tests$no_textural_gradient <- test_no_textural_gradient_b_latossolico(h)
  tests$no_clay_films <- test_no_clay_films(h)
  # ...
  DiagnosticResult$new(
    name = "B latossólico",
    passed = all(vapply(tests, `[[`, logical(1), "passed")),
    layers = tests$minimum_thickness$layers,
    evidence = tests,
    missing = attributes_missing_for_tests(tests, h),
    reference = "SiBCS 5ª ed. (Embrapa)"
  )
}
```

A chave SiBCS (`inst/rules/sibcs5/key.yaml`) testa na ordem: Latossolos,
Argissolos, Neossolos, Plintossolos, Chernossolos, Vertissolos,
Gleissolos, Nitossolos, Cambissolos, Espodossolos, Luvissolos,
Organossolos, Planossolos (com ajustes conforme a 5ª edição).

A vignette **07-cross-system-correlation.Rmd** classifica o mesmo perfil
em ambos os sistemas e documenta padrões conhecidos de correspondência
(Latossolo ↔︎ Ferralsol, Argissolo ↔︎ Acrisol/Lixisol/Alisol/Luvisol
dependendo da CTC e V%, Nitossolo Vermelho eutrófico ↔︎ Rhodic Nitisol,
etc.).

------------------------------------------------------------------------

## 7. Módulo 2 — Extração multimodal (VLM)

### 7.1 Princípio operacional

O VLM **nunca classifica**. Ele faz três coisas:

1.  Transforma descrições textuais (PDFs de levantamento, fichas de
    campo, relatórios) em dados estruturados validados por JSON schema.
2.  Extrai cor de Munsell de fotos quando houver cartão de referência.
3.  Aproxima limites de horizontes em fotos de perfil (com baixa
    confiança explicitamente marcada).

Todo valor extraído recebe (a) tag de proveniência `extracted_vlm`, (b)
score de confiança fornecido pelo modelo, (c) citação textual da fonte
quando aplicável.

### 7.2 Abstração de providers via `ellmer`

``` r

vlm_provider <- function(name = c("anthropic", "openai", "google", "ollama"),
                         model = NULL, ...) {
  name <- match.arg(name)
  model <- model %||% default_model(name)
  switch(name,
    anthropic = ellmer::chat_anthropic(model = model, ...),
    openai    = ellmer::chat_openai(model = model, ...),
    google    = ellmer::chat_gemini(model = model, ...),
    ollama    = ellmer::chat_ollama(model = model, ...)
  )
}
```

Isso te dá a opção importante: **rodar tudo local via Ollama** (Gemma 3,
Qwen2-VL, LLaVA) para preservar independência institucional e dados
sensíveis. Particularmente relevante dado o contexto de documentação do
Rosa do Deserto — toda a extração pode rodar em infraestrutura que você
controla.

### 7.3 Tarefas de extração

``` r

extract_horizons_from_pdf(pedon, pdf_path,  provider)
extract_munsell_from_photo(pedon, image,    provider)
extract_structure_from_photo(pedon, image,  provider)
extract_site_from_fieldsheet(pedon, image,  provider)
```

Cada função: 1. Lê documento/imagem. 2. Carrega template de prompt
(`inst/prompts/`) e schema JSON (`inst/schemas/`). 3. Chama o provider
com `type = "json"` forçado. 4. Valida retorno com `jsonvalidate`. 5. Se
falhar validação, reexecuta anexando o erro ao prompt (até 3
tentativas). 6. Reconcilia com dados existentes no `PedonRecord`: nunca
sobrescreve valores `measured`; VLM só preenche `NA` ou campos com
proveniência de menor autoridade, a menos que `overwrite = TRUE`.

### 7.4 Exemplo de prompt (extração de horizontes)

``` markdown
# inst/prompts/extract_horizons.md

Você é um pedólogo extraindo dados estruturados de um documento de descrição
de perfil de solo.

Extraia cada horizonte do perfil com os seguintes campos. Para cada campo,
também forneça:
- um score de confiança (0.0 a 1.0),
- uma citação textual curta (<= 20 palavras) do documento suportando a
  extração.

Se um campo não estiver presente, retorne null. NÃO INFIRA OU ADIVINHE.

Retorne JSON válido conforme este schema:
{schema_json}

Documento-fonte:
---
{document_text}
---
```

### 7.5 Schema de horizonte (trecho)

``` json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "horizons": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "top_cm": {"type": ["number", "null"]},
          "bottom_cm": {"type": ["number", "null"]},
          "designation": {"type": ["string", "null"], "pattern": "^[A-Z][a-zA-Z0-9]*$"},
          "munsell_moist": {
            "type": ["object", "null"],
            "properties": {
              "hue": {"type": "string"},
              "value": {"type": "integer", "minimum": 2, "maximum": 8},
              "chroma": {"type": "integer", "minimum": 1, "maximum": 8},
              "confidence": {"type": "number", "minimum": 0, "maximum": 1},
              "source_quote": {"type": "string"}
            }
          },
          "clay_pct": {
            "type": ["object", "null"],
            "properties": {
              "value": {"type": "number", "minimum": 0, "maximum": 100},
              "confidence": {"type": "number"},
              "source_quote": {"type": "string"}
            }
          }
        },
        "required": ["top_cm", "bottom_cm"]
      }
    }
  },
  "required": ["horizons"]
}
```

### 7.6 Foto de perfil: limites realistas

O paper SoilNet (Feldmann et al., 2025) mostra que VLMs genéricos não
fazem classificação de horizontes bem — precisa de modelo especializado
com segmentação sequencial e embeddings hierárquicos. Posição
conservadora para `soilKey`:

- **Munsell a partir de foto com cartão de referência**: testável,
  aceito com `confidence` moderada.
- **Limites de horizontes a partir de foto**: extraídos como *proposta*,
  sempre com `confidence <= 0.4`, marcados para revisão humana.
- **Atributos quantitativos (clay %, CEC, etc.) a partir de foto**:
  **nunca**. Esses vêm de laboratório ou do Módulo 4.

------------------------------------------------------------------------

## 8. Módulo 3 — Prior espacial

### 8.1 SoilGrids via WCS

ISRIC publica SoilGrids como Cloud-Optimized GeoTIFFs acessíveis via
WCS/WMS. Para WRB, o SoilGrids fornece diretamente P(RSG \| pixel) para
as principais RSGs.

``` r

spatial_prior_soilgrids <- function(pedon,
                                     system  = c("wrb2022", "usda"),
                                     buffer_m = 250,
                                     source_url = soilgrids_wcs_url()) {

  coord <- sf::st_sfc(sf::st_point(c(pedon$site$lon, pedon$site$lat)), crs = 4326)
  coord_utm <- sf::st_transform(coord, utm_crs_for(pedon$site))
  buf <- sf::st_buffer(coord_utm, dist = buffer_m)

  rst <- terra::rast(source_url)
  vals <- terra::extract(rst, terra::vect(buf), fun = mean, na.rm = TRUE)

  # Normaliza para probabilidade sobre as RSGs
  probs <- normalize_rsg_probs(vals, system = system)
  data.table::data.table(rsg_code = names(probs), probability = probs)
}
```

### 8.2 Uso do prior

O prior não entra na chave determinística (que continua sendo a
autoridade). Ele é usado em três situações:

1.  **Desempate em ambiguidade**: quando dois RSGs quase passam com
    atributos faltantes, o prior ordena candidatos.
2.  **Sanity check**: se a classificação final tem P_prior \< 0.01 na
    região, emite `warning` no `ClassificationResult`. Ex.: classificar
    como Cryosol em Gainesville gera alerta imediato.
3.  **Posterior bayesiana opcional**: função separada
    [`posterior_classify()`](https://hugomachadorodrigues.github.io/soilKey/reference/posterior_classify.md)
    para quando o usuário explicitamente quer um resultado
    probabilístico em vez de determinístico.

### 8.3 Para Brasil / SiBCS

Mapa de Solos do Brasil 1:5.000.000 (Embrapa 2021, estilo disponível
para QGIS) pode ser reprojetado como raster de classes SiBCS. Não
fornece probabilidades nativamente, mas uma aproximação via janela de
vizinhança (15×15 células) dá P aproximado baseado em frequências
locais.

------------------------------------------------------------------------

## 9. Módulo 4 — Espectroscopia via OSSL

### 9.1 Contexto

A Open Soil Spectral Library (OSSL), mantida pelo consórcio Soil
Spectroscopy for Global Good (Woodwell Climate Research Center,
OpenGeoHub, ISRIC), disponibiliza \>100 mil espectros pareados com dados
de laboratório nas faixas Vis-NIR (350–2500 nm) e MIR (4000–600 cm⁻¹),
sob licença aberta. Infraestrutura R está documentada em
`https://soilspectroscopy.github.io/ossl-manual/` com workflows para
PLSR, Cubist e memory-based learning.

Esse é terreno onde você tem vantagem comparativa direta — Módulo 4 é
basicamente formalização pedagógica do que você já faz com soilVAE e
pipelines Vis-NIR/SWIR.

### 9.2 Fluxo

``` r

fill_from_spectra <- function(pedon,
                              library    = "ossl",
                              region     = c("global", "south_america",
                                             "north_america", "europe", "africa"),
                              properties = c("clay_pct", "sand_pct", "silt_pct",
                                             "cec_cmol", "bs_pct", "ph_h2o",
                                             "oc_pct", "caco3_pct", "fe_dcb_pct"),
                              method     = c("pretrained", "mbl", "plsr_local"),
                              preprocess = "snv+sg1",
                              k_neighbors = 100,
                              overwrite  = FALSE) {

  region <- match.arg(region); method <- match.arg(method)

  # 1. Pré-processamento do espectro do usuário
  X <- preprocess_spectra(pedon$spectra$vnir, method = preprocess)

  # 2. Predição
  preds <- switch(method,
    pretrained  = predict_ossl_pretrained(X, properties = properties, region = region),
    mbl         = predict_ossl_mbl(X, properties = properties, region = region,
                                   k = k_neighbors),
    plsr_local  = predict_ossl_plsr_local(X, properties = properties, region = region,
                                           k = k_neighbors)
  )
  # preds: data.table com columns property, value, pi95_low, pi95_high, n_neighbors

  # 3. Merge com sanidade
  for (i in seq_len(nrow(preds))) {
    r <- preds[i]
    horizon_idx <- r$horizon_idx
    if (r$property %in% names(pedon$horizons) &&
        (is.na(pedon$horizons[[r$property]][horizon_idx]) || overwrite)) {
      pedon$horizons[[r$property]][horizon_idx] <- r$value
      pedon$add_measurement(horizon_idx = horizon_idx,
                            attribute = r$property,
                            value = r$value,
                            source = "predicted_spectra",
                            confidence = pi_to_confidence(r$pi95_low, r$pi95_high))
    }
  }
  pedon
}
```

### 9.3 Memory-based learning (recomendado)

MBL via
[`resemble::mbl`](https://l-ramirez-lopez.github.io/resemble/reference/mbl.html)
tende a superar PLSR global em bibliotecas heterogêneas como OSSL, e se
integra naturalmente com filtragem regional (South America subset para
perfis brasileiros). Parâmetros default sugeridos: k = 100 vizinhos,
distância PLS scores, 5 componentes locais, validação LOO interna.

### 9.4 Interação com Módulo 1

A chave WRB/SiBCS tem atributos críticos quantitativos: % argila (para
gradiente textural de argic/B textural), CEC por 1 kg argila (para
Ferralsol/Latossolo, Nitisol), saturação por bases (Acrisol vs. Lixisol
vs. Luvisol), % Fe DCB (plinthic, ferralic).

Se esses atributos vêm de predição espectral com PI95% estreito, o
`evidence_grade` desce de A para B. Se o PI95% é largo o suficiente para
cruzar um limiar diagnóstico crítico (ex.: predição de CEC = 15 ± 6
cmol/kg e o limiar é 16), o sistema reporta a ambiguidade em
`ClassificationResult$ambiguities` e sugere medição laboratorial
específica em `missing_data`.

### 9.5 Sanity biogeográfica

Predições fora de ranges plausíveis para o biome (cruzando SoilGrids de
clima + dados WoSIS sumarizados por biome) geram `warning`. Ex.:
predição de CaCO₃ = 12% em um perfil em Mata Atlântica úmida é quase
certamente erro do modelo espectral ou problema na aquisição.

------------------------------------------------------------------------

## 10. Orquestração — fluxo completo

``` r

library(soilKey)

# 1. Construir registro
pedon <- PedonRecord$new(
  site = list(lat = -22.5, lon = -43.7, date = "2024-03-10",
              country = "BR", parent_material = "gneiss",
              elevation_m = 180, slope_pct = 8),
  documents = "perfil_042_descricao.pdf",
  spectra   = list(vnir = vnir_matrix),   # matriz 6 horizontes × 2151 bandas
  images    = "perfil_042_parede.jpg"
)

# 2. Extrair do PDF e da foto
pedon <- pedon |>
  extract_horizons_from_pdf(
    provider = vlm_provider("ollama", model = "gemma3:27b")
  ) |>
  extract_munsell_from_photo(
    provider = vlm_provider("ollama", model = "gemma3:27b")
  )

pedon$validate()

# 3. Preencher lacunas com espectros via OSSL
pedon <- fill_from_spectra(
  pedon,
  method     = "mbl",
  region     = "south_america",
  properties = c("clay_pct", "silt_pct", "sand_pct",
                 "cec_cmol", "bs_pct", "ph_h2o", "oc_pct", "fe_dcb_pct")
)

# 4. Prior espacial
prior <- spatial_prior(pedon, source = "soilgrids", system = "wrb2022")

# 5. Classificar nos dois sistemas
res_wrb   <- classify_wrb2022(pedon, prior = prior, on_missing = "warn")
res_sibcs <- classify_sibcs(pedon)

# 6. Inspecionar
print(res_wrb)
# ClassificationResult (WRB 2022)
#   Name: Rhodic Ferralsol (Clayic, Humic, Dystric)
#   Evidence grade: B (clay %, CEC predicted by spectra; pi95 narrow)
#   Prior check: consistent (P_prior = 0.41)
#   Ambiguities: none
#   Missing data to refine: none
#
# Key trace (abbreviated):
#   HS (Histosols)     — failed (no organic material >= 10 cm)
#   AT (Anthrosols)    — failed (no anthric horizons)
#   ...
#   FR (Ferralsols)    — PASSED (ferralic horizon, 25–180 cm; CEC 12 cmol/kg clay)

# 7. Relatório
report(list(res_wrb, res_sibcs), file = "perfil_042_report.html")
```

------------------------------------------------------------------------

## 11. Estratégia de validação científica

### 11.1 Fixtures canônicas

`inst/examples/profiles_wrb/` contém **um perfil por RSG** (32 no total)
extraído de: - Exemplos didáticos da própria WRB 2022 (Annex de Schad
2023, Table 1). - *Soil Atlas of Europe* (JRC, 2005). - Soil monolith
collection da ISRIC (re-classificada para WRB 2022).

`inst/examples/profiles_sibcs/` contém **um perfil por ordem** (13)
extraído de: - RADAMBRASIL (volumes disponíveis digitalmente). -
Boletins técnicos da Embrapa Solos. - Perfis de referência do SiBCS 5ª
ed.

### 11.2 Testes

Cada diagnóstico tem um arquivo de teste dedicado:

``` r

# tests/testthat/test-diagnostics-wrb-argic.R
test_that("argic passes on canonical Luvisol profile", {
  pedon <- readRDS(test_path("fixtures", "luvisol_canonical.rds"))
  res <- argic(pedon)
  expect_true(res$passed)
  expect_gte(length(res$layers), 1)
})

test_that("argic fails on Ferralsol", {
  pedon <- readRDS(test_path("fixtures", "ferralsol_canonical.rds"))
  res <- argic(pedon)
  expect_false(isTRUE(res$passed))
})

test_that("argic returns NA when clay data missing", {
  pedon <- readRDS(test_path("fixtures", "no_clay_data.rds"))
  res <- argic(pedon)
  expect_true(is.na(res$passed))
  expect_true(length(res$missing) > 0)
})

test_that("argic discriminates from natric (ESP)", {
  pedon <- readRDS(test_path("fixtures", "solonetz_with_clay_increase.rds"))
  res_argic <- argic(pedon)
  res_natric <- natric(pedon)
  expect_false(isTRUE(res_argic$passed))  # excluded by ESP
  expect_true(res_natric$passed)
})
```

### 11.3 Benchmark WoSIS (vignette 09)

Rodar `soilKey` sobre o subset WoSIS que tem classificação WRB original,
computar: - Taxa de concordância exata (mesmo RSG). - Taxa de
concordância parcial (mesmo RSG “guia” conforme tabelas de
correspondência em Schad 2023). - Matriz de confusão por RSG. - Padrões
sistemáticos de discordância — fornecem feedback útil ao IUSS WRB
Working Group e são material direto para o paper metodológico.

Esse benchmark é o núcleo empírico do paper (candidatos: *SoftwareX*,
*Geoderma*, *Computers & Geosciences*, *European Journal of Soil
Science*).

------------------------------------------------------------------------

## 12. Roadmap de implementação

| Versão | Escopo | Status |
|----|----|----|
| v0.1 | Esqueleto, classes core, 3 diagnósticos WRB (argic, ferralic, mollic), Ferralsols end-to-end. | **shipped (commit `613c3f2`)** |
| v0.2 | +8 diagnósticos WRB (calcic, gypsic, salic, cambic, plinthic, spodic, gleyic_properties, vertic_properties); +7 diagnósticos RSG-derivados (acrisol, lixisol, alisol, luvisol, chernozem, kastanozem, phaeozem); 16/32 RSGs ligados na chave; scaffolds Módulo 5 (USDA) e Módulo 6 (SiBCS). | **shipped (5 commits, `8077adb`…`8a7d6d9`)** |
| v0.3 | +15 diagnósticos WRB (histic, leptic, arenic, umbric, duric, technic, andic, fluvic, natric, nitic, planic, stagnic, retic, cryic, anthric); **chave WRB 32/32 RSGs ligados end-to-end**; 31 fixtures canônicas; refinamento gleyic/stagnic via padrão de decay; nitic/ferralic exclusion. | **shipped (3 commits, v0.3a-c)** |
| **v0.3.1** | **Correções Tier-1 contra texto canônico WRB 2022 Ch 3.1**: argic 6/1.4/20 + faixa 50, ferralic remove ECEC, duric 10/10, vertic ≥25 cm, salic alkaline + produto. | **shipped (`a63925b`)** |
| **v0.3.2** | **Correção da ordem dos RSGs**: PL/ST entre PT e NT (canonical pp 95-126); FL antes de AR. 31 fixtures continuam classificando corretamente. | **shipped (`27d8f14`)** |
| **v0.4** | **Módulo 4 — integração OSSL**: [`predict_ossl_mbl()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md), [`predict_ossl_plsr_local()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_plsr_local.md), [`predict_ossl_pretrained()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_pretrained.md), [`preprocess_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/preprocess_spectra.md) (SNV / SG1), [`pi_to_confidence()`](https://hugomachadorodrigues.github.io/soilKey/reference/pi_to_confidence.md), [`fill_from_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md). Provenance tag `predicted_spectra` rebaixa evidence_grade A→B. | **shipped (`381c3ce`)** |
| **v0.5** | **Módulo 3 — SoilGrids/Embrapa prior**: `spatial_soilgrids_prior()` (WCS), `spatial_embrapa_prior()`, [`prior_consistency_check()`](https://hugomachadorodrigues.github.io/soilKey/reference/prior_consistency_check.md). Wired em [`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md) via `prior` e `prior_threshold`. **A chave determinística nunca é sobrescrita pelo prior.** | **shipped (`98e524e`)** |
| **v0.6** | **Módulo 2 — VLM extraction via ellmer**: [`extract_horizons_from_pdf()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_horizons_from_pdf.md), [`extract_munsell_from_photo()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_munsell_from_photo.md), [`extract_site_from_fieldsheet()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_site_from_fieldsheet.md). Schema-validation via jsonvalidate (draft-07). `MockVLMProvider` exportado para testes. Bug-fix NSE em `PedonRecord$add_measurement`. | **shipped (`80735f6`)** |
| **v0.3.3** | **Cobertura completa de WRB Ch 3.1 / 3.2 / 3.3**: +18 horizontes (albic, ferric, petrocalcic/petroduric/petrogypsic/petroplinthic/pisoplinthic, vertic_horizon, thionic, fragic, sombric, chernic, anthraquic, hydragric, hortic, irragric, plaggic, pretic, terric); +12 propriedades (abrupt_textural_difference, albeluvic_glossae, continuous_rock, lithic_discontinuity, protocalcic_properties, protogypsic_properties, reducing_conditions, shrink_swell_cracks, sideralic_properties, takyric_properties, vitric_properties, yermic_properties); +16 materiais (aeolic, artefacts, calcaric, claric, dolomitic, gypsiric, hypersulfidic, hyposulfidic, limnic, mineral, mulmic, organic, organotechnic, ornithogenic, soil_organic_carbon, solimovic, technic_hard, tephric). Schema +24 colunas. | **shipped (`aa4afd7`)** |
| **v0.3.4** | **Tier-2 gate strengthening WRB Ch 4**: 7 RSG-level diagnósticos rigorosos – vertisol (cracks), andosol (andic OR vitric, exclusões), gleysol (multi-path), planosol (abrupt + stagnic + reducing), ferralsol (argic-exclusion com WDC/dpH/SOC paths), chernozem_strict (chernic + protocalcic + BS), kastanozem_strict (mollic + carbonate \<= 70 cm + BS). Inclui spodic refinado para evitar conflito com andic. Fixtures VR/AN/CH enriquecidas com cracks_width_cm, volcanic_glass_pct, worm_holes_pct. | **shipped (`a402606`)** |
| **v0.3.5** | **Cobertura final Ch 3.1 – 32/32 horizontes**: +tsitelic (Caucasian red), +panpaic (buried-soil pattern), +limonic (meadow redox), +protovertic (vertic-spectrum lower bound, mutually exclusive with strict vertic_horizon). | **shipped** |
| **v0.7** | **Módulo 6 SiBCS 5ª ed. (Embrapa, 2018) implementado integralmente**: +17 atributos diagnósticos (atividade_argila_alta, eutrofico, carater_alitico, carater_carbonatico, carater_redoxico, …); +24 horizontes diagnósticos (horizonte_histico, A_chernozemico/humico/proeminente, B_textural/latossolico/incipiente/nitico/espodico/planico, plintico, glei, fragipa, duripa, …); +13 diagnósticos RSG-level seguindo a chave canônica do Cap 4 (organossolo, neossolo, vertissolo, …, argissolo); +13 fixtures canônicas + 30 testes; key.yaml reescrito na ordem canônica O-R-V-E-S-G-L-M-C-F-T-N-P. | **shipped (`929b4dd`)** |
| v0.8 | **Módulo 5 — USDA Soil Taxonomy paralelo** (12 orders + suborders + great groups + diagnósticos USDA). Requer “Keys to Soil Taxonomy” (USDA-NRCS, 12th ed. 2014). | 3–4 semanas |
| v0.9 | **Qualifiers WRB completos** — ~202 qualifiers + 10 specifiers (Ano-, Bathy-, Endo-, Epi-, Kato-, Panto-, Poly-, Supra-, Thapto-, Amphi-) + sub-qualifiers (Hyper-, Hypo-, Proto-, Neo-, Orto-). Ver inventário em §6.4. Vignettes 05–09; benchmark WoSIS. | 1 mês |
| **v0.9.1 (Bloco A)** | **Canonical Ch 4 principal-qualifier coverage** para os primeiros 5 RSGs da chave (HS / AT / TC / CR / LP). +42 funções `qual_*` em `R/qualifiers-wrb2022-v091.R` (Calcaric, Dolomitic, Gypsiric, Tephric, Limnic, Solimovic, Ornithic, Sulfidic, Mulmic; Hortic, Irragric, Plaggic, Pretic, Terric, Hydragric, Anthraquic; Technic, Hyperartefactic, Urbic, Spolic, Garbic, Ekranic, Linic; Yermic, Takyric, Glacic, Turbic; Lithic, Nudilithic, Hyperskeletic, Rendzic, Vermic; Petric, Thionic, Sapric/Hemic/Fibric mutuamente exclusivos via dominância de espessura, Drainic, Subaquatic, Tidalic, Reductic, Organotechnic). YAML expandido com listas Ch 4 canônicas para Bloco A; AT fixture enriquecida com `p_mehlich3_mg_kg`. Tests: +87 expectations. | **shipped** |
| **v0.9.1 (Bloco B)** | **Cobertura para SN / VR / SC / GL / AN** (saline / clay-rich / wet / volcanic). +14 funções `qual_*` em `R/qualifiers-wrb2022-v091b.R`: Chernic, Pisoplinthic, Abruptic, Aceric; Mazic, Grumic, Pellic (estrutura/cor de superfície dos Vertissolos); Aluandic vs Silandic (split Al/Si do componente ativo andic via razão molar Al/(Al+0.5\*Si)), Hydric (retenção de água a 1500 kPa), Melanic (escuro alto-OC andic), Acroxic (ECEC \<= 2 cmol+/kg em layer andic), Pachic (mollic/umbric \>= 50 cm), Eutrosilic (silandic + BS \>= 50%). YAML Ch 4 canônico para os 5 RSGs (SN 22 principais, VR 23, SC 22, GL 32, AN 31). AN fixture enriquecida com `si_ox_pct` e `water_content_1500kpa`. **Refinamento de v0.9.1.A**: `qual_plaggic` ganhou gate de evidência antropogênica (P \>= 50 mg/kg, artefacts \>= 0.5%, ou designação `Apl/Aplg`) para evitar falso-positivo do diagnóstico v0.3.3 sobre qualquer A horizonte espesso. Tests: +103 expectations. | **shipped** |
| **v0.9.1 (Bloco C)** | **Cobertura para PZ / PT / PL / ST / NT / FR** – *bloco brasileiro / tropical*: Latossolos, Argissolos e Espodossolos do SiBCS vivem aqui como Ferralsols / Acrisols / Lixisols / Podzols. +14 funções `qual_*` em `R/qualifiers-wrb2022-v091c.R`: **família espodica** (Hyperspodic, Carbic = humus-dominado, Rustic = Fe-dominado, Ortsteinic, Placic \<= 25 mm, Densic \>= 1.8 g/cm³); **família de muito-baixa-CTC dos solos tropicais altamente intemperizados** (Geric ECEC \<= 1.5 cmol+/kg ou ()pH \> 0; Vetic CTC \<= 6 cmol+/kg argila; Posic ()pH \> 0; Hyperdystric BS \< 5%; Hypereutric BS \>= 80%; Hyperalic Al sat \>= 50% no argico); Hyperalbic (albico contiguous \>= 100 cm com guard de evidência eluvial); Sombric (com exclusão de spodic / ferralic). **Bug-fix v0.3.3**: [`sombric()`](https://hugomachadorodrigues.github.io/soilKey/reference/sombric.md) chamava `test_top_at_or_above(min_top_cm=...)` com nome de argumento inválido – corrigido para usar filtro direto de profundidade. YAML Ch 4 canônico para os 6 RSGs (PZ 23 principais, PT 24, PL 30, ST 29, NT 23, FR 30). FR canônico agora classifica como **Geric Ferric Rhodic Chromic Ferralsol** – nome WRB completo de um Latossolo Distrocoeso. Tests: +111 expectations. | **shipped** |
| **v0.9.1 (Bloco D + E)** | **Cobertura para os 16 RSGs restantes** – fechamento dos 32 / 32 RSGs WRB 2022. Bloco D: CH, KS, PH, UM, DU, GY, CL, RT (estepe / árido / frio-úmido). Bloco E: AC, LX, AL, LV, CM, AR, RG, FL (argico-rico / cambissolico / arenoso / aluvial). +4 funções `qual_*` em `R/qualifiers-wrb2022-v091de.R`: Cutanic (filme de argila visível em argico, marca de Argissolos / Luvisols), Glossic (mollic com glossae albicas, Chernozems / Phaeozems da transição estepe-floresta), Brunic (cambic-only B em Arenosol – exclui argico/spodic/ferralic/nitic), Protic (sem nenhum B horizonte). YAML Ch 4 canônico para os 16 RSGs (CH 21, KS 21, PH 21, UM 15, DU 18, GY 18, CL 18, RT 18, AC 24, LX 23, AL 19, LV 21, CM 29, AR 24, RG 24, FL 25). Resoluções canônicas: AC/LX/LV → “Albic Cutanic ”, AL → “Hyperalic Albic Cutanic Alisol”, CM → “Eutric Cambisol”, AR → “Protic Albic Arenosol”, FL → “Haplic Fluvisol”, CH → “Vermic Chernic Cambic Chernozem”. Tests: +115 expectations. **Marco**: cobertura RSG-level completa de WRB 2022 Ch 4 – 32 / 32 RSGs com listas principais canônicas. | **shipped** |
| **v0.9.2.A** | **Hyper- / Hypo- / Proto- sub-qualifiers + family suppression**. +11 funções (Hypersalic / Hyposalic; Hypersodic / Hyposodic; Hypercalcic / Hypocalcic / Protocalcic; Hypergypsic / Hypogypsic / Protogypsic; Protovertic). Engine ganha `.suppress_qualifier_siblings()`: quando vários membros de uma família passam (e.g. Calcic + Hypocalcic + Protocalcic), só o mais específico aparece no nome resolvido (per WRB Ch 6). Famílias: salinity, sodicity, calcic, gypsic, vertic, albic, skeletic, eutric, dystric, alic. YAML expandido: SN/VR/SC/CH/KS/PH/DU/GY/CL ganham as variantes Hyper-/Hypo-/Proto- nas posições canônicas. Tests: +52 expectations. | **shipped** |
| **v0.9.2.B** | **Specifier infrastructure** (Ano- / Epi- / Endo- / Bathy- / Panto- via prefix dispatch no resolver). `.detect_specifier()` reconhece o prefixo, `.apply_specifier()` chama o `qual_*` base e filtra as camadas pela faixa de profundidade. Sem necessidade de definir uma função por (specifier × base) – o sistema é genérico. CH ganha Endogleyic / Endostagnic / Endocalcic em posições canônicas. Specifiers Kato- / Poly- / Supra- / Thapto- / Amphi- adiados para v0.9.3 (precisam de flags de horizonte enterrado / cadeia de designações). Tests: +55 expectations. | **shipped** |
| **v0.9.2.C** | **Correções de diagnósticos v0.3.x** – redução de falsos-positivos. **cambic** ganha gate de profundidade (`min_top_cm = 5`) e gate de desenvolvimento estrutural (`structure_grade ∈ {weak, moderate, strong}` AND `structure_type ∉ {massive, single grain}`); horizontes A e C-massivos não passam mais. **plaggic** ganha gate de evidência antropogênica diretamente no diagnóstico (P \>= 50 mg/kg OR artefacts \> 0 OR designação Apl/Aplg/Apk); o gate v0.9.1 em `qual_plaggic` foi removido (delegação direta agora). **sombric** ganha gate de iluviação de húmus (camada candidata deve ter OC ≥ OC_camada_acima + 0.1%); a permissividade do v0.3.3 é eliminada. Mudança de classificação canônica resultante: DU → “Duric Skeletic Durisol” (perde Cambic do BC1 maciço). FR (Latossolo) e demais 30 fixtures inalterados. Tests: +43 expectations. | **shipped** |
| **v0.9.3.A** | **Specifiers restantes (Kato/Amphi/Poly/Supra/Thapto) + supplementary engine**. Refatoração de `.wrb_specifiers` para suportar dois `kind`s – `depth` (banda de profundidade simples; reusa o caminho v0.9.2.B) e `filter` (função custom). Helpers: `.kato_filter` (top_cm \>= 50), `.amphi_filter` (Epi AND Endo), `.poly_filter` (\>= 2 runs disjuntos), `.supra_filter` (acima de barreira: continuous_rock / petric / technic_hard), `.thapto_filter` (designação ending in `b`). Engine: `resolve_wrb_qualifiers` agora processa também o slot `supplementary:` do YAML, retorna `principal` + `supplementary` com famílias suprimidas em ambos. `classify_wrb2022` renderiza o nome WRB Ch 6 completo com tags entre parênteses. Tests: +66 expectations. | **shipped** |
| **v0.9.3.B** | **Supplementary qualifier seed** – 5 funções novas (Aric: designação Ap; Cumulic: cobertura recente fluvic/aeolic/cumulic-style; Profondic: argico continua \>= 150 cm; Rubic: hue \<= 5YR + chroma \>= 4 menos estrito que Rhodic; Lamellic: lamelas Bt via designação proxy) em `R/qualifiers-wrb2022-v093b.R`. YAML expandido com `supplementary:` slot canônico para FR / AC / LX / AL / LV / CM / NT. **Resultado canônico do Latossolo brasileiro**: FR agora classifica como **“Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)”** – nome WRB Ch 6 completo com qualifiers principais e supplementários. Mesma estrutura para AC/LX/LV/AL/NT/CM (Argissolos / Cambissolos / Nitossolos brasileiros). Tests: +51 expectations. | **shipped** |
| **v0.9.4** | **Vignettes 02-06 + benchmark WoSIS**. Cinco vignettes novos: `02-classify-wrb-end-to-end` (Latossolo end-to-end com nome Ch 6 completo); `03-cross-system-correlation` (mesmo perfil resolvido em WRB / SiBCS / USDA, com tabela de correspondências); `04-vlm-extraction` (extração multimodal via Module 2 com `MockVLMProvider`, schema-validation, retry-loop, provenance); `05-spatial-spectra-pipeline` (Module 3 SoilGrids prior + Module 4 OSSL gap-fill com fixtures sintéticos para reprodutibilidade offline); `06-wosis-benchmark` (protocolo de validação contra WoSIS, mini-benchmark on-line nos 31 fixtures canônicos, instruções para o run paper-grade). Driver script `inst/benchmarks/run_wosis_benchmark.R` para o pipeline completo (read_wosis_profiles + build_pedon_from_wosis + run_wosis_benchmark) que gera relatórios versionados em `inst/benchmarks/reports/wosis_<DATE>.md`. Todos os vignettes constroem sem erro via [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html) – nenhum requer rede ou dados externos para o caminho default. | **shipped** |
| v1.0 | CRAN submission, paper metodológico submetido (SoftwareX / Geoderma / C&G / EJSS). | \+ 1 mês revisão |

Total estimado: **9–14 meses** para v1.0 com dedicação parcial.

### Estado factual em 2026-04-27 (após `929b4dd` – v0.7 SiBCS shipped)

- **20 commits** no `main`. Todos os 4 módulos do escopo original
  implementados (Módulo 1 chave WRB, Módulo 2 VLM, Módulo 3 prior
  espacial, Módulo 4 OSSL). v0.3.3 fechou a cobertura WRB de Ch
  3.1/3.2/3.3, v0.3.4 trouxe os gates RSG-level estritos, v0.3.5 fechou
  os 32 horizontes WRB. **v0.7 entrega o Módulo 6 (SiBCS 5ª ed.)
  completo**: 17 atributos diagnósticos + 24 horizontes + 13 ordens
  RSG-level wired na chave canônica do Cap 4.
- **368 test_that blocks / 830 expectations** passando. **1 skip
  deliberado** (test que requer ellmer instalado).
- **WRB 2022 + SiBCS 5ª ed. ambos com chave do 1º nível completa**:
  32/32 RSGs WRB + 13/13 ordens SiBCS, na ordem canônica de cada
  sistema. 31 fixtures WRB + 13 fixtures SiBCS, todas classificando para
  a classe pretendida.
- **Chave WRB 2022 32/32 RSGs em ordem canônica** com gates Tier-2
  estritos para VR/AN/GL/PL/FR/CH/KS. 31 fixtures canônicas, cada uma
  classifica para a RSG pretendida.
- **Cobertura de Ch 3.1 (horizontes diagnósticos):** **32 de 32
  horizontes implementados** (full coverage). Inclui todas as variantes
  petric (petrocalcic, petroduric, petrogypsic, petroplinthic), a
  anthropogenic family (anthraquic, hydragric, hortic, irragric,
  plaggic, pretic, terric), vertic_horizon (estricto), thionic, fragic,
  sombric, chernic, e os quatro nicho histórico de v0.3.5: tsitelic (red
  Caucasian), panpaic (buried), limonic (meadow redox), protovertic
  (vertic-spectrum lower bound).
- **Cobertura de Ch 3.2 (propriedades):** **17 de 17 propriedades
  implementadas** (full coverage): abrupt_textural_difference,
  albeluvic_glossae, andic_properties, anthric_properties (via
  anthric_horizons), continuous_rock, gleyic_properties,
  lithic_discontinuity, protocalcic_properties, protogypsic_properties,
  reducing_conditions, retic_properties, shrink_swell_cracks,
  sideralic_properties, stagnic_properties, takyric_properties,
  vitric_properties, yermic_properties. Mais cryic_conditions,
  planic_features, leptic_features (não pertencem ao Ch 3.2 mas são
  propriedades operacionais).
- **Cobertura de Ch 3.3 (materiais):** **19 de 19 materiais
  implementados** (full coverage): aeolic, artefacts, calcaric, claric,
  dolomitic, fluvic, gypsiric, hypersulfidic, hyposulfidic, limnic,
  mineral, mulmic, organic, organotechnic, ornithogenic,
  soil_organic_carbon, solimovic, technic_hard, tephric. Histic_horizon
  (Ch 3.1) também serve como gate para organic_material.
- **Schema horizon expandido** (24 novas colunas em v0.3.3):
  cementation_class, p_mehlich3_mg_kg, worm_holes_pct,
  water_dispersible_clay_pct, sulfidic_s_pct, volcanic_glass_pct,
  phosphate_retention_pct, artefacts_industrial_pct,
  artefacts_urbic_pct, rock_origin, permafrost_temp_C, cracks_width_cm,
  cracks_depth_cm, polygonal_cracks_spacing_cm, desert_pavement_pct,
  varnish_pct, ventifact_pct, vesicular_pores, rupture_resistance,
  plasticity, al_kcl_cmol, layer_origin.

### Módulo 5 — USDA Soil Taxonomy (v0.8)

Adicionado ao escopo em 2026-04-25 a pedido do usuário. v0.2 entrega o
scaffold estrutural: - `inst/rules/usda/key.yaml` — 12 Orders em ordem
canônica de chave (GE, HI, SP, AD, OX, VE, AS, UT, MO, AF, IN, EN).
Apenas Oxisols ligado em v0.2 via
[`oxic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/oxic_usda.md)
(delegação para WRB ferralic). - `R/key-usda.R` —
[`classify_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md),
[`run_usda_key()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_usda_key.md). -
`R/diagnostics-horizons-usda.R` —
[`oxic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/oxic_usda.md)
e
[`argillic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/argillic_usda.md)
como delegações para WRB ferralic / argic em v0.2 (criterios centrais
coincidem; diferenças de borda agendadas para v0.8).

v0.8 implementará: mollic_usda, umbric_usda, ochric, kandic,
spodic_usda, cambic_usda, calcic_usda, gypsic_usda, duripan, fragipan,
placic, albic, petrocalcic, petrogypsic, gelic conditions, organic
materials USDA, andic properties USDA, vertic features USDA, aridic
moisture regime, argillic_low_bs, argillic_high_bs.

Posicionamento: USDA Soil Taxonomy não tem implementação pública mantida
da chave; o pacote `SoilTaxonomy` no CRAN cobre tabelas de lookup.
soilKey + Módulo 5 será a primeira implementação pública da chave USDA.

### Módulo 6 — SiBCS 5ª edição (v0.7)

v0.2 entrega o scaffold estrutural: - `inst/rules/sibcs5/key.yaml` — 13
ordens em ordem provisória de chave (O, V, E, F, G, M, P, T, L, N, C, S,
R). Latossolos ligado via
[`B_latossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_latossolico.md)
(delegação para ferralic). Argissolos ligado via
[`B_textural()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_textural.md)
(delegação para argic). - `R/key-sibcs.R` —
[`classify_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md),
[`run_sibcs_key()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_sibcs_key.md). -
`R/diagnostics-horizons-sibcs.R` —
[`B_latossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_latossolico.md)
e
[`B_textural()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_textural.md)
como delegações em v0.2.

v0.7 implementará: B nítico, B espódico, B incipiente, B plânico, B
textural com alta atividade, A chernozêmico, A húmico, A proeminente,
caráter vértico, glei dentro top 50, horizonte plíntico, horizonte
hidromórfico orgânico, horizonte hortico, caráter alítico, caráter
alumínico, caráter ácrico. Validação direta com Embrapa Solos (parceria
via UFRRJ).

Posicionamento: nenhum pacote R/Python público implementa o SiBCS hoje.
soilKey + Módulo 6 será a primeira implementação pública.

------------------------------------------------------------------------

## 13. Riscos conhecidos e mitigações

| Risco | Mitigação |
|----|----|
| WRB 2022 tem definições ambíguas em alguns diagnósticos. | Documentar interpretação adotada em cada função; abrir issues que referenciem o capítulo exato; propor clarificações à IUSS WRB WG quando aplicável. |
| SiBCS 5ª ed. pode ter atualizações tácitas (Embrapa nem sempre publica errata). | Tagging de versão explícito em `rules/sibcs5/`; parceria direta com Embrapa Solos (sugerir via rede UFRRJ) para validação. |
| OSSL predictions podem falhar em solos brasileiros sub-representados (ex.: Amazônia). | Permitir bibliotecas espectrais locais (parceria com redes nacionais); reportar PI95% explicitamente; avisar quando n_neighbors em região filtrada é baixo. |
| VLMs evoluem rapidamente — modelos nomeados hoje podem estar obsoletos em um ano. | Abstração `ellmer` + configuração externa; testes de regressão sobre fixtures textuais com comparação de JSON resultante independente do provider. |
| Licenciamento dos dados de treinamento (WoSIS, RADAMBRASIL). | Usar apenas subsets com licença aberta documentada; fixtures canônicas no pacote são ≤ 50 perfis, fair use acadêmico. |

------------------------------------------------------------------------

## 14. Posicionamento científico

O pacote preenche três lacunas identificáveis na literatura:

1.  **Não existe implementação pública e mantida da chave WRB 2022
    completa.** O próprio prefácio da 4ª edição reconhece que algoritmos
    de classificação automatizada foram desenvolvidos internamente no
    IUSS WRB WG durante a elaboração, mas não há release público. O
    `SoilTaxonomy` no CRAN contém apenas tabelas de lookup.
2.  **Não existe pacote para SiBCS.** A Embrapa disponibiliza o livro e
    estilos QGIS, mas nenhuma biblioteca de inferência.
3.  **Não existe ferramenta que integre extração multimodal + predição
    espectral + chave taxonômica num único fluxo rastreável.**

O paper metodológico pode posicionar `soilKey` como infraestrutura
aberta para pedologia digital, complementar ao trabalho de mapeamento do
SoilGrids (que classifica a partir de *covariáveis* em escala de pixel)
— enquanto `soilKey` classifica a partir de *dados de perfil* em escala
de pedon, usando a chave canônica. São inferências distintas e
complementares.

------------------------------------------------------------------------

## 15. Próximos passos imediatos

1.  **Spec migrado.** Este documento é a versão atualizada com nome
    `soilKey`, vivendo em `ARCHITECTURE.md` no diretório do projeto. O
    rascunho original (`pedokey_architecture_1.md` no Desktop) fica como
    histórico.
2.  **Esqueleto do pacote criado** — DESCRIPTION, NAMESPACE, estrutura
    de `R/`, `inst/rules/`, `tests/`, `vignettes/`. `devtools::check()`
    passa em vazio.
3.  **v0.1 implementada** — `PedonRecord` + `DiagnosticResult` +
    `ClassificationResult` + 3 diagnósticos WRB (argic, ferralic,
    mollic) + Ferralsols end-to-end + fixture canônica + testes +
    vignette 01. Pré-print viável com benchmark Ferralsol limitado.
4.  **Sondar parcerias** — IUSS WRB Working Group (Peter Schad, Stephan
    Mantel) e Embrapa Solos (contato institucional via UFRRJ) para
    validação e possível endosso oficial. Endosso da IUSS seria valor
    inestimável para adoção internacional.
