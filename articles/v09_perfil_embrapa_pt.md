# Um perfil real do A ao Z (estilo Embrapa, em portugues)

## Introducao

Esta vinheta acompanha **um unico perfil de solo do Brasil tropical**
desde os dados brutos de campo + laboratorio ate as **tres
classificacoes paralelas** (SiBCS 5a edicao, WRB 2022, USDA Soil
Taxonomy 13a edicao), passando por:

1.  construcao do `PedonRecord` no formato canonico do soilKey;
2.  inspecao dos diagnosticos disparados (B textural, atividade da
    argila, V%);
3.  classificacao tripla com explicacao do trace;
4.  relatorio HTML pronto para entregar a um colega ou aluno;
5.  (opcional) cruzamento com o priore espacial do MapBiomas Solos
    v0.9.48.

O objetivo e dar ao agronomo / pedologo brasileiro um caminho
**reproduzivel e em portugues**, no estilo dos boletins da Embrapa
Solos. Todo o codigo abaixo roda sem dependencia externa pesada (nem
rede, nem ESDB, nem OSSL); a etapa final do MapBiomas e opcional e exige
um raster baixado.

## O perfil: Argissolo Vermelho-Amarelo distrofico tipico

Adaptado do **Levantamento Reconhecimento dos Solos do Estado do Rio de
Janeiro** (Embrapa, 2003), perfil RJ-1 da regiao de Itaguai. Coordenadas
aproximadas: 22.86 S, 43.78 W. Material de origem: sedimentos argilosos
do Terciario.

``` r

horizontes <- data.table::data.table(
  top_cm                = c(0,    20,   55,   115,  170),
  bottom_cm             = c(20,   55,   115,  170,  220),
  designation           = c("A",  "AB", "Bt1","Bt2","BC"),
  munsell_hue_moist     = c("10YR","7.5YR","5YR","2.5YR","2.5YR"),
  munsell_value_moist   = c(4,     4,     4,     3,     3),
  munsell_chroma_moist  = c(3,     5,     6,     6,     6),
  structure_grade       = c("moderate", "moderate",
                              "strong", "strong", "moderate"),
  structure_type        = c("granular", "subangular blocky",
                              "subangular blocky",
                              "subangular blocky",
                              "subangular blocky"),
  clay_films_amount     = c(NA, "few", "common", "common", "few"),
  clay_pct              = c(18,   28,   45,   42,   38),
  silt_pct              = c(30,   25,   20,   22,   24),
  sand_pct              = c(52,   47,   35,   36,   38),
  ph_h2o                = c(5.5,  5.3,  5.0,  5.0,  5.1),
  ph_kcl                = c(4.4,  4.3,  4.1,  4.1,  4.2),
  oc_pct                = c(1.5,  0.6,  0.3,  0.2,  0.2),
  cec_cmol              = c(8.0,  6.0,  5.5,  4.5,  4.0),
  bs_pct                = c(35,   25,   20,   18,   20),   # V baixa -> distrofico
  al_cmol               = c(0.5,  0.8,  1.2,  1.5,  1.4),
  ca_cmol               = c(2.0,  1.4,  1.0,  0.7,  0.7),
  mg_cmol               = c(0.6,  0.4,  0.3,  0.2,  0.2),
  k_cmol                = c(0.10, 0.06, 0.04, 0.03, 0.03),
  na_cmol               = c(0.02, 0.02, 0.02, 0.02, 0.02),
  bulk_density_g_cm3    = c(1.30, 1.40, 1.45, 1.45, 1.42)
)
horizontes <- soilKey:::ensure_horizon_schema(horizontes)
str(horizontes[, .(designation, top_cm, bottom_cm, ph_h2o, clay_pct, bs_pct)])
#> Classes 'data.table' and 'data.frame':   5 obs. of  6 variables:
#>  $ designation: chr  "A" "AB" "Bt1" "Bt2" ...
#>  $ top_cm     : num  0 20 55 115 170
#>  $ bottom_cm  : num  20 55 115 170 220
#>  $ ph_h2o     : num  5.5 5.3 5 5 5.1
#>  $ clay_pct   : num  18 28 45 42 38
#>  $ bs_pct     : num  35 25 20 18 20
#>  - attr(*, ".internal.selfref")=<pointer: 0x56116fb29f00>
```

Construindo o `PedonRecord` (R6 com `site` + `horizons`):

``` r

perfil <- PedonRecord$new(
  site = list(
    id        = "RJ-1-Itaguai",
    lat       = -22.86,
    lon       = -43.78,
    country   = "BR",
    state     = "RJ",
    municipality = "Itaguai",
    parent_material = "sedimentos argilosos do Terciario",
    survey_year = 2003,
    reference_source = "Embrapa Solos (2003) - Levantamento RJ"
  ),
  horizons = horizontes
)
perfil
#> 
#> ── PedonRecord ──
#> 
#> Site: id=RJ-1-Itaguai | (-22.8600, -43.7800) | BR | on sedimentos argilosos do
#> Terciario
#> Horizons (5):
#> 1) A 0-20 cm clay=18.0 silt=30.0 sand=52.0 CEC=8.0 pH=5.5 OC=1.5
#> 2) AB 20-55 cm clay=28.0 silt=25.0 sand=47.0 CEC=6.0 pH=5.3 OC=0.6
#> 3) Bt1 55-115 cm clay=45.0 silt=20.0 sand=35.0 CEC=5.5 pH=5.0 OC=0.3
#> 4) Bt2 115-170 cm clay=42.0 silt=22.0 sand=36.0 CEC=4.5 pH=5.0 OC=0.2
#> 5) BC 170-220 cm clay=38.0 silt=24.0 sand=38.0 CEC=4.0 pH=5.1 OC=0.2
```

## Diagnosticos manuais (sanity check)

Antes de chamar a chave completa, vale rodar os predicados-chave para
sentir o perfil. Isso e equivalente ao trabalho do classificador humano
antes de aplicar a chave:

``` r

# B textural (SiBCS Cap 5 / WRB argic): gradiente de argila
bt   <- soilKey::B_textural(perfil)
arg  <- soilKey::argic(perfil)
cat("B_textural (SiBCS):", bt$passed,
    "  argic (WRB):",     arg$passed, "\n")
#> B_textural (SiBCS): TRUE   argic (WRB): TRUE

# Atividade da argila (SiBCS Cap 5)
ta <- soilKey::atividade_argila_alta(perfil)
cat("atividade_argila_alta:", ta$passed, "\n")
#> atividade_argila_alta: FALSE

# Saturacao por bases (V%) -- distrofico se V < 50
distr <- soilKey::distrofico(perfil)
cat("distrofico:", distr$passed, "\n")
#> distrofico: TRUE
```

Os tres devem disparar `TRUE` para um Argissolo distrofico tipico.

## Tres classificacoes paralelas

A funcao
[`classify_all()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md)
corre os tres sistemas e devolve um `ClassificationResult` por sistema:

``` r

res <- soilKey::classify_all(perfil, on_missing = "silent")
names(res)
#> [1] "wrb"     "sibcs"   "usda"    "summary"
```

### SiBCS 5a edicao

``` r

print(res$sibcs)
#> 
#> ── ClassificationResult (SiBCS 5a edicao) ──
#> 
#> Name: Argissolos Vermelhos Distroficos tipicos, argilosa, moderado, Tmob
#> RSG/Order: Argissolos
#> Evidence grade: A
#> 
#> ── Ambiguities
#> - V: Indeterminate -- missing 1 attribute(s): slickensides
#> - E: Indeterminate -- missing 2 attribute(s): al_ox_pct, fe_ox_pct
#> - G: Indeterminate -- missing 1 attribute(s): redoximorphic_features_pct
#> - F: Indeterminate -- missing 1 attribute(s): plinthite_pct
#> 
#> ── Missing data that would refine result
#> slickensides, al_ox_pct, fe_ox_pct, redoximorphic_features_pct, plinthite_pct
#> 
#> ── Key trace
#> (10 RSGs tested before assignment)
#> 1. ??  -- NA
#> 2. ??  -- NA
#> 3. PV Argissolos Vermelhos -- NA
#> 4. ??  -- NA
#> 5. PVd Argissolos Vermelhos Distroficos -- NA
#> 6. ??  -- NA
#> 7. PVdTp Argissolos Vermelhos Distroficos tipicos -- NA
#> 8. ??  -- NA
```

A descida vai do nivel da Ordem (`Argissolos`) ate o Subgrupo, passando
pelos discriminantes:

- Matiz Munsell em B (`5YR` / `2.5YR`) -\> Subordem **Vermelho-Amarelo**
- V% baixa em B -\> Grande Grupo **Distrofico**
- Atividade da argila baixa, sem cerosidade forte -\> Subgrupo
  **tipico**

### WRB 2022

``` r

print(res$wrb)
#> 
#> ── ClassificationResult (WRB 2022) ──
#> 
#> Name: Ferralic Cutanic Acrisol (Loamic, Dystric, Ochric, Profondic, Rhodic,
#> Rubic, Chromic)
#> RSG/Order: Acrisols
#> Qualifiers: Ferralic, Cutanic, Loamic, Dystric, Ochric, Profondic, Rhodic,
#> Rubic, Chromic, FALSE, FALSE, FALSE, FALSE, top_cm, bottom_cm, FALSE, top_cm,
#> bottom_cm, FALSE, FALSE, FALSE, FALSE, plinthite_pct, FALSE, plinthite_pct,
#> FALSE, plinthite_pct, TRUE, FALSE, redoximorphic_features_pct, FALSE,
#> redoximorphic_features_pct, FALSE, al_ox_pct, fe_ox_pct,
#> phosphate_retention_pct, volcanic_glass_pct, FALSE, volcanic_glass_pct, FALSE,
#> slickensides, TRUE, FALSE, FALSE, coarse_fragments_pct, FALSE, FALSE,
#> p_mehlich3_mg_kg, FALSE, p_mehlich3_mg_kg, FALSE, FALSE, FALSE, FALSE, TRUE,
#> FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,
#> coarse_fragments_pct, TRUE
#> Evidence grade: A
#> 
#> ── Ambiguities
#> - AT: Indeterminate -- missing 1 attribute(s): p_mehlich3_mg_kg
#> - TC: Indeterminate -- missing 3 attribute(s): artefacts_pct,
#> geomembrane_present, technic_hardmaterial_pct
#> - CR: Indeterminate -- missing 1 attribute(s): permafrost_temp_C
#> - LP: Indeterminate -- missing 1 attribute(s): coarse_fragments_pct
#> - VR: Indeterminate -- missing 1 attribute(s): slickensides
#> - SC: Indeterminate -- missing 1 attribute(s): ec_dS_m
#> - PZ: Indeterminate -- missing 2 attribute(s): al_ox_pct, fe_ox_pct
#> - PT: Indeterminate -- missing 1 attribute(s): plinthite_pct
#> - ST: Indeterminate -- missing 1 attribute(s): redoximorphic_features_pct
#> - DU: Indeterminate -- missing 1 attribute(s): duripan_pct
#> - GY: Indeterminate -- missing 1 attribute(s): caso4_pct
#> - CL: Indeterminate -- missing 1 attribute(s): caco3_pct
#> 
#> ── Missing data that would refine result
#> p_mehlich3_mg_kg, artefacts_pct, geomembrane_present, technic_hardmaterial_pct,
#> permafrost_temp_C, coarse_fragments_pct, slickensides, ec_dS_m,
#> redoximorphic_features_pct, al_ox_pct, fe_ox_pct, phosphate_retention_pct,
#> volcanic_glass_pct, plinthite_pct, top_cm, bottom_cm, duripan_pct, caso4_pct,
#> caco3_pct
#> 
#> ── Key trace
#> (25 RSGs tested before assignment)
#> 1. HS Histosols -- failed
#> 2. AT Anthrosols -- NA (1 attrs missing)
#> 3. TC Technosols -- NA (3 attrs missing)
#> 4. CR Cryosols -- NA (1 attrs missing)
#> 5. LP Leptosols -- NA (1 attrs missing)
#> 6. SN Solonetz -- failed
#> 7. VR Vertisols -- NA (1 attrs missing)
#> 8. SC Solonchaks -- NA (1 attrs missing)
#> 9. GL Gleysols -- failed (1 attrs missing)
#> 10. AN Andosols -- failed (4 attrs missing)
#> 11. PZ Podzols -- NA (2 attrs missing)
#> 12. PT Plinthosols -- NA (1 attrs missing)
#> 13. PL Planosols -- failed
#> 14. ST Stagnosols -- NA (1 attrs missing)
#> 15. NT Nitisols -- failed
#> 16. FR Ferralsols -- failed
#> 17. CH Chernozems -- failed (2 attrs missing)
#> 18. KS Kastanozems -- failed (2 attrs missing)
#> 19. PH Phaeozems -- failed (2 attrs missing)
#> 20. UM Umbrisols -- failed
#> 21. DU Durisols -- NA (1 attrs missing)
#> 22. GY Gypsisols -- NA (1 attrs missing)
#> 23. CL Calcisols -- NA (1 attrs missing)
#> 24. RT Retisols -- failed
#> 25. AC Acrisols -- PASSED
```

WRB devolve o RSG mais qualificadores (em ordem canonica): **Acrisol
Dystric Cutanic** (caracter argico + V baixa + presenca de revestimentos
argilosos).

### USDA Soil Taxonomy 13a edicao

``` r

print(res$usda)
#> 
#> ── ClassificationResult (USDA Soil Taxonomy) ──
#> 
#> Name: Typic Kandiudults
#> RSG/Order: Ultisols
#> Evidence grade: A
#> 
#> ── Missing data that would refine result
#> permafrost_temp_C, al_ox_pct, fe_ox_pct, phosphate_retention_pct, slickensides,
#> site$soil_moisture_regime
#> 
#> ── Key trace
#> (7 RSGs tested before assignment)
#> 1. ??  -- NA
#> 2. ??  -- NA
#> 3. HC Udults -- NA
#> 4. ??  -- NA
#> 5. HCC Kandiudults -- NA
#> 6. ??  -- NA
#> 7. HCCD Typic Kandiudults -- NA
```

USDA-ST devolve a ordem (ate o nivel atualmente disponivel): **Ultisol**
(B argilico + saturacao por bases baixa em todo o solum).

## Comparacao cross-system

``` r

data.frame(
  Sistema  = c("SiBCS 5a", "WRB 2022", "USDA-ST 13a"),
  Classe   = c(res$sibcs$name, res$wrb$name, res$usda$name),
  EvidGrade = c(res$sibcs$evidence_grade %||% NA,
                  res$wrb$evidence_grade   %||% NA,
                  res$usda$evidence_grade  %||% NA)
)
#>       Sistema
#> 1    SiBCS 5a
#> 2    WRB 2022
#> 3 USDA-ST 13a
#>                                                                                  Classe
#> 1                    Argissolos Vermelhos Distroficos tipicos, argilosa, moderado, Tmob
#> 2 Ferralic Cutanic Acrisol (Loamic, Dystric, Ochric, Profondic, Rhodic, Rubic, Chromic)
#> 3                                                                     Typic Kandiudults
#>   EvidGrade
#> 1         A
#> 2         A
#> 3         A
```

Esses tres rotulos sao **complementares**, nao competidores. O SiBCS
captura nuance brasileira (Distrofico, Tb, presenca/ausencia de
cerosidade); o WRB captura o consenso internacional (Acrisol); o USDA-ST
encaixa no esquema norte-americano (Ultisol). Todos os tres apontam para
a mesma realidade: **um solo argico, acido, baixa em bases,
vermelho-amarelo**.

## Relatorio HTML

``` r

res$sibcs$report(
  file  = "perfil_RJ1.html",
  format = "html",
  pedon = perfil
)
# Ou, para os tres sistemas em um arquivo:
soilKey::report(
  list(res$sibcs, res$wrb, res$usda),
  file  = "perfil_RJ1_triplo.html",
  format = "html",
  pedon = perfil
)
```

O HTML carrega: nome canonico em cada sistema, qualificadores, evidencia
grade, trace dos predicados, ambiguidades, atributos faltantes, e os
horizontes formatados estilo planilha de boletim.

## Cruzamento com priore espacial (opcional)

Se voce baixou o raster nacional do **MapBiomas Solos Colecao 2** (30 m,
classes SiBCS, 2023+), pode cruzar a coordenada do perfil com o mapa
para verificar consistencia:

``` r

sibcs_no_mapa <- soilKey::lookup_mapbiomas_solos(
  coords      = c(perfil$site$lon, perfil$site$lat),
  raster_path = "soil_data/mapbiomas/mapbiomas_solos_30m_2023.tif",
  legend      = data.frame(
    value      = c(3),
    class_name = c("Argissolo Vermelho-Amarelo")
  )
)
sibcs_no_mapa
#> [1] "Argissolo Vermelho-Amarelo"
```

Concordancia entre o classificador e o mapa nacional aumenta a
confianca. Discordancia (ex.: classificador diz Argissolo, mapa diz
Latossolo) e um sinal de revisao – o pedologo decide qual fonte e mais
confiavel para o caso.

## Cruzamento com SoilGrids 250m (global)

Para qualquer coordenada do mundo,
[`lookup_soilgrids()`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_soilgrids.md)
(v0.9.48) le direto do endpoint COG da ISRIC sem download:

``` r

ph_topsoil <- soilKey::lookup_soilgrids(
  coords   = c(perfil$site$lon, perfil$site$lat),
  property = "phh2o",  depth = "0-5cm", quantile = "mean"
)
clay_subsoil <- soilKey::lookup_soilgrids(
  coords   = c(perfil$site$lon, perfil$site$lat),
  property = "clay",   depth = "30-60cm", quantile = "mean"
)
cat("SoilGrids pH (0-5cm):", ph_topsoil,
    " | clay subsoil (30-60cm):", clay_subsoil, "%\n")
```

## Resumo

| Etapa | Funcao | Onde |
|----|----|----|
| Construir o perfil | `PedonRecord$new()` | R |
| Diagnosticos manuais | [`B_textural()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_textural.md), [`argic()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md), [`distrofico()`](https://hugomachadorodrigues.github.io/soilKey/reference/distrofico.md), … | R |
| Tres classificacoes | [`classify_all()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md) | R |
| Relatorio | `res$sibcs$report()` ou [`soilKey::report()`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md) | R |
| Munsell de espectros | [`predict_munsell_from_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_munsell_from_spectra.md) (v0.9.47) | R |
| Quimica de espectros | [`predict_from_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_from_spectra.md) (v0.9.46) | R |
| Cruzamento Brasil | [`lookup_mapbiomas_solos()`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_mapbiomas_solos.md) (v0.9.48) | R + GeoTIFF |
| Cruzamento global | [`lookup_soilgrids()`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_soilgrids.md) (v0.9.48) | R + HTTPS |
| Cruzamento Europa | [`lookup_esdb()`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_esdb.md) (v0.9.44) | R + GeoTIFF |
| App interativo | [`run_classify_app()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_classify_app.md) | Shiny |

Esse e o caminho **reproduzivel** que o pacote oferece: o pedologo chega
com horizontes + cores Munsell + analises de laboratorio, roda **um
comando**
([`classify_all()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md)),
e recebe os tres rotulos junto com o trace explicando exatamente quais
diagnosticos dispararam em cada sistema. Em ~10 linhas de R.
