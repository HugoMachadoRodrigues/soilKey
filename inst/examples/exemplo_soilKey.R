#!/usr/bin/env Rscript
# =============================================================================
# soilKey - script de exemplo linear e auto-verificado
# -----------------------------------------------------------------------------
# Um tour completo pelas funcoes principais do pacote, do inicio ao fim, SEM
# nenhum loop (for/while) - le-se de cima para baixo como um tutorial.
# Cada etapa imprime o que fez e usa stopifnot() para se auto-verificar: se o
# script chegar ao fim sem parar, tudo funcionou.
#
# Depois de instalar o pacote (install.packages("soilKey")), localize este
# arquivo com:
#   system.file("examples", "exemplo_soilKey.R", package = "soilKey")
# e rode com:  Rscript <caminho>   (ou abra no R/RStudio e rode bloco a bloco)
# =============================================================================

library(soilKey, warn.conflicts = FALSE)
cat("=== soilKey", as.character(packageVersion("soilKey")), "===\n\n")


# -----------------------------------------------------------------------------
# 0. O JEITO MAIS SIMPLES - voce so precisa de uma planilha (CSV)
# -----------------------------------------------------------------------------
# Nao quer escrever codigo?  run_classify_app()  abre um app: suba um CSV,
# clique em Classify, pronto.  Quer em uma linha de R?  Ponha seu perfil num
# CSV (uma linha por horizonte; colunas com os nomes canonicos) e chame
# classify_csv().  Ha um CSV-modelo dentro do pacote para voce copiar:
cat("[0] O jeito mais simples: um CSV -> resultado, em uma linha\n")

modelo <- system.file("extdata", "perfil_exemplo.csv", package = "soilKey")
print(classify_csv(modelo))     # WRB / SiBCS / USDA de uma vez
cat("    (copie", basename(modelo), "-> edite com os SEUS dados -> classify_csv())\n\n")

# O resto do script mostra o que da para fazer alem disso (proveniencia, trace,
# familia, espectros, relatorio...). Mas, para o uso do dia a dia, o passo 0
# acima ja basta.


# -----------------------------------------------------------------------------
# 1. (Opcional) Construir um PedonRecord na mao - para entender o modelo
# -----------------------------------------------------------------------------
# A estrutura central e o PedonRecord: metadados de sitio + tabela de horizontes.
# Voce quase nunca precisa digitar isto (use um CSV, passo 0) - esta aqui so
# para mostrar o que ha por baixo. Um Latossolo Vermelho distrofico tipico:
cat("[1] Construindo um PedonRecord (Latossolo Vermelho distrofico)\n")

meu_pedon <- PedonRecord$new(
  site = list(
    id                    = "LVd-001",
    lat                   = -22.5,
    lon                   = -43.7,
    country               = "BR",
    parent_material       = "gnaisse",
    soil_moisture_regime  = "udic",
    soil_temperature_regime = "isohyperthermic"
  ),
  horizons = data.frame(
    top_cm      = c(0,  15, 65,  130),
    bottom_cm   = c(15, 65, 130, 200),
    designation = c("A", "AB", "Bw1", "Bw2"),
    munsell_hue_moist    = rep("2.5YR", 4),
    munsell_value_moist  = c(3, 3, 4, 4),
    munsell_chroma_moist = c(4, 6, 6, 6),
    clay_pct = c(50, 55, 60, 60),
    silt_pct = c(15, 10, 8,  8),
    sand_pct = c(35, 35, 32, 32),
    cec_cmol = c(8.0, 5.5, 5.0, 4.8),
    bs_pct   = c(24, 14, 13, 13),
    ph_h2o   = c(4.8, 4.7, 4.8, 4.9),
    oc_pct   = c(2.0, 0.6, 0.3, 0.2)
  )
)
cat("    horizontes:", nrow(meu_pedon$horizons), "| profundidade:",
    max(meu_pedon$horizons$bottom_cm), "cm\n\n")


# -----------------------------------------------------------------------------
# 2. Validar a geometria dos horizontes (antes de classificar)
# -----------------------------------------------------------------------------
cat("[2] validate_horizon_geometry() - checando profundidades\n")
geo <- validate_horizon_geometry(meu_pedon$horizons)
stopifnot(isTRUE(geo$valid))
cat("    geometria OK -", length(geo$errors), "erro(s),",
    length(geo$warnings), "aviso(s)\n\n")


# -----------------------------------------------------------------------------
# 3. Classificar nos tres sistemas (WRB 2022, SiBCS, USDA)
# -----------------------------------------------------------------------------
cat("[3] Classificando nos tres sistemas\n")
res_wrb   <- classify_wrb2022(meu_pedon, on_missing = "silent")
res_sibcs <- classify_sibcs(meu_pedon, include_familia = TRUE)
res_usda  <- classify_usda(meu_pedon, on_missing = "silent")

cat("    WRB 2022 :", res_wrb$name,   "\n")
cat("    SiBCS    :", res_sibcs$name, "\n")
cat("    USDA ST13:", res_usda$name,  "\n\n")

stopifnot(res_wrb$rsg_or_order == "Ferralsols")
stopifnot(grepl("Latossolos",     res_sibcs$name))
stopifnot(res_usda$rsg_or_order == "Oxisols")


# -----------------------------------------------------------------------------
# 4. Ler o "key trace" e a evidence grade (por que a chave decidiu isso?)
# -----------------------------------------------------------------------------
cat("[4] Evidence grade + trace deterministico\n")
cat("    evidence grade (WRB):", res_wrb$evidence_grade,
    " (A = tudo medido em laboratorio)\n")
cat("    RSG + qualifiers    :", res_wrb$name, "\n")
cat("    (o trace completo - por que cada diagnostico passou ou falhou -\n")
cat("     fica em res_wrb$trace, pronto para inspecao)\n\n")


# -----------------------------------------------------------------------------
# 5. Atalho: os tres sistemas numa chamada + tabela resumo
# -----------------------------------------------------------------------------
cat("[5] classify_all() - tudo de uma vez\n")
todos <- classify_all(meu_pedon, on_missing = "silent")
stopifnot(all(c("wrb", "sibcs", "usda", "summary") %in% names(todos)))
print(todos$summary)
cat("\n")


# -----------------------------------------------------------------------------
# 6. Proveniencia: preencher um valor e ver a evidence grade ficar honesta
# -----------------------------------------------------------------------------
cat("[6] Proveniencia por atributo (measured -> predicted_spectra)\n")
meu_pedon$add_measurement(
  horizon_idx = 4, attribute = "clay_pct", value = 62,
  source = "predicted_spectra", confidence = 0.85,
  notes = "Vis-NIR PLSR-local, biblioteca OSSL America do Sul",
  overwrite = TRUE
)
res_wrb_b <- classify_wrb2022(meu_pedon, on_missing = "silent")
cat("    evidence grade antes:", res_wrb$evidence_grade,
    " depois de preencher argila via espectro:", res_wrb_b$evidence_grade, "\n")
cat("    (o nome pode ser identico, mas a grade cai honestamente)\n\n")


# -----------------------------------------------------------------------------
# 7. Fixtures canonicas: perfis de referencia que ACOMPANHAM o pacote
# -----------------------------------------------------------------------------
cat("[7] Perfis canonicos de referencia (dados prontos no pacote)\n")
ferralsol <- make_ferralsol_canonical()   # Latossolo Vermelho canonico
vertisol  <- make_vertisol_canonical()    # Vertissolo canonico

cat("    Ferralsol  -> USDA:", classify_usda(ferralsol, on_missing="silent")$name, "\n")
cat("    Vertisol   -> WRB :", classify_wrb2022(vertisol, on_missing="silent")$rsg_or_order, "\n")

stopifnot(classify_usda(ferralsol, on_missing = "silent")$name == "Rhodic Hapludox")
stopifnot(classify_wrb2022(vertisol, on_missing = "silent")$rsg_or_order == "Vertisols")
cat("    (nomes de referencia conferem)\n\n")


# -----------------------------------------------------------------------------
# 8. Niveis mais profundos: familia USDA e familia SiBCS
# -----------------------------------------------------------------------------
cat("[8] Niveis mais profundos (familia)\n")
res_usda_fam  <- classify_usda(meu_pedon, on_missing = "silent", include_family = TRUE)
res_sibcs_fam <- classify_sibcs(meu_pedon, include_familia = TRUE)
cat("    USDA family :", res_usda_fam$name, "\n")
cat("    SiBCS familia:", res_sibcs_fam$name, "\n\n")


# -----------------------------------------------------------------------------
# 9. Gap-fill dentro do pedon (opcional, opt-in, nunca liga sozinho)
# -----------------------------------------------------------------------------
cat("[9] gapfill_within_pedon() - interpolar celulas NA no interior do perfil\n")
pedon_furado <- make_ferralsol_canonical()
pedon_furado$horizons$clay_pct[2] <- NA_real_          # criamos um buraco
preenchido <- gapfill_within_pedon(pedon_furado, attrs = "clay_pct")
cat("    argila do 2o horizonte: NA ->",
    round(preenchido$horizons$clay_pct[2], 1),
    "(interpolado, marcado como inferred_prior)\n\n")


# -----------------------------------------------------------------------------
# 10. Espectros Vis-NIR -> cor de Munsell (o motor espectral)
# -----------------------------------------------------------------------------
cat("[10] Espectros -> cor de Munsell (predict_munsell_from_spectra)\n")
comprimentos <- seq(380, 780, by = 5)
# um espectro avermelhado simples (refletancia sobe no vermelho)
refletancia  <- 0.06 + 0.40 * pmax(0, (comprimentos - 540) / 240)
cor <- predict_munsell_from_spectra(refletancia, wavelengths = comprimentos)
cat("    cor prevista:", cor$munsell_string, "\n")

# e no nivel do pedon: preencher Munsell faltante a partir dos espectros
pedon_spec <- make_synthetic_pedon_with_spectra(n_horizons = 3,
                                                wavelengths = seq(400, 2400, by = 10))
pedon_spec$horizons$munsell_hue_moist <- NA_character_
pedon_spec <- fill_munsell_from_spectra(pedon_spec, overwrite = TRUE, verbose = FALSE)
cat("    fill_munsell_from_spectra: preencheu",
    sum(!is.na(pedon_spec$horizons$munsell_hue_moist)), "horizonte(s)\n\n")


# -----------------------------------------------------------------------------
# 11. Correspondencia entre sistemas + cobertura da chave
# -----------------------------------------------------------------------------
cat("[11] Correspondencia entre sistemas e cobertura\n")
cat("    USDA Oxisols/Udox  ~ WRB:", usda_to_wrb_rsg("Oxisols", "Udox"), "\n")
cov <- coverage_report("usda_subgroup")
cat("    cobertura de subgrupos USDA:", cov$overall$covered_n, "/",
    cov$overall$canonical_n,
    sprintf("(%.1f%%)\n\n", 100 * cov$overall$covered_n / cov$overall$canonical_n))


# -----------------------------------------------------------------------------
# 12. Relatorio legivel (texto / HTML)
# -----------------------------------------------------------------------------
cat("[12] report() - relatorio legivel do resultado\n")
arquivo_html <- file.path(tempdir(), "relatorio_soilKey.html")
report(res_wrb, file = arquivo_html, format = "html", pedon = meu_pedon)
stopifnot(file.exists(arquivo_html))
cat("    relatorio HTML salvo em:", arquivo_html, "\n\n")


# -----------------------------------------------------------------------------
# Fim
# -----------------------------------------------------------------------------
cat("=============================================================\n")
cat("OK - todas as etapas rodaram e as verificacoes passaram.\n")
cat("Proximos passos: vignette('v01_getting_started_pt'),\n")
cat("                 vignette('v09_perfil_embrapa_pt'), ou run_classify_app().\n")
cat("=============================================================\n")
