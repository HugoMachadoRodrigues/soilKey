suppressMessages(suppressWarnings({pkgload::load_all(".", quiet=TRUE, helpers=FALSE)}))

cat("==============================================================\n")
cat("v0.9.87 cumulative benchmark sweep (post v0.9.86 stack)\n")
cat("==============================================================\n\n")

DATE <- format(Sys.Date(), "%Y-%m-%d")

# ---- Datasets we can hit cheaply --------------------------------------

cat("---- 1. BDsolos RJ (n=722 with 114 Lat / 232 Arg / 90 Cam / 270 Neo) ----\n")
RJ_PATH <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos/RJ.csv"
if (file.exists(RJ_PATH)) {
  peds_rj <- suppressMessages(suppressWarnings(load_bdsolos_csv(RJ_PATH, verbose = FALSE)))
  cat(sprintf("Loaded %d BDsolos RJ pedons.\n\n", length(peds_rj)))

  for (label in c("default canonical", "engine=aqp")) {
    opts <- if (label == "default canonical") list() else list(soilKey.diagnostic_engine = "aqp")
    res <- withr::with_options(opts, {
      suppressMessages(suppressWarnings(
        benchmark_bdsolos(peds_rj, systems = c("sibcs"), verbose = FALSE)))
    })
    sib <- res$per_system$sibcs
    cat(sprintf("  [%s] SiBCS Order accuracy = %.1f%% (%d / %d in_scope)\n",
                label, 100 * sib$accuracy,
                round(sib$accuracy * sib$n_compared), sib$n_compared))
    cf <- sib$confusion
    if ("Latossolos" %in% rownames(cf)) {
      cat(sprintf("    Latossolo recall: %d / %d (%.1f%%)\n",
                  cf["Latossolos","Latossolos"], sum(cf["Latossolos",]),
                  100 * cf["Latossolos","Latossolos"] / max(sum(cf["Latossolos",]), 1L)))
    }
  }
}

cat("\n---- 2. Redape (94 SiBCS, 4 levels) ----\n")
RED_DIR <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/redape_geotab"
if (dir.exists(RED_DIR)) {
  peds_red <- suppressMessages(suppressWarnings(load_redape_pedons(RED_DIR, verbose = FALSE)))
  cat(sprintf("Loaded %d Redape pedons.\n\n", length(peds_red)))
  for (label in c("default canonical", "engine=aqp + opt-ins")) {
    opts <- if (label == "default canonical") list() else list(
      soilKey.diagnostic_engine = "aqp",
      soilKey.gleyic_designation_inference = TRUE,
      soilKey.ferralic_texture_morphological_fallback = TRUE
    )
    cat(sprintf("  [%s]\n", label))
    for (lvl in c("order","subordem","gde_grupo","subgrupo")) {
      r <- withr::with_options(opts, {
        suppressMessages(suppressWarnings(benchmark_redape(peds_red, level = lvl, verbose = FALSE)))
      })
      cat(sprintf("    level=%-9s acc=%5.1f%% (%d / %d)\n",
                  lvl, 100 * r$accuracy,
                  round(r$accuracy * r$n_compared), r$n_compared))
    }
  }
}

cat("\n---- 3. KSSL+NASIS (n=99) ----\n")
# v0.9.91+: must use load_kssl_nasis_sample() so the
# reference_wrb_from_usda -> reference_wrb alias is applied.
s <- tryCatch(load_kssl_nasis_sample(), error = function(e) NULL)
if (!is.null(s)) {
  peds_ks <- s$pedons %||% s
  cat(sprintf("Loaded %d KSSL+NASIS pedons.\n\n", length(peds_ks)))
  for (label in c("default", "engine=aqp", "engine=aqp + andic_proxy + spodic_engine_aware")) {
    opts <- switch(label,
      "default" = list(),
      "engine=aqp" = list(soilKey.diagnostic_engine = "aqp"),
      "engine=aqp + andic_proxy + spodic_engine_aware" = list(
        soilKey.diagnostic_engine = "aqp",
        soilKey.andic_oc_bd_proxy = TRUE,
        soilKey.andic_oc_bd_proxy_extend = TRUE
      )
    )
    correct <- 0L; n <- 0L
    withr::with_options(opts, {
      for (p in peds_ks) {
        ref <- p$site$reference_wrb %||% NA_character_
        if (is.na(ref) || !nzchar(ref)) next
        cls <- tryCatch(classify_wrb2022(p, on_missing = "silent"), error = function(e) NULL)
        pred <- if (!is.null(cls)) cls$rsg_or_order %||% NA_character_ else NA_character_
        ref_norm <- normalise_febr_wrb(ref)
        n <- n + 1L
        if (!is.na(pred) && !is.na(ref_norm) && pred == ref_norm) correct <- correct + 1L
      }
    })
    cat(sprintf("  [%-50s] WRB acc = %.1f%% (%d / %d)\n",
                label, 100 * correct / max(n, 1L), correct, n))
  }
}

cat("\n---- 4. AfSP (n=120) ----\n")
fp <- system.file("extdata", "afsp_sample.rds", package = "soilKey")
if (!nzchar(fp)) fp <- "inst/extdata/afsp_sample.rds"
if (file.exists(fp)) {
  s <- readRDS(fp)
  peds_af <- s$pedons %||% s
  cat(sprintf("Loaded %d AfSP pedons.\n\n", length(peds_af)))
  for (label in c("default", "engine=aqp + andic_proxy + extend")) {
    opts <- switch(label,
      "default" = list(),
      "engine=aqp + andic_proxy + extend" = list(
        soilKey.diagnostic_engine = "aqp",
        soilKey.andic_oc_bd_proxy = TRUE,
        soilKey.andic_oc_bd_proxy_extend = TRUE,
        soilKey.gleyic_designation_inference = TRUE
      )
    )
    correct <- 0L; n <- 0L
    withr::with_options(opts, {
      for (p in peds_af) {
        ref <- p$site$reference_wrb %||% NA_character_
        if (is.na(ref) || !nzchar(ref)) next
        cls <- tryCatch(classify_wrb2022(p, on_missing = "silent"), error = function(e) NULL)
        pred <- if (!is.null(cls)) cls$rsg_or_order %||% NA_character_ else NA_character_
        ref_norm <- normalise_febr_wrb(ref)
        n <- n + 1L
        if (!is.na(pred) && !is.na(ref_norm) && pred == ref_norm) correct <- correct + 1L
      }
    })
    cat(sprintf("  [%-50s] WRB acc = %.1f%% (%d / %d)\n",
                label, 100 * correct / max(n, 1L), correct, n))
  }
}

cat("\n---- 5. WoSIS stratified (n=130) ----\n")
# v0.9.91+: must use load_wosis_stratified_sample() so the
# wosis_rsg -> reference_wrb alias is applied. Reading the RDS
# directly bypasses the alias and benchmark loops report 0/0
# because reference_wrb is NULL on every pedon.
s <- tryCatch(load_wosis_stratified_sample(), error = function(e) NULL)
if (!is.null(s)) {
  peds_w <- s$pedons %||% s
  cat(sprintf("Loaded %d WoSIS stratified pedons.\n\n", length(peds_w)))
  for (label in c("default", "engine=aqp + opt-ins")) {
    opts <- if (label == "default") list() else list(
      soilKey.diagnostic_engine = "aqp",
      soilKey.gleyic_designation_inference = TRUE,
      soilKey.andic_oc_bd_proxy = TRUE
    )
    correct <- 0L; n <- 0L
    withr::with_options(opts, {
      for (p in peds_w) {
        ref <- p$site$reference_wrb %||% NA_character_
        if (is.na(ref) || !nzchar(ref)) next
        cls <- tryCatch(classify_wrb2022(p, on_missing = "silent"), error = function(e) NULL)
        pred <- if (!is.null(cls)) cls$rsg_or_order %||% NA_character_ else NA_character_
        ref_norm <- normalise_febr_wrb(ref)
        n <- n + 1L
        if (!is.na(pred) && !is.na(ref_norm) && pred == ref_norm) correct <- correct + 1L
      }
    })
    cat(sprintf("  [%-30s] WRB acc = %.1f%% (%d / %d)\n",
                label, 100 * correct / max(n, 1L), correct, n))
  }
}

cat("\n[v0.9.87 sweep] DONE\n")
