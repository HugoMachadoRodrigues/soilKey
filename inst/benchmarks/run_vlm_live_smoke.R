# =============================================================================
# soilKey -- VLM live SMOKE test (Gemma 4 / Ollama, no real PDF needed)
#
# Self-contained smoke test that proves the end-to-end Gemma 4 path works:
# a synthetic but realistic PT-BR soil-description text is fed through the
# real VLM, the schema-validated JSON is parsed back, and the resulting
# horizons are written into a PedonRecord. Designed as a "does the
# pipeline empirically work?" gate to complement the unit tests
# (which use only MockVLMProvider).
#
# Usage:
#   Rscript inst/benchmarks/run_vlm_live_smoke.R
# =============================================================================

run_vlm_live_smoke <- function(provider = "auto",
                                  model    = NULL,
                                  out_dir  = file.path("inst", "benchmarks",
                                                       "reports"),
                                  verbose  = TRUE) {

  if (!requireNamespace("ellmer", quietly = TRUE))
    stop("install.packages('ellmer') is required for the live VLM smoke test")

  resolved <- if (identical(provider, "auto"))
                vlm_pick_provider(verbose = verbose) else provider
  prov <- vlm_provider(resolved, model = model)
  used_model <- model %||% default_model(resolved)

  # Synthetic PT-BR soil description that includes everything the
  # extraction prompt asks for: top/bottom, designation, Munsell,
  # texture (qualitative), structure, OC, plus a site sentence.
  description <- paste(c(
    "Perfil 042 - Latossolo Vermelho-Amarelo Distrofico, Seropedica, RJ.",
    "Coordenadas: 22 deg 45' S, 43 deg 41' W. Altitude 32 m. Material",
    "de origem: gnaisse intemperizado.",
    "",
    "A   0-15 cm; bruno-avermelhado-escuro (5YR 3/3 umido); franco-",
    "    argilo-arenosa; granular pequena moderada; friavel; transicao",
    "    plana e clara; raizes finas comuns; carbono organico 18 g/kg.",
    "",
    "BA  15-35 cm; vermelho-amarelado (5YR 4/6 umido); franco-argilosa;",
    "    blocos subangulares pequenos moderados; friavel; transicao",
    "    plana e gradual; raizes finas poucas; carbono organico 8 g/kg.",
    "",
    "Bw1 35-95 cm; vermelho (2.5YR 4/6 umido); muito argilosa;",
    "    granular media moderada; friavel; transicao plana e difusa;",
    "    raizes finas poucas; carbono organico 4 g/kg.",
    "",
    "Bw2 95-180+ cm; vermelho (2.5YR 4/8 umido); muito argilosa;",
    "    granular media moderada; friavel; raizes ausentes;",
    "    carbono organico 2 g/kg."
  ), collapse = "\n")

  # Fresh pedon to be populated by the extraction layer.
  pedon <- PedonRecord$new(site = list(id = paste0("vlm-smoke-", Sys.Date())))

  t0 <- Sys.time()
  pedon <- tryCatch(
    extract_horizons_from_pdf(pedon,
                                pdf_text = description,
                                provider = prov),
    error = function(e) {
      cat("[VLM-smoke] FAILED at extract_horizons_from_pdf:\n  ",
          conditionMessage(e), "\n", sep = "")
      list(error = conditionMessage(e), pedon = pedon)
    }
  )
  t_ext <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  if (is.list(pedon) && !is.null(pedon$error)) {
    return(invisible(list(ok = FALSE, error = pedon$error,
                              latency_s = t_ext)))
  }

  cls_wrb <- tryCatch(classify_wrb2022(pedon, on_missing = "silent"),
                        error = function(e) NULL)
  cls_sibcs <- tryCatch(classify_sibcs(pedon),
                          error = function(e) NULL)
  cls_usda <- tryCatch(classify_usda(pedon, on_missing = "silent"),
                         error = function(e) NULL)

  prov_tbl  <- pedon$provenance
  by_source <- if (is.null(prov_tbl) || nrow(prov_tbl) == 0)
                 data.frame(source = character(), n = integer())
               else
                 as.data.frame(table(source = prov_tbl$source))

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir,
                          sprintf("vlm_smoke_%s.md", Sys.Date()))
  writeLines(c(
    sprintf("# VLM live smoke test -- %s", Sys.Date()),
    "",
    sprintf("* Provider: **%s**", resolved),
    sprintf("* Model:    **%s**", used_model),
    sprintf("* Extraction latency: **%.1f s**", t_ext),
    sprintf("* Horizons extracted: **%d**", nrow(pedon$horizons %||% data.frame())),
    "",
    "## Provenance after extraction",
    "```",
    paste(capture.output(print(by_source)), collapse = "\n"),
    "```",
    "",
    "## Classification",
    sprintf("* WRB 2022:   **%s**",
              if (is.null(cls_wrb))   "(failed)" else cls_wrb$name),
    sprintf("* SiBCS 5:    **%s**",
              if (is.null(cls_sibcs)) "(failed)" else cls_sibcs$name),
    sprintf("* USDA ST 13: **%s**",
              if (is.null(cls_usda))  "(failed)" else cls_usda$name),
    "",
    "_End-to-end smoke test: real Gemma 4 / Ollama call -> schema-validated",
    "JSON -> populated PedonRecord -> deterministic key. Re-run on each",
    "release to verify the live VLM path is not regressed._"
  ), out_path)
  if (verbose) message(sprintf("[VLM-smoke] Report written to %s", out_path))

  invisible(list(
    ok          = TRUE,
    pedon       = pedon,
    cls_wrb     = cls_wrb,
    cls_sibcs   = cls_sibcs,
    cls_usda    = cls_usda,
    latency_s   = t_ext,
    by_source   = by_source,
    report_path = out_path
  ))
}


if (!interactive() && identical(commandArgs(trailingOnly = TRUE), character(0))) {
  pkgload::load_all(quiet = TRUE)
  res <- run_vlm_live_smoke()
  if (isTRUE(res$ok)) {
    cat("\n[VLM-smoke] OK\n")
    cat(sprintf("  Latency:   %.1f s\n", res$latency_s))
    cat(sprintf("  Report:    %s\n", res$report_path))
    cat(sprintf("  Horizons:  %d\n", nrow(res$pedon$horizons %||% data.frame())))
    cat(sprintf("  WRB name:  %s\n",
                  if (is.null(res$cls_wrb)) "(none)" else res$cls_wrb$name))
  } else {
    cat("\n[VLM-smoke] FAILED:", res$error, "\n")
    quit(status = 1L)
  }
}
