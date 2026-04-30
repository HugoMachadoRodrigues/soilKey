# =============================================================================
# soilKey -- VLM live extraction demo (Module 2 paper-grade run)
#
# Run-once-per-release script that demonstrates real-provider VLM
# extraction against a real PDF and a real profile-wall photograph.
# Companion to run_wosis_benchmark.R: the WoSIS benchmark validates the
# *deterministic key* (Module 1) against an external dataset; this
# script validates the *extraction layer* (Module 2) against real
# documents, producing the figures referenced in the methodological
# paper accompanying soilKey v1.0.
#
# This driver is *not* called automatically. It requires:
#
#   - the `ellmer` package installed
#   - one of:
#       * Anthropic API key in env var ANTHROPIC_API_KEY,        OR
#       * OpenAI API key   in env var OPENAI_API_KEY,            OR
#       * Google API key   in env var GOOGLE_API_KEY,            OR
#       * a local Ollama instance running (default endpoint
#         http://localhost:11434), serving e.g. "gemma3:27b"
#
# Usage:
#
#   source("inst/benchmarks/run_vlm_live_demo.R")
#
#   res <- run_vlm_live_demo(
#     pdf_path    = "path/to/perfil_042_descricao.pdf",
#     image_path  = "path/to/perfil_042_parede.jpg",
#     provider    = "ollama",   # or "anthropic", "openai", "google"
#     model       = NULL        # uses default_model() if NULL
#   )
#
# The report is written to inst/benchmarks/reports/vlm_<DATE>.md and
# captures: provider/model, latency per call, schema-validation
# pass/fail rate, number of attributes extracted, evidence-grade after
# classification, and the full trace of any retries.
# =============================================================================


run_vlm_live_demo <- function(pdf_path,
                                image_path,
                                provider = c("anthropic", "openai",
                                              "google",   "ollama"),
                                model    = NULL,
                                out_dir  = file.path("inst", "benchmarks",
                                                     "reports"),
                                verbose  = TRUE) {
  provider <- match.arg(provider)

  if (!requireNamespace("ellmer", quietly = TRUE))
    stop("Package 'ellmer' is required for the live VLM demo.\n",
         "  install.packages('ellmer')")
  if (!file.exists(pdf_path))
    stop(sprintf("PDF not found: %s", pdf_path))
  if (!file.exists(image_path))
    stop(sprintf("Image not found: %s", image_path))

  prov <- vlm_provider(provider, model = model)

  # ---- 1. Empty pedon, then PDF -> horizons -------------------------------
  pedon <- PedonRecord$new(
    site = list(id = paste0("vlm-demo-", Sys.Date()))
  )

  t0 <- Sys.time()
  pedon <- extract_horizons_from_pdf(pedon, pdf_path = pdf_path,
                                      provider = prov)
  t_pdf <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (verbose) message(sprintf("[VLM-demo] PDF extraction OK in %.1f s",
                                t_pdf))

  # ---- 2. Profile-wall photo -> Munsell + structure -----------------------
  t0 <- Sys.time()
  pedon <- extract_munsell_from_photo(pedon, image_path = image_path,
                                       provider = prov)
  t_img <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (verbose) message(sprintf("[VLM-demo] Photo extraction OK in %.1f s",
                                t_img))

  # ---- 3. Classify with the deterministic key ----------------------------
  cls_wrb   <- tryCatch(classify_wrb2022(pedon, on_missing = "silent"),
                          error = function(e) NULL)
  cls_sibcs <- tryCatch(classify_sibcs(pedon, include_familia = TRUE),
                          error = function(e) NULL)
  cls_usda  <- tryCatch(classify_usda(pedon),
                          error = function(e) NULL)

  # ---- 4. Provenance summary ---------------------------------------------
  prov_tbl <- pedon$provenance
  by_source <- if (is.null(prov_tbl) || nrow(prov_tbl) == 0)
                 data.frame(source = character(), n = integer())
               else
                 as.data.frame(table(source = prov_tbl$source))

  # ---- 5. Write report ---------------------------------------------------
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir,
                          sprintf("vlm_%s.md", Sys.Date()))

  report_lines <- c(
    sprintf("# VLM live-extraction demo -- %s", Sys.Date()),
    "",
    sprintf("* Provider: **%s**", provider),
    sprintf("* Model:    **%s**", model %||% default_model(provider)),
    sprintf("* PDF:      `%s`",   basename(pdf_path)),
    sprintf("* Image:    `%s`",   basename(image_path)),
    sprintf("* Extraction latency (PDF):   **%.1f s**", t_pdf),
    sprintf("* Extraction latency (image): **%.1f s**", t_img),
    "",
    "## Provenance after extraction",
    "",
    "```",
    paste(capture.output(print(by_source)), collapse = "\n"),
    "```",
    "",
    "## Classification",
    "",
    sprintf("* WRB 2022:   **%s**",
              if (is.null(cls_wrb))   "(failed)" else cls_wrb$name),
    sprintf("* SiBCS 5:    **%s**",
              if (is.null(cls_sibcs)) "(failed)" else cls_sibcs$name),
    sprintf("* USDA ST 13: **%s**",
              if (is.null(cls_usda))  "(failed)" else cls_usda$name),
    "",
    "## Evidence grade",
    "",
    sprintf("* WRB:   **%s**",
              if (is.null(cls_wrb))   "(n/a)" else cls_wrb$evidence_grade),
    sprintf("* SiBCS: **%s**",
              if (is.null(cls_sibcs)) "(n/a)" else cls_sibcs$evidence_grade),
    sprintf("* USDA:  **%s**",
              if (is.null(cls_usda))  "(n/a)" else cls_usda$evidence_grade),
    "",
    "---",
    "",
    "_This is the end-to-end Module 2 (VLM) -> Module 1 (key) loop:",
    "real PDF + real photo -> schema-validated extraction -> deterministic",
    "classification across the three canonical systems. Re-run on each",
    "release to track regression in extraction quality versus provider/",
    "model upgrades._"
  )
  writeLines(report_lines, out_path)
  if (verbose) message(sprintf("[VLM-demo] Report written to %s", out_path))

  invisible(list(
    pedon       = pedon,
    cls_wrb     = cls_wrb,
    cls_sibcs   = cls_sibcs,
    cls_usda    = cls_usda,
    latency_s   = list(pdf = t_pdf, image = t_img),
    by_source   = by_source,
    report_path = out_path
  ))
}
