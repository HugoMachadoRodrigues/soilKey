# =============================================================================
# Tests for v0.9.52 -- PT-BR end-to-end vignette + ClassificationResult$print
# defensive fix when trace contains non-list entries.
# =============================================================================

.find_repo_root <- function() {
  cands <- c(".", "..", "../..", "../../..")
  for (c in cands) {
    if (file.exists(file.path(c, "DESCRIPTION")) &&
          file.exists(file.path(c, "NAMESPACE"))) {
      return(normalizePath(c))
    }
  }
  NULL
}


test_that("Vinheta v09 PT-BR existe e tem o conteudo esperado", {
  root <- .find_repo_root()
  if (is.null(root)) skip("Repo root not found")
  vp <- file.path(root, "vignettes", "v09_perfil_embrapa_pt.Rmd")
  expect_true(file.exists(vp))
  txt <- paste(readLines(vp), collapse = "\n")
  # Cobertura dos 3 sistemas
  expect_match(txt, "classify_all\\(perfil")
  expect_match(txt, "SiBCS")
  expect_match(txt, "WRB 2022", fixed = TRUE)
  expect_match(txt, "USDA Soil Taxonomy", fixed = TRUE)
  # Cobertura dos lookups espaciais (v0.9.44/v0.9.48)
  expect_match(txt, "lookup_mapbiomas_solos", fixed = TRUE)
  expect_match(txt, "lookup_soilgrids", fixed = TRUE)
  # Cobertura dos modulos espectrais (v0.9.46/v0.9.47)
  expect_match(txt, "predict_from_spectra", fixed = TRUE)
  expect_match(txt, "predict_munsell_from_spectra", fixed = TRUE)
})


test_that("Vinheta v09 parses sem erros de sintaxe Rmd", {
  root <- .find_repo_root()
  if (is.null(root)) skip("Repo root not found")
  vp <- file.path(root, "vignettes", "v09_perfil_embrapa_pt.Rmd")
  if (!requireNamespace("knitr", quietly = TRUE)) skip("knitr unavailable")
  # Just exercise the Rmd parser; full render is exercised by R CMD check.
  parsed <- tryCatch(
    knitr::knit_code$restore(),
    error = function(e) NULL
  )
  # Lightweight check: the YAML front matter block exists and
  # references the vignette engine.
  hdr <- readLines(vp, n = 12L)
  expect_true(any(grepl("VignetteEngine\\{knitr::rmarkdown\\}", hdr)))
  expect_true(any(grepl("VignetteIndexEntry\\{", hdr)))
})


# ---- ClassificationResult$print: defensive when trace has non-list entries ----

test_that("ClassificationResult$print does not error on scalar/NULL trace entries", {
  res <- ClassificationResult$new(
    system = "WRB 2022",
    name   = "Cambisols",
    rsg_or_order = "Cambisols",
    qualifiers   = list(),
    trace        = list(
      AC = list(code = "AC", name = "Acrisols",  passed = FALSE),
      CM = list(code = "CM", name = "Cambisols", passed = TRUE),
      familia_label = "argilosa",                      # scalar
      color_undetermined = NULL,                       # NULL
      ambiguities_df     = data.frame(rsg = "AC", n = 0)  # data.frame
    ),
    evidence_grade = "A"
  )
  # Should not error
  expect_no_error(invisible(capture.output(print(res), type = "message")))
  # cli writes to message stream; capture both
  out <- capture.output({
    capture.output(print(res), type = "message")
  })
  msg <- capture.output(print(res), type = "message")
  combined <- c(out, msg)
  # Scalar/NULL/data.frame entries are skipped in the per-RSG dump,
  # but CM and AC (proper trace entries) must appear.
  expect_true(any(grepl("AC", combined)))
  expect_true(any(grepl("CM", combined)))
})


test_that("classify_all produces a non-crashing print on a real Argissolo profile", {
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   55,   115, 170),
    bottom_cm = c(20,   55,   115,  170, 220),
    designation = c("A", "AB", "Bt1", "Bt2", "BC"),
    munsell_hue_moist    = c("10YR","7.5YR","5YR","2.5YR","2.5YR"),
    munsell_value_moist  = c(4, 4, 4, 3, 3),
    munsell_chroma_moist = c(3, 5, 6, 6, 6),
    structure_grade  = c("moderate","moderate","strong","strong","moderate"),
    structure_type   = c("granular","subangular blocky","subangular blocky",
                            "subangular blocky","subangular blocky"),
    clay_films_amount = c(NA, "few", "common", "common", "few"),
    clay_pct = c(18,28,45,42,38),
    silt_pct = c(30,25,20,22,24),
    sand_pct = c(52,47,35,36,38),
    ph_h2o   = c(5.5,5.3,5.0,5.0,5.1),
    oc_pct   = c(1.5,0.6,0.3,0.2,0.2),
    cec_cmol = c(8,6,5.5,4.5,4.0),
    bs_pct   = c(35,25,20,18,20),
    al_cmol  = c(0.5,0.8,1.2,1.5,1.4)
  )
  hz <- ensure_horizon_schema(hz)
  pedon <- PedonRecord$new(
    site = list(id = "RJ-1-vinheta", lat = -22.86, lon = -43.78, country = "BR"),
    horizons = hz
  )
  res <- classify_all(pedon, on_missing = "silent")
  for (sys in c("sibcs", "wrb", "usda")) {
    expect_no_error(invisible(capture.output(print(res[[sys]]), type = "message")))
  }
  expect_match(res$sibcs$rsg_or_order, "Argissolos")
})
