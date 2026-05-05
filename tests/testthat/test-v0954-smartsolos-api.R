# =============================================================================
# Tests for v0.9.54 -- classify_via_smartsolos_api() + compare_smartsolos().
#
# All HTTP work is bypassed via the post_fn injection so tests run offline.
# An opt-in live test is gated by Sys.getenv("AGROAPI_TOKEN") +
# Sys.getenv("SOILKEY_NETWORK_TESTS") so users can validate against the
# real Embrapa endpoint when they have credentials.
# =============================================================================


.make_argissolo_for_smartsolos <- function() {
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   55,   115),
    bottom_cm = c(20,   55,   115,  170),
    designation          = c("A","AB","Bt","Bt2"),
    munsell_hue_moist    = c("10YR","7.5YR","5YR","2.5YR"),
    munsell_value_moist  = c(4,4,4,3),
    munsell_chroma_moist = c(3,5,6,6),
    structure_grade      = c("moderate","moderate","strong","strong"),
    structure_type       = c("granular","subangular blocky",
                                "subangular blocky","subangular blocky"),
    structure_size       = c("medium","medium","medium","medium"),
    clay_films_amount    = c(NA, "few", "common", "common"),
    clay_films_strength  = c(NA, "weak", "moderate", "moderate"),
    clay_pct = c(18, 28, 45, 42),
    silt_pct = c(30, 25, 20, 22),
    sand_pct = c(52, 47, 35, 36),
    ph_h2o   = c(5.5, 5.3, 5.0, 5.0),
    ph_kcl   = c(4.4, 4.3, 4.1, 4.1),
    oc_pct   = c(1.5, 0.6, 0.3, 0.2),
    ca_cmol  = c(2.0, 1.4, 1.0, 0.7),
    mg_cmol  = c(0.6, 0.4, 0.3, 0.2),
    k_cmol   = c(0.10, 0.06, 0.04, 0.03),
    na_cmol  = c(0.02, 0.02, 0.02, 0.02),
    al_cmol  = c(0.5, 0.8, 1.2, 1.5),
    p_mehlich3_mg_kg = c(15L, 8L, 4L, 3L)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(
    site = list(id = "RJ-test", lat = -22.86, lon = -43.78,
                  country = "BR"),
    horizons = hz
  )
}


# ---- Mapping helpers ----------------------------------------------------

test_that(".smartsolos_struct_grade maps PT/EN strings to 1..3", {
  expect_equal(soilKey:::.smartsolos_struct_grade("weak"),     1L)
  expect_equal(soilKey:::.smartsolos_struct_grade("Fraca"),    1L)
  expect_equal(soilKey:::.smartsolos_struct_grade("moderate"), 2L)
  expect_equal(soilKey:::.smartsolos_struct_grade("forte"),    3L)
  expect_true(is.na(soilKey:::.smartsolos_struct_grade(NA)))
  expect_true(is.na(soilKey:::.smartsolos_struct_grade("???")))
})


test_that(".smartsolos_struct_type recognises subangular before angular", {
  # 'subangular blocky' must match SUBANGULAR (3), not ANGULAR (2).
  expect_equal(soilKey:::.smartsolos_struct_type("subangular blocky"), 3L)
  expect_equal(soilKey:::.smartsolos_struct_type("angular blocky"),    2L)
  expect_equal(soilKey:::.smartsolos_struct_type("granular"),          1L)
  expect_equal(soilKey:::.smartsolos_struct_type("prismatic"),         4L)
  expect_equal(soilKey:::.smartsolos_struct_type("colunar"),           5L)
  expect_equal(soilKey:::.smartsolos_struct_type("laminar"),           6L)
})


test_that(".smartsolos_clay_films_amt + strength map few/common/many", {
  expect_equal(soilKey:::.smartsolos_clay_films_amt("few"),       1L)
  expect_equal(soilKey:::.smartsolos_clay_films_amt("common"),    2L)
  expect_equal(soilKey:::.smartsolos_clay_films_amt("muitas"),    3L)
  expect_equal(soilKey:::.smartsolos_clay_films_strength("weak"),     1L)
  expect_equal(soilKey:::.smartsolos_clay_films_strength("moderada"), 2L)
  expect_equal(soilKey:::.smartsolos_clay_films_strength("forte"),    3L)
})


# ---- Payload builder ----------------------------------------------------

test_that(".smartsolos_pedon_to_payload produces the documented schema", {
  p <- .make_argissolo_for_smartsolos()
  pl <- soilKey:::.smartsolos_pedon_to_payload(p, drenagem = "bem drenado")
  expect_named(pl, "items")
  expect_length(pl$items, 1L)
  it <- pl$items[[1L]]
  expect_equal(it$ID_PONTO, "RJ-test")
  expect_equal(it$DRENAGEM, 4L)
  expect_length(it$HORIZONTES, 4L)
  hz1 <- it$HORIZONTES[[1L]]
  expected_keys <- c("SIMB_HORIZ", "LIMITE_SUP", "LIMITE_INF",
                       "COR_UMIDA_MATIZ", "COR_UMIDA_VALOR",
                       "COR_UMIDA_CROMA", "COR_SECA_MATIZ",
                       "COR_SECA_VALOR", "COR_SECA_CROMA",
                       "ESTRUTURA_GRAU", "ESTRUTURA_TAMANHO",
                       "ESTRUTURA_TIPO", "CEROSIDADE_GRAU",
                       "CEROSIDADE_QUANTIDADE", "CONSISTENCIA_SECO",
                       "AREIA_GROS", "AREIA_FINA", "SILTE", "ARGILA",
                       "PH_AGUA", "PH_KCL", "C_ORG", "CA_TROC", "MG_TROC",
                       "K_TROC", "NA_TROC", "AL_TROC", "H_TROC", "P_ASSIM")
  expect_setequal(names(hz1), expected_keys)
})


test_that(".smartsolos_pedon_to_payload converts units (% -> g/kg) correctly", {
  p <- .make_argissolo_for_smartsolos()
  pl <- soilKey:::.smartsolos_pedon_to_payload(p)
  it <- pl$items[[1L]]
  hz1 <- it$HORIZONTES[[1L]]   # row 1: clay_pct = 18, oc_pct = 1.5
  expect_equal(hz1$ARGILA, 180L)         # 18% -> 180 g/kg
  expect_equal(hz1$SILTE,  300L)         # 30% -> 300 g/kg
  # Sand split 50/50 -> 520 / 2 = 260 each (rounded)
  expect_equal(hz1$AREIA_GROS + hz1$AREIA_FINA, 520L)
  expect_equal(hz1$C_ORG,  15)           # 1.5% -> 15 g/kg
  # pH unchanged
  expect_equal(hz1$PH_AGUA, 5.5)
  expect_equal(hz1$PH_KCL,  4.4)
})


test_that(".smartsolos_pedon_to_payload accepts integer drenagem", {
  p <- .make_argissolo_for_smartsolos()
  pl <- soilKey:::.smartsolos_pedon_to_payload(p, drenagem = 7)
  expect_equal(pl$items[[1L]]$DRENAGEM, 7L)
})


test_that(".smartsolos_pedon_to_payload encodes subangular blocky correctly", {
  p <- .make_argissolo_for_smartsolos()
  pl <- soilKey:::.smartsolos_pedon_to_payload(p)
  hz3 <- pl$items[[1L]]$HORIZONTES[[3L]]
  expect_equal(hz3$ESTRUTURA_TIPO, 3L)   # subangular blocky -> 3
  expect_equal(hz3$ESTRUTURA_GRAU, 3L)   # strong -> 3
})


# ---- Response parser ---------------------------------------------------

test_that(".smartsolos_response_to_result builds a ClassificationResult", {
  fake <- list(items = list(list(
    ID_PONTO  = "RJ-test",
    ORDEM     = "ARGISSOLO",
    SUBORDEM  = "VERMELHO",
    GDE_GRUPO = "Distrofico",
    SUBGRUPO  = "tipico"
  )))
  p <- .make_argissolo_for_smartsolos()
  res <- soilKey:::.smartsolos_response_to_result(fake, p, "classification")
  expect_s3_class(res, "ClassificationResult")
  expect_equal(res$rsg_or_order, "ARGISSOLO")
  expect_equal(res$qualifiers$subordem,  "VERMELHO")
  expect_equal(res$qualifiers$gde_grupo, "Distrofico")
  expect_equal(res$qualifiers$subgrupo,  "tipico")
  expect_equal(res$evidence_grade, "B")
  expect_match(res$name, "ARGISSOLO VERMELHO Distrofico tipico", fixed = TRUE)
})


# ---- classify_via_smartsolos_api: stubbed ------------------------------

test_that("classify_via_smartsolos_api routes through post_fn injection", {
  p <- .make_argissolo_for_smartsolos()
  captured_payload <- NULL
  stub <- function(payload) {
    captured_payload <<- payload
    list(items = list(list(
      ID_PONTO  = payload$items[[1]]$ID_PONTO,
      ORDEM     = "ARGISSOLO",
      SUBORDEM  = "VERMELHO",
      GDE_GRUPO = "Distrofico",
      SUBGRUPO  = "tipico"
    )))
  }
  res <- classify_via_smartsolos_api(p, post_fn = stub, verbose = FALSE)
  expect_s3_class(res, "ClassificationResult")
  expect_equal(res$rsg_or_order, "ARGISSOLO")
  # Payload was sent
  expect_false(is.null(captured_payload))
  expect_equal(captured_payload$items[[1]]$ID_PONTO, "RJ-test")
})


test_that("classify_via_smartsolos_api errors without an API token (and no stub)", {
  p <- .make_argissolo_for_smartsolos()
  withr::local_envvar(AGROAPI_TOKEN = "")
  expect_error(
    classify_via_smartsolos_api(p, api_key = "", verbose = FALSE),
    "API token"
  )
})


test_that("classify_via_smartsolos_api validates pedon", {
  expect_error(classify_via_smartsolos_api(list()),
                "PedonRecord")
})


# ---- Verification endpoint ---------------------------------------------

test_that("classify_via_smartsolos_api supports endpoint='verification'", {
  p <- .make_argissolo_for_smartsolos()
  stub <- function(payload) {
    list(
      items = list(list(
        ID_PONTO  = payload$items[[1]]$ID_PONTO,
        ORDEM     = "ARGISSOLO",
        SUBORDEM  = "VERMELHO",
        GDE_GRUPO = "Distrofico",
        SUBGRUPO  = "tipico"
      )),
      items_bd = list(list(
        ID_PONTO  = payload$items[[1]]$ID_PONTO,
        ORDEM     = "ARGISSOLO",
        SUBORDEM  = "AMARELO",  # mismatch
        GDE_GRUPO = "Distrofico",
        SUBGRUPO  = "tipico"
      )),
      summary = list(L0 = 0L, L1 = 1L, L2 = 0L, L3 = 0L, L4 = 0L)
    )
  }
  res <- classify_via_smartsolos_api(p,
                                       endpoint = "verification",
                                       reference_sibcs = list(
                                         ordem    = "ARGISSOLO",
                                         subordem = "AMARELO",
                                         gde_grupo = "Distrofico",
                                         subgrupo = "tipico"),
                                       post_fn = stub, verbose = FALSE)
  expect_equal(res$rsg_or_order, "ARGISSOLO")
  expect_equal(res$trace$smartsolos_endpoint, "verification")
  expect_false(is.null(res$trace$smartsolos_user_reference))
  expect_equal(res$trace$smartsolos_summary$L1, 1L)
})


# ---- compare_smartsolos -------------------------------------------------

test_that("compare_smartsolos returns local + remote + agreement", {
  p <- .make_argissolo_for_smartsolos()
  stub <- function(payload) {
    list(items = list(list(
      ID_PONTO  = payload$items[[1]]$ID_PONTO,
      ORDEM     = "ARGISSOLO",
      SUBORDEM  = "VERMELHO",
      GDE_GRUPO = "Distrofico",
      SUBGRUPO  = "tipico"
    )))
  }
  cmp <- compare_smartsolos(p, post_fn = stub, verbose = FALSE)
  expect_named(cmp, c("local", "remote", "agreement"))
  expect_s3_class(cmp$local,  "ClassificationResult")
  expect_s3_class(cmp$remote, "ClassificationResult")
  expect_s3_class(cmp$agreement, "data.frame")
  expect_named(cmp$agreement,
                c("point_id", "ordem", "subordem", "gde_grupo",
                  "subgrupo", "n_match"))
  # Both should agree at the Ordem level (both Argissolo)
  expect_true(isTRUE(cmp$agreement$ordem))
})


# ---- Live network test (opt-in) ----------------------------------------

test_that("classify_via_smartsolos_api hits the real endpoint when token is set", {
  if (!nzchar(Sys.getenv("AGROAPI_TOKEN")) ||
        !nzchar(Sys.getenv("SOILKEY_NETWORK_TESTS"))) {
    skip("Live test gated by AGROAPI_TOKEN + SOILKEY_NETWORK_TESTS env vars")
  }
  testthat::skip_if_not_installed("httr")
  testthat::skip_if_not_installed("jsonlite")
  p <- .make_argissolo_for_smartsolos()
  res <- classify_via_smartsolos_api(p, verbose = FALSE)
  expect_s3_class(res, "ClassificationResult")
  expect_true(nzchar(res$rsg_or_order %||% ""))
})
