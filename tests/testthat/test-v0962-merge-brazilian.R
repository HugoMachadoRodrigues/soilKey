# =============================================================================
# Tests for v0.9.62 -- merge_brazilian_pedons() and
#                       summarize_brazilian_overlap().
# =============================================================================


# ---- helper ---------------------------------------------------------------

.make_brz_pedon <- function(id, sisb = id, source = "Embrapa BDsolos",
                              ref_sibcs = "ARGISSOLO") {
  hz <- data.table::data.table(
    top_cm = c(0, 20),
    bottom_cm = c(20, 60),
    designation = c("A", "Bt"),
    munsell_hue_moist = c("10YR", "5YR"),
    munsell_value_moist = c(4, 4),
    munsell_chroma_moist = c(3, 6),
    clay_pct = c(20, 40)
  )
  PedonRecord$new(
    site = list(
      id      = as.character(id),
      sisb_id = if (is.na(sisb)) NA_character_ else as.character(sisb),
      country = "BR",
      reference_sibcs  = ref_sibcs,
      reference_source = source
    ),
    horizons = ensure_horizon_schema(hz)
  )
}


# ---- .get_sisb_id ---------------------------------------------------------

test_that(".get_sisb_id returns trimmed character or NA", {
  expect_equal(soilKey:::.get_sisb_id(.make_brz_pedon("100")), "100")
  expect_true(is.na(soilKey:::.get_sisb_id(
    .make_brz_pedon("100", sisb = NA)
  )))
  expect_true(is.na(soilKey:::.get_sisb_id(NULL)))
})


# ---- merge_brazilian_pedons ----------------------------------------------

test_that("merge_brazilian_pedons drops shared sisb_id, prefer = bdsolos", {
  bd <- list(.make_brz_pedon("100", source = "BD"),
              .make_brz_pedon("200", source = "BD"))
  fb <- list(.make_brz_pedon("100", source = "FE"),  # duplicate
              .make_brz_pedon("300", source = "FE"))
  m <- merge_brazilian_pedons(bd, fb, prefer = "bdsolos", verbose = FALSE)
  expect_equal(length(m), 3L)  # 100 (BD) + 200 (BD) + 300 (FE)
  shared <- Filter(function(p) identical(p$site$sisb_id, "100"), m)
  expect_length(shared, 1L)
  expect_equal(shared[[1L]]$site$merge_decision, "kept_bdsolos")
  expect_equal(shared[[1L]]$site$merge_source,   "BDsolos")
})


test_that("merge_brazilian_pedons drops shared sisb_id, prefer = febr", {
  bd <- list(.make_brz_pedon("100", source = "BD"))
  fb <- list(.make_brz_pedon("100", source = "FE"))
  m <- merge_brazilian_pedons(bd, fb, prefer = "febr", verbose = FALSE)
  expect_equal(length(m), 1L)
  expect_equal(m[[1L]]$site$merge_decision, "kept_febr")
  expect_equal(m[[1L]]$site$merge_source,   "FEBR")
})


test_that("merge_brazilian_pedons keeps unique pedons from both sides", {
  bd <- list(.make_brz_pedon("100"))
  fb <- list(.make_brz_pedon("999"))
  m <- merge_brazilian_pedons(bd, fb, verbose = FALSE)
  expect_equal(length(m), 2L)
  decisions <- vapply(m, function(p) p$site$merge_decision, character(1L))
  expect_setequal(decisions, c("unique", "unique"))
})


test_that("merge_brazilian_pedons keeps pedons without sisb_id (unmatchable)", {
  bd <- list(.make_brz_pedon("100", sisb = NA),
              .make_brz_pedon("101", sisb = NA))
  fb <- list(.make_brz_pedon("200", sisb = NA))
  m <- merge_brazilian_pedons(bd, fb, verbose = FALSE)
  expect_equal(length(m), 3L)  # cannot dedup, all kept
})


test_that("merge_brazilian_pedons handles empty input lists gracefully", {
  expect_equal(length(merge_brazilian_pedons(list(), list(), verbose = FALSE)), 0L)
  bd <- list(.make_brz_pedon("100"))
  expect_equal(length(merge_brazilian_pedons(bd, list(), verbose = FALSE)), 1L)
  fb <- list(.make_brz_pedon("100"))
  expect_equal(length(merge_brazilian_pedons(list(), fb, verbose = FALSE)), 1L)
})


test_that("merge_brazilian_pedons rejects non-PedonRecord input", {
  bd <- list(.make_brz_pedon("100"))
  expect_error(merge_brazilian_pedons(bd, list("not a pedon")),
                  "PedonRecord")
  expect_error(merge_brazilian_pedons(list("not a pedon"), list()),
                  "PedonRecord")
})


test_that("merge_brazilian_pedons accepts NULL inputs", {
  bd <- list(.make_brz_pedon("100"))
  expect_equal(length(merge_brazilian_pedons(NULL, NULL, verbose = FALSE)), 0L)
  expect_equal(length(merge_brazilian_pedons(bd,   NULL, verbose = FALSE)), 1L)
  expect_equal(length(merge_brazilian_pedons(NULL, bd,   verbose = FALSE)), 1L)
})


test_that("merge_brazilian_pedons preserves pedon order: shared, BD-only, FEBR-only, no-sisb", {
  bd <- list(.make_brz_pedon("100"),
              .make_brz_pedon("200"),  # BD-only
              .make_brz_pedon("nona", sisb = NA))
  fb <- list(.make_brz_pedon("100"),
              .make_brz_pedon("300"),  # FE-only
              .make_brz_pedon("nonb", sisb = NA))
  m <- merge_brazilian_pedons(bd, fb, prefer = "bdsolos", verbose = FALSE)
  expect_equal(length(m), 5L)
  # First slot is the shared pedon (kept_bdsolos)
  expect_equal(m[[1L]]$site$sisb_id, "100")
  expect_equal(m[[1L]]$site$merge_decision, "kept_bdsolos")
})


# ---- summarize_brazilian_overlap ------------------------------------------

test_that("summarize_brazilian_overlap counts overlap correctly", {
  bd <- list(.make_brz_pedon("100"),
              .make_brz_pedon("200"),
              .make_brz_pedon("nona", sisb = NA))
  fb <- list(.make_brz_pedon("100"),
              .make_brz_pedon("300"))
  s <- summarize_brazilian_overlap(bd, fb)
  expect_equal(s$n_bdsolos,           3L)
  expect_equal(s$n_febr,              2L)
  expect_equal(s$n_bdsolos_with_sisb, 2L)
  expect_equal(s$n_febr_with_sisb,    2L)
  expect_equal(s$n_shared,            1L)
  expect_equal(s$n_bdsolos_only,      1L)
  expect_equal(s$n_febr_only,         1L)
  expect_equal(s$n_unmatchable,       1L)
})


test_that("summarize_brazilian_overlap handles empty input", {
  s <- summarize_brazilian_overlap(list(), list())
  expect_equal(s$n_bdsolos, 0L)
  expect_equal(s$n_febr,    0L)
  expect_equal(s$n_shared,  0L)
})


# ---- Integration: load_bdsolos_csv assigns site$sisb_id ------------------

test_that("load_bdsolos_csv populates site$sisb_id from Codigo PA", {
  tf <- tempfile(fileext = ".csv")
  hdr <- paste(c("Codigo PA", "Simbolo Horizonte",
                   "Profundidade Superior", "Profundidade Inferior",
                   "Cor da Amostra Umida - Matiz",
                   "Cor da Amostra Umida - Valor",
                   "Cor da Amostra Umida - Croma",
                   "Classe de Solos Nivel 1",
                   "Classificacao Atual"), collapse = ";")
  rows <- c(
    paste(c("5310", "A1", "0", "20", "10YR", "4", "3",
              "ARGISSOLO", "ARGISSOLO VERMELHO Distrofico"),
            collapse = ";"),
    paste(c("5310", "Bt1", "20", "60", "5YR", "4", "6",
              "ARGISSOLO", "ARGISSOLO VERMELHO Distrofico"),
            collapse = ";")
  )
  writeLines(c("preamble", "", hdr, rows), tf)
  on.exit(unlink(tf), add = TRUE)
  pedons <- load_bdsolos_csv(tf, verbose = FALSE)
  expect_length(pedons, 1L)
  expect_equal(pedons[[1L]]$site$id,      "5310")
  expect_equal(pedons[[1L]]$site$sisb_id, "5310")
})
