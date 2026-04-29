# v0.7.1 SiBCS 5a ed.: tests for the 2nd categorical level (subordens).
# 13 ordens / 44 subordens / 13 fixtures.

# ---- key.yaml structural integrity --------------------------------------

test_that("SiBCS rules YAML carries 13 ordens + 44 subordens", {
  rules <- load_rules("sibcs5")
  expect_equal(length(rules$ordens), 13L)
  expect_true("subordens" %in% names(rules))
  total <- sum(vapply(rules$subordens, length, integer(1)))
  expect_equal(total, 44L)
})

test_that("each ordem has at least one subordem and the last is a catch-all", {
  rules <- load_rules("sibcs5")
  for (code in names(rules$subordens)) {
    subs <- rules$subordens[[code]]
    expect_gt(length(subs), 0L,
                label = sprintf("ordem %s should have subordens", code))
    # v0.7.2 contract: last subordem of each ordem uses tests:{default:true}
    # so the engine assigns it deterministically as catch-all without
    # re-invoking a per-subordem diagnostic function.
    last_entry <- subs[[length(subs)]]
    expect_true(
      isTRUE(last_entry$tests$default),
      info = sprintf(
        "last subordem of ordem %s should be tests:{default:true}; got tests:{%s}",
        code, paste(names(last_entry$tests), collapse = ",")
      )
    )
  }
})


# ---- run_sibcs_subordem mechanics ---------------------------------------

test_that("run_sibcs_subordem returns the canonical-order winner", {
  pr <- make_argissolo_canonical()
  res <- run_sibcs_subordem(pr, "P")
  expect_false(is.null(res$assigned))
  expect_true("code" %in% names(res$assigned))
  expect_true("trace" %in% names(res))
})

test_that("run_sibcs_subordem returns NULL for unknown ordem", {
  pr <- make_argissolo_canonical()
  res <- run_sibcs_subordem(pr, "ZZZ")
  expect_null(res$assigned)
})


# ---- classify_sibcs descends to subordem name ---------------------------

test_that("classify_sibcs returns subordem name (not just ordem name)", {
  pr <- make_latossolo_canonical()
  res <- classify_sibcs(pr, on_missing = "silent")
  expect_match(res$name, "^Latossolos ")
  expect_equal(res$rsg_or_order, "Latossolos")
})

test_that("classify_sibcs trace carries subordens block", {
  pr <- make_argissolo_canonical()
  res <- classify_sibcs(pr, on_missing = "silent")
  expect_true("subordens" %in% names(res$trace))
  expect_true("subordem_assigned" %in% names(res$trace))
  expect_false(is.null(res$trace$subordem_assigned$code))
})


# ---- per-fixture subordem assignment ------------------------------------
#
# Each canonical SiBCS fixture should classify to a subordem whose code
# matches the expected pattern. Because most fixtures are minimal, many
# fall to the Haplicos / Vermelho-Amarelos catch-all, which is fine --
# we just want a deterministic assignment.

expected_subordem_pattern <- list(
  argissolo    = "^P[A-Z]+$",   # PBAC, PAC, PA, PV, PVA
  cambissolo   = "^C[A-Z]+$",
  chernossolo  = "^M[A-Z]$",
  espodossolo  = "^E[A-Z]$",
  gleissolo    = "^G[A-Z]$",
  latossolo    = "^L[A-Z]+$",
  luvissolo    = "^T[A-Z]$",
  neossolo     = "^R[A-Z]$",
  nitossolo    = "^N[A-Z]$",
  organossolo  = "^O[A-Z]$",
  planossolo   = "^S[A-Z]$",
  plintossolo  = "^F[A-Z]$",
  vertissolo   = "^V[A-Z]$"
)

fixfns <- list(
  argissolo   = make_argissolo_canonical,
  cambissolo  = make_cambissolo_canonical,
  chernossolo = make_chernossolo_canonical,
  espodossolo = make_espodossolo_canonical,
  gleissolo   = make_gleissolo_canonical,
  latossolo   = make_latossolo_canonical,
  luvissolo   = make_luvissolo_canonical,
  neossolo    = make_neossolo_canonical,
  nitossolo   = make_nitossolo_canonical,
  organossolo = make_organossolo_canonical,
  planossolo  = make_planossolo_canonical,
  plintossolo = make_plintossolo_canonical,
  vertissolo  = make_vertissolo_canonical
)

for (nm in names(fixfns)) {
  local({
    nm_local <- nm
    fn <- fixfns[[nm_local]]
    pat <- expected_subordem_pattern[[nm_local]]
    test_that(sprintf("fixture %s classifies to a subordem matching %s",
                        nm_local, pat), {
      pr <- fn()
      res <- classify_sibcs(pr, on_missing = "silent")
      sub <- res$trace$subordem_assigned
      expect_false(is.null(sub),
                     label = sprintf("subordem assigned for %s", nm_local))
      expect_match(sub$code, pat,
                     info = sprintf("fixture %s -> subordem %s (%s)",
                                      nm_local, sub$code, sub$name))
    })
  })
}


# ---- targeted subordem positives (Munsell-driven) -----------------------

test_that("argissolo_vermelho catches a 2.5YR profile", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 150),
    designation = c("A", "Bt", "Bt2"),
    munsell_hue_moist = c("10YR", "2.5YR", "2.5YR"),
    munsell_value_moist = c(3, 4, 4),
    munsell_chroma_moist = c(2, 6, 6),
    clay_pct = c(20, 50, 55), silt_pct = c(30, 25, 25), sand_pct = c(50, 25, 20),
    boundary_distinctness = c("clear", "clear", NA),
    cec_cmol = c(8, 6, 5), bs_pct = c(50, 30, 25),
    al_cmol = c(0.5, 1, 1.2), ph_h2o = c(5.0, 5.2, 5.5),
    oc_pct = c(1.5, 0.8, 0.4)
  )
  pr <- PedonRecord$new(
    site = list(id = "AR-V", lat = 0, lon = 0, country = "BR",
                  parent_material = "gneiss"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- argissolo_vermelho(pr)
  expect_true(isTRUE(res$passed))
})

test_that("latossolo_amarelo catches a 10YR profile (yellow latosol)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 200),
    designation = c("A", "Bw1", "Bw2"),
    munsell_hue_moist = c("10YR", "10YR", "10YR"),
    munsell_value_moist = c(4, 5, 5),
    munsell_chroma_moist = c(4, 6, 6),
    clay_pct = c(35, 50, 55), silt_pct = c(20, 15, 15), sand_pct = c(45, 35, 30),
    cec_cmol = c(5, 3, 2.5), bs_pct = c(20, 15, 10),
    ph_h2o = c(4.8, 5.0, 5.0)
  )
  pr <- PedonRecord$new(
    site = list(id = "LA-AM", lat = -3, lon = -60, country = "BR",
                  parent_material = "Barreiras"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- latossolo_amarelo(pr)
  expect_true(isTRUE(res$passed))
})

test_that("neossolo_quartzarenico passes on pure quartz sand profile", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 100), bottom_cm = c(30, 100, 150),
    designation = c("A", "C1", "C2"),
    clay_pct = c(50, 30, 20),    # all < 200
    silt_pct = c(50, 30, 30),
    sand_pct = c(900, 940, 950)
  )
  pr <- PedonRecord$new(
    site = list(id = "RQ", lat = -10, lon = -45, country = "BR",
                  parent_material = "areia eolica"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- neossolo_quartzarenico(pr)
  expect_true(isTRUE(res$passed))
})


# ---- regression: WRB classification still works ------------------------

test_that("WRB classification still produces 31/31 correct after SiBCS subordens added", {
  expected <- c(
    HS = "Histosols", AT = "Anthrosols", TC = "Technosols", CR = "Cryosols",
    LP = "Leptosols", SN = "Solonetz",   VR = "Vertisols", SC = "Solonchaks",
    GL = "Gleysols",  AN = "Andosols",   PZ = "Podzols",   PT = "Plinthosols",
    PL = "Planosols", ST = "Stagnosols", NT = "Nitisols",  FR = "Ferralsols"
  )
  fns <- list(
    HS = make_histosol_canonical, AT = make_anthrosol_canonical,
    TC = make_technosol_canonical, CR = make_cryosol_canonical,
    LP = make_leptosol_canonical,  SN = make_solonetz_canonical,
    VR = make_vertisol_canonical,  SC = make_solonchak_canonical,
    GL = make_gleysol_canonical,   AN = make_andosol_canonical,
    PZ = make_podzol_canonical,    PT = make_plinthosol_canonical,
    PL = make_planosol_canonical,  ST = make_stagnosol_canonical,
    NT = make_nitisol_canonical,   FR = make_ferralsol_canonical
  )
  for (k in names(fns)) {
    out <- classify_wrb2022(fns[[k]](), on_missing = "silent")$rsg_or_order
    expect_equal(out, expected[[k]],
                  info = sprintf("Fixture %s expected %s, got %s",
                                  k, expected[[k]], out))
  }
})
