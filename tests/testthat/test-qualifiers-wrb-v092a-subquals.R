# v0.9.2.A -- Hyper- / Hypo- / Proto- sub-qualifiers and family
# suppression in the qualifier resolver.

# ---- New sub-qualifier behavioural contracts ------------------------------

test_that("Hypersalic >= 30 dS/m, Hyposalic 4-15 dS/m", {
  hz <- data.table::data.table(
    top_cm = c(0, 25), bottom_cm = c(25, 100),
    designation = c("Az", "Bz"),
    ec_dS_m = c(35, 8),  # layer 1 hypersalic, layer 2 hyposalic
    clay_pct = c(20, 22), silt_pct = c(40, 38), sand_pct = c(40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "S", lat = 0, lon = 0, country = "TEST",
                  parent_material = "alluvium"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_hypersalic(pr)$passed))
  expect_true(isTRUE(qual_hyposalic(pr)$passed))

  # Both fail at low EC.
  hz$ec_dS_m <- c(2, 1)
  pr2 <- PedonRecord$new(
    site = list(id = "S2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "alluvium"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(qual_hypersalic(pr2)$passed))
  expect_false(isTRUE(qual_hyposalic(pr2)$passed))
})

test_that("Hypersodic ESP >= 50%, Hyposodic 6-15%", {
  hz <- data.table::data.table(
    top_cm = c(0, 25), bottom_cm = c(25, 100),
    na_cmol = c(15, 1.5), cec_cmol = c(20, 15),
    clay_pct = c(20, 22), silt_pct = c(40, 38), sand_pct = c(40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "Na", lat = 0, lon = 0, country = "TEST",
                  parent_material = "marsh"),
    horizons = ensure_horizon_schema(hz)
  )
  # ESP layer 1 = 75% -> Hypersodic; layer 2 = 10% -> Hyposodic.
  expect_true(isTRUE(qual_hypersodic(pr)$passed))
  expect_true(isTRUE(qual_hyposodic(pr)$passed))
})

test_that("Hypercalcic / Hypocalcic / Protocalcic ladder", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
    designation = c("A", "Bk1", "Bk2"),
    caco3_pct = c(0.8, 8, 60),  # protocalcic / hypocalcic / hypercalcic + calcic
    clay_pct = c(20, 22, 22), silt_pct = c(40, 38, 38), sand_pct = c(40, 40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "C", lat = 0, lon = 0, country = "TEST",
                  parent_material = "calcareous loess"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_protocalcic(pr)$passed))
  expect_true(isTRUE(qual_hypocalcic(pr)$passed))
  expect_true(isTRUE(qual_hypercalcic(pr)$passed))
  # Hypocalcic threshold is 5-15%; layer 3 (60%) is above.
  res <- qual_hypocalcic(pr)
  h <- pr$horizons
  expect_true(all(h$caco3_pct[res$layers] < 15))
})

test_that("Hypergypsic / Hypogypsic / Protogypsic ladder", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
    designation = c("A", "By1", "By2"),
    caso4_pct = c(2, 3, 70),
    clay_pct = c(20, 22, 22), silt_pct = c(40, 38, 38), sand_pct = c(40, 40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "G", lat = 0, lon = 0, country = "TEST",
                  parent_material = "gypsic loess"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_protogypsic(pr)$passed))
  expect_true(isTRUE(qual_hypogypsic(pr)$passed))
  expect_true(isTRUE(qual_hypergypsic(pr)$passed))
})

test_that("Protovertic excludes layers that also satisfy strict Vertic", {
  # Profile that meets vertic in one layer (slickensides + clay 50 +
  # thickness 30) AND has another layer with weaker vertic-spectrum
  # signal (clay 40 but no slickensides).
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 150),
    designation = c("A", "Bss", "BC"),
    clay_pct = c(40, 55, 50), silt_pct = c(30, 25, 25), sand_pct = c(30, 20, 25),
    slickensides = c("absent", "many", "absent"),
    cracks_width_cm = c(NA_real_, 1.0, 0.5)
  )
  pr <- PedonRecord$new(
    site = list(id = "PV", lat = 0, lon = 0, country = "TEST",
                  parent_material = "smectitic alluvium"),
    horizons = ensure_horizon_schema(hz)
  )
  pv <- qual_protovertic(pr)
  # If protovertic fires, the layers must NOT overlap with vertic_horizon.
  v <- vertic_horizon(pr)
  expect_true(length(intersect(pv$layers, v$layers %||% integer(0))) == 0L)
})


# ---- Family-suppression contract in resolve_wrb_qualifiers ----------------

test_that("Family suppression keeps the most-specific of co-firing siblings", {
  # All four members of the calcic family pass; resolver must keep
  # only Hypercalcic.
  matched <- c("Vermic", "Hypercalcic", "Calcic", "Hypocalcic", "Protocalcic")
  kept <- soilKey:::.suppress_qualifier_siblings(matched)
  expect_setequal(kept, c("Vermic", "Hypercalcic"))

  # Sodic + Hyposodic -> keep only Sodic.
  expect_setequal(
    soilKey:::.suppress_qualifier_siblings(c("Sodic", "Hyposodic")),
    "Sodic")
  # Hyperalic + Alic -> keep only Hyperalic.
  expect_setequal(
    soilKey:::.suppress_qualifier_siblings(c("Hyperalic", "Alic")),
    "Hyperalic")
  # No family overlap -> nothing dropped.
  expect_equal(
    soilKey:::.suppress_qualifier_siblings(c("Mollic", "Cambic", "Stagnic")),
    c("Mollic", "Cambic", "Stagnic"))
})

test_that("Family suppression preserves YAML order of survivors", {
  # YAML-canonical order is reproduced after suppression.
  matched <- c("Mazic", "Sodic", "Hyposodic", "Calcic", "Cambic")
  kept <- soilKey:::.suppress_qualifier_siblings(matched)
  expect_equal(kept, c("Mazic", "Sodic", "Calcic", "Cambic"))
})

test_that("SC fixture name no longer doubles up Sodic + Hyposodic", {
  pr <- make_solonchak_canonical()
  res <- resolve_wrb_qualifiers(pr, "SC")
  # If Sodic passed, Hyposodic must NOT appear in the resolved list.
  if ("Sodic" %in% res$principal) {
    expect_false("Hyposodic" %in% res$principal)
  }
})

test_that("CH fixture surfaces Hypocalcic instead of bare Calcic when caco3 < 15%", {
  pr <- make_chernozem_canonical()
  res <- resolve_wrb_qualifiers(pr, "CH")
  # CH fixture has caco3 = 8 / 12 % in Bk / Ck (below the 15% Calcic
  # gate but inside the 5-15% Hypocalcic band).
  expect_true("Hypocalcic" %in% res$principal)
})


# ---- 31-fixture regression check ------------------------------------------

test_that("v0.9.2.A sub-qualifier additions do not regress 31 fixtures", {
  expected <- c(
    HS = "Histosols", AT = "Anthrosols", TC = "Technosols", CR = "Cryosols",
    LP = "Leptosols", SN = "Solonetz",   VR = "Vertisols", SC = "Solonchaks",
    GL = "Gleysols",  AN = "Andosols",   PZ = "Podzols",   PT = "Plinthosols",
    PL = "Planosols", ST = "Stagnosols", NT = "Nitisols",  FR = "Ferralsols",
    CH = "Chernozems", KS = "Kastanozems", PH = "Phaeozems", UM = "Umbrisols",
    DU = "Durisols",  GY = "Gypsisols", CL = "Calcisols", RT = "Retisols",
    AC = "Acrisols",  LX = "Lixisols",   AL = "Alisols",   LV = "Luvisols",
    CM = "Cambisols", AR = "Arenosols",  FL = "Fluvisols"
  )
  fixfns <- list(
    HS = make_histosol_canonical,  AT = make_anthrosol_canonical,
    TC = make_technosol_canonical, CR = make_cryosol_canonical,
    LP = make_leptosol_canonical,  SN = make_solonetz_canonical,
    VR = make_vertisol_canonical,  SC = make_solonchak_canonical,
    GL = make_gleysol_canonical,   AN = make_andosol_canonical,
    PZ = make_podzol_canonical,    PT = make_plinthosol_canonical,
    PL = make_planosol_canonical,  ST = make_stagnosol_canonical,
    NT = make_nitisol_canonical,   FR = make_ferralsol_canonical,
    CH = make_chernozem_canonical, KS = make_kastanozem_canonical,
    PH = make_phaeozem_canonical,  UM = make_umbrisol_canonical,
    DU = make_durisol_canonical,   GY = make_gypsisol_canonical,
    CL = make_calcisol_canonical,  RT = make_retisol_canonical,
    AC = make_acrisol_canonical,   LX = make_lixisol_canonical,
    AL = make_alisol_canonical,    LV = make_luvisol_canonical,
    CM = make_cambisol_canonical,  AR = make_arenosol_canonical,
    FL = make_fluvisol_canonical
  )
  for (k in names(fixfns)) {
    out <- classify_wrb2022(fixfns[[k]](), on_missing = "silent")$rsg_or_order
    expect_equal(out, expected[[k]],
                  info = sprintf("Fixture %s -> expected %s, got %s",
                                  k, expected[[k]], out))
  }
})
