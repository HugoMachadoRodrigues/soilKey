# v0.9.291: the Rudosolic and Tenosolic great groups of the Redoxic and
# Oxyaquic Hydrosols take the subgroup list of the Rudosol or Tenosol suborder
# the soil keys to, apart from wetness ("subgroups_from" in the rules).

mk_rt <- function(df, site = list()) {
  PedonRecord$new(site = utils::modifyList(list(id = "t", country = "AU", designation_system = "asc",
                                                drainage_class = "poorly drained", landform = "alluvial plain"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(df)))
}
# a wet A1 over C with no B: a Rudosolic Hydrosol (a Lutic Rudosol apart from wetness)
ru_rt <- function(des = c("A1", "C1", "C2"), ph = c(6.5, 6.8, 7.0), origin = c(NA, "alluvium", "alluvium"),
                  ec = 0.1, eff = "none", redox = c(0, 10, 20), mottles = c(NA, "distinct", "distinct"), cf = 0) {
  mk_rt(data.frame(designation = des, top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 120),
                   field_texture = c("L", "CL", "SCL"), clay_pct = c(20, 30, 25),
                   structure_grade = c("weak", "structureless", "structureless"),
                   structure_type = c("granular", "single-grain", "single-grain"), ph_h2o = ph,
                   effervescence_class = eff, water_saturation_days = c(100, 150, 200),
                   redoximorphic_features_pct = redox, mottle_contrast = mottles, coarse_fragments_pct = cf,
                   layer_origin = origin, oc_pct = c(1.5, 0.3, 0.1), ec_1to5_dS_m = ec, stringsAsFactors = FALSE))
}
# a wet A1 over a weak sandy-loam B2: a Tenosolic Hydrosol (a Brown Tenosol apart from wetness)
te_rt <- function(ph = c(6.5, 6.6, 6.8), hue = "10YR", val = c(4, 5, 6), chr = c(2, 4, 4)) {
  mk_rt(data.frame(designation = c("A1", "B2", "C"), top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 120),
                   field_texture = "SL", clay_pct = c(12, 13, 12),
                   structure_grade = c("weak", "structureless", "structureless"),
                   structure_type = c("granular", "massive", "massive"), ph_h2o = ph, effervescence_class = "none",
                   water_saturation_days = c(100, 150, 200), redoximorphic_features_pct = c(0, 10, 20),
                   mottle_contrast = c(NA, "distinct", "distinct"), layer_origin = c(NA, NA, "alluvium"),
                   oc_pct = c(1.5, 0.3, 0.1), ec_1to5_dS_m = 0.1, munsell_hue_moist = hue,
                   munsell_value_moist = val, munsell_chroma_moist = chr, stringsAsFactors = FALSE))
}
lv_rt <- function(r, level) r$levels$code[r$levels$level == level]
st_rt <- function(r, phase) vapply(r$trace[[phase]], function(e)
  if (isTRUE(e$passed)) "T" else if (isFALSE(e$passed)) "F" else "?", "")

test_that("the rules name the four great groups and the order whose lists they take", {
  skip_on_cran()
  rules <- load_rules("asc3")
  fr <- rules$subgroups_from
  expect_identical(vapply(fr, `[[`, "", "class"),
                   c("HY-ED-GT" = "TE", "HY-ED-GR" = "RU", "HY-DT-GT" = "TE", "HY-DT-GR" = "RU"))
  expect_true(all(vapply(fr, function(f) isTRUE(f$site$.apart_from_wetness), NA)))
  # their own lists stay empty, so the order-wide Hydrosol list does not apply
  for (k in names(fr)) expect_length(rules$subgroups[[k]], 0L)
  # the Salic ones keep the Salic list (Hugo's decision, 0.9.289)
  expect_null(fr[["HY-EG-GR"]])
  expect_length(soilKey:::.level_list(rules$subgroups, c("HY", "EG", "GR"), "GR"), 3L)
  # the validator finds every list and every _from entry well formed ...
  v <- soilKey:::.validate_rules("asc3")
  expect_length(v$codes, 0L); expect_length(v$missing, 0L)
  # ... and reports one that names no order, or a list that is not empty
  sys <- soilKey:::.system_get("asc3")
  bad <- rules
  bad$subgroups_from[["HY-ED-GR"]]$class <- "QQ"
  bad$subgroups_from[["HY-DT-GZ"]] <- "RU"
  probs <- soilKey:::.collect_code_problems(bad, sys)
  expect_true(any(grepl("subgroups_from/HY-ED-GR: names no class", probs)))
  expect_true(any(grepl("subgroups_from/HY-DT-GZ: the subgroups list of HY-DT-GZ must be present and empty", probs)))
})

test_that("a Rudosolic Hydrosol takes the list of its Rudosol suborder", {
  skip_on_cran()
  r <- classify_asc3(ru_rt(), on_missing = "silent")
  expect_identical(lv_rt(r, "great_groups"), "GR")
  expect_identical(r$name, "Basic, Rudosolic, Redoxic Hydrosol")
  # the walk is traced as a phase of its own, before the subgroups
  expect_identical(names(r$trace), c("orders", "suborders", "great_groups", "suborders, as a Rudosol", "subgroups"))
  expect_identical(st_rt(r, "suborders, as a Rudosol")[["GV"]], "T")
  # the Rudosol great groups are not keyed: the list depends on the suborder only
  expect_false(any(grepl("great_groups, as a", names(r$trace))))
  # the levels stay those of the Hydrosol
  expect_identical(r$levels$code, c("HY", "ED", "GR", "AR"))
  expect_identical(classify_asc3(ru_rt(ph = c(4.8, 5.0, 5.2)), on_missing = "silent")$name,
                   "Acidic, Rudosolic, Redoxic Hydrosol")
  # some part calcareous (a calcareous profile throughout would be Calcarosolic)
  expect_identical(classify_asc3(ru_rt(ph = c(6.8, 7.0, 8.2), eff = c("none", "none", "strong")),
                                 on_missing = "silent")$name,
                   "Calcareous, Rudosolic, Redoxic Hydrosol")
  # an Oxyaquic one (no mottles) takes the same list
  ro <- classify_asc3(ru_rt(redox = 0, mottles = NA), on_missing = "silent")
  expect_identical(ro$name, "Basic, Rudosolic, Oxyaquic Hydrosol")
})

test_that("the Rudosol suborders are read apart from wetness: Hypersalic by its salinity", {
  skip_on_cran()
  # highly saline and wet: Hypersalic as a Rudosol suborder, whose own test asks
  # for no water table and no mottles
  r <- classify_asc3(ru_rt(des = c("A1", "C1", "2C2"), ec = 5), on_missing = "silent")
  expect_identical(st_rt(r, "suborders, as a Rudosol")[["CS"]], "T")
  expect_identical(r$name, "Basic, Rudosolic, Redoxic Hydrosol")
  # the flag is set on a copy of the pedon, for the walk only
  p <- ru_rt(ec = 5)
  classify_asc3(p, on_missing = "silent")
  expect_null(p$site$.apart_from_wetness)
  expect_false(isTRUE(soilKey:::saline_suborder_asc(p, "hypersalic")$passed))
  q <- p$clone(deep = TRUE); q$site$.apart_from_wetness <- TRUE
  expect_true(isTRUE(soilKey:::saline_suborder_asc(q, "hypersalic")$passed))
  # the flag is the key's, never the user's: a saline, mottled Rudosol stays
  # out of the Hypersalic suborder even when its record carries it
  dry <- mk_rt(data.frame(designation = c("A1", "C1", "C2"), top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 120),
                          field_texture = c("L", "CL", "SCL"), clay_pct = c(20, 30, 25),
                          structure_grade = c("weak", "structureless", "structureless"),
                          structure_type = c("granular", "single-grain", "single-grain"), ph_h2o = 6.8,
                          effervescence_class = "none", redoximorphic_features_pct = c(0, 5, 5),
                          layer_origin = c(NA, "alluvium", "alluvium"), oc_pct = c(1.5, 0.3, 0.1),
                          ec_1to5_dS_m = 5, stringsAsFactors = FALSE),
               site = list(drainage_class = "well drained", landform = NULL))
  n0 <- classify_asc3(dry, on_missing = "silent")$name
  expect_false(grepl("Hypersalic", n0))
  dry$site$.apart_from_wetness <- TRUE
  expect_identical(classify_asc3(dry, on_missing = "silent")$name, n0)
  expect_true(isTRUE(dry$site$.apart_from_wetness))
})

test_that("a Tenosolic Hydrosol takes the list of its Tenosol suborder", {
  skip_on_cran()
  r <- classify_asc3(te_rt(), on_missing = "silent")
  expect_identical(lv_rt(r, "great_groups"), "GT")
  expect_identical(r$name, "Basic, Tenosolic, Redoxic Hydrosol")
  expect_identical(st_rt(r, "suborders, as a Tenosol")[["AB"]], "T")
  # the coloured list: Manganic and the peaty-to-melanic ones were tried before Basic
  expect_true(all(c("DW", "DC", "AI", "AR") %in% names(r$trace$subgroups)))
  expect_identical(classify_asc3(te_rt(ph = c(4.8, 5.0, 5.2)), on_missing = "silent")$name,
                   "Acidic, Tenosolic, Redoxic Hydrosol")
  # no colours: the colour suborders are undecided, so no list, and the
  # subgroup level is provisional by each of them
  rn <- classify_asc3(te_rt(hue = NA, val = NA, chr = NA), on_missing = "silent")
  expect_length(lv_rt(rn, "subgroups"), 0L)
  expect_true(all(paste0("subgroups:TE/", c("AA", "AB", "AC", "AD", "AE")) %in% rn$provisional_because))
  expect_false(any(grepl("No class of the suborders list", rn$warnings)))
})

test_that("no subgroup where the Rudosol key gives no list", {
  skip_on_cran()
  # shells in the C: a Shelly Rudosol apart from wetness, whose suborder has no subgroups
  r <- classify_asc3(ru_rt(origin = c(NA, "shells", "shells")), on_missing = "silent")
  expect_identical(lv_rt(r, "great_groups"), "GR")
  expect_length(lv_rt(r, "subgroups"), 0L)
  expect_identical(r$name, "Rudosolic, Redoxic Hydrosol")
  expect_identical(st_rt(r, "suborders, as a Rudosol")[["EL"]], "T")
  expect_null(r$trace$subgroups)
  expect_false(any(grepl("^subgroups:", r$provisional_because)))
  # exactly 10 % gravel: neither slightly gravelly (Arenic, Lutic, Stratic) nor
  # gravelly (Clastic), so no Rudosol suborder passes, and a note says so
  r <- classify_asc3(ru_rt(cf = 10), on_missing = "silent")
  expect_identical(r$name, "Rudosolic, Redoxic Hydrosol")
  expect_true(all(st_rt(r, "suborders, as a Rudosol") == "F"))
  expect_true(any(grepl("No class of the suborders list of the Rudosol passed", r$warnings)))
})

test_that("an undecided suborder counts only when it could lead to another list", {
  skip_on_cran()
  # no layer origin: the C may be saprolite, so Leptic is undecided before the
  # Lutic one assigned; both take the same list, so the subgroup is decided
  r <- classify_asc3(ru_rt(origin = c(NA, NA, NA)), on_missing = "silent")
  expect_identical(st_rt(r, "suborders, as a Rudosol")[["CY"]], "?")
  expect_identical(lv_rt(r, "subgroups"), "AR")
  expect_false(any(grepl("^subgroups:", r$provisional_because)))
})

# ---- the engine on small rule sets ---------------------------------------------
# a test that passes, one that fails, one left undecided
.P_rt <- list(default = TRUE)
.F_rt <- list(any_of = list())
.N_rt <- list(not_implemented_v01 = "x")
tx_rt <- function(code, tests, name = code) list(code = code, name = name, tests = tests)
toy_rt <- function(subgroups, sub_tests = list(S1 = .F_rt, S2 = .P_rt), gg_tests = list(G1 = .F_rt, G2 = .P_rt),
                   s2_has_ggs = TRUE) {
  gl <- c(list("B-S1" = list(tx_rt("G3", .P_rt))),
          if (s2_has_ggs) list("B-S2" = list(tx_rt("G1", gg_tests$G1), tx_rt("G2", gg_tests$G2))))
  list(orders = list(tx_rt("A", .F_rt, "Alpha"), tx_rt("B", .P_rt, "Beta")),
       suborders = list("B" = list(tx_rt("S1", sub_tests$S1), tx_rt("S2", sub_tests$S2))),
       great_groups = gl, subgroups = subgroups)
}
lev_rt <- c("orders", "suborders", "great_groups", "subgroups")
X_rt <- list(tx_rt("X", .P_rt)); Y_rt <- list(tx_rt("Y", .P_rt)); W_rt <- list(tx_rt("W", .P_rt))
kf_rt <- function(rules, spec = "B") soilKey:::.key_from(make_ferralsol_canonical(), rules, lev_rt, 4L, spec)
codes_rt <- function(k) vapply(k$taxa, `[[`, "", "code")

test_that(".key_from walks the other class only as deep as the list needs", {
  skip_on_cran()
  # a list for every class under the suborder: the great groups are not keyed
  k <- kf_rt(toy_rt(list("B-S2-*" = X_rt)))
  expect_identical(codes_rt(k), "X")
  expect_identical(names(k$trace), "suborders, as a Beta")
  # a list for one great group: the great groups are keyed too
  k <- kf_rt(toy_rt(list("B-S2-*" = X_rt, "B-S2-G2" = Y_rt)))
  expect_identical(codes_rt(k), "Y")
  expect_identical(names(k$trace), c("suborders, as a Beta", "great_groups, as a Beta"))
  # ... and another great group falls back on the wildcard, as in the main key
  k <- kf_rt(toy_rt(list("B-S2-*" = X_rt, "B-S2-G2" = Y_rt), gg_tests = list(G1 = .P_rt, G2 = .P_rt)))
  expect_identical(codes_rt(k), "X")
  # a wildcard of the whole class
  k <- kf_rt(toy_rt(list("B-*" = W_rt)))
  expect_identical(codes_rt(k), "W")
  # ... but not for a suborder with no great groups and no list of its own,
  # where the main key stops
  k <- kf_rt(toy_rt(list("B-*" = W_rt), s2_has_ggs = FALSE))
  expect_length(k$taxa, 0L)
  # a list named by the suborder's path serves only a suborder with no great
  # groups (the main key's level skip)
  k <- kf_rt(toy_rt(list("B-S2" = Y_rt), s2_has_ggs = FALSE))
  expect_identical(codes_rt(k), "Y")
  k <- kf_rt(toy_rt(list("B-S2" = Y_rt)))
  expect_length(k$taxa, 0L)
  # no list under the suborder: none, and no note
  k <- kf_rt(toy_rt(list("B-S1-*" = X_rt)))
  expect_length(k$taxa, 0L); expect_length(k$note, 0L)
  # site fields for the walk, given as a mapping with the class
  k <- kf_rt(toy_rt(list("B-S2-*" = X_rt)), list(class = "B", site = list(.apart_from_wetness = TRUE)))
  expect_identical(codes_rt(k), "X")
})

test_that(".key_from keeps an undecided class only when its list differs", {
  skip_on_cran()
  und <- list(S1 = .N_rt, S2 = .P_rt)
  # S1 would take another list: kept
  k <- kf_rt(toy_rt(list("B-S1-*" = Y_rt, "B-S2-*" = X_rt), sub_tests = und))
  expect_identical(codes_rt(k), "X"); expect_identical(k$undecided, "B/S1")
  # S1 would take no list: kept
  k <- kf_rt(toy_rt(list("B-S2-*" = X_rt), sub_tests = und))
  expect_identical(k$undecided, "B/S1")
  # S1 would take the same list: dropped
  k <- kf_rt(toy_rt(list("B-S1-*" = X_rt, "B-S2-*" = X_rt), sub_tests = und))
  expect_identical(codes_rt(k), "X"); expect_length(k$undecided, 0L)
  # the list of S2 depends on its great group: kept
  k <- kf_rt(toy_rt(list("B-S1-*" = X_rt, "B-S2-*" = X_rt, "B-S2-G2" = Y_rt), sub_tests = und))
  expect_identical(k$undecided, "B/S1")
  # no suborder passes, none undecided: a note
  k <- kf_rt(toy_rt(list("B-S2-*" = X_rt), sub_tests = list(S1 = .F_rt, S2 = .F_rt)))
  expect_length(k$taxa, 0L); expect_match(k$note, "No class of the suborders list of the Beta passed")
  # no suborder passes, one undecided: no note, the class left undecided
  k <- kf_rt(toy_rt(list("B-S2-*" = X_rt), sub_tests = list(S1 = .N_rt, S2 = .F_rt)))
  expect_length(k$taxa, 0L); expect_length(k$note, 0L); expect_identical(k$undecided, "B/S1")
})

test_that("the benchmark compares a survey subgroup of a Rudosolic or Tenosolic Hydrosol", {
  skip_on_cran()
  res <- data.frame(reference = "HY", reference_suborder = "DT", reference_great_group = "GR",
                    reference_subgroup = "AR", predicted = "HY", predicted_suborder = "DT",
                    predicted_great_group = "GR", predicted_subgroup = "AR", agree = TRUE, decided = TRUE,
                    provisional_suborder = "", provisional_great_group = "", provisional_subgroup = "",
                    stringsAsFactors = FALSE)
  s <- soilKey:::.natsoil_subgroup_summary(res)
  expect_identical(s$n, 1L); expect_identical(s$accuracy, 100)
  # a code of no Rudosol list is not comparable
  res$reference_subgroup <- "QQ"
  expect_null(soilKey:::.natsoil_subgroup_summary(res))
  # Tenosolic: a code of any Tenosol list (Subhumose is only in the Leptic one)
  res$reference_great_group <- "GT"; res$predicted_great_group <- "GT"
  res$reference_subgroup <- "DR"; res$predicted_subgroup <- "AR"
  s <- soilKey:::.natsoil_subgroup_summary(res)
  expect_identical(s$n, 1L); expect_identical(s$accuracy, 0)
})
