# v0.9.300: the keys to subgroups of the New Zealand Soil Classification,
# 4th ed., for the Podzol, Allophanic, Pumice and Melanic Soils (79
# subgroups). One profile per subgroup, built from the order examples, keys
# to its group and its subgroup; and the readings where the text is silent
# (inst/rules/nzsc4/readings.yaml).

.nzz_mod <- function(order, h = list(), site = list()) {
  p <- make_nzsc4_example(order)
  hz <- as.data.frame(p$horizons, stringsAsFactors = FALSE)
  for (nm in names(h)) {
    v <- h[[nm]]
    if (is.list(v)) { if (!nm %in% names(hz)) hz[[nm]] <- NA; for (k in names(v)) hz[[nm]][as.integer(k)] <- v[[k]] }
    else hz[[nm]] <- if (length(v) == 1L) rep(v, nrow(hz)) else v
  }
  PedonRecord$new(site = utils::modifyList(p$site, site), horizons = ensure_horizon_schema(data.table::as.data.table(hz)))
}
# edits over a base: a list of row edits applies to the base's vector
.nzz_h <- function(base, ...) {
  new <- list(...)
  for (nm in names(new)) {
    v <- new[[nm]]; b <- base[[nm]]
    base[[nm]] <- if (is.list(v) && !is.null(b) && !is.list(b)) { for (k in names(v)) b[as.integer(k)] <- v[[k]]; b }
                  else if (is.list(v) && is.list(b)) utils::modifyList(b, v) else v
  }
  base
}
# ---- Z: the group profiles of the groups test, with a thicker Bh where asked
.nzz_bh10 <- list(top_cm = c(-3, 0, 8, 30, 40, 60), bottom_cm = c(0, 8, 30, 40, 60, 110))
.nzz_zd <- function(des = c("Lm", "Ah", "Ed", "Bh", "Bs", "C"), ...) .nzz_mod("podzol", h = .nzz_h(list(designation = des), ...))
.nzz_zp0 <- list(munsell_chroma_moist = c(2, 1, 1, 3, 6, 4), packing_degree = list(`6` = "very high"),
                 field_texture = c(NA, "sandy loam", "loamy sand", "sandy loam", "sandy loam", "silt loam"))
.nzz_zp <- function(des = c("Lm", "Ah", "E", "Bh", "Bs", "C"), ..., site = list())
  .nzz_mod("podzol", h = .nzz_h(.nzz_h(.nzz_zp0, designation = des), ...), site = site)
.nzz_zg <- function(...) .nzz_mod("podzol", h = .nzz_h(list(munsell_chroma_moist = c(2, 1, 1, 3, 6, 1),
                                  designation = c("Lm", "Ah", "E", "Bh", "Bs", "Cg"), bottom_cm = c(0, 8, 30, 35, 60, 120)), ...))
# the eluvial horizon 5/3 (not low chroma), so a pan does not make a
# Perch-gley Podzol
.nzz_zx <- function(des = c("Lm", "Ah", "E", "Bh", "Bs", "C"), ...)
  .nzz_mod("podzol", h = .nzz_h(list(designation = des, munsell_value_moist = list(`3` = 5), munsell_chroma_moist = list(`3` = 3),
                                     structure_grade = list(`5` = "structureless"), structure_type = list(`5` = "massive"),
                                     strength_moist = list(`5` = "firm")), ...))
.nzz_zo <- function(des = c("Lm", "Ah", "E", "Bh", "Bs", "C"), ...)
  .nzz_mod("podzol", h = .nzz_h(list(designation = des, munsell_value_moist = list(`3` = 5), munsell_chroma_moist = list(`3` = 3),
                                     structure_grade = list(`4` = "weak", `5` = "weak"), structure_type = list(`4` = "blocky", `5` = "blocky")), ...))
# ---- L
.nzz_rhy <- list(parent_material = "rhyolitic tephra")
.nzz_l <- function(..., site = list()) .nzz_mod("allophanic", h = list(...), site = site)
.nzz_lo <- function(...) .nzz_mod("allophanic", h = .nzz_h(list(layer_origin = "loess"), ...), site = .nzz_rhy)
.nzz_lv <- list(layer_origin = c("andesitic tephra", "andesitic tephra", "andesitic tephra", NA),
                sand_pct = c(55, 60, 60, 40), field_texture = c("sandy loam", "sandy loam", "sandy loam", "loam"))
# ---- M
.nzz_m <- function(...) .nzz_mod("pumice", h = list(...))
.nzz_mp <- function(des = c("Ap", "Bg", "Bw2", "C"), ...) .nzz_m(designation = des, redoximorphic_features_pct = c(0, 10, 0, 0), coarse_fragments_pct = 0, ...)
.nzz_mi <- function(des = c("Ap", "Bw", "BC", "C"), ...) .nzz_m(designation = des, ksat_mm_h = list(`3` = 1), ...)
.nzz_ign <- list(layer_origin = list(`4` = "Taupo ignimbrite"), packing_degree = list(`4` = "high"), roots_abundance = list(`4` = "none"))
# ---- E
.nzz_e <- function(...) .nzz_mod("melanic", h = list(...))
.nzz_noncalc <- list(effervescence_class = "none", ph_h2o = c(6.5, 6.5, 6.5, NA))
.nzz_ev <- function(...) .nzz_mod("melanic", h = .nzz_h(list(cracks_width_cm = list(`2` = 1), cracks_depth_cm = list(`2` = 50)), ...))
.nzz_ep <- function(des = c("Ah", "Bg", "Bk", "R"), ...)
  .nzz_mod("melanic", h = .nzz_h(list(designation = des, redoximorphic_features_pct = c(0, 10, 0, NA), munsell_value_moist = c(2, 5, 5, NA),
                                      munsell_chroma_moist = c(1, 2, 3, NA)), ...))
.nzz_er <- function(des = c("Ah", "Bw", "Bk", "R"), ..., site = list())
  .nzz_mod("melanic", h = .nzz_h(list(designation = des, top_cm = c(0, 22, 40, 55), bottom_cm = c(22, 40, 55, 90)), ...), site = site)
.nzz_em <- function(...) .nzz_mod("melanic", h = .nzz_h(list(rock_origin = "basalt", munsell_value_moist = c(2, 3, 4, NA),
                                  munsell_chroma_moist = c(1, 2, 3, NA), ca_cmol = c(20, 15, 15, NA), mg_cmol = c(3, 3, 3, NA)), ...))
.nzz_eo <- function(des = c("Ah", "Bw", "Bk", "R"), ...)
  .nzz_mod("melanic", h = .nzz_h(list(designation = des, rock_origin = c(NA, NA, "loess", "basalt"), ca_cmol = c(20, 15, 15, NA),
                                      mg_cmol = c(3, 3, 3, NA)), ...))
.nzz_cases <- function() list(
  # ---- Z
  ZDU  = .nzz_zd(c("Lm", "Ah", "Ed", "Bhm", "Bs", "C")),
  ZDJH = .nzz_zd(c("Lm", "Ah", "Ed", "Bh", "Bt", "C")),
  ZDYH = do.call(.nzz_zd, c(list(), .nzz_bh10, list(field_texture = list(`5` = "clay"), structure_type = list(`5` = "prismatic")))),
  ZDH  = .nzz_zd(),
  ZDQ  = .nzz_zd(c("Lm", "Ah", "Ed", "Bs", "Bsm", "C")),
  ZDT  = .nzz_zd(c("Lm", "Ah", "Ed", "Bs1", "Bs2", "C")),
  ZPF  = .nzz_zp(fluidity_class = list(`3` = "moderately fluid")),
  ZPOZ = .nzz_zp(top_cm = c(-3, 0, 12, 30, 35, 60), bottom_cm = c(0, 12, 30, 35, 60, 110),
                 field_texture = c(NA, "peaty silt loam", "silt loam", "sandy loam", "sandy loam", "silt loam"),
                 strength_moist = list(`3` = "firm"), structure_grade = list(`3` = "structureless"), structure_type = list(`3` = "massive"),
                 site = list(drainage_class = "poorly drained")),
  ZPZ  = .nzz_zp(field_texture = c(NA, "silt loam", "silt loam", "sandy loam", "sandy loam", "silt loam"),
                 strength_moist = list(`3` = "firm"), structure_grade = list(`3` = "moderate"), structure_type = list(`3` = "prismatic"),
                 ped_size_max_mm = list(`3` = 100)),
  ZPU  = .nzz_zp(c("Lm", "Ah", "E", "Bhm", "Bs", "C")),
  ZPHQ = .nzz_zp(c("Lm", "Ah", "E", "Bh", "Bsm", "C"), top_cm = .nzz_bh10$top_cm, bottom_cm = .nzz_bh10$bottom_cm),
  ZPHP = .nzz_zp(c("Lm", "Ah", "E", "Bh", "Bfm", "C"), top_cm = .nzz_bh10$top_cm, bottom_cm = .nzz_bh10$bottom_cm),
  ZPH  = .nzz_zp(top_cm = .nzz_bh10$top_cm, bottom_cm = .nzz_bh10$bottom_cm),
  ZPQ  = .nzz_zp(c("Lm", "Ah", "E", "Bh", "Bsm", "C")),
  ZPP  = .nzz_zp(c("Lm", "Ah", "E", "Bh", "Bfm", "C")),
  ZPT  = .nzz_zp(),
  ZGH  = .nzz_zg(bottom_cm = c(0, 8, 30, 40, 60, 120), top_cm = c(-3, 0, 8, 30, 40, 60)),
  ZGT  = .nzz_zg(),
  ZXU  = .nzz_zx(c("Lm", "Ah", "E", "Bhm", "Bs", "C")),
  ZXJH = .nzz_zx(c("Lm", "Ah", "E", "Bh", "Bsm", "Bt")),
  ZXH  = .nzz_zx(c("Lm", "Ah", "E", "Bh", "Bsm", "C")),
  ZXX  = .nzz_zx(c("Lm", "Ah", "E", "Bh", "Bs", "Cx")),
  ZXQ  = .nzz_zx(c("Lm", "Ah", "E", "Bs1", "Bsm", "C")),
  ZXP  = .nzz_zx(c("Lm", "Ah", "E", "Bs1", "Bfm", "C")),
  ZXF  = .nzz_zx(),
  ZOJH = .nzz_zo(c("Lm", "Ah", "E", "Bh", "Bs", "Bt")),
  ZOH  = .nzz_zo(top_cm = .nzz_bh10$top_cm, bottom_cm = .nzz_bh10$bottom_cm),
  ZOT  = .nzz_zo(),
  # ---- L
  LPI  = .nzz_l(designation = c("Ah", "Bg1", "Blm", "C"), redoximorphic_features_pct = c(0, 15, 2, 0)),
  LPT  = .nzz_l(designation = c("Ah", "Bg1", "Bw2", "C"), redoximorphic_features_pct = c(0, 15, 2, 0)),
  LGO  = .nzz_l(designation = c("Ah", "Bg1", "Bg2", "C"), redoximorphic_features_pct = c(0, 15, 15, 15),
                field_texture = list(`1` = "peaty silt loam"), site = list(drainage_class = "poorly drained")),
  LGT  = .nzz_l(designation = c("Ah", "Bg1", "Bg2", "C"), redoximorphic_features_pct = c(0, 15, 15, 15)),
  LIMI = .nzz_l(designation = c("Ah", "Bw(g)1", "Blm", "C"), site = .nzz_rhy),
  LIM  = .nzz_l(designation = c("Ah", "Bw(g)1", "Bx", "C"), site = .nzz_rhy),
  LIT  = .nzz_l(designation = c("Ah", "Bw1", "Bx", "C"), site = .nzz_rhy),
  LOM  = .nzz_lo(designation = c("Ah", "Bw(g)1", "Bw2", "C")),
  LOVA = do.call(.nzz_l, c(.nzz_lv, list(ph_h2o = list(`2` = 5.2)))),
  LOV  = do.call(.nzz_l, .nzz_lv),
  LOA  = .nzz_lo(pore_coatings_hue_moist = list(`2` = "5YR"), pore_coatings_value_moist = list(`2` = 4),
                 pore_coatings_chroma_moist = list(`2` = 6)),
  LOBG = .nzz_lo(designation = c("Ah", "Bw1", "2Bw(h)2", "2C"), bulk_density_g_cm3 = c(0.6, 0.7, 1.1, 1.2),
                 phosphate_retention_pct = c(88, 95, 40, 30), clay_pct = list(`3` = 50), field_texture = list(`3` = "clay"),
                 structure_grade = list(`3` = "moderate"), structure_type = list(`3` = "polyhedral"), ped_size_max_mm = list(`3` = 10)),
  LOT  = .nzz_lo(),
  # ---- M
  MPU  = .nzz_mp(c("Ap", "Bg", "Bqm", "C")),
  MPT  = .nzz_mp(),
  MIMC = do.call(.nzz_mi, c(list(des = c("Ap", "Bw(g)", "BC", "Cx")), .nzz_ign)),
  MIC  = do.call(.nzz_mi, c(list(des = c("Ap", "Bw", "BC", "Cx")), .nzz_ign)),
  MIM  = .nzz_mi(c("Ap", "Bw(g)", "BC", "C")),
  MIT  = .nzz_mi(),
  MOM  = .nzz_m(designation = c("Ap", "Bw(g)", "BC", "C")),
  MOZ  = .nzz_m(munsell_hue_moist = list(`2` = "7.5YR"), top_cm = c(0, 20, 45, 55), bottom_cm = c(20, 45, 55, 120)),
  MOL  = .nzz_m(phosphate_retention_pct = list(`2` = 90), bulk_density_g_cm3 = list(`2` = 0.95)),
  MOBL = .nzz_m(phosphate_retention_pct = list(`4` = 90), sand_pct = list(`4` = 35), volcanic_glass_pct = list(`4` = 10),
                field_texture = list(`4` = "silt loam"), coarse_fragments_pct = 0),
  MOS  = .nzz_m(coarse_fragments_pct = c(60, 60, 0, 0), layer_origin = c("basaltic scoria", "basaltic scoria", NA, NA)),
  MOI  = .nzz_m(munsell_chroma_moist = list(`2` = 4)),
  MOT  = make_nzsc4_example("pumice"),
  # ---- E
  EVMC = .nzz_ev(redoximorphic_features_pct = list(`3` = 5)),
  EVM  = do.call(.nzz_ev, c(.nzz_noncalc, list(designation = c("Ah", "Bw", "Bw2", "R"), redoximorphic_features_pct = list(`3` = 5)))),
  EVC  = .nzz_ev(),
  EVT  = do.call(.nzz_ev, c(.nzz_noncalc, list(designation = c("Ah", "Bw", "Bw2", "R")))),
  EPV  = .nzz_ep(cracks_width_cm = list(`2` = 1), cracks_depth_cm = list(`2` = 50)),
  EPJ  = .nzz_ep(c("Ah", "Btg", "Bk", "R"), cole_value = c(NA, 0.03, 0.03, NA)),
  EPT  = .nzz_ep(cole_value = c(NA, 0.03, 0.03, NA)),
  ERO  = .nzz_er(field_texture = list(`1` = "peaty clay loam"), site = list(drainage_class = "poorly drained")),
  ERM  = .nzz_er(c("Ah", "Bw(g)", "Bk", "R")),
  ERJ  = .nzz_er(c("Ah", "Bt", "Bk", "R")),
  ERW  = .nzz_er(),
  ERT  = .nzz_er(c("Ah", "Bw", "Ck", "R"), top_cm = c(0, 22, 30, 55), bottom_cm = c(22, 30, 55, 90)),
  EMG  = .nzz_em(ca_cmol = list(`2` = 0.5), mg_cmol = list(`2` = 5)),
  EMM  = .nzz_em(designation = c("Ah", "Bw(g)", "Bk", "R")),
  EML  = .nzz_em(phosphate_retention_pct = list(`2` = 90)),
  EMA  = .nzz_em(ph_h2o = list(`2` = 5.2)),
  EMT  = .nzz_em(),
  EOMC = .nzz_eo(c("Ah", "Bw(g)", "Bk", "R")),
  EOJC = .nzz_eo(c("Ah", "Bt", "Bk", "R")),
  EODC = .nzz_eo(),
  EOC  = .nzz_eo(structure_grade = list(`2` = "weak")),
  EOMJ = do.call(.nzz_eo, c(list(des = c("Ah", "Bt(g)", "Bw2", "R")), .nzz_noncalc)),
  EOM  = do.call(.nzz_eo, c(list(des = c("Ah", "Bw(g)", "Bw2", "R")), .nzz_noncalc)),
  EOJ  = do.call(.nzz_eo, c(list(des = c("Ah", "Bt", "Bw2", "R")), .nzz_noncalc)),
  EOT  = do.call(.nzz_eo, c(list(des = c("Ah", "Bw", "Bw2", "R")), .nzz_noncalc))
)

test_that("every subgroup of the Podzol, Allophanic, Pumice and Melanic Soils is reached by its profile", {
  skip_on_cran()
  cases <- .nzz_cases()
  expect_length(cases, 79L)
  res <- lapply(cases, classify_nzsc4, on_missing = "silent")
  got <- vapply(res, function(r) { v <- r$levels$code[r$levels$level == "subgroups"]; if (length(v)) v else NA_character_ }, "")
  expect_identical(unname(got), names(cases))
  # and each is decided: no subgroup before it is left open
  open <- vapply(res, function(r) any(grepl("^subgroups:", r$provisional_because)), NA)
  expect_identical(names(cases)[open], character(0))
  expect_identical(res$MOT$name, "Typic Orthic Pumice Soils")
  expect_identical(res$EODC$name, "Pedal-calcareous Orthic Melanic Soils")
})

test_that("the subgroup keys of the four orders are registered and validate", {
  skip_on_cran()
  r <- load_rules("nzsc4")
  expect_true(all(c("ZD", "ZP", "ZG", "ZX", "ZO", "LP", "LG", "LI", "LO", "MP", "MI", "MO",
                    "EV", "EP", "ER", "EM", "EO") %in% names(r$subgroups)))
  expect_identical(sum(lengths(r$subgroups[c("ZD", "ZP", "ZG", "ZX", "ZO", "LP", "LG", "LI", "LO", "MP", "MI", "MO",
                                             "EV", "EP", "ER", "EM", "EO")])), 79L)
  v <- soilKey:::.validate_rules("nzsc4")
  expect_true(all(lengths(v[c("missing", "mixed", "unsuffixed", "malformed", "codes")]) == 0))
  # every group of the four orders ends in a default subgroup
  last <- vapply(r$subgroups[c("ZD", "ZP", "ZG", "ZX", "ZO", "LP", "LG", "LI", "LO", "MP", "MI", "MO",
                               "EV", "EP", "ER", "EM", "EO")], function(l) isTRUE(l[[length(l)]]$tests$default), NA)
  expect_true(all(last))
})

test_that("a Bh is read by its notation and its thickness by adjacent subhorizons", {
  skip_on_cran()
  # 5 cm: ZDH (5 cm or more) but not ZPH (more than 5 cm)
  expect_true(soilKey:::bh_subgroup_nzsc(.nzz_zd(), min_cm = 5, strict = FALSE)$passed)
  expect_false(soilKey:::bh_subgroup_nzsc(.nzz_zd(), min_cm = 5)$passed)
  # Bh1 over Bh2 add up
  p <- .nzz_zd(c("Lm", "Ah", "Ed", "Bh1", "Bh2", "C"))
  expect_true(soilKey:::bh_subgroup_nzsc(p, min_cm = 5)$passed)
  # a humus-pan is not a Bh
  expect_false(soilKey:::bh_subgroup_nzsc(.nzz_zd(c("Lm", "Ah", "Ed", "Bhm", "Bs", "C")), min_cm = 0)$passed)
  # an undesignated row under the eluvial horizon may be one, unless its colour rules it out
  p <- .nzz_zd(c("Lm", "Ah", "Ed", NA, "Bs", "C"))
  expect_true(is.na(soilKey:::bh_subgroup_nzsc(p, min_cm = 0)$passed))
  p <- .nzz_zd(c("Lm", "Ah", "Ed", NA, "Bs", "C"), munsell_value_moist = list(`4` = 6))
  expect_false(soilKey:::bh_subgroup_nzsc(p, min_cm = 0)$passed)
  # ZDYH (Hugo's decision): the B the Bh tops is clayey and prismatic as a
  # whole, read from the B row below the Bh ...
  p <- do.call(.nzz_zd, c(list(), .nzz_bh10, list(field_texture = list(`4` = "clay"), structure_type = list(`4` = "prismatic"))))
  expect_false(soilKey:::bh_subgroup_nzsc(p, clayey_prismatic = TRUE)$passed)
  # ... or from the Bh itself when no B lies below it
  p <- do.call(.nzz_zd, c(list(des = c("Lm", "Ah", "Ed", "Bh", "C", "C2")), .nzz_bh10,
                          list(field_texture = list(`4` = "clay"), structure_type = list(`4` = "prismatic"))))
  expect_true(soilKey:::bh_subgroup_nzsc(p, clayey_prismatic = TRUE)$passed)
})

test_that("the pans of the Podzol subgroups count at any depth, and an argillic horizon lies beneath", {
  skip_on_cran()
  p <- .nzz_zp(c("Lm", "Ah", "E", "Bh", "Bs", "Bhm"), top_cm = c(-3, 0, 8, 30, 35, 100), bottom_cm = c(0, 8, 30, 35, 100, 120))
  expect_true(soilKey:::pan_subgroup_nzsc(p, kind = "humus_pan", anywhere = TRUE)$passed)
  expect_false(soilKey:::pan_subgroup_nzsc(p, kind = "humus_pan", depth = 90)$passed)
  # an argillic horizon above the Bh is not beneath it
  p <- .nzz_zo(c("Lm", "Ah", "Bt", "Bh", "Bs", "C"))
  expect_false(soilKey:::argillic_beneath_nzsc(p, upper = "bh")$passed)
  # a pan is not the argillic horizon beneath
  p <- .nzz_zx(c("Lm", "Ah", "E", "Bh", "Bsm", "Btm"))
  expect_false(soilKey:::argillic_beneath_nzsc(p, upper = "ortstein_or_placic")$passed)
})

test_that("a silty layer is one of the silty classes (Hugo's decision)", {
  skip_on_cran()
  mk <- function(ft) .nzz_zp(field_texture = c(NA, ft, ft, "sandy loam", "sandy loam", "silt loam"),
                             strength_moist = list(`3` = "firm"), structure_grade = list(`3` = "structureless"),
                             structure_type = list(`3` = "massive"))
  expect_true(soilKey:::silt_mantled_podzol_nzsc(mk("silty clay"))$passed)
  expect_false(soilKey:::silt_mantled_podzol_nzsc(mk("loam"))$passed)
  # a weak eluvial horizon is not silt-mantled
  p <- .nzz_zp(field_texture = c(NA, "silt loam", "silt loam", "sandy loam", "sandy loam", "silt loam"),
               strength_moist = list(`3` = "weak"), structure_grade = list(`3` = "structureless"), structure_type = list(`3` = "massive"))
  expect_false(soilKey:::silt_mantled_podzol_nzsc(p)$passed)
  # prismatic peds coarser than 200 mm do not count
  p <- .nzz_zp(field_texture = c(NA, "silt loam", "silt loam", "sandy loam", "sandy loam", "silt loam"),
               strength_moist = list(`3` = "firm"), structure_grade = list(`3` = "moderate"), structure_type = list(`3` = "prismatic"),
               ped_size_max_mm = list(`3` = 250))
  expect_false(soilKey:::silt_mantled_podzol_nzsc(p)$passed)
})

test_that("coatings on pores are recorded when met (Hugo's decision)", {
  skip_on_cran()
  # none recorded and pH 5.5 or more: not acidic
  expect_false(soilKey:::acidic_b_subgroup_nzsc(.nzz_lo(), coatings = TRUE)$passed)
  # yellower coatings do not count
  p <- .nzz_lo(pore_coatings_hue_moist = list(`2` = "10YR"), pore_coatings_value_moist = list(`2` = 4),
               pore_coatings_chroma_moist = list(`2` = 6))
  expect_false(soilKey:::acidic_b_subgroup_nzsc(p, coatings = TRUE)$passed)
  # a hue alone leaves it open
  p <- .nzz_lo(pore_coatings_hue_moist = list(`2` = "5YR"))
  expect_true(is.na(soilKey:::acidic_b_subgroup_nzsc(p, coatings = TRUE)$passed))
  # the pH route alone (EMA reads no coatings)
  expect_true(soilKey:::acidic_b_subgroup_nzsc(.nzz_lo(ph_h2o = list(`2` = 5.2)))$passed)
  # the attributes are in the schema
  spec <- soilKey:::horizon_column_spec()
  expect_true(all(c("pore_coatings_hue_moist", "pore_coatings_value_moist", "pore_coatings_chroma_moist") %in% names(spec)))
})

test_that("vitric allophanic material reads the tephric share and the sand", {
  skip_on_cran()
  expect_true(soilKey:::vitric_allophanic_nzsc(do.call(.nzz_l, .nzz_lv))$passed)
  # not tephric: no
  lv <- .nzz_lv; lv$layer_origin <- "loess"
  expect_false(soilKey:::vitric_allophanic_nzsc(do.call(.nzz_l, lv))$passed)
  # tephric, sand from the class: silt loam is less than 50 % sand only at most
  lv <- .nzz_lv; lv$sand_pct <- NULL; lv$field_texture <- c("silt loam", "silt loam", "silt loam", "loam")
  expect_true(is.na(soilKey:::vitric_allophanic_nzsc(do.call(.nzz_l, lv))$passed))
  lv$field_texture <- c("silty clay loam", "silty clay loam", "silty clay loam", "loam")
  expect_false(soilKey:::vitric_allophanic_nzsc(do.call(.nzz_l, lv))$passed)
  # no origin recorded: undecided
  lv <- .nzz_lv; lv$layer_origin <- NULL
  expect_true(is.na(soilKey:::vitric_allophanic_nzsc(do.call(.nzz_l, lv))$passed))
})

test_that("the Pumice subgroups read Hugo's decisions", {
  skip_on_cran()
  # MOZ: 7.5YR or redder, more than 20 cm
  thick <- list(top_cm = c(0, 20, 45, 55), bottom_cm = c(20, 45, 55, 120))
  expect_true(soilKey:::podzolic_pumice_nzsc(do.call(.nzz_m, c(thick, list(munsell_hue_moist = list(`2` = "5YR")))))$passed)
  expect_false(soilKey:::podzolic_pumice_nzsc(do.call(.nzz_m, c(thick, list(munsell_hue_moist = list(`2` = "2.5Y")))))$passed)
  expect_false(soilKey:::podzolic_pumice_nzsc(.nzz_m(munsell_hue_moist = list(`2` = "5YR")))$passed)
  # MIC: any ignimbrite counts; brittle failure is recorded when met
  ig <- .nzz_ign; ig$layer_origin <- list(`4` = "non-welded ignimbrite")
  expect_true(soilKey:::compacted_ignimbrite_nzsc(do.call(.nzz_mi, c(list(des = c("Ap", "Bw", "BC", "Cx")), ig)))$passed)
  expect_false(soilKey:::compacted_ignimbrite_nzsc(do.call(.nzz_mi, c(list(des = c("Ap", "Bw", "BC", "C")), .nzz_ign)))$passed)
  # roots not recorded leave it open
  ig <- .nzz_ign; ig$roots_abundance <- NULL
  expect_true(is.na(soilKey:::compacted_ignimbrite_nzsc(do.call(.nzz_mi, c(list(des = c("Ap", "Bw", "BC", "Cx")), ig)))$passed))
  # MOI: a Bw thicker than 30 cm is not immature
  p <- .nzz_m(munsell_chroma_moist = list(`2` = 4), top_cm = c(0, 20, 55, 65), bottom_cm = c(20, 55, 65, 120))
  expect_false(soilKey:::immature_pumice_nzsc(p)$passed)
  # MOS: fragments not recorded read as none
  expect_false(soilKey:::scoriaceous_pumice_nzsc(.nzz_m(layer_origin = c("basaltic scoria", "basaltic scoria", NA, NA)))$passed)
  # allophanic material without the bulk density (MOL, MOBL)
  x <- soilKey:::.nzsc_horizons(.nzz_m(phosphate_retention_pct = list(`2` = 90), bulk_density_g_cm3 = list(`2` = 0.95)))
  expect_false(isTRUE(soilKey:::.nzsc_allophanic_rows(x)[2]))
  expect_true(soilKey:::.nzsc_allophanic_rows(x, density = FALSE)[2])
})

test_that("the Melanic subgroups read their depths, and the shared gates keep their defaults", {
  skip_on_cran()
  # a calcareous horizon with its top at 70 cm: EOC (90 cm) but not GOC (60 cm)
  p <- .nzz_eo(top_cm = c(0, 22, 70, 95), bottom_cm = c(22, 70, 95, 110), effervescence_class = c("none", "none", "strong", NA),
               ph_h2o = c(6.5, 6.5, 8, NA), designation = c("Ah", "Bw", "Ck", "R"))
  expect_true(soilKey:::calcareous_subgroup_nzsc(p, limit = "at_or_less", depth = 90)$passed)
  expect_false(soilKey:::calcareous_subgroup_nzsc(p, limit = "at_or_less")$passed)
  # redox segregations from 30 to 60 cm count, deeper ones do not
  expect_true(soilKey:::redox_subgroup_nzsc(.nzz_ev(redoximorphic_features_pct = list(`3` = 5)))$passed)
  p <- .nzz_ev(redoximorphic_features_pct = list(`3` = 5), top_cm = c(0, 22, 65, 75), bottom_cm = c(22, 65, 75, 90))
  expect_false(soilKey:::redox_subgroup_nzsc(p)$passed)
  # EML: a measured P retention decides
  p <- .nzz_em(phosphate_retention_pct = list(`2` = 60), naf_test = list(`2` = "strong"))
  expect_false(soilKey:::allophanic_b_subgroup_nzsc(p)$passed)
  p <- .nzz_em(phosphate_retention_pct = list(`2` = NA), naf_test = list(`2` = "strong"))
  expect_true(soilKey:::allophanic_b_subgroup_nzsc(p)$passed)
  # EMG reads the B or BC; UYG the B only
  p <- .nzz_em(designation = c("Ah", "Bw", "BCk", "R"), ca_cmol = list(`3` = 0.5), mg_cmol = list(`3` = 5))
  expect_true(soilKey:::magnesic_subgroup_nzsc(p, bc = TRUE)$passed)
  expect_false(soilKey:::magnesic_subgroup_nzsc(p)$passed)
  # EPV has no condition on redox segregations
  p <- .nzz_ep(cracks_width_cm = list(`2` = 1), cracks_depth_cm = list(`2` = 50))
  expect_true(soilKey:::vertic_subgroup_nzsc(p)$passed)
  # LGO and ERO: a buried peaty topsoil does not count
  p <- .nzz_er(field_texture = list(`1` = "clay loam"), site = list(drainage_class = "poorly drained"))
  expect_false(soilKey:::peaty_subgroup_nzsc(p, buried = FALSE)$passed)
})

test_that("the findings of the two reviews stay fixed", {
  skip_on_cran()
  ok_fast <- function(p) { t0 <- proc.time()[["elapsed"]]; r <- classify_nzsc4(p, on_missing = "silent")
    expect_lt(proc.time()[["elapsed"]] - t0, 30); r }
  # a row of no thickness no longer stops the walks (MOI, LOBG, the acidic subgroups)
  ok_fast(.nzz_m(designation = c("Ap", "Bw", "Bw2", "C"), top_cm = c(0, 20, 35, 35), bottom_cm = c(20, 35, 35, 120)))
  ok_fast(.nzz_lo(designation = c("Ah", "Bw1", "2Bw(h)2", "2C"), top_cm = c(0, 20, 50, 50), bottom_cm = c(20, 50, 50, 130)))
  expect_false(is.null(soilKey:::acidic_subgroup_nzsc(.nzz_mod("gley", h = list(designation = c("Ap", NA, "Bg2", "Bg3", "Cg"),
                         top_cm = c(0, 20, 20, 45, 75), bottom_cm = c(20, 20, 45, 75, 130))))$passed))
  # compaction: one measure below its limit and the other not recorded leaves it open
  ig <- .nzz_ign; ig$packing_degree <- list(`4` = "low")
  expect_true(is.na(soilKey:::compacted_ignimbrite_nzsc(do.call(.nzz_mi, c(list(des = c("Ap", "Bw", "BC", "Cx")), ig)))$passed))
  # a Bh needs no eluvial horizon: an undesignated dark row may be one
  p <- .nzz_zo(c("Lm", "Ah", NA, "Bh", "Bs", "C"), munsell_value_moist = list(`3` = 3), munsell_chroma_moist = list(`3` = 2),
               oc_pct = list(`3` = 3))
  expect_true(is.na(soilKey:::bh_subgroup_nzsc(p, min_cm = 5)$passed))
  # LOBG reads a buried B
  b <- list(designation = c("Ah", "Bw1", "2bBw(h)2", "2C"), bulk_density_g_cm3 = c(0.6, 0.7, 1.1, 1.2),
            phosphate_retention_pct = c(88, 95, 40, 30), clay_pct = list(`3` = 50), field_texture = list(`3` = "clay"),
            structure_grade = list(`3` = "moderate"), structure_type = list(`3` = "polyhedral"), ped_size_max_mm = list(`3` = 10))
  expect_true(soilKey:::buried_granular_allophanic_nzsc(do.call(.nzz_lo, b))$passed)
  # ... and an undecided allophanic row above it leaves it open
  b$bulk_density_g_cm3 <- c(0.6, NA, 1.1, 1.2)
  expect_true(is.na(soilKey:::buried_granular_allophanic_nzsc(do.call(.nzz_lo, b))$passed))
  # EODC: a row of unknown role under the A may be A too
  p <- .nzz_eo(c("Ah", NA, "Bw", "R"), top_cm = c(0, 15, 22, 75), bottom_cm = c(15, 22, 75, 90),
               structure_grade = list(`2` = "weak"), structure_type = list(`2` = "granular"))
  expect_true(is.na(soilKey:::pedal_weathered_b_nzsc(p)$passed))
  # B or BC: a thin acid B over an undesignated row stays open
  p <- .nzz_em(designation = c("Ah", "Bw", NA, "R"), top_cm = c(0, 22, 28, 75), bottom_cm = c(22, 28, 75, 90),
               ph_h2o = c(6.8, 5.2, 6.5, NA))
  expect_true(is.na(soilKey:::acidic_b_subgroup_nzsc(p)$passed))
  # within 90 cm: a pan with its top at 90 cm does not count (LPI)
  p <- .nzz_l(designation = c("Ah", "Bg1", "Bw2", "Blm"), redoximorphic_features_pct = c(0, 15, 2, 0),
              top_cm = c(0, 20, 50, 90), bottom_cm = c(20, 50, 90, 130))
  expect_false(soilKey:::pan_subgroup_nzsc(p, kind = "ironstone_pan", depth = 90, limit = "within")$passed)
  expect_true(soilKey:::pan_subgroup_nzsc(p, kind = "ironstone_pan", depth = 90)$passed)
  # MOL (Hugo's decision): the allophanic layer must be vitric itself
  p <- .nzz_m(phosphate_retention_pct = list(`3` = 90), sand_pct = list(`3` = 30), volcanic_glass_pct = list(`3` = 10),
              coarse_fragments_pct = 0)
  expect_false(soilKey:::allophanic_in_vitric_nzsc(p)$passed)
  # MOS (Hugo's decision): weighted by thickness; andesitic scoria is not basaltic
  p <- .nzz_m(coarse_fragments_pct = c(40, 70, 0, 0), top_cm = c(0, 10, 35, 55), bottom_cm = c(10, 35, 55, 120),
              layer_origin = c("scoria", "scoria", NA, NA))
  expect_true(soilKey:::scoriaceous_pumice_nzsc(p)$passed)
  p <- .nzz_m(coarse_fragments_pct = c(60, 60, 0, 0), layer_origin = c("andesitic scoria", "andesitic scoria", NA, NA))
  expect_false(soilKey:::scoriaceous_pumice_nzsc(p)$passed)
  # MOI: a neutral Bw is yellower than 10YR; chroma to the nearest chip
  expect_true(soilKey:::immature_pumice_nzsc(.nzz_m(munsell_hue_moist = list(`2` = "N"), munsell_chroma_moist = list(`2` = 0)))$passed)
  expect_true(soilKey:::immature_pumice_nzsc(.nzz_m(munsell_chroma_moist = list(`2` = 4.3)))$passed)
  # coatings of value 5 may be cutanic (chroma not recorded)
  p <- .nzz_er(c("Ah", "BC", "C", "R"), clay_films_amount = list(`2` = "common"), coatings_value_moist = list(`2` = 5))
  expect_true(is.na(soilKey:::weathered_or_cutanic_nzsc(p)$passed))
  # litter is not silty
  x <- soilKey:::.nzsc_horizons(.nzz_zp())
  expect_false(soilKey:::.nzsc_silty_rows(x)[1])
  # a weak grade rules the vertic pedality out on its own
  p <- .nzz_ep(structure_grade = c("strong", "weak", "weak", NA), structure_type = c("granular", NA, NA, NA),
               cole_value = c(NA, 0.12, 0.12, NA))
  expect_false(soilKey:::vertic_subgroup_nzsc(p)$passed)
  # EMG reads an undesignated row too
  p <- .nzz_em(designation = c("Ah", NA, "Bk", "R"), ca_cmol = list(`2` = 0.5), mg_cmol = list(`2` = 5))
  expect_true(is.na(soilKey:::magnesic_subgroup_nzsc(p, bc = TRUE)$passed))
})
