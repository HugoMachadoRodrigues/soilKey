# v0.9.301: the keys to subgroups of the New Zealand Soil Classification,
# 4th ed., for the Semiarid, Oxidic, Granular and Pallic Soils (78
# subgroups). One profile per subgroup, built from the order examples, keys
# to its group and its subgroup; and the readings where the text is silent
# (inst/rules/nzsc4/readings.yaml).

.nzp_mod <- function(order, h = list(), site = list()) {
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
.nzp_h <- function(base, ...) {
  new <- list(...)
  for (nm in names(new)) {
    v <- new[[nm]]; b <- base[[nm]]
    base[[nm]] <- if (is.list(v) && !is.null(b) && !is.list(b)) { for (k in names(v)) b[as.integer(k)] <- v[[k]]; b }
                  else if (is.list(v) && is.list(b)) utils::modifyList(b, v) else v
  }
  base
}
.nzp_wf <- list(coarse_fragments_pct = 10, weathered_fragments_pct = 80)
# ---- S
.nzp_sa <- function(des = c("Ap", "Bt", "Bk", "C"), ...)
  .nzp_mod("semiarid", h = .nzp_h(list(designation = des, munsell_hue_moist = c("10YR", "7.5YR", "10YR", "2.5Y")), ...))
.nzp_sz <- function(...) .nzp_mod("semiarid", h = .nzp_h(list(designation = c("Ap", "Bt", "Bk", "C"),
                                  structure_type = c("granular", "prismatic", "blocky", "massive"), clay_films_amount = list(`2` = "many"),
                                  coatings_value_moist = list(`2` = 3), ph_h2o = c(6.8, 8.8, 8.2, 8.3)), ...))
.nzp_sj <- function(des = c("Ap", "Bt", "Bk", "C"), ...) .nzp_mod("semiarid", h = .nzp_h(list(designation = des), ...))
.nzp_si <- function(...) .nzp_mod("semiarid", h = list(...))
# ---- X
.nzp_xp <- function(des = c("Ah", "Bog1", "Bo2", "BC"), ...)
  .nzp_mod("oxidic", h = .nzp_h(list(designation = des, redoximorphic_features_pct = c(0, 10, 0, 0),
                                     munsell_value_moist = c(3, 5, 3, 4), munsell_chroma_moist = c(3, 2, 6, 6)), ...))
.nzp_rock <- list(top_cm = c(0, 18, 60, 80), bottom_cm = c(18, 60, 80, 150))
.nzp_xn <- function(des = c("Ah", "Boi1", "Bo2", "BC"), ...) .nzp_mod("oxidic", h = .nzp_h(list(designation = des), ...))
.nzp_xo <- function(...) .nzp_mod("oxidic", h = list(...))
# ---- N
.nzp_np <- function(des = c("Ah", "Bg(h)1", "Bw(h)2", "BC"), ...)
  .nzp_mod("granular", h = .nzp_h(list(designation = des, redoximorphic_features_pct = c(0, 10, 0, 0),
                                       munsell_value_moist = c(3, 5, 5, 5), munsell_chroma_moist = c(2, 2, 6, 6)), ...))
.nzp_ne <- function(...) .nzp_mod("granular", h = .nzp_h(list(ph_h2o = c(6.0, 6.1, 6.2, 6.2)), ...))
.nzp_nx <- function(...) .nzp_mod("granular", h = .nzp_h(list(ped_size_max_mm = c(5, 10, 5, 5), stickiness = "very sticky",
                                  plasticity = "very plastic", structure_type = c("granular", "polyhedral", "polyhedral", "polyhedral")), ...))
.nzp_no <- function(...) .nzp_mod("granular", h = list(...))
.nzp_gm <- c("Ah", "Bw(h)(g)1", "Bw(h)2", "BC")
# ---- P
.nzp_pp <- function(des = c("Ap", "Bg", "Bx", "C"), ...)
  .nzp_mod("pallic", h = .nzp_h(list(designation = des, redoximorphic_features_pct = c(0, 15, 2, 0),
                                     munsell_value_moist = c(4, 6, 6, 6), munsell_chroma_moist = c(2, 2, 4, 4)), ...))
.nzp_p <- function(des, ...) .nzp_mod("pallic", h = .nzp_h(list(designation = des), ...))
.nzp_cases <- function() list(
  # ---- S
  SAM  = .nzp_sa(c("Ap", "Bt(g)", "Bk", "C")),
  SAW  = do.call(.nzp_sa, .nzp_wf),
  SAK  = .nzp_sa(ph_h2o = list(`3` = 8.8)),
  SAH  = .nzp_sa(top_cm = c(0, 18, 50, 80), bottom_cm = c(18, 50, 80, 120)),
  SAT  = .nzp_sa(),
  SZQ  = .nzp_sz(ec_1to5_dS_m = list(`2` = 1.5)),
  SZT  = .nzp_sz(),
  SJM  = .nzp_sj(c("Ap", "Bt(g)", "Bk", "C")),
  SJQ  = .nzp_sj(ec_1to5_dS_m = list(`2` = 1.5)),
  SJK  = .nzp_sj(ph_h2o = list(`3` = 8.8)),
  SJL  = .nzp_sj(c("Ap", "Bt lamellae", "Bk", "C")),
  SJT  = .nzp_sj(),
  SIM  = .nzp_si(designation = c("Ap", "Bw(g)", "Bk", "C")),
  SIQ  = .nzp_si(ec_1to5_dS_m = list(`2` = 1.5)),
  SIK  = .nzp_si(ph_h2o = list(`3` = 8.8)),
  SIT  = make_nzsc4_example("semiarid"),
  # ---- X
  XPN  = .nzp_xp(c("Ah", "Boig1", "Bo2", "BC")),
  XPT  = .nzp_xp(),
  XNX  = do.call(.nzp_xn, c(list(des = c("Ah", "Boi1", "Bo2", "R")), .nzp_rock)),
  XNB  = .nzp_xn(coarse_fragments_pct = list(`2` = 10)),
  XNT  = .nzp_xn(),
  XOM  = .nzp_xo(designation = c("Ah", "Bo(g)1", "Bo2", "BC")),
  XOX  = do.call(.nzp_xo, c(list(designation = c("Ah", "Bo1", "Bo2", "R")), .nzp_rock)),
  XOB  = .nzp_xo(coarse_fragments_pct = list(`2` = 10)),
  XOT  = make_nzsc4_example("oxidic"),
  # ---- N
  NPX  = .nzp_np(ped_size_max_mm = list(`3` = 5), stickiness = list(`3` = "very sticky"), plasticity = list(`3` = "very plastic")),
  NPA  = .nzp_np(ph_h2o = list(`2` = 5.0)),
  NPT  = .nzp_np(),
  NEM  = .nzp_ne(designation = .nzp_gm),
  NEL  = .nzp_ne(phosphate_retention_pct = list(`2` = 90)),
  NET  = .nzp_ne(),
  NXMA = .nzp_nx(designation = .nzp_gm, ph_h2o = list(`2` = 5.0)),
  NXM  = .nzp_nx(designation = .nzp_gm),
  NXL  = .nzp_nx(phosphate_retention_pct = list(`2` = 90)),
  NXA  = .nzp_nx(ph_h2o = list(`2` = 5.0)),
  NXT  = .nzp_nx(),
  NOMA = .nzp_no(designation = .nzp_gm, ph_h2o = list(`2` = 5.0)),
  NOM  = .nzp_no(designation = .nzp_gm),
  NOL  = .nzp_no(phosphate_retention_pct = list(`2` = 90)),
  NOA  = .nzp_no(ph_h2o = list(`2` = 5.0, `3` = 5.0)),
  NOT  = make_nzsc4_example("granular"),
  # ---- P
  PPU  = .nzp_pp(c("Ap", "Bg", "Bx", "Bqm")),
  PPJX = .nzp_pp(c("Ap", "Btg", "Bx", "C")),
  PPX  = .nzp_pp(),
  PPWJ = do.call(.nzp_pp, c(list(des = c("Ap", "Btg", "Bw", "C")), .nzp_wf)),
  PPJ  = .nzp_pp(c("Ap", "Btg", "Bw", "C")),
  PPC  = .nzp_pp(c("Ap", "Bg", "Bw", "C"), cementation_class = list(`3` = "weakly")),
  PPT  = .nzp_pp(c("Ap", "Bg", "Bw", "C")),
  PUM  = .nzp_p(c("Ap", "Bw(g)", "Bqm", "C")),
  PUJN = .nzp_p(c("Ap", "Btn", "Bqm", "C")),
  PUJ  = .nzp_p(c("Ap", "Bt", "Bqm", "C")),
  PUT  = .nzp_p(c("Ap", "Bw", "Bqm", "C")),
  PXJN = .nzp_p(c("Ap", "Btn", "Bx", "C")),
  PXMJ = .nzp_p(c("Ap", "Bt(g)", "Bx", "C")),
  PXJC = .nzp_p(c("Ap", "Bt", "Bx", "Ck")),
  PXJ  = .nzp_p(c("Ap", "Bt", "Bx", "C")),
  PXMC = .nzp_p(c("Ap", "Bw(g)", "Bx", "Ck")),
  PXM  = .nzp_p(c("Ap", "Bw(g)", "Bx", "C")),
  PXCN = .nzp_p(c("Ap", "Bw", "Bx", "Ckn")),
  PXC  = .nzp_p(c("Ap", "Bw", "Bx", "Ck")),
  PXT  = make_nzsc4_example("pallic"),
  PLM  = .nzp_p(c("Ap", "Bw(g)", "Bt(x) lamellae", "C")),
  PLC  = .nzp_p(c("Ap", "Bw", "Bt(x) lamellae", "Ck")),
  PLT  = .nzp_p(c("Ap", "Bw", "Bt(x) lamellae", "C")),
  PJMY = .nzp_p(c("Ap", "E", "Btg", "C"), ph_h2o = list(`3` = 5.0)),
  PJMW = do.call(.nzp_p, c(list(des = c("Ap", "Bt(g)", "BC", "C")), .nzp_wf)),
  PJM  = .nzp_p(c("Ap", "Bt(g)", "BC", "C")),
  PJA  = .nzp_p(c("Ap", "E", "Bt", "C"), munsell_hue_moist = list(`3` = "7.5YR"), phosphate_retention_pct = list(`3` = 25)),
  PJY  = .nzp_p(c("Ap", "E", "Bt", "C"), ph_h2o = list(`3` = 5.0)),
  PJN  = .nzp_p(c("Ap", "Btn", "BC", "C")),
  PJW  = do.call(.nzp_p, c(list(des = c("Ap", "Bt", "BC", "C")), .nzp_wf)),
  PJC  = .nzp_p(c("Ap", "Bt", "BCk", "C")),
  PJT  = .nzp_p(c("Ap", "Bt", "BC", "C")),
  PIMD = .nzp_p(c("Ap", "Bw(g)", "BC", "C"), ped_size_max_mm = list(`2` = 50)),
  PIM  = .nzp_p(c("Ap", "Bw(g)", "BC", "C"), ped_size_max_mm = list(`2` = 150)),
  PIC  = .nzp_p(c("Ap", "Bw", "BCk", "C")),
  PID  = .nzp_p(c("Ap", "Bw", "BC", "C"), ped_size_max_mm = list(`2` = 50)),
  PIT  = .nzp_p(c("Ap", "Bw", "BC", "C"), ped_size_max_mm = list(`2` = 150))
)

test_that("every subgroup of the Semiarid, Oxidic, Granular and Pallic Soils is reached by its profile", {
  skip_on_cran()
  cases <- .nzp_cases()
  expect_length(cases, 78L)
  res <- lapply(cases, classify_nzsc4, on_missing = "silent")
  got <- vapply(res, function(r) { v <- r$levels$code[r$levels$level == "subgroups"]; if (length(v)) v else NA_character_ }, "")
  expect_identical(unname(got), names(cases))
  # and each is decided: no subgroup before it is left open
  open <- vapply(res, function(r) any(grepl("^subgroups:", r$provisional_because)), NA)
  expect_identical(names(cases)[open], character(0))
  expect_identical(res$PXT$name, "Typic Fragic Pallic Soils")
})

test_that("the subgroup keys of the four orders are registered and validate", {
  skip_on_cran()
  r <- load_rules("nzsc4")
  g <- c("SA", "SZ", "SJ", "SI", "XP", "XN", "XO", "NP", "NE", "NX", "NO", "PP", "PU", "PX", "PL", "PJ", "PI")
  expect_true(all(g %in% names(r$subgroups)))
  expect_identical(sum(lengths(r$subgroups[g])), 78L)
  v <- soilKey:::.validate_rules("nzsc4")
  expect_true(all(lengths(v[c("missing", "mixed", "unsuffixed", "malformed", "codes")]) == 0))
  expect_true(all(vapply(r$subgroups[g], function(l) isTRUE(l[[length(l)]]$tests$default), NA)))
})

test_that("weathered coarse fragments are recorded when met (Hugo's decision)", {
  skip_on_cran()
  # none recorded: no; no fragments at all: no
  expect_false(soilKey:::weathered_fragments_nzsc(.nzp_sa(coarse_fragments_pct = 10))$passed)
  expect_false(soilKey:::weathered_fragments_nzsc(.nzp_sa(weathered_fragments_pct = 90))$passed)
  # the majority by volume: a horizon rich in fresh fragments outweighs a thin weathered one
  p <- .nzp_sa(coarse_fragments_pct = c(0, 5, 40, 0), weathered_fragments_pct = c(NA, 100, 10, NA))
  expect_false(soilKey:::weathered_fragments_nzsc(p)$passed)
  p <- .nzp_sa(coarse_fragments_pct = c(0, 40, 5, 0), weathered_fragments_pct = c(NA, 100, 10, NA))
  expect_true(soilKey:::weathered_fragments_nzsc(p)$passed)
  expect_true("weathered_fragments_pct" %in% names(soilKey:::horizon_column_spec()))
})

test_that("sodic features not measured are absent without the n suffix (Hugo's decision)", {
  skip_on_cran()
  # a designated argillic horizon without n: not sodic, so PUJ, not PUJN
  expect_false(soilKey:::near_layer_subgroup_nzsc(.nzp_p(c("Ap", "Bt", "Bqm", "C")), layer = "argillic_or_cutanic",
                                                 feature = "sodic")$passed)
  # measured ESP decides
  p <- .nzp_p(c("Ap", "Bt", "Bqm", "C"), esp_pct = list(`2` = 15), structure_type = list(`2` = "prismatic"))
  expect_true(soilKey:::near_layer_subgroup_nzsc(p, layer = "argillic_or_cutanic", feature = "sodic")$passed)
  # an undesignated row beneath the argillic horizon stays open
  p <- .nzp_p(c("Ap", "Bt", NA, "C"))
  expect_true(is.na(soilKey:::near_layer_subgroup_nzsc(p, layer = "argillic", feature = "sodic")$passed))
  # PUJN needs the sodic features for the argillic horizon too (else PUJ would never be reached)
  res <- classify_nzsc4(.nzp_p(c("Ap", "Bt", "Bqm", "C")), on_missing = "silent")
  expect_identical(res$levels$code[res$levels$level == "subgroups"], "PUJ")
})

test_that("the Pallic and Semiarid subgroups read their layers", {
  skip_on_cran()
  # a calcareous fragipan counts as carbonate in its prism interiors (PXC)
  expect_true(soilKey:::near_layer_subgroup_nzsc(.nzp_p(c("Ap", "Bw", "Bxk", "C")), layer = "fragipan",
                                                feature = "calcareous")$passed)
  # a calcareous horizon not next to the fragipan does not count
  expect_false(soilKey:::near_layer_subgroup_nzsc(.nzp_p(c("Ap", "Bx", "BC", "Ck")), layer = "fragipan",
                                                 feature = "calcareous")$passed)
  # an argillic horizon below the fragipan does not overlie it
  expect_false(soilKey:::over_fragipan_subgroup_nzsc(.nzp_p(c("Ap", "Bw", "Bx", "Bt")))$passed)
  # SJL: lamellae make most of the argillic horizon
  p <- .nzp_sj(c("Ap", "Bt lamellae", "Bt2", "C"), top_cm = c(0, 18, 45, 80), bottom_cm = c(18, 45, 80, 120))
  expect_false(soilKey:::lamellar_argillic_nzsc(p)$passed)
  # SAH: 30 cm or more of argillic horizon
  expect_false(soilKey:::argillic_subgroup_nzsc(.nzp_sa(), min_cm = 30)$passed)
  # alkaline: pH 8.6 only below 60 cm does not count
  expect_false(soilKey:::alkaline_subgroup_nzsc(.nzp_si(ph_h2o = list(`4` = 9.0)))$passed)
  # PPC: a cementation class of weakly cemented within 90 cm
  expect_false(soilKey:::cemented_subgroup_nzsc(.nzp_pp(c("Ap", "Bg", "Bw", "C"), cementation_class = list(`4` = "weakly")))$passed)
  # PID: peds coarser than 100 mm fail; not recorded leaves it open
  expect_false(soilKey:::pedal_b_subgroup_nzsc(.nzp_p(c("Ap", "Bw", "BC", "C"), ped_size_max_mm = list(`2` = 150)))$passed)
  expect_true(is.na(soilKey:::pedal_b_subgroup_nzsc(.nzp_p(c("Ap", "Bw", "BC", "C")))$passed))
  # PJC: a calcareous horizon directly below the argillic horizon, deeper than 90 cm
  p <- .nzp_p(c("Ap", "Bt", "BCk", "C"), top_cm = c(0, 20, 95, 110), bottom_cm = c(20, 95, 110, 130))
  expect_true(soilKey:::calcareous_argillic_nzsc(p)$passed)
})

test_that("the Oxidic and Granular subgroups read their windows", {
  skip_on_cran()
  # XNB: a decrease of more than 20 % below the clay maximum within 90 cm
  p <- .nzp_xn(clay_pct = c(55, 70, 50, 60), top_cm = c(0, 18, 60, 90), bottom_cm = c(18, 60, 90, 150))
  expect_true(soilKey:::brown_oxidic_nzsc(p)$passed)
  p <- .nzp_xn(clay_pct = c(55, 70, 60, 60), top_cm = c(0, 18, 60, 90), bottom_cm = c(18, 60, 90, 150))
  expect_false(soilKey:::brown_oxidic_nzsc(p)$passed)
  # a clay not measured leaves the decrease open
  p <- .nzp_xn(clay_pct = c(55, 70, NA, 60))
  expect_true(is.na(soilKey:::brown_oxidic_nzsc(p)$passed))
  # XNX: rock below 90 cm does not count; a description ending at 70 cm leaves it open
  expect_false(soilKey:::contact_subgroup_nzsc(.nzp_xn(c("Ah", "Boi1", "Bo2", "R")))$passed)
  p <- .nzp_xn(c("Ah", "Boi1", "Bo2", "BC"), top_cm = c(0, 18, 40, 60), bottom_cm = c(18, 40, 60, 70))
  expect_true(is.na(soilKey:::contact_subgroup_nzsc(p)$passed))
  # NPX: the cutanoxidic horizon must start below 25 cm
  p <- .nzp_np(ped_size_max_mm = list(`2` = 5, `3` = 5), stickiness = "very sticky", plasticity = "very plastic",
               designation = c("Ah", "Bw(h)1", "Bw(h)2", "BC"), top_cm = c(0, 15, 45, 95), bottom_cm = c(15, 45, 95, 130))
  expect_false(soilKey:::cutanoxidic_subgroup_nzsc(p)$passed)
  # NPA reads 5.1 or less; NXA less than 5.1
  expect_true(soilKey:::acidic_b_subgroup_nzsc(.nzp_np(ph_h2o = list(`2` = 5.1)), ph = 5.1, strict = FALSE)$passed)
  expect_false(soilKey:::acidic_b_subgroup_nzsc(.nzp_np(ph_h2o = list(`2` = 5.1)), ph = 5.1)$passed)
  # NOA: the major part of the B or BC to 60 cm
  expect_false(soilKey:::acidic_b_subgroup_nzsc(.nzp_no(ph_h2o = list(`3` = 5.0)), ph = 5.1, part = "major")$passed)
  # NEL: the allophanic layer must be 10 cm thick within 60 cm
  p <- .nzp_ne(phosphate_retention_pct = list(`3` = 90))
  expect_true(soilKey:::allophanic_layer_subgroup_nzsc(p)$passed)
  p <- .nzp_ne(phosphate_retention_pct = list(`3` = 90), top_cm = c(0, 15, 55, 95), bottom_cm = c(15, 55, 95, 130))
  expect_false(soilKey:::allophanic_layer_subgroup_nzsc(p)$passed)
})

test_that("the findings of the two reviews stay fixed", {
  skip_on_cran()
  # XNB: a missing clay above the decrease does not leave it open
  expect_true(soilKey:::brown_oxidic_nzsc(.nzp_xn(clay_pct = c(NA, 70, 50, 60)))$passed)
  # ... one below it does
  expect_true(is.na(soilKey:::brown_oxidic_nzsc(.nzp_xn(clay_pct = c(55, 70, 60, NA), top_cm = c(0, 18, 60, 80),
                                                        bottom_cm = c(18, 60, 80, 150)))$passed))
  # PID: an undesignated row under a thin pedal B may be B
  p <- .nzp_p(c("Ap", "Bw", NA, "C"), top_cm = c(0, 20, 30, 90), bottom_cm = c(20, 30, 90, 120),
              ped_size_max_mm = list(`2` = 50), structure_grade = list(`3` = "structureless"))
  expect_true(is.na(soilKey:::pedal_b_subgroup_nzsc(p)$passed))
  # PID: a massive B with no grade recorded is apedal
  p <- .nzp_p(c("Ap", "Bw", "BC", "C"), structure_grade = list(`2` = NA), structure_type = list(`2` = "massive"))
  expect_false(soilKey:::pedal_b_subgroup_nzsc(p)$passed)
  # SJL: lamellae with no bottom weigh both ways
  p <- .nzp_sj(c("Ap", "Bt", "Bt lamellae", "C"), top_cm = c(0, 18, 30, 80), bottom_cm = c(18, 30, NA, NA))
  expect_true(is.na(soilKey:::lamellar_argillic_nzsc(p)$passed))
  # PPC: a densipan or fragipan recorded as a pan type is not cemented
  expect_false(soilKey:::cemented_subgroup_nzsc(.nzp_pp(c("Ap", "Bg", "Bw", "C"), pan_type = list(`3` = "densipan")))$passed)
  # a fragipan the description ends in may have a calcareous horizon beneath
  p <- .nzp_p(c("Ap", "Bw", "Bx", NA), top_cm = c(0, 20, 45, NA), bottom_cm = c(20, 45, 90, NA))
  expect_true(is.na(soilKey:::near_layer_subgroup_nzsc(p, layer = "fragipan", feature = "calcareous")$passed))
  # XOX: an undesignated row with no fine-earth record may be rock
  p <- .nzp_xo(designation = c("Ah", "Bo1", "Bo2", NA), clay_pct = list(`4` = NA), sand_pct = list(`4` = NA),
               silt_pct = list(`4` = NA), ph_h2o = list(`4` = NA), oc_pct = list(`4` = NA), field_texture = list(`4` = NA),
               top_cm = c(0, 18, 60, 80), bottom_cm = c(18, 60, 80, 150))
  expect_true(is.na(soilKey:::contact_subgroup_nzsc(p)$passed))
  # sodic: Na below 0.7 with no CEC is not a measure
  p <- .nzp_p(c("Ap", "Bt", "BC", "C"), na_cmol = list(`2` = 0.5), structure_type = list(`2` = "prismatic"))
  expect_false(soilKey:::near_layer_subgroup_nzsc(p, layer = "argillic", feature = "sodic")$passed)
  p <- .nzp_p(c("Ap", "Bt", "BC", "C"), na_cmol = list(`2` = 0.5), cec_cmol = list(`2` = 5), structure_type = list(`2` = "prismatic"))
  expect_true(soilKey:::near_layer_subgroup_nzsc(p, layer = "argillic", feature = "sodic")$passed)
  # weathered share recorded where the fragments are not
  expect_true(soilKey:::weathered_fragments_nzsc(.nzp_sa(coarse_fragments_pct = NA, weathered_fragments_pct = c(NA, 90, 90, NA)))$passed)
  # coatings recorded with no colour value may be dark (cutanic)
  x <- soilKey:::.nzsc_horizons(.nzp_p(c("Ap", "Bw", "BC", "C"), clay_films_amount = list(`2` = "many")))
  expect_true(is.na(soilKey:::.nzsc_cutanic_or_coated(x)[2]))
})
