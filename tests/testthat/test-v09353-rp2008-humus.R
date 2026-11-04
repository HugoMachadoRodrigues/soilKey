# v0.9.353 -- the humus forms of the Referentiel pedologique 2008 (Annex 1):
# humus_form_rp(), the gates, the key of Figure 2 (11 outcomes), the open
# land, the qualifiers, the recorded form (Q22) and the result of
# classify_rp2008().

# A profile from its designations (top down), the thicknesses and any
# horizon columns; the RP notation unless `sys` says otherwise.
.hf_P <- function(des, th = NULL, site = list(), sys = "rp2008", ...) {
  n <- length(des)
  if (is.null(th)) th <- c(rep(2, max(0, n - 2)), 15, 40)[seq_len(n)]
  top <- cumsum(c(0, th))[seq_len(n)]
  h <- data.frame(designation = des, top_cm = top, bottom_cm = top + th, stringsAsFactors = FALSE)
  ex <- list(...)
  for (nm in names(ex)) h[[nm]] <- ex[[nm]]
  PedonRecord$new(site = c(list(id = "hf", designation_system = sys), site), horizons = h)
}
# Everything print() writes (cli to the messages, the trace to the output).
.hf_print <- function(res) { m <- NULL; o <- utils::capture.output(m <- utils::capture.output(print(res), type = "message")); c(m, o) }
# A column with `v` on layer `k` of `n`.
.hf_at <- function(n, k, v) { out <- rep(NA, n); out[k] <- v; out }
.hf <- function(p) humus_form_rp(p)
.hf_form <- function(p) .hf(p)$form

test_that("the eleven outcomes of the key of Figure 2 (p. 354)", {
  skip_on_cran()
  # A clearly crumbly: biogenic aggregates over 25 %, macro
  cr <- function(n, k) list(biogenic_aggregates_pct = .hf_at(n, k, 60), biogenic_aggregate_size = .hf_at(n, k, "macro"))
  nc <- function(n, k) list(biogenic_aggregates_pct = .hf_at(n, k, 10))
  P <- function(des, th = NULL, a = NULL, ...) {
    args <- c(list(des = des, th = th), a, list(...))
    do.call(.hf_P, args)
  }
  expect_identical(.hf_form(P(c("OLn", "OFzo", "OHzo", "A", "S"), a = cr(5, 4))), "euamphimus")
  expect_identical(.hf_form(P(c("OLn", "OFnoz", "OHnoz", "E", "BP"))), "mor_typique")
  expect_identical(.hf_form(P(c("OLn", "OFnoz", "OHzo", "E", "BP"))), "humimor")
  expect_identical(.hf_form(P(c("OLn", "OFzo", "OHzo", "A", "S"), th = c(2, 2, 2, 15, 40), a = nc(5, 4))), "dysmoder")
  expect_identical(.hf_form(P(c("OLn", "OFzo", "OHzo", "A", "S"), th = c(2, 2, 0.5, 15, 40), a = nc(5, 4))), "eumoder")
  expect_identical(.hf_form(P(c("OLn", "OFnoz", "E", "BP"), th = c(2, 3, 15, 40))), "hemimor")
  expect_identical(.hf_form(P(c("OLn", "OFzo", "A", "S"), th = c(2, 3, 15, 40), a = nc(4, 3))), "hemimoder")
  expect_identical(.hf_form(P(c("OLn", "OFzo", "A", "S"), th = c(2, 3, 15, 40), a = cr(4, 3))), "dysmull")
  expect_identical(.hf_form(P(c("OLn", "(OFzo)", "A", "S"), th = c(2, 1, 15, 40))), "oligomull")
  expect_identical(.hf_form(P(c("OLn", "OLv", "A", "S"), th = c(2, 3, 15, 40))), "oligomull")
  expect_identical(.hf_form(P(c("OLn", "(OLv)", "A", "S"), th = c(2, 3, 15, 40))), "mesomull")
  expect_identical(.hf_form(P(c("OLn", "A", "S"), th = c(2, 15, 40))), "mesomull")
  expect_identical(.hf_form(P(c("(OLn)", "A", "S"), th = c(1, 15, 40), a = cr(3, 2))), "eumull")
  # the eleven outcomes of the file, each a form of the aerated group
  r <- soilKey:::.rp_humus_rules()
  outs <- unique(unlist(lapply(r$humus_key, function(p) c(p$pass, p$fail))))
  outs <- setdiff(outs, c(vapply(r$humus_key, `[[`, "", "id"), NA))
  expect_length(outs, 11L)
  expect_true(all(vapply(outs, function(o) identical(r$form_by_code[[o]]$group, "aerated"), NA)))
  expect_true(all(vapply(r$humus_key, function(p) p$test %in% names(soilKey:::.rp_humus_tests), NA)))
})

test_that("the walk is reported, and the names, types and labels", {
  skip_on_cran()
  h <- .hf(.hf_P(c("OLn", "OFzo", "OHzo", "A", "S"), th = c(2, 2, 2, 15, 40), biogenic_aggregates_pct = .hf_at(5, 4, 10)))
  expect_identical(h$path, c("K1:yes", "K2:no", "K3:no", "K5:yes"))
  expect_identical(h$type, "moder")
  expect_identical(h$label, "dysmoder")
  expect_identical(h$group, "aerated")
  expect_identical(h$route, "horizons")
  expect_true(h$decided)
  expect_false(h$no_form_possible)
  h <- .hf(.hf_P(c("OLn", "OFnoz", "E", "BP"), th = c(2, 3, 15, 40)))
  expect_identical(h$name, "h\u00e9mimor")
  expect_identical(h$type, "mor")
})

test_that("zoogenic or not unrecorded leaves the form undecided; o_zoogenic decides", {
  skip_on_cran()
  # the notation OF and OH without zo or noz
  h <- .hf(.hf_P(c("OLn", "OF", "OH", "E", "BP")))
  expect_true(is.na(h$form))
  expect_setequal(h$candidates, c("humimor", "mor_typique", "dysmoder"))
  expect_true("o_zoogenic" %in% h$missing)
  expect_identical(h$route, "none")
  # o_zoogenic recorded on the layers
  h <- .hf(.hf_P(c("OLn", "OF", "OH", "E", "BP"), o_zoogenic = c(NA, "noz", "noz", NA, NA)))
  expect_identical(h$form, "mor_typique")
  # every candidate a mor: the type is decided, the form not
  h <- .hf(.hf_P(c("OLn", "OFnoz", "OH", "E", "BP")))
  expect_setequal(h$candidates, c("humimor", "mor_typique"))
  expect_identical(h$type, "mor")
  expect_identical(h$label, "mor")
  expect_false(h$decided)
})

test_that("the data route: the kind of O by the fine organic matter", {
  skip_on_cran()
  # no designation: OL, OF, OH by fine_organic_matter_pct, zo by o_zoogenic
  # (a well drained site: O, not H, Q19)
  wd <- list(drainage_class = "well drained")
  p <- .hf_P(rep(NA_character_, 5), sys = NA, th = c(2, 3, 3, 15, 40), site = wd,
             oc_pct = c(40, 38, 32, 3, 0.8), fine_organic_matter_pct = c(5, 50, 85, NA, NA),
             o_zoogenic = c(NA, "zo", "zo", NA, NA), biogenic_aggregates_pct = c(NA, NA, NA, 10, NA),
             clay_pct = c(NA, NA, NA, 18, 20), lateral_cover_pct = c(NA, 100, 100, NA, NA))
  h <- .hf(p)
  expect_identical(h$form, "dysmoder")
  # an OL of unknown kind (OLn or OLv) leaves the mulls undecided
  p <- .hf_P(rep(NA_character_, 3), sys = NA, th = c(2, 15, 40), site = wd, oc_pct = c(40, 3, 0.8),
             fine_organic_matter_pct = c(5, NA, NA), clay_pct = c(NA, 18, 20))
  h <- .hf(p)
  expect_true(is.na(h$form))
  expect_true(all(c("oligomull", "mesomull") %in% h$candidates))
  # with the drainage not recorded the frame leaves H or O open: undecided
  p <- .hf_P(rep(NA_character_, 5), sys = NA, th = c(2, 3, 3, 15, 40),
             oc_pct = c(40, 38, 32, 3, 0.8), fine_organic_matter_pct = c(5, 50, 85, NA, NA),
             o_zoogenic = c(NA, "zo", "zo", NA, NA), biogenic_aggregates_pct = c(NA, NA, NA, 10, NA),
             clay_pct = c(NA, NA, NA, 18, 20))
  expect_true(is.na(.hf_form(p)))
})

test_that("the continuity of the O horizons (Hugo, 2026-11-04)", {
  skip_on_cran()
  # lateral cover: 90 % or more continuous
  expect_identical(.hf_form(.hf_P(c("OLn", "OFzo", "A", "S"), th = c(2, 3, 15, 40),
                                  lateral_cover_pct = c(NA, 95, NA, NA), biogenic_aggregates_pct = c(NA, NA, 10, NA))), "hemimoder")
  expect_identical(.hf_form(.hf_P(c("OLn", "OFzo", "A", "S"), th = c(2, 3, 15, 40),
                                  lateral_cover_pct = c(NA, 50, NA, NA))), "oligomull")
  # an OH on under 10 % of the ground is none: a hemimoder (p. 344)
  expect_identical(.hf_form(.hf_P(c("OLn", "OFzo", "OHzo", "A", "S"), lateral_cover_pct = c(NA, NA, 5, NA, NA),
                                  biogenic_aggregates_pct = c(NA, NA, NA, 10, NA))), "hemimoder")
  # another notation, no cover: undecided between the continuous and the
  # discontinuous branches
  h <- .hf(.hf_P(c("Oi", "Oe", "A", "Bw"), sys = NA, th = c(2, 3, 15, 40), o_zoogenic = c(NA, "zo", NA, NA),
                 biogenic_aggregates_pct = c(NA, NA, 10, NA), oc_pct = c(40, 35, 3, 0.8), clay_pct = c(NA, NA, 15, 18)))
  expect_true(is.na(h$form))
  expect_true(all(c("hemimoder", "oligomull") %in% h$candidates))
  expect_true("lateral_cover_pct" %in% h$missing)
})

test_that("an O written in parentheses is that O, discontinuous; the CPCS (B) is unchanged", {
  skip_on_cran()
  d <- soilKey:::.rp_designations(c("(OLn)", "OFrzo", "(OHfzo)", "A", "S"), list(designation_system = "rp2008"))
  expect_identical(d$code, c("OLn", "OFr", "OHf", "A", "S"))
  expect_identical(d$discontinuous, c(TRUE, FALSE, TRUE, FALSE, FALSE))
  d <- soilKey:::.rp_designations(c("A1", "(B)", "C"))
  expect_false(any(d$discontinuous))
  expect_true(is.na(d$code[2L]))
})

test_that("the gates: tangel and anmoor, the codes the references read", {
  skip_on_cran()
  h <- .hf(.hf_P(c("OL", "OFzo", "OHta", "A", "R"), th = c(2, 5, 20, 5, 40)))
  expect_identical(h$form, "tangel")
  expect_identical(h$group, "no_fine_earth")
  expect_identical(h$type, "amphimus")
  h <- .hf(.hf_P(c("OL", "An", "G"), th = c(2, 20, 40)))
  expect_identical(h$form, "anmoor")
  expect_identical(h$group, "hydromorphic")
  expect_true(is.na(h$type))
  expect_identical(h$label, "anmoor")
  # the anmoor with no litter, (OL)/An
  expect_identical(.hf_form(.hf_P(c("An", "G"), th = c(20, 40))), "anmoor")
  # the same OHta as the frame of the references
  p <- make_rp2008_example("organosols_holorganiques")
  expect_identical(humus_form_rp(p)$form, "tangel")
  expect_true("OHta" %in% soilKey:::.rp_frame(p)$code)
})

test_that("the forms without fine mineral earth (pp. 348-349)", {
  skip_on_cran()
  expect_identical(.hf_form(.hf_P(c("OL", "Xh"), th = c(2, 40))), "peyromull")
  expect_identical(.hf_form(.hf_P(c("OL", "OFzo", "R"), th = c(2, 4, 40))), "lithomoder")
  expect_identical(.hf_form(.hf_P(c("OL", "OFzo", "OHzo", "D"), th = c(2, 4, 2, 40))), "lithomoder")
  expect_identical(.hf_form(.hf_P(c("OL", "OFnoz", "R"), th = c(2, 4, 40))), "lithomor")
  expect_identical(.hf_form(.hf_P(c("OL", "OFnoz", "OHnoz", "X"), th = c(2, 4, 2, 40))), "peyromor")
  expect_identical(.hf_form(.hf_P(c("OL", "OFzo", "Xh"), th = c(2, 4, 40))), "peyromoder")
  expect_identical(.hf_form(.hf_P(c("OL", "OFzo", "OHzo", "Xh"), th = c(2, 2, 2, 40),
                                  biogenic_aggregates_pct = c(NA, NA, NA, 10))), "peyromoder")
  expect_identical(.hf_form(.hf_P(c("OL", "OFzo", "OHzo", "Xh"), th = c(2, 2, 2, 40),
                                  biogenic_aggregates_pct = c(NA, NA, NA, 40), biogenic_aggregate_size = c(NA, NA, NA, "meso"))),
                   "peyro_amphimus")
  # the fine earth of the X not recorded: peyromoder or peyro-amphimus
  h <- .hf(.hf_P(c("OL", "OFzo", "OHzo", "Xh"), th = c(2, 2, 2, 40)))
  expect_setequal(h$candidates, c("peyromoder", "peyro_amphimus"))
  expect_identical(h$group, "no_fine_earth")
  # a litter alone over the rock: no form ("lithomull" is defined nowhere)
  h <- .hf(.hf_P(c("OL", "R"), th = c(2, 40)))
  expect_true(is.na(h$form))
  expect_length(h$candidates, 0L)
  expect_true(any(grepl("lithomull", h$notes)))
})

test_that("the hydromorphic forms (pp. 350-351; Hugo, 2026-11-04)", {
  skip_on_cran()
  expect_identical(.hf_form(.hf_P(c("OLn", "Ag", "G"), th = c(2, 15, 40),
                                  biogenic_aggregates_pct = c(NA, 60, NA), biogenic_aggregate_size = c(NA, "macro", NA))), "hydromull")
  expect_identical(.hf_form(.hf_P(c("OLn", "OFzo", "OHzo", "Ag", "G"), th = c(2, 2, 3, 15, 40),
                                  biogenic_aggregates_pct = c(NA, NA, NA, 10, NA))), "hydromoder")
  # a mor over an organo-mineral horizon whose lower neighbour is a G
  expect_identical(.hf_form(.hf_P(c("OLn", "OFnoz", "OHnoz", "Eh", "G"))), "hydromor")
  h <- .hf(.hf_P(c("OLn", "OFnoz", "OHnoz", "Eh", "G")))
  expect_identical(h$group, "hydromorphic")
  # no wet amphimus: the euamphimus is kept, with a note
  h <- .hf(.hf_P(c("OLn", "OFzo", "OHzo", "Ag", "G"), biogenic_aggregates_pct = c(NA, NA, NA, 60, NA),
                 biogenic_aggregate_size = c(NA, NA, NA, "macro", NA)))
  expect_identical(h$form, "euamphimus")
  expect_true(any(grepl("no hydromorphic amphimus", h$notes)))
})

test_that("the open land by the land use (p. 337; Hugo, 2026-11-04)", {
  skip_on_cran()
  crumb <- list(biogenic_aggregates_pct = c(60, NA), biogenic_aggregate_size = c("macro", NA))
  P <- function(des, site, ...) .hf_P(des, th = c(25, 40), site = site, ...)
  expect_identical(.hf_form(do.call(P, c(list(c("A", "S"), list(land_use = "cropland")), crumb))), "agri_mull")
  expect_identical(.hf_form(do.call(P, c(list(c("A", "S"), list(land_use = "permanent pasture")), crumb))), "mull_de_pelouse")
  expect_identical(.hf_form(P(c("A", "S"), list(land_use = "arable"), biogenic_aggregates_pct = c(10, NA),
                              structure_type = c("single grain", NA))), "agri_moder")
  expect_identical(.hf_form(P(c("A", "S"), list(land_use = "heath"), biogenic_aggregates_pct = c(10, NA),
                              structure_type = c("granular", NA))), "moder_de_pelouse")
  expect_identical(.hf_form(P(c("A", "S"), list(land_use = "vineyard"), biogenic_aggregates_pct = c(10, NA),
                              structure_type = c("massive", NA))), "agri_mor")
  expect_identical(.hf_form(P(c("A", "S"), list(land_use = "lawn"), biogenic_aggregates_pct = c(10, NA),
                              structure_type = c("massive", NA))), "mor_de_pelouse")
  expect_identical(.hf_form(.hf_P(c("OHzo", "A", "S"), th = c(2, 25, 40), site = list(land_use = "alpine grassland"),
                                  biogenic_aggregates_pct = c(NA, 60, NA), biogenic_aggregate_size = c(NA, "macro", NA))),
                   "amphimus_de_pelouse")
  # a ploughed L with no land use recorded: cultivated
  h <- .hf(.hf_P(c("LA", "S"), th = c(25, 40), biogenic_aggregates_pct = c(60, NA), biogenic_aggregate_size = c("macro", NA)))
  expect_identical(h$form, "agri_mull")
  expect_identical(h$name, "agri-mull")
  expect_identical(h$group, "open_land")
  # forest: the key
  expect_identical(.hf_form(.hf_P(c("OLn", "A", "S"), th = c(2, 15, 40), site = list(land_use = "deciduous forest"))), "mesomull")
})

test_that("no O horizon, H horizons at the top (Hugo, 2026-11-04)", {
  skip_on_cran()
  h <- .hf(.hf_P(c("A", "S"), th = c(25, 40)))
  expect_true(is.na(h$form))
  expect_length(h$candidates, 0L)
  expect_identical(h$missing, "humus_form")
  h <- .hf(.hf_P(c("OL", "Hs", "Hm"), th = c(2, 40, 60)))
  expect_true(is.na(h$form))
  expect_identical(h$group, "histic")
  # the recorded form still decides
  expect_identical(.hf_form(.hf_P(c("A", "S"), th = c(25, 40), site = list(humus_form = "eumull"))), "eumull")
})

test_that("recorded against keyed (Q22; Hugo, 2026-11-04)", {
  skip_on_cran()
  und <- c("OLn", "OF", "OH", "E", "BP")
  # undecided: the recorded form decides
  h <- .hf(.hf_P(und, site = list(humus_form = "Humimor")))
  expect_identical(h$form, "humimor")
  expect_identical(h$route, "recorded")
  expect_false(h$conflict)
  # a recorded type keeps the candidates of that type
  h <- .hf(.hf_P(und, site = list(humus_form = "mor")))
  expect_true(is.na(h$form))
  expect_identical(h$type, "mor")
  expect_setequal(h$candidates, c("humimor", "mor_typique"))
  expect_identical(h$label, "mor")
  # the form "mor typique" decides
  expect_identical(.hf_form(.hf_P(und, site = list(humus_form = "mor typique"))), "mor_typique")
  expect_identical(soilKey:::.rp_humus_recorded("Mor (humus brut)")$code, "mor")
  # outside the candidates: it decides, the conflict reported
  h <- .hf(.hf_P(und, site = list(humus_form = "m\u00e9somull")))
  expect_identical(h$form, "mesomull")
  expect_true(h$conflict)
  # the horizons decide: they prevail, the conflict reported
  h <- .hf(.hf_P(c("OLn", "OFnoz", "OHnoz", "E", "BP"), site = list(humus_form = "moder")))
  expect_identical(h$form, "mor_typique")
  expect_identical(h$name, "mor")
  expect_identical(h$route, "horizons")
  expect_true(h$conflict)
  # a recorded type that agrees is no conflict; the 1995 amphimull is the euamphimus
  h <- .hf(.hf_P(c("OLn", "OFnoz", "OHnoz", "E", "BP"), site = list(humus_form = "mor")))
  expect_false(h$conflict)
  expect_identical(soilKey:::.rp_humus_recorded("amphimull")$code, "euamphimus")
  expect_identical(soilKey:::.rp_humus_recorded("Agri-mull")$code, "agri_mull")
  expect_true(is.na(soilKey:::.rp_humus_recorded("tourbe")$code))
})

test_that("the qualifiers of the humus forms (pp. 338-340)", {
  skip_on_cran()
  q <- function(...) .hf(.hf_P(c("OLn", "A", "S"), th = c(2, 15, 40), ...))$qualifiers
  d <- q(ph_h2o = c(NA, 4.25, 5), bs_pct = c(NA, 82, 60), caco3_pct = c(NA, 0, 0),
         ca_cmol = c(NA, 6, 3), mg_cmol = c(NA, 1, 1))
  expect_identical(d$qualifier[d$group == "pH" & d$degree == "pass"], "acide")
  expect_identical(d$qualifier[d$group == "pH" & d$degree == "imperfect"], "tr\u00e8s acide")
  expect_identical(d$qualifier[d$group == "S/CEC" & d$degree == "pass"], "subsatur\u00e9e")
  expect_identical(d$qualifier[d$group == "S/CEC" & d$degree == "imperfect"], "m\u00e9sosatur\u00e9e")
  expect_identical(d$qualifier[d$group == "Ca/Mg"], "calcique")
  # carbonated: carbonatee, no saturee; Ca/Mg not read
  d <- q(ph_h2o = c(NA, 8, 8.2), bs_pct = c(NA, 100, 100), caco3_pct = c(NA, 12, 20), ca_cmol = c(NA, 20, 20), mg_cmol = c(NA, 1, 1))
  expect_true("carbonat\u00e9e" %in% d$qualifier)
  expect_false(any(d$qualifier %in% c("satur\u00e9e", "subsatur\u00e9e")))
  expect_false("Ca/Mg" %in% d$group)
  expect_identical(d$qualifier[d$group == "pH"], "basique")
  # sodisee; lignique and xylique by the woody debris of the O horizons
  d <- q(na_cmol = c(NA, 1, NA), cec_cmol = c(NA, 10, NA))
  expect_true("sodis\u00e9e" %in% d$qualifier)
  d <- q(woody_fragments_pct = c(50, NA, NA))
  expect_true("lignique" %in% d$qualifier)
  d <- q(woody_fragments_pct = c(85, NA, NA))
  expect_true("xylique" %in% d$qualifier)
  # Ca/Mg classes: 5 is calci-magnesique, 1/3 magnesique
  cl <- soilKey:::.rp_humus_rules()$humus_qualifiers$ca_mg$classes
  expect_identical(soilKey:::.rp_humus_classes(5, cl)$qualifier, "calci-magn\u00e9sique")
  expect_identical(soilKey:::.rp_humus_classes(0.2, cl)$qualifier, "hyper-magn\u00e9sique")
  expect_identical(soilKey:::.rp_humus_classes(1, cl)$qualifier, "magn\u00e9sique")
  # nothing recorded: only the carbonates, undecided
  d <- q()
  expect_identical(d$qualifier, "carbonat\u00e9e")
  expect_identical(d$degree, "undecided")
})

test_that("classify_rp2008() reports the humus form beside the name", {
  skip_on_cran()
  p <- make_rp2008_example("organosols_holorganiques")
  res <- classify_rp2008(p, on_missing = "silent")
  expect_s3_class(res$humus_form, "data.frame")
  expect_identical(res$humus_form$form, "tangel")
  expect_identical(res$humus_form$label, "tangel")
  expect_identical(res$trace$humus_form$form, "tangel")
  # the name and the levels do not carry it yet (the name builder of a later part)
  expect_false(grepl("tangel", res$name))
  expect_false(any(grepl("humus", res$levels$level)))
  out <- .hf_print(res)
  expect_true(any(grepl("Humus form: tangel", out, fixed = TRUE)))
  kt <- key_trace_table(res)
  expect_identical(kt$status[kt$phase == "humus_form"], "info")
  # undecided
  res <- classify_rp2008(make_rp2008_example("brunisols_eutriques"), on_missing = "silent")
  expect_false(res$humus_form$decided)
  expect_true(any(grepl("Humus form: undecided", .hf_print(res), fixed = TRUE)))
  # other systems have none
  expect_null(classify_wrb2022(p, on_missing = "silent")$humus_form)
})

test_that("the readings of the humus forms are confirmed and the code is ASCII", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "rp2008", "readings.yaml", package = "soilKey"))$readings
  hf <- Filter(function(r) grepl("^Humus forms", r$topic), rd)
  expect_length(hf, 12L)
  expect_true(all(vapply(hf, function(r) grepl("(Hugo, 2026-11-04)", r$reading, fixed = TRUE), NA)))
  src <- testthat::test_path("..", "..", "R", "diagnostics-rp-humus.R")
  skip_if_not(file.exists(src))
  txt <- readLines(src, warn = FALSE)
  expect_false(any(grepl("[^\\x01-\\x7F]", txt, perl = TRUE)))
})

test_that("the review of 0.9.353: no form possible, gates, the record, the wet test", {
  skip_on_cran()
  # a dead end of the key that is only possible leaves the form undecided
  h <- .hf(.hf_P(c("(OLn)", "A", "S"), th = c(1, 15, 40)))
  expect_true(is.na(h$form))
  expect_identical(h$candidates, "eumull")
  expect_true(h$no_form_possible)
  expect_true("biogenic_aggregates_pct" %in% h$missing)
  # an undecided gate followed by no form: undecided (no O horizon, the A
  # possibly an An)
  h <- .hf(.hf_P(c(NA, NA), th = c(20, 40), sys = NA, oc_pct = c(12, 0.5), clay_pct = c(30, 30)))
  expect_true(is.na(h$form))
  expect_true(h$no_form_possible)
  expect_true(is.na(h$group))
  # the recorded humus form does not steer the key (Q22): the horizons leave
  # dysmull or hemimoder, the record decides
  d <- c("OLn", "OFzo", "A", "S")
  h <- .hf(.hf_P(d, th = c(2, 3, 15, 40), structure_type = c(NA, NA, "crumb", NA)))
  expect_setequal(h$candidates, c("dysmull", "hemimoder"))
  h <- .hf(.hf_P(d, th = c(2, 3, 15, 40), structure_type = c(NA, NA, "crumb", NA), site = list(humus_form = "eumull")))
  expect_identical(h$form, "eumull")
  expect_identical(h$route, "recorded")
  expect_true(h$conflict)
  # the wet test of the mor reads the layer below for the mor only
  h <- .hf(.hf_P(c("OLn", "OF", "OH", "E", "G"), th = c(2, 2, 2, 10, 40)))
  expect_true("hydromor" %in% h$candidates)
  expect_true("dysmoder" %in% h$candidates)
  expect_false("hydromoder" %in% h$candidates)
  # open-land case 4 needs an OHzo: an OHnoz is a mull de pelouse
  expect_identical(.hf_form(.hf_P(c("OLn", "OFzo", "OHnoz", "A", "S"), site = list(land_use = "pasture"),
                                  biogenic_aggregates_pct = .hf_at(5, 4, 60), biogenic_aggregate_size = .hf_at(5, 4, "macro"))),
                   "mull_de_pelouse")
  # an undecided top layer that may be an H leaves no form possible
  h <- .hf(.hf_P(rep(NA_character_, 4), sys = NA, th = c(5, 30, 15, 40), oc_pct = c(40, 35, 3, 0.8),
                 fine_organic_matter_pct = c(5, 80, NA, NA), clay_pct = c(NA, NA, 18, 20)))
  expect_true(is.na(h$form))
  expect_true(h$no_form_possible)
  expect_true(is.na(h$group))
})

test_that("the review of 0.9.353: the dysmoder needs a continuous OH (Hugo, 2026-11-04)", {
  skip_on_cran()
  nc <- list(biogenic_aggregates_pct = .hf_at(5, 4, 10))
  expect_identical(do.call(function(...) .hf_form(.hf_P(c("OLn", "OFzo", "(OHzo)", "A", "S"), th = c(2, 2, 2, 15, 40), ...)), nc), "eumoder")
  expect_identical(do.call(function(...) .hf_form(.hf_P(c("OLn", "OFzo", "OHzo", "A", "S"), th = c(2, 2, 2, 15, 40),
                                                        lateral_cover_pct = c(NA, NA, 60, NA, NA), ...)), nc), "eumoder")
  # another notation, the cover not recorded: dysmoder or eumoder
  h <- .hf(.hf_P(c("Oi", "Oe", "Oa", "A", "Bw"), sys = NA, th = c(2, 2, 2, 15, 40), o_zoogenic = c(NA, "zo", "zo", NA, NA),
                 biogenic_aggregates_pct = c(NA, NA, NA, 10, NA), oc_pct = c(40, 35, 30, 3, 0.8), clay_pct = c(NA, NA, NA, 15, 18)))
  expect_setequal(h$candidates, c("dysmoder", "eumoder"))
})

test_that("the review of 0.9.353: forms without fine earth, the eumull note", {
  skip_on_cran()
  # an OH of open kind over a biomesostructured X keeps the peyromoder
  h <- .hf(.hf_P(c("OL", "OFzo", "O", "Xh"), th = c(2, 2, 2, 40),
                 biogenic_aggregates_pct = c(NA, NA, NA, 40), biogenic_aggregate_size = c(NA, NA, NA, "meso")))
  expect_true(all(c("peyromoder", "peyro_amphimus") %in% h$candidates))
  # an OL only over an R or D: no form; over an X: a peyromull
  expect_true(.hf(.hf_P(c("OL", "D"), th = c(2, 40)))$no_form_possible)
  # a continuous OLn over a clearly crumbly A: a mesomull, with the note (p. 342)
  h <- .hf(.hf_P(c("OLn", "A", "S"), th = c(2, 15, 40), biogenic_aggregates_pct = c(NA, 60, NA), biogenic_aggregate_size = c(NA, "macro", NA)))
  expect_identical(h$form, "mesomull")
  expect_true(any(grepl("early in winter", h$notes)))
})

test_that("the review of 0.9.353: the land use by whole words", {
  skip_on_cran()
  land <- function(lu) soilKey:::.rp_humus_land(list(x = soilKey:::.rp_profile(.hf_P(c("A", "S"), th = c(20, 40), site = list(land_use = lu))),
                                                      m = NA_integer_, fr = NULL))
  expect_identical(land("deforested area, now cropland"), "crop")
  expect_identical(land("forest clearing used as pasture"), "grass")
  expect_identical(land("woody heath (Calluna)"), "grass")
  expect_identical(land("aquaculture"), "other")
  expect_identical(land("silviculture"), "forest")
  expect_identical(land("hayfield"), "grass")
  expect_identical(land("parking lot"), "other")
  expect_identical(land("deciduous forest"), "forest")
})

test_that("the review of 0.9.353: the qualifiers with the carbonates undecided; the woody debris flagged", {
  skip_on_cran()
  d <- .hf(.hf_P(c("OLn", "A", "S"), th = c(2, 15, 40), ph_h2o = c(NA, 7, 7), bs_pct = c(NA, 100, 100),
                 ca_cmol = c(NA, 10, 10), mg_cmol = c(NA, 1, 1)))$qualifiers
  expect_identical(d$degree[d$qualifier == "satur\u00e9e"], "undecided")
  expect_identical(d$degree[d$group == "Ca/Mg"], "undecided")
  d <- .hf(.hf_P(c("OLn", "A", "S"), th = c(2, 15, 40), woody_fragments_pct = c(50, NA, NA)))$qualifiers
  expect_match(d$note[d$qualifier == "lignique"], "stands in")
})

test_that("an O written in parentheses now counts in the depth datum of the references", {
  skip_on_cran()
  # (OLn) is a litter OL: depths from the A below it (p. 64), as OLn
  p1 <- .hf_P(c("(OLn)", "A", "R"), th = c(4, 8, 30))
  p2 <- .hf_P(c("OLn", "A", "R"), th = c(4, 8, 30))
  expect_identical(classify_rp2008(p1, on_missing = "silent")$name, classify_rp2008(p2, on_missing = "silent")$name)
  expect_identical(soilKey:::.rp_frame(p1)$top_rp, soilKey:::.rp_frame(p2)$top_rp)
})
