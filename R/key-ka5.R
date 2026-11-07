# =============================================================================
# v0.9.362 -- the German soil systematics of the Bodenkundliche
# Kartieranleitung, 5th edition (Ad-hoc-AG Boden, Hannover 2005, KA5), keyed
# to the type by Tab. 51 (pp. 283-288): 4 divisions (Abteilungen), 21 classes
# (Klassen), 56 types (Typen). Experimental and opt-in: classify_all() runs it
# only when asked. The subtypes, overlays and varieties follow in later
# releases.
# =============================================================================

#' Classify a pedon under the German KA5 soil systematics (2005)
#'
#' Keys a \code{\link{PedonRecord}} to the divisions (Abteilungen), classes
#' (Klassen) and types (Typen) of the German soil systematics of the
#' Bodenkundliche Kartieranleitung, 5th edition (Ad-hoc-AG Boden 2005, KA5),
#' by its key Tab. 51. \strong{Experimental}: this release keys the 4
#' divisions, 21 classes and 56 types; the subtypes of the type texts, the
#' overlays ("X ueber Y") and the varieties are not keyed yet.
#'
#' The key reads the horizons of the profile
#' (\code{identify_horizons(pedon, "ka5")}): the KA5 notation where the
#' profile is written in it (declared with \code{site$designation_system =
#' "ka5"}, or recognised by a sign only it writes: the degree sign, a
#' lower-case prefix before a master such as lC, fAh, aM, tmGo, Sw, Sd, Axh,
#' Bfv ...), otherwise soilKey's order of tests on the recorded data. The last
#' symbol of a designation carries the horizon (Bv-Go is a Go, Go-Bv a Bv).
#' Depths are read from the GOF, the top of the first horizon that is not L,
#' O, Of, Oh or Ovh (p. 79). The order and gates are Tab. 51's (the Moore, the
#' subhydric soils, the semiterrestrial soils, the terrestrial soils), with
#' the numbers of the type texts where the compressed key differs and two
#' rules of the figures Abb. 22 and 23 (Hugo's decisions Q5 and Q6). A gate
#' the data leave open makes the result provisional
#' (\code{provisional_because}); a type that holds only on the designation, a
#' measurement missing (the Fahlerde without its clay, C13), or by the key's
#' order with the record missing (the Rigosol without the kind of deep
#' cultivation, Q23), is listed there too.
#'
#' Names as printed (German) in \code{name} and \code{levels$name}, with
#' soilKey's English gloss in \code{levels$name_en}. The horizon panel (one
#' row per layer: depths from the GOF, the horizon read, the route notation,
#' data or both, the candidates, the evidence) is in
#' \code{trace$horizon_panel}, the full frame in \code{trace$horizons}, the
#' consistency flags of the figures in \code{trace$figures}. The
#' readings where the text is silent are listed in \code{system.file("rules",
#' "ka5", "readings.yaml", package = "soilKey")}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{load_rules("ka5")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"ka5"}: the deepest class assigned in \code{name}, the division in
#'   \code{rsg_or_order}, the division, class and type in \code{levels}, the
#'   key trace, and \code{provisional_because}.
#' @references Ad-hoc-AG Boden (2005). Bodenkundliche Kartieranleitung. 5.
#'   Auflage. Bundesanstalt fuer Geowissenschaften und Rohstoffe,
#'   Hannover.
#' @seealso \code{\link{make_ka5_example}}, \code{\link{identify_horizons}},
#'   \code{\link{soilkey_systems}}.
#' @examples
#' res <- classify_ka5(make_ka5_example("BB"), on_missing = "silent")
#' res$name
#' res$levels
#' @export
classify_ka5 <- function(pedon, rules = NULL, on_missing = c("warn", "silent", "error"),
                         gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  if (!inherits(pedon, "PedonRecord"))
    rlang::abort("classify_ka5(): 'pedon' must be a PedonRecord (see PedonRecord$new()).")
  if (!is.null(pedon$horizons) && !all(c("top_cm", "bottom_cm") %in% names(pedon$horizons))) {
    pedon <- pedon$clone(deep = TRUE)
    pedon$horizons <- ensure_horizon_schema(data.table::as.data.table(pedon$horizons))
  }
  pedon <- .classify_apply_gapfill(pedon, gapfill)
  .classify_require_horizons(pedon, "classify_ka5")
  rules <- rules %||% load_rules("ka5")
  view <- .ka5_make_view(pedon)
  prev <- list(pedon = .ka5_state$pedon, view = .ka5_state$view)
  .ka5_state$pedon <- pedon
  .ka5_state$view <- view
  on.exit({
    .ka5_state$pedon <- prev$pedon
    .ka5_state$view <- prev$view
  }, add = TRUE)
  res <- classify_hierarchical("ka5", pedon, rules = rules, on_missing = on_missing, gapfill = FALSE)
  # a semiterrestrial division that keys no class falls through to the
  # terrestrial soils (D2, Hugo 2026-11-06)
  lv <- res$levels
  through <- NULL
  if (is.data.frame(lv) && nrow(lv) == 1L && identical(lv$code[1L], "[semiterrestrisch]")) {
    cl <- res$trace$classes
    if (length(cl) && all(vapply(cl, function(e) isFALSE(e$passed), NA))) {
      r2 <- rules
      r2$divisions <- Filter(function(d) !identical(d$code, "[semiterrestrisch]"), rules$divisions)
      through <- res$trace
      res <- classify_hierarchical("ka5", pedon, rules = r2, on_missing = on_missing, gapfill = FALSE)
      res$warnings <- c(res$warnings, paste("The semiterrestrial gate passed but no semiterrestrial class did:",
                                            "the profile is keyed as a terrestrial soil (D2)."))
    }
  }
  res <- .ka5_result_notes(res, view)
  if (!is.null(through)) res$trace$semiterrestrial_attempt <- through
  res
}

# Search a test result tree for the predicates that passed only provisionally.
.ka5_provisional_hits <- function(ev) {
  out <- character(0)
  walk <- function(e) {
    if (!is.list(e)) return(invisible())
    if (isTRUE(e$provisional) && isTRUE(e$passed)) out <<- c(out, as.character(e$missing %||% character(0)), "*")
    if (!is.null(e$evidence) && is.list(e$evidence)) for (z in e$evidence) walk(z)
    invisible()
  }
  for (z in ev) walk(z)
  out
}

# The notes of a KA5 result: the horizon panel and frame, the provisional
# passes of the assigned classes, the routes of the horizons, an empty key.
.ka5_result_notes <- function(res, view) {
  fr <- view$frame
  tr <- res$trace
  notes <- character(0)
  prov <- character(0)
  lv <- res$levels
  if (is.data.frame(lv) && nrow(lv)) {
    for (k in seq_len(nrow(lv))) {
      e <- tr[[lv$level[k]]][[lv$code[k]]]
      h <- .ka5_provisional_hits(e$evidence)
      if (length(h)) {
        prov <- c(prov, sprintf("%s:%s", lv$level[k], lv$code[k]))
        notes <- c(notes, sprintf("%s (%s) holds only provisionally: %s", lv$name[k], lv$code[k],
                                  paste(unique(unlist(lapply(e$evidence, function(z) z$notes))), collapse = "; ")))
      }
    }
  }
  # a class with no type: the type level provisional (D16)
  if (is.data.frame(lv) && nrow(lv) == 2L && identical(lv$level[2L], "classes")) {
    prov <- c(prov, sprintf("types:%s", lv$code[2L]))
    notes <- c(notes, sprintf("No type of the class %s (%s) passed: the type level is provisional.", lv$name[2L], lv$code[2L]))
  }
  # a stand-in or a lower-grade reading decided a horizon the gates read (D13)
  si <- which(!(fr$buried > 0L) & (vapply(fr$stand_ins, length, 0L) > 0L | fr$lower_grade %in% TRUE) & !is.na(fr$code))
  if (length(si)) {
    prov <- c(prov, sprintf("horizons:%s", fr$code[si]))
    notes <- c(notes, sprintf("Horizon %s (%s) rests on %s: a lower grade of evidence (D13).", fr$code[si], fr$designation[si] %||% "",
                              vapply(si, function(i) if (length(fr$stand_ins[[i]])) paste0("soilKey's WRB ", paste0(fr$stand_ins[[i]], "()", collapse = ", "))
                                     else "soilKey's colour cuts of the F kinds", "")))
  }
  if (length(prov)) res$provisional_because <- unique(c(res$provisional_because, prov))
  # no class: Tab. 51 row 14, only when every class failed (D16)
  cls <- tr$classes
  if (is.data.frame(lv) && nrow(lv) == 1L && identical(lv$code[1L], "[terrestrisch]") && length(cls) &&
      all(vapply(cls, function(e) isFALSE(e$passed), NA)))
    notes <- c(notes, "No class of the terrestrial soils passed: no horizon differentiation recognisable by the key (Tab. 51, p. 288): help needed, or a substrate.")
  rt <- fr$route
  if (any(rt %in% "data"))
    notes <- c(notes, paste("Some layers are not written in the KA5 notation: their horizons come from soilKey's order",
                            "of tests on the recorded data (see identify_horizons(pedon, \"ka5\"))."))
  und <- which(is.na(fr$code))
  if (length(und))
    notes <- c(notes, sprintf("Layer(s) %s left undecided by the data (candidates: %s).", paste(und, collapse = ", "),
                              paste(vapply(und, function(i) paste(fr$candidates[[i]], collapse = "/"), ""), collapse = "; ")))
  fl <- .ka5_figure_flags(view, lv)
  if (nrow(fl)) notes <- c(notes, sprintf("Consistency with the figures: %s", fl$note))
  if (length(notes)) res$warnings <- c(res$warnings, notes)
  res$trace$figures <- fl
  res$trace$horizons <- fr
  res$trace$horizon_panel <- .ka5_horizon_panel(fr)
  res
}


# The figures Abb. 21-23 (pp. 192-194) as consistency flags (Hugo's decision
# Q5: Tab. 51 decides; where the figures would read the profile otherwise,
# the trace says so): Abb. 21 asks the subhydric soils before the Moore; Abb.
# 22 asks the anthropogenic horizons first, reads an S at 4 dm (<= 4 dm) as a
# Stauwasserboden, and sends a fully developed Ah of 4 dm or more to the
# Schwarzerden.
.ka5_figure_flags <- function(view, lv) {
  fr <- view$frame
  code <- function(level) { k <- which(lv$level == level); if (length(k)) lv$code[k[1L]] else NA_character_ }
  div <- code("divisions"); cls <- code("classes")
  out <- data.frame(figure = character(0), note = character(0), stringsAsFactors = FALSE)
  add <- function(fig, note) out[nrow(out) + 1L, ] <<- list(fig, note)
  first <- .ka5_rows_from_gof(view)[1L]
  if (identical(div, "[moore]") && !is.na(first) && isTRUE(.ka5_rows(view, list(masters = "F"))[first]))
    add("Abb. 21", "an F horizon at the GOF: Abb. 21 asks the subhydric soils before the Moore")
  if (identical(div, "[terrestrisch]")) {
    mer <- .ka5_within(view, list(masters = c("M", "E", "R")), reaching_cm = 40)
    if (isTRUE(mer$passed) && !identical(cls, "Y"))
      add("Abb. 22", "an M, E or R horizon reaching 4 dm: Abb. 22 asks the anthropogenic soils first")
    s40 <- which(.ka5_rows(view, list(masters = "S")) %in% TRUE & fr$top_gof == 40)
    if (length(s40) && !identical(cls, "S"))
      add("Abb. 22", "an S horizon beginning at 4 dm: Abb. 22 reads an S at 4 dm or less as a Stauwasserboden (Tab. 51 and the type texts: under 4 dm, Q6)")
    a <- .ka5_top_run(view, list(pattern = "^A[^ela]"))
    if (a$thickness >= 40 && !identical(cls, "T"))
      add("Abb. 22", sprintf("a topsoil of %s cm: Abb. 22 sends a fully developed Ah of 4 dm or more to the Schwarzerden (Q17)", format(a$thickness)))
  }
  out
}


#' Example pedons for the types of the German KA5 soil systematics
#'
#' Small, plausible profiles written for the tests and examples, one per type
#' of Tab. 51 (56), with designations in the KA5 notation (declared in
#' \code{site$designation_system}) and the properties soilKey's order of tests
#' reads on the data. They are not real profiles. \code{near_miss = TRUE}
#' gives a twin that misses the type's gate (a depth, a share, a record moved
#' across it); \code{route = "data"} drops the designations and the declared
#' notation, so that the horizons are read on the data.
#'
#' @param type The code of a type: \code{"KV"}, \code{"KM"}, \code{"KH"},
#'   \code{"HH"}, \code{"HN"} (Moore); \code{"JP"}, \code{"JG"}, \code{"JS"},
#'   \code{"JD"}, \code{"IA"}, \code{"IW"} (subhydric); \code{"UEA"} (the
#'   Strand, the key's Ue-A), \code{"MR"}, \code{"MC"}, \code{"MO"}, \code{"MK"},
#'   \code{"MD"}, \code{"MH"}, \code{"MN"}, \code{"GH"}, \code{"GM"},
#'   \code{"GN"}, \code{"GG"}, \code{"AO"}, \code{"AT"}, \code{"AQ"},
#'   \code{"AZ"}, \code{"AB"} (semiterrestrial); \code{"FF"}, \code{"FS"},
#'   \code{"OO"}, \code{"OL"}, \code{"TC"}, \code{"TT"}, \code{"RN"},
#'   \code{"RQ"}, \code{"RR"}, \code{"RZ"}, \code{"XX"}, \code{"YK"},
#'   \code{"YE"}, \code{"YY"}, \code{"YU"}, \code{"YO"}, \code{"SG"},
#'   \code{"SH"}, \code{"SS"}, \code{"PP"}, \code{"LF"}, \code{"LL"},
#'   \code{"BB"}, \code{"CR"}, \code{"CF"}, \code{"DD"}, \code{"VV"},
#'   \code{"VW"} (terrestrial).
#' @param near_miss Return the twin that misses the type's gate.
#' @param route \code{"notation"} (default) or \code{"data"}.
#' @return A \code{\link{PedonRecord}}.
#' @examples
#' classify_ka5(make_ka5_example("LL"), on_missing = "silent")$name
#' classify_ka5(make_ka5_example("LL", route = "data"), on_missing = "silent")$name
#' @export
make_ka5_example <- function(type = c("KV", "KM", "KH", "HH", "HN", "JP", "JG", "JS", "JD", "IA", "IW",
                                      "UEA", "MR", "MC", "MO", "MK", "MD", "MH", "MN", "GH", "GM", "GN", "GG",
                                      "AO", "AT", "AQ", "AZ", "AB", "FF", "FS", "OO", "OL", "TC", "TT", "RN",
                                      "RQ", "RR", "RZ", "XX", "YK", "YE", "YY", "YU", "YO", "SG", "SH", "SS",
                                      "PP", "LF", "LL", "BB", "CR", "CF", "DD", "VV", "VW"),
                             near_miss = FALSE, route = c("notation", "data")) {
  if (identical(type, "\u00dcA")) type <- "UEA"
  type <- match.arg(type)
  route <- match.arg(route)
  nm <- isTRUE(near_miss)
  gw <- list(groundwater_kind = "observed", groundwater_depth_cm = 30)
  pw <- list(groundwater_kind = "perched")
  mk <- function(des, top, bot, ..., site = list()) {
    n <- length(des)
    h <- c(list(designation = des, top_cm = top, bottom_cm = bot), list(...))
    h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
    base <- list(id = paste0("ka5-", type, if (nm) "-near-miss"), country = "DE", designation_system = "ka5")
    site <- utils::modifyList(base, site)
    if (route == "data") {
      h$designation <- rep(NA_character_, n)
      site$designation_system <- NULL
    }
    PedonRecord$new(site = site, horizons = ensure_horizon_schema(
      data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  }
  col <- function(hue, value, chroma) list(munsell_hue_moist = hue, munsell_value_moist = value, munsell_chroma_moist = chroma)
  # the profiles: designations, depths and data
  marsh <- function(des, top, bot, oc, caco3, rust, red, rh, origin = "tidal-marine", ec = 0.3, clay = 30, ...,
                    site = list()) {
    do.call(mk, c(list(des, top, bot, oc_pct = oc, clay_pct = clay, silt_pct = 50, sand_pct = 20, caco3_pct = caco3,
                       ec_dS_m = ec, redoximorphic_features_pct = rust, reduction_colours_pct = red, rh_value = rh,
                       bleached_pct = 0, layer_origin = origin), col(c("10YR", rep("2.5Y", length(des) - 2L), "5GY"),
                                                                     c(3, rep(5, length(des) - 2L), 5), c(2, rep(3, length(des) - 2L), 1)),
                  list(...), list(site = utils::modifyList(gw, site))))
  }
  gley <- function(des, top, bot, oc, rust, red, rh, ...) {
    do.call(mk, c(list(des, top, bot, oc_pct = oc, clay_pct = 20, silt_pct = 40, sand_pct = 40, caco3_pct = 0,
                       redoximorphic_features_pct = rust, reduction_colours_pct = red, rh_value = rh, bleached_pct = 0),
                  col(c("10YR", rep("2.5Y", length(des) - 2L), "5GY"), c(3, rep(5, length(des) - 2L), 5),
                      c(2, rep(3, length(des) - 2L), 1)), list(...), list(site = gw)))
  }
  auen <- function(des, top, bot, oc, caco3, ...) {
    do.call(mk, c(list(des, top, bot, oc_pct = oc, clay_pct = 15, silt_pct = 45, sand_pct = 40, caco3_pct = caco3,
                       redoximorphic_features_pct = c(0, 0, 10), reduction_colours_pct = c(0, 0, 5), bleached_pct = 0),
                  col(c("10YR", "10YR", "2.5Y"), c(4, 5, 5), c(3, 3, 3)), list(...),
                  list(site = utils::modifyList(gw, list(landform = "floodplain", groundwater_depth_cm = 90)))))
  }
  moor <- function(des, top, bot, peat, ...) {
    mk(des, top, bot, oc_pct = 45, peat_moor_type = peat, ..., site = list(artificially_drained = TRUE))
  }
  sub <- function(des, top, bot, ..., site) mk(des, top, bot, ..., site = site)
  terr <- function(des, top, bot, oc, clay, silt = 30, hue = "10YR", value = 4, chroma = 3, ..., site = list()) {
    do.call(mk, c(list(des, top, bot, oc_pct = oc, clay_pct = clay, silt_pct = silt, sand_pct = pmax(0, 100 - clay - silt)),
                  col(hue, value, chroma), list(...), list(site = site)))
  }
  switch(type,
    KV = if (!nm) moor(c("nHv", "nHt", "nHw", "nHr"), c(0, 20, 35, 80), c(20, 35, 80, 150), "low-moor",
                       structure_type = c("crumb", "prismatic", "massive", "massive"), redoximorphic_features_pct = c(0, 0, 10, 0),
                       water_saturation_days = c(100, 150, 200, 365))
         else moor(c("nHv", "nHt", "nHw", "nHr"), c(0, 8, 35, 80), c(8, 35, 80, 150), "low-moor",
                   structure_type = c("crumb", "prismatic", "massive", "massive"), redoximorphic_features_pct = c(0, 0, 10, 0),
                   water_saturation_days = c(100, 150, 200, 365)),
    KM = moor(c("nHm", "nHa", "nHt", "nHw"), if (nm) c(0, 8, 30, 45) else c(0, 15, 30, 45), c(if (nm) 8 else 15, 30, 45, 100),
              "low-moor", von_post_index = c(8, 6, 5, 4), munsell_hue_moist = "N", munsell_value_moist = c(2, 3, 3, 3),
              munsell_chroma_moist = 0, water_repellence = c("repellent", NA, NA, NA),
              structure_type = c("single grain", "subangular blocky", "prismatic", "massive"), redoximorphic_features_pct = c(0, 0, 0, 10)),
    KH = moor(c("hHv", "hHw", "hHr"), c(0, if (nm) 6 else 15, 60), c(if (nm) 6 else 15, 60, 150), "high-moor",
              structure_type = c("crumb", "massive", "massive"), redoximorphic_features_pct = c(0, 5, 0),
              water_saturation_days = c(100, 200, 365), ph_cacl2 = 3.5),
    HH = moor(c("hHw", "hHr", "IInHr"), c(0, 40, 150), c(40, 150, 200), c("high-moor", "high-moor", "low-moor"),
              redoximorphic_features_pct = c(5, 0, 0), water_saturation_days = c(200, 365, 365), ph_cacl2 = if (nm) 4.2 else 3.4),
    HN = if (!nm) mk(c("nHw", "nHr", "IIfF"), c(0, 50, 120), c(50, 120, 140), oc_pct = c(45, 48, 4), clay_pct = c(NA, NA, 25),
                     peat_moor_type = c("low-moor", "low-moor", NA), redoximorphic_features_pct = c(5, 0, 0),
                     water_saturation_days = c(200, 365, 365))
         else mk(c("nH", "IIGr"), c(0, 25), c(25, 100), oc_pct = c(45, 0.3), clay_pct = c(NA, 20), peat_moor_type = c("low-moor", NA),
                 reduction_colours_pct = c(0, 99), redoximorphic_features_pct = 0, rh_value = c(NA, 12), site = gw),
    JP = sub(if (nm) "Fo" else "Fi", 0, 30, oc_pct = if (nm) 3 else 0.2, redoximorphic_features_pct = if (nm) 10 else 0,
             rh_value = if (nm) 15 else NA, munsell_hue_moist = "2.5Y", munsell_value_moist = 6, munsell_chroma_moist = 2,
             site = list(subaqueous = TRUE)),
    JG = sub("Fo", 0, 40, oc_pct = 3, redoximorphic_features_pct = 10, rh_value = if (nm) 11 else 15, munsell_hue_moist = "5Y",
             munsell_value_moist = 4, munsell_chroma_moist = 2, site = list(subaqueous = TRUE)),
    JS = sub("Fr", 0, 40, oc_pct = 5, rh_value = if (nm) 14 else 10, munsell_hue_moist = "N", munsell_value_moist = 2,
             munsell_chroma_moist = 0, site = list(subaqueous = TRUE)),
    JD = sub("Fh", 0, 40, oc_pct = 8, rh_value = if (nm) 20 else 16, munsell_hue_moist = "7.5YR", munsell_value_moist = 3,
             munsell_chroma_moist = 2, site = list(subaqueous = TRUE)),
    IA = sub(c(if (nm) "tmzFo" else "tmzFw", "tmzFr"), c(0, 30), c(30, 80), oc_pct = c(0.4, 2), ec_dS_m = 20,
             layer_origin = "tidal-marine", redoximorphic_features_pct = c(if (nm) 10 else 0, 0),
             munsell_hue_moist = c("2.5Y", "N"), munsell_value_moist = c(6, 3), munsell_chroma_moist = c(2, 0),
             site = list(tidal_zone = "intertidal")),
    IW = sub(c(if (nm) "tmzeFw" else "tmzeFo", "tmzeFr"), c(0, 15), c(15, 80), oc_pct = c(1, 2), ec_dS_m = 20, caco3_pct = 5,
             layer_origin = "tidal-marine", redoximorphic_features_pct = c(if (nm) 0 else 10, 0),
             munsell_hue_moist = c("2.5Y", "N"), munsell_value_moist = c(6, 3), munsell_chroma_moist = c(2, 0),
             site = list(tidal_zone = "intertidal")),
    UEA = mk(c("tmzAi", "tmzlC", "tmzeGo"), c(0, 5, if (nm) 10 else 40), c(5, if (nm) 10 else 40, 100),
             oc_pct = c(0.2, 0.05, 0.1), clay_pct = 2, silt_pct = 3, sand_pct = 95, ec_dS_m = 2, caco3_pct = c(0, 0, 5),
             rock_diggable = c(NA, TRUE, NA), redoximorphic_features_pct = c(0, 0, 10), reduction_colours_pct = c(0, 0, 5),
             bleached_pct = 0, layer_origin = "tidal-marine", munsell_hue_moist = "2.5Y", munsell_value_moist = c(6, 6, 5),
             munsell_chroma_moist = c(2, 2, 3),
             site = utils::modifyList(gw, list(tidal_zone = "supratidal", groundwater_depth_cm = 40))),
    MR = marsh(c("tmzeGo-Ah", "tmzeGo", "tmzeGr"), c(0, 10, 30), c(10, 30, 100), oc = c(3, 0.8, 0.5), caco3 = 5,
               rust = c(2, 20, 1), red = c(0, 5, 99), rh = c(NA, NA, 12), ec = 5, site = list(embanked = nm)),
    MC = if (!nm) marsh(c("tmeAh", "tmeGo", "tmzeGr"), c(0, 20, 50), c(20, 50, 120), oc = c(3, 0.6, 0.4), caco3 = 8,
                        rust = c(0, 20, 1), red = c(0, 5, 99), rh = c(NA, NA, 12), ec = c(0.3, 0.3, 2))
         else marsh(c("tmAh", "tmGo", "tmzGr"), c(0, 20, 50), c(20, 50, 120), oc = c(3, 0.6, 0.4), caco3 = 0,
                    rust = c(0, 20, 1), red = c(0, 5, 99), rh = c(NA, NA, 12), ec = c(0.3, 0.3, 2)),
    MO = marsh(if (nm) c("tmAh", "tmGo", "tmGr") else c("tmoAh", "tmoGo", "tmoGr"), c(0, 20, 50), c(20, 50, 120),
               oc = c(6, 1, 0.5), caco3 = 0, rust = c(0, 20, 1), red = c(0, 5, 99), rh = c(NA, NA, 12), clay = if (nm) 40 else 50),
    MK = marsh(c("tbAh", "tbSw", "tbSq", "tbGr"), c(0, 20, 35, if (nm) 50 else 60), c(20, 35, if (nm) 50 else 60, 120),
               oc = c(3, 0.6, 0.4, 0.3), caco3 = 0, rust = c(0, 30, 20, 1), red = c(0, 5, 5, 99), rh = c(NA, NA, NA, 12),
               origin = "tidal-brackish", knick_degree = c(NA, NA, "strong", NA)),
    MD = marsh(c("tpAh", "tpSw", paste0("IIftpAh", "\u00b0", "Sd"), paste0("IIftpGo", "\u00b0", "Sd"), "IItpGo", "IItpGr"),
               c(0, 20, if (nm) 45 else 35, if (nm) 60 else 50, 70, 90), c(20, if (nm) 45 else 35, if (nm) 60 else 50, 70, 90, 150),
               oc = c(3, 0.6, 2, 0.8, 0.4, 0.3), caco3 = 0, rust = c(0, 30, 20, 20, 20, 1), red = c(0, 5, 5, 5, 5, 99),
               rh = c(NA, NA, NA, NA, NA, 12), origin = "tidal-fluvial"),
    MH = marsh(c("tmAh", if (nm) "tmGo" else "tmSg-Go", "tmzeGr"), c(0, 20, 60), c(20, 60, 120), oc = c(3, 0.6, 0.4),
               caco3 = c(0, 0, 5), rust = c(0, 20, 1), red = c(0, 5, 99), rh = c(NA, NA, 12), ec = c(0.3, 0.3, 2)),
    MN = marsh(c("tmAh", if (nm) "tmeGo" else "tmGo", "tmzeGr"), c(0, 20, 60), c(20, 60, 120), oc = c(3, 0.6, 0.4),
               caco3 = c(0, if (nm) 5 else 0, 5), rust = c(0, 20, 1), red = c(0, 5, 99), rh = c(NA, NA, 12), ec = c(0.3, 0.3, 2)),
    GH = mk(c("nH", "IIGr"), c(0, if (nm) 8 else 20), c(if (nm) 8 else 20, 100), oc_pct = c(45, 0.3), clay_pct = c(NA, 20),
            silt_pct = c(NA, 40), sand_pct = c(NA, 40), peat_moor_type = c("low-moor", NA), redoximorphic_features_pct = c(0, 1),
            reduction_colours_pct = c(0, 99), rh_value = c(NA, 12), munsell_hue_moist = c("10YR", "5GY"),
            munsell_value_moist = c(2, 5), munsell_chroma_moist = c(1, 1), site = gw),
    GM = gley(c("Go-Aa", "Gr"), c(0, if (nm) 8 else 20), c(if (nm) 8 else 20, 100), oc = c(12, 0.3), rust = c(2, 1),
              red = c(0, 99), rh = c(NA, 12)),
    GN = if (!nm) gley(c("Go-Ah", "Gr"), c(0, 25), c(25, 100), oc = c(3, 0.3), rust = c(2, 1), red = c(0, 99), rh = c(NA, 12))
         else gley(c("Go-Ah", "Go", "Gr"), c(0, 25, 50), c(25, 50, 100), oc = c(3, 0.5, 0.3), rust = c(2, 20, 1),
                   red = c(0, 5, 99), rh = c(NA, NA, 12)),
    GG = if (!nm) gley(c("Ah", "Go", "Gr"), c(0, 25, 60), c(25, 60, 120), oc = c(3, 0.5, 0.3), rust = c(0, 20, 1),
                       red = c(0, 5, 99), rh = c(NA, NA, 12))
         else gley(c("Ah", "Bv", "Go", "Gr"), c(0, 25, 45, 80), c(25, 45, 80, 120), oc = c(3, 0.3, 0.3, 0.2),
                   rust = c(0, 0, 20, 1), red = c(0, 0, 5, 99), rh = c(NA, NA, NA, 12)),
    AO = auen(c(if (nm) "aAh" else "aAi", "alC", "aG"), c(0, if (nm) 20 else 3, 90), c(if (nm) 20 else 3, 90, 150),
              oc = c(if (nm) 3 else 0.1, 0.1, 0.1), caco3 = 0, rock_diggable = c(NA, TRUE, NA)),
    AT = auen(c("aAxh", "aM", "aG"), c(0, if (nm) 35 else 50, 90), c(if (nm) 35 else 50, 90, 150), oc = c(2.5, 0.8, 0.3), caco3 = 0,
              bs_pct = c(90, 85, 80), worm_holes_pct = c(5, 1, 0), structure_type = c("granular", "subangular blocky", "massive"),
              layer_origin = c(NA, "fluvic", NA), stratification_pattern = c(NA, "continuous", NA),
              munsell_hue_moist = c("10YR", "10YR", "2.5Y"), munsell_value_moist = c(2, 4, 5), munsell_chroma_moist = c(1, 3, 3)),
    AQ = auen(c("aAh", if (nm) "aelC" else "ailC", "aG"), c(0, 20, 90), c(20, 90, 150), oc = c(3, 0.2, 0.2),
              caco3 = if (nm) 3 else c(0.3, 0.5, 0.5), rock_diggable = c(NA, TRUE, NA)),
    AZ = auen(if (nm) c("aAh", "ailC", "aG") else c("aeAh", "aelC", "aG"), c(0, 20, 90), c(20, 90, 150), oc = c(3, 0.2, 0.2),
              caco3 = if (nm) 0.5 else 8, rock_diggable = c(NA, TRUE, NA)),
    AB = auen(c("aAh", if (nm) "aBv" else "aM", "aG"), c(0, 20, 90), c(20, 90, 150), oc = c(3, 0.8, 0.3), caco3 = 0,
              layer_origin = c(NA, if (nm) NA else "fluvic", NA), stratification_pattern = c(NA, if (nm) NA else "continuous", NA)),
    FF = if (!nm) mk(c("O", "mC"), c(0, 8), c(8, 30), oc_pct = c(40, NA), fine_organic_matter_pct = c(60, NA), rock_diggable = c(NA, FALSE))
         else mk(c("O", "xC"), c(0, 5), c(5, 40), oc_pct = c(40, NA), fine_organic_matter_pct = c(60, NA), coarse_fragments_pct = c(NA, 98)),
    FS = if (!nm) mk(c("O", "xC+O", "mC"), c(0, 5, 40), c(5, 40, 60), oc_pct = c(40, NA, NA), fine_organic_matter_pct = c(60, NA, NA),
                     coarse_fragments_pct = c(NA, 98, NA), rock_diggable = c(NA, NA, FALSE))
         else mk(c("O", "mC"), c(0, 5), c(5, 30), oc_pct = c(40, NA), fine_organic_matter_pct = c(60, NA), rock_diggable = c(NA, FALSE)),
    OO = mk(c("Ai", if (nm) "lC" else "mC"), c(0, 1), c(1, 30), oc_pct = c(0.2, NA), clay_pct = c(5, NA), silt_pct = c(10, NA),
            rock_diggable = c(NA, nm)),
    OL = mk(c(if (nm) "Ah" else "Ai", if (nm) "ilC" else "lC"), c(0, if (nm) 20 else 2), c(if (nm) 20 else 2, 100),
            oc_pct = c(if (nm) 3 else 0.2, 0.05), clay_pct = 5, silt_pct = 10, sand_pct = 85, caco3_pct = 0, rock_diggable = c(NA, TRUE)),
    TC = terr(c(if (nm) "Axh" else "Acxh", if (nm) "Axh+elC" else "Acxh+elCc", if (nm) "elC" else "elCc"), c(0, 55, 75), c(55, 75, 150),
              oc = c(2.5, 1.2, 0.3), clay = 22, silt = 65, hue = "10YR", value = c(2, 3, 5), chroma = c(1, 2, 3),
              bs_pct = c(95, 98, 100), worm_holes_pct = c(10, 5, 0), structure_type = c("granular", "granular", "massive"),
              secondary_carbonates_pct = if (nm) c(0, 0, 0) else c(2, 4, 5), caco3_pct = c(3, 10, 15), rock_diggable = c(NA, NA, TRUE)),
    TT = terr(c("Axh", if (nm) "Axh+elCc" else "Axh+lCc", if (nm) "elCc" else "lCc"), c(0, if (nm) 35 else 60, if (nm) 55 else 80),
              c(if (nm) 35 else 60, if (nm) 55 else 80, 150), oc = c(2.5, 1.2, 0.3), clay = 22, silt = 65, hue = "10YR",
              value = c(2, 3, 5), chroma = c(1, 2, 3), bs_pct = c(95, 98, 100), worm_holes_pct = c(10, 5, 0),
              structure_type = c("granular", "granular", "massive"), secondary_carbonates_pct = c(0, 0, 5),
              caco3_pct = c(0, 2, 15), rock_diggable = c(NA, NA, TRUE)),
    RN = terr(c("Ah", "imC"), c(0, if (nm) 35 else 20), c(if (nm) 35 else 20, 60), oc = c(4, NA), clay = c(15, NA), silt = c(30, NA),
              caco3_pct = c(0, 0), rock_diggable = c(NA, FALSE)),
    RQ = terr(c("Ah", if (nm) "elC" else "ilC"), c(0, 20), c(20, 120), oc = c(3, 0.2), clay = c(15, 12),
              caco3_pct = c(0, if (nm) 15 else 0), rock_diggable = c(NA, TRUE)),
    RR = terr(c("Ah", if (nm) "emC" else "cmC"), c(0, 20), c(20, 60), oc = c(5, NA), clay = c(25, NA), silt = c(40, NA),
              carbonate_whole_soil_pct = c(NA, if (nm) 40 else 95), rock_diggable = c(NA, FALSE)),
    RZ = terr(c("Ah", if (nm) "ilC" else "elC"), c(0, 25), c(25, 150), oc = c(3, 0.2), clay = c(15, 12),
              caco3_pct = c(1, if (nm) 0 else 15), rock_diggable = c(NA, TRUE)),
    XX = terr(c("Ah", "Yo", "Yr"), c(0, 15, 30), c(15, 30, 100), oc = c(3, 1, 0.5), clay = 15, soil_gas_ch4_co2_pct = c(NA, 20, 40),
              redoximorphic_features_pct = c(0, 10, 0), reduction_colours_pct = c(0, 5, 90),
              site = list(reductgas_source = if (nm) "natural_wetness" else "refuse_sludge_harbour_mud")),
    YK = terr(c("Ah", "M", "IIfAh", "IIBv"), c(0, 30, if (nm) 35 else 70, if (nm) 50 else 90), c(30, if (nm) 35 else 70, if (nm) 50 else 90, 120),
              oc = c(2, 1.2, 1.5, 0.3), clay = 15, silt = 40, layer_origin = c(NA, "colluvic", NA, NA),
              stratification_pattern = c(NA, "continuous", NA, NA), caco3_pct = 0),
    YE = terr(c("Ah", "E", "IIfAh", "IIBhs"), c(0, 30, if (nm) 35 else 70, if (nm) 50 else 80), c(30, if (nm) 35 else 70, if (nm) 50 else 80, 100),
              oc = c(2, 1.5, 1.2, 0.8), clay = 5, silt = 10, p2o5_al_mg_kg = c(300, 250, 60, 20),
              c_pyro_pct = c(NA, NA, NA, 0.8), fe_pyro_pct = c(NA, NA, NA, 0.2),
              site = list(deep_cultivation_kind = "plaggen", mechanical_mixing_depth_cm = 30)),
    YY = terr(c("R-Ap", "R", "C"), c(0, 30, 70), c(30, 70, 120), oc = c(2, 1, 0.2), clay = 15, caco3_pct = 0,
              rock_diggable = c(NA, NA, TRUE),
              site = list(deep_cultivation_kind = if (nm) "single_deep_ploughing" else "repeated_trenching",
                          mechanical_mixing_depth_cm = 70, land_use = "vineyard")),
    YU = terr(c("R-Ap", "R+Bhs", "C"), c(0, 30, 60), c(30, 60, 100), oc = c(2, 1, 0.2), clay = 5, silt = 10, caco3_pct = 0,
              rock_diggable = c(NA, NA, TRUE),
              site = list(deep_cultivation_kind = if (nm) "repeated_trenching" else "single_deep_ploughing",
                          mechanical_mixing_depth_cm = 60, land_use = "arable")),
    YO = terr(c("Ap", "Ex", "C"), c(0, 30, if (nm) 35 else 60), c(30, if (nm) 35 else 60, 100), oc = c(4, 2, 0.2), clay = 15, silt = 30,
              p2o5_al_mg_kg = c(400, 300, 30), worm_holes_pct = c(5, 5, 0), rock_diggable = c(NA, NA, TRUE), caco3_pct = 0,
              site = list(deep_cultivation_kind = "garden", mechanical_mixing_depth_cm = 30, land_use = "arable")),
    SG = terr(c("Sw-Ah", "Srw", if (nm) "IISd" else "IISrd"), c(0, 15, 40), c(15, 40, 100), oc = c(4, 0.5, 0.3), clay = c(15, 15, 30),
              redoximorphic_features_pct = c(5, 30, 30), bleached_pct = c(0, 55, 30), reduction_colours_pct = c(0, 5, if (nm) 5 else 20),
              water_saturation_days = c(150, 250, if (nm) 250 else 330), redox_segregations_location = c(NA, "voids", NA),
              air_capacity_pct = c(NA, 6, 2), ksat_mm_h = c(NA, 10, 1), site = pw),
    SH = if (!nm) terr(c("Ah", "Sg", "IIC"), c(0, 20, 90), c(20, 90, 120), oc = c(3, 0.3, 0.1), clay = 12, silt = 60,
                       sand_ffs_pct = 10, redoximorphic_features_pct = c(0, 40, 0), bleached_pct = c(0, 45, 0),
                       air_capacity_pct = c(NA, 2, NA), rock_diggable = c(NA, NA, TRUE), caco3_pct = 0, site = pw)
         else terr(c("Ah", "Bv", "Sg", "IIC"), c(0, 25, 45, 90), c(25, 45, 90, 120), oc = c(3, 0.3, 0.3, 0.1), clay = 12, silt = 60,
                   sand_ffs_pct = 10, redoximorphic_features_pct = c(0, 0, 40, 0), bleached_pct = c(0, 0, 45, 0),
                   air_capacity_pct = c(NA, NA, 2, NA), rock_diggable = c(NA, NA, NA, TRUE), caco3_pct = 0, site = pw),
    SS = if (!nm) terr(c("Ah", "Sw", "IISd"), c(0, 20, 40), c(20, 40, 100), oc = c(3, 0.5, 0.3), clay = c(15, 15, 30),
                       redoximorphic_features_pct = c(0, 30, 40), bleached_pct = c(0, 55, 30), ksat_mm_h = c(NA, 20, 1),
                       air_capacity_pct = c(NA, 8, 2), rh_value = c(NA, 17, NA), site = pw)
         else terr(c("Ah", "Bv", "Sw", "IISd"), c(0, 25, 45, 60), c(25, 45, 60, 100), oc = c(3, 0.3, 0.3, 0.2),
                   clay = c(15, 15, 15, 30), redoximorphic_features_pct = c(0, 0, 30, 40), bleached_pct = c(0, 0, 55, 30),
                   ksat_mm_h = c(NA, NA, 20, 1), air_capacity_pct = c(NA, NA, 8, 2), rh_value = c(NA, NA, 17, NA), site = pw),
    PP = if (!nm) terr(c("Ahe", "Ae", "Bsh", "Bhs", "C"), c(0, 10, 25, 30, 50), c(10, 25, 30, 50, 100), oc = c(3, 0.3, 2, 1, 0.1),
                       clay = 3, silt = 5, hue = c("10YR", "10YR", "5YR", "7.5YR", "10YR"), value = c(4, 6, 3, 4, 6),
                       chroma = c(1, 1, 3, 6, 4), c_pyro_pct = c(NA, NA, 1.5, 0.9, NA), fe_pyro_pct = c(NA, NA, 0.3, 0.2, NA),
                       rock_diggable = c(NA, NA, NA, NA, TRUE), caco3_pct = 0)
         else terr(c("Ahe", "Ae", "Bhs", "Bv", "C"), c(0, 5, 8, 12, 50), c(5, 8, 12, 50, 100), oc = c(3, 0.3, 1, 0.3, 0.1),
                   clay = 3, silt = 5, hue = c("10YR", "10YR", "7.5YR", "7.5YR", "10YR"), value = c(4, 6, 4, 4, 6),
                   chroma = c(1, 1, 6, 6, 4), c_pyro_pct = c(NA, NA, 0.9, NA, NA), fe_pyro_pct = c(NA, NA, 0.2, NA, NA),
                   rock_diggable = c(NA, NA, NA, NA, TRUE), caco3_pct = 0),
    LF = terr(c("Ah", "Ael", "Ael+Bt", "Bt", "C"), c(0, 20, 45, 60, 100), c(20, 45, 60, 100, 130), oc = c(2, 0.3, 0.3, 0.2, 0.1),
              clay = c(12, if (nm) 18 else 12, 24, 26, 20), silt = 60, hue = c("10YR", "10YR", "10YR", "7.5YR", "10YR"),
              value = c(3, 7, 5, 4, 5), chroma = c(2, 2, 4, 4, 4), clay_films_amount = c(NA, NA, "common", "many", NA),
              caco3_pct = c(0, 0, 0, 0, 10), rock_diggable = c(NA, NA, NA, NA, TRUE)),
    LL = if (!nm) terr(c("Ah", "Al", "Bt", "C"), c(0, 25, 45, 90), c(25, 45, 90, 130), oc = c(2, 0.4, 0.3, 0.1), clay = c(15, 15, 24, 18),
                       silt = 60, value = c(3, 5, 4, 5), chroma = c(2, 4, 4, 4), clay_films_amount = c(NA, NA, "common", NA),
                       caco3_pct = c(0, 0, 0, 10), rock_diggable = c(NA, NA, NA, TRUE))
         else terr(c("Ah", "Bv-Al", "Btv", "C"), c(0, 20, 35, 90), c(20, 35, 90, 130), oc = c(2, 0.4, 0.3, 0.1), clay = c(15, 15, 18, 16),
                   silt = 60, hue = c("10YR", "10YR", "7.5YR", "10YR"), value = c(3, 5, 4, 5), chroma = c(2, 4, 6, 4),
                   clay_films_amount = c(NA, NA, "few", NA), caco3_pct = c(0, 0, 0, 10), rock_diggable = c(NA, NA, NA, TRUE)),
    BB = if (!nm) terr(c("Ah", "Bv", "C"), c(0, 15, 50), c(15, 50, 100), oc = c(2.5, 0.3, 0.1), clay = c(12, 15, 10),
                       hue = c("10YR", "7.5YR", "10YR"), value = c(3, 4, 5), chroma = c(2, 6, 4), caco3_pct = 0,
                       rock_diggable = c(NA, NA, TRUE))
         else terr(c("Ah", "Bv", "Tv", "cC"), c(0, 8, 12, 45), c(8, 12, 45, 60), oc = c(5, 0.6, 0.5, NA), clay = c(40, 45, 70, NA),
                   hue = c("10YR", "7.5YR", "7.5YR", NA), value = c(3, 4, 4, NA), chroma = c(2, 6, 6, NA),
                   structure_type = c("granular", "subangular blocky", "subangular blocky", NA), clay_films_amount = c(NA, "none", "none", NA),
                   caco3_pct = c(0, 0, 0, NA), carbonate_whole_soil_pct = c(NA, NA, NA, 95), rock_diggable = c(NA, NA, NA, FALSE)),
    CR = terr(c("Ah", if (nm) "Tv" else "Tu", "cC"), c(0, 10, 40), c(10, 40, 60), oc = c(5, 0.5, NA), clay = c(40, 70, NA),
              hue = c("10YR", if (nm) "7.5YR" else "2.5YR", NA), value = c(3, 4, NA), chroma = c(2, 6, NA),
              structure_type = c("granular", "subangular blocky", NA), clay_films_amount = c(NA, "none", NA), caco3_pct = c(0, 0, NA),
              carbonate_whole_soil_pct = c(NA, NA, 95), rock_diggable = c(NA, NA, FALSE)),
    CF = terr(c("Ah", "Tv", "cC"), c(0, if (nm) 35 else 10, if (nm) 60 else 45), c(if (nm) 35 else 10, if (nm) 60 else 45, 80),
              oc = c(5, 0.5, NA), clay = c(40, 70, NA), hue = c("10YR", "7.5YR", NA), value = c(3, 4, NA), chroma = c(2, 6, NA),
              structure_type = c("granular", "subangular blocky", NA), clay_films_amount = c(NA, "none", NA), caco3_pct = c(0, 0, NA),
              carbonate_whole_soil_pct = c(NA, NA, 95), rock_diggable = c(NA, NA, FALSE)),
    DD = terr(c("Ah", "P", "eC"), c(0, if (nm) 35 else 15, 60), c(if (nm) 35 else 15, 60, 100), oc = c(3, 0.4, 0.2), clay = c(40, 55, 50),
              silt = 30, slickensides = c(NA, "common", NA), structure_type = c("granular", "prismatic", "massive"),
              clay_films_amount = c(NA, "none", NA), parent_rock_ka5 = c(NA, "Tonstein", "Tonstein"), caco3_pct = c(0, 0, 8),
              rock_diggable = c(NA, NA, TRUE)),
    VV = if (!nm) terr(c("Ah", "IIrBj", "IICj"), c(0, 20, 80), c(20, 80, 120), oc = c(3, 0.3, 0.1), clay = c(20, 40, 30))
         else terr(c("Ah", "Bv", "IIrBj", "IICj"), c(0, 10, 40, 80), c(10, 40, 80, 120), oc = c(3, 0.3, 0.3, 0.1), clay = c(20, 22, 40, 30)),
    VW = terr(c("Ah", if (nm) "IIrBj" else "IIrBu", "IICj"), c(0, 20, 90), c(20, 90, 120), oc = c(3, 0.3, 0.1), clay = c(20, 45, 30))
  )
}

# The types whose example keys to them on the data as well (the others need
# the notation: fossil and relict horizons, interfingering and transitions
# the data key does not build).
.KA5_DATA_ROUTE_TYPES <- c("KV", "KM", "KH", "HH", "HN", "JP", "JG", "JS", "JD", "IA", "IW", "UEA", "MR", "MC",
                           "MO", "MN", "GH", "GM", "GN", "GG", "AO", "AT", "AQ", "AZ", "AB", "FF", "FS", "OO",
                           "OL", "TC", "TT", "RN", "RQ", "RR", "RZ", "XX", "YK", "YE", "YY", "YU", "YO", "SG",
                           "SH", "SS", "PP", "LF", "LL", "BB", "CR", "CF", "DD")
