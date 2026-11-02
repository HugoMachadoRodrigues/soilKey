# =============================================================================
# v0.9.337 -- the Russian soil classification (Field Guide of 2008, the field
# version of the classification of 2004), keyed to its trunks and divisions.
# The keys to trunks (2.2) and divisions (2.3) are branching trees; soilKey
# flattens them into first-match lists (Hugo, 2026-11-02), each gate the
# conjunction of the conditions of the book's path, and reads them on the
# diagnostic horizons of identify_horizons(pedon, "kidpr2008"). The types and
# subtypes follow. Experimental and opt-in: classify_all() runs it only when
# asked.
# =============================================================================

#' Classify a pedon under the Russian soil classification (Field Guide, 2008)
#'
#' Keys a \code{\link{PedonRecord}} to the trunks and divisions of the Russian
#' soil classification, in the version of its Field Guide (Polevoi
#' opredelitel' pochv Rossii, Dokuchaev Soil Science Institute, 2008).
#' \strong{Experimental}: this release keys the five trunks (key 2.2: the
#' organogenic, synlithogenic, postlithogenic and weakly developed soils, and
#' trunk E, not a soil: rock outcrops, technogenic surface formations, or a
#' deposit that is not humified over a buried soil) and the 27 divisions
#' (key 2.3). The types (profile formulas) and subtypes follow in later
#' releases.
#'
#' The keys read the diagnostic horizons of the profile: the Russian
#' notation where the profile is written in it (declared with
#' \code{site$designation_system = "kidpr"}, or recognised by an index only
#' it writes: AY, EL, BEL, BHF, BCA, PU, TUR ...), otherwise the key to
#' horizons 2.1 run on the recorded data (see \code{\link{identify_horizons}}).
#' From them the keys read whether a horizon is present, whether it starts
#' the profile (at the surface, or under a deposit of 40 cm or less),
#' whether it lies directly over another, whether the profile consists of
#' one horizon only, the depth of hard rock, a deposit at the surface, the
#' peat body of trunk A (51 cm or more, the agro-peat horizons included)
#' and the thickness of a turbated horizon; of the site, a river floodplain
#' on modern alluvium (\code{landform}, \code{landform_element},
#' \code{layer_origin}, \code{rock_origin}, \code{parent_material}, the
#' alluvial mark of the notation), volcanic ash (the same, the ash mark,
#' \code{volcanic_glass_pct}) and the contaminated horizon X
#' (\code{contamination_hazard}). With several organic and humus horizons
#' at the surface the thickest is the diagnostic one (p. 37).
#'
#' The keys are branching trees; soilKey flattens them into ordered lists
#' in which each gate is the conjunction of the conditions of the book's path
#' to the class, and repairs only the gaps the book's own types prove; the
#' other combinations the key leaves without an exit are "not provided by
#' the Field Guide of 2008": the trunk is assigned and the division left
#' unassigned, with a note. Names: an English gloss in \code{name}, the
#' Russian of the book in \code{levels$name_native}. The readings where the
#' text is silent are listed in \code{system.file("rules", "kidpr2008",
#' "readings.yaml", package = "soilKey")}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{load_rules("kidpr2008")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"kidpr2008"}, the deepest class assigned in \code{name} (the
#'   division, or the trunk when no division is assigned), the trunk in
#'   \code{rsg_or_order}, the trunk and division in \code{levels} (with
#'   \code{name_native}, the Russian name), the key trace, the notes on a
#'   profile that is not a soil or whose combination of horizons the Field
#'   Guide does not provide for in \code{warnings}, and
#'   \code{provisional_because} (classes before the assigned one that could
#'   not be decided).
#' @references Polevoi opredelitel' pochv Rossii [Field Guide of Russian
#'   Soils] (2008). Dokuchaev Soil Science Institute, Moscow.
#' @seealso \code{\link{make_kidpr2008_example}}, \code{\link{identify_horizons}},
#'   \code{\link{soilkey_systems}}.
#' @examples
#' res <- classify_kidpr2008(make_kidpr2008_example("CA"), on_missing = "silent")
#' res$name
#' res$levels
#' @export
classify_kidpr2008 <- function(pedon, rules = NULL,
                               on_missing = c("warn", "silent", "error"),
                               gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  if (inherits(pedon, "PedonRecord")) {
    # a horizon table without its depth columns is completed first
    if (!all(c("top_cm", "bottom_cm") %in% names(pedon$horizons))) {
      pedon <- pedon$clone(deep = TRUE)
      pedon$horizons <- ensure_horizon_schema(data.table::as.data.table(pedon$horizons))
    }
    pedon <- .classify_apply_gapfill(pedon, gapfill)
  }
  .classify_require_horizons(pedon, "classify_kidpr2008")
  rules <- rules %||% load_rules("kidpr2008")
  # the profile read once: every predicate of the keys reads this view
  view <- .kidpr_dv_pedon(pedon)
  prev <- list(pedon = .kidpr_dv_state$pedon, view = .kidpr_dv_state$view)
  .kidpr_dv_state$pedon <- pedon
  .kidpr_dv_state$view <- view
  on.exit({
    .kidpr_dv_state$pedon <- prev$pedon
    .kidpr_dv_state$view <- prev$view
  }, add = TRUE)
  res <- classify_hierarchical("kidpr2008", pedon, rules = rules,
                               on_missing = on_missing, gapfill = FALSE)
  .kidpr_result_notes(res, view, pedon)
}

# The notes of a result: trunk E is not a soil; a profile of trunk C that no
# division takes is a thin layer of loose rock over hard rock (C.2b) or a
# combination the Field Guide does not provide for (C.21-C.23); horizons read
# from the data.
.kidpr_result_notes <- function(res, view, pedon) {
  lv <- res$levels
  trunk <- if (nrow(lv)) lv$code[1L] else NA_character_
  notes <- character(0)
  if (identical(trunk, "E")) {
    notes <- c(notes, paste(
      "Trunk E: not a soil (key 2.2): no soil horizon within about 1 m (a rock outcrop or a technogenic",
      "surface formation, TPO), or a deposit over 40 cm that is not humified over a buried soil. The",
      "groups of technogenic surface formations (chapter 8) are not keyed."))
  }
  undecided <- any(startsWith(res$provisional_because %||% character(0), "divisions:"))
  if (nrow(lv) == 1L && !identical(trunk, "E") && !undecided) {
    rock <- tryCatch(rock_within_kidpr(view, max_cm = 30)$passed, error = function(e) NA)
    notes <- c(notes, if (identical(trunk, "C") && isTRUE(rock)) paste(
      "Hard rock within 30 cm and none of the horizons of key step C.2: a thin layer of loose rock",
      "over hard rock (C.2b), not a division of the Field Guide of 2008; the division is unassigned.")
    else paste(
      "This combination of horizons is not provided by the Field Guide of 2008 (key 2.3, steps",
      "C.21-C.23): the division is unassigned."))
  }
  rt <- view$f$route
  if (any(rt %in% "data")) {
    notes <- c(notes, paste(
      "Some layers are not written in the Russian notation: their horizons come from key 2.1 run on",
      "the recorded data (see identify_horizons(pedon, \"kidpr2008\"))."))
  }
  if (length(notes)) res$warnings <- c(res$warnings, notes)
  res
}


#' Example pedons for the divisions of the Russian soil classification
#'
#' Small, plausible profiles written for the tests and examples, one per
#' division of the Russian soil classification (Field Guide of 2008), with
#' designations in the Russian notation (declared in
#' \code{site$designation_system}) and a few properties. They are not real
#' profiles.
#'
#' @param division The code of a division: \code{"AA"}, \code{"AB"}
#'   (trunk A), \code{"BA"}, \code{"BB"}, \code{"BC"} (trunk B),
#'   \code{"DA"} (trunk D, the division soilKey codes [DA]), \code{"CA"} to
#'   \code{"CU"} (trunk C).
#' @return A \code{\link{PedonRecord}}.
#' @examples
#' classify_kidpr2008(make_kidpr2008_example("CI"), on_missing = "silent")$name
#' @export
make_kidpr2008_example <- function(division = c("AA", "AB", "BA", "BB", "BC", "DA",
                                                "CA", "CB", "CC", "CD", "CE", "CF", "CG",
                                                "CH", "CI", "CJ", "CK", "CL", "CM", "CN",
                                                "CO", "CP", "CQ", "CR", "CS", "CT", "CU")) {
  division <- match.arg(division)
  mk <- function(h, site = list()) {
    n <- max(lengths(h))
    h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
    base <- list(id = paste0("kidpr2008-", division), country = "RU", designation_system = "kidpr")
    PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
      data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  }
  switch(division,
    AA = mk(list(designation = c("TO", "TT1", "TT2"), top_cm = c(0, 40, 100), bottom_cm = c(40, 100, 180),
                 oc_pct = c(45, 48, 50), ph_h2o = 3.8, peat_moor_type = "high-moor"),
            list(landform = "raised bog", drainage_class = "very poorly drained")),
    AB = mk(list(designation = c("PT", "TT", "G"), top_cm = c(0, 25, 110), bottom_cm = c(25, 110, 140),
                 oc_pct = c(40, 46, 1.5), ph_h2o = c(5.6, 5.4, 6.2)),
            list(land_use = "arable", artificially_drained = TRUE)),
    BA = mk(list(designation = c("AU", "C~~1", "C~~2"), top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 120),
                 oc_pct = c(3.5, 0.8, 0.4), clay_pct = c(28, 18, 22), silt_pct = 50, sand_pct = c(22, 32, 28),
                 ph_h2o = 6.8, layer_origin = "alluvium"),
            list(landform = "floodplain")),
    BB = mk(list(designation = c("AO", "BH", "BAN", "C\""), top_cm = c(0, 6, 15, 60), bottom_cm = c(6, 15, 60, 90),
                 oc_pct = c(12, 6, 3, 0.8), ph_h2o = 5.4, layer_origin = "volcanic ash"),
            list(parent_material = "volcanic ash")),
    BC = mk(list(designation = c("RU1", "RU2", "D"), top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 110),
                 oc_pct = c(3, 2.2, 0.5), ph_h2o = 7.0, stratification_pattern = c("continuous", "continuous", NA))),
    DA = mk(list(designation = c("W", "R"), top_cm = c(0, 3), bottom_cm = c(3, 30),
                 oc_pct = c(1.2, NA), rock_origin = c(NA, "granite"))),
    CA = mk(list(designation = c("AY", "EL", "BEL", "BT1", "BT2", "C"), top_cm = c(0, 15, 30, 45, 80, 120),
                 bottom_cm = c(15, 30, 45, 80, 120, 150), oc_pct = c(2.5, 0.6, 0.4, 0.3, 0.2, 0.1),
                 clay_pct = c(14, 12, 18, 30, 32, 26), silt_pct = 55, sand_pct = c(31, 33, 27, 15, 13, 19),
                 ph_h2o = c(5.2, 5.0, 5.1, 5.3, 5.6, 6.0))),
    CB = mk(list(designation = c("O", "E", "BHF", "C"), top_cm = c(0, 5, 15, 40), bottom_cm = c(5, 15, 40, 100),
                 oc_pct = c(40, 0.4, 1.2, 0.1), clay_pct = c(NA, 3, 5, 3), sand_pct = c(NA, 90, 85, 92),
                 silt_pct = c(NA, 7, 10, 5), ph_h2o = c(3.9, 4.2, 4.6, 5.0))),
    CC = mk(list(designation = c("AO", "BFM", "C"), top_cm = c(0, 8, 50), bottom_cm = c(8, 50, 100),
                 oc_pct = c(10, 0.8, 0.2), ph_h2o = c(4.5, 5.0, 5.2))),
    CD = mk(list(designation = c("AY", "BM", "C"), top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 100),
                 oc_pct = c(3, 0.9, 0.3), clay_pct = c(20, 24, 20), silt_pct = 40, sand_pct = c(40, 36, 40),
                 ph_h2o = c(5.4, 5.6, 5.8))),
    CE = mk(list(designation = c("O", "CRM", "C"), top_cm = c(0, 8, 40), bottom_cm = c(8, 40, 80),
                 oc_pct = c(35, 1, 0.4), ph_h2o = c(4.6, 5.4, 5.8)),
            list(frost_regime = "permafrost")),
    CF = mk(list(designation = c("AJ", "BPL", "BCA", "Cca"), top_cm = c(0, 20, 60, 100), bottom_cm = c(20, 60, 100, 140),
                 oc_pct = c(2.2, 0.8, 0.4, 0.2), caco3_pct = c(0, 0, 12, 10), ph_h2o = c(7.2, 7.6, 8.2, 8.3)),
            list(frost_regime = "long-seasonal")),
    CG = mk(list(designation = c("O", "CR", "C\u22a5"), top_cm = c(0, 5, 40), bottom_cm = c(5, 40, 60),
                 oc_pct = c(30, 2, 0.8), ph_h2o = c(5.0, 5.8, 6.0)),
            list(frost_regime = "permafrost")),
    CH = mk(list(designation = c("O", "G", "CG"), top_cm = c(0, 8, 50), bottom_cm = c(8, 50, 100),
                 oc_pct = c(30, 1.2, 0.5), ph_h2o = c(4.8, 5.6, 6.0)),
            list(drainage_class = "poorly drained")),
    CI = mk(list(designation = c("AU1", "AU2", "BCA", "Cca"), top_cm = c(0, 25, 50, 100), bottom_cm = c(25, 50, 100, 150),
                 oc_pct = c(4.5, 3, 1, 0.4), caco3_pct = c(0, 0, 10, 12), clay_pct = 35, silt_pct = 45, sand_pct = 20,
                 ph_h2o = c(6.8, 7.0, 8.0, 8.2))),
    CJ = mk(list(designation = c("AJ", "BM", "BCA", "Cca"), top_cm = c(0, 10, 30, 60), bottom_cm = c(10, 30, 60, 100),
                 oc_pct = c(1, 0.6, 0.3, 0.2), caco3_pct = c(0, 1, 15, 12), ph_h2o = c(7.8, 8.0, 8.4, 8.5))),
    CK = mk(list(designation = c("AU", "SEL", "ASN", "BCAs,cs", "Cca,s"), top_cm = c(0, 12, 20, 45, 80),
                 bottom_cm = c(12, 20, 45, 80, 120), oc_pct = c(3.5, 1.5, 1.8, 0.6, 0.3),
                 ph_h2o = c(7.0, 7.2, 8.6, 8.8, 8.6), clay_pct = c(25, 22, 38, 34, 30), silt_pct = 40,
                 sand_pct = c(35, 38, 22, 26, 30))),
    CL = mk(list(designation = c("S", "Gs", "CGs"), top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 80),
                 oc_pct = c(1.2, 0.5, 0.3), ec_dS_m = c(40, 18, 12), ph_h2o = c(8.4, 8.2, 8.0)),
            list(drainage_class = "poorly drained", surface_condition = "salt crust")),
    CM = mk(list(designation = c("AU", "Q", "CQ"), top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 100),
                 oc_pct = c(4, 1, 0.4), ph_h2o = c(6.6, 7.0, 7.4))),
    CN = mk(list(designation = c("AY", "C"), top_cm = c(0, 20), bottom_cm = c(20, 80),
                 oc_pct = c(2.8, 0.3), ph_h2o = c(5.8, 6.2))),
    CO = mk(list(designation = c("AY", "EL", "D"), top_cm = c(0, 10, 35), bottom_cm = c(10, 35, 80),
                 oc_pct = c(2.5, 0.4, 0.2), clay_pct = c(8, 5, 30), sand_pct = c(75, 82, 30), silt_pct = c(17, 13, 40),
                 ph_h2o = c(5.0, 5.2, 6.0))),
    CP = mk(list(designation = c("AU", "C", "R"), top_cm = c(0, 15, 25), bottom_cm = c(15, 25, 40),
                 oc_pct = c(5, 1, NA), ph_h2o = c(6.5, 6.8, NA), rock_origin = c(NA, NA, "granite"))),
    CQ = mk(list(designation = c("BM", "C"), top_cm = c(0, 30), bottom_cm = c(30, 80),
                 oc_pct = c(0.6, 0.2), ph_h2o = c(6.0, 6.4)),
            list(anthropic_truncation = TRUE)),
    CR = mk(list(designation = c("PB", "BCA", "Cca"), top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 100),
                 oc_pct = c(0.8, 0.4, 0.2), caco3_pct = c(2, 12, 10), ph_h2o = c(7.8, 8.2, 8.3)),
            list(land_use = "arable")),
    CS = mk(list(designation = c("P", "BT", "C"), top_cm = c(0, 25, 80), bottom_cm = c(25, 80, 120),
                 oc_pct = c(1.6, 0.4, 0.2), clay_pct = c(14, 30, 24), silt_pct = 50, sand_pct = c(36, 20, 26),
                 ph_h2o = c(5.8, 5.6, 6.0)),
            list(land_use = "arable")),
    CT = mk(list(designation = c("P", "TUR", "C"), top_cm = c(0, 25, 75), bottom_cm = c(25, 75, 120),
                 oc_pct = c(1.8, 0.8, 0.2), ph_h2o = c(6.6, 7.2, 7.8)),
            list(land_use = "arable")),
    CU = mk(list(designation = c("AY+X", "EL", "BT", "C"), top_cm = c(0, 15, 30, 90), bottom_cm = c(15, 30, 90, 120),
                 oc_pct = c(2.2, 0.5, 0.3, 0.2), ph_h2o = c(5.6, 5.4, 5.6, 6.0),
                 contamination_hazard = c("extremely hazardous", NA, NA, NA),
                 pollutants = c("Cu, Ni, Co", NA, NA, NA)),
            list(contaminated = TRUE))
  )
}
