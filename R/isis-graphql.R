# =============================================================================
# soilKey -- live ISIS queries (ISRIC World Soil Reference Collection).
#
# ISIS is the database of ISRIC's World Soil Reference Collection: about a
# thousand profiles from some 80 countries, described and sampled to the FAO
# Guidelines, each with the WRB and Soil Taxonomy names ISRIC gives it. It is
# served by the same GraphQL server as WoSIS, and it is read the same way
# (R/wosis-graphql.R): each call goes from the caller's machine to ISRIC on an
# explicit request, nothing is written to disk, and every pedon carries the
# source, the date it was read, the licence and the citation.
#
# LICENCE. ISRIC lists ISIS among the point data sets it compiled between 1980
# and 2015 and released for educational and non-commercial use with the source
# acknowledged; commercial use needs ISRIC's written permission. The terms
# travel with every pedon (site$licence) and isis_citation() gives the
# acknowledgement. Check ISRIC's Data Hub for the current terms.
#
# WHAT THE TRANSLATION READS (Hugo, 2026-10-29):
#   * Laboratory samples in rows of their own (no layer name, no field
#     description) go to the described horizon that holds their mid-depth, as
#     a thickness-weighted mean filling only what the horizon lacks; a
#     described horizon left with no laboratory value takes the samples that
#     overlap it. ISIS keeps the description and the samples apart in more
#     than half its profiles, so without this colour and analysis never meet.
#   * Clay films are the FAO (1977) cutans of kind clay, clay and sesquioxides
#     or clay and humus: patchy, broken and continuous are few, common and many;
#     thin, moderately thick and thick are faint, distinct and prominent; clay
#     cutans recorded with no abundance are few, and so are cutans of no kind in
#     a horizon whose designation has t. Cutans of kind slickensides are the
#     slickensides, common when patchy, broken or of no recorded abundance.
#   * Cracks: the profile's cracking (large or small) or, without it, gilgai
#     give cracks of 0.5 cm or more in the horizons starting above 100 cm.
#     v0.9.315: else its surface cracks (cracksSurface): medium cracks are 1 cm;
#     "no cracks observed" is no cracks only in a profile described dry at the
#     surface (dryfrom 0), and unknown otherwise.
#   * Coarse fragments: cfvo when measured, else the middle of the FAO class of
#     rocksArtefactAbundVol (V 1, F 3.5, C 10, M 27.5, A 60, D 90 %).
#   * Inclusions: calcareous ones are secondary carbonates; hard Fe/Mn
#     concretions and nodules over 2 mm are nodules; in a v horizon or an iron
#     pan, mottles and soft Fe/Mn concretions are plinthite (middle of the FAO
#     classes). Nothing is inferred where none is recorded.
#   * EC 1:2.5 only where it decides the salic horizon: 15 dS/m or more is a
#     lower bound of the paste EC; below that, x 12.5 is an upper bound
#     (ec_max_dS_m), read only by the salic test.
#   * Water-dispersible clay is a clay content.
#   * absent_unless_recorded = TRUE (default): in a horizon with a field
#     description, mottles, slickensides, pressure faces, plinthite, a duripan
#     and artefacts that the description does not mention are absent (v0.9.313:
#     slickensides and pressure faces only where the one cutans field of the
#     layer is free; where it records cutans of another kind they stay
#     unknown). Rows
#     with laboratory data only stay unknown, and so does anything a
#     designation or a pan suggests (v for plinthite, qm for a duripan, u for
#     artefacts), and the mottles of a g, l or r horizon with no mottle field.
#   * Base saturation (100 x exchangeable bases / CEC at pH 7), the ECEC and
#     the aluminium saturation are calculated when ISIS has the parts; silt is
#     100 - sand - clay when only those two are given.
#   * The groundwater of the site (groundwaterKind; depthGroundwater, or
#     groundwaterTop where the depth is 0) becomes groundwater_kind and
#     groundwater_depth_cm, which tell gleyic from stagnic properties in WRB
#     2022 (v0.9.312).
#   * v0.9.314: the deposition of the parent material: fluvial (F, which ISIS
#     describes as fluvio-glacial sediments), marine (M) and lacustrine (L)
#     sediments, the origins of WRB 2022 fluvic material (Ch 3.3.6, criterion
#     1), are the rock_origin of the mineral layers. Other deposits are not
#     read, and nothing is set when a second deposit of another kind is
#     recorded, since ISIS does not say which layers it holds.
#   * v0.9.317: cutans of kind pressure faces (R) are the pressure faces of the
#     WRB 2022 nitic horizon (patchy, broken, continuous: few, common, many;
#     absent unless recorded, as slickensides); the second structure ISIS
#     records (structureType2) is the second-level structure, read only by the
#     nitic horizon. A particle-size record of zeros only is missing. A row
#     designated R inside the depth range of another described horizon is
#     left out, with a note in site$depth_note.
# =============================================================================

.ISIS_ENDPOINT <- "https://graphql.isric.org/isis/graphql"

#' POST one GraphQL query to the ISIS endpoint (see .wosis_post).
#' @noRd
.isis_post <- function(query, variables = NULL, timeout = 60) {
  .wosis_post(query, variables = variables, timeout = timeout,
              endpoint = .ISIS_ENDPOINT)
}

#' Citation for data read from ISIS
#'
#' Returns the acknowledgement for profiles of the ISRIC World Soil Reference
#' Collection read through the ISIS GraphQL service, with the date they were
#' read. ISRIC asks users to cite the data it supplies in their products,
#' web services, papers and reports.
#'
#' @param accessed Date the data were read (default today).
#' @return A character string.
#' @examples
#' isis_citation(as.Date("2026-10-29"))
#' @export
isis_citation <- function(accessed = Sys.Date()) {
  paste0("ISRIC - World Soil Information. ISIS, the ISRIC Soil Information ",
         "System: database of the World Soil Reference Collection. Wageningen, ",
         "the Netherlands. Read through ", .ISIS_ENDPOINT, " on ",
         format(as.Date(accessed), "%Y-%m-%d"), ".")
}

# ---- What is asked for ------------------------------------------------------

.ISIS_LAB <- c("clay", "silt", "sand", "cfvo", "phaq", "phkc", "phca", "orgc",
               "nitkjd", "cecph7", "ecec", "caExcph7", "mgExcph7", "kExcph7",
               "naExcph7", "exbas", "alExch", "esp", "tceq", "gypsumContent",
               "feExtdith", "feExtacox", "alExtox", "sietox", "bdfiod", "bdfiot",
               "wv1500", "elcosp", "elco25", "clayWaterdispTotal", "phnf")
.ISIS_MORPH <- c("colourMoistHue", "colourMoistValue", "colourMoistChroma",
                 "colourDryHue", "colourDryValue", "colourDryChroma",
                 "structureGrade1", "structureType", "structureSize",
                 "structureType2",
                 "consistenceSoilmassMoist", "consistenceSoilmassDry",
                 "stickinessClass", "consistencyWetPlasticity",
                 "cutansAbundance", "cutansKind", "cutansThickness",
                 "mottlesAbundance", "mottlesAbundance2", "mottlesColour",
                 "mottlesColour2", "mottlesContrast", "carbonateReaction",
                 "rocksArtefactAbundVol", "pansKind", "cementCompactDegree",
                 "mottlesSizeclass", "horizBoundaryDistinctness",
                 "horizBoundaryTopography", "inclusionsComposition",
                 "inclusionsComposition2", "inclusionsType", "inclusionsSize",
                 "concretionsMineralHardness", "mineralConcAbundance",
                 "mineralConcAbundance2")

.isis_profile_fields <- function() {
  paste(
    "profileCode countryCode profileId geog { latitude longitude }",
    "moistureRegimeSoil temperatureRegimeSoil drainageClass siteElevation",
    "depthGroundwater groundwaterTop groundwaterKind",
    "slopeGradientNumeric lithologyParmat lithologyParmatDeposition",
    "lithologyParmatDeposition2 cracking cracksSurface dryfrom microreliefKind",
    "wrbClassifications(first: 20) { publicationYear referenceSoilGroupCode referenceSoilGroup qualifier }",
    "usdaClassifications(first: 20) { publicationYear orderName suborder greatGroup subgroup moistureRegime temperatureRegime }",
    "layers(first: 100, orderBy: [UPPER_DEPTH_ASC]) { layerNumber upperDepth lowerDepth layerName",
    paste(c(.ISIS_LAB, .ISIS_MORPH), collapse = " "), "}")
}

# ---- Reading ISIS values ----------------------------------------------------

# A measured value comes as a list of strings (replicates): their mean.
.isis_num <- function(x) {
  if (is.null(x) || !length(x)) return(NA_real_)
  v <- suppressWarnings(as.numeric(unlist(x)))
  v <- v[is.finite(v)]
  if (length(v)) mean(v) else NA_real_
}
# A coded value comes as {item, description}: its code.
.isis_code <- function(x) {
  if (is.null(x) || !is.list(x) || is.null(x$item)) return(NA_character_)
  as.character(x$item)[1]
}
.isis_desc <- function(x) {
  if (is.null(x) || !is.list(x) || is.null(x$description)) return(NA_character_)
  as.character(x$description)[1]
}
.isis_lookup <- function(table, code) {
  if (is.na(code) || !code %in% names(table)) NA_character_ else unname(table[[code]])
}

# FAO (2006) structure, consistence and field classes in soilKey's words. A
# class that spans two grades or sizes is read by the lower one.
.ISIS_GRADE <- c("0" = "structureless", WC = "structureless", MC = "structureless",
                 SC = "structureless", VW = "weak", WE = "weak", WM = "weak",
                 MO = "moderate", MS = "moderate", ST = "strong", VS = "strong")
.ISIS_STRUCTURE <- c(GR = "granular", CR = "granular", SB = "subangular blocky",
                     SA = "subangular blocky", AB = "angular blocky",
                     AS = "angular blocky", AP = "angular blocky",
                     AW = "wedge-shaped angular blocky",
                     SN = "nutty subangular blocky", PR = "prismatic",
                     PS = "prismatic", CO = "columnar", PL = "platy",
                     SG = "single grain", MA = "massive", PM = "massive",
                     RS = "rock structure", SS = "stratified",
                     WE = "wedge-shaped angular blocky", BL = "blocky",
                     WC = "granular")                                # WC: worm casts
.ISIS_SIZE <- c(VF = "very fine", FF = "very fine", FI = "fine", FM = "fine",
                ME = "medium", MC = "medium", CO = "coarse", CV = "coarse",
                VC = "very coarse")
.ISIS_MOIST <- c(LO = "loose", VFR = "very friable", FR = "friable", FI = "firm",
                 VFI = "very firm", EFI = "extremely firm")
.ISIS_DRY <- c(LO = "loose", SO = "soft", SHA = "slightly hard", HA = "hard",
               VHA = "very hard", EHA = "extremely hard")
.ISIS_STICKY <- c(NST = "non-sticky", SST = "slightly sticky",
                  ST = "moderately sticky", VST = "very sticky")
.ISIS_PLASTIC <- c(NPL = "non-plastic", SPL = "slightly plastic",
                   PL = "moderately plastic", VPL = "very plastic")
.ISIS_CUTAN_AMOUNT <- c("0" = "none", P = "few", B = "common", C = "many")
.ISIS_CUTAN_STRENGTH <- c(f = "faint", m = "distinct", t = "prominent")
.ISIS_CUTAN_CLAY <- c("C", "F", "U")          # clay; clay and sesquioxides; clay and humus
.ISIS_CUTAN_SLICKENSIDES <- "P"
.ISIS_CUTAN_PRESSURE <- "R"                    # pressure faces (stress cutans)
.ISIS_CONTRAST <- c(F = "faint", D = "distinct", P = "prominent")
.ISIS_EFFERVESCENCE <- c(N = "none", SL = "slight", MO = "moderate",
                         ST = "strong", EX = "very strong")
.ISIS_CEMENT <- c("0" = "none", W = "weakly", M = "moderately", S = "strongly",
                  I = "indurated")
.ISIS_BOUNDARY <- c(V = "very abrupt", A = "abrupt", C = "clear", G = "gradual",
                    D = "diffuse")
.ISIS_TOPOGRAPHY <- c(S = "smooth", W = "wavy", I = "irregular", B = "broken")
# Slickensides (cutans of kind P): any recorded, patchy included, are enough
# for the vertic horizon (Hugo, 2026-10-29); the clay-film reading stays.
.ISIS_SLICKENSIDES <- c("0" = "absent", P = "common", B = "common", C = "many")
# pansKind in the Field Handbook words of pan_type: L is indurated plinthite.
.ISIS_PAN <- c(L = "ferricrete", I = "ironpan", F = "fragipan", K = "calcrete",
               D = "duripan", Y = "other pan", S = "other pan", P = "other pan")
# Middle of the FAO (2006) abundance classes, % by volume
.ISIS_ROCK_PCT <- c(N = 0, V = 1, F = 3.5, C = 10, M = 27.5, A = 60, D = 90)
.ISIS_MOTTLE_PCT <- c(N = 0, V = 1, F = 3.5, C = 10, M = 27.5, A = 70)
# the dilution from a 1:2.5 extract to the saturated paste, for a saturation
# percentage of 20 % or more: 250 / 20
.ISIS_EC_DILUTION <- 12.5

# A row that only carries a bulk density or water retention sample.
.isis_is_sample_row <- function(L) {
  is.null(L$layerName) && !any(vapply(c(.ISIS_MORPH, setdiff(.ISIS_LAB, c("bdfiod", "bdfiot", "wv1500"))),
                                      function(f) !is.null(L[[f]]), NA))
}
# A row with a field description.
.isis_described <- function(L) any(vapply(.ISIS_MORPH, function(f) !is.null(L[[f]]), NA))

# Laboratory samples in rows of their own (no layer name, no field
# description) go to the described horizon that holds their mid-depth: the
# thickness-weighted mean of the samples, filling only what the horizon lacks.
# A described horizon left with no laboratory value takes the samples that
# overlap it, weighted by the overlap. Samples outside every described horizon
# stay as horizons of their own. (Hugo, 2026-10-29: the bulk density rule,
# extended to every laboratory value.)
.isis_merge_samples <- function(layers) {
  desc <- vapply(layers, function(L) !is.null(L$layerName) || .isis_described(L), NA)
  has_lab <- function(L) any(vapply(.ISIS_LAB, function(f) !is.null(L[[f]]), NA))
  lab <- vapply(layers, has_lab, NA)
  keep <- desc | lab
  D <- which(desc); S <- which(!desc & lab)
  if (!length(D) || !length(S)) return(list(layers = layers[keep], merged = 0L))
  tp <- vapply(layers, function(L) .isis_num(L$upperDepth), 0)
  bt <- vapply(layers, function(L) .isis_num(L$lowerDepth), 0)
  wmean <- function(rows, w, f) {
    v <- vapply(rows, function(r) .isis_num(layers[[r]][[f]]), 0)
    ok <- is.finite(v) & is.finite(w) & w > 0
    if (any(ok)) list(as.character(sum(v[ok] * w[ok]) / sum(w[ok]))) else NULL
  }
  host <- vapply(S, function(r) {
    mid <- (tp[r] + bt[r]) / 2
    i <- D[tp[D] <= mid & bt[D] > mid]
    if (length(i)) i[1] else NA_integer_
  }, 0L)
  for (i in unique(host[!is.na(host)])) {
    rows <- S[which(host == i)]
    for (f in .ISIS_LAB) if (is.null(layers[[i]][[f]])) {
      v <- wmean(rows, bt[rows] - tp[rows], f)
      if (!is.null(v)) layers[[i]][[f]] <- v
    }
  }
  for (i in D) if (!has_lab(layers[[i]])) {
    ov <- pmax(0, pmin(bt[S], bt[i]) - pmax(tp[S], tp[i]))
    if (!any(ov > 0)) next
    for (f in .ISIS_LAB) {
      v <- wmean(S[ov > 0], ov[ov > 0], f)
      if (!is.null(v)) layers[[i]][[f]] <- v
    }
  }
  drop <- S[!is.na(host)]
  list(layers = layers[setdiff(which(keep), drop)], merged = length(drop))
}

# v0.9.317: two guards on the rows of a profile, before the samples are merged.
#   * A particle-size record whose values are all 0 (clay, silt and sand, as
#     many as are recorded) is no analysis: it is read as missing, not as 0%
#     clay (IN001, a Ck2 of silt loam texture with every fraction at 0.0).
#   * A row designated R, described or not, that lies inside the depth range
#     of another described horizon is not continuous rock below the profile: it
#     is left out, with a note (IT017: an "R 0-5" inside "Apkc 0-35", the
#     bedrock below 230 cm entered with wrong depths). Other named rows with
#     neither laboratory data nor a description are kept: in some profiles
#     (FR009, UY002) they carry the designations of horizons whose data sit in
#     unnamed rows of the same depths, and they are not read as rock.
.isis_guard_layers <- function(layers) {
  notes <- character(0)
  nm <- function(L) as.character(L$layerName %||% NA_character_)
  where <- function(L) sprintf("%s (%g-%g cm)", if (is.na(nm(L))) "a sample row" else nm(L),
                               .isis_num(L$upperDepth), .isis_num(L$lowerDepth))
  for (k in seq_along(layers)) {
    v <- vapply(c("clay", "silt", "sand"), function(f) .isis_num(layers[[k]][[f]]), 0)
    if (any(is.finite(v)) && all(v[is.finite(v)] == 0)) {
      for (f in c("clay", "silt", "sand")) layers[[k]][f] <- list(NULL)
      notes <- c(notes, sprintf("The particle-size record of %s is all zeros and is read as missing.",
                                where(layers[[k]])))
    }
  }
  if (length(layers) < 2L) return(list(layers = layers, notes = notes))
  tp <- vapply(layers, function(L) .isis_num(L$upperDepth), 0)
  bt <- vapply(layers, function(L) .isis_num(L$lowerDepth), 0)
  desc <- vapply(layers, .isis_described, NA)
  rock <- vapply(layers, function(L) grepl("^([0-9]+|[IVX]+)?R", nm(L)), NA)
  host <- desc & !rock & is.finite(tp) & is.finite(bt)
  drop <- integer(0)
  for (r in which(rock & is.finite(tp) & is.finite(bt))) {
    # inside, not at the lower boundary ("R 100-100" below a "C 50-100" stays)
    inside <- which(host & seq_along(layers) != r & tp <= tp[r] & bt >= bt[r] & tp[r] < bt)
    if (!length(inside)) next
    drop <- c(drop, r)
    notes <- c(notes, sprintf(
      "ISIS layer %s lies inside the described horizon %s: it is not read as continuous rock and is left out.",
      where(layers[[r]]), where(layers[[inside[1]]])))
  }
  if (length(drop)) layers <- layers[-drop]
  list(layers = layers, notes = notes)
}

# ---- One profile ------------------------------------------------------------

.isis_wrb_full <- function(w) {
  q <- w$qualifier
  if (is.null(q) || !length(q)) return(w$referenceSoilGroup)
  pos <- vapply(q, function(x) as.character(x$position %||% ""), "")
  plc <- vapply(q, function(x) as.numeric(x$place %||% 99), 0)
  nm  <- vapply(q, function(x) as.character(x$qualifier %||% ""), "")
  pre <- pos %in% c("principal", "prefix"); suf <- !pre
  paste0(paste(c(nm[pre][order(plc[pre])], w$referenceSoilGroup), collapse = " "),
         if (any(suf)) paste0(" (", paste(nm[suf][order(plc[suf])], collapse = ", "), ")") else "")
}

# ISIS names a class in the singular ("Ferralsol", "Oxisol"); soilKey in the
# plural. Solonetz is its own plural.
.isis_plural <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NA_character_)
  if (grepl("(s|z)$", x)) x else paste0(x, "s")
}

.isis_reference <- function(p) {
  out <- list()
  w <- p$wrbClassifications %||% list()
  if (length(w)) {
    yr <- vapply(w, function(x) as.integer(x$publicationYear %||% NA), 0L)
    w <- w[[which.max(replace(yr, is.na(yr), -1L))]]
    rsg <- as.character(w$referenceSoilGroup %||% NA)
    out$reference_wrb <- .isis_plural(rsg)
    out$reference_wrb_full <- .isis_wrb_full(w)
    out$reference_wrb_edition <- as.character(w$publicationYear)
  }
  u <- p$usdaClassifications %||% list()
  if (length(u)) {
    yr <- vapply(u, function(x) as.integer(x$publicationYear %||% NA), 0L)
    u <- u[[which.max(replace(yr, is.na(yr), -1L))]]
    ord <- as.character(u$orderName %||% NA)
    out$reference_usda <- .isis_plural(ord)
    out$reference_usda_suborder <- as.character(u$suborder %||% NA)
    out$reference_usda_grtgroup <- as.character(u$greatGroup %||% NA)
    sg <- as.character(u$subgroup %||% NA)
    gg <- as.character(u$greatGroup %||% NA)
    out$reference_usda_subgroup <- if (is.na(sg) || is.na(gg)) NA_character_ else paste(sg, gg)
    out$reference_usda_edition <- as.character(u$publicationYear)
  }
  out
}

# The soil moisture and temperature regimes: the profile's own, else the
# latest USDA record that gives them.
.isis_regime <- function(p, profile_field, usda_field) {
  r <- .isis_desc(p[[profile_field]])
  if (is.na(r)) {
    u <- p$usdaClassifications %||% list()
    yr <- vapply(u, function(x) as.integer(x$publicationYear %||% NA), 0L)
    for (i in order(-replace(yr, is.na(yr), -1L))) {
      v <- u[[i]][[usda_field]]
      if (!is.null(v) && nzchar(v)) { r <- v; break }
    }
  }
  if (is.na(r)) return(NULL)
  r <- tolower(trimws(r))
  if (identical(r, "torric")) "aridic" else r
}

# The groundwater of the site (FAO groundwaterKind): N none observed, W
# observed, A apparent, P perched, F flooded. Its depth is depthGroundwater,
# in cm; ISIS writes 0 there when it records the level in groundwaterTop
# instead (the upper limit of the water table), which is then read. A depth
# of 0 with neither stays unknown, unless the site is flooded; none is kept
# when no water table was observed.
.ISIS_GROUNDWATER <- c(N = "none observed", W = "observed", A = "apparent",
                       P = "perched", F = "flooded")
.isis_groundwater <- function(p) {
  kind <- .isis_lookup(.ISIS_GROUNDWATER, .isis_code(p$groundwaterKind))
  d <- .isis_num(p$depthGroundwater); t <- .isis_num(p$groundwaterTop)
  depth <- if (is.finite(d) && d > 0) d else if (is.finite(t) && t > 0) t
           else if (identical(kind, "flooded")) 0 else NA_real_
  if (identical(kind, "none observed")) depth <- NA_real_
  list(kind = kind, depth = depth)
}

.isis_layers_to_horizons <- function(layers, absent_unless_recorded = TRUE,
                                     cracking = NA_character_, gilgai = FALSE,
                                     cracks_surface = NA_character_,
                                     dry_at_surface = FALSE) {
  layers <- Filter(function(L) !is.null(L$upperDepth) && !is.null(L$lowerDepth), layers)
  if (!length(layers)) return(list(horizons = NULL, provenance = NULL, notes = character(0)))
  if (all(vapply(layers, .isis_is_sample_row, NA)))
    return(list(horizons = NULL, provenance = NULL,
                notes = "ISIS has sample rows only for this profile."))
  gd <- .isis_guard_layers(layers)
  mg <- .isis_merge_samples(gd$layers)
  H <- mg$layers
  top <- vapply(H, function(L) .isis_num(L$upperDepth), 0)
  ord <- order(top); H <- H[ord]
  out <- data.frame(top_cm = vapply(H, function(L) .isis_num(L$upperDepth), 0),
                    bottom_cm = vapply(H, function(L) .isis_num(L$lowerDepth), 0),
                    designation = vapply(H, function(L) as.character(L$layerName %||% NA), ""),
                    stringsAsFactors = FALSE)
  prov <- list()
  note <- function(col, idx, text, source = "measured") {
    if (length(idx)) prov[[length(prov) + 1L]] <<- data.frame(
      horizon_idx = idx, attribute = col, source = source, confidence = 1,
      notes = text, stringsAsFactors = FALSE)
  }
  setnum <- function(col, field, factor = 1, unit = "") {
    v <- vapply(H, function(L) .isis_num(L[[field]]), 0) * factor
    if (is.null(out[[col]])) out[[col]] <<- NA_real_
    fill <- is.na(out[[col]]) & is.finite(v)
    out[[col]][fill] <<- v[fill]
    note(col, which(fill), sprintf("ISIS %s (%s)%s", field, unit,
                                   if (factor != 1) sprintf(" x %g", factor) else ""))
  }
  setnum("clay_pct", "clay", 1, "g/100g"); setnum("silt_pct", "silt", 1, "g/100g")
  setnum("sand_pct", "sand", 1, "g/100g")
  si <- 100 - out$sand_pct - out$clay_pct
  fill <- is.na(out$silt_pct) & is.finite(si) & si >= 0
  out$silt_pct[fill] <- si[fill]
  note("silt_pct", which(fill), "calculated: 100 - sand - clay (ISIS)")
  setnum("water_dispersible_clay_pct", "clayWaterdispTotal", 1, "g/100g, a clay content")
  setnum("coarse_fragments_pct", "cfvo", 1, "cm3/100cm3")
  setnum("ph_h2o", "phaq", 1, "pH water"); setnum("ph_kcl", "phkc", 1, "pH KCl")
  setnum("ph_cacl2", "phca", 1, "pH CaCl2"); setnum("ph_naf", "phnf", 1, "pH NaF")
  setnum("oc_pct", "orgc", 0.1, "g/kg"); setnum("n_total_pct", "nitkjd", 0.1, "g/kg")
  setnum("cec_cmol", "cecph7", 1, "cmol(c)/kg, pH 7"); setnum("ecec_cmol", "ecec", 1, "cmol(c)/kg")
  setnum("ca_cmol", "caExcph7", 1, "cmol(c)/kg"); setnum("mg_cmol", "mgExcph7", 1, "cmol(c)/kg")
  setnum("k_cmol", "kExcph7", 1, "cmol(c)/kg"); setnum("na_cmol", "naExcph7", 1, "cmol(c)/kg")
  setnum("al_cmol", "alExch", 1, "cmol(c)/kg, 1 M KCl"); setnum("al_kcl_cmol", "alExch", 1, "cmol(c)/kg, 1 M KCl")
  setnum("esp_pct", "esp", 1, "%")
  setnum("caco3_pct", "tceq", 0.1, "g/kg"); setnum("caso4_pct", "gypsumContent", 0.1, "g/kg")
  setnum("fe_dcb_pct", "feExtdith", 1, "g/100g"); setnum("fe_ox_pct", "feExtacox", 1, "g/100g")
  setnum("al_ox_pct", "alExtox", 1, "g/100g"); setnum("si_ox_pct", "sietox", 1, "g/100g")
  setnum("bulk_density_g_cm3", "bdfiod", 1, "kg/dm3, oven dry")
  setnum("bulk_density_g_cm3", "bdfiot", 1, "kg/dm3")
  setnum("ec_dS_m", "elcosp", 1, "dS/m, saturated paste")
  # EC 1:2.5 only where it decides the salic horizon (Hugo, 2026-10-29): the
  # paste is less dilute, so its EC is at least EC 1:2.5 (a value of 15 or
  # more is enough), and at most EC 1:2.5 x 12.5 (an upper bound, read only by
  # the salic test).
  e25 <- vapply(H, function(L) .isis_num(L$elco25), 0)
  fill <- is.na(out$ec_dS_m) & is.finite(e25) & e25 >= 15
  out$ec_dS_m[fill] <- e25[fill]
  note("ec_dS_m", which(fill), "ISIS elco25 (EC 1:2.5) as a lower bound of the saturated-paste EC")
  up <- ifelse(is.na(out$ec_dS_m) & is.finite(e25), e25 * .ISIS_EC_DILUTION, NA_real_)
  out$ec_max_dS_m <- up
  note("ec_max_dS_m", which(is.finite(up)), "ISIS elco25 (EC 1:2.5) x 12.5: upper bound of the saturated-paste EC")
  # water at 1500 kPa: ISIS gives it by volume, soilKey by mass
  wv <- vapply(H, function(L) .isis_num(L$wv1500), 0)
  out$water_content_1500kpa <- wv / out$bulk_density_g_cm3
  note("water_content_1500kpa", which(is.finite(out$water_content_1500kpa)),
       "ISIS wv1500 (cm3/100cm3) / bulk density")
  # base saturation, ECEC and Al saturation from the parts
  bases <- vapply(H, function(L) .isis_num(L$exbas), 0)
  four <- out$ca_cmol + out$mg_cmol + out$k_cmol + out$na_cmol
  bases[!is.finite(bases)] <- four[!is.finite(bases)]
  bs <- ifelse(is.finite(bases) & is.finite(out$cec_cmol) & out$cec_cmol > 0,
               pmin(100, 100 * bases / out$cec_cmol), NA_real_)
  out$bs_pct <- bs
  note("bs_pct", which(is.finite(bs)), "calculated: 100 x exchangeable bases / CEC at pH 7 (ISIS)")
  ec <- bases + out$al_cmol
  fill <- is.na(out$ecec_cmol) & is.finite(ec)
  out$ecec_cmol[fill] <- ec[fill]
  note("ecec_cmol", which(fill), "calculated: exchangeable bases + exchangeable Al (ISIS)")
  al <- ifelse(is.finite(out$al_cmol) & is.finite(out$ecec_cmol) & out$ecec_cmol > 0,
               100 * out$al_cmol / out$ecec_cmol, NA_real_)
  out$al_sat_pct <- al
  note("al_sat_pct", which(is.finite(al)), "calculated: 100 x exchangeable Al / ECEC (ISIS)")

  code <- function(field) vapply(H, function(L) .isis_code(L[[field]]), "")
  setchr <- function(col, field, table) {
    v <- vapply(code(field), function(k) .isis_lookup(table, k), "", USE.NAMES = FALSE)
    out[[col]] <<- unname(v)
    note(col, which(!is.na(v)), sprintf("ISIS %s (FAO code)", field))
  }
  for (x in list(c("munsell_hue_moist", "colourMoistHue"), c("munsell_hue_dry", "colourDryHue"))) {
    v <- code(x[2]); out[[x[1]]] <- v; note(x[1], which(!is.na(v)), sprintf("ISIS %s", x[2]))
  }
  for (x in list(c("munsell_value_moist", "colourMoistValue"), c("munsell_chroma_moist", "colourMoistChroma"),
                 c("munsell_value_dry", "colourDryValue"), c("munsell_chroma_dry", "colourDryChroma"))) {
    v <- suppressWarnings(as.numeric(code(x[2]))); out[[x[1]]] <- v
    note(x[1], which(!is.na(v)), sprintf("ISIS %s", x[2]))
  }
  setchr("structure_grade", "structureGrade1", .ISIS_GRADE)
  setchr("structure_type", "structureType", .ISIS_STRUCTURE)
  setchr("structure_size", "structureSize", .ISIS_SIZE)
  # v0.9.317: the second structure ISIS records, as the second-level structure
  # the first-level units break into; read only by the WRB nitic horizon
  setchr("structure_type_secondary", "structureType2", .ISIS_STRUCTURE)
  setchr("consistence_moist", "consistenceSoilmassMoist", .ISIS_MOIST)
  setchr("rupture_resistance", "consistenceSoilmassDry", .ISIS_DRY)
  setchr("stickiness", "stickinessClass", .ISIS_STICKY)
  setchr("plasticity", "consistencyWetPlasticity", .ISIS_PLASTIC)
  setchr("effervescence_class", "carbonateReaction", .ISIS_EFFERVESCENCE)
  setchr("cementation_class", "cementCompactDegree", .ISIS_CEMENT)
  setchr("mottle_contrast", "mottlesContrast", .ISIS_CONTRAST)
  setchr("boundary_distinctness", "horizBoundaryDistinctness", .ISIS_BOUNDARY)
  setchr("boundary_topography", "horizBoundaryTopography", .ISIS_TOPOGRAPHY)
  setchr("pan_type", "pansKind", .ISIS_PAN)

  # cutans: clay films, or slickensides
  kind <- code("cutansKind"); amt <- code("cutansAbundance"); thk <- code("cutansThickness")
  a <- vapply(amt, function(k) .isis_lookup(.ISIS_CUTAN_AMOUNT, k), "", USE.NAMES = FALSE)
  s <- vapply(thk, function(k) .isis_lookup(.ISIS_CUTAN_STRENGTH, k), "", USE.NAMES = FALSE)
  clay <- kind %in% .ISIS_CUTAN_CLAY
  # cutans with an abundance but no kind, in a horizon whose designation has t,
  # are clay films (Hugo, 2026-10-29)
  dsg <- ifelse(is.na(out$designation), "", out$designation)
  kindless_t <- is.na(kind) & !is.na(a) & a != "none" & grepl("^[0-9]*[A-Z]+[a-z]*t", dsg)
  clay <- clay | kindless_t
  out$clay_films_amount <- ifelse(clay, ifelse(is.na(a), "few", a),
                                  ifelse(amt %in% "0", "none", NA_character_))
  out$clay_films_strength <- unname(ifelse(clay, s, NA_character_))
  note("clay_films_amount", which(!is.na(out$clay_films_amount)),
       "ISIS cutansAbundance of clay cutans (FAO 1977: patchy few, broken common, continuous many; no abundance read as few; cutans of no kind in a t horizon read as clay)")
  note("clay_films_strength", which(!is.na(out$clay_films_strength)),
       "ISIS cutansThickness of clay cutans (thin faint, moderately thick distinct, thick prominent)")
  sl <- kind %in% .ISIS_CUTAN_SLICKENSIDES
  sa <- vapply(amt, function(k) .isis_lookup(.ISIS_SLICKENSIDES, k), "", USE.NAMES = FALSE)
  out$slickensides <- unname(ifelse(sl, ifelse(is.na(sa) | sa == "absent", "common", sa), NA_character_))
  note("slickensides", which(sl), "ISIS cutans of kind slickensides (patchy, broken or no abundance read as common; continuous as many)")
  # v0.9.317: cutans of kind pressure faces are the pressure faces of the WRB
  # nitic horizon (Ch 3.1.22): patchy, broken and continuous are few, common
  # and many, as for clay films, and with no abundance few
  pr <- kind %in% .ISIS_CUTAN_PRESSURE
  out$pressure_faces <- unname(ifelse(pr, ifelse(is.na(a), "few", a), NA_character_))
  note("pressure_faces", which(pr), "ISIS cutans of kind pressure faces (patchy few, broken common, continuous many; no abundance read as few)")
  # cracks: the profile's cracking (L large, C small) or, without it, gilgai,
  # in every horizon starting above 100 cm; the width of 0.5 cm is inferred
  cr <- if (!is.na(cracking) && cracking %in% c("L", "C")) cracking else if (isTRUE(gilgai)) "gilgai" else NA_character_
  if (!is.na(cr)) {
    rows <- which(out$top_cm < 100)
    out$cracks_width_cm <- NA_real_
    out$cracks_width_cm[rows] <- 0.5
    note("cracks_width_cm", rows, sprintf("inferred from the profile's %s: cracks of 0.5 cm or more",
      switch(cr, L = "cracking (large cracks)", C = "cracking (small cracks)", gilgai = "gilgai microrelief")))
  }
  # v0.9.315 (Hugo, 2026-10-30): the surface cracks of the profile. Medium
  # cracks (1-2 cm) are cracks of 1 cm. "No cracks observed" reads as no cracks
  # only where the profile was described dry at the surface: cracks "may be
  # evident only when the soil is dry" (WRB 2022 3.2.12, p. 80), so in a
  # profile described moist they stay unknown. Fine cracks (< 1 cm) do not
  # decide the 0.5 cm and are not read.
  if (is.na(cr) && !is.na(cracks_surface)) {
    rows <- which(out$top_cm < 100)
    if (cracks_surface == "M" || (cracks_surface == "N" && isTRUE(dry_at_surface))) {
      if (is.null(out$cracks_width_cm)) out$cracks_width_cm <- NA_real_
      out$cracks_width_cm[rows] <- if (cracks_surface == "M") 1 else 0
      note("cracks_width_cm", rows, if (cracks_surface == "M")
             "ISIS cracksSurface medium (1-2 cm): cracks of 1 cm"
           else "ISIS cracksSurface 'no cracks observed', profile described dry at the surface: no cracks")
    }
  }

  # coarse fragments from the field class when not measured
  pct <- function(field, table) vapply(code(field), function(k)
    if (is.na(k) || !k %in% names(table)) NA_real_ else table[[k]], 0, USE.NAMES = FALSE)
  rk <- pct("rocksArtefactAbundVol", .ISIS_ROCK_PCT)
  fill <- is.na(out$coarse_fragments_pct) & is.finite(rk)
  out$coarse_fragments_pct[fill] <- rk[fill]
  note("coarse_fragments_pct", which(fill), "middle of the FAO class of ISIS rocksArtefactAbundVol")

  # mottles
  m1 <- pct("mottlesAbundance", .ISIS_MOTTLE_PCT)
  m2 <- pct("mottlesAbundance2", .ISIS_MOTTLE_PCT)
  m <- ifelse(is.finite(m1) | is.finite(m2), pmin(100, ifelse(is.finite(m1), m1, 0) + ifelse(is.finite(m2), m2, 0)), NA_real_)
  out$redoximorphic_features_pct <- m
  note("redoximorphic_features_pct", which(is.finite(m)), "middle of the FAO class of ISIS mottlesAbundance (and mottlesAbundance2)")
  mc <- vapply(H, function(L) {
    v <- c(.isis_code(L$mottlesColour), .isis_code(L$mottlesColour2)); v <- v[!is.na(v)]
    if (length(v)) paste(v, collapse = "; ") else NA_character_ }, "")
  out$mottle_colors <- mc
  note("mottle_colors", which(!is.na(mc)), "ISIS mottlesColour")

  # inclusions: calcareous ones are secondary carbonates; hard ferruginous or
  # manganiferous concretions and nodules over 2 mm are nodules; soft ones join
  # the mottles as plinthite in a v horizon or an iron pan (Hugo, 2026-10-29).
  # Nothing is inferred where none is recorded.
  comp1 <- code("inclusionsComposition"); comp2 <- code("inclusionsComposition2")
  ab1 <- pct("mineralConcAbundance", .ISIS_ROCK_PCT); ab2 <- pct("mineralConcAbundance2", .ISIS_ROCK_PCT)
  part <- function(comp, ab, want) ifelse(comp %in% want & is.finite(ab), ab, NA_real_)
  addna <- function(x, y) ifelse(is.finite(x) | is.finite(y), pmin(100, ifelse(is.finite(x), x, 0) + ifelse(is.finite(y), y, 0)), NA_real_)
  sc <- addna(part(comp1, ab1, "K"), part(comp2, ab2, "K"))
  out$secondary_carbonates_pct <- sc
  note("secondary_carbonates_pct", which(is.finite(sc)), "middle of the FAO class of ISIS calcareous inclusions (mineralConcAbundance)")
  hard <- code("concretionsMineralHardness"); size <- code("inclusionsSize")
  big <- !size %in% c("S", "P")
  fe1 <- part(comp1, ab1, c("F", "M")); fe2 <- part(comp2, ab2, c("F", "M"))
  nod <- addna(ifelse(hard %in% c("H", "B") & big, fe1, NA_real_), ifelse(hard %in% c("H", "B") & big, fe2, NA_real_))
  out$ferric_nodules_pct <- nod
  note("ferric_nodules_pct", which(is.finite(nod)), "middle of the FAO class of ISIS hard Fe/Mn concretions and nodules (> 2 mm when the size is recorded)")
  soft <- addna(ifelse(hard %in% "S", fe1, NA_real_), ifelse(hard %in% "S", fe2, NA_real_))
  dsg_v <- grepl("^[0-9]*[A-Z]+[a-z]*v", dsg) | code("pansKind") %in% c("L", "I")
  pl <- ifelse(dsg_v, addna(out$redoximorphic_features_pct, soft), NA_real_)
  out$plinthite_pct <- pl
  note("plinthite_pct", which(is.finite(pl)), "mottles and soft Fe/Mn concretions of a v horizon or an iron pan (middle of the FAO classes)")

  # absent unless recorded, in a horizon with a field description
  if (isTRUE(absent_unless_recorded)) {
    desc <- vapply(H, .isis_described, NA)
    d <- ifelse(is.na(out$designation), "", out$designation)
    pan <- code("pansKind")
    why <- "not in the field description of this horizon: read as absent (absent_unless_recorded)"
    set0 <- function(col, rows, value) {
      if (is.null(out[[col]])) out[[col]] <<- rep(value[NA_integer_], nrow(out))
      rows <- rows & is.na(out[[col]])
      out[[col]][rows] <<- value
      note(col, which(rows), why)
    }
    # mottles stay unknown in a g, l or r horizon with no mottle field, and
    # where their colour, contrast or size is recorded without the abundance
    mfields <- vapply(H, function(L) !is.null(L$mottlesAbundance) || !is.null(L$mottlesAbundance2), NA)
    mother <- vapply(H, function(L) !is.null(L$mottlesColour) || !is.null(L$mottlesContrast) || !is.null(L$mottlesSizeclass), NA)
    glr <- grepl("^[0-9]*[A-Z]+[a-z]*[glr]", d) & !grepl("^[0-9]*R", d)
    set0("redoximorphic_features_pct", desc & !mfields & !(mother | glr), 0)
    # v0.9.313 (Hugo, 2026-10-29): ISIS has one cutans field per layer, so
    # slickensides and pressure faces are absent only where it is free. Where it
    # records cutans of another kind (clay cutans, say), or cutans of no kind,
    # they could not have been recorded there and stay unknown.
    occupied <- !is.na(kind) | (!is.na(amt) & amt != "0")
    set0("slickensides", desc & !occupied, "absent")
    # v0.9.317: pressure faces too, recorded in the same cutans field
    set0("pressure_faces", desc & !occupied, "none")
    set0("plinthite_pct", desc & !grepl("^[0-9]*[A-Z]+[a-z]*v", d) & !pan %in% c("L", "I"), 0)
    set0("duripan_pct", desc & !grepl("qm", d) & !pan %in% "D", 0)
    u <- grepl("^[0-9]*[A-Z]+[a-z]*u", d)
    set0("artefacts_pct", desc & !u, 0)
    set0("technic_hardmaterial_pct", desc & !u, 0)
    set0("geomembrane_present", desc & !u, FALSE)
  }
  prov <- if (length(prov)) do.call(rbind, prov) else
    data.frame(horizon_idx = integer(0), attribute = character(0), source = character(0),
               confidence = numeric(0), notes = character(0), stringsAsFactors = FALSE)
  rownames(out) <- NULL
  notes <- gd$notes
  if (mg$merged) notes <- c(notes, sprintf("%d laboratory sample row(s) with no field description were given to the described horizon that holds them.", mg$merged))
  list(horizons = out, provenance = prov, notes = notes)
}

# v0.9.314: fluviatile, marine and lacustrine parent materials
# (lithologyParmatDeposition F, M, L) as the rock_origin of the mineral layers
# (not O, H or R), when no second deposit of another kind is recorded.
.ISIS_WATER_DEPOSITION <- c(F = "fluviatile", M = "marine", L = "lacustrine")
.isis_water_deposition <- function(x, profile) {
  h <- x$horizons
  if (is.null(h) || !nrow(h)) return(x)
  d1 <- .isis_code(profile$lithologyParmatDeposition)
  d2 <- .isis_code(profile$lithologyParmatDeposition2)
  if (is.na(d1) || !d1 %in% names(.ISIS_WATER_DEPOSITION) ||
      (!is.na(d2) && !d2 %in% names(.ISIS_WATER_DEPOSITION))) return(x)
  origin <- unname(.ISIS_WATER_DEPOSITION[[d1]])
  if (!is.na(d2) && d2 != d1)
    origin <- paste(origin, "and", .ISIS_WATER_DEPOSITION[[d2]])
  dsg <- ifelse(is.na(h$designation), "", h$designation)
  rows <- which(!grepl("^[0-9]*[OHR]", dsg))
  if (!length(rows)) return(x)
  if (is.null(h$rock_origin)) h$rock_origin <- NA_character_
  h$rock_origin[rows] <- origin
  x$horizons <- h
  pv <- data.frame(horizon_idx = rows, attribute = "rock_origin", source = "measured",
                   confidence = 1,
                   notes = sprintf("ISIS lithologyParmatDeposition %s%s: deposition of the parent material",
                                   d1, if (is.na(d2)) "" else paste0(" and ", d2)),
                   stringsAsFactors = FALSE)
  x$provenance <- if (is.null(x$provenance) || !nrow(x$provenance)) pv
                  else rbind(x$provenance, pv)
  x
}

#' Turn one ISIS profile into a PedonRecord
#'
#' Builds a \code{\link{PedonRecord}} from one profile as the ISIS GraphQL
#' service returns it (one element of the \code{profiles} list; see
#' \code{\link{read_isis}}, which calls this for every profile it reads).
#' Laboratory values are converted to soilKey units, FAO field codes to
#' soilKey's words, and each value records its ISIS field in the provenance.
#' The site carries the WRB and USDA names ISRIC gives the profile
#' (\code{reference_wrb}, \code{reference_wrb_full}, \code{reference_usda},
#' \code{reference_usda_subgroup} and their editions), the soil moisture and
#' temperature regimes and the groundwater (\code{groundwater_kind},
#' \code{groundwater_depth_cm}) when ISIS records them, the licence and the
#' citation.
#'
#' @param profile One ISIS profile, a list as parsed from the service's JSON.
#' @param absent_unless_recorded If \code{TRUE} (default), mottles,
#'   slickensides, pressure faces, plinthite, a duripan and artefacts that
#'   the field description of a horizon does not mention are recorded as
#'   absent in that horizon; rows with laboratory data only stay unknown.
#'   ISIS has one cutans field per layer, so slickensides and pressure faces
#'   are read as absent only where that field is free (v0.9.313). \code{FALSE}
#'   leaves everything not recorded unknown.
#' @param accessed Date the data were read (default today).
#' @return A \code{\link{PedonRecord}}.
#' @export
isis_profile_to_pedon <- function(profile, absent_unless_recorded = TRUE,
                                  accessed = Sys.Date()) {
  stopifnot(is.list(profile), !is.null(profile$profileCode))
  code <- as.character(profile$profileCode)
  gil <- identical(.isis_code(profile$microreliefKind), "G")
  x <- .isis_layers_to_horizons(profile$layers %||% list(), absent_unless_recorded,
                                cracking = .isis_code(profile$cracking), gilgai = gil,
                                cracks_surface = .isis_code(profile$cracksSurface),
                                dry_at_surface = isTRUE(.isis_num(profile$dryfrom) == 0))
  site <- list(id = code,
               lat = suppressWarnings(as.numeric(profile$geog$latitude %||% NA)),
               lon = suppressWarnings(as.numeric(profile$geog$longitude %||% NA)),
               country = as.character(profile$countryCode %||% NA))
  el <- .isis_num(profile$siteElevation); if (is.finite(el)) site$elevation_m <- el
  sl <- .isis_num(profile$slopeGradientNumeric); if (is.finite(sl)) site$slope_pct <- sl
  dr <- .isis_desc(profile$drainageClass); if (!is.na(dr)) site$drainage_class <- tolower(dr)
  gw <- .isis_groundwater(profile)
  if (!is.na(gw$kind)) site$groundwater_kind <- gw$kind
  if (!is.na(gw$depth)) site$groundwater_depth_cm <- gw$depth
  pm <- .isis_desc(profile$lithologyParmat); if (!is.na(pm)) site$parent_material <- pm
  if (gil) site$gilgai_presence <- TRUE
  smr <- .isis_regime(profile, "moistureRegimeSoil", "moistureRegime")
  if (!is.null(smr)) site$soil_moisture_regime <- smr
  str <- .isis_regime(profile, "temperatureRegimeSoil", "temperatureRegime")
  if (!is.null(str)) site$soil_temperature_regime <- str
  ref <- .isis_reference(profile)
  site <- c(site, ref)
  site$reference_source <- "ISRIC ISIS (World Soil Reference Collection)"
  site$source   <- "ISRIC ISIS"
  site$dataset  <- "ISRIC World Soil Reference Collection"
  site$accessed <- format(as.Date(accessed), "%Y-%m-%d")
  site$licence  <- paste("ISRIC licence for its 1980-2015 point data: educational",
                         "and non-commercial use with the source acknowledged")
  site$citation <- isis_citation(accessed)
  site$attribution <- paste0("Soil profile ", code, " of the ISRIC World Soil ",
                             "Reference Collection, read through ISIS on ",
                             site$accessed, ".")
  if (length(x$notes)) site$depth_note <- paste(x$notes, collapse = " ")
  x <- .isis_water_deposition(x, profile)
  h <- if (is.null(x$horizons)) NULL else
    ensure_horizon_schema(data.table::as.data.table(x$horizons))
  prov <- if (is.null(x$provenance) || !nrow(x$provenance)) NULL else
    data.table::as.data.table(x$provenance)
  PedonRecord$new(site = site, horizons = h, provenance = prov)
}

#' Read profiles of the ISRIC World Soil Reference Collection (ISIS)
#'
#' Reads profiles live from ISRIC's ISIS GraphQL service and returns them as
#' \code{\link{PedonRecord}}s, ready for \code{\link{classify_all}} or
#' \code{\link{benchmark_isis}}. Nothing is written to disk. Each pedon carries
#' the names ISRIC gives it, the licence and the citation
#' (\code{\link{isis_citation}}); see \code{\link{isis_profile_to_pedon}} for
#' what the translation reads.
#'
#' @param profile_codes Profile codes, e.g. \code{c("KE009", "ZW005")}.
#' @param country ISO 3166 two-letter country code, e.g. \code{"KE"}, read
#'   when \code{profile_codes} is \code{NULL}.
#' @param max_profiles The most profiles to read (default 100).
#' @param absent_unless_recorded Passed to \code{\link{isis_profile_to_pedon}}.
#' @param timeout Seconds to wait for each request (default 60).
#' @param progress Optional \code{function(done, total)}.
#' @return A named list of \code{\link{PedonRecord}}s, one per profile read.
#'   On a failed request the profiles read until then are returned with a
#'   warning.
#' @examples
#' \dontrun{
#' p <- read_isis(c("KE009", "ZW005"))
#' classify_all(p$KE009)
#' p$KE009$site$reference_wrb_full
#' }
#' @export
read_isis <- function(profile_codes = NULL, country = NULL, max_profiles = 100L,
                      absent_unless_recorded = TRUE, timeout = 60,
                      progress = NULL) {
  if (is.null(profile_codes) && is.null(country))
    rlang::abort("Give `profile_codes` or `country`.")
  if (!is.null(profile_codes)) {
    profile_codes <- unique(toupper(trimws(as.character(profile_codes))))
    max_profiles <- min(max_profiles, length(profile_codes))
    filt <- "filter: { profileCode: { in: $codes } }"
    vars <- list(codes = as.list(profile_codes))
    decl <- "($codes: [String!])"
  } else {
    filt <- "filter: { countryCode: { equalTo: $country } }"
    vars <- list(country = toupper(trimws(as.character(country)[1])))
    decl <- "($country: String)"
  }
  step <- 5L; off <- 0L; got <- list(); accessed <- Sys.Date(); failed <- FALSE
  repeat {
    first <- min(step, max_profiles - length(got))
    if (first <= 0L) break
    q <- sprintf("query %s { profiles(first: %d, offset: %d, orderBy: [PROFILE_ID_ASC], %s) { %s } }",
                 decl, first, off, filt, .isis_profile_fields())
    r <- .isis_post(q, variables = vars, timeout = timeout)
    if (!is.null(r$error)) {
      rlang::warn(paste0("ISIS: ", r$error, if (length(got))
        sprintf(" (returning the %d profile(s) read so far)", length(got)) else ""))
      failed <- TRUE
      break
    }
    batch <- r$data$profiles %||% list()
    for (p in batch) got[[as.character(p$profileCode)]] <-
      isis_profile_to_pedon(p, absent_unless_recorded = absent_unless_recorded,
                            accessed = accessed)
    if (is.function(progress)) progress(length(got), max_profiles)
    if (length(batch) < first) break
    off <- off + first
  }
  if (!is.null(profile_codes) && !failed) {
    miss <- setdiff(profile_codes, names(got))
    if (length(miss) && length(got) < max_profiles)
      rlang::warn(paste("ISIS has no profile", paste(miss, collapse = ", ")))
  }
  got
}

#' Compare soilKey with ISRIC's names for ISIS profiles
#'
#' Classifies each pedon read by \code{\link{read_isis}} with the WRB 2022 and
#' Soil Taxonomy keys and sets the result beside the names ISRIC gives the
#' profile. The WRB comparison uses the Reference Soil Group, and only when
#' ISRIC's name is from the 2022 edition; the USDA one uses the order (ISRIC's
#' USDA names are from the 1975 to 1999 editions, so subgroups are shown but
#' not scored).
#'
#' @param pedons A list of \code{\link{PedonRecord}}s from \code{read_isis}.
#' @return A data frame, one row per profile, with ISRIC's and soilKey's names,
#'   \code{wrb_agree} and \code{usda_order_agree} (\code{NA} where ISRIC gives
#'   no comparable name) and the number of close calls of each key.
#' @export
benchmark_isis <- function(pedons) {
  rows <- lapply(pedons, function(p) {
    s <- p$site
    w <- tryCatch(classify_wrb2022(p, on_missing = "silent"), error = function(e) NULL)
    u <- tryCatch(classify_usda(p, on_missing = "silent"), error = function(e) NULL)
    ref_rsg <- s$reference_wrb %||% NA_character_
    ed <- s$reference_wrb_edition %||% NA_character_
    sk_rsg <- if (is.null(w)) NA_character_ else as.character(w$rsg_or_order %||% NA)
    ref_ord <- s$reference_usda %||% NA_character_
    sk_ord <- if (is.null(u)) NA_character_ else as.character(u$rsg_or_order %||% NA)
    data.frame(
      profile = s$id, country = s$country %||% NA_character_,
      isric_wrb = s$reference_wrb_full %||% NA_character_, isric_wrb_edition = ed,
      soilkey_wrb = if (is.null(w)) NA_character_ else w$name,
      isric_rsg = ref_rsg, soilkey_rsg = sk_rsg,
      wrb_agree = if (identical(ed, "2022") && !is.na(ref_rsg) && !is.na(sk_rsg))
        tolower(ref_rsg) == tolower(sk_rsg) else NA,
      wrb_close_calls = if (is.null(w)) NA_integer_ else length(w$ambiguities),
      isric_usda = s$reference_usda_subgroup %||% NA_character_,
      isric_usda_edition = s$reference_usda_edition %||% NA_character_,
      soilkey_usda = if (is.null(u)) NA_character_ else u$name,
      isric_usda_order = ref_ord, soilkey_usda_order = sk_ord,
      usda_order_agree = if (!is.na(ref_ord) && !is.na(sk_ord))
        tolower(ref_ord) == tolower(sk_ord) else NA,
      usda_close_calls = if (is.null(u)) NA_integer_ else length(u$ambiguities),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
