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
#   * Cracks: the profile's cracking or, without it, gilgai give cracks in the
#     horizons starting above 100 cm: large cracks and gilgai of 1 cm or more,
#     small cracks of 0.5 cm or more (v0.9.392, Hugo, 2026-11-10).
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
#     100 - sand - clay when only those two are given (v0.9.416: and sand
#     100 - clay - silt).
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
#   * v0.9.408 (Hugo, 2026-11-18): where the first field of a layer is empty,
#     the alternative that holds the same observation is read, with its own
#     provenance: the Munsell colour written whole (colourMoistMatrix, then
#     colourMoistMatrix2, and the same dry); the second entry of the structure
#     grade, type and size, of the mottle contrast and size and of the hardness
#     and size of the inclusions; and the coatings of nature clay, clay and
#     sesquioxides or clay and humus as clay films where no cutans are recorded.
#     The organic matter (orgm) is not read as organic carbon: ISIS measures it
#     by loss on ignition, and where a layer has both, orgm / orgc runs from
#     below 0.2 to above 11 (between 1.6 and 1.85 in 49 of 1,064 layers).
#   * v0.9.416 (Hugo, 2026-11-20, the Soil Taxonomy audit): the field texture
#     class (textureClassesFao2006Small) as texture_class_usda, which the
#     texture tests read where clay and sand are not measured; chroma 0, which
#     ISIS does not code, from the colour written whole, and for the neutral
#     hue N; no base saturation or ECEC from bases recorded without Ca (exbas
#     then is Mg + K + Na); sand as 100 - clay - silt; the optical density of
#     the oxalate extract (odoe); the decomposition of the organic matter of an
#     O or H horizon as a von Post index inside its class; and the share of a
#     recorded duripan from its continuity. ISIS records worm channels only as
#     a kind of biological feature, with no volume, so worm_holes_pct is not
#     read; its two silt fractions (fine and coarse silt, which add up to the
#     silt; the second is the larger in the loess profiles, so likely 2-20 and
#     20-50 um) serve no test: the 0.02-2 mm fraction of the Soil Taxonomy is
#     the one whose volcanic glass is counted, and ISIS has no glass counts.
# =============================================================================

.ISIS_ENDPOINT <- "https://graphql.isric.org/isis/graphql"

#' POST one GraphQL query to the ISIS endpoint (see .wosis_post).
#' @noRd
.isis_post <- function(query, variables = NULL, timeout = 60) {
  .wosis_post(query, variables = variables, timeout = timeout,
              endpoint = .ISIS_ENDPOINT)
}

# v0.9.408: reading the whole collection takes about 195 requests, and one
# failed request used to end the reading. A request is tried three times; a
# rejected query (HTTP 4xx) is not tried again.
.isis_post_retry <- function(query, variables = NULL, timeout = 60, tries = 3L) {
  wait <- getOption("soilKey.isis_retry_wait", 2)
  for (i in seq_len(tries)) {
    r <- .isis_post(query, variables = variables, timeout = timeout)
    if (is.null(r$error) || grepl("HTTP 4[0-9][0-9]", r$error)) return(r)
    if (i < tries) Sys.sleep(wait * i)
  }
  r$error <- sprintf("%s (after %d attempts)", r$error, tries)
  r
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
               "naExcph7", "exbas", "alExch", "acidityExc", "esp", "tceq", "gypsumContent",
               "feExtdith", "feExtacox", "alExtox", "sietox", "bdfiod", "bdfiot",
               "wv1500", "elcosp", "elco25", "clayWaterdispTotal", "phnf",
               "opticalDensityOxalate")
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
                 "mineralConcAbundance2",
                 "colourMoistMatrix", "colourMoistMatrix2",
                 "colourDryMatrix", "colourDryMatrix2",
                 "structureGrade2", "structureSize2",
                 "mottlesContrast2", "mottlesSizeclass2",
                 "concretionsMineralHardness2", "inclusionsSize2",
                 "coatingsNature", "coatingsAbundance", "coatingsContrast",
                 "mineralConcNature", "mineralConcNature2",
                 "concretionsMineralKinds", "concretionsMineralKinds2",
                 "concretionsMineralSize", "concretionsMineralSize2")
# v0.9.416: field observations read for one attribute each, which do not make
# a row a described horizon (.isis_described)
.ISIS_FIELD_OTHER <- c("textureClassesFao2006Small", "organicMattterDecomprate",
                       "cementCompactContinuity")

.isis_profile_fields <- function() {
  paste(
    "profileCode countryCode profileId geog { latitude longitude }",
    "moistureRegimeSoil temperatureRegimeSoil drainageClass siteElevation",
    "depthGroundwater groundwaterTop groundwaterKind",
    "slopeGradientNumeric lithologyParmat lithologyParmat2 lithologyParmatDeposition",
    "lithologyParmatDeposition2 lithologyParmatXxx lithologyParmatXx2 cracking cracksSurface dryfrom microreliefKind",
    "wrbClassifications(first: 20) { publicationYear referenceSoilGroupCode referenceSoilGroup qualifier }",
    "usdaClassifications(first: 20) { publicationYear orderName suborder greatGroup subgroup moistureRegime temperatureRegime }",
    "layers(first: 100, orderBy: [UPPER_DEPTH_ASC]) { layerNumber upperDepth lowerDepth layerName",
    paste(c(.ISIS_LAB, .ISIS_MORPH, .ISIS_FIELD_OTHER), collapse = " "), "}")
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
# v0.9.406 (Hugo, 2026-11-13): the parent material is every lithology field
# ISIS records, as written, joined by "; " in this order: the parent material
# (lithologyParmat, lithologyParmat2), its deposition (lithologyParmatDeposition,
# lithologyParmatDeposition2, such as "Ejecta ash") and its specific lithology
# (lithologyParmatXxx, lithologyParmatXx2, such as "Basic ash" or "Tuff").
# The water depositions (F, M, L of .ISIS_WATER_DEPOSITION) are left out:
# they are read as the rock_origin of the layers (v0.9.314,
# .isis_water_deposition()), and as text the keys that read the parent
# material took old marine and lacustrine deposits for lowland alluvium
# (Hugo, 2026-11-13). Until 0.9.405 only lithologyParmat was read, which
# most profiles record as "Unconsolidated and unspecified deposits".
.ISIS_PARENT_MATERIAL_FIELDS <- c("lithologyParmat", "lithologyParmat2",
                                  "lithologyParmatDeposition", "lithologyParmatDeposition2",
                                  "lithologyParmatXxx", "lithologyParmatXx2")
.isis_parent_material <- function(profile) {
  v <- vapply(.ISIS_PARENT_MATERIAL_FIELDS, function(k) {
    z <- profile[[k]]
    if (k %in% c("lithologyParmatDeposition", "lithologyParmatDeposition2") &&
        .isis_code(z) %in% names(.ISIS_WATER_DEPOSITION)) return(NA_character_)
    .isis_desc(z)
  }, character(1), USE.NAMES = FALSE)
  v <- trimws(v[!is.na(v)])
  v <- unique(v[nzchar(v)])
  if (!length(v)) NA_character_ else paste(v, collapse = "; ")
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
# v0.9.408: the coatings ISIS records apart from the cutans
.ISIS_COATING_CLAY <- c("C", "CS", "CH")       # clay; clay and sesquioxides; clay and humus
.ISIS_COATING_AMOUNT <- c(N = "none", V = "few", F = "few", C = "common",
                          M = "many", A = "many", D = "many")
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

# v0.9.408: a Munsell colour written whole ("7.5YR 3/2", or "N 5/" on the
# neutral axis, which has no chroma). Anything else ("N4/N5") is not read.
.isis_parse_matrix <- function(x) {
  none <- list(hue = NA_character_, value = NA_real_, chroma = NA_real_)
  if (is.na(x)) return(none)
  x <- trimws(x)
  m <- regmatches(x, regexec("^([0-9.]+[A-Z]+) ([0-9.]+)/([0-9.]+)$", x))[[1]]
  if (length(m)) return(list(hue = m[2], value = as.numeric(m[3]), chroma = as.numeric(m[4])))
  m <- regmatches(x, regexec("^N ([0-9.]+)/$", x))[[1]]
  if (length(m)) return(list(hue = "N", value = as.numeric(m[2]), chroma = NA_real_))
  none
}

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
    # v0.9.408 (Hugo, 2026-11-18): the name with its year, and every edition
    # ISRIC gives the profile, newest first; reference_wrb_full stays the name
    year_txt <- ifelse(is.na(yr), "year not recorded", as.character(yr))
    names_all <- vapply(w, function(x) as.character(.isis_wrb_full(x))[1], "")
    newest <- order(replace(yr, is.na(yr), -1L), decreasing = TRUE)
    out$reference_wrb_all <- paste(sprintf("%s: %s", year_txt[newest], names_all[newest]),
                                   collapse = " | ")
    top <- which.max(replace(yr, is.na(yr), -1L))
    w <- w[[top]]
    rsg <- as.character(w$referenceSoilGroup %||% NA)
    out$reference_wrb <- .isis_plural(rsg)
    out$reference_wrb_full <- .isis_wrb_full(w)
    out$reference_wrb_edition <- as.character(w$publicationYear)
    out$reference_wrb_cited <- sprintf("%s; publication year: %s", names_all[top], year_txt[top])
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
  # v0.9.416: and sand is 100 - clay - silt when only those two are given
  # (ES002, whose laboratory rows record clay and silt only)
  sa <- 100 - out$clay_pct - out$silt_pct
  fill <- is.na(out$sand_pct) & is.finite(sa) & sa >= 0
  out$sand_pct[fill] <- sa[fill]
  note("sand_pct", which(fill), "calculated: 100 - clay - silt (ISIS)")
  setnum("water_dispersible_clay_pct", "clayWaterdispTotal", 1, "g/100g, a clay content")
  setnum("coarse_fragments_pct", "cfvo", 1, "cm3/100cm3")
  setnum("ph_h2o", "phaq", 1, "pH water"); setnum("ph_kcl", "phkc", 1, "pH KCl")
  setnum("ph_cacl2", "phca", 1, "pH CaCl2"); setnum("ph_naf", "phnf", 1, "pH NaF")
  setnum("oc_pct", "orgc", 0.1, "g/kg"); setnum("n_total_pct", "nitkjd", 0.1, "g/kg")
  setnum("cec_cmol", "cecph7", 1, "cmol(c)/kg, pH 7"); setnum("ecec_cmol", "ecec", 1, "cmol(c)/kg")
  setnum("ca_cmol", "caExcph7", 1, "cmol(c)/kg"); setnum("mg_cmol", "mgExcph7", 1, "cmol(c)/kg")
  setnum("k_cmol", "kExcph7", 1, "cmol(c)/kg"); setnum("na_cmol", "naExcph7", 1, "cmol(c)/kg")
  setnum("al_cmol", "alExch", 1, "cmol(c)/kg, 1 M KCl"); setnum("al_kcl_cmol", "alExch", 1, "cmol(c)/kg, 1 M KCl")
  # v0.9.412: the exchangeable acidity (H + Al, 1 M KCl): with the four bases
  # it bounds the sum of bases and Al from above, which decides the Geric
  # qualifier where no Al is recorded (WRB 2022 Ch 5, p. 135).
  setnum("exch_acidity_kcl_cmol", "acidityExc", 1, "cmol(c)/kg, 1 M KCl")
  setnum("esp_pct", "esp", 1, "%")
  setnum("caco3_pct", "tceq", 0.1, "g/kg"); setnum("caso4_pct", "gypsumContent", 0.1, "g/kg")
  setnum("fe_dcb_pct", "feExtdith", 1, "g/100g"); setnum("fe_ox_pct", "feExtacox", 1, "g/100g")
  setnum("al_ox_pct", "alExtox", 1, "g/100g"); setnum("si_ox_pct", "sietox", 1, "g/100g")
  setnum("bulk_density_g_cm3", "bdfiod", 1, "kg/dm3, oven dry")
  setnum("bulk_density_g_cm3", "bdfiot", 1, "kg/dm3")
  setnum("ec_dS_m", "elcosp", 1, "dS/m, saturated paste")
  # v0.9.416: the optical density of the acid oxalate extract
  setnum("odoe", "opticalDensityOxalate", 1, "optical density of the oxalate extract")
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
  # v0.9.416 (Hugo, 2026-11-20): where Ca is not recorded, an exbas that is the
  # sum of the Mg, K and Na recorded is not the sum of the bases (48 calcareous
  # layers of 9 US profiles, where the NH4OAc extract dissolves carbonates and
  # Ca was not reported, and one layer of KE015): no base saturation and no
  # ECEC are calculated from it; they stay undecided
  others <- cbind(out$mg_cmol, out$k_cmol, out$na_cmol)
  some <- rowSums(is.finite(others)) > 0
  no_ca <- is.na(out$ca_cmol) & some & is.finite(bases) &
    abs(bases - rowSums(others, na.rm = TRUE)) < 0.15
  bases[no_ca] <- NA_real_
  ca_notes <- if (any(no_ca)) sprintf(
    "ISIS gives exchangeable bases without Ca in %d layer(s) (exbas is Mg + K + Na): base saturation and ECEC are not calculated there.",
    sum(no_ca)) else character(0)
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
  # v0.9.408: a layer with no hue, value and chroma of its own may hold the
  # colour written whole, in the first entry or the second
  for (st in c("Moist", "Dry")) {
    low <- tolower(st)
    hc <- paste0("munsell_hue_", low); vc <- paste0("munsell_value_", low); cc <- paste0("munsell_chroma_", low)
    for (field in paste0("colour", st, c("Matrix", "Matrix2"))) {
      parsed <- lapply(code(field), .isis_parse_matrix)
      hue <- vapply(parsed, function(z) z$hue, "")
      fill <- which(is.na(out[[hc]]) & is.na(out[[vc]]) & is.na(out[[cc]]) & !is.na(hue))
      if (!length(fill)) next
      out[[hc]][fill] <- hue[fill]
      out[[vc]][fill] <- vapply(parsed[fill], function(z) z$value, 0)
      out[[cc]][fill] <- vapply(parsed[fill], function(z) z$chroma, 0)
      for (col in c(hc, vc, cc))
        note(col, fill[!is.na(out[[col]][fill])], sprintf("ISIS %s (the colour written whole)", field))
    }
  }
  # v0.9.416: ISIS codes no chroma 0, so a layer with a hue and a value and no
  # chroma lost "7.5YR 2/0" (22 layers of KE067, KE066, ZW007, NL048, NL049,
  # NL051, NL056, NL057): the chroma 0 of the colour written whole, of the same
  # hue, is read; and the neutral hue N has chroma 0
  for (st in c("Moist", "Dry")) {
    low <- tolower(st)
    hc <- paste0("munsell_hue_", low); cc <- paste0("munsell_chroma_", low)
    for (field in paste0("colour", st, c("Matrix", "Matrix2"))) {
      parsed <- lapply(code(field), .isis_parse_matrix)
      hue <- vapply(parsed, function(z) z$hue, "")
      chr <- vapply(parsed, function(z) z$chroma, 0)
      fill <- which(!is.na(out[[hc]]) & is.na(out[[cc]]) & !is.na(hue) &
                      hue == out[[hc]] & chr %in% 0)
      out[[cc]][fill] <- 0
      note(cc, fill, sprintf("ISIS %s (chroma 0 of the colour written whole; ISIS codes no chroma 0)", field))
    }
    fill <- which(out[[hc]] %in% "N" & is.na(out[[cc]]))
    out[[cc]][fill] <- 0
    note(cc, fill, "the neutral hue N: chroma 0")
  }
  setchr("structure_grade", "structureGrade1", .ISIS_GRADE)
  setchr("structure_type", "structureType", .ISIS_STRUCTURE)
  setchr("structure_size", "structureSize", .ISIS_SIZE)
  # v0.9.408: the second entry stands in where the first is empty
  second_entry <- function(col, first, second, table) {
    v <- vapply(code(second), function(k) .isis_lookup(table, k), "", USE.NAMES = FALSE)
    fill <- is.na(code(first)) & !is.na(v)
    out[[col]][fill] <<- v[fill]
    note(col, which(fill), sprintf("ISIS %s (second entry; the first is empty)", second))
    fill
  }
  second_entry("structure_grade", "structureGrade1", "structureGrade2", .ISIS_GRADE)
  second_entry("structure_size", "structureSize", "structureSize2", .ISIS_SIZE)
  type_moved <- second_entry("structure_type", "structureType", "structureType2", .ISIS_STRUCTURE)
  # v0.9.317: the second structure ISIS records, as the second-level structure
  # the first-level units break into; read only by the WRB nitic horizon
  # (v0.9.408: not when it is the only structure of the layer)
  v <- vapply(code("structureType2"), function(k) .isis_lookup(.ISIS_STRUCTURE, k), "", USE.NAMES = FALSE)
  v[type_moved] <- NA_character_
  out$structure_type_secondary <- unname(v)
  note("structure_type_secondary", which(!is.na(v)), "ISIS structureType2 (FAO code)")
  # v0.9.413: its grade and size too, read by the Grumic qualifier
  for (x in list(c("structure_grade_secondary", "structureGrade2", "structureGrade1"),
                 c("structure_size_secondary", "structureSize2", "structureSize"))) {
    tab <- if (grepl("grade", x[1])) .ISIS_GRADE else .ISIS_SIZE
    v2 <- vapply(code(x[2]), function(k) .isis_lookup(tab, k), "", USE.NAMES = FALSE)
    v2[type_moved | is.na(code(x[3]))] <- NA_character_
    out[[x[1]]] <- unname(v2)
    note(x[1], which(!is.na(v2)), sprintf("ISIS %s (FAO code)", x[2]))
  }
  setchr("consistence_moist", "consistenceSoilmassMoist", .ISIS_MOIST)
  setchr("rupture_resistance", "consistenceSoilmassDry", .ISIS_DRY)
  setchr("stickiness", "stickinessClass", .ISIS_STICKY)
  setchr("plasticity", "consistencyWetPlasticity", .ISIS_PLASTIC)
  setchr("effervescence_class", "carbonateReaction", .ISIS_EFFERVESCENCE)
  setchr("cementation_class", "cementCompactDegree", .ISIS_CEMENT)
  setchr("mottle_contrast", "mottlesContrast", .ISIS_CONTRAST)
  second_entry("mottle_contrast", "mottlesContrast", "mottlesContrast2", .ISIS_CONTRAST)
  setchr("boundary_distinctness", "horizBoundaryDistinctness", .ISIS_BOUNDARY)
  setchr("boundary_topography", "horizBoundaryTopography", .ISIS_TOPOGRAPHY)
  setchr("pan_type", "pansKind", .ISIS_PAN)
  # v0.9.416: the field texture class (FAO 2006 codes, the USDA classes), read
  # by the texture tests only where clay and sand are not measured
  tc <- code("textureClassesFao2006Small")
  out$texture_class_usda <- tc
  note("texture_class_usda", which(!is.na(tc)), "ISIS textureClassesFao2006Small (FAO 2006 code)")
  # v0.9.416: the decomposition of the organic matter of an O or H horizon as
  # the von Post index, inside the range of its class: nil H1, slightly H3
  # (fibric, H1-H4), moderately H5 (hemic, H5-H6), highly H8 (sapric, H7-H10)
  dsg0 <- ifelse(is.na(out$designation), "", out$designation)
  vp <- unname(c("0" = 1L, S = 3L, M = 5L, H = 8L)[code("organicMattterDecomprate")])
  vp[!grepl("^[0-9]*[OH]([^A-Z]|$)", dsg0)] <- NA_integer_
  out$von_post_index <- as.integer(vp)
  note("von_post_index", which(!is.na(vp)),
       "ISIS organicMattterDecomprate of an O or H horizon (nil H1, slightly H3, moderately H5, highly H8)")
  # v0.9.416: a recorded duripan (pansKind D) is cemented in the share of the
  # FAO (2006) class of its continuity: broken (< 50 %) 25, discontinuous
  # (50-90 %) 70, continuous (> 90 %) 95. With no continuity recorded, the m of
  # its designation (continuous or nearly continuous cementation) reads as 95.
  # A q and m designation with no pan recorded stays unknown.
  dp <- unname(c(B = 25, D = 70, C = 95)[code("cementCompactContinuity")])
  suffix <- sub("^[0-9]*[A-Z]+", "", dsg0)
  dpan <- code("pansKind") %in% "D"
  dp[!dpan] <- NA_real_
  from_m <- dpan & is.na(dp) & grepl("m", suffix)
  dp[from_m] <- 95
  out$duripan_pct <- dp
  note("duripan_pct", which(dpan & !is.na(dp) & !from_m),
       "ISIS cementCompactContinuity of a duripan (pansKind D): middle of the FAO class (broken 25, discontinuous 70, continuous 95)")
  note("duripan_pct", which(from_m),
       "ISIS duripan (pansKind D) with no continuity recorded, designation m (continuous or nearly continuous cementation): 95")

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
  # v0.9.408: where no cutans are recorded, the coatings of clay are clay films
  coat <- is.na(kind) & is.na(amt) & is.na(thk) & code("coatingsNature") %in% .ISIS_COATING_CLAY
  if (any(coat)) {
    ca <- vapply(code("coatingsAbundance"), function(k) .isis_lookup(.ISIS_COATING_AMOUNT, k), "", USE.NAMES = FALSE)
    cs <- vapply(code("coatingsContrast"), function(k) .isis_lookup(.ISIS_CONTRAST, k), "", USE.NAMES = FALSE)
    out$clay_films_amount[coat] <- ifelse(is.na(ca[coat]), "few", ca[coat])
    out$clay_films_strength[coat] <- ifelse(ca[coat] %in% "none", NA_character_, cs[coat])
    note("clay_films_amount", which(coat),
         "ISIS coatingsAbundance of coatings of clay, no cutans recorded (very few and few are few; many, abundant and dominant are many; no abundance read as few)")
    note("clay_films_strength", which(coat & !is.na(out$clay_films_strength)),
         "ISIS coatingsContrast of coatings of clay, no cutans recorded")
  }
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
  # in every horizon starting above 100 cm. ISIS gives the cracking no width.
  # v0.9.392 (Hugo, 2026-11-10): large cracks and gilgai are cracks of 1 cm
  # or more, the lower limit of the medium surface cracks (1-2 cm) of the FAO
  # field guidelines' classes that ISIS's own cracksSurface uses (fine < 1 cm,
  # medium 1-2 cm); small cracks keep 0.5 cm, since a small crack may be a
  # fine one (< 1 cm). Before, all three were 0.5 cm.
  cr <- if (!is.na(cracking) && cracking %in% c("L", "C")) cracking else if (isTRUE(gilgai)) "gilgai" else NA_character_
  if (!is.na(cr)) {
    rows <- which(out$top_cm < 100)
    w <- if (cr == "C") 0.5 else 1
    out$cracks_width_cm <- NA_real_
    out$cracks_width_cm[rows] <- w
    note("cracks_width_cm", rows, sprintf("inferred from the profile's %s: cracks of %s cm or more",
      switch(cr, L = "cracking (large cracks)", C = "cracking (small cracks)", gilgai = "gilgai microrelief"),
      format(w)))
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
  # v0.9.316: the mottle size, FAO (2006) classes V, F, M and A (> 20 mm); the
  # older coarse class C (> 15 mm) does not decide the 20 mm of the HSCS iron
  # horizon, and H (heterogeneous) names no size, so neither is read
  ms <- code("mottlesSizeclass"); ms2 <- code("mottlesSizeclass2")
  from2 <- is.na(ms) & !is.na(ms2)
  ms[from2] <- ms2[from2]
  sz <- unname(c(V = "very fine", F = "fine", M = "medium", A = "coarse")[ms])
  out$mottle_size_class <- sz
  note("mottle_size_class", which(!is.na(sz) & !from2), "ISIS mottlesSizeclass (V, F, M, A of FAO 2006; C and H not read)")
  note("mottle_size_class", which(!is.na(sz) & from2), "ISIS mottlesSizeclass2 (second entry; the first is empty)")

  # inclusions: calcareous ones are secondary carbonates; hard ferruginous or
  # manganiferous concretions and nodules over 2 mm are nodules; soft ones join
  # the mottles as plinthite in a v horizon or an iron pan (Hugo, 2026-10-29).
  # Nothing is inferred where none is recorded.
  comp1 <- code("inclusionsComposition"); comp2 <- code("inclusionsComposition2")
  # v0.9.413: the FAO (2006) fields of the same observation where the older
  # ones are empty: mineralConcNature F (iron), M (manganese), FM (iron and
  # manganese) for the composition (other natures are not read)
  nat <- c(F = "F", M = "M", FM = "F")
  n1 <- unname(nat[code("mineralConcNature")]); n2 <- unname(nat[code("mineralConcNature2")])
  comp1[is.na(comp1)] <- n1[is.na(comp1)]; comp2[is.na(comp2)] <- n2[is.na(comp2)]
  ab1 <- pct("mineralConcAbundance", .ISIS_ROCK_PCT); ab2 <- pct("mineralConcAbundance2", .ISIS_ROCK_PCT)
  part <- function(comp, ab, want) ifelse(comp %in% want & is.finite(ab), ab, NA_real_)
  addna <- function(x, y) ifelse(is.finite(x) | is.finite(y), pmin(100, ifelse(is.finite(x), x, 0) + ifelse(is.finite(y), y, 0)), NA_real_)
  sc <- addna(part(comp1, ab1, "K"), part(comp2, ab2, "K"))
  out$secondary_carbonates_pct <- sc
  note("secondary_carbonates_pct", which(is.finite(sc)), "middle of the FAO class of ISIS calcareous inclusions (mineralConcAbundance)")
  hard <- code("concretionsMineralHardness"); size <- code("inclusionsSize")
  hard2 <- code("concretionsMineralHardness2"); size2 <- code("inclusionsSize2")
  hard[is.na(hard)] <- hard2[is.na(hard)]; size[is.na(size)] <- size2[is.na(size)]
  # v0.9.413: concretionsMineralSize V (< 2 mm), F, M, C (> 2 mm), and a soft
  # concretion (concretionsMineralKinds SC) is soft
  csz <- c(V = "S", F = "M", M = "L", C = "L")
  cs1 <- unname(csz[code("concretionsMineralSize")]); cs2 <- unname(csz[code("concretionsMineralSize2")])
  cs1[is.na(cs1)] <- cs2[is.na(cs1)]
  size[is.na(size)] <- cs1[is.na(size)]
  hard[is.na(hard) & (code("concretionsMineralKinds") %in% "SC" |
                        code("concretionsMineralKinds2") %in% "SC")] <- "S"
  big <- !size %in% c("S", "P")
  fe1 <- part(comp1, ab1, c("F", "M")); fe2 <- part(comp2, ab2, c("F", "M"))
  nod <- addna(ifelse(hard %in% c("H", "B") & big, fe1, NA_real_), ifelse(hard %in% c("H", "B") & big, fe2, NA_real_))
  out$ferric_nodules_pct <- nod
  note("ferric_nodules_pct", which(is.finite(nod)), "middle of the FAO class of ISIS hard Fe/Mn concretions and nodules (> 2 mm when the size is recorded; inclusionsComposition or mineralConcNature)")
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
    mother <- vapply(H, function(L) !is.null(L$mottlesColour) || !is.null(L$mottlesContrast) || !is.null(L$mottlesSizeclass) ||
                       !is.null(L$mottlesContrast2) || !is.null(L$mottlesSizeclass2), NA)
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
  notes <- c(gd$notes, ca_notes)
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
#' \code{reference_usda_subgroup} and their editions;
#' \code{reference_wrb_cited}, the latest WRB name with its publication year;
#' \code{reference_wrb_all}, every WRB edition, newest first), the soil moisture and
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
  # v0.9.317: ISIS harmonises the horizon designations to the FAO guidelines,
  # so the national keys that read their own notation (the Canadian z for a
  # frozen layer, for instance, which is FAO z for salts) must not read them
  # as theirs
  site <- list(id = code, designation_system = "fao2006",
               lat = suppressWarnings(as.numeric(profile$geog$latitude %||% NA)),
               lon = suppressWarnings(as.numeric(profile$geog$longitude %||% NA)),
               country = as.character(profile$countryCode %||% NA))
  el <- .isis_num(profile$siteElevation); if (is.finite(el)) site$elevation_m <- el
  sl <- .isis_num(profile$slopeGradientNumeric); if (is.finite(sl)) site$slope_pct <- sl
  dr <- .isis_desc(profile$drainageClass); if (!is.na(dr)) site$drainage_class <- tolower(dr)
  gw <- .isis_groundwater(profile)
  if (!is.na(gw$kind)) site$groundwater_kind <- gw$kind
  if (!is.na(gw$depth)) site$groundwater_depth_cm <- gw$depth
  # v0.9.413: the reader has read the field observations it can as absent
  # (absent_unless_recorded); the WRB qualifiers leave its other readings alone
  if (isTRUE(absent_unless_recorded)) site$absent_unless_recorded <- TRUE
  pm <- .isis_parent_material(profile); if (!is.na(pm)) site$parent_material <- pm
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
#' @param max_profiles The most profiles to read (default 100). Ignored with
#'   \code{all = TRUE}.
#' @param absent_unless_recorded Passed to \code{\link{isis_profile_to_pedon}}.
#' @param timeout Seconds to wait for each request (default 60).
#' @param progress Optional \code{function(done, total)}; \code{total} is
#'   \code{NA} with \code{all = TRUE}.
#' @param all If \code{TRUE}, reads the whole collection, including the
#'   profiles ISIS records with no country. It takes neither
#'   \code{profile_codes} nor \code{country}. A warning names the profiles that
#'   came with no horizons to classify; they stay in the result.
#' @return A named list of \code{\link{PedonRecord}}s, one per profile read.
#'   On a failed request the profiles read until then are returned with a
#'   warning. Each pedon gives ISRIC's latest WRB name in
#'   \code{site$reference_wrb_full}, its year in
#'   \code{site$reference_wrb_edition}, both together in
#'   \code{site$reference_wrb_cited}, and every edition in
#'   \code{site$reference_wrb_all}.
#' @examples
#' \dontrun{
#' p <- read_isis(c("KE009", "ZW005"))
#' classify_all(p$KE009)
#' p$KE009$site$reference_wrb_cited
#' everything <- read_isis(all = TRUE)
#' }
#' @export
read_isis <- function(profile_codes = NULL, country = NULL, max_profiles = 100L,
                      absent_unless_recorded = TRUE, timeout = 60,
                      progress = NULL, all = FALSE) {
  all <- isTRUE(all)
  if (all && (!is.null(profile_codes) || !is.null(country)))
    rlang::abort("`all = TRUE` reads the whole collection: give neither `profile_codes` nor `country`.")
  if (!all && is.null(profile_codes) && is.null(country))
    rlang::abort("Give `profile_codes` or `country`, or use `all = TRUE`.")
  if (all) {
    max_profiles <- Inf
    filt <- ""; vars <- NULL; decl <- ""
  } else if (!is.null(profile_codes)) {
    profile_codes <- unique(.isis_normalise_code(profile_codes))
    if (length(profile_codes) > max_profiles)
      rlang::warn(sprintf("Reading the first %d of %d profile codes: raise `max_profiles` to read them all.",
                          as.integer(max_profiles), length(profile_codes)))
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
    r <- .isis_post_retry(q, variables = vars, timeout = timeout)
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
    if (is.function(progress)) progress(length(got), if (all) NA_integer_ else max_profiles)
    if (length(batch) < first) break
    off <- off + first
  }
  if (!is.null(profile_codes) && !failed) {
    miss <- setdiff(profile_codes, names(got))
    if (length(miss) && length(got) < max_profiles)
      rlang::warn(paste("ISIS has no profile", paste(miss, collapse = ", ")))
  }
  if (all) {
    empty <- names(got)[vapply(got, function(p) is.null(p$horizons) || !nrow(p$horizons), NA)]
    if (length(empty))
      rlang::warn(sprintf("%d of the %d ISIS profiles read have no horizons to classify: %s",
                          length(empty), length(got), paste(empty, collapse = ", ")))
  }
  got
}

# ISIS codes are two capitals, digits and sometimes a small letter ("BT001a").
.isis_normalise_code <- function(x) {
  sub("^([A-Z]{2}[0-9]+)([A-Z])$", "\\1\\L\\2", toupper(trimws(as.character(x))), perl = TRUE)
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
#' A profile whose latest ISRIC name is older than 2022 is not scored:
#' \code{wrb_agree} is \code{NA} and it counts neither for nor against the
#' agreement. A message says how many profiles were scored and how many were
#' left out, by edition.
#'
#' @param pedons A list of \code{\link{PedonRecord}}s from \code{read_isis}.
#' @param quiet If \code{TRUE}, no message.
#' @return A data frame, one row per profile, with ISRIC's and soilKey's names,
#'   \code{wrb_agree} and \code{usda_order_agree} (\code{NA} where ISRIC gives
#'   no comparable name) and the number of close calls of each key. The
#'   attribute \code{"wrb_editions"} is a data frame with one row per edition of
#'   ISRIC's latest name: the profiles, those soilKey named, those with the same
#'   Reference Soil Group, and whether the edition is scored. The counts of the
#'   editions before 2022 are for information only.
#' @export
benchmark_isis <- function(pedons, quiet = FALSE) {
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
  ed <- ifelse(is.na(out$isric_wrb_edition), "none", out$isric_wrb_edition)
  named <- !is.na(out$isric_rsg) & !is.na(out$soilkey_rsg)
  same <- named & tolower(out$isric_rsg) == tolower(out$soilkey_rsg)
  eds <- sort(unique(ed))
  tab <- data.frame(edition = eds,
                    profiles = vapply(eds, function(e) sum(ed == e), 0L, USE.NAMES = FALSE),
                    named_by_soilkey = vapply(eds, function(e) sum(named[ed == e]), 0L, USE.NAMES = FALSE),
                    same_rsg = vapply(eds, function(e) sum(same[ed == e]), 0L, USE.NAMES = FALSE),
                    scored = eds == "2022", stringsAsFactors = FALSE)
  attr(out, "wrb_editions") <- tab
  if (!isTRUE(quiet)) {
    n <- sum(!is.na(out$wrb_agree)); ok <- sum(out$wrb_agree %in% TRUE)
    old <- tab[!tab$scored, , drop = FALSE]
    message(sprintf("WRB: %d of %d profiles scored against ISRIC's names of the 2022 edition, %d agree%s.%s",
                    n, nrow(out), ok, if (n) sprintf(" (%.1f %%)", 100 * ok / n) else "",
                    if (nrow(old)) paste0(" Not scored, by the edition of ISRIC's latest name: ",
                                          paste(sprintf("%s (%d)", old$edition, old$profiles), collapse = ", "), ".") else ""))
  }
  out
}
