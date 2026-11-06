# =============================================================================
# v0.9.358 -- example profiles of the South African soil classification of
# 1991: one synthetic profile per soil form, built from one layer per
# diagnostic horizon or material of the form's sequence (key.yaml), each
# with the attributes soilKey's tests read, and four special cases. Not real
# profiles; no data of the sources.
# =============================================================================

# One layer of each horizon or material: designation, colours, structure,
# texture and what its test reads.
.ZA_EX_LAYERS <- list(
  OO = list(designation = "O", munsell_hue_moist = "10YR", munsell_value_moist = 2, munsell_chroma_moist = 1,
            oc_pct = 30, water_saturation_days = 150, structure_type = "massive", structure_grade = "structureless"),
  VA = list(designation = "A", munsell_value_moist = 2, munsell_chroma_moist = 1, munsell_value_dry = 3, munsell_chroma_dry = 1,
            structure_type = "angular blocky", structure_grade = "strong", slickensides = "many", clay_pct = 55,
            silt_pct = 25, sand_pct = 20, oc_pct = 1.5, ca_cmol = 25, mg_cmol = 10, k_cmol = 0.5, na_cmol = 1),
  MA = list(designation = "A", munsell_value_moist = 2, munsell_chroma_moist = 1, munsell_value_dry = 3, munsell_chroma_dry = 1,
            structure_type = "subangular blocky", structure_grade = "moderate", slickensides = "none", clay_pct = 35,
            silt_pct = 25, sand_pct = 40, oc_pct = 2, ca_cmol = 15, mg_cmol = 5, k_cmol = 0.5, na_cmol = 0.3),
  HA = list(designation = "A", munsell_value_moist = 3, munsell_chroma_moist = 2, munsell_value_dry = 4, munsell_chroma_dry = 2,
            structure_type = "subangular blocky", structure_grade = "weak", clay_pct = 30, silt_pct = 20, sand_pct = 50,
            oc_pct = 3, ca_cmol = 0.5, mg_cmol = 0.2, k_cmol = 0.1, na_cmol = 0.05),
  OA = list(designation = "A", munsell_value_moist = 4, munsell_chroma_moist = 3, munsell_value_dry = 5, munsell_chroma_dry = 3,
            structure_type = "subangular blocky", structure_grade = "weak", clay_pct = 15, silt_pct = 15, sand_pct = 70,
            oc_pct = 1, ca_cmol = 1, mg_cmol = 0.5, k_cmol = 0.2, na_cmol = 0.1),
  E = list(designation = "E", munsell_value_moist = 5, munsell_chroma_moist = 2, munsell_value_dry = 7, munsell_chroma_dry = 2,
           clay_pct = 8, silt_pct = 10, sand_pct = 82, oc_pct = 0.3),
  G = list(designation = "G", munsell_value_moist = 5, munsell_chroma_moist = 1, munsell_value_dry = 6, munsell_chroma_dry = 1,
           structure_type = "angular blocky", structure_grade = "moderate", clay_pct = 40, silt_pct = 25, sand_pct = 35),
  RAB = list(designation = "B", munsell_hue_moist = "2.5YR", munsell_value_moist = 3, munsell_chroma_moist = 6,
             munsell_hue_dry = "2.5YR", munsell_value_dry = 4, munsell_chroma_dry = 6, clay_pct = 25),
  YAB = list(designation = "B", munsell_value_moist = 5, munsell_chroma_moist = 6, munsell_value_dry = 6, munsell_chroma_dry = 6,
             clay_pct = 25),
  RSB = list(designation = "B", munsell_hue_moist = "2.5YR", munsell_value_moist = 3, munsell_chroma_moist = 4,
             munsell_hue_dry = "2.5YR", munsell_value_dry = 4, munsell_chroma_dry = 4, structure_type = "subangular blocky",
             structure_grade = "moderate", clay_pct = 45, silt_pct = 20, sand_pct = 35),
  NCU = list(designation = "B", munsell_value_moist = 4, munsell_chroma_moist = 4, structure_type = "subangular blocky",
             structure_grade = "weak", variegated_colors = "10YR 5/6; 7.5YR 4/6", clay_pct = 22),
  NCA = list(designation = "Bk", munsell_value_moist = 5, munsell_chroma_moist = 4, effervescence_class = "strong",
             caco3_pct = 4, clay_pct = 20),
  PCU = list(designation = "Bt", munsell_value_moist = 4, munsell_chroma_moist = 4, structure_type = "angular blocky",
             structure_grade = "moderate", clay_films_amount = "many", clay_pct = 35, silt_pct = 20, sand_pct = 45),
  PRC = list(designation = "Btn", munsell_value_moist = 4, munsell_chroma_moist = 3, munsell_value_dry = 5, munsell_chroma_dry = 3,
             structure_type = "prismatic", structure_grade = "strong", clay_pct = 40, silt_pct = 20, sand_pct = 40),
  LCU = list(designation = "BC", munsell_value_moist = 4, munsell_chroma_moist = 4, bedrock_pct = 40, clay_pct = 20),
  PZB = list(designation = "Bhs", munsell_hue_moist = "7.5YR", munsell_value_moist = 3, munsell_chroma_moist = 4,
             munsell_hue_dry = "7.5YR", munsell_value_dry = 4, munsell_chroma_dry = 4, oc_pct = 1.5, ph_h2o = 4.5,
             al_ox_pct = 0.4, fe_ox_pct = 0.6, clay_pct = 5, silt_pct = 5, sand_pct = 90),
  PLP = list(designation = "Bsm", munsell_hue_moist = "5YR", munsell_value_moist = 3, munsell_chroma_moist = 4,
             cementation_class = "strongly", pan_type = "thin ironpan", clay_pct = 5),
  SPL = list(designation = "B", munsell_value_moist = 6, munsell_chroma_moist = 6, munsell_value_dry = 7, munsell_chroma_dry = 6,
             redoximorphic_features_pct = 25, mottle_colors = "10YR 6/1; 7.5YR 5/8", clay_pct = 30),
  HPL = list(designation = "Bm", munsell_hue_moist = "7.5YR", munsell_value_moist = 5, munsell_chroma_moist = 8,
             cementation_class = "indurated", pan_type = "ferricrete", plinthite_pct = 60),
  UMW = list(designation = "C", munsell_value_moist = 5, munsell_chroma_moist = 4, redoximorphic_features_pct = 10,
             mottle_colors = "10YR 6/2", clay_pct = 25),
  USW = list(designation = "C", munsell_value_moist = 5, munsell_chroma_moist = 2, munsell_value_dry = 6, munsell_chroma_dry = 2,
             clay_pct = 30),
  SCH = list(designation = "Ck", munsell_value_moist = 7, munsell_chroma_moist = 3, munsell_value_dry = 8, munsell_chroma_dry = 3,
             caco3_pct = 25, secondary_carbonates_pct = 15, effervescence_class = "strong", cementation_class = "none"),
  HCH = list(designation = "Ckm", munsell_value_moist = 7, munsell_chroma_moist = 3, munsell_value_dry = 8, munsell_chroma_dry = 2,
             caco3_pct = 50, effervescence_class = "strong", cementation_class = "indurated", pan_type = "calcrete"),
  DOR = list(designation = "Bqm", munsell_hue_moist = "7.5YR", munsell_value_moist = 5, munsell_chroma_moist = 4,
             cementation_class = "strongly", pan_type = "duripan", duripan_pct = 60),
  SAP = list(designation = "C", weathering_stage = "saprolite", saprolite_pct = 80, munsell_value_moist = 5, munsell_chroma_moist = 4),
  HRK = list(designation = "R"),
  SAL = list(designation = "C", stratification_pattern = "continuous", layer_origin = "fluvic", clay_pct = 18),
  RSA = list(designation = "C", munsell_hue_moist = "7.5YR", munsell_value_moist = 5, munsell_chroma_moist = 6,
             munsell_hue_dry = "7.5YR", munsell_value_dry = 6, munsell_chroma_dry = 6, sand_pct = 92, silt_pct = 5,
             clay_pct = 3, layer_origin = "aeolic", cross_bedding = TRUE, structure_type = "single grain"),
  MMD = list(designation = "C", anthropic_material = "earthy", clay_pct = 20),
  UMN = list(designation = "C", munsell_value_moist = 5, munsell_chroma_moist = 4, clay_pct = 20),
  NONE = list(designation = "C", munsell_value_moist = 5, munsell_chroma_moist = 4, clay_pct = 20))

# The thickness (cm) of each layer of an example, by its horizon.
.ZA_EX_THICK <- c(OO = 40, VA = 60, MA = 40, HA = 30, OA = 25, E = 25, PLP = 1, LCU = 30, HPL = 30, DOR = 30,
                  HCH = 30, SAP = 50, HRK = 20, PZB = 30, PCU = 45, G = 60)

#' Example pedons for the soil forms of the South African classification
#'
#' Small synthetic profiles written for the tests and examples, one per soil
#' form of the South African soil classification of 1991 (named in lower
#' case: \code{"hutton"}, \code{"clovelly"} ...), each with one layer per
#' diagnostic horizon or material of the form's sequence and the properties
#' soilKey's tests read, and five special cases: \code{"lichtenburg"} (named
#' by van Huyssteen 2020, outside the 1991 key: provisional),
#' \code{"lithosol"} (an orthic A of 80 mm over unconsolidated material: no
#' form, the land class rock and lithosols), \code{"undecided"} (an orthic A
#' over a B whose hue is not recorded: Hutton or Clovelly undecided),
#' \code{"buried"} (400 mm of aeolian sand over a Valsrivier: the buried soil
#' keys) and \code{"truncated"} (a red apedal B at the surface over a
#' dorbank: Garies, truncated). They are not real profiles.
#'
#' @param form The form, in lower case, or a special case.
#' @param notation Write the name of each horizon in \code{diagnostic_za}
#'   (the South African notation) besides the data (default \code{FALSE}).
#' @return A \code{\link{PedonRecord}}.
#' @examples
#' classify_za1991(make_za1991_example("clovelly"), on_missing = "silent")$name
#' @export
make_za1991_example <- function(form = "hutton", notation = FALSE) {
  fs <- .za_forms()
  keys <- tolower(vapply(fs, function(f) f$name, ""))
  specials <- c("lithosol", "undecided", "buried", "truncated")
  form <- match.arg(tolower(form), c(keys, specials))
  base <- list(munsell_hue_moist = "10YR", munsell_value_moist = 5, munsell_chroma_moist = 4, munsell_hue_dry = "10YR",
               munsell_value_dry = 6, munsell_chroma_dry = 4, structure_type = "massive", structure_grade = "structureless",
               clay_pct = 20, silt_pct = 20, sand_pct = 60, oc_pct = 0.3, effervescence_class = "none",
               clay_films_amount = "none")
  layer <- function(tok, top, bottom, ...) {
    d <- base
    t <- .ZA_EX_LAYERS[[tok]]
    d[names(t)] <- t
    a <- list(...)
    d[names(a)] <- a
    d$top_cm <- top; d$bottom_cm <- bottom
    if (notation) d$diagnostic_za <- if (tok == "NONE") "none" else .za_vocabulary()$info[tok, "name"]
    d
  }
  build <- function(ls, id) {
    nm <- unique(unlist(lapply(ls, names)))
    h <- as.data.frame(lapply(stats::setNames(nm, nm), function(k)
      unlist(lapply(ls, function(l) if (is.null(l[[k]])) NA else l[[k]]))), stringsAsFactors = FALSE)
    PedonRecord$new(site = list(id = paste0("za1991-", id), country = "ZA", lat = -28.5, lon = 29.5),
                    horizons = ensure_horizon_schema(data.table::as.data.table(h)))
  }
  if (form %in% specials) {
    ls <- switch(form,
      lithosol = list(layer("OA", 0, 8), layer("NONE", 8, 150)),
      undecided = list(layer("OA", 0, 25), layer("YAB", 25, 150, munsell_hue_moist = NA, munsell_hue_dry = NA)),
      buried = list(layer("RSA", 0, 40, designation = "C", cross_bedding = NA),
                    layer("OA", 40, 60, designation = "2Ab"), layer("PCU", 60, 100, designation = "2Btb"),
                    layer("UMN", 100, 160, designation = "2Cb")),
      truncated = list(layer("RAB", 0, 40), layer("DOR", 40, 70)))
    if (notation && form == "buried") ls[[1L]]$diagnostic_za <- "regic sand"
    return(build(ls, form))
  }
  f <- fs[[which(keys == form)]]
  s <- f$slots
  ls <- list(); top <- 0
  extra <- list()
  # the transitions the tests read: abrupt into a prismacutanic B, gradual
  # into a red structured B
  if ("PRC" %in% s) extra$boundary_distinctness <- "abrupt"
  if ("RSB" %in% s) extra$boundary_distinctness <- "gradual"
  for (j in seq_along(s)) {
    tok <- s[j]
    last <- j == length(s)
    th <- if (!is.na(.ZA_EX_THICK[tok])) .ZA_EX_THICK[[tok]] else 35
    bottom <- if (last && !tok %in% c(.ZA_TOPSOILS, "HRK", "PLP", "HPL", "DOR", "HCH", "LCU", "SAP", "PZB")) 150 else top + th
    if (tok == "SAP" && last) bottom <- top + th
    args <- c(list(tok, top, bottom), if (j < length(s)) extra)
    ls[[j]] <- do.call(layer, args)
    top <- bottom
  }
  # what lies below the last horizon: rock under a lithocutanic B; a layer of
  # no diagnostic horizon under a pan or the podzol B of a podzol form
  tl <- s[length(s)]
  if (tl == "LCU") ls[[length(ls) + 1L]] <- layer("HRK", top, top + 20)
  if (tl %in% c("PLP", "HPL", "DOR", "HCH")) ls[[length(ls) + 1L]] <- layer("NONE", top, 150)
  # the podzol and topsoil-only forms: thinner topsoils over sandy layers
  if ("PZB" %in% s) {
    ls[[1L]]$bottom_cm <- 10; ls[[1L]]$clay_pct <- 5; ls[[1L]]$sand_pct <- 85; ls[[1L]]$silt_pct <- 10
    sh <- 15
    for (j in seq_along(ls)[-1L]) { ls[[j]]$top_cm <- ls[[j]]$top_cm - sh; if (ls[[j]]$bottom_cm < 150) ls[[j]]$bottom_cm <- ls[[j]]$bottom_cm - sh }
    for (j in seq_along(ls)) if (identical(ls[[j]]$designation, "E")) { ls[[j]]$clay_pct <- 3; ls[[j]]$munsell_chroma_dry <- 1 }
  }
  if (length(s) == 1L) ls[[length(ls) + 1L]] <- layer("NONE", ls[[1L]]$bottom_cm, 150)
  build(ls, form)
}
