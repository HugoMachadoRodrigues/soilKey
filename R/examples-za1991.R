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
#' Since v0.9.359 each form example also holds the classes of one of its
#' families: the first of the form by default (Hu1100 for Hutton), or the
#' family named in \code{family}, made by changing the layer each family
#' criterion reads (its colour, clay, cations, structure, carbonates,
#' consistence) or adding the layer it reads (rock under the organic O, a
#' podzolic layer under the yellow-brown apedal B of Constantia).
#'
#' @param form The form, in lower case, or a special case.
#' @param notation Write the name of each horizon in \code{diagnostic_za}
#'   (the South African notation) besides the data (default \code{FALSE}).
#' @param family Optional family code of the form (\code{"Hu1200"}, or its
#'   four digits \code{"1200"}); \code{NULL} (default) for the first family.
#' @return A \code{\link{PedonRecord}}.
#' @examples
#' classify_za1991(make_za1991_example("clovelly"), on_missing = "silent")$name
#' classify_za1991(make_za1991_example("hutton", family = "Hu2200"), on_missing = "silent")$name
#' @export
make_za1991_example <- function(form = "hutton", notation = FALSE, family = NULL) {
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
  # v0.9.359: the classes of the family (the first of the form by default)
  fam <- .za_families_file()$family_digits[[f$code]]
  if (!is.null(fam)) {
    dg <- .za_ex_family_digits(f$code, family, length(fam))
    ls <- .za_ex_family(ls, s, fam, dg, layer)
  } else if (!is.null(family)) rlang::abort(sprintf("%s has no families.", f$name))
  build(ls, form)
}

# The digits of a family code ("Hu1200", "1200"), checked against the form;
# NULL gives the first family of the form.
.za_ex_family_digits <- function(code, family, n) {
  if (is.null(family)) return(rep(1L, n))
  fam <- as.character(family)[1L]
  if (startsWith(fam, code)) fam <- substring(fam, nchar(code) + 1L)
  codes <- vapply(.za_families_file()$families[[code]], function(e) e$code, "")
  if (!(paste0(code, fam) %in% codes))
    rlang::abort(sprintf("No family %s%s in the form %s (its families: %s).", code, fam, code, paste(codes, collapse = ", ")))
  as.integer(strsplit(fam, "")[[1]])[seq_len(n)]
}

#' The layers of an example made to hold the classes of a family: each
#' condition of each digit changes the layer the criterion reads (its
#' colour, clay, cations, structure, carbonates, consistence ...), or adds
#' the layer it reads (rock under an O, a podzolic layer under a
#' yellow-brown apedal B). Synthetic values, no data of the sources.
#' @noRd
.za_ex_family <- function(ls, s, fam, dg, layer) {
  slot <- function(tok) { j <- match(tok, s); if (is.na(j)) rlang::abort(sprintf("no %s in the example", tok)); j }
  set <- function(j, ...) { a <- list(...); ls[[j]][names(a)] <<- a }
  # a new thickness for layer j, the layers below moved with it (a lowest
  # layer reaching 150 cm keeps that end)
  resize <- function(j, th) {
    d <- (ls[[j]]$top_cm + th) - ls[[j]]$bottom_cm
    ls[[j]]$bottom_cm <<- ls[[j]]$top_cm + th
    for (k in seq_along(ls)[seq_along(ls) > j]) {
      ls[[k]]$top_cm <<- ls[[k]]$top_cm + d
      if (!(k == length(ls) && ls[[k]]$bottom_cm >= 150)) ls[[k]]$bottom_cm <<- ls[[k]]$bottom_cm + d
    }
  }
  wet <- function(j) set(j, mottle_colors = "10YR 6/2", redoximorphic_features_pct = 10)
  # a red pedocutanic B: cutans of another colour than the red matrix (S 31)
  red_cutans <- function(j) set(j, coatings_hue_moist = "10YR", coatings_value_moist = 4, coatings_chroma_moist = 3)
  conds <- unlist(lapply(seq_along(fam), function(p) fam[[p]]$classes[[dg[p]]]), recursive = FALSE)
  crit <- vapply(conds, function(cn) as.integer(cn$criterion), 0L)
  # thickness first (FC 2), the luvic clay before the base status (FC 10, 8)
  conds <- conds[order(match(crit, c(2L, 19L, 17L, 10L)), na.last = TRUE)]
  for (cn in conds) {
    cl <- as.character(unlist(cn$class))[1L]
    h <- cn$horizon
    j <- if (identical(h, "-")) length(s) else slot(h)
    switch(as.character(cn$criterion),
      "1" = set(j, fiber_content_unrubbed_pct = if (cl == "fibrous") 80 else 20),
      "2" = { resize(j, if (cl == "thick") 50 else 30); set(j, oc_pct = 3) },
      "3" = if (cl == "dark") set(j, munsell_value_moist = 3, munsell_chroma_moist = 1, munsell_value_dry = 5, munsell_chroma_dry = 1),
      "4" = if (cl == "bleached") set(j, munsell_value_dry = 7, munsell_chroma_dry = 2),
      "5" = switch(cl,
              dark = set(j, munsell_hue_moist = "10YR", munsell_value_moist = 3, munsell_chroma_moist = 1),
              red = { set(j, munsell_hue_moist = "2.5YR", munsell_value_moist = 3, munsell_chroma_moist = 4,
                          munsell_hue_dry = "2.5YR", munsell_value_dry = 4, munsell_chroma_dry = 4)
                      if (h == "PCU") red_cutans(j) },
              other = set(j, munsell_hue_moist = "10YR", munsell_value_moist = 4, munsell_chroma_moist = 3,
                          munsell_hue_dry = "10YR", munsell_value_dry = 5, munsell_chroma_dry = 3)),
      "6" = if (cl == "yellow") set(j, munsell_hue_moist = "10YR", munsell_value_moist = 5, munsell_chroma_moist = 6),
      "7" = if (cl == "present") set(j, lamellae_cumulative_cm = 3),
      "8" = { c0 <- ls[[j]]$clay_pct; v <- c(dystrophic = 2, mesotrophic = 10, eutrophic = 25)[[cl]] * c0 / 100
              set(j, ca_cmol = 0.7 * v, mg_cmol = 0.2 * v, k_cmol = 0.05 * v, na_cmol = 0.05 * v) },
      "9" = if (cl == "red") { set(j, munsell_hue_moist = "2.5YR", munsell_value_moist = 3, munsell_chroma_moist = 6,
                                   munsell_hue_dry = "2.5YR", munsell_value_dry = 4, munsell_chroma_dry = 6)
                               if (h == "PCU") red_cutans(j) }
            else set(j, munsell_hue_moist = "10YR", munsell_hue_dry = "10YR"),
      "10" = set(j, clay_pct = ls[[j - 1L]]$clay_pct + if (cl == "luvic") 10 else 2),
      "11" = if (cl == "medium_coarse") set(j, structure_type = "angular blocky", structure_size = "medium")
             else set(j, structure_type = "subangular blocky", structure_size = "fine"),
      "12" = if (cl == "with") set(j, coatings_value_moist = 2, coatings_chroma_moist = 1, coatings_continuity = "continuous"),
      "13" = if (cl == "hardened") set(j, cementation_class = "weakly"),
      "14" = if (h == "SAP") set(j, saprolite_pct = 80, rupture_resistance = if (cl == "hard") "very hard" else "slightly hard")
             else set(j, bedrock_pct = if (cl == "hard") 80 else 40),
      "15" = if (cl == "present") switch(cn$part, below = wet(j + 1L), wet(j)),
      "16" = if (cl == "calcareous") {
               k <- if (cn$part == "with_next") j + 1L else j
               # slightly calcareous, too little carbonate for a carbonate horizon
               set(k, effervescence_class = "slight", caco3_pct = 2, secondary_carbonates_pct = 0)
             },
      "17" = if (cl == "present") {
               b <- ls[[j]]$bottom_cm
               ls[[j]]$bottom_cm <- 110
               ls <- append(ls, list(layer("PZB", 110, max(b, 150))), after = j)
             },
      "18" = set(j, consistence_moist = cl),
      "19" = if (cl == "rock_or_saprolite") ls[[j + 1L]] <- layer("HRK", ls[[j]]$bottom_cm, ls[[j]]$bottom_cm + 20))
  }
  ls
}
