# =============================================================================
# v0.9.317 -- shared readings for WRB 2022 Chapter 3 criteria that several
# diagnostics word the same way:
#   * the lithic discontinuity read from the data (3.2.7, p. 76-77), for the
#     argic horizon criterion 2.a.i;
#   * the plough layer, for the argic horizon criterion 2.a.ii;
#   * soil aggregate structure with an average aggregate size of <= 10 cm, or
#     cloddy structure (criterion 1 of the mollic and umbric horizons, 3.1.20
#     and 3.1.39; criterion 1 of the irragric horizon, 3.1.18).
# =============================================================================

# Whether each layer is mineral material: not an O or H layer and < 20% SOC
# (WRB 2022 Ch 3.3.15 organic material, read as soilKey reads it elsewhere).
.wrb_mineral_layer <- function(h) {
  n <- nrow(h)
  d <- as.character(h$designation %||% rep(NA_character_, n))
  oc <- h$oc_pct %||% rep(NA_real_, n)
  !(!is.na(oc) & oc >= 20) & !(!is.na(d) & grepl("^[0-9]*[OH]", d))
}

#' Lithic discontinuities read from the data (WRB 2022 Ch 3.2.7, p. 76-77)
#'
#' For each layer, in the order of the horizon table: whether the data show a
#' lithic discontinuity between it and the layer directly above. Two directly
#' superimposed layers of mineral material are compared by the criteria that
#' soilKey can read:
#' \itemize{
#'   \item criterion 2.a.iv: \eqn{\ge} 10\% sand and \eqn{\ge} 10\% silt (in
#'         both layers), a difference of \eqn{\ge} 25\% in the ratio sand to
#'         silt (ABS(ratio1 - ratio2) / MAX(ratio1, ratio2) x 100, p. 77) and
#'         a difference of \eqn{\ge} 5\% (absolute) in sand and/or silt.
#'         Criterion 2.b (the differences are not patches within a layer) is
#'         read as met: the two are layers of their own.
#'   \item criterion 6: the overlying layer has \eqn{\ge} 10\% (absolute, by
#'         volume) more coarse fragments, unless animal activity created the
#'         difference: with bioturbation or worm holes recorded in either
#'         layer it is left undecided.
#' }
#' The other criteria need what soilKey does not record (the sand fractions,
#' the lithology, rounding and weathering rinds of the coarse fragments,
#' resistant minerals, TiO2/ZrO2) or a judgement of soil formation (criteria
#' 1, 7 and 8); criterion 11 (CEC per kg clay) is not read, see the v0.9.317
#' notes. A pair the data do not decide is not a discontinuity, as a
#' designation without a material number is not.
#' @param h Horizons table.
#' @return A list: \code{at} (logical, TRUE where a discontinuity lies at the
#'   upper boundary of the layer) and \code{criterion} (the criterion that
#'   showed it, NA elsewhere).
#' @noRd
.wrb_lithic_discontinuity_above <- function(h) {
  n <- nrow(h)
  at <- rep(FALSE, n)
  crit <- rep(NA_character_, n)
  if (n < 2L) return(list(at = at, criterion = crit))
  col <- function(v) suppressWarnings(as.numeric(h[[v]] %||% rep(NA_real_, n)))
  sand <- col("sand_pct"); silt <- col("silt_pct"); cf <- col("coarse_fragments_pct")
  worm <- col("worm_holes_pct")
  bio <- .clay_films_class(h$bioturbation_density %||% rep(NA_character_, n))
  animal <- (!is.na(worm) & worm > 0) | (!is.na(bio) & bio >= 1L)
  mineral <- .wrb_mineral_layer(h)
  for (i in seq.int(2L, n)) {
    u <- i - 1L
    if (!mineral[u] || !mineral[i]) next
    # criterion 2.a.iv, sand and silt
    if (!anyNA(c(sand[c(u, i)], silt[c(u, i)])) &&
        all(sand[c(u, i)] >= 10) && all(silt[c(u, i)] >= 10)) {
      r <- sand[c(u, i)] / silt[c(u, i)]
      if (abs(r[1] - r[2]) / max(r) * 100 >= 25 &&
          (abs(sand[u] - sand[i]) >= 5 || abs(silt[u] - silt[i]) >= 5)) {
        at[i] <- TRUE
        crit[i] <- "3.2.7 criterion 2.a.iv (ratio sand to silt)"
        next
      }
    }
    # criterion 6, more coarse fragments in the overlying layer
    if (!is.na(cf[u]) && !is.na(cf[i]) && cf[u] - cf[i] >= 10 &&
        !animal[u] && !animal[i]) {
      at[i] <- TRUE
      crit[i] <- "3.2.7 criterion 6 (coarse fragments)"
    }
  }
  list(at = at, criterion = crit)
}

#' Plough layers: horizons whose designation has the suffix p (Ap, Ap2, 2Apk),
#' FAO (2006) Guidelines: a mineral horizon disturbed by ploughing is an Ap.
#' @noRd
.wrb_plough_layer <- function(h) {
  n <- nrow(h)
  d <- .desg_master(h$designation %||% rep(NA_character_, n))
  !is.na(d) & grepl("^A[a-z]*p", d)
}

# Size class of structural units, ordinal: 1 very fine (very thin), 2 fine
# (thin), 3 medium, 4 coarse (thick), 5 very coarse (very thick), 6 extremely
# coarse; Portuguese: muito pequena, pequena, media, grande, muito grande. A
# range ("fine and medium", "pequena a media") is read by its upper end, since
# the size is tested against a maximum.
.wrb_structure_size_class <- function(x) {
  x <- tolower(trimws(as.character(x)))
  one <- function(s) {
    if (is.na(s) || !nzchar(s)) return(NA_integer_)
    parts <- trimws(strsplit(s, "\\s+(a|to|and|e|ou|or)\\s+|\\s*[-/,;]\\s*")[[1]])
    cls <- vapply(parts, function(p) {
      if (grepl("^(extremely coarse|extremamente grande)", p)) 6L
      else if (grepl("^(very coarse|very thick|muito grande)", p)) 5L
      else if (grepl("^(very fine|very thin|muito pequena)", p)) 1L
      else if (grepl("^(coarse|thick|grande)", p)) 4L
      else if (grepl("^(medium|m\u00e9dia|media)", p)) 3L
      else if (grepl("^(fine|thin|pequena)", p)) 2L
      else NA_integer_
    }, integer(1))
    if (all(is.na(cls))) NA_integer_ else max(cls, na.rm = TRUE)
  }
  vapply(x, one, integer(1), USE.NAMES = FALSE)
}

# Kind of structure of a layer, from structure_type (the first level) and
# structure_grade, in English or Portuguese: "prismatic" (prismatic, columnar,
# wedge-shaped), "blocky" (angular, subangular, nutty, polyhedral, lenticular),
# "fine" (granular, crumb, platy, flat-edged, pseudosand), "cloddy", "none"
# (massive, single grain, structureless, rock structure, stratified) or NA.
# The term written first decides ("massive breaking into blocky" is massive).
.wrb_structure_kind <- function(type, grade = NULL) {
  type <- tolower(trimws(as.character(type)))
  grade <- if (is.null(grade)) rep(NA_character_, length(type))
           else tolower(trimws(as.character(grade)))
  pats <- c(
    none      = "massive|maci|single|simples|structureless|sem estrutura|apedal|rock structure|estrutura de rocha|stratif|estratif",
    cloddy    = "clod|torr",
    prismatic = "prism|column|colun|wedge|cunei",
    blocky    = "block|bloco|nutty|nucif|polyhedr|poli[e\u00e9]dr|lentic|angular",
    fine      = "granul|grum|crumb|plat|lamin|flat-edged|pseudo")
  vapply(seq_along(type), function(i) {
    t <- type[i]
    if (!is.na(t) && nzchar(t)) {
      pos <- vapply(pats, function(p) regexpr(p, t, perl = TRUE)[1], integer(1))
      if (any(pos > 0L)) return(names(pats)[pos > 0L][which.min(pos[pos > 0L])])
    }
    g <- grade[i]
    if (!is.na(g) && grepl("^(structureless|sem estrutura|apedal)", g)) "none" else NA_character_
  }, character(1))
}

#' WRB 2022 criterion 1 of the mollic and umbric horizons (3.1.20, p. 52;
#' 3.1.39, p. 69), per layer: "soil aggregate structure with an average
#' aggregate size of <= 10 cm" or "cloddy structure or other structural
#' elements created by agricultural practices". The size classes of Annex 1,
#' Table 8.45 (p. 189): prismatic, columnar and wedge-shaped aggregates are
#' > 10 cm from the very coarse class, blocky, polyhedral and lenticular ones in
#' the extremely coarse class, and granular, platy and flat-edged ones are
#' <= 10 cm up to the very coarse class (the extremely coarse one, > 20 mm, is
#' open and stays undecided). TRUE / FALSE / NA; NA where the structure or the
#' size of an aggregate structure is not recorded.
#' @noRd
.wrb_aggregate_structure_le_10cm <- function(h) {
  n <- nrow(h)
  kind <- .wrb_structure_kind(h$structure_type %||% rep(NA_character_, n),
                              h$structure_grade %||% rep(NA_character_, n))
  size <- .wrb_structure_size_class(h$structure_size %||% rep(NA_character_, n))
  vapply(seq_len(n), function(i) {
    k <- kind[i]
    if (is.na(k)) return(NA)
    if (k == "none") return(FALSE)
    if (k == "cloddy") return(TRUE)
    s <- size[i]
    if (is.na(s)) return(NA)
    switch(k, prismatic = s <= 4L, blocky = s <= 5L,
           fine = if (s <= 5L) TRUE else NA)
  }, logical(1))
}

#' A criterion that holds "in >= x% (by volume)" of a horizon, read from its
#' layers weighted by thickness: TRUE when the layers that meet it make up the
#' share, FALSE when those that do not leave less than the share, NA otherwise.
#' @noRd
.wrb_share_by_volume <- function(h, layers, status, min_share) {
  w <- pmax(h$bottom_cm[layers] - h$top_cm[layers], 0)
  ok <- !is.na(w)
  w <- w[ok]; s <- status[ok]
  if (!length(w) || sum(w) <= 0) return(NA)
  yes <- sum(w[s %in% TRUE]) / sum(w)
  no  <- sum(w[s %in% FALSE]) / sum(w)
  if (yes >= min_share) TRUE else if (1 - no < min_share) FALSE else NA
}

#' Criterion 1 of the WRB 2022 mollic and umbric horizons over their layers
#' (see test_mollic_structure(system = "wrb2022")): >= 50% by volume of
#' aggregate structure <= 10 cm or cloddy structure. Passes with every
#' candidate layer, since the criterion holds for the horizon as a whole.
#' @noRd
.wrb_mollic_structure <- function(h, candidate_layers, min_share = 0.5) {
  cl <- .candidate_layers(h, candidate_layers)
  if (!length(cl)) return(.subtest_result(passed = FALSE))
  st <- .wrb_aggregate_structure_le_10cm(h)[cl]
  passed <- .wrb_share_by_volume(h, cl, st, min_share)
  .subtest_result(
    passed  = passed,
    layers  = if (isTRUE(passed)) cl else integer(0),
    missing = if (is.na(passed)) c("structure_type", "structure_size") else character(0),
    details = data.frame(
      layer = cl,
      structure_type = as.character((h$structure_type %||% rep(NA, nrow(h)))[cl]),
      structure_size = as.character((h$structure_size %||% rep(NA, nrow(h)))[cl]),
      aggregate_le_10cm = st, stringsAsFactors = FALSE),
    notes   = paste("WRB 2022 Ch 3.1.20 / 3.1.39 criterion 1: aggregate",
                    "structure <= 10 cm or cloddy, >= 50% by volume"))
}
