# =============================================================================
# v0.9.289 -- subgroups of the Australian Soil Classification (3rd ed.) for
# the Hydrosols, Arenosols, Rudosols and Tenosols: acidity and carbonate over
# a part of the profile (the upper 1.0 m, the B, the A2, the solum, the
# soil), sulfidic materials near the surface, fusic surface layers.
# Three-valued; the readings are in inst/rules/asc3/readings.yaml.
# =============================================================================

#' Acidity and carbonate over a part of the profile (subgroups)
#'
#' \code{prop}: "acid" (strongly acid), "not_acid" (not strongly acid),
#' "calcareous". \code{how}: "major" (the major part, by depth), "some" (at
#' least some part), "none" (no part), "not_major" (not the major part). \code{part}:
#' \itemize{
#'   \item "upper100": the upper 1.0 m of the soil profile (Arenosols),
#'     cut at the top of a rock or hard layer the description ends on;
#'   \item "b": the B horizons (Chernic Tenosols: "the B horizon (if
#'     present)"; with none, the condition fails);
#'   \item "b_bc_cb": the B, BC or C/B horizons, likewise;
#'   \item "a2": the A2 horizons (with none, the condition fails);
#'   \item "solum": the solum as described (\code{.asc_solum_sets});
#'   \item "soil": the soil above rock as described (Rudosols, "the major
#'     part").
#' }
#' Rows of unknown role that may belong to the part are tried in and out; a
#' part not described within the window is undecided.
#' @noRd
part_prop_asc <- function(pedon, prop = c("acid", "not_acid", "calcareous"),
                          part = c("upper100", "b", "b_bc_cb", "a2", "solum", "soil"),
                          how = c("major", "some", "none", "not_major")) {
  prop <- match.arg(prop); part <- match.arg(part); how <- match.arg(how)
  x <- .asc_horizons(pedon); h <- x$h; n <- x$n
  acid <- .asc_strongly_acid(h)
  ok <- switch(prop, acid = acid, not_acid = !acid, calcareous = .asc_calcareous(x))
  live <- !x$des$buried & !is.na(h$top_cm)
  # some / none over a set of rows, with rows that may belong to it
  some_of <- function(rows, maybe = integer(0), described = TRUE) {
    if (any(ok[rows] %in% TRUE)) TRUE
    else if (anyNA(ok[rows]) || any(!(ok[maybe] %in% FALSE)) || !described) NA else FALSE
  }
  major_of <- function(rows, maybe = integer(0)) {
    if (!length(rows) && !length(maybe)) return(FALSE)
    if (!length(rows)) return(if (all(ok[maybe] %in% FALSE)) FALSE else NA)
    .asc_rows_major_open(x, rows, maybe, ok)
  }
  passed <- if (!x$any_known && part %in% c("b", "b_bc_cb", "a2", "solum")) NA else switch(part,
    upper100 = {
      # the soil profile: buried horizons count, as for the major part
      w <- .asc_upper_window(x, 100)
      rows <- which(!is.na(h$top_cm) & !x$rock & h$top_cm < w[2L] & (is.na(h$bottom_cm) | h$bottom_cm > w[1L]))
      if (how %in% c("major", "not_major")) .asc_upper_major(x, ok, 100)
      else some_of(rows, described = .asc_described(x, w[1L], w[2L]) || .asc_ends_hard(x))
    },
    b = , b_bc_cb = , a2 = {
      bm <- .asc_b_and_maybe(x)
      if (part == "a2") {
        rows <- which(x$role %in% "A2" & live)
        b_top <- suppressWarnings(min(h$top_cm[grepl("^B", x$role %||% "") & live], na.rm = TRUE))
        maybe <- which(x$maybe_soil & !x$trailing & !is.na(h$top_cm) & h$top_cm < b_top)
        sw <- .asc_sandwiched(x, maybe, rows)
        rows <- sort(c(rows, sw)); maybe <- setdiff(maybe, sw)
        # a row of unknown role among A horizons may be an A2 only where
        # no A2 is designated
        if (length(rows)) maybe <- integer(0)
      } else {
        rows <- bm$b; maybe <- bm$maybe
        if (part == "b_bc_cb") {
          cb <- which(x$role %in% c("C/B", "CB") & live)
          rows <- sort(unique(c(rows, cb)))
        }
      }
      if (!length(rows) && !length(maybe)) FALSE
      else if (how %in% c("major", "not_major")) major_of(rows, maybe)
      else some_of(rows, maybe)
    },
    solum = {
      sets <- .asc_solum_sets(x)
      if (is.null(sets) || anyNA(sets)) NA
      else {
        bv <- .asc_bottom_variants(h)
        if (is.null(bv)) NA
        else .asc_agree(unlist(lapply(sets, function(r) vapply(bv, function(b) {
          if (!length(r)) return(FALSE)
          x2 <- x; x2$h$bottom_cm <- b
          if (how %in% c("major", "not_major")) .asc_major_part(x2, r, ok, min(h$top_cm[r]), max(b[r]))
          else some_of(r, described = .asc_described(x2, min(h$top_cm[r]), max(b[r])))
        }, NA))))
      }
    },
    soil = {
      # the soil above rock, a hard layer or a pan (substrate) and above any
      # buried soil
      hard <- which(live & (x$rock | x$hard | .asc_pan_layer(x)))
      bur <- which(x$des$buried & !is.na(h$top_cm))
      stops <- c(h$top_cm[hard], h$top_cm[bur])
      stop_at <- if (length(stops)) min(stops) else Inf
      rows <- which(live & !x$rock & !x$hard & !.asc_pan_layer(x) & h$top_cm < stop_at - 0.5)
      bv <- .asc_bottom_variants(h)
      if (!length(rows)) NA
      else if (how %in% c("major", "not_major")) {
        if (is.null(bv)) NA
        else .asc_agree(vapply(bv, function(b) {
          x2 <- x; x2$h$bottom_cm <- b
          .asc_major_part(x2, rows, ok, .asc_profile_top(x) %|NA|% min(h$top_cm[rows]),
                          min(max(b[rows]), stop_at))
        }, NA))
      } else some_of(rows)
    })
  if (how %in% c("none", "not_major")) passed <- if (is.na(passed)) NA else !passed
  .asc_result(paste("sg", part, prop, how, sep = "_"), passed, integer(0), list(prop = prop, part = part, how = how),
              if (is.na(passed)) c("ph_h2o", "ph_cacl2", "effervescence_class", "caco3_pct", "designation")
              else character(0),
              "Subgroups Acidic, Basic, Calcareous (Hydrosols, Arenosols, Rudosols, Tenosols)")
}

#' Episulfidic subgroups (Salic Hydrosols): sulfidic materials in the A or
#' near-surface horizons, read as an A horizon or a horizon starting within
#' the upper 0.5 m; sulfidic as for the acid sulfate great groups (0.01 \%
#' sulfidic S or more, or hypersulfidic material; a test not done is
#' undecided).
#' @noRd
episulfidic_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  top0 <- .asc_profile_top(x) %|NA|% 0
  s_pct <- suppressWarnings(as.numeric(h$sulfidic_s_pct %||% rep(NA_real_, x$n)))
  hyper <- .asc_hypersulfidic_rows(x)
  sul <- ifelse(!is.na(s_pct), s_pct >= 0.01, ifelse(hyper %in% TRUE, TRUE, .asc_untested_material()))
  near <- !is.na(h$top_cm) & !x$rock & !x$des$buried &
          (grepl("^A", x$role %||% "") | h$top_cm < top0 + 50)
  passed <- .asc_any_rows(sul[near])
  .asc_result("sg_episulfidic", passed, which(near & sul %in% TRUE), list(),
              if (is.na(passed)) c("sulfidic_s_pct", "incubation_ph") else character(0),
              "Hydrosols, subgroups Episulfidic; glossary 'Sulfidic materials'")
}

#' Fusic subgroup (Hydrosols): surface layers that comprise more than 20 %
#' fusic material (burnt organic material), read from the layer origin of
#' the layers at the top of the profile (within 0.3 m); fusic material
#' shows, so none named is none.
#' @noRd
fusic_surface_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  top0 <- .asc_profile_top(x) %|NA|% 0
  txt <- tolower(paste(h$layer_origin %||% rep(NA, x$n), h$rock_origin %||% rep(NA, x$n)))
  fus <- grepl("fusic|burnt peat|burnt organic|burned peat|burned organic", txt)
  surf <- !is.na(h$top_cm) & h$top_cm < top0 + 30
  passed <- any(fus & surf)
  .asc_result("sg_fusic", passed, which(fus & surf), list(), character(0),
              "Hydrosols, subgroups Fusic; glossary 'Fusic material'")
}

#' Calcenic Tenosol subgroups: the calcareous horizon (the horizons of the
#' Calcenic material, \code{.asc_calcenic_rows}) with more than 50 \% hard
#' calcrete fragments, carbonate nodules or concretions ("lithocalcic"),
#' 20-50 \% ("supracalcic"), or less than 20 \% ("hypercalcic"); a horizon
#' that may belong to it may decide it.
#' @noRd
calcenic_sg_asc <- function(pedon, class = c("lithocalcic", "supracalcic", "hypercalcic")) {
  class <- match.arg(class)
  x <- .asc_horizons(pedon); h <- x$h
  hard <- .asc_carbonate(h)$hard
  ok <- .asc_calcenic_rows(x)
  # a pan (a calcrete under the soil) is substrate, not the calcareous horizon
  ok[.asc_pan_layer(x) | (x$hard & !x$rock) | x$rock] <- FALSE
  sure <- which(ok %in% TRUE); may <- which(is.na(ok))
  test <- switch(class,
    lithocalcic = ifelse(is.na(hard), NA, hard > 50),
    supracalcic = ifelse(is.na(hard), NA, hard >= 20 & hard <= 50),
    hypercalcic = ifelse(is.na(hard), NA, hard < 20))
  passed <- if (class == "hypercalcic") {
    if (any(test[sure] %in% FALSE) ) FALSE
    else if (length(sure) && all(test[sure] %in% TRUE) && all(test[may] %in% TRUE)) TRUE else NA
  } else {
    if (any(test[sure] %in% TRUE)) TRUE
    else if (anyNA(test[sure]) || any(!(test[may] %in% FALSE)) || !length(sure)) NA else FALSE
  }
  .asc_result(paste0("sg_calcenic_", class), passed, sure, list(),
              if (is.na(passed)) c("carbonate_hard_pct", "caco3_pct", "carbonate_soft_pct") else character(0),
              "Tenosols, subgroups of the Calcenic suborder; glossary 'Calcareous horizon'")
}
