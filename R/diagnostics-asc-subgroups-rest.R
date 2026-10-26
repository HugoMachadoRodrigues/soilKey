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
#' least some part), "none" (no part). \code{part}:
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
                          how = c("major", "some", "none")) {
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
      w <- .asc_upper_window(x, 100)
      rows <- which(live & !x$rock & !is.na(h$bottom_cm) & h$top_cm < w[2L] & h$bottom_cm > w[1L])
      if (how == "major") .asc_upper_major(x, ok, 100)
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
      else if (how == "major") major_of(rows, maybe)
      else some_of(rows, maybe)
    },
    solum = {
      sets <- .asc_solum_sets(x)
      if (is.null(sets) || anyNA(sets)) NA
      else .asc_agree(vapply(sets, function(r) {
        if (!length(r)) return(FALSE)
        if (how == "major") {
          if (anyNA(h$bottom_cm[r])) return(NA)
          .asc_major_part(x, r, ok, min(h$top_cm[r]), max(h$bottom_cm[r]))
        } else some_of(r)
      }, NA))
    },
    soil = {
      rows <- which(live & !x$rock)
      if (!length(rows)) NA
      else if (how == "major") {
        if (anyNA(h$bottom_cm[rows])) NA
        else .asc_major_part(x, rows, ok, .asc_profile_top(x) %|NA|% min(h$top_cm[rows]), max(h$bottom_cm[rows]))
      } else some_of(rows)
    })
  if (how == "none") passed <- if (is.na(passed)) NA else !passed
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
