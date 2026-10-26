# =============================================================================
# v0.9.285 -- great groups of the Australian Soil Classification (3rd ed.)
# for the Hydrosols, Arenosols, Rudosols and Tenosols, and the machinery the
# great groups share: acid sulfate materials (with the Organosols), what the
# soil directly overlies (with the Calcarosols), and the upper part of the
# profile. Three-valued; the readings are in inst/rules/asc3/readings.yaml.
# =============================================================================

# ---- the upper part of the profile ------------------------------------------------

# The window from the top of the profile (an O1 litter left out) down
# `depth` cm, cut at the top of a rock or hard layer the description ends on.
.asc_upper_window <- function(x, depth) {
  h <- x$h
  top0 <- .asc_profile_top(x) %|NA|% 0
  zb <- top0 + depth
  if (.asc_ends_hard(x)) {
    last <- which(!is.na(h$top_cm)); zb <- min(zb, max(h$top_cm[last]))
  }
  c(top0, zb)
}

# Whether the rows describe all of [zt, zb] (overlaps counted once; a missing
# bottom, or a gap, leaves it not all described).
.asc_described <- function(x, zt, zb) {
  h <- x$h
  win <- which(!is.na(h$top_cm) & h$top_cm < zb & (is.na(h$bottom_cm) | h$bottom_cm > zt))
  cover <- 0; end <- zt
  for (i in win[order(h$top_cm[win])]) {
    if (is.na(h$bottom_cm[i])) return(FALSE)
    a <- max(h$top_cm[i], end); b <- min(h$bottom_cm[i], zb)
    if (b > a) { cover <- cover + b - a; end <- b }
  }
  cover >= zb - zt - 0.5
}

# The major part, by depth, of the upper `depth` cm of the profile passes
# `ok` (rock left out; a part not described is undecided).
.asc_upper_major <- function(x, ok, depth) {
  w <- .asc_upper_window(x, depth)
  if (w[2L] <= w[1L]) return(NA)
  rows <- which(!x$rock)
  .asc_major_part(x, rows, ok, w[1L], w[2L])
}

# ---- acid sulfate materials --------------------------------------------------------

# The acid sulfate great groups shared by the Organosols, Hydrosols and the
# Hypersalic Arenosols and Rudosols: "sulfuric" (a run of sulfuric material at
# least 15 cm thick starting within the upper 1.5 m), "hypersulfidic" and
# "hyposulfidic" (that material within 1.5 m, where the incubation test shows
# it: replacements used only with that evidence), "histic_hypersulfidic" and
# "histic_hyposulfidic" (with organic materials dominating the upper 0.5 m),
# the "monosulfidic" ones (not used: no monosulfidic test is recorded), and
# "sulfidic" (0.01 % sulfidic S or hypersulfidic material within 1.5 m; a
# test not done is undecided). The upper 1.5 m includes buried horizons, R
# horizons left out; a part of it not described (the description stops above
# it, a depth is missing, or the record has a gap; but not below a rock or
# hard layer the description ends on) may hold sulfuric or sulfidic material.
.asc_acid_sulfate <- function(x, pedon, kind) {
  h <- x$h
  top0 <- .asc_profile_top(x) %|NA|% 0
  # rock is not soil material: R horizons are not tested
  in150 <- !is.na(h$top_cm) & h$top_cm < top0 + 150 & !x$rock
  unseen <- !.asc_described(x, top0, .asc_upper_window(x, 150)[2L])
  open_below <- function(v) if (identical(v, FALSE) && unseen) NA else v
  s_pct <- suppressWarnings(as.numeric(h$sulfidic_s_pct %||% rep(NA_real_, x$n)))
  inc <- suppressWarnings(as.numeric(h$incubation_ph %||% rep(NA_real_, x$n)))
  ph <- suppressWarnings(as.numeric(h$ph_h2o %||% rep(NA_real_, x$n)))
  hyper <- .asc_hypersulfidic_rows(x)
  # hyposulfidic: sulfidic (S 0.01 % or more), a field pH of 4 or more, and
  # no substantial drop (0.5 unit or more) to below 4 on incubation
  hypo <- ifelse(is.na(inc), FALSE,
          ifelse(!is.na(s_pct) & s_pct < 0.01, FALSE,
          ifelse(!is.na(ph) & ph < 4, FALSE,
          ifelse(!is.na(ph) & inc < 4 & round(ph - inc, 6) >= 0.5, FALSE,
          ifelse(is.na(ph) | is.na(s_pct), NA, TRUE)))))
  # the replacements need the incubation: untested rows are not evidence
  hyper_gg <- function() .asc_any_rows(ifelse(is.na(inc), FALSE, hyper)[in150])
  hypo_gg <- function() .asc_any_rows(hypo[in150])
  histic <- function() .asc_upper_major(x, .asc_organic_material(x, pedon), 50)
  switch(kind,
    monosulfidic_sulfuric = , monohypersulfidic = , monohyposulfidic = FALSE,
    sulfuric = {
      # a run starting within the upper 1.5 m may go on below it
      x2 <- x; x2$top_ms <- h$top_cm - top0
      v <- .asc_sulfuric_rows(x); v[x$rock] <- FALSE
      open_below(.cssc_run_test(x2, v, 15, max_top_cm = 150 - 1e-6)$passed)
    },
    hypersulfidic = hyper_gg(),
    hyposulfidic = hypo_gg(),
    histic_hypersulfidic = .and3(histic(), hyper_gg()),
    histic_hyposulfidic = .and3(histic(), hypo_gg()),
    sulfidic = {
      sul <- ifelse(!is.na(s_pct), s_pct >= 0.01, ifelse(hyper %in% TRUE, TRUE, .asc_untested_material()))
      open_below(.asc_any_rows(sul[in150]))
    })
}

#' Acid sulfate great groups (Hydrosols, Hypersalic Arenosols and Rudosols)
#'
#' \code{kind}: "sulfuric", "monosulfidic_sulfuric", "sulfidic",
#' "monohypersulfidic", "histic_hypersulfidic", "hypersulfidic",
#' "monohyposulfidic", "histic_hyposulfidic", "hyposulfidic"; read as for the
#' Organosols (\code{.asc_acid_sulfate}).
#' @noRd
acid_sulfate_gg_asc <- function(pedon, kind = c("sulfuric", "monosulfidic_sulfuric", "sulfidic", "monohypersulfidic",
                                                "histic_hypersulfidic", "hypersulfidic", "monohyposulfidic",
                                                "histic_hyposulfidic", "hyposulfidic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon)
  passed <- .asc_acid_sulfate(x, pedon, kind)
  .asc_result(paste0("gg_", kind), passed, integer(0), list(kind = kind),
              if (is.na(passed)) c("sulfidic_s_pct", "incubation_ph", "ph_h2o", "jarosite_present", "oc_pct")
              else character(0),
              "Great groups of the acid sulfate soils; glossary 'Sulfidic materials', 'Sulfuric material'")
}

# ---- what the soil directly overlies -----------------------------------------------

# Whether the soil directly overlies `kind`: the row(s) whose top is the bottom
# of the deepest horizon of the solum that is not a pan (`from = "b"`: of the
# deepest B horizon), the rows of unknown role that may belong to it tried
# out and in (the answer holds only when both agree). Kinds by the substrate
# (`.asc_substrate_kind`): "rock", "saprolite", "marl", "unconsolidated"; by
# the pan (`.asc_pan_rows`, a pan designated within the solum counting too, and
# a pan first below a gap in the record leaving it undecided): "duric",
# "petrocalcic", "petroferric", "silpanic"; by the record (absent unless the
# record names it): "reticulate", "coquina". `below_cm` (Arenosols): a hard
# layer (rock, saprolite, a pan) counts only from that depth down.
.asc_overlie <- function(x, kind, from = c("solum", "b"), below_cm = 0) {
  from <- match.arg(from)
  h <- x$h
  if (!x$any_known) return(NA)
  top0 <- .asc_profile_top(x) %|NA|% 0
  lam <- x$role %in% "lamellae" & !x$des$buried & !is.na(h$top_cm) & h$top_cm < x$c_top
  base <- if (from == "solum") which(x$solum %in% TRUE | lam) else union(.asc_b_rows(x), which(lam))
  maybe <- which(x$maybe_soil)
  hard_kind <- kind %in% c("rock", "saprolite", "duric", "petrocalcic", "petroferric", "silpanic")
  deep_ok <- function(i) below_cm <= 0 || (!is.na(h$top_cm[i]) && h$top_cm[i] >= top0 + below_cm - 0.5)
  origin <- tolower(paste(h$layer_origin %||% rep(NA, x$n), h$rock_origin %||% rep(NA, x$n),
                          h$pan_type %||% rep(NA, x$n)))
  cem <- tolower(as.character(h$cementation_class %||% rep(NA_character_, x$n)))
  named <- switch(kind,
    reticulate = grepl("reticul", origin) & !grepl("petroreticul", origin),
    coquina = grepl("coquin", origin) |
              (grepl("shell", origin) & (x$des$m | cem %in% c("weakly", "moderately", "strongly", "indurated"))),
    NULL)
  pan_kinds <- switch(kind, duric = "red-brown hardpan", petrocalcic = "calcrete",
                      petroferric = c("ferricrete", "alcrete", "petroferric", "petroreticulate"),
                      silpanic = c("silcrete", "siliceous"), NULL)
  pan_v <- if (!is.null(pan_kinds)) .asc_pan_rows(x, pan_kinds) else NULL
  one <- function(sol) {
    if (from == "b" && !length(sol)) return(FALSE)                   # no B horizon
    u <- .asc_under_solum(x, sol)
    if (!is.null(pan_v) || !is.null(named)) {
      # pans, reticulate horizons and coquina show: none recorded, none
      # the pans of the solum, and its horizons that may be one (a pan the
      # origin names on an uncemented horizon, a cemented one of unknown kind)
      cand <- c(u$rows, if (!is.null(pan_v)) which(x$solum %in% TRUE & (.asc_pan_layer(x) | is.na(pan_v))))
      v <- if (!is.null(pan_v)) pan_v else named
      v[!vapply(seq_len(x$n), deep_ok, NA)] <- FALSE
      if (length(cand) && any(v[cand] %in% TRUE)) return(TRUE)
      return(if (anyNA(v[cand]) || any(!(v[u$after_gap] %in% FALSE))) NA else FALSE)
    }
    if (!length(u$rows)) return(NA)
    sk <- vapply(u$rows, function(i) .asc_substrate_kind(x, i), "")
    # a C or D of unrecorded origin, or a pan of unrecorded kind, is not hard
    # rock (a row of unknown role may be); sandy material is not marl
    if (kind == "rock") sk[is.na(sk) & !is.na(x$role[u$rows]) & !x$unknown_role[u$rows]] <- "other"
    # a cemented layer is not unconsolidated, nor marl, but may be hard rock
    # or a cemented saprolite
    sk[sk %in% "hard"] <- if (kind %in% c("unconsolidated", "marl")) "other" else NA
    if (kind == "marl") sk[is.na(sk) & .asc_is_sandy(h)[u$rows] %in% TRUE] <- "other"
    # a hard layer above `below_cm` does not count
    if (hard_kind) sk[!vapply(u$rows, deep_ok, NA)] <- "other"
    if (any(sk %in% kind)) TRUE else if (anyNA(sk)) NA else FALSE
  }
  a <- one(base); b <- one(union(base, maybe))
  if (identical(a, b)) a else NA
}

#' What the soil directly overlies (Arenosols, Rudosols, Tenosols)
#'
#' \code{kind}: "lithic" (hard rock), "paralithic" (partially weathered or
#' decomposed rock or saprolite), "marly" (marl), "regolithic" (other
#' unconsolidated mineral materials), "duric" (a red-brown hardpan),
#' "petrocalcic" (a calcrete pan), "petroferric" (ferricrete, a petroferric
#' or a petroreticulate horizon), "silpanic" (a siliceous pan), "reticulate"
#' (a reticulate horizon), "coquinic" (a coquina substrate). "Overlie" in the
#' Arenosol and Tenosol lists is read as "directly overlie". \code{from =
#' "b"}: the B horizon directly overlies it (Regolithic Tenosols).
#' \code{below_cm}: hard layers only from that depth (100 for the Arenosols).
#' @noRd
substrate_gg_asc <- function(pedon, kind = c("lithic", "paralithic", "marly", "regolithic", "duric", "petrocalcic",
                                             "petroferric", "silpanic", "reticulate", "coquinic"),
                             from = c("solum", "b"), below_cm = 0) {
  kind <- match.arg(kind); from <- match.arg(from)
  x <- .asc_horizons(pedon)
  k <- switch(kind, lithic = "rock", paralithic = "saprolite", marly = "marl", regolithic = "unconsolidated",
              coquinic = "coquina", kind)
  passed <- .asc_overlie(x, k, from, below_cm)
  .asc_result(paste0("gg_overlie_", kind), passed, integer(0), list(from = from, below_cm = below_cm),
              if (is.na(passed)) c("designation", "layer_origin", "rock_origin", "pan_type", "cementation_class")
              else character(0),
              "Great groups on what the soil directly overlies; glossary 'Unconsolidated mineral materials', 'Marl', 'Siliceous pan', 'Reticulate horizon', 'Coquina'")
}

# ---- horizons and materials ---------------------------------------------------------

# A visual amount above `cut`: not recorded (NA) is absent, as decided for
# the nodules, gypsum and halite (they show when present); NaN, an amount the
# recorded classes leave on either side of the limit, is undecided.
.asc_recorded_over <- function(v, cut) ifelse(is.nan(v), NA, ifelse(is.na(v), FALSE, v > cut))

#' Ferric, bauxitic and manganic horizons
#'
#' A run of adjacent horizons at least 0.1 m thick with more than 20 %
#' ferruginous (ferric_nodules_pct), bauxite (bauxitic_nodules_pct) or
#' manganiferous (manganiferous_nodules_pct) nodules or concretions, not in a
#' cemented layer and not buried. \code{where}: "profile" (anywhere),
#' "solum" (within the solum; rows of unknown role that may belong to it are
#' open), "a2" (the A2 horizon contains or directly overlies it). Nodules
#' not recorded are absent (more than 20 \% of them shows), as for the
#' Sesqui-Nodular Tenosols; an amount the record leaves on either side of
#' 20 \% (NaN) is undecided.
#' @noRd
nodular_horizon_asc <- function(pedon, kind = c("ferric", "bauxitic", "manganic"),
                                where = c("profile", "solum", "a2")) {
  kind <- match.arg(kind); where <- match.arg(where)
  x <- .asc_horizons(pedon); h <- x$h
  v <- switch(kind, ferric = h$ferric_nodules_pct, bauxitic = h$bauxitic_nodules_pct,
              manganic = h$manganiferous_nodules_pct) %||% rep(NA_real_, x$n)
  ok <- .asc_recorded_over(v, 20)
  ok[x$hard | x$des$buried | !x$mineral] <- FALSE
  x2 <- x; max_top <- NULL
  if (where == "solum") {
    sure <- if (x$any_known) x$solum %in% TRUE else x$mineral & !x$des$buried
    may <- if (x$any_known) x$maybe_soil & !sure else rep(FALSE, x$n)
    # a row of unknown role between two solum horizons is solum
    sw <- .asc_sandwiched(x, which(may), which(sure))
    sure[sw] <- TRUE; may[sw] <- FALSE
    ok[!(sure | may)] <- FALSE
    ok[may & ok %in% TRUE] <- NA
  } else if (where == "a2") {
    a2 <- which(x$role %in% "A2" & !x$des$buried)
    if (!length(a2)) {
      # a row of unknown role above the first B horizon may be the A2
      b_top <- suppressWarnings(min(h$top_cm[grepl("^B", x$role) & !x$des$buried], na.rm = TRUE))
      may_a2 <- x$maybe_soil & !x$trailing & !is.na(h$top_cm) & h$top_cm < b_top
      passed <- if (x$any_known && !any(may_a2)) FALSE else NA
      return(.asc_result(paste0("gg_", kind, "_horizon"), passed, integer(0), list(where = where),
                         if (is.na(passed)) "designation" else character(0), "Glossary 'Ferric horizon'"))
    }
    a2_top <- min(h$top_cm[a2], na.rm = TRUE); a2_bot <- max(h$bottom_cm[a2], na.rm = TRUE)
    ok[is.na(h$top_cm) | h$top_cm < a2_top - 0.5] <- FALSE
    # the run starts within the A2 or at its lower boundary
    x2$top_ms <- h$top_cm - a2_top; max_top <- a2_bot - a2_top + 0.5
  }
  passed <- if (is.null(max_top)) .cssc_run_test(x, ok, 10)$passed
            else .cssc_run_test(x2, ok, 10, max_top_cm = max_top)$passed
  .asc_result(paste0("gg_", kind, "_horizon"), passed, which(ok %in% TRUE), list(where = where),
              if (is.na(passed)) c("ferric_nodules_pct", "bauxitic_nodules_pct", "manganiferous_nodules_pct")
              else character(0),
              sprintf("Glossary '%s horizon'", tools::toTitleCase(kind)))
}

#' Gypsic horizon
#'
#' More than 20 % visible gypsum (gypsum_visible_pct) over at least 0.1 m,
#' its upper boundary within \code{within} cm of the top of the profile (the
#' glossary disregards one below 1.0 m). Without visible gypsum recorded, lab
#' gypsum (caso4_pct) of 20 % or less rules a horizon out (the visible
#' gypsum is part of it) and more leaves it undecided; with neither, the
#' horizon has none, as for the Hypergypsic suborders. \code{where = "b"}
#' (subgroups): within the B or BC horizon.
#' @noRd
gypsic_horizon_asc <- function(pedon, within = 100, where = c("profile", "b")) {
  where <- match.arg(where)
  x <- .asc_horizons(pedon); h <- x$h
  top0 <- .asc_profile_top(x) %|NA|% 0
  vis <- h$gypsum_visible_pct %||% rep(NA_real_, x$n); lab <- h$caso4_pct %||% rep(NA_real_, x$n)
  ok <- ifelse(is.nan(vis), NA, ifelse(!is.na(vis), vis > 20,
               ifelse(!is.na(lab), ifelse(lab <= 20, FALSE, NA), .asc_unrecorded_site())))
  ok[x$des$buried | !x$mineral | x$rock] <- FALSE
  if (where == "b") {
    # B horizons (BC included); a row of unknown role below the A horizons
    # may be one, and one between two B horizons is one
    bm <- .asc_b_and_maybe(x)
    b <- seq_len(x$n) %in% c(bm$b, which(!is.na(x$role) & grepl("^B", x$role) & !x$des$buried))
    may <- seq_len(x$n) %in% bm$maybe
    ok[!(b | may)] <- FALSE
    ok[may & ok %in% TRUE] <- NA
  }
  x2 <- x; x2$top_ms <- h$top_cm - top0
  passed <- .cssc_run_test(x2, ok, 10, max_top_cm = min(within, 100) - 1e-6)$passed
  .asc_result("gg_gypsic", passed, which(ok %in% TRUE), list(within_cm = within),
              if (is.na(passed)) c("gypsum_visible_pct", "caso4_pct") else character(0),
              "Glossary 'Gypsic horizon'")
}

#' Argic horizon (clay lamellae)
#'
#' Read from the designation (lamellae are conspicuous: none recorded, none).
#' \code{where}: "profile" (anywhere, not buried), "solum" (in a horizon of
#' the solum, or a lamellae horizon above the first C, D or R), "b" (a B
#' designation naming lamellae, or such a lamellae horizon).
#' @noRd
argic_horizon_asc <- function(pedon, where = c("profile", "solum", "b")) {
  where <- match.arg(where)
  x <- .asc_horizons(pedon); h <- x$h
  live <- x$des$lamellae & !x$des$buried
  lam <- x$role %in% "lamellae" & !x$des$buried & !is.na(h$top_cm) & h$top_cm < x$c_top
  hit <- switch(where, profile = live, solum = live & (x$solum %in% TRUE | lam),
                b = (live & grepl("^B", x$role)) | lam)
  passed <- if (!x$any_known && !any(hit)) NA else any(hit)
  .asc_result("gg_argic", passed, which(hit), list(where = where),
              if (is.na(passed)) "designation" else character(0), "Glossary 'Argic horizon'")
}

#' Inceptic: a transitional C/B horizon in fissures of the parent rock or
#' saprolite (10 to 50 % of B horizon material, as the C/B notation says).
#' \code{below_cm} (Arenosols, whose hard layers count only below 1.0 m):
#' the C/B from that depth down.
#' @noRd
inceptic_asc <- function(pedon, below_cm = 0) {
  x <- .asc_horizons(pedon)
  top0 <- .asc_profile_top(x) %|NA|% 0
  hit <- x$role %in% c("C/B", "CB") & !x$des$buried &
         (below_cm <= 0 | (!is.na(x$h$top_cm) & x$h$top_cm >= top0 + below_cm - 0.5))
  passed <- if (!x$any_known) NA else any(hit)
  .asc_result("gg_inceptic", passed, which(hit), list(),
              if (is.na(passed)) "designation" else character(0), "Great groups Inceptic")
}

#' Tephric: formed in tephric materials (a layer or rock origin naming tephra,
#' volcanic ash, pyroclastics, scoria, cinders, pumice or lapilli); the parent
#' material shows in the field, so with none named the soil is not Tephric.
#' \code{gravel = TRUE} (Clastic Rudosols): the gravel consists dominantly of
#' them (the major part of the gravelly layers).
#' @noRd
tephric_asc <- function(pedon, gravel = FALSE) {
  x <- .asc_horizons(pedon); h <- x$h
  origin <- tolower(paste(h$layer_origin %||% rep(NA, x$n), h$rock_origin %||% rep(NA, x$n)))
  teph <- grepl("tephr|volcanic ash|pyroclast|scoria|cinder|pumice|lapilli", origin) & !x$des$buried
  passed <- if (!gravel) any(teph) else {
    cf <- ifelse(is.na(h$coarse_fragments_pct), 0, h$coarse_fragments_pct)
    grav <- which(cf > 10 & x$mineral & !x$des$buried)
    if (!length(grav)) FALSE else .asc_rows_major(x, grav, teph)
  }
  .asc_result("gg_tephric", passed, which(teph), list(gravel = gravel),
              if (is.na(passed)) c("layer_origin", "coarse_fragments_pct") else character(0),
              "Great groups Tephric; glossary 'Tephric materials'")
}

#' Gritty: the major part of the upper 1.0 m with a distinct gritty feel
#' (horizon attribute gritty; not recorded is absent, as decided).
#' @noRd
gritty_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  g <- as.logical(h$gritty %||% rep(NA, x$n))
  ok <- g %in% TRUE
  passed <- if (!any(ok)) FALSE else .asc_upper_major(x, ok, 100)
  .asc_result("gg_gritty", passed, which(ok), list(),
              if (is.na(passed)) "gritty" else character(0), "Great groups Gritty; glossary 'Gritty'")
}

#' Gravic: the major part of the upper 1.5 m with more than 3 % visible heavy
#' or opaque minerals (heavy_minerals_pct; not recorded is absent, as decided).
#' @noRd
gravic_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  hm <- h$heavy_minerals_pct %||% rep(NA_real_, x$n)
  ok <- !is.na(hm) & hm > 3
  passed <- if (!any(ok)) FALSE else .asc_upper_major(x, ok, 150)
  .asc_result("gg_gravic", passed, which(ok), list(),
              if (is.na(passed)) "heavy_minerals_pct" else character(0),
              "Arenosols, great group Gravic; glossary 'Wet panning'")
}

#' Halic and Subhalic: the major part of the upper 0.5 m of materials
#' dominated (more than 50 %) by halite crystals, or with less than 50 %
#' (halite_visible_pct; halite crystals not recorded are absent, as for the
#' gypsum soils).
#' @noRd
halite_asc <- function(pedon, kind = c("halic", "subhalic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h
  ha <- h$halite_visible_pct %||% rep(NA_real_, x$n)
  ok <- if (kind == "halic") .asc_recorded_over(ha, 50)
        else ifelse(is.nan(ha), NA, ifelse(is.na(ha), TRUE, ha < 50))
  passed <- .asc_upper_major(x, ok, 50)
  .asc_result(paste0("gg_", kind), passed, which(ok), list(),
              if (is.na(passed)) "halite_visible_pct" else character(0),
              sprintf("Great group %s", tools::toTitleCase(kind)))
}

#' Kandic, Tenic and Palic Arenosols: the texture of the layers other than
#' the surface horizon. "kandic": one beginning within the upper 1.0 m with
#' a light sandy loam texture (about 10-15 % clay); "tenic": one with a
#' clayey sand or loamy sand texture (about 5-10 % clay); "palic": sand
#' (less than 5 % clay) throughout at least the upper 1.0 m. The field
#' texture decides, lab clay where it is not recorded.
#' @noRd
sand_texture_gg_asc <- function(pedon, kind = c("kandic", "tenic", "palic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h
  top0 <- .asc_profile_top(x) %|NA|% 0
  # the class of each row: S sand; LS, CS tenic; SL (not SL+) kandic; lab
  # clay where no field texture is recorded
  tx <- .asc_texture(h$field_texture %||% rep(NA_character_, x$n))
  cl <- h$clay_pct
  by_tx <- ifelse(is.na(tx$grade), NA,
                  ifelse(tx$grade == "S", "sand", ifelse(tx$grade %in% c("LS", "CS"), "tenic",
                         ifelse(tx$grade == "SL" & tx$qualifier != "+", "kandic", "other"))))
  by_lab <- ifelse(is.na(cl), NA, ifelse(cl < 5, "sand", ifelse(cl < 10, "tenic", ifelse(cl <= 15, "kandic", "other"))))
  cls <- ifelse(!is.na(by_tx), by_tx, by_lab)
  ord <- order(h$top_cm, na.last = NA)
  ord <- ord[!x$rock[ord] & !x$des$buried[ord] & x$mineral[ord] & h$top_cm[ord] >= top0 - 0.5]
  surf <- if (length(ord)) ord[1L] else integer(0)
  rest <- setdiff(ord, surf)
  within <- rest[h$top_cm[rest] < top0 + 100]
  passed <- switch(kind,
    kandic = , tenic = {
      v <- .asc_any_rows(cls[within] == kind)
      # a part of the upper 1.0 m not described may hold such a layer
      zt <- if (length(surf)) h$bottom_cm[surf] else NA
      if (identical(v, FALSE) && (is.na(zt) || !.asc_described(x, zt, .asc_upper_window(x, 100)[2L]))) NA else v
    },
    palic = {
      zt <- if (length(surf)) h$bottom_cm[surf] else NA
      if (is.na(zt)) NA else if (zt >= top0 + 100) TRUE
      else .asc_every_in(x, ifelse(is.na(cls), NA, cls == "sand"), zt, top0 + 100)
    })
  .asc_result(paste0("gg_", kind), passed, within, list(),
              if (is.na(passed)) c("field_texture", "clay_pct") else character(0),
              "Arenosols, great groups Kandic, Tenic, Palic")
}

#' Epibasic and Hypervescent (Calsilic Arenosols)
#'
#' "epibasic": not calcareous in the A1 or Ap horizon at the surface, or in
#' the upper 0.2 m without one; "hypervescent": the major part of the upper
#' 0.1 m with more than 20 % fine-earth carbonate (visual, carbonate_soft_pct),
#' and/or a strong effervescence, and/or more than 8 % fine-earth carbonate by
#' analysis.
#' @noRd
calsilic_gg_asc <- function(pedon, kind = c("epibasic", "hypervescent")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h
  calc <- .asc_calcareous(x)
  top0 <- .asc_profile_top(x) %|NA|% 0
  passed <- switch(kind,
    epibasic = {
      ord <- order(h$top_cm, na.last = NA); ord <- ord[h$top_cm[ord] >= top0 - 0.5]
      a1 <- integer(0); prev <- top0
      for (i in ord) {
        if (!(x$role[i] %in% "A1") || abs(h$top_cm[i] - prev) > 0.5) break
        a1 <- c(a1, i); prev <- h$bottom_cm[i]; if (is.na(prev)) break
      }
      rows <- if (length(a1)) a1
              else which(!is.na(h$top_cm) & h$top_cm < top0 + 20 & !x$rock & !x$des$buried)
      if (!length(rows)) NA else if (any(calc[rows] %in% TRUE)) FALSE else if (all(calc[rows] %in% FALSE)) TRUE else NA
    },
    hypervescent = {
      eff <- tolower(as.character(h$effervescence_class %||% rep(NA_character_, x$n)))
      soft <- h$carbonate_soft_pct %||% rep(NA_real_, x$n); lab <- h$caco3_pct %||% rep(NA_real_, x$n)
      strong <- ifelse(is.na(eff) | !nzchar(eff), NA, grepl("strong|high", eff))
      ok <- .any3v(.any3v(strong, ifelse(is.na(soft), NA, soft > 20)), ifelse(is.na(lab), NA, lab > 8))
      # no effervescence: no fine-earth carbonate to see or measure
      ok[eff %in% c("none", "nil", "non-calcareous")] <- FALSE
      .asc_upper_major(x, ok, 10)
    })
  .asc_result(paste0("gg_", kind), passed, integer(0), list(),
              if (is.na(passed)) c("effervescence_class", "caco3_pct", "carbonate_soft_pct") else character(0),
              "Arenosols, great groups Epibasic and Hypervescent")
}

#' Arenic Tenosols: not or only slightly gravelly (less than 20 % coarse
#' fragments) throughout, loose or weakly coherent both moist and dry, sandy
#' (S, LS, CS) in more than 80 % of the profile, and no layer with more than
#' 15 % clay. Coarse fragments not recorded are none, as for the Arenosols.
#' @noRd
tenosol_arenic_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  rows <- which(x$mineral & !x$des$buried & !x$rock)
  cf <- ifelse(is.na(h$coarse_fragments_pct), 0, h$coarse_fragments_pct)
  every <- function(v) if (any(v %in% FALSE)) FALSE else if (all(v %in% TRUE)) TRUE else NA
  sandy <- .asc_is_sandy(h); th <- x$thick
  if (!length(rows) || anyNA(th[rows])) {
    frac <- NA
  } else {
    tot <- sum(th[rows]); t_t <- sum(th[rows][sandy[rows] %in% TRUE]); t_u <- sum(th[rows][is.na(sandy[rows])])
    frac <- if (tot <= 0) NA else if (t_t / tot > 0.8) TRUE else if ((t_t + t_u) / tot <= 0.8) FALSE else NA
  }
  passed <- .and3(all(cf[rows] < 20), frac, every(!.asc_over15(h)[rows]), every(.asc_loose_rows(x)[rows]))
  .asc_result("gg_arenic", passed, rows, list(),
              if (is.na(passed)) c("field_texture", "clay_pct", "coarse_fragments_pct", "strength_dry",
                                   "strength_moist") else character(0),
              "Tenosols, great group Arenic")
}

#' Clastic Rudosols: the gravel by its origin. "colluvic" (colluvial mass
#' movement debris: scree, talus, landslide, mudflow and creep deposits),
#' "fluvic" (rounded materials transported by streams or waves): the major
#' part of the gravelly layers (more than 10 % coarse fragments) with a layer
#' origin naming them; an origin the record does not give is read as absent.
#' @noRd
clastic_gg_asc <- function(pedon, kind = c("colluvic", "fluvic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h
  origin <- tolower(as.character(h$layer_origin %||% rep(NA_character_, x$n)))
  pat <- switch(kind, colluvic = "colluv|scree|talus|landslide|mudflow|mass movement|creep|solimov",
                fluvic = "alluv|fluvi|stream|river|beach|wave|marine|gravel bar")
  ok <- grepl(pat, origin)
  cf <- ifelse(is.na(h$coarse_fragments_pct), 0, h$coarse_fragments_pct)
  grav <- which(cf > 10 & x$mineral & !x$des$buried)
  passed <- if (!length(grav)) FALSE else .asc_rows_major(x, grav, ok)
  .asc_result(paste0("gg_", kind), passed, grav, list(),
              if (is.na(passed)) c("layer_origin", "coarse_fragments_pct") else character(0),
              sprintf("Rudosols, great group %s", tools::toTitleCase(kind)))
}

# ---- Hydrosols -------------------------------------------------------------------------

# The particle size that dominates (more than half of) the fine earth of each
# row. Lab particle size decides: the fraction itself, or another fraction
# above 50 % (which rules it out), or the remainder of the other two when
# both are measured. Otherwise the field texture where it tells: a sand,
# loamy sand or clayey sand is sand-sized; a medium heavy or heavy clay
# (more than 50 % clay) is clay-sized, a texture with at most 50 % clay is
# not; a clay of more than 50 % rules out sand and silt.
.asc_dominant_size <- function(x, size) {
  h <- x$h; n <- x$n
  lab <- list(clay = h$clay_pct %||% rep(NA_real_, n), silt = h$silt_pct %||% rep(NA_real_, n),
              sand = h$sand_pct %||% rep(NA_real_, n))
  others <- setdiff(names(lab), size)
  own <- lab[[size]]
  rest <- 100 - lab[[others[1]]] - lab[[others[2]]]
  own <- ifelse(!is.na(own), own, rest)
  other_over <- (!is.na(lab[[others[1]]]) & lab[[others[1]]] > 50) | (!is.na(lab[[others[2]]]) & lab[[others[2]]] > 50)
  by_lab <- ifelse(!is.na(own), own > 50, ifelse(other_over, FALSE, NA))
  tx <- .asc_texture(h$field_texture %||% rep(NA_character_, n))
  # lab clay, where measured, speaks for the clay (the field grade does not
  # override it)
  heavy <- tx$grade %in% c("MHC", "HC") & is.na(lab$clay)
  le50 <- !is.na(tx$clay_hi) & tx$clay_hi <= 50
  by_field <- switch(size,
    clay = ifelse(heavy, TRUE, ifelse(le50, FALSE, NA)),
    sand = ifelse(tx$sandy %in% TRUE, TRUE, ifelse(heavy, FALSE, NA)),
    silt = ifelse(tx$sandy %in% TRUE | heavy, FALSE, NA))
  ifelse(!is.na(by_lab), by_lab, by_field)
}

#' Hydrosol great groups beyond the acid sulfate ones
#'
#' \code{kind}: "histic" (organic materials dominating the upper 0.5 m),
#' "faunic" (other organic-rich, non-vegetative materials such as faunal
#' debris dominating it: a layer origin naming them; absent unless recorded),
#' "epicalcareous" (the major part of the upper 0.5 m calcareous),
#' "argillaceous", "lutaceous", "arenaceous" (dominantly clay-, silt- or
#' sand-sized to 0.5 m: more than half of the fine earth), "mottled" and
#' "haplic" (the major part of the upper 0.5 m mottled, or whole coloured),
#' "calcarosolic" (calcareous as for the Calcarosol order, and no clear or
#' abrupt textural B), "sodosolic" (a textural B sodic and not strongly acid
#' over the major part of the upper 0.2 m of the B2), "chromosolic" (a
#' textural B not strongly acid there).
#' @noRd
hydrosol_gg_asc <- function(pedon, kind = c("histic", "faunic", "epicalcareous", "argillaceous", "lutaceous",
                                            "arenaceous", "mottled", "haplic", "calcarosolic", "sodosolic",
                                            "chromosolic")) {
  kind <- match.arg(kind)
  if (kind %in% c("sodosolic", "chromosolic")) {
    r <- if (kind == "sodosolic")
           .asc_textural_gate(pedon, "gg_sodosolic",
                              function(x) list(sodic_not_strongly_acid = .and3v(.asc_sodic(x$h), !.asc_strongly_acid(x$h))),
                              "Hydrosols, great group Sodosolic")
         else .asc_textural_gate(pedon, "gg_chromosolic", function(x) list(strongly_acid = .asc_strongly_acid(x$h)),
                                 "Hydrosols, great group Chromosolic", negate = "strongly_acid")
    return(r)
  }
  x <- .asc_horizons(pedon); h <- x$h
  passed <- switch(kind,
    histic = .asc_upper_major(x, .asc_organic_material(x, pedon), 50),
    faunic = {
      origin <- tolower(as.character(h$layer_origin %||% rep(NA_character_, x$n)))
      ok <- grepl("faun|shell|coral|guano|ornithogen|bone", origin)
      if (!any(ok)) FALSE else .asc_upper_major(x, ok, 50)
    },
    epicalcareous = .asc_upper_major(x, .asc_calcareous(x), 50),
    argillaceous = .asc_upper_major(x, .asc_dominant_size(x, "clay"), 50),
    lutaceous = .asc_upper_major(x, .asc_dominant_size(x, "silt"), 50),
    arenaceous = .asc_upper_major(x, .asc_dominant_size(x, "sand"), 50),
    mottled = .asc_upper_major(x, .asc_mottled_rows(h), 50),
    haplic = .asc_upper_major(x, !.asc_mottled_rows(h), 50),
    calcarosolic = .and3(.asc_calcarosol_calcareous(x)$calcareous, !textural_b_asc(pedon)$passed))
  .asc_result(paste0("gg_", kind), passed, integer(0), list(),
              if (is.na(passed)) c("oc_pct", "layer_origin", "effervescence_class", "clay_pct", "silt_pct",
                                   "sand_pct", "field_texture", "redoximorphic_features_pct") else character(0),
              sprintf("Hydrosols, great group %s", tools::toTitleCase(kind)))
}
