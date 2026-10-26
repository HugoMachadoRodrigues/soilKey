# =============================================================================
# v0.9.288 -- subgroups of the Australian Soil Classification (3rd ed.) for
# the Vertosols, Calcarosols, Podosols and Organosols: the upper and lower
# parts of the solum (Vertosols, Calcarosols), the coherence of the Podosol
# B and a densipan in its A2, and what the organic materials of the
# Organosols directly overlie. Three-valued; the readings are in
# inst/rules/asc3/readings.yaml.
# =============================================================================

# The solum as described: its horizons (P, O2, A, B and their transitions,
# not buried; pans left out), with the rows of unknown role that may belong to it (below
# the last designated row too) tried out and in: a row between two solum
# horizons belongs to it, and the solum may begin or end at any of the
# others above or below it. A list of sets of rows; a verdict holds when it
# holds for each. NULL for a profile with no designations at all.
.asc_solum_sets <- function(x) {
  h <- x$h
  if (!x$any_known) return(NULL)
  # a pan that makes up its layer is substrate, not solum (as the B is read
  # for the Petrocalcic great groups)
  pan <- .asc_pan_layer(x)
  sure <- which(x$solum %in% TRUE & !is.na(h$top_cm) & !pan)
  may <- which(x$maybe_soil & !(x$solum %in% TRUE) & !is.na(h$top_cm) & !pan)
  sw <- .asc_sandwiched(x, may, sure)
  lo <- sort(c(sure, sw)); may <- setdiff(may, sw)
  if (!length(may)) return(list(lo))
  if (!length(lo)) {
    # no designated solum horizon: the solum runs from the top through any
    # number of the rows of unknown role (or there is none)
    ord <- may[order(h$top_cm[may])]
    return(lapply(0:length(ord), function(k) ord[seq_len(k)]))
  }
  top_lo <- min(h$top_cm[lo])
  above <- may[h$top_cm[may] < top_lo]; above <- above[order(-h$top_cm[above])]   # nearest first
  below <- setdiff(may, above); below <- below[order(h$top_cm[below])]
  # too many ways to try: undecided
  if ((length(above) + 1) * (length(below) + 1) > 500) return(list(NA))
  sets <- list()
  for (j in 0:length(above)) for (k in 0:length(below))
    sets[[length(sets) + 1L]] <- sort(c(lo, head(above, j), head(below, k)))
  sets
}

.asc_agree <- function(v) if (all(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA

# A horizon of unknown depth may be thin or as thick as the room down to
# the next horizon allows (500 cm with none below); the verdicts here move
# one way with its thickness, so both ends suffice.
.asc_bottom_variants <- function(h) {
  top <- h$top_cm; bot <- h$bottom_cm
  thin <- ifelse(is.na(bot) & !is.na(top), top + 1, bot)
  nxt <- vapply(seq_along(top), function(i) {
    if (is.na(top[i])) return(NA_real_)
    d <- top[!is.na(top) & top > top[i] + 0.5]
    if (length(d)) min(d) else top[i] + 500
  }, 0)
  thick <- ifelse(is.na(bot) & !is.na(top), nxt, bot)
  if (identical(thin, thick)) list(thin) else list(thin, thick)
}

#' Parts of the solum (Vertosol and Calcarosol subgroups)
#'
#' \code{part}: "upper" (the upper 0.5 m of the solum, all of it when it is
#' thinner) or "lower" (the solum below 0.5 m; none when it is thinner).
#' \code{prop}: "acid" (strongly acid), "calcareous", "mottled",
#' "whole_coloured", "hypersodic" (an ESP of 15 or more). \code{how}:
#' "major" (the major part, by depth), "some" (some horizon of the solum in
#' that part), "some_subsurface" (some horizon below the surface one). The
#' solum is read as described (its depth below the description is not
#' assumed); a part not described within it is undecided.
#' @noRd
solum_part_asc <- function(pedon, prop = c("acid", "calcareous", "mottled", "whole_coloured", "hypersodic"),
                           part = c("upper", "lower"), how = c("major", "some", "some_subsurface")) {
  prop <- match.arg(prop); part <- match.arg(part); how <- match.arg(how)
  x <- .asc_horizons(pedon); h <- x$h
  ok <- switch(prop,
    acid = .asc_strongly_acid(h), calcareous = .asc_calcareous(x),
    mottled = .asc_mottled_rows(h), whole_coloured = !.asc_mottled_rows(h),
    hypersodic = { e <- .asc_esp(h); ifelse(e$low_charge, FALSE, ifelse(is.na(e$esp), NA, e$esp >= 15)) })
  one <- function(rows, bot) {
    if (!length(rows)) return(FALSE)
    x$h$bottom_cm <- bot; h <- x$h
    st <- min(h$top_cm[rows])
    sb <- max(h$bottom_cm[rows])
    zt <- if (part == "upper") st else st + 50
    zb <- if (part == "upper") min(st + 50, sb) else sb
    if (zb <= zt + 0.5) return(FALSE)
    if (how == "major") return(.asc_major_part(x, rows, ok, zt, zb))
    inw <- rows[h$top_cm[rows] < zb - 0.01 & h$bottom_cm[rows] > zt + 0.01]
    if (how == "some_subsurface") inw <- inw[h$top_cm[inw] > st + 0.5]
    v <- ok[inw]
    if (any(v %in% TRUE)) TRUE else if (anyNA(v) || !.asc_described(x, zt, zb)) NA else FALSE
  }
  sets <- .asc_solum_sets(x)
  bv <- .asc_bottom_variants(h)
  passed <- if (is.null(sets) || anyNA(sets)) NA
            else .asc_agree(unlist(lapply(sets, function(r) vapply(bv, function(b) one(r, b), NA))))
  .asc_result(paste("sg_solum", part, prop, how, sep = "_"), passed, integer(0),
              list(prop = prop, part = part, how = how),
              if (is.na(passed)) c("ph_h2o", "ph_cacl2", "effervescence_class", "na_cmol", "cec_cmol",
                                   "redoximorphic_features_pct", "designation") else character(0),
              "Vertosols and Calcarosols, subgroups (upper 0.5 m of the solum, solum below 0.5 m)")
}

#' Episodic subgroups (Vertosols): the upper 0.1 m of the solum sodic, read
#' as its weighted average ESP (glossary 'Weighted average') of 6 or more; a
#' horizon of low charge counts as not sodic (ESP 0), and a part of unknown
#' ESP may hold any value.
#' @noRd
episodic_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  e <- .asc_esp(h); esp <- ifelse(e$low_charge, 0, e$esp)
  one <- function(rows, b) {
    if (!length(rows)) return(FALSE)
    st <- min(h$top_cm[rows])
    sb <- max(b[rows])
    zb <- min(st + 10, sb); span <- zb - st
    if (span <= 0) return(FALSE)
    top <- h$top_cm[rows]; bot <- b[rows]; v <- esp[rows]
    # the window cut at every row boundary; overlapping rows are counted once
    # (their lowest and highest ESP bound it); a part no row covers, or a row
    # of unknown ESP, may hold any value
    cuts <- sort(unique(c(st, zb, top[top > st & top < zb], bot[bot > st & bot < zb])))
    lo <- 0; hi <- 0
    for (k in seq_len(length(cuts) - 1L)) {
      w <- cuts[k + 1L] - cuts[k]; mid <- (cuts[k] + cuts[k + 1L]) / 2
      vv <- v[top <= mid & bot > mid]
      if (!length(vv) || anyNA(vv)) { hi <- hi + w * 100; next }
      lo <- lo + w * min(vv); hi <- hi + w * max(vv)
    }
    if (lo / span >= 6) TRUE else if (hi / span < 6) FALSE else NA
  }
  sets <- .asc_solum_sets(x)
  bv <- .asc_bottom_variants(h)
  passed <- if (is.null(sets) || anyNA(sets)) NA
            else .asc_agree(unlist(lapply(sets, function(r) vapply(bv, function(b) one(r, b), NA))))
  .asc_result("sg_episodic", passed, integer(0), list(),
              if (is.na(passed)) c("na_cmol", "cec_cmol", "ecec_cmol", "esp_pct") else character(0),
              "Vertosols, subgroups Episodic; glossary 'Sodic', 'Weighted average'")
}

#' Strongly and weakly coherent B horizons (Podosol subgroups Parapanic and
#' Fragic)
#'
#' Strongly coherent: a B horizon with a consistence strength of very firm or
#' more (force 4 and over, dry or moist: the glossary notes it is usually
#' independent of the water status), or a pan-like layer (ortstein, coffee
#' rock, sandrock; a cemented B other than a thin ironpan). Weakly coherent:
#' every B horizon loose to firm (force 0-3) and no pans of any kind. A
#' strength not recorded leaves a horizon undecided.
#' @noRd
podosol_coherence_asc <- function(pedon, kind = c("strong", "weak")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h
  str_of <- function(v) tolower(trimws(as.character(v %||% rep(NA_character_, x$n))))
  sd <- str_of(h$strength_dry); sm <- str_of(h$strength_moist)
  hi <- c("very firm", "strong", "very strong", "rigid"); lo <- c("loose", "very weak", "weak", "firm")
  txt <- tolower(paste(h$pan_type %||% rep(NA, x$n), h$layer_origin %||% rep(NA, x$n)))
  panlike <- grepl("ortstein|coffee ?rock|sandrock", txt) |
             ((x$des$m | (x$hard & !x$rock)) & !(.asc_pan_rows(x, "thin ironpan") %in% TRUE))
  pan_any <- x$des$m | x$hard | (!is.na(h$pan_type) & !tolower(trimws(h$pan_type)) %in% c("", "none"))
  # a thin ironpan is no Podosol B feature (glossary, Bs horizons): the
  # strength of a horizon that is the pan does not make the B coherent,
  # though it is a pan (a band within a horizon leaves that horizon's
  # strength to count)
  tip <- .asc_pan_rows(x, "thin ironpan") %in% TRUE & (.asc_pan_layer(x) | x$des$m | x$hard)
  strong <- ((sd %in% hi | sm %in% hi) & !tip) | panlike
  weak_rec <- (sd %in% lo | sm %in% lo | tip) & !strong
  row_strong <- ifelse(strong, TRUE, ifelse(weak_rec, FALSE, NA))
  bm <- .asc_b_and_maybe(x)
  b <- sort(unique(c(which(!is.na(x$role) & grepl("^B", x$role) & !x$des$buried), bm$b)))
  maybe <- setdiff(bm$maybe, b)
  passed <- if (!x$any_known) NA
            else if (kind == "strong") {
              if (any(row_strong[b] %in% TRUE)) TRUE
              else if (anyNA(row_strong[b]) || any(!(row_strong[maybe] %in% FALSE))) NA else FALSE
            } else {
              if (!length(b)) FALSE
              else if (any(row_strong[c(b, maybe)] %in% TRUE) || any(pan_any[b])) FALSE
              else if (all(row_strong[b] %in% FALSE) && all(row_strong[maybe] %in% FALSE) && !any(pan_any[maybe])) TRUE
              else NA
            }
  .asc_result(paste0("sg_", kind, "ly_coherent_b"), passed, b[row_strong[b] %in% TRUE], list(),
              if (is.na(passed)) c("strength_dry", "strength_moist", "pan_type", "designation") else character(0),
              "Podosols, subgroups Parapanic and Fragic; glossary 'Strongly coherent B horizon', 'Weakly coherent B horizon'")
}

#' Densic subgroups (Podosols): a densipan in the A2 horizon (pan_type; pans
#' show, so none recorded is none); a row of unknown role above the first B
#' horizon may be the A2.
#' @noRd
densipan_a2_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  dens <- grepl("densipan", tolower(as.character(h$pan_type %||% rep(NA_character_, x$n))))
  a2 <- x$role %in% "A2" & !x$des$buried
  b_top <- suppressWarnings(min(h$top_cm[!is.na(x$role) & grepl("^B", x$role) & !x$des$buried], na.rm = TRUE))
  may <- x$maybe_soil & !x$trailing & !is.na(h$top_cm) & h$top_cm < b_top
  passed <- if (any(dens & a2)) TRUE else if (any(dens & may) || (!x$any_known && any(dens))) NA else FALSE
  .asc_result("sg_densic", passed, which(dens & a2), list(),
              if (is.na(passed)) c("pan_type", "designation") else character(0),
              "Podosols, subgroups Densic; glossary 'Densipan'")
}

#' Organosol subgroups: what the organic materials directly overlie, and
#' the mineral layers within or below them
#'
#' The organic materials are the rows of organic material (rows whose
#' material is unknown tried out and in). "lithic" (directly over hard
#' rock), "paralithic" (over saprolite or weathered rock), "marly" (a marl
#' layer within the organic materials or directly under them), "rudaceous"
#' (directly over fragmental material, coarse fragments 50 \% or more or an
#' origin naming gravel, cobbles, stones, boulders, scree or talus, whose
#' interstices hold organic material: organic C of 12 \% or more, or the
#' deepest organic layer itself fragmental), "modic" (directly over mineral
#' material with the organic C of a humose horizon or the colour of a
#' melacic or melanic one), "placic" (a thin ironpan below them),
#' "terric" (a layer of unconsolidated mineral material within or below them,
#' starting within 0.8 m of the surface), "regolithic" (such a layer below
#' 0.8 m). Unconsolidated mineral material: a mineral layer between organic
#' ones, an A or B horizon, a buried soil, or a layer whose origin names a
#' sediment; a C or D of unrecorded origin may be saprolite. Rows of unknown
#' material left out of the organic materials are mineral; "within or
#' below" starts at their top (a mineral cover above them does not count).
#' @noRd
organosol_sg_asc <- function(pedon, kind = c("lithic", "paralithic", "marly", "rudaceous", "modic", "placic",
                                             "terric", "regolithic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h; n <- x$n
  om <- .asc_organic_material(x, pedon)
  top0 <- .asc_profile_top(x) %|NA|% 0
  has_top <- !is.na(h$top_cm)
  org <- which(om %in% TRUE & !x$rock & has_top & !(x$role %in% "O1"))
  may <- which(is.na(om) & !x$rock & has_top)
  sets <- if (length(may)) list(org, sort(unique(c(org, may)))) else list(org)
  sk <- vapply(seq_len(n), function(i) .asc_substrate_kind(x, i), "")
  soil_hz <- grepl("^[AB]", x$role %||% "") & !(x$role %in% "B/C")
  named_sap <- grepl("saprolit|saprock|weathered rock|decomposed rock",
                     tolower(paste(h$layer_origin %||% rep(NA, n), h$rock_origin %||% rep(NA, n))))
  ends_hard <- .asc_ends_hard(x)
  last_bot <- suppressWarnings(max(h$bottom_cm[has_top], na.rm = TRUE))
  frag <- {
    cf <- h$coarse_fragments_pct %||% rep(NA_real_, n)
    txt <- tolower(paste(h$layer_origin %||% rep(NA, n), h$rock_origin %||% rep(NA, n)))
    (!is.na(cf) & cf >= 50) | grepl("gravel|cobble|stones|boulder|scree|talus", txt)
  }
  # material of a humose, melacic or melanic horizon (organic C or colour)
  hum_blk <- {
    cr <- x$clay
    lo <- ifelse(is.na(cr$lo), 0, pmin(cr$lo, 60)); hi <- ifelse(is.na(cr$hi), 60, pmin(cr$hi, 60))
    oc <- h$oc_pct
    hum <- ifelse(is.na(oc), NA, ifelse(oc > 4 + 2 * hi / 60 | oc >= 6, TRUE, ifelse(oc <= 4 + 2 * lo / 60, FALSE, NA)))
    val <- suppressWarnings(as.numeric(h$munsell_value_moist)); chr <- suppressWarnings(as.numeric(h$munsell_chroma_moist))
    dval <- suppressWarnings(as.numeric(h$munsell_value_dry))
    blk <- .and3v(ifelse(is.na(val) | is.na(chr), NA, val <= 3 & chr <= 2), ifelse(is.na(dval), NA, dval <= 5))
    .any3v(hum, blk)
  }
  one <- function(S) {
    if (!length(S)) return(FALSE)
    # the base of the organic materials must be known
    if (anyNA(h$bottom_cm[S])) return(NA)
    bot <- max(h$bottom_cm[S]); s_top <- min(h$top_cm[S])
    # in this reading, rows of unknown material left out of the organic
    # materials are mineral
    mineral <- (om %in% FALSE | (is.na(om) & !(seq_len(n) %in% S))) & !x$rock
    # the kind of each row as substrate: an A or B horizon is soil material,
    # not rock, saprolite or marl; a C or D of unrecorded origin may be
    # saprolite but is not hard rock
    kind_of <- function(i) {
      k <- sk[i]
      # an A or B horizon is soil material unless its origin names saprolite
      if (soil_hz[i] && (is.na(k) || (k %in% "saprolite" && !named_sap[i]))) "unconsolidated" else k
    }
    uncons <- vapply(seq_len(n), function(i) {
      if (!mineral[i]) return(FALSE)
      k <- kind_of(i)
      if (k %in% c("rock", "pan", "hard", "saprolite")) FALSE
      else if (k %in% c("unconsolidated", "marl") || x$des$buried[i]) TRUE else NA
    }, NA)
    u <- .asc_under_solum(x, S)
    under <- u$rows
    open_under <- if (length(u$after_gap) || !ends_hard) NA else FALSE
    within <- which(has_top & h$top_cm >= s_top - 0.5 & h$top_cm < bot - 0.5 & !(seq_len(n) %in% S))
    over <- function(k_want) {
      if (!length(under)) return(open_under)
      k <- vapply(under, kind_of, "")
      if (k_want == "rock") k[is.na(k) & !is.na(x$role[under]) & !x$unknown_role[under]] <- "other"
      k[k %in% "hard"] <- if (k_want == "marl") "other" else NA
      # sandy material is not marl
      if (k_want == "marl") k[is.na(k) & .asc_is_sandy(h)[under] %in% TRUE] <- "other"
      if (any(k %in% k_want)) TRUE else if (anyNA(k)) NA else FALSE
    }
    switch(kind,
      lithic = over("rock"),
      paralithic = over("saprolite"),
      marly = .any3(c(vapply(within, kind_of, "") %in% "marl", over("marl"))),
      rudaceous = {
        d <- S[which.max(h$bottom_cm[S])]
        if (frag[d]) TRUE
        else if (!length(under)) open_under
        else .asc_any_rows(ifelse(!frag[under], FALSE,
                           ifelse(om[under] %in% TRUE | (!is.na(h$oc_pct[under]) & h$oc_pct[under] >= 12), TRUE,
                                  ifelse(is.na(h$oc_pct[under]), NA, FALSE))))
      },
      modic = if (!length(under)) open_under else .asc_any_rows(ifelse(mineral[under], hum_blk[under], FALSE)),
      placic = .asc_any_rows(.asc_pan_rows(x, "thin ironpan")[which(has_top & h$top_cm >= bot - 0.5)]),
      terric = , regolithic = {
        lim <- top0 + 80
        v <- uncons
        v[seq_len(n) %in% within & mineral] <- TRUE         # between organic layers
        rows <- which(has_top & h$top_cm >= s_top - 0.5 & !(seq_len(n) %in% S))
        rows <- if (kind == "terric") rows[h$top_cm[rows] < lim] else rows[h$top_cm[rows] >= lim - 0.5]
        res <- .asc_any_rows(v[rows])
        # a part of the window not described may hold such a layer
        seen <- if (kind == "terric") .asc_described(x, s_top, lim) || ends_hard else ends_hard && is.finite(last_bot)
        if (identical(res, FALSE) && !seen) NA else res
      })
  }
  passed <- .asc_agree(vapply(sets, function(S) { r <- one(S); if (length(r) != 1L) NA else r }, NA))
  .asc_result(paste0("sg_organosol_", kind), passed, integer(0), list(kind = kind),
              if (is.na(passed)) c("designation", "oc_pct", "layer_origin", "rock_origin", "coarse_fragments_pct",
                                   "pan_type") else character(0),
              "Organosols, subgroups; glossary 'Unconsolidated mineral materials', 'Marl'")
}
