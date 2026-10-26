# =============================================================================
# v0.9.282 -- suborders of the Australian Soil Classification (3rd ed.) for
# the Anthroposols (the kind of human material), Arenosols, Rudosols and
# Tenosols: the peaty, humose, melacic and melanic horizons, the
# conspicuously bleached A2, hard materials within 0.5 m, salinity,
# stratification, gravel, nodules and fine-earth carbonate. Three-valued;
# the readings are in inst/rules/asc3/readings.yaml.
# =============================================================================


# ---- the profile and runs from its surface -------------------------------------

# The top of the soil profile: the top of the first row, an O1 (litter) at
# the surface excepted (it is outside the solum).
.asc_profile_top <- function(x) {
  h <- x$h; ord <- order(h$top_cm, na.last = NA)
  if (!length(ord)) return(NA_real_)
  start <- h$top_cm[ord[1L]]
  for (i in ord) {
    if (x$role[i] %in% "O1" && !is.na(h$bottom_cm[i]) && abs(h$top_cm[i] - start) <= 0.5) start <- h$bottom_cm[i]
    else break
  }
  start
}

# A run of adjacent rows from depth `from` down, while `ok` holds (rows
# above `from` are not read): `sure` is the thickness of the rows that
# certainly qualify, from `from` down to the first that may not; `possible`
# the thickness of the rows that may (a gap in the record, a row of unknown
# thickness, or the end of the description above no rock, may go on; a gap
# directly above the first row that does not qualify may, since 0.9.287).
# `stop_top` is the top of that row (NA when the run is not stopped).
.asc_run_from <- function(x, ok, from) {
  h <- x$h; ord <- order(h$top_cm, na.last = NA)
  ord <- ord[h$top_cm[ord] >= from - 0.5]
  sure <- 0; poss <- 0; sure_rows <- integer(0); rows <- integer(0)
  sure_on <- TRUE; prev <- from; stopped <- FALSE; stop_top <- NA_real_
  for (i in ord) {
    gap <- h$top_cm[i] - prev
    if (ok[i] %in% FALSE) {
      if (gap > 0.5) poss <- poss + gap
      stopped <- TRUE; stop_top <- h$top_cm[i]; break
    }
    if (gap > 0.5) { sure_on <- FALSE; poss <- poss + gap }
    th <- h$bottom_cm[i] - h$top_cm[i]
    rows <- c(rows, i)
    if (is.na(th)) { poss <- Inf; break }
    poss <- poss + th
    if (sure_on && ok[i] %in% TRUE) { sure <- sure + th; sure_rows <- c(sure_rows, i) } else sure_on <- FALSE
    prev <- h$bottom_cm[i]
  }
  # a run that reaches the end of the description may go on below it, unless
  # the description ends on rock or a hard layer
  if (!stopped && length(rows) && !.asc_ends_hard(x)) poss <- Inf
  list(sure = sure, possible = poss, sure_rows = sure_rows, rows = rows, stop_top = stop_top)
}

.asc_run_at_least <- function(run, cm) if (run$sure >= cm) TRUE else if (run$possible < cm) FALSE else NA

# The major part, by the thickness of each row inside [zt, zb], of `rows`
# (the described part of the window).
.asc_overlap_major <- function(x, rows, ok, zt, zb) {
  h <- x$h
  if (!length(rows) || is.na(zt) || is.na(zb)) return(NA)
  w <- .asc_overlap(h$top_cm[rows], h$bottom_cm[rows], zt, zb)
  keep <- !is.na(w) & w > 0; rows <- rows[keep]; w <- w[keep]
  if (!length(rows)) return(NA)
  tot <- sum(w); o <- ok[rows]
  if (sum(w[o %in% TRUE]) > tot / 2) TRUE else if (sum(w[o %in% FALSE]) >= tot / 2) FALSE else NA
}

# Every horizon of [zt, zb] passes: FALSE when one fails; TRUE when all pass
# and the rows cover the window (or the description ends on rock or a hard
# layer); NA otherwise.
.asc_every_in <- function(x, ok, zt, zb) {
  h <- x$h
  rows <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & h$bottom_cm > zt + 0.01 & h$top_cm < zb &
                  !x$des$buried & x$mineral)
  if (any(ok[rows] %in% FALSE)) return(FALSE)
  if (!length(rows) || !all(ok[rows] %in% TRUE)) return(NA)
  # the part of the window the rows cover (each clipped to it, overlaps once)
  a <- pmax(h$top_cm[rows], zt); b <- pmin(h$bottom_cm[rows], zb)
  o <- order(a); a <- a[o]; b <- b[o]
  cover <- 0; end <- zt
  for (k in seq_along(a)) { if (b[k] > end) { cover <- cover + b[k] - max(a[k], end); end <- b[k] } }
  if (cover >= zb - zt - 0.5 || .asc_ends_hard(x)) TRUE else NA
}

# ---- peaty, humose, melacic and melanic horizons ---------------------------------

# The surface or near-surface horizons of the glossary: a peaty horizon
# (organic materials, P or O2, at least 0.2 m from the top of the profile);
# a humose horizon (at least 0.2 m, not organic material, average organic C
# above 4 % with no clay to 6 % at 60 % clay or more, proportionally);
# melacic and melanic horizons (at least 0.2 m, within the A horizons when
# the soil has a clear or abrupt textural B, black when moist -- value 3 or
# less, chroma 2 or less -- throughout and dry value 5 or less; melacic with
# pH 1:5 below 5.5, melanic with pH 5.5 or more and more than a weak grade of
# structure with peds of 10-20 mm or less over the major part, waived for an
# Ap or a dry strength of weak or less). The non-peaty ones start at the
# mineral surface.
.asc_chernic_parts <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  om <- .asc_organic_material(x, pedon)
  top0 <- .asc_profile_top(x)
  if (is.na(top0)) return(list(peaty = NA, humose = NA, dark = NA, melacic = NA, melanic = NA, x = x))
  # peaty: organic materials (not litter) from the top of the profile
  ok_peat <- ifelse(x$role %in% "O1", FALSE, om)
  peaty <- .asc_run_at_least(.asc_run_from(x, ok_peat, top0), 20)
  # the mineral surface: below the organic rows at the top
  ok_o <- ifelse(om %in% TRUE & !(x$role %in% "O1"), TRUE, ifelse(is.na(om), NA, FALSE))
  orun <- .asc_run_from(x, ok_o, top0)
  if (is.infinite(orun$possible))
    return(list(peaty = peaty, humose = NA, dark = NA, melacic = NA, melanic = NA, x = x))
  # where it may lie (since 0.9.287): below the surely organic rows, at the
  # top of a row that may be organic, at the start of a gap, or at the end
  # of the run; a verdict holds only when it holds at each
  cand <- top0 + orun$sure
  if (orun$possible > orun$sure) {
    rr <- orun$rows
    for (k in seq_along(rr)) {
      i <- rr[k]
      if (i %in% orun$sure_rows) next
      if (is.na(ok_o[i])) cand <- c(cand, h$top_cm[i])
      nxt <- if (k < length(rr)) h$top_cm[rr[k + 1L]] else orun$stop_top
      if (!is.na(nxt) && !is.na(h$bottom_cm[i]) && nxt - h$bottom_cm[i] > 0.5) cand <- c(cand, h$bottom_cm[i])
    }
    cand <- c(cand, top0 + orun$possible)
  }
  cand <- sort(unique(round(cand[!is.na(cand)], 3)))
  at <- lapply(cand, function(tm) .asc_chernic_at(pedon, x, om, tm))
  agree <- function(v) if (all(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA
  one <- function(k) agree(vapply(at, function(a) a[[k]], NA))
  list(peaty = peaty, humose = one("humose"), dark = one("dark"), melacic = one("melacic"),
       melanic = one("melanic"), x = x)
}

# The humose, melacic and melanic horizons with the mineral surface at depth
# `tm` (see .asc_chernic_parts).
.asc_chernic_at <- function(pedon, x, om, tm) {
  h <- x$h
  ends_hard <- .asc_ends_hard(x)
  # humose: the upper 0.2 m of the mineral soil, averaged
  win <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & h$bottom_cm > tm + 0.01 & h$top_cm < tm + 20 & !x$rock)
  w <- if (length(win)) .asc_overlap(h$top_cm[win], h$bottom_cm[win], tm, tm + 20) else numeric(0)
  humose <- if (!length(win)) NA else if (sum(w) < 20 - 0.5) {
    if (ends_hard) FALSE else NA
  } else if (any(om[win] %in% TRUE) || anyNA(om[win][h$top_cm[win] > tm + 0.5]) || anyNA(h$oc_pct[win])) NA else {
    oc <- sum(h$oc_pct[win] * w) / sum(w)
    cr <- x$clay
    lo <- ifelse(is.na(cr$lo[win]), 0, pmin(cr$lo[win], 60)); hi <- ifelse(is.na(cr$hi[win]), 60, pmin(cr$hi[win], 60))
    need_lo <- 4 + 2 * sum(lo * w) / sum(w) / 60; need_hi <- 4 + 2 * sum(hi * w) / sum(w) / 60
    if (oc > need_hi || oc >= 6) TRUE else if (oc <= need_lo) FALSE else NA
  }
  # melacic or melanic: black rows from the mineral surface
  val <- suppressWarnings(as.numeric(h$munsell_value_moist)); chr <- suppressWarnings(as.numeric(h$munsell_chroma_moist))
  dval <- suppressWarnings(as.numeric(h$munsell_value_dry))
  black <- ifelse(is.na(val) | is.na(chr), NA, val <= 3 & chr <= 2)
  dark_ok <- .and3v(black, ifelse(is.na(dval), NA, dval <= 5))
  dark_ok[x$rock | om %in% TRUE] <- FALSE
  # a row that may be organic material may end the horizon (the one at the
  # mineral surface is mineral here: the surface lies at its top)
  at_surface <- !is.na(h$top_cm) & abs(h$top_cm - tm) <= 0.5
  dark_ok[is.na(om) & !at_surface & dark_ok %in% TRUE] <- NA
  tb <- tryCatch(textural_b_asc(pedon)$passed, error = function(e) NA)
  not_a <- !grepl("^A", x$role %||% "")
  if (isTRUE(tb)) {
    # within the A horizons: a row of unknown role between two A horizons is
    # one; one above the first B may be (since 0.9.287; before, it was not)
    unk <- is.na(x$role) | x$unknown_role
    b_top <- suppressWarnings(min(h$top_cm[!unk & grepl("^B", x$role %||% "") & !x$des$buried], na.rm = TRUE))
    a_rows <- which(!unk & !not_a & !x$des$buried & !is.na(h$top_cm) & h$top_cm < b_top)
    sw <- .asc_sandwiched(x, which(unk), a_rows)
    may_a <- unk & !(seq_len(x$n) %in% sw) & !is.na(h$top_cm) & h$top_cm < b_top - 0.5
    dark_ok[not_a & !unk] <- FALSE
    dark_ok[unk & !(seq_len(x$n) %in% sw) & !may_a] <- FALSE
    dark_ok[may_a & dark_ok %in% TRUE] <- NA
  } else if (is.na(tb)) dark_ok[not_a & dark_ok %in% TRUE] <- NA
  drun <- .asc_run_from(x, dark_ok, tm)
  dark_thick <- .asc_run_at_least(drun, 20)
  melacic_out <- melanic_out <- FALSE
  dark <- if (identical(dark_thick, FALSE)) FALSE else {
    # more than a weak grade (moderate or strong), peds of 10-20 mm or less
    grade_ok <- .asc_pedal(h)
    ps <- h$ped_size_max_mm %||% rep(NA_real_, x$n)
    size_ok <- ifelse(is.na(ps), NA, ps <= 20)
    waived <- x$des$p | tolower(as.character(h$strength_dry %||% rep(NA_character_, x$n))) %in% c("loose", "very weak", "weak")
    struct_ok <- ifelse(waived, TRUE, .and3v(grade_ok, size_ok))
    # the horizon may end at any row from
    # the last surely dark one to the last that may be dark, or within a gap
    # in the record (which may or may not be dark); a verdict holds only
    # when every possible extent gives it (melacic, melanic, and either of
    # them for the suborders, since 0.9.287). An extent is continuous, so a gap
    # inside it is part of it, of unknown kind; an extent under 0.2 m is no
    # horizon; a run that reaches the end of the description (above no
    # rock) may go on below it. Within a gap the verdict moves one way with
    # the depth of the end, so its two ends suffice.
    ph_acid <- ifelse(is.na(h$ph_h2o), NA, h$ph_h2o < 5.5)
    all_rows <- drun$rows; n_all <- length(all_rows); n_sure <- length(drun$sure_rows)
    r_top <- h$top_cm[all_rows]; r_bot <- h$bottom_cm[all_rows]
    gap_before <- if (n_all) c(r_top[1L] - tm, r_top[-1L] - r_bot[-n_all]) > 0.5 else logical(0)
    gap_before[is.na(gap_before)] <- TRUE
    open_end <- n_all > 0L && is.infinite(drun$possible) && !anyNA(x$thick[all_rows])
    # extents: k rows from the mineral surface, ending at a depth `end`
    ext <- list()
    for (k in n_sure:n_all) {
      nxt_dark <- k < n_all && dark_ok[all_rows[k + 1L]] %in% TRUE
      # at the bottom of row k, when what follows may not be dark
      if (k > 0L && (k == n_all || !nxt_dark || gap_before[k + 1L])) ext[[length(ext) + 1L]] <- c(k, r_bot[k])
      # through the gap below row k (the gap may be dark down to just above
      # the next row, even a black one)
      if (k < n_all && gap_before[k + 1L]) ext[[length(ext) + 1L]] <- c(k, r_top[k + 1L])
    }
    # through a gap directly above the row that stops the run
    last_bot <- if (n_all) r_bot[n_all] else tm
    if (!is.na(drun$stop_top) && !is.na(last_bot) && drun$stop_top - last_bot > 0.5)
      ext[[length(ext) + 1L]] <- c(n_all, drun$stop_top)
    if (n_sure == 0L) ext[[length(ext) + 1L]] <- c(0, tm)
    # the major part of an extent from tm to `end`, a gap counted as unknown
    maj <- function(rr, ok, span) {
      w <- x$thick[rr]
      if (sum(w[ok[rr] %in% TRUE]) > span / 2) TRUE else if (sum(w[ok[rr] %in% FALSE]) >= span / 2) FALSE else NA
    }
    kinds <- lapply(ext, function(e) {
      rr <- all_rows[seq_len(e[1L])]
      if (anyNA(x$thick[rr]) || is.na(e[2L])) return(c(NA, NA))
      span <- e[2L] - tm
      if (span < 20 - 0.5) return(c(FALSE, FALSE))
      ac <- maj(rr, ph_acid, span)
      c(ac, .and3(!ac, maj(rr, struct_ok, span)))
    })
    if (open_end) kinds <- c(kinds, list(c(NA, NA)))
    agree <- function(v) if (all(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA
    melacic_out <- agree(vapply(kinds, `[`, NA, 1L)); melanic_out <- agree(vapply(kinds, `[`, NA, 2L))
    agree(vapply(kinds, function(k) .any3(k), NA))
  }
  list(humose = humose, dark = dark, melacic = melacic_out, melanic = melanic_out)
}

#' A peaty, humose, melacic or melanic horizon (Chernic suborders)
#' @noRd
chernic_horizon_asc <- function(pedon) {
  p <- .asc_chernic_parts(pedon)
  passed <- .any3(c(p$peaty, p$humose, p$dark))
  .asc_result("chernic_horizon", passed, integer(0),
              list(peaty = p$peaty, humose = p$humose, melacic_or_melanic = p$dark),
              if (is.na(passed)) c("oc_pct", "munsell_value_moist", "munsell_chroma_moist", "munsell_value_dry",
                                   "ph_h2o", "structure_grade", "ped_size_max_mm") else character(0),
              "Suborders Chernic; glossary 'Peaty horizon', 'Humose horizon', 'Melacic horizon', 'Melanic horizon'")
}

# ---- the conspicuously bleached A2 horizon -----------------------------------------

# Each A2 horizon (an E of the FAO or USDA notation included): conspicuously
# bleached when 80 % or more of it is bleached (glossary): dry value 7 or
# more and chroma 4 or less, or value 6 or more and chroma 4 or less where an
# adjacent horizon has a hue of 5YR or redder; more than 20 % mottles leave
# less than 80 % bleached. In the Australian notation the Field Handbook marks
# it with an e, and a sporadically bleached one with a j (not conspicuous);
# an A2 with neither is read by its dry colour, undecided without it (the
# glossary defines the bleaching by the dry colour, not by the notation).
.asc_bleached_rows <- function(x) {
  h <- x$h; n <- x$n
  aus <- isTRUE(x$des$notation$ok)
  j <- x$des$j
  dv <- suppressWarnings(as.numeric(h$munsell_value_dry)); dc <- suppressWarnings(as.numeric(h$munsell_chroma_dry))
  hue <- ifelse(is.na(h$munsell_hue_dry), as.character(h$munsell_hue_moist), as.character(h$munsell_hue_dry))
  # 5YR or redder: TRUE or FALSE when every way of matching the hue agrees;
  # a neutral hue is not; an unrecorded or unreadable hue is unknown
  red <- vapply(hue, function(v) {
    r <- .asc_hue_redder(v)
    if (!length(r)) NA else if (length(r) == 1L && is.na(r)) FALSE else if (all(r)) TRUE else if (!any(r)) FALSE else NA
  }, NA, USE.NAMES = FALSE)
  ord <- order(h$top_cm, na.last = NA)
  adj_red <- rep(FALSE, n)
  for (k in seq_along(ord)) {
    nb <- c(if (k > 1L) ord[k - 1L], if (k < length(ord)) ord[k + 1L])
    adj_red[ord[k]] <- if (any(red[nb] %in% TRUE)) TRUE else if (all(red[nb] %in% FALSE)) FALSE else NA
  }
  pale7 <- !is.na(dv) & !is.na(dc) & dv >= 7 & dc <= 4
  pale6 <- !is.na(dv) & !is.na(dc) & dv >= 6 & dc <= 4
  by_col <- ifelse(is.na(dv) | is.na(dc), NA,
                   ifelse(pale7 | (pale6 & adj_red %in% TRUE), TRUE,
                          ifelse(pale6 & is.na(adj_red), NA, FALSE)))
  # 80 % or more bleached: a bleached matrix with more than 20 % mottles is
  # not; with mottles not recorded it is undecided (as for the Hydrosols)
  mott <- h$redoximorphic_features_pct %||% rep(NA_real_, n)
  by_col[by_col %in% TRUE & !is.na(mott) & mott > 20] <- FALSE
  by_col[by_col %in% TRUE & is.na(mott)] <- NA
  row <- if (aus) ifelse(x$des$e, TRUE, ifelse(j, FALSE, by_col)) else by_col
  list(row = row, colour = by_col)
}

#' A conspicuously bleached A2 horizon (Bleached suborders)
#' @noRd
bleached_a2_asc <- function(pedon) {
  x <- .asc_horizons(pedon)
  bl <- .asc_bleached_rows(x); b <- bl$row
  a2 <- x$role %in% "A2" & !x$des$buried
  # a row of unknown role among the designated ones may be an A2: by its
  # dry colour; without designations any mineral row may be one
  maybe <- if (x$any_known) which(x$maybe_soil & x$unknown_role) else which(x$mineral & !x$des$buried)
  v <- b[a2]; mb <- bl$colour[maybe]
  passed <- if (any(v %in% TRUE)) TRUE
            else if (anyNA(v) || any(mb %in% TRUE) || anyNA(mb)) NA else FALSE
  .asc_result("bleached_a2", passed, which(a2 & b %in% TRUE), list(a2_rows = which(a2)),
              if (is.na(passed)) c("designation", "munsell_value_dry", "munsell_chroma_dry") else character(0),
              "Suborders Bleached; glossary 'Conspicuously bleached A2 horizon'")
}

# ---- hard materials within 0.5 m ----------------------------------------------------

# A calcrete pan, hard unweathered rock or other hard materials, or partially
# weathered or decomposed rock or saprolite, with its top within 0.5 m of the
# surface (Leptic suborders): an R horizon, a hard layer (moderately or more
# cemented, or marked m), or a C horizon whose origin is weathered rock or
# saprolite. A C horizon of unrecorded origin, or a D horizon, may be either
# saprolite or unconsolidated material, which does not count; a C of
# sedimentary origin does not. With none of them, the soil is not leptic when
# the description reaches 0.5 m, and undecided when it stops above.
leptic_asc <- function(pedon, depth = 50) {
  x <- .asc_horizons(pedon); h <- x$h
  top0 <- .asc_profile_top(x)
  if (is.na(top0)) return(.asc_result("leptic", NA, integer(0), list(), "top_cm", "Suborders Leptic"))
  within <- !is.na(h$top_cm) & h$top_cm <= top0 + depth & !x$des$buried
  origin <- tolower(paste(h$layer_origin %||% rep(NA, x$n), h$rock_origin %||% rep(NA, x$n)))
  sap <- grepl("saprolit|saprock|weathered rock|decomposed rock|residual|in situ", origin)
  sed <- grepl("alluv|fluvi|aeolian|aeolic|eolian|colluv|marine|lacustrine|sediment|dune|transported", origin)
  c_role <- x$role %in% c("C", "D", "C/B")
  hard <- x$hard | x$role %in% "R"
  c_state <- ifelse(c_role & sap, TRUE, ifelse(c_role & sed & !sap, FALSE, ifelse(c_role, NA, FALSE)))
  hit <- within & (hard | c_state %in% TRUE)
  open_c <- within & is.na(c_state) & !hard
  deepest <- suppressWarnings(max(h$bottom_cm[!x$des$buried], na.rm = TRUE))
  passed <- if (any(hit)) TRUE else if (any(open_c)) NA
            else if (is.finite(deepest) && deepest > top0 + depth) FALSE else NA
  .asc_result("leptic", passed, which(hit), list(depth_cm = depth, described_to_cm = deepest),
              if (is.na(passed)) c("designation", "cementation_class", "layer_origin", "bottom_cm") else character(0),
              "Suborders Leptic (calcrete pan, rock or saprolite within 0.5 m)")
}

# ---- salinity, gypsum, water table ------------------------------------------------

# 'Highly saline' (EC 1:5 above 2 dS/m): from ec_1to5_dS_m over the major
# part, by thickness, of the measured horizons of the upper 1.0 m ("high"
# when above 2, "low" when below, NA otherwise). With no EC, a soil whose
# site records a saline surface (salt crust) is undecided and any other is
# read as not highly saline: high salinity shows at the site.
.asc_salinity <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  ec <- h$ec_1to5_dS_m %||% rep(NA_real_, x$n)
  top0 <- .asc_profile_top(x)
  t0 <- top0 %|NA|% 0
  rows <- which(!is.na(ec) & !is.na(h$top_cm) & !is.na(h$bottom_cm) & h$top_cm < t0 + 100 & !x$des$buried)
  if (length(rows)) {
    hi <- .asc_overlap_major(x, rows, ec > 2, t0, t0 + 100)
    lo <- .asc_overlap_major(x, rows, ec < 2, t0, t0 + 100)
    return(if (isTRUE(hi)) "high" else if (isTRUE(lo)) "low" else NA_character_)
  }
  surf <- .asc_site_text(pedon$site$surface_condition)
  saline <- isTRUE(as.logical(pedon$site$salinised %||% NA)[1L]) ||
    (grepl("\\bsalin|\\bsalt", surf) && !grepl("non[- ]?salin|not salin|no salt", surf))
  if (saline) NA_character_ else "low"
}

# Dominantly gypsum crystals: caso4_pct over 50 % in the major part of the
# profile (the upper 1.0 m); 50 % CaCO3 or more rules a horizon out; no
# gypsum recorded is read as absent (a soil made of gypsum crystals shows).
.asc_gypsum_soil <- function(x) {
  h <- x$h
  g <- h$caso4_pct %||% rep(NA_real_, x$n); lab_c <- h$caco3_pct %||% rep(NA_real_, x$n)
  ok <- ifelse(!is.na(g), g > 50, ifelse(!is.na(lab_c) & lab_c >= 50, FALSE, .asc_unrecorded_site()))
  t0 <- .asc_profile_top(x) %|NA|% 0
  rows <- which(x$mineral & !x$des$buried & !is.na(h$top_cm) & !is.na(h$bottom_cm) & h$top_cm < t0 + 100)
  if (!length(rows)) NA else .asc_overlap_major(x, rows, ok, t0, t0 + 100)
}

#' Hypergypsic (dominantly gypsum crystals, not highly saline) and
#' Hypersalic (highly saline, no water table, no mottling) suborders of the
#' Arenosols and Rudosols
#' @noRd
saline_suborder_asc <- function(pedon, kind = c("hypergypsic", "hypersalic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h
  sal <- .asc_salinity(pedon)
  passed <- if (kind == "hypergypsic") {
    .and3(.asc_gypsum_soil(x), if (is.na(sal)) NA else sal == "low")
  } else {
    high <- if (is.na(sal)) NA else sal == "high"
    # a permanent or seasonal water table: saturation recorded, or a poorly
    # or very poorly drained site; moderately well drained or better has none
    days <- h$water_saturation_days %||% rep(NA_real_, x$n)
    d <- pedon$site$drainage_class %||% NA
    wet_end <- .drainage_class(d, "wetter"); dry_end <- .drainage_class(d, "drier")
    wt <- if (any(days > 0, na.rm = TRUE)) TRUE
          else if (!is.na(dry_end) && dry_end >= 7) TRUE
          else if (length(d) == 1L && !is.na(d) && grepl("^\\s*rapid", tolower(as.character(d)))) FALSE
          else if (!is.na(wet_end) && wet_end <= 5) FALSE else NA
    # evidence of episodic wetting: mottles recorded
    m <- h$redoximorphic_features_pct %||% rep(NA_real_, x$n)
    mott <- if (any(m > 0, na.rm = TRUE)) TRUE else if (all(!is.na(m[x$mineral]))) FALSE else NA
    .and3(high, !wt, !mott)
  }
  .asc_result(paste0("saline_", kind), passed, integer(0), list(salinity = sal),
              if (is.na(passed)) c("ec_1to5_dS_m", "caso4_pct", "site$drainage_class", "redoximorphic_features_pct")
              else character(0),
              sprintf("Suborders %s (Arenosols, Rudosols)", if (kind == "hypergypsic") "Hypergypsic" else "Hypersalic"))
}

# ---- stratification and gravel ------------------------------------------------------

# Distinct sedimentary layers or buried soils, from the designations: a
# lithologic discontinuity (a numeric prefix) or a buried horizon in the
# profile. Without designations, undecided.
.asc_stratified <- function(x) {
  if (!x$any_known) return(NA)
  pre <- x$des$prefix
  any(x$des$buried) || any(nzchar(pre) & pre != "1")
}

# Gravelly (more than 10 % coarse fragments) in some layer; not or only
# slightly gravelly (less than 10 %) in every layer. A missing coarse
# fragment record is read as none, as for the Arenosols.
.asc_gravel <- function(x) {
  h <- x$h
  rows <- which(x$mineral & !x$des$buried)
  cf <- ifelse(is.na(h$coarse_fragments_pct[rows]), 0, h$coarse_fragments_pct[rows])
  list(gravelly = any(cf > 10), slight = all(cf < 10))
}

# Loose or only weakly coherent, both moist and dry: a single-grain
# structure; or a recorded strength of weak or less both dry and moist. A
# strength of firm or more when dry or moist is coherent; one state of weak
# or less with the other unrecorded is undecided.
.asc_loose_rows <- function(x) {
  h <- x$h; n <- x$n
  weak <- c("loose", "very weak", "weak")
  sd <- tolower(as.character(h$strength_dry %||% rep(NA_character_, n)))
  sm <- tolower(as.character(h$strength_moist %||% rep(NA_character_, n)))
  single <- grepl("single", tolower(paste(h$structure_type, h$structure_grade)))
  firm <- (!is.na(sd) & !sd %in% weak & nzchar(sd)) | (!is.na(sm) & !sm %in% weak & nzchar(sm))
  ifelse(single & !firm, TRUE, ifelse(firm, FALSE, ifelse(sd %in% weak & sm %in% weak, TRUE, NA)))
}

#' Suborders on unconsolidated material: Arenic, Lutic and Clastic Rudosols,
#' Stratic Rudosols and Arenosols
#' @noRd
material_suborder_asc <- function(pedon, kind = c("arenic", "lutic", "stratic", "clastic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h
  rows <- which(x$mineral & !x$des$buried)
  g <- .asc_gravel(x)
  strat <- .asc_stratified(x)
  sandy <- .asc_is_sandy(h)
  loose <- .asc_loose_rows(x)
  every <- function(v) if (any(v %in% FALSE)) FALSE else if (all(v %in% TRUE)) TRUE else NA
  passed <- switch(kind,
    arenic = {
      dom_sandy <- .asc_rows_major(x, rows, sandy)
      no15 <- every(!.asc_over15(h)[rows])
      .and3(g$slight, !strat, dom_sandy, no15, every(loose[rows]))
    },
    lutic = {
      # sandy material must be coherent
      coh <- every(ifelse(sandy[rows] %in% FALSE, TRUE, ifelse(sandy[rows] %in% TRUE, !loose[rows], NA)))
      .and3(g$slight, !strat, coh)
    },
    stratic = {
      sal <- .asc_salinity(pedon)
      .and3(g$slight, strat, if (is.na(sal)) NA else sal == "low")
    },
    clastic = g$gravelly)
  .asc_result(paste0("rudosol_", kind), passed, rows, list(stratified = strat, gravelly = g$gravelly),
              if (is.na(passed)) c("designation", "coarse_fragments_pct", "field_texture", "strength_dry",
                                   "strength_moist", "structure_type") else character(0),
              sprintf("Rudosols, suborder %s", tools::toTitleCase(kind)))
}

# ---- materials named by their origin ------------------------------------------------

# The major part of a window of the profile whose layer origin names a
# material; with no such record the material is read as absent (shell
# deposits, carbic debris and fusic ash show in the field).
.asc_origin_major <- function(x, pattern, from, to) {
  h <- x$h
  origin <- tolower(as.character(h$layer_origin %||% rep(NA_character_, x$n)))
  ok <- grepl(pattern, origin)
  rows <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & h$bottom_cm > from & h$top_cm < to & !x$des$buried & !x$rock)
  if (!length(rows)) return(FALSE)
  zb <- min(to, max(h$bottom_cm[rows]))
  .asc_major_part(x, rows, ok, from, zb)
}

#' Shelly and Carbic Rudosols
#' @noRd
rudosol_origin_asc <- function(pedon, kind = c("shelly", "carbic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon)
  top0 <- .asc_profile_top(x)
  passed <- if (is.na(top0)) NA else if (kind == "shelly") {
    .asc_origin_major(x, "shell|coquina|midden", top0, top0 + 1000)
  } else {
    .asc_origin_major(x, "carbic|charcoal|carboni[sz]ed|burnt plant|bushfire debris", top0, top0 + 50)
  }
  .asc_result(paste0("rudosol_", kind), passed, integer(0), list(kind = kind),
              if (is.na(passed)) "layer_origin" else character(0),
              sprintf("Rudosols, suborder %s%s", tools::toTitleCase(kind),
                      if (kind == "carbic") "; glossary 'Carbic materials'" else ""))
}

#' Anthroposol suborders: the kind of human-made material
#'
#' Read from the layer origin of the anthropic layers (and urbic
#' artefacts): Fusic (burnt peat ash, fusic material; 0.3 m or more),
#' Cumulic (mill mud, middens, shells; 0.3 m or more), Hortic (composts,
#' mulches, organic wastes), Garbic (landfill of organic refuse), Urbic
#' (fill of manufactured materials; urbic artefacts alone do not tell it
#' from Spolic, which may hold a few), Dredgic (dredged or slurried
#' material, tailings), Spolic (earth moved by machinery, mine spoil),
#' Scalpic (a surface cut by machinery). TRUE when the origin names the
#' kind, FALSE when it names others only, undecided when it names none.
#' @noRd
anthroposol_kind_asc <- function(pedon, kind = c("fusic", "cumulic", "hortic", "garbic", "urbic", "dredgic",
                                                 "spolic", "scalpic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h
  origin <- tolower(as.character(h$layer_origin %||% rep(NA_character_, x$n)))
  pats <- c(fusic = "fusic|burnt peat|peat ash", cumulic = "cumulic|mill.?mud|midden|shell",
            hortic = "hortic|compost|mulch|organic waste|sewage sludge|manure",
            garbic = "garbic|landfill|refuse|garbage|domestic waste|putrescible",
            urbic = "urbic|rubble|demolition|building waste|construction waste|industrial waste",
            dredgic = "dredg|tailings|slurry|coal wash", spolic = "spolic|spoil|mine waste|overburden|earthmov",
            scalpic = "scalpic|scalped|cut surface|truncat")
  named <- vapply(pats, function(p) grepl(p, origin), logical(x$n))
  if (!is.matrix(named)) named <- matrix(named, nrow = x$n, dimnames = list(NULL, names(pats)))
  mine <- named[, kind]; any_kind <- rowSums(named) > 0
  # Fusic: surface layers 0.3 m or more; Cumulic: a deposit 0.3 m or more
  thick_ok <- if (kind == "fusic") .asc_run_at_least(.asc_run_from(x, mine, .asc_profile_top(x) %|NA|% 0), 30)
              else if (kind == "cumulic") { th <- x$thick[mine]
                if (sum(th, na.rm = TRUE) >= 30) TRUE else if (anyNA(th)) NA else FALSE }
              else TRUE
  passed <- if (any(mine)) thick_ok else if (any(any_kind)) FALSE else NA
  .asc_result(paste0("anthroposol_", kind), passed, which(mine), list(kind = kind),
              if (is.na(passed)) "layer_origin" else character(0),
              sprintf("Anthroposols, suborder %s", tools::toTitleCase(kind)))
}

# ---- carbonate ---------------------------------------------------------------------

#' Carbonatic and Calsilic Arenosols
#'
#' Carbonatic: calcareous throughout the upper 1.0 m of the profile, and
#' more than 40 % fine-earth CaCO3 (laboratory) in the major part of its
#' upper 0.1 m (a slight or moderate effervescence there rules it out).
#' Calsilic: calcareous throughout the upper 1.0 m below the A1 or Ap or
#' below 0.2 m, whichever is shallower ("with the possible exception of the
#' surface").
#' @noRd
arenosol_carbonate_asc <- function(pedon, kind = c("carbonatic", "calsilic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h
  top0 <- .asc_profile_top(x)
  calc <- .asc_calcareous(x)
  rows_in <- function(a, b) which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & h$bottom_cm > a + 0.01 & h$top_cm < b &
                                    !x$des$buried & x$mineral)
  passed <- if (is.na(top0)) NA else if (kind == "carbonatic") {
    thr <- .asc_every_in(x, calc, top0, top0 + 100)
    t10 <- rows_in(top0, top0 + 10)
    eff <- tolower(as.character(h$effervescence_class %||% rep(NA_character_, x$n)))
    lab <- h$caco3_pct %||% rep(NA_real_, x$n)
    rich_row <- ifelse(!is.na(lab), lab > 40, ifelse(grepl("^(none|non|nil|slight|moderate)", eff), FALSE, NA))
    rich <- if (!length(t10)) NA else .asc_major_part(x, t10, rich_row, top0, top0 + 10)
    .and3(thr, rich)
  } else {
    # below the A1 or Ap at the surface, or 0.2 m, whichever is shallower
    ord <- order(h$top_cm, na.last = NA); a_bot <- NA_real_
    if (x$any_known) for (i in ord) {
      if (h$top_cm[i] < top0 - 0.5) next
      if (!(x$role[i] %in% "A1")) break
      a_bot <- h$bottom_cm[i]
    }
    d <- if (is.na(a_bot)) top0 + 20 else min(a_bot, top0 + 20)
    .asc_every_in(x, calc, d, top0 + 100)
  }
  .asc_result(paste0("arenosol_", kind), passed, integer(0), list(kind = kind),
              if (is.na(passed)) c("effervescence_class", "caco3_pct", "secondary_carbonates_pct") else character(0),
              sprintf("Arenosols, suborder %s; glossary 'Calcareous'", tools::toTitleCase(kind)))
}

#' Calcenic Tenosols: a horizon at least 0.2 m thick with more than 20 %
#' fine-earth carbonate (laboratory CaCO3, or more than 20 % soft carbonate
#' segregations); a horizon that is not calcareous has none.
#' @noRd
calcenic_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  c <- .asc_carbonate(h)
  calc <- .asc_calcareous(x)
  lab <- h$caco3_pct %||% rep(NA_real_, x$n)
  # the laboratory decides; soft segregations over 20 % pass; less in
  # segregations says nothing of the carbonate of the fine earth
  ok <- ifelse(!is.na(lab), lab > 20, ifelse(!is.na(c$soft_visual) & c$soft_visual > 20, TRUE,
                                             ifelse(calc %in% FALSE, FALSE, NA)))
  ok[x$des$buried | !x$mineral] <- FALSE
  r <- .cssc_run_test(x, ok, 20)
  .asc_result("calcenic", r$passed, r$rows, list(),
              if (is.na(r$passed)) c("caco3_pct", "carbonate_soft_pct", "effervescence_class") else character(0),
              "Tenosols, suborder Calcenic")
}

#' Sesqui-Nodular Tenosols: a ferric or bauxitic horizon (more than 20 %
#' ferruginous or bauxite nodules or concretions, mostly uncemented) at least
#' 0.2 m thick that occupies more than half of the depth of the solum
#' (cemented layers left out of it).
#' @noRd
sesqui_nodular_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  fe <- h$ferric_nodules_pct %||% rep(NA_real_, x$n); bx <- h$bauxitic_nodules_pct %||% rep(NA_real_, x$n)
  # nodules not recorded are absent (more than 20 % of them shows; since
  # 0.9.285); an amount the record leaves on either side of 20 % is undecided
  one <- function(v) .asc_recorded_over(v, 20)
  ok <- .any3v(one(fe), one(bx))
  ok[x$hard | x$des$buried | !x$mineral] <- FALSE
  # within the solum (cemented layers left out); rows of unknown role in the
  # present soil may belong to it
  sol_sure <- if (x$any_known) x$solum %in% TRUE & !x$hard else x$mineral & !x$hard & !x$des$buried
  sol_maybe <- if (x$any_known) x$maybe_soil & !x$hard & !sol_sure else rep(FALSE, x$n)
  # a row of unknown role between two solum horizons is solum (as for the order)
  sw <- .asc_sandwiched(x, which(sol_maybe), which(sol_sure))
  sol_sure[sw] <- TRUE; sol_maybe[sw] <- FALSE
  ok[!(sol_sure | sol_maybe)] <- FALSE
  ok[sol_maybe & ok %in% TRUE] <- NA
  r <- .cssc_run_test(x, ok, 20)
  th <- x$thick
  depth_lo <- if (anyNA(th[sol_sure])) NA else sum(th[sol_sure])
  depth_hi <- depth_lo + sum(ifelse(is.na(th[sol_maybe]), Inf, th[sol_maybe]))
  # the thickest run that certainly qualifies, and the one that may
  runs <- .cssc_run_groups(x, ok %in% TRUE); maybe_runs <- .cssc_run_groups(x, !(ok %in% FALSE))
  span <- function(rr) if (!length(rr)) 0 else max(vapply(rr, function(g) { s <- .cssc_run_span(x, g); if (is.na(s)) Inf else s }, 0))
  sure <- span(runs); poss <- span(maybe_runs)
  passed <- if (is.na(depth_lo) || depth_lo <= 0) { if (poss < 20) FALSE else NA }
            else if (sure >= 20 && sure > depth_hi / 2) TRUE
            else if (poss < 20 || poss <= depth_lo / 2) FALSE else NA
  .asc_result("sesqui_nodular", passed, r$rows, list(solum_cm = depth_lo, nodular_cm = sure),
              if (is.na(passed)) c("ferric_nodules_pct", "bauxitic_nodules_pct") else character(0),
              "Tenosols, suborder Sesqui-Nodular; glossary 'Ferric horizon', 'Bauxitic horizon'")
}

.any3v <- function(a, b) ifelse(a %in% TRUE | b %in% TRUE, TRUE, ifelse(a %in% FALSE & b %in% FALSE, FALSE, NA))
