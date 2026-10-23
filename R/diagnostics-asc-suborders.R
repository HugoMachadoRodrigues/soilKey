# =============================================================================
# v0.9.280 -- suborders of the Australian Soil Classification (3rd ed.) for
# the orders whose suborders are colour classes: Kurosols, Sodosols and
# Chromosols (upper 0.2 m of the B2t), Ferrosols, Dermosols and Kandosols
# (upper 0.5 m of the B2), and Vertosols (Aquic first, then the colour of the
# upper 0.5 m of the solum). Three-valued, as the order key; the readings are
# in inst/rules/asc3/readings.yaml.
# =============================================================================

.ASC_COLOUR_CLASSES <- c("Red", "Brown", "Yellow", "Grey", "Black")

# Is a hue "5YR or redder"? TRUE or FALSE for each way the hue may be matched
# to a chip: the hue number goes to the nearest chip of its family (2.5, 5,
# 7.5, 10), both when it lies half way. R, RP and P hues are redder; Y, GY,
# G, BG, B and PB hues are yellower. A neutral hue (N) returns NA: it has no
# chroma, so the classes do not depend on it.
.asc_hue_redder <- function(hue) {
  p <- .munsell_hue_parts(hue)
  fam <- p$family
  if (is.na(fam)) return(logical(0))
  if (fam == "N") return(NA)
  if (fam %in% c("R", "RP", "P")) return(TRUE)
  if (fam != "YR") return(FALSE)
  n <- p$number
  if (is.na(n)) return(logical(0))
  lo <- 2.5 * floor(n / 2.5); hi <- lo + 2.5
  chips <- if (abs(n - lo) < 1e-9) lo else if (n - lo < hi - n) lo else if (n - lo > hi - n) hi else c(lo, hi)
  unique(chips <= 5)
}

# The chips a Munsell value or chroma may be matched to: the nearest whole
# number (value 2.5 also has chips), both when it lies half way.
.asc_munsell_steps <- function(v, chips_half = FALSE) {
  if (is.na(v)) return(numeric(0))
  chips <- if (chips_half) sort(c(0:10, 2.5)) else 0:30
  d <- abs(chips - v)
  chips[abs(d - min(d)) < 1e-9]
}

# The colour class of one moist colour (Colour classes, Fig. 3): NA when the
# colour is missing or when the ways it may be matched to a chip fall in
# different classes.
.asc_colour_class1 <- function(hue, value, chroma) {
  red <- .asc_hue_redder(hue)
  neutral <- length(red) == 1L && is.na(red)
  if (neutral) chroma <- 0                  # a neutral hue has no chroma
  if (!length(red)) {
    # an unreadable hue: Black and Grey (chroma 2 or less) hold for all hues
    cs <- .asc_munsell_steps(chroma)
    if (!length(cs) || any(cs > 2)) return(NA_character_)
    red <- NA; neutral <- TRUE
  }
  vs <- .asc_munsell_steps(value, chips_half = TRUE); cs <- .asc_munsell_steps(chroma)
  if (!length(vs) || !length(cs)) return(NA_character_)
  out <- character(0)
  for (r in red) for (v in vs) for (k in cs) {
    cl <- if (k <= 2) { if (v <= 3) "Black" else "Grey" }
          else if (!neutral && isTRUE(r)) "Red"
          else if (v <= 5) "Brown"
          else if (k >= 4) "Yellow"
          else "Grey"          # yellower than 5YR, value 6 or more, chroma 3
    out <- c(out, cl)
  }
  out <- unique(out)
  if (length(out) == 1L) out else NA_character_
}

# The colour class of each horizon, from its moist matrix colour.
.asc_colour_classes <- function(h) {
  n <- nrow(h)
  hue <- as.character(h$munsell_hue_moist %||% rep(NA_character_, n))
  val <- suppressWarnings(as.numeric(h$munsell_value_moist %||% rep(NA_real_, n)))
  chr <- suppressWarnings(as.numeric(h$munsell_chroma_moist %||% rep(NA_real_, n)))
  vapply(seq_len(n), function(i) .asc_colour_class1(hue[i], val[i], chr[i]), "")
}

# A depth window: from the top of the first row of `cand` at or below
# `from_top`, down through the rows of `cand` until `depth` cm, the bottom of
# the last of them, or the first other row that starts below (a BC, a C, a
# row of unknown role tried out of the horizon), whichever comes first. A
# gap in the record does not end it (it is a part of unknown value), and a
# row with no bottom leaves it open to `depth`. `fallback` stands in when no
# row of `cand` qualifies.
.asc_window <- function(x, cand, from_top, depth, fallback = integer(0)) {
  h <- x$h; top <- h$top_cm; bot <- h$bottom_cm
  rows <- which(cand & !is.na(top) & top >= from_top - 0.5)
  if (!length(rows)) { rows <- fallback; cand <- cand; cand[fallback] <- TRUE }
  if (!length(rows)) return(NULL)
  zt <- min(top[rows])
  ord <- order(top, na.last = NA)
  ord <- ord[top[ord] >= zt - 0.5 & top[ord] < zt + depth]
  keep <- integer(0); end <- zt; open_end <- FALSE
  for (r in ord) {
    if (!cand[r]) { if (top[r] > zt + 0.5) break else next }
    keep <- c(keep, r)
    if (is.na(bot[r])) { open_end <- TRUE; break }
    end <- max(end, bot[r])
  }
  zb <- if (open_end) zt + depth else min(zt + depth, end)
  list(rows = keep, top = zt, bottom = zb)
}

# The verdict for `colour` in one window: TRUE when it is the dominant
# class, FALSE when it cannot be, NA otherwise. The dominant class covers
# more than half of the window; when no class can, it is the one with the
# largest share, whatever the parts of unknown colour (horizons without a
# colour class, and parts of the window no horizon covers) turn out to be. A
# tie leaves it undecided.
.asc_colour_verdict <- function(x, w, cls, colour) {
  if (is.null(w)) return(NA)
  h <- x$h; rows <- w$rows; total <- w$bottom - w$top
  if (!is.finite(total) || total <= 0) return(NA)
  # the window cut at every row boundary: a segment takes the class of the
  # rows covering it when they all have one and agree; otherwise (no row, a
  # row of unknown class, rows that disagree) its colour is unknown
  tops <- h$top_cm[rows]; bots <- h$bottom_cm[rows]
  cuts <- sort(unique(c(w$top, w$bottom, tops[tops > w$top & tops < w$bottom],
                        bots[!is.na(bots) & bots > w$top & bots < w$bottom])))
  share <- setNames(numeric(length(.ASC_COLOUR_CLASSES)), .ASC_COLOUR_CLASSES)
  for (k in seq_len(length(cuts) - 1L)) {
    a <- cuts[k]; b <- cuts[k + 1L]; mid <- (a + b) / 2
    over <- rows[!is.na(tops) & !is.na(bots) & tops <= mid & bots > mid]
    cl <- unique(cls[over])
    if (length(over) && length(cl) == 1L && !is.na(cl)) share[[cl]] <- share[[cl]] + (b - a)
  }
  unk <- max(0, total - sum(share))
  half <- total / 2
  me <- share[[colour]]; others <- share[names(share) != colour]
  if (any(share > half)) return(share[[colour]] > half)
  # the class wins whatever the unknown parts are (no other class can reach
  # the major part, nor the largest share)
  if (all(me > others + unk)) return(TRUE)
  if (any(share + unk > half)) {
    # a class may still reach the major part
    if (!(me + unk > half) && any(others > me + unk)) return(FALSE)
    return(NA)
  }
  if (any(others > me + unk)) return(FALSE)
  NA
}

#' Colour suborders of the Australian Soil Classification
#'
#' The dominant moist colour class of a window: \code{"upper_b2t"}, the upper
#' 0.2 m of the B2t below the clear or abrupt textural B (Kurosols, Sodosols,
#' Chromosols); \code{"b2_50"}, the upper 0.5 m of the B2 (Ferrosols,
#' Dermosols, Kandosols); \code{"solum_50"}, the upper 0.5 m of the solum
#' (Vertosols); \code{"profile_100"} and \code{"profile_50"}, the upper 1.0 m
#' (Arenosols) or 0.5 m (Tenosols) of the profile; each, or the whole of it
#' when it is thinner. Each horizon is
#' placed in a class by its moist matrix colour (\code{.asc_colour_class1}).
#' The dominant class covers the major part (more than half) of the window;
#' when no class does, the one with the largest share, a tie being undecided
#' (\code{.asc_colour_verdict}). Rows of unknown role that may belong to the
#' horizons of the window are tried in and out of it.
#' @noRd
colour_suborder_asc <- function(pedon, colour = .ASC_COLOUR_CLASSES,
                                window = c("upper_b2t", "b2_50", "solum_50", "profile_100", "profile_50")) {
  colour <- match.arg(colour); window <- match.arg(window)
  x <- .asc_horizons(pedon); h <- x$h
  cls <- .asc_colour_classes(h)
  name <- paste0("colour_", tolower(colour))
  where <- sprintf("Suborders, colour class (%s); 'Colour classes'", window)
  undecided <- function(ev) .asc_result(name, NA, integer(0), ev,
                                        c("munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist"), where)
  if (window == "upper_b2t") {
    tb <- textural_b_asc(pedon)
    if (!isTRUE(tb$passed)) return(undecided(list(textural_b = tb$passed)))
    b_row <- tb$layers[1L]; from <- h$top_cm[b_row]
    maybe <- .asc_b2_maybe(x)
    maybe <- maybe[!is.na(h$top_cm[maybe]) & h$top_cm[maybe] >= from - 0.5]
    t0 <- suppressWarnings(min(h$top_cm[x$b2 %in% TRUE & h$top_cm >= from - 0.5], na.rm = TRUE))
    if (is.finite(t0)) maybe <- maybe[h$top_cm[maybe] < t0 + 20]
    wf <- function(sub) { cand <- x$b2 %in% TRUE; cand[sub] <- TRUE; .asc_window(x, cand, from, 20, b_row) }
  } else if (window == "b2_50") {
    rows <- .asc_b2_rows(x)
    if (is.null(rows)) return(undecided(list(b2 = NA)))
    if (!length(rows)) return(.asc_result(name, FALSE, integer(0), list(b2 = FALSE), character(0), where))
    maybe <- .asc_b2_maybe(x)
    t0 <- min(h$top_cm[rows], na.rm = TRUE)
    maybe <- maybe[!is.na(h$top_cm[maybe]) & h$top_cm[maybe] < t0 + 50]
    wf <- function(sub) { cand <- x$b2 %in% TRUE; cand[sub] <- TRUE; .asc_window(x, cand, -Inf, 50) }
  } else if (window %in% c("profile_100", "profile_50")) {
    # the soil profile from its top (an O1 litter left out), above rock and
    # any buried soil: the upper 1.0 m (Arenosols) or 0.5 m (Tenosols), or
    # the whole of it when thinner
    depth <- if (window == "profile_100") 100 else 50
    prof <- !x$des$buried & !x$rock & !(x$role %in% "O1")
    if (!any(prof)) return(undecided(list(profile = NA)))
    maybe <- integer(0)
    wf <- function(sub) .asc_window(x, prof, -Inf, depth)
  } else {
    # without designations, the mineral rows (as the order key reads the solum)
    sol <- if (x$any_known) x$solum %in% TRUE else x$mineral
    if (!any(sol)) return(undecided(list(solum = NA)))
    t0 <- min(h$top_cm[sol], na.rm = TRUE)
    # without designations every mineral row is read as solum (none is tried out)
    maybe <- if (x$any_known) which(x$maybe_soil) else integer(0)
    maybe <- maybe[!is.na(h$top_cm[maybe]) & h$top_cm[maybe] < t0 + 50]
    wf <- function(sub) { cand <- sol; cand[sub] <- TRUE; .asc_window(x, cand, -Inf, 50) }
  }
  passed <- if (length(maybe) > 8L) NA else {
    subsets <- lapply(seq_len(2^length(maybe)) - 1L, function(k) maybe[bitwAnd(k, 2^(seq_along(maybe) - 1L)) > 0])
    ans <- vapply(subsets, function(sub) .asc_colour_verdict(x, wf(sub), cls, colour), NA)
    if (all(ans %in% TRUE)) TRUE else if (all(ans %in% FALSE)) FALSE else NA
  }
  w0 <- wf(integer(0))
  .asc_result(name, passed, w0$rows %||% integer(0),
              list(window = window, window_cm = c(w0$top, w0$bottom), classes = cls[w0$rows %||% integer(0)]),
              if (is.na(passed)) c("munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist")
              else character(0), where)
}

#' Aquic Vertosols (suborder)
#'
#' Water ponds on the surface or some part of the upper 0.5 m is saturated
#' for long periods (about 2-3 months) in most years: from
#' water_saturation_days of the horizons within 0.5 m (90 days or more in any
#' of them passes; fewer than 60 in all of them, with the whole 0.5 m
#' described, fails), otherwise from the site drainage class as for the
#' Hydrosols (very poorly drained passes, poorly drained is undecided,
#' imperfectly drained or better fail).
#' @noRd
aquic_vertosol_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  days <- h$water_saturation_days %||% rep(NA_real_, x$n)
  in50 <- !is.na(h$top_cm) & h$top_cm < 50
  by_days <- NA
  if (any(!is.na(days[in50]))) {
    covered <- sum(.asc_overlap(h$top_cm[in50], h$bottom_cm[in50], 0, 50), na.rm = TRUE)
    by_days <- if (any(days[in50] >= 90, na.rm = TRUE)) TRUE
               else if (all(!is.na(days[in50]) & days[in50] < 60) && covered >= 50) FALSE else NA
  }
  by_drain <- .asc_long_saturation(pedon)
  passed <- if (!is.na(by_days)) by_days else by_drain
  .asc_result("aquic_vertosol_asc", passed, integer(0),
              list(route = if (!is.na(by_days)) "saturation days" else "drainage class",
                   by_days = by_days, by_drainage = by_drain),
              if (is.na(passed)) c("water_saturation_days", "site$drainage_class") else character(0),
              "Vertosols, suborder Aquic")
}
