# =============================================================================
# v0.9.359 -- the South African soil classification of 1991, part 5: the
# families. A family is the second and last level of the system: each of the
# 73 forms has 2 to 16 families (404 in all), coded by the form code and four
# digits (Hu1200), one digit per family criterion the form uses, in the order
# the key prints them (inst/rules/za1991/families.yaml, written by
# data-raw/za1991_families.R).
#
# The 19 family criteria (FC 1-19) are read here: family_class_za() gives the
# class of a pedon under one criterion, on the horizon the family names (the
# B1 of the red apedal B, the E, the upper G, the layer under the placic pan
# ...), taken from the horizons the form key identified (.za_view(); nothing
# is identified again), read once per view and kept. A class not decided for
# lack of data is NA with the attributes that would decide it; the family
# level then gives the first family whose classes all hold, or a provisional
# result with the families left undecided, and the partial code with "x" for
# each undecided digit (Hu1x00, soilKey's notation, in brackets).
# =============================================================================


# --- the families file -----------------------------------------------------------------

# The families file, read once.
.za_families_file <- function() {
  if (is.null(.za_cache$families)) {
    path <- system.file("rules", "za1991", "families.yaml", package = "soilKey")
    if (!nzchar(path)) rlang::abort("The families of the South African forms are not installed.")
    .za_cache$families <- yaml::read_yaml(path)
  }
  .za_cache$families
}

# The classes of a criterion, in the order of its digit.
.za_fc_classes <- function(criterion) {
  cr <- .za_families_file()$family_criteria
  as.character(unlist(cr[[as.integer(criterion)]]$classes))
}

# soilKey's name of a criterion.
.za_fc_name <- function(criterion) .za_families_file()$family_criteria[[as.integer(criterion)]]$name


# --- the horizon a criterion reads ---------------------------------------------------------

#' The layers of the horizon a family criterion reads, in a view of the
#' form key: the first horizon of the sequence with that token (one of its
#' candidates, flagged, when the horizon is undecided), and the part of it
#' the family names (data-raw/za1991_families.R): whole, B1 (the layer
#' upper layer of the horizon; Q20, R5), upper, lower, below (the
#' first layer under it), below_all, with_below, with_next (the horizon and
#' the first layer of the horizon under it), profile (every layer).
#' @noRd
.za_fc_where <- function(view, horizon, part) {
  x <- view$x
  el <- view$elements
  # v0.9.359 review D6: a bound not recorded is written "?"
  lab <- function(ly) {
    if (!length(ly)) return("no layer")
    t <- x$top[ly]; b <- x$bot[ly]
    sprintf("%s-%s cm", if (anyNA(t)) "?" else format(min(t)), if (anyNA(b)) "?" else format(max(b)))
  }
  if (identical(horizon, "-") || part == "profile") {
    ly <- x$ord
    return(list(layers = ly, k = NA_integer_, element = NULL, what = sprintf("the profile within 1500 mm (%s)", lab(ly)),
                found = TRUE, candidate = FALSE, part = "profile", horizon = "-", has_below = TRUE))
  }
  k <- which(vapply(el, function(e) identical(e$codes, horizon), NA))
  cand <- FALSE
  if (!length(k)) {
    k <- which(vapply(el, function(e) horizon %in% e$codes, NA))
    cand <- length(k) > 0L
  }
  if (!length(k)) return(list(layers = integer(0), k = NA_integer_, element = NULL, found = FALSE, candidate = FALSE,
                              what = sprintf("no %s identified", .za_label(horizon))))
  k <- k[1L]
  e <- el[[k]]
  ly <- e$layers
  after <- function(i) { if (!length(i)) return(integer(0)); r <- max(x$rank[i]); x$ord[x$rank > r] }
  under <- if (length(ly)) after(ly) else {
    # a horizon supplied with no layer (a truncated profile's A): the layers
    # of the horizons below it
    unlist(lapply(el[seq_along(el) > k], `[[`, "layers"))
  }
  nm <- .za_label(horizon)
  out <- switch(part,
    whole = list(ly, sprintf("the %s", nm)),
    # the upper layer of the first B, as P defines the B1 (Q20; R5, Hugo,
    # 2026-11-06), whatever its designation
    B1 = list(utils::head(ly, 1L), sprintf("the B1 of the %s", nm)),
    upper = list(utils::head(ly, 1L), sprintf("the upper layer of the %s", nm)),
    lower = list(utils::tail(ly, 1L), sprintf("the lower layer of the %s", nm)),
    below = list(utils::head(under, 1L), sprintf("the layer under the %s", nm)),
    below_all = list(under, sprintf("the layers under the %s", nm)),
    with_below = list(c(ly, utils::head(under, 1L)), sprintf("the %s and the layer under it", nm)),
    from = list(c(ly, under), sprintf("the %s and every layer under it", nm)),
    with_next = {
      nx <- if (k < length(el)) el[[k + 1L]]$layers else integer(0)
      list(c(ly, utils::head(nx, 1L)), sprintf("the %s and the upper layer of the horizon under it", nm))
    },
    rlang::abort(sprintf("No part '%s' of a horizon.", part)))
  list(layers = as.integer(out[[1L]]), k = k, element = e, found = TRUE, candidate = cand,
       what = sprintf("%s (%s)", out[[2L]], lab(out[[1L]])), part = part, horizon = horizon,
       has_below = length(under) > 0L)
}

# A record of a criterion: the class (or NA), the classes still possible,
# and what was read.
.za_fc_rec <- function(class = NA_character_, candidates = character(0), missing = character(0), note = character(0),
                       tag = character(0)) {
  list(class = as.character(class), candidates = unique(as.character(candidates)),
       missing = if (is.na(class)) unique(as.character(missing)) else character(0),
       note = unique(as.character(note[!is.na(note)])), tag = unique(as.character(tag)))
}

# The class of a horizon of several layers from the classes of its layers
# (Hugo, 2026-11-06): the first of `order` that covers
# more than half of its thickness (red_greater_part), else the residual
# class when no class of `order` can still reach that; NA otherwise. Each
# layer gives list(class, missing, note, tag); a layer of unrecorded
# thickness weighs as much as each other one.
.za_fc_major <- function(x, layers, per_layer, order, residual, share = FALSE) {
  if (!length(layers)) return(.za_fc_rec(NA, c(order, residual), note = "no layer of the horizon described"))
  rs <- lapply(layers, per_layer)
  w <- x$thick[layers]
  if (anyNA(w) || !sum(w)) w <- rep(1, length(layers))
  cls <- vapply(rs, function(r) as.character(r$class), "")
  und <- is.na(cls)
  half <- sum(w) * .za_thv("red_greater_part") / 100
  notes <- unlist(lapply(rs, `[[`, "note")); tags <- unlist(lapply(rs, `[[`, "tag"))
  miss <- unlist(lapply(rs[und], `[[`, "missing"))
  several <- length(layers) > 1L
  for (c in order) {
    got <- sum(w[!und & cls == c])
    if (got > half)
      return(.za_fc_rec(c, note = c(notes, if (several) sprintf("%s over the greater part of the horizon", c)), tag = tags))
    if (got + sum(w[und]) > half)
      return(.za_fc_rec(NA, candidates = c(order[seq(match(c, order), length(order))], residual), missing = miss, note = notes, tag = tags))
  }
  # R6 (FC 5; Hugo, 2026-11-06): no class over the greater part, the class of
  # `order` with the larger share (the first on a tie)
  if (share) {
    got <- vapply(order, function(c) sum(w[!und & cls == c]), 0)
    if (any(got > 0)) {
      if (any(und)) return(.za_fc_rec(NA, candidates = c(order, residual), missing = miss, note = notes, tag = tags))
      c <- order[which.max(got)]
      return(.za_fc_rec(c, note = c(notes, sprintf("no class over the greater part: %s, the larger share (R6)", c)), tag = tags))
    }
  }
  .za_fc_rec(residual, note = c(notes, if (several) sprintf("no class of %s over the greater part: %s",
                                                            paste(order, collapse = ", "), residual)), tag = tags)
}

# A presence over layers (any layer TRUE: present; all FALSE: absent).
.za_fc_any <- function(rs, yes, no) {
  if (!length(rs)) return(.za_fc_rec(NA, c(no, yes), note = "no layer described"))
  r <- do.call(.rp_any, rs)
  if (isTRUE(r$passed)) .za_fc_rec(yes, note = r$note, tag = r$tag)
  else if (isFALSE(r$passed)) .za_fc_rec(no, note = r$note, tag = r$tag)
  else .za_fc_rec(NA, candidates = c(no, yes), missing = r$missing, note = r$note, tag = r$tag)
}

# The thickness-weighted mean of an attribute over layers, NA when one of
# them lacks it (or its thickness).
.za_fc_mean <- function(x, layers, name, from = NA_real_, to = NA_real_) {
  if (!length(layers)) return(NA_real_)
  v <- vapply(layers, function(i) .za_num(x, i, name), 0)
  t0 <- x$top[layers]; t1 <- x$bot[layers]
  if (!is.na(from)) { t0 <- pmax(t0, from); t1 <- pmin(t1, to) }
  w <- pmax(0, t1 - t0)
  keep <- !is.na(w) & w > 0
  if (!any(keep) || anyNA(v[keep])) return(NA_real_)
  sum(v[keep] * w[keep]) / sum(w[keep])
}


# --- the 19 criteria --------------------------------------------------------------------------

#' FC 1 (V 37; default 4): fibrous organic material (unrubbed fibre 2/3 or
#' more of the volume, else a von Post degree of H1 to H4) in one
#' contiguous run of 200
#' mm or more within the O (R4); less, humified. Fibre not recorded: undecided.
#' @noRd
.za_fc01 <- function(view, w) {
  x <- view$x
  fib <- lapply(w$layers, function(i) {
    f <- .za_num(x, i, "fiber_content_unrubbed_pct")
    if (!is.na(f)) return(.za_r(f >= .za_thv("fibrous_fibre"), th = "fibrous_fibre", note = sprintf("unrubbed fibre %s %%", format(f))))
    vp <- .za_txt(x, i, "von_post_index")
    n <- suppressWarnings(as.numeric(sub("^h", "", vp)))
    if (!is.na(n)) return(.za_r(n <= .za_thv("fibrous_von_post"), note = sprintf("von Post H%s", format(n))))
    .za_r(NA, missing = c("fiber_content_unrubbed_pct", "von_post_index"), note = "fibre content not recorded")
  })
  th <- x$thick[w$layers]
  p <- vapply(fib, `[[`, NA, "passed")
  if (anyNA(th)) return(.za_fc_rec(NA, c("fibrous", "humified"), missing = c("top_cm", "bottom_cm"), note = "thickness not recorded"))
  lim <- .za_thv("fibrous_thickness") / 10
  # R4 (Hugo, 2026-11-06): one contiguous run of fibrous layers, no sum
  # across a humified layer
  run <- function(ok) { best <- 0; cur <- 0; for (k in seq_along(ok)) { cur <- if (ok[k]) cur + th[k] else 0; best <- max(best, cur) }; best }
  yes <- run(p %in% TRUE); may <- run(!(p %in% FALSE))
  notes <- c(unlist(lapply(fib, `[[`, "note")), sprintf("the thickest run of fibrous layers %s cm (200 mm or more: fibrous)", format(yes)))
  tags <- unlist(lapply(fib, `[[`, "tag"))
  if (yes >= lim) return(.za_fc_rec("fibrous", note = notes, tag = tags))
  if (may < lim) return(.za_fc_rec("humified", note = notes, tag = tags))
  .za_fc_rec(NA, c("fibrous", "humified"), missing = unlist(lapply(fib, `[[`, "missing")), note = notes, tag = tags)
}

#' FC 2: a thick humic A is more than 450 mm thick with more than 1.8 %
#' organic carbon between 250 and 450 mm (the weighted mean of that part);
#' a thin one is not thick and has more than 1.8 % over its whole thickness
#' (the weighted mean). Neither: no family (Hugo, 2026-11-06).
#' @noRd
.za_fc02 <- function(view, w) {
  x <- view$x
  e <- w$element
  thick_lim <- .za_thv("humic_thick") / 10
  oc_lim <- .za_thv("humic_oc")
  smp <- .za_thv("humic_thick_sample") / 10
  if (is.na(e$bottom) || is.na(e$top)) return(.za_fc_rec(NA, c("thin", "thick"), missing = c("top_cm", "bottom_cm")))
  th <- e$bottom - e$top
  whole <- .za_fc_mean(x, w$layers, "oc_pct")
  thick <- if (th <= thick_lim) FALSE else {
    s <- .za_fc_mean(x, w$layers, "oc_pct", e$top + smp[1L], e$top + smp[2L])
    if (is.na(s)) NA else s > oc_lim
  }
  note <- c(sprintf("humic A %s cm thick", format(th)),
            if (!is.na(whole)) sprintf("organic carbon %s %% over the whole A (weighted)", format(round(whole, 2))))
  if (isTRUE(thick)) return(.za_fc_rec("thick", note = c(note, "more than 450 mm with more than 1.8 % organic carbon at 250-450 mm")))
  if (is.na(whole)) return(.za_fc_rec(NA, if (is.na(thick)) c("thin", "thick") else "thin", missing = "oc_pct", note = note))
  if (isFALSE(thick)) {
    if (whole > oc_lim) return(.za_fc_rec("thin", note = note))
    return(.za_fc_rec("neither", note = c(note, "neither thick nor 1.8 % organic carbon over the whole A: no family (Hugo, 2026-11-06)")))
  }
  .za_fc_rec(NA, c("thin", "thick"), missing = "oc_pct", note = c(note, "organic carbon at 250-450 mm not recorded"))
}

#' FC 3 (Fernwood): a dark A has a moist value of 4 or less and a chroma of
#' 1 or less (over the greater part of the A).
#' @noRd
.za_fc03 <- function(view, w) {
  x <- view$x
  .za_fc_major(x, w$layers, function(i) {
    m <- .za_colour(x, i, "moist")
    if (is.na(m$value) || is.na(m$chroma)) {
      if ((!is.na(m$value) && m$value > .za_thv("dark_a_value")) || (!is.na(m$chroma) && m$chroma > .za_thv("dark_a_chroma")))
        return(list(class = "light", note = sprintf("%s: light", .za_col_text(m))))
      return(list(class = NA_character_, missing = c("munsell_value_moist", "munsell_chroma_moist")))
    }
    d <- m$value <= .za_thv("dark_a_value") && m$chroma <= .za_thv("dark_a_chroma")
    list(class = if (d) "dark" else "light", note = sprintf("%s: %s", .za_col_text(m), if (d) "dark" else "light"))
  }, "dark", "light")
}

#' FC 4: a bleached A is grey dry as the E is (Q9; flagged), over the greater
#' part of the A.
#' @noRd
.za_fc04 <- function(view, w) {
  x <- view$x
  .za_fc_major(x, w$layers, function(i) {
    g <- .za_grey_dry(x, i)
    list(class = if (is.na(g$passed)) NA_character_ else if (g$passed) "bleached" else "not_bleached",
         missing = g$missing, note = g$note, tag = g$tag)
  }, "bleached", "not_bleached")
}

#' FC 5: dark (moist value 4 or less and chroma 1 or less; with a 10YR or
#' 7.5YR hue, chroma 2 or less), else red (the red hues, Q11), else of other
#' colour; dark is tested first, as the key lists it.
#' @noRd
.za_fc05 <- function(view, w) {
  x <- view$x
  .za_fc_major(x, w$layers, function(i) {
    m <- .za_colour(x, i, "moist")
    vmax <- .za_thv("fc5_dark_value"); c1 <- .za_thv("fc5_dark_chroma"); c2 <- .za_thv("fc5_dark_chroma_yr")
    yr <- !is.na(m$hue) && m$hue %in% as.character(unlist(.za_thv("fc5_dark_hues")))
    dark <- if (!is.na(m$value) && m$value > vmax) FALSE
            else if (!is.na(m$chroma) && m$chroma > c2) FALSE
            else if (is.na(m$value) || is.na(m$chroma)) NA
            else if (m$chroma <= c1) TRUE
            else if (is.na(m$hue)) NA else yr
    if (isTRUE(dark)) return(list(class = "dark", note = sprintf("%s: dark", .za_col_text(m))))
    if (is.na(dark)) return(list(class = NA_character_, missing = c("munsell_value_moist", "munsell_chroma_moist", "munsell_hue_moist"),
                                 note = "moist colour not recorded: dark undecided"))
    r <- .za_red(x, i)
    list(class = if (is.na(r$passed)) NA_character_ else if (r$passed) "red" else "other",
         missing = r$missing, note = c(sprintf("%s: not dark", .za_col_text(m)), r$note), tag = r$tag)
  }, c("dark", "red"), "other", share = TRUE)
}

#' FC 6: a yellow E is yellow when moist (a hue of 5YR or yellower, FC 6
#' allowing the occasional 5YR, with a moist chroma above the low chroma,
#' Q10, Q12), grey when dry; a grey E is grey moist too (low chroma or a gley
#' hue). A redder E with a higher chroma is grey (R1, Hugo, 2026-11-06).
#' @noRd
.za_fc06 <- function(view, w) {
  x <- view$x
  .za_fc_major(x, w$layers, function(i) {
    m <- .za_colour(x, i, "moist")
    lc <- .za_low_chroma(x, i)
    if (isTRUE(lc$passed)) return(list(class = "grey", note = lc$note, tag = lc$tag))
    if (is.na(m$family) || is.na(m$chroma))
      return(list(class = NA_character_, missing = c("munsell_hue_moist", "munsell_chroma_moist"), note = "moist colour not recorded"))
    if (m$chroma <= .za_thv("low_chroma")) return(list(class = "grey", note = sprintf("%s: grey moist", .za_col_text(m))))
    lim <- .rp_hue_pos(.munsell_hue_parts(.za_thv("yellow_e_hue")))
    p <- .rp_hue_pos(m)
    ok <- m$family %in% c("YR", "Y") && !is.na(p) && p >= lim - 1e-9
    if (ok) return(list(class = "yellow", note = sprintf("%s: yellow moist", .za_col_text(m)), tag = "stand_in:yellow_e_hue"))
    # R1 (Hugo, 2026-11-06): FC 6 sets apart only the E yellow when moist
    list(class = "grey", tag = "stand_in:yellow_e_hue",
         note = sprintf("%s: redder than 5YR, not the yellow of FC 6: grey (R1)", .za_col_text(m)))
  }, "yellow", "grey")
}

#' FC 7 (Fernwood; default 12): lamellae recorded in the E
#' (lamellae_cumulative_cm over 0); not recorded, absent.
#' @noRd
.za_fc07 <- function(view, w) {
  x <- view$x
  if (!length(w$layers)) return(.za_fc_rec(NA, c("absent", "present"), note = "no layer of the E described"))
  v <- vapply(w$layers, function(i) .za_num(x, i, "lamellae_cumulative_cm"), 0)
  if (any(v > 0, na.rm = TRUE)) return(.za_fc_rec("present", note = sprintf("lamellae %s cm in the E", format(sum(v, na.rm = TRUE)))))
  .za_fc_rec("absent", note = if (all(is.na(v))) "no lamellae recorded: absent" else "lamellae 0 cm")
}

#' FC 8 (default 10): the exchangeable Ca, Mg, K and Na of the B1 (the
#' S-value, cmol(+)/kg soil) x 100 / clay %: under 5 dystrophic, 5 to 15
#' mesotrophic, over 15 eutrophic. A calcareous B1 has no base status of
#' FC 8 (a diagnostic eutrophic horizon is not calcareous; soilKey's
#' reading: no class). A sum with a cation not recorded is a lower end: over
#' 15 already, eutrophic, flagged. A high electrical conductivity flags the
#' S-value (soluble salts).
#' @noRd
.za_fc08 <- function(view, w) {
  x <- view$x
  cls <- c("dystrophic", "mesotrophic", "eutrophic")
  if (!length(w$layers)) return(.za_fc_rec(NA, cls, note = "no B1 layer"))
  i <- w$layers[1L]
  ca <- .za_calcareous(x, i)
  if (isTRUE(ca$passed)) return(.za_fc_rec("calcareous", note = c(ca$note, "a calcareous B1: no base status of FC 8 (Hugo, 2026-11-06)"),
                                           tag = ca$tag))
  cat_ <- vapply(c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol"), function(a) .za_num(x, i, a), 0)
  clay <- .za_num(x, i, "clay_pct")
  tag <- character(0); note <- character(0)
  ec <- .za_num(x, i, "ec_dS_m")
  if (isTRUE(ec >= .za_thv("ec_high"))) {
    tag <- c(tag, "ec_high")
    note <- c(note, sprintf("EC %s dS/m: soluble salts may raise the S-value (default 10), flagged", format(ec)))
  }
  if (is.na(clay) || clay <= 0) return(.za_fc_rec(NA, cls, missing = "clay_pct", note = "clay of the B1 not recorded"))
  s <- sum(cat_, na.rm = TRUE)
  v <- s * 100 / clay
  lo <- .za_thv("dystrophic_max"); hi <- .za_thv("eutrophic_min")
  if (anyNA(cat_)) {
    miss <- names(cat_)[is.na(cat_)]
    if (v > hi) return(.za_fc_rec(if (is.na(ca$passed)) NA else "eutrophic", cls[3L], missing = ca$missing, tag = c(tag, "bases_partial"),
                                  note = c(note, sprintf("S-value at least %s cmol(+)/kg soil (%s not recorded): %s per kg clay, over %s",
                                                         format(round(s, 2)), paste(miss, collapse = ", "), format(round(v, 1)), format(hi)))))
    # D8: a sum already at 5 or more leaves mesotrophic or eutrophic
    return(.za_fc_rec(NA, if (v >= lo) cls[2:3] else cls, missing = miss,
                      note = c(note, sprintf("%s not recorded: the base status undecided", paste(miss, collapse = ", ")))))
  }
  c0 <- if (v < lo) "dystrophic" else if (v <= hi) "mesotrophic" else "eutrophic"
  note <- c(note, sprintf("S-value %s cmol(+)/kg soil x 100 / %s %% clay = %s cmol(+)/kg clay: %s", format(round(s, 2)), format(clay),
                          format(round(v, 1)), c0))
  if (is.na(ca$passed)) return(.za_fc_rec(NA, cls, missing = ca$missing, note = c(note, ca$note)))
  .za_fc_rec(c0, note = note, tag = tag)
}

#' FC 9 (Q11): red (the red hues, moist) over the greater part of the
#' horizon, else non-red.
#' @noRd
.za_fc09 <- function(view, w) {
  x <- view$x
  .za_fc_major(x, w$layers, function(i) {
    r <- .za_red(x, i)
    list(class = if (is.na(r$passed)) NA_character_ else if (r$passed) "red" else "non_red", missing = r$missing, note = r$note, tag = r$tag)
  }, "red", "non_red")
}

#' FC 10 (Q21): the B1 against the A or E directly above it: when any layer
#' of the A or E has 15 % clay or less, the B1 has at least 5 points more;
#' otherwise at least 1.3 times as much. A layer of the A or E whose clay is
#' not recorded leaves the rule open: decided only when both rules agree.
#' @noRd
.za_fc10 <- function(view, w) {
  x <- view$x
  cls <- c("non_luvic", "luvic")
  el <- view$elements
  if (!length(w$layers)) return(.za_fc_rec(NA, cls, note = "no B1 layer"))
  i <- w$layers[1L]
  cb <- .za_num(x, i, "clay_pct")
  ae <- which(vapply(el[seq_len(w$k - 1L)], function(e) any(e$codes %in% c(.ZA_TOPSOILS, "E")), NA))
  above <- if (w$k > 1L) el[[w$k - 1L]] else NULL
  if (is.null(above) || !any(above$codes %in% c(.ZA_TOPSOILS, "E")) || !length(above$layers))
    return(.za_fc_rec(NA, cls, note = "no A or E described directly above the B1"))
  j <- utils::tail(above$layers, 1L)
  ca <- .za_num(x, j, "clay_pct")
  if (is.na(cb) || is.na(ca)) return(.za_fc_rec(NA, cls, missing = "clay_pct", note = "clay of the B1 or of the A or E above not recorded"))
  sub <- unlist(lapply(el[ae], `[[`, "layers"))
  cs <- vapply(sub, function(k) .za_num(x, k, "clay_pct"), 0)
  low <- if (any(cs <= .za_thv("luvic_clay_low"), na.rm = TRUE)) TRUE else if (anyNA(cs)) NA else FALSE
  r1 <- cb - ca >= .za_thv("luvic_points")
  r2 <- cb / ca >= .za_thv("luvic_ratio")
  p <- if (isTRUE(low)) r1 else if (isFALSE(low)) r2 else if (r1 == r2) r1 else NA
  note <- sprintf("B1 %s %% clay, the %s above %s %%: %s", format(cb), .za_label(above$codes[1L]), format(ca),
                  if (isTRUE(low)) "an A or E layer with 15 % clay or less, 5 points more needed"
                  else if (isFALSE(low)) "every A or E layer over 15 % clay, 1.3 times needed"
                  else "the clay of an A or E layer not recorded")
  if (is.na(p)) return(.za_fc_rec(NA, cls, missing = "clay_pct", note = note))
  .za_fc_rec(if (p) "luvic" else "non_luvic", note = note)
}

#' FC 11 (default 11): subangular blocky of any size, or fine angular
#' blocky, against medium or coarse angular blocky, over the greater part
#' of the B; a range of sizes read by its lower end; "blocky" alone, angular
#' blocky without a size, or a structure that is not blocky: undecided.
#' @noRd
.za_fc11 <- function(view, w) {
  x <- view$x
  .za_fc_major(x, w$layers, function(i) {
    st <- .kidpr_structure(x, i)
    ty <- st$type; sz <- st$size
    if (is.na(ty)) return(list(class = NA_character_, missing = "structure_type", note = "structure type not recorded"))
    if (grepl("sub.?angular", ty)) return(list(class = "subangular_fine", note = sprintf("structure %s", ty)))
    if (!grepl("angular", ty))
      return(list(class = NA_character_, missing = if (grepl("block", ty)) "structure_type" else character(0),
                  note = sprintf("structure %s: not read as subangular or angular blocky", ty)))
    if (is.na(sz)) return(list(class = NA_character_, missing = "structure_size", note = sprintf("structure %s, size not recorded", ty)))
    fine <- grepl("^(very fine|extremely fine|fine|vf|f)( |$|-|/)", sz) || grepl("^(very thin|thin)", sz)
    big <- grepl("^(medium|coarse|very coarse|extremely coarse|m|c|vc)( |$|-|/)", sz)
    if (fine) return(list(class = "subangular_fine", note = sprintf("structure %s %s", sz, ty)))
    if (big) return(list(class = "medium_coarse", note = sprintf("structure %s %s", sz, ty)))
    list(class = NA_character_, missing = "structure_size", note = sprintf("structure size '%s' not read", sz))
  }, "medium_coarse", "subangular_fine")
}

#' FC 12 (Estcourt; default 5): continuous black cutans on the ped faces of
#' the prismacutanic B: coatings of moist value 2 or less and chroma 1 or
#' less (or a neutral hue), continuous; continuity not recorded, clay films
#' many, flagged. Coatings not recorded: absent.
#' @noRd
.za_fc12 <- function(view, w) {
  x <- view$x
  rs <- lapply(w$layers, function(i) {
    v <- .za_num(x, i, "coatings_value_moist")
    if (is.na(v)) return(.za_r(FALSE, note = "no coatings recorded: no black cutans"))
    if (v > .za_thv("black_cutans_value")) return(.za_r(FALSE, th = "black_cutans_value", note = sprintf("coatings of moist value %s: not black", format(v))))
    hue <- .za_txt(x, i, "coatings_hue_moist")
    ch <- .za_num(x, i, "coatings_chroma_moist")
    neutral <- isTRUE(grepl("^n", hue))
    if (!neutral) {
      if (is.na(ch)) return(.za_r(NA, missing = "coatings_chroma_moist", note = sprintf("coatings of moist value %s, chroma not recorded", format(v))))
      if (ch > .za_thv("black_cutans_chroma")) return(.za_r(FALSE, th = "black_cutans_chroma", note = sprintf("coatings of chroma %s: not black", format(ch))))
    }
    co <- .za_txt(x, i, "coatings_continuity")
    if (!is.na(co)) return(.za_r(grepl("^continuous", co), th = c("black_cutans_value", "black_cutans_chroma"),
                                 note = sprintf("black coatings, %s", co)))
    cf <- .clay_films_class(.za_txt(x, i, "clay_films_amount"))
    if (is.na(cf)) return(.za_r(NA, missing = c("coatings_continuity", "clay_films_amount"), note = "black coatings, their continuity not recorded"))
    .za_r(cf >= 4L, tag = "continuity_from_clay_films", th = c("black_cutans_value", "black_cutans_chroma"),
          note = sprintf("black coatings, continuity not recorded: clay films %s read for it, flagged", .za_txt(x, i, "clay_films_amount")))
  })
  .za_fc_any(rs, "with", "without")
}

#' FC 13 (default 12): ortstein, the podzol B (or a layer of it) at least
#' weakly cemented; cementation not recorded, not cemented.
#' @noRd
.za_fc13 <- function(view, w) {
  x <- view$x
  if (!length(w$layers)) return(.za_fc_rec(NA, c("not_hardened", "hardened"), note = "no layer of the podzol B described"))
  lim <- .za_cement_level(.za_thv("ortstein_cementation"))
  lev <- vapply(w$layers, function(i) .za_cement(x, i)$level, 0L)
  if (any(lev >= lim, na.rm = TRUE))
    return(.za_fc_rec("hardened", note = sprintf("%s: ortstein", .za_cement_word(max(lev, na.rm = TRUE)))))
  .za_fc_rec("not_hardened", note = if (all(is.na(lev))) "no cementation recorded: not hardened" else "not cemented")
}

# The rupture resistance (or consistence) words read as hard or more, and
# as softer (Q19; the USDA rows, D4).
# D4 (Hugo, 2026-11-06): the rows of the USDA rupture resistance: dry hard
# or harder, moist very firm or firmer, or moderately cemented or more.
.ZA_HARD_WORDS <- paste0("^(hard|very hard|extremely hard|slightly rigid|rigid|very rigid|very firm|extremely firm|",
                         "moderately cemented|strongly cemented|very strongly cemented|indurated)")
.ZA_SOFT_WORDS <- paste0("^(loose|soft|slightly hard|moderately hard|very friable|friable|firm|non.?cemented|",
                         "extremely weakly cemented|very weakly cemented|weakly cemented)")

#' FC 14 (Q19): hard when more than 70 % of the layer is rock in place
#' (bedrock_pct); without it, coarse fragments or saprolite over 70 % with a
#' rupture resistance of hard or more, flagged; else not hard.
#' @noRd
.za_fc14 <- function(view, w) {
  x <- view$x
  cls <- c("not_hard", "hard")
  if (!length(w$layers)) return(.za_fc_rec(NA, cls, note = "no layer described"))
  i <- w$layers[1L]
  lim <- .za_thv("hard_rock_pct")
  br <- .za_num(x, i, "bedrock_pct")
  if (!is.na(br)) return(.za_fc_rec(if (br > lim) "hard" else "not_hard", note = sprintf("rock in place %s %% (hard over %s %%)", format(br), format(lim))))
  rk <- .za_max(.za_num(x, i, "saprolite_pct"), .za_num(x, i, "coarse_fragments_pct"))
  if (is.na(rk)) return(.za_fc_rec(NA, cls, missing = "bedrock_pct", note = "rock in place not recorded"))
  if (rk <= lim) return(.za_fc_rec("not_hard", note = sprintf("saprolite or coarse fragments %s %%: not hard", format(rk))))
  rr <- .za_txt(x, i, "rupture_resistance")
  if (is.na(rr)) return(.za_fc_rec(NA, cls, missing = c("bedrock_pct", "rupture_resistance"),
                                   note = sprintf("saprolite or coarse fragments %s %%, their hardness not recorded", format(rk))))
  h <- if (grepl(.ZA_HARD_WORDS, rr)) TRUE else if (grepl(.ZA_SOFT_WORDS, rr)) FALSE else NA
  if (is.na(h)) return(.za_fc_rec(NA, cls, missing = "rupture_resistance", note = sprintf("rupture resistance '%s' not read", rr)))
  .za_fc_rec(if (h) "hard" else "not_hard", tag = "hard_from_fragments",
             note = sprintf("bedrock_pct not recorded: %s %% of rock fragments, rupture resistance %s, flagged (Q19)", format(rk), rr))
}

#' FC 15 (Q12): signs of wetness (a grey low-chroma matrix, or low-chroma
#' mottles on 5 % or more; flagged) in any layer of the part the family
#' names. A part not described (the layer under a pan at the end of the
#' description): undecided.
#' @noRd
.za_fc15 <- function(view, w) {
  x <- view$x
  if (!length(w$layers)) return(.za_fc_rec(NA, c("absent", "present"), missing = "bottom_cm", note = sprintf("%s: not described", w$what)))
  f <- if (identical(w$horizon, "SCH")) function(i) .za_sch_wetness(x, i) else function(i) .za_wetness(x, i)
  .za_fc_open(view, w, .za_fc_any(lapply(w$layers, f), "present", "absent"), "absent")
}

#' Signs of wetness in a soft carbonate horizon (D3; Hugo, 2026-11-06): a
#' pale carbonate matrix is no sign of wetness; only mottles (grey,
#' low-chroma or of iron oxides) on 5 % or more of the layer (Q12) or a gley
#' hue of the matrix count; none of these, absent, flagged.
#' @noRd
.za_sch_wetness <- function(x, i) {
  m <- .za_colour(x, i, "moist")
  gh <- as.character(unlist(.za_thv("gley_hues")))
  if (!is.na(m$family) && (m$family %in% gh || (!is.na(m$hue) && m$hue %in% gh)))
    return(.za_r(TRUE, th = "gley_hues", note = sprintf("%s: a gley hue in the soft carbonate horizon", .za_col_text(m))))
  rf <- .za_num(x, i, "redoximorphic_features_pct")
  mot <- .munsell_notations(as.character(.kidpr_v(x, i, "mottle_colors")))
  thr <- .za_thv("wetness_signs")
  if (!is.na(rf)) return(.za_r(rf >= thr, th = "wetness_signs", tag = "sch_mottles_only",
                               note = sprintf("mottles on %s %% of the soft carbonate horizon (its pale matrix not read, D3)", format(rf))))
  if (nrow(mot)) return(.za_r(NA, missing = "redoximorphic_features_pct", note = "mottles recorded, their extent not recorded"))
  if (isTRUE(.za_mottles_present(x, i)))
    return(.za_r(NA, missing = "redoximorphic_features_pct", note = "mottles recorded as present, their extent not recorded"))
  .za_r(FALSE, tag = "sch_mottles_only", note = "no mottles or gley hue in the soft carbonate horizon (its pale matrix not read, D3), flagged")
}

#' D2 (Hugo, 2026-11-06): a negative class over a part that reaches the end
#' of the description (the profile, the A and the layer under it, a horizon
#' and every layer under it) is undecided when the description stops above
#' 1500 mm, or when no layer is described under the A; a positive finding
#' still decides.
#' @noRd
.za_fc_open <- function(view, w, r, neg) {
  if (!identical(r$class, neg)) return(r)
  short <- (w$part %in% c("profile", "with_below", "from") && isTRUE(view$open)) ||
           (identical(w$part, "with_below") && !isTRUE(w$has_below))
  if (!short) return(r)
  .za_fc_rec(NA, c(neg, setdiff(r$candidates, neg), if (neg == "absent") "present" else "calcareous"), missing = "bottom_cm",
             note = c(r$note, "the description stops above 1500 mm or below the A: the negative class undecided (D2)"), tag = r$tag)
}

#' FC 16 (Q22): calcareous in some part (effervescence, else CaCO3 1 % or
#' more, flagged; relict nodules in a non-effervescent matrix do not count)
#' in any layer of the part the family names.
#' @noRd
.za_fc16 <- function(view, w) {
  x <- view$x
  if (!length(w$layers)) return(.za_fc_rec(NA, c("non_calcareous", "calcareous"), missing = "bottom_cm",
                                           note = sprintf("%s: not described", w$what)))
  .za_fc_open(view, w, .za_fc_any(lapply(w$layers, function(i) .za_calcareous(x, i)), "calcareous", "non_calcareous"),
             "non_calcareous")
}

#' FC 17 (Q13): a layer under the yellow-brown apedal B that would be a
#' podzol B directly under an A or E (soilKey's podzol B test with the
#' illuvial mark taken as given). Nothing under it within a complete
#' description: absent; the description stops above 1500 mm: undecided.
#' @noRd
.za_fc17 <- function(view, w) {
  x <- view$x
  cls <- c("absent", "present")
  if (!length(w$layers)) {
    if (view$open) return(.za_fc_rec(NA, cls, missing = "bottom_cm", note = "the description stops under the yellow-brown apedal B"))
    return(.za_fc_rec("absent", note = "the yellow-brown apedal B reaches the end of a complete description"))
  }
  r <- .za_fc_any(lapply(w$layers, function(i) .za_t_PZB(x, i, assume_mark = TRUE)), "present", "absent")
  if (identical(r$class, "absent") && view$open) return(.za_fc_rec(NA, cls, missing = "bottom_cm", note = c(r$note, "the description stops above 1500 mm")))
  r
}

#' FC 18 (default 12): the moist consistence of the upper C: loose, very
#' friable, friable or slightly firm friable, firmer firm; not recorded, a
#' rupture resistance recorded in the moist words stands in, flagged; else
#' undecided. A C below 1500 mm (not described): undecided.
#' @noRd
.za_fc18 <- function(view, w) {
  x <- view$x
  cls <- c("friable", "firm")
  if (!length(w$layers)) return(.za_fc_rec(NA, cls, missing = "consistence_moist", note = sprintf("%s: not described", w$what)))
  i <- w$layers[1L]
  rd <- function(v) if (grepl("^(loose|very friable|friable|slightly firm)", v)) "friable"
                    else if (grepl("^(firm|very firm|extremely firm|firmer)", v)) "firm" else NA_character_
  cm <- .za_txt(x, i, "consistence_moist")
  if (!is.na(cm)) {
    c0 <- rd(cm)
    if (is.na(c0)) return(.za_fc_rec(NA, cls, missing = "consistence_moist", note = sprintf("moist consistence '%s' not read", cm)))
    return(.za_fc_rec(c0, note = sprintf("moist consistence %s", cm)))
  }
  rr <- .za_txt(x, i, "rupture_resistance")
  c0 <- if (is.na(rr)) NA_character_ else rd(rr)
  if (!is.na(c0)) return(.za_fc_rec(c0, tag = "consistence_from_rupture",
                                    note = sprintf("moist consistence not recorded: rupture resistance %s, flagged", rr)))
  .za_fc_rec(NA, cls, missing = "consistence_moist", note = "moist consistence of the upper C not recorded")
}

#' FC 19: rock or saprolite under the O, its top within 500 mm of the lower
#' limit of the O and above 1500 mm; else unconsolidated material, when the
#' description reaches that depth; else undecided.
#' @noRd
.za_fc19 <- function(view, w) {
  x <- view$x
  cls <- c("rock_or_saprolite", "unconsolidated")
  e <- w$element
  # D1: the lower limit of the O not recorded
  if (is.null(e$bottom) || is.na(e$bottom))
    return(.za_fc_rec(NA, cls, missing = "bottom_cm", note = "the lower limit of the O not recorded"))
  lim <- min(e$bottom + .za_thv("organic_rock_depth") / 10, .za_thv("diagnostic_depth") / 10)
  # every layer under the O (the part "below" names the first)
  under <- if (length(e$layers)) x$ord[which(x$rank > max(x$rank[e$layers]))] else integer(0)
  ly <- under[which(!is.na(x$top[under]) & x$top[under] <= lim & x$top[under] < .za_thv("diagnostic_depth") / 10)]
  und <- FALSE; miss <- character(0)
  for (i in ly) {
    if (.za_is_rock(x, i)) return(.za_fc_rec("rock_or_saprolite", note = sprintf("rock at %s cm, under the O", format(x$top[i]))))
    s <- .za_t_SAP(x, i)
    if (isTRUE(s$passed)) return(.za_fc_rec("rock_or_saprolite", note = c(s$note, sprintf("saprolite at %s cm, under the O", format(x$top[i]))),
                                            tag = s$tag))
    if (is.na(s$passed)) { und <- TRUE; miss <- c(miss, s$missing) }
  }
  if (und) return(.za_fc_rec(NA, cls, missing = miss, note = "a layer under the O may be saprolite"))
  bot <- view$bottom
  if (isTRUE(bot >= lim) || !view$open) return(.za_fc_rec("unconsolidated", note = sprintf("no rock or saprolite within %s cm", format(lim))))
  .za_fc_rec(NA, cls, missing = "bottom_cm", note = sprintf("the description stops at %s cm, above %s cm", format(bot), format(lim)))
}

.ZA_FC <- list(.za_fc01, .za_fc02, .za_fc03, .za_fc04, .za_fc05, .za_fc06, .za_fc07, .za_fc08, .za_fc09, .za_fc10,
               .za_fc11, .za_fc12, .za_fc13, .za_fc14, .za_fc15, .za_fc16, .za_fc17, .za_fc18, .za_fc19)


# --- the predicate ------------------------------------------------------------------------------

#' The class of a pedon under one family criterion of the South African
#' classification of 1991
#'
#' Reads one of the 19 family criteria (FC 1-19) on the horizon a family
#' names, in the view of the form key (.za_view(): the horizons the key
#' identified), once per view. With `class`, the test of a family
#' (families.yaml): TRUE when the class read is one of `class`, FALSE when
#' it is another, NA when it is undecided and one of `class` is still
#' possible.
#'
#' @param pedon A PedonRecord.
#' @param criterion The family criterion, 1 to 19.
#' @param part The part of the horizon read (see data-raw/za1991_families.R).
#' @param horizon The token of the horizon (horizons.yaml), "-" for the
#'   profile.
#' @param class NULL for the record of the criterion; else the class or
#'   classes of a family.
#' @return Without `class`, a list: criterion, name, horizon, part, what
#'   (the layers read), layers, class (NA when undecided), candidates,
#'   missing, note, tag. With `class`, a DiagnosticResult.
#' @noRd
family_class_za <- function(pedon, criterion, part = "whole", horizon = "-", class = NULL) {
  view <- .za_current_view(pedon)
  rec <- .za_family_class(view, criterion, part, horizon)
  if (is.null(class)) return(rec)
  want <- as.character(unlist(class))
  passed <- if (!is.na(rec$class)) rec$class %in% want
            else if (length(rec$candidates) && !any(rec$candidates %in% want)) FALSE else NA
  DiagnosticResult$new(
    name = "family_class_za", passed = passed, layers = rec$layers,
    evidence = list(criterion = rec$criterion, name = rec$name, read = rec$what, class = rec$class,
                    candidates = rec$candidates, wanted = want, tags = rec$tag),
    missing = if (is.na(passed)) rec$missing else character(0),
    reference = .ZA_REFERENCE,
    notes = .za_fc_text(rec))
}

# One line of a criterion read, for the trace and the result.
.za_fc_text <- function(rec) {
  sprintf("FC %d (%s) on %s: %s%s%s", rec$criterion, rec$name, rec$what,
          if (is.na(rec$class)) sprintf("undecided (%s)", paste(rec$candidates, collapse = " or ")) else rec$class,
          if (length(rec$note)) paste0("; ", paste(rec$note, collapse = "; ")) else "",
          if (length(rec$tag)) sprintf(" [flagged: %s]", paste(rec$tag, collapse = ", ")) else "")
}

#' The record of a criterion in a view, kept in the view.
#' @noRd
.za_family_class <- function(view, criterion, part = "whole", horizon = "-") {
  criterion <- as.integer(criterion)
  if (length(criterion) != 1L || is.na(criterion) || criterion < 1L || criterion > 19L)
    rlang::abort("A South African family criterion is a number from 1 to 19.")
  key <- sprintf("%d|%s|%s", criterion, part, horizon)
  if (!is.null(view$fc) && !is.null(view$fc[[key]])) return(view$fc[[key]])
  w <- .za_fc_where(view, horizon, part)
  r <- if (!w$found) .za_fc_rec(NA, .za_fc_classes(criterion), note = w$what)
       else {
         r0 <- .ZA_FC[[criterion]](view, w)
         if (is.na(r0$class) && !length(r0$candidates)) r0$candidates <- .za_fc_classes(criterion)
         r0
       }
  if (w$found && w$candidate && !is.na(r$class)) r$tag <- unique(c(r$tag, "horizon_candidate"))
  rec <- c(list(criterion = criterion, name = .za_fc_name(criterion), horizon = horizon, part = part, what = w$what,
                layers = w$layers), r)
  if (!is.null(view$fc)) assign(key, rec, envir = view$fc)
  rec
}


# --- the family of a result ---------------------------------------------------------------------

#' The family part of a result (Q23): the criteria read for the form's
#' families, the partial code of an undecided family (an "x" for each
#' undecided digit), and the name "Form, Code Family" with the texture
#' class of the A (USDA; the sand grade with sand_grade).
#' @noRd
.za_family_notes <- function(res, view, form) {
  fam <- .za_families_file()
  digits <- fam$family_digits[[form]]
  if (is.null(digits)) return(list(res = res, notes = character(0)))
  notes <- character(0)
  # every condition of every digit, read once
  recs <- list()
  dg <- vapply(digits, function(d) {
    ok <- vapply(d$classes, function(conds) {
      v <- vapply(conds, function(cnd) {
        rec <- .za_family_class(view, cnd$criterion, cnd$part, cnd$horizon)
        recs[[sprintf("%d|%s|%s", rec$criterion, rec$part, rec$horizon)]] <<- rec
        want <- as.character(unlist(cnd$class))
        if (!is.na(rec$class)) rec$class %in% want else if (length(rec$candidates) && !any(rec$candidates %in% want)) FALSE else NA
      }, NA)
      .kidpr_and(v)
    }, NA)
    if (sum(ok %in% TRUE) == 1L) as.character(which(ok %in% TRUE)) else if (all(ok %in% FALSE)) "-" else "x"
  }, "")
  # D7: a digit where no class holds gives no family and no partial code;
  # soilKey's brackets around the whole partial code, none inside it
  partial <- if (any(dg == "-")) NA_character_
             else paste0(gsub("[][]", "", form), paste(dg, collapse = ""), strrep("0", 4L - length(dg)))
  crit <- data.frame(
    criterion = vapply(recs, function(r) r$criterion, 0L),
    name = vapply(recs, function(r) r$name, ""),
    read = vapply(recs, function(r) r$what, ""),
    class = vapply(recs, function(r) r$class, ""),
    candidates = vapply(recs, function(r) paste(r$candidates, collapse = ", "), ""),
    flagged = vapply(recs, function(r) paste(r$tag, collapse = ", "), ""),
    missing = vapply(recs, function(r) paste(r$missing, collapse = ", "), ""),
    note = vapply(recs, function(r) paste(r$note, collapse = "; "), ""),
    stringsAsFactors = FALSE, row.names = NULL)
  lv <- res$levels
  form_name <- lv$name[1L]
  got <- nrow(lv) >= 2L && identical(lv$level[2L], "families")
  tex <- .za_texture_a(view)
  q <- res$qualifiers %||% list()
  q$za_family <- list(code = if (got) lv$code[2L] else NA_character_, name = if (got) lv$name[2L] else NA_character_,
                      partial = if (got) NA_character_ else partial, criteria = crit, texture = tex$text)
  res$qualifiers <- q
  core <- if (got) sprintf("%s, %s %s", form_name, lv$code[2L], lv$name[2L])
          else if (!is.na(partial)) sprintf("%s, [%s]", form_name, partial) else form_name
  res$name <- if (!is.na(tex$text)) sprintf("%s, %s", core, tex$text) else core
  if (!got) {
    cand <- sub("^families:", "", grep("^families:", res$provisional_because, value = TRUE))
    notes <- c(notes, if (length(cand)) sprintf("The family is undecided (%s, soilKey's partial code with x for each undecided digit): %s.",
                                                partial, paste(cand, collapse = ", "))
                      else sprintf("No family of %s holds: see the criteria read in qualifiers$za_family$criteria.", form_name))
  }
  st <- unique(unlist(lapply(recs, function(r) sub("^stand_in:", "", grep("^stand_in:", r$tag, value = TRUE)))))
  if (length(st)) notes <- c(notes, sprintf("Family criteria read with soilKey's stand-ins: %s (thresholds.yaml).", paste(st, collapse = ", ")))
  if (!is.na(tex$note)) notes <- c(notes, tex$note)
  list(res = res, notes = notes)
}

# The USDA names of the texture classes of texture_class_from_pct().
.ZA_TEXTURE <- c(S = "sand", LS = "loamy sand", SL = "sandy loam", L = "loam", SIL = "silt loam", SI = "silt",
                 SCL = "sandy clay loam", CL = "clay loam", SICL = "silty clay loam", SC = "sandy clay",
                 SIC = "silty clay", C = "clay")

#' The texture class of the A for the name (Q23): the USDA class of the
#' thickness-weighted clay, silt and sand of the A, with the sand grade
#' (sand_grade, one grade for the whole A) before a class with sand in it.
#' Not all three recorded: no texture in the name.
#' @noRd
.za_texture_a <- function(view) {
  none <- list(text = NA_character_, note = NA_character_)
  if (!length(view$elements)) return(none)
  ly <- view$elements[[1L]]$layers
  if (!length(ly) || !any(view$elements[[1L]]$codes %in% .ZA_TOPSOILS)) return(none)
  x <- view$x
  v <- vapply(c("clay_pct", "silt_pct", "sand_pct"), function(a) .za_fc_mean(x, ly, a), 0)
  if (anyNA(v)) return(list(text = NA_character_, note = "The texture of the A is not in the name: clay, silt and sand not all recorded (Q23)."))
  cl <- texture_class_from_pct(v[["clay_pct"]], v[["silt_pct"]], v[["sand_pct"]])
  txt <- unname(.ZA_TEXTURE[cl])
  if (is.na(txt)) return(none)
  sg <- unique(stats::na.omit(vapply(ly, function(i) .za_txt(x, i, "sand_grade"), "")))
  if (length(sg) == 1L && sg %in% c("fine", "medium", "coarse") && grepl("sand", txt)) txt <- paste(sg, txt)
  list(text = txt, note = NA_character_)
}
