# =============================================================================
# v0.9.336 -- the key to the diagnostic horizons of the Russian soil
# classification (Field Guide of 2008, section 2.1, pp. 12-27), run on the
# recorded data.
#
# The key is a branching tree of 66 positions over one horizon; each answer
# leads to another position or to a horizon (50 horizons, the modifications
# AV, BH and BF, the peat as rock TT), a feature (u, tr, rh, r, ao, e, el,
# ca), or no diagnostic horizon (position 66). Positions 3, 4 and 23 find an
# overlay (S, X, V) and go on looking for a second horizon in the same layer
# (positions 5 and 24); positions 13 and 64 note a feature (u, ca) and go on
# (14 and 65). The tree below asks the positions in the book's order; each
# question is a function of the layer and of the whole profile (what lies
# above or below, the depth of a boundary, frost in the profile), answered
# by the readers of R/kidpr-readers.R. The walker (.walk_key(),
# R/horizon-identification.R) follows every branch of an undecided question:
# one outcome gives the horizon, several leave it undecided with the
# candidates (Hugo's decision Q4, 2026-11-02). The overlays and the features
# that continue are kept apart by copies of the positions after them, so an
# undecided overlay leaves the overlay undecided, not the horizon.
#
# Where a field descriptor the key asks for is missing, a soilKey WRB
# diagnostic stands in, flagged in the evidence (Q4): spodic() for the iron
# films of BHF (position 30, and its colour pattern at 56), argic() for the
# cutans of BT and BI (31, 63; not on massive or carbonate layers, nor
# outside the clay maximum), calcic() or protocalcic_properties() for the
# carbonate neoformations of BCA (62; else the carbonate maximum of the
# solum), the structure criterion of the natric horizon for the solonetz
# structure of ASN and BSN (58: a columnar structure recorded in the layer's
# run; the exchangeable sodium never decides, p. 52), cambic() for the
# transformed structure of BM (33, 65), and andic_properties() for the
# volcanic ash of BAN (55). The review of 0.9.336 and Hugo's decisions of
# 2026-11-02 set the frost of position 26, the peat body of 41 and 44, and
# the gley colours of 2.5Y.
#
# .kidpr_identify() is the horizon identification of the registry namespace
# kidpr (identify_horizons(pedon, "kidpr2008") once the registry entry
# exists): one row per layer. A layer written in the Russian notation keeps
# the horizon the notation gives; the key still runs on its data, and a
# reading that differs goes to the evidence, never replaces it.
# =============================================================================

# Book letters of the answers (a b c d = the Cyrillic a, be, ve, ge).
.KIDPR_LETTERS <- c(a = "\u0430", b = "\u0431", c = "\u0432", d = "\u0433")

# The WRB diagnostics that stand in for a field descriptor.
.KIDPR_STAND_INS <- c("spodic", "argic", "calcic", "protocalcic_properties", "natric_horizon", "cambic", "andic_properties")

# Horizons by kind, for the questions on the layers above.
.KIDPR_AGRO <- c("P", "PU", "PB", "PT", "PTR")
.kidpr_codes_of <- function(groups) {
  g <- .kidpr_vocabulary()$group
  names(g)[g %in% groups]
}

# The candidates of a gap of the key (an answer with no exit).
# (gap 7 is reached only when 7b, tr, is FALSE: TUR alone).
.KIDPR_GAPS <- list("7" = "TUR", "60" = c("CAT", "BI", "BT"))


# --- a question of the key ----------------------------------------------------------

# A question at a position: the reader `f(x, i)` asked once per layer, its
# answer logged for the evidence.
.kidpr_ask <- function(x, i, pos, f) {
  key <- sprintf("q_%s_%d", pos, i)
  r <- x$cache[[key]]
  if (is.null(r)) {
    r <- tryCatch(f(x, i), error = function(e)
      .kidpr_r(NA, note = sprintf("position %s: %s", pos, conditionMessage(e))))
    assign(key, r, envir = x$cache)
    lg <- x$log[[i]]
    lg[[pos]] <- r
    x$log[[i]] <- lg
  }
  list(passed = r$passed, missing = r$missing)
}
.kq <- function(pos, f) { force(pos); force(f); function(layer, context) .kidpr_ask(context, layer, pos, f) }
.kb <- function(label, to, test = TRUE) list(test = test, to = to, label = label)
# A binary position: a question, its yes (a) and its no (b).
.kn2 <- function(pos, f, yes, no) list(branches = list(.kb("a", yes, .kq(pos, f)), .kb("b", no)))


# --- the profile around a layer -----------------------------------------------------

# The first mineral layer: the top of the soil under its litter and organic
# layers (TRUE), undecided while an organic layer above is undecided.
.kidpr_is_first_mineral <- function(x, i) {
  s <- .kidpr_start_row(x)
  if (is.na(s) || x$rank[i] < x$rank[s]) return(FALSE)
  above <- x$ord[seq.int(x$rank[s], length.out = x$rank[i] - x$rank[s])]
  if (!length(above)) return(TRUE)
  o <- vapply(above, function(j) .kidpr_org23(x, j)$passed, NA)
  if (any(o %in% FALSE)) FALSE else if (all(o %in% TRUE)) TRUE else NA
}

# Where a horizon "starts at the surface" (key positions 12, 14, 20; D17): the
# first mineral layer, the layer directly under the agro layers, the layer
# directly under the surface deposit.
.kidpr_is_start <- function(x, i) {
  .kidpr_memo(x, sprintf("start_%d", i), function() {
    fm <- .kidpr_is_first_mineral(x, i)
    if (isTRUE(fm)) return(TRUE)
    dep <- .kidpr_deposit(x)
    if (length(dep$rows) && identical(.kidpr_next(x, dep$rows[length(dep$rows)]), i)) return(TRUE)
    p <- .kidpr_prev(x, i)
    if (is.na(p)) return(fm)
    ap <- .kidpr_agro(x, p)$passed; ai <- .kidpr_agro(x, i)$passed
    under_agro <- .kidpr_and(ap, .kidpr_not(ai))
    .kidpr_or(fm, under_agro)
  })
}

# A horizon that starts at the surface with a colour (grey at 12, not grey at
# 20): a layer where a horizon starts, or one that continues the horizon of
# the layer above as its sub-horizon (AY1, AY2): the same hue page, value
# and chroma within one unit, the same darkness, both agro or both natural,
# no sign of a light, gley or olive horizon, not heavier by a gradation and
# no clay films the layer above lacks. A light horizon (key 22) directly
# under an organic, agro or deposited layer is not a surface humus horizon
# (the E under O or P of a podzol); and a grey layer under an organic layer,
# or continuing the surface horizon, must look like a humus horizon
# (.kidpr_humus_like(), the review's H1): the CRM, CR or G under the O of a
# cryozem or a gleyzem is not.
.kidpr_surface_colour <- function(x, i, grey) {
  key <- sprintf("surface_%s_%d", if (grey) "grey" else "notgrey", i)
  .kidpr_memo(x, key, function() {
    g <- .kidpr_grey(x, i)
    col <- if (grey) g else .kidpr_r(.kidpr_not(g$passed), missing = g$missing, note = g$note, tag = g$tag)
    if (isFALSE(col$passed)) return(col)
    st <- .kidpr_is_start(x, i)
    light <- .kidpr_q22(x, i)$passed
    hl <- NULL
    if (grey && isTRUE(st) && !identical(.kidpr_start_row(x), i)) {
      if (!isFALSE(light)) st <- if (isTRUE(light)) FALSE else NA
      if (isFALSE(st)) return(.kidpr_r(FALSE, note = "a light horizon under an organic, agro or deposited layer, not a humus horizon"))
      if (isTRUE(.kidpr_is_first_mineral(x, i))) {
        hl <- .kidpr_humus_like(x, i)
        st <- .kidpr_and(st, hl$passed)
        if (isFALSE(st)) return(.kidpr_r(FALSE, note = c("under an organic layer, not a humus horizon", hl$note)))
      }
    }
    cont <- FALSE
    p <- .kidpr_prev(x, i)
    if (!isTRUE(st) && !is.na(p) && !isTRUE(.kidpr_org23(x, p)$passed)) {
      cont <- .kidpr_and(.kidpr_surface_colour(x, p, grey)$passed, .kidpr_subhorizon(x, p, i, grey))
      if (grey && !isFALSE(cont)) {
        hl <- .kidpr_humus_like(x, i)
        cont <- .kidpr_and(cont, hl$passed)
      }
    }
    pos <- .kidpr_or(st, cont)
    .kidpr_r(.kidpr_and(pos, col$passed), missing = c(col$missing, if (is.na(pos)) c("land_use", "designation", hl$missing)),
             note = c(col$note, if (isTRUE(pos)) "starts at the surface (or under the agro layers or the deposit)"
                                else if (isFALSE(pos)) "does not start at the surface", hl$note),
             tag = col$tag)
  })
}

# Does a grey layer look like a humus horizon (the review's H1)? Not with a
# gley or olive moist colour, not massive, single-grained or stratified, not
# frozen, and darker when dry than the parent material by a value or more
# (or as dark with a chroma 2 lower); with no parent material below, yes.
.kidpr_humus_like <- function(x, i) {
  .kidpr_memo(x, sprintf("humus_like_%d", i), function() {
    if (isTRUE(.kidpr_gley(x, i)$passed) || isTRUE(.kidpr_olive(x, i)$passed))
      return(.kidpr_r(FALSE, note = "a gley or olive colour, not a humus horizon"))
    st <- .kidpr_structure(x, i)
    if (isTRUE(grepl(.KIDPR_RAW_STRUCTURE, st$type)) || isTRUE(.field_grade_class(st$grade) == 0L))
      return(.kidpr_r(FALSE, note = "massive or structureless, not a humus horizon"))
    if (isTRUE(.kidpr_num(x, i, "permafrost_temp_C") <= 0) || isTRUE(.kidpr_num(x, i, "ice_pct") > 0))
      return(.kidpr_r(FALSE, note = "frozen, not a humus horizon"))
    p <- .kidpr_parent_row(x, i)
    if (is.na(p)) return(.kidpr_r(TRUE))
    ci <- .kidpr_colour(x, i, "dry"); cp <- .kidpr_colour(x, p, "dry")
    if (is.na(ci$value) || is.na(cp$value)) return(.kidpr_r(NA, missing = "munsell_value_dry"))
    dk <- ci$value <= cp$value - 1 || (ci$value == cp$value && isTRUE(ci$chroma <= cp$chroma - 2))
    .kidpr_r(dk, note = if (!dk) sprintf("not darker than the parent material (%s)", .kidpr_colour_text(cp)))
  })
}

# Does layer i continue the horizon of the layer p above it (three-valued)?
.kidpr_subhorizon <- function(x, p, i, grey) {
  cp <- .kidpr_colour(x, p, "dry"); ci <- .kidpr_colour(x, i, "dry")
  if (is.na(cp$value) || is.na(ci$value) || is.na(cp$hue) || is.na(ci$hue)) return(NA)
  up <- suppressWarnings(.munsell_hue_units(cp$hue)); ui <- suppressWarnings(.munsell_hue_units(ci$hue))
  same_hue <- if (is.na(up) || is.na(ui)) identical(cp$hue, ci$hue) else abs(up - ui) <= 2.5
  near <- same_hue && abs(cp$value - ci$value) <= 1 && (is.na(cp$chroma) || is.na(ci$chroma) || abs(cp$chroma - ci$chroma) <= 1)
  if (!near) return(FALSE)
  same_agro <- .kidpr_agro(x, p)$passed == .kidpr_agro(x, i)$passed
  same_dark <- if (grey) .kidpr_dark(x, p)$passed == .kidpr_dark(x, i)$passed else TRUE
  other <- .kidpr_or(.kidpr_q22(x, i)$passed, .kidpr_gley(x, i)$passed, .kidpr_olive(x, i)$passed)
  # nor a heavier layer, or one with clay films the layer above lacks
  d <- .kidpr_grad_diff(x, i, p)$diff
  heavier <- !is.na(d) && d >= 1
  fi <- .clay_films_class(.kidpr_v(x, i, "clay_films_amount")); fp <- .clay_films_class(.kidpr_v(x, p, "clay_films_amount"))
  films <- isTRUE(fi >= 3) && !isTRUE(fp >= 3)
  .kidpr_and(same_agro, same_dark, .kidpr_not(other), !heavier, !films)
}

# The parent material below a layer: the deepest mineral layer under it that
# is not hard rock, or the first one written C; NA when there is none.
.kidpr_parent_row <- function(x, i) {
  b <- .kidpr_below(x, i)
  b <- b[!vapply(b, function(j) .kidpr_hard_rock(x, j) || isTRUE(.kidpr_org23(x, j)$passed), NA)]
  if (!length(b)) return(NA_integer_)
  isc <- vapply(b, function(j) .kidpr_parent_like(x, j) %in% TRUE, NA)
  if (any(isc)) b[which(isc)[1L]] else b[length(b)]
}

# Hard rock by the designation (an R of any notation).
.kidpr_hard_rock <- function(x, i) {
  if (x$kidpr && isTRUE(x$des$known[i])) return(identical(x$des$index[i], "R"))
  d <- as.character(.kidpr_v(x, i, "designation"))
  !is.na(d) && grepl(.WRB_ROCK_DESIGNATION, d)
}

# Parent material, rock or a transition by the designation (C, D, R, CLM,
# TT; FAO C, R, AC); NA with no designation.
.kidpr_parent_like <- function(x, i) {
  if (x$kidpr && isTRUE(x$des$known[i]))
    return(x$des$index[i] %in% .KIDPR_ROCKS || isTRUE(x$des$transitional[i]))
  d <- as.character(.kidpr_v(x, i, "designation"))
  if (is.na(d) || !nzchar(trimws(d))) return(NA)
  grepl("^[0-9IVX]*(C|R|D|AC|CA)([^A-Z]|$)", d)
}

# The identification of another layer, as far as a question needs it: its
# code, or the codes still possible. Only layers above are asked for, so no
# question waits on itself.
.kidpr_code_in <- function(x, j, codes) {
  if (is.na(j)) return(FALSE)
  r <- .kidpr_identify_layer(x, j)
  if (isTRUE(r$busy)) return(NA)
  if (!is.na(r$code)) return(r$code %in% codes)
  cand <- r$candidates
  if (isFALSE(r$passed) || !length(cand)) return(FALSE)
  if (all(cand %in% codes)) TRUE else if (any(cand %in% codes)) NA else FALSE
}

# Is some layer above one of `codes` (three-valued)?
.kidpr_above_in <- function(x, i, codes) {
  a <- .kidpr_above(x, i)
  if (!length(a)) return(FALSE)
  .kidpr_or(vapply(a, function(j) .kidpr_code_in(x, j, codes), NA))
}

# A run of adjoining layers around `i` for which `pred(x, j)` holds; `open`
# when a neighbour of the run is undecided.
.kidpr_run <- function(x, i, pred) {
  rows <- i
  open <- FALSE
  j <- .kidpr_prev(x, i)
  while (!is.na(j)) {
    p <- pred(x, j)
    if (isTRUE(p)) { rows <- c(j, rows); j <- .kidpr_prev(x, j) } else { if (is.na(p)) open <- TRUE; break }
  }
  j <- .kidpr_next(x, i)
  while (!is.na(j)) {
    p <- pred(x, j)
    if (isTRUE(p)) { rows <- c(rows, j); j <- .kidpr_next(x, j) } else { if (is.na(p)) open <- TRUE; break }
  }
  list(rows = rows, thick = sum(x$thick[rows]), top = x$top[rows[1L]], bottom = x$bot[rows[length(rows)]],
       open = open)
}

# The thickness of a run against a bound, three-valued: an undecided
# neighbour leaves a thin run undecided.
.kidpr_run_over <- function(run, cm, word) {
  if (is.na(run$thick)) return(.kidpr_r(NA, missing = c("top_cm", "bottom_cm")))
  if (run$thick > cm) return(.kidpr_r(TRUE, note = sprintf("%s %s cm thick (over %s)", word, format(run$thick), cm)))
  if (run$open) return(.kidpr_r(NA, note = sprintf("%s %s cm thick, its neighbours undecided", word, format(run$thick))))
  .kidpr_r(FALSE, note = sprintf("%s %s cm thick (%s or less)", word, format(run$thick), cm))
}


# A hue of 7.5YR or redder (chestnut, reddish brown); a neutral or a gley hue
# is not.
.kidpr_reddish <- function(col) {
  if (is.na(col$family)) return(NA)
  if (!col$family %in% c("R", "YR", "Y")) return(FALSE)
  u <- suppressWarnings(.munsell_hue_units(col$hue))
  if (is.na(u)) NA else u <= 15
}

# --- the positions ------------------------------------------------------------------

# 1: salts over 1 % in the 0-10 cm of a surface layer, or extremely hazardous
# pollutants within 30 cm.
.kidpr_q01 <- function(x, i) .kidpr_ror(.kidpr_salts_s(x, i), .kidpr_contaminated(x, i))
# 2: the salts.
.kidpr_q02 <- function(x, i) .kidpr_salts_s(x, i)
# 3: a black silty layer smelling of hydrogen sulfide under the salt crust
# (sulfide_odour; not recorded, sulfidic S 0.01 % or more; neither, no).
.kidpr_q03 <- function(x, i) {
  od <- as.logical(.kidpr_v(x, i, "sulfide_odour"))
  if (!is.na(od)) return(.kidpr_r(od, note = sprintf("smell of hydrogen sulfide %s", if (od) "recorded" else "recorded absent")))
  s <- .kidpr_num(x, i, "sulfidic_s_pct")
  if (!is.na(s)) return(.kidpr_r(s >= 0.01, tag = "sulfide_from_s",
                                 note = sprintf("no smell recorded; sulfidic S %s %%", format(s))))
  .kidpr_r(FALSE, note = "no smell of hydrogen sulfide recorded")
}
# 4: the pollutants.
.kidpr_q04 <- function(x, i) .kidpr_contaminated(x, i)

# 5: the profile is one weakly developed surface horizon over the parent
# material: thinner than 5 cm in loam or clay, or in sand and loamy sand the
# colour of the parent with a grey tint; live roots (not recorded, absent).
.kidpr_q05 <- function(x, i) {
  fm <- .kidpr_is_first_mineral(x, i)
  if (!isTRUE(fm)) return(.kidpr_rl(fm, note = "not the top mineral layer"))
  if (!identical(.kidpr_start_row(x), i)) return(.kidpr_r(FALSE, note = "an organic horizon above"))
  b <- .kidpr_below(x, i)
  pl <- vapply(b, function(j) .kidpr_parent_like(x, j), NA)
  single <- if (!length(b)) TRUE else .kidpr_and(pl)
  if (isFALSE(single)) return(.kidpr_r(FALSE, note = "other horizons below"))
  ra <- .kidpr_txt(x, i, "roots_abundance")
  roots <- !is.na(ra) && !grepl("^(none|no|absent)", ra)
  if (!roots) return(.kidpr_r(FALSE, note = "no live roots recorded"))
  g <- .kidpr_grad(x, i)
  thin <- if (is.na(x$thick[i])) NA else x$thick[i] < 5
  loam <- if (is.na(g$step)) NA else g$step >= 3
  sandy <- if (is.na(g$step)) NA else g$step <= 2
  tint <- NA
  if (length(b)) {
    c1 <- .kidpr_colour(x, i, "dry"); c0 <- .kidpr_colour(x, b[1L], "dry")
    if (!is.na(c1$family) && !is.na(c0$family) && !is.na(c1$value) && !is.na(c0$value)) {
      same <- identical(c1$family, c0$family) && isTRUE(all.equal(c1$number, c0$number))
      tint <- same && (c1$value < c0$value || isTRUE(c1$chroma < c0$chroma))
    }
  }
  p <- .kidpr_and(single, .kidpr_or(.kidpr_and(loam, thin), .kidpr_and(sandy, tint)))
  .kidpr_r(p, missing = if (is.na(p)) c(if (is.na(single)) "designation", if (is.na(g$step)) .KIDPR_TEXTURE_MISSING,
                                        if (is.na(tint)) "munsell_value_dry") else character(0),
           note = c(if (isTRUE(p)) "the only horizon of the profile, weakly developed" else if (isFALSE(p)) "not a weakly developed single horizon",
                    g$note),
           tag = g$tag)
}

# 6: a heterogeneous mixture of mechanically mixed fragments of several
# horizons: foreign_fragments_pct 80 % or more (the undisturbed part under
# 20 %, p. 53); not recorded, a layer within the site's mixing depth,
# flagged; neither, no (a field observation).
.kidpr_mixed <- function(x, i) {
  .kidpr_memo(x, sprintf("mixed_%d", i), function() {
    ff <- .kidpr_num(x, i, "foreign_fragments_pct")
    if (!is.na(ff)) return(.kidpr_r(ff >= 80, note = sprintf("fragments of other horizons %s %%", format(ff))))
    d <- suppressWarnings(as.numeric(.kidpr_site(x, "mechanical_mixing_depth_cm")))
    g <- .kidpr_ground(x)
    if (!is.na(d) && !is.na(x$top[i]) && !is.na(g)) {
      # the plough layer is not the mixed layer of key 6 (the review's H6)
      ag <- .kidpr_agro(x, i)$passed
      if (isTRUE(ag)) return(.kidpr_r(FALSE, note = "the plough layer: not mixed by the site's mixing depth"))
      p <- .kidpr_and(.kidpr_not(ag), x$top[i] - g < d)
      return(.kidpr_r(p, missing = if (is.na(p)) c("land_use", "designation") else character(0), tag = "mixing_from_site",
                      note = sprintf("fragments not recorded: the site's mixing depth (%s cm), below the plough layer", format(d))))
    }
    .kidpr_r(FALSE)
  })
}
.kidpr_q06 <- function(x, i) .kidpr_mixed(x, i)

# 7: the mixed layer is TUR when over 40 cm thick wherever it lies, or under
# P or PU reaching deeper than 40 cm (a); the feature tr at the surface or
# under P or PU with its base above 40 cm (b); otherwise the key has no exit.
.kidpr_mixed_run <- function(x, i) {
  r <- .kidpr_run(x, i, function(x, j) .kidpr_mixed(x, j)$passed)
  d <- suppressWarnings(as.numeric(.kidpr_site(x, "mechanical_mixing_depth_cm")))
  g <- .kidpr_ground(x)
  if (is.na(.kidpr_num(x, i, "foreign_fragments_pct")) && !is.na(d) && !is.na(g)) {
    r$bottom <- min(r$bottom, g + d)
    r$thick <- r$bottom - r$top
  }
  r$above <- .kidpr_prev(x, r$rows[1L])
  r
}
.kidpr_q07a <- function(x, i) {
  r <- .kidpr_mixed_run(x, i)
  g <- .kidpr_ground(x)
  under <- .kidpr_code_in(x, r$above, c("P", "PU"))
  p <- .kidpr_or(r$thick > 40, .kidpr_and(under, r$bottom - g > 40))
  .kidpr_r(p, missing = if (is.na(p)) "designation" else character(0),
           note = sprintf("mixed layer %s-%s cm (%s cm thick)%s", format(r$top), format(r$bottom), format(r$thick),
                          if (isTRUE(under)) ", under an agro horizon P or PU" else ""))
}
.kidpr_q07b <- function(x, i) {
  r <- .kidpr_mixed_run(x, i)
  g <- .kidpr_ground(x)
  surf <- .kidpr_is_first_mineral(x, r$rows[1L])
  under <- .kidpr_code_in(x, r$above, c("P", "PU"))
  .kidpr_rl(.kidpr_and(.kidpr_or(surf, under), r$bottom - g < 40), missing = "designation")
}

# 8-9: a deposited layer at the surface, and the layer is in it.
.kidpr_q08 <- function(x, i) {
  d <- .kidpr_deposit(x)
  .kidpr_r(d$passed, note = d$note, tag = d$tag %||% character(0))
}
.kidpr_q09 <- function(x, i) {
  d <- .kidpr_deposit(x)
  .kidpr_r(i %in% d$rows, note = if (i %in% d$rows) "the layer is part of the deposit" else "the layer is under the deposit")
}

# 10-11: plant residues.
.kidpr_q10 <- function(x, i) .kidpr_org23(x, i)
.kidpr_q11 <- function(x, i) .kidpr_res13(x, i)

# 12: starts at the surface (or under the deposit) and is mainly grey; in a
# profile dark from a dark rock (13), a grey middle horizon too, which 13a
# sends on with the feature u (the book: 58 for the middle horizons; soilKey
# goes on at 31, whose yes leads to 58, so that a layer without cutans is
# not forced into it).
.kidpr_q12 <- function(x, i) {
  s <- .kidpr_surface_colour(x, i, grey = TRUE)
  if (!isFALSE(s$passed)) return(s)
  if (isTRUE(.kidpr_dark_rock(x)$passed) && isTRUE(.kidpr_grey(x, i)$passed))
    return(.kidpr_r(TRUE, note = "a grey middle horizon of a profile dark from a dark rock"))
  s
}

# 13: the whole profile is dark from a dark rock (shungite, carbonaceous or
# black shales): the site's parent material names one and every mineral
# layer below is dark (dry value 4 or less).
.KIDPR_DARK_ROCK <- paste0("shungit|black shale|carbonaceous|coal|graphit|anthracit|",
                           "\u0448\u0443\u043d\u0433\u0438\u0442|\u0443\u0433\u043b\u0438\u0441\u0442|\u0447\u0435\u0440\u043d\u044b\u0445? \u0441\u043b\u0430\u043d\u0446")
.kidpr_dark_rock <- function(x) {
  .kidpr_memo(x, "dark_rock", function() {
    b <- x$ord[!vapply(x$ord, function(j) .kidpr_hard_rock(x, j) || .kidpr_litter(x, j) ||
                         isTRUE(.kidpr_org23(x, j)$passed), NA)]
    dk <- vapply(b, function(j) { v <- .kidpr_colour(x, j, "dry")$value; if (is.na(v)) NA else v <= 4 }, NA)
    if (any(dk %in% FALSE) || length(b) < 2L) return(.kidpr_r(FALSE, note = "the profile is not dark throughout"))
    pm <- .kidpr_site_txt(x, "parent_material")
    rock <- if (is.na(pm)) NA else grepl(.KIDPR_DARK_ROCK, pm, perl = TRUE)
    p <- .kidpr_and(.kidpr_and(dk), rock)
    .kidpr_r(p, missing = if (is.na(p)) c("parent_material", "munsell_value_dry") else character(0),
             note = if (isTRUE(p)) sprintf("dark throughout, from a dark rock (%s)", pm)
                    else if (isFALSE(p)) "the dark colour is not inherited from the rock")
  })
}
# 13a for a surface horizon (go on at 14) or a middle one (go on at 31).
.kidpr_q13s <- function(x, i) {
  d <- .kidpr_dark_rock(x)
  sf <- .kidpr_surface_colour(x, i, grey = TRUE)$passed
  .kidpr_r(.kidpr_and(d$passed, sf), missing = d$missing, note = c(d$note, if (isTRUE(sf)) "a surface horizon"))
}
.kidpr_q13m <- function(x, i) {
  d <- .kidpr_dark_rock(x)
  sf <- .kidpr_surface_colour(x, i, grey = TRUE)$passed
  .kidpr_r(.kidpr_and(d$passed, .kidpr_not(sf)), missing = d$missing, note = c(d$note, if (isFALSE(sf)) "a middle horizon"))
}

# 14: an agro horizon (b) or a natural one (a).
.kidpr_q14 <- function(x, i) .kidpr_agro(x, i)
# 15: dark grey to black, a brown or coffee tint allowed (a: dry value 4 or
# less, chroma 3 or less), or light grey to grey (b).
.kidpr_q15 <- function(x, i) {
  col <- .kidpr_colour(x, i, "dry")
  p <- if (!is.na(col$value) && col$value > 4) FALSE else if (!is.na(col$chroma) && col$chroma > 3) FALSE
       else if (is.na(col$value) || is.na(col$chroma)) NA else TRUE
  .kidpr_colour_word(col, p, "dark grey to black (a brown tint allowed)")
}

# The three vertic signs together (key 16b, 23; default 18): slickensides
# common or more (small and large when slickensides_size is recorded),
# wedge-shaped peds, clay 30 % or more, the top within 80 cm.
.kidpr_vertic3 <- function(x, i) {
  .kidpr_memo(x, sprintf("vertic3_%d", i), function() {
    sl <- .kidpr_txt(x, i, "slickensides")
    slk <- !is.na(sl) && grepl("^(common|many|continuous|abundant|comum|muit)", sl)
    sz <- .kidpr_txt(x, i, "slickensides_size")
    sizes <- if (is.na(sz)) TRUE else grepl("both", sz)
    st <- .kidpr_structure(x, i)$type
    wedge <- if (is.na(st)) NA else grepl("wedge|cuneif|parallelepip|\u043a\u043b\u0438\u043d", st)
    cl <- .kidpr_num(x, i, "clay_pct")
    clay <- if (is.na(cl)) NA else cl >= 30
    g <- .kidpr_ground(x)
    shallow <- if (is.na(x$top[i]) || is.na(g)) NA else x$top[i] - g <= 80
    p <- .kidpr_and(slk, sizes, wedge, clay, shallow)
    partial <- (slk || isTRUE(wedge) || (!is.na(sl) && grepl("^few", sl))) && !isTRUE(p)
    r <- .kidpr_r(p, missing = if (is.na(p)) c(if (is.na(wedge)) "structure_type", if (is.na(clay)) "clay_pct") else character(0),
                  note = if (isTRUE(p)) "slickensides, wedge-shaped peds and clay together"
                         else if (!slk) "slickensides not common" else if (isFALSE(p)) "not all three vertic signs")
    r$partial <- partial
    r
  })
}

# The structure of a solonetz horizon (key 16c, 58a): columnar (prisms with
# rounded tops). Not recorded, the structure criterion of the WRB natric
# horizon stands in, read on the layer's own run (the adjoining layers with
# continuous clay films recorded): a recorded columnar structure there yes,
# recorded structures that are not columnar no, none recorded undecided. The
# exchangeable sodium never decides (the review's H4).
.KIDPR_COLUMNAR <- "column|colun|\u0441\u0442\u043e\u043b\u0431"
.kidpr_columnar <- function(x, i) {
  .kidpr_memo(x, sprintf("columnar_%d", i), function() {
    st <- .kidpr_structure(x, i)$type
    if (!is.na(st)) {
      p <- grepl(.KIDPR_COLUMNAR, st)
      return(.kidpr_r(p, note = sprintf("structure %s", st)))
    }
    films <- function(x, j) isTRUE(.clay_films_class(.kidpr_v(x, j, "clay_films_amount")) >= 3L)
    run <- setdiff(.kidpr_run(x, i, films)$rows, i)
    ss <- vapply(run, function(j) .kidpr_structure(x, j)$type, "")
    ss <- ss[!is.na(ss)]
    if (!length(ss)) return(.kidpr_r(NA, missing = "structure_type", note = "structure not recorded in the layer or its run"))
    c3 <- any(grepl(.KIDPR_COLUMNAR, ss))
    .kidpr_r(c3, tag = "natric_horizon",
             note = sprintf("structure not recorded: the structure of its run (%s) stands in, the structure criterion of the WRB natric horizon without the sodium",
                            paste(unique(ss), collapse = ", ")))
  })
}

# A solonetz horizon directly under a layer (key 18b, 51): the next layer
# down has continuous cutans and the solonetz structure.
.kidpr_over_solonetz <- function(x, i, from = i) {
  j <- .kidpr_next(x, from)
  if (is.na(j)) return(.kidpr_r(FALSE, note = "nothing below"))
  r <- .kidpr_rand(.kidpr_cutans(x, j), .kidpr_columnar(x, j))
  r$note <- c(sprintf("the layer below (%s) %s", .kidpr_des_text(x, j),
                      if (isTRUE(r$passed)) "is a solonetz horizon" else if (isFALSE(r$passed)) "is not a solonetz horizon"
                      else "may be a solonetz horizon"), r$note)
  r
}
.kidpr_des_text <- function(x, j) {
  d <- as.character(.kidpr_v(x, j, "designation"))
  if (is.na(d)) sprintf("%s-%s cm", format(x$top[j]), format(x$bot[j])) else d
}

# 16: b the vertic signs (V, its dark modification AV); c the solonetz
# structure with continuous humus-clay cutans (ASN); a AU. The key gives no
# exit for a dark surface horizon with none of the three: soilKey reads AU,
# its loose crumb-granular structure as evidence.
.kidpr_q16b <- function(x, i) .kidpr_vertic3(x, i)
.kidpr_q16c <- function(x, i) {
  cu <- .kidpr_cutans(x, i)
  if (isFALSE(cu$passed)) return(cu)
  .kidpr_rand(cu, .kidpr_columnar(x, i))
}
.kidpr_q16a <- function(x, i) {
  st <- .kidpr_structure(x, i)$type
  wh <- .kidpr_num(x, i, "worm_holes_pct")
  ok <- is.na(st) || grepl("granul|crumb|block|nut|prism|polyhed", st)
  .kidpr_r(TRUE, note = c(if (!is.na(st)) sprintf("structure %s%s", st, if (ok) "" else ", not the crumb-granular one of AU: read as AU, the dark-humus outcome"),
                          if (isTRUE(wh > 0)) sprintf("worm channels %s %%", format(wh))))
}

# 17: acid or slightly acid.
.kidpr_q17 <- function(x, i) .kidpr_acid(x, i)

# 18: b platy (dark when wet, whitish when dry) directly over ASN or BSN
# (SEL); a AJ.
.kidpr_q18b <- function(x, i) {
  st <- .kidpr_structure(x, i)$type
  platy <- if (is.na(st)) NA else grepl("platy|lamin|scaly|flaky|leaf|\u043f\u043b\u0438\u0442|\u043b\u0438\u0441\u0442", st)
  pl <- .kidpr_rl(platy, missing = "structure_type", note = if (!is.na(st)) sprintf("structure %s", st))
  if (isFALSE(platy)) return(pl)
  .kidpr_rand(pl, .kidpr_over_solonetz(x, i))
}
.kidpr_q18a <- function(x, i) .kidpr_r(TRUE)

# 19: the colour of the agro horizon: dark grey to black (PU), light grey or
# grey (P: grey as Q5 reads it, chroma 3 or less in 10YR, 2.5Y, 5Y or N,
# with a dry value of 5 or more; default 17, Hugo 2026-11-02), else brown,
# brownish or pale (PB).
.kidpr_q19a <- function(x, i) .kidpr_dark(x, i)
.kidpr_q19b <- function(x, i) {
  g <- .kidpr_grey(x, i)
  col <- .kidpr_colour(x, i, "dry")
  if (!isTRUE(g$passed)) return(g)
  p <- if (is.na(col$value)) NA else col$value >= 5
  .kidpr_colour_word(col, p, "light grey or grey, dry value 5 or more")
}

# 20: starts at the surface with a colour almost free of grey.
.kidpr_q20 <- function(x, i) .kidpr_surface_colour(x, i, grey = FALSE)

# 21: a the natural surface horizon, the darkest of the profile, chestnut or
# reddish brown (hue 7.5YR or redder), with permafrost (AK); b ploughed (PB);
# c an exposed horizon (go to 22).
.kidpr_q21a <- function(x, i) {
  ag <- .kidpr_agro(x, i)
  col <- .kidpr_colour(x, i, "dry")
  red <- .kidpr_reddish(col)
  others <- setdiff(x$ord, i)
  others <- others[!vapply(others, function(j) .kidpr_hard_rock(x, j) || isTRUE(.kidpr_org23(x, j)$passed), NA)]
  ov <- vapply(others, function(j) .kidpr_colour(x, j, "dry")$value, 0)
  # strictly the darkest (a tie is not), with the chroma of AK (2-4)
  darkest <- if (is.na(col$value)) NA else if (any(ov <= col$value, na.rm = TRUE)) FALSE
             else if (anyNA(ov)) NA else TRUE
  chroma <- if (is.na(col$chroma)) NA else col$chroma >= 2 && col$chroma <= 4
  pf <- .kidpr_permafrost(x)
  .kidpr_rand(.kidpr_r(.kidpr_not(ag$passed), missing = ag$missing, note = ag$note, tag = ag$tag),
              .kidpr_rl(red, missing = "munsell_hue_dry", note = sprintf("%s: %s", .kidpr_colour_text(col),
                                                                          if (isTRUE(red)) "chestnut or reddish brown" else "not reddish"),
                        tag = col$tag),
              .kidpr_rl(darkest, missing = "munsell_value_dry",
                        note = if (isTRUE(darkest)) "the darkest layer of the profile" else if (isFALSE(darkest)) "not the darkest layer"),
              .kidpr_rl(chroma, missing = "munsell_chroma_dry", note = if (isFALSE(chroma)) "not the chroma 2-4 of AK"),
              pf)
}
.kidpr_q21b <- function(x, i) .kidpr_agro(x, i)

# 22: the light horizon: (1) platy or structureless and the lightest of the
# profile (no layer above lighter, and lighter than the next one down: a
# higher value, or the same value with a chroma 2 lower), or (2)
# many whitish spots without effervescence. soilKey adds that no eluvial
# horizon effervesces continuously (chapter 3).
.kidpr_q22 <- function(x, i) {
  .kidpr_memo(x, sprintf("light_%d", i), function() {
    col <- .kidpr_colour(x, i, "dry")
    nx <- .kidpr_next(x, i)
    above <- .kidpr_above(x, i)
    above <- above[!vapply(above, function(j) .kidpr_litter(x, j) || isTRUE(.kidpr_org23(x, j)$passed), NA)]
    # lighter: a higher value, or the same value with a chroma 2 lower (M1)
    lighter <- function(a, b) {
      if (is.na(a$value) || is.na(b$value)) return(NA)
      a$value > b$value || (a$value == b$value && isTRUE(a$chroma <= b$chroma - 2))
    }
    la <- vapply(above, function(j) lighter(.kidpr_colour(x, j, "dry"), col), NA)
    ln <- if (is.na(nx) || .kidpr_hard_rock(x, nx)) NA else lighter(col, .kidpr_colour(x, nx, "dry"))
    lightest <- if (is.na(nx) || .kidpr_hard_rock(x, nx)) FALSE
                else if (is.na(col$value)) NA
                else if (any(la %in% TRUE) || isFALSE(ln)) FALSE
                else if (anyNA(la) || is.na(ln)) NA else TRUE
    st <- .kidpr_structure(x, i)$type
    flat <- if (is.na(st)) NA else grepl(paste0("platy|lamin|scaly|flaky|", .KIDPR_RAW_STRUCTURE), st)
    # a bluish or greenish layer is answered at 24, not here
    blue <- isTRUE(.kidpr_colour(x, i, "moist")$family %in% c("GY", "G", "BG", "B", "PB"))
    c1 <- .kidpr_and(lightest, flat, !blue)
    m <- .kidpr_morphons(x, i)
    wspots <- m$leached || any(.kidpr_morphon_whitish(m$mottles)) || any(.kidpr_morphon_whitish(m$variegated))
    ea <- .kidpr_eff_any(x, i)
    c2 <- .kidpr_and(wspots, .kidpr_not(ea$passed))
    ec <- .kidpr_eff_cont(x, i)
    p <- .kidpr_and(.kidpr_or(c1, c2), .kidpr_not(ec$passed))
    .kidpr_r(p, missing = if (is.na(p)) c(if (is.na(lightest)) col$missing, if (is.na(flat)) "structure_type",
                                          if (is.na(c2)) ea$missing, if (is.na(ec$passed)) ec$missing) else character(0),
             note = c(sprintf("%s%s", .kidpr_colour_text(col),
                              if (isTRUE(lightest)) ", the lightest of the profile" else if (isFALSE(lightest)) ", not the lightest" else ""),
                      if (!is.na(st)) sprintf("structure %s", st),
                      if (wspots) "whitish spots or morphons",
                      if (isTRUE(ec$passed)) "continuous effervescence: not an eluvial horizon",
                      col$note),
             tag = c(col$tag, ec$tag))
  })
}

# 23: the three vertic signs (V, then the key goes on at 24).
.kidpr_q23 <- function(x, i) .kidpr_vertic3(x, i)
# 24: over half the section in cold (bluish) colours: the moist matrix colour
# in the gley set. 25: olive.
.kidpr_q24 <- function(x, i) .kidpr_gley(x, i)
.kidpr_q25 <- function(x, i) .kidpr_olive(x, i)
# 26: long-seasonal or perennial frost.
.kidpr_q26 <- function(x, i) .kidpr_frost(x)

# 27: almost black (CRH); greyish or dirty brown (28); pale or pale light
# brown (29); other (30). Dry colour, one cut each: almost black value 2 or
# less with chroma 2 or less; dull brown value 3-5, chroma 3 or less;
# pale value 6 or more, chroma 4 or less, in red or yellow hues.
.kidpr_col27 <- function(x, i, which) {
  col <- .kidpr_colour(x, i, "dry")
  v <- col$value; c <- col$chroma; f <- col$family
  p <- switch(which,
              a = if (is.na(v) || is.na(c)) NA else v <= 2 && c <= 2,
              b = if (is.na(v) || is.na(c) || is.na(f)) NA else v >= 3 && v <= 5 && c <= 3 && f %in% c("YR", "Y", "N"),
              c = if (is.na(v) || is.na(c) || is.na(f)) NA else v >= 6 && c <= 4 && c >= 1 && f %in% c("YR", "Y"))
  word <- c(a = "almost black", b = "greyish or dirty brown", c = "pale")[[which]]
  .kidpr_colour_word(col, p, word)
}
.kidpr_q27a <- function(x, i) .kidpr_col27(x, i, "a")
.kidpr_q27b <- function(x, i) .kidpr_col27(x, i, "b")
.kidpr_q27c <- function(x, i) .kidpr_col27(x, i, "c")

# 28: a no structure, thixotropic when wet, or frozen (CR); b friable,
# angular-crumbly or lenticular cryogenic structure (CRM); any other
# structure goes on at 30 (the review's M2).
.kidpr_q28b <- function(x, i) {
  st <- .kidpr_structure(x, i)
  if (is.na(st$type)) return(.kidpr_r(NA, missing = "structure_type"))
  cryo <- (grepl("lentic|crumb|granul|platy|lamin", st$type) ||
             (grepl("angular", st$type) && (!grepl("block", st$type) || isTRUE(grepl("fine", st$size))))) &&
    !grepl("prism|column|wedge", st$type)
  .kidpr_r(cryo, note = sprintf("structure %s%s", st$type, if (cryo) ": cryogenic" else ": not the cryogenic one of CRM"))
}
.kidpr_q28a <- function(x, i) {
  st <- .kidpr_structure(x, i)
  raw <- if (is.na(st$type) && is.na(st$grade)) NA
         else isTRUE(grepl(.KIDPR_RAW_STRUCTURE, st$type)) || isTRUE(.field_grade_class(st$grade) == 0L)
  th <- .kidpr_num(x, i, "thixotropic_index")
  frozen <- isTRUE(.kidpr_num(x, i, "permafrost_temp_C") <= 0) || isTRUE(.kidpr_num(x, i, "ice_pct") > 0)
  p <- .kidpr_or(raw, isTRUE(th >= 50), frozen)
  .kidpr_r(p, missing = if (is.na(p)) "structure_type" else character(0),
           note = c(if (!is.na(st$type)) sprintf("structure %s", st$type), if (isTRUE(th >= 50)) "thixotropic",
                    if (frozen) "frozen"))
}

# 29: a dull weak iron films, weak structure, a BCA below (BPL); b none (30).
.kidpr_bca_like <- function(x, j) .kidpr_and(.kidpr_eff_cont(x, j)$passed, .kidpr_carb_neo(x, j)$passed)
.kidpr_q29a <- function(x, i) {
  st <- .kidpr_structure(x, i)
  gc <- .field_grade_class(st$grade)
  weak <- if (!is.na(gc)) gc <= 1 else if (!is.na(st$type)) grepl(.KIDPR_RAW_STRUCTURE, st$type) else NA
  b <- .kidpr_below(x, i)
  bca <- if (!length(b)) FALSE else .kidpr_or(vapply(b, function(j) .kidpr_bca_like(x, j), NA))
  .kidpr_rand(.kidpr_rl(weak, missing = "structure_grade", note = if (isTRUE(weak)) "weak structure"),
              .kidpr_rl(bca, missing = c("secondary_carbonates_form", "caco3_pct"),
                        note = if (isTRUE(bca)) "a carbonate-accumulative horizon below" else if (isFALSE(bca)) "no carbonate-accumulative horizon below"))
}

# 30: the colour comes from continuous iron or iron-humus films on the grains
# (films on under 20 % of the grains: no). uncoated_grains_pct; not
# recorded, soilKey's spodic horizon stands in, and so do an iron pan or iron
# nodules over half the section and andic volcanic ash (positions 54 and 55
# follow); a coloured layer of sand
# to light loam stays undecided; a loam or clay, or a grey one, has no films
# that colour it.
.kidpr_films <- function(x, i) {
  .kidpr_memo(x, sprintf("films_%d", i), function() {
    ug <- .kidpr_num(x, i, "uncoated_grains_pct")
    if (!is.na(ug)) return(.kidpr_r(ug <= 80, note = sprintf("uncoated grains %s %%: films on %s 20 %% of the grains",
                                                            format(ug), if (ug <= 80) "at least" else "under")))
    sp <- .kidpr_wrb_layer(x, "spodic", i)
    if (isTRUE(sp)) return(.kidpr_r(TRUE, tag = "spodic", note = "films not recorded: soilKey's spodic horizon stands in (passes)"))
    pt <- .kidpr_txt(x, i, "pan_type")
    if (isTRUE(grepl("iron|ortstein|ferricrete|placic|bog", pt)) || isTRUE(.kidpr_num(x, i, "ferric_nodules_pct") > 50))
      return(.kidpr_r(TRUE, note = "films not recorded: an iron pan"))
    an <- .kidpr_ash(x, i)
    if (isTRUE(an$passed) && isTRUE(.kidpr_wrb_layer(x, "andic_properties", i)))
      return(.kidpr_r(TRUE, tag = "andic_properties", note = "films not recorded: andic volcanic ash"))
    col <- .kidpr_colour(x, i, "dry")
    g <- .kidpr_grad(x, i)
    light <- if (is.na(g$step)) NA else g$step <= 3
    # a film colour, brighter than the parent material (the review)
    coloured <- if (is.na(col$chroma) || is.na(col$family)) NA
                else .kidpr_and(col$family %in% c("R", "YR", "Y") && col$chroma >= 3, .kidpr_brighter(x, i))
    coarse <- isTRUE(.kidpr_num(x, i, "coarse_fragments_pct") >= 50)
    open <- .kidpr_and(.kidpr_or(light, coarse), coloured)
    if (isFALSE(open)) return(.kidpr_r(FALSE, tag = if (!is.na(sp)) "spodic",
                                       note = c(sprintf("films not recorded; %s", if (isFALSE(coloured)) "not a film colour"
                                                         else "loam or clay: the colour is not from films on the grains"),
                                                if (isFALSE(sp)) "soilKey's spodic horizon fails", g$note)))
    .kidpr_r(NA, missing = "uncoated_grains_pct", tag = c(g$tag, col$tag),
             note = c(sprintf("films not recorded on a coloured layer (%s)", .kidpr_colour_text(col)),
                      if (isFALSE(sp)) "soilKey's spodic horizon fails (no films of BHF; BFM still possible)", g$note))
  })
}
.kidpr_q30 <- function(x, i) .kidpr_films(x, i)

# 31: loam or clay with continuous clay or humus-clay cutans on the lateral
# or all faces of the peds.
.kidpr_q31 <- function(x, i) {
  g <- .kidpr_grad_test(x, i, function(s) s >= 3, "loam or clay")
  if (isFALSE(g$passed)) return(g)
  .kidpr_rand(g, .kidpr_cutans(x, i))
}
# 32: continuous effervescence.
.kidpr_q32 <- function(x, i) .kidpr_eff_cont(x, i)

# The structure of a layer transformed from that of the parent rock (key 33,
# 65b): a pedogenic structure where the parent is massive, single-grained or
# layered (or of a lower grade); without the parent's structure, or the
# layer's own, soilKey's cambic horizon stands in.
.kidpr_transformed <- function(x, i) {
  .kidpr_memo(x, sprintf("transformed_%d", i), function() {
    st <- .kidpr_structure(x, i)
    cam <- function(why) {
      w <- .kidpr_wrb_layer(x, "cambic", i)
      .kidpr_r(w, missing = if (is.na(w)) c("structure_type", "structure_grade") else character(0), tag = "cambic",
               note = sprintf("%s: soilKey's cambic horizon %s", why,
                              if (is.na(w)) "undecided" else if (w) "stands in (passes)" else "stands in (fails)"))
    }
    if (is.na(st$type)) return(cam("structure not recorded"))
    gc <- .field_grade_class(st$grade)
    own <- grepl(.KIDPR_PED_STRUCTURE, st$type) && !grepl(.KIDPR_RAW_STRUCTURE, st$type) && !isTRUE(gc == 0L)
    if (!own) return(.kidpr_r(FALSE, note = sprintf("structure %s: not pedogenic", st$type)))
    p <- .kidpr_parent_row(x, i)
    ps <- if (is.na(p)) list(type = NA_character_) else .kidpr_structure(x, p)
    if (is.na(ps$type)) return(cam("the structure of the parent material not recorded"))
    if (grepl(.KIDPR_RAW_STRUCTURE, ps$type))
      return(.kidpr_r(TRUE, note = sprintf("structure %s over a parent %s", st$type, ps$type)))
    pg <- .field_grade_class(ps$grade)
    up <- if (!is.na(gc) && !is.na(pg)) gc > pg else NA
    .kidpr_r(up, missing = if (is.na(up)) "structure_grade" else character(0),
             note = sprintf("structure %s over a parent %s", st$type, ps$type))
  })
}

# Under a humus, eluvial or agro horizon (key 33, 65b); an exposed horizon at
# the surface (key 21c) is read as lying under its removed humus horizon.
.kidpr_under_humus <- function(x, i) {
  codes <- c(.kidpr_codes_of(c("humus", "eluvial")), .KIDPR_AGRO)
  a <- .kidpr_above_in(x, i, codes)
  if (isTRUE(a)) return(.kidpr_r(TRUE, note = "under a humus, eluvial or agro horizon"))
  st <- .kidpr_is_first_mineral(x, i)
  if (isTRUE(st)) return(.kidpr_r(TRUE, note = "an exposed horizon at the surface, read as under its removed humus horizon"))
  .kidpr_rl(.kidpr_or(a, st), missing = "designation",
            note = if (isFALSE(a)) "not under a humus or eluvial horizon")
}
.kidpr_q33 <- function(x, i) {
  u <- .kidpr_under_humus(x, i)
  if (isFALSE(u$passed)) return(u)
  .kidpr_rand(u, .kidpr_transformed(x, i))
}

# 34: the deposit is grey (humified). 35, 38: its thickness. 36: dark. 37:
# acid.
.kidpr_q34 <- function(x, i) .kidpr_grey(x, i)
.kidpr_q35 <- function(x, i) {
  d <- .kidpr_deposit(x)
  .kidpr_r(d$thick < 40, note = sprintf("deposit %s cm thick", format(d$thick)))
}
.kidpr_q36 <- function(x, i) .kidpr_dark(x, i)
.kidpr_q37 <- function(x, i) .kidpr_acid(x, i)
.kidpr_q38 <- function(x, i) {
  d <- .kidpr_deposit(x)
  .kidpr_r(d$thick <= 40, note = sprintf("deposit %s cm thick", format(d$thick)))
}

# 39: decomposition 50 % or more (H).
.kidpr_q39 <- function(x, i) .kidpr_decomp50(x, i)
# 40: mechanically mixed, homogenised, abrupt smooth base (PT).
.kidpr_q40 <- function(x, i) .kidpr_agro(x, i)

# The peat body of positions 41 and 44: the adjoining layers of weakly
# decomposed natural peat around a layer, without H, PT or PTR, as the key
# writes it (Hugo, 2026-11-02); truncated when the description ends in it.
.kidpr_peat_body <- function(x, i) {
  r <- .kidpr_run(x, i, function(x, j) .kidpr_and(.kidpr_org23(x, j)$passed, .kidpr_not(.kidpr_decomp50(x, j)$passed),
                                                  .kidpr_not(.kidpr_agro(x, j)$passed)))
  r$truncated <- identical(r$rows[length(r$rows)], x$ord[x$n])
  r
}
# 41: the peat 10 cm or less (O), 10-50 cm (42), over 50 cm (43).
.kidpr_q41a <- function(x, i) {
  b <- .kidpr_peat_body(x, i)
  if (is.na(b$thick)) return(.kidpr_r(NA, missing = c("top_cm", "bottom_cm")))
  p <- if (b$thick > 10) FALSE else if (b$truncated || b$open) NA else TRUE
  .kidpr_r(p, note = sprintf("organic layers %s cm thick%s", format(b$thick), if (b$truncated) ", the description ends in them" else ""))
}
.kidpr_q41b <- function(x, i) {
  b <- .kidpr_peat_body(x, i)
  if (is.na(b$thick)) return(.kidpr_r(NA, missing = c("top_cm", "bottom_cm")))
  p <- if (b$thick > 50) FALSE else if (b$truncated || b$open) NA else TRUE
  .kidpr_r(p, note = sprintf("organic layers %s cm thick", format(b$thick)))
}

# 42: hydrophilic (mire) residues (T) or mesophilic ones (TJ); 43:
# oligotrophic (44), eutrophic or mesotrophic (45), mesophilic (TJ). By
# peat_moor_type (high-moor oligotrophic; transitional and low-moor eutrophic
# or mesotrophic; mesophilic); Sphagnum over half the fibres is mire peat,
# oligotrophic; a pH(H2O) of 6.5 or more is not oligotrophic (Q22).
.kidpr_moor <- function(x, i) {
  mt <- .kidpr_txt(x, i, "peat_moor_type")
  sph <- .kidpr_num(x, i, "sphagnum_fibres_pct")
  ph <- .kidpr_num(x, i, "ph_h2o")
  list(type = mt, sph = sph, ph = ph)
}
.kidpr_q42 <- function(x, i) {
  m <- .kidpr_moor(x, i)
  if (!is.na(m$type)) return(.kidpr_r(m$type != "mesophilic", note = sprintf("peat of %s plants", m$type)))
  if (isTRUE(m$sph > 50)) return(.kidpr_r(TRUE, note = "Sphagnum peat"))
  .kidpr_r(NA, missing = "peat_moor_type")
}
.kidpr_q43a <- function(x, i) {
  m <- .kidpr_moor(x, i)
  if (!is.na(m$type)) return(.kidpr_r(m$type == "high-moor", note = sprintf("peat of %s plants", m$type)))
  if (isTRUE(m$sph > 50)) return(.kidpr_r(TRUE, tag = "sphagnum", note = "Sphagnum over half the fibres: oligotrophic"))
  if (isTRUE(m$ph >= 6.5)) return(.kidpr_r(FALSE, tag = "ph_not_oligotrophic", note = sprintf("pH(H2O) %s: not oligotrophic", format(m$ph))))
  .kidpr_r(NA, missing = c("peat_moor_type", "sphagnum_fibres_pct"))
}
.kidpr_q43b <- function(x, i) {
  m <- .kidpr_moor(x, i)
  if (!is.na(m$type)) return(.kidpr_r(m$type %in% c("transitional", "low-moor"), note = sprintf("peat of %s plants", m$type)))
  .kidpr_r(NA, missing = "peat_moor_type")
}
# 44, 45: in the upper part of the peat body (TO, TE) or deeper (TT): a layer
# starting within the upper 50 cm of the body (Q22, reading the key's 0-30 cm
# as the start of the layer).
.kidpr_q44 <- function(x, i) {
  b <- .kidpr_peat_body(x, i)
  d <- x$top[i] - b$top
  if (is.na(d)) return(.kidpr_r(NA, missing = "top_cm"))
  .kidpr_r(d < 50, note = sprintf("starts %s cm below the top of the peat", format(d)))
}

# 46: a agro (PTR); c natural, dark grey to black with plant residues (AH);
# b natural, a mixture of organic material and uncoated mineral grains (47).
.kidpr_q46a <- function(x, i) .kidpr_agro(x, i)
.kidpr_q46c <- function(x, i) .kidpr_dark(x, i)
# 47: the coarse-humus layer over 10 cm (AO) or less (ao).
.kidpr_ao_like <- function(x, j) {
  .kidpr_and(.kidpr_res13(x, j)$passed, .kidpr_not(.kidpr_org23(x, j)$passed),
             .kidpr_not(.kidpr_agro(x, j)$passed), .kidpr_not(.kidpr_dark(x, j)$passed))
}
.kidpr_q47 <- function(x, i) .kidpr_run_over(.kidpr_run(x, i, .kidpr_ao_like), 10, "coarse-humus layer")

# 48: the colour of the light horizon homogeneous (49) or heterogeneous
# (53): heterogeneous when the layer is variegated, has leached spots, or
# whitish mottles in a matrix that is not whitish (mottles not recorded:
# absent).
.kidpr_heterogeneous <- function(x, i) {
  m <- .kidpr_morphons(x, i)
  col <- .kidpr_colour(x, i, "dry")
  wm <- .kidpr_is_whitish(col$value, col$chroma)
  het <- nrow(m$variegated) >= 2L || m$leached || (any(.kidpr_morphon_whitish(m$mottles)) && !isTRUE(wm))
  het
}
.kidpr_q48 <- function(x, i) {
  het <- .kidpr_heterogeneous(x, i)
  .kidpr_r(!het, note = if (het) "heterogeneous colour (whitish morphons)" else "homogeneous colour")
}

# The light homogeneous horizon a layer belongs to (key 49, 50, 52).
.kidpr_light_like <- function(x, j) .kidpr_and(.kidpr_q22(x, j)$passed, !.kidpr_heterogeneous(x, j))
.kidpr_light_run <- function(x, i) .kidpr_run(x, i, .kidpr_light_like)

# 49: the texture of the light horizon equal to the middle horizon below it
# (the first layer under it that is not light, past a BEL) (50), or lighter
# by one gradation or more (51).
.kidpr_q49 <- function(x, i) {
  r <- .kidpr_light_run(x, i)
  j <- .kidpr_next(x, r$rows[length(r$rows)])
  # the first middle horizon: past the light layers under the run (a BEL)
  while (!is.na(j) && isTRUE(.kidpr_q22(x, j)$passed)) j <- .kidpr_next(x, j)
  if (is.na(j)) return(.kidpr_r(NA, missing = .KIDPR_TEXTURE_MISSING, note = "no horizon below"))
  d <- .kidpr_grad_diff(x, i, j)
  p <- if (is.na(d$diff)) NA else d$diff > -1
  .kidpr_r(p, missing = if (is.na(p)) .KIDPR_TEXTURE_MISSING else character(0), tag = d$tag,
           note = c(if (!is.na(p)) sprintf("texture %s the horizon below (%s)", if (p) "the same as" else "lighter than",
                                           .kidpr_des_text(x, j)), d$note))
}
# 50, 52: the thickness of the light horizon.
.kidpr_q50 <- function(x, i) .kidpr_run_over(.kidpr_light_run(x, i), 2, "light horizon")
.kidpr_q52 <- function(x, i) .kidpr_run_over(.kidpr_light_run(x, i), 5, "light horizon")
# 51: directly over ASN or BSN (SEL).
.kidpr_q51 <- function(x, i) {
  r <- .kidpr_light_run(x, i)
  .kidpr_over_solonetz(x, i, from = r$rows[length(r$rows)])
}
# 53: whitish and grey morphons (AEL) or whitish and brown ones (BEL).
.kidpr_q53 <- function(x, i) {
  m <- .kidpr_morphons(x, i)
  g <- .kidpr_grey(x, i)
  brown <- any(.kidpr_morphon_brown(m$mottles)) || any(.kidpr_morphon_brown(m$variegated))
  matrix_brown <- .kidpr_not(g$passed)
  p <- .kidpr_or(brown, matrix_brown)
  .kidpr_r(.kidpr_not(p), missing = if (is.na(p)) g$missing else character(0),
           note = c(if (isTRUE(p)) "whitish and brown morphons" else if (isFALSE(p)) "whitish, light grey and grey morphons", g$note),
           tag = g$tag)
}

# 54: mostly cemented, or a continuous plate (F): a cementation class of
# moderately or more, an iron pan, or iron nodules over half the section
# (cementation not recorded: none).
.kidpr_q54 <- function(x, i) {
  cc <- .kidpr_txt(x, i, "cementation_class")
  pt <- .kidpr_txt(x, i, "pan_type")
  fn <- .kidpr_num(x, i, "ferric_nodules_pct")
  cem <- isTRUE(grepl("moderat|strong|indurat|cement", cc)) && !isTRUE(grepl("weak|none|non", cc))
  pan <- isTRUE(grepl("iron|ortstein|ferricrete|placic|bog", pt))
  p <- cem || pan || isTRUE(fn > 50)
  .kidpr_r(p, note = if (p) "cemented by iron, or a continuous plate" else "not cemented")
}

# Volcanic ash: the layer's origin, or 30 % volcanic glass.
.kidpr_ash <- function(x, i) {
  o <- paste(.kidpr_txt(x, i, "layer_origin"), .kidpr_txt(x, i, "rock_origin"))
  vg <- .kidpr_num(x, i, "volcanic_glass_pct")
  p <- grepl("tephr|pyroclast|volcan|ash|\u043f\u0435\u043f\u0435\u043b", o) || isTRUE(vg >= 30)
  .kidpr_r(p, note = if (p) "volcanic ash")
}
# 55: bright ochre, roe structure, in volcanic ash (BAN). Without an ash
# record, soilKey's andic properties stand in for it.
.kidpr_q55 <- function(x, i) {
  ash <- .kidpr_ash(x, i)
  if (!isTRUE(ash$passed)) {
    an <- .kidpr_wrb_layer(x, "andic_properties", i)
    ash <- if (isTRUE(an)) .kidpr_r(TRUE, tag = "andic_properties", note = "no ash recorded: soilKey's andic properties stand in (pass)")
           else .kidpr_r(FALSE, note = "no volcanic ash recorded")
  }
  st <- .kidpr_structure(x, i)$type
  roe <- if (is.na(st)) NA else grepl("granul|roe|crumb", st)
  col <- .kidpr_colour(x, i, "dry")
  ochre <- if (is.na(col$chroma) || is.na(col$family)) NA else col$family %in% c("YR", "Y") && col$chroma >= 6
  .kidpr_rand(ash, .kidpr_rl(roe, missing = "structure_type", note = if (!is.na(st)) sprintf("structure %s", st)),
              .kidpr_colour_word(col, ochre, "bright ochre"))
}

# Is a layer brighter (more intensely coloured) than its parent material: a
# higher chroma, or a hue 2.5 units redder? The parent itself, or a layer
# with no parent below, is not.
.kidpr_brighter <- function(x, i) {
  p <- .kidpr_parent_row(x, i)
  if (is.na(p)) return(FALSE)
  ci <- .kidpr_colour(x, i, "dry"); cp <- .kidpr_colour(x, p, "dry")
  if (is.na(ci$chroma) || is.na(cp$chroma)) return(NA)
  ui <- suppressWarnings(.munsell_hue_units(ci$hue)); up <- suppressWarnings(.munsell_hue_units(cp$hue))
  ci$chroma > cp$chroma || isTRUE(ui <= up - 2.5)
}
# 56a: rusty brown or brown (hue 7.5YR or redder, or a value of 5 or less;
# not the yellow ochre of BF), more intensely coloured than the parent
# material (BFM).
.kidpr_q56a <- function(x, i) {
  col <- .kidpr_colour(x, i, "dry")
  rusty <- .kidpr_or(.kidpr_reddish(col), if (is.na(col$value)) NA else col$value <= 5)
  b <- .kidpr_brighter(x, i)
  .kidpr_rand(.kidpr_colour_word(col, rusty, "rusty brown or brown"),
              .kidpr_rl(b, missing = "munsell_chroma_dry",
                        note = if (isTRUE(b)) "more intensely coloured than the parent material" else if (isFALSE(b)) "not brighter than the parent material"))
}

# 56: b the colour brightest at the top of the horizon, paling downward
# (57, BHF); a uniform, rusty brown, more intense than the parent (BFM);
# neither, a film horizon that is not BFM is read as BHF (57). Within the
# horizon: the next (or the previous) layer with films is paler (value 1
# higher or chroma 1 lower) or not; a horizon of one layer: soilKey's spodic
# horizon stands in.
.kidpr_q56b <- function(x, i) {
  pale <- function(a, b) {   # is b paler than a?
    ca <- .kidpr_colour(x, a, "dry"); cb <- .kidpr_colour(x, b, "dry")
    if (is.na(ca$value) || is.na(cb$value)) return(NA)
    cb$value >= ca$value + 1 || isTRUE(cb$chroma <= ca$chroma - 1)
  }
  nx <- .kidpr_next(x, i); pv <- .kidpr_prev(x, i)
  if (!is.na(nx) && isTRUE(.kidpr_films(x, nx)$passed)) {
    p <- pale(i, nx)
    if (!is.na(p)) return(.kidpr_r(p, note = if (p) "paler in the layer below: the colour fades downward" else "uniform colour with the layer below"))
  }
  if (!is.na(pv) && isTRUE(.kidpr_films(x, pv)$passed)) {
    p <- pale(pv, i)
    if (!is.na(p)) return(.kidpr_r(p, note = if (p) "paler than the layer above: the colour fades downward" else "uniform colour with the layer above"))
  }
  sp <- .kidpr_wrb_layer(x, "spodic", i)
  .kidpr_r(sp, missing = if (is.na(sp)) c("munsell_value_dry", "uncoated_grains_pct") else character(0), tag = "spodic",
           note = sprintf("a horizon of one layer: soilKey's spodic horizon %s", if (is.na(sp)) "undecided"
                          else if (sp) "stands in (passes, BHF)" else "stands in (fails, BFM)"))
}
# 57: coffee brown (BH), ochre (BF), or neither clearly (BHF): dry value 5
# or less with chroma 4 or more coffee, value 6 or more with chroma 6 or
# more ochre (the lower ends of the chapter 3 ranges); any other colour
# leaves the choice to 57c. Without a colour, humus over 3 % (OC over 1.74 %)
# BH and under it BF.
.kidpr_q57 <- function(x, i, which) {
  col <- .kidpr_colour(x, i, "dry")
  oc <- .kidpr_num(x, i, "oc_pct")
  if (!is.na(col$value) && !is.na(col$chroma)) {
    p <- if (which == "a") col$value <= 5 && col$chroma >= 4 else col$value >= 6 && col$chroma >= 6
    return(.kidpr_colour_word(col, p, if (which == "a") "coffee brown" else "ochre"))
  }
  if (!is.na(oc)) {
    hum <- oc * .KIDPR_HUMUS_OC
    p <- if (which == "a") hum > 3 else hum < 3
    return(.kidpr_r(p, note = sprintf("no colour; humus %s %%", format(round(hum, 2)))))
  }
  .kidpr_r(FALSE, note = "neither the colour nor the humus recorded")
}

# 58: the solonetz structure (59) or prismatic, nutty-prismatic with cutans
# (60). 59: dark grey or black (ASN) or brown to dark brown (BSN), moist.
.kidpr_q58 <- function(x, i) .kidpr_columnar(x, i)
.kidpr_q59 <- function(x, i) {
  col <- .kidpr_colour(x, i, "moist")
  p <- if (!is.na(col$value) && col$value > 3) FALSE else if (!is.na(col$chroma) && col$chroma > 2) FALSE
       else if (is.na(col$value) || is.na(col$chroma)) NA else TRUE
  .kidpr_colour_word(col, p, "dark grey or black")
}

# 60: a continuous effervescence with white-eye (CAT); b no continuous
# effervescence (61); otherwise the key has no exit.
.kidpr_q60a <- function(x, i) .kidpr_rand(.kidpr_eff_cont(x, i), .kidpr_belo(x, i))
.kidpr_q60b <- function(x, i) {
  e <- .kidpr_eff_cont(x, i)
  .kidpr_r(.kidpr_not(e$passed), missing = e$missing, note = e$note, tag = e$tag)
}

# 61: heavier than the overlying humus or eluvial horizons (BT) or about the
# same (BI): the clay ratio to the lightest of them 1.4 or more (Q7); without
# the clay, one gradation heavier, flagged.
.kidpr_q61 <- function(x, i) {
  codes <- c(.kidpr_codes_of(c("humus", "eluvial")), .KIDPR_AGRO)
  a <- .kidpr_above(x, i)
  inn <- vapply(a, function(j) .kidpr_code_in(x, j, codes), NA)
  over <- a[inn %in% TRUE]
  if (!length(over)) return(.kidpr_r(NA, missing = "designation",
                                     note = "no overlying humus or eluvial horizon to compare the texture with"))
  cl <- .kidpr_num(x, i, "clay_pct")
  oc <- vapply(over, function(j) .kidpr_num(x, j, "clay_pct"), 0)
  if (!is.na(cl) && any(!is.na(oc))) {
    lo <- min(oc, na.rm = TRUE)
    ratio <- if (lo > 0) cl / lo else Inf
    return(.kidpr_r(ratio >= 1.4, note = sprintf("clay ratio %s to the overlying horizon (%s %% over %s %%)",
                                                 format(round(ratio, 2)), format(cl), format(lo))))
  }
  ds <- vapply(over, function(j) .kidpr_grad_diff(x, i, j)$diff, 0)
  if (all(is.na(ds))) return(.kidpr_r(NA, missing = c("clay_pct", .KIDPR_TEXTURE_MISSING)))
  dm <- max(ds, na.rm = TRUE)
  d1 <- .kidpr_grad_diff(x, i, over[which.max(ds)])
  .kidpr_r(dm >= 1, tag = c("grad_for_ratio", d1$tag),
           note = c(sprintf("no clay: %s gradation(s) heavier than the overlying horizon", format(dm)), d1$note))
}

# 62: distinct carbonate neoformations.
.kidpr_q62 <- function(x, i) .kidpr_carb_neo(x, i)
# 63: a darker than the parent, white-eye, thin clay cutans on prismatic-
# nutty peds (CAT); b pale like the parent, weak structure, no or
# fragmentary cutans (BCA).
.kidpr_q63a <- function(x, i) {
  p <- .kidpr_parent_row(x, i)
  ci <- .kidpr_colour(x, i, "dry")
  darker <- if (is.na(p)) NA else { cp <- .kidpr_colour(x, p, "dry"); if (is.na(ci$value) || is.na(cp$value)) NA else ci$value < cp$value }
  .kidpr_rand(.kidpr_belo(x, i), .kidpr_cutans(x, i, thin = TRUE),
              .kidpr_rl(darker, missing = "munsell_value_dry", note = if (isTRUE(darker)) "darker than the parent material"
                                                                     else if (isFALSE(darker)) "not darker than the parent material"))
}
# 64: a light pale or dirty white, violent effervescence with CO2 of
# carbonates over 30 % (CaCO3 over 68.2 %), soft (ML); b uniform
# effervescence otherwise (the feature ca, then 65).
.kidpr_q64 <- function(x, i) {
  cc <- .kidpr_num(x, i, "caco3_pct")
  col <- .kidpr_colour(x, i, "dry")
  white <- if (is.na(col$value)) NA else col$value >= 7
  carb <- if (is.na(cc)) NA else cc > 68.2
  .kidpr_rand(.kidpr_rl(carb, missing = "caco3_pct", note = if (!is.na(cc)) sprintf("CaCO3 %s %% (CO2 of carbonates %s 30 %%)",
                                                                                   format(cc), if (cc > 68.2) "over" else "not over")),
              .kidpr_colour_word(col, white, "light pale or dirty white"))
}
# 65: a brownish or reddish (chestnut) with a fine prismatic or pencil-like
# structure breaking into small nutty peds, effervescing uniformly (BMK; the
# book's parenthetical and chapter 3); b a transformed structure
# under humus or eluvial horizons without effervescence, or under a humus
# horizon or BMK with it (BM); otherwise no diagnostic horizon (66).
.kidpr_q65a <- function(x, i) {
  col <- .kidpr_colour(x, i, "dry")
  chest <- .kidpr_and(.kidpr_reddish(col), if (is.na(col$chroma)) NA else col$chroma >= 3)
  st <- .kidpr_structure(x, i)
  pencil <- if (is.na(st$type)) NA else grepl("prism|pencil", st$type) &&
    (isTRUE(grepl("fine", st$size)) || isTRUE(grepl("block|nut|polyhed", st$secondary)))
  .kidpr_rand(.kidpr_colour_word(col, chest, "brownish or reddish brown (chestnut)"),
              .kidpr_rl(pencil, missing = "structure_type", note = if (!is.na(st$type)) sprintf("structure %s", st$type)),
              .kidpr_eff_cont(x, i))
}
.kidpr_q65b <- function(x, i) {
  e <- .kidpr_eff_cont(x, i)
  pos <- if (isTRUE(e$passed)) {
    codes <- c(.kidpr_codes_of("humus"), .KIDPR_AGRO, "BMK")
    a <- .kidpr_above_in(x, i, codes)
    .kidpr_rl(a, missing = "designation", note = if (isTRUE(a)) "effervescent, under a humus horizon or BMK"
                                                 else if (isFALSE(a)) "effervescent, not under a humus horizon or BMK")
  } else if (isFALSE(e$passed)) .kidpr_under_humus(x, i)
  else .kidpr_ror(.kidpr_under_humus(x, i), .kidpr_r(.kidpr_above_in(x, i, c(.kidpr_codes_of("humus"), "BMK"))))
  .kidpr_rand(.kidpr_transformed(x, i), pos)
}


# --- the tree ---------------------------------------------------------------------

#' The key 2.1 as positions for .walk_key(), in the book's order (1 to 66),
#' before the copies of the overlays and of the features that continue.
#' @noRd
.kidpr_key_base <- function() {
  q <- function(n) get(sprintf(".kidpr_q%02d", n), mode = "function")
  qq <- function(s) get(sprintf(".kidpr_q%s", s), mode = "function")
  k <- list()
  k[["1"]] <- .kn2("1", q(1), "2", "5")
  k[["2"]] <- .kn2("2", q(2), "3", "4")
  k[["3"]] <- .kn2("3", q(3), "SS", "5")            # 3b: S, go on at 5 (the S copy)
  k[["4"]] <- .kn2("4", q(4), "5", "5")             # 4a: X, go on at 5 (the X copy)
  k[["5"]] <- .kn2("5", q(5), "W", "6")
  k[["6"]] <- .kn2("6", q(6), "7", "8")
  k[["7"]] <- list(branches = list(.kb("a", "TUR", .kq("7a", qq("07a"))), .kb("b", "tr", .kq("7b", qq("07b"))),
                                   .kb("c", "_gap7")))
  k[["8"]] <- .kn2("8", q(8), "9", "10")
  k[["9"]] <- .kn2("9", q(9), "34", "10")
  k[["10"]] <- .kn2("10", q(10), "39", "11")
  k[["11"]] <- .kn2("11", q(11), "46", "12")
  k[["12"]] <- .kn2("12", q(12), "13", "20")
  # 13a: the feature u, go on at 14 (a surface horizon) or 31 (a middle one)
  # in the u copies
  k[["13"]] <- list(branches = list(.kb("a", "14", .kq("13a", qq("13s"))), .kb("a", "31", .kq("13d", qq("13m"))),
                                    .kb("b", "14")))
  k[["14"]] <- list(branches = list(.kb("b", "19", .kq("14", q(14))), .kb("a", "15")))
  k[["15"]] <- .kn2("15", q(15), "16", "17")
  k[["16"]] <- list(branches = list(.kb("b", "AV", .kq("16b", qq("16b"))), .kb("c", "ASN", .kq("16c", qq("16c"))),
                                    .kb("a", "AU", .kq("16a", qq("16a")))))
  k[["17"]] <- .kn2("17", q(17), "AY", "18")
  k[["18"]] <- list(branches = list(.kb("b", "SEL", .kq("18b", qq("18b"))), .kb("a", "AJ", .kq("18a", qq("18a")))))
  k[["19"]] <- list(branches = list(.kb("a", "PU", .kq("19a", qq("19a"))), .kb("b", "P", .kq("19b", qq("19b"))),
                                    .kb("c", "PB")))
  k[["20"]] <- .kn2("20", q(20), "21", "22")
  k[["21"]] <- list(branches = list(.kb("a", "AK", .kq("21a", qq("21a"))), .kb("b", "PB", .kq("21b", qq("21b"))),
                                    .kb("c", "22")))
  k[["22"]] <- .kn2("22", q(22), "48", "23")
  k[["23"]] <- .kn2("23", q(23), "24", "24")        # 23a: V, go on at 24 (the V copy)
  k[["24"]] <- .kn2("24", q(24), "G", "25")
  k[["25"]] <- .kn2("25", q(25), "Q", "26")
  k[["26"]] <- .kn2("26", q(26), "27", "30")
  k[["27"]] <- list(branches = list(.kb("a", "CRH", .kq("27a", qq("27a"))), .kb("b", "28", .kq("27b", qq("27b"))),
                                    .kb("c", "29", .kq("27c", qq("27c"))), .kb("d", "30")))
  k[["28"]] <- list(branches = list(.kb("a", "CR", .kq("28a", qq("28a"))), .kb("b", "CRM", .kq("28b", qq("28b"))),
                                    .kb("c", "30")))
  k[["29"]] <- list(branches = list(.kb("a", "BPL", .kq("29a", qq("29a"))), .kb("b", "30")))
  k[["30"]] <- .kn2("30", q(30), "54", "31")
  k[["31"]] <- .kn2("31", q(31), "58", "32")
  k[["32"]] <- .kn2("32", q(32), "62", "33")
  k[["33"]] <- .kn2("33", q(33), "65", "66")
  k[["34"]] <- .kn2("34", q(34), "35", "38")
  k[["35"]] <- .kn2("35", q(35), "rh", "36")
  k[["36"]] <- .kn2("36", q(36), "RU", "37")
  k[["37"]] <- .kn2("37", q(37), "RY", "RJ")
  k[["38"]] <- .kn2("38", q(38), "r", "_deposit")
  k[["39"]] <- .kn2("39", q(39), "H", "40")
  k[["40"]] <- list(branches = list(.kb("b", "PT", .kq("40", q(40))), .kb("a", "41")))
  k[["41"]] <- list(branches = list(.kb("a", "O", .kq("41a", qq("41a"))), .kb("b", "42", .kq("41b", qq("41b"))),
                                    .kb("c", "43")))
  k[["42"]] <- .kn2("42", q(42), "T", "TJ")
  k[["43"]] <- list(branches = list(.kb("a", "44", .kq("43a", qq("43a"))), .kb("b", "45", .kq("43b", qq("43b"))),
                                    .kb("c", "TJ")))
  k[["44"]] <- .kn2("44", q(44), "TO", "TT")
  k[["45"]] <- .kn2("45", q(44), "TE", "TT")
  k[["46"]] <- list(branches = list(.kb("a", "PTR", .kq("46a", qq("46a"))), .kb("c", "AH", .kq("46c", qq("46c"))),
                                    .kb("b", "47")))
  k[["47"]] <- list(branches = list(.kb("b", "AO", .kq("47", q(47))), .kb("a", "ao")))
  k[["48"]] <- .kn2("48", q(48), "49", "53")
  k[["49"]] <- .kn2("49", q(49), "50", "51")
  k[["50"]] <- list(branches = list(.kb("b", "E", .kq("50", q(50))), .kb("a", "e")))
  k[["51"]] <- .kn2("51", q(51), "SEL", "52")
  k[["52"]] <- list(branches = list(.kb("b", "EL", .kq("52", q(52))), .kb("a", "el")))
  k[["53"]] <- .kn2("53", q(53), "AEL", "BEL")
  k[["54"]] <- .kn2("54", q(54), "F", "55")
  k[["55"]] <- .kn2("55", q(55), "BAN", "56")
  k[["56"]] <- list(branches = list(.kb("b", "57", .kq("56b", qq("56b"))), .kb("a", "BFM", .kq("56a", qq("56a"))),
                                    .kb("b", "57")))
  k[["57"]] <- list(branches = list(.kb("a", "BH", .kq("57a", function(x, i) .kidpr_q57(x, i, "a"))),
                                    .kb("b", "BF", .kq("57b", function(x, i) .kidpr_q57(x, i, "b"))),
                                    .kb("c", "BHF")))
  k[["58"]] <- .kn2("58", q(58), "59", "60")
  k[["59"]] <- .kn2("59", q(59), "ASN", "BSN")
  k[["60"]] <- list(branches = list(.kb("a", "CAT", .kq("60a", qq("60a"))), .kb("b", "61", .kq("60b", qq("60b"))),
                                    .kb("c", "_gap60")))
  k[["61"]] <- list(branches = list(.kb("b", "BT", .kq("61", q(61))), .kb("a", "BI")))
  k[["62"]] <- .kn2("62", q(62), "63", "64")
  k[["63"]] <- list(branches = list(.kb("a", "CAT", .kq("63a", qq("63a"))), .kb("b", "BCA")))
  k[["64"]] <- .kn2("64", q(64), "ML", "65")        # 64b: the feature ca, go on at 65 (the ca copy)
  k[["65"]] <- list(branches = list(.kb("a", "BMK", .kq("65a", qq("65a"))), .kb("b", "BM", .kq("65b", qq("65b"))),
                                    .kb("c", "66")))
  k[["66"]] <- list(branches = list(.kb("a", "_none")))
  k
}

# Copy the positions `set` of a tree under a prefix: their targets inside the
# set point to the copies, their outcomes carry `tag`.
.kidpr_tree_clone <- function(tree, set, prefix, tag) {
  set <- intersect(set, names(tree))
  add <- list()
  for (id in set) {
    node <- tree[[id]]
    node$branches <- lapply(node$branches, function(b) {
      t <- b$to
      if (t %in% set) b$to <- paste0(prefix, t)
      else if (!t %in% names(tree)) b$to <- paste0(t, tag)
      b
    })
    add[[paste0(prefix, id)]] <- node
  }
  c(tree, add)
}

#' The key 2.1 as the walker runs it: the 66 positions, and the copies that
#' carry an overlay (S after 3b, X after 4a, V after 23a) or a feature that
#' continues (u after 13a, at 14 or 31; ca after 64b) to the outcome.
#' @noRd
.kidpr_key_tree <- function() {
  if (!is.null(.kidpr_cache$key_tree)) return(.kidpr_cache$key_tree)
  t <- .kidpr_key_base()
  t <- .kidpr_tree_clone(t, c("65", "66"), "ca.", "/ca")
  t[["64"]]$branches[[2L]]$to <- "ca.65"
  t <- .kidpr_tree_clone(t, c(as.character(24:66), "ca.65", "ca.66"), "V.", "+V")
  t[["23"]]$branches[[1L]]$to <- "V.24"
  t <- .kidpr_tree_clone(t, c(as.character(14:19), as.character(c(31:33, 58:66)), "ca.65", "ca.66"), "u.", "/u")
  t[["13"]]$branches[[1L]]$to <- "u.14"
  t[["13"]]$branches[[2L]]$to <- "u.31"
  rest <- setdiff(names(t), as.character(1:4))
  t <- .kidpr_tree_clone(t, rest, "S.", "+S")
  t <- .kidpr_tree_clone(t, rest, "X.", "+X")
  t[["3"]]$branches[[2L]]$to <- "S.5"
  t[["4"]]$branches[[1L]]$to <- "X.5"
  .kidpr_cache$key_tree <- t
  t
}


# --- one layer ------------------------------------------------------------------------

# An outcome of the walk read back: the horizon (with its modification), the
# overlays, the features, and what kind of outcome it is.
.kidpr_parse_outcome <- function(o) {
  ov <- sub("^\\+", "", regmatches(o, gregexpr("\\+[A-Z]+", o))[[1L]])
  ft <- sub("^/", "", regmatches(o, gregexpr("/[a-z]+", o))[[1L]])
  base <- sub("[+/].*$", "", o)
  mods <- .kidpr_vocabulary()$modifications
  kind <- if (base == "_none") "none" else if (base == "_deposit") "deposit"
          else if (startsWith(base, "_gap")) "gap" else if (grepl("^[a-z]", base)) "feature" else "horizon"
  code <- NA_character_; mod <- NA_character_
  if (kind == "horizon") {
    if (base %in% names(mods)) { code <- mods[[base]]; mod <- base } else code <- base
  }
  if (kind == "feature") ft <- c(base, ft)
  # with no horizon of its own, the layer is its overlay (an S, X or V alone)
  if (is.na(code) && kind != "gap" && length(ov)) { code <- ov[1L]; ov <- ov[-1L]; kind <- "horizon" }
  list(code = code, modification = mod, overlay = ov, features = unique(ft), kind = kind,
       gap = if (kind == "gap") sub("^_gap", "", base) else NA_character_)
}

# The label of an outcome among the candidates: its horizon, else its
# feature, else "" (no horizon); a gap gives the horizons on both sides.
.kidpr_outcome_labels <- function(p) {
  if (!is.na(p$code)) return(p$code)
  if (p$kind == "gap") return(.KIDPR_GAPS[[p$gap]])
  if (p$kind == "feature") return(p$features[1L])
  ""
}

# A path of the walk in the book's notation: each position with the Cyrillic
# letter of its answer (12a > 14a > 15b, the letters a, be, ve, ge).
.kidpr_path_text <- function(steps) {
  if (!length(steps)) return("")
  pos <- sub("^([A-Za-z]+\\.)+", "", sub(":.*$", "", steps))
  lab <- sub("\\.[0-9]+$", "", sub("^.*:", "", steps))
  paste0(pos, unname(.KIDPR_LETTERS[lab]), collapse = " > ")
}

# The logged answers of the positions of some paths: notes and tags.
.kidpr_path_log <- function(x, i, paths) {
  pos <- unique(sub("^([A-Za-z]+\\.)+", "", sub(":.*$", "", unlist(paths))))
  lg <- x$log[[i]]
  keep <- names(lg)[sub("[a-d]$", "", names(lg)) %in% pos]
  # in the order of the key
  keep <- keep[order(suppressWarnings(as.numeric(sub("[a-d]$", "", keep))), keep)]
  list(note = unique(unlist(lapply(lg[keep], `[[`, "note"))),
       tag = unique(unlist(lapply(lg[keep], `[[`, "tag"))))
}

#' The key 2.1 on the data of one layer
#'
#' Walks the key for layer `i` of the profile `x` (.kidpr_profile()) and
#' reads the outcomes back: the horizon when every outcome reached gives the
#' same one, its overlays and features (those some outcomes give and others
#' not are undecided), the candidates, the path in the book's notation, the
#' notes of the questions on it, the WRB stand-ins and the fallbacks used.
#' Fresh litter is no horizon; hard rock (an R of any notation) is R.
#' @noRd
.kidpr_key_layer <- function(x, i) {
  out <- list(code = NA_character_, modification = NA_character_, passed = NA, overlay = character(0),
              overlay_na = character(0), features = character(0), features_na = character(0),
              small = FALSE, candidates = character(0), missing = character(0), path = "",
              outcome = NA_character_, notes = character(0), stand_ins = character(0), fallbacks = character(0))
  if (.kidpr_litter(x, i)) {
    out$passed <- FALSE
    out$notes <- "fresh litter, moss or felt: not a diagnostic horizon (p. 36)"
    return(out)
  }
  if (.kidpr_hard_rock(x, i)) {
    out$code <- "R"; out$passed <- TRUE
    out$notes <- "hard rock by its designation"
    return(out)
  }
  w <- .walk_key(.kidpr_key_tree(), i, x)
  oc <- names(w$paths)
  parsed <- lapply(oc, .kidpr_parse_outcome)
  labels <- unique(unlist(lapply(parsed, .kidpr_outcome_labels)))
  if (w$dead_end) labels <- unique(c(labels, ""))
  ovs <- lapply(parsed, `[[`, "overlay"); fts <- lapply(parsed, `[[`, "features")
  inter <- function(l) if (!length(l)) character(0) else Reduce(intersect, l)
  uni <- function(l) unique(unlist(l))
  out$overlay <- inter(ovs); out$overlay_na <- setdiff(uni(ovs), out$overlay)
  out$features <- inter(fts); out$features_na <- setdiff(uni(fts), out$features)
  mods <- unique(vapply(parsed, `[[`, "", "modification"))
  decided <- length(labels) == 1L && !w$dead_end && !any(vapply(parsed, function(p) p$kind == "gap", NA))
  if (decided) {
    p1 <- parsed[[1L]]
    out$code <- p1$code
    out$modification <- if (length(mods) == 1L) mods else NA_character_
    out$passed <- !is.na(p1$code)
    sm <- .kidpr_vocabulary()$small
    out$small <- is.na(p1$code) && p1$kind == "feature" && any(p1$features %in% sm)
    out$outcome <- oc[1L]
  } else {
    out$candidates <- labels
  }
  out$path <- paste(vapply(oc, function(o) sprintf("%s: %s", .kidpr_path_text(w$paths[[o]]), o), ""), collapse = "; ")
  lg <- .kidpr_path_log(x, i, w$paths)
  out$notes <- c(lg$note, w$notes)
  out$stand_ins <- intersect(lg$tag, .KIDPR_STAND_INS)
  out$fallbacks <- setdiff(lg$tag, .KIDPR_STAND_INS)
  if (!decided || length(out$overlay_na) || length(out$features_na)) out$missing <- w$missing
  # the vertic signs short of V make the feature v
  vt <- x$cache[[sprintf("vertic3_%d", i)]]
  if (!is.null(vt) && isTRUE(vt$partial) && !"V" %in% c(out$code, out$overlay)) out$features <- unique(c(out$features, "v"))
  out
}

# The reading of a layer written in the Russian notation.
.kidpr_notation_layer <- function(x, i) {
  d <- x$des
  idx <- d$index[i]
  small <- isTRUE(d$small[i])
  trans <- isTRUE(d$transitional[i])
  list(code = idx, modification = d$modification[i], passed = if (small || trans || is.na(idx)) FALSE else TRUE,
       overlay = d$overlay[[i]], overlay_na = character(0), features = d$features[[i]],
       features_na = character(0), small = small, candidates = character(0), missing = character(0),
       notation = TRUE)
}

#' The identification of one layer, memoised: the notation where the profile
#' is written in it, else the key on the data. A question about the layers
#' above calls it; a layer asked for while it is being read answers
#' undecided.
#' @noRd
.kidpr_identify_layer <- function(x, i) {
  if (!is.null(x$ident[[i]])) return(x$ident[[i]])
  r <- if (x$kidpr && isTRUE(x$des$known[i])) .kidpr_notation_layer(x, i) else .kidpr_data_layer(x, i)
  if (!isTRUE(r$busy)) x$ident[[i]] <- r
  r
}

# The key on the data of one layer, run once (a layer written in the
# notation is read by the key too, for the comparison).
.kidpr_data_layer <- function(x, i) {
  key <- sprintf("keylayer_%d", i)
  r <- x$cache[[key]]
  if (!is.null(r)) return(r)
  if (isTRUE(x$busy[i])) return(list(code = NA_character_, passed = NA, candidates = character(0), busy = TRUE))
  x$busy[i] <- TRUE
  on.exit(x$busy[i] <- FALSE)
  r <- .kidpr_key_layer(x, i)
  assign(key, r, envir = x$cache)
  r
}

# The evidence of a layer as one text.
.kidpr_evidence <- function(parts) {
  parts <- unique(parts[!is.na(parts) & nzchar(parts)])
  if (!length(parts)) NA_character_ else paste(parts, collapse = "; ")
}


# --- the profile ------------------------------------------------------------------------

#' The diagnostic horizons of a pedon by the Russian Field Guide of 2008
#'
#' The horizon identification of the registry namespace kidpr
#' (identify_horizons()): every layer read as one of the diagnostic horizons
#' of key 2.1. A layer written in the Russian notation keeps the horizon the
#' notation gives (route "notation"); the key runs on its data as well, and
#' when it reaches the same horizon the route is "both", otherwise the key's
#' reading goes to the evidence. A layer that is not (designations in another
#' notation, or none) is read by the key on the data (route "data"), with the
#' hints of its designation in the evidence.
#'
#' @param pedon A PedonRecord.
#' @param ... Not used.
#' @param .profile The profile of the readers (.kidpr_profile()) when the
#'   caller already has it (the feature tests share it, v0.9.344).
#' @return The frame of identify_horizons(), one row per layer, with the
#'   columns `modification` (BH, BF of BHF; AV of V), `overlay_na` (overlays
#'   left undecided), `data_code` (the key's horizon on the data), `key_path`
#'   (the paths of the key in the book's notation, with their outcomes),
#'   `stand_ins` (soilKey's WRB diagnostics that stood in for a field
#'   descriptor) and `fallbacks` (the readings that used a fallback: dry from
#'   moist, the USDA texture, the EC for salts, the LOI from carbon ...).
#' @noRd
.kidpr_identify <- function(pedon, ..., .profile = NULL) {
  if (!inherits(pedon, "PedonRecord")) rlang::abort("'pedon' must be a PedonRecord.")
  fr <- .horizon_frame(pedon)
  n <- nrow(fr)
  fr$modification <- rep(NA_character_, n)
  fr$overlay_na <- rep(list(character(0)), n)
  fr$data_code <- rep(NA_character_, n)
  fr$key_path <- rep(NA_character_, n)
  fr$stand_ins <- rep(list(character(0)), n)
  fr$fallbacks <- rep(list(character(0)), n)
  if (!n) return(fr)
  x <- .profile %||% .kidpr_profile(pedon)
  d <- x$des
  for (i in x$ord) {
    k <- .kidpr_data_layer(x, i)
    fr$data_code[i] <- k$code
    fr$key_path[i] <- if (nzchar(k$path %||% "")) k$path else NA_character_
    fr$stand_ins[[i]] <- k$stand_ins
    fr$fallbacks[[i]] <- k$fallbacks
    hint <- d$hint[[i]] %||% character(0)
    hint_txt <- if (length(hint)) sprintf("the designation '%s' suggests %s (a hint, not read as a Russian horizon)",
                                          fr$designation[i], paste(hint, collapse = ", ")) else NA_character_
    if (x$kidpr && isTRUE(d$known[i])) {
      r <- .kidpr_identify_layer(x, i)
      fr$code[i] <- r$code
      fr$modification[i] <- r$modification
      fr$passed[i] <- r$passed
      fr$overlay[[i]] <- r$overlay
      fr$features[[i]] <- r$features
      fr$small[i] <- r$small
      fr$transitional[i] <- isTRUE(d$transitional[i])
      fr$buried[i] <- if (isTRUE(d$buried[i])) 1L else 0L
      fr$rockflag[i] <- d$rockflag[i]
      fr$mark[i] <- d$mark[i]
      fr$composition[[i]] <- d$composition[[i]]
      written <- if (!is.na(r$code)) r$code else if (r$small && length(r$features)) r$features[1L] else NA_character_
      data_lab <- if (!is.na(k$code)) k$code else if (isTRUE(k$small) && length(k$features)) k$features[1L] else NA_character_
      comparable <- !is.na(written) && !(written %in% .KIDPR_ROCKS) && !isTRUE(d$transitional[i])
      agree <- comparable && !is.na(data_lab) && identical(data_lab, written)
      fr$route[i] <- if (agree) "both" else "notation"
      dis <- if (!comparable) NA_character_
             else if (agree) sprintf("the key on the data reaches %s too", written)
             else if (!is.na(data_lab)) sprintf("the key on the data reads %s (%s); the notation's %s is kept",
                                               data_lab, k$path, written)
             else if (length(k$candidates)) sprintf("the key on the data leaves it undecided (%s); the notation's %s is kept",
                                                  paste(ifelse(nzchar(k$candidates), k$candidates, "no horizon"), collapse = ", "), written)
             else sprintf("the key on the data finds no diagnostic horizon; the notation's %s is kept", written)
      fr$evidence[i] <- .kidpr_evidence(c(sprintf("the Russian notation: %s", fr$designation[i]), dis,
                                          d$notes[[i]]))
    } else {
      fr$code[i] <- k$code
      fr$modification[i] <- k$modification
      fr$passed[i] <- k$passed
      fr$overlay[[i]] <- k$overlay
      fr$overlay_na[[i]] <- k$overlay_na
      fr$features[[i]] <- k$features
      fr$features_na[[i]] <- k$features_na
      fr$small[i] <- isTRUE(k$small)
      fr$buried[i] <- if (.kidpr_buried_des(x, i)) 1L else 0L
      fr$candidates[[i]] <- k$candidates
      fr$missing[[i]] <- k$missing
      fr$route[i] <- "data"
      fr$evidence[i] <- .kidpr_evidence(c(if (nzchar(k$path %||% "")) sprintf("key 2.1: %s", k$path),
                                          k$notes, if (length(k$stand_ins)) sprintf("soilKey's WRB stand-in: %s",
                                                                                     paste0(k$stand_ins, "()", collapse = ", ")),
                                          hint_txt))
    }
  }
  fr
}
