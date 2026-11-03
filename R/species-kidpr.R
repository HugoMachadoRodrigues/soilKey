# =============================================================================
# v0.9.347 -- the species (vidy) of the Russian soil classification (Field
# Guide of 2008, chapter 5.1, pp. 135-137).
#
# The species tell the quantitative degree or the position of the features
# of the type and subtype (p. 8): the thickness of the humus, peat and agro
# horizons, the depth and place of gleying, the lower boundary of the
# bleached horizon, the thickness of the layer over the solonetzic horizon,
# the depth of the carbonates and of the soluble salts, and, for the soils
# of the organogenic trunk, the decomposition and the botanical composition
# of the peat and the thickness of the peat body. Twelve tables (the humus
# horizons are two), in inst/rules/kidpr2008/species.yaml. A soil whose type
# is assigned is named, after its subtypes, by the class of every table that
# applies to it, in the order of the tables, comma-separated, in the plural
# as printed; the bleached horizon takes the prefix form of p. 136
# (glubokoelyuvial'nye, neglubokopodzolistye, melkoosolodelye). The species
# are rows of level "species" of `levels`, as the subtypes are (Hugo's
# decision Q1, 2026-11-02).
#
# Three-valued: a thickness or a depth that is not recorded, a layer whose
# horizon the data leave undecided at the edge of the measured run, or a run
# that reaches the bottom of the description when a deeper class is
# possible, leaves the table undecided; the measure is carried as an
# interval and a class is named only when the whole interval lies in it. An
# undecided species is not named and does not make the name provisional:
# it is listed in qualifiers$species_undecided, in the trace (phase
# "species") and in a note (soilKey's reading, for Hugo to confirm,
# 2026-11-03).
# =============================================================================

#' The species tables (inst/rules/kidpr2008/species.yaml), cached.
#' @noRd
.kidpr_sp_rules <- function() {
  if (is.null(.kidpr_cache$sp_rules)) {
    f <- system.file("rules", "kidpr2008", "species.yaml", package = "soilKey")
    if (!nzchar(f)) rlang::abort("The species tables of the Russian classification are not installed.")
    .kidpr_cache$sp_rules <- yaml::read_yaml(f)
  }
  .kidpr_cache$sp_rules
}


# --- the classes ---------------------------------------------------------------------

# Is a value in a class: from (at least), over (more than), under (less
# than), to (at most).
.kidpr_sp_in <- function(v, cl) {
  if (is.na(v)) return(NA)
  ok <- TRUE
  if (!is.null(cl$from)) ok <- ok && v >= cl$from
  if (!is.null(cl$over)) ok <- ok && v > cl$over
  if (!is.null(cl$under)) ok <- ok && v < cl$under
  if (!is.null(cl$to)) ok <- ok && v <= cl$to
  ok
}

# The class of a value: its id, or "" when no class of the table holds it.
.kidpr_sp_class_of <- function(v, classes) {
  for (cl in classes) if (isTRUE(.kidpr_sp_in(v, cl))) return(as.character(cl$id))
  ""
}

# The class of an interval [lo, hi]: decided when both ends (and every value
# between them, the classes being consecutive ranges) fall in the same class
# or both outside the table. The ends are read on both sides of every limit
# the interval crosses.
.kidpr_sp_class_interval <- function(lo, hi, classes) {
  if (is.na(lo) || is.na(hi)) return(list(id = NA_character_, decided = FALSE))
  cuts <- unlist(lapply(classes, function(cl) c(cl$from, cl$over, cl$under, cl$to)))
  probes <- c(lo, if (is.finite(hi)) hi else max(c(lo, cuts)) + 1e6)
  inside <- cuts[cuts > lo & cuts < hi]
  probes <- c(probes, inside, inside - 1e-9, inside + 1e-9)
  ids <- unique(vapply(probes, function(v) .kidpr_sp_class_of(v, classes), ""))
  if (length(ids) == 1L) list(id = ids, decided = TRUE) else list(id = NA_character_, decided = FALSE, candidates = ids)
}


# --- the profile -------------------------------------------------------------------------

# The depth of a layer's top or bottom from the ground (the top layer that is
# not litter).
.kidpr_sp_depth <- function(ctx, v) {
  g <- .kidpr_ground(ctx$x)
  if (is.na(g)) g <- 0
  v - g
}

# The first run of `H` layers among the main layers (small horizons inside
# the run belong to it): the layers that surely are H (`sure`), the run
# widened by the undecided layers at its edges (`maybe`), and whether an
# undecided layer above it could open an earlier run. `state` absent (no
# layer is or may be H), undecided (no layer surely is H, some may be) or
# found.
.kidpr_sp_run <- function(ctx, H) {
  m <- ctx$main
  if (!length(m)) return(list(state = "absent"))
  hv <- vapply(m, function(j) .kidpr_ft_is_host(ctx, j, H), NA)
  k <- which(hv %in% TRUE)[1L]
  if (is.na(k)) return(list(state = if (any(is.na(hv))) "undecided" else "absent",
                            maybe = m[is.na(hv)]))
  a <- k; while (a > 1L && isTRUE(hv[a - 1L])) a <- a - 1L
  b <- k; while (b < length(m) && isTRUE(hv[b + 1L])) b <- b + 1L
  a2 <- a; while (a2 > 1L && is.na(hv[a2 - 1L])) a2 <- a2 - 1L
  b2 <- b; while (b2 < length(m) && is.na(hv[b2 + 1L])) b2 <- b2 + 1L
  list(state = "found", sure = m[seq.int(a, b)], maybe = m[seq.int(a2, b2)],
       earlier = a2 > 1L && any(is.na(hv[seq_len(a2 - 1L)])),
       bottom = b2 == length(m), above = if (a2 > 1L) m[a2 - 1L] else NA_integer_,
       hv = hv, m = m)
}

# The labels of layers, for the evidence.
.kidpr_sp_labs <- function(ctx, rows) paste(vapply(rows, function(j) .kidpr_ft_lab(ctx, j), ""), collapse = ", ")

# A measure: the interval, the layers, the evidence, the missing attributes.
.kidpr_sp_measure <- function(lo = NA_real_, hi = lo, layers = integer(0), evidence = character(0),
                              missing = character(0), absent = FALSE) {
  list(lo = lo, hi = hi, layers = as.integer(layers), evidence = evidence, missing = missing, absent = absent)
}

# The thickness of the first run of the table's hosts, with an agro horizon
# directly over it where the table counts one (PU over AU, P over AY).
.kidpr_sp_thickness <- function(ctx, tab, depth = c("thickness", "bottom", "body")) {
  depth <- match.arg(depth)
  H <- .kidpr_ft_hosts(tab$hosts)
  r <- .kidpr_sp_run(ctx, H)
  hs <- paste(H, collapse = ", ")
  if (r$state == "absent")
    return(.kidpr_sp_measure(absent = TRUE, evidence = sprintf("no %s horizon in the profile", hs)))
  if (r$state == "undecided")
    return(.kidpr_sp_measure(evidence = sprintf("the horizon of %s undecided (%s or another)", .kidpr_sp_labs(ctx, r$maybe), hs),
                             missing = unique(unlist(ctx$fr$missing[r$maybe]))))
  x <- ctx$x
  first <- r$sure[1L]; last <- r$sure[length(r$sure)]
  first2 <- r$maybe[1L]; last2 <- r$maybe[length(r$maybe)]
  notes <- character(0)
  # the peat body starts at the ground (under an agro-peat-mineral PTR)
  if (depth == "body" && !.kidpr_sp_peat_start(ctx, first2)) {
    return(.kidpr_sp_measure(absent = TRUE, evidence = "the peat does not start the profile"))
  }
  # an agro horizon directly over the run counted with it
  agro <- .kidpr_ft_hosts(tab$with_agro_over %||% character(0))
  top_hi <- x$top[first]; top_lo <- x$top[first2]
  if (length(tab$with_agro_over) && !is.na(r$above)) {
    j <- r$above; i <- match(j, r$m)
    while (!is.na(i) && i >= 1L) {
      ag <- .kidpr_ft_is_host(ctx, r$m[i], agro)
      if (isFALSE(ag)) break
      if (isTRUE(ag)) top_hi <- x$top[r$m[i]]
      top_lo <- x$top[r$m[i]]
      notes <- c(notes, sprintf("the agro horizon %s over it counted with it", .kidpr_ft_lab(ctx, r$m[i])))
      i <- i - 1L
    }
  }
  bot_lo <- x$bot[last]; bot_hi <- x$bot[last2]
  used <- c(top_hi, top_lo, bot_lo, bot_hi)
  if (anyNA(used))
    return(.kidpr_sp_measure(evidence = sprintf("the depths of %s not recorded", .kidpr_sp_labs(ctx, r$maybe)),
                             missing = c("top_cm", "bottom_cm")))
  if (depth == "bottom") {
    lo <- .kidpr_sp_depth(ctx, bot_lo); hi <- .kidpr_sp_depth(ctx, bot_hi)
  } else {
    lo <- bot_lo - top_hi; hi <- bot_hi - top_lo
  }
  if (isTRUE(r$bottom)) {
    hi <- Inf
    notes <- c(notes, "the run reaches the bottom of the description: it may go deeper")
  }
  if (isTRUE(r$earlier)) {
    lo <- 0
    hi <- Inf
    notes <- c(notes, "an undecided layer above could open an earlier run")
  }
  miss <- unique(unlist(ctx$fr$missing[setdiff(r$maybe, r$sure)]))
  what <- if (depth == "bottom") "lower boundary at" else "thickness"
  val <- if (lo == hi) sprintf("%s cm", format(lo)) else sprintf("%s to %s cm", format(lo), if (is.finite(hi)) format(hi) else "more")
  .kidpr_sp_measure(lo, hi, layers = r$sure,
                    evidence = c(sprintf("%s: %s %s", .kidpr_sp_labs(ctx, r$sure), what, val), notes),
                    missing = miss)
}

# The depth of the top of the first layer of the table's hosts (the
# solonetzic horizon ASN or BSN): the thickness of the layer over it.
.kidpr_sp_top_depth <- function(ctx, tab) {
  H <- .kidpr_ft_hosts(tab$hosts)
  m <- ctx$main
  hv <- vapply(m, function(j) .kidpr_ft_is_host(ctx, j, H), NA)
  k <- which(hv %in% TRUE)[1L]
  if (is.na(k)) {
    if (any(is.na(hv))) return(.kidpr_sp_measure(evidence = sprintf("the horizon of %s undecided", .kidpr_sp_labs(ctx, m[is.na(hv)]))))
    return(.kidpr_sp_measure(absent = TRUE, evidence = sprintf("no %s horizon in the profile", paste(H, collapse = ", "))))
  }
  na_before <- which(is.na(hv[seq_len(k - 1L)]))
  t_hi <- ctx$x$top[m[k]]
  t_lo <- if (length(na_before)) ctx$x$top[m[na_before[1L]]] else t_hi
  if (is.na(t_hi) || is.na(t_lo))
    return(.kidpr_sp_measure(evidence = "the depth of the solonetzic horizon not recorded", missing = "top_cm"))
  lo <- .kidpr_sp_depth(ctx, t_lo); hi <- .kidpr_sp_depth(ctx, t_hi)
  .kidpr_sp_measure(lo, hi, layers = m[k],
                    evidence = sprintf("%s starts at %s cm: the layer over it is %s cm thick", .kidpr_ft_lab(ctx, m[k]),
                                       format(hi), format(hi)))
}


# --- the properties of a layer --------------------------------------------------------

# Carbonates in a layer: a horizon of carbonates (BCA, CAT, ML) by its code;
# else the effervescence or CaCO3 recorded; else the Russian notation (ca, a
# carbonate form or Rca written on the layer); else undecided.
.kidpr_sp_carbonates <- function(ctx, i) {
  cd <- ctx$code[i]
  if (isTRUE(cd %in% c("BCA", "CAT", "ML"))) return(list(passed = TRUE, note = sprintf("%s: a carbonate horizon", .kidpr_ft_lab(ctx, i))))
  e <- .kidpr_eff_any(ctx$x, i)
  if (!is.na(e$passed)) return(list(passed = e$passed, note = sprintf("%s: %s", .kidpr_ft_lab(ctx, i), paste(e$note, collapse = "; "))))
  d <- ctx$x$des
  if (isTRUE(ctx$x$kidpr) && isTRUE(d$known[i])) {
    fe <- d$features[[i]] %||% character(0)
    w <- any(c("ca", "lc", "mc", "nc", "dc", "ic") %in% fe)
    return(list(passed = w, note = sprintf("%s: the Russian notation %s carbonates", .kidpr_ft_lab(ctx, i), if (w) "writes" else "writes no")))
  }
  list(passed = NA, note = sprintf("%s: effervescence not recorded", .kidpr_ft_lab(ctx, i)),
       missing = c("effervescence_class", "caco3_pct"))
}

# Soluble salts in a layer: the salt horizon S by its code; else the salts
# or the EC recorded (the s of 3.2: over 0.1 %, or the EC of chapter 9);
# else the Russian notation (s, or an S laid over the layer); else
# undecided.
.kidpr_sp_salts <- function(ctx, i) {
  cd <- ctx$code[i]
  if (isTRUE(cd %in% c("S", "SS"))) return(list(passed = TRUE, note = sprintf("%s: the salt horizon", .kidpr_ft_lab(ctx, i))))
  s <- .kidpr_ft_salts(ctx, i)
  if (!is.na(s$saline)) return(list(passed = s$saline, note = sprintf("%s: %s", .kidpr_ft_lab(ctx, i), paste(s$note, collapse = "; "))))
  d <- ctx$x$des
  if (isTRUE(ctx$x$kidpr) && isTRUE(d$known[i])) {
    w <- "s" %in% (d$features[[i]] %||% character(0)) || "S" %in% (d$overlay[[i]] %||% character(0))
    return(list(passed = w, note = sprintf("%s: the Russian notation %s salts", .kidpr_ft_lab(ctx, i), if (w) "writes" else "writes no")))
  }
  list(passed = NA, note = sprintf("%s: salts not measured", .kidpr_ft_lab(ctx, i)),
       missing = c("soluble_salts_pct", "ec_dS_m", "ec_1to5_dS_m"))
}

# Gleying in a layer, its degree not considered (p. 135): the gley horizon
# G, a gleyed rock (CG), or the feature g as the feature tests read it.
.kidpr_sp_gley <- function(ctx, i) {
  cd <- ctx$code[i]
  if (isTRUE(cd == "G")) return(list(passed = TRUE, note = sprintf("%s: the gley horizon", .kidpr_ft_lab(ctx, i))))
  rf <- ctx$x$des$rockflag[i] %||% NA_character_
  if (isTRUE(ctx$x$kidpr) && isTRUE(ctx$x$des$known[i]) && identical(as.character(rf), "G"))
    return(list(passed = TRUE, note = sprintf("%s: a gleyed rock", .kidpr_ft_lab(ctx, i))))
  meta <- .kidpr_ft_meta()[["g"]]
  dres <- tryCatch(.kidpr_ft_g(ctx, i), error = function(e) .kidpr_fd(NA, note = conditionMessage(e)))
  v <- .kidpr_ft_combine(dres, .kidpr_ft_written(ctx, i, "g"), meta$kind %||% "observed")
  list(passed = v$passed, note = sprintf("%s: %s", .kidpr_ft_lab(ctx, i), paste(unique(v$notes), collapse = "; ")),
       missing = v$missing)
}

# The depth of the top of the first layer with a property (carbonates,
# salts): from the first layer that may have it to the first that has it;
# none in the profile, from the bottom of the description down.
.kidpr_sp_first_depth <- function(ctx, tab) {
  f <- switch(tab$property, carbonates = .kidpr_sp_carbonates, salts = .kidpr_sp_salts)
  o <- ctx$ord
  if (!length(o)) return(.kidpr_sp_measure(absent = TRUE, evidence = "no layers"))
  pv <- lapply(o, function(i) f(ctx, i))
  p <- vapply(pv, function(v) v$passed, NA)
  k <- which(p %in% TRUE)[1L]
  na <- which(is.na(p))
  x <- ctx$x
  if (!is.na(k)) {
    nb <- na[na < k]
    t_hi <- x$top[o[k]]
    t_lo <- if (length(nb)) x$top[o[nb[1L]]] else t_hi
    ev <- c(pv[[k]]$note, unlist(lapply(pv[nb], `[[`, "note")))
    miss <- unique(unlist(lapply(pv[nb], `[[`, "missing")))
    lay <- o[k]
  } else {
    bottom <- suppressWarnings(max(x$bot[o], na.rm = TRUE))
    t_hi <- Inf
    t_lo <- if (length(na)) x$top[o[na[1L]]] else bottom
    ev <- c(sprintf("no %s down to %s cm", tab$property, format(bottom)), unlist(lapply(pv[na], `[[`, "note")))
    miss <- unique(unlist(lapply(pv[na], `[[`, "missing")))
    lay <- integer(0)
  }
  if (is.na(t_lo) || is.na(t_hi))
    return(.kidpr_sp_measure(evidence = "the depth of the layers not recorded", missing = c("top_cm", "bottom_cm")))
  .kidpr_sp_measure(.kidpr_sp_depth(ctx, t_lo), if (is.finite(t_hi)) .kidpr_sp_depth(ctx, t_hi) else Inf,
                    layers = lay, evidence = ev, missing = miss,
                    absent = is.na(k) && !length(na))
}

# Is there a layer with the property (for a table that applies where the
# profile has it).
.kidpr_sp_any <- function(ctx, property) {
  f <- switch(property, carbonates = .kidpr_sp_carbonates, salts = .kidpr_sp_salts)
  any(vapply(ctx$ord, function(i) isTRUE(f(ctx, i)$passed), NA))
}


# --- the tables of their own ---------------------------------------------------------

# The place of gleying: deep (only from the layer transitional to the rock,
# its top at 100-130 cm, nothing gleyed above 100 cm), surface (a gleyed
# layer starting above 50 cm, the profile not gleyed throughout), profile
# (every mineral layer gleyed). Organic layers and hard rock are left out.
.kidpr_sp_gley_place <- function(ctx, tab) {
  m <- ctx$main
  m <- m[!vapply(m, function(j) isTRUE(.kidpr_ft_is_peat(ctx, j)) || isTRUE(ctx$code[j] %in% c("R", "TT")), NA)]
  if (!length(m)) return(list(passed = NA, absent = TRUE, evidence = "no mineral layers"))
  gv <- lapply(m, function(j) .kidpr_sp_gley(ctx, j))
  p <- vapply(gv, function(v) v$passed, NA)
  top <- .kidpr_sp_depth(ctx, ctx$x$top[m])
  cls <- stats::setNames(tab$classes, vapply(tab$classes, `[[`, "", "id"))
  sc <- cls$surface; dc <- cls$deep
  in_s <- vapply(top, function(t) .kidpr_sp_in(t, sc), NA)
  in_d <- vapply(top, function(t) .kidpr_sp_in(t, dc), NA)
  above_d <- vapply(top, function(t) if (is.na(t)) NA else t < (dc$from %||% 100), NA)
  prof <- .kidpr_and(p)
  if (length(m) < 2L) prof <- FALSE
  surf <- .kidpr_and(c(.kidpr_or(vapply(seq_along(m), function(k) .kidpr_and(c(in_s[k], p[k])), NA)), .kidpr_not(prof)))
  deep <- .kidpr_and(c(.kidpr_not(.kidpr_or(vapply(seq_along(m), function(k) .kidpr_and(c(above_d[k], p[k])), NA))),
                       .kidpr_or(vapply(seq_along(m), function(k) .kidpr_and(c(in_d[k], p[k])), NA)),
                       .kidpr_not(prof)))
  v <- c(deep = deep, surface = surf, profile = prof)
  gl <- m[p %in% TRUE]
  ev <- if (length(gl)) sprintf("gleyed: %s", .kidpr_sp_labs(ctx, gl)) else "no gleyed layer"
  und <- m[is.na(p)]
  if (length(und)) ev <- c(ev, sprintf("gleying undecided: %s", .kidpr_sp_labs(ctx, und)))
  miss <- unique(unlist(lapply(gv[is.na(p)], `[[`, "missing")))
  id <- if (sum(v %in% TRUE) == 1L && !anyNA(v[!(v %in% TRUE)])) names(v)[v %in% TRUE]
        else if (all(v %in% FALSE)) "" else NA_character_
  list(id = id, layers = gl, evidence = ev, missing = if (is.na(id)) miss else character(0))
}

# Does the peat body start the profile: its first layer is the first main
# layer, or the one under an agro-peat-mineral horizon PTR at the surface
# (the agro-mineral peatozems PTR-TT).
.kidpr_sp_peat_start <- function(ctx, j) {
  m <- ctx$main
  if (!length(m) || is.na(j)) return(FALSE)
  identical(j, m[1L]) || (length(m) > 1L && identical(j, m[2L]) && isTRUE(ctx$code[m[1L]] == "PTR"))
}

# The peat body: the run of peat layers from the ground.
.kidpr_sp_peat_rows <- function(ctx, tab) {
  r <- .kidpr_sp_run(ctx, .kidpr_ft_hosts(tab$hosts %||% c("T", "TO", "TE", "TJ", "TT", "PT")))
  if (!identical(r$state, "found") || !.kidpr_sp_peat_start(ctx, r$sure[1L])) return(integer(0))
  r$sure
}

# The thickness-weighted mean of an attribute over the peat body (the
# degree of decomposition): an interval, a layer not recorded counting from
# 0 to 100 %.
.kidpr_sp_weighted <- function(ctx, tab) {
  rows <- .kidpr_sp_peat_rows(ctx, list(hosts = c("T", "TO", "TE", "TJ", "TT", "PT")))
  if (!length(rows)) return(.kidpr_sp_measure(absent = TRUE, evidence = "no peat body at the surface"))
  x <- ctx$x
  th <- x$thick[rows]
  if (anyNA(th) || sum(th) <= 0)
    return(.kidpr_sp_measure(evidence = "the depths of the peat layers not recorded", missing = c("top_cm", "bottom_cm")))
  v <- vapply(rows, function(j) .kidpr_num(x, j, tab$attribute), 0)
  lo <- sum(th * ifelse(is.na(v), 0, v)) / sum(th)
  hi <- sum(th * ifelse(is.na(v), 100, v)) / sum(th)
  ev <- if (all(is.na(v))) sprintf("%s not recorded in the peat (%s)", tab$attribute, .kidpr_sp_labs(ctx, rows))
        else sprintf("%s of the peat (%s): %s", tab$attribute, .kidpr_sp_labs(ctx, rows),
                     paste(ifelse(is.na(v), "not recorded", paste0(format(v), " %")), collapse = ", "))
  if (!anyNA(v)) ev <- c(ev, sprintf("thickness-weighted mean %s %%", format(round(lo, 1))))
  .kidpr_sp_measure(lo, hi, layers = rows, evidence = ev, missing = if (anyNA(v)) tab$attribute else character(0))
}

# The plants of one layer's botanical record: a share per plant ("sedge 50;
# wood 30"), NA where the record names a plant without a share.
.kidpr_sp_plants_of <- function(txt, plants) {
  parts <- trimws(strsplit(txt, "[;,+/]")[[1L]])
  parts <- parts[nzchar(parts)]
  out <- stats::setNames(numeric(0), character(0))
  for (p in parts) {
    lp <- tolower(p)
    hit <- NA_character_
    for (pl in plants) if (grepl(pl$pattern, lp, perl = TRUE)) { hit <- as.character(pl$id); break }
    if (is.na(hit)) return(NULL)
    sh <- suppressWarnings(as.numeric(regmatches(lp, regexpr("[0-9]+(\\.[0-9]+)?", lp))))
    out[hit] <- if (length(sh)) sh else NA_real_
  }
  out
}

# The botanical species of the peat body: the plants whose remains are more
# than 40 % of it (the shares weighted by thickness), two giving a double
# name; a record with no shares names its plants as the dominant ones.
.kidpr_sp_botanical <- function(ctx, tab) {
  rows <- .kidpr_sp_peat_rows(ctx, list(hosts = c("T", "TO", "TE", "TJ", "TT", "PT")))
  if (!length(rows)) return(list(id = "", absent = TRUE, evidence = "no peat body at the surface"))
  x <- ctx$x
  plants <- tab$plants
  txt <- vapply(rows, function(j) .kidpr_txt(x, j, tab$attribute), "")
  if (anyNA(txt))
    return(list(id = NA_character_, evidence = sprintf("%s not recorded in %s", tab$attribute, .kidpr_sp_labs(ctx, rows[is.na(txt)])),
                missing = tab$attribute))
  rec <- lapply(txt, function(t) .kidpr_sp_plants_of(t, plants))
  if (any(vapply(rec, is.null, NA)))
    return(list(id = NA_character_, evidence = sprintf("a plant not read in %s (%s)", tab$attribute, paste(txt, collapse = " | ")),
                missing = tab$attribute))
  over <- as.numeric(tab$dominant_over %||% 40)
  ev <- sprintf("%s: %s", .kidpr_sp_labs(ctx, rows), paste(txt, collapse = " | "))
  th <- x$thick[rows]
  shares_known <- all(vapply(rec, function(r) !anyNA(r), NA)) && !anyNA(th) && sum(th) > 0
  if (shares_known) {
    ids <- unique(unlist(lapply(rec, names)))
    w <- vapply(ids, function(id) sum(th * vapply(rec, function(r) if (id %in% names(r)) r[[id]] else 0, 0)) / sum(th), 0)
    dom <- ids[w > over]
    dom <- dom[order(-w[dom], match(dom, vapply(plants, `[[`, "", "id")))]
    ev <- c(ev, sprintf("thickness-weighted shares: %s", paste(sprintf("%s %s %%", ids, format(round(w, 1))), collapse = ", ")))
  } else {
    # names without shares: the plants recorded are the dominant ones, the
    # same in every layer
    sets <- lapply(rec, function(r) if (anyNA(r)) names(r) else names(r)[r > over])
    if (!all(vapply(sets, function(s) identical(sort(s), sort(sets[[1L]])), NA)) || length(sets[[1L]]) > 2L)
      return(list(id = NA_character_, evidence = c(ev, "the dominant plants differ between the layers or are not given by share"),
                  missing = tab$attribute))
    dom <- sets[[1L]]
  }
  if (!length(dom)) return(list(id = "", evidence = c(ev, sprintf("no plant over %s %%", format(over)))))
  if (length(dom) > 2L) return(list(id = NA_character_, evidence = c(ev, "more than two dominant plants"), missing = tab$attribute))
  list(id = paste(dom, collapse = "+"), layers = rows, evidence = ev)
}


# --- applicability -------------------------------------------------------------------

# Does a table apply to the soil: its trunk, division, the horizons and
# features its type's formula writes, or (`or_profile`) a layer with the
# property.
.kidpr_sp_applies <- function(tab, trunk, division, toks, ctx) {
  a <- tab$applies %||% list()
  if (length(a$trunks) && !(trunk %in% unlist(a$trunks))) return(FALSE)
  if (length(a$divisions) && !(division %in% unlist(a$divisions))) return(FALSE)
  idx <- vapply(toks, function(t) t$index, "")
  fe <- unique(unlist(lapply(toks, `[[`, "features")))
  if (length(a$formula_not_first) && length(idx) && idx[1L] %in% unlist(a$formula_not_first)) return(FALSE)
  if (length(a$formula_first) && !(length(idx) && idx[1L] %in% unlist(a$formula_first))) return(FALSE)
  need <- c(a$formula_any, a$formula_feature)
  if (!length(need)) return(TRUE)
  if (any(idx %in% unlist(a$formula_any)) || any(fe %in% unlist(a$formula_feature))) return(TRUE)
  isTRUE(a$or_profile) && !is.null(tab$property) && .kidpr_sp_any(ctx, tab$property)
}


# --- one table -------------------------------------------------------------------------

# The name of a class: the printed adjective, or for the bleached horizon the
# prefix and the term of the horizon's genesis.
.kidpr_sp_name <- function(tab, cl, ctx, layers, type) {
  if (identical(tab$id, "bleached")) {
    tm <- tab$term
    sol <- tm$solodised
    t <- if (type %in% unlist(sol$types)) sol
         else {
           cd <- unique(stats::na.omit(ctx$code[layers]))
           if (length(cd) == 1L && cd %in% names(tm)) tm[[cd]] else tm$default
         }
    return(list(ru = paste0(cl$prefix, t$name), en = paste0(cl$prefix_en, t$name_en)))
  }
  list(ru = as.character(cl$name), en = as.character(cl$name_en))
}

# One table read on the soil: the class (its id, "" when none holds, NA when
# undecided), the names, the measure, the evidence.
.kidpr_sp_table <- function(tab, ctx, type) {
  classes <- tab$classes
  r <- switch(tab$measure,
    thickness = .kidpr_sp_thickness(ctx, tab, "thickness"),
    bottom_depth = .kidpr_sp_thickness(ctx, tab, "bottom"),
    peat_body = .kidpr_sp_thickness(ctx, tab, "body"),
    top_depth = .kidpr_sp_top_depth(ctx, tab),
    first_depth = .kidpr_sp_first_depth(ctx, tab),
    weighted_pct = .kidpr_sp_weighted(ctx, tab),
    gley_place = .kidpr_sp_gley_place(ctx, tab),
    botanical = .kidpr_sp_botanical(ctx, tab),
    rlang::abort(sprintf("No measure '%s' of a species table.", tab$measure)))
  if (!is.null(r$id)) {
    id <- r$id
  } else if (isTRUE(r$absent)) {
    id <- ""
  } else {
    ci <- .kidpr_sp_class_interval(r$lo, r$hi, classes)
    id <- if (isTRUE(ci$decided)) ci$id else NA_character_
    if (!isTRUE(ci$decided) && length(ci$candidates))
      r$evidence <- c(r$evidence, sprintf("the class undecided between %s",
                                          paste(ifelse(nzchar(ci$candidates), ci$candidates, "none"), collapse = " and ")))
  }
  out <- list(table = tab$id, id = id, passed = if (is.na(id)) NA else nzchar(id),
              lo = r$lo %||% NA_real_, hi = r$hi %||% NA_real_, layers = r$layers %||% integer(0),
              evidence = r$evidence, missing = if (is.na(id)) unique(r$missing %||% character(0)) else character(0),
              ru = NA_character_, en = NA_character_)
  if (!is.na(id) && nzchar(id)) {
    if (identical(tab$measure, "botanical")) {
      pl <- stats::setNames(tab$plants, vapply(tab$plants, `[[`, "", "id"))
      ps <- strsplit(id, "+", fixed = TRUE)[[1L]]
      out$ru <- if (length(ps) == 1L) pl[[ps]]$name else paste0(pl[[ps[1L]]]$stem, "-", pl[[ps[2L]]]$name)
      out$en <- paste0(paste(vapply(ps, function(p) pl[[p]]$name_en, ""), collapse = "-"), " peat")
    } else {
      cl <- Filter(function(c) identical(as.character(c$id), id), classes)[[1L]]
      nm <- .kidpr_sp_name(tab, cl, ctx, out$layers, type)
      out$ru <- nm$ru; out$en <- nm$en
    }
  }
  out
}


# --- the level -----------------------------------------------------------------------

# Add the species to a classify_kidpr2008() result: every table that applies
# to the soil read on it; the classes that hold as rows of level "species"
# (code "<table>:<class>", name the English gloss, name_native the Russian),
# after the subtypes in the full name (English in `name`, Russian in
# qualifiers$name_native), in the order of the tables; the undecided tables
# in qualifiers$species_undecided and a note; every table read in the trace
# (phase "species"). A soil whose type is unassigned, and a chemozem (CU,
# no types of its own), gets none.
.kidpr_add_species <- function(res, pedon, rules, ctx = NULL) {
  lv <- res$levels
  if (is.null(lv) || nrow(lv) < 3L || isTRUE(.kidpr_dv_state$masking)) return(res)
  at <- which(lv$level == "types")
  if (!length(at) || is.na(lv$code[at[1L]])) return(res)
  type <- lv$code[at[1L]]
  div <- gsub("[][]", "", lv$code[lv$level == "divisions"][1L])
  trunk <- lv$code[lv$level == "trunks"][1L]
  if (identical(div, "CU") || startsWith(type, "[CU:")) return(res)
  te <- Filter(function(t) identical(as.character(t$code), type), .kidpr_ty_list(rules$types %||% list(), div))
  formula <- if (length(te)) as.character(te[[1L]]$formula) else NA_character_
  toks <- .kidpr_st_formula_tokens(formula)
  ctx <- ctx %||% .kidpr_ft_context(pedon, div)
  ctx <- .kidpr_st_own_context(ctx, formula)
  tabs <- .kidpr_sp_rules()$tables
  read <- list(); trace <- list()
  for (tab in tabs) {
    if (!.kidpr_sp_applies(tab, trunk, div, toks, ctx)) next
    r <- tryCatch(.kidpr_sp_table(tab, ctx, type),
                  error = function(e) list(table = tab$id, id = NA_character_, passed = NA, evidence = paste("error:", conditionMessage(e)),
                                           missing = character(0), lo = NA_real_, hi = NA_real_, layers = integer(0),
                                           ru = NA_character_, en = NA_character_))
    read[[tab$id]] <- r
    code <- if (!is.na(r$id) && nzchar(r$id)) sprintf("%s:%s", tab$id, r$id) else tab$id
    trace[[tab$id]] <- list(code = code, name = if (!is.na(r$en)) r$en else tab$title_en,
                            name_native = if (!is.na(r$ru)) r$ru else NA_character_,
                            passed = r$passed,
                            status = if (isTRUE(r$passed)) "named" else if (isFALSE(r$passed)) "no class of the table" else "undecided",
                            evidence = r$evidence, missing = r$missing, page = tab$page,
                            value = c(lo = r$lo, hi = r$hi), layers = r$layers)
  }
  named <- Filter(function(r) isTRUE(r$passed), read)
  und <- names(Filter(function(r) is.na(r$passed), read))
  if (length(named)) {
    add <- lv[rep(NA_integer_, length(named)), , drop = FALSE]
    add$level <- "species"
    add$code <- vapply(named, function(r) sprintf("%s:%s", r$table, r$id), "")
    add$name <- vapply(named, `[[`, "", "en")
    if ("name_native" %in% names(lv)) add$name_native <- vapply(named, `[[`, "", "ru")
    if ("name_en" %in% names(lv)) add$name_en <- NA_character_
    rownames(add) <- NULL
    res$levels <- rbind(lv, add)
    rownames(res$levels) <- NULL
    res$name <- paste(c(res$name, vapply(named, `[[`, "", "en")), collapse = ", ")
  }
  q <- res$qualifiers %||% list()
  q$species <- unname(vapply(named, `[[`, "", "en"))
  q$species_native <- unname(vapply(named, `[[`, "", "ru"))
  if (length(named)) {
    base <- q$name_native %||% (lv$name_native[at[1L]] %||% lv$name[at[1L]])
    q$name_native <- paste(c(base, q$species_native), collapse = ", ")
  }
  q$species_undecided <- und
  res$qualifiers <- q
  tr <- res$trace %||% list()
  tr$species <- trace
  res$trace <- tr
  if (length(und)) {
    miss <- unique(unlist(lapply(read[und], `[[`, "missing")))
    res$warnings <- c(res$warnings, sprintf(
      "Species undecided (not named; the name stands): %s%s.",
      paste(vapply(und, function(u) Filter(function(t) identical(t$id, u), tabs)[[1L]]$title_en, ""), collapse = "; "),
      if (length(miss)) sprintf(" (would be decided by %s)", paste(miss, collapse = ", ")) else ""))
  }
  res
}
