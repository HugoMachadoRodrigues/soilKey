# =============================================================================
# v0.9.358 -- the South African soil classification of 1991, part 4: the
# profile view and the gates of the soil forms.
#
# .za_view() reads a profile as the key reads it (plan 3.1): the general
# rules first (the 1500 mm of R1, Q7; a recent deposit over a buried soil,
# R3 and R4, Q8), then the identification of every layer (.za_identify()),
# then the sequence of diagnostic horizons and materials from the top: the
# topsoil one horizon, consecutive layers of one code one horizon, layers of
# no diagnostic horizon skipped, an orthic A supplied when the profile is
# truncated (K 34, 70, 72, 73), and an unconsolidated material supplied
# under a pedocutanic or podzol B that reaches 1500 mm (Q4).
#
# A form is its sequence (key.yaml): it holds when its horizons are the
# first of the profile's, in order, and the horizon that follows (if any) is
# not one that would make another form (`closed_on`, Q3). A layer left
# undecided is completed in every way its candidates allow: TRUE when every
# completion holds, FALSE when none does, NA otherwise. A description that
# stops above 1500 mm (and not on hard rock) leaves the longer forms
# undecided (Q4), so the shorter one is provisional.
# =============================================================================

.za_state <- new.env(parent = emptyenv())

# The forms of key.yaml, by code, read once.
.za_forms <- function() {
  if (is.null(.za_cache$forms)) {
    path <- system.file("rules", "za1991", "key.yaml", package = "soilKey")
    if (!nzchar(path)) rlang::abort("The key of the South African forms is not installed.")
    fs <- yaml::read_yaml(path)$forms
    out <- lapply(fs, function(f) list(code = f$code, name = f$name, name_en = f$name_en,
                                       slots = strsplit(f$sequence, "-", fixed = TRUE)[[1]],
                                       closed_on = as.character(unlist(f$closed_on)),
                                       alternatives = lapply(f$alternatives %||% list(), function(a) as.character(unlist(a))),
                                       outside = isTRUE(f$outside_1991_key)))
    names(out) <- vapply(fs, function(f) f$code, "")
    .za_cache$forms <- out
  }
  .za_cache$forms
}

# The tokens of the 30 horizons and materials.
.za_tokens <- function() as.character(.za_vocabulary()$info$token)


# --- the general rules --------------------------------------------------------------

# A pedon with only some of its layers (the same site).
.za_subset <- function(pedon, keep) {
  p <- pedon$clone(deep = TRUE)
  h <- data.table::as.data.table(p$horizons)
  p$horizons <- h[keep, ]
  p
}

#' A recent deposit at the surface (G, R3 and R4; Hugo's decision Q8): the
#' layers from the top that recent_deposit marks; where it is not recorded, a
#' layer_origin aeolian, fluvial, colluvial or transported, or
#' anthropic_material, flagged; or the layers above a buried horizon (a
#' designation with the suffix b), flagged. Returns the layers of the
#' deposit, how it was read, and its thickness.
#' @noRd
.za_deposit <- function(x) {
  if (!x$n) return(NULL)
  flag <- function(i) {
    rd <- .za_lgl(x, i, "recent_deposit")
    if (!is.na(rd)) return(c(rd, FALSE))
    lo <- .za_origin(x, i)
    if (!is.na(lo) && grepl(paste(.ZA_AEOLIAN, .ZA_FLUVIAL, "transport", sep = "|"), lo)) return(c(TRUE, TRUE))
    am <- .za_txt(x, i, "anthropic_material")
    if (!is.na(am) && !grepl("^(none|no|absent|natural)", am)) return(c(TRUE, TRUE))
    c(FALSE, FALSE)
  }
  dep <- integer(0); flagged <- FALSE
  # D8 (G R3; K 38, K 74 note i; Hugo, 2026-11-06): when the layer under the
  # surface run is a deposit, the run (its A) is part of the deposit
  run <- .za_surface_run(x)
  start <- x$ord
  with_run <- FALSE
  if (length(run) && !flag(x$ord[1L])[1L] && length(run) < x$n) {
    nx <- x$ord[length(run) + 1L]
    if (flag(nx)[1L]) { dep <- run; start <- x$ord[-seq_along(run)]; with_run <- TRUE }
  }
  for (i in start) {
    f <- flag(i)
    if (!f[1L]) break
    dep <- c(dep, i); flagged <- flagged || f[2L]
  }
  how <- if (flagged) "origin" else "recorded"
  if (!length(dep)) {
    b <- which(vapply(x$ord, function(i) isTRUE(grepl("^[0-9]*[A-Z]+[a-z]*b[0-9]*$", .za_des(x, i))), NA))
    if (length(b) && b[1L] > 1L) { dep <- x$ord[seq_len(b[1L] - 1L)]; how <- "buried_mark" }
  }
  if (!length(dep) || length(dep) == x$n) return(NULL)
  list(layers = dep, how = how, thickness = x$bot[dep[length(dep)]], below = x$ord[length(dep) + 1L], with_run = with_run)
}

#' The profile as the key reads it, its layers identified and its sequence
#' of diagnostic horizons and materials (plan 3.1).
#' @noRd
.za_view <- function(pedon) {
  notes <- character(0); tags <- character(0)
  # R1 (Q7): a horizon is diagnostic only if its top lies above 1500 mm
  lim <- .za_thv("diagnostic_depth") / 10
  top <- suppressWarnings(as.numeric(pedon$horizons$top_cm))
  deep <- which(!is.na(top) & top >= lim)
  x0 <- NULL; below_lim <- NA_integer_
  if (length(deep)) {
    x0 <- .za_x(pedon)
    below_lim <- deep[which.min(top[deep])]
    near <- top[deep] < lim + 10
    if (any(near)) notes <- c(notes, sprintf("a layer starting at %s cm, just below 1500 mm, is not diagnostic (R1; Q7)",
                                             paste(format(top[deep][near]), collapse = ", ")))
    pedon <- .za_subset(pedon, setdiff(seq_len(nrow(pedon$horizons)), deep))
  }
  # R3, R4 (Q8): a recent deposit over a buried soil
  x <- .za_x(pedon)
  dep <- .za_deposit(x)
  if (!is.null(dep)) {
    j <- dep$below
    cls <- !(.za_is_rock(x, j) || isTRUE(.za_t_SAP(x, j)$passed))
    th <- dep$thickness
    by <- switch(dep$how, origin = "its layer_origin or anthropic_material, flagged (Q8)",
                 buried_mark = "the buried horizon marked b below it, flagged (Q8)", recorded = "recent_deposit")
    if (dep$how != "recorded") tags <- c(tags, "deposit_by_evidence")
    if (isTRUE(dep$with_run)) {
      tags <- c(tags, "topsoil_in_deposit")
      notes <- c(notes, "the layer under the topsoil is a recent deposit: the topsoil is part of it (G R3; K 38, 74; D8)")
    }
    if (!cls) {
      notes <- c(notes, sprintf("a recent deposit (%s) over rock or saprolite: the deposit is classified (R4)", by))
    } else if (is.na(th)) {
      notes <- c(notes, "a recent deposit of unrecorded thickness: the profile is classified from the top")
    } else if (th < .za_thv("burial_deposit") / 10) {
      notes <- c(notes, sprintf("a recent deposit %s cm thick (%s) over a buried soil: the buried soil is classified (R3)", format(th), by))
      tags <- c(tags, "buried_soil_classified")
      pedon <- .za_subset(pedon, setdiff(seq_len(nrow(pedon$horizons)), dep$layers))
      x <- .za_x(pedon)
    } else {
      notes <- c(notes, sprintf("a recent deposit %s cm thick (%s): the deposit is classified (R3)", format(th), by))
      tags <- c(tags, "deposit_classified")
      pedon <- .za_subset(pedon, sort(dep$layers))
      x <- .za_x(pedon)
    }
  }
  fr <- .za_identify(pedon, .profile = x)
  run <- .za_topsoil_layers(x)
  # D7: a surface layer the identification reads as a B is no topsoil
  run <- run[!(fr$code[run] %in% setdiff(.za_tokens(), .ZA_TOPSOILS))]
  el <- list()
  add <- function(codes, layers, flag = character(0)) {
    el[[length(el) + 1L]] <<- list(codes = codes, layers = layers, top = min(x$top[layers]), bottom = max(x$bot[layers]),
                                    flag = flag)
  }
  if (length(run)) {
    tc <- .za_topsoil_codes(x, fr, run)
    if (length(tc$notes)) notes <- c(notes, tc$notes)
    add(tc$codes, run)
  }
  for (i in setdiff(x$ord, run)) {
    if (isFALSE(fr$passed[i])) next
    codes <- if (!is.na(fr$code[i])) fr$code[i] else fr$candidates[[i]]
    k <- length(el)
    # consecutive layers of one code, or undecided between the same codes
    # (D4), are one horizon
    if (k && length(el[[k]]$layers) && setequal(el[[k]]$codes, codes) && length(el[[k]]$codes) == length(codes) &&
        !(length(run) && k == 1L)) {
      el[[k]]$layers <- c(el[[k]]$layers, i)
      el[[k]]$bottom <- max(el[[k]]$bottom, x$bot[i])
    } else add(codes, i)
  }
  # R6 (Hugo, 2026-11-06): a B of layers that differ in colour (red and
  # yellow-brown apedal) is two horizons when each is 20 cm or more thick,
  # else one, of the colour of the greater thickness
  repeat {
    hit <- FALSE
    for (k in seq_len(max(0L, length(el) - 1L))) {
      a <- el[[k]]; b <- el[[k + 1L]]
      if (length(a$codes) != 1L || length(b$codes) != 1L || !all(c(a$codes, b$codes) %in% c("RAB", "YAB")) ||
          a$codes == b$codes) next
      ta <- a$bottom - a$top; tb <- b$bottom - b$top
      lim2 <- .za_thv("colour_split_thickness")
      if (isTRUE(ta >= lim2) && isTRUE(tb >= lim2)) next
      code <- if (isTRUE(tb > ta)) b$codes else a$codes
      el[[k]] <- list(codes = code, layers = c(a$layers, b$layers), top = a$top, bottom = b$bottom, flag = "colour_majority")
      el[[k + 1L]] <- NULL
      tags <- c(tags, "colour_majority")
      notes <- c(notes, sprintf("a red and a yellow-brown apedal layer, one under %s cm thick: one %s (R6), flagged",
                                format(lim2), .za_label(code)))
      hit <- TRUE
      break
    }
    if (!hit) break
  }
  truncated <- FALSE
  tr_codes <- c("RAB", "YAB", "NCA", "DOR", "HPL")
  if (!length(run) && length(el) && all(el[[1L]]$codes %in% tr_codes)) {
    el <- c(list(list(codes = "OA", layers = integer(0), top = 0, bottom = 0, flag = "truncated")), el)
    truncated <- TRUE
    tags <- c(tags, "truncated")
    notes <- c(notes, "no topsoil: the profile is read as truncated, an orthic A supplied (K 34, 70, 72, 73), flagged")
  }
  bot <- suppressWarnings(max(x$bot, na.rm = TRUE))
  if (!is.finite(bot)) bot <- NA_real_
  last <- if (length(el)) el[[length(el)]] else NULL
  ends_rock <- !is.null(last) && identical(last$codes, "HRK")
  open <- !ends_rock && (is.na(bot) || bot < lim)
  implicit <- FALSE
  if (!is.null(last) && length(last$codes) == 1L && last$codes %in% c("PCU", "PZB") && isTRUE(last$bottom >= lim)) {
    rec <- FALSE
    if (!is.null(x0) && !is.na(below_lim)) {
      # R7 (Hugo, 2026-11-06): a layer described below the 1500 mm is read
      rec <- TRUE
      codes <- if (.za_is_rock(x0, below_lim)) "HRK" else if (isTRUE(.za_t_SAP(x0, below_lim)$passed)) "SAP"
               else { w <- .za_wetness(x0, below_lim)$passed
                      if (isTRUE(w)) "UMW" else if (isFALSE(w)) "UMN" else c("UMW", "UMN") }
      tags <- c(tags, "material_below_recorded")
    } else {
      w <- .za_wetness(x, x$ord[x$n])$passed
      codes <- if (isTRUE(w)) "UMW" else if (isFALSE(w)) "UMN" else c("UMW", "UMN")
    }
    el[[length(el) + 1L]] <- list(codes = codes, layers = integer(0), top = last$bottom, bottom = last$bottom, flag = "implicit")
    implicit <- TRUE
    open <- FALSE
    tags <- c(tags, "material_below_1500mm")
    notes <- c(notes, sprintf("the %s reaches 1500 mm: the material below read as %s%s, flagged (Q4)", .za_label(last$codes),
                              paste(vapply(codes, .za_label, ""), collapse = " or "),
                              if (rec) " from the layer described below it (R7)" else " by its lowest layer"))
  }
  if (open) notes <- c(notes, sprintf("the description stops at %s cm, above 1500 mm: longer forms stay possible (Q4)",
                                      if (is.na(bot)) "an unrecorded depth" else format(bot)))
  # v0.9.359: the classes of the family criteria, read once per view (fc)
  list(pedon = pedon, x = x, frame = fr, elements = el, open = open, truncated = truncated,
       implicit = implicit, notes = notes, tags = unique(tags), bottom = bot, fc = new.env(parent = emptyenv()))
}

#' The topsoil horizon of a surface run of several layers (D9; R5, Hugo,
#' 2026-11-06): for every completion of its undecided layers, an organic O,
#' vertic, melanic or humic A (in that order) holds when its layers cover
#' half or more of the run's thickness or its upper 200 mm; otherwise the
#' orthic A. The codes reached.
#' @noRd
.za_topsoil_codes <- function(x, fr, run) {
  opts <- lapply(run, function(i) { cs <- if (!is.na(fr$code[i])) fr$code[i] else fr$candidates[[i]]
                                    cs <- cs[nzchar(cs)]; if (length(cs)) cs else .ZA_TOPSOILS })
  if (length(run) == 1L) return(list(codes = opts[[1L]], notes = character(0)))
  th <- x$thick[run]; if (anyNA(th)) th <- rep(1, length(run))
  t0 <- x$top[run]; up <- .za_thv("topsoil_upper") / 10
  first <- if (all(!is.na(t0))) min(t0) else NA_real_
  in_upper <- if (is.na(first)) seq_along(run) == 1L else t0 < first + up
  one <- function(cs) {
    for (c in setdiff(.ZA_TOPSOILS, "OA")) {
      on <- cs == c
      if (!any(on)) next
      if (sum(th[on]) * 2 >= sum(th) || all(on[in_upper])) return(c)
    }
    "OA"
  }
  n <- prod(lengths(opts))
  if (n > 5000) return(list(codes = unique(unlist(opts)), notes = "too many completions of the topsoil layers: undecided"))
  out <- character(0)
  ix <- rep(1L, length(opts))
  repeat {
    out <- unique(c(out, one(vapply(seq_along(opts), function(k) opts[[k]][ix[k]], ""))))
    k <- length(ix)
    while (k > 0L) { if (ix[k] < length(opts[[k]])) { ix[k] <- ix[k] + 1L; break }; ix[k] <- 1L; k <- k - 1L }
    if (k == 0L) break
  }
  cs <- vapply(run, function(i) if (is.na(fr$code[i])) "?" else fr$code[i], "")
  nt <- if (length(unique(cs)) > 1L) sprintf("topsoil layers read as %s: the topsoil is %s (R5)", paste(cs, collapse = ", "),
                                             paste(out, collapse = " or ")) else character(0)
  list(codes = out, notes = nt)
}

# The view of a pedon: the one classify_za1991() is keying, else built.
.za_current_view <- function(pedon) {
  if (!is.null(.za_state$view) && identical(.za_state$pedon, pedon)) return(.za_state$view)
  .za_view(pedon)
}

# The sequence of a view as text: undecided horizons in brackets.
.za_sequence_text <- function(view) {
  if (!length(view$elements)) return("(no diagnostic horizon)")
  paste(vapply(view$elements, function(e) {
    cs <- ifelse(nzchar(e$codes), e$codes, "none")
    if (length(cs) == 1L) cs else sprintf("{%s}", paste(cs, collapse = "|"))
  }, ""), collapse = " - ")
}


# --- the match of a form -------------------------------------------------------------

#' A form against the sequence of a view (D4, Hugo, 2026-11-06): one pass
#' over the horizons, keeping every state a completion may reach (the slots
#' matched, the last code kept, or a verdict); an undecided horizon steps
#' with each of its candidates, "" (no diagnostic horizon) a skip, a code
#' equal to the last kept one the same horizon. TRUE when every state ends
#' in a match, FALSE when none does, NA otherwise; a description that stops
#' before the form's last horizon (`open`) leaves a partial match undecided.
#' @noRd
.za_match_form <- function(view, f) {
  all_codes <- c(.za_tokens(), "")
  s <- f$slots; k <- length(s)
  ok <- function(j, cd) cd == s[j] || cd %in% (f$alternatives[[s[j]]] %||% character(0))
  st <- list(list(j = 0L, last = "", v = NA, done = FALSE))
  key <- function(z) paste(z$j, z$last, z$v, z$done)
  for (e in view$elements) {
    opts <- if (length(e$codes)) e$codes else all_codes
    nxt <- list()
    for (z in st) for (cd in opts) {
      y <- z
      if (!z$done && nzchar(cd) && cd != z$last) {
        if (z$j < k) {
          if (ok(z$j + 1L, cd)) { y$j <- z$j + 1L; y$last <- cd } else { y$done <- TRUE; y$v <- FALSE }
        } else { y$done <- TRUE; y$v <- !(cd %in% f$closed_on) }
      }
      nxt[[key(y)]] <- y
    }
    st <- unname(nxt)
  }
  vals <- vapply(st, function(z) if (z$done) z$v else if (z$j == k) TRUE else if (view$open) NA else FALSE, NA)
  p <- if (anyNA(vals)) NA else if (all(vals)) TRUE else if (!any(vals)) FALSE else NA
  list(passed = p, completions = length(st))
}

# The attributes that would decide the undecided layers of a view.
.za_view_missing <- function(view) {
  und <- which(is.na(view$frame$code) & !(view$frame$passed %in% FALSE))
  unique(unlist(view$frame$missing[und]))
}

# The nearest forms of a sequence by edit distance (Q2).
.za_nearest_forms <- function(view, n = 3L) {
  fs <- .za_forms()
  fs <- fs[!vapply(fs, function(f) f$outside, NA)]
  wit <- vapply(view$elements, function(e) { cs <- e$codes[nzchar(e$codes)]; if (length(cs)) cs[1L] else "" }, "")
  wit <- wit[nzchar(wit)]
  toks <- .za_tokens()
  ch <- function(v) paste(intToUtf8(64L + match(v, toks), multiple = TRUE), collapse = "")
  d <- vapply(fs, function(f) as.numeric(utils::adist(ch(wit), ch(f$slots))), 0)
  o <- utils::head(order(d, seq_along(d)), n)
  data.frame(code = names(fs)[o], name = vapply(fs[o], function(f) f$name, ""),
             sequence = vapply(fs[o], function(f) paste(f$slots, collapse = "-"), ""),
             distance = d[o], stringsAsFactors = FALSE, row.names = NULL)
}


# --- the gates ------------------------------------------------------------------------

#' The sequence of a soil form (key.yaml): its horizons the first of the
#' profile's, in order, and the next one not one of `closed_on` (Q3).
#' @param pedon A PedonRecord.
#' @param form The code of the form.
#' @noRd
form_sequence_za <- function(pedon, form) {
  view <- .za_current_view(pedon)
  f <- .za_forms()[[form]]
  if (is.null(f)) rlang::abort(sprintf("No South African form '%s'.", form))
  m <- .za_match_form(view, f)
  DiagnosticResult$new(
    name = "form_sequence_za", passed = m$passed, layers = integer(0),
    evidence = list(form = form, formula = paste(f$slots, collapse = "-"), sequence = .za_sequence_text(view),
                    closed_on = f$closed_on, open = view$open, completions = m$completions),
    missing = if (is.na(m$passed)) unique(c(.za_view_missing(view), if (view$open) "bottom_cm")) else character(0),
    reference = .ZA_REFERENCE,
    notes = sprintf("profile %s against %s %s", .za_sequence_text(view), f$name, paste(f$slots, collapse = "-")))
}

#' What lies below a horizon (K 24-26, 49-51, 53-55; plan 3.3): with
#' material "SAP", saprolite (or hard rock) within `max_cm` of the lower limit
#' of the horizon (of its upper layer, the B1, with from = "B1"); with
#' "non_saprolite", at least `min_cm` of material that is not saprolite or
#' rock below it. A description that stops short leaves it undecided (Q4); a
#' B that reaches 1500 mm has its material below (Q4), flagged.
#' @noRd
below_horizon_za <- function(pedon, horizon, material = c("SAP", "non_saprolite"), min_cm = NA_real_,
                             max_cm = NA_real_, from = c("horizon", "B1")) {
  material <- match.arg(material); from <- match.arg(from)
  view <- .za_current_view(pedon)
  el <- view$elements
  res <- function(p, note, missing = character(0))
    DiagnosticResult$new(name = "below_horizon_za", passed = p, layers = integer(0), evidence = list(sequence = .za_sequence_text(view)),
                         missing = if (is.na(p)) missing else character(0), reference = .ZA_REFERENCE, notes = note)
  e <- which(vapply(el, function(z) identical(z$codes, horizon), NA))
  if (!length(e)) {
    cand <- any(vapply(el, function(z) horizon %in% z$codes, NA))
    return(res(if (cand) NA else FALSE, sprintf("no %s identified", .za_label(horizon)), .za_view_missing(view)))
  }
  e <- e[1L]
  x <- view$x
  ly <- el[[e]]$layers
  limit <- if (from == "B1" && length(ly)) x$bot[ly[1L]] else el[[e]]$bottom
  rockish <- c("SAP", "HRK")
  hit <- NA_integer_; amb <- FALSE
  for (k in seq_along(el)[seq_along(el) > e]) {
    cs <- el[[k]]$codes
    if (length(cs) == 1L && cs %in% rockish) { hit <- k; break }
    if (any(cs %in% rockish)) { amb <- TRUE; break }
  }
  bot <- view$bottom
  if (amb) return(res(NA, "a layer below may be saprolite or rock", .za_view_missing(view)))
  if (material == "SAP") {
    if (!is.na(hit)) {
      gap <- el[[hit]]$top - limit
      return(res(isTRUE(gap <= max_cm), sprintf("saprolite or rock %s cm below the %s (%s cm or less)", format(gap), .za_label(horizon), format(max_cm))))
    }
    return(res(if (view$open) NA else FALSE, "no saprolite or rock below", "bottom_cm"))
  }
  if (!is.na(hit)) {
    gap <- el[[hit]]$top - limit
    return(res(isTRUE(gap >= min_cm), sprintf("%s cm of material above saprolite or rock below the %s (%s cm or more)", format(gap), .za_label(horizon), format(min_cm))))
  }
  if (view$implicit || isTRUE(bot - limit >= min_cm))
    return(res(TRUE, sprintf("no saprolite or rock within the description%s", if (view$implicit) "; the B reaches 1500 mm (Q4)" else "")))
  res(if (view$open) NA else TRUE, sprintf("the description stops %s cm below the %s", format(bot - limit), .za_label(horizon)), "bottom_cm")
}

#' The orthic A and a material (man-made deposit, stratified alluvium, regic
#' sand) more than `min_cm` thick when they lie on a classifiable buried
#' soil (K 38, 74, 75; R5); on anything else, any thickness.
#' @noRd
deposit_thickness_za <- function(pedon, material, min_cm = 50) {
  view <- .za_current_view(pedon)
  el <- view$elements
  res <- function(p, note) DiagnosticResult$new(name = "deposit_thickness_za", passed = p, layers = integer(0),
                                                evidence = list(sequence = .za_sequence_text(view)),
                                                missing = if (is.na(p)) .za_view_missing(view) else character(0),
                                                reference = .ZA_REFERENCE, notes = note)
  e <- which(vapply(el, function(z) identical(z$codes, material), NA))
  if (!length(e)) return(res(if (any(vapply(el, function(z) material %in% z$codes, NA))) NA else FALSE,
                             sprintf("no %s identified", .za_label(material))))
  e <- e[1L]
  not_soil <- c("HRK", "SAP", "G", "USW", "UMW", "UMN", material, "")
  below <- seq_along(el)[seq_along(el) > e]
  soil <- vapply(below, function(k) !all(el[[k]]$codes %in% not_soil), NA)
  x <- view$x
  marked <- any(vapply(unlist(lapply(el[below], `[[`, "layers")), function(i)
    isTRUE(grepl("^[0-9]*[A-Z]+[a-z]*b[0-9]*$", .za_des(x, i))), NA))
  if (!any(soil) && !marked) return(res(TRUE, "no classifiable buried soil below: any thickness (R5)"))
  th <- el[[e]]$bottom
  res(isTRUE(th > min_cm), sprintf("the A and the %s %s cm thick over a buried soil (more than %s cm, R5)",
                                  .za_label(material), format(th), format(min_cm)))
}
