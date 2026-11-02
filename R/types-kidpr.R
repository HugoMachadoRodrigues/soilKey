# =============================================================================
# v0.9.338 -- the types of the Russian soil classification (Field Guide of
# 2008, chapter 4): the profile formula of each type, matched on the
# diagnostic horizons of the profile.
#
# The Field Guide names a type by the formula of its profile, drawn as a
# column of horizons ("O-EL-BEL-BT-C"); it has no key to types. soilKey
# matches the profile against the formulas of the division it keyed to, with
# match_horizon_sequence() (R/sequence-engine.R), three-valued. The profile
# is the one the divisions read (R/diagnostics-kidpr.R): fresh litter left
# out, under a deposit of 40 cm or less, the surface horizons by the
# thickest-horizon rule of p. 37 (the thinner natural ones dropped, a W of
# 5 cm or less over another horizon dropped); the matcher merges the
# sub-horizons (BT1, BT2) and leaves out the transitions and the small
# horizons; an overlay (S, X or V laid over a host) is a feature of its
# layer, not a horizon of the sequence.
#
# Hugo's decisions of 2026-11-02:
#   Q13 the formulas are closed on the division: a horizon that some formula
#       of the division writes must be in the type's formula when it is in
#       the profile (in CA: EL, AEL, BEL, BT, BCA, G, Q); a horizon foreign
#       to the division is ignored (it may name a subtype). The surface and
#       agro horizons a formula does not write stay out of that closure, so
#       the natural remnant under the plough does not block an agro type.
#       The types are tried most specific first (more required horizons,
#       then more required marks), agro before natural; when two match, the
#       more specific names the type and the other is an intergrade, kept in
#       the trace. BEL is the exception (Hugo, 2026-11-02, from p. 44): it
#       closes only the formulas where it is diagnostic (CA07, CA08, CA11,
#       CA20, CA21, CD09); elsewhere a BEL is read as part of the passage from
#       the eluvial to the middle horizon, neither blocking a formula nor
#       required by it (the type files list it under `ignore`). So is BCA
#       in the formulas of CD that do not write it (p. 85), and in CB and
#       CE an EL counts with E (p. 43): it fills the E of a formula and
#       blocks the formulas without E.
#   Q14 a horizon in parentheses is optional; BEL is required only where
#       p. 44 calls it diagnostic (CA07, CA08, CA11, CA20, CA21, CD09) and
#       optional elsewhere (the type files say where); the g and q marks on
#       BEL, BT, BHF or BCA are expected, not required (the G or the Q makes
#       the gley type); the parent material (C, Cca, CG, CQ, C with
#       permafrost, CLM, D) is not tested; "directly under" passes over
#       transitions and sub-horizons, with up to 15 cm with no diagnostic
#       horizon between AU and BCA (p. 94).
#   Q15 a profile no formula matches keeps its division, the type
#       unassigned, with the nearest formulas in the trace and a note that
#       the Field Guide of 2008 does not provide for the combination. Before
#       that only near-identity readings, written on the types they concern
#       (`near_identity`): the BF of CB05, CE06 and CE07 is a BHF of either
#       kind; CJ01 takes BCA or CAT; CH02 takes O, T or H over G-CRM.
#
# An undecided layer that could change the match leaves the type undecided:
# a type ranked before the one assigned that is undecided makes it
# provisional; with none matched and some undecided, the type is left
# unassigned and the candidates are named.
#
# The formulas are in inst/rules/kidpr2008/types/<division>.yaml, one entry
# per type, the code in brackets (soilKey's: [CA01] ...) in the order of
# chapter 4, natural types first and then the agro types; the entry's test
# is type_formula_kidpr(). classify_kidpr2008() reorders each list into the
# order of trial before the generic engine walks it.
# =============================================================================

.KIDPR_TY_REF <- "Polevoi opredelitel' pochv Rossii (2008), chapter 4 (the profile formulas of the types)."

# Near-identity readings every formula takes (soilKey's reading): the peat
# T of a formula includes the oligotrophic and eutrophic peats TO and TE, as
# in the key to divisions.
.KIDPR_TY_ALTERNATIVES <- list(T = c("TO", "TE"))

# "Directly under": a carbonate-free part of up to 15 cm between AU and BCA
# is not counted (p. 94; Q14).
.KIDPR_TY_GAPS <- c("AU>BCA" = 15)

# Parsed formulas of the type lists, by the content of the list.
.kidpr_ty_cache <- new.env(parent = emptyenv())


# The division of a type code: "[CA01]" -> "CA".
.kidpr_ty_division <- function(type) {
  x <- gsub("[][]", "", as.character(type)[1L])
  sub("[0-9]+$", "", x)
}

# The type lists of the installed rules (a predicate called outside a
# classification).
.kidpr_ty_installed <- function() {
  if (is.null(.kidpr_ty_cache$lists)) .kidpr_ty_cache$lists <- load_rules("kidpr2008")$types %||% list()
  .kidpr_ty_cache$lists
}

# The type entries of a division in a set of lists (keyed by the division
# code, or by its bracketed code, [DA]).
.kidpr_ty_list <- function(lists, div) {
  lists[[div]] %||% lists[[paste0("[", div, "]")]] %||% list()
}

# The formulas of a list of type entries, parsed: the near-identity readings
# of each type (with the T of every formula), its optional horizons and the
# parent slots it tests; the order of trial (the agro types, then the
# natural ones, each in the order of the list; then most specific first:
# the order .pick_scheme() reads); the vocabulary each formula is closed on:
# the division's (Q13), less the horizons the type ignores (BEL where it is
# not diagnostic, Hugo, 2026-11-02).
.kidpr_ty_parse <- function(entries) {
  key <- paste(vapply(entries, function(e) paste(
    e$code, e$formula, paste(unlist(e$optional), collapse = ","),
    paste(names(e$near_identity), vapply(e$near_identity %||% list(), paste, "", collapse = "|"), collapse = ";"),
    paste(unlist(e$test_terminals), collapse = ","), paste(unlist(e$ignore), collapse = ","),
    sep = "\t"), ""), collapse = "\r")
  hit <- .kidpr_ty_cache[[key]]
  if (!is.null(hit)) return(hit)
  voc <- .kidpr_vocabulary()
  codes <- vapply(entries, function(e) as.character(e$code), "")
  specs <- lapply(entries, function(e) {
    alt <- .KIDPR_TY_ALTERNATIVES
    ni <- e$near_identity %||% list()
    for (k in names(ni)) alt[[k]] <- unique(c(alt[[k]], as.character(unlist(ni[[k]]))))
    .parse_profile_formula(e$formula, masters = voc$masters, features = voc$features,
                           test_terminals = as.character(unlist(e$test_terminals)),
                           optional = as.character(unlist(e$optional)), alternatives = alt)
  })
  names(specs) <- codes
  agro <- vapply(specs, function(s) {
    z <- Filter(function(z) identical(z$role, "surface"), s$slots)
    length(z) > 0L && any(z[[1L]]$codes %in% .KIDPR_DV_AGRO)
  }, NA)
  base <- c(which(agro), which(!agro))
  sp <- vapply(specs, function(s) as.numeric(s$specificity[1:2]), c(0, 0))
  trial <- base[order(-sp[1L, base], -sp[2L, base], seq_along(base))]
  voc <- .formula_vocabulary(specs)
  vocabularies <- stats::setNames(lapply(entries, function(e) setdiff(voc, as.character(unlist(e$ignore)))), codes)
  out <- list(specs = specs[base], codes = codes, trial = codes[trial], agro = agro,
              vocabulary = voc, vocabularies = vocabularies)
  assign(key, out, envir = .kidpr_ty_cache)
  out
}

# The type lists of a rule set in the order of trial, kept on the view of the
# classification for type_formula_kidpr(). classify_hierarchical() walks a
# list in its order and makes the class assigned provisional when one before
# it is undecided, so the lists must be in the order the matcher picks in.
.kidpr_types_attach <- function(rules, view = NULL) {
  ty <- rules$types %||% list()
  for (d in names(ty)) {
    e <- ty[[d]]
    if (length(e) < 2L) next
    sch <- .kidpr_ty_parse(e)
    ty[[d]] <- e[match(sch$trial, sch$codes)]
  }
  if (length(ty)) rules$types <- ty
  if (!is.null(view)) view$type_lists <- ty
  rules
}

# The profile as the types read it: the rows of the view under a deposit of
# 40 cm or less, fresh litter out, buried rows marked; an overlay is a feature
# of its layer. The surface horizons all stay, but for a W of 5 cm or less
# over another horizon (p. 39): the matcher applies the thickest-horizon rule
# of p. 37 itself (agro_remnant "thickest": under the agro horizon only the
# thickest natural horizon stays, its remnant), so that a tie or a missing
# thickness, or an undecided layer that could thicken a horizon of the
# surface run, leaves the type undecided instead of following one reading.
# Rows the view reads as transitions or loose rock (the FAO transitions BC,
# B/C and the FAO C of the data route) are transitions and parent material
# here too. A data-route layer with no horizon may be a transition or loose
# rock (Hugo, 2026-11-02): it is undecided between no horizon and loose
# rock, so between two horizons of a formula it leaves the type undecided.
.kidpr_ty_table <- function(v) {
  f <- v$f
  rows <- if (v$start <= v$n) seq.int(v$start, v$n) else integer(0)
  if (length(v$dep$rows) && isTRUE(v$dep$thick <= 40)) rows <- setdiff(rows, v$dep$rows)
  # the W of 5 cm or less the view drops in every reading
  wdrop <- integer(0)
  for (e in v$hz_raw) if (identical(e$code, "W") && e$kind == "horizon" && isTRUE(e$thick <= 5) &&
                          all(vapply(v$var, function(w) !any(vapply(w$hz, function(h) identical(h$rows, e$rows), NA)), NA)))
    wdrop <- c(wdrop, e$rows)
  rows <- setdiff(rows, wdrop)
  cols <- c("horizon_idx", "top_cm", "bottom_cm", "designation", "code", "passed", "small",
            "transitional", "rockflag", "mark", "route", "evidence", "features", "features_na",
            "composition", "missing", "candidates")
  t <- f[rows, intersect(cols, names(f)), drop = FALSE]
  kind <- v$kind[rows]
  t$transitional <- t$transitional | kind == "trans"
  rk <- kind == "rock" & is.na(t$code)
  t$code[rk] <- "C"
  t$passed[rk] <- TRUE
  nh <- kind == "none" & !(t$route %in% "notation")
  t$passed[nh] <- NA
  t$candidates[nh] <- rep(list(c("", "C")), sum(nh))
  t$missing[nh] <- lapply(t$missing[nh], function(m) if (length(m)) m else "designation")
  t$no_horizon <- nh
  t$buried <- v$bur[rows]
  ov <- f$overlay[rows]
  ovna <- if (is.null(f$overlay_na)) rep(list(character(0)), length(rows)) else f$overlay_na[rows]
  t$features <- Map(function(a, b) unique(c(as.character(a), as.character(b))), t$features, ov)
  t$features_na <- Map(function(a, b) unique(c(as.character(a), as.character(b))), t$features_na, ovna)
  t$overlay <- rep(list(character(0)), length(rows))
  rownames(t) <- NULL
  t
}

# The match of the profile against the formulas of a division, once per
# view: .match_schemes() with the pick, the intergrades, the nearest
# formulas and every result; NULL when the division has no types.
.kidpr_ty_match <- function(v, div) {
  .kidpr_dv_memo(v, paste0("types|", div), function() {
    entries <- .kidpr_ty_list(v$type_lists %||% .kidpr_ty_installed(), div)
    if (!length(entries)) return(NULL)
    sch <- .kidpr_ty_parse(entries)
    tab <- .kidpr_ty_table(v)
    m <- .match_schemes(tab, sch$specs, vocabulary = sch$vocabularies,
                        surface = .KIDPR_DV_NATURAL, agro = .KIDPR_DV_AGRO, agro_remnant = "thickest",
                        pair_gaps_cm = .KIDPR_TY_GAPS)
    m$division <- div
    m$schemes <- sch
    m$table <- tab
    m$names <- stats::setNames(vapply(entries, function(e) as.character(e$name %||% ""), ""),
                               sch$codes)
    m
  })
}

# The verdict of one type within the pick: TRUE for the type picked; for a
# type ranked before it in the order of trial, its own answer (FALSE, or NA,
# which makes the type picked provisional); FALSE for a type after it (an
# intergrade when its formula matches too). With no type picked, its own
# answer.
.kidpr_ty_verdict <- function(m, code) {
  r <- m$results[[code]]
  if (isTRUE(m$passed)) {
    if (identical(code, m$code)) return(TRUE)
    if (match(code, m$schemes$trial) < match(m$code, m$schemes$trial)) return(r$passed)
    return(FALSE)
  }
  r$passed
}

# The rows (horizon_idx) of the horizons that fill the slots of a match.
.kidpr_ty_layers <- function(matched) {
  if (is.null(matched) || !nrow(matched)) return(integer(0))
  ids <- regmatches(matched$horizon, regexpr("\\(([^()]*)\\)$", matched$horizon))
  ids <- gsub("[()]", "", ids)
  out <- integer(0)
  for (s in unlist(strsplit(ids, ", ", fixed = TRUE))) {
    ab <- suppressWarnings(as.integer(strsplit(s, "-", fixed = TRUE)[[1L]]))
    if (length(ab) == 2L && !anyNA(ab)) out <- c(out, seq.int(ab[1L], ab[2L]))
    else if (length(ab) == 1L && !is.na(ab)) out <- c(out, ab)
  }
  unique(out)
}

# The attributes that would decide an undecided match: those of the
# undecided layers, else the depths (a tie of thickness at the surface).
.kidpr_ty_missing <- function(m) {
  tab <- m$table
  und <- which(is.na(tab$code) & !(tab$passed %in% FALSE) & tab$buried == 0L)
  miss <- unique(unlist(tab$missing[und]))
  if (!length(miss)) miss <- c("top_cm", "bottom_cm")
  miss
}

# The horizons of the profile as the types read them, from the top
# ("AY-EL-BT-G-C"): sub-horizons merged, transitions, small horizons and
# layers with no horizon left out, an undecided layer "?".
.kidpr_ty_sequence <- function(tab) {
  nh <- if (is.null(tab$no_horizon)) FALSE else tab$no_horizon
  keep <- !(tab$small | tab$transitional) & !(is.na(tab$code) & tab$passed %in% FALSE) & !nh & tab$buried == 0L
  cd <- ifelse(is.na(tab$code[keep]), "?", tab$code[keep])
  if (!length(cd)) return("")
  paste(cd[c(TRUE, cd[-1L] != cd[-length(cd)] | cd[-1L] == "?")], collapse = "-")
}

# One line on a formula and how far the profile is from it: the slots
# missing, the horizons not in the formula, and a horizon not directly under
# the one above it (a gap with no diagnostic horizon).
.kidpr_ty_describe <- function(r) {
  gap <- grep(" across .* with no diagnostic horizon", r$notes, value = TRUE)
  parts <- c(if (length(r$missing)) sprintf("missing %s", paste(r$missing, collapse = ", ")),
             if (length(r$extra)) sprintf("not in the formula: %s", paste(r$extra, collapse = ", ")),
             gap)
  sprintf("%s, distance %s%s", r$formula, format(r$distance),
          if (length(parts)) paste0(" (", paste(parts, collapse = "; "), ")") else "")
}


#' Types of the Russian soil classification (internal)
#'
#' The test of the types of the Russian soil classification (Field Guide
#' of 2008, chapter 4), read by \code{\link{classify_kidpr2008}} through
#' the type lists of \code{inst/rules/kidpr2008/types/}. A type is named by
#' the formula of its profile ("O-EL-BEL-BT-C"); the profile is matched
#' against every formula of its division with
#' \code{\link{match_horizon_sequence}}, three-valued, on the horizons the
#' divisions read (\code{\link{identify_horizons}}): fresh litter left out,
#' the profile under a deposit of 40 cm or less, the surface horizons by the
#' thickest-horizon rule, sub-horizons merged, transitions and small
#' horizons left out, an overlay a feature of its layer.
#'
#' Hugo's decisions of 2026-11-02: the match is closed on the horizons the
#' formulas of the division write (a horizon foreign to the division is
#' ignored), BEL only where it is the diagnostic eluvial horizon (CA07, CA08,
#' CA11, CA20, CA21, CD09; elsewhere a BEL neither blocks a formula nor is
#' required by it); a horizon in parentheses is optional, and so is BEL
#' except where it is diagnostic; the g and q marks are expected, not
#' required; the parent material is not tested; "directly under" passes over
#' transitions and sub-horizons, and over up to 15 cm with no diagnostic
#' horizon between AU and BCA. The types are tried most specific first, agro
#' before natural: the first that matches is the type, and another that
#' matches is an intergrade, in the evidence. A few near-identity readings
#' are written on the types they concern (the BF of CB05, CE06 and CE07 is a
#' BHF of either kind; CJ01 takes BCA or CAT; CH02 takes O, T or H; in CB and
#' CE an EL fills the E of a formula); in CD a BCA, like BEL, closes no
#' formula that does not write it. A tie or a missing thickness at the
#' surface, or a layer the data route reads as no horizon between two
#' horizons of a formula, leaves the type undecided.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param type The code of a type, as its rule file writes it
#'   (\code{"[CA01]"}).
#' @return A \code{\link{DiagnosticResult}}: TRUE for the type of the
#'   profile; FALSE for another type of the division (one whose formula
#'   matches as well is an intergrade, named in the evidence); NA when an
#'   undecided layer could change the match. The evidence gives the formula,
#'   the horizons that fill its slots, the optional ones absent, the slots
#'   missing, the horizons not in the formula, the horizons left out of the
#'   match, the surface horizon and the edit distance.
#' @keywords internal
#' @name kidpr2008_types
NULL

#' @rdname kidpr2008_types
#' @export
type_formula_kidpr <- function(pedon, type) {
  v <- .kidpr_dv_of(pedon)
  type <- as.character(unlist(type))[1L]
  div <- .kidpr_ty_division(type)
  m <- .kidpr_ty_match(v, div)
  r <- if (is.null(m)) NULL else m$results[[type]]
  if (is.null(r)) {
    return(DiagnosticResult$new(name = "type_formula_kidpr", passed = NA,
                                evidence = list(type = type), missing = "diagnostic_type_formula_kidpr",
                                reference = .KIDPR_TY_REF,
                                notes = sprintf("No formula of the type %s in the rules.", type)))
  }
  p <- .kidpr_ty_verdict(m, type)
  picked <- isTRUE(p)
  notes <- r$notes
  if (isTRUE(r$passed) && !picked && isTRUE(m$passed))
    notes <- c(notes, sprintf("the formula matches too: an intergrade of %s, which names the type (Hugo's decision Q13)",
                              m$code))
  if (picked && length(m$intergrades))
    notes <- c(notes, sprintf("intergrade: the formula of %s matches too (Hugo's decision Q13)",
                              paste(m$intergrades, collapse = ", ")))
  ev <- list(type = type, formula = r$formula, matched = r$matched, absent = r$absent,
             missing_slots = r$missing, extra = r$extra, ignored = r$ignored,
             surface = r$surface, absorbed = r$absorbed, sequence = r$sequence,
             distance = r$distance, undecided = r$undecided)
  if (picked) {
    ev$intergrades <- m$intergrades
    ev$provisional_because <- m$provisional_because
  }
  DiagnosticResult$new(name = "type_formula_kidpr", passed = p,
                       layers = if (picked) .kidpr_ty_layers(r$matched) else integer(0),
                       evidence = ev,
                       missing = if (is.na(p)) .kidpr_ty_missing(m) else character(0),
                       reference = .KIDPR_TY_REF,
                       notes = if (length(notes)) unique(notes) else NA_character_)
}


# The notes of the type level on a result: an intergrade; the candidates
# when the type is undecided; a combination of horizons no formula of the
# division provides for (Q15), with the nearest formulas, in the warnings and
# in the trace (phase "types_nearest").
.kidpr_types_notes <- function(res, view) {
  lv <- res$levels
  if (is.null(lv) || nrow(lv) < 2L || !identical(lv$level[2L], "divisions")) return(res)
  div <- lv$code[2L]
  if (!length(.kidpr_ty_list(view$type_lists %||% .kidpr_ty_installed(), div))) return(res)
  m <- .kidpr_ty_match(view, div)
  if (is.null(m)) return(res)
  label <- function(k) sprintf("%s %s", k, m$results[[k]]$formula)
  notes <- character(0)
  if (nrow(lv) >= 3L) {
    if (length(m$intergrades)) notes <- c(notes, sprintf(
      "Intergrade: the profile also matches the formula of %s; %s names the type, the more specific or the first in the order of trial (Hugo's decision Q13).",
      paste(vapply(m$intergrades, label, ""), collapse = "; "), label(lv$code[3L])))
  } else if (is.na(m$passed)) {
    cands <- vapply(m$candidates, label, "")
    notes <- c(notes, sprintf(
      "The type is undecided%s: an undecided layer could change the match (see $missing_data); the name stops at the division.",
      if (length(cands) == 1L) sprintf(", the formula of %s may match", cands)
      else sprintf(" between %s", paste(cands, collapse = "; "))))
  } else {
    near <- m$nearest
    stop_note <- sprintf("^No class of the types list under %s passed", div)
    res$warnings <- res$warnings[!grepl(stop_note, res$warnings)]
    seq_txt <- .kidpr_ty_sequence(m$table)
    notes <- c(notes, sprintf(
      "Combination not provided by the Field Guide of 2008: no type formula of the %s (%s) matches the horizons %s, so the type is unassigned (Hugo's decision Q15). Nearest formulas: %s.",
      lv$name[2L], div, if (nzchar(seq_txt)) seq_txt else "of the profile",
      paste(vapply(seq_len(nrow(near)), function(i)
        sprintf("%s %s", near$code[i], .kidpr_ty_describe(m$results[[near$code[i]]])), ""), collapse = "; ")))
    res$trace[["types_nearest"]] <- stats::setNames(lapply(seq_len(nrow(near)), function(i) {
      r <- m$results[[near$code[i]]]
      list(code = near$code[i], name = m$names[[near$code[i]]], passed = FALSE,
           formula = r$formula, distance = r$distance, missing_slots = r$missing, extra = r$extra,
           missing = character(0), notes = .kidpr_ty_describe(r))
    }), near$code)
  }
  if (length(notes)) res$warnings <- c(res$warnings, notes)
  res
}
