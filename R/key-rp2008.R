# =============================================================================
# v0.9.346 -- the French Referentiel pedologique 2008 (AFES; Baize D.,
# Girard M.-C. coord.; Quae, 2009): the attachment of a solum to its
# references. Experimental and opt-in: classify_all() runs it only when
# asked.
#
# The RP is not a key: a solum attaches to one, two or no reference, simply
# or imperfectly (pp. XIV, XVII). soilKey tests every reference every time
# (Hugo's decision Q3): each one's degree from a strict and a tolerant
# evaluation of its tests (.evaluate_degree(), the tolerances of p. 64,
# Q4-Q5), the explicit imperfect cases of its chapter; then one main name
# by the precedence (the book's explicit statements, the degree, the order
# of the rapid key, Q6), the doubles the book prescribes (Q7), "[non
# rattache]" with the three nearest references when nothing attaches; and
# the `candidates` slot with every reference that may attach. This release
# tests the 71 references of the first twenty-one GER (v0.9.346: the first
# nine; v0.9.348: the ferrallitisols and oxydisols, ferruginosols,
# fersialsols, fluviosols, grisols and gypsosols; v0.9.349: the histosols,
# leptismectisols, lithosols, luvisols, nitosols and organosols); the 39 of
# the other 13 GER come later, and a profile showing their horizons is told
# so in the candidates (their signposts in key.yaml), its name provisional.
# =============================================================================

.rp_rules_cache <- new.env(parent = emptyenv())

# The rules of the RP, read once (load_rules("rp2008"): key.yaml, the
# references and the files of system.yaml), with the lookups the classifier
# needs.
.rp_rules <- function() {
  if (!is.null(.rp_rules_cache$rules)) return(.rp_rules_cache$rules)
  r <- .rp_rules_index(load_rules("rp2008"))
  assign("rules", r, envir = .rp_rules_cache)
  r
}

.rp_rules_index <- function(r) {
  r$ger_by_code <- stats::setNames(r$ger, vapply(r$ger, function(g) g$code, ""))
  r$ref_by_code <- stats::setNames(r$references, vapply(r$references, function(x) x$code, ""))
  # the position of each GER in the rapid key: its first outcome
  pos <- list(); n <- 0L
  for (e in r$rapid_key) for (o in as.character(unlist(e$outcomes))) {
    n <- n + 1L
    if (is.null(pos[[o]])) pos[[o]] <- n
  }
  r$ger_rank <- pos
  r$entry_of <- list()
  for (e in r$rapid_key) for (o in as.character(unlist(e$outcomes)))
    if (is.null(r$entry_of[[o]])) r$entry_of[[o]] <- e$entry
  r
}

# The GER label of a reference or GER code, as the results show it.
.rp_ger_label <- function(rules, ger) {
  g <- rules$ger_by_code[[ger]]
  if (is.null(g)) return(NA_character_)
  g$label %||% g$name
}

# The order that breaks the remaining ties (Q6): the GER by their first
# entry in the rapid key (entries 1 to 10, then 11 and 12, the grisols in
# soilKey's 7c), the references of a GER in the book's order; a GER of a
# later release by its signpost row, before its references would come.
.rp_order <- function(rules) {
  rows <- list()
  for (g in rules$ger) {
    rk <- rules$ger_rank[[g$code]] %||% 999L
    if (isTRUE(g$implemented)) {
      refs <- as.character(unlist(g$references))
      for (k in seq_along(refs)) rows[[length(rows) + 1L]] <- data.frame(code = refs[k], rk = rk, k = k)
    } else rows[[length(rows) + 1L]] <- data.frame(code = g$code, rk = rk, k = 0L)
  }
  d <- do.call(rbind, rows)
  d$code[order(d$rk, d$k)]
}

# A test block of the rules evaluated strictly on the pedon (the `when` of
# a precedence pair, an exclusion or a double): TRUE when absent.
.rp_when <- function(pedon, when) {
  if (is.null(when)) return(TRUE)
  as.logical(evaluate_rsg_tests(pedon, when)$passed)
}

# The explicit precedence pairs among the codes present ("*" for every
# other code).
.rp_precedence_rows <- function(rules, pedon, codes) {
  out <- list()
  for (r in rules$precedence) {
    f <- as.character(unlist(r$first)); s <- as.character(unlist(r$second))
    ex <- as.character(unlist(r$except))
    if (identical(f, "*")) f <- setdiff(codes, c(s, ex))
    if (identical(s, "*")) s <- setdiff(codes, c(f, ex))
    f <- intersect(f, codes); s <- intersect(s, codes)
    if (!length(f) || !length(s)) next
    w <- .rp_when(pedon, r[["when"]])
    for (a in f) for (b in s) if (a != b)
      out[[length(out) + 1L]] <- list(first = a, second = b, when = w,
                                      note = sprintf("%s (p. %s)", r$note %||% "", r$page %||% "?"))
  }
  out
}

.rp_exclusion_rows <- function(rules, pedon, codes) {
  out <- list()
  for (r in rules$exclusions) {
    if (!(r$code %in% codes) || !(r$by %in% codes)) next
    out[[length(out) + 1L]] <- list(code = r$code, by = r$by, when = .rp_when(pedon, r[["when"]]),
                                    note = sprintf("%s (p. %s)", r$note %||% "", r$page %||% "?"))
  }
  out
}

# The doubles of doubles.yaml whose references are all implemented.
.rp_group_rows <- function(rules, pedon, codes, evals = list()) {
  out <- list()
  for (d in rules$doubles) {
    cc <- as.character(unlist(d$codes))
    if (identical(d$status, "later") || !all(cc %in% codes)) next
    w <- .rp_when(pedon, d[["when"]])
    # `when_degree`: the degree a member must have for the double (the
    # anthroposol transforme imperfect, the intermediate case of p. 91)
    for (k in names(d$when_degree %||% list())) {
      dg <- evals[[k]]$degree %||% NA_character_
      w <- w && (dg %in% as.character(unlist(d$when_degree[[k]])))
    }
    out[[length(out) + 1L]] <- list(codes = cc, when = w,
                                    note = sprintf("p. %s", d$page %||% "?"))
  }
  out
}

# The degree of one reference: its tests, then the explicit imperfect
# cases of its chapter when the tests do not attach it (an imperfect
# attachment named by its qualifier, p. XVII).
.rp_eval_reference <- function(ref, pedon, tol) {
  d <- .evaluate_degree(pedon, ref$tests, tol, system = .RP_SYSTEM)
  d$alt <- NULL
  if (!length(ref$imperfect)) return(d)
  # imperfect within tolerance: the imperfect case of the chapter whose test
  # holds names it (calcosol leptique at [A + Sca] 32 cm; the review's D1)
  if (d$degree == "imperfect") {
    for (alt in ref$imperfect) {
      if (is.null(alt$qualifier)) next
      a <- .evaluate_degree(pedon, alt$tests, tol, system = .RP_SYSTEM)
      # only a case whose test holds strictly names it
      if (identical(a$degree, "simple")) {
        d$imperfect_by <- unique(c(sprintf("%s (p. %s)", alt$qualifier, alt$page %||% ref$page),
                                   d$imperfect_by, a$imperfect_by))
        d$alt <- alt
        d$alt_evidence <- a$evidence
        return(d)
      }
    }
    return(d)
  }
  if (!(d$degree %in% c("fail", "undecided"))) return(d)
  for (alt in ref$imperfect) {
    a <- .evaluate_degree(pedon, alt$tests, tol, system = .RP_SYSTEM)
    lab <- if (!is.null(alt$qualifier)) sprintf("%s (p. %s)", alt$qualifier, alt$page %||% ref$page)
           else sprintf("%s", alt$criterion %||% "an imperfect case of the chapter")
    if (a$degree %in% .ATTACHED) {
      if (d$degree == "fail") {
        d$degree <- "imperfect"; d$strict <- FALSE; d$best <- "imperfect"
        d$failed <- character(0); d$distance <- NA_real_
        d$imperfect_by <- unique(c(lab, a$imperfect_by))
      } else {
        d$degree <- "simple_or_imperfect"; d$best <- "simple"
        d$imperfect_by <- unique(c(lab, a$imperfect_by))
      }
      d$alt <- alt
      d$alt_evidence <- a$evidence
      return(d)
    }
    if (a$degree == "undecided" && d$degree == "fail") {
      d$degree <- "undecided"; d$strict <- FALSE; d$best <- "imperfect"
      d$undecided_by <- unique(c(sprintf("%s: undecided", lab), a$undecided_by))
      d$failed <- character(0); d$distance <- NA_real_
      d$missing <- unique(c(d$missing, a$missing))
    }
  }
  d
}

# The GER of later releases whose signpost the profile shows: undecided
# candidates, "not implemented yet" (item 4 of the brief: never routed
# elsewhere silently).
.rp_signposts <- function(rules, pedon) {
  out <- list()
  for (g in rules$ger) {
    if (isTRUE(g$implemented) || is.null(g$signpost)) next
    s <- evaluate_rsg_tests(pedon, g$signpost)
    if (isFALSE(s$passed)) next
    n <- length(g$references)
    entry <- rules$entry_of[[g$code]] %||% "-"
    out[[g$code]] <- list(
      code = g$code, name = g$name, label = g$name, group = g$code, degree = "undecided",
      strict = NA,
      undecided_by = sprintf(paste("not implemented yet: the %d reference(s) of the %s (pp. %s, rapid key %s)",
                                   "are not tested in this release; the profile %s their horizons"),
                             n, g$name, g$pages, entry, if (isTRUE(s$passed)) "shows" else "may show"),
      missing = s$missing %||% character(0), evidence = s)
  }
  out
}

#' Attach a pedon to the references of the French Referentiel pedologique 2008
#'
#' Attaches a \code{\link{PedonRecord}} to the references of the
#' \emph{Referentiel pedologique} 2008 (AFES; Baize and Girard coord.;
#' Editions Quae, 2009). \strong{Experimental}: this release tests the 71
#' references of the first twenty-one groups of references (GER): alocrisols
#' (2), andosols (6), anthroposols (5), arenosols (1), brunisols (2), the
#' solums dominated by calcium or magnesium (7: rendosols, calcosols,
#' dolomitosols, rendisols, calcisols, magnesisols, calcarisols),
#' chernosols (3), colluviosols (1), cryosols (2), ferrallitisols and
#' oxydisols (7), ferruginosols (4), fersialsols (4), fluviosols (4),
#' grisols (3), gypsosols (2), histosols (5), leptismectisols (1),
#' lithosols (1), luvisols (6), nitosols (1) and organosols (4). The 39
#' references of the other 13 GER come in later releases: a profile that
#' shows their reference horizons gets them among the candidates as not
#' implemented yet, and its name is provisional. The horizons of the
#' intertropical chapters (F, OX, RT, ND, FE, OXc, OXm) are read only in the
#' intertropical domain, and the results of those chapters and of the
#' nitosols say that the book calls them a first version (Q18); a fluviosol
#' needs a recorded alluvial origin and is provisional while the position in
#' the valley and the water table are not recorded (Q14). The histosols read
#' their predominant H horizon between 40 and 120 cm, the luvisols their
#' texture contrast by the bands of the chapter, the organosols the S/CEC of
#' their Aho as a whole.
#'
#' The RP is not a key: a solum attaches to one, two or no reference, simply
#' or imperfectly (pp. XIV, XVII). Every reference is tested every time:
#' its sequences of reference horizons, matched on the horizon frame (the
#' RP notation where the profile is written in it, else the horizons
#' identified on the data, \code{identify_horizons(pedon, "rp2008")}), and
#' the side tests of its chapter (depths, thicknesses, forbidden horizons,
#' saturation, carbonates, permafrost, colluvial or anthropic materials).
#' Each test is evaluated strictly and with the tolerances of p. 64 (depth
#' and thickness 10 to 200 cm, pH +/- 0.2, CEC +/- 1, S/CEC +/- 5, organic
#' carbon 8 +/- 2 and 30 +/- 5; any other threshold strict): a reference met
#' only within tolerance, or by an imperfect case its chapter names
#' (calcosol leptique, rendosol pachique, arenosol redoxique ...), is an
#' imperfect attachment, never a simple one. The main name follows the
#' book's explicit statements of precedence, then the degree, then the order
#' of the rapid key (pp. 66-68); a double attachment is written only for the
#' pairs the book prescribes (calcisol-calcosol, the anthroposols
#' transformes); with no reference, the name is "[non rattache]" and the
#' three nearest references are listed. The choice of one name is a
#' convention of soilKey, and \code{$candidates} lists every reference that
#' may attach the solum with its degree and the criteria that fail, pass
#' only within tolerance or are undecided.
#'
#' Names are French, as printed, in the singular (\code{name},
#' \code{levels$name_native}), with an English gloss (\code{levels$name_en});
#' the GER is a label (\code{rsg_or_order}, \code{levels$ger}), not a
#' level. The qualifiers and the types come later. The readings where the
#' text is silent are listed in \code{system.file("rules", "rp2008",
#' "readings.yaml", package = "soilKey")}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rules (\code{load_rules("rp2008")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"rp2008"}: the main name in \code{name} ("[non rattache]" when
#'   nothing attaches), the GER label in \code{rsg_or_order}, the references
#'   in the name in \code{levels} (\code{level} "references", \code{code},
#'   \code{name}, \code{name_native}, \code{name_en}, \code{ger},
#'   \code{attachment}: simple, imperfect, simple_or_imperfect, double),
#'   every reference that may attach in \code{candidates} (with the columns
#'   of \code{ClassificationResult}, \code{group} the GER, plus
#'   \code{name_en}, \code{implemented} and \code{precedence}, the book's
#'   statements that put a reference of the name before it or exclude it),
#'   the evaluation of every
#'   reference, the horizon frame and the rapid key in \code{trace} (with
#'   \code{trace$position}, the depth of the first ND, OXm or OXc of a
#'   ferrallitisol or oxydisol and its proximal or distal type), and
#'   \code{provisional_because} (\code{"references:<code>"}: the references
#'   or GER that could take the name once their data are known).
#' @references Baize D., Girard M.-C. (coord.) (2009). Referentiel
#'   pedologique 2008. AFES, Editions Quae, Versailles.
#' @seealso \code{\link{make_rp2008_example}}, \code{\link{soilkey_systems}},
#'   \code{\link{within_tolerance}}.
#' @examples
#' res <- classify_rp2008(make_rp2008_example("calcosols"), on_missing = "silent")
#' res$name
#' res$candidates[, c("code", "degree", "in_name")]
#' @export
classify_rp2008 <- function(pedon, rules = NULL,
                            on_missing = c("warn", "silent", "error"),
                            gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  if (!inherits(pedon, "PedonRecord"))
    rlang::abort("classify_rp2008(): 'pedon' must be a PedonRecord (see PedonRecord$new()).")
  if (!is.null(pedon$horizons) && !all(c("top_cm", "bottom_cm") %in% names(pedon$horizons))) {
    pedon <- pedon$clone(deep = TRUE)
    pedon$horizons <- ensure_horizon_schema(data.table::as.data.table(pedon$horizons))
  }
  pedon <- .classify_apply_gapfill(pedon, gapfill)
  .classify_require_horizons(pedon, "classify_rp2008")
  rules <- if (is.null(rules)) .rp_rules() else if (is.null(rules$ref_by_code)) .rp_rules_index(rules) else rules
  sys <- .system_get("rp2008")
  frame <- .rp_frame_codes(.rp_identify(pedon))
  view <- .rp_profile(pedon)
  tol <- .tolerance_table(rules$tolerances)

  .rp_with_state(pedon, frame, view, {
    evals <- lapply(rules$references, .rp_eval_reference, pedon = pedon, tol = tol)
    names(evals) <- names(rules$ref_by_code)
    signs <- .rp_signposts(rules, pedon)
    entries <- lapply(names(evals), function(cd) {
      ref <- rules$ref_by_code[[cd]]; d <- evals[[cd]]
      label <- ref$name_singular
      if (!is.null(d$alt$qualifier) && d$degree == "imperfect") label <- paste(label, d$alt$qualifier)
      list(code = cd, name = ref$name_singular, label = label, group = ref$ger, degree = d$degree,
           strict = d$strict, failed = d$failed, imperfect_by = d$imperfect_by,
           undecided_by = d$undecided_by, distance = d$distance)
    })
    entries <- c(entries, unname(lapply(signs, function(s) s[c("code", "name", "label", "group", "degree",
                                                                 "strict", "undecided_by")])))
    codes <- vapply(entries, `[[`, "", "code")
    prec <- .rp_precedence_rows(rules, pedon, codes)
    excl <- .rp_exclusion_rows(rules, pedon, codes)
    comb <- .attach_combine(entries, precedence = prec, order = .rp_order(rules),
                            groups = .rp_group_rows(rules, pedon, codes, evals), exclusions = excl,
                            n_nearest = length(entries), none_label = "[non rattach\u00e9]", sep = "-")
    # the three nearest references (Q3), and the colluviosols beside them
    # when colluvial material is recorded (the review's minor point)
    if (is.na(comb$degree)) {
      cd <- comb$candidates
      near <- which(cd$nearest)
      keep <- near[seq_len(min(3L, length(near)))]
      co <- near[cd$code[near] == "colluviosols"]
      if (length(co) && !(co %in% keep) && isTRUE(colluvial_rp(pedon, min_thickness = 1)$passed)) keep <- c(keep, co)
      drop <- setdiff(near, keep)
      if (length(drop)) comb$candidates <- cd[-drop, , drop = FALSE]
      rownames(comb$candidates) <- NULL
    }
    # the conditions a reference of the name reads only when recorded (the
    # site of the fluviosols, Q14): not recorded, the attachment is
    # provisional; and the position of the hard or nodular horizon of the
    # ferrallitisols and oxydisols (the proximal and distal types, D2)
    cond <- list()
    for (cd in comb$codes) {
      pu <- rules$ref_by_code[[cd]]$provisional_unless
      if (is.null(pu)) next
      v <- evaluate_rsg_tests(pedon, pu)
      if (is.na(v$passed)) cond[[cd]] <- v
    }
    posn <- .rp_positions_of(rules, pedon, comb$codes)
  })

  .rp_result(pedon, rules, sys, evals, signs, comb, frame, on_missing, prec, excl, cond, posn)
}

# The depth of the first hard or nodular horizon of a reference of the name
# (`position` in its file: ND, OXm or OXc within 120 cm) and its type: the
# chapter's proximal (from 0 to 60 cm; p. 45 says 40, the chapter wins, Q17),
# distal (80 to 120 cm), or neither between 60 and 80 cm, the reference kept
# and the depth reported (D2). A row per reference; NULL when none applies.
.rp_positions_of <- function(rules, pedon, codes) {
  rows <- list()
  fr <- .rp_frame(pedon)
  for (cd in codes) {
    ps <- rules$ref_by_code[[cd]]$position
    if (is.null(ps) || !nrow(fr)) next
    m <- .rp_members(fr, unlist(ps$codes))
    top <- .rp_tops(fr)
    k <- which(m %in% TRUE & !is.na(top))
    if (!length(k)) next
    k <- k[which.min(top[k])]
    t <- top[k]
    type <- if (t < 60) "proximal" else if (t < 80) "neither (60-80 cm)" else "distal"
    rows[[length(rows) + 1L]] <- data.frame(code = cd, horizon = fr$code[k], top_cm = t, position = type,
                                            page = "158-162", stringsAsFactors = FALSE)
  }
  if (length(rows)) do.call(rbind, rows) else NULL
}

# The ClassificationResult of an attachment.
.rp_result <- function(pedon, rules, sys, evals, signs, comb, frame, on_missing,
                       prec = list(), excl = list(), cond = list(), posn = NULL) {
  impl <- names(rules$ref_by_code)
  ref_field <- function(cd, f) {
    r <- rules$ref_by_code[[cd]]
    if (!is.null(r)) as.character(r[[f]] %||% NA_character_)
    else if (f %in% c("name_en_singular", "name_singular")) as.character(rules$ger_by_code[[cd]]$name %||% cd)
    else NA_character_
  }
  # the references in the name
  members <- comb$codes
  levels <- NULL
  if (length(members)) {
    cand <- comb$candidates
    lab <- function(cd) {
      d <- evals[[cd]]
      nm <- ref_field(cd, "name_singular")
      if (!is.null(d$alt$qualifier) && d$degree == "imperfect") paste(nm, d$alt$qualifier) else nm
    }
    levels <- data.frame(
      level = "references", code = members,
      name = vapply(members, lab, "", USE.NAMES = FALSE),
      name_native = vapply(members, lab, "", USE.NAMES = FALSE),
      name_en = vapply(members, function(cd) ref_field(cd, "name_en_singular"), "", USE.NAMES = FALSE),
      ger = vapply(members, function(cd) .rp_ger_label(rules, rules$ref_by_code[[cd]]$ger), "", USE.NAMES = FALSE),
      attachment = if (length(members) > 1L) comb$attachment else cand$degree[match(members, cand$code)],
      page = vapply(members, function(cd) ref_field(cd, "page"), "", USE.NAMES = FALSE),
      stringsAsFactors = FALSE, row.names = NULL)
  }
  cand <- comb$candidates
  cand$name_en <- vapply(cand$code, function(cd) ref_field(cd, "name_en_singular"), "", USE.NAMES = FALSE)
  cand$implemented <- cand$code %in% impl
  # why a candidate is not in the name: the explicit statements of the book
  # that put a reference of the name before it, or exclude it
  cand$precedence <- vapply(cand$code, function(cd) {
    p1 <- Filter(function(r) identical(r$second, cd) && isTRUE(r$when) && r$first %in% members, prec)
    e1 <- Filter(function(r) identical(r$code, cd) && isTRUE(r$when) && r$by %in% cand$code[cand$degree %in% .ATTACHED], excl)
    paste(unique(c(vapply(p1, function(r) sprintf("%s first: %s", r$first, r$note), ""),
                   vapply(e1, function(r) sprintf("excluded by %s: %s", r$by, r$note), ""))), collapse = "; ")
  }, "", USE.NAMES = FALSE)

  # the trace: every reference with its degree, the GER signposts, the
  # horizon frame, the rapid key
  steps <- lapply(impl, function(cd) {
    d <- evals[[cd]]
    list(code = cd, name = ref_field(cd, "name_singular"),
         passed = if (d$degree %in% .ATTACHED) TRUE else if (d$degree == "fail") FALSE else NA,
         degree = d$degree, failed = d$failed, imperfect_by = d$imperfect_by,
         undecided_by = d$undecided_by, missing = d$missing,
         imperfect_case = d$alt$qualifier %||% d$alt$criterion,
         evidence = d$evidence)
  })
  steps <- c(steps, unname(lapply(signs, function(s)
    list(code = s$code, name = sprintf("%s (not implemented yet)", s$name), passed = NA,
         degree = "undecided", undecided_by = s$undecided_by, missing = s$missing,
         evidence = s$evidence))))
  attached_ger <- unique(cand$group[cand$degree %in% .ATTACHED])
  rk <- do.call(rbind, lapply(rules$rapid_key, function(e) data.frame(
    entry = e$entry, outcomes = paste(unlist(e$outcomes), collapse = ", "),
    attached = any(unlist(e$outcomes) %in% attached_ger),
    soilkey_added = paste(unlist(e$soilkey_added), collapse = ", "),
    page = e$page %||% NA_character_, stringsAsFactors = FALSE)))
  trace <- list(references = steps, horizons = frame, rapid_key = rk)
  if (!is.null(posn)) trace$position <- posn

  attached <- !is.na(comb$degree)
  main <- if (length(members)) members[1L] else NA_character_
  rsg <- if (!is.na(main)) .rp_ger_label(rules, rules$ref_by_code[[main]]$ger) else NA_character_
  prov <- if (length(comb$provisional_because)) paste0("references:", comb$provisional_because) else character(0)
  # a reference of the name whose conditions are not recorded (Q14)
  prov <- unique(c(prov, if (length(cond)) paste0("references:", names(cond))))
  und <- cand$code[cand$degree %in% c("undecided", "simple_or_imperfect")]
  missing_data <- unique(unlist(c(lapply(evals[intersect(und, impl)], `[[`, "missing"),
                                  lapply(signs[intersect(und, names(signs))], `[[`, "missing"),
                                  lapply(cond, `[[`, "missing"))))
  missing_data <- missing_data[!is.na(missing_data) & nzchar(missing_data)]
  grade <- compute_evidence_grade(pedon, NULL)

  warnings <- character(0)
  if (is.na(grade)) warnings <- c(warnings, .NO_PROPERTIES_WARNING)
  warnings <- c(warnings, paste(
    "The choice of one main name among the references that may attach the solum is a convention of",
    "soilKey (the RP allows several attachments, p. XIV): see $candidates."))
  if (!attached)
    warnings <- c(warnings, sprintf(paste("No reference of the %d tested attaches the solum: %s, with the",
                                          "nearest references in $candidates (an intergrade, p. XVII)."),
                                    length(impl), comb$name))
  if (length(signs))
    warnings <- c(warnings, sprintf(paste("The profile shows the horizons of %s, whose references are not",
                                          "implemented yet: they are in $candidates, undecided."),
                                    paste(vapply(signs, `[[`, "", "name"), collapse = ", ")))
  if (length(comb$provisional_because))
    warnings <- c(warnings, sprintf("Provisional: %d reference(s) or group(s) could take the name once their data are known (%s).",
                                    length(comb$provisional_because), paste(comb$provisional_because, collapse = ", ")))
  for (cd in names(cond))
    warnings <- c(warnings, sprintf(paste("Provisional: the %s attaches on its alluvial origin; the position in the valley",
                                          "and the alluvial water table are not recorded (valley_position, flood_frequency,",
                                          "groundwater_kind; Q14)."), ref_field(cd, "name_singular")))
  # the GER of the name: the first version of the tropical chapters (p. VI,
  # Q18) and what the chapter reports without testing
  for (g in unique(vapply(members, function(cd) rules$ref_by_code[[cd]]$ger, ""))) {
    gg <- rules$ger_by_code[[g]]
    if (isTRUE(gg$first_draft))
      warnings <- c(warnings, sprintf(paste("The chapter of the %s is a first version of the book, to be revised after",
                                            "use (p. VI): the attachment follows it as printed (Q18)."), gg$name))
    if (!is.null(gg$result_note)) warnings <- c(warnings, sprintf("%s: %s.", gg$name, gg$result_note))
  }
  if (!is.null(posn))
    for (k in seq_len(nrow(posn)))
      warnings <- c(warnings, sprintf("%s: the %s starts at %s cm, %s (the type, p. %s; proximal 0-60 cm, distal 80-120 cm, D2).",
                                      ref_field(posn$code[k], "name_singular"), posn$horizon[k], format(posn$top_cm[k]),
                                      if (posn$position[k] == "neither (60-80 cm)") "neither proximal nor distal"
                                      else paste("a", posn$position[k], "type"), posn$page[k]))
  if (any(frame$lower_grade %in% TRUE))
    warnings <- c(warnings, "Some reference horizons were read through soilKey's WRB stand-ins or other substitutes, flagged in $trace$horizons.")
  warnings <- c(warnings, comb$notes)
  if (length(missing_data) > 0L) {
    msg <- sprintf("%d distinct attribute(s) missing for the undecided references -- see $missing_data",
                   length(missing_data))
    if (on_missing == "warn") warnings <- c(warnings, msg)
    else if (on_missing == "error") rlang::abort(msg)
  }

  ClassificationResult$new(
    system = sys$display$result %||% sys$display$en,
    name = comb$name,
    rsg_or_order = rsg,
    qualifiers = list(),
    trace = trace,
    ambiguities = list(),
    missing_data = missing_data,
    evidence_grade = grade,
    prior_check = NULL,
    warnings = warnings,
    system_id = "rp2008",
    levels = levels,
    provisional_because = prov,
    candidates = cand
  )
}


#' Example pedons for the references of the Referentiel pedologique 2008
#'
#' Small, plausible profiles written for the tests and examples, one per
#' reference implemented (the 71 of the first twenty-one GER), in the RP notation
#' (\code{site$designation_system = "rp2008"}) with the properties their
#' tests read. They are not real profiles.
#'
#' @param reference The code of a reference (see
#'   \code{system.file("rules", "rp2008", "key.yaml", package = "soilKey")}):
#'   \code{"alocrisols_typiques"}, \code{"alocrisols_humiques"},
#'   \code{"vitrandosols"}, \code{"silandosols_eutriques"},
#'   \code{"silandosols_dystriques"}, \code{"silandosols_perhydriques"},
#'   \code{"aluandosols_hapliques"}, \code{"aluandosols_perhydriques"},
#'   \code{"anthroposols_transformes"}, \code{"anthroposols_artificiels"},
#'   \code{"anthroposols_reconstitues"}, \code{"anthroposols_construits"},
#'   \code{"anthroposols_archeologiques"}, \code{"arenosols"},
#'   \code{"brunisols_eutriques"}, \code{"brunisols_dystriques"},
#'   \code{"rendosols"}, \code{"calcosols"}, \code{"dolomitosols"},
#'   \code{"rendisols"}, \code{"calcisols"}, \code{"magnesisols"},
#'   \code{"calcarisols"}, \code{"chernosols_hapliques"},
#'   \code{"chernosols_typiques"}, \code{"chernosols_melanoluviques"},
#'   \code{"colluviosols"}, \code{"cryosols_histiques"},
#'   \code{"cryosols_mineraux"}, \code{"ferrallitisols_meubles"},
#'   \code{"ferrallitisols_nodulaires"}, \code{"ferrallitisols_petroxydiques"},
#'   \code{"ferrallitisols_duroxydiques"}, \code{"oxydisols_meubles"},
#'   \code{"oxydisols_nodulaires"}, \code{"oxydisols_petroxydiques"},
#'   \code{"ferruginosols_meubles"}, \code{"ferruginosols_semiluviques"},
#'   \code{"ferruginosols_luviques"}, \code{"ferruginosols_petroxydiques"},
#'   \code{"fersialsols_carbonates"}, \code{"fersialsols_calciques"},
#'   \code{"fersialsols_insatures"}, \code{"fersialsols_eluviques"},
#'   \code{"fluviosols_bruts"}, \code{"fluviosols_juveniles"},
#'   \code{"fluviosols_typiques"}, \code{"fluviosols_brunifies"},
#'   \code{"grisols_eluviques"}, \code{"grisols_degrades"},
#'   \code{"grisols_hapliques"}, \code{"gypsosols_hapliques"},
#'   \code{"gypsosols_petrogypsiques"}, \code{"histosols_fibriques"},
#'   \code{"histosols_mesiques"}, \code{"histosols_sapriques"},
#'   \code{"histosols_composites"}, \code{"histosols_leptiques"},
#'   \code{"leptismectisols"}, \code{"lithosols"}, \code{"neoluvisols"},
#'   \code{"luvisols_typiques"}, \code{"luvisols_degrades"},
#'   \code{"luvisols_derniques"}, \code{"luvisols_tronques"},
#'   \code{"quasi_luvisols"}, \code{"nitosols"},
#'   \code{"organosols_holorganiques"}, \code{"organosols_calcaires"},
#'   \code{"organosols_satures"}, \code{"organosols_insatures"}.
#' @return A \code{\link{PedonRecord}}.
#' @examples
#' classify_rp2008(make_rp2008_example("brunisols_eutriques"), on_missing = "silent")$name
#' @export
make_rp2008_example <- function(reference = c(
    "alocrisols_typiques", "alocrisols_humiques", "vitrandosols", "silandosols_eutriques",
    "silandosols_dystriques", "silandosols_perhydriques", "aluandosols_hapliques",
    "aluandosols_perhydriques", "anthroposols_transformes", "anthroposols_artificiels",
    "anthroposols_reconstitues", "anthroposols_construits", "anthroposols_archeologiques",
    "arenosols", "brunisols_eutriques", "brunisols_dystriques", "rendosols", "calcosols",
    "dolomitosols", "rendisols", "calcisols", "magnesisols", "calcarisols",
    "chernosols_hapliques", "chernosols_typiques", "chernosols_melanoluviques",
    "colluviosols", "cryosols_histiques", "cryosols_mineraux",
    "ferrallitisols_meubles", "ferrallitisols_nodulaires", "ferrallitisols_petroxydiques",
    "ferrallitisols_duroxydiques", "oxydisols_meubles", "oxydisols_nodulaires", "oxydisols_petroxydiques",
    "ferruginosols_meubles", "ferruginosols_semiluviques", "ferruginosols_luviques",
    "ferruginosols_petroxydiques", "fersialsols_carbonates", "fersialsols_calciques",
    "fersialsols_insatures", "fersialsols_eluviques", "fluviosols_bruts", "fluviosols_juveniles",
    "fluviosols_typiques", "fluviosols_brunifies", "grisols_eluviques", "grisols_degrades",
    "grisols_hapliques", "gypsosols_hapliques", "gypsosols_petrogypsiques",
    "histosols_fibriques", "histosols_mesiques", "histosols_sapriques", "histosols_composites", "histosols_leptiques",
    "leptismectisols", "lithosols", "neoluvisols", "luvisols_typiques", "luvisols_degrades", "luvisols_derniques",
    "luvisols_tronques", "quasi_luvisols", "nitosols", "organosols_holorganiques", "organosols_calcaires",
    "organosols_satures", "organosols_insatures")) {
  reference <- match.arg(reference)
  mk <- function(h, site = list()) {
    n <- max(lengths(h))
    h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
    base <- list(id = paste0("rp2008-", reference), country = "FR", designation_system = "rp2008",
                 drainage_class = "well drained")
    PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
      data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  }
  # an acid andosol profile and a saturated one, shared by their references
  andic <- function(des, water = c(60, 60, 40), bs = c(30, 25, 20), sb = c(8, 6, 4)) mk(list(
    designation = des, top_cm = c(0, 45, 90), bottom_cm = c(45, 90, 130),
    oc_pct = c(8, 2, 0.5), clay_pct = c(25, 22, 15), al_ox_pct = c(4, 4.5, 1), fe_ox_pct = c(1.5, 1.6, 0.5),
    si_ox_pct = c(1.2, 1.4, 0.3), bulk_density_g_cm3 = c(0.6, 0.7, 1.1), phosphate_retention_pct = c(92, 95, 40),
    water_content_1500kpa_undried = water, bs_pct = bs, cec_cmol = c(30, 25, 15),
    ca_cmol = sb * 0.7, mg_cmol = sb * 0.2, k_cmol = sb * 0.07, na_cmol = sb * 0.03,
    ph_h2o = c(5.4, 5.6, 5.8), munsell_hue_moist = "10YR", munsell_value_moist = c(2, 4, 5),
    munsell_chroma_moist = c(2, 4, 4)), list(parent_material = "volcanic ash", country = "FR"))
  # the profiles of part 5 (v0.9.348): a horizon frame with the values the
  # tests read, in their regions (the tropical ones between the tropics)
  lay <- function(des, top, bot, ..., site = list(), base_site = list()) {
    h <- c(list(designation = des, top_cm = top, bottom_cm = bot), list(...))
    mk(h, utils::modifyList(base_site, site))
  }
  trop <- function(des, top, bot, ...) lay(des, top, bot, oc_pct = c(2.5, rep(0.4, length(des) - 1L)),
    clay_pct = c(25, rep(45, length(des) - 1L)), ph_h2o = c(5.0, rep(5.2, length(des) - 1L)), ...,
    base_site = list(country = "CI", lat = 6.8, lon = -5.3, koppen_climate = "Aw", parent_material = "granite"))
  oxy <- function(des, top, bot, ...) lay(des, top, bot, oc_pct = c(2, rep(0.3, length(des) - 1L)),
    ph_h2o = c(5.8, rep(6.0, length(des) - 1L)), ph_kcl = c(5.6, rep(6.2, length(des) - 1L)), ...,
    base_site = list(country = "NC", lat = -21.6, lon = 166.4, koppen_climate = "Aw", parent_material = "peridotite"))
  sahel <- function(des, top, bot, ...) {
    a <- list(...)
    n <- length(des)
    if (is.null(a$clay_pct)) a$clay_pct <- c(10, rep(20, n - 1L))
    do.call(lay, c(list(des, top, bot, oc_pct = c(0.6, rep(0.2, n - 1L)), ph_h2o = c(6.2, rep(6.2, n - 1L))), a,
                   list(base_site = list(country = "BF", lat = 11.2, lon = -4.3, koppen_climate = "Aw",
                                         parent_material = "sandstone"))))
  }
  med <- function(des, top, bot, ...) lay(des, top, bot, oc_pct = c(2.5, rep(0.5, length(des) - 1L)),
    munsell_hue_moist = c("5YR", rep("2.5YR", length(des) - 1L)), munsell_value_moist = c(3, rep(4, length(des) - 1L)),
    munsell_chroma_moist = c(3, rep(6, length(des) - 1L)), structure_type = c("granular", rep("angular blocky", length(des) - 1L)),
    structure_grade = c("moderate", rep("strong", length(des) - 1L)), ...,
    base_site = list(country = "FR", lat = 43.5, koppen_climate = "Csa", parent_material = "limestone"))
  flu <- function(des, top, bot, ..., site = list()) lay(des, top, bot,
    layer_origin = "fluvic", ..., site = site,
    base_site = list(country = "FR", lat = 47.4, koppen_climate = "Cfb", parent_material = "Loire alluvium",
                     valley_position = "major bed", flood_frequency = "occasional"))
  gri <- function(des, top = c(0, 25, 40, 90, 130), bot = c(25, 40, 90, 130, 170), clay_pct = c(20, 18, 32, 30, 24),
                  oc_pct = c(3.5, 1.2, 0.9, 0.4, 0.2), bs_pct = c(70, 75, 80, 85, 90)) lay(des, top, bot,
    clay_pct = clay_pct, oc_pct = oc_pct, bs_pct = bs_pct, ph_h2o = c(6.0, rep(6.2, length(des) - 1L)), caco3_pct = 0,
    munsell_hue_moist = "10YR", munsell_value_moist = c(3, rep(4, length(des) - 1L)),
    munsell_chroma_moist = c(2, rep(2, length(des) - 1L)),
    base_site = list(country = "RO", lat = 46.6, koppen_climate = "Dfb", land_use = "deciduous forest", parent_material = "loess"))
  gyp <- function(des, top, bot, ...) lay(des, top, bot, oc_pct = c(0.4, rep(0.2, length(des) - 1L)), ...,
    base_site = list(country = "TN", lat = 33.9, lon = 9.5, koppen_climate = "BWh", parent_material = "gypseous marl"))
  # the profiles of part 6 (v0.9.349): peats of the French plains and
  # mountains, the Beauce, the loess of the Paris basin, the Antilles, the Jura
  # and the Vosges
  peat <- function(des, top, bot, fib, ...) lay(des, top, bot, oc_pct = ifelse(grepl("^H", des), 45, NA),
    loi_pct = ifelse(grepl("^H", des), 85, NA), fiber_content_rubbed_pct = fib, ...,
    base_site = list(country = "FR", lat = 47.2, lon = 6.3, elevation_m = 850, koppen_climate = "Cfb",
                     drainage_class = "very poorly drained", parent_material = "peat"))
  loess <- function(des, top, bot, clay, ..., site = list()) lay(des, top, bot, clay_pct = clay,
    oc_pct = c(2.0, rep(0.4, length(des) - 1L)), ph_h2o = c(5.8, rep(6.2, length(des) - 1L)), caco3_pct = 0, ...,
    site = site, base_site = list(country = "FR", lat = 48.8, lon = 2.9, koppen_climate = "Cfb",
                                  land_use = "deciduous forest", parent_material = "loess"))
  mont <- function(des, top, bot, ..., site = list()) lay(des, top, bot, ..., site = site,
    base_site = list(country = "FR", lat = 46.6, lon = 6.1, elevation_m = 1300, koppen_climate = "Dfb",
                     land_use = "mountain pasture"))
  switch(reference,
    alocrisols_typiques = mk(list(
      designation = c("A", "Sal", "C"), top_cm = c(0, 8, 55), bottom_cm = c(8, 55, 100),
      oc_pct = c(5, 1.2, 0.3), clay_pct = c(18, 20, 12), ph_h2o = c(4.4, 4.6, 4.8), al_kcl_cmol = c(5, 4.5, 3),
      cec_cmol = c(14, 11, 8), bs_pct = c(15, 12, 15), ca_cmol = c(1, 0.7, 0.6), mg_cmol = c(0.6, 0.4, 0.4),
      k_cmol = 0.2, na_cmol = 0.05, munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 6),
      munsell_chroma_moist = c(2, 6, 4)), list(parent_material = "granite")),
    alocrisols_humiques = mk(list(
      designation = c("Aho", "Salh", "Sal", "C"), top_cm = c(0, 15, 45, 80), bottom_cm = c(15, 45, 80, 120),
      oc_pct = c(12, 3, 1.5, 0.3), clay_pct = c(16, 18, 18, 12), ph_h2o = c(4.2, 4.5, 4.7, 4.9),
      al_kcl_cmol = c(6, 5, 4, 3), cec_cmol = c(20, 13, 10, 7), bs_pct = c(10, 10, 12, 15),
      munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 5, 6), munsell_chroma_moist = c(2, 3, 5, 4)),
      list(parent_material = "schist")),
    vitrandosols = mk(list(
      designation = c("Avi", "C"), top_cm = c(0, 40), bottom_cm = c(40, 100), oc_pct = c(2, 0.3),
      clay_pct = c(8, 5), sand_pct = c(55, 60), bulk_density_g_cm3 = c(1.0, 1.2), phosphate_retention_pct = c(40, 20),
      volcanic_glass_pct = c(40, 50), cec_cmol = c(12, 6), bs_pct = c(60, 70), ph_h2o = c(6.2, 6.5)),
      list(parent_material = "volcanic ash")),
    silandosols_eutriques = andic(c("And", "Snd", "C"), bs = c(65, 70, 70), sb = c(20, 18, 10)),
    silandosols_dystriques = andic(c("And", "Snd", "C")),
    silandosols_perhydriques = andic(c("And", "Snd", "C"), water = c(80, 130, 40)),
    aluandosols_hapliques = andic(c("Alu", "Slu", "C")),
    aluandosols_perhydriques = andic(c("Alu", "Slu", "C"), water = c(110, 140, 40)),
    anthroposols_transformes = mk(list(
      designation = c("LA", "S", "C"), top_cm = c(0, 35, 75), bottom_cm = c(35, 75, 110),
      oc_pct = c(4, 0.8, 0.3), clay_pct = c(15, 18, 14), sand_pct = c(55, 50, 60), ph_h2o = 6.8,
      cec_cmol = c(14, 10, 8), bs_pct = c(85, 80, 80)),
      list(rp_anthropic_history = "d\u00e9foncement; amendements organiques", mechanical_mixing_depth_cm = 60,
           land_use = "market gardening")),
    anthroposols_artificiels = mk(list(
      designation = c("JZ", "Z", "II C"), top_cm = c(0, 10, 70), bottom_cm = c(10, 70, 110),
      oc_pct = c(2, 0.5, 0.3), artefacts_pct = c(60, 75, 0), clay_pct = c(15, 12, 20), ph_h2o = 7.8),
      list(land_use = "industrial")),
    anthroposols_reconstitues = mk(list(
      designation = c("Ltp", "II C"), top_cm = c(0, 60), bottom_cm = c(60, 100),
      oc_pct = c(3, 0.4), clay_pct = c(20, 25), sand_pct = c(45, 40), ph_h2o = 7.0,
      anthropic_material = c("transported", NA)), list(land_use = "urban park")),
    anthroposols_construits = mk(list(
      designation = c("JZtc", "Ztc"), top_cm = c(0, 15), bottom_cm = c(15, 80),
      oc_pct = c(3, 1), anthropic_material = "technological", ph_h2o = 7.6),
      list(rp_anthropic_history = "construction (by-products of a paper mill)", land_use = "urban")),
    anthroposols_archeologiques = mk(list(
      designation = c("Zar", "II S", "II C"), top_cm = c(0, 60, 90), bottom_cm = c(60, 90, 120),
      oc_pct = c(2.5, 0.6, 0.3), anthropic_material = c("archaeological", NA, NA),
      anthropic_material_pct = c(35, NA, NA), clay_pct = c(20, 25, 20), ph_h2o = 7.2,
      bs_pct = c(90, 85, 85), cec_cmol = c(16, 14, 12))),
    arenosols = mk(list(
      designation = c("A", "C"), top_cm = c(0, 20), bottom_cm = c(20, 150), oc_pct = c(1.2, 0.1),
      sand_pct = c(88, 93), clay_pct = c(4, 3), silt_pct = c(8, 4), ph_h2o = c(5.0, 5.4),
      structure_type = c("granular", "single grain"), redoximorphic_features_pct = 0),
      list(parent_material = "aeolian sand")),
    brunisols_eutriques = mk(list(
      designation = c("A", "S", "C"), top_cm = c(0, 20, 70), bottom_cm = c(20, 70, 110),
      oc_pct = c(3, 0.8, 0.3), clay_pct = c(22, 25, 20), ph_h2o = c(6.2, 6.4, 6.6),
      cec_cmol = c(16, 13, 10), bs_pct = c(70, 75, 80), ca_cmol = c(9, 8, 6), mg_cmol = c(1.5, 1.4, 1.2),
      munsell_hue_moist = "10YR", munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(2, 4, 4)),
      list(parent_material = "loess")),
    brunisols_dystriques = mk(list(
      designation = c("A", "S", "C"), top_cm = c(0, 15, 65), bottom_cm = c(15, 65, 100),
      oc_pct = c(4, 1, 0.3), clay_pct = c(18, 20, 15), ph_h2o = c(5.2, 5.4, 5.6), al_kcl_cmol = c(1.5, 1.2, 1),
      cec_cmol = c(14, 10, 8), bs_pct = c(35, 30, 30)), list(parent_material = "sandstone")),
    rendosols = mk(list(
      designation = c("Aca", "Rca"), top_cm = c(0, 25), bottom_cm = c(25, 60),
      oc_pct = c(4, NA), caco3_pct = c(30, NA), clay_pct = c(30, NA), ph_h2o = c(8.1, NA)),
      list(parent_material = "limestone")),
    calcosols = mk(list(
      designation = c("Aca", "Sca", "Cca"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
      oc_pct = c(3, 0.8, 0.3), caco3_pct = c(15, 25, 40), clay_pct = c(30, 32, 25), ph_h2o = c(8.0, 8.2, 8.3)),
      list(parent_material = "marl")),
    dolomitosols = mk(list(
      designation = c("Ado", "Sdo", "Rdo"), top_cm = c(0, 20, 55), bottom_cm = c(20, 55, 80),
      oc_pct = c(3.5, 1, NA), caco3_pct = c(8, 10, NA), mgco3_pct = c(9, 11, NA), clay_pct = c(25, 28, NA),
      ph_h2o = c(7.9, 8.1, NA)), list(parent_material = "dolomite")),
    rendisols = mk(list(
      designation = c("Aci", "Rca"), top_cm = c(0, 25), bottom_cm = c(25, 60),
      oc_pct = c(5, NA), clay_pct = c(35, NA), bs_pct = c(95, NA), cec_cmol = c(30, NA),
      ca_cmol = c(25, NA), mg_cmol = c(2, NA), ph_h2o = c(7.2, NA)), list(parent_material = "limestone")),
    calcisols = mk(list(
      designation = c("Aci", "Sci", "Cca"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
      oc_pct = c(3, 0.9, 0.3), clay_pct = c(28, 32, 25), bs_pct = c(95, 98, 100), cec_cmol = c(22, 20, 15),
      ca_cmol = c(18, 17, 13), mg_cmol = c(2, 1.8, 1.5), ph_h2o = c(7.0, 7.3, 8.1), caco3_pct = c(0, 0, 25)),
      list(parent_material = "limestone")),
    magnesisols = mk(list(
      designation = c("Amg", "Smg", "C"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
      oc_pct = c(3, 0.8, 0.2), clay_pct = c(30, 35, 30), bs_pct = c(90, 95, 95), cec_cmol = c(25, 24, 20),
      ca_cmol = c(10, 9, 7), mg_cmol = c(11, 12, 11), ph_h2o = c(7.0, 7.2, 7.4)),
      list(parent_material = "serpentinite")),
    calcarisols = mk(list(
      designation = c("Aca", "Km", "C"), top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 90),
      oc_pct = c(2.5, NA, 0.2), caco3_pct = c(35, 80, 30), clay_pct = c(25, NA, 20), ph_h2o = c(8.2, NA, 8.4),
      cementation_class = c(NA, "indurated", NA)), list(parent_material = "limestone")),
    chernosols_hapliques = mk(list(
      designation = c("LAch", "Ach", "Ck"), top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 120),
      oc_pct = c(3.5, 2, 0.4), clay_pct = c(30, 30, 25), caco3_pct = c(1, 3, 12), bs_pct = c(98, 100, 100),
      cec_cmol = c(30, 28, 20), ph_h2o = c(7.4, 7.8, 8.2), munsell_hue_moist = "10YR",
      munsell_value_moist = c(2, 3, 5), munsell_chroma_moist = c(1, 2, 3)), list(country = "BG")),
    chernosols_typiques = mk(list(
      designation = c("Ach", "Sh", "Ck"), top_cm = c(0, 50, 90), bottom_cm = c(50, 90, 140),
      oc_pct = c(3.5, 1, 0.3), clay_pct = c(30, 32, 28), bs_pct = c(90, 92, 100), cec_cmol = c(30, 26, 20),
      ph_h2o = c(7.0, 7.3, 8.1), munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 5),
      munsell_chroma_moist = c(1, 2, 3)), list(country = "BG")),
    chernosols_melanoluviques = mk(list(
      designation = c("Ach", "BTh", "Ck"), top_cm = c(0, 45, 110), bottom_cm = c(45, 110, 160),
      oc_pct = c(3, 0.8, 0.3), clay_pct = c(25, 45, 30), bs_pct = c(82, 85, 100), cec_cmol = c(28, 30, 20),
      ph_h2o = c(6.6, 6.8, 8.0), clay_films_amount = c("none", "many", "none"),
      munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 5), munsell_chroma_moist = c(1, 2, 3)),
      list(country = "BG")),
    colluviosols = mk(list(
      designation = c("A", "Jp", "C"), top_cm = c(0, 25, 75), bottom_cm = c(25, 75, 120),
      oc_pct = c(2.5, 1, 0.4), clay_pct = c(28, 30, 25), sand_pct = c(30, 28, 35), ph_h2o = 7.0,
      layer_origin = c("colluvic", "colluvic", NA)), list(landform_element = "footslope")),
    cryosols_histiques = mk(list(
      designation = c("OL", "Hf", "H*"), top_cm = c(0, 3, 40), bottom_cm = c(3, 40, 90),
      oc_pct = c(45, 42, 40), fiber_content_rubbed_pct = c(NA, 60, 30)),
      list(frost_regime = "permafrost", drainage_class = "very poorly drained", country = "NO")),
    cryosols_mineraux = mk(list(
      designation = c("Hcry", "Jscry", "C*"), top_cm = c(0, 8, 40), bottom_cm = c(8, 40, 100),
      oc_pct = c(25, 1.5, 0.4), clay_pct = c(NA, 15, 12), cryoturbation = c("present", "present", NA)),
      list(frost_regime = "permafrost", country = "NO")),
    ferrallitisols_meubles = trop(c("A", "F", "RT", "C"), c(0, 15, 120, 200), c(15, 120, 200, 260)),
    ferrallitisols_nodulaires = trop(c("A", "ND", "F", "RT"), c(0, 10, 45, 130), c(10, 45, 130, 200),
                                     nodules_wt_pct = c(NA, 70, NA, NA)),
    ferrallitisols_petroxydiques = trop(c("A", "F", "OXm", "RT"), c(0, 15, 90, 130), c(15, 90, 130, 200),
                                        cementation_class = c(NA, NA, "indurated", NA)),
    ferrallitisols_duroxydiques = trop(c("A", "OXc", "RT"), c(0, 10, 50), c(10, 50, 150),
                                       cementation_class = c(NA, "moderately", NA)),
    oxydisols_meubles = oxy(c("A", "OX", "C"), c(0, 10, 150), c(10, 150, 200)),
    oxydisols_nodulaires = oxy(c("A", "OX", "ND", "C"), c(0, 10, 90, 115), c(10, 90, 115, 200),
                               nodules_wt_pct = c(NA, NA, 75, NA)),
    oxydisols_petroxydiques = oxy(c("A", "OXm", "OX"), c(0, 10, 40), c(10, 40, 150),
                                  cementation_class = c(NA, "indurated", NA)),
    ferruginosols_meubles = sahel(c("A", "FE", "S", "C"), c(0, 15, 90, 160), c(15, 90, 160, 220)),
    ferruginosols_semiluviques = sahel(c("A", "FE", "BT", "C", "R"), c(0, 15, 60, 130, 180), c(15, 60, 130, 180, 200),
                                       clay_films_amount = c(NA, "few", "common", NA, NA)),
    ferruginosols_luviques = sahel(c("A", "FE", "Ea", "BT", "II C"), c(0, 12, 50, 80, 140), c(12, 50, 80, 140, 200),
                                   clay_pct = c(10, 18, 8, 30, 20), clay_films_amount = c(NA, NA, NA, "common", NA)),
    ferruginosols_petroxydiques = sahel(c("A", "OXm", "Ea", "BTg", "C"), c(0, 10, 60, 90, 140), c(10, 60, 90, 140, 200),
                                        cementation_class = c(NA, "indurated", NA, NA, NA), clay_pct = c(8, NA, 6, 30, 20)),
    fersialsols_carbonates = med(c("Aca", "FS", "R"), c(0, 20, 70), c(20, 70, 100),
                                 caco3_pct = c(12, 8, NA), effervescence_class = c("strong", "strong", NA),
                                 bs_pct = c(100, 100, NA), ca_cmol = c(25, 24, NA), mg_cmol = c(1.5, 1.5, NA),
                                 ph_h2o = c(8.1, 8.2, NA)),
    fersialsols_calciques = med(c("Aci", "FS", "R"), c(0, 15, 70), c(15, 70, 100),
                                caco3_pct = c(0, 0, NA), bs_pct = c(92, 95, NA), ca_cmol = c(20, 22, NA),
                                mg_cmol = c(2.5, 2.8, NA), ph_h2o = c(7.0, 7.2, NA)),
    fersialsols_insatures = med(c("A", "FS", "C"), c(0, 15, 80), c(15, 80, 120),
                                caco3_pct = 0, bs_pct = c(55, 60, 65), ca_cmol = c(6, 8, 7), mg_cmol = c(2, 2.5, 2.5),
                                ph_h2o = c(5.8, 6.0, 6.2)),
    fersialsols_eluviques = med(c("A", "E", "FSt", "C"), c(0, 10, 30, 90), c(10, 30, 90, 130),
                                clay_pct = c(16, 15, 42, 30), caco3_pct = 0, bs_pct = c(70, 65, 75, 80),
                                ca_cmol = c(8, 6, 14, 12), mg_cmol = c(2, 1.5, 3, 3), ph_h2o = c(6.2, 6.0, 6.4, 6.6),
                                clay_films_amount = c(NA, "none", "common", NA)),
    fluviosols_bruts = flu(c("M", "D"), c(0, 30), c(30, 100), oc_pct = c(0.1, NA), clay_pct = c(3, NA),
                           sand_pct = c(85, NA), site = list(valley_position = "minor bed", flood_frequency = "frequent")),
    fluviosols_juveniles = flu(c("Js", "D"), c(0, 20), c(20, 100), oc_pct = c(0.4, NA), clay_pct = c(8, NA),
                               sand_pct = c(60, NA)),
    fluviosols_typiques = flu(c("A", "Jp", "D"), c(0, 20, 60), c(20, 60, 120), oc_pct = c(2.2, 0.2, NA),
                              clay_pct = c(20, 18, NA), sand_pct = c(35, 40, NA)),
    fluviosols_brunifies = flu(c("A", "S", "M", "D"), c(0, 25, 80, 130), c(25, 80, 130, 160),
                               oc_pct = c(3, 1.5, 0.4, NA), clay_pct = c(30, 32, 25, NA), sand_pct = c(20, 18, 30, NA),
                               bs_pct = c(85, 88, 90, NA), ph_h2o = c(7.0, 7.2, 7.4, NA)),
    grisols_eluviques = gri(c("Aso", "Eh", "BTh", "BT", "C")),
    grisols_degrades = gri(c("Aso", "Eh", "BThd", "BT", "C")),
    grisols_hapliques = gri(c("Aso", "BThd", "BT", "C"), top = c(0, 25, 80, 120), bot = c(25, 80, 120, 160),
                            clay_pct = c(24, 28, 30, 24), oc_pct = c(3.5, 0.9, 0.4, 0.2), bs_pct = c(70, 80, 85, 90)),
    gypsosols_hapliques = gyp(c("Ys", "Cy", "My"), c(0, 35, 80), c(35, 80, 120),
                              caso4_pct = c(60, 20, 15), caco3_pct = c(8, 10, 12)),
    gypsosols_petrogypsiques = gyp(c("Ym", "Ys", "Cy", "My"), c(0, 10, 40, 90), c(10, 40, 90, 130),
                                   caso4_pct = c(70, 60, 20, 15), caco3_pct = c(6, 8, 10, 12),
                                   cementation_class = c("indurated", NA, NA, NA), roots_abundance = c("none", "many", "few", NA)),
    histosols_fibriques = peat(c("Hf", "Hm", "M"), c(0, 90, 110), c(90, 110, 150), fib = c(60, 25, NA)),
    histosols_mesiques = peat(c("Hm", "Hs", "D"), c(0, 100, 130), c(100, 130, 160), fib = c(25, 6, NA)),
    histosols_sapriques = peat(c("Hs", "Hm", "G"), c(0, 90, 110), c(90, 110, 150), fib = c(5, 20, NA)),
    histosols_composites = peat(c("Hf", "Hm", "Hs", "M"), c(0, 40, 80, 120), c(40, 80, 120, 150), fib = c(55, 20, 5, NA)),
    histosols_leptiques = peat(c("Hs", "R"), c(0, 35), c(35, 80), fib = c(6, NA)),
    leptismectisols = lay(c("Av", "SV", "R"), c(0, 18, 40), c(18, 40, 80), clay_pct = c(55, 58, NA),
                          oc_pct = c(2.2, 1.0, NA), cec_cmol = c(42, 44, NA), ca_cmol = c(38, 40, NA), mg_cmol = c(2.5, 2.5, NA),
                          bs_pct = c(98, 98, NA), ph_h2o = c(7.6, 7.8, NA), slickensides = c("few", "few", NA),
                          clay_mineralogy = c("smectite", "smectite", NA),
                          base_site = list(country = "FR", lat = 48.3, lon = 2.0, koppen_climate = "Cfb",
                                           parent_material = "Etampes limestone")),
    lithosols = lay(c("A", "R"), c(0, 6), c(6, 60), oc_pct = c(4, NA), clay_pct = c(25, NA),
                    base_site = list(country = "FR", lat = 44.3, lon = 3.2, koppen_climate = "Cfb",
                                     parent_material = "hard limestone")),
    neoluvisols = loess(c("A", "E", "BT", "C"), c(0, 20, 45, 100), c(20, 45, 100, 150), clay = c(15, 17, 26, 22),
                        clay_films_amount = c(NA, "few", "common", NA)),
    luvisols_typiques = loess(c("A", "E", "BT", "C"), c(0, 15, 50, 110), c(15, 50, 110, 150), clay = c(14, 13, 28, 22),
                              clay_films_amount = c(NA, NA, "many", NA)),
    luvisols_degrades = loess(c("A", "E", "BTgd", "C"), c(0, 8, 55, 100), c(8, 55, 100, 150), clay = c(13, 12, 30, 24),
                              clay_films_amount = c(NA, NA, "many", NA), degradation_form = c(NA, NA, "tongues", NA)),
    luvisols_derniques = loess(c("A", "E", "Ea", "BTd", "C"), c(0, 10, 25, 40, 90), c(10, 25, 40, 90, 140),
                               clay = c(13, 12, 10, 32, 24), clay_films_amount = c(NA, NA, NA, "many", NA),
                               site = list(country = "PL", lat = 52.2, lon = 21.0, koppen_climate = "Dfb")),
    luvisols_tronques = loess(c("A", "BT", "C"), c(0, 10, 80), c(10, 80, 130), clay = c(25, 30, 22),
                              clay_films_amount = c(NA, "many", NA)),
    quasi_luvisols = loess(c("LE", "II BT", "II C"), c(0, 35, 90), c(35, 90, 140), clay = c(16, 30, 24),
                           clay_films_amount = c(NA, "many", NA), site = list(land_use = "cropland")),
    nitosols = lay(c("A", "Sn", "C"), c(0, 20, 120), c(20, 120, 200), clay_pct = c(45, 65, 40), oc_pct = c(4, 0.9, 0.3),
                   halloysite_pct = c(NA, 70, NA), pressure_faces = c(NA, "common", NA), clay_films_amount = c(NA, "few", NA),
                   cec_cmol = c(25, 18, 12), ph_h2o = c(5.6, 5.8, 6.0),
                   base_site = list(country = "GP", lat = 16.2, lon = -61.6, koppen_climate = "Af",
                                    parent_material = "andesitic pyroclastic materials")),
    organosols_holorganiques = mont(c("OL", "OF", "OHta", "R"), c(0, 3, 13, 38), c(3, 13, 38, 80), oc_pct = c(45, 40, 35, NA),
                                    ph_h2o = c(6.0, 6.2, 6.5, NA), site = list(parent_material = "hard limestone")),
    organosols_calcaires = mont(c("OF", "OH", "Acaho", "C"), c(0, 8, 15, 45), c(8, 15, 45, 80), oc_pct = c(40, 35, 12, 1),
                                caco3_pct = c(NA, NA, 15, 60), effervescence_class = c(NA, NA, "strong", "strong"),
                                ph_h2o = c(7.0, 7.2, 7.8, 8.2), site = list(parent_material = "limestone scree")),
    organosols_satures = mont(c("OF", "Aho", "C"), c(0, 5, 40), c(5, 40, 80), oc_pct = c(40, 10, 1), caco3_pct = c(NA, 0, 0),
                              bs_pct = c(NA, 90, 95), ph_h2o = c(6.2, 6.5, 6.8), clay_pct = c(NA, 35, 30),
                              site = list(parent_material = "marl")),
    organosols_insatures = mont(c("OF", "OH", "Aho", "C"), c(0, 5, 12, 45), c(5, 12, 45, 90), oc_pct = c(42, 35, 12, 1),
                                bs_pct = c(NA, NA, 35, 40), ph_h2o = c(4.4, 4.5, 4.8, 5.0), clay_pct = c(NA, NA, 18, 12),
                                site = list(country = "FR", lat = 48.0, lon = 7.0, elevation_m = 1200, parent_material = "gneiss",
                                            drainage_class = "well drained"))
  )
}


# The rapid-key check: every GER of the catalogue with the entries of the
# rapid key that reach it, and whether only soilKey's placement reaches it
# (the grisols, in 7c). A GER no entry reaches would be flagged here.
.rp_rapid_key_check <- function(rules = .rp_rules()) {
  rows <- lapply(rules$ger, function(g) {
    by <- Filter(function(e) g$code %in% as.character(unlist(e$outcomes)), rules$rapid_key)
    book <- Filter(function(e) !(g$code %in% as.character(unlist(e$soilkey_added))), by)
    data.frame(ger = g$code, entries = paste(vapply(by, function(e) e$entry, ""), collapse = ", "),
               reached = length(by) > 0L, by_the_book = length(book) > 0L,
               implemented = isTRUE(g$implemented), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
