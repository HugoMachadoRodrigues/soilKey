# =============================================================================
# v0.9.346 -- the tests of the references of the French Referentiel
# pedologique 2008 (AFES; Baize and Girard coord.; Quae, 2009): the
# predicates *_rp the reference files inst/rules/rp2008/references/*.yaml
# are written with.
#
# They read the horizon frame of the RP (.rp_identify(): the RP notation
# where written, else the data) and the recorded data. Every one is
# three-valued and reads its printed thresholds through the tolerant helpers
# of R/tolerance.R (.rule_threshold(), .rule_check(), .rule_distance()), so
# that the tolerances of p. 64 (tolerances.yaml) apply in the tolerant pass
# of .evaluate_degree() and only there (Hugo's decisions Q4, Q5 and default
# 9). The sequences of the book are matched with match_horizon_sequence();
# depths, thicknesses and forbidden horizons are side tests (default 13):
#   sequence_rp           the reference's sequences of horizons (open slots)
#   horizon_within_rp     a horizon starting within a depth (or a window)
#   no_horizon_within_rp  no such horizon within a depth
#   thickness_rp          the thickness of a set of horizons (from the
#                         surface, or in all)
#   thickness_ratio_rp    the ratio of two thicknesses ([Aci + Sci] / Sca)
#   attribute_rp          a value of the horizons of a set, or of the
#                         mineral layers of a window
#   attribute_thickness_rp the thickness of the layers whose value meets a
#                         threshold (perhydric: 35 cm within 100 cm)
#   coarse_rp, idt_rp, carbonate_reserve_rp, permafrost_rp, ice_layer_rp,
#   cryoturbation_rp, colluvial_rp, anthropic_material_rp,
#   transported_material_rp, anthropic_history_rp, origin_rp, recorded_rp
#                         the tests of the chapters
#
# Horizon codes in the files: a code as printed is the code with its family
# when it heads one ("A" is every A horizon, "S" every structural horizon,
# "Sca" only Sca); lower-case letters after a code are features it must
# carry ("Ck": a C with secondary carbonates); "-g" is a feature on any code;
# "Sa+" and "Na+" the overlays of the salic and sodic horizons; "S=" the
# code alone, not its family. The L of a ploughed horizon does not change
# its code in the sequences (LAca is an Aca); written in a side test (LAh,
# LS) it must be there.
#
# A layer the frame reads as a code only within the tolerance of p. 64 (its
# `imperfect` column) counts for that code in the tolerant pass of a test
# that needs the code, and in the strict pass as the next code its
# identification lists (or no reference horizon); for a test that forbids
# it, it counts as that code in both (soilKey's reading).
# =============================================================================

.RP_SYSTEM <- "rp2008"

# The pedon being classified, its frame and its view, set by
# classify_rp2008() for the predicates (computed once per profile); a
# predicate called on another pedon reads it afresh.
.rp_state <- new.env(parent = emptyenv())

.rp_with_state <- function(pedon, frame, view, expr) {
  old <- list(pedon = .rp_state$pedon, frame = .rp_state$frame, view = .rp_state$view,
              cache = .rp_state$cache)
  on.exit({
    .rp_state$pedon <- old$pedon
    .rp_state$frame <- old$frame
    .rp_state$view <- old$view
    .rp_state$cache <- old$cache
  }, add = TRUE)
  .rp_state$pedon <- pedon
  .rp_state$frame <- frame
  .rp_state$view <- view
  .rp_state$cache <- new.env(parent = emptyenv())
  force(expr)
}

.rp_current <- function(pedon) !is.null(.rp_state$pedon) && identical(.rp_state$pedon, pedon)

.rp_frame <- function(pedon) {
  if (.rp_current(pedon) && !is.null(.rp_state$frame)) return(.rp_state$frame)
  .rp_frame_codes(.rp_identify(pedon))
}

# The frame with every code read as a code of the vocabulary: the data
# identification also writes Av and Anod, which the notation reads as an A
# with the feature v or nod; candidates likewise.
.rp_frame_codes <- function(fr) {
  if (!nrow(fr)) return(fr)
  voc <- rownames(.rp_vocabulary()$info)
  norm <- function(cd) {
    if (is.na(cd) || cd %in% voc) return(list(code = cd, features = character(0)))
    r <- tryCatch(.rp_parse_one(cd), error = function(e) NULL)
    if (is.null(r) || is.na(r$code)) list(code = cd, features = character(0))
    else list(code = r$code, features = r$features)
  }
  for (k in seq_len(nrow(fr))) {
    nr <- norm(fr$code[k])
    if (!identical(nr$code, fr$code[k])) {
      fr$code[k] <- nr$code
      fr$features[[k]] <- unique(c(fr$features[[k]], nr$features))
    }
    cc <- fr$candidates[[k]]
    if (length(cc)) fr$candidates[[k]] <- unique(vapply(cc, function(c1) if (is.na(c1) || !nzchar(c1)) c1 else norm(c1)$code, ""))
  }
  fr
}

.rp_view <- function(pedon) {
  if (.rp_current(pedon) && !is.null(.rp_state$view)) return(.rp_state$view)
  .rp_profile(pedon)
}

# A value cached for the pedon being classified (computed each time
# otherwise).
.rp_cached <- function(pedon, key, f) {
  if (!.rp_current(pedon) || is.null(.rp_state$cache)) return(f())
  v <- .rp_state$cache[[key]]
  if (is.null(v)) {
    v <- list(f())
    assign(key, v, envir = .rp_state$cache)
  }
  v[[1L]]
}

# A predicate's answer, as the rule engine reads it.
.rp_answer <- function(passed, layers = integer(0), missing = character(0), evidence = list(),
                       notes = character(0), distance = NA_real_) {
  p <- if (length(passed) == 1L) as.logical(passed) else NA
  list(passed = p, layers = as.integer(layers),
       missing = if (is.na(p)) unique(as.character(missing)) else character(0),
       evidence = evidence, notes = unique(as.character(notes)), distance = distance)
}

.rp_or3 <- function(v) if (any(v %in% TRUE)) TRUE else if (anyNA(v)) NA else FALSE
.rp_and3 <- function(v) if (any(v %in% FALSE)) FALSE else if (anyNA(v)) NA else TRUE


# --- horizon codes ----------------------------------------------------------------------------

# The codes a family head stands for (the head itself and its members),
# and the variants of a code (OF: OFr and OFm; G: Gr and Go).
.rp_family_members <- function(code) {
  info <- .rp_vocabulary()$info
  fam <- info[, "family"]
  var <- info[, "variant_of"]
  members <- rownames(info)[(!is.na(fam) & fam == code) | (!is.na(var) & var == code)]
  unique(c(code, members))
}

# A code of the reference files read once: the codes it stands for, the
# features it requires, or a feature alone ("-g") or an overlay ("Sa+").
.rp_spec <- function(s) {
  key <- paste0("spec_", s)
  memo <- .rp_spec_memo[[key]]
  if (!is.null(memo)) return(memo)
  out <- if (grepl("\\+$", s)) list(overlay = sub("\\+$", "", s), codes = character(0), features = character(0))
  else if (grepl("=$", s)) list(codes = sub("=$", "", s), features = character(0))
  else {
    r <- .rp_parse_one(s)
    if (!isTRUE(r$known)) rlang::abort(sprintf("'%s' is no code of the RP notation.", s))
    if (is.na(r$code)) list(feature_only = r$features[1L], codes = character(0), features = character(0))
    else list(codes = .rp_family_members(r$code), features = setdiff(r$features, "*"),
              overlay_req = intersect(r$overlay, "L"))
  }
  out$written <- s
  assign(key, out, envir = .rp_spec_memo)
  out
}
.rp_spec_memo <- new.env(parent = emptyenv())

# Is layer k of the frame one of `spec`? TRUE, FALSE, or NA when the layer
# is undecided and could be. `strict_pos`: the strict pass of a test that
# needs the code, where a layer read only within tolerance does not count.
.rp_member1 <- function(fr, k, spec, strict_pos = FALSE) {
  code <- fr$code[k]
  feats <- fr$features[[k]]
  if (!is.null(spec[["overlay"]])) {
    if (spec[["overlay"]] %in% fr$overlay[[k]]) return(TRUE)
    if (spec[["overlay"]] %in% (fr$overlay_na[[k]] %||% character(0))) return(NA)
    return(FALSE)
  }
  if (!is.null(spec[["feature_only"]])) {
    if (spec[["feature_only"]] %in% feats || identical(code, spec[["feature_only"]])) return(TRUE)
    if (spec[["feature_only"]] %in% (fr$features_na[[k]] %||% character(0))) return(NA)
    return(FALSE)
  }
  if (!is.na(code) && strict_pos && isTRUE(fr$imperfect[k])) {
    # read only within tolerance: the strict pass reads the next code the
    # identification lists, or no reference horizon
    code <- .rp_strict_code(fr, k)
    if (is.na(code)) return(FALSE)
  }
  # a ploughed horizon written with its L (LAh, LS) must be ploughed
  if (length(spec[["overlay_req"]]) && !(spec[["overlay_req"]] %in% fr$overlay[[k]])) {
    if (spec[["overlay_req"]] %in% (fr$overlay_na[[k]] %||% character(0)) && !isFALSE(.rp_member1(fr, k, spec[c("codes", "features")], strict_pos))) return(NA)
    return(FALSE)
  }
  if (!is.na(code)) {
    if (!code %in% spec[["codes"]]) {
      # an H or an O whose kind is open (Hf, Hm or Hs; OL, OF or OH): one of
      # its kinds may be in the set (0.9.349; .rp_kinds_open())
      kv <- .rp_kinds_open(fr, k)
      if (!length(kv) || !any(kv %in% spec[["codes"]])) return(FALSE)
      if (length(spec[["features"]]) && !all(spec[["features"]] %in% c(feats, fr$features_na[[k]] %||% character(0))))
        return(FALSE)
      return(if (all(kv %in% spec[["codes"]]) && !length(spec[["features"]])) TRUE else NA)
    }
    if (!length(spec[["features"]]) || all(spec[["features"]] %in% feats)) return(TRUE)
    if (all(spec[["features"]] %in% c(feats, fr$features_na[[k]] %||% character(0)))) return(NA)
    return(FALSE)
  }
  if (isTRUE(fr$transitional[k]) || isFALSE(fr$passed[k])) return(FALSE)
  # an undecided layer: one of its candidates, with the features required
  # present or undecided
  if (length(spec[["features"]]) && !all(spec[["features"]] %in% c(feats, fr$features_na[[k]] %||% character(0))))
    return(FALSE)
  cand <- fr$candidates[[k]]
  # an undecided H or O may be any of its kinds (0.9.349)
  if (any(cand %in% names(.RP_KINDS))) cand <- unique(c(cand, unlist(.RP_KINDS[intersect(cand, names(.RP_KINDS))])))
  if (length(cand)) {
    # undecided only between codes of the set (Aci or A, for the A family):
    # the layer is one of them
    if (all(cand %in% spec[["codes"]]) && !length(spec[["features"]])) return(TRUE)
    return(if (any(cand %in% spec[["codes"]])) NA else FALSE)
  }
  NA
}

# The kinds an H or an O of the frame may still be when its code is the head
# alone (0.9.349): the kinds the data leave open (its `variants`), or, written
# H or O in the RP notation (p. 2: not to be used alone), the kind the data
# read, else every kind (Hf, Hm, Hs; OL, OF, OH, OHta: the review of 0.9.349).
# Empty for any other code.
.RP_KINDS <- list(H = c("Hf", "Hm", "Hs"), O = c("OL", "OF", "OH", "OHta"))
.rp_kinds_open <- function(fr, k) {
  cd <- fr$code[k]
  if (is.na(cd) || !cd %in% names(.RP_KINDS)) return(character(0))
  v <- as.character(unlist(fr$variants[[k]] %||% character(0)))
  if (length(v)) return(v)
  dc <- fr$data_code[k] %||% NA_character_
  if (!is.na(dc) && dc %in% .rp_family_members(cd) && dc != cd) return(dc)
  .RP_KINDS[[cd]]
}

# The code of a layer read only within tolerance, as the strict pass reads
# it: the first other code its identification lists (NA: none).
.rp_strict_code <- function(fr, k) {
  cc <- setdiff(as.character(fr$candidates[[k]]), c(fr$code[k], ""))
  if (length(cc)) cc[1L] else NA_character_
}

# The membership of every layer of the frame in a list of codes.
.rp_members <- function(fr, codes, strict_pos = FALSE) {
  # memoised for the frame of the pedon being classified (the same object)
  cache <- .rp_state$cache
  memo <- !is.null(cache) && !is.null(.rp_state$frame) && identical(fr, .rp_state$frame)
  if (memo) {
    key <- paste0("members_", paste(as.character(unlist(codes)), collapse = "|"), "_", isTRUE(strict_pos))
    v <- cache[[key]]
    if (!is.null(v)) return(v)
  }
  specs <- lapply(as.character(unlist(codes)), .rp_spec)
  out <- vapply(seq_len(nrow(fr)), function(k)
    .rp_or3(vapply(specs, function(sp) .rp_member1(fr, k, sp, strict_pos), NA)), NA)
  if (memo) assign(key, out, envir = cache)
  out
}

# The layers of the first material: the first lithological unit (the
# Roman numerals) at least 50 cm thick, a thinner one above being a cover
# (recouvert par, p. 61: an epianthropic Z of 30 cm over a brunisol); the
# whole profile when no numeral is written.
.rp_first_material <- function(fr) {
  lith <- suppressWarnings(as.integer(fr$lith %||% rep(1L, nrow(fr))))
  lith[is.na(lith)] <- 1L
  if (all(lith == 1L)) return(rep(TRUE, nrow(fr)))
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  for (u in sort(unique(lith))) {
    th <- sum(pmax(0, bot[lith == u] - pmax(top[lith == u], 0)), na.rm = TRUE)
    if (th >= 50 || u == max(lith)) return(lith == u)
  }
  lith == lith[1L]
}

# Is the current pass the strict pass of a test that needs its codes (not
# inside a none_of block)?
.rp_strict_positive <- function() !.rule_tolerant(.RP_SYSTEM) && !isTRUE(.rule_ctx$negated)

# The depths of the frame from the datum (OL excluded, p. 64).
.rp_tops <- function(fr) as.numeric(fr$top_rp %||% fr$top_cm)
.rp_bots <- function(fr) as.numeric(fr$bottom_rp %||% fr$bottom_cm)

# The attributes that would decide the undecided layers.
.rp_missing_of <- function(fr, rows) unique(unlist(fr$missing[rows]))

# A threshold as the current pass reads it, and the comparison with it: an
# `open` threshold ("over 65 %") is compared strictly as printed and with
# the edge of the band in the tolerant pass.
.rp_meets <- function(value, threshold, kind, direction, open = FALSE) {
  t <- .rule_threshold(threshold, kind, direction, .RP_SYSTEM)
  if (isTRUE(open) && t == threshold) {
    if (direction == "min") value > t else value < t
  } else .rule_check(value, threshold, kind, direction, .RP_SYSTEM)
}

# The window of depths a test reads: rows whose top lies within `within`
# and whose bottom lies deeper than `from` (both optional). `window =
# "depth"` moves the edges by their tolerance so as to favour the class:
# narrower when every layer of the window must meet a value, wider when one
# is enough.
.rp_in_window <- function(fr, within = NULL, from = NULL, window = "none", layers = "all") {
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  narrow <- layers %in% c("all", "weighted")
  w_to <- if (is.null(within)) Inf
          else if (identical(window, "depth")) .rule_threshold(within, "depth", if (narrow) "min" else "max", .RP_SYSTEM)
          else within
  w_from <- if (is.null(from)) -Inf
            else if (identical(window, "depth")) .rule_threshold(from, "depth", if (narrow) "max" else "min", .RP_SYSTEM)
            else from
  ok <- (top < w_to) & (is.na(bot) | bot > w_from) & (is.na(bot) | bot > 0)
  list(ok = ok, to = w_to, from = w_from)
}


# --- the sequences -------------------------------------------------------------------------------

# The matcher reads masters as runs of capitals, so the RP codes are written
# for it as ASCII capitals (Sal SAL, Aca ACA, BTss BTSS, Fe FEX against FE).
.rp_tokens <- function() {
  if (!is.null(.rp_spec_memo$tokens)) return(.rp_spec_memo$tokens)
  codes <- rownames(.rp_vocabulary()$info)
  tok <- toupper(gsub("\u00df", "SS", codes, fixed = TRUE))
  tok <- gsub("[^A-Z]", "X", tok)
  for (i in seq_along(tok)) while (tok[i] %in% tok[seq_len(i - 1L)]) tok[i] <- paste0(tok[i], "X")
  out <- stats::setNames(tok, codes)
  assign("tokens", out, envir = .rp_spec_memo)
  out
}

.rp_token <- function(code) {
  tk <- .rp_tokens()
  ifelse(is.na(code), NA_character_, unname(tk[code]))
}

# Split a string on a character outside parentheses.
.rp_split_top <- function(s, sep) {
  ch <- strsplit(s, "", fixed = TRUE)[[1L]]
  depth <- 0L; cur <- character(0); out <- character(0)
  for (c1 in ch) {
    if (c1 == "(") depth <- depth + 1L
    if (c1 == ")") depth <- depth - 1L
    if (c1 == sep && depth == 0L) { out <- c(out, paste(cur, collapse = "")); cur <- character(0) }
    else cur <- c(cur, c1)
  }
  c(out, paste(cur, collapse = ""))
}

# A sequence of the reference files ("Aci-(Sci)-Sca-(C|M|R|D)") as a
# profile formula of the matcher, and its parse: each code to its token, the
# features it is written with kept (typical, not tested, unless required);
# a Roman numeral alone (Z-II, the buried natural layers) names no slot.
.rp_formula <- function(s, required_features = character(0)) {
  key <- paste0("formula_", s, "|", paste(required_features, collapse = ","))
  memo <- .rp_spec_memo[[key]]
  if (!is.null(memo)) return(memo)
  items <- .rp_split_top(s, "-")
  out <- character(0)
  # the parent material written last, optional ("(C|M|R|D)"): a slot matches
  # a run of one code, so the slot is written as one optional slot per layer
  # code, each of the C, M and D in any order (an M or a D over a C, a C over
  # an M: the review of 0.9.348), the R last, so that a description going
  # through a C into an R matches (0.9.348)
  layer_fam <- c("C", "M", "D", "R")
  for (n_it in seq_along(items)) {
    it <- items[n_it]
    opt <- grepl("^\\(.*\\)$", it)
    inner <- if (opt) substr(it, 2L, nchar(it) - 1L) else it
    alts <- character(0); fams <- character(0)
    for (a in .rp_split_top(inner, "|")) {
      r <- .rp_parse_one(a)
      if (!isTRUE(r$known)) rlang::abort(sprintf("'%s' is no code of the RP notation (in %s).", a, s))
      if (is.na(r$code)) next
      f <- gsub("[^a-z]", "", paste(r$features, collapse = ""))
      alts <- c(alts, paste0(.rp_token(r$code), f))
      fams <- c(fams, r$code)
    }
    if (!length(alts)) next
    if (opt && n_it == length(items) && length(alts) > 1L && all(fams %in% layer_fam)) {
      o <- order(match(fams, layer_fam))
      alts <- alts[o]; fams <- fams[o]
      free <- unique(alts[fams != "R"]); last <- unique(alts[fams == "R"])
      fs <- if (length(free) > 1L) rep(paste(free, collapse = "|"), length(free)) else free
      out <- c(out, paste0("(", c(fs, last), ")"))
      next
    }
    item <- paste(unique(alts), collapse = "|")
    out <- c(out, if (opt) paste0("(", item, ")") else item)
  }
  info <- .rp_vocabulary()$info
  fam <- info[, "family"]
  heads <- intersect(unique(fam[!is.na(fam)]), rownames(info))
  alts <- list()
  for (h in heads) {
    mem <- setdiff(.rp_family_members(h), h)
    if (length(mem)) alts[[.rp_token(h)]] <- unname(.rp_token(mem))
  }
  spec <- .parse_profile_formula(paste(out, collapse = "-"), masters = unname(.rp_tokens()),
                                 parents = character(0), alternatives = alts,
                                 required_features = as.character(required_features))
  spec$written_rp <- s
  spec$rp_voc <- .formula_vocabulary(list(spec))
  assign(key, spec, envir = .rp_spec_memo)
  spec
}

# The frame as the matcher reads it: codes as tokens, depths from the datum;
# in the strict pass a layer read only within tolerance is no reference
# horizon.
.rp_match_table <- function(fr, strict = FALSE) {
  code <- fr$code
  passed <- fr$passed
  if (strict) {
    imp <- which(!is.na(code) & fr$imperfect %in% TRUE)
    for (k in imp) {
      code[k] <- .rp_strict_code(fr, k)
      passed[k] <- !is.na(code[k])
    }
  }
  data.frame(horizon_idx = fr$horizon_idx, top_cm = .rp_tops(fr), bottom_cm = .rp_bots(fr),
             designation = fr$designation, code = .rp_token(code), passed = passed,
             transitional = fr$transitional %in% TRUE,
             stringsAsFactors = FALSE) -> tab
  tab$features <- lapply(fr$features, function(f) gsub("[^a-z]", "", f[nzchar(gsub("[^a-z]", "", f))]))
  tab$features_na <- lapply(fr$features_na %||% rep(list(character(0)), nrow(fr)),
                            function(f) gsub("[^a-z]", "", f[nzchar(gsub("[^a-z]", "", f))]))
  tab$candidates <- lapply(fr$candidates, function(cc) {
    cc <- as.character(cc)
    unique(ifelse(is.na(cc) | !nzchar(cc), "", .rp_token(cc)))
  })
  tab
}

# Every completion of the undecided layers of a match table (each layer one
# of its candidates, "" no horizon), up to 32 of them: TRUE when one of the
# sequences matches every completion, FALSE when none matches any, NA
# otherwise or beyond 32.
.rp_seq_completions <- function(tab, specs, max_n = 32L) {
  und <- which(is.na(tab$code) & !(tab$passed %in% FALSE) & !(tab$transitional %in% TRUE))
  if (!length(und)) return(NA)
  opts <- lapply(und, function(k) { cc <- tab$candidates[[k]]; if (length(cc)) cc else NULL })
  if (any(vapply(opts, is.null, NA))) return(NA)
  n <- prod(lengths(opts))
  if (n > max_n) return(NA)
  grid <- expand.grid(opts, stringsAsFactors = FALSE)
  voc <- .rp_specs_vocabulary(specs)
  seen_t <- FALSE; seen_f <- FALSE
  for (g in seq_len(nrow(grid))) {
    t2 <- tab
    for (j in seq_along(und)) {
      cd <- grid[g, j]
      t2$code[und[j]] <- if (nzchar(cd)) cd else NA_character_
      t2$passed[und[j]] <- nzchar(cd)
      t2$candidates[[und[j]]] <- character(0)
    }
    # one sequence matching is enough (.pick_scheme(): TRUE when any is)
    ok <- FALSE
    for (sp in specs) if (isTRUE(.mhs_match(t2, .mhs_context(t2, sp, vocabulary = voc))$passed)) { ok <- TRUE; break }
    if (ok) seen_t <- TRUE else seen_f <- TRUE
    # both verdicts seen: undecided, whatever the other completions give
    if (seen_t && seen_f) return(NA)
  }
  seen_t
}

# The vocabulary of a list of parsed sequences: .formula_vocabulary() of the
# list, the union in order of the vocabularies of its sequences, each read
# once when the sequence is parsed (.rp_formula()).
.rp_specs_vocabulary <- function(specs) {
  unique(unlist(lapply(specs, function(s) s$rp_voc %||% .formula_vocabulary(list(s))), use.names = FALSE)) %||% character(0)
}

# .match_schemes() on a table already read by .as_horizon_table() and
# sequences already parsed and named: the same result, the table not read
# again (the timing of the review of 0.9.348).
.rp_match_schemes <- function(tab, specs, n_nearest = 2L) {
  voc <- .rp_specs_vocabulary(specs)
  res <- lapply(specs, function(sp) .mhs_match(tab, .mhs_context(tab, sp, vocabulary = voc)))
  names(res) <- names(specs)
  pick <- .pick_scheme(res)
  pick$nearest <- if (!isTRUE(pick$passed)) .nearest_schemes(res, n_nearest) else NULL
  pick$results <- res
  pick
}

#' The reference's sequences of horizons (RP 2008)
#'
#' One of the sequences matches the horizon frame (match_horizon_sequence();
#' the alternatives of one reference closed on their codes, every other
#' horizon left out: the book writes its exclusions as side tests). A layer
#' the frame leaves undecided leaves the match undecided when it decides it.
#' @noRd
sequence_rp <- function(pedon, sequences, required_features = NULL, first_material = FALSE) {
  sequences <- as.character(unlist(sequences))
  if (!length(sequences)) return(.rp_answer(TRUE, notes = "no sequence printed"))
  strict <- .rp_strict_positive()
  fm <- isTRUE(first_material)
  fr <- .rp_frame(pedon)
  # only the first material (above the first lithological discontinuity)
  if (fm && nrow(fr)) fr <- fr[.rp_first_material(fr), , drop = FALSE]
  # the strict pass reads another table only when a layer is read within
  # tolerance; otherwise both passes share one match (the same answer)
  if (strict && !any(!is.na(fr$code) & fr$imperfect %in% TRUE)) strict <- FALSE
  key <- sprintf("seq_%s_%s_%s_%s", paste(sequences, collapse = "~"),
                 paste(required_features, collapse = ","), strict, fm)
  .rp_cached(pedon, key, function() {
    if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons", notes = "no horizons"))
    specs <- lapply(sequences, .rp_formula, required_features = as.character(unlist(required_features)))
    names(specs) <- sequences
    tab <- .rp_cached(pedon, sprintf("match_table_%s_%s", strict, fm), function()
      .as_horizon_table(.rp_match_table(fr, strict = strict)))
    res <- .rp_match_schemes(tab, specs, n_nearest = 2L)
    # undecided over the alternatives of the reference: complete the
    # undecided layers (few candidates) and decide when every completion
    # gives one verdict (Aci or A over Sci, an A or an Slu under an Alu)
    if (is.na(res$passed)) {
      v <- .rp_seq_completions(tab, specs)
      if (!is.na(v)) {
        res$passed <- v
        res$code <- if (v) "(every completion of the undecided layers)" else NA_character_
      }
    }
    und <- which(is.na(fr$code) & !(fr$passed %in% FALSE))
    layers <- if (isTRUE(res$passed)) {
      m <- res$results[[res$code]]$matched
      if (is.data.frame(m) && "horizon_idx" %in% names(m)) stats::na.omit(as.integer(m$horizon_idx)) else integer(0)
    } else integer(0)
    .rp_answer(res$passed, layers = layers, missing = .rp_missing_of(fr, und),
               evidence = list(sequence = res$code, undecided = res$candidates,
                               nearest = res$nearest),
               notes = if (isTRUE(res$passed)) sprintf("the sequence %s", res$code)
                       else if (is.na(res$passed)) sprintf("undecided: %s", paste(res$candidates, collapse = "; "))
                       else sprintf("no sequence of %s", paste(sequences, collapse = "; ")))
  })
}


# --- presence, absence, thickness ----------------------------------------------------------------

#' A horizon of a set starting within a depth (RP 2008)
#'
#' Some layer of `codes` has its top within `depth` cm (and at `from` cm or
#' deeper), the depths from the datum of p. 64. Tolerant: `depth` widened and
#' `from` lowered by their tolerances (narrowed inside a none_of block).
#' @noRd
horizon_within_rp <- function(pedon, codes, depth = NULL, from = NULL) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  m <- .rp_members(fr, codes, strict_pos = .rp_strict_positive())
  top <- .rp_tops(fr)
  t_to <- if (is.null(depth)) Inf else .rule_threshold(depth, "depth", "max", .RP_SYSTEM)
  t_from <- if (is.null(from)) -Inf else .rule_threshold(from, "depth", "min", .RP_SYSTEM)
  edge <- 1e-9 * max(1, abs(t_to))
  inwin <- if (is.infinite(t_to)) top >= t_from
           else if (!is.null(depth) && t_to != depth) top <= t_to + edge & top >= t_from
           else top < t_to & top >= t_from
  inwin <- inwin & (is.na(.rp_bots(fr)) | .rp_bots(fr) > 0)
  v <- vapply(seq_len(nrow(fr)), function(k) .rp_and3(c(m[k], inwin[k])), NA)
  p <- .rp_or3(v)
  dist <- NA_real_
  if (!is.null(depth) && any(m %in% TRUE))
    dist <- .rule_distance(top[m %in% TRUE], depth, "depth", "max", "any", .RP_SYSTEM)
  .rp_answer(p, layers = fr$horizon_idx[v %in% TRUE], missing = .rp_missing_of(fr, which(is.na(v))),
             evidence = list(codes = codes, depth = depth, from = from),
             notes = sprintf("%s %s%s", paste(unlist(codes), collapse = ", "),
                             if (isTRUE(p)) "present" else if (isFALSE(p)) "absent" else "undecided",
                             if (!is.null(depth)) sprintf(" within %s cm", format(t_to)) else ""),
             distance = dist)
}

#' No horizon of a set within a depth (RP 2008)
#'
#' No layer of `codes` starts within `depth` cm (the whole profile when
#' NULL), in the first material only with `first_material`. Tolerant:
#' `depth` lowered by its tolerance ("no BT within 120 cm" looks within 100
#' cm).
#' @noRd
no_horizon_within_rp <- function(pedon, codes, depth = NULL, first_material = FALSE) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  m <- .rp_members(fr, codes)
  # only in the first material: the layers above the first lithological
  # discontinuity (a brunisol on a buried calcosol, p. 117; the review's D6)
  if (isTRUE(first_material)) m[!.rp_first_material(fr)] <- FALSE
  top <- .rp_tops(fr)
  t <- if (is.null(depth)) Inf else .rule_threshold(depth, "depth", "min", .RP_SYSTEM)
  inwin <- if (is.infinite(t)) rep(TRUE, nrow(fr)) else top < t
  inwin <- inwin & (is.na(.rp_bots(fr)) | .rp_bots(fr) > 0)
  v <- vapply(seq_len(nrow(fr)), function(k) .rp_and3(c(m[k], inwin[k])), NA)
  p <- if (any(v %in% TRUE)) FALSE else if (anyNA(v)) NA else TRUE
  dist <- NA_real_
  if (!is.null(depth) && any(v %in% TRUE))
    dist <- .rule_distance(top[v %in% TRUE], depth, "depth", "min", "all", .RP_SYSTEM)
  hit <- fr$code[v %in% TRUE]
  .rp_answer(p, layers = fr$horizon_idx[v %in% TRUE], missing = .rp_missing_of(fr, which(is.na(v))),
             evidence = list(codes = codes, depth = depth, found = hit),
             notes = if (isFALSE(p)) sprintf("%s present%s", paste(unique(hit), collapse = ", "),
                                              if (!is.null(depth)) sprintf(" within %s cm", format(t)) else "")
                     else character(0),
             distance = dist)
}

# The thickness of a set of horizons as a lower and an upper bound (the
# undecided layers that could belong to it), in all or as the run from the
# surface (the holorganic O layers above skipped unless the set holds them;
# a `cover` of other codes allowed above it up to `cover_max` cm).
.rp_thickness_bounds <- function(fr, codes, from_surface = FALSE, cover = NULL, cover_max = NULL,
                                 within = NULL, strict_pos = FALSE) {
  m <- .rp_members(fr, codes, strict_pos = strict_pos)
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  lim <- if (is.null(within)) Inf else within
  th <- pmax(0, pmin(bot, lim) - pmax(top, 0))
  keep <- !is.na(top) & (is.na(bot) | bot > 0)
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[keep[ord]]
  if (!from_surface) {
    if (any(is.na(th[ord][m[ord] %in% c(TRUE, NA)]))) return(list(lo = NA_real_, hi = NA_real_, rows = integer(0)))
    lo <- sum(th[ord][m[ord] %in% TRUE]); hi <- lo + sum(th[ord][is.na(m[ord])])
    return(list(lo = lo, hi = hi, rows = ord[m[ord] %in% TRUE], und = ord[is.na(m[ord])]))
  }
  holo <- c("O", .rp_family_members("O"))
  has_o <- any(holo %in% unlist(lapply(as.character(unlist(codes)), function(s) .rp_spec(s)$codes)))
  cov <- if (length(cover)) .rp_members(fr, cover) else rep(FALSE, nrow(fr))
  cov_th <- 0
  lo <- 0; hi <- 0; lo_open <- TRUE; rows <- integer(0); und <- integer(0)
  started <- FALSE
  for (k in ord) {
    if (!started) {
      if (!has_o && isTRUE(fr$code[k] %in% holo)) next
      if (isTRUE(cov[k]) && !isTRUE(m[k])) {
        cov_th <- cov_th + th[k]
        if (!is.null(cover_max) && isTRUE(cov_th >= cover_max)) break
        next
      }
      started <- TRUE
    }
    if (isFALSE(m[k])) break
    if (is.na(th[k])) return(list(lo = NA_real_, hi = NA_real_, rows = integer(0)))
    hi <- hi + th[k]
    if (isTRUE(m[k]) && lo_open) { lo <- lo + th[k]; rows <- c(rows, k) }
    else { lo_open <- FALSE; und <- c(und, k) }
  }
  list(lo = lo, hi = hi, rows = rows, und = und)
}

#' The thickness of a set of horizons (RP 2008)
#'
#' The thickness of the layers of `codes` (summed through the solum, or the
#' run from the surface with `from_surface`), at least `min` and at most
#' `max` cm (over `min` and under `max` with `open`, as printed: "more than
#' 10 cm"). Tolerant: `min` lowered, `max` raised by the tolerance of their
#' value (p. 64; 40 cm and the others the table does not list stay strict).
#' @noRd
thickness_rp <- function(pedon, codes, min = NULL, max = NULL, from_surface = FALSE,
                         cover = NULL, cover_max = NULL, within = NULL, open = FALSE) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  pos <- !is.null(min) && .rp_strict_positive()
  b <- .rp_thickness_bounds(fr, codes, isTRUE(from_surface), unlist(cover), cover_max, within,
                            strict_pos = pos)
  if (is.na(b$lo)) return(.rp_answer(NA, missing = c("top_cm", "bottom_cm"),
                                     notes = "a depth of the set is not recorded"))
  res <- logical(0)
  ds <- numeric(0)
  chk <- function(v, t, dir) if (isTRUE(open)) .rp_meets(v, t, "depth", dir, open = TRUE) else .rule_check(v, t, "depth", dir, .RP_SYSTEM)
  if (!is.null(min)) {
    res <- c(res, if (isTRUE(chk(b$lo, min, "min"))) TRUE
                  else if (isFALSE(chk(b$hi, min, "min"))) FALSE else NA)
    ds <- c(ds, .rule_distance(b$hi, min, "depth", "min", "any", .RP_SYSTEM))
  }
  if (!is.null(max)) {
    res <- c(res, if (isTRUE(chk(b$hi, max, "max"))) TRUE
                  else if (isFALSE(chk(b$lo, max, "max"))) FALSE else NA)
    ds <- c(ds, .rule_distance(b$lo, max, "depth", "max", "any", .RP_SYSTEM))
  }
  dist <- if (all(is.na(ds))) NA_real_ else sum(ds, na.rm = TRUE)
  p <- .rp_and3(res)
  .rp_answer(p, layers = fr$horizon_idx[b$rows], missing = .rp_missing_of(fr, b$und),
             evidence = list(codes = codes, thickness = b$lo, thickness_upper = b$hi, min = min, max = max,
                             from_surface = isTRUE(from_surface)),
             notes = sprintf("%s %s cm thick%s", paste(unlist(codes), collapse = " + "), format(b$lo),
                             if (b$hi > b$lo) sprintf(" (up to %s with the undecided layers)", format(b$hi)) else ""),
             distance = if (isFALSE(p)) dist else NA_real_)
}

#' The ratio of the thicknesses of two sets of horizons (RP 2008)
#'
#' [Aci + Sci] / Sca and the like (p. 113): strict, the ratios are soilKey's
#' (default 21).
#' @noRd
thickness_ratio_rp <- function(pedon, numerator, denominator, min = NULL, max = NULL, open = FALSE) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  n <- .rp_thickness_bounds(fr, numerator)
  d <- .rp_thickness_bounds(fr, denominator)
  if (is.na(n$lo) || is.na(d$lo)) return(.rp_answer(NA, missing = c("top_cm", "bottom_cm")))
  if (n$hi > n$lo || d$hi > d$lo)
    return(.rp_answer(NA, missing = .rp_missing_of(fr, c(n$und, d$und)), notes = "undecided layers in the ratio"))
  r <- if (d$lo > 0) n$lo / d$lo else if (n$lo > 0) Inf else NA_real_
  if (is.na(r)) return(.rp_answer(FALSE, notes = "neither set is present"))
  ok <- c(if (!is.null(min)) (if (isTRUE(open)) r > min else r >= min),
          if (!is.null(max)) (if (isTRUE(open)) r < max else r <= max))
  .rp_answer(all(ok), evidence = list(ratio = r, numerator = n$lo, denominator = d$lo),
             notes = sprintf("[%s] / [%s] = %s", paste(unlist(numerator), collapse = " + "),
                             paste(unlist(denominator), collapse = " + "), format(signif(r, 3))))
}


# --- values of the horizons ------------------------------------------------------------------------

# A value of layer i as the RP reads it: S/CEC (bs, Q12), Ca/Mg, the sum of
# the bases, the molar CaCO3/MgCO3, or a recorded attribute.
.rp_value <- function(x, i, attribute) {
  switch(attribute,
    bs = .rp_bs(x, i)$value,
    ca_mg = .rp_camg(x, i),
    sum_bases = {
      b <- vapply(c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol"), function(a) .rp_num(x, i, a), 0)
      if (anyNA(b)) NA_real_ else sum(b)
    },
    caco3_mgco3 = .rp_carbonates(x, i)$ratio,
    # the saturation of the complex by Ca and Mg, % of the CEC (the
    # leptismectisols, p. 217)
    camg_sat = {
      ca <- .rp_num(x, i, "ca_cmol"); mg <- .rp_num(x, i, "mg_cmol"); cec <- .rp_num(x, i, "cec_cmol")
      if (is.na(ca) || is.na(mg) || !isTRUE(cec > 0)) NA_real_ else 100 * (ca + mg) / cec
    },
    .rp_num(x, i, attribute))
}

.rp_value_attrs <- function(attribute) {
  switch(attribute, bs = c("bs_pct", "cec_cmol"), ca_mg = c("ca_cmol", "mg_cmol"),
         sum_bases = c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol"),
         caco3_mgco3 = c("caco3_pct", "mgco3_pct"), camg_sat = c("ca_cmol", "mg_cmol", "cec_cmol"), attribute)
}

# The mineral layers of the frame (no holorganic O or H, no rock R).
.rp_mineral_rows <- function(fr) {
  holo <- unique(c(.rp_family_members("O"), .rp_family_members("H"), "R"))
  !(fr$code %in% holo)
}

#' A value of the horizons of a set (RP 2008)
#'
#' The value of `attribute` (a recorded attribute, or bs, ca_mg, sum_bases,
#' caco3_mgco3, camg_sat) in the layers of `codes` (every mineral layer when NULL)
#' within the window, against `threshold`: every layer (`layers = "all"`),
#' one (`"any"`), or the mean weighted by thickness (`"weighted"`); only the
#' first layer of the set with `position = "first"`. `kind` names the
#' tolerance of p. 64 (bs, ph, cec, organic_carbon; "none" strict). No layer
#' of the set: FALSE (undecided when an undecided layer could be one).
#' @noRd
attribute_rp <- function(pedon, attribute, threshold, direction = c("min", "max"), kind = "none",
                         open = FALSE, codes = NULL, within = NULL, from = NULL, window = "none",
                         layers = c("all", "any", "weighted"), position = c("all", "first")) {
  direction <- match.arg(direction); layers <- match.arg(layers); position <- match.arg(position)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  m <- if (is.null(codes)) ifelse(.rp_mineral_rows(fr), TRUE, FALSE)
       else .rp_members(fr, codes, strict_pos = .rp_strict_positive())
  win <- .rp_in_window(fr, within, from, window, layers)$ok
  m <- ifelse(win %in% TRUE, m, FALSE)
  top <- .rp_tops(fr)
  rows <- which(m %in% TRUE)
  rows <- rows[order(top[rows])]
  # the first layer of the set: an undecided layer above it that could
  # belong to the set leaves the answer open unless its value gives the
  # same verdict (the review's D8)
  above <- integer(0)
  if (position == "first") {
    poss <- which(is.na(m))
    lim <- if (length(rows)) top[rows[1L]] else Inf
    above <- poss[!is.na(top[poss]) & top[poss] < lim]
    if (length(rows)) rows <- rows[1L]
  }
  if (length(above)) {
    va <- vapply(fr$horizon_idx[above], function(i) as.numeric(.rp_value(x, i, attribute)), 0)
    oa <- .rp_meets(va, threshold, kind, direction, open)
    vf <- if (length(rows)) .rp_meets(as.numeric(.rp_value(x, fr$horizon_idx[rows], attribute)), threshold, kind,
                                      direction, open) else NA
    if (anyNA(oa) || is.na(vf) || any(oa != vf))
      return(.rp_answer(NA, missing = unique(c(.rp_missing_of(fr, above), .rp_value_attrs(attribute))),
                        notes = "an undecided layer above the first of the set could be one"))
  }
  if (!length(rows)) {
    p <- if (any(is.na(m))) NA else FALSE
    return(.rp_answer(p, missing = .rp_missing_of(fr, which(is.na(m))),
                      notes = sprintf("no %s layer in the window", if (is.null(codes)) "mineral" else paste(unlist(codes), collapse = ", "))))
  }
  hi <- fr$horizon_idx[rows]
  v <- vapply(hi, function(i) as.numeric(.rp_value(x, i, attribute)), 0)
  if (layers == "weighted") {
    th <- pmax(0, .rp_bots(fr)[rows] - pmax(top[rows], 0))
    v <- if (anyNA(v) || anyNA(th) || sum(th) <= 0) NA_real_ else sum(v * th) / sum(th)
  }
  ok <- .rp_meets(v, threshold, kind, direction, open)
  p <- if (layers == "any") .rp_or3(ok) else .rp_and3(ok)
  .rp_answer(p, layers = if (isTRUE(p)) hi else integer(0),
             missing = .rp_value_attrs(attribute),
             evidence = list(attribute = attribute, values = v, threshold = threshold,
                             read_as = .rule_threshold(threshold, kind, direction, .RP_SYSTEM)),
             notes = sprintf("%s %s (%s %s%s)", attribute, paste(format(signif(v, 4)), collapse = ", "),
                             if (direction == "min") ">=" else "<=", format(threshold),
                             if (layers == "weighted") ", mean weighted by thickness" else ""),
             distance = .rule_distance(v, threshold, kind, direction,
                                       if (layers == "any") "any" else "all", .RP_SYSTEM))
}

#' The thickness of the layers whose value meets a threshold (RP 2008)
#'
#' The layers of `codes` within `within` cm whose `attribute` meets
#' `threshold`, summed: at least `min` cm (its tolerance of p. 64). The
#' perhydric andosols: water at 1500 kPa 100 % or more over 35 cm within 100.
#' @noRd
attribute_thickness_rp <- function(pedon, attribute, threshold, direction = c("min", "max"),
                                   kind = "none", codes = NULL, within = NULL, min = NULL, open = FALSE) {
  direction <- match.arg(direction)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  m <- if (is.null(codes)) .rp_mineral_rows(fr) else .rp_members(fr, codes, strict_pos = .rp_strict_positive())
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  lim <- if (is.null(within)) Inf else within
  th <- pmax(0, pmin(bot, lim) - pmax(top, 0))
  rows <- which(!(m %in% FALSE) & top < lim)
  if (!length(rows)) return(.rp_answer(FALSE, notes = "no layer of the set"))
  v <- vapply(fr$horizon_idx[rows], function(i) as.numeric(.rp_value(x, i, attribute)), 0)
  ok <- .rp_meets(v, threshold, kind, direction, open)
  sure <- m[rows] %in% TRUE & ok %in% TRUE
  maybe <- sure | (is.na(m[rows]) & !(ok %in% FALSE)) | (m[rows] %in% TRUE & is.na(ok))
  lo <- sum(th[rows][sure], na.rm = TRUE); hi <- sum(th[rows][maybe], na.rm = TRUE)
  if (is.null(min)) min <- 0
  p <- if (isTRUE(.rule_check(lo, min, "depth", "min", .RP_SYSTEM))) TRUE
       else if (isFALSE(.rule_check(hi, min, "depth", "min", .RP_SYSTEM))) FALSE else NA
  .rp_answer(p, layers = fr$horizon_idx[rows][sure], missing = .rp_value_attrs(attribute),
             evidence = list(attribute = attribute, thickness = lo, thickness_upper = hi),
             notes = sprintf("%s cm of %s meeting %s %s", format(lo), paste(unlist(codes), collapse = ", "),
                             attribute, format(threshold)),
             distance = if (isFALSE(p)) .rule_distance(hi, min, "depth", "min", "any", .RP_SYSTEM) else NA_real_)
}

#' Coarse elements under a threshold by weight (RP 2008)
#'
#' Every mineral layer within `within` cm has coarse elements under
#' `threshold` % by weight of the whole soil (Q11: by weight, else the
#' volume converted or its band; not recorded, none).
#' @noRd
coarse_rp <- function(pedon, threshold = 60, within = NULL) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  win <- .rp_in_window(fr, within, NULL, "depth", "all")$ok
  rows <- which(.rp_mineral_rows(fr) & win %in% TRUE)
  r <- lapply(fr$horizon_idx[rows], function(i) .rp_coarse_wt(x, i, threshold))
  over <- vapply(r, function(z) z$passed, NA)
  p <- if (any(over %in% TRUE)) FALSE else if (anyNA(over)) NA else TRUE
  .rp_answer(p, missing = c("coarse_fragments_wt_pct", "bulk_density_g_cm3"),
             notes = unique(unlist(lapply(r, `[[`, "note"))))
}

#' The texture differentiation index (RP 2008)
#'
#' The clay of the most clayey layer of `lower` over the clay of `upper`
#' (the mean weighted by thickness), as the lexicon of p. 401 reads it.
#' Strict: 1.3 is soilKey's boundary of the chernosols (p. 127).
#' @noRd
idt_rp <- function(pedon, upper, lower, min = NULL, max = NULL, open = FALSE) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  mu <- .rp_members(fr, upper); ml <- .rp_members(fr, lower)
  ru <- which(mu %in% TRUE); rl <- which(ml %in% TRUE)
  if (!length(ru) || !length(rl))
    return(.rp_answer(if (anyNA(c(mu, ml))) NA else FALSE, missing = .rp_missing_of(fr, which(is.na(c(mu, ml)))),
                      notes = "no upper or lower horizon for the index"))
  cu <- vapply(fr$horizon_idx[ru], function(i) .rp_clay(x, i), 0)
  cl <- vapply(fr$horizon_idx[rl], function(i) .rp_clay(x, i), 0)
  th <- pmax(0, .rp_bots(fr)[ru] - pmax(.rp_tops(fr)[ru], 0))
  if (anyNA(cu) || all(is.na(cl)) || anyNA(th) || sum(th) <= 0)
    return(.rp_answer(NA, missing = "clay_pct", notes = "clay not recorded"))
  up <- sum(cu * th) / sum(th)
  r <- if (up > 0) max(cl, na.rm = TRUE) / up else NA_real_
  if (is.na(r)) return(.rp_answer(NA, missing = "clay_pct"))
  ok <- c(if (!is.null(min)) (if (isTRUE(open)) r > min else r >= min),
          if (!is.null(max)) (if (isTRUE(open)) r < max else r <= max))
  .rp_answer(all(ok), evidence = list(idt = r),
             notes = sprintf("texture differentiation index %s", format(signif(r, 3))))
}


# --- the tests of the chapters ----------------------------------------------------------------------

# The calcareous rocks and materials (a closed vocabulary, whole words) and
# the words that deny them (the review's D7).
.RP_CALCAREOUS_WORDS <- paste0("\\b(limestones?|chalk|chalky|marls?|marly|marlstones?|dolomites?|dolomitic|",
                               "calcareous|calcarenites?|calcrete|caliche|travertines?|calc-schists?|",
                               "calcaires?|craies?|crayeux|crayeuse|marnes?|marneux|marneuse|marno-calcaires?|",
                               "dolomies?|dolomitiques?|calcschistes?|faluns?)\\b")
.RP_CALCAREOUS_NOT <- paste0("\\b(non|not|no)[- ]?(calcareous|calcaire|calc)|noncalcareous|decalcif|",
                             "d\u00e9calcif|decarbonat|d\u00e9carbonat|carbonate[- ]free|sans calcaire")
.rp_calcareous_text <- function(t) {
  !is.na(t) && grepl(.RP_CALCAREOUS_WORDS, t, perl = TRUE) && !grepl(.RP_CALCAREOUS_NOT, t, perl = TRUE)
}

#' A carbonate reserve recorded in the solum's material (RP 2008, Q16)
#'
#' TRUE when carbonates are recorded below the solum (a C, M, R, D or K, an
#' Sca or Sdo, a ca or do feature, CaCO3 or an effervescence in those
#' layers), in a calcareous rock (rock_origin) or parent material: the
#' positive proof that makes a calcisol rather than a brunisol eutrique
#' sature (pp. 107, 112, 116). FALSE otherwise: a field observation not
#' recorded is absent, and the reserve upslope is never in the data.
#' @noRd
carbonate_reserve_rp <- function(pedon) {
  fr <- .rp_frame(pedon)
  x <- .rp_view(pedon)
  below <- c("C", "R", "D", .rp_family_members("M"), .rp_family_members("K"), "Sca", "Sdo")
  ev <- character(0)
  for (k in seq_len(nrow(fr))) {
    i <- fr$horizon_idx[k]
    cd <- fr$code[k]
    feats <- fr$features[[k]]
    if (isTRUE(cd %in% c(.rp_family_members("K"), "Sca", "Sdo")) || any(c("ca", "do") %in% feats))
      ev <- c(ev, sprintf("%s at %s cm", fr$composed[k] %||% cd, format(.rp_tops(fr)[k])))
    else if (isTRUE(cd %in% below)) {
      cc <- .rp_num(x, i, "caco3_pct")
      ec <- .rp_txt(x, i, "effervescence_class")
      if (isTRUE(cc > 0) || (!is.na(ec) && !grepl("^(none|no|absent)", ec)))
        ev <- c(ev, sprintf("carbonates in the %s at %s cm", cd, format(.rp_tops(fr)[k])))
    }
    ro <- .rp_txt(x, i, "rock_origin")
    if (.rp_calcareous_text(ro)) ev <- c(ev, sprintf("a calcareous rock (%s)", ro))
  }
  pm <- .rp_site_txt(x, "parent_material")
  if (.rp_calcareous_text(pm)) ev <- c(ev, sprintf("a calcareous parent material (%s)", pm))
  p <- length(ev) > 0L
  .rp_answer(p, evidence = list(reserve = unique(ev)),
             notes = if (p) sprintf("a carbonate reserve: %s", paste(unique(ev), collapse = "; "))
                     else "no carbonate reserve recorded (the reserve upslope is never in the data, Q16)")
}

.RP_COLD_KOPPEN <- c("Dfc", "Dfd", "Dsc", "Dsd", "Dwc", "Dwd")

#' A permafrost layer within a depth (RP 2008)
#'
#' TRUE from a layer P or a frozen designation (-*), ice in a layer
#' (ice_pct, ice_form), permafrost_temp_C at or under 0 within `depth` cm
#' (200 +/- 30), or site$frost_regime permafrost; FALSE when the site records
#' another frost regime, a soil temperature regime warmer than cryic, a
#' Koppen climate outside the polar and subarctic ones, or a latitude under
#' 50 degrees with an elevation under 1500 m (soilKey's reading);
#' undecided otherwise (default 21: two years of frost are never measured);
#' with `proven`, FALSE unless the data show it.
#' @noRd
permafrost_rp <- function(pedon, depth = 200, proven = FALSE) {
  if (isTRUE(proven)) {
    r <- permafrost_rp(pedon, depth)
    if (is.na(r$passed)) r <- .rp_answer(FALSE, notes = "permafrost not shown by the data")
    return(r)
  }
  fr <- .rp_frame(pedon)
  x <- .rp_view(pedon)
  t <- .rule_threshold(depth, "depth", "max", .RP_SYSTEM)
  top <- .rp_tops(fr)
  ev <- character(0)
  for (k in seq_len(nrow(fr))) {
    if (is.na(top[k]) || top[k] > t) next
    i <- fr$horizon_idx[k]
    pt <- .rp_num(x, i, "permafrost_temp_C")
    ice <- .rp_num(x, i, "ice_pct")
    form <- .rp_txt(x, i, "ice_form")
    if (identical(fr$code[k], "P") || "*" %in% fr$features[[k]] || "*" %in% (x$des$hint_features[[i]] %||% character(0)))
      ev <- c(ev, sprintf("a frozen layer at %s cm", format(top[k])))
    if (isTRUE(pt <= 0)) ev <- c(ev, sprintf("%s C at %s cm", format(pt), format(top[k])))
    if (isTRUE(ice > 0)) ev <- c(ev, sprintf("ice %s %% at %s cm", format(ice), format(top[k])))
    if (!is.na(form) && !grepl("^none", form)) ev <- c(ev, sprintf("ground ice (%s) at %s cm", form, format(top[k])))
  }
  frg <- .rp_site_txt(x, "frost_regime")
  if (isTRUE(grepl("permafrost", frg))) ev <- c(ev, "frost regime: permafrost")
  if (length(ev)) return(.rp_answer(TRUE, evidence = list(permafrost = ev), notes = paste(ev, collapse = "; ")))
  if (!is.na(frg)) return(.rp_answer(FALSE, notes = sprintf("frost regime %s: no permafrost", frg)))
  str <- .rp_site_txt(x, "soil_temperature_regime")
  if (!is.na(str) && !grepl("gelic|pergelic|cryic", str))
    return(.rp_answer(FALSE, notes = sprintf("soil temperature regime %s: no permafrost (soilKey's reading)", str)))
  kp <- .rp_site_txt(x, "koppen_climate")
  if (!is.na(kp) && !startsWith(toupper(kp), "E") && !(tolower(kp) %in% tolower(.RP_COLD_KOPPEN)))
    return(.rp_answer(FALSE, notes = sprintf("Koppen climate %s: no permafrost (soilKey's reading)", kp)))
  lat <- suppressWarnings(as.numeric(.rp_site(x, "lat")))
  elev <- suppressWarnings(as.numeric(.rp_site(x, "elevation_m")))
  if (length(lat) == 1L && !is.na(lat) && abs(lat) < 50 && length(elev) == 1L && !is.na(elev) && elev < 1500)
    return(.rp_answer(FALSE, notes = sprintf("latitude %s and elevation %s m: no permafrost (soilKey's reading)",
                                             format(lat), format(elev))))
  .rp_answer(NA, missing = c("frost_regime", "permafrost_temp_C", "ice_pct"),
             notes = "permafrost not recorded")
}

#' A layer of ice within a depth (RP 2008)
#'
#' A layer of massive ice, or ice 50 % or more, starting within `depth` cm
#' and at least `min_thickness` cm thick (cryosols histiques, p. 140; the
#' share is soilKey's reading). Not recorded: none.
#' @noRd
ice_layer_rp <- function(pedon, depth = 30, min_thickness = 30) {
  fr <- .rp_frame(pedon)
  x <- .rp_view(pedon)
  t <- .rule_threshold(depth, "depth", "max", .RP_SYSTEM)
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  hit <- vapply(seq_len(nrow(fr)), function(k) {
    i <- fr$horizon_idx[k]
    form <- .rp_txt(x, i, "ice_form"); ice <- .rp_num(x, i, "ice_pct")
    icy <- isTRUE(grepl("massive", form)) || isTRUE(ice >= 50)
    icy && isTRUE(top[k] <= t) && isTRUE(.rule_check(bot[k] - top[k], min_thickness, "depth", "min", .RP_SYSTEM))
  }, NA)
  .rp_answer(any(hit), layers = fr$horizon_idx[hit], notes = if (any(hit)) "a layer of ice" else "no layer of ice recorded")
}

#' Cryoturbation recorded (RP 2008)
#'
#' A layer with cryoturbation present, or written with -cry; a field
#' observation, absent when not recorded. With `not_recorded`, TRUE when no
#' layer records the cryoturbation either way (Hugo's decision 1 of
#' 2026-11-03: a mineral cryosol imperfect then).
#' @noRd
cryoturbation_rp <- function(pedon, not_recorded = FALSE) {
  fr <- .rp_frame(pedon)
  x <- .rp_view(pedon)
  rec <- vapply(seq_len(nrow(fr)), function(k) .rp_txt(x, fr$horizon_idx[k], "cryoturbation"), "")
  hit <- vapply(seq_len(nrow(fr)), function(k) "cry" %in% fr$features[[k]] || isTRUE(grepl("present|yes", rec[k])), NA)
  if (isTRUE(not_recorded)) {
    none <- !any(hit) && all(is.na(rec))
    return(.rp_answer(none, notes = if (none) "the cryoturbation is not recorded" else "the cryoturbation is recorded"))
  }
  .rp_answer(any(hit), layers = fr$horizon_idx[hit],
             notes = if (any(hit)) "cryoturbation" else "no cryoturbation recorded")
}

#' A horizon of a set directly under another (RP 2008)
#'
#' The layer directly under the deepest layer of `upper` is one of `codes`
#' (a BT directly under the Ach of a chernosol, p. 127). Three-valued.
#' @noRd
below_rp <- function(pedon, upper, codes) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  mu <- .rp_members(fr, upper)
  top <- .rp_tops(fr)
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[is.na(.rp_bots(fr)[ord]) | .rp_bots(fr)[ord] > 0]
  pos <- which(mu[ord] %in% TRUE)
  if (!length(pos)) return(.rp_answer(if (anyNA(mu)) NA else FALSE, notes = "no upper horizon"))
  nxt <- ord[max(pos) + 1L]
  if (is.na(nxt)) return(.rp_answer(FALSE, notes = "nothing under the upper horizons"))
  if (is.na(mu[nxt])) return(.rp_answer(NA, missing = .rp_missing_of(fr, nxt)))
  m <- .rp_members(fr, codes)[nxt]
  .rp_answer(m, layers = if (isTRUE(m)) fr$horizon_idx[nxt] else integer(0), missing = .rp_missing_of(fr, nxt),
             notes = sprintf("under the %s: %s", paste(unlist(upper), collapse = ", "), fr$code[nxt] %||% "undecided"))
}

#' A biomacrostructured A (RP 2008)
#'
#' The first layer of `codes` (the A family) is biomacrostructured (pp.
#' 10-12; .rp_biomacro()): the arenosols against the rendosols, calcosols and
#' dolomitosols (p. 116, single-grain sands).
#' @noRd
biomacro_rp <- function(pedon, codes = "A") {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  m <- .rp_members(fr, codes)
  top <- .rp_tops(fr)
  rows <- which(m %in% TRUE)
  if (!length(rows)) return(.rp_answer(if (anyNA(m)) NA else FALSE, notes = "no A horizon"))
  k <- rows[which.min(top[rows])]
  r <- .rp_biomacro(.rp_view(pedon), fr$horizon_idx[k])
  .rp_answer(r$passed, layers = if (isTRUE(r$passed)) fr$horizon_idx[k] else integer(0), missing = r$missing, notes = r$note)
}

# The run of layers from the surface (holorganic O layers above skipped)
# for which `test(k)` holds, as a thickness.
.rp_run_from_surface <- function(fr, test) {
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & (is.na(bot[ord]) | bot[ord] > 0)]
  holo <- .rp_family_members("O")
  th <- 0; rows <- integer(0); started <- FALSE
  for (k in ord) {
    if (!started && isTRUE(fr$code[k] %in% holo)) next
    started <- TRUE
    if (!isTRUE(test(k))) break
    if (is.na(bot[k])) return(list(thickness = NA_real_, rows = rows))
    th <- th + bot[k] - max(top[k], 0)
    rows <- c(rows, k)
  }
  list(thickness = th, rows = rows)
}

.rp_run_answer <- function(fr, run, min_thickness, what) {
  if (is.na(run$thickness)) return(.rp_answer(NA, missing = "bottom_cm"))
  p <- isTRUE(.rule_check(run$thickness, min_thickness, "depth", "min", .RP_SYSTEM))
  .rp_answer(p, layers = fr$horizon_idx[run$rows],
             notes = sprintf("%s over %s cm from the surface", what, format(run$thickness)),
             distance = if (!p) .rule_distance(run$thickness, min_thickness, "depth", "min", "any", .RP_SYSTEM) else NA_real_)
}

#' Colluvial material from the surface (RP 2008, Q14)
#'
#' layer_origin colluvic (or colluvium) over at least `min_thickness` cm
#' from the surface (50 +/- 10, p. 132). The origin is required: not
#' recorded, the material is not colluvial.
#' @noRd
colluvial_rp <- function(pedon, min_thickness = 50) {
  fr <- .rp_frame(pedon)
  x <- .rp_view(pedon)
  run <- .rp_run_from_surface(fr, function(k) isTRUE(grepl("colluv", .rp_txt(x, fr$horizon_idx[k], "layer_origin"))))
  .rp_run_answer(fr, run, min_thickness, "colluvial material")
}

# The kind of anthropic material of layer k: from its Z code, else
# anthropic_material; NA when none.
.rp_anthropic_kind <- function(fr, x, k) {
  cd <- fr$code[k]
  am <- .rp_txt(x, fr$horizon_idx[k], "anthropic_material")
  kinds <- c(Ztc = "technological", Ztr = "earthy", ZO = "holorganic", Zar = "archaeological")
  if (isTRUE(cd %in% names(kinds))) return(kinds[[cd]])
  if (!is.na(am)) return(am)
  if (identical(cd, "Z")) return("any")
  NA_character_
}

#' Anthropic material from the surface (RP 2008, Q15)
#'
#' Layers of anthropic material of one of `kinds` (technological, earthy,
#' holorganic, archaeological; "any" for a Z of no stated kind) from the
#' surface over at least `min_thickness` cm (50 +/- 10, 30 +/- 5, 10 +/- 5),
#' its share (anthropic_material_pct, else artefacts_pct) over `min_pct` %
#' when recorded; a layer written Z, or with anthropic_material recorded, is
#' of that material.
#' @noRd
anthropic_material_rp <- function(pedon, kinds, min_pct = NULL, min_thickness = 50) {
  fr <- .rp_frame(pedon)
  x <- .rp_view(pedon)
  kinds <- as.character(unlist(kinds))
  test <- function(k) {
    kd <- .rp_anthropic_kind(fr, x, k)
    if (is.na(kd) || !(kd %in% kinds || ("any" %in% kinds && kd == "any"))) return(FALSE)
    if (is.null(min_pct)) return(TRUE)
    i <- fr$horizon_idx[k]
    s <- .rp_num(x, i, "anthropic_material_pct")
    if (is.na(s)) s <- .rp_num(x, i, "artefacts_pct")
    is.na(s) || s >= min_pct
  }
  .rp_run_answer(fr, .rp_run_from_surface(fr, test), min_thickness,
                 sprintf("anthropic material (%s)", paste(kinds, collapse = ", ")))
}

#' Transported pedological material from the surface (RP 2008, Q15)
#'
#' Layers written with -tp (Ltp), or with anthropic_material or
#' layer_origin "transported", from the surface over at least
#' `min_thickness` cm: the anthroposols reconstitues (p. 92).
#' @noRd
transported_material_rp <- function(pedon, min_thickness = 50) {
  fr <- .rp_frame(pedon)
  x <- .rp_view(pedon)
  test <- function(k) {
    i <- fr$horizon_idx[k]
    "tp" %in% fr$features[[k]] || isTRUE(grepl("transport", .rp_txt(x, i, "anthropic_material"))) ||
      isTRUE(grepl("transport", .rp_txt(x, i, "layer_origin")))
  }
  .rp_run_answer(fr, .rp_run_from_surface(fr, test), min_thickness, "transported material")
}

#' The history of an anthroposol transforme recorded (RP 2008, Q15)
#'
#' site$rp_anthropic_history recorded (sanding, deep turning, organic
#' amendments, small terraces, levelling ...) and the transformation reaching
#' `min_depth` cm (50 +/- 10) by site$mechanical_mixing_depth_cm; never
#' inferred. The depth not recorded leaves it undecided.
#' @noRd
anthropic_history_rp <- function(pedon, min_depth = 50) {
  x <- .rp_view(pedon)
  hist <- .rp_site_txt(x, "rp_anthropic_history")
  # the construction of an anthroposol construit is no transformation
  if (!is.na(hist)) {
    parts <- trimws(strsplit(hist, ";", fixed = TRUE)[[1L]])
    parts <- parts[nzchar(parts) & !grepl("constru", parts)]
    hist <- if (length(parts)) paste(parts, collapse = "; ") else NA_character_
  }
  if (is.na(hist) || !nzchar(trimws(hist)))
    return(.rp_answer(FALSE, notes = "no history recorded (rp_anthropic_history; never inferred, Q15)"))
  d <- suppressWarnings(as.numeric(.rp_site(x, "mechanical_mixing_depth_cm")))
  if (length(d) != 1L || is.na(d))
    return(.rp_answer(NA, missing = "mechanical_mixing_depth_cm",
                      notes = sprintf("history recorded (%s); the depth of the transformation not recorded", hist)))
  p <- isTRUE(.rule_check(d, min_depth, "depth", "min", .RP_SYSTEM))
  .rp_answer(p, notes = sprintf("history %s over %s cm", hist, format(d)),
             distance = if (!p) .rule_distance(d, min_depth, "depth", "min", "any", .RP_SYSTEM) else NA_real_)
}

#' A recorded origin of the layers within a depth (RP 2008)
#'
#' layer_origin matching `pattern` in a layer starting within `depth` cm:
#' the signposts of the fluviosols and thalassosols. Not recorded: none.
#' @noRd
origin_rp <- function(pedon, pattern, depth = 50) {
  fr <- .rp_frame(pedon)
  x <- .rp_view(pedon)
  top <- .rp_tops(fr)
  hit <- vapply(seq_len(nrow(fr)), function(k)
    isTRUE(top[k] < depth) && isTRUE(grepl(pattern, .rp_txt(x, fr$horizon_idx[k], "layer_origin"))), NA)
  .rp_answer(any(hit), layers = fr$horizon_idx[hit])
}

#' A word recorded in a site field (RP 2008)
#'
#' The site attribute `field` matches `pattern` (the construction of an
#' anthroposol construit in rp_anthropic_history).
#' @noRd
recorded_rp <- function(pedon, field, pattern) {
  v <- .rp_site_txt(.rp_view(pedon), field)
  .rp_answer(isTRUE(grepl(pattern, v)), notes = if (!is.na(v)) sprintf("%s: %s", field, v) else character(0))
}


# --- the tests of the chapters of part 5 (v0.9.348) ------------------------------------------------
# The ferrallitisols and oxydisols, ferruginosols, fersialsols, fluviosols,
# grisols and gypsosols (pp. 146-200).

#' The intertropical domain (RP 2008, Q18)
#'
#' The ferrallitisols, oxydisols and ferruginosols are solums of the
#' intertropical domain, whose horizons F, OX, RT, ND, FE, OXc and OXm the
#' book calls specific to it (p. 6): declared (site$intertropical), a Koppen
#' climate of group A or a latitude between the tropics; outside, FALSE.
#' Neither recorded: a profile that writes those horizons in the RP notation
#' is read as placed in the domain by its author (soilKey's reading), any
#' other undecided.
#' @noRd
tropical_rp <- function(pedon) {
  x <- .rp_view(pedon)
  tr <- .rp_tropical(x)
  if (!is.na(tr$passed)) return(.rp_answer(tr$passed, notes = tr$note))
  fr <- .rp_frame(pedon)
  trop <- c(.rp_family_members("F"), .rp_family_members("OX"), "RT", "ND", "FE")
  wr <- nrow(fr) > 0L && any(fr$code %in% trop & fr$route %in% "notation")
  if (wr) return(.rp_answer(TRUE, notes = paste("the climate and the latitude are not recorded; the intertropical",
                                                  "horizons written in the RP notation place the profile in the domain",
                                                  "(soilKey's reading)")))
  .rp_answer(NA, missing = tr$missing, notes = tr$note)
}

# The loose horizons of the ferrallitisols and of the oxydisols (pp. 149-152):
# F and its variants (Fox, Fhy), OX and its loose variants (OXal, OXfl, OXfr,
# OXba, OXti; not the carapace OXc or the cuirasse OXm). The nodular ones
# (Fnod, OXnod) are the same codes with the feature nod.
.RP_LOOSE <- list(F = c("F", "Fox", "Fhy"), OX = c("OX=", "OXal", "OXfl", "OXfr", "OXba", "OXti"))

#' The loose horizon that makes a ferrallitisol or an oxydisol (RP 2008, D4)
#'
#' The thicker of the loose horizons F and OX within `within` cm (120, the
#' depth of the chapter, pp. 157-161) decides: `kind = "F"` holds when the F
#' are at least as thick as the OX, or when neither is present (a
#' ferrallitisol, the book's examples without F, pp. 159; default 21);
#' `kind = "OX"` when the OX are thicker. Strict: the comparison is
#' soilKey's. Undecided layers that could change the comparison leave it
#' undecided.
#' @noRd
loose_rp <- function(pedon, kind = c("F", "OX"), within = 120) {
  kind <- match.arg(kind)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  f <- .rp_thickness_bounds(fr, .RP_LOOSE$F, within = within)
  o <- .rp_thickness_bounds(fr, .RP_LOOSE$OX, within = within)
  if (is.na(f$lo) || is.na(o$lo)) return(.rp_answer(NA, missing = c("top_cm", "bottom_cm")))
  f_dom <- if (f$lo >= o$hi) TRUE else if (f$hi < o$lo) FALSE else NA
  p <- if (kind == "F") f_dom else if (is.na(f_dom)) NA else !f_dom
  .rp_answer(p, layers = fr$horizon_idx[if (kind == "F") f$rows else o$rows],
             missing = .rp_missing_of(fr, c(f$und, o$und)),
             evidence = list(F = f$lo, OX = o$lo, within = within),
             notes = sprintf("loose horizons within %s cm: F %s cm, OX %s cm%s", format(within), format(f$lo),
                             format(o$lo), if (f$lo == 0 && o$lo == 0) " (neither: a ferrallitisol, D4)" else ""))
}

# The words of an alluvial origin (fluviatile or lacustrine, p. 183), whole
# word stems; a colluvial or marine origin is none.
.RP_ALLUVIAL_WORDS <- "fluv|alluv|lacustr|riverine|floodplain deposit"

#' The three conditions of the fluviosols (RP 2008, Q14)
#'
#' The material is recent fluviatile or lacustrine alluvium (p. 183): an
#' alluvial layer_origin in a layer starting within 50 cm, or an alluvial
#' rock_origin there, or an alluvial parent material; the origin is required,
#' not recorded it is none. The low position (valley_position minor bed,
#' major bed or dried marsh, or their French words; a terrace or an alluvial
#' fan is none, pp. 183, 189; another value is not read) and the alluvial
#' water table (flood_frequency other than none, or a water table recorded:
#' groundwater_kind or groundwater_depth_cm; none only when the flooding and
#' the groundwater are both recorded as none, "none observed" being no
#' record: Hugo's decision D1) are read when recorded. `bed = "minor"`:
#' the fluviosols bruts, in the minor bed (p. 185). `site = "lenient"`: the
#' site conditions not recorded pass (the reference attaches, and the result
#' says it is provisional, from `provisional_unless`); `"required"`: they
#' leave the answer undecided.
#' @noRd
alluvial_rp <- function(pedon, bed = NULL, site = c("lenient", "required")) {
  site <- match.arg(site)
  fr <- .rp_frame(pedon)
  x <- .rp_view(pedon)
  top <- .rp_tops(fr)
  word <- function(t) !is.na(t) && grepl(.RP_ALLUVIAL_WORDS, tolower(t)) && !grepl("^colluv", tolower(t))
  org <- vapply(seq_len(nrow(fr)), function(k) {
    i <- fr$horizon_idx[k]
    isTRUE(top[k] < 50) && (word(.rp_txt(x, i, "layer_origin")) || word(.rp_txt(x, i, "rock_origin")))
  }, NA)
  pm <- .rp_site_txt(x, "parent_material")
  origin <- any(org) || word(pm)
  if (!origin)
    return(.rp_answer(FALSE, notes = "no alluvial origin recorded (layer_origin, rock_origin, parent_material; required, Q14)"))
  vp <- tolower(.rp_site_txt(x, "valley_position") %||% NA_character_)
  # the vocabulary of the attribute and its French words (lit mineur, lit
  # majeur, terrasse, cone, marais); another value is not read (undecided)
  vk <- if (is.na(vp)) NA_character_
        else if (grepl("terrace|terrasse|fan|c[o\u00f4]ne", vp)) "out"
        else if (grepl("minor|mineur", vp)) "minor"
        else if (grepl("major|majeur|flood ?plain", vp)) "major"
        else if (grepl("marsh|marais", vp)) "marsh"
        else NA_character_
  pos <- if (is.na(vk)) NA
         else if (vk == "out") FALSE
         else if (!is.null(bed) && identical(bed, "minor")) vk == "minor"
         else TRUE
  # the water table: flooding and the groundwater fail it only when both are
  # recorded as none (an embanked valley is not flooded, p. 186; a pit with
  # no water seen in the dry season is no record of none: "none observed"
  # reads as not recorded; Hugo's decision D1, 2026-11-03)
  ff <- tolower(.rp_site_txt(x, "flood_frequency") %||% NA_character_)
  gk <- tolower(.rp_site_txt(x, "groundwater_kind") %||% NA_character_)
  if (!is.na(gk) && grepl("observed|not seen|unknown", gk) && grepl("^(none|not|no)\\b", gk)) gk <- NA_character_
  gd <- suppressWarnings(as.numeric(.rp_site(x, "groundwater_depth_cm")))
  gd <- if (length(gd) == 1L) gd else NA_real_
  ff_none <- !is.na(ff) && grepl("^(none|never|no)\\b", ff)
  gw_none <- is.na(gd) && !is.na(gk) && grepl("^(none|absent|no)\\b", gk)
  wt <- if (!is.na(ff) && !ff_none) TRUE
        else if (!is.na(gd) || (!is.na(gk) && !gw_none)) TRUE
        else if (ff_none && gw_none) FALSE
        else NA
  notes <- c(sprintf("alluvial material%s", if (word(pm)) sprintf(" (%s)", pm) else ""),
             if (!is.na(vk)) sprintf("valley position %s", vp)
             else if (!is.na(vp)) sprintf("valley position %s: not a value soilKey reads (undecided)", vp)
             else "the position in the valley not recorded",
             if (!is.na(wt)) sprintf("alluvial water table or flooding %s", if (wt) "recorded" else "recorded as none")
             else "the alluvial water table and the flooding not recorded")
  cond <- .rp_and3(c(pos, wt))
  if (isFALSE(cond)) return(.rp_answer(FALSE, notes = notes))
  if (is.na(cond) && site == "lenient")
    return(.rp_answer(TRUE, notes = c(notes, "the site conditions not recorded: provisional (Q14)")))
  .rp_answer(cond, missing = c(if (is.na(pos)) "valley_position", if (is.na(wt)) c("flood_frequency", "groundwater_kind")),
             notes = notes)
}

#' The release of iron of the FS horizons (RP 2008, D14)
#'
#' In the layers of `codes`, the easily extractable iron (FFE) over the total
#' iron above 0.20 (pp. 179-180), on one basis (fe_ffe_pct / (0.6994 x
#' fe_total_pct)); a luvisol has less (p. 182). Not measured, the free over
#' total iron above 0.50 (fe_dcb_pct) is evidence only, and the field route
#' of the horizon stands (colour, structure): TRUE, noted. FALSE only when the
#' FFE is measured and too low. With `low` the other side, the BT of the
#' luvisols (p. 230; 0.9.349): FALSE only when the FFE is measured and over
#' 0.20 (the siallitic criteria only against the fersialsols, default 21).
#' @noRd
iron_release_rp <- function(pedon, codes = "FS", low = FALSE) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  m <- .rp_members(fr, codes)
  rows <- which(m %in% TRUE)
  if (!length(rows)) return(.rp_answer(if (anyNA(m)) NA else FALSE,
                                       notes = sprintf("no %s horizon", paste(unlist(codes), collapse = ", "))))
  r <- lapply(fr$horizon_idx[rows], function(i) {
    ft <- .rp_num(x, i, "fe_total_pct"); ffe <- .rp_num(x, i, "fe_ffe_pct"); fd <- .rp_num(x, i, "fe_dcb_pct")
    if (!is.na(ffe) && isTRUE(ft > 0)) { q <- ffe / (0.6994 * ft)
      list(p = if (isTRUE(low)) q <= 0.2 else q > 0.2, note = sprintf("FFE/Fe total %s", format(signif(q, 3)))) }
    else if (!is.na(fd) && isTRUE(ft > 0)) { q <- fd / (0.6994 * ft)
      list(p = TRUE, note = sprintf("FFE not measured; Fe free/Fe total %s (evidence%s)", format(signif(q, 3)),
                                    if (q > 0.5) "" else ": under 0.50")) }
    else list(p = TRUE, note = if (isTRUE(low)) "the iron release not measured: the BT of the frame stands (default 21)"
                               else "the iron release not measured: the field route of the FS (D14)")
  })
  p <- all(vapply(r, `[[`, NA, "p"))
  .rp_answer(p, layers = fr$horizon_idx[rows], notes = unique(vapply(r, `[[`, "", "note")))
}

#' Carbonates in the fine earth of a set of horizons (RP 2008)
#'
#' Each layer of `codes` is carbonated (CaCO3 2 % or more, or an
#' effervescence; .rp_carbonates()) or not; an effervescence only local
#' (effervescence_extent local) is not carbonated (p. 181: the FS calciques
#' are not effervescent, or only locally). `carbonated = TRUE`: the larger
#' part of the thickness of the set carbonated, the FS carbonates of the
#' fersialsols (p. 181; Hugo's decision D3, 2026-11-03); `carbonated =
#' FALSE`: no layer carbonated, the FS calciques. `partial = TRUE`: some
#' layers carbonated, not the larger part (the calciques imperfect, D3);
#' `where = "below"` with the shallowest layer of the set not carbonated
#' (bathycarbonate), `"above"` with it carbonated (recarbonate). Three-valued.
#' @noRd
carbonated_rp <- function(pedon, codes, carbonated = TRUE, partial = FALSE, where = c("any", "below", "above")) {
  where <- match.arg(where)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  m <- .rp_members(fr, codes)
  rows <- which(m %in% TRUE)
  what <- paste(unlist(codes), collapse = ", ")
  if (!length(rows)) return(.rp_answer(if (anyNA(m)) NA else FALSE, notes = sprintf("no %s", what)))
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  rows <- rows[order(top[rows])]
  v <- vapply(fr$horizon_idx[rows], function(i) {
    cb <- .rp_carbonates(x, i)
    if (identical(cb$extent, "local")) FALSE else cb$carbonated
  }, NA)
  th <- pmax(0, bot[rows] - pmax(top[rows], 0))
  if (anyNA(th) || sum(th) <= 0) th <- rep(1, length(rows))
  tot <- sum(th)
  lo <- sum(th[v %in% TRUE]) / tot          # surely carbonated
  hi <- sum(th[!(v %in% FALSE)]) / tot      # possibly carbonated
  most <- if (lo > 0.5) TRUE else if (hi <= 0.5) FALSE else NA
  none <- if (all(v %in% FALSE)) TRUE else if (any(v %in% TRUE)) FALSE else NA
  p <- if (isTRUE(partial)) {
    some <- if (any(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA
    pos <- switch(where, any = TRUE, below = if (is.na(v[1L])) NA else !v[1L], above = v[1L])
    .rp_and3(c(some, if (is.na(most)) NA else !most, pos))
  } else if (isTRUE(carbonated)) most else none
  .rp_answer(p, layers = if (isTRUE(p)) fr$horizon_idx[rows] else integer(0),
             missing = c("caco3_pct", "effervescence_class"),
             evidence = list(carbonated_share = lo, carbonated_share_upper = hi),
             notes = sprintf("%s: %s %% of the thickness carbonated in the fine earth%s (%s)", what,
                             format(round(100 * lo)), if (hi > lo) sprintf(" (up to %s %%)", format(round(100 * hi))) else "",
                             if (isTRUE(partial)) "some, not the larger part: D3"
                             else if (isTRUE(carbonated)) "the larger part needed: D3"
                             else "none needed, or only locally"))
}

#' The Aso of the grisols, by the chapter (RP 2008, Q17, D15)
#'
#' The first A horizon (the holorganic layers skipped) is the sombric Aso of
#' p. 193: a moist value under 4.5 and chroma under 4, 4/3 excluded, not
#' calcareous, and under permanent vegetation S/CEC 50-80 % and pH in water
#' 5.5-6.5 (their tolerances of p. 64); cultivated (an L, or a cultivated
#' land use) the complex is not tested (D19). The general definition of p. 13
#' (value under 4, value + chroma under 6, organic carbon over 1.2 %) is
#' read in the strict pass only: met by the chapter alone, the grisol is
#' imperfect (D15). An Aso, LAso or LAh written with no colour recorded is
#' the author's reading of its colour; the S/CEC, pH and carbonates recorded
#' are still read, and a written Aso found calcareous fails (the review's R4).
#' A layer with another A code (Ach, Aca, Aci ...) is
#' no Aso.
#' @noRd
sombric_rp <- function(pedon) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  mA <- .rp_members(fr, "A")
  top <- .rp_tops(fr)
  holo <- .rp_family_members("O")
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!(fr$code[ord] %in% holo) & (is.na(.rp_bots(fr)[ord]) | .rp_bots(fr)[ord] > 0)]
  if (!length(ord)) return(.rp_answer(FALSE, notes = "no mineral layer"))
  k <- ord[1L]
  if (is.na(mA[k])) return(.rp_answer(NA, missing = .rp_missing_of(fr, k), notes = "the first layer is undecided"))
  if (!isTRUE(mA[k])) return(.rp_answer(FALSE, notes = "the first mineral layer is no A"))
  cd <- fr$code[k]
  if (!cd %in% c("A", "Aso")) return(.rp_answer(FALSE, notes = sprintf("an %s, not a sombric A", cd)))
  i <- fr$horizon_idx[k]
  written <- identical(fr$route[k], "notation") && (cd == "Aso" || "h" %in% fr$features[[k]])
  col <- .rp_colour(x, i, "moist")
  v <- col$value; c1 <- col$chroma
  nocol <- is.na(v) || is.na(c1)
  if (nocol && !written) return(.rp_answer(NA, missing = col$missing, notes = "the colour of the A is not recorded"))
  if (!nocol) {
    chap <- v < 4.5 && c1 < 4 && !(v == 4 && c1 == 3)
    if (!chap) return(.rp_answer(FALSE, notes = sprintf("%s: not sombric by the chapter (value under 4.5, chroma under 4, not 4/3, p. 193)",
                                                        .rp_colour_text(col))))
  }
  nc <- .rp_not_carbonated(x, i)$passed
  if (isFALSE(nc)) return(.rp_answer(FALSE, notes = "calcareous: no Aso"))
  # written, the Aso is not calcareous unless the data say it is (recorded
  # checks only; the review's R4)
  if (written && cd != "A") nc <- TRUE
  cult <- "L" %in% fr$overlay[[k]] || isTRUE(.rp_ploughed(x, i, TRUE)$passed)
  sat <- if (cult) TRUE else {
    b <- .rp_bs(x, i)$value; ph <- .rp_num(x, i, "ph_h2o")
    ck <- c(.rp_meets(b, 50, "bs", "min"), .rp_meets(b, 80, "bs", "max"),
            .rp_meets(ph, 5.5, "ph", "min"), .rp_meets(ph, 6.5, "ph", "max"))
    # written with no colour: the values recorded are still read (R4)
    if (nocol) ck <- ck[c(!is.na(b), !is.na(b), !is.na(ph), !is.na(ph))]
    .rp_and3(ck)
  }
  if (isFALSE(sat)) return(.rp_answer(FALSE, notes = "under permanent vegetation the Aso has S/CEC 50-80 % and pH 5.5-6.5 (p. 193)"))
  if (nocol)
    return(.rp_answer(TRUE, layers = i, notes = c(sprintf("%s written, its colour not recorded", fr$designation[k]),
                                                  if (cult) "cultivated: the complex not tested (D19)")))
  gen <- TRUE
  if (!.rule_tolerant(.RP_SYSTEM)) {
    oc <- .rp_num(x, i, "oc_pct")
    gen <- v < 4 && v + c1 < 6
    gen <- if (!gen) FALSE else if (is.na(oc)) (if (written) TRUE else NA) else oc > 1.2
  }
  p <- .rp_and3(c(nc, sat, gen))
  .rp_answer(p, layers = if (isTRUE(p)) i else integer(0),
             missing = c(if (is.na(nc)) c("caco3_pct", "effervescence_class"), if (is.na(sat)) c("bs_pct", "ph_h2o"),
                         if (is.na(gen)) "oc_pct"),
             notes = c(sprintf("%s: sombric by the chapter (p. 193)", .rp_colour_text(col)),
                       if (isFALSE(gen)) "not by the general definition of p. 13 (value under 4, value + chroma under 6, organic carbon over 1.2 %): imperfect (D15)",
                       if (cult) "cultivated: the complex not tested (D19)"))
}


# --- the tests of the chapters of part 6 (v0.9.349) ------------------------------------------------
# The histosols, leptismectisols, lithosols, luvisols, nitosols and
# organosols (pp. 201-242).

# The layers and horizons the histosols allow under their H horizons (p.
# 207 and the figure of p. 209: only G, M, D or R, with thin limnic or terric
# materials), and the C, which soilKey reads as the M of a description that
# writes the weathered substrate so (soilKey's reading).
.RP_UNDER_H <- c("H", "Mli", "Mt", "G", "M", "D", "R", "C")

#' The solum of a histosol (RP 2008, pp. 207-209)
#'
#' The first H horizon (Hf, Hm, Hs, Ha or LH) starts within `cover_max` cm
#' (50 +/- 10: a cover of other materials of 50 cm at most is the qualifier
#' recouvert, more makes another reference bathyhistique, the figure of p.
#' 209), and under it there is nothing but H horizons, limnic or terric
#' materials, G horizons and M, D, R layers (p. 207; a C read as the M).
#' @noRd
histic_solum_rp <- function(pedon, cover_max = 50) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  mH <- .rp_members(fr, "H")
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & (is.na(bot[ord]) | bot[ord] > 0)]
  first <- ord[mH[ord] %in% TRUE][1L]
  if (is.na(first))
    return(.rp_answer(if (anyNA(mH[ord])) NA else FALSE, missing = .rp_missing_of(fr, ord[is.na(mH[ord])]),
                      notes = "no H horizon"))
  # a cover of 50 cm at most (an undecided layer above that could be an H
  # could only lift the first H)
  cov <- .rule_check(top[first], cover_max, "depth", "max", .RP_SYSTEM)
  above <- ord[seq_len(match(first, ord) - 1L)]
  if (!isTRUE(cov) && any(is.na(mH[above]) & top[above] <= .rule_threshold(cover_max, "depth", "max", .RP_SYSTEM)))
    cov <- NA
  below <- ord[-seq_len(match(first, ord))]
  ok <- .rp_members(fr, .RP_UNDER_H)[below]
  bad <- below[ok %in% FALSE]
  p <- .rp_and3(c(cov, ok))
  .rp_answer(p, layers = if (isTRUE(p)) fr$horizon_idx[ord[match(first, ord):length(ord)]] else integer(0),
             missing = .rp_missing_of(fr, c(below[is.na(ok)], above[is.na(mH[above])])),
             evidence = list(first_h_top = top[first], under = fr$code[below]),
             notes = c(sprintf("the first H at %s cm%s", format(top[first]),
                               if (isFALSE(cov)) sprintf(": more than %s cm of other materials over it", format(cover_max)) else ""),
                       if (length(bad)) sprintf("%s under the H horizons: only G, M, D, R and thin limnic or terric materials (p. 207)",
                                                paste(unique(fr$code[bad]), collapse = ", "))),
             distance = if (isFALSE(cov)) .rule_distance(top[first], cover_max, "depth", "max", "any", .RP_SYSTEM) else NA_real_)
}

# The kinds of H a layer of the frame may be: Hf, Hm, Hs, or Ha for the
# drained Ha and the ploughed LH, which count for no kind (map 3, D3), and
# whether the layer is surely an H of that one kind.
.rp_h_kinds <- function(fr, k, mH) {
  if (isFALSE(mH[k])) return(NULL)
  cd <- fr$code[k]
  ploughed <- "L" %in% (fr$overlay[[k]] %||% character(0))
  kinds <- if (is.na(cd)) {
    cc <- as.character(fr$candidates[[k]])
    unique(unlist(lapply(cc[cc %in% c("H", "Hf", "Hm", "Hs", "Ha")], function(c1) if (c1 == "H") .RP_KINDS$H else c1)))
  } else if (cd %in% c("Hf", "Hm", "Hs", "Ha")) cd
  else if (cd == "H") .rp_kinds_open(fr, k)
  else character(0)
  if (!length(kinds)) return(NULL)
  if (ploughed) kinds <- "Ha"
  list(kinds = kinds, sure = isTRUE(mH[k]) && length(kinds) == 1L)
}

#' The predominant H horizon of a histosol (RP 2008, pp. 207-208)
#'
#' `kind` (Hf, Hm or Hs) is the predominant H horizon: the thickest kind,
#' thicknesses cumulated, between 40 and 120 cm, or between 0 and 50 cm when
#' an M, D or R layer starts within 50 cm (p. 207); a tie is no kind truly
#' predominant (the histosols composites, p. 208); the drained Ha and the
#' ploughed LH count for no kind (map 3, D3). With `min`, the horizons of
#' that kind are more than `min` cm thick, cumulated through the solum (the
#' 60 cm of the fibriques, +/- 10; the 40 cm of the mesiques and sapriques,
#' strict; map 3, D1). The kinds read on the data by the rubbed fibres, then
#' von Post (.rp_t_H()); a kind left open leaves the test undecided when it
#' decides it. The windows are read as printed, strict.
#' @noRd
histic_kind_rp <- function(pedon, kind = c("Hf", "Hm", "Hs"), min = NULL) {
  kind <- match.arg(kind)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  mH <- .rp_members(fr, "H")
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  info <- lapply(seq_len(nrow(fr)), function(k) .rp_h_kinds(fr, k, mH))
  all_k <- c("Hf", "Hm", "Hs", "Ha")
  bounds <- function(a, b) {
    lo <- stats::setNames(rep(0, 4), all_k); hi <- lo
    for (k in seq_len(nrow(fr))) {
      z <- info[[k]]
      if (is.null(z) || is.na(top[k])) next
      th <- max(0, min(if (is.na(bot[k])) top[k] else bot[k], b) - max(top[k], a))
      if (th <= 0) next
      if (z$sure) lo[z$kinds] <- lo[z$kinds] + th
      hi[z$kinds] <- hi[z$kinds] + th
    }
    list(lo = lo, hi = hi)
  }
  pred_in <- function(a, b) {
    w <- bounds(a, b)
    oth <- setdiff(all_k, kind)
    if (w$hi[[kind]] <= 0 || w$hi[[kind]] <= max(w$lo[oth])) return(FALSE)
    if (w$lo[[kind]] > max(w$hi[oth])) return(TRUE)
    NA
  }
  # the window: 0-50 cm when an M, D or R starts within 50 cm
  mdr <- .rp_members(fr, c("M", "D", "R"))
  shallow <- .rp_or3(ifelse(!is.na(top) & top < 50, mdr, FALSE))
  pr <- if (isTRUE(shallow)) pred_in(0, 50) else if (isFALSE(shallow)) pred_in(40, 120)
        else { a <- pred_in(0, 50); b <- pred_in(40, 120); if (identical(a, b)) a else NA }
  th <- NA
  tot <- bounds(-Inf, Inf)
  if (!is.null(min)) {
    th <- if (isTRUE(.rp_meets(tot$lo[[kind]], min, "depth", "min", open = TRUE))) TRUE
          else if (isFALSE(.rp_meets(tot$hi[[kind]], min, "depth", "min", open = TRUE))) FALSE else NA
  }
  p <- .rp_and3(c(pr, if (!is.null(min)) th))
  und <- which(vapply(info, function(z) !is.null(z) && !z$sure, NA))
  .rp_answer(p, layers = if (isTRUE(p)) fr$horizon_idx[vapply(info, function(z) !is.null(z) && identical(z$kinds, kind), NA)] else integer(0),
             missing = c(.rp_missing_of(fr, und), if (length(und)) c("fiber_content_rubbed_pct", "von_post_index")),
             evidence = list(kind = kind, window = if (isTRUE(shallow)) "0-50" else "40-120", thickness = tot$lo[[kind]],
                             thickness_upper = tot$hi[[kind]]),
             notes = sprintf("%s %s predominant between %s cm%s", kind,
                             if (isTRUE(pr)) "is" else if (isFALSE(pr)) "is not" else "may be",
                             if (isTRUE(shallow)) "0 and 50" else "40 and 120",
                             if (!is.null(min)) sprintf("; %s cm of %s in the solum (more than %s needed)",
                                                        format(tot$lo[[kind]]), kind, format(min)) else ""),
             distance = if (isFALSE(th)) .rule_distance(tot$hi[[kind]], min, "depth", "min", "any", .RP_SYSTEM) else NA_real_)
}

#' The texture contrast of a luvisol (RP 2008, pp. 225-226, 230, 401)
#'
#' The clay of the E (the mean of the E layers weighted by their thickness;
#' with no E, the ploughed horizon LA or LE, map 3 D12) against the clay of
#' the most clayey BT (the lexicon, p. 401), by the bands of the chapter: with
#' an E under 10 % clay the BT has 3 points more (neoluvisols) or 8 (luvisols
#' typiques); 10 to 30 %, the index 1.3 to 1.8 or over 1.8, the index of the
#' lexicon: the most clayey BT over the poorest E or Ae (Hugo's decision 4,
#' 2026-11-04); over 30 %, 9 or 24 points more. `level`: "neo" the band of
#' the neoluvisols (at least their minimum, under the typiques'), "typique"
#' the typiques', "dernique" the index of the lexicon over 1.8 with no bands
#' (p. 227), "luvic" at least the neoluvisols' (the luvisols
#' against the brunisols, p. 230: over 1.3), "flat" under it (no contrast
#' left: the ploughed horizon of a luvisol tronque, p. 227, D12). Strict
#' (clay thresholds are not in the table of p. 64, Q5); the layers of the
#' first material only (one parent material, p. 221).
#' @noRd
luvic_contrast_rp <- function(pedon, level = c("neo", "typique", "luvic", "flat", "dernique"), first_material = TRUE) {
  level <- match.arg(level)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  keep <- if (isTRUE(first_material)) .rp_first_material(fr) else rep(TRUE, nrow(fr))
  mE <- .rp_members(fr, "E"); mB <- .rp_members(fr, "BT")
  mE[!keep] <- FALSE; mB[!keep] <- FALSE
  up <- which(mE %in% TRUE)
  what <- "E"
  if (!length(up)) {
    if (anyNA(mE)) return(.rp_answer(NA, missing = .rp_missing_of(fr, which(is.na(mE))), notes = "an undecided layer could be the E"))
    mL <- .rp_members(fr, c("L+", "L")); mL[!keep] <- FALSE
    up <- which(mL %in% TRUE & !(mB %in% TRUE))
    what <- "ploughed horizon"
  }
  lo <- which(mB %in% TRUE)
  if (!length(up) || !length(lo))
    return(.rp_answer(if (anyNA(mB)) NA else FALSE, missing = .rp_missing_of(fr, which(is.na(mB))),
                      notes = "no E (or ploughed horizon) and BT for the texture contrast"))
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  ce <- vapply(fr$horizon_idx[up], function(i) .rp_clay(x, i), 0)
  cb <- vapply(fr$horizon_idx[lo], function(i) .rp_clay(x, i), 0)
  th <- pmax(0, bot[up] - pmax(top[up], 0))
  if (anyNA(ce) || all(is.na(cb)) || anyNA(th) || sum(th) <= 0)
    return(.rp_answer(NA, missing = "clay_pct", notes = "the clay of the E or of the BT is not recorded"))
  e <- sum(ce * th) / sum(th); b <- max(cb, na.rm = TRUE)
  if (e <= 0) return(.rp_answer(NA, missing = "clay_pct", notes = "no clay in the E: no index"))
  # the index of the lexicon (p. 401): the most clayey BT over the poorest
  # of the E and Ae layers (Hugo's decision 4 on the review, 2026-11-04)
  ae <- if (what == "E") which(.rp_members(fr, "Ae") %in% TRUE & keep) else integer(0)
  cmin <- suppressWarnings(min(c(ce, vapply(fr$horizon_idx[ae], function(i) .rp_clay(x, i), 0)), na.rm = TRUE))
  if (!is.finite(cmin) || cmin <= 0) return(.rp_answer(NA, missing = "clay_pct", notes = "no clay in the E: no index"))
  d <- b - e; idt <- b / cmin
  neo_min <- if (e < 10) d >= 3 else if (e <= 30) idt >= 1.3 else d >= 9
  typ <- if (e < 10) d >= 8 else if (e <= 30) idt > 1.8 else d >= 24
  p <- switch(level, neo = neo_min && !typ, typique = typ, luvic = neo_min, flat = !neo_min, dernique = idt > 1.8)
  band <- if (level == "dernique") sprintf("index %s, BT over the poorest E or Ae (over 1.8 dernique, p. 227)", format(signif(idt, 3)))
          else if (e < 10) sprintf("E clay under 10 %%: BT %s points more (3 neo, 8 typique)", format(signif(d, 3)))
          else if (e <= 30) sprintf("E clay 10 to 30 %%: index %s, BT over the poorest E or Ae (1.3 to 1.8 neo, over 1.8 typique)", format(signif(idt, 3)))
          else sprintf("E clay over 30 %%: BT %s points more (9 neo, 24 typique)", format(signif(d, 3)))
  .rp_answer(p, layers = if (isTRUE(p)) fr$horizon_idx[c(up, lo)] else integer(0),
             evidence = list(e_clay = e, bt_clay = b, idt = idt, e_min = cmin, upper = what),
             notes = sprintf("%s clay %s %%, BT %s %%: %s", what, format(signif(e, 3)), format(b), band))
}

#' A lithological discontinuity between two sets of horizons (RP 2008)
#'
#' A layer of `lower` lies in a later material (a higher Roman numeral) than
#' the layers of `upper` above it: the E over II BT of the quasi-luvisols (p.
#' 227). Not recorded (no Roman numeral), none.
#' @noRd
discontinuity_rp <- function(pedon, upper, lower) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  lith <- suppressWarnings(as.integer(fr$lith %||% rep(1L, nrow(fr))))
  lith[is.na(lith)] <- 1L
  mu <- .rp_members(fr, upper); ml <- .rp_members(fr, lower)
  top <- .rp_tops(fr)
  hit <- vapply(seq_len(nrow(fr)), function(k) {
    if (!isTRUE(ml[k])) return(if (is.na(ml[k])) NA else FALSE)
    u <- which(!(mu %in% FALSE) & !is.na(top) & top < top[k])
    if (!length(u)) return(FALSE)
    v <- ifelse(mu[u] %in% TRUE, lith[u] < lith[k], NA)
    .rp_or3(v)
  }, NA)
  p <- .rp_or3(hit)
  .rp_answer(p, layers = fr$horizon_idx[hit %in% TRUE],
             notes = if (isTRUE(p)) sprintf("%s in a later material than the %s above it", paste(unlist(lower), collapse = ", "),
                                            paste(unlist(upper), collapse = ", "))
                     else "no lithological discontinuity recorded between them")
}

# The organic horizons of the organosols (OF, OH, OHta and the hemiorganic
# A, Aho; the OL is not counted, p. 238) and the organo-mineral horizons they
# tolerate thin (E, S, BP ..., p. 239).
.RP_ORGANIC <- c("OF", "OH", "OHta", "Aho")
.RP_ORGANO_MINERAL <- c("A", "E", "S", "BT", "BP", "FS", "F", "OX", "FE", "V", "Js", "Jp", "L")

#' The organic horizons of an organosol dominate (RP 2008, p. 239)
#'
#' The organo-mineral horizons (E, S, BP, an A that is not hemiorganic ...)
#' are `max_mineral` cm thick at most in all (10, +/- 5: the book's example
#' of an S of 10 cm tolerated, of 15 cm attached imperfectly), and the
#' organic horizons [OF + OH + OHta + Aho] at least `ratio` times as thick
#' (twice: an S of 10 cm needs 20 cm of OF and OH, p. 239; strict); an S
#' over `s_max` cm fails, strict (a brunisol, p. 240; Hugo's decision 9).
#' @noRd
organic_dominance_rp <- function(pedon, max_mineral = 10, ratio = 2, s_max = 10) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  mo <- .rp_members(fr, .RP_ORGANIC)
  mm <- .rp_members(fr, .RP_ORGANO_MINERAL)
  mm <- ifelse(mo %in% TRUE, FALSE, ifelse(is.na(mo) & mm %in% TRUE, NA, mm))
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  th <- pmax(0, bot - pmax(top, 0))
  if (any(is.na(th[!(mo %in% FALSE) | !(mm %in% FALSE)])))
    return(.rp_answer(NA, missing = c("top_cm", "bottom_cm")))
  m_lo <- sum(th[mm %in% TRUE]); m_hi <- m_lo + sum(th[is.na(mm)])
  o_lo <- sum(th[mo %in% TRUE]); o_hi <- o_lo + sum(th[is.na(mo)])
  t1 <- if (isTRUE(.rule_check(m_hi, max_mineral, "depth", "max", .RP_SYSTEM))) TRUE
        else if (isFALSE(.rule_check(m_lo, max_mineral, "depth", "max", .RP_SYSTEM))) FALSE else NA
  t2 <- if (o_lo >= ratio * m_hi) TRUE else if (o_hi < ratio * m_lo) FALSE else NA
  # an S over 10 cm makes a brunisol (p. 240), strict (Hugo's decision 9 on
  # the review, 2026-11-04)
  sb <- .rp_thickness_bounds(fr, "S=")
  t3 <- if (is.na(sb$lo)) NA else if (sb$lo > s_max) FALSE else if (sb$hi > s_max) NA else TRUE
  p <- .rp_and3(c(t1, t2, t3))
  .rp_answer(p, missing = .rp_missing_of(fr, which(is.na(mm) | is.na(mo))),
             evidence = list(organo_mineral = m_lo, organic = o_lo),
             notes = sprintf("organo-mineral horizons %s cm (%s at most), organic [OF + OH + OHta + Aho] %s cm (%s times as thick needed)",
                             format(m_lo), format(max_mineral), format(o_lo), format(ratio)),
             distance = if (isFALSE(t1)) .rule_distance(m_lo, max_mineral, "depth", "max", "any", .RP_SYSTEM) else NA_real_)
}

#' The dominant clay mineral recorded (RP 2008)
#'
#' The layers of `codes` are not recorded with another dominant clay mineral
#' than `mineral` (clay_mineralogy; or its share, smectite_pct,
#' halloysite_pct, kaolinite_pct, 50 % or less): the smectites of the
#' leptismectisols (p. 217). Not recorded, the vertic horizons stand for it
#' and the test passes, noted (soilKey's reading); FALSE when recorded
#' otherwise.
#' @noRd
clay_mineral_rp <- function(pedon, codes, mineral = c("smectite", "halloysite", "kaolinite")) {
  mineral <- match.arg(mineral)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  m <- .rp_members(fr, codes)
  rows <- which(m %in% TRUE)
  att <- paste0(mineral, "_pct")
  rec <- vapply(fr$horizon_idx[rows], function(i) {
    cm <- .rp_txt(x, i, "clay_mineralogy"); sh <- .rp_num(x, i, att)
    if (!is.na(sh)) sh > 50 else if (!is.na(cm)) identical(cm, mineral) else NA
  }, NA)
  if (any(rec %in% FALSE))
    return(.rp_answer(FALSE, notes = sprintf("a clay mineral other than %s recorded dominant", mineral)))
  if (any(rec %in% TRUE)) return(.rp_answer(TRUE, layers = fr$horizon_idx[rows[rec %in% TRUE]],
                                            notes = sprintf("%s dominant, recorded", mineral)))
  .rp_answer(TRUE, notes = sprintf("the clay mineralogy not recorded: the %s not tested (soilKey's reading)", mineral))
}

#' The obstacle of a lithosol (RP 2008, pp. 219, 238; the review of 0.9.349)
#'
#' The first layer of `codes` starts at `depth` cm or less (10, +/- 5; Hugo's
#' decision 1: "at 10 cm or less"), the OL not counted, with at most
#' `max_above` horizon over it (decision 2: "un horizon ... de moins de 10
#' cm"); the holorganic O horizons over it count as one horizon, the
#' [OF + OH] of a lithosol holorganique (p. 241).
#' @noRd
lithic_obstacle_rp <- function(pedon, codes, depth = 10, max_above = 1) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  m <- .rp_members(fr, codes, strict_pos = .rp_strict_positive())
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & (is.na(bot[ord]) | bot[ord] > 0)]
  k <- ord[m[ord] %in% TRUE][1L]
  if (is.na(k)) return(.rp_answer(if (anyNA(m[ord])) NA else FALSE, missing = .rp_missing_of(fr, ord[is.na(m[ord])]),
                                  notes = "no obstacle"))
  w <- .rule_check(top[k], depth, "depth", "max", .RP_SYSTEM)
  above <- ord[seq_len(match(k, ord) - 1L)]
  holo <- c(.rp_family_members("O"), .rp_family_members("H"))
  n_above <- sum(!(fr$code[above] %in% holo)) + any(fr$code[above] %in% holo)
  und <- above[is.na(m[above])]
  p <- .rp_and3(c(w, n_above <= max_above))
  if (isFALSE(w) && length(und)) p <- NA
  .rp_answer(p, layers = if (isTRUE(p)) fr$horizon_idx[k] else integer(0), missing = .rp_missing_of(fr, und),
             evidence = list(obstacle = fr$code[k], top = top[k], above = length(above)),
             notes = sprintf("%s at %s cm, %d horizon(s) over it%s", fr$code[k], format(top[k]), n_above,
                             if (n_above > max_above) sprintf(" (at most %d, p. 219)", max_above) else ""),
             distance = if (isFALSE(w)) .rule_distance(top[k], depth, "depth", "max", "any", .RP_SYSTEM) else NA_real_)
}

#' A horizon of a set under another within a depth (RP 2008)
#'
#' A layer of `codes` starts below the top of the first layer of `upper` and
#' within `within` cm: the horizons of the tropical chapters under a cuirasse
#' or carapace (Hugo's decision 3 on the review of 0.9.349, pp. 159-161,
#' 219-220).
#' @noRd
under_rp <- function(pedon, upper, codes, within = NULL) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  mu <- .rp_members(fr, upper); m <- .rp_members(fr, codes)
  top <- .rp_tops(fr)
  u <- which(mu %in% TRUE & !is.na(top))
  if (!length(u)) return(.rp_answer(if (anyNA(mu)) NA else FALSE, notes = "no upper horizon"))
  t0 <- min(top[u])
  lim <- if (is.null(within)) Inf else within
  inwin <- !is.na(top) & top > t0 & top < lim
  v <- ifelse(inwin, m, FALSE)
  p <- .rp_or3(v)
  .rp_answer(p, layers = fr$horizon_idx[v %in% TRUE], missing = .rp_missing_of(fr, which(is.na(v))),
             notes = sprintf("%s under the %s%s", paste(unlist(codes), collapse = ", "), paste(unlist(upper), collapse = ", "),
                             if (isTRUE(p)) "" else ": none"))
}

#' The BT of a luvisol is not fersiallitic (RP 2008, p. 230)
#'
#' When recorded, a hue of 5YR or redder or a micro-polyhedral structure (a
#' fine or very fine, strong angular or subangular blocky structure, or
#' written micropolyedrique) in a layer of `codes` fails it: the fersialsols
#' eluviques (Hugo's decision 6 on the review of 0.9.349). Not recorded, it
#' passes (the FFE is read by iron_release_rp()).
#' @noRd
siallitic_rp <- function(pedon, codes = "BT") {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  rows <- which(.rp_members(fr, codes) %in% TRUE)
  bad <- character(0)
  for (k in rows) {
    i <- fr$horizon_idx[k]
    col <- .rp_colour(x, i)
    if (isTRUE(.rp_hue_in(col, -10, 15))) bad <- c(bad, sprintf("%s: 5YR or redder", .rp_colour_text(col)))
    st <- .kidpr_structure(x, i)
    micro <- isTRUE(grepl("micropoly|micro-poly", tolower(st$type))) ||
      (isTRUE(grepl("angular|polyhed|block", st$type)) && isTRUE(.field_grade_class(st$grade) >= 3L) &&
         isTRUE(grepl("^(very fine|fine|tres fin|fin)", tolower(st$size))))
    if (micro) bad <- c(bad, sprintf("a micro-polyhedral structure (%s)", paste(stats::na.omit(c(st$grade, st$size, st$type)), collapse = " ")))
  }
  p <- !length(bad)
  .rp_answer(p, layers = if (p) fr$horizon_idx[rows] else integer(0),
             notes = if (p) "the BT not fersiallitic by its colour or structure (not recorded: not tested)"
                     else sprintf("the BT fersiallitic: %s (p. 230)", paste(unique(bad), collapse = "; ")))
}
