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
    # the overlays written with the code must be there: the L of LAh or LS,
    # and (0.9.352) the Na and Sa of the sodic and salic horizons (NaBT,
    # SaA: the sodisols solonetziques, p. 297)
    else list(codes = .rp_family_members(r$code), features = setdiff(r$features, "*"),
              overlay_req = intersect(r$overlay, c("L", "Na", "Sa")))
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
  # a ploughed horizon written with its L (LAh, LS) must be ploughed; a
  # sodic or salic one written with its Na or Sa (NaBT, SaA) must be so
  req <- spec[["overlay_req"]]
  if (length(req) && !all(req %in% fr$overlay[[k]])) {
    if (all(req %in% c(fr$overlay[[k]], fr$overlay_na[[k]] %||% character(0))) &&
        !isFALSE(.rp_member1(fr, k, spec[c("codes", "features")], strict_pos))) return(NA)
    return(FALSE)
  }
  if (!is.na(code)) {
    if (!code %in% spec[["codes"]]) {
      # a peyric X read on the data whose fine earth also holds an E, BP, FS
      # or G (its candidates) serves the tests of those horizons: the
      # peyrosol-podzosol, -fersialsol and -reductisol doubles (p. 253; Hugo's
      # decisions 6 and 9 on the review of 0.9.351, 2026-11-04)
      if (code %in% .RP_X_CODES && .rp_x_from_surface(fr, k)) {
        xc <- intersect(as.character(fr$candidates[[k]]), .rp_x_serves())
        if (length(xc) && any(xc %in% spec[["codes"]])) {
          if (length(spec[["features"]]) && !all(spec[["features"]] %in% c(feats, fr$features_na[[k]] %||% character(0))))
            return(FALSE)
          return(if (!length(spec[["features"]]) || all(spec[["features"]] %in% feats)) TRUE else NA)
        }
      }
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

# The peyric codes, and the codes a peyric X of the data may also serve
# (0.9.351): the E, BP, FS and G families.
.RP_X_CODES <- c("X", "Xc", "Xp", "Xgr")
.rp_x_serves <- function() {
  v <- .rp_spec_memo$x_serves
  if (is.null(v)) {
    v <- unique(c(.rp_family_members("E"), .rp_family_members("BP"), "FS", .rp_family_members("G")))
    assign("x_serves", v, envir = .rp_spec_memo)
  }
  v
}

# Is layer k in the run of peyric X layers from the surface (the holorganic
# layers above skipped)? Only there does an X serve another horizon's tests:
# the context of the peyrosols (p. 253).
.rp_x_from_surface <- function(fr, k) {
  top <- as.numeric(fr$top_rp %||% fr$top_cm)
  if (is.na(top[k])) return(FALSE)
  holo <- c(.rp_family_members("O"), .rp_family_members("H"))
  above <- which(!is.na(top) & top < top[k] & !(fr$code %in% holo))
  all(fr$code[above] %in% .RP_X_CODES)
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
    # the exchangeable Al over the sum of the bases, Al/S (the veracrisols
    # under heath, p. 314; 0.9.352)
    al_bases = {
      al <- .rp_num(x, i, "al_kcl_cmol"); if (is.na(al)) al <- .rp_num(x, i, "al_cmol")
      b <- vapply(c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol"), function(a) .rp_num(x, i, a), 0)
      if (is.na(al) || anyNA(b)) NA_real_ else if (sum(b) > 0) al / sum(b) else Inf
    },
    .rp_num(x, i, attribute))
}

.rp_value_attrs <- function(attribute) {
  switch(attribute, bs = c("bs_pct", "cec_cmol"), ca_mg = c("ca_cmol", "mg_cmol"),
         sum_bases = c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol"),
         caco3_mgco3 = c("caco3_pct", "mgco3_pct"), camg_sat = c("ca_cmol", "mg_cmol", "cec_cmol"),
         al_bases = c("al_kcl_cmol", "ca_cmol", "mg_cmol", "k_cmol", "na_cmol"), attribute)
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
#' Strict: 1.3 is soilKey's boundary of the chernosols (p. 127). With
#' `adjacent`, only the run of `lower` layers directly under the upper ones
#' (a thin E between them skipped): the Sh or BTh of the phaeosols, not a
#' deeper non-humic BT of an older phase (p. 256; 0.9.351).
#' @noRd
idt_rp <- function(pedon, upper, lower, min = NULL, max = NULL, open = FALSE, adjacent = FALSE) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  mu <- .rp_members(fr, upper); ml <- .rp_members(fr, lower)
  ru <- which(mu %in% TRUE); rl <- which(ml %in% TRUE)
  if (isTRUE(adjacent) && length(ru) && length(rl)) {
    tp <- .rp_tops(fr); ord <- order(ifelse(is.na(tp), Inf, tp)); ord <- ord[!is.na(tp[ord])]
    j <- max(match(ru, ord)) + 1L
    mE <- .rp_members(fr, "E")
    while (j <= length(ord) && isTRUE(mE[ord[j]]) && !isTRUE(ml[ord[j]])) j <- j + 1L
    run <- integer(0)
    while (j <= length(ord) && isTRUE(ml[ord[j]])) { run <- c(run, ord[j]); j <- j + 1L }
    rl <- run
  }
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
below_rp <- function(pedon, upper, codes, layer = c("deepest", "any")) {
  layer <- match.arg(layer)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  mu <- .rp_members(fr, upper)
  top <- .rp_tops(fr)
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[is.na(.rp_bots(fr)[ord]) | .rp_bots(fr)[ord] > 0]
  pos <- which(mu[ord] %in% TRUE)
  if (!length(pos)) return(.rp_answer(if (anyNA(mu)) NA else FALSE, notes = "no upper horizon"))
  # `layer = "any"`: some layer of `upper`, not only the deepest, has a layer
  # of `codes` directly under it (the E over the BP of the podzosols under a
  # deeper E, 0.9.351)
  if (layer == "any") {
    mc <- .rp_members(fr, codes)
    v <- vapply(seq_along(ord)[-length(ord)], function(j)
      .rp_and3(c(mu[ord[j]], mc[ord[j + 1L]])), NA)
    p <- .rp_or3(v)
    return(.rp_answer(p, layers = fr$horizon_idx[ord[which(v %in% TRUE) + 1L]],
                      missing = .rp_missing_of(fr, ord[c(which(is.na(v)), which(is.na(v)) + 1L)]),
                      notes = sprintf("%s directly under a %s: %s", paste(unlist(codes), collapse = ", "),
                                      paste(unlist(upper), collapse = ", "),
                                      if (isTRUE(p)) "yes" else if (isFALSE(p)) "no" else "undecided")))
  }
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
# The words of a marine or fluvio-marine origin (the thalassosols, p. 303:
# marine or fluvio-marine alluvium, estuaries and deltas, the slikke and the
# schorre, the bri and the tangue of the French coasts; 0.9.352).
.RP_MARINE_WORDS <- "(?<![a-z])marin|(?<!non-)(?<!non )(?<![a-z])(?:inter|supra)?tidal|(?<![a-z])estuar|(?<![a-z])slikke|(?<![a-z])schorre|(?<![a-z])tangue|\\bbri\\b|(?<![a-z])mangrove|(?<![a-z])wadden|salt marsh|(?<![a-z])mudflat"
# (perl regular expressions: word starts, "non-tidal" and "submarine" are
# none; 0.9.352, the review's D10)

#' The three conditions of the fluviosols (RP 2008, Q14)
#'
#' The material is recent fluviatile or lacustrine alluvium (p. 183): an
#' alluvial layer_origin in a layer starting within 50 cm, or an alluvial
#' rock_origin there, or an alluvial parent material; the origin is required,
#' not recorded it is none; a marine or fluvio-marine one is the
#' thalassosols' (0.9.352). The low position (valley_position minor bed,
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
  # a marine or fluvio-marine origin is the thalassosols' (p. 303; the
  # rapid key 5a by the parent material, p. 67; 0.9.352), not an alluvium
  # of the fluviosols
  word <- function(t) !is.na(t) && grepl(.RP_ALLUVIAL_WORDS, tolower(t)) && !grepl("^colluv", tolower(t)) &&
    !grepl(.RP_MARINE_WORDS, tolower(t), perl = TRUE)
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
#' [OF + OH] of a lithosol holorganique (p. 241). The regosols read the
#' same test on their M, D or C (0.9.352; `page` 291), with `window`: an
#' undecided layer deeper than the depth is no obstacle, and `above_codes`:
#' the horizon over it organo-mineral or holorganic.
#' @noRd
lithic_obstacle_rp <- function(pedon, codes, depth = 10, max_above = 1, page = "219", window = FALSE, above_codes = NULL) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  m <- .rp_members(fr, codes, strict_pos = .rp_strict_positive())
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & (is.na(bot[ord]) | bot[ord] > 0)]
  k <- ord[m[ord] %in% TRUE][1L]
  holo <- c(.rp_family_members("O"), .rp_family_members("H"))
  # the horizons over a layer: the holorganic O and H as one, and adjacent
  # layers of one code as one horizon (A1/A2 written A/A; 0.9.352, the
  # review's D7)
  n_of <- function(a) {
    cd <- fr$code[a]; mn <- a[!(cd %in% holo)]; cm <- fr$code[mn]
    runs <- if (length(mn)) sum(c(TRUE, is.na(cm[-1L]) | is.na(cm[-length(cm)]) | cm[-1L] != cm[-length(cm)])) else 0L
    as.integer(runs + any(cd %in% holo))
  }
  n_over <- function(j) n_of(ord[seq_len(match(j, ord) - 1L)])
  # `window`: an undecided layer starting deeper than the depth (with its
  # tolerance), or under more horizons than allowed, cannot be the obstacle
  keep_und <- function(u) if (!isTRUE(window)) u else
    u[top[u] <= .rule_threshold(depth, "depth", "max", .RP_SYSTEM) & vapply(u, n_over, 0L) <= max_above]
  und_k <- keep_und(ord[is.na(m[ord])])
  if (is.na(k)) return(.rp_answer(if (length(und_k)) NA else FALSE, missing = .rp_missing_of(fr, und_k),
                                  notes = "no obstacle"))
  w <- .rule_check(top[k], depth, "depth", "max", .RP_SYSTEM)
  above <- ord[seq_len(match(k, ord) - 1L)]
  n_above <- n_of(above)
  und <- above[is.na(m[above])]
  und <- keep_und(und)
  # `above`: the horizon over the obstacle is one of these (the
  # organo-mineral or holorganic horizon of the regosols, p. 291)
  kind_ok <- if (is.null(above_codes) || !length(above)) TRUE else .rp_and3(.rp_members(fr, above_codes)[above])
  p <- .rp_and3(c(w, n_above <= max_above, kind_ok))
  if (isFALSE(w) && length(und)) p <- NA
  .rp_answer(p, layers = if (isTRUE(p)) fr$horizon_idx[k] else integer(0), missing = .rp_missing_of(fr, und),
             evidence = list(obstacle = fr$code[k], top = top[k], above = length(above)),
             notes = sprintf("%s at %s cm, %d horizon(s) over it%s", fr$code[k], format(top[k]), n_above,
                             if (n_above > max_above) sprintf(" (at most %d, p. %s)", max_above, page) else ""),
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


# --- the tests of the chapters of part 7 (v0.9.351) ------------------------------------------------
# The pelosols, peyrosols, phaeosols, planosols, podzosols, rankosols,
# reductisols and redoxisols (pp. 243-290).

# The hydromorphic horizons (pp. 280-281): the reductic G (Gr, Go, and an A
# with reductic characters, AG) and the redox g or -g; the ferric Fe, Fem
# and Femp that the redistribution of the iron forms inside a run of them
# (p. 281) continue the run (soilKey's reading).
.RP_REDUCTIC <- c("G", "AG")
.RP_REDOX <- c("g", "-g")
.RP_REDOX_RUN <- c("Fe", "Fem", "Femp")

# The membership of every layer of the frame in the reductic and the
# functional redox horizons: a redox feature recorded relict (redox_relict,
# Q13) is not functional, a reductic one always is (p. 281).
.rp_hydro_members <- function(fr, x) {
  mG <- .rp_members(fr, .RP_REDUCTIC)
  mg <- .rp_members(fr, .RP_REDOX)
  relict <- vapply(fr$horizon_idx, function(i) isTRUE(.rp_lgl(x, i, "redox_relict")), NA)
  mg[relict & !(mG %in% TRUE)] <- FALSE
  list(G = mG, g = mg, relict = relict, run = .rp_members(fr, .RP_REDOX_RUN))
}

#' The hydromorphy of a solum (RP 2008, pp. 279-284; Q8, Q13, Q17)
#'
#' The first functional horizon of `kind` (G, the reductic horizons; g, the
#' redox g or -g, over 5 % redox features, Q17; any, either) starts within
#' `within` cm (50 +/- 10), and with `min` the reductic or redox features,
#' always functional, continue over at least `min` cm (50 +/- 10): the
#' longest run of adjacent G, g and -g layers starting within the window
#' (the ferric Fe, Fem and Femp inside it counted; soilKey's reading). A
#' redox feature recorded relict (redox_relict) is not functional (Q13); a
#' drained site keeps the attachment, noted (drain\u00e9, Q13). A run that
#' reaches the bottom of the description short of `min` leaves the answer
#' undecided (the description stops).
#' @noRd
hydromorphy_rp <- function(pedon, kind = c("any", "G", "g"), within = NULL, min = NULL) {
  kind <- match.arg(kind)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  hm <- .rp_hydro_members(fr, x)
  anyh <- vapply(seq_len(nrow(fr)), function(k) .rp_or3(c(hm$G[k], hm$g[k])), NA)
  mk <- switch(kind, any = anyh, G = hm$G, g = hm$g)
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & (is.na(bot[ord]) | bot[ord] > 0)]
  t_to <- if (is.null(within)) Inf else .rule_threshold(within, "depth", "max", .RP_SYSTEM)
  edge <- 1e-9 * max(1, abs(t_to))
  inwin <- if (is.infinite(t_to)) rep(TRUE, nrow(fr))
           else if (!is.null(within) && t_to != within) top <= t_to + edge else top < t_to
  v <- vapply(seq_len(nrow(fr)), function(k) .rp_and3(c(mk[k], inwin[k])), NA)
  win <- .rp_or3(v[ord])
  sel <- function(w) ord[w[ord] %in% TRUE]
  first_k <- function(m) { r <- sel(m); if (length(r)) top[r[1L]] else NA_real_ }
  ev <- list(g_top = first_k(hm$g), G_top = first_k(hm$G))
  p_run <- NA; lo_best <- 0; hi_best <- 0
  if (!is.null(min)) {
    member <- vapply(seq_len(nrow(fr)), function(k) .rp_or3(c(anyh[k], hm$run[k])), NA)
    th <- pmax(0, bot - pmax(top, 0))
    # the longest run of adjacent members starting within the window: sure
    # (members only) and possible (members or undecided)
    runs <- function(ok) {
      blocks <- list(); cur <- NULL
      for (j in seq_along(ord)) {
        k <- ord[j]
        if (isTRUE(ok[k])) {
          if (is.null(cur)) cur <- list(start = k, th = 0, last = j)
          cur$th <- cur$th + (if (is.na(th[k])) 0 else th[k]); cur$last <- j
        } else if (!is.null(cur)) { blocks[[length(blocks) + 1L]] <- cur; cur <- NULL }
      }
      if (!is.null(cur)) blocks[[length(blocks) + 1L]] <- cur
      keep <- Filter(function(b) isTRUE(inwin[b$start]), blocks)
      list(best = if (length(keep)) max(vapply(keep, `[[`, 0, "th")) else 0,
           at_bottom = any(vapply(keep, function(b) b$last == length(ord), NA)))
    }
    sure <- runs(member %in% TRUE & !is.na(member))
    poss <- runs(!(member %in% FALSE))
    lo_best <- sure$best; hi_best <- poss$best
    t_min <- min
    p_run <- if (isTRUE(.rule_check(lo_best, t_min, "depth", "min", .RP_SYSTEM)) && lo_best > 0) TRUE
             else if (isFALSE(.rule_check(hi_best, t_min, "depth", "min", .RP_SYSTEM)) && !poss$at_bottom) FALSE
             else NA
    ev$run <- lo_best
  }
  p <- .rp_and3(c(win, if (!is.null(min)) p_run))
  drained <- isTRUE(.rp_site_lgl(x, "artificially_drained"))
  rel <- fr$code[hm$relict & !(hm$G %in% TRUE)]
  what <- switch(kind, any = "reductic or redox features", G = "a reductic G", g = "a redox g or -g")
  dist <- NA_real_
  if (isFALSE(win) && !is.null(within)) {
    tk <- top[ord[mk[ord] %in% TRUE]]
    if (length(tk)) dist <- .rule_distance(tk, within, "depth", "max", "any", .RP_SYSTEM)
  } else if (isFALSE(p_run)) dist <- .rule_distance(hi_best, min, "depth", "min", "any", .RP_SYSTEM)
  .rp_answer(p, layers = fr$horizon_idx[v %in% TRUE],
             missing = c(.rp_missing_of(fr, which(is.na(mk) | is.na(anyh))), "redoximorphic_features_pct"),
             evidence = ev,
             notes = c(sprintf("%s %s%s", what,
                               if (isTRUE(win)) "starts" else if (isFALSE(win)) "does not start" else "may start",
                               if (!is.null(within)) sprintf(" within %s cm", format(within)) else ""),
                       if (!is.null(min)) sprintf("functional hydromorphic features over %s cm from there (%s needed)%s",
                                                  format(lo_best), format(min),
                                                  if (is.na(p_run)) ": the description ends or a layer is undecided" else ""),
                       if (length(rel)) sprintf("redox features recorded relict in %s: not functional (Q13)", paste(unique(stats::na.omit(rel)), collapse = ", ")),
                       if (drained && isTRUE(p)) "the site is artificially drained: the attachment is kept, the qualifier drain\u00e9 (Q13)"),
             distance = dist)
}

# The depth class of the hydromorphy of a solum (pp. 282, 284, 289): the
# top of the first functional g or -g and of the first G, and the class of
# the shallower: under 20 cm surredoxique (redox only), under 50 the GER of
# the reductisols and redoxisols or a double, 50 to 80 the qualifiers
# redoxique or reductique, 80 to 120 a horizon redoxique or reductique de
# profondeur, deeper none. NULL when no hydromorphic horizon is read.
.rp_hydromorphy_class <- function(pedon) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(NULL)
  hm <- .rp_hydro_members(fr, .rp_view(pedon))
  top <- .rp_tops(fr)
  f <- function(m) { k <- which(m %in% TRUE & !is.na(top)); if (length(k)) min(top[k]) else NA_real_ }
  gt <- f(hm$g); Gt <- f(hm$G)
  if (is.na(gt) && is.na(Gt)) return(NULL)
  cls <- function(t, kind) {
    if (is.na(t)) return(NA_character_)
    w <- if (kind == "G") "r\u00e9ductique" else "r\u00e9doxique"
    if (t < 20 && kind == "g") "surr\u00e9doxique (under 20 cm)"
    else if (t < 50) "within 50 cm: the r\u00e9ductisols and r\u00e9doxisols, or a double"
    else if (t < 80) sprintf("%s (50 to 80 cm)", w)
    else if (t < 120) sprintf("\u00e0 horizon %s de profondeur (80 to 120 cm)", w)
    else "deeper than 120 cm"
  }
  data.frame(kind = c("g", "G"), top_cm = c(gt, Gt), class = c(cls(gt, "g"), cls(Gt, "G")),
             page = "282, 284, 289", stringsAsFactors = FALSE)[!is.na(c(gt, Gt)), , drop = FALSE]
}

#' The base of the reductic horizons (RP 2008, p. 282; default 20, D23)
#'
#' The reductisols typiques have their G to the base of the solum (a deep,
#' phreatic water table); the stagniques a G over a C, R or M that is not
#' reduced (a perched water table). Read on the morphology: the run of G
#' from the first one, then the layer under it; a G reaching the bottom of
#' the description is typique unless a perched water table is recorded; an R
#' or M under the G is stagnique; a C or D under it is typique with a phreatic
#' water table recorded (groundwater_kind observed, apparent or phreatic),
#' else, as any other horizon under the G, stagnique (Hugo's decision 5 on
#' the review of 0.9.351, 2026-11-04).
#' @noRd
reductic_base_rp <- function(pedon, kind = c("typique", "stagnique")) {
  kind <- match.arg(kind)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  mG <- .rp_members(fr, .RP_REDUCTIC)
  top <- .rp_tops(fr)
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & (is.na(.rp_bots(fr)[ord]) | .rp_bots(fr)[ord] > 0)]
  j <- which(mG[ord] %in% TRUE)[1L]
  if (is.na(j)) return(.rp_answer(if (anyNA(mG)) NA else FALSE, missing = .rp_missing_of(fr, which(is.na(mG))),
                                  notes = "no G horizon"))
  while (j < length(ord) && isTRUE(mG[ord[j + 1L]])) j <- j + 1L
  nb <- if (j < length(ord)) ord[j + 1L] else NA_integer_
  gw <- .rp_site_txt(x, "groundwater_kind")
  perched <- isTRUE(grepl("perch", gw))
  phreatic <- isTRUE(grepl("phreat|observed|apparent", gw)) && !isTRUE(grepl("^none", gw))
  if (!is.na(nb) && is.na(mG[nb]))
    return(.rp_answer(NA, missing = .rp_missing_of(fr, nb), notes = "the layer under the G is undecided"))
  # Hugo's decision 5 on the review of 0.9.351 (2026-11-04): an R or M under
  # the G, or a perched water table, is stagnique; a C or D under it with a
  # phreatic water table recorded is typique; no water recorded, the base
  # decides (any horizon under the G stagnique)
  hard <- !is.na(nb) && isTRUE(.rp_members(fr, c("R", "M"))[nb])
  loose <- !is.na(nb) && isTRUE(.rp_members(fr, c("C", "D"))[nb])
  morph <- if (is.na(nb)) "typique" else "stagnique"
  res <- if (perched || hard) "stagnique" else if (loose && phreatic) "typique" else morph
  note <- if (is.na(nb)) sprintf("the G reaches the base of the description%s",
                                 if (perched) "; a perched water table recorded: stagnique (D23)" else "")
          else sprintf("a %s under the G%s", fr$code[nb] %||% "layer",
                       if (res == "typique") sprintf(", the groundwater %s recorded: a deep, phreatic water table (typique)", gw)
                       else ": the reducing conditions stop at depth")
  .rp_answer(res == kind, evidence = list(base = morph, groundwater_kind = gw), notes = note)
}

#' An aerated structural S over the redox horizons (RP 2008, p. 107)
#'
#' The double brunisol-redoxisol needs an S that is well aerated and well
#' structured: an S (S=) with no redox feature lies directly over the first
#' -g, which is an S with -g (Sg; Q8: an S over a g horizon is the simple
#' redoxisol, case 1 of p. 285; soilKey's reading).
#' @noRd
aerated_s_rp <- function(pedon) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  hm <- .rp_hydro_members(fr, x)
  top <- .rp_tops(fr)
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord])]
  j <- which(!(hm$g[ord] %in% FALSE))[1L]
  if (is.na(j)) return(.rp_answer(FALSE, notes = "no redox horizon"))
  if (is.na(hm$g[ord[j]])) return(.rp_answer(NA, missing = .rp_missing_of(fr, ord[j])))
  if (j == 1L) return(.rp_answer(FALSE, notes = "the redox features start at the surface"))
  sS <- .rp_members(fr, "S=")
  up <- ord[j - 1L]; lo <- ord[j]
  p <- .rp_and3(c(sS[up], !hm$g[up] %in% TRUE, sS[lo]))
  .rp_answer(p, layers = if (isTRUE(p)) fr$horizon_idx[c(up, lo)] else integer(0),
             notes = if (isTRUE(p)) "an aerated S over an Sg (p. 107)"
                     else "no aerated S directly over an Sg: the redoxisol alone (p. 107)")
}

#' A humic structural or argillic horizon under the Aso (RP 2008, p. 256)
#'
#' The layer directly under the deepest layer of `upper` (an E thinner than
#' `gap` cm between them allowed, p. 256) is one of `codes` and humic, the
#' Sh, Sth or BTh of the phaeosols: written with h, or, read on the data,
#' organic carbon over `oc` % in it (0.6, strict) and S/CEC over `bs` %
#' (80 +/- 5). Three-valued.
#' @noRd
humic_under_rp <- function(pedon, codes, upper = "Aso", oc = 0.6, bs = 80, gap = 5) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  mu <- .rp_members(fr, upper); mE <- .rp_members(fr, "E")
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & (is.na(bot[ord]) | bot[ord] > 0)]
  pos <- which(mu[ord] %in% TRUE)
  if (!length(pos)) return(.rp_answer(if (anyNA(mu)) NA else FALSE, notes = sprintf("no %s", paste(unlist(upper), collapse = ", "))))
  j <- max(pos) + 1L
  while (j <= length(ord) && isTRUE(mE[ord[j]]) && isTRUE(bot[ord[j]] - top[ord[j]] < gap)) j <- j + 1L
  if (j > length(ord)) return(.rp_answer(FALSE, notes = "nothing under the Aso"))
  k <- ord[j]
  mc <- .rp_members(fr, codes)[k]
  what <- paste(unlist(codes), collapse = ", ")
  if (isFALSE(mc)) return(.rp_answer(FALSE, notes = sprintf("under the %s: %s, not %s", paste(unlist(upper), collapse = ", "),
                                                             fr$code[k] %||% "undecided", what)))
  if (is.na(mc)) return(.rp_answer(NA, missing = .rp_missing_of(fr, k)))
  if ("h" %in% fr$features[[k]]) return(.rp_answer(TRUE, layers = fr$horizon_idx[k], notes = sprintf("%sh written", fr$code[k])))
  i <- fr$horizon_idx[k]
  o <- .rp_num(x, i, "oc_pct"); b <- .rp_bs(x, i)$value
  ok <- c(if (is.na(o)) NA else o > oc, if (is.na(b)) NA else .rp_meets(b, bs, "bs", "min", open = TRUE))
  p <- .rp_and3(ok)
  .rp_answer(p, layers = if (isTRUE(p)) i else integer(0), missing = c("oc_pct", "bs_pct", "cec_cmol"),
             notes = sprintf("under the %s, %s: organic carbon %s %% (over %s), S/CEC %s %% (over %s): %s",
                             paste(unlist(upper), collapse = ", "), fr$code[k], format(o), format(oc), format(round(b, 1)),
                             format(bs), if (isTRUE(p)) "humic (p. 256)" else if (isFALSE(p)) "not humic" else "undecided"))
}

#' The complex of the Aso of a phaeosol (RP 2008, pp. 255-257; D9)
#'
#' Under permanent vegetation the Aso has S/CEC 50 to 80 % (+/- 5), read as
#' the mean of its layers weighted by thickness; cultivated (an L, or a
#' cultivated land use) the complex is not tested (resaturated, p. 256).
#' Over 80 % the solum is undecided against the chernosols (D9); with
#' `dystric`, the first A run of the colour and carbon of the Aso (p. 255:
#' moist value and chroma under 4, their sum under 6, organic carbon over
#' 1.2 %) has S/CEC under 50 %: the phaeosol dystrique, imperfect (p. 257).
#' A written Aso with no S/CEC recorded is the author's reading.
#' @noRd
sombric_complex_rp <- function(pedon, dystric = FALSE) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  mA <- .rp_members(fr, if (isTRUE(dystric)) "A" else "Aso")
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  holo <- c(.rp_family_members("O"), .rp_family_members("H"))
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & !(fr$code[ord] %in% holo) & (is.na(bot[ord]) | bot[ord] > 0)]
  run <- integer(0)
  for (k in ord) { if (isTRUE(mA[k])) run <- c(run, k) else break }
  if (!length(run)) return(.rp_answer(if (length(ord) && is.na(mA[ord[1L]])) NA else FALSE,
                                      notes = "no Aso from the surface"))
  cult <- any(vapply(run, function(k) "L" %in% fr$overlay[[k]], NA)) ||
    isTRUE(grepl("crop|arable|cultiv|tillage|plough|plow", .rp_site_txt(x, "land_use")))
  if (cult) return(.rp_answer(!isTRUE(dystric), notes = "cultivated: the complex of the Aso not tested (p. 256)"))
  hi <- fr$horizon_idx[run]
  if (isTRUE(dystric)) {
    # another A code (Ach, Aca, Aci ...) is no A of a phaeosol
    if (!all(fr$code[run] %in% c("A", "Aso")))
      return(.rp_answer(FALSE, notes = sprintf("an %s: not the A of a phaeosol", paste(setdiff(fr$code[run], c("A", "Aso")), collapse = ", "))))
    col <- lapply(hi, function(i) .rp_colour(x, i)); o <- vapply(hi, function(i) .rp_num(x, i, "oc_pct"), 0)
    dark <- vapply(seq_along(hi), function(j) {
      if (identical(fr$code[run[j]], "Aso")) return(TRUE)
      v <- col[[j]]$value; c <- col[[j]]$chroma
      if (is.na(v) || is.na(c) || is.na(o[j])) NA else v < 4 && c < 4 && v + c < 6 && o[j] > 1.2
    }, NA)
    if (any(dark %in% FALSE)) return(.rp_answer(FALSE, notes = "the A has not the colour and carbon of an Aso (p. 255)"))
    if (anyNA(dark)) return(.rp_answer(NA, missing = c("munsell_value_moist", "munsell_chroma_moist", "oc_pct")))
  }
  b <- vapply(hi, function(i) as.numeric(.rp_bs(x, i)$value), 0)
  th <- pmax(0, bot[run] - pmax(top[run], 0))
  if (anyNA(b)) {
    written <- all(fr$route[run] %in% c("notation", "both") & fr$code[run] %in% "Aso")
    if (written && !isTRUE(dystric)) return(.rp_answer(TRUE, notes = "a written Aso, its S/CEC not recorded: the author's reading"))
    return(.rp_answer(NA, missing = c("bs_pct", "cec_cmol"), notes = "the S/CEC of the Aso is not recorded"))
  }
  m <- if (sum(th) > 0) sum(b * th) / sum(th) else mean(b)
  if (isTRUE(dystric))
    return(.rp_answer(isTRUE(.rp_meets(m, 50, "bs", "max", open = TRUE)),
                      notes = sprintf("S/CEC of the A %s %%: under 50, the phaeosol dystrique (p. 257)", format(round(m, 1)))))
  lo <- .rp_meets(m, 50, "bs", "min"); hi80 <- .rp_meets(m, 80, "bs", "max")
  p <- if (!isTRUE(lo)) FALSE else if (!isTRUE(hi80)) NA else TRUE
  .rp_answer(p,
             notes = sprintf("S/CEC of the Aso %s %% (50 to 80 under permanent vegetation%s)", format(round(m, 1)),
                             if (is.na(p)) ": over 80, undecided against the chernosols (D9)" else ""))
}

#' The lower boundary of a set of horizons (RP 2008)
#'
#' The lower boundary of the deepest layer of `upper` matches `pattern`
#' (boundary_distinctness): the abrupt Aso/BTh transition of the phaeosols
#' melanoluviques (p. 257). Not recorded, undecided.
#' @noRd
boundary_rp <- function(pedon, upper, pattern = "abrupt|sharp") {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  mu <- .rp_members(fr, upper)
  top <- .rp_tops(fr)
  rows <- which(mu %in% TRUE & !is.na(top))
  if (!length(rows)) return(.rp_answer(if (anyNA(mu)) NA else FALSE, notes = "no upper horizon"))
  k <- rows[which.max(top[rows])]
  b <- .rp_txt(x, fr$horizon_idx[k], "boundary_distinctness")
  if (is.na(b)) return(.rp_answer(NA, missing = "boundary_distinctness", notes = "the boundary is not recorded"))
  .rp_answer(grepl(pattern, tolower(b)), notes = sprintf("the lower boundary of the %s: %s", fr$code[k], b))
}

# The boundary of a layer within 8 cm (the abrupt change of the planosols,
# p. 259): abrupt (under 2 cm) or clear (2 to 5) yes, diffuse (over 15) no,
# gradual (5 to 15) or not recorded undecided.
.rp_sharp_boundary <- function(x, i) {
  b <- tolower(.rp_txt(x, i, "boundary_distinctness") %||% NA_character_)
  if (is.na(b)) return(.rp_r(NA, missing = "boundary_distinctness", note = "the lower boundary of the E is not recorded"))
  if (grepl("abrupt|sharp|clear|net", b)) return(.rp_r(TRUE, note = sprintf("the lower boundary of the E %s: within 8 cm", b)))
  if (grepl("diffus", b)) return(.rp_r(FALSE, note = sprintf("the lower boundary of the E %s: over 8 cm", b)))
  .rp_r(NA, missing = "boundary_distinctness", note = sprintf("the lower boundary of the E %s: within 8 cm or not", b))
}

# A compacted or indurated floor (the planosols structuraux, p. 261): a
# fragic or indurated layer (x, c), a duripan Sim or Si, a pan Fem or Femp
# (the grep of the boulbenes), a Km; or a cementation or a pan recorded.
.rp_tight_floor <- function(fr, x, k) {
  i <- fr$horizon_idx[k]
  if (isTRUE(fr$code[k] %in% c("Sim", "Si", "Fem", "Femp", "Km", "OXm")) || any(c("x", "c") %in% fr$features[[k]]))
    return(.rp_r(TRUE, note = sprintf("a %s floor", fr$composed[k] %||% fr$code[k])))
  cc <- .rp_txt(x, i, "cementation_class"); pt <- .rp_txt(x, i, "pan_type")
  if (!is.na(cc) && !grepl("^(none|not|no)", cc)) return(.rp_r(TRUE, note = sprintf("a floor cemented %s", cc)))
  if (!is.na(pt) && !grepl("^(none|no)", pt)) return(.rp_r(TRUE, note = sprintf("a floor with a %s", pt)))
  .rp_r(FALSE, note = "no compacted or indurated floor recorded")
}

#' The planosolic contact (RP 2008, pp. 259-263; D1, D2, D3)
#'
#' The upper horizons (the A, E and ploughed layers from the surface) over a
#' floor, the first layer under them, starting within `within` cm (120 +/-
#' 20). `kind = "textural"`: the four characters of p. 259: a clay
#' difference in the solum of 20 points or more (D1), the least clayey E 30
#' % or less, the most clayey horizon 25 % or more; an abrupt textural
#' change between the last upper horizon and the floor (under 20 % clay,
#' more than the double; from 20 %, 20 points more, D2) within 8 cm (its
#' lower boundary abrupt or clear); a subhorizontal contact (no irregular or
#' broken boundary, no tongues in the floor); the redox Eg is the
#' reference's own test. `kind = "structural"`: a compacted or indurated
#' floor (p. 261) with the abrupt and subhorizontal contact. `truncated`:
#' the upper horizon is the ploughed LE (or L) alone, directly on an Sg (the
#' planosols typiques tronques, p. 263). Strict: the clay thresholds are not
#' in the table of p. 64 (Q5).
#' @noRd
planosol_rp <- function(pedon, kind = c("textural", "structural"), truncated = FALSE, within = 120) {
  kind <- match.arg(kind)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  mU <- .rp_members(fr, c("A", "E", "L"))
  mE <- .rp_members(fr, c("E", "L"))
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  holo <- c(.rp_family_members("O"), .rp_family_members("H"))
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & !(fr$code[ord] %in% holo) & (is.na(bot[ord]) | bot[ord] > 0)]
  run <- integer(0); j <- 1L
  while (j <= length(ord)) {
    k <- ord[j]
    if (is.na(mU[k])) return(.rp_answer(NA, missing = .rp_missing_of(fr, k), notes = "an upper layer is undecided"))
    if (!mU[k]) break
    run <- c(run, k); j <- j + 1L
  }
  if (!length(run) || j > length(ord)) return(.rp_answer(FALSE, notes = "no upper horizons over a floor"))
  fl <- ord[j]; last <- run[length(run)]
  Ek <- run[mE[run] %in% TRUE]
  if (!length(Ek)) return(.rp_answer(FALSE, notes = "no E over the floor"))
  if (isTRUE(truncated)) {
    sg <- .rp_members(fr, "Sg")[fl]
    if (length(run) != 1L || !isTRUE("L" %in% c(fr$overlay[[last]], fr$code[last])) || !isTRUE(sg))
      return(.rp_answer(if (is.na(sg)) NA else FALSE, notes = "no ploughed horizon directly on an Sg (p. 263)"))
  }
  w <- .rule_check(top[fl], within, "depth", "max", .RP_SYSTEM)
  ans <- list(.rp_r(w, note = sprintf("the floor (%s) at %s cm (within %s)", fr$code[fl] %||% "a layer", format(top[fl]), format(within))))
  bd <- .rp_sharp_boundary(x, fr$horizon_idx[last])
  bt <- tolower(.rp_txt(x, fr$horizon_idx[last], "boundary_topography") %||% NA_character_)
  df <- .rp_txt(x, fr$horizon_idx[fl], "degradation_form")
  sub <- !isTRUE(grepl("irregular|broken", bt)) && !isTRUE(grepl("tongue", df)) && !("d" %in% fr$features[[fl]] && isTRUE(grepl("tongue", df)))
  ans <- c(ans, list(bd, .rp_r(sub, note = if (sub) "a subhorizontal contact" else "an irregular contact or tongues in the floor (p. 259)")))
  if (kind == "textural") {
    clay <- function(k) .rp_clay(x, fr$horizon_idx[k])
    ce <- vapply(Ek, clay, 0); cl <- clay(last); cf <- clay(fl)
    mins <- vapply(ord[.rp_mineral_rows(fr)[ord]], clay, 0)
    if (anyNA(c(ce, cl, cf)) || all(is.na(mins)))
      return(.rp_answer(NA, missing = "clay_pct", notes = "the clay of the E or of the floor is not recorded"))
    mx <- max(mins, na.rm = TRUE); mn <- min(ce)
    c1 <- mx - mn >= 20 && mn <= 30 && mx >= 25
    jump <- if (cl < 20) cf > 2 * cl else cf - cl >= 20
    ans <- c(ans, list(.rp_r(c1, note = sprintf("clay difference %s points, the least clayey E %s %%, the most clayey %s %% (p. 259)",
                                                format(mx - mn), format(mn), format(mx))),
                       .rp_r(jump, note = sprintf("from %s %% clay to %s %% under it (%s, p. 259)", format(cl), format(cf),
                                                  if (cl < 20) "more than the double" else "20 points more"))))
  } else ans <- c(ans, list(.rp_tight_floor(fr, x, fl)))
  r <- do.call(.rp_all, ans)
  .rp_answer(r$passed, layers = if (isTRUE(r$passed)) fr$horizon_idx[c(run, fl)] else integer(0),
             missing = r$missing, evidence = list(floor = fr$code[fl], floor_top = top[fl]),
             notes = r$note,
             distance = if (isFALSE(w)) .rule_distance(top[fl], within, "depth", "max", "any", .RP_SYSTEM) else NA_real_)
}

# The humus forms of the podzosols (p. 267): moder or mor, never a mull.
.RP_MULL <- "mull"
.RP_MODER_MOR <- "moder|mor|amphimus|tangel"

#' The humus of a podzosol (RP 2008, p. 267)
#'
#' A moder or a mor: every mull episolum is excluded, unless the site is
#' cultivated (podzolisation stopped by cultivation). FALSE when a mull is
#' recorded (humus_form); undecided when no OF or OH horizon and no humus
#' form are recorded and the pH in water of the first A is recorded 5.0 or
#' more (+/- 0.2; the A of a moder is under 5.0); TRUE otherwise, a mull not
#' recorded being absent (a field observation; Hugo's decisions on the review
#' of 0.9.351, 2026-11-04).
#' @noRd
podzolic_humus_rp <- function(pedon) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  hf <- tolower(.rp_site_txt(x, "humus_form") %||% NA_character_)
  cult <- isTRUE(grepl("crop|arable|cultiv|tillage|plough|plow", .rp_site_txt(x, "land_use"))) ||
    any(vapply(fr$overlay, function(o) "L" %in% o, NA)) || any(fr$code %in% "L")
  if (!is.na(hf) && grepl(.RP_MULL, hf) && !grepl("amphimus", hf))
    return(.rp_answer(cult, notes = sprintf("humus form %s: %s", hf, if (cult) "cultivated, the podzolisation stopped (p. 267)"
                                                              else "a mull episolum is excluded (p. 267)")))
  if (!is.na(hf) && grepl(.RP_MODER_MOR, hf)) return(.rp_answer(TRUE, notes = sprintf("humus form %s", hf)))
  mO <- .rp_members(fr, c("OF", "OH", "OHta"))
  if (any(mO %in% TRUE)) return(.rp_answer(TRUE, notes = "OF or OH horizons: a moder or a mor (p. 267)"))
  if (cult) return(.rp_answer(TRUE, notes = "cultivated: no O horizon expected (p. 267)"))
  mA <- .rp_members(fr, "A")
  top <- .rp_tops(fr)
  ra <- which(mA %in% TRUE & !is.na(top))
  ph <- if (length(ra)) .rp_num(x, fr$horizon_idx[ra[which.min(top[ra])]], "ph_h2o") else NA_real_
  if (!is.na(ph) && isFALSE(.rp_meets(ph, 5.0, "ph", "max", open = TRUE)))
    return(.rp_answer(NA, missing = c("humus_form"),
                      notes = sprintf("no O horizon or humus form recorded; the A at pH %s, not under 5.0: the humus undecided (p. 267; Hugo's decision 9, 2026-11-04)", format(ph))))
  .rp_answer(TRUE, notes = if (!is.na(ph)) sprintf("no O horizon recorded; the A at pH %s: a moder", format(ph))
                           else "no humus form, O horizon or pH of the A recorded: no mull recorded (soilKey's reading)")
}

#' A cemented horizon of a set (RP 2008, p. 267)
#'
#' A layer of `codes` is cemented, the alios or ortstein of the podzosols
#' duriques and humo-duriques: written with c, or a cementation recorded
#' (cementation_class other than none), or an ortstein pan. Not recorded:
#' loose (a field observation).
#' @noRd
cemented_rp <- function(pedon, codes) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  m <- .rp_members(fr, codes)
  rows <- which(m %in% TRUE)
  if (!length(rows)) return(.rp_answer(if (anyNA(m)) NA else FALSE, notes = "no layer of the set"))
  hit <- vapply(rows, function(k) {
    i <- fr$horizon_idx[k]
    cc <- .rp_txt(x, i, "cementation_class"); pt <- .rp_txt(x, i, "pan_type")
    des <- fr$designation[k] %||% NA_character_
    # an FAO or USDA m (Bhsm, Bsm) of a layer read on the data: cemented
    # (Hugo's decision 9 on the review of 0.9.351, 2026-11-04)
    fm <- !identical(fr$route[k], "notation") && isTRUE(grepl("^[0-9]*B[a-z]*m[0-9]*$", des))
    "c" %in% fr$features[[k]] || (!is.na(cc) && !grepl("^(none|not|no)", cc)) || isTRUE(grepl("ortstein|alios", pt)) || fm
  }, NA)
  .rp_answer(any(hit), layers = fr$horizon_idx[rows[hit]],
             notes = if (any(hit)) sprintf("a cemented %s (alios)", paste(unlist(codes), collapse = ", "))
                     else sprintf("the %s loose (no cementation recorded)", paste(unlist(codes), collapse = ", ")))
}

#' A placic horizon with the BP (RP 2008, p. 270)
#'
#' A Femp directly above a BP or inside it (a BP directly under it): not
#' under the BP alone.
#' @noRd
placic_rp <- function(pedon) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  mF <- .rp_members(fr, "Femp"); mB <- .rp_members(fr, "BP")
  top <- .rp_tops(fr)
  ord <- order(ifelse(is.na(top), Inf, top)); ord <- ord[!is.na(top[ord])]
  # directly above the BP, or inside it (a BP under it, p. 270); a Femp
  # under the BP only is no placique's (Hugo's decision 9, 2026-11-04)
  v <- vapply(seq_along(ord), function(j) {
    k <- ord[j]
    under <- if (j < length(ord)) mB[ord[j + 1L]] else FALSE
    .rp_and3(c(mF[k], under))
  }, NA)
  p <- .rp_or3(v)
  .rp_answer(p, layers = fr$horizon_idx[ord[v %in% TRUE]],
             notes = if (isTRUE(p)) "a placic Femp with the BP (p. 270)" else "no Femp with the BP")
}

#' Coarse elements by weight in a set of horizons (RP 2008, Q11)
#'
#' Every layer of `codes` has coarse elements over `min` % by weight of the
#' whole soil (and under `max` with it): the more than 25 % of the rankosols
#' (p. 276). By weight, else the volume converted or its band (Q11; under
#' 20 % by volume no, for 25); not recorded, none (a field observation), or
#' undecided with `unrecorded = "undecided"` (the rankosols). Strict (Q5).
#' @noRd
coarse_band_rp <- function(pedon, codes, min = NULL, max = NULL, unrecorded = c("none", "undecided")) {
  unrecorded <- match.arg(unrecorded)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  m <- .rp_members(fr, codes)
  rows <- which(m %in% TRUE)
  if (!length(rows)) return(.rp_answer(if (anyNA(m)) NA else FALSE, notes = "no layer of the set"))
  r <- lapply(fr$horizon_idx[rows], function(i) {
    # `unrecorded = "undecided"`: no coarse elements recorded leaves the
    # test undecided (the rankosols, Hugo's decision 7, 2026-11-04)
    if (unrecorded == "undecided" && all(is.na(vapply(c("coarse_fragments_wt_pct", "gravel_wt_pct", "cobbles_wt_pct",
                                                        "stones_wt_pct", "boulders_wt_pct", "coarse_fragments_pct"),
                                                      function(a) .rp_num(x, i, a), 0))))
      return(list(p = NA, note = "the coarse elements are not recorded: undecided"))
    a <- if (!is.null(min)) .rp_coarse_wt(x, i, min) else NULL
    b <- if (!is.null(max)) .rp_coarse_wt(x, i, max) else NULL
    list(p = .rp_and3(c(if (!is.null(a)) a$passed, if (!is.null(b)) !b$passed)),
         note = c(a$note, b$note))
  })
  p <- .rp_and3(vapply(r, `[[`, NA, "p"))
  .rp_answer(p, layers = if (isTRUE(p)) fr$horizon_idx[rows] else integer(0),
             missing = c("coarse_fragments_wt_pct", "bulk_density_g_cm3"),
             notes = unique(unlist(lapply(r, `[[`, "note"))))
}

#' A flag recorded at the site (RP 2008)
#'
#' The logical site attribute `field` (lateral_bp_downslope: the BP of the
#' podzosols eluviques downslope, p. 270; Q14): TRUE or FALSE as recorded,
#' undecided when not recorded (the reference a candidate).
#' @noRd
site_flag_rp <- function(pedon, field) {
  v <- .rp_site_lgl(.rp_view(pedon), field)
  .rp_answer(v, missing = field, notes = if (is.na(v)) sprintf("%s not recorded (Q14)", field) else sprintf("%s: %s", field, v))
}


#' The peyric horizons from the surface (RP 2008, pp. 250-252; Q11)
#'
#' The run from the surface (holorganic layers above skipped) of peyric
#' horizons: an X (Xc, Xp, Xgr), or a horizon of another code whose coarse
#' elements are recorded over 60 % by weight (or the volume converted, Q11:
#' an E or BP of the notation in a scree, the peyrosol-podzosol meuble of p.
#' 252; Hugo's decision 9 on the review of 0.9.351, 2026-11-04); a layer C,
#' M, D, R or P is no horizon (D18). Over `min` cm (50 +/- 10); with
#' `leptic`, over 10 cm (+/- 5) and under 50, directly on an M, R or D.
#' @noRd
peyric_rp <- function(pedon, min = 50, leptic = FALSE) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  mX <- .rp_members(fr, "X")
  layers <- c(.rp_family_members("C"), .rp_family_members("M"), .rp_family_members("D"), .rp_family_members("R"), "P")
  test <- function(k) {
    if (!is.na(mX[k]) && mX[k]) return(TRUE)
    cd <- fr$code[k]
    if (is.na(cd) || cd %in% layers) return(if (is.na(mX[k])) NA else FALSE)
    w <- .rp_coarse_wt(x, fr$horizon_idx[k], 60)
    if (isTRUE(w$passed)) TRUE else if (is.na(mX[k])) NA else FALSE
  }
  run <- .rp_run_from_surface(fr, function(k) isTRUE(test(k)))
  if (is.na(run$thickness)) return(.rp_answer(NA, missing = "bottom_cm"))
  th <- run$thickness
  if (!isTRUE(leptic)) {
    p <- .rp_meets(th, min, "depth", "min", open = TRUE)
    return(.rp_answer(p, layers = fr$horizon_idx[run$rows],
                      notes = sprintf("peyric horizons over %s cm from the surface (more than %s)", format(th), format(min)),
                      distance = if (isFALSE(p)) .rule_distance(th, min, "depth", "min", "any", .RP_SYSTEM) else NA_real_))
  }
  t10 <- .rp_meets(th, 10, "depth", "min", open = TRUE)
  top <- .rp_tops(fr)
  ord <- order(ifelse(is.na(top), Inf, top))
  last <- if (length(run$rows)) run$rows[length(run$rows)] else NA_integer_
  j <- if (is.na(last)) NA_integer_ else match(last, ord) + 1L
  nb <- if (is.na(j) || j > length(ord)) NA_integer_ else ord[j]
  on <- if (is.na(nb)) FALSE else .rp_members(fr, c("M", "R", "D"))[nb]
  p <- .rp_and3(c(t10, on))
  .rp_answer(p, layers = if (isTRUE(p)) fr$horizon_idx[run$rows] else integer(0),
             notes = sprintf("peyric horizons %s cm from the surface%s", format(th),
                             if (isTRUE(on)) " directly on M, R or D" else ", not on M, R or D"))
}

#' The kind of the ploughed horizon, LA or LE (RP 2008, p. 271)
#'
#' The post-podzosols need an LE over the E: written in the notation it is
#' read (an LA is no LE); a ploughed layer read on the data cannot be told
#' LA or LE, and the answer is undecided (Hugo's decision 9 on the review of
#' 0.9.351, 2026-11-04).
#' @noRd
ploughed_kind_rp <- function(pedon) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  mL <- .rp_members(fr, c("L+", "L"))
  top <- .rp_tops(fr)
  k <- which(mL %in% TRUE & !is.na(top))
  if (!length(k)) return(.rp_answer(FALSE, notes = "no ploughed horizon"))
  k <- k[which.min(top[k])]
  if (identical(fr$route[k], "data"))
    return(.rp_answer(NA, missing = "designation", notes = "a ploughed layer on the data: LA or LE undecided (p. 271)"))
  .rp_answer(isTRUE(fr$code[k] %in% .rp_family_members("E")),
             notes = sprintf("the ploughed horizon written %s", fr$composed[k] %||% fr$code[k]))
}

#' The podzolic episolum of a solum (RP 2008, pp. 271, 273)
#'
#' [A + E + BP] (or [A + BP]): from the first mineral layer to the base of the
#' first run of BP layers, the O not counted; `min` to `max` cm (open as
#' printed): the micropodzol (under 20 cm) and the podzosol leptique (under
#' 30 cm) over a luvisol (p. 273). No BP: FALSE.
#' @noRd
podzolic_episolum_rp <- function(pedon, min = NULL, max = NULL) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  mB <- .rp_members(fr, "BP")
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  holo <- c(.rp_family_members("O"), .rp_family_members("H"))
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & !(fr$code[ord] %in% holo) & (is.na(bot[ord]) | bot[ord] > 0)]
  j <- which(mB[ord] %in% TRUE)[1L]
  if (is.na(j)) return(.rp_answer(if (anyNA(mB[ord])) NA else FALSE, notes = "no BP"))
  while (j < length(ord) && isTRUE(mB[ord[j + 1L]])) j <- j + 1L
  th <- bot[ord[j]] - max(0, top[ord[1L]])
  if (is.na(th)) return(.rp_answer(NA, missing = "bottom_cm"))
  ok <- c(if (!is.null(min)) .rp_meets(th, min, "depth", "min", open = TRUE),
          if (!is.null(max)) .rp_meets(th, max, "depth", "max", open = TRUE))
  .rp_answer(.rp_and3(ok), notes = sprintf("the podzolic episolum [A + E + BP] %s cm", format(th)))
}


# --- the tests of the chapters of part 8 (v0.9.352) ------------------------------------------------
# The regosols, salisols and sodisols, thalassosols, thiosols and
# sulfatosols, veracrisols and vertisols (pp. 291-326).

#' Two horizons superposed (RP 2008, p. 297)
#'
#' The first layer of `upper` starts above the first layer of `lower`: the
#' sodisalisols (a sodic horizon over a salic one, NaA/SaS) against the
#' salisodisols (SaA/NaS). Both are needed; an undecided layer that could
#' change the order leaves the answer undecided.
#' @noRd
superposed_rp <- function(pedon, upper, lower) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  mu <- .rp_members(fr, upper); ml <- .rp_members(fr, lower)
  top <- .rp_tops(fr)
  first <- function(m, sure) {
    r <- which((if (sure) m %in% TRUE else !(m %in% FALSE)) & !is.na(top))
    if (length(r)) min(top[r]) else Inf
  }
  u1 <- first(mu, TRUE); u0 <- first(mu, FALSE); l1 <- first(ml, TRUE); l0 <- first(ml, FALSE)
  what <- sprintf("%s over %s", paste(unlist(upper), collapse = ", "), paste(unlist(lower), collapse = ", "))
  if (is.infinite(u0) || is.infinite(l0)) return(.rp_answer(FALSE, notes = sprintf("%s: one of them is absent", what)))
  p <- if (is.finite(u1) && u1 < l0) TRUE else if (is.finite(l1) && l1 <= u0) FALSE else NA
  .rp_answer(p, missing = .rp_missing_of(fr, which(is.na(mu) | is.na(ml))),
             notes = sprintf("%s: %s", what, if (isTRUE(p)) "yes" else if (isFALSE(p)) "no" else "undecided"))
}

#' The series of a salic horizon (RP 2008, p. 295; Q12)
#'
#' The first salic layer (Sa) starting within `within` cm: the neutral series
#' (chlorides, sulfates or nitrates, the paste extract under pH 8.5: the
#' salisols chloruro-sulfates) or the alkaline series (bicarbonates and
#' carbonates, over pH 8.5: the salisols carbonates). Read on the anions of
#' the soluble salts when recorded (the carbonate and bicarbonate share over
#' the chloride and sulfate shares: alkaline); else on the pH in water as
#' recorded, standing in for the pH of the paste extract (+/- 0.2, p. 64),
#' noted; neither recorded, undecided (soilKey's reading).
#' @noRd
salic_series_rp <- function(pedon, series = c("neutral", "alkaline"), within = 60) {
  series <- match.arg(series)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  m <- .rp_members(fr, "Sa+")
  top <- .rp_tops(fr)
  lim <- .rule_threshold(within, "depth", "max", .RP_SYSTEM)
  win <- !is.na(top) & top <= lim
  rows <- which(m %in% TRUE & win)
  if (!length(rows)) return(.rp_answer(if (any(is.na(m) & win)) NA else FALSE, missing = .rp_missing_of(fr, which(is.na(m))),
                                       notes = "no salic horizon"))
  k <- rows[which.min(top[rows])]
  i <- fr$horizon_idx[k]
  cb <- .rp_num(x, i, "soluble_salts_carbonate_pct")
  ns <- c(.rp_num(x, i, "soluble_salts_chloride_pct"), .rp_num(x, i, "soluble_salts_sulfate_pct"))
  if (!is.na(cb) && any(!is.na(ns))) {
    alk <- cb > sum(ns, na.rm = TRUE)
    return(.rp_answer(if (series == "alkaline") alk else !alk, layers = i,
                      notes = sprintf("the salts of the %s: carbonates %s %% against chlorides and sulfates %s %%: the %s series",
                                      fr$code[k], format(cb), format(sum(ns, na.rm = TRUE)), if (alk) "alkaline" else "neutral")))
  }
  ph <- .rp_num(x, i, "ph_h2o")
  if (is.na(ph)) return(.rp_answer(NA, missing = c("soluble_salts_carbonate_pct", "ph_h2o"),
                                   notes = "the series of the salic horizon: neither the anions nor the pH recorded"))
  ok <- if (series == "neutral") .rp_meets(ph, 8.5, "ph", "max", open = TRUE) else .rp_meets(ph, 8.5, "ph", "min", open = TRUE)
  .rp_answer(ok, layers = if (isTRUE(ok)) i else integer(0),
             notes = sprintf("the pH in water %s of the %s stands for the pH of the paste extract (%s 8.5, the %s series, p. 295; Q12)",
                             format(ph), fr$code[k], if (series == "neutral") "under" else "over", series),
             distance = .rule_distance(ph, 8.5, "ph", if (series == "neutral") "max" else "min", "any", .RP_SYSTEM))
}

#' A marine or fluvio-marine material (RP 2008, pp. 303-304; Q14)
#'
#' The thalassosols are developed in marine or fluvio-marine deposits: a
#' marine origin recorded in a layer starting within `depth` cm (layer_origin
#' or rock_origin) or in the parent material; required, not recorded it is
#' none (Hugo's decision Q14).
#' @noRd
marine_rp <- function(pedon, depth = 50) {
  fr <- .rp_frame(pedon)
  x <- .rp_view(pedon)
  top <- .rp_tops(fr)
  word <- function(t) !is.na(t) && grepl(.RP_MARINE_WORDS, tolower(t), perl = TRUE)
  org <- vapply(seq_len(nrow(fr)), function(k) {
    i <- fr$horizon_idx[k]
    isTRUE(top[k] < depth) && (word(.rp_txt(x, i, "layer_origin")) || word(.rp_txt(x, i, "rock_origin")))
  }, NA)
  pm <- .rp_site_txt(x, "parent_material")
  p <- any(org) || word(pm)
  .rp_answer(p, layers = fr$horizon_idx[org],
             notes = if (p) sprintf("a marine or fluvio-marine material%s", if (word(pm)) sprintf(" (%s)", pm) else "")
                     else "no marine or fluvio-marine origin recorded (layer_origin, rock_origin, parent_material; required, Q14)")
}

#' The site of the thalassosols (RP 2008, p. 304; Q14, D18)
#'
#' `kind = "tidal"`: the sea reaches the site (the thalassosols bruts, at every
#' high tide, and juveniles, at the very high tides): tidal_inundation, or a
#' tidal_zone intertidal or supratidal, or a salt marsh, mudflat, slikke or
#' schorre in landform or land_use; a polder (poldered TRUE), a tidal_zone
#' subtidal or extratidal, or tidal_inundation FALSE fails it. `kind =
#' "polder"`: embanked, drained or reclaimed (the polderises): poldered
#' TRUE, or artificially_drained with no tidal flooding recorded, or a polder
#' named in landform or land_use (flagged, Q14); poldered FALSE or a tidal
#' flooding recorded fails it. The horizons decide between the three
#' references (D18); the site confirms. `site = "lenient"`: not recorded, it
#' passes (the attachment is provisional, `provisional_unless`);
#' `"required"`: undecided.
#' @noRd
tidal_site_rp <- function(pedon, kind = c("tidal", "polder"), site = c("lenient", "required")) {
  kind <- match.arg(kind); site <- match.arg(site)
  x <- .rp_view(pedon)
  pol <- .rp_site_lgl(x, "poldered")
  ti <- .rp_site_lgl(x, "tidal_inundation")
  tz <- tolower(.rp_site_txt(x, "tidal_zone") %||% NA_character_)
  txt <- tolower(paste(stats::na.omit(c(.rp_site_txt(x, "landform"), .rp_site_txt(x, "landform_element"),
                                        .rp_site_txt(x, "land_use"))), collapse = " "))
  dr <- .rp_site_lgl(x, "artificially_drained")
  # a polder, a reclaimed, embanked or former tidal land named in the texts
  # is read before the tidal words of the same texts (0.9.352, the review's
  # D10); the tide itself from tidal_zone and tidal_inundation first
  poltxt <- grepl("polder|reclaim|embank|endigu|former|ancien|dyked|diked", txt)
  tidal <- if (isTRUE(ti) || isTRUE(grepl("intertidal|supratidal", tz))) TRUE
           else if (isFALSE(ti) || isTRUE(grepl("subtidal|extratidal", tz))) FALSE
           else if (poltxt) FALSE
           else if (grepl("salt marsh|mudflat|slikke|schorre|(?<!non-)(?<!non )(?<![a-z])(?:inter|supra)?tidal", txt, perl = TRUE)) TRUE
           else NA
  p <- NA; note <- character(0)
  if (kind == "tidal") {
    if (isTRUE(pol)) { p <- FALSE; note <- "a polder: the sea no longer reaches the site" }
    else if (!is.na(tidal)) { p <- tidal; note <- sprintf("the tidal flooding %s", if (tidal) "recorded" else "recorded as none") }
  } else {
    if (!is.na(pol)) { p <- pol; note <- sprintf("poldered %s", if (pol) "recorded" else "recorded as FALSE") }
    else if (isTRUE(tidal)) { p <- FALSE; note <- "the tidal flooding recorded: not embanked" }
    else if (isTRUE(dr) || poltxt) {
      p <- TRUE; note <- "poldered not recorded: an artificial drainage or a polder named stands in, flagged (Q14)"
    }
  }
  if (is.na(p) && site == "lenient")
    return(.rp_answer(TRUE, notes = sprintf("the site (%s) not recorded: provisional (Q14)",
                                           if (kind == "tidal") "tidal_zone, tidal_inundation, poldered" else "poldered, artificially_drained")))
  .rp_answer(p, missing = if (kind == "tidal") c("tidal_zone", "tidal_inundation", "poldered") else c("poldered", "artificially_drained"),
             notes = if (length(note)) note else "the site not recorded (Q14)")
}

#' A sulfidic material directly under the sulfated horizon (RP 2008, p. 309)
#'
#' The layer directly under the deepest U of the first run of U layers is a
#' TH; the description ending at the U leaves it undecided (soilKey's
#' reading).
#' @noRd
sulfidic_under_rp <- function(pedon) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  mU <- .rp_members(fr, "U"); mT <- .rp_members(fr, "TH")
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & (is.na(bot[ord]) | bot[ord] > 0)]
  j <- which(mU[ord] %in% TRUE)[1L]
  if (is.na(j)) return(.rp_answer(if (anyNA(mU)) NA else FALSE, notes = "no sulfated horizon"))
  while (j < length(ord) && isTRUE(mU[ord[j + 1L]])) j <- j + 1L
  if (j == length(ord)) return(.rp_answer(NA, missing = "horizons", notes = "the description ends at the U: the TH under it is not seen"))
  k <- ord[j + 1L]
  .rp_answer(mT[k], layers = if (isTRUE(mT[k])) fr$horizon_idx[k] else integer(0), missing = .rp_missing_of(fr, k),
             notes = sprintf("under the U: %s", fr$code[k] %||% "undecided"))
}

#' The consistency of the sulfidic solums (RP 2008, pp. 308-309; D12)
#'
#' The index n of Pons and Zonneveld in the layers of `codes` starting within
#' `within` cm (the TH of the thiosols, over 1.4; the U of the sulfatosols,
#' under 1.4), strict: n_value, else the fluidity class (very or moderately
#' fluid read as over 1.4, slightly fluid or non-fluid as under it,
#' flagged). Not recorded in any layer, the material itself stands for its
#' consistency, noted (the waterlogged TH, the oxidised U; soilKey's reading).
#' @noRd
maturation_rp <- function(pedon, codes, threshold = 1.4, direction = c("min", "max"), within = 50) {
  direction <- match.arg(direction)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  m <- .rp_members(fr, codes)
  top <- .rp_tops(fr)
  lim <- .rule_threshold(within, "depth", "max", .RP_SYSTEM)
  rows <- which(m %in% TRUE & !is.na(top) & top <= lim)
  if (!length(rows)) return(.rp_answer(if (anyNA(m)) NA else FALSE, notes = "no layer of the set"))
  ok <- vapply(fr$horizon_idx[rows], function(i) {
    n <- .rp_num(x, i, "n_value")
    if (!is.na(n)) return(if (direction == "min") n > threshold else n < threshold)
    fc <- tolower(.rp_txt(x, i, "fluidity_class") %||% NA_character_)
    if (is.na(fc)) return(NA)
    fluid <- grepl("very|moderately", fc)
    if (direction == "min") fluid else !fluid
  }, NA)
  what <- paste(unlist(codes), collapse = ", ")
  if (all(is.na(ok)))
    return(.rp_answer(TRUE, layers = fr$horizon_idx[rows],
                      notes = sprintf("the consistency (n) of the %s not recorded: the material stands for it (soilKey's reading)", what)))
  p <- all(ok[!is.na(ok)])
  .rp_answer(p, layers = if (p) fr$horizon_idx[rows] else integer(0),
             notes = sprintf("the consistency of the %s: n %s %s (p. 309)", what, if (direction == "min") "over" else "under", format(threshold)))
}

#' The acidification of the sulfidic material on drying (RP 2008, p. 309)
#'
#' The pH of the TH falls under 3.5 after drying or oxidation: incubation_ph
#' or ph_h2o2, when recorded, in the layers of `codes` starting within
#' `within` cm (+/- 0.2). Not recorded, the TH stands for it, noted
#' (soilKey's reading).
#' @noRd
acidification_rp <- function(pedon, codes = "TH", threshold = 3.5, within = 50) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  m <- .rp_members(fr, codes)
  top <- .rp_tops(fr)
  lim <- .rule_threshold(within, "depth", "max", .RP_SYSTEM)
  rows <- which(m %in% TRUE & !is.na(top) & top <= lim)
  if (!length(rows)) return(.rp_answer(if (anyNA(m)) NA else FALSE, notes = "no layer of the set"))
  v <- vapply(fr$horizon_idx[rows], function(i) {
    a <- .rp_num(x, i, "incubation_ph"); if (is.na(a)) a <- .rp_num(x, i, "ph_h2o2"); a
  }, 0)
  if (all(is.na(v)))
    return(.rp_answer(TRUE, layers = fr$horizon_idx[rows],
                      notes = "the pH after drying or oxidation not recorded: the sulfidic material stands for it (soilKey's reading)"))
  ok <- .rp_meets(v[!is.na(v)], threshold, "ph", "max", open = TRUE)
  p <- .rp_or3(ok)
  .rp_answer(p, layers = if (isTRUE(p)) fr$horizon_idx[rows] else integer(0),
             notes = sprintf("the pH after drying or oxidation %s (under %s, p. 309)", paste(format(v[!is.na(v)]), collapse = ", "),
                             format(threshold)),
             distance = .rule_distance(v[!is.na(v)], threshold, "ph", "max", "any", .RP_SYSTEM))
}

#' The vermihumic episolum of the veracrisols (RP 2008, pp. 313-315; D17)
#'
#' The Ah horizons from the surface (an OL possible), thick, very humic,
#' biomacrostructured, dark: written Ah in the notation, or read on the data
#' as the A layers of the surface run (not ploughed) with a moist value of 3
#' or less and a chroma of 2 or less (10YR 3/2 or a lower chroma, p. 314),
#' every one of them biomacrostructured (.rp_biomacro()). Under heath (no
#' ploughed horizon) the Ah are over `min` cm thick (50 +/- 10) and their
#' complex acid and aluminic: S/CEC under 30 % (+/- 5) and Al/S over 1
#' (strict) in every Ah; under culture (an LAh or LA at the surface) an Ah
#' under it with its base deeper than `min` cm, the complex not tested
#' (resaturated, p. 314). With `max`, the Ah are `min` to `max` cm thick
#' (the leptique, 30 to 50 cm, an imperfect attachment, p. 315). The climate
#' and the region are reported, not tested.
#' @noRd
vermihumic_rp <- function(pedon, min = 50, max = NULL) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  holo <- c(.rp_family_members("O"), .rp_family_members("H"))
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & (is.na(bot[ord]) | bot[ord] > 0) & !(fr$code[ord] %in% holo)]
  if (!length(ord)) return(.rp_answer(FALSE, notes = "no mineral horizon"))
  mA <- .rp_members(fr, "A"); mL <- .rp_members(fr, c("L+", "L"))
  # the ploughed layers at the surface (culture), then the Ah under them
  j <- 1L; ploughed <- integer(0)
  while (j <= length(ord) && isTRUE(mL[ord[j]])) { ploughed <- c(ploughed, ord[j]); j <- j + 1L }
  culture <- length(ploughed) > 0L
  notes <- character(0); ok <- logical(0)
  is_ah <- function(k) {
    if (!isTRUE(mA[k]) || isTRUE(mL[k])) return(if (is.na(mA[k])) NA else FALSE)
    if ("h" %in% fr$features[[k]]) return(TRUE)
    if (!identical(fr$route[k], "data")) return(FALSE)
    col <- .rp_colour(x, fr$horizon_idx[k])
    if (is.na(col$value) || is.na(col$chroma)) return(NA)
    col$value <= 3 && col$chroma <= 2
  }
  # the run of Ah from there: sure (every layer an Ah) and possible (an
  # undecided layer, its colour not recorded, may be one)
  ah <- integer(0); maybe <- integer(0); sure <- TRUE
  while (j <= length(ord)) {
    a <- is_ah(ord[j])
    if (isFALSE(a)) break
    if (is.na(a)) sure <- FALSE
    if (sure) ah <- c(ah, ord[j])
    maybe <- c(maybe, ord[j]); j <- j + 1L
  }
  if (!length(maybe)) return(.rp_answer(FALSE, notes = "no vermihumic Ah from the surface"))
  thick <- function(r) if (length(r)) sum(pmax(0, bot[r] - pmax(top[r], 0))) else 0
  th_lo <- thick(ah); th_hi <- thick(maybe)
  b_lo <- if (length(ah)) bot[ah[length(ah)]] else 0; b_hi <- bot[maybe[length(maybe)]]
  if (anyNA(c(th_lo, th_hi, b_lo, b_hi))) return(.rp_answer(NA, missing = "bottom_cm"))
  # the thickness (heath) or the base (culture); the leptique by thickness
  chk <- function(lo, hi, t, dir, open = FALSE) {
    f <- function(v) if (open) .rp_meets(v, t, "depth", dir, open = TRUE) else .rule_check(v, t, "depth", dir, .RP_SYSTEM)
    best <- if (dir == "min") hi else lo; worst <- if (dir == "min") lo else hi
    if (isTRUE(f(worst))) TRUE else if (isFALSE(f(best))) FALSE else NA
  }
  dep <- c(if (is.null(max) && culture) chk(b_lo, b_hi, min, "min", open = TRUE) else chk(th_lo, th_hi, min, "min"),
           if (!is.null(max)) chk(th_lo, th_hi, max, "max"))
  th <- th_lo; base <- b_lo
  notes <- c(notes, sprintf("%s: Ah %s cm thick%s, its base at %s cm", if (culture) "under culture" else "under heath",
                            format(th_lo), if (th_hi > th_lo) sprintf(" (up to %s with the layers whose colour is not recorded)", format(th_hi)) else "",
                            format(b_lo)))
  if (isFALSE(.rp_and3(dep)))
    return(.rp_answer(FALSE, notes = notes,
                      distance = .rule_distance(if (culture && is.null(max)) b_hi else th_hi, min, "depth", "min", "any", .RP_SYSTEM)))
  ok <- c(ok, dep)
  und <- setdiff(maybe, ah)
  if (length(und)) ok <- c(ok, NA)
  ah <- maybe
  # dark and biomacrostructured, every Ah
  for (k in ah) {
    i <- fr$horizon_idx[k]
    col <- .rp_colour(x, i)
    dk <- if (is.na(col$value) || is.na(col$chroma)) NA else col$value <= 3 && col$chroma <= 2
    bm <- .rp_biomacro(x, i)
    ok <- c(ok, dk, bm$passed)
    if (isFALSE(dk)) notes <- c(notes, sprintf("the %s at %s: not dark enough (moist value 3 and chroma 2 at most)", fr$code[k], .rp_colour_text(col)))
    if (!isTRUE(bm$passed)) notes <- c(notes, bm$note)
    if (!culture) {
      b <- .rp_bs(x, i)$value
      al <- .rp_value(x, i, "al_bases")
      ok <- c(ok, if (is.na(b)) NA else .rp_meets(b, 30, "bs", "max", open = TRUE),
              if (is.na(al)) NA else al > 1)
      if (isFALSE(al > 1)) notes <- c(notes, sprintf("Al/S %s: not aluminic (over 1, p. 314)", format(signif(al, 3))))
    }
  }
  p <- .rp_and3(ok)
  .rp_answer(p, layers = if (isTRUE(p)) fr$horizon_idx[ah] else integer(0),
             missing = c("munsell_value_moist", "munsell_chroma_moist", "biogenic_aggregates_pct",
                         if (!culture) c("bs_pct", "al_kcl_cmol", "ca_cmol", "mg_cmol", "k_cmol", "na_cmol")),
             evidence = list(thickness = th, base = base, culture = culture),
             notes = notes)
}

# The words of the low or flat positions of the topovertisols and of the
# slopes and interfluves of the lithovertisols (pp. 322-323; the CPCS III-1
# and III-2, p. 395).
.RP_LOW_WORDS <- "depress|basin|bas-fond|bottom|cuvette|plain|floodplain|playa|backswamp|lowland|swamp|marsh|karst|doline"
.RP_SLOPE_WORDS <- "slope|versant|interfluv|hill|crest|ridge|shoulder|plateau|summit|upland|rolling"

#' The position of a vertisol (RP 2008, pp. 322-323, 395; Q20)
#'
#' The topovertisols lie low (alluvial plains, karstic depressions, gilgai
#' basins) with a confined SV saturated by the bases at a neutral or alkaline
#' pH; the lithovertisols on interfluves and slopes, fed by the rain only,
#' their SV possibly slightly unsaturated. The site decides when recorded: a
#' slope under 2 % or a low or flat landform (landform, landform_element) the
#' topovertisols, a slope of 2 % or more or a slope or interfluve the
#' lithovertisols; else the SV: every SV layer with S/CEC 95 % or more and a
#' pH in water of 7 or more (+/- 0.2) the topovertisols, any other SV the
#' lithovertisols; neither recorded, undecided (Q20). An SV recorded under 95
#' % makes the lithovertisol whatever the landform, and the site not
#' recorded makes the attachment provisional (`site = "required"`, Hugo's
#' decision 2, 2026-11-04).
#' @noRd
vertisol_site_rp <- function(pedon, kind = c("topo", "litho"), site = c("lenient", "required")) {
  kind <- match.arg(kind); site <- match.arg(site)
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  sl <- suppressWarnings(as.numeric(.rp_site(x, "slope_pct")))
  sl <- if (length(sl) == 1L) sl else NA_real_
  txt <- tolower(paste(stats::na.omit(c(.rp_site_txt(x, "landform"), .rp_site_txt(x, "landform_element"))), collapse = " "))
  low <- if (!is.na(sl)) sl < 2
         else {
           lw <- grepl(.RP_LOW_WORDS, txt); up <- grepl(.RP_SLOPE_WORDS, txt)
           if (lw && !up) TRUE else if (up && !lw) FALSE else NA
         }
  # `site = "required"`: the site not recorded leaves it undecided, the
  # attachment provisional when only the SV decides (Hugo's decision 2)
  if (site == "required")
    return(.rp_answer(if (is.na(low)) NA else TRUE, missing = c("slope_pct", "landform"),
                      notes = if (is.na(low)) "the site of the vertisol not recorded: only the SV decides (decision 2)" else "the site recorded"))
  mSV <- .rp_members(fr, "SV")
  rows <- which(mSV %in% TRUE)
  bs <- vapply(fr$horizon_idx[rows], function(i) .rp_bs(x, i)$value, 0)
  ph <- vapply(fr$horizon_idx[rows], function(i) .rp_num(x, i, "ph_h2o"), 0)
  # an SV recorded unsaturated (under 95 %) makes the lithovertisol, whatever
  # the landform (the palusmectic lithovertisol of p. 324; decision 2)
  if (length(rows) && any(bs < 95, na.rm = TRUE))
    return(.rp_answer(kind == "litho", notes = sprintf("the SV not saturated (S/CEC %s %%): a lithovertisol (p. 324; decision 2)",
                                                        paste(format(round(bs)), collapse = ", "))))
  if (!is.na(low)) {
    p <- if (kind == "topo") low else !low
    return(.rp_answer(p, notes = sprintf("the site %s: %s (Q20)", if (!is.na(sl)) sprintf("slope %s %%", format(sl)) else txt,
                                         if (low) "a low or flat position" else "a slope or an interfluve")))
  }
  if (!length(rows)) return(.rp_answer(if (anyNA(mSV)) NA else FALSE, notes = "no SV"))
  sat <- if (anyNA(bs)) NA else all(bs >= 95)
  alk <- if (anyNA(ph)) NA else all(.rp_meets(ph, 7, "ph", "min"))
  topo <- if (isFALSE(sat)) FALSE else .rp_and3(c(sat, alk))
  p <- if (kind == "topo") topo else !topo
  .rp_answer(p, missing = c("slope_pct", "landform", "bs_pct", "ph_h2o"),
             notes = sprintf("the site not recorded; the SV: S/CEC %s %%, pH %s (Q20)", paste(format(round(bs)), collapse = ", "),
                             paste(format(ph), collapse = ", ")))
}

#' A vertic surface horizon (RP 2008, pp. 23, 322-323)
#'
#' The first mineral layer starting within `depth` cm (10 +/- 5) is an Av or
#' LAv: written or read so, or, read on the data as another A (an Aci or Aso,
#' which the fixed order tests first), with the vertic characters of the Av
#' (40 % clay or more, slickensides or shrinkage cracks; .rp_t_Av()): the
#' lithovertisols and topovertisols, against the paravertisols whose surface
#' is no longer vertic (soilKey's reading, 0.9.352).
#' @noRd
vertic_surface_rp <- function(pedon, depth = 10) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  holo <- c(.rp_family_members("O"), .rp_family_members("H"))
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & (is.na(bot[ord]) | bot[ord] > 0) & !(fr$code[ord] %in% holo)]
  if (!length(ord)) return(.rp_answer(FALSE, notes = "no mineral horizon"))
  k <- ord[1L]
  w <- .rule_check(top[k], depth, "depth", "max", .RP_SYSTEM)
  if (isFALSE(w)) return(.rp_answer(FALSE, notes = sprintf("the first mineral horizon starts at %s cm", format(top[k])),
                                    distance = .rule_distance(top[k], depth, "depth", "max", "any", .RP_SYSTEM)))
  av <- .rp_members(fr, "Av")[k]
  if (isTRUE(av)) return(.rp_answer(TRUE, layers = fr$horizon_idx[k], notes = "an Av at the surface"))
  mA <- .rp_members(fr, c("A", "L"))[k]
  if (isFALSE(mA)) return(.rp_answer(FALSE, notes = "no A at the surface"))
  # the vertic characters of the Av read on the recorded data of the surface
  # A, written A, LA or SaA in the notation or read on the data (0.9.352, the
  # review's D2): 40 % clay or more and slickensides or shrinkage cracks;
  # not recorded, undecided (on the data soilKey's vertic_properties()
  # stands in, flagged)
  i <- fr$horizon_idx[k]
  cl <- .rp_vertic_clay(x, i)
  if (isFALSE(cl$passed)) return(.rp_answer(FALSE, notes = c(cl$note, "the surface A under 40 % clay: not vertic")))
  sl <- .rp_slickensides(x, i); ck <- .rp_cracks(x, i)
  v <- if (isTRUE(sl >= 1L) || isTRUE(ck)) .rp_r(TRUE, note = "slickensides or shrinkage cracks")
       else if (!is.na(sl) || !is.na(ck)) .rp_r(FALSE, note = "no slickensides and no shrinkage cracks recorded")
       else if (identical(fr$route[k], "data")) .rp_stand_in(x, i, "vertic_properties", "the vertic signs")
       else .rp_r(NA, missing = c("slickensides", "cracks_width_cm"), note = "the vertic signs of the surface A not recorded")
  p <- .rp_and3(c(cl$passed, v$passed, mA))
  .rp_answer(p, layers = if (isTRUE(p)) i else integer(0), missing = c(cl$missing, v$missing),
             notes = c(cl$note, v$note, "the vertic characters of the Av read on the surface A (soilKey's reading)"))
}

#' The bleached E of the sodisols solodises (RP 2008, p. 297)
#'
#' The layer directly over the first NaBT is an E of the complete
#' desaturation: written Ea in the notation; a written E only with a pH in
#' water of 5 or less recorded (+/- 0.2), none recorded ruling it out; read
#' on the data, the pH of 5 or less decides, not recorded undecided (Ea is
#' read only from the notation; 0.9.352, the review's D5).
#' @noRd
solodic_e_rp <- function(pedon) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  mN <- .rp_members(fr, "NaBT"); mE <- .rp_members(fr, "E")
  top <- .rp_tops(fr); bot <- .rp_bots(fr)
  ord <- order(ifelse(is.na(top), Inf, top))
  ord <- ord[!is.na(top[ord]) & (is.na(bot[ord]) | bot[ord] > 0)]
  j <- which(!(mN[ord] %in% FALSE))[1L]
  if (is.na(j)) return(.rp_answer(FALSE, notes = "no NaBT"))
  if (is.na(mN[ord[j]])) return(.rp_answer(NA, missing = .rp_missing_of(fr, ord[j])))
  if (j == 1L) return(.rp_answer(FALSE, notes = "nothing over the NaBT"))
  k <- ord[j - 1L]
  if (isFALSE(mE[k])) return(.rp_answer(FALSE, notes = sprintf("a %s over the NaBT, not an E", fr$code[k] %||% "layer")))
  if (is.na(mE[k])) return(.rp_answer(NA, missing = .rp_missing_of(fr, k)))
  written <- !identical(fr$route[k], "data")
  if (written && "a" %in% fr$features[[k]]) return(.rp_answer(TRUE, notes = "an Ea written over the NaBT"))
  ph <- .rp_num(x, fr$horizon_idx[k], "ph_h2o")
  if (is.na(ph))
    return(if (written) .rp_answer(FALSE, notes = "an E written over the NaBT, no pH recorded: not bleached")
           else .rp_answer(NA, missing = "ph_h2o", notes = "the pH of the E over the NaBT is not recorded"))
  ok <- .rp_meets(ph, 5, "ph", "max")
  .rp_answer(ok, notes = sprintf("the E over the NaBT at pH %s (5 or less: solodise, p. 297)", format(ph)),
             distance = .rule_distance(ph, 5, "ph", "max", "any", .RP_SYSTEM))
}

#' The description ends in a set of horizons (RP 2008)
#'
#' The deepest layer is one of `codes`: what lies under it is not seen, and
#' a test of the horizons under it is undecided, not failed (the veracrisol
#' whose description ends in the Ah; 0.9.352, the review's D11). NA when it
#' does, FALSE otherwise.
#' @noRd
ends_in_rp <- function(pedon, codes) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  top <- .rp_tops(fr)
  k <- which.max(ifelse(is.na(top), -Inf, top))
  m <- .rp_members(fr, codes)[k]
  if (isTRUE(m)) return(.rp_answer(NA, missing = "horizons", notes = "the description ends in the horizon: what lies under it is not seen"))
  .rp_answer(FALSE, notes = "the description goes under the horizon")
}

#' The clay of the written SV and V (RP 2008, p. 318; Hugo's decision 7)
#'
#' Every layer of `codes` with its clay recorded has `min` % or more; a layer
#' written in the notation with no clay recorded carries it (the author wrote
#' an SV or V); on the data the identification already needs the clay.
#' @noRd
clay_rp <- function(pedon, codes, min = 40) {
  fr <- .rp_frame(pedon)
  if (!nrow(fr)) return(.rp_answer(NA, missing = "horizons"))
  x <- .rp_view(pedon)
  m <- .rp_members(fr, codes)
  rows <- which(m %in% TRUE)
  if (!length(rows)) return(.rp_answer(if (anyNA(m)) NA else FALSE, notes = "no layer of the set"))
  cl <- vapply(fr$horizon_idx[rows], function(i) .rp_num(x, i, "clay_pct"), 0)
  rec <- !is.na(cl)
  ok <- cl[rec] >= min
  p <- all(ok)
  .rp_answer(p, layers = if (p) fr$horizon_idx[rows] else integer(0),
             notes = c(if (any(rec)) sprintf("clay %s %% (%s %% or more)", paste(format(cl[rec]), collapse = ", "), format(min)),
                       if (any(!rec)) "a written SV or V with no clay recorded carries it (decision 7)"))
}
