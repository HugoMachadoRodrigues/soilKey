# =============================================================================
# v0.9.337 -- the trunks and divisions of the Russian soil classification
# (Field Guide of 2008, keys 2.2 and 2.3, pp. 28-35).
#
# The keys to trunks and divisions read the identified horizons of a profile
# and a few site facts: whether a horizon is present, where it lies (at the
# surface or under a deposit of 40 cm or less, directly over another, in the
# middle of the profile, alone), the depth of hard rock, a deposit at the
# surface, a peat body, a river floodplain and volcanic ash. They never read
# the genetic features, which are for the subtypes. The predicates below
# read the frame of identify_horizons(pedon, "kidpr2008") (the notation where
# the profile is written in it, else key 2.1 on the data) through one view
# per profile, built once per classification:
#
#   * the rows in depth order, from the ground (fresh litter left out; a
#     description that starts with litter starts at 0 cm for the guard of a
#     truncated soil, Hugo's decision Q19);
#   * the kind of each row: a horizon, an undecided layer (a candidate list of
#     the data route), a small horizon, a transition (of the Russian notation,
#     or an FAO or WRB transition such as AB, BC or E/B in which the key on the
#     data finds no diagnostic horizon: one rule on both routes, Q14), rock
#     (an FAO C read as no diagnostic horizon is loose rock too), the
#     technogenic surface formation TPO, the deposit of horizon key position
#     38b, or a layer with no diagnostic horizon (position 66);
#   * the deposit at the surface (key 2.2, step 2; Hugo's decision Q21): TPO,
#     the deposit of position 38b, the stratified humus horizons RJ, RY and
#     RU, the small deposited layers (r, rh, ur, rr, rx), and loose rock (C,
#     D) lying over a buried soil or over an organic or humus horizon;
#     humified when it holds RJ, RY or RU;
#   * the profile: the rows under a deposit of 40 cm or less (a buried soil
#     directly under it is the soil keyed), else every row that is not
#     buried;
#   * the horizons of the profile in order, sub-horizons merged, small
#     horizons and transitions left out (p. 36), and the surface horizons
#     read by the thickest-horizon rule (p. 37; Hugo's decision Q11): of the
#     organic, humus and agro horizons that start the profile, the agro
#     horizon is kept, and of the natural ones the thickest (the others are
#     features); a W of 5 cm or less over another horizon is the feature w.
#     A missing thickness that would decide the rule gives several readings,
#     and a predicate is decided only when they all agree.
#
# Three-valued: TRUE, FALSE, or NA when an undecided layer (a candidate list
# of the data route) or a missing record could change the answer; a missing
# depth leaves a depth limit undecided. Names carry the _kidpr suffix. The
# flattened key that reads them is inst/rules/kidpr2008/key.yaml and
# divisions/; the readings are in readings.yaml.
# =============================================================================

.KIDPR_DV_REF <- "Polevoi opredelitel' pochv Rossii (2008), keys 2.2 and 2.3."

# Rock and parent symbols (3.3): not soil horizons; TPO is no soil either.
.KIDPR_DV_ROCKS <- c("C", "D", "R", "CLM", "TT")
.KIDPR_DV_RSTAR <- c("RJ", "RY", "RU")
.KIDPR_DV_AGRO <- c("P", "PU", "PT", "PTR", "PB")
# Natural organic and humus horizons that may start a profile (the surface
# horizons of the thickest-horizon rule).
.KIDPR_DV_NATURAL <- c("AJ", "AY", "AU", "AO", "AK", "AH", "W", "O", "T", "TO", "TE", "TJ", "H")
.KIDPR_DV_SURFACE <- c(.KIDPR_DV_AGRO, .KIDPR_DV_NATURAL)
.KIDPR_DV_ORGANIC <- c("O", "T", "TO", "TE", "TJ", "H")
.KIDPR_DV_HUMUS <- c("AJ", "AY", "AU", "AO", "AK", "AH", "W")
# What the peat body of trunk A sums (Q22): organic layers, muck and the
# ploughed peat included, and the peat as rock below.
.KIDPR_DV_PEAT <- c("O", "T", "TO", "TE", "TJ", "TT", "H", "PT", "PTR")
# Small horizons that are a deposited layer (3.2.6).
.KIDPR_DV_DEPOSIT_FEATURES <- c("r", "rh", "ur", "rr", "rx")

# Text of the site and the layers, read as words (no letter before a stem):
# a terrace (above the floodplain or not) is no floodplain; glaciofluvial
# material, outwash, moraine and marine material are no alluvium; outwash or
# washed sand is no ash.
.KIDPR_DV_NL <- "(?<!\\p{L})"
.KIDPR_DV_FLOODPLAIN <- paste0("\\bflood[ -]?plains?\\b|\\bbottomlands?\\b|\\blevees?\\b|\\bback[ -]?swamps?\\b|",
                              "\\bchannel bars?\\b|\\bpoint bars?\\b|\\boxbows?\\b|\\briver[ -]?banks?\\b|",
                              .KIDPR_DV_NL, "\u043f\u043e\u0439\u043c")
.KIDPR_DV_NOT_FLOODPLAIN <- paste0("\\bterraces?\\b|", .KIDPR_DV_NL, "\u0442\u0435\u0440\u0440\u0430\u0441|",
                                  "\u043d\u0430\u0434\u043f\u043e\u0439\u043c")
.KIDPR_DV_VALLEY <- paste0("\\bvalleys?\\b|\\balluvial plains?\\b|\\bdeltas?\\b|\\bdeltaic\\b|\\bfluvial\\b|",
                          .KIDPR_DV_NL, "\u0434\u043e\u043b\u0438\u043d|", .KIDPR_DV_NL, "\u0434\u0435\u043b\u044c\u0442")
.KIDPR_DV_ALLUVIUM <- paste0("\\balluv|\\bfluv|\\briver|\\blacustr|\\blakes?\\b|", .KIDPR_DV_NL,
                            "\u0430\u043b\u043b\u044e\u0432|", .KIDPR_DV_NL, "\u043e\u0437\u0435\u0440")
.KIDPR_DV_NOT_ALLUVIUM <- paste0("glaci|fluvio-?glac|\\boutwash|\\bmarine\\b|\\bmorain|", .KIDPR_DV_NL,
                                "\u043c\u043e\u0440\u0441\u043a|\u043b\u0435\u0434\u043d\u0438\u043a|",
                                "\u0444\u043b\u044e\u0432\u0438\u043e\u0433\u043b\u044f\u0446|",
                                "\u0437\u0430\u0434\u0440\u0443\u0441\u0441\u043a|\u043c\u043e\u0440\u0435\u043d")
.KIDPR_DV_ASH <- paste0("\\btephr|\\bpyroclast|\\bvolcan|\\bash(es)?\\b|\\bpumic|\\bscori|", .KIDPR_DV_NL,
                       "\u043f\u0435\u043f\u0435?\u043b|", .KIDPR_DV_NL, "\u0432\u0443\u043b\u043a\u0430\u043d|",
                       .KIDPR_DV_NL, "\u0442\u0435\u0444\u0440")

# The view of the profile in force during one classification.
.kidpr_dv_state <- new.env(parent = emptyenv())


# --- three-valued helpers ----------------------------------------------------------

.kidpr_dv_or <- function(v) if (any(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA
.kidpr_dv_and <- function(v) if (any(v %in% FALSE)) FALSE else if (all(v %in% TRUE)) TRUE else NA
# A depth less than a limit, undecided when the depth is missing.
.kidpr_dv_lt <- function(x, cm) ifelse(is.na(x), NA, x < cm)

# A predicate's answer.
.kidpr_dv_result <- function(name, passed, layers = integer(0), evidence = list(),
                             missing = character(0), where = "") {
  passed <- if (length(passed) == 1L) as.logical(passed) else NA
  DiagnosticResult$new(name = name, passed = passed, layers = as.integer(unique(layers)),
                       evidence = evidence,
                       missing = if (is.na(passed)) unique(as.character(missing)) else character(0),
                       reference = sprintf("%s %s", .KIDPR_DV_REF, where))
}

# Text of a field, lower case (NA when not recorded).
.kidpr_dv_text <- function(v) {
  if (is.null(v) || !length(v)) return(NA_character_)
  v <- as.character(v[[1L]])
  if (is.na(v) || !nzchar(trimws(v))) NA_character_ else tolower(trimws(v))
}

# Does a text match a pattern (FALSE when not recorded)?
.kidpr_dv_match <- function(pattern, x) !is.na(x) & grepl(pattern, x, perl = TRUE)


# --- the view ------------------------------------------------------------------------

# An FAO or WRB transitional designation: two master letters (AB, BA, BC, CB,
# AE, EA, BE, EB, AC; ABg, 2BC) or a mixed form (A/B, E/B, B/C, A&E), a
# lithological number before it allowed.
.kidpr_dv_fao_transition <- function(d) {
  d <- as.character(d)
  s <- sub("^([0-9]+|[IVX]+)(?=[A-Z])", "", ifelse(is.na(d), "", trimws(d)), perl = TRUE)
  two <- grepl("^[ABEC]{2}[a-z0-9]*$", s) & substr(s, 1L, 1L) != substr(s, 2L, 2L)
  sec <- sub("^[ABEC][a-z0-9]*[/&]", "", s)
  mixed <- grepl("^[ABEC][a-z0-9]*[/&][ABEC][a-z0-9]*$", s) & substr(s, 1L, 1L) != substr(sec, 1L, 1L)
  two | mixed
}

# An FAO or WRB designation of loose parent material (C, Cg, 2Ck).
.kidpr_dv_fao_c <- function(d) {
  d <- as.character(d)
  grepl("^([0-9]+|[IVX]+)?C[a-z0-9]*$", ifelse(is.na(d), "", trimws(d)))
}

# The kind of each row of the frame.
.kidpr_dv_kinds <- function(f) {
  n <- nrow(f)
  if (!n) return(character(0))
  code <- f$code
  none <- is.na(code) & f$passed %in% FALSE & !f$small
  data <- !is.na(f$route) & f$route == "data"
  kp <- if (is.null(f$key_path)) rep(NA_character_, n) else as.character(f$key_path)
  # the outcome of key 2.1 on the data (one, when decided)
  oc <- ifelse(is.na(kp), NA_character_, sub("[+/].*$", "", sub("^.*: ", "", kp)))
  litter <- none & !is.na(f$evidence) & grepl("litter", f$evidence, fixed = TRUE)
  deposit <- none & data & oc %in% "_deposit"
  fao_tr <- none & data & !litter & !deposit & .kidpr_dv_fao_transition(f$designation)
  fao_c <- none & data & !litter & !deposit & .kidpr_dv_fao_c(f$designation)
  ifelse(f$small, "small",
  ifelse(f$transitional | fao_tr, "trans",
  ifelse((!is.na(code) & code %in% .KIDPR_DV_ROCKS) | fao_c, "rock",
  ifelse(!is.na(code) & code == "TPO", "nonsoil",
  ifelse(!is.na(code), "horizon",
  ifelse(is.na(f$passed), "undecided",
  ifelse(litter, "litter",
  ifelse(deposit, "deposit", "none"))))))))
}

#' The profile as keys 2.2 and 2.3 read it
#'
#' Builds the view of a frame of identified horizons (identify_horizons(), or
#' any table .as_horizon_table() reads), with the site and the horizon
#' attributes of the pedon. An environment, so that the predicates cache
#' their answers once per profile.
#' @param frame The identified horizons.
#' @param site The site list of the pedon.
#' @param horizons The horizon table of the pedon (rows matched by
#'   `horizon_idx`), or NULL.
#' @return An environment of class `kidpr_view`.
#' @noRd
.kidpr_dv <- function(frame, site = list(), horizons = NULL) {
  f <- .as_horizon_table(frame, keep_extra = TRUE)
  n <- nrow(f)
  if (is.null(f$overlay_na)) f$overlay_na <- rep(list(character(0)), n)
  f$overlay_na <- lapply(f$overlay_na, function(e) as.character(unlist(e)))
  v <- new.env(parent = emptyenv())
  class(v) <- "kidpr_view"
  v$site <- if (is.list(site)) site else list()
  v$h <- horizons
  v$cache <- new.env(parent = emptyenv())
  thick <- f$bottom_cm - f$top_cm
  thick[!is.na(thick) & thick < 0] <- 0
  kind <- .kidpr_dv_kinds(f)
  v$f <- f; v$n <- n; v$kind <- kind; v$thick <- thick
  v$buried <- as.integer(f$buried)
  # the ground: the top of the first row, past fresh litter
  start <- 1L
  while (start <= n && kind[start] == "litter") start <- start + 1L
  v$start <- start
  v$ground <- if (start <= n) f$top_cm[start] else NA_real_
  # the top of the description, litter included (the guard of Q19)
  v$desc_top <- if (n) f$top_cm[1L] else NA_real_
  # depths below the ground (the first row of the profile at 0)
  rt <- f$top_cm - v$ground
  if (start <= n) rt[start] <- 0
  v$rtop <- rt
  v$dep <- .kidpr_dv_deposit(v)
  # the profile: under a deposit of 40 cm or less (a buried soil directly
  # under it is the soil keyed), else the rows that are not buried
  bur <- v$buried
  rows <- if (start <= n) seq.int(start, n) else integer(0)
  dep <- v$dep
  if (length(dep$rows) && isTRUE(dep$thick <= 40)) {
    after <- setdiff(rows, dep$rows)
    if (length(after) && bur[after[1L]] > 0L) {
      lev <- bur[after[1L]]
      k <- 1L
      while (k <= length(after) && bur[after[k]] == lev) { bur[after[k]] <- 0L; k <- k + 1L }
    }
    rows <- after
  }
  v$bur <- bur
  v$prof <- rows[bur[rows] == 0L]
  v$base <- if (length(v$prof)) f$top_cm[v$prof[1L]] else v$ground
  .kidpr_dv_horizons(v)
  v
}

# The deposit at the surface (key 2.2, step 2; Q21): the run of rows from the
# ground made of the technogenic formation TPO, the deposit of horizon key
# position 38b, stratified humus horizons (RJ, RY, RU), small deposited
# layers (r, rh, ur, rr, rx) and loose rock (C, D, not buried); the loose
# rock counts only when a buried soil or an organic or humus horizon lies
# under the run (else it is the parent material under the deposit), TPO and
# the deposit of 38b always. Humified when it holds RJ, RY or RU.
.kidpr_dv_deposit <- function(v) {
  f <- v$f; kind <- v$kind
  none <- list(rows = integer(0), thick = 0, humified = FALSE, note = character(0))
  i <- v$start
  run <- integer(0)
  while (i <= v$n) {
    k <- kind[i]; cd <- f$code[i]
    if (v$buried[i] > 0L) break
    is_dep <- (k == "horizon" && cd %in% .KIDPR_DV_RSTAR) ||
      (k == "small" && any(f$features[[i]] %in% .KIDPR_DV_DEPOSIT_FEATURES)) ||
      (k == "rock" && (cd %in% c("C", "D") || is.na(cd))) || k %in% c("nonsoil", "deposit")
    if (!is_dep) break
    run <- c(run, i)
    i <- i + 1L
  }
  if (!length(run)) return(none)
  # loose rock belongs to the deposit only over a buried soil or an organic
  # or humus horizon; otherwise it is the parent material under the
  # stratified horizons or the deposited small layers
  after <- if (i <= v$n) i else NA_integer_
  over <- !is.na(after) && (v$buried[after] > 0L ||
    (kind[after] == "horizon" && f$code[after] %in% c(.KIDPR_DV_ORGANIC, .KIDPR_DV_HUMUS, .KIDPR_DV_RSTAR)))
  if (!over) {
    dep <- which(kind[run] != "rock")
    if (!length(dep)) return(none)
    run <- run[seq_len(max(dep))]
  }
  rstar <- any(f$code[run] %in% .KIDPR_DV_RSTAR)
  th <- sum(v$thick[run])
  lab <- ifelse(!is.na(f$designation[run]), f$designation[run],
                ifelse(kind[run] == "deposit", "deposit", NA_character_))
  list(rows = run, thick = th, humified = rstar,
       note = sprintf("a deposit of %s cm at the surface (%s)%s", format(th),
                      paste(stats::na.omit(lab), collapse = ", "),
                      if (rstar) ", humified (RJ, RY or RU)" else ""))
}

# Overlays that are no horizon of the key (v0.9.338, Hugo, 2026-11-02): a V
# laid over the horizon of a layer is a feature of that layer for every step
# that reads middle horizons (C.5, C.8, C.13 ...); only a V that is the
# horizon of its own layer counts.
.KIDPR_DV_FEATURE_OVERLAYS <- "V"

# One horizon entry of the profile: its code, kind, overlays (S and X; a V
# overlay is a feature), candidates, thickness, top (and its depth below the
# ground) and rows.
.kidpr_dv_entry <- function(v, rows) {
  f <- v$f
  i <- rows[1L]
  list(code = f$code[i], kind = v$kind[i], rows = rows,
       overlay = setdiff(unique(unlist(f$overlay[rows])), .KIDPR_DV_FEATURE_OVERLAYS),
       overlay_na = setdiff(unique(unlist(f$overlay_na[rows])), .KIDPR_DV_FEATURE_OVERLAYS),
       cands = if (v$kind[i] == "undecided") as.character(f$candidates[[i]]) else character(0),
       missing = unique(unlist(f$missing[rows])),
       thick = sum(v$thick[rows]), top = f$top_cm[i], rtop = v$rtop[i], bottom = f$bottom_cm[rows[length(rows)]])
}

# The horizons of the profile in order (sub-horizons merged; small horizons,
# transitions and litter out) and the surface by the thickest-horizon rule
# (Q11), once per reading the missing thicknesses allow.
.kidpr_dv_horizons <- function(v) {
  s <- v$prof[!v$kind[v$prof] %in% c("small", "trans", "litter")]
  f <- v$f
  ent <- list()
  k <- 1L
  while (k <= length(s)) {
    i <- s[k]
    rows <- i
    if (v$kind[i] %in% c("horizon", "rock", "nonsoil")) {
      while (k < length(s) && v$kind[s[k + 1L]] == v$kind[i] && identical(f$code[s[k + 1L]], f$code[i])) {
        k <- k + 1L
        rows <- c(rows, s[k])
      }
    }
    ent[[length(ent) + 1L]] <- .kidpr_dv_entry(v, rows)
    k <- k + 1L
  }
  # the surface run: the organic, humus and agro horizons that start it
  run <- 0L
  while (run < length(ent) && ent[[run + 1L]]$kind == "horizon" && ent[[run + 1L]]$code %in% .KIDPR_DV_SURFACE)
    run <- run + 1L
  nxt <- if (run < length(ent)) ent[[run + 1L]] else NULL
  v$run_open <- !is.null(nxt) && nxt$kind == "undecided" &&
    (!length(nxt$cands) || any(nxt$cands %in% .KIDPR_DV_SURFACE))
  v$hz_raw <- ent
  codes <- vapply(ent, function(e) e$code %||% NA_character_, "")
  th <- vapply(ent, function(e) as.numeric(e$thick), 0)
  v$var <- lapply(.kidpr_dv_drops(codes, th, run, nxt), function(d) .kidpr_dv_reading(ent, codes, run, d))
  .kidpr_dv_use(v, v$var[[1L]])
  invisible(v)
}

# The horizons the rule leaves out (as features), one set per reading: the
# natural horizons of the surface run other than the thickest (every one
# that may be the thickest when a thickness is missing), or a W of 5 cm or
# less over another horizon (p. 39), both ways when its thickness is missing.
.kidpr_dv_drops <- function(codes, th, run, nxt) {
  if (run == 0L) return(list(integer(0)))
  rc <- codes[seq_len(run)]
  nat <- which(rc %in% .KIDPR_DV_NATURAL)
  if (length(nat) > 1L) {
    t <- th[nat]
    known <- !is.na(t)
    keepers <- c(if (any(known)) nat[known][which.max(t[known])], nat[!known])
    return(lapply(unique(keepers), function(k) setdiff(nat, k)))
  }
  if (length(nat) == 1L && rc[nat] == "W" && !any(rc %in% .KIDPR_DV_AGRO) && !is.null(nxt) &&
      nxt$kind %in% c("horizon", "undecided")) {
    if (is.na(th[nat])) return(list(nat, integer(0)))
    if (th[nat] <= 5) return(list(nat))
  }
  list(integer(0))
}

# One reading of the surface: the horizons kept, the length of the surface
# run, its features and the surface horizon (the agro horizon, else the
# natural one kept).
.kidpr_dv_reading <- function(ent, codes, run, drop) {
  keep <- setdiff(seq_along(ent), drop)
  hz <- ent[keep]
  run_n <- run - length(drop)
  sc <- if (run_n > 0L) vapply(hz[seq_len(run_n)], `[[`, "", "code") else character(0)
  ag <- which(sc %in% .KIDPR_DV_AGRO)
  list(hz = hz, run_n = run_n, run_features = codes[drop],
       surface = if (length(ag)) ag[1L] else if (length(sc)) 1L else NA_integer_)
}
.kidpr_dv_use <- function(v, w) for (k in names(w)) assign(k, w[[k]], envir = v)

# A predicate read on every reading of the surface: decided when they agree,
# else undecided (the thickness that decides the rule is asked for).
.kidpr_dv_each <- function(v, f) {
  vars <- v$var
  if (length(vars) <= 1L) return(f())
  on.exit(.kidpr_dv_use(v, vars[[1L]]), add = TRUE)
  rs <- lapply(vars, function(w) { .kidpr_dv_use(v, w); f() })
  p <- vapply(rs, function(r) as.logical(r$passed)[1L], NA)
  if (all(p %in% TRUE) || all(p %in% FALSE)) return(rs[[1L]])
  r1 <- rs[[1L]]
  DiagnosticResult$new(name = r1$name, passed = NA, layers = integer(0),
                       evidence = c(r1$evidence, list(surface_rule = paste(
                         "the thickest organic or humus horizon at the surface (p. 37) is not decided:",
                         "a thickness is missing"))),
                       missing = unique(c(unlist(lapply(rs, function(r) r$missing)), "top_cm", "bottom_cm")),
                       reference = r1$reference)
}

# Is an entry (or a row read as one) one of the horizons K?
.kidpr_dv_isk <- function(e, K) {
  if (identical(e$kind, "undecided")) {
    cd <- e$cands
    if (!length(cd)) return(NA)
    if (all(cd %in% K)) return(TRUE)
    if (any(cd %in% K) || any(e$overlay_na %in% K)) return(NA)
    return(FALSE)
  }
  if ((!is.na(e$code) && e$code %in% K) || any(e$overlay %in% K)) return(TRUE)
  if (any(e$overlay_na %in% K)) return(NA)
  FALSE
}

# The view of a pedon: identify_horizons() read once.
.kidpr_dv_pedon <- function(pedon) {
  frame <- identify_horizons(pedon, "kidpr2008")
  h <- pedon$horizons
  .kidpr_dv(frame, pedon$site %||% list(),
            if (is.null(h)) NULL else as.data.frame(h, stringsAsFactors = FALSE))
}

# The view a predicate reads: the one of the classification in progress, a
# view passed in place of the pedon, or one built for this call.
.kidpr_dv_of <- function(pedon) {
  if (inherits(pedon, "kidpr_view")) return(pedon)
  st <- .kidpr_dv_state
  if (!is.null(st$pedon) && identical(st$pedon, pedon)) return(st$view)
  .kidpr_dv_pedon(pedon)
}

# A predicate answered once per view and argument set.
.kidpr_dv_memo <- function(v, key, f) {
  r <- v$cache[[key]]
  if (!is.null(r)) return(r)
  r <- f()
  assign(key, r, envir = v$cache)
  r
}

.kidpr_dv_key <- function(...) {
  a <- list(...)
  for (k in seq_along(a)) {
    x <- a[[k]]
    a[[k]] <- if (is.list(x)) paste(vapply(x, paste, "", collapse = "+"), collapse = ";")
              else paste(x, collapse = ",")
  }
  paste(unlist(a), collapse = "|")
}

# A horizon attribute of a row of the frame (NA when absent).
.kidpr_dv_attr <- function(v, i, name) {
  h <- v$h
  if (is.null(h) || is.null(h[[name]])) return(NA)
  j <- v$f$horizon_idx[i]
  if (is.na(j) || j < 1L || j > nrow(h)) return(NA)
  h[[name]][[j]]
}

# The rows from the ground down.
.kidpr_dv_rows <- function(v) if (v$start <= v$n) seq.int(v$start, v$n) else integer(0)


# --- the predicates --------------------------------------------------------------------

#' Trunks and divisions of the Russian soil classification (internal)
#'
#' The predicates of the keys to trunks and divisions of the Russian soil
#' classification (Field Guide of 2008, keys 2.2 and 2.3), read by
#' \code{\link{classify_kidpr2008}} through the flattened key of
#' \code{inst/rules/kidpr2008/key.yaml} and \code{divisions/}. They read the
#' identified horizons of the profile (\code{\link{identify_horizons}}:
#' the Russian notation where the profile is written in it, else key 2.1
#' on the data) and a few site facts. Each returns a
#' \code{\link{DiagnosticResult}}: TRUE, FALSE, or NA when an undecided
#' layer or a missing record (a depth among them) could change the answer.
#'
#' The profile is read top-down from the ground. A deposit at the surface
#' (\code{surface_deposit_kidpr}) is the run of technogenic formations
#' (TPO), deposits (horizon key position 38b), stratified humus horizons
#' (RJ, RY, RU), small deposited layers (r, rh, ur, rr, rx) and loose rock
#' over a buried soil or an organic or humus horizon; under a deposit of
#' 40 cm or less the soil below is the one keyed. Sub-horizons are merged,
#' small horizons and transitions left out (the FAO and WRB transitions AB,
#' BC, E/B ... too, when the key on the data finds no diagnostic horizon in
#' them); of the organic, humus and agro horizons that start the profile the
#' agro horizon is kept and, of the natural ones, the thickest (the others
#' are features; a W of 5 cm or less over another horizon is the feature w).
#'
#' \describe{
#'   \item{\code{has_horizon_kidpr}}{One of the horizons is in the profile
#'     (an overlay S or X counts; a V laid over another horizon is a
#'     feature of its layer).}
#'   \item{\code{starts_at_surface_kidpr}}{The first horizon of the profile
#'     (at the surface, or under a deposit of 40 cm or less) is one of the
#'     horizons.}
#'   \item{\code{surface_horizon_kidpr}}{The surface horizon (the agro
#'     horizon, else the thickest natural organic or humus horizon that starts
#'     the profile) is one of the horizons.}
#'   \item{\code{directly_over_kidpr}}{One of the \code{upper} horizons lies
#'     directly over one of the \code{lower} ones (transitions and small
#'     horizons between them do not count; layers with no diagnostic horizon up
#'     to \code{gap_cm} thick are passed over).}
#'   \item{\code{below_kidpr}}{One of the \code{lower} horizons lies
#'     somewhere below one of the \code{upper} ones.}
#'   \item{\code{only_horizons_kidpr}}{The horizons of the profile (rock and
#'     parent material left out), read after the thickest-horizon rule, are
#'     exactly one of the \code{sets}; with \code{raw = TRUE} read before it.}
#'   \item{\code{rock_within_kidpr}}{Hard rock (R) starts within
#'     \code{max_cm} of the soil surface, organic horizons included (key C.1).}
#'   \item{\code{turbated_kidpr}}{A turbated horizon TUR more than
#'     \code{min_cm} thick (Hugo's decision Q20).}
#'   \item{\code{peat_body_kidpr}}{A peat body of more than \code{min_cm}:
#'     the organic layers contiguous from the surface (or under a deposit of
#'     40 cm or less), muck and the agro-peat horizons included (key 2.2,
#'     step 6; Hugo's decision Q22); a layer the data leave undecided between
#'     peat horizons counts. A description that ends in peat short of it
#'     leaves it undecided.}
#'   \item{\code{surface_deposit_kidpr}}{A deposit at the surface (key 2.2,
#'     steps 2-4; Hugo's decision Q21) more than \code{min_cm} thick, with
#'     (\code{humified = TRUE}) or without (\code{FALSE}) a stratified humus
#'     horizon RJ, RY or RU.}
#'   \item{\code{stratified_body_kidpr}}{A stratified humus body from the
#'     surface, the agro horizons P and PU over RJ, RY or RU counted with them,
#'     of \code{min_cm} or more (the stratozems, B.2; Q21).}
#'   \item{\code{soil_horizon_within_kidpr}}{A soil horizon (of the profile
#'     or of a buried soil) starts within \code{max_cm} of the ground (key 2.2,
#'     step 1). A layer the data leave undecided between soil horizons is one;
#'     a layer in which the key on the data finds no diagnostic horizon
#'     (position 66) proves nothing, since it may be a transition; only rock,
#'     TPO and the deposit of position 38b rule a soil horizon out.}
#'   \item{\code{floodplain_kidpr}}{The soil lies within a river floodplain
#'     on modern alluvium (B.1; Hugo's decision Q21): a floodplain recorded
#'     (landform or landform element; a terrace is none) and alluvial
#'     material (the alluvial mark of the notation, or a fluvial or lacustrine
#'     origin of a layer or of the parent material; marine and glaciofluvial
#'     material and outwash do not count); with no landform recorded,
#'     alluvial material that soilKey's fluvic material confirms; alluvial
#'     material alone leaves it undecided. Nothing recorded: not a floodplain
#'     soil.}
#'   \item{\code{volcanic_ash_kidpr}}{The soil is developed in volcanic ash:
#'     the ash mark of the notation, an ash, tephra or pyroclastic origin of a
#'     layer or of the parent material, or 30 % volcanic glass. Not recorded:
#'     no ash. The key does not read it: the ochre horizon BAN is formed in
#'     ash by its definition, so BAN alone makes the volcanic soils (Hugo's
#'     decision of 2026-11-02).}
#'   \item{\code{contaminated_x_kidpr}}{The chemically contaminated horizon X
#'     in a layer starting within \code{within_cm} (C.3; Hugo's decision Q23):
#'     an X of the profile, or pollutants of the extremely hazardous category
#'     (\code{contamination_hazard}); contamination recorded without its
#'     category (\code{site$contaminated}, \code{contamination_type}) leaves it
#'     undecided; nothing recorded, no X.}
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param codes,upper,lower Russian horizon codes (\code{"BT"},
#'   \code{"G"} ...).
#' @param within_cm A layer starting less than this depth below the ground
#'   (NULL: any depth); a layer of unknown depth leaves it undecided.
#' @param buried Also read the horizons of buried soils.
#' @param gap_cm Thickness of layers with no diagnostic horizon allowed
#'   between \code{upper} and \code{lower}.
#' @param sets A list of code vectors: the profile's horizons must be
#'   exactly one of them.
#' @param raw For \code{only_horizons_kidpr}: read every horizon of the
#'   profile, before the thickest-horizon rule of the surface.
#' @param max_cm,min_cm Depth or thickness limits (see each predicate).
#' @param from_ground_zero For \code{starts_at_surface_kidpr}: a TRUE
#'   answer stands only when the description starts at 0 cm (fresh litter
#'   included), else it is undecided (a truncated soil, Hugo's decision
#'   Q19).
#' @param humified For \code{surface_deposit_kidpr}: TRUE a deposit with
#'   RJ, RY or RU, FALSE one without, NULL either.
#' @return A \code{\link{DiagnosticResult}}.
#' @keywords internal
#' @name kidpr2008_divisions
NULL

#' @rdname kidpr2008_divisions
#' @export
has_horizon_kidpr <- function(pedon, codes, within_cm = NULL, buried = FALSE) {
  v <- .kidpr_dv_of(pedon)
  codes <- as.character(unlist(codes))
  .kidpr_dv_memo(v, .kidpr_dv_key("has", codes, within_cm %||% "", buried), function() .kidpr_dv_each(v, function() {
    ents <- v$hz
    if (isTRUE(buried)) {
      rows <- seq_len(v$n)
      rows <- rows[rows >= v$start & v$bur[rows] > 0L & !v$kind[rows] %in% c("small", "trans", "litter")]
      ents <- c(ents, lapply(rows, function(i) .kidpr_dv_entry(v, i)))
    }
    r <- vapply(ents, .kidpr_dv_isk, NA, K = codes)
    depth_na <- FALSE
    if (!is.null(within_cm) && length(ents)) {
      w <- vapply(ents, function(e) .kidpr_dv_lt(e$rtop, within_cm), NA)
      depth_na <- any(is.na(w) & !(r %in% FALSE))
      r <- r & w
    }
    p <- if (length(r)) .kidpr_dv_or(r) else FALSE
    .kidpr_dv_result("has_horizon_kidpr", p,
                     unlist(lapply(ents[r %in% TRUE], `[[`, "rows")),
                     list(codes = codes, within_cm = within_cm),
                     c(unlist(lapply(ents[is.na(r)], `[[`, "missing")), if (depth_na) "top_cm"),
                     "(horizon present)")
  }))
}

#' @rdname kidpr2008_divisions
#' @export
starts_at_surface_kidpr <- function(pedon, codes, from_ground_zero = FALSE) {
  v <- .kidpr_dv_of(pedon)
  codes <- as.character(unlist(codes))
  .kidpr_dv_memo(v, .kidpr_dv_key("starts", codes, from_ground_zero), function() .kidpr_dv_each(v, function() {
    if (!length(v$hz)) return(.kidpr_dv_result("starts_at_surface_kidpr", FALSE, where = "(at the surface)"))
    e <- v$hz[[1L]]
    p <- .kidpr_dv_isk(e, codes)
    # a description that does not start at 0 cm (fresh litter included) may
    # have left out the top of the soil (Hugo's decision Q19)
    guard <- isTRUE(from_ground_zero) && isTRUE(p) && !isTRUE(v$desc_top == 0)
    if (guard) p <- NA
    .kidpr_dv_result("starts_at_surface_kidpr", p, if (isTRUE(p)) e$rows,
                     list(codes = codes, first = e$code, under_deposit = length(v$dep$rows) > 0L,
                          ground_cm = v$ground, description_top_cm = v$desc_top),
                     c(e$missing, if (guard) "top_cm"), "(at the surface or under a deposit of 40 cm or less)")
  }))
}

#' @rdname kidpr2008_divisions
#' @export
surface_horizon_kidpr <- function(pedon, codes) {
  v <- .kidpr_dv_of(pedon)
  codes <- as.character(unlist(codes))
  .kidpr_dv_memo(v, .kidpr_dv_key("surface", codes), function() .kidpr_dv_each(v, function() {
    s <- v$surface
    if (is.na(s)) {
      p <- if (isTRUE(v$run_open)) NA else FALSE
      return(.kidpr_dv_result("surface_horizon_kidpr", p, evidence = list(codes = codes, surface = NA),
                              missing = if (is.na(p)) v$hz[[1L]]$missing, where = "(surface horizon, p. 37)"))
    }
    e <- v$hz[[s]]
    p <- .kidpr_dv_isk(e, codes)
    # an undecided layer that may still belong to the surface run could change
    # a natural surface horizon (the thickest wins); an agro one stays
    if (isTRUE(v$run_open) && !(e$code %in% .KIDPR_DV_AGRO)) p <- if (isTRUE(p) || isFALSE(p)) NA else p
    .kidpr_dv_result("surface_horizon_kidpr", p, if (isTRUE(p)) e$rows,
                     list(codes = codes, surface = e$code, features = v$run_features),
                     if (is.na(p)) unlist(lapply(v$hz[seq_len(min(length(v$hz), v$run_n + 1L))], `[[`, "missing")),
                     "(surface horizon, p. 37)")
  }))
}

#' @rdname kidpr2008_divisions
#' @export
directly_over_kidpr <- function(pedon, upper, lower, gap_cm = 0) {
  v <- .kidpr_dv_of(pedon)
  upper <- as.character(unlist(upper)); lower <- as.character(unlist(lower))
  .kidpr_dv_memo(v, .kidpr_dv_key("over", upper, lower, gap_cm), function() .kidpr_dv_each(v, function() {
    hz <- v$hz
    n <- length(hz)
    r <- logical(0); lay <- integer(0); miss <- character(0)
    for (i in seq_len(n)) {
      a <- .kidpr_dv_isk(hz[[i]], upper)
      if (isFALSE(a)) next
      # layers with no diagnostic horizon passed over, up to gap_cm (one of
      # unknown thickness may or may not fit)
      j <- i + 1L; g <- 0; unsure <- FALSE
      while (gap_cm > 0 && j <= n && hz[[j]]$kind == "none") {
        t <- hz[[j]]$thick
        if (is.na(t)) unsure <- TRUE else if (g + t > gap_cm) break else g <- g + t
        j <- j + 1L
      }
      b <- if (j <= n) .kidpr_dv_isk(hz[[j]], lower) else FALSE
      if (unsure && isTRUE(b)) b <- NA
      ab <- .kidpr_dv_and(c(a, b))
      r <- c(r, ab)
      if (isTRUE(ab)) lay <- c(lay, hz[[i]]$rows, hz[[j]]$rows)
      if (is.na(ab)) miss <- c(miss, hz[[i]]$missing, if (j <= n) hz[[j]]$missing, if (unsure) c("top_cm", "bottom_cm"))
    }
    p <- if (length(r)) .kidpr_dv_or(r) else FALSE
    .kidpr_dv_result("directly_over_kidpr", p, lay, list(upper = upper, lower = lower, gap_cm = gap_cm),
                     miss, "(directly over)")
  }))
}

#' @rdname kidpr2008_divisions
#' @export
below_kidpr <- function(pedon, upper, lower) {
  v <- .kidpr_dv_of(pedon)
  upper <- as.character(unlist(upper)); lower <- as.character(unlist(lower))
  .kidpr_dv_memo(v, .kidpr_dv_key("below", upper, lower), function() .kidpr_dv_each(v, function() {
    hz <- v$hz
    n <- length(hz)
    a <- vapply(hz, .kidpr_dv_isk, NA, K = upper)
    b <- vapply(hz, .kidpr_dv_isk, NA, K = lower)
    r <- logical(0); lay <- integer(0); miss <- character(0)
    for (i in seq_len(n)) {
      if (isFALSE(a[i]) || i == n) next
      for (j in seq.int(i + 1L, n)) {
        ab <- .kidpr_dv_and(c(a[i], b[j]))
        r <- c(r, ab)
        if (isTRUE(ab)) lay <- c(lay, hz[[i]]$rows, hz[[j]]$rows)
        if (is.na(ab)) miss <- c(miss, hz[[i]]$missing, hz[[j]]$missing)
      }
    }
    p <- if (length(r)) .kidpr_dv_or(r) else FALSE
    .kidpr_dv_result("below_kidpr", p, lay, list(upper = upper, lower = lower), miss, "(lies below)")
  }))
}

#' @rdname kidpr2008_divisions
#' @export
only_horizons_kidpr <- function(pedon, sets, raw = FALSE) {
  v <- .kidpr_dv_of(pedon)
  sets <- lapply(sets, function(s) sort(unique(as.character(unlist(s)))))
  .kidpr_dv_memo(v, .kidpr_dv_key("only", sets, raw), function() .kidpr_dv_each(v, function() {
    hz <- if (isTRUE(raw)) v$hz_raw else v$hz
    dec <- unique(unlist(lapply(hz, function(e) {
      if (e$kind == "horizon") c(e$code, e$overlay) else if (e$kind == "undecided") character(0) else e$overlay
    })))
    und <- Filter(function(e) e$kind == "undecided" || length(e$overlay_na), hz)
    match1 <- function(s) any(vapply(sets, function(t) identical(sort(unique(s)), t), NA))
    if (!length(und)) {
      p <- match1(dec)
    } else {
      # every way the undecided layers could be read ("" no horizon)
      opts <- lapply(und, function(e) {
        cd <- if (e$kind == "undecided") e$cands else character(0)
        o <- if (e$kind == "undecided" && !length(cd)) NA_character_ else setdiff(c(cd, ""), .KIDPR_DV_ROCKS)
        if (length(e$overlay_na)) o <- unique(c(o, paste0(o, "+", e$overlay_na)))
        unique(o)
      })
      if (any(vapply(opts, function(o) anyNA(o), NA)) || prod(lengths(opts)) > 256) {
        p <- NA
      } else {
        grid <- expand.grid(opts, stringsAsFactors = FALSE)
        m <- apply(grid, 1L, function(row) {
          add <- unlist(strsplit(row, "+", fixed = TRUE))
          match1(c(dec, add[nzchar(add)]))
        })
        p <- if (all(m)) TRUE else if (!any(m)) FALSE else NA
      }
    }
    .kidpr_dv_result("only_horizons_kidpr", p, if (isTRUE(p)) unlist(lapply(hz, `[[`, "rows")),
                     list(horizons = sort(dec), sets = vapply(sets, paste, "", collapse = "+")),
                     unlist(lapply(und, `[[`, "missing")), "(the profile consists of)")
  }))
}

#' @rdname kidpr2008_divisions
#' @export
rock_within_kidpr <- function(pedon, max_cm = 30) {
  v <- .kidpr_dv_of(pedon)
  .kidpr_dv_memo(v, .kidpr_dv_key("rock", max_cm), function() {
    rows <- .kidpr_dv_rows(v)
    rows <- rows[v$buried[rows] == 0L]
    top <- v$rtop[rows]
    isr <- !is.na(v$f$code[rows]) & v$f$code[rows] == "R"
    hit <- rows[isr & !is.na(top) & top <= max_cm]
    # an R of unknown depth may lie within the limit
    rna <- rows[isr & is.na(top)]
    und <- rows[v$kind[rows] == "undecided" & (is.na(top) | top <= max_cm) &
                  vapply(rows, function(i) { cd <- v$f$candidates[[i]]; !length(cd) || "R" %in% cd }, NA)]
    p <- if (length(hit)) TRUE else if (length(rna) || length(und)) NA else FALSE
    known <- top[isr & !is.na(top)]
    depth <- if (length(known)) min(known) else NA_real_
    .kidpr_dv_result("rock_within_kidpr", p, hit, list(rock_top_cm = depth, max_cm = max_cm),
                     c(unlist(v$f$missing[und]), if (length(rna)) "top_cm"), "(C.1, hard rock)")
  })
}

#' @rdname kidpr2008_divisions
#' @export
turbated_kidpr <- function(pedon, min_cm = 40) {
  v <- .kidpr_dv_of(pedon)
  .kidpr_dv_memo(v, .kidpr_dv_key("tur", min_cm), function() .kidpr_dv_each(v, function() {
    tur <- Filter(function(e) e$kind == "horizon" && identical(e$code, "TUR"), v$hz)
    und <- Filter(function(e) e$kind == "undecided" && (!length(e$cands) || "TUR" %in% e$cands), v$hz)
    th <- vapply(tur, `[[`, 0, "thick")
    p <- if (any(th > min_cm, na.rm = TRUE)) TRUE
         else if (anyNA(th) || length(und)) NA else FALSE
    .kidpr_dv_result("turbated_kidpr", p, unlist(lapply(tur[th > min_cm & !is.na(th)], `[[`, "rows")),
                     list(tur_thickness_cm = th, min_cm = min_cm),
                     c(if (anyNA(th)) c("top_cm", "bottom_cm"), unlist(lapply(und, `[[`, "missing"))),
                     "(TUR, C.15; Q20)")
  }))
}

#' @rdname kidpr2008_divisions
#' @export
peat_body_kidpr <- function(pedon, min_cm = 50) {
  v <- .kidpr_dv_of(pedon)
  .kidpr_dv_memo(v, .kidpr_dv_key("peat", min_cm), function() {
    tot <- 0; thick_na <- FALSE; used <- integer(0); open <- FALSE; miss <- character(0); ended <- TRUE
    for (i in v$prof) {
      k <- v$kind[i]
      if (k %in% c("small", "litter")) next
      cd <- v$f$code[i]
      cds <- if (k == "undecided") as.character(v$f$candidates[[i]]) else character(0)
      # a peat horizon, or a layer the data leave between peat horizons
      peat <- (k %in% c("horizon", "rock") && cd %in% .KIDPR_DV_PEAT) ||
        (k == "undecided" && length(cds) > 0L && all(cds %in% .KIDPR_DV_PEAT))
      if (peat) {
        if (is.na(v$thick[i])) thick_na <- TRUE else tot <- tot + v$thick[i]
        used <- c(used, i)
        next
      }
      # a layer that may or may not be peat stops the body undecided
      if (k == "undecided" && (!length(cds) || any(cds %in% .KIDPR_DV_PEAT))) {
        open <- TRUE
        miss <- c(miss, v$f$missing[[i]])
      }
      ended <- FALSE
      break
    }
    p <- if (tot > min_cm) TRUE
         else if (!length(used)) (if (open) NA else FALSE)
         else if (thick_na || open || ended) NA else FALSE
    .kidpr_dv_result("peat_body_kidpr", p, if (isTRUE(p)) used,
                     list(peat_cm = tot, min_cm = min_cm, ends_in_peat = length(used) > 0L && ended),
                     c(miss, if (thick_na) c("top_cm", "bottom_cm"), if (length(used) && ended) "bottom_cm"),
                     "(2.2, step 6; Q22)")
  })
}

#' @rdname kidpr2008_divisions
#' @export
surface_deposit_kidpr <- function(pedon, min_cm = 40, humified = NULL) {
  v <- .kidpr_dv_of(pedon)
  .kidpr_dv_memo(v, .kidpr_dv_key("deposit", min_cm, humified %||% ""), function() {
    d <- v$dep
    p <- if (!length(d$rows)) FALSE else if (is.na(d$thick)) NA
         else d$thick > min_cm && (is.null(humified) || identical(d$humified, isTRUE(humified)))
    .kidpr_dv_result("surface_deposit_kidpr", p, d$rows,
                     list(deposit_cm = d$thick, humified = d$humified, note = d$note),
                     if (is.na(p)) c("top_cm", "bottom_cm"), "(2.2, steps 2-4; Q21)")
  })
}

#' @rdname kidpr2008_divisions
#' @export
stratified_body_kidpr <- function(pedon, min_cm = 40) {
  v <- .kidpr_dv_of(pedon)
  .kidpr_dv_memo(v, .kidpr_dv_key("strat", min_cm), function() {
    rows <- .kidpr_dv_rows(v)
    rows <- rows[v$buried[rows] == 0L]
    tot <- 0; used <- integer(0); open <- FALSE
    for (i in rows) {
      k <- v$kind[i]
      if (k %in% c("small", "litter")) next
      cd <- v$f$code[i]
      # v0.9.338: the agro horizon counts only over the stratified horizons
      # (BC07-BC10: P-RY-D, PU-RU-D), not under them
      seen <- any(v$f$code[used] %in% .KIDPR_DV_RSTAR)
      if (k == "horizon" && (cd %in% .KIDPR_DV_RSTAR || (cd %in% c("P", "PU") && !seen))) {
        tot <- tot + v$thick[i]; used <- c(used, i); next
      }
      if (k == "undecided") {
        cds <- v$f$candidates[[i]]
        open <- !length(cds) || any(cds %in% .KIDPR_DV_RSTAR)
      }
      break
    }
    rstar <- any(v$f$code[used] %in% .KIDPR_DV_RSTAR)
    p <- if (!rstar) (if (open) NA else FALSE) else if (is.na(tot)) NA
         else if (tot >= min_cm) TRUE else if (open) NA else FALSE
    .kidpr_dv_result("stratified_body_kidpr", p, if (isTRUE(p)) used,
                     list(body_cm = tot, min_cm = min_cm), if (is.na(p)) "designation", "(B.2; Q21)")
  })
}

#' @rdname kidpr2008_divisions
#' @export
soil_horizon_within_kidpr <- function(pedon, max_cm = 100) {
  v <- .kidpr_dv_of(pedon)
  .kidpr_dv_memo(v, .kidpr_dv_key("soil", max_cm), function() {
    rows <- .kidpr_dv_rows(v)
    f <- v$f
    within <- .kidpr_dv_lt(v$rtop[rows], max_cm)
    kind <- v$kind[rows]
    cands <- f$candidates[rows]
    soil_code <- function(cd) nzchar(cd) & grepl("^[A-Z]", cd) & !cd %in% c(.KIDPR_DV_ROCKS, "TPO")
    # a layer the data leave between soil horizons is a soil horizon
    und_soil <- kind == "undecided" & vapply(cands, function(cd) length(cd) > 0L && all(soil_code(cd)), NA)
    hor <- kind %in% c("horizon", "trans") | lengths(f$overlay[rows]) > 0L | und_soil
    # an undecided layer that may hold a soil horizon, and a layer with no
    # diagnostic horizon (key 2.1, position 66: it may be a transition), prove
    # nothing; rock, TPO and the deposit of position 38b rule one out
    maybe <- (kind == "undecided" & !und_soil &
                vapply(cands, function(cd) !length(cd) || any(soil_code(cd) | !nzchar(cd)), NA)) |
      kind == "none"
    p <- if (any(hor & within %in% TRUE)) TRUE
         else if (any(hor & is.na(within)) || any(maybe & !(within %in% FALSE))) NA
         else FALSE
    mrows <- rows[maybe & kind == "undecided"]
    .kidpr_dv_result("soil_horizon_within_kidpr", p, rows[hor & within %in% TRUE], list(max_cm = max_cm),
                     c(unlist(f$missing[mrows]), if (any(maybe & kind == "none")) "designation",
                       if (anyNA(within)) "top_cm"), "(2.2, step 1)")
  })
}

#' @rdname kidpr2008_divisions
#' @export
floodplain_kidpr <- function(pedon) {
  v <- .kidpr_dv_of(pedon)
  .kidpr_dv_memo(v, "floodplain", function() {
    lf <- paste(stats::na.omit(c(.kidpr_dv_text(v$site$landform), .kidpr_dv_text(v$site$landform_element))),
                collapse = "; ")
    fp <- if (!nzchar(lf)) NA
          else if (grepl(.KIDPR_DV_NOT_FLOODPLAIN, lf, perl = TRUE)) FALSE
          else if (grepl(.KIDPR_DV_FLOODPLAIN, lf, perl = TRUE)) TRUE
          else if (grepl(.KIDPR_DV_VALLEY, lf, perl = TRUE)) NA else FALSE
    rows <- v$prof
    mark <- any(v$f$mark[rows] %in% "~~", na.rm = TRUE)
    org <- unlist(lapply(rows, function(i) c(.kidpr_dv_text(.kidpr_dv_attr(v, i, "layer_origin")),
                                            .kidpr_dv_text(.kidpr_dv_attr(v, i, "rock_origin")))))
    org <- c(stats::na.omit(org), stats::na.omit(.kidpr_dv_text(v$site$parent_material)))
    allu_txt <- .kidpr_dv_match(.KIDPR_DV_ALLUVIUM, org) & !.kidpr_dv_match(.KIDPR_DV_NOT_ALLUVIUM, org)
    alluvial <- if (mark || any(allu_txt)) TRUE else if (length(org)) FALSE else NA
    fluvic <- function() {
      if (inherits(pedon, "kidpr_view") || !inherits(pedon, "PedonRecord")) return(NA)
      r <- tryCatch(suppressWarnings(fluvic_material(pedon)), error = function(e) NULL)
      if (is.null(r)) NA else as.logical(r$passed)
    }
    fl <- NA
    p <- if (isFALSE(fp) || isFALSE(alluvial)) FALSE
         else if (isTRUE(fp) && isTRUE(alluvial)) TRUE
         else if (is.na(fp) && is.na(alluvial) && !nzchar(lf)) FALSE
         else {
           fl <- fluvic()
           if (isTRUE(fl) && (isTRUE(fp) || isTRUE(alluvial))) TRUE else NA
         }
    .kidpr_dv_result("floodplain_kidpr", p, if (isTRUE(p)) rows,
                     list(landform = if (nzchar(lf)) lf else NA_character_, floodplain = fp,
                          alluvial_material = alluvial, alluvial_mark = mark, fluvic_material = fl),
                     c(if (is.na(fp)) "landform", if (is.na(alluvial)) "layer_origin"), "(B.1; Q21)")
  })
}

#' @rdname kidpr2008_divisions
#' @export
volcanic_ash_kidpr <- function(pedon) {
  v <- .kidpr_dv_of(pedon)
  .kidpr_dv_memo(v, "ash", function() {
    rows <- .kidpr_dv_rows(v)
    mark <- rows[v$f$mark[rows] %in% "\""]
    org <- vapply(rows, function(i) paste(.kidpr_dv_text(.kidpr_dv_attr(v, i, "layer_origin")),
                                          .kidpr_dv_text(.kidpr_dv_attr(v, i, "rock_origin"))), "")
    txt <- rows[.kidpr_dv_match(.KIDPR_DV_ASH, org)]
    vg <- suppressWarnings(as.numeric(vapply(rows, function(i) as.character(.kidpr_dv_attr(v, i, "volcanic_glass_pct")), "")))
    glass <- rows[!is.na(vg) & vg >= 30]
    pm <- .kidpr_dv_text(v$site$parent_material)
    p <- length(c(mark, txt, glass)) > 0L || .kidpr_dv_match(.KIDPR_DV_ASH, pm)
    .kidpr_dv_result("volcanic_ash_kidpr", p, c(mark, txt, glass),
                     list(ash_mark = length(mark) > 0L, parent_material = pm), where = "(B.2)")
  })
}

#' @rdname kidpr2008_divisions
#' @export
contaminated_x_kidpr <- function(pedon, within_cm = 30) {
  v <- .kidpr_dv_of(pedon)
  .kidpr_dv_memo(v, .kidpr_dv_key("x", within_cm), function() .kidpr_dv_each(v, function() {
    ents <- v$hz
    r <- vapply(ents, .kidpr_dv_isk, NA, K = "X") &
      vapply(ents, function(e) .kidpr_dv_lt(e$rtop, within_cm), NA)
    rows <- .kidpr_dv_rows(v)
    w <- .kidpr_dv_lt(v$rtop[rows], within_cm)
    hz <- vapply(rows, function(i) .kidpr_dv_text(.kidpr_dv_attr(v, i, "contamination_hazard")), "")
    ct <- vapply(rows, function(i) .kidpr_dv_text(.kidpr_dv_attr(v, i, "contamination_type")), "")
    ext <- !is.na(hz) & grepl("^extrem", hz) & w
    site_c <- isTRUE(as.logical(v$site$contaminated))
    p <- if (any(r %in% TRUE) || any(ext %in% TRUE)) TRUE
         else if (anyNA(r) || anyNA(ext)) NA
         else if (any(!is.na(hz) & w %in% TRUE)) FALSE
         else if (site_c || any(!is.na(ct) & !(w %in% FALSE))) NA else FALSE
    .kidpr_dv_result("contaminated_x_kidpr", p,
                     c(unlist(lapply(ents[r %in% TRUE], `[[`, "rows")), rows[ext %in% TRUE]),
                     list(within_cm = within_cm, hazard = unique(stats::na.omit(hz)),
                          contaminated = site_c),
                     c("contamination_hazard", if (anyNA(w)) "top_cm"), "(C.3; Q23)")
  }))
}
