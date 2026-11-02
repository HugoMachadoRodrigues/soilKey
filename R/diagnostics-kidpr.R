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
#   * the rows in depth order, from the ground (fresh litter left out);
#   * the deposit at the surface (key 2.2, step 2; Hugo's decision Q21): the
#     stratified humus horizons RJ, RY and RU, the small deposited layers (r,
#     rh, ur, rr, rx), and loose rock (C, D) lying over a buried soil or over
#     an organic or humus horizon; humified when it holds RJ, RY or RU;
#   * the profile: the rows under a deposit of 40 cm or less (a buried soil
#     directly under it is the soil keyed), else every row that is not
#     buried;
#   * the horizons of the profile in order, sub-horizons merged, small
#     horizons and transitions left out (p. 36), and the surface horizons
#     read by the thickest-horizon rule (p. 37; Hugo's decision Q11): of the
#     organic, humus and agro horizons that start the profile, the agro
#     horizon is kept, and of the natural ones the thickest (the others are
#     features); a W of 5 cm or less over another horizon is the feature w.
#
# Three-valued: TRUE, FALSE, or NA when an undecided layer (a candidate list
# of the data route) or a missing record could change the answer. Names
# carry the _kidpr suffix. The flattened key that reads them is
# inst/rules/kidpr2008/key.yaml and divisions/; the readings are in
# readings.yaml.
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

# Text of the site and the layers.
.KIDPR_DV_FLOODPLAIN <- paste0("flood ?-?plain|bottomland|levee|back ?swamp|channel bar|point bar|oxbow|",
                              "river ?bank|\u043f\u043e\u0439\u043c")
.KIDPR_DV_VALLEY <- "valley|alluvial plain|delta|fluvial|\u0434\u043e\u043b\u0438\u043d"
.KIDPR_DV_ALLUVIUM <- paste0("alluv|fluv|river|lacustr|lake|\u0430\u043b\u043b\u044e\u0432|",
                            "\u043e\u0437\u0435\u0440")
.KIDPR_DV_NOT_ALLUVIUM <- "glacio|fluvioglac|marine|\u043c\u043e\u0440\u0441\u043a"
.KIDPR_DV_ASH <- "tephr|pyroclast|volcan|ash|\u043f\u0435\u043f\u0435\u043b"

# The view of the profile in force during one classification.
.kidpr_dv_state <- new.env(parent = emptyenv())


# --- three-valued helpers ----------------------------------------------------------

.kidpr_dv_or <- function(v) if (any(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA
.kidpr_dv_and <- function(v) if (any(v %in% FALSE)) FALSE else if (all(v %in% TRUE)) TRUE else NA

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


# --- the view ------------------------------------------------------------------------

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
  code <- f$code
  kind <- ifelse(f$small, "small", ifelse(f$transitional, "trans",
          ifelse(!is.na(code) & code %in% .KIDPR_DV_ROCKS, "rock",
          ifelse(!is.na(code) & code == "TPO", "nonsoil",
          ifelse(!is.na(code), "horizon",
          ifelse(is.na(f$passed), "undecided", "none"))))))
  v$f <- f; v$n <- n; v$kind <- kind; v$thick <- thick
  v$buried <- as.integer(f$buried)
  # the ground: the top of the first row, past fresh litter (a row with no
  # horizon whose evidence says litter)
  start <- 1L
  while (start <= n && kind[start] == "none" && isTRUE(grepl("litter", f$evidence[start]))) start <- start + 1L
  v$start <- start
  v$ground <- if (start <= n) f$top_cm[start] else NA_real_
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
# ground made of stratified humus horizons (RJ, RY, RU), small deposited
# layers (r, rh, ur, rr, rx) and loose rock (C, D, not buried); the loose
# rock counts only when a buried soil or an organic or humus horizon lies
# under the run (else it is the parent material under the deposit).
# Humified when it holds RJ, RY or RU.
.kidpr_dv_deposit <- function(v) {
  f <- v$f; kind <- v$kind
  none <- list(rows = integer(0), thick = 0, humified = FALSE, note = character(0))
  i <- v$start
  run <- integer(0)
  while (i <= v$n) {
    k <- kind[i]; cd <- f$code[i]
    is_dep <- (k == "horizon" && cd %in% .KIDPR_DV_RSTAR) ||
      (k == "small" && any(f$features[[i]] %in% .KIDPR_DV_DEPOSIT_FEATURES)) ||
      (k == "rock" && cd %in% c("C", "D") && v$buried[i] == 0L)
    if (!is_dep || v$buried[i] > 0L) break
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
  list(rows = run, thick = th, humified = rstar,
       note = sprintf("a deposit of %s cm at the surface (%s)%s", format(th),
                      paste(stats::na.omit(f$designation[run]), collapse = ", "),
                      if (rstar) ", humified (RJ, RY or RU)" else ""))
}

# One horizon entry of the profile: its code, kind, overlays, candidates,
# thickness, top and rows.
.kidpr_dv_entry <- function(v, rows) {
  f <- v$f
  i <- rows[1L]
  list(code = f$code[i], kind = v$kind[i], rows = rows,
       overlay = unique(unlist(f$overlay[rows])), overlay_na = unique(unlist(f$overlay_na[rows])),
       cands = if (v$kind[i] == "undecided") as.character(f$candidates[[i]]) else character(0),
       missing = unique(unlist(f$missing[rows])),
       thick = sum(v$thick[rows]), top = f$top_cm[i], bottom = f$bottom_cm[rows[length(rows)]])
}

# The horizons of the profile in order (sub-horizons merged; small horizons
# and transitions out) and the surface by the thickest-horizon rule (Q11).
.kidpr_dv_horizons <- function(v) {
  s <- v$prof[!v$kind[v$prof] %in% c("small", "trans")]
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
  drop <- integer(0)
  features <- character(0)
  if (run > 0L) {
    codes <- vapply(ent[seq_len(run)], `[[`, "", "code")
    th <- vapply(ent[seq_len(run)], `[[`, 0, "thick")
    th[is.na(th)] <- 0
    nat <- which(codes %in% .KIDPR_DV_NATURAL)
    if (length(nat) > 1L) {
      keep <- nat[which.max(th[nat])]
      drop <- setdiff(nat, keep)
    }
    # a W of 5 cm or less over another horizon is the feature w (p. 39)
    w <- setdiff(which(codes == "W"), drop)
    if (length(w) == 1L && length(nat) == 1L && !any(codes %in% .KIDPR_DV_AGRO) && th[w] <= 5 &&
        !is.null(nxt) && nxt$kind %in% c("horizon", "undecided"))
      drop <- c(drop, w)
    features <- codes[drop]
  }
  v$run_features <- features
  v$hz_raw <- ent
  keep <- setdiff(seq_along(ent), drop)
  v$run_n <- run - length(drop)
  v$hz <- ent[keep]
  # the surface horizon: the agro horizon, else the natural one kept
  sc <- if (v$run_n > 0L) vapply(v$hz[seq_len(v$run_n)], `[[`, "", "code") else character(0)
  ag <- which(sc %in% .KIDPR_DV_AGRO)
  v$surface <- if (length(ag)) ag[1L] else if (length(sc)) 1L else NA_integer_
  invisible(v)
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

# Rows of the profile that start within `cm` of the ground.
.kidpr_dv_within <- function(v, rows, cm) {
  if (is.null(cm)) return(rows)
  top <- v$f$top_cm[rows] - v$ground
  rows[is.na(top) | top < cm]
}


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
#' layer or a missing record could change the answer.
#'
#' The profile is read top-down from the ground. A deposit at the surface
#' (\code{surface_deposit_kidpr}) is the run of stratified humus horizons
#' (RJ, RY, RU), small deposited layers (r, rh, ur, rr, rx) and loose rock
#' over a buried soil or an organic or humus horizon; under a deposit of
#' 40 cm or less the soil below is the one keyed. Sub-horizons are merged,
#' small horizons and transitions left out; of the organic, humus and agro
#' horizons that start the profile the agro horizon is kept and, of the
#' natural ones, the thickest (the others are features; a W of 5 cm or
#' less over another horizon is the feature w).
#'
#' \describe{
#'   \item{\code{has_horizon_kidpr}}{One of the horizons is in the profile
#'     (an overlay counts).}
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
#'     parent material left out) are exactly one of the \code{sets}; with
#'     \code{raw = TRUE} read before the thickest-horizon rule (key 2.2, step
#'     5).}
#'   \item{\code{rock_within_kidpr}}{Hard rock (R) starts within
#'     \code{max_cm} of the soil surface, organic horizons included (key C.1).}
#'   \item{\code{turbated_kidpr}}{A turbated horizon TUR more than
#'     \code{min_cm} thick (Hugo's decision Q20).}
#'   \item{\code{peat_body_kidpr}}{A peat body of \code{min_cm} or more: the
#'     organic layers contiguous from the surface (or under a deposit of 40 cm
#'     or less), muck and the agro-peat horizons included (key 2.2, step 6;
#'     Hugo's decision Q22). A description that ends in peat short of it leaves
#'     it undecided.}
#'   \item{\code{surface_deposit_kidpr}}{A deposit at the surface (key 2.2,
#'     steps 2-4; Hugo's decision Q21) more than \code{min_cm} thick, with
#'     (\code{humified = TRUE}) or without (\code{FALSE}) a stratified humus
#'     horizon RJ, RY or RU.}
#'   \item{\code{stratified_body_kidpr}}{A stratified humus body from the
#'     surface, the agro horizons P and PU over RJ, RY or RU counted with them,
#'     of \code{min_cm} or more (the stratozems, B.2; Q21).}
#'   \item{\code{soil_horizon_within_kidpr}}{A soil horizon (of the profile
#'     or of a buried soil) starts within \code{max_cm} of the ground (key 2.2,
#'     step 1).}
#'   \item{\code{floodplain_kidpr}}{The soil lies within a river floodplain
#'     on modern alluvium (B.1; Hugo's decision Q21): a floodplain recorded
#'     (landform or landform element) and alluvial material (the alluvial mark
#'     of the notation, or a fluvial or lacustrine origin of a layer or of the
#'     parent material; marine and glaciofluvial do not count); with no
#'     landform recorded, alluvial material that soilKey's fluvic material
#'     confirms; alluvial material alone leaves it undecided. Nothing recorded:
#'     not a floodplain soil.}
#'   \item{\code{volcanic_ash_kidpr}}{The soil is developed in volcanic ash
#'     (B.2): the ash mark of the notation, an ash, tephra or pyroclastic
#'     origin of a layer or of the parent material, or 30 % volcanic glass. Not
#'     recorded: no ash.}
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
#'   (NULL: any depth).
#' @param buried Also read the horizons of buried soils.
#' @param gap_cm Thickness of layers with no diagnostic horizon allowed
#'   between \code{upper} and \code{lower}.
#' @param sets A list of code vectors: the profile's horizons must be
#'   exactly one of them.
#' @param raw For \code{only_horizons_kidpr}: read every horizon of the
#'   profile, before the thickest-horizon rule of the surface.
#' @param max_cm,min_cm Depth or thickness limits (see each predicate).
#' @param from_ground_zero For \code{starts_at_surface_kidpr}: a TRUE
#'   answer stands only when the description starts at 0 cm, else it is
#'   undecided (a truncated soil, Hugo's decision Q19).
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
  .kidpr_dv_memo(v, .kidpr_dv_key("has", codes, within_cm %||% "", buried), function() {
    ents <- v$hz
    if (isTRUE(buried)) {
      rows <- seq_len(v$n)
      rows <- rows[rows >= v$start & v$bur[rows] > 0L & !v$kind[rows] %in% c("small", "trans")]
      ents <- c(ents, lapply(rows, function(i) .kidpr_dv_entry(v, i)))
    }
    if (!is.null(within_cm))
      ents <- Filter(function(e) is.na(e$top) || e$top - v$ground < within_cm, ents)
    r <- vapply(ents, .kidpr_dv_isk, NA, K = codes)
    p <- if (length(r)) .kidpr_dv_or(r) else FALSE
    .kidpr_dv_result("has_horizon_kidpr", p,
                     unlist(lapply(ents[r %in% TRUE], `[[`, "rows")),
                     list(codes = codes, within_cm = within_cm),
                     unlist(lapply(ents[is.na(r)], `[[`, "missing")), "(horizon present)")
  })
}

#' @rdname kidpr2008_divisions
#' @export
starts_at_surface_kidpr <- function(pedon, codes, from_ground_zero = FALSE) {
  v <- .kidpr_dv_of(pedon)
  codes <- as.character(unlist(codes))
  .kidpr_dv_memo(v, .kidpr_dv_key("starts", codes, from_ground_zero), function() {
    if (!length(v$hz)) return(.kidpr_dv_result("starts_at_surface_kidpr", FALSE, where = "(at the surface)"))
    e <- v$hz[[1L]]
    p <- .kidpr_dv_isk(e, codes)
    # a description that does not start at 0 cm may have left out the top
    # of the soil (Hugo's decision Q19)
    guard <- isTRUE(from_ground_zero) && isTRUE(p) && !isTRUE(v$ground == 0)
    if (guard) p <- NA
    .kidpr_dv_result("starts_at_surface_kidpr", p, if (isTRUE(p)) e$rows,
                     list(codes = codes, first = e$code, under_deposit = length(v$dep$rows) > 0L,
                          ground_cm = v$ground),
                     c(e$missing, if (guard) "top_cm"), "(at the surface or under a deposit of 40 cm or less)")
  })
}

#' @rdname kidpr2008_divisions
#' @export
surface_horizon_kidpr <- function(pedon, codes) {
  v <- .kidpr_dv_of(pedon)
  codes <- as.character(unlist(codes))
  .kidpr_dv_memo(v, .kidpr_dv_key("surface", codes), function() {
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
  })
}

#' @rdname kidpr2008_divisions
#' @export
directly_over_kidpr <- function(pedon, upper, lower, gap_cm = 0) {
  v <- .kidpr_dv_of(pedon)
  upper <- as.character(unlist(upper)); lower <- as.character(unlist(lower))
  .kidpr_dv_memo(v, .kidpr_dv_key("over", upper, lower, gap_cm), function() {
    hz <- v$hz
    n <- length(hz)
    r <- logical(0); lay <- integer(0); miss <- character(0)
    for (i in seq_len(n)) {
      a <- .kidpr_dv_isk(hz[[i]], upper)
      if (isFALSE(a)) next
      j <- i + 1L; g <- 0
      while (j <= n && hz[[j]]$kind == "none" && !is.na(hz[[j]]$thick) && g + hz[[j]]$thick <= gap_cm) {
        g <- g + hz[[j]]$thick
        j <- j + 1L
      }
      b <- if (j <= n) .kidpr_dv_isk(hz[[j]], lower) else FALSE
      ab <- .kidpr_dv_and(c(a, b))
      r <- c(r, ab)
      if (isTRUE(ab)) lay <- c(lay, hz[[i]]$rows, hz[[j]]$rows)
      if (is.na(ab)) miss <- c(miss, hz[[i]]$missing, if (j <= n) hz[[j]]$missing)
    }
    p <- if (length(r)) .kidpr_dv_or(r) else FALSE
    .kidpr_dv_result("directly_over_kidpr", p, lay, list(upper = upper, lower = lower, gap_cm = gap_cm),
                     miss, "(directly over)")
  })
}

#' @rdname kidpr2008_divisions
#' @export
below_kidpr <- function(pedon, upper, lower) {
  v <- .kidpr_dv_of(pedon)
  upper <- as.character(unlist(upper)); lower <- as.character(unlist(lower))
  .kidpr_dv_memo(v, .kidpr_dv_key("below", upper, lower), function() {
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
  })
}

#' @rdname kidpr2008_divisions
#' @export
only_horizons_kidpr <- function(pedon, sets, raw = FALSE) {
  v <- .kidpr_dv_of(pedon)
  sets <- lapply(sets, function(s) sort(unique(as.character(unlist(s)))))
  .kidpr_dv_memo(v, .kidpr_dv_key("only", sets, raw), function() {
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
  })
}

#' @rdname kidpr2008_divisions
#' @export
rock_within_kidpr <- function(pedon, max_cm = 30) {
  v <- .kidpr_dv_of(pedon)
  .kidpr_dv_memo(v, .kidpr_dv_key("rock", max_cm), function() {
    rows <- if (v$start <= v$n) seq.int(v$start, v$n) else integer(0)
    rows <- rows[v$buried[rows] == 0L]
    top <- v$f$top_cm[rows] - v$ground
    r <- rows[!is.na(v$f$code[rows]) & v$f$code[rows] == "R"]
    hit <- r[!is.na(v$f$top_cm[r]) & v$f$top_cm[r] - v$ground <= max_cm]
    und <- rows[v$kind[rows] == "undecided" & (is.na(top) | top <= max_cm) &
                  vapply(rows, function(i) { cd <- v$f$candidates[[i]]; !length(cd) || "R" %in% cd }, NA)]
    p <- if (length(hit)) TRUE else if (length(und)) NA else FALSE
    depth <- if (length(r)) min(v$f$top_cm[r] - v$ground, na.rm = TRUE) else NA_real_
    .kidpr_dv_result("rock_within_kidpr", p, hit, list(rock_top_cm = depth, max_cm = max_cm),
                     unlist(v$f$missing[und]), "(C.1, hard rock)")
  })
}

#' @rdname kidpr2008_divisions
#' @export
turbated_kidpr <- function(pedon, min_cm = 40) {
  v <- .kidpr_dv_of(pedon)
  .kidpr_dv_memo(v, .kidpr_dv_key("tur", min_cm), function() {
    tur <- Filter(function(e) e$kind == "horizon" && identical(e$code, "TUR"), v$hz)
    und <- Filter(function(e) e$kind == "undecided" && (!length(e$cands) || "TUR" %in% e$cands), v$hz)
    th <- vapply(tur, `[[`, 0, "thick")
    p <- if (any(th > min_cm, na.rm = TRUE)) TRUE
         else if (anyNA(th) || length(und)) NA else FALSE
    .kidpr_dv_result("turbated_kidpr", p, unlist(lapply(tur[th > min_cm & !is.na(th)], `[[`, "rows")),
                     list(tur_thickness_cm = th, min_cm = min_cm),
                     c(if (anyNA(th)) c("top_cm", "bottom_cm"), unlist(lapply(und, `[[`, "missing"))),
                     "(TUR, C.15; Q20)")
  })
}

#' @rdname kidpr2008_divisions
#' @export
peat_body_kidpr <- function(pedon, min_cm = 51) {
  v <- .kidpr_dv_of(pedon)
  .kidpr_dv_memo(v, .kidpr_dv_key("peat", min_cm), function() {
    rows <- v$prof
    tot <- 0; used <- integer(0); open <- FALSE; miss <- character(0); ended <- TRUE
    for (i in rows) {
      k <- v$kind[i]
      if (k == "small") next
      cd <- v$f$code[i]
      if (k %in% c("horizon", "rock") && cd %in% .KIDPR_DV_PEAT) {
        tot <- tot + if (is.na(v$thick[i])) NA else v$thick[i]
        used <- c(used, i)
        next
      }
      if (k == "undecided") {
        cds <- v$f$candidates[[i]]
        if (!length(cds) || any(cds %in% .KIDPR_DV_PEAT)) { open <- TRUE; miss <- c(miss, v$f$missing[[i]]) }
      }
      ended <- FALSE
      break
    }
    p <- if (is.na(tot)) NA else if (tot >= min_cm) TRUE
         else if (open) NA else if (length(used) && ended) NA else FALSE
    if (!length(used)) p <- if (open) NA else FALSE
    .kidpr_dv_result("peat_body_kidpr", p, if (isTRUE(p)) used, list(peat_cm = tot, min_cm = min_cm,
                                                                     ends_in_peat = length(used) > 0L && ended),
                     c(miss, if (is.na(p) && ended) "bottom_cm"), "(2.2, step 6; Q22)")
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
    rows <- if (v$start <= v$n) seq.int(v$start, v$n) else integer(0)
    rows <- rows[v$buried[rows] == 0L]
    tot <- 0; used <- integer(0); open <- FALSE
    for (i in rows) {
      k <- v$kind[i]
      if (k == "small") next
      cd <- v$f$code[i]
      if (k == "horizon" && cd %in% c(.KIDPR_DV_RSTAR, "P", "PU")) {
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
    rows <- if (v$start <= v$n) seq.int(v$start, v$n) else integer(0)
    rows <- .kidpr_dv_within(v, rows, max_cm)
    f <- v$f
    hor <- rows[(v$kind[rows] %in% c("horizon", "trans")) |
                  vapply(rows, function(i) length(f$overlay[[i]]) > 0L, NA)]
    und <- rows[v$kind[rows] == "undecided" & vapply(rows, function(i) {
      cd <- f$candidates[[i]]
      !length(cd) || any(nzchar(cd) & !cd %in% c(.KIDPR_DV_ROCKS, "TPO"))
    }, NA)]
    p <- if (length(hor)) TRUE else if (length(und)) NA else FALSE
    .kidpr_dv_result("soil_horizon_within_kidpr", p, hor, list(max_cm = max_cm),
                     unlist(f$missing[und]), "(2.2, step 1)")
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
          else if (grepl(.KIDPR_DV_FLOODPLAIN, lf, perl = TRUE)) TRUE
          else if (grepl(.KIDPR_DV_VALLEY, lf, perl = TRUE)) NA else FALSE
    rows <- v$prof
    mark <- any(v$f$mark[rows] %in% "~~", na.rm = TRUE)
    org <- unlist(lapply(rows, function(i) c(.kidpr_dv_text(.kidpr_dv_attr(v, i, "layer_origin")),
                                            .kidpr_dv_text(.kidpr_dv_attr(v, i, "rock_origin")))))
    org <- c(stats::na.omit(org), stats::na.omit(.kidpr_dv_text(v$site$parent_material)))
    allu_txt <- grepl(.KIDPR_DV_ALLUVIUM, org, perl = TRUE) & !grepl(.KIDPR_DV_NOT_ALLUVIUM, org, perl = TRUE)
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
    rows <- if (v$start <= v$n) seq.int(v$start, v$n) else integer(0)
    mark <- rows[v$f$mark[rows] %in% "\""]
    org <- vapply(rows, function(i) paste(.kidpr_dv_text(.kidpr_dv_attr(v, i, "layer_origin")),
                                          .kidpr_dv_text(.kidpr_dv_attr(v, i, "rock_origin"))), "")
    txt <- rows[grepl(.KIDPR_DV_ASH, org, perl = TRUE)]
    vg <- suppressWarnings(as.numeric(vapply(rows, function(i) as.character(.kidpr_dv_attr(v, i, "volcanic_glass_pct")), "")))
    glass <- rows[!is.na(vg) & vg >= 30]
    pm <- .kidpr_dv_text(v$site$parent_material)
    p <- length(c(mark, txt, glass)) > 0L || isTRUE(grepl(.KIDPR_DV_ASH, pm, perl = TRUE))
    .kidpr_dv_result("volcanic_ash_kidpr", p, c(mark, txt, glass),
                     list(ash_mark = length(mark) > 0L, parent_material = pm), where = "(B.2)")
  })
}

#' @rdname kidpr2008_divisions
#' @export
contaminated_x_kidpr <- function(pedon, within_cm = 30) {
  v <- .kidpr_dv_of(pedon)
  .kidpr_dv_memo(v, .kidpr_dv_key("x", within_cm), function() {
    ents <- Filter(function(e) is.na(e$top) || e$top - v$ground < within_cm, v$hz)
    r <- vapply(ents, .kidpr_dv_isk, NA, K = "X")
    rows <- .kidpr_dv_within(v, if (v$start <= v$n) seq.int(v$start, v$n) else integer(0), within_cm)
    hz <- vapply(rows, function(i) .kidpr_dv_text(.kidpr_dv_attr(v, i, "contamination_hazard")), "")
    ct <- vapply(rows, function(i) .kidpr_dv_text(.kidpr_dv_attr(v, i, "contamination_type")), "")
    ext <- rows[!is.na(hz) & grepl("^extrem", hz)]
    site_c <- isTRUE(as.logical(v$site$contaminated))
    p <- if (any(r %in% TRUE) || length(ext)) TRUE
         else if (any(is.na(r))) NA
         else if (any(!is.na(hz))) FALSE
         else if (site_c || any(!is.na(ct))) NA else FALSE
    .kidpr_dv_result("contaminated_x_kidpr", p, c(unlist(lapply(ents[r %in% TRUE], `[[`, "rows")), ext),
                     list(within_cm = within_cm, hazard = unique(stats::na.omit(hz)),
                          contaminated = site_c),
                     "contamination_hazard", "(C.3; Q23)")
  })
}
