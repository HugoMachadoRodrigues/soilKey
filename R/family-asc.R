# =============================================================================
# v0.9.292 -- the families of the Australian Soil Classification (3rd ed.,
# Isbell & NCST 2021, online version with errata). The family is not a level
# of the key: each criterion of the order (the thickness of the A1, or of the
# whole A, gravel of the surface and A1, the A1 and B textures, soil depth,
# water repellence; the Vertosols' laboratory clay, the Organosols' organic
# materials, the Calcarosols' depth to the Bk) places the soil in one class.
# A class is given only when the data decide it: code YY when they cannot,
# ZZ when no class of the order's list fits. The classes and the criteria of
# each order are in inst/rules/asc3/families.yaml; the readings in
# readings.yaml (topic "Families").
# =============================================================================

.ASC_FAM_PAGES <- "order pages, 'Family Criteria'; 'Using the classification'; glossary"

# The family rules (families.yaml), read once per session.
.asc_family_rules <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      f <- system.file("rules", "asc3", "families.yaml", package = "soilKey")
      if (!nzchar(f)) rlang::abort("inst/rules/asc3/families.yaml not found")
      cache <<- yaml::read_yaml(f)
    }
    cache
  }
})

# The criteria for a soil of `order` (and, for the Hydrosols, `suborder`):
# a list, or NULL when the order has no family (Anthroposols; the Subtidal,
# Subaqueous and Intertidal Hydrosols; a Hydrosol of unknown suborder).
.asc_family_criteria <- function(order, suborder = NULL) {
  fr <- .asc_family_rules()
  order <- as.character(order %||% NA)[1L]
  if (is.na(order)) return(NULL)
  if (order == "HY") {
    sub <- as.character(suborder %||% NA)[1L]
    if (is.na(sub)) return(NULL)
    if (sub %in% fr$hydrosol_suborders$full) return(fr$orders$HY)
    if (sub %in% fr$hydrosol_suborders$partial) return(fr$orders[["HY-partial"]])
    return(NULL)
  }
  fr$orders[[order]]
}

# ---- classes ----------------------------------------------------------------

# The class of a value: the first whose limits hold it ("ZZ" when none does).
.asc_fam_bin <- function(v, cls) {
  if (is.na(v)) return(NA_character_)
  for (k in cls) {
    lo_ok <- is.null(k$lower) || v >= k$lower
    up_ok <- is.null(k$upper) || (if (isTRUE(k$closed)) v <= k$upper else v < k$upper)
    if (lo_ok && up_ok) return(k$code)
  }
  "ZZ"
}

# The class of a value known to lie in [lo, hi]: decided when both ends fall
# in the same class.
.asc_fam_bin_range <- function(lo, hi, cls) {
  a <- .asc_fam_bin(lo, cls); b <- .asc_fam_bin(hi, cls)
  if (is.na(a) || is.na(b) || a != b) NA_character_ else a
}

# One criterion as a FamilyAttribute: `value` the class name (NULL when the
# class is undecided or none fits), the code and what was measured in
# `evidence`.
.asc_fam_attr <- function(cr, code, measure = list(), missing = character(0), note = NA_character_) {
  code <- if (is.na(code)) "YY" else code
  kind <- .asc_family_rules()$classes[[cr$kind]]
  if (!is.null(cr$classes) && !code %in% c("YY", "ZZ") && !code %in% cr$classes) code <- "ZZ"
  hit <- Filter(function(k) identical(k$code, code), kind)
  name <- if (length(hit)) hit[[1L]]$name else NULL
  if (!is.null(name) && !is.null(cr$names[[code]])) name <- cr$names[[code]]
  FamilyAttribute$new(
    name = cr$id, value = name,
    evidence = c(list(code = code, kind = cr$kind, pair = cr$pair), measure,
                 if (!is.na(note)) list(note = note)),
    missing = if (code == "YY") unique(missing) else character(0),
    reference = sprintf("%s %s", .ASC_REF, .ASC_FAM_PAGES))
}

# ---- horizons -------------------------------------------------------------------

# Field texture classes of the family level (field textures; the clay
# percentages of the book are a guide only): sandy S, LS, CS; loamy SL, L;
# clay loamy SCL, CL (and CLS); silty ZL, ZCL; clayey LC to HC; peaty, an
# organic material. Without a field texture, lab clay stands in at the
# limits of the classes (10, 20 and 35 % clay), undecided within 2 points;
# silty then needs 25-35 % clay and 25 % silt or more.
.ASC_FAM_TEX <- c(S = "K", LS = "K", CS = "K", SL = "L", L = "L", ZL = "N", SCL = "M", CLS = "M",
                  CL = "M", ZCL = "N", LC = "O", LMC = "O", MC = "O", MHC = "O", HC = "O", C = "O",
                  peat = "J")
.ASC_FAM_RANK <- c(J = 0, K = 1, L = 2, M = 3, N = 3, O = 4)

.asc_fam_texture_class <- function(x) {
  h <- x$h; n <- x$n
  g <- x$clay$texture$grade
  cls <- ifelse(is.na(g), NA_character_, unname(.ASC_FAM_TEX[g]))
  clay <- h$clay_pct %||% rep(NA_real_, n); silt <- h$silt_pct %||% rep(NA_real_, n)
  mn <- ifelse(!is.na(silt) & !is.na(clay) & clay >= 27 & silt >= 27, "N",
               ifelse(!is.na(clay) & clay < 23, "M", ifelse(!is.na(silt) & silt < 23, "M", NA_character_)))
  lab <- ifelse(is.na(clay), NA_character_,
         ifelse(clay <= 8, "K", ifelse(clay > 12 & clay <= 18, "L",
         ifelse(clay > 22 & clay <= 33, mn, ifelse(clay > 37, "O", NA_character_)))))
  out <- ifelse(!is.na(cls), cls, lab)
  # organic material with no texture recorded: peaty
  out[is.na(out) & x$organic %in% TRUE] <- "J"
  out
}

# The class covering the major part of `rows` by thickness, else the one with
# the largest share, whatever the thickness of unknown class (rows of unknown
# class, rows that may belong, rows of unknown thickness) turns out to be; NA
# for a tie or when the unknown part could change it.
.asc_fam_dominant <- function(x, rows, cls, maybe = integer(0)) {
  h <- x$h
  th <- h$bottom_cm - h$top_cm
  share <- numeric(0); unk <- 0
  for (i in rows) {
    if (is.na(th[i])) { unk <- Inf; next }
    if (is.na(cls[i])) unk <- unk + th[i]
    else share[cls[i]] <- (if (is.na(share[cls[i]])) 0 else share[[cls[i]]]) + th[i]
  }
  for (i in maybe) unk <- unk + (if (is.na(th[i])) Inf else th[i])
  if (!length(share)) return(NA_character_)
  s <- sort(share, decreasing = TRUE)
  second <- if (length(s) > 1L) s[[2L]] else 0
  if (s[[1L]] > second + unk) names(s)[1L] else NA_character_
}

# The most clayey texture class of `rows` (sandy < loamy < clay loamy and
# silty < clayey; between clay loamy and silty, the more clayey texture);
# NA when a row of unknown texture, or one that may belong, could change it.
.asc_fam_most_clayey <- function(x, rows, cls, maybe = integer(0)) {
  r <- unname(.ASC_FAM_RANK[cls])
  known <- rows[!is.na(cls[rows])]
  if (!length(known)) return(NA_character_)
  best <- max(r[known])
  if (best < 4) {
    if (any(is.na(cls[rows]))) return(NA_character_)
    if (any(is.na(cls[maybe]) | r[maybe] > best)) return(NA_character_)
  }
  top <- known[r[known] == best]
  rivals <- maybe[!is.na(cls[maybe]) & r[maybe] == best]
  codes <- unique(cls[top])
  if (length(codes) == 1L && !any(cls[rivals] != codes)) return(codes)
  # clay loamy against silty: the more clayey texture (lab clay, else the
  # middle of the range of its field texture grade) decides; a tie, or a
  # row that may belong, leaves it undecided
  if (length(rivals)) return(NA_character_)
  mid <- (x$clay$lo + x$clay$hi) / 2
  m <- vapply(codes, function(k) max(mid[top[cls[top] == k]]), 0)
  if (anyNA(m) || sum(m == max(m)) > 1L) return(NA_character_)
  codes[which.max(m)]
}

# A run of rows from the top of the profile (an O1 at the surface left out)
# whose roles are in `set`: the run of .asc_run_from, with `maybe` the rows
# that may belong to it. Buried rows and rows below the last designated one
# (outside the description) do not; rows of unknown role may.
.ASC_FAM_B_ROLES <- c("B1", "B2", "B3", "BA", "BC", "B/A", "B/C")
.asc_fam_run <- function(x, set) {
  top0 <- .asc_profile_top(x)
  if (is.na(top0)) return(NULL)
  ok <- ifelse(x$des$buried, FALSE, ifelse(x$unknown_role, NA, x$role %in% set))
  ok[x$trailing] <- FALSE
  r <- .asc_run_from(x, ok, top0)
  r$top0 <- top0
  r$maybe <- setdiff(r$rows, r$sure_rows)
  r
}

# The surface soil: the A1 horizon (the rows of the A1 run, organic surface
# horizons apart), or, with no A1, the first row at the top of the profile;
# `organic` when organic horizons (O2, P) lie at the top.
.asc_fam_surface <- function(x) {
  run <- .asc_fam_run(x, c("O2", "P", "A1"))
  if (is.null(run)) return(NULL)
  org_row <- x$role %in% c("O2", "P") | (x$unknown_role & x$organic %in% TRUE)
  org <- run$sure_rows[org_row[run$sure_rows]]
  a1 <- setdiff(run$sure_rows, org)
  maybe <- setdiff(run$maybe, which(org_row))
  first <- {
    h <- x$h; o <- order(h$top_cm, na.last = NA)
    o <- o[h$top_cm[o] >= run$top0 - 0.5 & !x$des$buried[o] & !x$trailing[o] & !org_row[o]]
    if (length(o)) o[1L] else integer(0)
  }
  if (!length(a1) && !length(maybe)) a1 <- first
  list(rows = a1, maybe = maybe, organic = length(org) > 0L, top0 = run$top0)
}

# ---- criteria -------------------------------------------------------------------

# Thickness of the A1 (part a1) or of the whole A (part a), with organic
# horizons (O2, P) over it; or (part above_bk) the depth of the upper
# boundary of the first Bk horizon, NULL when there is none.
.asc_fam_thickness <- function(x, cr, great_group = NULL) {
  part <- cr$part
  if (!is.null(cr$part_a_great_groups)) {
    gg <- as.character(great_group %||% NA)[1L]
    part <- if (is.na(gg)) "a_or_a1" else if (gg %in% cr$part_a_great_groups) "a" else "a1"
  }
  cls <- .asc_family_rules()$classes[[cr$kind]]
  miss <- c("designation", "top_cm", "bottom_cm")
  one <- function(p) {
    set <- if (p == "a") c("O2", "P", "A1", "A2", "A3", "AB", "A/B") else c("O2", "P", "A1")
    run <- .asc_fam_run(x, set)
    # a horizon of unknown depth that is or may be part of it
    nat <- is.na(x$h$top_cm) & (x$role %in% set | x$unknown_role)
    if (is.null(run) || any(nat)) return(list(code = NA_character_, lo = NA, hi = NA))
    list(code = .asc_fam_bin_range(run$sure, run$possible, cls), lo = run$sure, hi = run$possible)
  }
  if (part == "above_bk") {
    # the Bk: the calcareous horizon of the glossary ("usually a Bk"), a B or
    # BC marked k or with 10 % or more carbonate segregations
    h <- x$h; top0 <- .asc_profile_top(x)
    ch <- .asc_calcareous_horizons(x)
    # a BA or B/A (a B for the family) marked k, or with 10 % or more
    # carbonate segregations, likewise
    seg <- h$secondary_carbonates_pct %||% rep(NA_real_, x$n)
    ba <- x$role %in% c("BA", "B/A") & !x$des$buried
    ch[ba] <- ifelse(x$des$k[ba] & x$des$m[ba], NA, x$des$k[ba] | (!is.na(seg[ba]) & seg[ba] >= 10))
    ok <- !is.na(h$top_cm) & !x$trailing
    bk <- which(ok & ch %in% TRUE)
    unk <- which(ok & is.na(ch))
    # a horizon of unknown depth that is or may be the Bk
    if (any(is.na(h$top_cm) & !(ch %in% FALSE)))
      return(.asc_fam_attr(cr, NA_character_, list(part = part), c(miss, "secondary_carbonates_pct")))
    if (!length(bk)) {
      if (!length(unk)) return(NULL)
      return(.asc_fam_attr(cr, NA_character_, list(part = part), c(miss, "secondary_carbonates_pct")))
    }
    z <- min(h$top_cm[bk]); lo_z <- min(c(z, h$top_cm[unk][h$top_cm[unk] < z]))
    code <- .asc_fam_bin_range(lo_z - top0, z - top0, cls)
    return(.asc_fam_attr(cr, code, list(part = part, cm = c(lo_z, z) - top0), miss))
  }
  if (part == "a_or_a1") {
    a <- one("a"); a1 <- one("a1")
    code <- if (!is.na(a$code) && identical(a$code, a1$code)) a$code else NA_character_
    return(.asc_fam_attr(cr, code, list(part = part, cm = c(min(a$lo, a1$lo), max(a$hi, a1$hi))),
                         c(miss, "great_group"),
                         "The great group, which decides between the A and the A1, is not known."))
  }
  r <- one(part)
  .asc_fam_attr(cr, r$code, list(part = part, cm = c(r$lo, r$hi)), miss)
}

# Gravel (2-60 mm) of a horizon, as a range: gravel_pct, else
# coarse_fragments_pct (a fragment of unrecorded size counts as gravel), else
# none (no record); NaN, a record of unknown abundance, may be anything. The
# thickness-weighted mean of `rows`, as c(lo, hi).
.asc_fam_gravel_rows <- function(x, rows) {
  if (!length(rows)) return(c(NA_real_, NA_real_))
  h <- x$h
  g <- h$gravel_pct %||% rep(NA_real_, x$n); cf <- h$coarse_fragments_pct %||% rep(NA_real_, x$n)
  v <- ifelse(!is.na(g), g, ifelse(is.nan(g), NaN, ifelse(!is.na(cf), cf, ifelse(is.nan(cf), NaN, 0))))[rows]
  lo <- ifelse(is.nan(v), 0, v); hi <- ifelse(is.nan(v), 100, v)
  th <- (h$bottom_cm - h$top_cm)[rows]
  ok <- !is.na(th) & th > 0
  if (!any(ok)) return(c(mean(lo), mean(hi)))
  c(sum(lo[ok] * th[ok]), sum(hi[ok] * th[ok])) / sum(th[ok])
}

# Gravel of the surface and A1 (the larger of the two estimates; one of them
# when only one is recorded), of the surface soil, or of the surface horizon.
# Rows that may belong to the A1 give every A1 the run allows (the sure rows
# and the first k of the others, in depth order).
.asc_fam_gravel <- function(x, cr, pedon) {
  cls <- .asc_family_rules()$classes$gravel
  where <- cr$where
  miss <- c("coarse_fragments_pct", "gravel_pct", "site$surface_gravel_pct")
  s <- .asc_fam_surface(x)
  if (is.null(s)) return(.asc_fam_attr(cr, NA_character_, list(where = where), c("top_cm", miss)))
  h <- x$h
  rows <- if (where == "surface_horizon") {
    o <- order(h$top_cm, na.last = NA); o <- o[h$top_cm[o] >= s$top0 - 0.5]
    if (length(o)) o[1L] else integer(0)
  } else s$rows
  maybe <- if (where == "surface_horizon") integer(0) else s$maybe[order(h$top_cm[s$maybe])]
  cand <- lapply(0:length(maybe), function(k) .asc_fam_gravel_rows(x, c(rows, maybe[seq_len(k)])))
  cand <- Filter(function(v) !anyNA(v), cand)
  site <- pedon$site
  num1 <- function(v) suppressWarnings(as.numeric(v %||% NA)[1L])
  surf <- if (where != "surface_a1") NA_real_
          else if (!is.na(num1(site$surface_gravel_pct)) || is.nan(num1(site$surface_gravel_pct))) num1(site$surface_gravel_pct)
          else num1(site$surface_coarse_fragments_pct)
  srange <- if (is.nan(surf)) c(0, 100) else if (is.na(surf)) NULL else c(surf, surf)
  if (!length(cand) && is.null(srange))
    return(.asc_fam_attr(cr, NA_character_, list(where = where), miss))
  # the larger of the two estimates, for each A1 the run allows
  both <- if (!length(cand)) list(srange) else lapply(cand, function(a) if (is.null(srange)) a else pmax(a, srange))
  lo <- min(vapply(both, `[`, 0, 1L)); hi <- max(vapply(both, `[`, 0, 2L))
  code <- .asc_fam_bin_range(lo, hi, cls)
  .asc_fam_attr(cr, code, list(where = where, surface_pct = surf, horizon_pct = if (length(cand)) cand[[1L]] else NULL,
                               pct = c(lo, hi)), miss)
}

# Textures: the A1 (or the surface soil), peaty under organic surface
# horizons; the most clayey of the B (b_max; with a C/B in the Tenosols,
# b_max_cb), or of the subsoil within the upper 1.0 m (subsoil_1m).
.asc_fam_texture <- function(x, cr) {
  where <- cr$where
  cls <- .asc_fam_texture_class(x)
  miss <- c("field_texture", "clay_pct", "designation")
  if (where %in% c("a1", "surface_soil")) {
    s <- .asc_fam_surface(x)
    if (is.null(s)) return(.asc_fam_attr(cr, NA_character_, list(where = where), miss))
    if (s$organic) return(.asc_fam_attr(cr, "J", list(where = where, rows = integer(0))))
    code <- .asc_fam_dominant(x, s$rows, cls, s$maybe)
    return(.asc_fam_attr(cr, code, list(where = where, rows = s$rows), miss))
  }
  h <- x$h
  if (where %in% c("b_max", "b_max_cb")) {
    roles <- c(.ASC_FAM_B_ROLES, if (where == "b_max_cb") c("C/B", "CB"))
    rows <- which(!x$des$buried & x$des$known & x$role %in% roles & !x$des$lamellae)
    maybe <- which(x$unknown_role & x$maybe_b2)
    # no B horizon: the criterion does not apply
    if (!length(rows) && !length(maybe)) return(NULL)
    code <- .asc_fam_most_clayey(x, rows, cls, maybe)
    return(.asc_fam_attr(cr, code, list(where = where, rows = rows), miss))
  }
  if (where == "subsoil_1m") {
    s <- .asc_fam_surface(x)
    if (is.null(s)) return(.asc_fam_attr(cr, NA_character_, list(where = where), miss))
    below <- suppressWarnings(max(h$bottom_cm[s$rows], na.rm = TRUE))
    if (!is.finite(below)) return(.asc_fam_attr(cr, NA_character_, list(where = where), miss))
    # rows that may belong to the surface soil may be subsoil too
    all <- which(!is.na(h$top_cm) & h$top_cm >= below - 0.5 & h$top_cm < s$top0 + 100 &
                 x$mineral & !x$hard & !x$trailing)
    maybe <- intersect(all, s$maybe); rows <- setdiff(all, s$maybe)
    # no subsoil described within the upper 1.0 m: undecided
    if (!length(rows) && !length(maybe)) return(.asc_fam_attr(cr, NA_character_, list(where = where), miss))
    code <- if (length(rows)) .asc_fam_most_clayey(x, rows, cls, maybe) else NA_character_
    return(.asc_fam_attr(cr, code, list(where = where, rows = rows), miss))
  }
  rlang::abort(sprintf("Unknown texture part '%s' in families.yaml", where))
}

# Vertosols: laboratory clay of the upper 0.1 m (below a surface crusty
# horizon, a surface horizon 30 mm thick or less that is massive or weakly
# structured) or the largest of the B.
.asc_fam_clay <- function(x, cr) {
  cls <- .asc_family_rules()$classes$clay
  h <- x$h; clay <- h$clay_pct
  miss <- c("clay_pct")
  if (cr$where == "b_max") {
    rows <- which(!x$des$buried & x$des$known & x$role %in% .ASC_FAM_B_ROLES)
    maybe <- which(x$unknown_role & x$maybe_b2)
    if (!length(rows) && !length(maybe)) return(NULL)
    known <- rows[!is.na(clay[rows])]
    best <- if (length(known)) max(clay[known]) else NA_real_
    open <- any(is.na(clay[rows])) || any(is.na(clay[maybe]) | clay[maybe] > (best %|NA|% -Inf))
    code <- if (is.na(best)) NA_character_ else if (open && .asc_fam_bin(best, cls) != "S") NA_character_
            else .asc_fam_bin(best, cls)
    return(.asc_fam_attr(cr, code, list(where = cr$where, clay_pct = best), miss))
  }
  top0 <- .asc_profile_top(x)
  if (is.na(top0)) return(.asc_fam_attr(cr, NA_character_, list(where = cr$where), miss))
  o <- order(h$top_cm, na.last = NA); o <- o[h$top_cm[o] >= top0 - 0.5]
  first <- if (length(o)) o[1L] else NA_integer_
  crust <- if (is.na(first) || !x$mineral[first] || is.na(x$thick[first]) || x$thick[first] > 3) FALSE
           else { p <- .asc_pedal(h)[first]; if (is.na(p)) NA else !p }
  mean_in <- function(zt) {
    zb <- zt + 10
    rows <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & h$top_cm < zb & h$bottom_cm > zt)
    w <- .asc_overlap(h$top_cm[rows], h$bottom_cm[rows], zt, zb)
    if (!length(rows) || sum(w) < 10 - 0.5 || anyNA(clay[rows][w > 0])) return(NA_real_)
    sum(clay[rows] * w) / sum(w)
  }
  starts <- if (isTRUE(crust)) h$bottom_cm[first] else if (isFALSE(crust)) top0 else c(top0, h$bottom_cm[first])
  v <- vapply(starts, mean_in, 0)
  codes <- vapply(v, .asc_fam_bin, "", cls = cls)
  code <- if (anyNA(codes) || length(unique(codes)) > 1L) NA_character_ else codes[1L]
  .asc_fam_attr(cr, code, list(where = cr$where, clay_pct = v, crusty = crust), miss)
}

# Soil depth: to rock, a cemented pan (not a thin ironpan), another cemented
# layer, or saprolite; otherwise, in a soil with a B horizon, to the top of
# the first C below it, and in a soil without one, the depth described (an
# unconsolidated C counts). A D horizon and a buried soil count as soil.
# Measured from the top of the profile (an O1 left out; the Organosols from
# the top of their organic horizons). Rows of unknown role may be a B or a
# C, and give a range.
.asc_fam_depth <- function(x, cr) {
  cls <- .asc_family_rules()$classes$depth
  h <- x$h; n <- x$n
  miss <- c("designation", "top_cm", "bottom_cm", "cementation_class")
  rows <- which(!is.na(h$top_cm) & !x$trailing)
  # a horizon of unknown depth may be the one that ends the soil
  if (!length(rows) || any(is.na(h$top_cm))) return(.asc_fam_attr(cr, NA_character_, list(), miss))
  s0 <- if (identical(cr$from, "organic")) min(h$top_cm[rows]) else .asc_profile_top(x)
  rows <- rows[order(h$top_cm[rows])]
  rows <- rows[h$top_cm[rows] >= s0 - 0.5]
  if (is.na(s0) || !length(rows)) return(.asc_fam_attr(cr, NA_character_, list(), miss))
  pt <- tolower(as.character(h$pan_type %||% rep(NA_character_, n)))
  thin <- grepl("thin ironpan|placic", pt) | (x$des$m & x$des$s & !x$des$h)
  origin <- tolower(paste(h$layer_origin %||% rep(NA, n), h$rock_origin %||% rep(NA, n)))
  sap <- (x$role %in% c("C", "D", "C/B", "CB", "R") | x$unknown_role) &
         grepl("saprolit|saprock|weathered rock|decomposed rock", origin)
  hard <- (x$rock | x$hard | .asc_pan_layer(x) | sap) & !thin
  b_roles <- c(.ASC_FAM_B_ROLES, "AB", "A/B")
  b_known <- !x$des$buried & x$des$known & x$role %in% b_roles
  b_maybe <- x$unknown_role & x$maybe_soil
  has_b <- if (any(b_known)) TRUE else if (any(b_maybe)) NA else FALSE
  first_b <- suppressWarnings(min(h$top_cm[b_known | b_maybe], na.rm = TRUE))
  last <- rows[length(rows)]
  bottom <- if (is.na(h$bottom_cm[last])) NA_real_ else max(h$bottom_cm[rows], na.rm = TRUE)
  depth_with <- function(with_b) {
    lo <- NA_real_; hi <- NA_real_
    for (i in rows) {
      st <- if (hard[i]) TRUE
            else if (!with_b || h$top_cm[i] < first_b) FALSE
            else if (x$des$known[i] && x$role[i] %in% c("C", "C/B", "CB")) TRUE
            else if (x$unknown_role[i] && !b_known[i]) NA
            else FALSE
      if (is.na(lo) && !isFALSE(st)) lo <- h$top_cm[i]
      if (isTRUE(st)) { hi <- h$top_cm[i]; break }
    }
    if (is.na(hi)) hi <- bottom
    if (is.na(lo)) lo <- hi
    c(lo, hi) - s0
  }
  d <- if (isTRUE(has_b)) depth_with(TRUE) else if (isFALSE(has_b)) depth_with(FALSE)
       else { a <- depth_with(TRUE); b <- depth_with(FALSE); c(min(a[1L], b[1L]), max(a[2L], b[2L])) }
  code <- if (anyNA(d)) NA_character_ else .asc_fam_bin_range(d[1L], d[2L], cls)
  .asc_fam_attr(cr, code, list(cm = d, has_b = has_b), miss)
}

# Water repellence of the surface soil (the first horizon of the profile),
# from water_repellence; undecided when not tested.
.asc_fam_repellence_code <- function(v) {
  v <- tolower(trimws(as.character(v)))
  if (is.na(v) || !nzchar(v)) return(NA_character_)
  if (grepl("^(n|nr|none|no|non)$", v) || grepl("non[- ]?(water[- ]?)?repell|not (water )?repell", v)) return("NR")
  if (grepl("^(s|sr)$", v) || grepl("strong", v)) return("SR")
  if (grepl("^(r|w|wr)$", v) || grepl("repell", v)) return("WR")
  NA_character_
}
.asc_fam_repellence <- function(x, cr) {
  h <- x$h; top0 <- .asc_profile_top(x)
  o <- order(h$top_cm, na.last = NA)
  o <- if (is.na(top0)) integer(0) else o[h$top_cm[o] >= top0 - 0.5]
  v <- if (length(o)) (h$water_repellence %||% rep(NA_character_, x$n))[o[1L]] else NA_character_
  .asc_fam_attr(cr, .asc_fam_repellence_code(v), list(recorded = v), "water_repellence")
}

# ---- Organosols ------------------------------------------------------------------

# The uppermost organic materials: the run of organic rows (O1, O2, P) that
# holds the first organic row other than an O1, with the O1 directly over it
# (a litter over a mineral layer, and the mineral layer, are passed over: an
# Organosol may hold its organic materials under a mineral cover); the first
# organic row when all are O1.
.asc_fam_organic_run <- function(x) {
  h <- x$h
  org <- which(x$organic %in% TRUE & !x$des$buried & !is.na(h$top_cm))
  if (!length(org)) return(NULL)
  main <- org[!(x$role[org] %in% "O1")]
  first <- if (length(main)) main[which.min(h$top_cm[main])] else org[which.min(h$top_cm[org])]
  top1 <- h$top_cm[first]
  repeat {
    above <- org[!is.na(h$bottom_cm[org]) & abs(h$bottom_cm[org] - top1) <= 0.5 & h$top_cm[org] < top1]
    if (!length(above)) break
    top1 <- min(h$top_cm[above])
  }
  ok <- ifelse(x$des$buried, FALSE, x$organic)
  r <- .asc_run_from(x, ok, top1)
  r$maybe <- setdiff(r$rows, r$sure_rows)
  r
}

# Nature of altered organic materials, only when present: granular (a layer
# of granular or subangular blocky organic material at least 0.2 m thick;
# also a granular peat field texture), ashy (ash), vitric (fusic material);
# the thickest when more than one is present.
.asc_fam_alteration <- function(x, cr) {
  h <- x$h
  run <- .asc_fam_organic_run(x)
  if (is.null(run) || !length(run$sure_rows)) return(NULL)
  rows <- run$sure_rows
  th <- pmax(0, h$bottom_cm - h$top_cm)
  t <- tolower(as.character(h$structure_type)); g <- tolower(as.character(h$structure_grade))
  tex <- toupper(gsub("[^A-Za-z]", "", as.character(h$field_texture %||% rep(NA, x$n))))
  gran <- (grepl("granular|subangular", t) & !grepl("structureless|none", g)) | tex %in% "GP"
  org <- tolower(paste(h$layer_origin %||% rep(NA, x$n)))
  ashy <- grepl("\\bash(y)?\\b", org)
  vitric <- grepl("fusic|burnt peat|burnt organic|burned peat|burned organic", org)
  # granular: the thickest run of adjacent granular rows
  gl <- 0; cur <- 0; prev <- NA_real_
  for (i in rows[order(h$top_cm[rows])]) {
    if (gran[i] && !is.na(th[i]) && (is.na(prev) || abs(h$top_cm[i] - prev) <= 0.5)) cur <- cur + th[i] else cur <- if (gran[i]) th[i] %|NA|% 0 else 0
    prev <- h$bottom_cm[i]; gl <- max(gl, cur)
  }
  amt <- c(P = if (gl >= 20) gl else 0, HZ = sum(th[rows][ashy[rows]], na.rm = TRUE),
           JZ = sum(th[rows][vitric[rows]], na.rm = TRUE))
  amt <- amt[amt > 0]
  if (!length(amt)) return(NULL)
  code <- if (sum(amt == max(amt)) > 1L) NA_character_ else names(amt)[which.max(amt)]
  .asc_fam_attr(cr, code, list(cm = amt), c("structure_type", "layer_origin"))
}

.asc_fam_organic_thickness <- function(x, cr) {
  cls <- .asc_family_rules()$classes$organic_thickness
  run <- .asc_fam_organic_run(x)
  if (is.null(run)) return(.asc_fam_attr(cr, NA_character_, list(), c("oc_pct", "designation")))
  .asc_fam_attr(cr, .asc_fam_bin_range(run$sure, run$possible, cls), list(cm = c(run$sure, run$possible)),
                c("oc_pct", "designation", "bottom_cm"))
}

# Dominant kind (fibric, hemic, sapric peat) or texture (sandy, loamy,
# clayey peat) of the uppermost organic materials, the O1 left out; Z when
# there are none.
.asc_fam_organic_type <- function(x, cr) {
  run <- .asc_fam_organic_run(x)
  miss <- c("field_texture", "von_post_index", "fiber_content_rubbed_pct")
  if (is.null(run)) return(.asc_fam_attr(cr, NA_character_, list(), miss))
  rows <- setdiff(run$sure_rows, which(x$role %in% "O1"))
  maybe <- setdiff(run$maybe, which(x$role %in% "O1"))
  if (!length(rows) && !length(maybe)) return(.asc_fam_attr(cr, "Z", list()))
  tex <- toupper(gsub("[^A-Za-z]", "", as.character(x$h$field_texture %||% rep(NA, x$n))))
  type <- c(SP = "OD", LP = "OE", CP = "OF")[tex]
  kind <- c(fibric = "OA", hemic = "OB", sapric = "OC")[.asc_peat_types(x)]
  type <- ifelse(is.na(type), kind, type)
  .asc_fam_attr(cr, .asc_fam_dominant(x, rows, unname(type), maybe), list(rows = rows), miss)
}

# Dominant texture of the mineral horizons under the deepest organic
# materials (down to rock or a hard layer); Z when there are none.
.asc_fam_underlying <- function(x, cr) {
  h <- x$h
  miss <- c("field_texture", "clay_pct", "oc_pct")
  rows <- which(!is.na(h$top_cm) & !x$trailing)
  if (anyNA(x$organic[rows])) return(.asc_fam_attr(cr, NA_character_, list(), miss))
  org <- rows[x$organic[rows]]
  if (!length(org)) return(.asc_fam_attr(cr, NA_character_, list(), miss))
  z <- max(h$bottom_cm[org])
  if (is.na(z)) return(.asc_fam_attr(cr, NA_character_, list(), miss))
  below <- rows[h$top_cm[rows] >= z - 0.5]
  below <- below[order(h$top_cm[below])]
  stop <- which(x$hard[below])
  if (length(stop)) below <- below[seq_len(stop[1L] - 1L)]
  # no mineral horizon under them: absent over rock or a hard layer; when the
  # description ends in the organic materials, unknown
  if (!length(below)) return(.asc_fam_attr(cr, if (length(stop)) "Z" else NA_character_, list(), miss))
  cls <- .asc_fam_texture_class(x)
  .asc_fam_attr(cr, .asc_fam_dominant(x, below, cls), list(rows = below), miss)
}

# ---- the family ---------------------------------------------------------------

#' Family of a soil under the Australian Soil Classification
#'
#' Places a soil in the family classes of its order (Australian Soil
#' Classification, 3rd edition, the 'Family Criteria' of each order page).
#' The family is not a level of the key: each criterion is a class of its
#' own, given by a one- or two-letter code. Most orders use the thickness of
#' the A1 horizon (of the whole A horizon in the Kurosols, Sodosols and
#' Chromosols and in the Kurosolic, Sodosolic and Chromosolic Hydrosols, as
#' the book's general rule has it), with any organic horizon over it; gravel
#' (2-60 mm) of the surface and A1 horizon (the larger of the estimate of the
#' ground surface, \code{site$surface_gravel_pct} or
#' \code{site$surface_coarse_fragments_pct}, and that of the A1); the field
#' texture of the A1 (its major part, peaty under organic surface horizons)
#' and the most clayey field texture of the B; soil depth; and water
#' repellence of the surface soil (\code{water_repellence}). The Vertosols
#' use laboratory clay (upper 0.1 m and the B), the Calcarosols the depth of
#' the first Bk horizon, the Arenosols and Rudosols the surface soil, and the
#' Organosols the kind and thickness of their organic materials and the
#' texture under them. Soil depth is the depth to rock, a cemented pan (not a
#' thin ironpan), another cemented layer or saprolite; otherwise, in a soil
#' with a B horizon, the depth to the C below it, and in a soil without one,
#' the depth described. The Anthroposols have no family, nor do the
#' Subtidal, Subaqueous and Intertidal Hydrosols. The readings where the
#' text is silent are in \code{system.file("rules", "asc3", "readings.yaml",
#' package = "soilKey")} (topic "Families").
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param order,suborder,great_group Codes of the soil's classes (as in
#'   \code{levels} of a \code{\link{classify_asc3}} result). When
#'   \code{order} is \code{NULL} the soil is classified first.
#' @return A named list of \code{\link{FamilyAttribute}}, one per criterion
#'   of the order that applies, in the order of the book's family name:
#'   \code{value} is the class name (\code{NULL} when undecided or when no
#'   class fits), \code{evidence$code} the class code (\code{"YY"} undecided,
#'   \code{"ZZ"} no class of the order fits). An empty list when the soil has
#'   no family.
#' @references Isbell, R.F. and the National Committee on Soil and Terrain
#'   (2021). The Australian Soil Classification, 3rd ed. CSIRO Publishing,
#'   Melbourne. Online version: www.soilscienceaustralia.org.au/asc.
#' @seealso \code{\link{classify_asc3}}.
#' @examples
#' fam <- classify_asc3_family(make_asc3_example("chromosols"))
#' vapply(fam, function(a) a$evidence$code, "")
#' @export
classify_asc3_family <- function(pedon, order = NULL, suborder = NULL, great_group = NULL) {
  if (!inherits(pedon, "PedonRecord")) rlang::abort("pedon must be a PedonRecord")
  pedon <- .cssc_prepare_pedon(pedon)
  if (is.null(order)) {
    res <- classify_asc3(pedon, on_missing = "silent")
    at <- function(l) { k <- res$levels$code[res$levels$level == l]; if (length(k)) k[1L] else NULL }
    order <- at("orders"); suborder <- at("suborders"); great_group <- at("great_groups")
  }
  crit <- .asc_family_criteria(order, suborder)
  if (!length(crit)) return(list())
  x <- .asc_horizons(pedon)
  out <- list()
  for (cr in crit) {
    a <- switch(cr$kind,
      thickness = .asc_fam_thickness(x, cr, great_group),
      gravel = .asc_fam_gravel(x, cr, pedon),
      texture = .asc_fam_texture(x, cr),
      clay = .asc_fam_clay(x, cr),
      depth = .asc_fam_depth(x, cr),
      repellence = .asc_fam_repellence(x, cr),
      alteration = .asc_fam_alteration(x, cr),
      organic_thickness = .asc_fam_organic_thickness(x, cr),
      organic_type = .asc_fam_organic_type(x, cr),
      underlying = .asc_fam_underlying(x, cr),
      rlang::abort(sprintf("Unknown family criterion kind '%s'", cr$kind)))
    if (!is.null(a)) out[[cr$id]] <- a
  }
  out
}

# The family as the book writes it after the name: the class names in order,
# two textures joined ("sandy/clayey"); classes not decided (YY) or with no
# class (ZZ) are left out, and so is a pair with such a side (one texture
# alone would read as the other); the absent ones of the Organosols (Z) are
# left out of their pair, and a pair whose other side does not apply (no B)
# is written alone.
.asc_family_label <- function(fam) {
  parts <- character(0); pair <- character(0); pair_ok <- TRUE
  flush <- function() {
    if (length(pair) && pair_ok) parts <<- c(parts, paste(pair, collapse = "/"))
    pair <<- character(0); pair_ok <<- TRUE
  }
  for (a in fam) {
    code <- a$evidence$code
    bad <- code %in% c("YY", "ZZ")
    v <- if (is.null(a$value) || code == "Z") NA_character_ else a$value
    p <- a$evidence$pair %||% 0L
    if (p == 1L) { flush(); if (bad) pair_ok <- FALSE else if (!is.na(v)) pair <- v; next }
    if (p == 2L) { if (bad) pair_ok <- FALSE else if (!is.na(v)) pair <- c(pair, v); flush(); next }
    flush()
    if (!is.na(v)) parts <- c(parts, v)
  }
  flush()
  paste(parts, collapse = ", ")
}

# The family codes in order ("B E L O V NR"; YY undecided, ZZ no class).
.asc_family_code <- function(fam) {
  paste(vapply(fam, function(a) a$evidence$code, ""), collapse = " ")
}
