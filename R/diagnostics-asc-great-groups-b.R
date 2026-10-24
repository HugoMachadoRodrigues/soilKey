# =============================================================================
# v0.9.283 -- great groups of the Australian Soil Classification (3rd ed.)
# for the orders with a B horizon of their own: Kurosols, Sodosols,
# Chromosols, Ferrosols, Dermosols and Kandosols. Pans in or under the B,
# the structure, plasticity and chemistry of the B2 or B2t (base status,
# exchangeable Ca/Mg, ESP), carbonate. Three-valued; the readings are in
# inst/rules/asc3/readings.yaml.
# =============================================================================

# ---- windows of the B ---------------------------------------------------------

# The major part of a window of the B2 or the B2t:
#   "b2"        the B2 (Ferrosols, Dermosols, Kandosols), as the order reads it;
#   "b2t"       the whole B2t: the B2 horizons from the clear or abrupt
#               textural B down, adjacent (as the upper B2t of the order);
#   "upper_b2t" its upper 0.2 m, or the whole of it when thinner;
#   "upper_b2"  the upper 0.2 m of the B2, or the whole of it when thinner.
# Rows of unknown role that may be part of it are tried in and out; the
# verdict holds only when both ways agree.
.asc_b_major <- function(pedon, x, ok, window = c("b2", "b2t", "upper_b2t", "upper_b2")) {
  window <- match.arg(window)
  if (window == "b2") return(.asc_b2_major(x, ok))
  .asc_b_verdict(pedon, x, window, function(w) .asc_major_part(x, w$rows, ok, w$top, w$bottom))
}

# A verdict on a window of the B2t or B2 (`fn` takes the window: rows, top,
# bottom), with every way the rows of unknown role that may be part of it
# can be in it or out (up to 8 of them; with more, undecided): TRUE or
# FALSE only when all agree.
.asc_b_verdict <- function(pedon, x, window, fn) {
  h <- x$h
  maybe <- .asc_b2_maybe(x)
  b_row <- NULL
  if (window %in% c("b2t", "upper_b2t")) {
    tb <- textural_b_asc(pedon)
    if (!isTRUE(tb$passed)) return(NA)
    b_row <- tb$layers[1L]
  } else if (!x$any_known) return(NA)
  depth <- if (window %in% c("b2t", "b2")) 1e4 else 20
  # only rows that could fall in the window are tried: at or below its
  # start, and for the upper 0.2 m starting within 0.2 m of the first B2
  from <- if (is.null(b_row)) -Inf else h$top_cm[b_row]
  maybe <- maybe[!is.na(h$top_cm[maybe]) & h$top_cm[maybe] >= from - 0.5]
  if (depth < 1e4) {
    # a known B2 at or below the start caps where the window can begin;
    # with none, any of the rows may begin it
    t0 <- suppressWarnings(min(h$top_cm[x$b2 %in% TRUE & !is.na(h$top_cm) & h$top_cm >= from - 0.5], na.rm = TRUE))
    if (is.finite(t0)) maybe <- maybe[h$top_cm[maybe] < t0 + depth]
  }
  one <- function(extra) {
    cand <- x$b2 %in% TRUE; cand[extra] <- TRUE
    w <- if (is.null(b_row)) .asc_window(x, cand, -Inf, depth)
         else .asc_window(x, cand, x$h$top_cm[b_row], depth, fallback = b_row)
    if (is.null(w) || !length(w$rows)) NA else fn(w)
  }
  if (length(maybe) > 8L) return(NA)
  subsets <- lapply(seq_len(2^length(maybe)) - 1L, function(k) maybe[bitwAnd(k, 2^(seq_along(maybe) - 1L)) > 0])
  ans <- vapply(subsets, one, NA)
  if (all(ans %in% TRUE)) TRUE else if (all(ans %in% FALSE)) FALSE else NA
}

# The B horizons of the present soil (B1, B2, B3, BC, B/C and the like), a
# cemented pan (m) left out (the glossary counts pans as substrate features,
# not B horizon development: for whether the B is calcareous); with
# `pans = TRUE` the B-designated pans too (a pan 'within the B'); and the
# row directly under the deepest B that is not a pan (a pan under another
# pan lies under substrate, not under the B).
.asc_b_rows <- function(x, pans = FALSE) {
  which(!is.na(x$role) & grepl("^B", x$role) & !x$des$buried & (pans | !x$des$m))
}
.asc_under_b <- function(x) {
  h <- x$h; b <- .asc_b_rows(x)
  if (!length(b)) return(integer(0))
  bot <- suppressWarnings(max(h$bottom_cm[b], na.rm = TRUE))
  if (!is.finite(bot)) return(integer(0))
  which(!is.na(h$top_cm) & abs(h$top_cm - bot) <= 0.5 & !x$des$buried & !(seq_len(x$n) %in% b))
}

# ---- pans ----------------------------------------------------------------------

# A pan of one of `kinds` in each row: TRUE when pan_type names one of them,
# or the designation does (km a calcrete pan; sm without h a thin ironpan;
# qm a silcrete; cm a ferricrete, as the glossary's Bcm); NA for a pan or a hard (cemented) layer of another or
# unknown kind that may be one ('other pan', an m of no named kind, a
# cemented layer with no pan recorded); FALSE otherwise: pan_type 'none', a
# pan of another named kind, or no pan and no cemented layer recorded (a pan
# shows: the surveyor records it, as the order reads hard layers).
.asc_pan_rows <- function(x, kinds) {
  h <- x$h; n <- x$n
  pt <- tolower(trimws(as.character(h$pan_type %||% rep(NA_character_, n))))
  sfx <- x$des$suffixes
  has <- function(l) vapply(sfx, function(s) l %in% s, NA)
  des_kind <- ifelse(x$des$m & x$des$k, "calcrete",
                     ifelse(x$des$m & x$des$s & !x$des$h, "thin ironpan",
                            ifelse(x$des$m & has("q"), "silcrete",
                                   ifelse(x$des$m & has("c"), "ferricrete", ifelse(x$des$m, "unknown", NA)))))
  pat <- paste0("\\b(", paste(kinds, collapse = "|"), ")\\b")
  named <- (!is.na(pt) & grepl(pat, pt)) | (!is.na(des_kind) & grepl(pat, des_kind))
  # the red-brown hardpan is one of the silica pans known as duripans
  # (glossary): a duripan may be one
  maybe_kind <- !is.na(pt) & ("red-brown hardpan" %in% kinds) & grepl("duripan", pt)
  other_named <- !is.na(pt) & pt != "none" & !grepl(pat, pt) & !grepl("other pan", pt) & !maybe_kind
  unknown <- (!is.na(pt) & grepl("other pan", pt)) | des_kind %in% "unknown" | maybe_kind |
             (x$hard & !x$rock & is.na(pt) & is.na(des_kind))
  ifelse(named, TRUE, ifelse(unknown & !other_named, NA, FALSE))
}

.asc_any_rows <- function(v) if (any(v %in% TRUE)) TRUE else if (anyNA(v)) NA else FALSE

#' Pans of the great groups: Duric (a red-brown hardpan in or directly
#' under the B), Petroferric (ferricrete, a petroferric or, with
#' where = "b", a petroreticulate horizon, within the solum or in or directly
#' under the B), Placic (a thin ironpan in or directly under the B)
#' @noRd
pan_great_group_asc <- function(pedon, kind = c("duric", "petroferric", "placic"), where = c("b", "solum")) {
  kind <- match.arg(kind); where <- match.arg(where)
  x <- .asc_horizons(pedon)
  kinds <- switch(kind, duric = "red-brown hardpan",
                  petroferric = c("ferricrete", "alcrete", "petroferric", if (where == "b") "petroreticulate"),
                  placic = "thin ironpan")
  rows <- if (!x$any_known) integer(0)
          else if (where == "solum") which(x$solum %in% TRUE | x$maybe_soil)
          else c(.asc_b_rows(x, pans = TRUE), .asc_under_b(x))
  v <- .asc_pan_rows(x, kinds)
  passed <- if (!x$any_known) { if (any(v %in% TRUE)) NA else if (all(v %in% FALSE)) FALSE else NA }
            else .asc_any_rows(v[rows])
  .asc_result(paste0("gg_", kind), passed, rows[v[rows] %in% TRUE], list(where = where),
              if (is.na(passed)) c("pan_type", "designation", "cementation_class") else character(0),
              sprintf("Great groups %s; glossary '%s'", tools::toTitleCase(kind),
                      switch(kind, duric = "Red-brown hardpan", petroferric = "Petroferric horizon",
                             placic = "Thin ironpan")))
}

#' Petrocalcic: a B horizon that is not calcareous, directly over a calcrete pan
#' @noRd
petrocalcic_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  calc <- .asc_calcareous(x)
  pan_v <- .asc_pan_rows(x, "calcrete")
  if (!x$any_known) {
    passed <- if (all(pan_v %in% FALSE)) FALSE else NA
    return(.asc_result("gg_petrocalcic", passed, integer(0), list(), "designation",
                       "Great groups Petrocalcic; glossary 'Calcrete pan'"))
  }
  b <- .asc_b_rows(x)
  # each calcrete pan (or layer that may be one) in or under the B: the B
  # horizons over it, the deepest of them directly over it, not calcareous
  cand <- intersect(c(.asc_b_rows(x, pans = TRUE), .asc_under_b(x)), which(!(pan_v %in% FALSE)))
  per <- vapply(cand, function(p) {
    above <- b[!is.na(h$bottom_cm[b]) & h$bottom_cm[b] <= h$top_cm[p] + 0.5]
    if (!length(above) || !any(abs(h$bottom_cm[above] - h$top_cm[p]) <= 0.5)) return(FALSE)
    b_not <- if (any(calc[above] %in% TRUE)) FALSE else if (all(calc[above] %in% FALSE)) TRUE else NA
    .and3(b_not, pan_v[p])
  }, NA)
  passed <- if (!length(cand)) FALSE else .asc_any_rows(per)
  under <- cand
  .asc_result("gg_petrocalcic", passed, under, list(pans = cand),
              if (is.na(passed)) c("effervescence_class", "pan_type", "designation") else character(0),
              "Great groups Petrocalcic; glossary 'Calcrete pan'")
}

# ---- the structure and plasticity of the B2 ------------------------------------

#' Pedaric: strong blocky or polyhedral structure in the upper B2t (or B2),
#' and peds that hold together very weakly (a spade goes in very easily when
#' dry: a dry strength of weak or less)
#' @noRd
pedaric_asc <- function(pedon, window = c("upper_b2t", "upper_b2")) {
  window <- match.arg(window)
  x <- .asc_horizons(pedon); h <- x$h
  g <- tolower(as.character(h$structure_grade)); t <- tolower(as.character(h$structure_type))
  strong <- ifelse(is.na(g) | !nzchar(g), NA, grepl("strong", g) & !grepl("moderate", g))
  bp <- ifelse(is.na(t) | !nzchar(t), NA, grepl("blocky|polyhedral", t))
  strong_bp <- .and3v(strong, bp)
  # peds that hold together very weakly, the soil weak when dry (a spade
  # enters very easily): a dry strength of weak or less; firm or more is not
  sd <- tolower(as.character(h$strength_dry %||% rep(NA_character_, x$n)))
  weak_adh <- ifelse(is.na(sd) | !nzchar(sd), NA, sd %in% c("loose", "very weak", "weak"))
  ok <- .and3v(strong_bp, weak_adh)
  # the text does not say 'major part': every row of the window passes, and
  # one that fails rules it out
  passed <- .asc_b_verdict(pedon, x, window, function(w) {
    if (any(ok[w$rows] %in% FALSE)) FALSE else if (all(ok[w$rows] %in% TRUE)) TRUE else NA
  })
  .asc_result("gg_pedaric", passed, integer(0), list(window = window),
              if (is.na(passed)) c("structure_grade", "structure_type", "strength_dry") else character(0),
              "Great groups Pedaric")
}

#' Subplastic: the major part of the B2t (or B2) strongly subplastic (a
#' horizon with no record is read as not, as for the order)
#' @noRd
subplastic_asc <- function(pedon, window = c("b2t", "b2")) {
  window <- match.arg(window)
  x <- .asc_horizons(pedon)
  passed <- .asc_b_major(pedon, x, .asc_strongly_subplastic(x$h), window)
  .asc_result("gg_subplastic", passed, integer(0), list(window = window),
              if (is.na(passed)) c("subplasticity", "designation") else character(0),
              "Great groups Subplastic; glossary 'Strongly subplastic'")
}

# ---- chemistry of the B2 ----------------------------------------------------------

#' Magnesic: exchangeable Ca/Mg below 0.1 in the major part of the B2t (or
#' B2); with `natric`, also sodic in the major part of the upper 0.2 m of
#' the B2t (Magnesic-Natric)
#' @noRd
magnesic_asc <- function(pedon, window = c("b2t", "b2"), natric = FALSE) {
  window <- match.arg(window)
  x <- .asc_horizons(pedon); h <- x$h
  ratio <- ifelse(is.na(h$ca_cmol) | is.na(h$mg_cmol), NA,
                  ifelse(h$mg_cmol <= 0, FALSE, h$ca_cmol / h$mg_cmol < 0.1))
  mg <- .asc_b_major(pedon, x, ratio, window)
  passed <- if (isTRUE(natric)) .and3(mg, .asc_b_major(pedon, x, .asc_sodic(h), "upper_b2t")) else mg
  .asc_result(if (isTRUE(natric)) "gg_magnesic_natric" else "gg_magnesic", passed, integer(0), list(window = window),
              if (is.na(passed)) c("ca_cmol", "mg_cmol", "na_cmol", "cec_cmol") else character(0),
              "Great groups Magnesic, Magnesic-Natric")
}

#' Natric (Kurosols): sodic in the major part of the upper 0.2 m of the B2t
#' @noRd
natric_asc <- function(pedon) {
  x <- .asc_horizons(pedon)
  passed <- .asc_b_major(pedon, x, .asc_sodic(x$h), "upper_b2t")
  .asc_result("gg_natric", passed, integer(0), list(),
              if (is.na(passed)) c("na_cmol", "cec_cmol", "esp_pct") else character(0),
              "Great groups Natric; glossary 'Sodic'")
}

# The base status of each horizon (glossary): exchangeable Ca, Mg, K and Na
# of the fine earth x 100 / clay %, in cmol(+)/kg clay, as a range when the
# clay is a field texture range.
.asc_base_status <- function(x) {
  h <- x$h
  s <- h$ca_cmol + h$mg_cmol + h$k_cmol + h$na_cmol
  cr <- x$clay
  list(lo = ifelse(is.na(s) | is.na(cr$hi), NA, 100 * s / pmax(cr$hi, 1e-9)),
       hi = ifelse(is.na(s) | is.na(cr$lo), NA, ifelse(cr$lo <= 0, Inf, 100 * s / cr$lo)))
}

#' Dystrophic, Mesotrophic and Eutrophic: the base status of the major part
#' of the B2t (or B2) below 5, from 5 to 15, or above 15 cmol(+)/kg clay;
#' Eutrophic also needs B and BC horizons that are not calcareous
#' @noRd
base_status_asc <- function(pedon, class = c("dystrophic", "mesotrophic", "eutrophic"), window = c("b2t", "b2")) {
  class <- match.arg(class); window <- match.arg(window)
  x <- .asc_horizons(pedon)
  bs <- .asc_base_status(x)
  ok <- switch(class,
    dystrophic = ifelse(is.na(bs$hi), ifelse(!is.na(bs$lo) & bs$lo >= 5, FALSE, NA), ifelse(bs$hi < 5, TRUE, ifelse(bs$lo >= 5, FALSE, NA))),
    mesotrophic = ifelse(!is.na(bs$lo) & !is.na(bs$hi) & bs$lo >= 5 & bs$hi <= 15, TRUE,
                         ifelse((!is.na(bs$hi) & bs$hi < 5) | (!is.na(bs$lo) & bs$lo > 15), FALSE, NA)),
    eutrophic = ifelse(!is.na(bs$lo) & bs$lo > 15, TRUE, ifelse(!is.na(bs$hi) & bs$hi <= 15, FALSE, NA)))
  maj <- .asc_b_major(pedon, x, ok, window)
  passed <- if (class == "eutrophic") {
    calc <- .asc_calcareous(x); b <- .asc_b_rows(x)
    not_calc <- if (!length(b)) NA else if (any(calc[b] %in% TRUE)) FALSE else if (all(calc[b] %in% FALSE)) TRUE else NA
    .and3(maj, not_calc)
  } else maj
  .asc_result(paste0("gg_", class), passed, integer(0), list(window = window),
              if (is.na(passed)) c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol", "clay_pct") else character(0),
              sprintf("Great groups %s; glossary 'Base status'", tools::toTitleCase(class)))
}

#' ESP great groups of the Sodosols: Subnatric (6 to less than 15),
#' Mesonatric (15 to 25), Hypernatric (more than 25) in the major part of the
#' upper 0.2 m of the B2t; with `mottled`, that part mottled too
#' @noRd
esp_great_group_asc <- function(pedon, class = c("subnatric", "mesonatric", "hypernatric"), mottled = FALSE) {
  class <- match.arg(class)
  x <- .asc_horizons(pedon); h <- x$h
  e <- .asc_esp(h)
  esp <- e$esp
  ok <- ifelse(e$low_charge, FALSE, ifelse(is.na(esp), NA, switch(class,
    subnatric = esp >= 6 & esp < 15, mesonatric = esp >= 15 & esp <= 25, hypernatric = esp > 25)))
  # one major part that is in the class and, for the Mottled ones, mottled
  passed <- .asc_b_major(pedon, x, if (isTRUE(mottled)) .and3v(ok, .asc_mottled_rows(h)) else ok, "upper_b2t")
  .asc_result(paste0("gg_", if (isTRUE(mottled)) "mottled_" else "", class), passed, integer(0), list(),
              if (is.na(passed)) c("na_cmol", "cec_cmol", "esp_pct", "redoximorphic_features_pct", "mottle_contrast")
              else character(0),
              "Sodosols, great groups; glossary 'ESP', 'Mottled horizon'")
}

#' Effervescent (Sodosols): the fine earth of every horizon of the solum
#' effervesces with 1 M HCl
#' @noRd
effervescent_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  eff <- tolower(trimws(as.character(h$effervescence_class %||% rep(NA_character_, x$n))))
  yes <- grepl("slight|moderate|strong|high|very", eff) & !grepl("^non|^none|^nil", eff)
  no <- grepl("^non|^none|^nil|^n$", eff)
  lab <- h$caco3_pct %||% rep(NA_real_, x$n)
  acid <- ifelse(!is.na(h$ph_h2o), h$ph_h2o < 7.0, !is.na(h$ph_cacl2) & h$ph_cacl2 < 6.5)
  ok <- ifelse(yes | (!is.na(lab) & lab >= 0.5), TRUE,
               ifelse(no | (!is.na(lab) & lab < 0.5) | ((is.na(eff) | !nzchar(eff)) & is.na(lab) & acid %in% TRUE), FALSE, NA))
  sol <- if (x$any_known) which(x$solum %in% TRUE & x$mineral) else which(x$mineral & !x$des$buried)
  maybe <- if (x$any_known) which(x$maybe_soil) else integer(0)
  passed <- if (!length(sol)) NA else if (any(ok[sol] %in% FALSE)) FALSE
            else if (all(ok[c(sol, maybe)] %in% TRUE)) TRUE else NA
  .asc_result("gg_effervescent", passed, sol, list(),
              if (is.na(passed)) c("effervescence_class", "caco3_pct") else character(0),
              "Sodosols, great group Effervescent")
}

#' Calcareous (Ferrosols): some part of a B or BC horizon calcareous
#' @noRd
calcareous_b_asc <- function(pedon) {
  x <- .asc_horizons(pedon)
  b <- .asc_b_rows(x)
  passed <- if (!length(b)) { if (x$any_known) FALSE else NA } else .asc_any_rows(.asc_calcareous(x)[b])
  .asc_result("gg_calcareous", passed, b, list(),
              if (is.na(passed)) c("effervescence_class", "caco3_pct", "secondary_carbonates_pct") else character(0),
              "Ferrosols, great group Calcareous; glossary 'Calcareous'")
}

#' Mellic (Kandosols): massive to weakly structured B horizons that are very
#' porous with a weak consistence strength when moist. Porosity is not
#' recorded by soilKey: a pedal B (moderate or strong) or a moist strength of
#' firm or more rules it out; otherwise it is undecided.
#' @noRd
mellic_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  b <- .asc_b_rows(x)
  pedal <- .asc_pedal(h)
  sm <- tolower(as.character(h$strength_moist %||% rep(NA_character_, x$n)))
  firm <- !is.na(sm) & nzchar(sm) & !sm %in% c("loose", "very weak", "weak")
  passed <- if (!length(b)) { if (x$any_known) FALSE else NA }
            else if (any(pedal[b] %in% TRUE | firm[b])) FALSE else NA
  .asc_result("gg_mellic", passed, b, list(),
              if (is.na(passed)) c("structure_grade", "strength_moist") else character(0),
              "Kandosols, great group Mellic")
}

#' Andic (Kandosols): andic properties in basaltic tephric materials. The
#' tephric parent material is seen in the field: with no layer or rock origin
#' naming tephra or volcanic ash the soil is not Andic; with one, the andic
#' properties are undecided (soilKey does not test them here).
#' @noRd
andic_great_group_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  origin <- tolower(paste(h$layer_origin %||% rep(NA, x$n), h$rock_origin %||% rep(NA, x$n)))
  tephric <- grepl("tephr|volcanic ash|pyroclast|scoria|cinder", origin)
  passed <- if (any(tephric)) NA else FALSE
  .asc_result("gg_andic", passed, which(tephric), list(),
              if (is.na(passed)) c("layer_origin", "al_ox_pct", "fe_ox_pct", "phosphate_retention_pct") else character(0),
              "Kandosols, great group Andic; glossary 'Andic properties', 'Tephric materials'")
}
