# =============================================================================
# v0.9.325 -- the great groups of the Comprehensive Soil Classification
# System of Japan, First Approximation (CSCJ 2011; Obara et al., NIAES). The
# Dark Red soils (E) and the CSCJ's own Red-Yellow soils (G) are new gates;
# the other great groups are the SCSJ 2017 gates read by the CSCJ edition
# (site$jp_edition), under their CSCJ names. Three-valued.
# =============================================================================

.CSCJ_REF <- "Obara et al. (2011). Comprehensive Soil Classification System of Japan, First Approximation."

# The pedon marked to be read by the CSCJ edition (marked once).
.cscj_marked <- function(pedon) {
  if (!inherits(pedon, "PedonRecord") || identical((pedon$site %||% list())$jp_edition, "cscj2011")) pedon
  else .cscj_mark(pedon)
}

.cscj_result <- function(name, passed, layers = integer(0), evidence = list(),
                         missing = character(0), where = "") {
  DiagnosticResult$new(name = name, passed = passed, layers = as.integer(layers),
                       evidence = evidence, missing = unique(missing),
                       reference = sprintf("%s %s", .CSCJ_REF, where), notes = NA_character_)
}

# A gate the CSCJ shares with the SCSJ, read by the CSCJ edition and named
# and referenced as the CSCJ's.
.cscj_shared <- function(gate, name, where) {
  force(gate); force(name); force(where)
  function(pedon) {
    r <- gate(.cscj_marked(pedon))
    r$name <- name
    r$reference <- sprintf("%s %s", .CSCJ_REF, where)
    r
  }
}

#' @noRd
man_made_soils_cscj <- .cscj_shared(function(p) man_made_soils_scsj(p), "man_made_soils_cscj", "V A, p. 28")
#' @noRd
organic_soils_cscj <- .cscj_shared(function(p) organic_soils_scsj(p), "organic_soils_cscj", "V B, p. 28")
#' @noRd
podzols_cscj <- .cscj_shared(function(p) podzols_scsj(p), "podzols_cscj", "V C, p. 28")
#' @noRd
andosols_cscj <- .cscj_shared(function(p) andosols_scsj(p), "andosols_cscj", "V D, p. 28")
#' @noRd
lowland_soils_cscj <- .cscj_shared(function(p) fluvic_soils_scsj(p), "lowland_soils_cscj", "V F, p. 29")
#' @noRd
stagnic_soils_cscj <- .cscj_shared(function(p) stagnic_soils_scsj(p), "stagnic_soils_cscj", "V H, p. 29")
#' @noRd
brown_forest_soils_cscj <- .cscj_shared(function(p) brown_forest_soils_scsj(p), "brown_forest_soils_cscj", "V I, p. 29")

# Limestone or calcareous deposits (CSCJ IV.3: 40 % or more calcium
# carbonate equivalent) as the origin of the soil: the words limestone,
# calcareous, coral, marl, chalk, travertine (and the Japanese words) in
# rock_origin or layer_origin of the layers to 1 m, or in the site parent
# material (not for a profile that is all fill), negated words taken out;
# or a layer to 1 m with 40 % or more CaCO3. Nothing recorded: not (Hugo,
# 2026-11-01).
.SCSJ_LIMESTONE_RE <- paste0("\\b(limestones?|calcareous|corals?|coralline|marls?|marly|chalk|chalky|travertine)\\b|",
                             "\u77f3\u7070\u5ca9|\u77f3\u7070\u8cea|\u73ca\u745a|\u30b5\u30f3\u30b4|\u6ce5\u7070\u5ca9")
.SCSJ_NOT_LIMESTONE_RE <- paste0("\\b(non[- ]?|not +|no +|without +|free of +|lacking +)",
                                 "(limestones?|calcareous|corals?|coralline|marls?|marly|chalk|chalky|travertine)\\b|",
                                 "\\b(limestone|carbonate|calcareous|coral|travertine)[- ]free\\b|",
                                 "(\u77f3\u7070\u5ca9|\u77f3\u7070\u8cea|\u73ca\u745a|\u30b5\u30f3\u30b4)(\u4ee5\u5916|\u3067(\u306f)?\u306a\u3044|\u306a\u3057|\u3092\u542b\u307e\u306a\u3044)|",
                                 "\u975e\u77f3\u7070\u8cea|\u975e\u77f3\u7070")
.cscj_limestone_origin <- function(x) {
  soil <- !is.na(x$dtop) & x$dtop < 100
  txt <- c(.scsj_txt(x$h, "rock_origin")[soil], .scsj_txt(x$h, "layer_origin")[soil])
  if (any(soil & !x$orig$fill))
    txt <- c(txt, tolower(as.character(unlist(x$site$parent_material %||% character(0)))))
  txt <- txt[!is.na(txt)]
  txt <- gsub(.SCSJ_NOT_LIMESTONE_RE, " ", txt, perl = TRUE)
  words <- any(grepl(.SCSJ_LIMESTONE_RE, txt, perl = TRUE))
  cc <- .scsj_col(x$h, "caco3_pct")
  words || any(soil & !is.na(cc) & cc >= 40)
}

# The colour of the subsurface for the Dark Red soils (Hugo, 2026-11-01):
# that of the thickest B horizon (a buried B included) of the subsurface
# section (20 to 60 cm), else of the first B horizon below it, else of the
# thickest layer of the section (the text's general rule). An undesignated
# row is a B where it is a weathered or argic horizon; an unreadable
# designation may be one. Three-valued: rows that may be the one read
# (undecided B status, a horizon without a bottom, depth the description
# does not cover) leave it undecided where their colours would disagree.
.cscj_subsurface_dark_red <- function(x, f = .scsj_horizon_flags(x)) {
  if (is.na(x$surface)) return(NA)
  dr <- x$colour$dark_red
  wa <- f$weathered %in% TRUE | f$argic %in% TRUE
  is_b <- ifelse(x$role %in% "other", ifelse(wa, TRUE, NA),
          ifelse(!is.na(x$role), grepl("^B", x$role),
          ifelse(f$weathered %in% TRUE | f$argic %in% TRUE, TRUE,
          ifelse(f$weathered %in% FALSE & f$argic %in% FALSE, FALSE, NA))))
  cs <- .scsj_control(x)
  rows <- cs$rows
  # each row's thickness in the section: certain (an open row may end at its
  # top) and possible (to the end of the section)
  open <- is.na(x$dbot[rows])
  lo <- pmax(pmin(ifelse(open, x$dtop[rows], x$dbot[rows]), cs$to) - pmax(x$dtop[rows], cs$from), 0)
  hi <- pmax(pmin(ifelse(open, cs$to, x$dbot[rows]), cs$to) - pmax(x$dtop[rows], cs$from), 0)
  und <- if (isTRUE(cs$covered)) 0 else .scsj_cum(x, rep(FALSE, x$n), cs$from, cs$to)$poss
  agree <- function(v) if (all(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA
  # the colour of the thickest of `cand` (positions in rows); `extra` rows
  # that may also be candidates
  thickest <- function(cand, extra = integer(0)) {
    best <- max(lo[cand])
    if (und > 0 && und >= best - 1e-9) return(NA)
    pool <- c(cand, extra)
    agree(dr[rows[pool[hi[pool] >= best - 1e-9]]])
  }
  sure <- which(is_b[rows] %in% TRUE); maybe <- which(is.na(is_b[rows]))
  if (length(sure)) return(thickest(sure, maybe))
  # no B surely in the section: undescribed depth there may hold one
  if (und >= 5) return(NA)
  # the first B below (a horizon reaching below the section), else the
  # thickest layer of the section; undescribed depth before that B may hold
  # another
  below_of <- function() {
    below <- x$ord[!is.na(x$dtop[x$ord]) & x$dtop[x$ord] >= cs$from & !(x$ord %in% rows) &
                   (is.na(x$dbot[x$ord]) | x$dbot[x$ord] > cs$to + 0.5) & !(x$mineral[x$ord] %in% FALSE)]
    for (i in below) {
      if (x$dtop[i] > cs$to + 0.5 && .scsj_cum(x, rep(FALSE, x$n), cs$to, x$dtop[i])$poss > 0.5) return(NA)
      if (is_b[i] %in% TRUE) return(dr[i])
      if (is.na(is_b[i])) return(NA)
    }
    e <- .scsj_ends(x)
    if (isTRUE(x$complete) || e$on_rock || (!is.na(e$depth) && e$depth >= 100)) {
      if (!length(rows)) return(NA)
      return(thickest(seq_along(rows)))
    }
    NA
  }
  fallback <- below_of()
  if (!length(maybe)) return(fallback)
  # rows that may be B: any of them may be the only one, so their colours
  # and the fallback must agree
  agree(c(dr[rows[maybe]], fallback))
}

# Dark Red soils (CSCJ 2011, great group E): (1) the top of a weathered or
# argic horizon within 50 cm; (2) a dark red subsurface, or soil from
# limestone or calcareous deposits whose every subhorizon of the subsurface
# has pH(H2O) 5.5 or more or base saturation 50 % or more; (3) no bedrock
# within 30 cm.
dark_red_soils_cscj <- function(pedon) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) {
    r <- .scsj_no_surface("dark_red_soils_cscj", x, "")
    r$reference <- sprintf("%s V E, p. 29", .CSCJ_REF)
    return(r)
  }
  f <- .scsj_horizon_flags(x)
  b <- .scsj_b_within_50(x)
  dark <- .cscj_subsurface_dark_red(x, f)
  lime <- .cscj_limestone_origin(x)
  bs <- .scsj_base_status(x)
  c2 <- .scsj_or(dark, .scsj_and(lime, bs$high))
  rock <- .scsj_no_rock_within(x, 30)
  passed <- .scsj_and3(b$passed, c2, rock)
  .cscj_result("dark_red_soils_cscj", passed, b$rows,
               list(weathered_or_argic_within_50 = b$passed, dark_red_subsurface = dark,
                    limestone_origin = lime, base_rich_subsurface = bs$high, no_rock_within_30 = rock),
               if (is.na(passed)) c("munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist", "ph_h2o",
                                    "bs_pct", "designation", "structure_grade", "bottom_cm") else character(0),
               "V E, p. 29")
}

# Red-Yellow soils (CSCJ 2011, great group G): a weathered or argic horizon
# with the red-yellow feature, 25 cm or more (one horizon: adjacent layers;
# Hugo, 2026-11-01), its top within 50 cm; no bedrock within 30 cm. Unlike
# the SCSJ, no base-status criterion and no red-yellow weathered horizon.
red_yellow_soils_cscj <- function(pedon) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) {
    r <- .scsj_no_surface("red_yellow_soils_cscj", x, "")
    r$reference <- sprintf("%s V G, p. 29", .CSCJ_REF)
    return(r)
  }
  f <- .scsj_horizon_flags(x)
  ry <- .scsj_red_yellow_rows(x)
  bry <- .scsj_and(.scsj_or(f$argic, f$weathered), ry)
  a <- .scsj_horizon_within(x, bry, 25, 50)$passed
  rock <- .scsj_no_rock_within(x, 30)
  passed <- .scsj_and(a, rock)
  .cscj_result("red_yellow_soils_cscj", passed, which(bry %in% TRUE),
               list(red_yellow_weathered_or_argic = a, no_rock_within_30 = rock),
               if (is.na(passed)) c("munsell_hue_moist", "clay_pct", "oc_pct", "designation", "structure_grade")
               else character(0), "V G, p. 29")
}
