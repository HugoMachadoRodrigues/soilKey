# =============================================================================
# v0.9.325 -- the two great groups of the Comprehensive Soil Classification
# System of Japan, First Approximation (CSCJ 2011; Obara et al., NIAES) that
# the SCSJ 2017 does not share: the Dark Red soils (E) and its own Red-Yellow
# soils (G). The other great groups are keyed by the SCSJ gates, read by the
# CSCJ edition (site$jp_edition, set by classify_cscj2011()). Three-valued.
# =============================================================================

# Limestone or calcareous deposits (CSCJ IV.3: 40 % or more calcium
# carbonate equivalent) as the origin of the soil: words in rock_origin,
# layer_origin or the site parent material, or a layer with 40 % or more
# CaCO3; nothing recorded: not (Hugo, 2026-11-01).
.SCSJ_LIMESTONE_RE <- paste0("limestone|calcareous|\\bcoral|marl|chalk|travertine|",
                             "\u77f3\u7070\u5ca9|\u77f3\u7070\u8cea|\u73ca\u745a|\u30b5\u30f3\u30b4|\u6ce5\u7070\u5ca9")
.SCSJ_NOT_LIMESTONE_RE <- "non[- ]?calcareous|not calcareous|\u975e\u77f3\u7070"
.cscj_limestone_origin <- function(x) {
  txt <- c(.scsj_txt(x$h, "rock_origin"), .scsj_txt(x$h, "layer_origin"),
           tolower(as.character(unlist(x$site$parent_material %||% character(0)))))
  txt <- txt[!is.na(txt)]
  words <- any(grepl(.SCSJ_LIMESTONE_RE, txt, perl = TRUE) & !grepl(.SCSJ_NOT_LIMESTONE_RE, txt, perl = TRUE))
  cc <- .scsj_col(x$h, "caco3_pct")
  words || any(!is.na(cc) & cc >= 40)
}

# The colour of the subsurface for the Dark Red soils (Hugo, 2026-11-01):
# that of the thickest B horizon in the subsurface section (20 to 60 cm),
# else of the first B horizon below the section; an undesignated row counts
# as a B where it is a weathered or argic horizon. Three-valued: B rows of
# equal thickness that disagree, or rows that may be B, leave it undecided.
.cscj_subsurface_dark_red <- function(x, f = .scsj_horizon_flags(x)) {
  dr <- x$colour$dark_red
  is_b <- ifelse(!is.na(x$role), grepl("^B", x$role) & !x$des$buried,
          ifelse(f$weathered %in% TRUE | f$argic %in% TRUE, TRUE,
          ifelse(f$weathered %in% FALSE & f$argic %in% FALSE, FALSE, NA)))
  cs <- .scsj_control(x)
  if (!length(cs$rows) && is.na(x$surface)) return(NA)
  inside <- cs$rows
  ov <- pmin(ifelse(is.na(x$dbot[inside]), cs$to, x$dbot[inside]), cs$to) - pmax(x$dtop[inside], cs$from)
  b_sure <- inside[is_b[inside] %in% TRUE]
  b_maybe <- inside[is.na(is_b[inside])]
  if (length(b_sure)) {
    th <- ov[match(b_sure, inside)]
    top <- b_sure[th >= max(th) - 1e-9]
    # a row that may be a B and is thicker than the thickest sure one
    if (length(b_maybe) && any(ov[match(b_maybe, inside)] > max(th) + 1e-9)) return(NA)
    v <- dr[top]
    return(if (all(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA)
  }
  if (length(b_maybe)) return(NA)
  # no B in the section (an A reaching through it): the first B below
  below <- x$ord[!is.na(x$dtop[x$ord]) & x$dtop[x$ord] >= cs$to - 0.5 & !(is_b[x$ord] %in% FALSE)]
  if (!length(below)) return(NA)
  i <- below[1L]
  if (is.na(is_b[i])) return(NA)
  dr[i]
}

# Dark Red soils (CSCJ 2011, great group E): (1) the top of a weathered or
# argic horizon within 50 cm; (2) a dark red subsurface, or soil from
# limestone or calcareous deposits whose every subhorizon of the subsurface
# has pH(H2O) 5.5 or more or base saturation 50 % or more; (3) no bedrock
# within 30 cm.
dark_red_soils_cscj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("dark_red_soils_cscj", x, "CSCJ V E, p. 29"))
  f <- .scsj_horizon_flags(x)
  b <- .scsj_b_within_50(x)
  dark <- .cscj_subsurface_dark_red(x, f)
  lime <- .cscj_limestone_origin(x)
  bs <- .scsj_base_status(x)
  c2 <- .scsj_or(dark, .scsj_and(lime, bs$high))
  rock <- .scsj_no_rock_within(x, 30)
  passed <- .scsj_and3(b$passed, c2, rock)
  .scsj_result("dark_red_soils_cscj", passed, b$rows,
               list(weathered_or_argic_within_50 = b$passed, dark_red_subsurface = dark,
                    limestone_origin = lime, base_rich_subsurface = bs$high, no_rock_within_30 = rock),
               if (is.na(passed)) c("munsell_hue_moist", "munsell_value_moist", "ph_h2o", "bs_pct", "rock_origin")
               else character(0), "CSCJ V E, p. 29")
}

# Red-Yellow soils (CSCJ 2011, great group G): the top within 50 cm of a
# weathered or argic horizon with the red-yellow feature, 25 cm or more
# (cumulative); no bedrock within 30 cm. Unlike the SCSJ, no base-status
# criterion and no red-yellow weathered horizon.
red_yellow_soils_cscj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("red_yellow_soils_cscj", x, "CSCJ V G, p. 29"))
  f <- .scsj_horizon_flags(x)
  ry <- .scsj_red_yellow_rows(x)
  bry <- .scsj_and(.scsj_or(f$argic, f$weathered), ry)
  top <- .scsj_top_in(x, bry, -Inf, 50, 0)
  a <- .scsj_and(top, .scsj_at_least(.scsj_cum(x, bry), 25))
  rock <- .scsj_no_rock_within(x, 30)
  passed <- .scsj_and(a, rock)
  .scsj_result("red_yellow_soils_cscj", passed, which(bry %in% TRUE),
               list(red_yellow_weathered_or_argic = a, no_rock_within_30 = rock),
               if (is.na(passed)) c("munsell_hue_moist", "clay_pct", "oc_pct") else character(0),
               "CSCJ V G, p. 29")
}
