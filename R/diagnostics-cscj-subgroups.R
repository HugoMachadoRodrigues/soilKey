# =============================================================================
# v0.9.327 -- the 116 subgroups of the Comprehensive Soil Classification
# System of Japan, First Approximation (CSCJ 2011, chapter VI). The subgroups
# the CSCJ shares with the SCSJ 2017 are the SCSJ's generic gates, read by
# the CSCJ edition under CSCJ names, with their depths and features as
# parameters in inst/rules/cscj2011/subgroups/. New: the Cumulic Fulvic
# Andosols (the brown polyhumic surface horizon, 50 cm in 60 cm), the
# Low-humic Allophanic Andosols, and the Eutric and Terrace Brown Forest
# soils. Three-valued.
# =============================================================================

# The SCSJ subgroup gates the CSCJ reads, each under its CSCJ name
# (<name>_cscj), forwarding its parameters.
.CSCJ_SHARED_SUBGROUP_GATES <- c(
  "andic_subgroup", "aquic_above_rock", "bleached_horizon", "bleached_paddy_surface", "brown_layer",
  "buried_humic", "calcaric", "ekranic", "endofluvic", "humic_surface", "kuroboku_within", "moor_type",
  "no_wet_top", "organic_artefacts", "paddy_fe", "paddy_reverse_gley", "para_lithic", "parent_rock",
  "peat_within", "podzolic_horizon", "protoanthraquic", "reddish", "sapric_peat", "shallow_groundwater",
  "strong_gley", "thapto_red_yellow", "thapto_upland", "thionic", "upland", "wet_top")
for (.g in .CSCJ_SHARED_SUBGROUP_GATES) {
  assign(paste0(.g, "_cscj"),
         local({ g <- .g; .cscj_shared(function(p, ...) get(paste0(g, "_scsj"), mode = "function")(p, ...),
                                        paste0(g, "_cscj"), "VI (subgroups)") }))
}
rm(.g)

# Generic: the brown polyhumic kuroboku surface horizon, `thick` cm within
# `within` cm (the Fulvic Andosols 30 in 40; their Cumulic subgroup 50 in 60).
brown_polyhumic_cscj <- function(pedon, within = 40, thick = 30) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) return(.cscj_result("brown_polyhumic_cscj", NA, integer(0), list(), "oc_pct", "VI D"))
  passed <- .cscj_brown_polyhumic(x, within, thick)
  .cscj_result("brown_polyhumic_cscj", passed, integer(0), list(within = within, thick = thick),
               if (is.na(passed)) c("oc_pct", "munsell_value_moist", "munsell_chroma_moist") else character(0),
               "VI D, p. 32")
}

# Low-humic Allophanic Andosols: no humic or polyhumic surface horizon.
low_humic_cscj <- function(pedon) {
  r <- humic_surface_scsj(.cscj_marked(pedon), thick = 25)
  passed <- .scsj_not(r$passed)
  .cscj_result("low_humic_cscj", passed, integer(0), list(humic_surface = r$passed),
               if (is.na(passed)) r$missing else character(0), "VI D, p. 33")
}

# Eutric Brown Forest soils (CSCJ): every subhorizon of the subsurface at
# pH(H2O) 6.5 or more, on pH alone (unlike the SCSJ's one subhorizon, pH or
# base saturation); no liming clause (Hugo, 2026-11-01). No subsurface: not
# met; 5 cm or more of the section undescribed in one stretch: undecided.
eutric_all_cscj <- function(pedon) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) return(.cscj_result("eutric_all_cscj", NA, integer(0), list(), "ph_h2o", "VI I, p. 42"))
  cs <- .scsj_control(x)
  ph <- .scsj_col(x$h, "ph_h2o")[cs$rows]
  passed <- if (!length(cs$rows)) { if (isTRUE(cs$covered)) FALSE else NA } else .scsj_all(ifelse(is.na(ph), NA, ph >= 6.5))
  if (isTRUE(passed) && .cscj_undescribed(x, cs$from, cs$to, longest = TRUE) >= 5) passed <- NA
  .cscj_result("eutric_all_cscj", passed, cs$rows, list(every_subhorizon_ph_6_5 = passed),
               if (is.na(passed)) "ph_h2o" else character(0), "VI I, p. 42")
}

# Terrace Brown Forest soils: from Pleistocene deposits, by the words
# Pleistocene, diluvial, terrace deposit (and the Japanese words) in the
# layer_origin of the layers to 1 m or the site parent material; nothing
# recorded: not (Hugo, 2026-11-01).
.SCSJ_PLEISTOCENE_RE <- "pleistocene|diluvi|terrace deposit|terrace gravel|terrace sediment|\u6d2a\u7a4d|\u6bb5\u4e18|\u66f4\u65b0\u4e16"
terrace_cscj <- function(pedon) {
  x <- .scsj_profile(.cscj_marked(pedon))
  soil <- !is.na(x$dtop) & x$dtop < 100
  txt <- c(.scsj_txt(x$h, "layer_origin")[soil], tolower(as.character(unlist(x$site$parent_material %||% character(0)))))
  txt <- txt[!is.na(txt)]
  passed <- any(grepl(.SCSJ_PLEISTOCENE_RE, txt, perl = TRUE))
  .cscj_result("terrace_cscj", passed, integer(0), list(pleistocene_deposits = passed), character(0), "VI I, p. 43")
}
