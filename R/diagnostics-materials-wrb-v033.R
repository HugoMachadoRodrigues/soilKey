# ============================================================================
# v0.3.3 -- WRB 2022 Ch 3.3 diagnostic materials not previously
# implemented:
#   aeolic_material, artefacts, calcaric_material, claric_material,
#   dolomitic_material, gypsiric_material, hypersulfidic_material,
#   hyposulfidic_material, limnic_material, mineral_material,
#   mulmic_material, organic_material, organotechnic_material,
#   ornithogenic_material, soil_organic_carbon, solimovic_material,
#   technic_hard_material, tephric_material.
# ============================================================================


#' Aeolic material (WRB 2022 Ch 3.3.1)
#'
#' Wind-deposited material in the upper 20 cm: rounded matt-surfaced sand
#' grains OR aeroturbation features, AND < 1\\% SOC in the upper 10 cm.
#' v0.3.3 detects via \code{rock_origin == "aeolian"} OR
#' \code{layer_origin == "aeolic"}.
#' @export
aeolic_material <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  tests$origin <- test_pattern_match(h, "rock_origin", "aeolian|aeolic")
  alt <- test_pattern_match(h, "layer_origin", "aeolic")
  if (!isTRUE(tests$origin$passed) && isTRUE(alt$passed)) {
    tests$origin <- alt
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "aeolic_material",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.1"
  )
}


#' Artefacts (WRB 2022 Ch 3.3.2)
#'
#' Per the canonical definition: human-made / human-altered / human-
#' excavated material. v0.3.3 returns the layers where
#' \code{artefacts_pct >= 1}.
#' @export
artefacts <- function(pedon, min_pct = 1) {
  h <- pedon$horizons
  tests <- list()
  tests$artefacts <- test_numeric_above(h, "artefacts_pct",
                                            threshold = min_pct)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "artefacts",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.2"
  )
}


#' Calcaric material (WRB 2022 Ch 3.3.3): \\>= 2\\% CaCO3 throughout the
#' fine earth, primary carbonates from the parent material.
#' @export
calcaric_material <- function(pedon, min_caco3_pct = 2) {
  h <- pedon$horizons
  tests <- list()
  tests$caco3 <- test_caco3_concentration(h, min_pct = min_caco3_pct)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "calcaric_material",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.3",
    notes = "v0.3.3: HCl effervescence proxy via caco3_pct"
  )
}


#' Claric material (WRB 2022 Ch 3.3.4): light-coloured fine earth with
#' Munsell criteria.
#' @export
claric_material <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  tests$munsell <- test_claric_munsell(h)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "claric_material",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.4"
  )
}


#' Dolomitic material (WRB 2022 Ch 3.3.5): \\>= 2\\% Mg-rich carbonate,
#' CaCO3/MgCO3 < 1.5. v0.3.3: detects via designation pattern
#' \code{kdo|do|magn} as proxy when ratio data missing.
#' @export
dolomitic_material <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  tests$caco3 <- test_caco3_concentration(h, min_pct = 2)
  tests$proxy <- test_pattern_match(h, "designation", "kdo|do$|magn")
  if (!isTRUE(tests$proxy$passed)) {
    # When designation absent but caco3 present, treat as ambiguous
    tests$proxy$passed <- NA
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "dolomitic_material",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.5",
    notes = "v0.3.3: HCl-Mg ratio test deferred (no schema column)"
  )
}


#' Gypsiric material (WRB 2022 Ch 3.3.7): \\>= 5\\% gypsum that is
#' primary (not secondary). Without a "secondary fraction" schema column,
#' v0.3.3 treats any layer with caso4_pct >= 5 as gypsiric unless it
#' explicitly carries gypsic-horizon designation.
#' @export
gypsiric_material <- function(pedon, min_caso4_pct = 5) {
  h <- pedon$horizons
  tests <- list()
  tests$caso4 <- test_caso4_concentration(h, min_pct = min_caso4_pct)
  is_gypsic <- gypsic(pedon)
  if (isTRUE(is_gypsic$passed)) {
    not_gypsic_layers <- setdiff(tests$caso4$layers, is_gypsic$layers)
    tests$caso4$layers <- not_gypsic_layers
    if (length(not_gypsic_layers) == 0L) tests$caso4$passed <- FALSE
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "gypsiric_material",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.7"
  )
}


#' Hypersulfidic material (WRB 2022 Ch 3.3.8): \\>= 0.01\\% inorganic
#' sulfidic S, pH \\>= 4, capable of severe acidification on aerobic
#' incubation.
#' @export
hypersulfidic_material <- function(pedon, min_s_pct = 0.01,
                                      min_pH = 4) {
  h <- pedon$horizons
  tests <- list()
  tests$sulfidic <- test_numeric_above(h, "sulfidic_s_pct",
                                          threshold = min_s_pct)
  tests$pH <- list()
  for (i in tests$sulfidic$layers) {
    if (is.na(h$ph_h2o[i])) next
  }
  ph_pass <- which(!is.na(h$ph_h2o) & h$ph_h2o >= min_pH)
  tests$pH <- .subtest_result(
    passed = if (length(ph_pass) > 0L) TRUE
             else if (all(is.na(h$ph_h2o))) NA
             else FALSE,
    layers = ph_pass,
    missing = if (any(is.na(h$ph_h2o))) "ph_h2o" else character(0),
    details = list()
  )
  shared <- intersect(tests$sulfidic$layers, tests$pH$layers)
  passed <- if (length(shared) > 0L) TRUE
            else if (is.na(tests$sulfidic$passed) ||
                     is.na(tests$pH$passed)) NA
            else FALSE
  DiagnosticResult$new(
    name = "hypersulfidic_material",
    passed = passed, layers = shared,
    evidence = tests,
    missing = unique(c(tests$sulfidic$missing, tests$pH$missing)),
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.8",
    notes = "v0.3.3: 8-week incubation acidification test deferred"
  )
}


#' Hyposulfidic material (WRB 2022 Ch 3.3.9): same S and pH as
#' hypersulfidic but does NOT consist of hypersulfidic (i.e. not capable
#' of severe acidification). v0.3.3: returns sulfidic layers that don't
#' meet hypersulfidic.
#' @export
hyposulfidic_material <- function(pedon, min_s_pct = 0.01,
                                     min_pH = 4) {
  h <- pedon$horizons
  hyper <- hypersulfidic_material(pedon, min_s_pct = min_s_pct,
                                     min_pH = min_pH)
  has_s <- which(!is.na(h$sulfidic_s_pct) & h$sulfidic_s_pct >= min_s_pct)
  ok_pH <- which(!is.na(h$ph_h2o) & h$ph_h2o >= min_pH)
  candidates <- intersect(has_s, ok_pH)
  hypo_layers <- setdiff(candidates, hyper$layers %||% integer(0))
  passed <- if (length(hypo_layers) > 0L) TRUE
            else if (length(candidates) == 0L &&
                     any(is.na(h$sulfidic_s_pct))) NA
            else FALSE
  DiagnosticResult$new(
    name = "hyposulfidic_material",
    passed = passed, layers = hypo_layers,
    evidence = list(hypersulfidic = hyper),
    missing = if (any(is.na(h$sulfidic_s_pct))) "sulfidic_s_pct"
              else character(0),
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.9"
  )
}


#' Limnic material (WRB 2022 Ch 3.3.10): subaquatic deposits (coprogenous
#' earth, diatomaceous earth, marl, gyttja). v0.3.3: detects via
#' \code{rock_origin %in% c("lacustrine", "marine")} or designation pattern.
#' @export
limnic_material <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  tests$origin <- test_pattern_match(h, "rock_origin",
                                         "lacustrine|marine|limnic")
  if (!isTRUE(tests$origin$passed)) {
    proxy <- test_pattern_match(h, "designation", "limn|marl|gyttja|copr")
    if (isTRUE(proxy$passed)) tests$origin <- proxy
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "limnic_material",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.10"
  )
}


#' Mineral material (WRB 2022 Ch 3.3.11): < 20\\% SOC AND < 35\\% volume
#' artefacts containing >= 20\\% organic carbon. The complement of
#' organic_material / organotechnic_material.
#' @export
mineral_material <- function(pedon, max_oc = 20, max_organotechnic = 35) {
  h <- pedon$horizons
  tests <- list()
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    oc <- h$oc_pct[i]
    art_org <- h$artefacts_industrial_pct[i]
    if (is.na(oc)) { missing <- c(missing, "oc_pct"); next }
    layer_pass <- oc < max_oc &&
                    (is.na(art_org) || art_org < max_organotechnic)
    details[[as.character(i)]] <- list(idx = i, oc_pct = oc,
                                        artefacts_industrial_pct = art_org,
                                        passed = layer_pass)
    if (layer_pass) passing <- c(passing, i)
  }
  tests$mineral <- .subtest_result(
    passed = if (length(passing) > 0L) TRUE
             else if (length(details) == 0L && length(missing) > 0L) NA
             else FALSE,
    layers = passing, missing = missing, details = details
  )
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "mineral_material",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.11"
  )
}


#' Mulmic material (WRB 2022 Ch 3.3.12): mineral material developed from
#' organic material; \\>= 8\\% SOC, with low BD, structural / chroma
#' criteria.
#' @export
mulmic_material <- function(pedon, min_oc = 8, max_chroma = 2) {
  h <- pedon$horizons
  tests <- list()
  tests$oc      <- test_oc_above(h, min_pct = min_oc)
  tests$chroma  <- test_numeric_above(h, "munsell_chroma_moist",
                                          threshold = -Inf,
                                          candidate_layers = tests$oc$layers)
  # Replace test_numeric_above with chroma <= max_chroma logic.
  ok_chroma <- which(!is.na(h$munsell_chroma_moist) &
                     h$munsell_chroma_moist <= max_chroma)
  tests$chroma <- .subtest_result(
    passed = if (length(intersect(tests$oc$layers, ok_chroma)) > 0L) TRUE
             else if (all(is.na(h$munsell_chroma_moist))) NA
             else FALSE,
    layers = intersect(tests$oc$layers, ok_chroma),
    missing = if (any(is.na(h$munsell_chroma_moist)))
                 "munsell_chroma_moist" else character(0),
    details = list()
  )
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "mulmic_material",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.12"
  )
}


#' Organic material (WRB 2022 Ch 3.3.13): \\>= 20\\% SOC + recognisability
#' criteria. v0.3.3: SOC threshold only.
#' @export
organic_material <- function(pedon, min_oc = 20) {
  h <- pedon$horizons
  tests <- list()
  tests$oc <- test_oc_above(h, min_pct = min_oc)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "organic_material",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.13"
  )
}


#' Organotechnic material (WRB 2022 Ch 3.3.14): \\>= 35\\% volume of
#' artefacts that themselves contain \\>= 20\\% organic C. Soil itself
#' has < 20\\% SOC.
#' @export
organotechnic_material <- function(pedon, min_artefacts = 35,
                                     max_oc = 20) {
  h <- pedon$horizons
  tests <- list()
  tests$artefacts <- test_numeric_above(h, "artefacts_industrial_pct",
                                            threshold = min_artefacts)
  ok_oc <- which(!is.na(h$oc_pct) & h$oc_pct < max_oc)
  tests$low_oc <- .subtest_result(
    passed = if (length(ok_oc) > 0L) TRUE else if (all(is.na(h$oc_pct))) NA else FALSE,
    layers = ok_oc,
    missing = if (any(is.na(h$oc_pct))) "oc_pct" else character(0),
    details = list()
  )
  shared <- intersect(tests$artefacts$layers, tests$low_oc$layers)
  passed <- if (length(shared) > 0L) TRUE
            else if (is.na(tests$artefacts$passed) ||
                     is.na(tests$low_oc$passed)) NA
            else FALSE
  DiagnosticResult$new(
    name = "organotechnic_material",
    passed = passed, layers = shared,
    evidence = tests,
    missing = unique(c(tests$artefacts$missing, tests$low_oc$missing)),
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.14"
  )
}


#' Ornithogenic material (WRB 2022 Ch 3.3.15): bird-influenced topsoil.
#' Mehlich-3 P >= 750 mg/kg + designation pattern \code{Aornit|Bornit}.
#' @export
ornithogenic_material <- function(pedon, min_p_mehlich3 = 750) {
  h <- pedon$horizons
  tests <- list()
  tests$p <- test_numeric_above(h, "p_mehlich3_mg_kg",
                                   threshold = min_p_mehlich3)
  tests$designation <- test_pattern_match(h, "designation",
                                              "ornit|bird|guano")
  if (isTRUE(tests$p$passed) || isTRUE(tests$designation$passed)) {
    layers <- union(tests$p$layers, tests$designation$layers)
    passed <- TRUE
  } else if (is.na(tests$p$passed) && is.na(tests$designation$passed)) {
    layers <- integer(0); passed <- NA
  } else {
    layers <- integer(0); passed <- FALSE
  }
  DiagnosticResult$new(
    name = "ornithogenic_material",
    passed = passed, layers = layers,
    evidence = tests,
    missing = unique(c(tests$p$missing, tests$designation$missing)),
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.15"
  )
}


#' Soil organic carbon (WRB 2022 Ch 3.3.16): organic C that does NOT
#' belong to artefacts. v0.3.3: any layer with oc_pct >= 0.1 and
#' artefacts_industrial_pct < 35.
#' @export
soil_organic_carbon <- function(pedon, min_oc = 0.1,
                                   max_artefacts = 35) {
  h <- pedon$horizons
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    oc <- h$oc_pct[i]
    art <- h$artefacts_industrial_pct[i]
    if (is.na(oc)) { missing <- c(missing, "oc_pct"); next }
    layer_pass <- oc >= min_oc &&
                    (is.na(art) || art < max_artefacts)
    details[[as.character(i)]] <- list(idx = i, oc_pct = oc,
                                        artefacts_industrial_pct = art,
                                        passed = layer_pass)
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(details) == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "soil_organic_carbon",
    passed = passed, layers = passing,
    evidence = list(details = details),
    missing = unique(missing),
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.16"
  )
}


#' Solimovic material (WRB 2022 Ch 3.3.17): hetero genous mass-movement
#' material on slopes / footslopes (formerly "colluvic"). v0.3.3: detects
#' via \code{rock_origin == "colluvial"} OR \code{layer_origin ==
#' "solimovic"}.
#' @export
solimovic_material <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  tests$origin <- test_pattern_match(h, "rock_origin",
                                         "colluvial|solimovic|colluvial")
  if (!isTRUE(tests$origin$passed)) {
    alt <- test_pattern_match(h, "layer_origin", "solimovic|colluvial")
    if (isTRUE(alt$passed)) tests$origin <- alt
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "solimovic_material",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.17"
  )
}


#' Technic hard material (WRB 2022 Ch 3.3.18): consolidated human-made
#' material (asphalt, concrete, worked stones).
#' @export
technic_hard_material <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  tests$designation <- test_pattern_match(h, "designation",
                                              "Cgeo|Cgem|asph|concrete|cement")
  tests$cementation <- test_cemented(h, min_class = "strongly")
  if (isTRUE(tests$designation$passed) || isTRUE(tests$cementation$passed)) {
    layers <- union(tests$designation$layers, tests$cementation$layers)
    passed <- TRUE
  } else if (is.na(tests$designation$passed) && is.na(tests$cementation$passed)) {
    layers <- integer(0); passed <- NA
  } else {
    layers <- integer(0); passed <- FALSE
  }
  DiagnosticResult$new(
    name = "technic_hard_material",
    passed = passed, layers = layers,
    evidence = tests,
    missing = unique(c(tests$designation$missing, tests$cementation$missing)),
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.18"
  )
}


#' Tephric material (WRB 2022 Ch 3.3.19): \\>= 30\\% volcanic glass in
#' 0.02-2 mm fraction AND no andic / vitric properties.
#' @export
tephric_material <- function(pedon, min_glass = 30) {
  h <- pedon$horizons
  tests <- list()
  tests$glass <- test_numeric_above(h, "volcanic_glass_pct",
                                       threshold = min_glass)
  has_andic <- andic_properties(pedon)
  has_vitric <- vitric_properties(pedon)
  exclude <- union(has_andic$layers %||% integer(0),
                   has_vitric$layers %||% integer(0))
  tephric_layers <- setdiff(tests$glass$layers, exclude)
  passed <- if (length(tephric_layers) > 0L) TRUE
            else if (is.na(tests$glass$passed)) NA
            else FALSE
  DiagnosticResult$new(
    name = "tephric_material",
    passed = passed, layers = tephric_layers,
    evidence = list(glass = tests$glass,
                     andic = has_andic, vitric = has_vitric),
    missing = tests$glass$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.3.19"
  )
}
