# ============================================================================
# WRB 2022 (4th ed.) -- Qualifiers Bloco A (v0.9.1).
#
# Adds the principal qualifiers required to fully wire the canonical
# Ch 4 lists for the first 5 RSGs of the key:
#
#   HS  Histosols    AT  Anthrosols   TC  Technosols
#   CR  Cryosols     LP  Leptosols
#
# Every function returns a DiagnosticResult, follows the .q_presence
# / DiagnosticResult$new contract, and is restricted to the canonical
# reference depth defined in WRB 2022 Ch 5.
#
# Where the WRB criterion needs a schema column not yet present in
# horizon_column_spec(), v0.9.1 uses the same proxy idiom already used
# by the v0.3.3 material diagnostics (e.g. limnic_material, solimovic_
# material): a designation- or site-pattern fallback, with an explicit
# `notes` annotation so the trace makes the proxy visible. Hard
# diagnostics (independent schema columns + lab-grade tests) are
# scheduled for v0.9.2 alongside the supplementary qualifiers.
# ============================================================================


# ---------- MATERIAL-BASED PRINCIPAL QUALIFIERS -----------------------------

#' Calcaric qualifier (cl): calcaric material >= 25 cm in upper 100 cm.
#' @export
qual_calcaric <- function(pedon) .q_presence("Calcaric", calcaric_material(pedon), 100, pedon)

#' Dolomitic qualifier (do): dolomitic material in upper 100 cm.
#' @export
qual_dolomitic <- function(pedon) .q_presence("Dolomitic", dolomitic_material(pedon), 100, pedon)

#' Gypsiric qualifier (gc): gypsiric material >= 25 cm in upper 100 cm.
#' @export
qual_gypsiric <- function(pedon) .q_presence("Gypsiric", gypsiric_material(pedon), 100, pedon)

#' Tephric qualifier (tf): tephric material >= 30 cm in upper 100 cm.
#' @export
qual_tephric <- function(pedon) .q_presence("Tephric", tephric_material(pedon), 100, pedon)

#' Limnic qualifier (lm): limnic material (lacustrine / marine subaquatic
#' deposits) anywhere in the profile.
#' @export
qual_limnic <- function(pedon) {
  lm <- limnic_material(pedon)
  DiagnosticResult$new(
    name = "Limnic", passed = isTRUE(lm$passed),
    layers = if (isTRUE(lm$passed)) lm$layers else integer(0),
    evidence = list(limnic = lm),
    missing = lm$missing %||% character(0),
    reference = "WRB (2022) Ch 5, Limnic"
  )
}

#' Solimovic qualifier (sv): solimovic material (mass-movement deposits).
#' @export
qual_solimovic <- function(pedon) {
  sv <- solimovic_material(pedon)
  DiagnosticResult$new(
    name = "Solimovic", passed = isTRUE(sv$passed),
    layers = if (isTRUE(sv$passed)) sv$layers else integer(0),
    evidence = list(solimovic = sv),
    missing = sv$missing %||% character(0),
    reference = "WRB (2022) Ch 5, Solimovic"
  )
}

#' Ornithic qualifier (oc): ornithogenic material (bird-influenced topsoil)
#' in the upper 50 cm.
#' @export
qual_ornithic <- function(pedon) .q_presence("Ornithic", ornithogenic_material(pedon), 50, pedon)

#' Sulfidic qualifier (sf): hyper- OR hyposulfidic material in upper 100 cm
#' (the WRB Sulfidic qualifier covers either acidification class).
#' @export
qual_sulfidic <- function(pedon) {
  hyper <- hypersulfidic_material(pedon)
  hypo  <- hyposulfidic_material(pedon)
  layers <- union(hyper$layers %||% integer(0), hypo$layers %||% integer(0))
  in_upper <- intersect(layers, .in_upper(pedon, 100))
  passed <- (isTRUE(hyper$passed) || isTRUE(hypo$passed)) && length(in_upper) > 0L
  DiagnosticResult$new(
    name = "Sulfidic", passed = passed,
    layers = if (passed) in_upper else integer(0),
    evidence = list(hypersulfidic = hyper, hyposulfidic = hypo),
    missing = unique(c(hyper$missing, hypo$missing)),
    reference = "WRB (2022) Ch 5, Sulfidic"
  )
}

#' Mulmic qualifier (ml): mulmic material in upper 100 cm.
#' @export
qual_mulmic <- function(pedon) .q_presence("Mulmic", mulmic_material(pedon), 100, pedon)


# ---------- ANTHROPOGENIC-HORIZON PRINCIPAL QUALIFIERS ----------------------

#' Hortic qualifier (ht): hortic horizon (long-cultivated dark surface).
#' @export
qual_hortic     <- function(pedon) .q_presence("Hortic",     hortic(pedon),     50, pedon)

#' Irragric qualifier (ir): irragric horizon (irrigation-deposited surface).
#' @export
qual_irragric   <- function(pedon) .q_presence("Irragric",   irragric(pedon),   50, pedon)

#' Plaggic qualifier (pa): plaggic horizon (sod-amended surface).
#'
#' v0.9.2.C: thin wrapper around the v0.3.3 \code{\link{plaggic}}
#' diagnostic now that the anthropic-evidence gate (P / artefacts /
#' Apl-family designation) lives inside the diagnostic itself. The
#' v0.9.1 qualifier-side gate is therefore retired.
#' @export
qual_plaggic <- function(pedon) .q_presence("Plaggic", plaggic(pedon), 50, pedon)

#' Pretic qualifier (pt): pretic (pre-Columbian Amerindian dark earth) horizon.
#' @export
qual_pretic     <- function(pedon) .q_presence("Pretic",     pretic(pedon),     100, pedon)

#' Terric qualifier (te): terric horizon (anthropogenic added mineral
#' material on top of cultivated land).
#' @export
qual_terric     <- function(pedon) .q_presence("Terric",     terric(pedon),     50, pedon)

#' Hydragric qualifier (hg): hydragric horizon (puddled-rice subsurface).
#' @export
qual_hydragric  <- function(pedon) .q_presence("Hydragric",  hydragric(pedon),  100, pedon)

#' Anthraquic qualifier (aq): anthraquic horizon (puddled-rice surface).
#' @export
qual_anthraquic <- function(pedon) .q_presence("Anthraquic", anthraquic(pedon), 50, pedon)


# ---------- TECHNIC-FAMILY PRINCIPAL QUALIFIERS -----------------------------
# WRB 2022 Ch 5: Technic ("artefacts >= 20% within 100 cm OR continuous
# geomembrane within 100 cm OR technic hard material >= 95% within 5 cm").
# Hyperartefactic / Urbic / Spolic / Garbic / Ekranic / Linic are
# semantic refinements: they check the artefact subtype.
# Only `artefacts_pct` and `artefacts_urbic_pct` are first-class schema
# columns today; the rest fall back to a designation-pattern proxy.

#' Technic qualifier (tc): >= 20% artefacts in upper 100 cm OR equivalent
#' geomembrane / technic-hard cover.
#' @export
qual_technic <- function(pedon) {
  tf <- technic_features(pedon)
  th <- technic_hard_material(pedon)
  in_upper <- function(d) intersect(d$layers %||% integer(0), .in_upper(pedon, 100))
  layers <- union(in_upper(tf), in_upper(th))
  passed <- (isTRUE(tf$passed) || isTRUE(th$passed)) && length(layers) > 0L
  DiagnosticResult$new(
    name = "Technic", passed = passed,
    layers = if (passed) layers else integer(0),
    evidence = list(artefacts = tf, hard = th),
    missing = unique(c(tf$missing, th$missing)),
    reference = "WRB (2022) Ch 5, Technic"
  )
}

#' Hyperartefactic qualifier (yr): >= 80% artefacts (any type) in the
#' upper 100 cm.
#' @export
qual_hyperartefactic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Hyperartefactic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "artefacts_pct",
            reference = "WRB (2022) Ch 5, Hyperartefactic"))
  art <- h$artefacts_pct[layers]
  passed <- any(!is.na(art) & art >= 80)
  DiagnosticResult$new(
    name = "Hyperartefactic", passed = passed,
    layers = layers[which(!is.na(art) & art >= 80)],
    evidence = list(artefacts_pct = art),
    missing = if (any(is.na(art))) "artefacts_pct" else character(0),
    reference = "WRB (2022) Ch 5, Hyperartefactic"
  )
}

#' Urbic qualifier (ub): >= 20% urbic artefacts (rubble, refuse) in the
#' upper 100 cm.
#' @export
qual_urbic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Urbic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "artefacts_urbic_pct",
            reference = "WRB (2022) Ch 5, Urbic"))
  ub <- h$artefacts_urbic_pct[layers]
  proxy <- grepl("^C?u(rb)?|urbic|rubble", h$designation[layers], ignore.case = TRUE)
  proxy[is.na(proxy)] <- FALSE
  ok <- (!is.na(ub) & ub >= 20) | proxy
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Urbic", passed = passed,
    layers = layers[ok],
    evidence = list(artefacts_urbic_pct = ub, designation_proxy = proxy),
    missing = if (all(is.na(ub))) "artefacts_urbic_pct" else character(0),
    reference = "WRB (2022) Ch 5, Urbic",
    notes = "v0.9.1: designation-pattern fallback when artefacts_urbic_pct missing"
  )
}

#' Spolic qualifier (sp): >= 20% mineral spoil artefacts (mining /
#' industrial-process slag) in the upper 100 cm. v0.9.1 proxy: designation
#' pattern (\code{Cspol|spoil|slag|mine}) or \code{rock_origin == "spoil"}.
#' Hard schema column \code{artefacts_spolic_pct} scheduled for v0.9.2.
#' @export
qual_spolic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Spolic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = character(0),
            reference = "WRB (2022) Ch 5, Spolic"))
  d <- h$designation[layers]
  ro <- h$rock_origin[layers]
  proxy <- (grepl("Cspol|spoil|slag|^mine|spolic", d, ignore.case = TRUE) |
              grepl("spoil|tailing|slag", ro, ignore.case = TRUE))
  proxy[is.na(proxy)] <- FALSE
  passed <- any(proxy)
  DiagnosticResult$new(
    name = "Spolic", passed = passed,
    layers = layers[proxy],
    evidence = list(designation = d, rock_origin = ro),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Spolic",
    notes = "v0.9.1: designation/rock_origin proxy; artefacts_spolic_pct in v0.9.2"
  )
}

#' Garbic qualifier (ga): >= 20% organic-waste artefacts (landfill
#' refuse) in the upper 100 cm. v0.9.1 proxy: designation pattern
#' (\code{Cgarb|garb|landfill|refuse}). Hard schema column
#' \code{artefacts_garbic_pct} scheduled for v0.9.2.
#' @export
qual_garbic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Garbic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = character(0),
            reference = "WRB (2022) Ch 5, Garbic"))
  d <- h$designation[layers]
  proxy <- grepl("Cgarb|garb|landfill|refuse|waste", d, ignore.case = TRUE)
  proxy[is.na(proxy)] <- FALSE
  passed <- any(proxy)
  DiagnosticResult$new(
    name = "Garbic", passed = passed,
    layers = layers[proxy],
    evidence = list(designation = d),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Garbic",
    notes = "v0.9.1: designation proxy; artefacts_garbic_pct in v0.9.2"
  )
}

#' Ekranic qualifier (ek): impervious cover (asphalt, concrete) starting
#' within 5 cm of the surface. v0.9.1: technic_hard_material with top
#' depth <= 5 cm.
#' @export
qual_ekranic <- function(pedon) {
  th <- technic_hard_material(pedon)
  if (!isTRUE(th$passed))
    return(DiagnosticResult$new(name = "Ekranic", passed = FALSE,
            layers = integer(0), evidence = list(hard = th),
            missing = th$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Ekranic"))
  h <- pedon$horizons
  shallow <- intersect(th$layers, which(!is.na(h$top_cm) & h$top_cm <= 5))
  passed <- length(shallow) > 0L
  DiagnosticResult$new(
    name = "Ekranic", passed = passed,
    layers = if (passed) shallow else integer(0),
    evidence = list(hard = th),
    missing = th$missing %||% character(0),
    reference = "WRB (2022) Ch 5, Ekranic"
  )
}

#' Linic qualifier (li): continuous artificial geomembrane within 100 cm.
#' v0.9.1 proxy: designation pattern (\code{linic|geomemb|liner}).
#' @export
qual_linic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Linic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = character(0),
            reference = "WRB (2022) Ch 5, Linic"))
  d <- h$designation[layers]
  proxy <- grepl("linic|geomemb|liner|membr", d, ignore.case = TRUE)
  proxy[is.na(proxy)] <- FALSE
  passed <- any(proxy)
  DiagnosticResult$new(
    name = "Linic", passed = passed,
    layers = layers[proxy],
    evidence = list(designation = d),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Linic",
    notes = "v0.9.1: designation proxy; geomembrane flag column in v0.9.2"
  )
}


# ---------- ARIDIC PRINCIPAL QUALIFIERS -------------------------------------

#' Yermic qualifier (ye): yermic properties in upper 50 cm.
#' @export
qual_yermic <- function(pedon) .q_presence("Yermic", yermic_properties(pedon), 50, pedon)

#' Takyric qualifier (ty): takyric properties in upper 50 cm.
#' @export
qual_takyric <- function(pedon) .q_presence("Takyric", takyric_properties(pedon), 50, pedon)


# ---------- COLD / CRYO PRINCIPAL QUALIFIERS -------------------------------

#' Glacic qualifier (gc): >= 75% ice by volume within 100 cm. v0.9.1
#' proxy: cryic conditions + designation pattern (\code{ice|gel|glac}).
#' Schema column \code{ice_pct} scheduled for v0.9.2.
#' @export
qual_glacic <- function(pedon) {
  cy <- cryic_conditions(pedon)
  if (!isTRUE(cy$passed))
    return(DiagnosticResult$new(name = "Glacic", passed = FALSE,
            layers = integer(0), evidence = list(cryic = cy),
            missing = cy$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Glacic"))
  h <- pedon$horizons
  layers <- intersect(cy$layers, .in_upper(pedon, 100))
  d <- h$designation[layers]
  proxy <- grepl("ice|^gel|glac|^Wf$", d, ignore.case = TRUE)
  proxy[is.na(proxy)] <- FALSE
  passed <- any(proxy)
  DiagnosticResult$new(
    name = "Glacic", passed = passed,
    layers = layers[proxy],
    evidence = list(cryic = cy, designation = d),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Glacic",
    notes = "v0.9.1: cryic + designation proxy; ice_pct column in v0.9.2"
  )
}

#' Turbic qualifier (tb): cryoturbation features within 100 cm. v0.9.1
#' proxy: cryic conditions + designation pattern (\code{turb|jj|cryot})
#' OR slickensides "common"/"many" in a cryic profile.
#' @export
qual_turbic <- function(pedon) {
  cy <- cryic_conditions(pedon)
  if (!isTRUE(cy$passed))
    return(DiagnosticResult$new(name = "Turbic", passed = FALSE,
            layers = integer(0), evidence = list(cryic = cy),
            missing = cy$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Turbic"))
  h <- pedon$horizons
  layers <- intersect(cy$layers, .in_upper(pedon, 100))
  d <- h$designation[layers]
  ss <- h$slickensides[layers]
  proxy <- grepl("turb|jj|cryot|@@", d, ignore.case = TRUE) |
            (!is.na(ss) & ss %in% c("common", "many", "continuous"))
  proxy[is.na(proxy)] <- FALSE
  passed <- any(proxy)
  DiagnosticResult$new(
    name = "Turbic", passed = passed,
    layers = layers[proxy],
    evidence = list(cryic = cy, designation = d, slickensides = ss),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Turbic",
    notes = "v0.9.1: cryic + designation/slickensides proxy"
  )
}


# ---------- LEPTIC-FAMILY FINE-GRAINED PRINCIPAL QUALIFIERS -----------------

#' Lithic qualifier (lt): continuous rock starting within 10 cm. Tighter
#' depth gate than Leptic (which is <= 100 cm) and Nudilithic (== 0 cm).
#' @export
qual_lithic <- function(pedon) {
  rk <- continuous_rock(pedon)
  if (!isTRUE(rk$passed))
    return(DiagnosticResult$new(name = "Lithic", passed = FALSE,
            layers = integer(0), evidence = list(rock = rk),
            missing = rk$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Lithic"))
  h <- pedon$horizons
  shallow <- intersect(rk$layers, which(!is.na(h$top_cm) & h$top_cm <= 10))
  passed <- length(shallow) > 0L
  DiagnosticResult$new(
    name = "Lithic", passed = passed,
    layers = if (passed) shallow else integer(0),
    evidence = list(rock = rk),
    missing = rk$missing %||% character(0),
    reference = "WRB (2022) Ch 5, Lithic"
  )
}

#' Nudilithic qualifier (nt): continuous rock at the soil surface (top_cm == 0).
#' @export
qual_nudilithic <- function(pedon) {
  rk <- continuous_rock(pedon)
  if (!isTRUE(rk$passed))
    return(DiagnosticResult$new(name = "Nudilithic", passed = FALSE,
            layers = integer(0), evidence = list(rock = rk),
            missing = rk$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Nudilithic"))
  h <- pedon$horizons
  surface <- intersect(rk$layers, which(!is.na(h$top_cm) & h$top_cm <= 0))
  passed <- length(surface) > 0L
  DiagnosticResult$new(
    name = "Nudilithic", passed = passed,
    layers = if (passed) surface else integer(0),
    evidence = list(rock = rk),
    missing = rk$missing %||% character(0),
    reference = "WRB (2022) Ch 5, Nudilithic"
  )
}

#' Hyperskeletic qualifier (hk): coarse fragments >= 90% throughout the
#' upper 100 cm.
#' @export
qual_hyperskeletic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Hyperskeletic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "coarse_fragments_pct",
            reference = "WRB (2022) Ch 5, Hyperskeletic"))
  cf <- h$coarse_fragments_pct[layers]
  passed <- length(cf) > 0L && all(!is.na(cf) & cf >= 90)
  DiagnosticResult$new(
    name = "Hyperskeletic", passed = passed,
    layers = if (passed) layers else integer(0),
    evidence = list(coarse_fragments = cf),
    missing = if (any(is.na(cf))) "coarse_fragments_pct" else character(0),
    reference = "WRB (2022) Ch 5, Hyperskeletic"
  )
}

#' Rendzic qualifier (rz): mollic horizon directly over calcaric material
#' (or limestone), shallow. Defined as Mollic + (Calcaric OR continuous
#' rock with carbonate parent material).
#' @export
qual_rendzic <- function(pedon) {
  mo <- mollic(pedon)
  if (!isTRUE(mo$passed))
    return(DiagnosticResult$new(name = "Rendzic", passed = FALSE,
            layers = integer(0), evidence = list(mollic = mo),
            missing = mo$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Rendzic"))
  ca <- calcaric_material(pedon)
  parent <- pedon$site$parent_material %||% ""
  carb_parent <- grepl("limestone|chalk|dolomit|carbonate|marl",
                       parent, ignore.case = TRUE)
  passed <- isTRUE(ca$passed) || carb_parent
  DiagnosticResult$new(
    name = "Rendzic", passed = passed,
    layers = if (passed) mo$layers else integer(0),
    evidence = list(mollic = mo, calcaric = ca, parent_material = parent),
    missing = c(mo$missing, ca$missing) %||% character(0),
    reference = "WRB (2022) Ch 5, Rendzic"
  )
}

#' Vermic qualifier (vm): >= 50% bioturbation by worm casts / krotovinas
#' in the upper 100 cm. v0.9.1: \code{worm_holes_pct >= 50}.
#' @export
qual_vermic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Vermic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "worm_holes_pct",
            reference = "WRB (2022) Ch 5, Vermic"))
  wp <- h$worm_holes_pct[layers]
  passed <- any(!is.na(wp) & wp >= 50)
  DiagnosticResult$new(
    name = "Vermic", passed = passed,
    layers = layers[which(!is.na(wp) & wp >= 50)],
    evidence = list(worm_holes_pct = wp),
    missing = if (all(is.na(wp))) "worm_holes_pct" else character(0),
    reference = "WRB (2022) Ch 5, Vermic"
  )
}


# ---------- PETRIC AND SULFIDIC SPECIALS -----------------------------------

#' Petric qualifier (pt): any petro-cemented horizon (petrocalcic /
#' petroduric / petrogypsic / petroplinthic) within 100 cm.
#' @export
qual_petric <- function(pedon) {
  pc <- petrocalcic(pedon)
  pd <- petroduric(pedon)
  pg <- petrogypsic(pedon)
  pp <- petroplinthic(pedon)
  any_pass <- vapply(list(pc, pd, pg, pp), function(d) isTRUE(d$passed),
                     logical(1))
  layers <- unique(unlist(lapply(list(pc, pd, pg, pp), `[[`, "layers")))
  in_upper <- intersect(layers, .in_upper(pedon, 100))
  passed <- any(any_pass) && length(in_upper) > 0L
  DiagnosticResult$new(
    name = "Petric", passed = passed,
    layers = if (passed) in_upper else integer(0),
    evidence = list(petrocalcic = pc, petroduric = pd,
                    petrogypsic = pg, petroplinthic = pp),
    missing = unique(unlist(lapply(list(pc, pd, pg, pp), `[[`, "missing"))),
    reference = "WRB (2022) Ch 5, Petric"
  )
}

#' Thionic qualifier (tn): thionic horizon within 100 cm.
#' @export
qual_thionic <- function(pedon) .q_presence("Thionic", thionic(pedon), 100, pedon)


# ---------- SITE / MOISTURE / ORGANIC-DECOMPOSITION QUALIFIERS --------------
# Histosols Ch 4 distinguishes Sapric / Hemic / Fibric (decomposition
# state) and Ombric / Rheic (water source). The schema does not yet
# carry a decomposition_class column or a hydrology source field.
# v0.9.1 uses the conventional designation tokens (US Soil Taxonomy
# Histosol convention reused by WRB tables):
#   Oa -> Sapric    Oe -> Hemic    Oi -> Fibric
# and site$drainage_class for Drainic. Floatic / Subaquatic / Tidalic /
# Ombric / Rheic are deferred to v0.9.2 (need first-class flags).

# Internal helper -- picks the thickness-dominant decomposition class
# in the upper 100 cm of organic material. WRB 2022 Ch 5 specifies
# Sapric / Hemic / Fibric as mutually exclusive ("the dominant kind of
# organic material"); a single soil therefore receives at most one of
# the three.
.decomp_class <- function(pedon) {
  om <- organic_material(pedon)
  if (!isTRUE(om$passed)) {
    return(list(class = NA_character_, layers = integer(0),
                organic = om))
  }
  h <- pedon$horizons
  oi <- intersect(om$layers, .in_upper(pedon, 100))
  if (length(oi) == 0L)
    return(list(class = NA_character_, layers = integer(0),
                organic = om))
  d <- h$designation[oi]
  thk <- pmax(0, h$bottom_cm[oi] - h$top_cm[oi])
  classes <- rep(NA_character_, length(oi))
  classes[grepl("^Oa\\b|^Oa[a-z]?$|sapric", d, ignore.case = TRUE)] <- "sapric"
  classes[grepl("^Oe\\b|^Oe[a-z]?$|hemic",  d, ignore.case = TRUE)] <- "hemic"
  classes[grepl("^Oi\\b|^Oi[a-z]?$|fibric", d, ignore.case = TRUE)] <- "fibric"
  if (all(is.na(classes)))
    return(list(class = NA_character_, layers = integer(0),
                organic = om))
  agg <- tapply(thk, classes, sum, na.rm = TRUE)
  max_thk <- max(agg, na.rm = TRUE)
  tied <- names(agg)[!is.na(agg) & agg == max_thk]
  winner <- if (length(tied) == 1L) {
    tied
  } else {
    # Tiebreak: take the class of the topmost organic layer.
    top_class <- classes[which.min(h$top_cm[oi])]
    if (!is.na(top_class) && top_class %in% tied) top_class else tied[1L]
  }
  layers <- oi[!is.na(classes) & classes == winner]
  list(class = winner, layers = layers, organic = om,
       per_class_thickness = agg)
}

.qual_decomp <- function(pedon, want, name) {
  dc <- .decomp_class(pedon)
  passed <- !is.na(dc$class) && dc$class == want
  DiagnosticResult$new(
    name = name, passed = passed,
    layers = if (passed) dc$layers else integer(0),
    evidence = list(organic = dc$organic, dominant_class = dc$class,
                    per_class_thickness = dc$per_class_thickness),
    missing = character(0),
    reference = sprintf("WRB (2022) Ch 5, %s", name),
    notes = "v0.9.1: thickness-dominant designation proxy (Oa/Oe/Oi)"
  )
}

#' Sapric qualifier (sa): organic material whose dominant decomposition
#' class in the upper 100 cm is sapric (rubbed fiber < 1/6).
#' v0.9.1: thickness-weighted dominance via Oa designation.
#' @export
qual_sapric <- function(pedon) .qual_decomp(pedon, "sapric", "Sapric")

#' Hemic qualifier (hc): organic material whose dominant decomposition
#' class in the upper 100 cm is hemic (1/6 - 2/3 fiber).
#' v0.9.1: thickness-weighted dominance via Oe designation.
#' @export
qual_hemic <- function(pedon) .qual_decomp(pedon, "hemic", "Hemic")

#' Fibric qualifier (fi): organic material whose dominant decomposition
#' class in the upper 100 cm is fibric (>= 2/3 fiber).
#' v0.9.1: thickness-weighted dominance via Oi designation.
#' @export
qual_fibric <- function(pedon) .qual_decomp(pedon, "fibric", "Fibric")

#' Drainic qualifier (dr): artificially drained organic soil. v0.9.1:
#' site$drainage_class or site$land_use carries an explicit
#' \emph{artificial} drainage marker AND organic_material passes.
#' Natural drainage classes (e.g. "very poorly drained", "well drained")
#' do NOT trigger Drainic on their own.
#' @export
qual_drainic <- function(pedon) {
  om <- organic_material(pedon)
  if (!isTRUE(om$passed))
    return(DiagnosticResult$new(name = "Drainic", passed = FALSE,
            layers = integer(0), evidence = list(organic = om),
            missing = om$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Drainic"))
  drain <- pedon$site$drainage_class %||% NA_character_
  land  <- pedon$site$land_use %||% NA_character_
  artific_re <- "artific|reclaim|drained\\s+(organic|peat|mire|wetland|fen|bog)|\\bditch\\b|drained\\)"
  drained <- (!is.na(drain) && grepl(artific_re, drain, ignore.case = TRUE)) ||
               (!is.na(land)  && grepl(artific_re, land,  ignore.case = TRUE))
  DiagnosticResult$new(
    name = "Drainic", passed = drained,
    layers = if (drained) om$layers else integer(0),
    evidence = list(organic = om, drainage_class = drain, land_use = land),
    missing = if (is.na(drain) && is.na(land)) "site$drainage_class" else character(0),
    reference = "WRB (2022) Ch 5, Drainic",
    notes = "v0.9.1: site proxy (artificial-drainage marker in drainage_class or land_use)"
  )
}

#' Subaquatic qualifier (sq): permanently under water. v0.9.1:
#' site$drainage_class == "subaquatic" or "submerged".
#' @export
qual_subaquatic <- function(pedon) {
  drain <- pedon$site$drainage_class %||% NA_character_
  passed <- !is.na(drain) &&
              grepl("subaquatic|submerged|underwater", drain, ignore.case = TRUE)
  DiagnosticResult$new(
    name = "Subaquatic", passed = passed,
    layers = if (passed) seq_len(nrow(pedon$horizons)) else integer(0),
    evidence = list(drainage_class = drain),
    missing = if (is.na(drain)) "site$drainage_class" else character(0),
    reference = "WRB (2022) Ch 5, Subaquatic",
    notes = "v0.9.1: site$drainage_class proxy"
  )
}

#' Tidalic qualifier (td): subject to tidal flooding. v0.9.1:
#' site$drainage_class contains "tidal".
#' @export
qual_tidalic <- function(pedon) {
  drain <- pedon$site$drainage_class %||% NA_character_
  land  <- pedon$site$land_use      %||% NA_character_
  passed <- (!is.na(drain) && grepl("tidal", drain, ignore.case = TRUE)) ||
              (!is.na(land) && grepl("tidal|salt marsh|mangrove", land, ignore.case = TRUE))
  DiagnosticResult$new(
    name = "Tidalic", passed = passed,
    layers = if (passed) seq_len(nrow(pedon$horizons)) else integer(0),
    evidence = list(drainage_class = drain, land_use = land),
    missing = if (is.na(drain) && is.na(land)) "site$drainage_class" else character(0),
    reference = "WRB (2022) Ch 5, Tidalic",
    notes = "v0.9.1: site$drainage_class / site$land_use proxy"
  )
}

#' Reductic qualifier (rd): permanently reducing conditions caused by
#' anthropogenic gas / liquid emissions (typical of Technosols on
#' landfills). v0.9.1: reducing_conditions + Technic context.
#' @export
qual_reductic <- function(pedon) {
  rc <- reducing_conditions(pedon)
  tc <- qual_technic(pedon)
  passed <- isTRUE(rc$passed) && isTRUE(tc$passed)
  DiagnosticResult$new(
    name = "Reductic", passed = passed,
    layers = if (passed)
               intersect(rc$layers, tc$layers)
             else integer(0),
    evidence = list(reducing = rc, technic = tc),
    missing = unique(c(rc$missing, tc$missing)),
    reference = "WRB (2022) Ch 5, Reductic"
  )
}


# ---------- ORGANO-TECHNIC QUALIFIER ----------------------------------------

#' Organotechnic qualifier (ot): organotechnic material in upper 100 cm.
#' @export
qual_organotechnic <- function(pedon) {
  ot <- organotechnic_material(pedon)
  passed <- isTRUE(ot$passed) &&
              length(intersect(ot$layers, .in_upper(pedon, 100))) > 0L
  DiagnosticResult$new(
    name = "Organotechnic", passed = passed,
    layers = if (passed) intersect(ot$layers, .in_upper(pedon, 100))
             else integer(0),
    evidence = list(organotechnic = ot),
    missing = ot$missing %||% character(0),
    reference = "WRB (2022) Ch 5, Organotechnic"
  )
}
