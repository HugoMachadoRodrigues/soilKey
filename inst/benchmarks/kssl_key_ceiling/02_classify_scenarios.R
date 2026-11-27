# Run the USDA key on the KSSL panel under six information scenarios and
# record, for each pedon, the predicted classes and WHY the order was wrong.
#
#   A  lab only, strict                      (what a property map could feed)
#   B  lab + NASIS morphology, strict
#   C  B + morphological inference from horizon designations
#   D  C + temperature regime from the record (oracle)
#   E  C + moisture regime from the record    (oracle)
#   F  C + both regimes                       (oracle)
#   G/H C + regimes from PRISM climate (Thornthwaite / Hargreaves SMR)
#   S  SOLUS100 properties only + climate regimes (H), no designations
#   U  S + the lab horizon designations (with designation inference)
#   T  S resampled to the SOLUS depth slices (0-5-15-30-60-100-150-200 cm)
#
# "Oracle" regimes are read off the pedologist's own classification, so they
# bound from above what a perfect climate-derived regime could add. They are
# circular for regime-defined classes (e.g. Aridisols) and are reported only
# as an upper bound.
#
# Usage: Rscript 02_classify_scenarios.R <cache_dir> <out_dir> [cores] [scenarios, e.g. S,U,T]

args  <- commandArgs(trailingOnly = TRUE)
cache <- args[1]; out <- args[2]
cores <- if (length(args) >= 3) as.integer(args[3]) else 8L
only  <- if (length(args) >= 4) strsplit(args[4], ",")[[1]] else NULL
dir.create(out, showWarnings = FALSE, recursive = TRUE)

SK <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/Packages/soilKey"
suppressMessages(pkgload::load_all(SK, quiet = TRUE))
library(data.table)

pan  <- readRDS(file.path(cache, "kssl_panel.rds"))
meta <- pan$meta

# ---- oracle regimes from the reference classification -----------------------
smr_from_label <- function(order, suborder, gg, moistscl) {
  o <- tolower(order); s <- tolower(suborder); g <- tolower(gg)
  m <- tolower(trimws(moistscl))
  txt <- paste(s, g)
  if (isTRUE(o == "aridisols")) return("aridic")
  if (grepl("aqu", txt))  return("aquic")
  if (grepl("torr", txt)) return("aridic")
  if (grepl("^per|perud", txt)) return("perudic")
  if (grepl("ust", txt))  return("ustic")
  if (grepl("xer", txt))  return("xeric")
  if (grepl("ud", txt))   return("udic")
  if (isTRUE(m %in% c("udic", "ustic", "xeric", "aquic"))) return(m)
  if (isTRUE(grepl("aridic|torric", m))) return("aridic")
  NA_character_
}
str_from_label <- function(tempregime, gg) {
  t <- tolower(trimws(tempregime))
  if (!is.na(t) && nzchar(t)) return(t)
  if (grepl("cry", tolower(gg))) return("cryic")
  NA_character_
}

ORDER_CODE <- c(Gelisols = "GE", Histosols = "HI", Spodosols = "SP",
                Andisols = "AD", Oxisols = "OX", Vertisols = "VE",
                Aridisols = "AS", Ultisols = "UT", Mollisols = "MO",
                Alfisols = "AF", Inceptisols = "IN", Entisols = "EN")
KEY_ORDER <- names(ORDER_CODE)

# PedonRecord is an R6 object: the NASIS enrichment in 01 mutated the "lab"
# list too. Build the lab-only twin explicitly: deep copy, field morphology
# blanked (laboratory designations kept, as the lab gpkg carries them).
MORPH_COLS <- c("munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist",
                "munsell_hue_dry", "munsell_value_dry", "munsell_chroma_dry",
                "structure_grade", "structure_size", "structure_type",
                "clay_films_amount", "clay_films_strength", "slickensides",
                "cracks_width_cm", "cracks_depth_cm")
strip_morph <- function(p) {
  q <- p$clone(deep = TRUE)
  h <- data.table::copy(q$horizons)
  for (cc in intersect(MORPH_COLS, names(h))) h[[cc]] <- h[[cc]][NA_integer_]
  q$horizons <- h
  q$site$nasis_diagnostic_features <- NULL
  q
}

# The soilKey KSSL loader does not read EC, gypsum, ESP or the pH 8.2 CEC /
# acidity (needed for base saturation by sum of cations) although the lab
# database has them; add them by depth match (laboratory data, all scenarios).
GPKG <- file.path(SK, "soil_data/KSSL/ncss_labdata.gpkg")
add_lab_salts <- function(peds) {
  con <- DBI::dbConnect(RSQLite::SQLite(), GPKG); on.exit(DBI::dbDisconnect(con))
  keys <- vapply(peds, function(p) p$site$id, "")
  q <- sprintf("SELECT l.pedon_key, l.hzn_top, l.hzn_bot,
                       c.electrical_conductivity_satx AS ec,
                       c.corrected_gypsum_lt_2_mm AS gyp,
                       c.sum_of_cations_cec_pH_8_2 AS cec82,
                       c.acidity_bacl2_tea_ph_8_2 AS acid82
                  FROM lab_layer l JOIN lab_chemical_properties c USING (layer_key)
                 WHERE l.pedon_key IN (%s)", paste(keys, collapse = ","))
  s <- as.data.table(DBI::dbGetQuery(con, q)); s[, pedon_key := as.character(pedon_key)]
  for (p in peds) {
    h <- data.table::copy(p$horizons); ss <- s[pedon_key == p$site$id]
    if (!nrow(ss)) next
    i <- match(paste(h$top_cm, h$bottom_cm), paste(ss$hzn_top, ss$hzn_bot))
    if (!"ec_dS_m" %in% names(h)) h$ec_dS_m <- NA_real_
    if (!"caso4_pct" %in% names(h)) h$caso4_pct <- NA_real_
    h$ec_dS_m   <- fifelse(is.na(h$ec_dS_m), as.numeric(ss$ec[i]), h$ec_dS_m)
    h$caso4_pct <- fifelse(is.na(h$caso4_pct), as.numeric(ss$gyp[i]), h$caso4_pct)
    for (cc in c("cec_ph82_cmol", "extractable_acidity_ph82_cmol"))
      if (!cc %in% names(h)) h[[cc]] <- NA_real_
    h$cec_ph82_cmol <- fifelse(is.na(h$cec_ph82_cmol), as.numeric(ss$cec82[i]), h$cec_ph82_cmol)
    h$extractable_acidity_ph82_cmol <- fifelse(is.na(h$extractable_acidity_ph82_cmol),
                                               as.numeric(ss$acid82[i]), h$extractable_acidity_ph82_cmol)
    # NH4OAc base saturation is not reported for calcareous layers (treated
    # as 100 % by convention); elsewhere compute it from bases / CEC7.
    if (all(c("bs_pct", "ca_cmol", "mg_cmol", "k_cmol", "na_cmol", "cec_cmol") %in% names(h))) {
      bases <- h$ca_cmol + h$mg_cmol + h$k_cmol + h$na_cmol
      h$bs_pct <- fifelse(is.na(h$bs_pct) & !is.na(bases) & h$cec_cmol > 0,
                          pmin(100, 100 * bases / h$cec_cmol), h$bs_pct)
      if ("caco3_pct" %in% names(h))
        h$bs_pct <- fifelse(is.na(h$bs_pct) & !is.na(h$caco3_pct) & h$caco3_pct > 0,
                            100, h$bs_pct)
    }
    if ("esp_pct" %in% names(h) && all(c("na_cmol", "cec_cmol") %in% names(h)))
      h$esp_pct <- fifelse(is.na(h$esp_pct) & h$cec_cmol > 0,
                           100 * h$na_cmol / h$cec_cmol, h$esp_pct)
    p$horizons <- h
  }
  invisible(peds)
}
add_lab_salts(pan$nasis)
pan$lab <- lapply(pan$nasis, strip_morph)

# ---- SOLUS-only inputs (scenarios S, T, U) -----------------------------------
# SOLUS100 (Nauman et al. 2024) maps, at 0-200 cm: sand / silt / clay, SOC,
# pH 1:1 H2O, CEC7, ECEC, CaCO3, gypsum, EC, SAR, oven-dry bulk density and
# coarse-fragment volume (+ sand fractions and restriction depths, which the
# USDA order key does not read). Here the "map" is perfect: the pedon's own
# lab values for exactly those properties, nothing else (no colour, no base
# saturation, no bases, no oxalate, no designations unless stated).
SOLUS_COLS <- c("clay_pct", "silt_pct", "sand_pct", "oc_pct", "ph_h2o",
                "cec_cmol", "ecec_cmol", "caco3_pct", "caso4_pct", "ec_dS_m",
                "sar", "bulk_density_g_cm3", "coarse_fragments_pct")
SOLUS_SLICES <- c(0, 5, 15, 30, 60, 100, 150, 200)

add_solus_extra <- function(peds) {
  con <- DBI::dbConnect(RSQLite::SQLite(), GPKG); on.exit(DBI::dbDisconnect(con))
  keys <- vapply(peds, function(p) p$site$id, "")
  q <- sprintf("SELECT l.pedon_key, l.hzn_top, l.hzn_bot,
                       c.ecec_base_plus_aluminum AS ecec,
                       c.sodium_absorption_ratio AS sar,
                       f.total_frag_wt_pct_gt_2_mm_ws AS frag
                  FROM lab_layer l
                  LEFT JOIN lab_chemical_properties c USING (layer_key)
                  LEFT JOIN lab_physical_properties f USING (layer_key)
                 WHERE l.pedon_key IN (%s)", paste(keys, collapse = ","))
  s <- as.data.table(DBI::dbGetQuery(con, q)); s[, pedon_key := as.character(pedon_key)]
  s <- unique(s, by = c("pedon_key", "hzn_top", "hzn_bot"))
  lapply(peds, function(p) {
    h <- data.table::copy(p$horizons); ss <- s[pedon_key == p$site$id]
    i <- match(paste(h$top_cm, h$bottom_cm), paste(ss$hzn_top, ss$hzn_bot))
    h$ecec_cmol <- as.numeric(ss$ecec[i])
    h$sar <- as.numeric(ss$sar[i])
    # SOLUS fragvol is % by volume; the lab gives % by weight of the whole
    # soil (an approximation, larger than the volume for the same fragments).
    h$coarse_fragments_pct <- as.numeric(ss$frag[i])
    list(id = p$site$id, h = h)
  })
}

# Keep only SOLUS properties; optionally the lab designations; optionally
# resample to the SOLUS depth slices (depth-weighted means).
to_solus <- function(p, extra, keep_desg = FALSE, slices = FALSE) {
  h <- extra$h
  keep <- c("top_cm", "bottom_cm", if (keep_desg) "designation", SOLUS_COLS)
  h <- h[, intersect(keep, names(h)), with = FALSE]
  if (slices) {
    h <- h[!is.na(top_cm) & !is.na(bottom_cm) & bottom_cm > top_cm]
    zmax <- min(200, max(h$bottom_cm))
    br <- SOLUS_SLICES[SOLUS_SLICES < zmax]; br <- c(br, zmax)
    h <- rbindlist(lapply(seq_len(length(br) - 1L), function(k) {
      a <- br[k]; b <- br[k + 1L]
      w <- pmax(0, pmin(h$bottom_cm, b) - pmax(h$top_cm, a))
      vals <- lapply(SOLUS_COLS, function(cc) {
        v <- h[[cc]]; ok <- w > 0 & !is.na(v)
        if (sum(w[ok]) < 0.5 * (b - a)) NA_real_ else sum(v[ok] * w[ok]) / sum(w[ok])
      })
      names(vals) <- SOLUS_COLS
      as.data.table(c(list(top_cm = a, bottom_cm = b), vals))
    }))
  }
  st <- p$site
  site <- list(id = st$id, lat = st$lat, lon = st$lon, country = st$country,
               reference_usda = st$reference_usda,
               reference_usda_subgroup = st$reference_usda_subgroup,
               reference_usda_grtgroup = st$reference_usda_grtgroup,
               reference_usda_suborder = st$reference_usda_suborder,
               reference_source = st$reference_source)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(h))
}

set_regimes <- function(p, smr, str) {
  if (!is.na(smr)) p$site$soil_moisture_regime <- smr
  if (!is.na(str)) p$site$soil_temperature_regime <- str
  p
}

run_one <- function(p, morph) {
  options(soilKey.morphological_inference = morph)
  r <- tryCatch(classify_usda(p, on_missing = "silent"), error = function(e) e)
  if (inherits(r, "error"))
    return(list(order = NA_character_, suborder = NA, gg = NA, sg = NA, err = conditionMessage(r),
                trace = NULL, prov = NA))
  tr <- r$trace$orders
  list(order    = r$rsg_or_order,
       suborder = r$trace$suborder_assigned$name %||% NA,
       gg       = r$trace$great_group_assigned$name %||% NA,
       sg       = r$name,
       err      = NA,
       trace    = lapply(tr, function(t) list(passed = t$passed, missing = t$missing)),
       prov     = paste(r$provisional_because, collapse = ";"))
}

attribute <- function(ref, res) {
  # Why is the order wrong? First-match key: compare key positions.
  if (is.na(res$order)) return(list(type = "error", detail = res$err))
  if (identical(ref, res$order)) return(list(type = "correct", detail = NA))
  ir <- match(ref, KEY_ORDER); ia <- match(res$order, KEY_ORDER)
  if (is.na(ir)) return(list(type = "ref_unknown", detail = ref))
  if (ia < ir) {
    return(list(type = paste0("preempted_by_", res$order), detail = NA))
  }
  t <- res$trace[[ORDER_CODE[[ref]]]]
  if (is.null(t)) return(list(type = "ref_not_tested", detail = NA))
  if (is.na(t$passed))
    return(list(type = "ref_undecided_missing",
                detail = paste(t$missing, collapse = ";")))
  list(type = "ref_failed_criteria", detail = NA)
}

scen <- list(
  A = list(src = "lab",   morph = FALSE, smr = FALSE, str = FALSE),
  B = list(src = "nasis", morph = FALSE, smr = FALSE, str = FALSE),
  C = list(src = "nasis", morph = TRUE,  smr = FALSE, str = FALSE),
  D = list(src = "nasis", morph = TRUE,  smr = FALSE, str = TRUE),
  E = list(src = "nasis", morph = TRUE,  smr = TRUE,  str = FALSE),
  F = list(src = "nasis", morph = TRUE,  smr = TRUE,  str = TRUE))

ids <- vapply(pan$nasis, function(p) p$site$id, character(1))
mm  <- meta[match(ids, meta$pedon_key)]
smr <- mapply(smr_from_label, mm$samp_taxorder, mm$samp_taxsuborder,
              mm$samp_taxgrtgroup, mm$samp_taxmoistscl)
strr <- mapply(str_from_label, mm$samp_taxtempregime, mm$samp_taxgrtgroup)

# G: regimes from climate alone (03_climate_regimes.R), no label information
f_clim <- file.path(out, "kssl_climate_regimes.csv")
if (file.exists(f_clim)) {
  clim <- fread(f_clim, colClasses = list(character = "pedon_key"))
  clim <- clim[match(ids, clim$pedon_key)]
  scen$G <- list(src = "nasis", morph = TRUE, smr = "clim", str = "clim")
  if ("smr_clim_hg" %in% names(clim))
    scen$H <- list(src = "nasis", morph = TRUE, smr = "clim_hg", str = "clim")
}
if (!is.null(scen$H)) {
  scen$S <- list(src = "solus",       morph = FALSE, smr = "clim_hg", str = "clim")
  scen$U <- list(src = "solus_desg",  morph = TRUE,  smr = "clim_hg", str = "clim")
  scen$T <- list(src = "solus_slice", morph = FALSE, smr = "clim_hg", str = "clim")
}
build_src <- function(name) {
  ex <- add_solus_extra(pan$nasis)
  Map(function(p, e) to_solus(p, e, keep_desg = name == "solus_desg",
                              slices = name == "solus_slice"), pan$nasis, ex)
}
pick_regime <- function(flag, oracle, climv, i) {
  if (isTRUE(flag)) oracle[i] else if (identical(flag, "clim")) climv[i] else
  if (identical(flag, "clim_hg")) clim$smr_clim_hg[i] else NA
}

if (!is.null(only)) scen <- scen[intersect(names(scen), only)]
tag <- if (is.null(only)) "" else paste0("_", paste(names(scen), collapse = ""))
rows <- list(); traces <- list()
for (sn in names(scen)) {
  s <- scen[[sn]]
  f_part <- file.path(out, paste0("kssl_scenarios_part_", sn, ".csv"))
  if (file.exists(f_part)) { message(sn, ": already done, skipped"); next }
  src <- if (!is.null(pan[[s$src]])) pan[[s$src]] else build_src(s$src)
  t0 <- Sys.time()
  res <- parallel::mclapply(seq_along(src), function(i) {
    p <- src[[i]]
    p <- set_regimes(p,
                     pick_regime(s$smr, smr,  if (exists("clim")) clim$smr_clim else NULL, i),
                     pick_regime(s$str, strr, if (exists("clim")) clim$str_clim else NULL, i))
    run_one(p, s$morph)
  }, mc.cores = cores)
  message(sn, ": ", format(Sys.time() - t0))
  ref <- vapply(src, function(p) p$site$reference_usda %||% NA_character_, character(1))
  at  <- Map(attribute, ref, res)
  rows[[sn]] <- data.table(
    scenario = sn, pedon_key = ids,
    ref_order = ref,
    ref_suborder = vapply(src, function(p) p$site$reference_usda_suborder %||% NA_character_, character(1)),
    ref_gg = vapply(src, function(p) p$site$reference_usda_grtgroup %||% NA_character_, character(1)),
    pred_order = vapply(res, `[[`, "", "order"),
    pred_suborder = vapply(res, function(r) as.character(r$suborder)[1], ""),
    pred_gg = vapply(res, function(r) as.character(r$gg)[1], ""),
    pred_sg = vapply(res, function(r) as.character(r$sg)[1], ""),
    provisional = vapply(res, function(r) as.character(r$prov)[1], ""),
    err_type = vapply(at, `[[`, "", "type"),
    err_detail = vapply(at, function(a) as.character(a$detail)[1], ""),
    smr_oracle = smr, str_oracle = strr,
    mlra = mm$mlra_key, state = mm$state_key,
    lat = vapply(src, function(p) p$site$lat, 0),
    lon = vapply(src, function(p) p$site$lon, 0))
  fwrite(rows[[sn]], f_part)   # checkpoint: a stopped run resumes here
  traces[[sn]] <- lapply(res, `[[`, "trace")
}
dt <- rbindlist(rows)
fwrite(dt, file.path(out, paste0("kssl_scenarios", tag, ".csv")))
saveRDS(traces, file.path(cache, paste0("kssl_traces", tag, ".rds")))
message("done: ", nrow(dt), " rows")
