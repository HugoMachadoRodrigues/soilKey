# =============================================================================
# Benchmark dataset loaders -- v0.9.15
#
# Three real-data benchmarks that complement the synthetic 31-canonical-
# fixture suite and the WoSIS forensic. Each loader takes a path to the
# raw archive (the user must download separately because of license / size
# constraints) and returns a list of PedonRecord objects with the
# published reference classification attached.
#
# The reference classifications are stored on `pedon$site$reference_*`
# fields. Downstream the benchmark runners compute confusion matrices
# and bootstrap CIs (`benchmark_run_classification`).
#
#   load_kssl_pedons()   -- USDA NCSS / KSSL pedon DB (USDA Soil Taxonomy)
#   load_lucas_pedons()  -- EU-LUCAS topsoil + ESDB profile pairs (WRB)
#   load_embrapa_pedons() -- Embrapa dadosolos (SiBCS, PT-BR)
#
# These loaders SCAFFOLD the integration: each accepts an optional `head`
# argument so users can validate the parser on a small subset first, and
# each emits a consistent metadata block on stderr.
# =============================================================================


#' Load NCSS / KSSL pedons with reference USDA Soil Taxonomy classification
#'
#' Reads the KSSL pedon CSV export (typically named
#' \code{NCSS_Pedon_Layer.csv} or similar) plus the lab-data CSV, joins
#' on \code{pedon_key}, and assembles a list of \code{PedonRecord}
#' objects. The published USDA Soil Taxonomy classification (from the
#' \code{Series} or \code{Subgroup} field) is attached as
#' \code{pedon$site$reference_usda}.
#'
#' KSSL is the de-facto standard for validating USDA Soil Taxonomy keys
#' (~50k profiles, lab-grade analytical data, professional pedon
#' descriptions). Get the export from
#' \url{https://ncsslabdatamart.sc.egov.usda.gov/}.
#'
#' @param pedon_csv Path to the pedon-level CSV (one row per profile,
#'        with site-level metadata + classification).
#' @param layer_csv Path to the layer-level CSV (one row per horizon,
#'        with horizon properties).
#' @param head Optional integer; if not \code{NULL}, returns only the
#'        first \code{head} pedons (useful for parser validation).
#' @param verbose If \code{TRUE} (default), emits a summary of the
#'        load.
#' @return A list of \code{\link{PedonRecord}} objects.
#' @export
load_kssl_pedons <- function(pedon_csv,
                                layer_csv,
                                head    = NULL,
                                verbose = TRUE) {
  if (!file.exists(pedon_csv))
    stop(sprintf("KSSL pedon CSV not found: %s", pedon_csv))
  if (!file.exists(layer_csv))
    stop(sprintf("KSSL layer CSV not found: %s", layer_csv))

  ped <- data.table::fread(pedon_csv)
  lay <- data.table::fread(layer_csv)
  if (!is.null(head)) ped <- utils::head(ped, n = head)

  .kssl_required <- c("pedon_key")
  miss <- setdiff(.kssl_required, names(ped))
  if (length(miss))
    stop("KSSL pedon CSV missing required columns: ",
         paste(miss, collapse = ", "))

  out <- vector("list", nrow(ped))
  for (i in seq_len(nrow(ped))) {
    p <- ped[i, ]
    layers <- lay[lay$pedon_key == p$pedon_key, ]

    hz <- data.table::data.table(
      top_cm        = layers$hzn_top   %||% layers$top_cm,
      bottom_cm     = layers$hzn_bot   %||% layers$bottom_cm,
      designation   = layers$hzn_desgn %||% layers$horizon_designation,
      clay_pct      = layers$clay_pct  %||% layers$clay_total,
      silt_pct      = layers$silt_pct  %||% layers$silt_total,
      sand_pct      = layers$sand_pct  %||% layers$sand_total,
      ph_h2o        = layers$ph_h2o    %||% layers$ph_water,
      oc_pct        = layers$oc_pct    %||% layers$organic_carbon,
      cec_cmol      = layers$cec_nh4   %||% layers$cec,
      bs_pct        = layers$base_sat  %||% layers$bs_pct,
      bulk_density_g_cm3 = layers$bulk_density %||% layers$db_13b
    )
    hz <- ensure_horizon_schema(hz)

    out[[i]] <- PedonRecord$new(
      site = list(
        id              = as.character(p$pedon_key),
        lat             = p$latitude_decimal_degrees %||% p$lat,
        lon             = p$longitude_decimal_degrees %||% p$lon,
        country         = p$country %||% "US",
        parent_material = p$parent_material %||% NA_character_,
        reference_usda  = p$taxonomic_subgroup %||% p$series %||%
                          p$reference_usda %||% NA_character_,
        reference_source = "KSSL"
      ),
      horizons = hz
    )
  }
  if (isTRUE(verbose))
    cli::cli_alert_success("KSSL: loaded {.val {length(out)}} pedons")
  out
}


#' Load EU-LUCAS / ESDB pedons with reference WRB classification
#'
#' Reads the EU-LUCAS topsoil dataset joined with the ESDB profile
#' archive (the v3 release produced by JRC). Assembles a list of
#' \code{PedonRecord} objects with the WRB Reference Soil Group
#' attached as \code{pedon$site$reference_wrb}.
#'
#' LUCAS is harvested every 3-6 years on a regular grid; the ESDB
#' classification is updated synchronously. ~28k profile cells with
#' WRB labels in the 2015-2018 release.
#'
#' @param lucas_csv Path to the LUCAS topsoil CSV.
#' @param head Optional integer for parser validation.
#' @param verbose If \code{TRUE} (default), emits a summary.
#' @return A list of \code{\link{PedonRecord}} objects.
#' @export
load_lucas_pedons <- function(lucas_csv, head = NULL, verbose = TRUE) {
  if (!file.exists(lucas_csv))
    stop(sprintf("LUCAS CSV not found: %s", lucas_csv))

  d <- data.table::fread(lucas_csv)
  if (!is.null(head)) d <- utils::head(d, n = head)

  out <- vector("list", nrow(d))
  for (i in seq_len(nrow(d))) {
    r <- d[i, ]
    # LUCAS reports topsoil only (0-20 cm). Build a single-horizon
    # pedon -- enough for surface-only diagnostics; users wanting
    # deeper data should join with ESDB profile sheets separately.
    hz <- data.table::data.table(
      top_cm    = 0,
      bottom_cm = 20,
      designation = "Ap",
      clay_pct = r$clay     %||% r$clay_pct,
      silt_pct = r$silt     %||% r$silt_pct,
      sand_pct = r$sand     %||% r$sand_pct,
      ph_h2o   = r$pH_H2O   %||% r$ph_h2o,
      oc_pct   = r$OC       %||% r$oc_pct,
      cec_cmol = r$CEC      %||% r$cec_cmol,
      caco3_pct = r$CaCO3   %||% r$caco3_pct
    )
    hz <- ensure_horizon_schema(hz)

    out[[i]] <- PedonRecord$new(
      site = list(
        id              = as.character(r$POINT_ID %||% r$id %||% i),
        lat             = r$TH_LAT  %||% r$lat,
        lon             = r$TH_LONG %||% r$lon,
        country         = r$NUTS_0  %||% r$country %||% NA_character_,
        reference_wrb   = r$WRB     %||% r$wrb_rsg %||%
                          r$reference_wrb %||% NA_character_,
        reference_source = "LUCAS-ESDB"
      ),
      horizons = hz
    )
  }
  if (isTRUE(verbose))
    cli::cli_alert_success("LUCAS: loaded {.val {length(out)}} pedons")
  out
}


#' Load Embrapa dadosolos pedons with reference SiBCS classification
#'
#' Reads the Embrapa BDsolos CSV export (or the dadosolos R package
#' data frame, if present). Assembles a list of \code{PedonRecord}
#' objects with the SiBCS classification attached as
#' \code{pedon$site$reference_sibcs}.
#'
#' The dadosolos / BDsolos archive ships with ~5k profiles in PT-BR
#' with full SiBCS classification, lab data, and horizon morphology --
#' the primary validation set for Brazilian-context use. Available
#' from \url{https://www.bdsolos.cnptia.embrapa.br/}.
#'
#' @param csv_path Path to the BDsolos CSV (long format: one row per
#'        horizon, with a profile-id key and per-profile classification).
#' @param head Optional integer for parser validation.
#' @param verbose If \code{TRUE} (default), emits a summary.
#' @return A list of \code{\link{PedonRecord}} objects.
#' @export
load_embrapa_pedons <- function(csv_path, head = NULL, verbose = TRUE) {
  if (!file.exists(csv_path))
    stop(sprintf("Embrapa BDsolos CSV not found: %s", csv_path))

  d <- data.table::fread(csv_path)
  id_col <- intersect(c("id_perfil", "profile_id", "id"), names(d))[1]
  if (is.na(id_col))
    stop("Embrapa CSV must have one of: id_perfil, profile_id, id")
  ids <- unique(d[[id_col]])
  if (!is.null(head)) ids <- utils::head(ids, n = head)

  out <- vector("list", length(ids))
  for (i in seq_along(ids)) {
    rid <- ids[i]
    layers <- d[d[[id_col]] == rid, ]
    if (nrow(layers) == 0L) next

    hz <- data.table::data.table(
      top_cm      = layers$prof_sup    %||% layers$top_cm    %||% layers$top,
      bottom_cm   = layers$prof_inf    %||% layers$bottom_cm %||% layers$bottom,
      designation = layers$horizonte   %||% layers$designation,
      clay_pct    = layers$argila_pct  %||% layers$clay_pct,
      silt_pct    = layers$silte_pct   %||% layers$silt_pct,
      sand_pct    = layers$areia_pct   %||% layers$sand_pct,
      ph_h2o      = layers$ph_agua     %||% layers$ph_h2o,
      ph_kcl      = layers$ph_kcl,
      oc_pct      = layers$c_org_pct   %||% layers$oc_pct,
      cec_cmol    = layers$ctc_cmol    %||% layers$cec_cmol,
      ca_cmol     = layers$ca_cmol,
      mg_cmol     = layers$mg_cmol,
      k_cmol      = layers$k_cmol,
      al_cmol     = layers$al_cmol,
      bs_pct      = layers$v_pct       %||% layers$bs_pct,
      al_sat_pct  = layers$m_pct       %||% layers$al_sat_pct,
      fe_dcb_pct  = layers$fe_dcb_pct
    )
    hz <- ensure_horizon_schema(hz)

    p <- layers[1, ]
    out[[i]] <- PedonRecord$new(
      site = list(
        id              = as.character(rid),
        lat             = p$latitude  %||% p$lat,
        lon             = p$longitude %||% p$lon,
        country         = "BR",
        parent_material = p$material_origem %||% p$parent_material,
        reference_sibcs = p$classificacao_sibcs %||% p$sibcs %||%
                          p$reference_sibcs %||% NA_character_,
        reference_source = "BDsolos / Embrapa dadosolos"
      ),
      horizons = hz
    )
  }
  out <- out[!vapply(out, is.null, logical(1))]
  if (isTRUE(verbose))
    cli::cli_alert_success("Embrapa BDsolos: loaded {.val {length(out)}} pedons")
  out
}


#' Run a benchmark across one of the loaded pedon lists
#'
#' Classifies each pedon in \code{pedons} against the named system,
#' compares against the published reference (e.g.
#' \code{site$reference_wrb}), and returns a confusion matrix +
#' top-1 / top-3 accuracy + bootstrap CI on top-1.
#'
#' @param pedons List of \code{\link{PedonRecord}} objects (output of
#'        one of the \code{load_*} functions).
#' @param system One of \code{"wrb2022"}, \code{"sibcs"}, \code{"usda"}.
#' @param level Granularity of the comparison:
#'   \itemize{
#'     \item \code{"order"} (default) -- the top-level RSG / Ordem /
#'           Order, compared against \code{cls$rsg_or_order};
#'     \item \code{"subgroup"} -- the full classified name (Subgroup
#'           in USDA, Subgrupo in SiBCS, RSG + qualifiers in WRB),
#'           compared against \code{cls$name} after case-insensitive
#'           token normalisation;
#'     \item \code{"subordem"} -- SiBCS-only, the 2nd-level
#'           "Ordem + Subordem" (e.g. "Latossolos Vermelhos").
#'           Comparison via the first two normalised tokens of the
#'           predicted name vs the reference.
#'   }
#' @param boot_n Bootstrap replicates for CI (default 1000).
#' @return A list with elements \code{accuracy_top1},
#'         \code{accuracy_ci}, \code{confusion}, and
#'         \code{per_pedon} (one row per pedon with predicted vs
#'         reference).
#' @export
benchmark_run_classification <- function(pedons,
                                            system = c("wrb2022",
                                                       "sibcs", "usda"),
                                            level  = c("order", "subgroup",
                                                       "subordem"),
                                            boot_n = 1000L) {
  system <- match.arg(system)
  level  <- match.arg(level)
  # v0.9.22: USDA subgroup-level benchmark uses
  # `reference_usda_subgroup` (populated by load_kssl_pedons_gpkg
  # from KSSL `samp_taxsubgrp`). Falls back to `reference_usda` if
  # the subgroup field is absent.
  ref_field <- switch(system,
                        wrb2022 = "reference_wrb",
                        sibcs   = "reference_sibcs",
                        usda    = if (level == "subgroup")
                                    "reference_usda_subgroup"
                                  else "reference_usda")
  classify <- switch(system,
                       wrb2022 = function(p) classify_wrb2022(p,
                                                                on_missing = "silent"),
                       sibcs   = function(p) classify_sibcs(p),
                       usda    = function(p) classify_usda(p,
                                                              on_missing = "silent"))

  rows <- vector("list", length(pedons))
  for (i in seq_along(pedons)) {
    p <- pedons[[i]]
    ref <- p$site[[ref_field]]
    if (is.null(ref) || (length(ref) == 1 && is.na(ref))) next
    cls <- tryCatch(classify(p), error = function(e) NULL)
    if (is.null(cls)) {
      rows[[i]] <- list(id = p$site$id, ref = ref, pred = NA_character_,
                            order_pred = NA_character_)
      next
    }
    pred <- cls$name %||% NA_character_
    order_pred <- cls$rsg_or_order %||% NA_character_
    rows[[i]] <- list(id = p$site$id, ref = ref, pred = pred,
                        order_pred = order_pred)
  }
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L)
    return(list(accuracy_top1 = NA_real_, accuracy_ci = c(NA_real_, NA_real_),
                  confusion = NULL, per_pedon = data.frame()))
  per_pedon <- do.call(rbind, lapply(rows, as.data.frame))

  # Choose comparison column + normalisation per level. v0.9.22:
  # subgroup + subordem use case-insensitive token normalisation
  # (lowercase, strip leading qualifiers in parens, collapse
  # whitespace) so that "Typic Hapludalfs" (soilKey output) matches
  # "typic hapludalfs" (KSSL samp_taxsubgrp).
  if (level == "order") {
    pred_col <- "order_pred"
    .norm <- function(x) as.character(x)
  } else if (level == "subgroup") {
    pred_col <- "pred"
    .norm <- function(x) {
      v <- as.character(x)
      v <- gsub("\\s*\\(.*$", "", v)        # strip qualifiers in parens
      v <- tolower(trimws(v))
      v <- gsub("\\s+", " ", v)
      v
    }
  } else if (level == "subordem") {
    pred_col <- "pred"
    .norm <- function(x) {
      v <- as.character(x)
      v <- gsub("\\s*\\(.*$", "", v)
      v <- tolower(trimws(v))
      v <- gsub("\\s+", " ", v)
      # Take only the first two tokens (Ordem + Subordem in SiBCS).
      vapply(strsplit(v, " ", fixed = TRUE), function(toks) {
        if (length(toks) >= 2L) paste(toks[1:2], collapse = " ")
        else if (length(toks) == 1L) toks[1]
        else NA_character_
      }, character(1))
    }
  }
  per_pedon$ref      <- .norm(per_pedon$ref)
  per_pedon[[pred_col]] <- .norm(per_pedon[[pred_col]])
  matches  <- !is.na(per_pedon$ref) & !is.na(per_pedon[[pred_col]]) &
                as.character(per_pedon$ref) ==
                  as.character(per_pedon[[pred_col]])
  acc <- mean(matches, na.rm = TRUE)

  # Bootstrap CI.
  if (boot_n >= 1L && length(matches) > 1L) {
    set.seed(42L)
    boots <- replicate(boot_n,
                         mean(sample(matches, replace = TRUE)))
    ci <- stats::quantile(boots, c(0.025, 0.975), na.rm = TRUE)
  } else {
    ci <- c(NA_real_, NA_real_)
  }

  conf <- tryCatch(
    table(reference = per_pedon$ref,
          predicted = per_pedon[[pred_col]]),
    error = function(e) NULL
  )

  list(
    accuracy_top1 = acc,
    accuracy_ci   = ci,
    confusion     = conf,
    per_pedon     = per_pedon,
    n_evaluated   = nrow(per_pedon),
    system        = system,
    level         = level
  )
}
