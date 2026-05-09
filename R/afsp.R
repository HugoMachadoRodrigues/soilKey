# =============================================================================
# AfSP (Africa Soil Profiles) loader + benchmark (v0.9.77)
#
# AfSP is the Africa Soil Profiles database (Leenaars et al. 2014),
# 18,533 georeferenced profiles harmonised by ISRIC across 41 sub-
# Saharan African countries. It ships WRB 2006 (RSG-level) and FAO 1988
# classifications, plus rich morphological + lab data:
# Munsell colours, structure, exchangeable bases (Ca, Mg, Na, K),
# exchangeable acidity, base saturation, CaCO3, CaSO4, organic carbon,
# texture, bulk density, EC -- everything needed for WRB classification.
#
# Source data: AF-AfSP1.2.zip from
#   https://files.isric.org/public/afsp/AF-AfSP1.2.zip
#
# Reference:
#   Leenaars, J. G. B., van Oostrum, A. J. M., & Ruiperez Gonzalez, M.
#   (2014). Africa Soil Profiles Database, Version 1.2. ISRIC Report
#   2014/01. ISRIC -- World Soil Information, Wageningen.
#   Project page: https://isric.org/projects/africa-soil-profiles-database-afsp
# =============================================================================


#' WRB 2006 RSG code -> 2022 RSG name
#'
#' AfSP ships WRB 2006 RSG codes (2-letter, e.g.\ LV, AC, AR). The
#' 2-letter codes are stable across WRB editions (2006 -> 2022); only
#' a handful of qualifier names changed. This helper maps the codes
#' to the WRB 2022 RSG names that \code{classify_wrb2022} emits.
#'
#' @param code Character vector of WRB 2006 codes.
#' @return Character vector of singular WRB 2022 RSG names; \code{NA}
#'   for unrecognised codes.
#' @export
wrb06_code_to_rsg <- function(code) {
  m <- c(
    AC = "Acrisol",     AL = "Alisol",      AN = "Andosol",     AR = "Arenosol",
    AT = "Anthrosol",   CH = "Chernozem",   CL = "Calcisol",    CM = "Cambisol",
    CR = "Cryosol",     DU = "Durisol",     FL = "Fluvisol",    FR = "Ferralsol",
    GL = "Gleysol",     GY = "Gypsisol",    HS = "Histosol",    KS = "Kastanozem",
    LP = "Leptosol",    LV = "Luvisol",     LX = "Lixisol",     NT = "Nitisol",
    PH = "Phaeozem",    PL = "Planosol",    PT = "Plinthosol",  PZ = "Podzol",
    RG = "Regosol",     SC = "Solonchak",   SN = "Solonetz",    ST = "Stagnosol",
    TC = "Technosol",   UM = "Umbrisol",    VR = "Vertisol",    RT = "Retisol",
    # Albeluvisols (WRB 2006) merged into Retisols (WRB 2014/2022)
    AB = "Retisol"
  )
  out <- m[toupper(trimws(as.character(code)))]
  unname(out)
}


#' Convert AfSP NoData sentinel (-9999) to NA
#' @keywords internal
.afsp_unna <- function(x) {
  if (is.numeric(x)) {
    x[!is.na(x) & x == -9999] <- NA_real_
  } else if (is.character(x)) {
    x[!is.na(x) & x == "-9999"] <- NA_character_
    x[!is.na(x) & x == "NA"]    <- NA_character_
  }
  x
}


#' Parse AfSP Munsell colour string (e.g. "10YR 4/3") into hue/value/chroma
#' @keywords internal
.afsp_parse_munsell <- function(s) {
  if (is.na(s) || !nzchar(s)) return(list(hue = NA_character_,
                                              value = NA_real_,
                                              chroma = NA_real_))
  s <- trimws(s)
  # AfSP packs hue+value with NO space: "10YR3/2", "2.5YR3/6", "7.5YR4/4".
  # Some legacy datasets DO use a space ("10YR 4/3") -- accept both.
  m <- regmatches(s, regexec("^([0-9.]+[A-Z]+)\\s*([0-9]+)/([0-9]+)$", s))[[1]]
  if (length(m) >= 4L) {
    return(list(
      hue    = m[2],
      value  = as.numeric(m[3]),
      chroma = as.numeric(m[4])
    ))
  }
  list(hue = NA_character_, value = NA_real_, chroma = NA_real_)
}


#' Build a soilKey PedonRecord from AfSP Profiles + Layers rows
#' @keywords internal
.afsp_to_pedon <- function(profile_row, layer_rows) {
  # Map AfSP Layers fields -> soilKey horizon schema
  hz_list <- lapply(seq_len(nrow(layer_rows)), function(i) {
    L <- layer_rows[i, ]
    # Parse Munsell colours
    cm <- .afsp_parse_munsell(.afsp_unna(L$ColorM))
    cd <- .afsp_parse_munsell(.afsp_unna(L$ColorD))
    # Compute CEC: prefer CecSoil (NH4OAc), fall back to Ecec
    cec <- as.numeric(.afsp_unna(L$CecSoil))
    ecec <- as.numeric(.afsp_unna(L$Ecec))
    if (is.na(cec) && !is.na(ecec)) cec <- ecec
    # Bases
    ca <- as.numeric(.afsp_unna(L$ExCa))
    mg <- as.numeric(.afsp_unna(L$ExMg))
    k  <- as.numeric(.afsp_unna(L$ExK))
    na <- as.numeric(.afsp_unna(L$ExNa))
    al <- as.numeric(.afsp_unna(L$ExAl))
    # OC: g/kg in AfSP -> %
    oc_gkg <- as.numeric(.afsp_unna(L$OrgC))
    oc_pct <- if (!is.na(oc_gkg)) oc_gkg / 10 else NA_real_
    # bsat
    bs <- as.numeric(.afsp_unna(L$Bsat))

    list(
      top_cm                  = as.numeric(.afsp_unna(L$UpDpth)),
      bottom_cm               = as.numeric(.afsp_unna(L$LowDpth)),
      designation             = .afsp_unna(as.character(L$HorDes)),
      munsell_hue_moist       = cm$hue,
      munsell_value_moist     = cm$value,
      munsell_chroma_moist    = cm$chroma,
      munsell_hue_dry         = cd$hue,
      munsell_value_dry       = cd$value,
      munsell_chroma_dry      = cd$chroma,
      structure_grade         = .afsp_unna(as.character(L$StrGrade)),
      structure_size          = .afsp_unna(as.character(L$StrSize)),
      structure_type          = .afsp_unna(as.character(L$StrType)),
      clay_pct                = as.numeric(.afsp_unna(L$Clay)),
      silt_pct                = as.numeric(.afsp_unna(L$Silt)),
      sand_pct                = as.numeric(.afsp_unna(L$Sand)),
      coarse_fragments_pct    = as.numeric(.afsp_unna(L$CfPc)),
      ph_h2o                  = as.numeric(.afsp_unna(L$PHH2O)),
      ph_kcl                  = as.numeric(.afsp_unna(L$PHKCl)),
      ph_cacl2                = as.numeric(.afsp_unna(L$PHCaCl2)),
      ec_dS_m                 = as.numeric(.afsp_unna(L$EC)),
      oc_pct                  = oc_pct,
      n_total_pct             = as.numeric(.afsp_unna(L$TotalN)) / 10,
      cec_cmol                = cec,
      ecec_cmol               = ecec,
      bs_pct                  = bs,
      ca_cmol                 = ca,
      mg_cmol                 = mg,
      k_cmol                  = k,
      na_cmol                 = na,
      al_cmol                 = al,
      caco3_pct               = as.numeric(.afsp_unna(L$CaCO3)),
      caso4_pct               = as.numeric(.afsp_unna(L$CaSO4)),
      bulk_density_g_cm3      = as.numeric(.afsp_unna(L$BlkDens))
    )
  })
  hz <- data.table::rbindlist(hz_list, fill = TRUE)
  # Sort by depth
  if ("top_cm" %in% colnames(hz))
    hz <- hz[order(hz$top_cm), ]
  hz <- ensure_horizon_schema(hz)

  PedonRecord$new(
    site = list(
      id                       = as.character(profile_row$ProfileID),
      country                  = as.character(profile_row$Country),
      reference_wrb            = wrb06_code_to_rsg(profile_row$WRB06rg),
      reference_wrb06_full     = as.character(profile_row$WRB06),
      reference_fao88          = as.character(profile_row$FAO88),
      reference_usda           = as.character(profile_row$USDA),
      reference_source         = "ISRIC AfSP v1.2 (Leenaars et al. 2014)"
    ),
    horizons = hz
  )
}


#' Load Africa Soil Profiles (AfSP) v1.2 as PedonRecord objects
#'
#' Reads the AfSP DBase tables shipped inside \code{AF-AfSP1.2.zip}
#' (downloadable from
#' \url{https://files.isric.org/public/afsp/AF-AfSP1.2.zip}) and
#' converts each profile + its horizons to a soilKey
#' \code{\link{PedonRecord}}. Filters to profiles with a populated
#' WRB 2006 RSG code (i.e.\ classifiable; AfSP has ~7000 of these of
#' the total 18,533).
#'
#' @param afsp_dir Directory containing the extracted AfSP DBase
#'        tables (\code{AfSP012Qry_Profiles.dbf},
#'        \code{AfSP012Qry_Layers.dbf}).
#' @param max_n Optional integer; take a random sample of this size
#'        from the classifiable profiles.
#' @param countries Optional character vector of ISO country codes to
#'        keep (e.g.\ \code{c("MW", "ET", "TZ")}).
#' @param wrb_codes Optional character vector of WRB 2006 RSG codes
#'        to keep (e.g.\ \code{c("VR", "FR", "AC")}).
#' @param verbose Print progress.
#'
#' @return A list of \code{\link{PedonRecord}} objects.
#'
#' @section References:
#' Leenaars, J. G. B., van Oostrum, A. J. M., & Ruiperez Gonzalez, M.
#' (2014). Africa Soil Profiles Database, Version 1.2. ISRIC Report
#' 2014/01. ISRIC -- World Soil Information, Wageningen.
#' Project page:
#' \url{https://isric.org/projects/africa-soil-profiles-database-afsp}.
#'
#' @export
load_afsp_pedons <- function(afsp_dir,
                              max_n = NULL,
                              countries = NULL,
                              wrb_codes = NULL,
                              verbose = TRUE) {
  if (!requireNamespace("foreign", quietly = TRUE))
    stop("install.packages('foreign') required to read AfSP DBF files")
  prof_path <- file.path(afsp_dir, "AfSP012Qry_Profiles.dbf")
  lay_path  <- file.path(afsp_dir, "AfSP012Qry_Layers.dbf")
  if (!file.exists(prof_path)) stop(sprintf("Profiles DBF not found: %s", prof_path))
  if (!file.exists(lay_path))  stop(sprintf("Layers DBF not found: %s", lay_path))

  if (isTRUE(verbose)) cat("[afsp] reading Profiles DBF ...\n")
  prof <- foreign::read.dbf(prof_path, as.is = TRUE)
  prof <- prof[!is.na(prof$WRB06rg) & nzchar(prof$WRB06rg) &
                 prof$WRB06rg != "NA", ]
  if (!is.null(countries))
    prof <- prof[prof$Country %in% countries, ]
  if (!is.null(wrb_codes))
    prof <- prof[prof$WRB06rg %in% wrb_codes, ]
  if (!is.null(max_n) && nrow(prof) > max_n)
    prof <- prof[sample(nrow(prof), max_n), ]
  if (nrow(prof) == 0L) {
    warning("No profiles match the filter")
    return(list())
  }

  if (isTRUE(verbose)) cat(sprintf("[afsp] %d classifiable profiles selected\n",
                                      nrow(prof)))

  if (isTRUE(verbose)) cat("[afsp] reading Layers DBF (large, ~4 min) ...\n")
  lay <- foreign::read.dbf(lay_path, as.is = TRUE)
  lay <- lay[lay$ProfileID %in% prof$ProfileID, ]

  if (isTRUE(verbose)) cat(sprintf("[afsp] building %d PedonRecords ...\n",
                                      nrow(prof)))
  pedons <- vector("list", nrow(prof))
  for (i in seq_len(nrow(prof))) {
    p_row <- prof[i, ]
    l_rows <- lay[lay$ProfileID == p_row$ProfileID, ]
    pedons[[i]] <- tryCatch(.afsp_to_pedon(p_row, l_rows),
                              error = function(e) NULL)
  }
  pedons <- pedons[!vapply(pedons, is.null, logical(1))]

  if (isTRUE(verbose)) cat(sprintf("[afsp] returned %d pedons\n",
                                      length(pedons)))
  pedons
}


#' Load the bundled AfSP stratified sample (v0.9.77)
#'
#' Returns a 130-profile snapshot from AfSP v1.2 stratified by WRB
#' RSG (5 profiles per RSG x 26 RSGs), pre-built so users can run
#' the African WRB benchmark offline without the 35 MB ZIP download.
#'
#' This is the African analogue of
#' \code{\link{load_wosis_stratified_sample}} (global WoSIS) and
#' \code{\link{load_kssl_nasis_sample}} (US KSSL+NASIS).
#'
#' @return A list with \code{pedons}, \code{pulled_on}, \code{source},
#'   \code{filter}.
#'
#' @section Reference:
#' Leenaars, J. G. B., van Oostrum, A. J. M., & Ruiperez Gonzalez, M.
#' (2014). Africa Soil Profiles Database, Version 1.2. ISRIC Report
#' 2014/01.
#'
#' @export
load_afsp_sample <- function() {
  # v0.9.94: routed through the lazy-fetch helper. The .rds is no
  # longer bundled in CRAN releases; the helper looks in
  # `inst/extdata/` (back-compat for developer checkouts), then in
  # the user cache at `tools::R_user_dir("soilKey", "data")`,
  # then offers an on-demand download from GitHub Release.
  .lazy_fetch_readRDS("afsp_sample")
}


#' Benchmark soilKey WRB predictions against AfSP ground truth
#'
#' @param pedons List of \code{\link{PedonRecord}} from
#'        \code{\link{load_afsp_pedons}} or \code{\link{load_afsp_sample}}.
#' @param verbose Print progress.
#'
#' @return List with \code{accuracy}, \code{n_compared}, \code{confusion},
#'   \code{per_class_recall}.
#' @export
benchmark_afsp <- function(pedons, verbose = TRUE) {
  if (length(pedons) == 0L) stop("Empty pedon list.")
  if (isTRUE(verbose))
    cat(sprintf("[afsp] benchmarking %d pedons\n", length(pedons)))

  preds <- vapply(pedons, function(pr) {
    res <- tryCatch(classify_wrb2022(pr, on_missing = "silent"),
                     error = function(e) NULL)
    if (is.null(res)) NA_character_ else sub("s$", "", res$rsg_or_order)
  }, character(1))
  refs <- vapply(pedons, function(p) p$site$reference_wrb %||% NA_character_,
                  character(1))

  in_scope <- !is.na(refs) & !is.na(preds)
  n_correct <- sum(in_scope & refs == preds)
  n_total   <- sum(in_scope)
  acc <- if (n_total > 0L) n_correct / n_total else NA_real_

  conf <- table(reference = refs[in_scope], predicted = preds[in_scope])
  per_class <- data.frame(
    reference_rsg = rownames(conf),
    n             = rowSums(conf),
    n_correct     = vapply(rownames(conf),
                            function(r) if (r %in% colnames(conf)) conf[r, r] else 0L,
                            integer(1)),
    stringsAsFactors = FALSE
  )
  per_class$recall <- per_class$n_correct / per_class$n

  if (isTRUE(verbose))
    cat(sprintf("[afsp] accuracy = %.1f%% on n = %d\n", 100 * acc, n_total))

  list(accuracy = acc, n_compared = n_total, n_total = length(pedons),
       confusion = conf, per_class_recall = per_class,
       refs = refs, preds = preds)
}
