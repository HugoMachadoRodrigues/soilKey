# =============================================================================
# KSSL + NASIS combined loader (v0.9.20)
#
# The NCSS Lab Data Mart (`ncss_labdata.gpkg`) carries the laboratory
# chemistry + physics; the NASIS Morphological export
# (`NASIS_Morphological_09142021.sqlite`) carries the field morphology
# (Munsell, structure, clay films, surveyor-identified diagnostic
# horizons). The two link via the `peiid` (Pedon Element ID).
#
# This loader joins both sources on a per-horizon basis:
#
#   1. lab_combine_nasis_ncss.peiid        <-> nasis.pedon.peiid
#   2. nasis.pedon.peiid                   <-> nasis.phorizon.peiidref
#   3. nasis.phorizon.phiid                <-> nasis.phcolor.phiidref
#                                              nasis.phstructure.phiidref
#                                              nasis.phpvsf.phiidref
#                                              nasis.phcracks.phiidref
#
# For each KSSL lab layer, we find the matching NASIS phorizon by
# (peiid, hzdept ~ hzn_top, hzdepb ~ hzn_bot) and pull:
#
#   munsell_*      from phcolor (filtered by colormoistst)
#   structure_*    from phstructure
#   clay_films_*   from phpvsf where pvsfkind LIKE '%clay films%'
#   slickensides   from phpvsf (pedogenic) + pediagfeatures
#   cracks_*_cm    from phcracks
#
# The returned PedonRecord is a strict superset of the lab-only one:
# every lab field is preserved, plus the morphology fields the field
# surveyor recorded but which the lab gpkg does not capture.
# =============================================================================


#' Load KSSL pedons enriched with NASIS morphology
#'
#' Joins the NCSS Lab Data Mart GeoPackage with the NASIS
#' Morphological SQLite to produce PedonRecord objects whose horizons
#' table has BOTH lab chemistry + physics AND field morphology
#' (Munsell, structure, clay films, slickensides, cracks). Required
#' for the morphological-evidence diagnostics
#' (\code{\link{argic}} clay-films, \code{\link{vertic_horizon}}
#' slickensides, \code{\link{mollic_epipedon_usda}} Munsell, etc.) to
#' fire on KSSL profiles -- the lab gpkg alone has none of those.
#'
#' @param gpkg Path to \code{ncss_labdata.gpkg}.
#' @param sqlite Path to \code{NASIS_Morphological_*.sqlite}.
#' @param head Optional integer; load only the first N classified
#'        pedons. Useful for parser validation / scaling.
#' @param require_b_horizon If \code{TRUE} (default), drops pedons
#'        whose deepest horizon's bottom_cm < 30.
#' @param verbose If \code{TRUE} (default), emits progress messages.
#' @return A list of \code{\link{PedonRecord}} objects.
#' @export
load_kssl_pedons_with_nasis <- function(gpkg,
                                            sqlite,
                                            head              = NULL,
                                            require_b_horizon = TRUE,
                                            verbose           = TRUE) {
  if (!requireNamespace("sf", quietly = TRUE))
    stop("install.packages('sf') required to read GeoPackage")
  if (!requireNamespace("RSQLite", quietly = TRUE))
    stop("install.packages('RSQLite') required to read NASIS sqlite")
  if (!requireNamespace("DBI", quietly = TRUE))
    stop("install.packages('DBI') required to read NASIS sqlite")
  if (!file.exists(gpkg))   stop(sprintf("gpkg not found: %s", gpkg))
  if (!file.exists(sqlite)) stop(sprintf("sqlite not found: %s", sqlite))

  if (verbose) cli::cli_alert_info("Loading lab data via existing loader ...")
  lab_peds <- load_kssl_pedons_gpkg(gpkg, head = head,
                                       require_b_horizon = require_b_horizon,
                                       verbose = FALSE)
  if (length(lab_peds) == 0L) return(list())

  # Build lookup: pedon$site$id (= as.character(pedon_key)) -> peiid.
  if (verbose) cli::cli_alert_info("Linking pedons to NASIS via peiid ...")
  combine <- data.table::as.data.table(suppressWarnings(
    sf::st_drop_geometry(sf::read_sf(gpkg, layer = "lab_combine_nasis_ncss",
                                       query = "SELECT pedon_key, peiid FROM lab_combine_nasis_ncss"))
  ))
  pk_to_peiid <- setNames(as.integer(combine$peiid),
                            as.character(combine$pedon_key))

  peiids <- unname(pk_to_peiid[vapply(lab_peds, function(p) p$site$id,
                                          character(1))])
  peiids <- peiids[!is.na(peiids)]
  if (length(peiids) == 0L) {
    if (verbose) cli::cli_alert_warning("No NASIS peiids matched -- returning lab-only pedons")
    return(lab_peds)
  }

  if (verbose) cli::cli_alert_info("Reading NASIS tables in full (faster + more reliable than IN-filter for large pedon sets) ...")
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  peiid_set <- as.integer(peiids)

  ph <- data.table::as.data.table(DBI::dbGetQuery(con,
    "SELECT phiid, peiidref, hzdept, hzdepb, hzname FROM phorizon"))
  ph <- ph[ph$peiidref %in% peiid_set, ]
  if (nrow(ph) == 0L) {
    if (verbose) cli::cli_alert_warning("No NASIS phorizon rows for those peiids")
    return(lab_peds)
  }
  phiid_set <- as.integer(ph$phiid)

  phc <- data.table::as.data.table(DBI::dbGetQuery(con,
    "SELECT phiidref, colorhue, colorvalue, colorchroma, colormoistst, colorpct FROM phcolor"))
  phc <- phc[phc$phiidref %in% phiid_set, ]
  phs <- data.table::as.data.table(DBI::dbGetQuery(con,
    "SELECT phiidref, structgrade, structsize, structtype FROM phstructure"))
  phs <- phs[phs$phiidref %in% phiid_set, ]
  phpv <- data.table::as.data.table(DBI::dbGetQuery(con,
    "SELECT phiidref, pvsfpct, pvsfkind, pvsfdistinct FROM phpvsf"))
  phpv <- phpv[phpv$phiidref %in% phiid_set, ]
  phcr <- data.table::as.data.table(DBI::dbGetQuery(con,
    "SELECT phiidref, crackfreq, crackwidth, crackdepth FROM phcracks"))
  phcr <- phcr[phcr$phiidref %in% phiid_set, ]
  pdf <- data.table::as.data.table(DBI::dbGetQuery(con,
    "SELECT peiidref, featkind, featdept, featdepb FROM pediagfeatures"))
  pdf <- pdf[pdf$peiidref %in% peiid_set, ]

  # Pre-index by phiidref / peiidref for fast per-pedon filtering.
  data.table::setkeyv(ph,   "peiidref")
  data.table::setkeyv(phc,  "phiidref")
  data.table::setkeyv(phs,  "phiidref")
  data.table::setkeyv(phpv, "phiidref")
  data.table::setkeyv(phcr, "phiidref")
  data.table::setkeyv(pdf,  "peiidref")

  # ---- enrich each lab PedonRecord -----------------------------------------
  if (verbose) cli::cli_alert_info("Enriching {.val {length(lab_peds)}} pedons with morphology ...")
  for (i in seq_along(lab_peds)) {
    p <- lab_peds[[i]]
    peiid <- pk_to_peiid[[p$site$id]]
    if (is.null(peiid) || is.na(peiid)) next
    p_ph <- ph[ph$peiidref == peiid, ]
    if (nrow(p_ph) == 0L) next

    # Diagnostic features for this pedon (site-level info).
    p_pdf <- pdf[pdf$peiidref == peiid, ]
    p$site$nasis_diagnostic_features <- p_pdf$featkind

    h <- p$horizons
    n_h <- nrow(h)
    if (n_h == 0L) next

    # Match NASIS phorizon to lab horizon by depth (best overlap).
    matched_phiid <- integer(n_h)
    for (j in seq_len(n_h)) {
      lt <- h$top_cm[j]; lb <- h$bottom_cm[j]
      if (is.na(lt) || is.na(lb)) { matched_phiid[j] <- NA_integer_; next }
      overlap <- pmax(0, pmin(p_ph$hzdepb, lb) - pmax(p_ph$hzdept, lt))
      best <- which.max(overlap)
      matched_phiid[j] <- if (length(best) == 1L && overlap[best] > 0)
                              p_ph$phiid[best] else NA_integer_
    }

    # Default to NA columns; fill where matched.
    for (col in c("munsell_hue_moist","munsell_hue_dry")) {
      if (!col %in% names(h)) h[[col]] <- NA_character_
    }
    for (col in c("munsell_value_moist","munsell_chroma_moist",
                  "munsell_value_dry","munsell_chroma_dry",
                  "cracks_width_cm","cracks_depth_cm")) {
      if (!col %in% names(h)) h[[col]] <- NA_real_
    }
    for (col in c("structure_grade","structure_size","structure_type",
                  "clay_films_amount","clay_films_strength","slickensides")) {
      if (!col %in% names(h)) h[[col]] <- NA_character_
    }

    for (j in seq_len(n_h)) {
      pid <- matched_phiid[j]
      if (is.na(pid)) next

      # Munsell: most-abundant color (largest colorpct) per state.
      jc <- phc[phc$phiidref == pid, ]
      if (nrow(jc) > 0L) {
        moist <- jc[!is.na(jc$colormoistst) &
                      tolower(jc$colormoistst) %in% c("moist","wet"), ]
        dry   <- jc[!is.na(jc$colormoistst) &
                      tolower(jc$colormoistst) == "dry", ]
        pick_dominant <- function(d) {
          if (nrow(d) == 0L) return(NULL)
          if (any(!is.na(d$colorpct))) d <- d[order(-d$colorpct), ]
          d[1, ]
        }
        pm <- pick_dominant(moist)
        pd <- pick_dominant(dry)
        if (!is.null(pm)) {
          if (is.na(h$munsell_hue_moist[j]))
            h$munsell_hue_moist[j] <- as.character(pm$colorhue)
          if (is.na(h$munsell_value_moist[j]))
            h$munsell_value_moist[j] <- as.numeric(pm$colorvalue)
          if (is.na(h$munsell_chroma_moist[j]))
            h$munsell_chroma_moist[j] <- as.numeric(pm$colorchroma)
        }
        if (!is.null(pd)) {
          if (is.na(h$munsell_hue_dry[j]))
            h$munsell_hue_dry[j] <- as.character(pd$colorhue)
          if (is.na(h$munsell_value_dry[j]))
            h$munsell_value_dry[j] <- as.numeric(pd$colorvalue)
          if (is.na(h$munsell_chroma_dry[j]))
            h$munsell_chroma_dry[j] <- as.numeric(pd$colorchroma)
        }
      }

      # Structure: dominant (any of grade/size/type recorded).
      # NASIS ships values capitalised ("Moderate" / "Strong") whereas
      # soilKey diagnostics expect lowercase canonical strings;
      # normalise here so downstream gates match without case-twiddling.
      js <- phs[phs$phiidref == pid, ]
      if (nrow(js) > 0L) {
        if (is.na(h$structure_grade[j])) {
          v <- as.character(js$structgrade[1])
          h$structure_grade[j] <- if (is.na(v) || !nzchar(v)) NA_character_
                                    else tolower(v)
        }
        if (is.na(h$structure_size[j])) {
          v <- as.character(js$structsize[1])
          h$structure_size[j] <- if (is.na(v) || !nzchar(v)) NA_character_
                                    else tolower(v)
        }
        if (is.na(h$structure_type[j])) {
          v <- as.character(js$structtype[1])
          h$structure_type[j] <- if (is.na(v) || !nzchar(v)) NA_character_
                                    else tolower(v)
        }
      }

      # Clay films + slickensides from phpvsf.
      jp <- phpv[phpv$phiidref == pid, ]
      if (nrow(jp) > 0L) {
        cf <- jp[!is.na(jp$pvsfkind) &
                   grepl("clay films|clay bridges|organoargillan", jp$pvsfkind,
                         ignore.case = TRUE), ]
        if (nrow(cf) > 0L && is.na(h$clay_films_amount[j])) {
          # Map pvsfpct (% of ped surface) to soilKey's qualitative
          # tiers: < 5 -> few, 5-25 -> common, 25-50 -> many,
          # >= 50 -> continuous. Fall back to "common" when no pct.
          pct <- max(cf$pvsfpct, na.rm = TRUE)
          h$clay_films_amount[j] <-
            if (!is.finite(pct))    "common"
            else if (pct < 5)        "few"
            else if (pct < 25)       "common"
            else if (pct < 50)       "many"
            else                     "continuous"
        }
        ss <- jp[!is.na(jp$pvsfkind) &
                   grepl("slickensides", jp$pvsfkind, ignore.case = TRUE), ]
        if (nrow(ss) > 0L && is.na(h$slickensides[j])) {
          pct <- max(ss$pvsfpct, na.rm = TRUE)
          h$slickensides[j] <-
            if (!is.finite(pct))    "common"
            else if (pct < 5)        "few"
            else if (pct < 25)       "common"
            else if (pct < 50)       "many"
            else                     "continuous"
        }
      }

      # Cracks.
      jcr <- phcr[phcr$phiidref == pid, ]
      if (nrow(jcr) > 0L) {
        if (is.na(h$cracks_width_cm[j]))
          h$cracks_width_cm[j] <- as.numeric(jcr$crackwidth[1]) / 10  # mm -> cm
        if (is.na(h$cracks_depth_cm[j]))
          h$cracks_depth_cm[j] <- as.numeric(jcr$crackdepth[1])
      }

      # Designation from NASIS (richer than lab gpkg).
      if (is.na(h$designation[j])) {
        nasis_hzname <- p_ph$hzname[p_ph$phiid == pid][1]
        if (!is.na(nasis_hzname) && nzchar(nasis_hzname))
          h$designation[j] <- as.character(nasis_hzname)
      }
    }

    p$horizons <- h
  }

  if (verbose)
    cli::cli_alert_success("KSSL + NASIS: enriched {.val {length(lab_peds)}} pedons with morphology")
  lab_peds
}
