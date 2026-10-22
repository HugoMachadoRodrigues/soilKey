# =============================================================================
# v0.9.278 -- NatSoil, the national soil site database of CSIRO (Australia),
# read through its public API, and a benchmark of classify_asc3() against the
# orders the surveyors gave. The data are licensed per site (most under
# CC BY 4.0); they are not shipped with soilKey. fetch_natsoil() saves the
# API's JSON to a folder of the user's choice, load_natsoil_pedons() turns
# it into PedonRecords, and benchmark_natsoil() compares the orders.
# =============================================================================

.NATSOIL_API <- "https://soils.csiro.au/data/natsoil-api/sites"

# Exchangeable cations: the extraction methods of NatSoil (Rayment and
# Lyons 2011), in order of preference; "15_NR" is a method not recorded.
.NATSOIL_BASE_METHODS <- c("15A1", "15A2", "15A3", "15B1", "15B2", "15D1", "15D2", "15D3",
                           "15E1", "15F1", "15C1", "15_NR")

# Laboratory codes read for each attribute, in order of preference (codes
# are matched whatever their case). The units are already soilKey's: %, and
# meq/100g, which is cmol(+)/kg. "_NR" codes are methods not recorded.
.NATSOIL_LAB <- list(
  clay_pct   = list(codes = c("P10_CF_C", "P10_PB_C", "P10A1_C", "P10_I_C", "P10_HYD_C", "2Z2_C",
                              "P10_S_2", "P10_NR_C")),
  ph_h2o     = list(codes = c("4A1", "4_NR")),
  ph_cacl2   = list(codes = c("4B1", "4B2")),
  na_cmol    = list(codes = paste0(.NATSOIL_BASE_METHODS, "_NA")),
  ca_cmol    = list(codes = paste0(.NATSOIL_BASE_METHODS, "_CA")),
  mg_cmol    = list(codes = paste0(.NATSOIL_BASE_METHODS, "_MG")),
  k_cmol     = list(codes = paste0(.NATSOIL_BASE_METHODS, "_K")),
  cec_cmol   = list(codes = c("15A2_CEC", "15A1_CEC", "15B1_CEC", "15B2_CEC", "15D1_CEC", "15D2_CEC",
                              "15D3_CEC", "15I1", "15I3", "15C1_CEC", "15F3", "15_NR_CEC")),
  ecec_cmol  = list(codes = c("15J1", "15J1_ECEC", "15J_H", "15_NR")),
  esp_pct    = list(codes = c("15N1", "15_NR_ESP")),
  oc_pct     = list(codes = c("6B2", "6B3", "6_DC", "6A1", "6Z")),
  fe_dcb_pct = list(codes = c("13C1_FE", "13_C_FE", "13C_C_FE")),
  caco3_pct  = list(codes = c("19A1", "19B1", "19_COL", "19B_NR"))
)

# A value below the detection limit ("<x") is read as x / 2 unless x lies
# above a limit the keys read for that attribute, where the value is unknown.
.NATSOIL_CENSOR_LIMIT <- c(caco3_pct = 0.5, na_cmol = 0.18, esp_pct = 6, fe_dcb_pct = 5,
                           oc_pct = 12, clay_pct = 10)

#' Download NatSoil sites and profiles (CSIRO)
#'
#' Reads NatSoil, the national soil site database of CSIRO, through its
#' public API and saves the JSON it returns in \code{dir}: first every
#' public site (pages of \code{page_size} sites, without horizons, in
#' \code{dir/sites}), then, for each site whose licence allows external use
#' (and, by default, that carries an order of the Australian Soil
#' Classification), its horizons, samples and laboratory results (one file
#' per site in \code{dir/profiles}). Files already in \code{dir} are not
#' downloaded again, so an interrupted run can be resumed. The data are
#' licensed per site, most under CC BY 4.0; cite them as each site's
#' \code{citation} field says (kept in \code{site$citation} by
#' \code{\link{load_natsoil_pedons}}).
#'
#' The whole site pass is about 150 requests and the profile pass one request
#' per site (about 9,800 with an ASC order), with a pause between them.
#'
#' @param dir Folder for the downloaded files (created when missing).
#' @param classified_only Download profiles only for sites with an ASC order.
#' @param max_sites Maximum number of profiles to download in this call.
#' @param page_size Sites per page in the site pass.
#' @param pause Seconds to wait between requests.
#' @param verbose Print progress.
#' @return Invisibly, a data frame of the sites selected for the profile
#'   pass: \code{agency_code}, \code{proj_code}, \code{s_id}, the ASC order
#'   given (\code{asc_order}), \code{file} and whether it is on disk
#'   (\code{fetched}).
#' @references CSIRO. CSIRO National Soil Site Database (NatSoil).
#'   https://doi.org/10.25919/sg5w-ny87
#' @seealso \code{\link{load_natsoil_pedons}}, \code{\link{benchmark_natsoil}}.
#' @examples
#' \dontrun{
#' fetch_natsoil("natsoil")
#' peds <- load_natsoil_pedons("natsoil")
#' benchmark_natsoil(peds)
#' }
#' @export
fetch_natsoil <- function(dir, classified_only = TRUE, max_sites = Inf, page_size = 200L,
                          pause = 0.3, verbose = TRUE) {
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE))
    stop("fetch_natsoil() needs the 'httr' and 'jsonlite' packages.", call. = FALSE)
  sdir <- file.path(dir, "sites"); pdir <- file.path(dir, "profiles")
  dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
  dir.create(pdir, recursive = TRUE, showWarnings = FALSE)
  # site pass, page by page, until an empty page
  page <- 1L
  repeat {
    f <- file.path(sdir, sprintf("page_s%d_%05d.json.gz", as.integer(page_size), page))
    if (!file.exists(f)) {
      body <- .natsoil_get(list(page_num = page, page_size = page_size, horizons = "false"))
      .natsoil_write(body, f)
      Sys.sleep(pause)
    }
    n <- length(.natsoil_read_json(f))
    if (verbose) message(sprintf("NatSoil site pass: page %d, %d sites", page, n))
    if (n == 0L) break
    page <- page + 1L
  }
  sites <- .natsoil_site_list(sdir, classified_only, page_size)
  sites$file <- file.path(pdir, .natsoil_safe(sites$agency_code), .natsoil_safe(sites$proj_code),
                          paste0(.natsoil_safe(sites$s_id), ".json.gz"))
  todo <- which(!file.exists(sites$file))
  if (is.finite(max_sites)) todo <- utils::head(todo, max_sites)
  if (verbose) message(sprintf("NatSoil profile pass: %d sites selected, %d to download",
                               nrow(sites), length(todo)))
  for (k in seq_along(todo)) {
    i <- todo[k]
    body <- tryCatch(.natsoil_get(list(agency_code = sites$agency_code[i], proj_code = sites$proj_code[i],
                                       s_id = sites$s_id[i], horizons = "true", samples = "true",
                                       labResults = "true")),
                     error = function(e) { if (verbose) message("  ", conditionMessage(e)); NULL })
    if (!is.null(body)) {
      dir.create(dirname(sites$file[i]), recursive = TRUE, showWarnings = FALSE)
      .natsoil_write(body, sites$file[i])
    }
    if (verbose && k %% 200L == 0L) message(sprintf("  %d of %d", k, length(todo)))
    Sys.sleep(pause)
  }
  sites$fetched <- file.exists(sites$file)
  invisible(sites)
}

# One request to the NatSoil API, with retries; returns the body as text.
.natsoil_get <- function(query, tries = 4L, timeout = 240) {
  ua <- httr::user_agent("soilKey (R package; https://github.com/HugoMachadoRodrigues/soilKey)")
  last <- NULL
  for (attempt in seq_len(tries)) {
    r <- tryCatch(httr::GET(.NATSOIL_API, query = query, httr::timeout(timeout), ua),
                  error = function(e) e)
    if (!inherits(r, "error") && httr::status_code(r) == 200L)
      return(httr::content(r, as = "text", encoding = "UTF-8"))
    last <- if (inherits(r, "error")) conditionMessage(r) else paste("HTTP", httr::status_code(r))
    Sys.sleep(5 * attempt)
  }
  stop(sprintf("NatSoil API request failed (%s): %s", last,
               paste(names(query), unlist(query), sep = "=", collapse = "&")), call. = FALSE)
}

# Written to a temporary file and renamed, so that an interrupted write
# leaves no truncated file behind.
.natsoil_write <- function(text, file) {
  tmp <- paste0(file, ".part")
  con <- gzfile(tmp, "w", encoding = "UTF-8")
  writeLines(text, con, useBytes = TRUE)
  close(con)
  file.rename(tmp, file)
}

.natsoil_read_json <- function(file) {
  txt <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

.natsoil_safe <- function(x) gsub("[^A-Za-z0-9._-]", "_", as.character(x))

# A field of a JSON record, NA when missing or null.
.ns <- function(x, k) {
  v <- x[[k]]
  if (is.null(v) || length(v) == 0L) NA else v
}

# Is the site licensed for external use (a Creative Commons licence)?
.natsoil_licensed <- function(s) {
  grepl("creativecommons\\.org/licenses", as.character(.ns(s, "usage_license")), ignore.case = TRUE)
}

# The first observation of a site that carries an ASC order (or the first
# observation, when none does and classified = FALSE).
.natsoil_observation <- function(s, classified = TRUE) {
  obs <- s$z_observations %||% list()
  has <- vapply(obs, function(o) nzchar(trimws(as.character(.ns(o, "o_asc_ord") %|NA|% ""))), NA)
  if (any(has)) return(obs[[which(has)[1L]]])
  if (!classified && length(obs)) return(obs[[1L]])
  NULL
}

`%|NA|%` <- function(a, b) if (length(a) == 0L || is.na(a)) b else a

# The licensed sites of the site pages (of one page size), with their ASC
# order.
.natsoil_site_list <- function(sdir, classified_only = TRUE, page_size = 200L) {
  rows <- list()
  pat <- sprintf("^page_s%d_[0-9]+\\.json(\\.gz)?$", as.integer(page_size))
  for (f in sort(list.files(sdir, pattern = pat, full.names = TRUE))) {
    for (s in .natsoil_read_json(f)) {
      if (!.natsoil_licensed(s)) next
      o <- .natsoil_observation(s, classified = classified_only)
      if (classified_only && is.null(o)) next
      rows[[length(rows) + 1L]] <- data.frame(
        agency_code = as.character(.ns(s, "agency_code")), proj_code = as.character(.ns(s, "proj_code")),
        s_id = as.character(.ns(s, "s_id")),
        asc_order = if (is.null(o)) NA_character_ else trimws(as.character(.ns(o, "o_asc_ord"))),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(data.frame(agency_code = character(0), proj_code = character(0),
                                       s_id = character(0), asc_order = character(0)))
  unique(do.call(rbind, rows))
}

#' Read NatSoil profiles as pedons
#'
#' Turns the profile files saved by \code{\link{fetch_natsoil}} (or any JSON
#' of the NatSoil API with horizons, samples and laboratory results) into
#' \code{\link{PedonRecord}} objects for \code{\link{classify_asc3}} and the
#' other keys. Sites whose licence does not allow external use are left out.
#'
#' The horizons keep their designation in the Australian notation (lithologic
#' prefix, master, subdivision and suffixes), depths in cm, the field texture
#' grade, boundary distinctness, the grade and type of the first structure
#' (lenticular when any structure is), the moist and the dry matrix colours
#' (a colour with no moisture status is read as moist),
#' slickensides, crack width, coarse fragments (abundance classes at their
#' middle, added up to at most 100 \%; unknown when a record has no
#' abundance), calcareous segregations (a record with only its nature is a
#' template and is not read; one with a form or size but no abundance counts
#' as very few, 1 \%), cementation of pans, effervescence, plasticity type and
#' the moisture when described.
#'
#' Laboratory results are matched to the horizons by depth: a sample counts
#' for the horizon it is filed under where they overlap, and for another one
#' when it lies mostly within it, weighed by the overlap (a sample with no
#' usable depth counts for the horizon it is filed under). Codes are matched
#' whatever their case, the "not recorded" methods included. A value below
#' the detection limit, written "<x", is read as half the limit, unless the
#' limit lies above a limit the keys read for that property (0.5 \% CaCO3,
#' 0.18 cmol/kg Na, an ESP of 6, 5 \% free Fe, 12 \% organic C, 10 \% clay),
#' where the value is unknown and the next method is tried. pH in water is
#' the lab 1:5 value (method 4A1, or a pH whose method is not recorded, often
#' the field pH itself); the field pH is used only where the lab has no pH
#' at all. Organic C is the measured value, or an uncorrected Walkley-Black
#' value times 1.3; half the loss on ignition only in peat (an O or P
#' horizon, or a peat field texture). Exchangeable bases and CEC are read
#' whatever the extraction method; a laboratory ESP is kept as
#' \code{esp_pct}. Free iron is read only from dithionite extractions; most
#' profiles have none, which leaves the Ferrosols undecided.
#'
#' The site keeps the surveyors' classification
#' (\code{reference_asc_order}, \code{reference_asc_suborder},
#' \code{reference_asc_great_group}, \code{reference_asc_subgroup}, the
#' confidence \code{reference_asc_confidence}), the drainage class, the
#' surface condition, the microrelief, coordinates (GDA94), the licence and
#' the citation.
#'
#' @param path A folder (searched recursively for \code{.json} and
#'   \code{.json.gz} files) or a vector of files.
#' @param classified_only Keep only sites with an ASC order.
#' @param max_n Optional maximum number of files read (a random sample;
#'   a profile file of \code{\link{fetch_natsoil}} holds one site).
#' @param seed Optional seed for that sample.
#' @param verbose Print a summary.
#' @return A named list of \code{\link{PedonRecord}} objects (names are
#'   \code{agency_project_site}).
#' @references CSIRO. CSIRO National Soil Site Database (NatSoil).
#'   https://doi.org/10.25919/sg5w-ny87
#' @seealso \code{\link{fetch_natsoil}}, \code{\link{benchmark_natsoil}}.
#' @export
load_natsoil_pedons <- function(path, classified_only = TRUE, max_n = NULL, seed = NULL,
                                verbose = TRUE) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("load_natsoil_pedons() needs the 'jsonlite' package.", call. = FALSE)
  files <- if (length(path) == 1L && dir.exists(path)) {
    list.files(file.path(path, if (dir.exists(file.path(path, "profiles"))) "profiles" else ""),
               pattern = "\\.json(\\.gz)?$", recursive = TRUE, full.names = TRUE)
  } else path
  # the site pages of fetch_natsoil() hold no horizons
  pages <- grepl("(^|[/\\\\])page_s?[0-9_]+\\.json(\\.gz)?$", files)
  if (any(pages) && verbose)
    message(sprintf("NatSoil: %d site page files skipped (they hold no horizons; read the profile files)",
                    sum(pages)))
  files <- files[!pages]
  if (!is.null(max_n) && length(files) > max_n) {
    if (!is.null(seed)) set.seed(seed)
    files <- sample(files, max_n)
  }
  out <- list(); bad <- 0L; skipped <- 0L; failed <- 0L
  for (f in files) {
    recs <- tryCatch(.natsoil_read_json(f), error = function(e) NULL)
    if (is.null(recs)) { bad <- bad + 1L; next }
    if (!is.null(names(recs))) recs <- list(recs)
    for (s in recs) {
      p <- tryCatch(.natsoil_site_to_pedon(s, classified_only), error = function(e) e)
      if (inherits(p, "error")) { failed <- failed + 1L; next }
      if (is.null(p)) { skipped <- skipped + 1L; next }
      out[[p$site$id]] <- p
    }
  }
  if (verbose) message(sprintf(paste("NatSoil: %d pedons from %d files (%d sites left out: no licence,",
                                     "no ASC order or no horizons; %d failed; %d unreadable files)"),
                               length(out), length(files), skipped, failed, bad))
  out
}

# NatSoil code tables (Australian Soil and Land Survey Field Handbook).
.NATSOIL_CODES <- list(
  boundary = c(A = "abrupt", C = "clear", D = "diffuse", G = "gradual", S = "sharp"),
  grade = c(G = "structureless", V = "structureless", W = "weak", M = "moderate", S = "strong"),
  grade_type = c(G = "single-grain", V = "massive"),
  ped_type = c(AB = "angular blocky", CA = "cast", CO = "columnar", GR = "granular", LE = "lenticular",
               PL = "platy", PO = "polyhedral", PR = "prismatic", SB = "subangular blocky"),
  effervescence = c(N = "none", S = "slight", M = "moderate", H = "strong", V = "very strong"),
  drainage = c("1" = "very poorly drained", "2" = "poorly drained", "3" = "imperfectly drained",
               "4" = "moderately well drained", "5" = "well drained", "6" = "rapidly drained"),
  cf_mid = c("0" = 0, "1" = 1, "2" = 6, "3" = 15, "4" = 35, "5" = 70, "6" = 95),
  seg_mid = c("0" = 0, "1" = 1, "2" = 6, "3" = 15, "4" = 35, "5" = 60),
  cementation = c("0" = NA, "1" = "weakly", "2" = "moderately", "3" = "strongly", "4" = "indurated"),
  crack_cm = c("1" = 0.2, "2" = 0.5, "3" = 1, "4" = 2, "5" = 5),
  cutan_abun = c("0" = "absent", "1" = "few", "2" = "common", "3" = "many"),
  water = c(D = "dry", T = "moderately moist", M = "moist", W = "wet"),
  plasticity = c(N = "none", U = "subplastic", T = "strongly subplastic", S = "superplastic")
)

.natsoil_map <- function(v, table) {
  v <- as.character(v)
  if (length(v) != 1L || is.na(v) || !v %in% names(.NATSOIL_CODES[[table]])) return(NA_character_)
  unname(.NATSOIL_CODES[[table]][[v]])
}

# The samples of an observation: their depths (metres), the horizon they
# are filed under, and their results by code (upper case), with the limit of
# any "<x" value.
.natsoil_samples <- function(hz) {
  num <- function(v) suppressWarnings(as.numeric(v))
  out <- list()
  for (k in seq_along(hz)) for (smp in hz[[k]]$z_samples %||% list()) {
    vals <- list(); lim <- list()
    for (r in smp$z_lab_results %||% list()) {
      code <- toupper(as.character(.ns(r, "labm_code"))); v <- num(.ns(r, "labr_value"))
      if (is.na(code) || is.na(v)) next
      if (identical(trimws(as.character(.ns(r, "labr_value_prefix") %|NA|% "")), "<")) {
        lim[[code]] <- max(lim[[code]] %||% -Inf, v); v <- v / 2
      }
      vals[[code]] <- c(vals[[code]], v)
    }
    out[[length(out) + 1L]] <- list(top = num(.ns(smp, "samp_upper_depth")), bot = num(.ns(smp, "samp_lower_depth")),
                                    link = k, vals = lapply(vals, mean), lim = lim)
  }
  out
}

# The laboratory results of horizon k. A sample counts for the horizon it is
# filed under where they overlap, and for another horizon when it lies
# mostly (half or more of its depth) within it; it is weighed by the
# overlap. A sample with no usable depth (none, a point, or reversed) counts
# for the horizon it is filed under, weighed by that horizon's thickness;
# when the horizon has no depth, every sample filed under it counts alike.
# Returns the weighted mean of each code and the largest "<x" limit that
# entered it.
.natsoil_lab <- function(samples, k, top, bot) {
  vals <- list(); lim <- list()
  h_ok <- !is.na(top) && !is.na(bot) && bot > top
  for (smp in samples) {
    s_ok <- !is.na(smp$top) && !is.na(smp$bot) && smp$bot > smp$top
    w <- if (!h_ok || !s_ok) {
      if (smp$link == k) (if (h_ok) bot - top else 1) else 0
    } else {
      ov <- max(0, min(bot, smp$bot) - max(top, smp$top))
      if (smp$link == k || ov >= 0.5 * (smp$bot - smp$top)) ov else 0
    }
    if (w <= 0) next
    for (code in names(smp$vals)) {
      vals[[code]] <- rbind(vals[[code]], c(smp$vals[[code]], w))
      if (!is.null(smp$lim[[code]])) lim[[code]] <- max(lim[[code]] %||% -Inf, smp$lim[[code]])
    }
  }
  list(value = vapply(vals, function(m) sum(m[, 1] * m[, 2]) / sum(m[, 2]), 0), lim = lim)
}

# The first code of `codes` with a usable result, times `factor`; a result
# below a detection limit that, times `factor`, lies above the limit the keys
# read for `attr` is not usable (the next code is tried).
.natsoil_first <- function(lab, codes, attr = NULL, factor = 1) {
  cap <- if (is.null(attr)) NA else .NATSOIL_CENSOR_LIMIT[attr]
  for (code in toupper(codes)[toupper(codes) %in% names(lab$value)]) {
    if (!is.na(cap) && !is.null(lab$lim[[code]]) && lab$lim[[code]] * factor > cap) next
    return(unname(lab$value[[code]]) * factor)
  }
  NA_real_
}

.natsoil_horizon <- function(h, lab) {
  get <- function(attr) .natsoil_first(lab, .NATSOIL_LAB[[attr]]$codes, attr)
  st <- h$z_structures %||% list()
  st1 <- if (length(st)) st[[1L]] else list()
  grade_code <- .ns(st1, "str_ped_grade")
  stype <- .natsoil_map(grade_code, "grade_type")
  if (is.na(stype)) stype <- .natsoil_map(.ns(st1, "str_ped_type"), "ped_type")
  if (any(vapply(st, function(z) identical(.ns(z, "str_ped_type"), "LE"), NA))) stype <- "lenticular"
  # colours: the moist ones to the moist columns, the dry ones to the dry
  cols <- h$z_colours %||% list()
  pick <- function(stat) { z <- Filter(function(z) identical(.ns(z, "col_moisture_stat"), stat), cols)
                           if (length(z)) z[[1L]] else list() }
  cm <- pick("M"); cd <- pick("D")
  # a colour with no moisture status is read as moist (the standard
  # condition of the Field Handbook) when no moist colour is recorded
  if (!length(cm)) cm <- { z <- Filter(function(z) is.na(.ns(z, "col_moisture_stat")), cols)
                           if (length(z)) z[[1L]] else list() }
  # slickensides (cutan type K) at their abundance (few when not given, none
  # when given as 0); other cutans recorded and no slickensides: absent
  cut <- h$z_cutans %||% list()
  slk <- vapply(cut, function(z) identical(.ns(z, "cutan_type"), "K"), NA)
  slick <- if (any(slk)) (.natsoil_map(.ns(cut[[which(slk)[1L]]], "cutan_abun"), "cutan_abun") %|NA|% "few")
           else if (length(cut)) "absent" else NA_character_
  cracks <- h$z_cracks %||% list()
  cw <- suppressWarnings(max(vapply(cracks, function(z) {
    k <- as.character(.ns(z, "crack_width")); if (!is.na(k) && k %in% names(.NATSOIL_CODES$crack_cm)) .NATSOIL_CODES$crack_cm[[k]] else NA_real_
  }, 0), na.rm = TRUE))
  if (!is.finite(cw)) cw <- NA_real_
  # coarse fragments: the abundance classes at their middle, added up and
  # capped at 100 %; a record with no abundance leaves the total unknown
  cfs <- h$z_coarse_frags %||% list()
  cf_each <- vapply(cfs, function(z) {
    k <- as.character(.ns(z, "cf_abun")); if (!is.na(k) && k %in% names(.NATSOIL_CODES$cf_mid)) .NATSOIL_CODES$cf_mid[[k]] else NA_real_
  }, 0)
  cf <- if (!length(cfs) || anyNA(cf_each)) NA_real_ else min(100, sum(cf_each))
  # calcareous segregations: a record with only its nature (no abundance,
  # form or size) is a template, not an observation; one with a form or size
  # but no abundance is read as very few (1 %)
  segs <- Filter(function(z) !is.na(.ns(z, "seg_abun")) || !is.na(.ns(z, "seg_form")) || !is.na(.ns(z, "seg_size")),
                 h$z_segregations %||% list())
  kseg <- Filter(function(z) identical(.ns(z, "seg_nature"), "K"), segs)
  seg_k <- if (length(kseg)) max(vapply(kseg, function(z) {
    k <- as.character(.ns(z, "seg_abun")); if (!is.na(k) && k %in% names(.NATSOIL_CODES$seg_mid)) .NATSOIL_CODES$seg_mid[[k]] else 1
  }, 0)) else if (length(segs)) 0 else NA_real_
  pans <- h$z_pans %||% list()
  cem <- if (length(pans)) {
    lv <- suppressWarnings(max(vapply(pans, function(z) suppressWarnings(as.integer(.ns(z, "pan_cementation"))), 0L), na.rm = TRUE))
    if (is.finite(lv)) .natsoil_map(lv, "cementation") else NA_character_
  } else NA_character_
  # pH: the lab value in water; the field pH only where the lab has no pH
  ph <- get("ph_h2o"); ph_ca <- get("ph_cacl2")
  if (is.na(ph) && is.na(ph_ca)) {
    phs <- h$z_phs %||% list()
    if (length(phs)) ph <- suppressWarnings(as.numeric(.ns(phs[[1L]], "ph_value")))
  }
  tex <- .ns(h, "h_texture"); q <- .ns(h, "h_texture_qual")
  master <- toupper(as.character(.ns(h, "h_desig_master") %|NA|% ""))
  # organic C: measured, or uncorrected Walkley-Black x 1.3; half the loss
  # on ignition only in peat (elsewhere it carries water and carbonate)
  oc <- get("oc_pct")
  if (is.na(oc)) oc <- .natsoil_first(lab, "6A1_UC", "oc_pct", factor = 1.3)
  peat <- master %in% c("O", "P") || identical(.asc_texture(tex)$grade, "peat")
  if (is.na(oc) && peat) oc <- .natsoil_first(lab, "2_LOI", "oc_pct", factor = 0.5)
  fe <- get("fe_dcb_pct")
  if (is.na(fe)) fe <- .natsoil_first(lab, "13C1_FE203", "fe_dcb_pct", factor = 0.6994)
  top <- suppressWarnings(as.numeric(.ns(h, "h_depth_upper"))); bot <- suppressWarnings(as.numeric(.ns(h, "h_depth_lower")))
  num <- function(z, k) suppressWarnings(as.numeric(.ns(z, k)))
  data.frame(
    designation = {
      d <- paste0(.ns(h, "h_desig_num_pref") %|NA|% "", .ns(h, "h_desig_master") %|NA|% "",
                  .ns(h, "h_desig_subdiv") %|NA|% "", .ns(h, "h_desig_suffix") %|NA|% "")
      if (nzchar(d)) d else NA_character_
    },
    top_cm = round(100 * top, 4), bottom_cm = round(100 * bot, 4),     # metres to cm
    field_texture = if (is.na(tex)) NA_character_ else paste0(tex, if (!is.na(q) && q %in% c("+", "-")) q else ""),
    boundary_distinctness = .natsoil_map(.ns(h, "h_bound_distinct"), "boundary"),
    structure_grade = .natsoil_map(grade_code, "grade"), structure_type = stype,
    munsell_hue_moist = as.character(.ns(cm, "col_hue")), munsell_value_moist = num(cm, "col_value"),
    munsell_chroma_moist = num(cm, "col_chroma"),
    munsell_hue_dry = as.character(.ns(cd, "col_hue")), munsell_value_dry = num(cd, "col_value"),
    munsell_chroma_dry = num(cd, "col_chroma"),
    effervescence_class = .natsoil_map(.ns(h, "h_carbonate_eff"), "effervescence"),
    slickensides = slick, cracks_width_cm = cw, coarse_fragments_pct = cf,
    secondary_carbonates_pct = seg_k, cementation_class = cem,
    subplasticity = .natsoil_map(.ns(h, "h_plasticity_type"), "plasticity"),
    soil_water_status = .natsoil_map(.ns(h, "h_soil_water_stat"), "water"),
    clay_pct = get("clay_pct"), ph_h2o = ph, ph_cacl2 = ph_ca,
    na_cmol = get("na_cmol"), ca_cmol = get("ca_cmol"), mg_cmol = get("mg_cmol"), k_cmol = get("k_cmol"),
    cec_cmol = get("cec_cmol"), ecec_cmol = get("ecec_cmol"), esp_pct = get("esp_pct"),
    fe_dcb_pct = fe, oc_pct = oc, caco3_pct = get("caco3_pct"),
    stringsAsFactors = FALSE)
}

.natsoil_site_to_pedon <- function(s, classified_only = TRUE) {
  if (!.natsoil_licensed(s)) return(NULL)
  o <- .natsoil_observation(s, classified = classified_only)
  if (is.null(o)) return(NULL)
  hz <- o$z_horizons %||% list()
  if (!length(hz)) return(NULL)
  samples <- .natsoil_samples(hz)
  h <- do.call(rbind, lapply(seq_along(hz), function(k) {
    top <- suppressWarnings(as.numeric(.ns(hz[[k]], "h_depth_upper")))
    bot <- suppressWarnings(as.numeric(.ns(hz[[k]], "h_depth_lower")))
    .natsoil_horizon(hz[[k]], .natsoil_lab(samples, k, top, bot))
  }))
  txt <- function(z, key, field) {
    v <- vapply(z, function(e) tolower(as.character(.ns(e, key))), "")
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v)) paste(v, collapse = "; ") else NA_character_
  }
  code <- function(k) { v <- trimws(as.character(.ns(o, k))); if (is.na(v) || !nzchar(v)) NA_character_ else v }
  lat <- suppressWarnings(as.numeric(.ns(o, "o_latitude_GDA94") %|NA|% .ns(o, "o_latitude")))
  lon <- suppressWarnings(as.numeric(.ns(o, "o_longitude_GDA94") %|NA|% .ns(o, "o_longitude")))
  site <- list(
    id = paste(.ns(s, "agency_code"), .ns(s, "proj_code"), .ns(s, "s_id"), sep = "_"),
    lat = lat, lon = lon, crs = "EPSG:4283", country = "AU", designation_system = "asc",
    date = as.character(.ns(o, "o_date_desc")),
    drainage_class = .natsoil_map(.ns(o, "o_drainage"), "drainage"),
    surface_condition = txt(o$z_surf_conditions %||% list(), "scon_stat_desc"),
    microrelief_form = txt(o$z_micro_reliefs %||% list(), "mr_type_desc"),
    reference_asc_order = code("o_asc_ord"), reference_asc_suborder = code("o_asc_subord"),
    reference_asc_great_group = code("o_asc_gg"), reference_asc_subgroup = code("o_asc_subg"),
    reference_asc_confidence = code("o_asc_conf"),
    licence = as.character(.ns(s, "usage_license")), citation = as.character(.ns(s, "citation")),
    source = "NatSoil (CSIRO)")
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

# The ASC order codes and names.
.ASC_ORDER_CODES <- c(AN = "Anthroposol", OR = "Organosol", PO = "Podosol", VE = "Vertosol",
                      HY = "Hydrosol", KU = "Kurosol", SO = "Sodosol", CH = "Chromosol",
                      CA = "Calcarosol", FE = "Ferrosol", DE = "Dermosol", KA = "Kandosol",
                      RE = "Arenosol", RU = "Rudosol", TE = "Tenosol")

#' Benchmark the Australian order key on NatSoil
#'
#' Classifies NatSoil pedons with \code{\link{classify_asc3}} and compares
#' the order with the one the surveyors gave (\code{site$reference_asc_order}).
#' A pedon is decided when no order before the assigned one was left
#' undecided for missing data; the agreement is given for all pedons and for
#' the decided ones. The surveys predate the 3rd edition's Arenosols (2021),
#' whose soils earlier editions placed in the Tenosols and Rudosols: the
#' \emph{edition} figures count an Arenosol given to a surveyed Tenosol or
#' Rudosol as agreement.
#'
#' @param pedons A list of \code{\link{PedonRecord}} from
#'   \code{\link{load_natsoil_pedons}}.
#' @param cores Number of processes (\code{parallel::mclapply}; always 1
#'   on Windows).
#' @param verbose Print a summary.
#' @return A list: \code{n}; \code{accuracy} and \code{accuracy_edition};
#'   \code{decided} (count), \code{decided_accuracy} and
#'   \code{decided_accuracy_edition}; \code{by_confidence} (a data frame by
#'   the surveyors' confidence, 1 = all necessary analytical data); \code{recall}
#'   by surveyed order; \code{confusion} (rows the survey, columns soilKey);
#'   \code{first_undecided} (the first order left undecided, among
#'   provisional pedons); and \code{results}, one row per pedon.
#' @seealso \code{\link{load_natsoil_pedons}}, \code{\link{classify_asc3}}.
#' @export
benchmark_natsoil <- function(pedons, cores = 1L, verbose = TRUE) {
  one <- function(p) {
    ref <- toupper(as.character(p$site$reference_asc_order %||% NA))
    r <- tryCatch(classify_asc3(p, on_missing = "silent"), error = function(e) NULL)
    pred <- if (is.null(r)) NA_character_ else names(.ASC_ORDER_CODES)[match(r$name, .ASC_ORDER_CODES)]
    prov <- if (is.null(r)) NA_character_
            else paste(sub("^orders:", "", grep("^orders:", r$provisional_because, value = TRUE)), collapse = ",")
    data.frame(id = p$site$id, reference = ref, predicted = pred, provisional = prov,
               confidence = as.character(p$site$reference_asc_confidence %||% NA), stringsAsFactors = FALSE)
  }
  cores <- if (.Platform$OS.type == "windows") 1L else max(1L, as.integer(cores))
  rows <- if (cores > 1L) parallel::mclapply(pedons, one, mc.cores = cores) else lapply(pedons, one)
  res <- do.call(rbind, rows)
  res <- res[res$reference %in% names(.ASC_ORDER_CODES), , drop = FALSE]
  res$decided <- !is.na(res$provisional) & !nzchar(res$provisional)
  res$agree <- !is.na(res$predicted) & res$predicted == res$reference
  res$agree_edition <- res$agree | (res$predicted %in% "RE" & res$reference %in% c("TE", "RU"))
  pct <- function(x) if (length(x)) 100 * mean(x) else NA_real_
  conf <- sort(unique(res$confidence[!is.na(res$confidence)]))
  by_conf <- do.call(rbind, lapply(conf, function(k) {
    s <- res$confidence %in% k
    data.frame(confidence = k, n = sum(s), accuracy = pct(res$agree[s]), decided = sum(s & res$decided),
               decided_accuracy = pct(res$agree[s & res$decided]),
               decided_accuracy_edition = pct(res$agree_edition[s & res$decided]))
  }))
  lev <- names(.ASC_ORDER_CODES)
  out <- list(
    n = nrow(res), accuracy = pct(res$agree), accuracy_edition = pct(res$agree_edition),
    decided = sum(res$decided), decided_accuracy = pct(res$agree[res$decided]),
    decided_accuracy_edition = pct(res$agree_edition[res$decided]),
    by_confidence = by_conf,
    recall = vapply(lev[lev %in% res$reference], function(k) pct(res$agree[res$reference == k]), 0),
    confusion = table(survey = factor(res$reference, lev), soilKey = factor(res$predicted, lev)),
    first_undecided = sort(table(sub(",.*$", "", res$provisional[!res$decided & !is.na(res$provisional)])),
                           decreasing = TRUE),
    results = res)
  if (verbose) {
    message(sprintf(paste("NatSoil, ASC orders: %d pedons, agreement %.1f %% (%.1f %% counting the",
                          "Arenosols as Tenosols or Rudosols); %d decided, %.1f %% (%.1f %%)"),
                    out$n, out$accuracy, out$accuracy_edition, out$decided,
                    out$decided_accuracy, out$decided_accuracy_edition))
  }
  out
}
