# =============================================================================
# v0.9.320 -- SALI, the Soil and Land Information system of Queensland, read
# from its public services: the site layer of Queensland Spatial (MapServer
# layer 101: every site with an ASC label and laboratory data), the per-site
# report (SALI4320, a PDF: the only place the morphology is published) and
# the SALI soil chemistry API (OData Samples with their results). The data
# are CC BY 4.0 (State of Queensland); they are not shipped with soilKey.
#
# The report is read from the coordinates of its words (pdftools), never from
# its text layout, and each row of the morphology table is translated into
# the coded horizon of the NatSoil API (Field Handbook codes), so that the
# horizons and the laboratory samples go through the NatSoil reader's own
# .natsoil_horizon() and .natsoil_lab(): both Australian panels are read by
# the same conventions. Hugo's decisions of 2026-10-30 (MIR predictions only
# where nothing was measured, the report's lab table only when the API holds
# nothing, a 150 KB cap per report) and the pilot's readings are documented
# in read_sali_pedons().
# =============================================================================

.SALI_LAYER_URL <- paste0("https://spatial-gis.information.qld.gov.au/arcgis/rest/services/",
                          "GeoscientificInformation/SoilsAndLandResource/MapServer/101")
.SALI_REPORT_URL <- "https://resources.information.qld.gov.au/soils/reports/sites"
.SALI_ODATA_URL <- "https://soil-chem.information.qld.gov.au/odata/"
.SALI_LAYER_WHERE <- "australian_soil_classification IS NOT NULL AND laboratory_data_available='Yes'"
.SALI_UA <- "soilKey (R package; https://github.com/HugoMachadoRodrigues/soilKey)"
.SALI_LICENCE <- "https://creativecommons.org/licenses/by/4.0/"

# The hosts fetch_sali() talks to: the layer (Queensland Spatial), and per
# site the report host and the soil chemistry API.
.SALI_HOSTS_RE <- paste0("^https://(spatial-gis\\.information\\.qld\\.gov\\.au/arcgis/rest/services/",
                         "GeoscientificInformation/SoilsAndLandResource/MapServer/101",
                         "|resources\\.information\\.qld\\.gov\\.au/soils/reports/",
                         "|soil-chem\\.information\\.qld\\.gov\\.au/odata/)")

# A page asking for a login, a key or the acceptance of terms.
.SALI_BLOCK_RE <- "log ?in|sign ?in|password|api[ _-]?key|accept (the )?terms|terms (and|&) conditions"

#' Citation for data read from Queensland's SALI
#'
#' Returns the attribution for the soil site data of Queensland's Soil and
#' Land Information system (SALI) read by \code{\link{fetch_sali}} and
#' \code{\link{read_sali_pedons}}: the site layer of Queensland Spatial, the
#' site listing reports (SALI4320) and the SALI soil chemistry API. The data
#' are licensed under CC BY 4.0; the State makes no representations or
#' warranties about them.
#'
#' @param accessed Optional date the data were downloaded (its year is the
#'   year of the attribution; the current year when \code{NULL}).
#' @return A character string.
#' @examples
#' sali_citation()
#' @export
sali_citation <- function(accessed = NULL) {
  year <- format(if (is.null(accessed)) Sys.Date() else as.Date(accessed), "%Y")
  paste0("(c) State of Queensland (Department of Environment, Tourism, Science and Innovation), ", year,
         ". Soil and Land Information (SALI): Queensland soil survey sites (Queensland Spatial, ",
         "SoilsAndLandResource MapServer layer 101), site listing reports (SALI4320) and the SALI ",
         "soil chemistry API", if (!is.null(accessed)) paste0(", downloaded on ", format(as.Date(accessed), "%Y-%m-%d")),
         ". Based on or contains data provided by the State of Queensland ", year,
         ". Licensed under CC BY 4.0 (", .SALI_LICENCE, ").")
}

# ---- Download ------------------------------------------------------------------

#' Download Queensland SALI sites, reports and laboratory data
#'
#' Reads Queensland's Soil and Land Information system (SALI) from its
#' public services and saves what they return in \code{dir}: the site layer
#' (Queensland Spatial, MapServer layer 101, the sites with an Australian
#' Soil Classification label and laboratory data, one JSON file per page in
#' \code{dir/layer}), then for each site its report (the SALI4320 site
#' listing report, a PDF, in \code{dir/pdf/<PROJECT>_<SITE>.pdf}) and its
#' laboratory samples with their results from the SALI soil chemistry API
#' (OData \code{Samples} with \code{SiteLabMethodResults}, in
#' \code{dir/lab/<PROJECT>_<SITE>_samples.json}). \code{\link{read_sali_pedons}}
#' reads the folder.
#'
#' Some reports embed full-size site photographs (one was over 50 MB): a
#' report is downloaded only up to \code{cap_bytes}; a larger one is aborted
#' as soon as it passes the cap, marked by a file
#' \code{<PROJECT>_<SITE>.pdf.oversize} and not tried again. Files already
#' in \code{dir} (and oversize marks) are not downloaded again, so an
#' interrupted run can be resumed; a failed request is tried again on the
#' next run. Each request waits \code{pause} seconds, retries with a growing
#' backoff, and the run stops (an error) on any page that asks for a login,
#' a key or the acceptance of terms, or that a web application firewall
#' rejected. Requests go only to the layer of Queensland Spatial and, per
#' site, to the report host and the soil chemistry API; the report address
#' is built from the project code and site number, not taken from the data.
#'
#' The data are licensed under CC BY 4.0; cite them as
#' \code{\link{sali_citation}} says.
#'
#' @param dir Folder for the downloaded files (created when missing).
#' @param sites Optional sites to download: a data frame with
#'   \code{project_code} and \code{site_id}, or a character vector of
#'   \code{"<PROJECT>_<SITE>"}. By default every site of the layer whose
#'   label names an ASC order (the layer's "Unclassified" sites, which
#'   \code{\link{read_sali_pedons}} leaves out, are not downloaded).
#' @param cap_bytes Largest report downloaded, in bytes (150 KB).
#' @param workers Number of processes downloading at once
#'   (\code{parallel::mclapply}, each taking every \code{workers}-th site;
#'   always 1 on Windows).
#' @param pause Seconds to wait after each request (in each process).
#' @param max_sites Maximum number of sites downloaded in this call.
#' @param verbose Print progress.
#' @return Invisibly, a data frame of the sites selected: \code{project_code},
#'   \code{site_id}, \code{asc_label}, \code{stem}, \code{pdf} (\code{"ok"},
#'   \code{"oversize"}, \code{"failed"} or \code{NA} when not attempted in
#'   this call) and \code{lab} (\code{"ok"}, \code{"failed"} or \code{NA}).
#' @references Queensland Government. Soil and Land Information (SALI).
#'   https://www.qld.gov.au/environment/land/soil/soil-data
#' @seealso \code{\link{read_sali_pedons}}, \code{\link{benchmark_sali}},
#'   \code{\link{sali_citation}}.
#' @examples
#' \dontrun{
#' fetch_sali("sali", max_sites = 20)
#' peds <- read_sali_pedons("sali")
#' benchmark_sali(peds)
#' }
#' @export
fetch_sali <- function(dir, sites = NULL, cap_bytes = 150e3, workers = 1L, pause = 1,
                       max_sites = Inf, verbose = TRUE) {
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE))
    stop("fetch_sali() needs the 'httr' and 'jsonlite' packages.", call. = FALSE)
  for (d in c("layer", "pdf", "lab")) dir.create(file.path(dir, d), recursive = TRUE, showWarnings = FALSE)
  .sali_layer_pass(dir, pause, verbose)
  lay <- .sali_layer_table(dir)
  sel <- .sali_select(lay, sites, verbose)
  if (is.null(sites)) sel <- sel[.sali_order_code(sel$asc_label) %in% names(.ASC_ORDER_CODES), , drop = FALSE]
  sel$pdf <- NA_character_; sel$lab <- NA_character_
  done <- function(i) {
    f <- .sali_files(dir, sel$stem[i])
    (file.exists(f[["pdf"]]) || file.exists(f[["oversize"]])) && file.exists(f[["lab"]])
  }
  todo <- which(!vapply(seq_len(nrow(sel)), done, NA))
  if (is.finite(max_sites)) todo <- utils::head(todo, max_sites)
  if (verbose) message(sprintf("SALI: %d sites selected, %d to download (cap %.0f KB per report)",
                               nrow(sel), length(todo), cap_bytes / 1e3))
  workers <- if (.Platform$OS.type == "windows") 1L else max(1L, as.integer(workers))
  run <- function(idx) {
    out <- vector("list", length(idx))
    for (j in seq_along(idx)) {
      i <- idx[j]
      out[[j]] <- .sali_fetch_site(sel$project_code[i], sel$site_id[i], dir, cap_bytes, pause)
      if (verbose && j %% 100L == 0L) message(sprintf("  SALI: %d of %d sites", j, length(idx)))
    }
    do.call(rbind, out)
  }
  if (length(todo)) {
    parts <- split(todo, (seq_along(todo) - 1L) %% workers)
    res <- if (workers > 1L && length(parts) > 1L) {
      parallel::mclapply(parts, run, mc.cores = workers, mc.preschedule = TRUE)
    } else lapply(parts, run)
    for (r in res) if (inherits(r, "try-error")) stop(attr(r, "condition") %||% simpleError(as.character(r)))
    st <- do.call(rbind, res)
    m <- match(sel$stem, st$stem)
    sel$pdf[!is.na(m)] <- st$pdf[m[!is.na(m)]]; sel$lab[!is.na(m)] <- st$lab[m[!is.na(m)]]
  }
  have <- vapply(seq_len(nrow(sel)), function(i) {
    f <- .sali_files(dir, sel$stem[i])
    if (file.exists(f[["pdf"]])) "ok" else if (file.exists(f[["oversize"]])) "oversize" else NA_character_
  }, "")
  sel$pdf[is.na(sel$pdf)] <- have[is.na(sel$pdf)]
  lab_have <- file.exists(file.path(dir, "lab", paste0(sel$stem, "_samples.json")))
  sel$lab[is.na(sel$lab) & lab_have] <- "ok"
  if (verbose) message(sprintf("SALI: reports on disk %d, over the cap %d, failed %d; laboratory files %d",
                               sum(sel$pdf %in% "ok"), sum(sel$pdf %in% "oversize"), sum(sel$pdf %in% "failed"),
                               sum(sel$lab %in% "ok")))
  invisible(sel)
}

.sali_stem <- function(project, site) paste0(.natsoil_safe(project), "_", .natsoil_safe(site))

.sali_files <- function(dir, stem) {
  pdf <- file.path(dir, "pdf", paste0(stem, ".pdf"))
  c(pdf = pdf, oversize = paste0(pdf, ".oversize"), lab = file.path(dir, "lab", paste0(stem, "_samples.json")))
}

# A stop that is not retried and not caught by the per-site loop.
.sali_stop <- function(msg) {
  stop(structure(class = c("sali_stop", "error", "condition"),
                 list(message = paste0("SALI: ", msg, "; stopped"), call = NULL)))
}

.sali_host_ok <- function(url) grepl(.SALI_HOSTS_RE, as.character(url))

# Stop (not retry) on a response asking for authentication or acceptance, or
# rejected by the server's firewall.
.sali_guard <- function(status, ctype, text, url) {
  if (status %in% c(401L, 403L, 407L))
    .sali_stop(sprintf("HTTP %d (authentication or permission required) at %s", status, url))
  if (grepl("html", ctype)) {
    if (grepl("Request Rejected", text, fixed = TRUE))
      .sali_stop(sprintf("the server (WAF) rejected the request at %s", url))
    if (grepl(.SALI_BLOCK_RE, text, ignore.case = TRUE))
      .sali_stop(sprintf("the response at %s looks like a login, key or terms page", url))
  }
  invisible(TRUE)
}

# GET with retries (backoff 5, 10, 20, 40 s); returns the body as text.
.sali_get <- function(url, query = NULL, tries = 5L, timeout = 120, expect = NULL) {
  if (!.sali_host_ok(url)) stop("SALI: not a SALI address: ", url, call. = FALSE)
  ua <- httr::user_agent(.SALI_UA)
  last <- NULL
  for (attempt in seq_len(tries)) {
    r <- tryCatch(if (is.null(query)) httr::GET(url, httr::timeout(timeout), ua)
                  else httr::GET(url, query = query, httr::timeout(timeout), ua),
                  error = function(e) e)
    if (!inherits(r, "error")) {
      final <- r$url %||% url
      if (!.sali_host_ok(final)) .sali_stop(sprintf("redirected away from the SALI services (%s)", final))
      ct <- tolower(httr::headers(r)[["content-type"]] %||% "")
      txt <- tryCatch(httr::content(r, as = "text", encoding = "UTF-8"), error = function(e) "")
      .sali_guard(httr::status_code(r), ct, txt, final)
      if (httr::status_code(r) == 200L) {
        if (is.null(expect) || grepl(expect, ct)) return(txt)
        last <- paste("unexpected content type", ct)
      } else last <- paste("HTTP", httr::status_code(r))
    } else last <- conditionMessage(r)
    if (attempt < tries) Sys.sleep(5 * 2^(attempt - 1L))
  }
  stop(sprintf("SALI request failed after %d tries (%s): %s", tries, last, url), call. = FALSE)
}

# A capped download: the body is streamed and the transfer aborted as soon as
# it passes `cap` bytes. Returns list(status = "ok" | "oversize" | "error",
# bytes, data). An oversize body is not retried (it is deterministic).
.sali_get_capped <- function(url, cap, tries = 4L, timeout = 120, expect = "pdf") {
  if (!.sali_host_ok(url)) stop("SALI: not a SALI address: ", url, call. = FALSE)
  last <- NULL
  for (attempt in seq_len(tries)) {
    buf <- list(); n <- 0
    sink <- function(x) {
      n <<- n + length(x)
      if (n > cap) stop("SALI_OVERSIZE", call. = FALSE)
      buf[[length(buf) + 1L]] <<- x
      invisible(TRUE)
    }
    r <- tryCatch(httr::GET(url, httr::timeout(timeout), httr::user_agent(.SALI_UA), httr::write_stream(sink)),
                  error = function(e) e)
    if (inherits(r, "error") && grepl("SALI_OVERSIZE", conditionMessage(r)))
      return(list(status = "oversize", bytes = n, data = NULL))
    if (!inherits(r, "error")) {
      final <- r$url %||% url
      if (!.sali_host_ok(final)) .sali_stop(sprintf("redirected away from the SALI services (%s)", final))
      body <- if (length(buf)) do.call(c, buf) else raw(0)
      ct <- tolower(httr::headers(r)[["content-type"]] %||% "")
      txt <- if (grepl("html", ct)) tryCatch(rawToChar(body), error = function(e) "") else ""
      .sali_guard(httr::status_code(r), ct, txt, final)
      if (httr::status_code(r) == 200L && grepl(expect, ct)) return(list(status = "ok", bytes = n, data = body))
      last <- paste("HTTP", httr::status_code(r), ct)
    } else last <- conditionMessage(r)
    if (attempt < tries) Sys.sleep(5 * 2^(attempt - 1L))
  }
  list(status = "error", bytes = NA_real_, data = NULL, message = last)
}

# Written to a temporary file and renamed: an interrupted write leaves nothing.
.sali_write <- function(x, file) {
  tmp <- paste0(file, ".part")
  if (is.raw(x)) writeBin(x, tmp) else {
    con <- file(tmp, "w", encoding = "UTF-8"); writeLines(x, con, useBytes = TRUE); close(con)
  }
  file.rename(tmp, file)
}

# The layer pass: the count, the page size of the service, then one file per
# page (pages on disk are not fetched again).
.sali_layer_pass <- function(dir, pause = 1, verbose = TRUE) {
  ldir <- file.path(dir, "layer")
  base <- paste0(.SALI_LAYER_URL, "/query")
  cnt <- jsonlite::fromJSON(.sali_get(base, list(where = .SALI_LAYER_WHERE, returnCountOnly = "true", f = "json")))$count
  meta <- jsonlite::fromJSON(.sali_get(.SALI_LAYER_URL, list(f = "json")))
  page <- as.integer(meta$maxRecordCount %||% 1000L)
  if (is.null(cnt) || !is.numeric(cnt)) stop("SALI: the site layer returned no count", call. = FALSE)
  if (verbose) message(sprintf("SALI site layer: %d sites with an ASC label and laboratory data", as.integer(cnt)))
  for (o in if (cnt > 0) seq(0L, as.integer(cnt) - 1L, by = page) else integer(0)) {
    f <- file.path(ldir, sprintf("layer101_off%06d.json", o))
    if (file.exists(f)) next
    txt <- .sali_get(base, list(where = .SALI_LAYER_WHERE, outFields = "*", returnGeometry = "false",
                                orderByFields = "objectid", resultOffset = o, resultRecordCount = page, f = "json"))
    js <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    if (!is.null(js$error)) stop("SALI: the site layer returned an error at offset ", o, ": ",
                                 js$error$message %||% "", call. = FALSE)
    .sali_write(txt, f)
    Sys.sleep(pause)
  }
  invisible(TRUE)
}

# The site table: the layer's JSON pages (as fetch_sali() writes them), or a
# saved data frame layer/layer101.rds.
.sali_layer_table <- function(path) {
  ldir <- file.path(path, "layer")
  pages <- sort(list.files(ldir, pattern = "^layer101_off[0-9]+\\.json$", full.names = TRUE))
  tab <- if (length(pages)) {
    as.data.frame(data.table::rbindlist(lapply(pages, function(f) {
      js <- jsonlite::fromJSON(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
      a <- js$features$attributes
      if (is.null(a) || !NROW(a)) NULL else data.table::as.data.table(a)
    }), fill = TRUE))
  } else if (file.exists(file.path(ldir, "layer101.rds"))) {
    readRDS(file.path(ldir, "layer101.rds"))
  } else stop("SALI: no site layer under ", ldir, " (layer101_off*.json or layer101.rds); run fetch_sali() first",
              call. = FALSE)
  need <- c("project_code", "site_id", "australian_soil_classification")
  if (!all(need %in% names(tab))) stop("SALI: the site layer lacks ", paste(setdiff(need, names(tab)), collapse = ", "),
                                       call. = FALSE)
  if (!is.null(tab$objectid)) tab <- tab[!duplicated(tab$objectid), , drop = FALSE]
  tab$asc_label <- trimws(as.character(tab$australian_soil_classification))
  tab$stem <- .sali_stem(tab$project_code, tab$site_id)
  rownames(tab) <- NULL
  tab
}

# The sites asked for (a data frame with project_code and site_id, or stems).
.sali_select <- function(lay, sites, verbose = TRUE) {
  if (is.null(sites)) return(lay[, intersect(c("project_code", "site_id", "asc_label", "stem"), names(lay))])
  want <- if (is.data.frame(sites)) .sali_stem(sites$project_code, sites$site_id) else as.character(sites)
  miss <- setdiff(want, lay$stem)
  if (length(miss) && verbose)
    message(sprintf("SALI: %d of the sites asked for are not in the site layer (left out)", length(miss)))
  out <- lay[lay$stem %in% want, intersect(c("project_code", "site_id", "asc_label", "stem"), names(lay)), drop = FALSE]
  rownames(out) <- NULL
  out
}

# The ASC order code of a label ("Vertosol Black ..." -> "VE"); NA when the
# label does not begin with an order name ("Unclassified").
.sali_order_code <- function(label) {
  w <- tolower(sub(" .*$", "", trimws(as.character(label))))
  codes <- stats::setNames(names(.ASC_ORDER_CODES), tolower(.ASC_ORDER_CODES))
  unname(codes[w])
}

.sali_fetch_site <- function(pc, sid, dir, cap, pause) {
  stem <- .sali_stem(pc, sid)
  f <- .sali_files(dir, stem)
  pdf <- if (file.exists(f[["pdf"]])) "ok" else if (file.exists(f[["oversize"]])) "oversize" else {
    url <- sprintf("%s?project=%s&site=%s", .SALI_REPORT_URL, utils::URLencode(as.character(pc), reserved = TRUE),
                   as.integer(sid))
    g <- .sali_get_capped(url, cap)
    Sys.sleep(pause)
    if (g$status == "ok") { .sali_write(g$data, f[["pdf"]]); "ok" }
    else if (g$status == "oversize") {
      writeLines(sprintf("over %.0f bytes (cap %.0f): not downloaded", g$bytes, cap), f[["oversize"]]); "oversize"
    } else "failed"
  }
  lab <- if (file.exists(f[["lab"]])) "ok" else {
    filt <- sprintf("ProjectCode eq '%s' and SiteId eq %d", gsub("'", "''", as.character(pc)), as.integer(sid))
    x <- tryCatch(.sali_get(paste0(.SALI_ODATA_URL, "Samples"),
                            list(`$filter` = filt, `$expand` = "SiteLabMethodResults"), expect = "json"),
                  error = function(e) if (inherits(e, "sali_stop")) stop(e) else NULL)
    Sys.sleep(pause)
    ok <- !is.null(x) && !inherits(tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) e), "error")
    if (ok) { .sali_write(x, f[["lab"]]); "ok" } else "failed"
  }
  data.frame(stem = stem, pdf = pdf, lab = lab, stringsAsFactors = FALSE)
}

# ---- The site report (PDF) --------------------------------------------------------

.SALI_SECTIONS <- c("Project description", "Site characteristics", "Site location", "Soil classification",
                    "Vegetation", "Profile morphology", "Notes", "Laboratory test results", "Field test",
                    "Photos", "Site photos", "Photographs", "Observation photos", "Microrelief",
                    "Surface coarse fragments", "Rock outcrop", "Disturbance", "Erosion",
                    "Land use", "Site disturbance")

# The words of the report (the list pdftools::pdf_data() returns, one data
# frame of x, y, width, height and text per page), in one data frame without
# the page headers, with a global y (page offset) and the observation number
# each page header gives ("Project <P> Site <S> Observation <N> ... SALI4320").
.sali_pdf_words <- function(pd) {
  out <- list(); obs <- integer(0)
  for (p in seq_along(pd)) {
    w <- as.data.frame(pd[[p]], stringsAsFactors = FALSE)
    if (!nrow(w)) next
    w <- w[order(w$y, w$x), , drop = FALSE]
    hl <- which(w$text == "Observation" & w$y < 80)
    hy <- if (length(hl)) w$y[hl[1]] else NA
    ob <- if (length(hl)) suppressWarnings(as.integer(w$text[which(w$y == hy & w$x > w$x[hl[1]])[1]])) else NA
    if (!is.na(hy)) w <- w[w$y > hy + 4, , drop = FALSE]
    if (!nrow(w)) next
    w$page <- p; w$obs <- ob; w$gy <- p * 10000 + w$y
    out[[length(out) + 1L]] <- w
    obs <- c(obs, ob)
  }
  list(words = if (length(out)) do.call(rbind, out) else NULL, pages = length(pd), obs = obs)
}

# Lines: words of the same page whose y lie within 1.5 px, joined.
.sali_lines <- function(w) {
  if (is.null(w) || !nrow(w)) return(data.frame(page = integer(0), gy = numeric(0), text = character(0),
                                                x0 = numeric(0)))
  w <- w[order(w$gy, w$x), , drop = FALSE]
  key <- cumsum(c(TRUE, diff(w$gy) > 1.5))
  do.call(rbind, lapply(split(seq_len(nrow(w)), key), function(i)
    data.frame(page = w$page[i[1]], gy = w$gy[i[1]], text = paste(w$text[i], collapse = " "),
               x0 = min(w$x[i]), stringsAsFactors = FALSE)))
}

# Section titles in reading order (gy of each).
.sali_sections <- function(lines) {
  t <- trimws(lines$text)
  hit <- t %in% .SALI_SECTIONS | grepl("^Field test( Field test)*$", t)
  data.frame(title = sub("^(Field test).*$", "\\1", t[hit]), gy = lines$gy[hit], stringsAsFactors = FALSE)
}

.sali_section_range <- function(sec, title, all_gy_end) {
  i <- which(sec$title == title)
  if (!length(i)) return(NULL)
  i <- i[1]
  end <- if (i < nrow(sec)) sec$gy[i + 1] else all_gy_end
  c(sec$gy[i], end)
}

# The two-column "Label: value" block (Site characteristics): the value
# columns start at the typical x of the first word after a label's colon,
# left and right of the label "Observation type:".
.sali_keyvals <- function(w) {
  if (is.null(w) || !nrow(w)) return(list())
  w <- w[order(w$gy, w$x), , drop = FALSE]
  colon <- which(grepl(":$", w$text))
  ot <- which(w$text == "Observation" & c(w$text[-1], "") == "type:")
  rx <- if (length(ot)) w$x[ot[1]] else stats::median(w$x[colon][w$x[colon] > 200]) - 30
  first_after <- function(side) {
    xs <- c()
    for (i in colon) {
      if ((w$x[i] < rx) != side) next
      j <- which(w$gy == w$gy[i] & w$x > w$x[i] & (if (side) w$x < rx else TRUE))
      if (length(j)) xs <- c(xs, min(w$x[j]))
    }
    if (length(xs)) as.numeric(names(sort(table(xs), decreasing = TRUE))[1]) else NA
  }
  v1 <- first_after(TRUE); v2 <- first_after(FALSE)
  if (is.na(v1)) v1 <- rx - 1
  if (is.na(v2)) v2 <- rx + 80
  key <- cumsum(c(TRUE, diff(w$gy) > 1.5)); out <- list()
  cur <- list(L = list(lab = "", val = "", done = FALSE), R = list(lab = "", val = "", done = FALSE))
  flush <- function(side) {
    z <- cur[[side]]
    if (nzchar(z$lab)) out[[sub(":$", "", trimws(z$lab))]] <<- trimws(z$val)
  }
  for (li in split(seq_len(nrow(w)), key)) {
    for (side in c("L", "R")) {
      ii <- li[if (side == "L") w$x[li] < rx else w$x[li] >= rx]
      if (!length(ii)) next
      vx <- if (side == "L") v1 else v2
      lab <- paste(w$text[ii][w$x[ii] < vx - 2], collapse = " ")
      val <- paste(w$text[ii][w$x[ii] >= vx - 2], collapse = " ")
      # a new label: the previous one is complete, or this line carries its
      # own value (a label written without its colon, "Slope type"); a label
      # line with no value continues the previous label ("Surface coarse /
      # fragments:")
      if (nzchar(lab) && (cur[[side]]$done || !nzchar(cur[[side]]$lab) || nzchar(val))) {
        flush(side); cur[[side]] <- list(lab = lab, val = val, done = grepl(":$", lab))
      } else {
        if (nzchar(lab)) cur[[side]]$lab <- paste(cur[[side]]$lab, lab)
        cur[[side]]$done <- cur[[side]]$done || grepl(":$", lab)
        if (nzchar(val)) cur[[side]]$val <- paste(cur[[side]]$val, val)
      }
    }
  }
  flush("L"); flush("R")
  names(out) <- sub(":$", "", names(out))
  out
}

# A table: the header words give the column starts; a row starts at a
# number of the first column (its cells may begin a little above it, `up`);
# a cell wrapped over several lines is joined (glued without spaces for the
# name and the depths).
.sali_cells <- function(w, cols, row_col = 1L, row_re = "^[0-9]+$", up = 4) {
  starts <- vapply(cols, function(z) z$x, 0)
  colof <- function(x) { k <- max(c(0L, which(starts - 3 <= x))); if (k == 0L) 1L else k }
  w$col <- vapply(w$x, colof, 0L)
  anch <- which(w$col == row_col & grepl(row_re, w$text))
  if (!length(anch)) return(NULL)
  anch <- anch[order(w$gy[anch])]
  ys <- w$gy[anch]
  bounds <- c(ys - up, Inf)
  rows <- lapply(seq_along(anch), function(k) {
    ii <- which(w$gy >= bounds[k] & w$gy < bounds[k + 1])
    ii <- ii[order(w$gy[ii], w$x[ii])]
    cells <- vapply(seq_along(cols), function(cc) {
      jj <- ii[w$col[ii] == cc]
      if (!length(jj)) return("")
      if (isTRUE(cols[[cc]]$glue)) paste(w$text[jj], collapse = "") else paste(w$text[jj], collapse = " ")
    }, "")
    names(cells) <- names(cols)
    as.data.frame(as.list(cells), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# The report, read from `pw` (.sali_pdf_words()): the observation numbers of
# its pages, the site characteristics (label -> value), the Soil
# classification row (asc, confidence, reference, buried, gsg, ppf), the
# Profile morphology table (one row per horizon, cell text), the field tests
# (test, depth_m, value) and the notes, all of observation `obs_want`.
.sali_pdf_parse <- function(pw, obs_want = 1L) {
  w <- pw$words; problems <- character(0)
  if (is.null(w)) return(list(pages = pw$pages, problems = "no text"))
  obs_all <- sort(unique(stats::na.omit(pw$obs)))
  if (length(obs_all) > 1L)
    problems <- c(problems, sprintf("observations %s in the report; observation %d read",
                                    paste(obs_all, collapse = ","), obs_want))
  if (obs_want %in% w$obs) w <- w[w$obs %in% obs_want, , drop = FALSE]
  lines <- .sali_lines(w)
  sec <- .sali_sections(lines)
  gy_end <- max(w$gy) + 1
  in_rng <- function(r) if (is.null(r)) w[0, , drop = FALSE] else w[w$gy > r[1] + 1 & w$gy < r[2] - 0.5, , drop = FALSE]
  site <- .sali_keyvals(in_rng(.sali_section_range(sec, "Site characteristics", gy_end)))
  # soil classification: one header line, the class below it
  sc <- in_rng(.sali_section_range(sec, "Soil classification", gy_end))
  asc <- NULL
  if (nrow(sc)) {
    hdr <- sc[sc$gy == min(sc$gy), , drop = FALSE]
    hx <- function(t) { i <- which(hdr$text == t); if (length(i)) hdr$x[i[1]] else NA }
    cols <- list(asc = list(x = min(hdr$x)), confidence = list(x = hx("Confidence")),
                 reference = list(x = hx("ASC")[1]), buried = list(x = hx("Buried")),
                 gsg = list(x = hx("GSG")), ppf = list(x = hx("PPF")))
    # the reference column starts at the last "ASC" of the header ("(ASC)"
    # is another token)
    ia <- which(hdr$text == "ASC"); if (length(ia)) cols$reference$x <- hdr$x[ia[length(ia)]]
    cols <- cols[!is.na(vapply(cols, function(z) z$x, 0))]
    body <- sc[sc$gy > min(sc$gy) + 1, , drop = FALSE]
    if (nrow(body)) {
      starts <- vapply(cols, function(z) z$x, 0)
      body$col <- vapply(body$x, function(x) max(c(1L, which(starts - 3 <= x))), 0L)
      asc <- vapply(seq_along(cols), function(k) {
        s <- body$col == k
        paste(body$text[s][order(body$gy[s], body$x[s])], collapse = " ")
      }, "")
      names(asc) <- names(cols); asc <- as.list(asc)
    }
  }
  # profile morphology
  pm <- in_rng(.sali_section_range(sec, "Profile morphology", gy_end))
  table <- NULL
  if (nrow(pm)) {
    hl <- pm[pm$text == "No" & pm$x < 30, , drop = FALSE]
    if (nrow(hl)) {
      hy <- hl$gy[1]
      hdr <- pm[abs(pm$gy - hy) < 1.5, , drop = FALSE]
      hdr <- hdr[order(hdr$x), , drop = FALSE]
      # header words to columns ("Coarse fragments" is two words; the
      # depths carry "depth" and "(m)" on the lines below)
      hdr <- hdr[!(hdr$text %in% c("fragments", "depth", "(m)")), , drop = FALSE]
      cols <- lapply(seq_len(nrow(hdr)), function(k) list(x = hdr$x[k], glue = hdr$text[k] %in% c("Name", "Upper", "Lower")))
      names(cols) <- make.names(hdr$text, unique = TRUE)
      sub_hdr <- pm[pm$gy > hy & pm$gy < hy + 30 & pm$text %in% c("depth", "(m)"), , drop = FALSE]
      y0 <- if (nrow(sub_hdr)) max(sub_hdr$gy) + 1 else hy + 1
      body <- pm[pm$gy > y0, , drop = FALSE]
      # a header repeated on a later page
      rep_h <- body$gy[body$text == "No" & body$x < 30]
      for (ry in rep_h)
        body <- body[!(abs(body$gy - ry) < 1.5 | (body$gy > ry & body$gy < ry + 30 & body$text %in% c("depth", "(m)"))), ,
                     drop = FALSE]
      table <- .sali_cells(body, cols)
      if (is.null(table)) problems <- c(problems, "morphology table without rows")
    } else problems <- c(problems, "morphology header not found")
  } else problems <- c(problems, "no Profile morphology section")
  # field tests: blocks side by side, each under its "Field test" title
  ft <- NULL
  fi <- which(sec$title == "Field test")
  if (length(fi)) {
    r <- c(sec$gy[fi[1]], if (max(fi) < nrow(sec)) sec$gy[max(fi) + 1] else gy_end)
    fw <- w[w$gy >= r[1] - 0.5 & w$gy < r[2] - 0.5, , drop = FALSE]
    tl <- fw[fw$text == "Field" & c(fw$text[-1], "") == "test", , drop = FALSE]
    bx <- sort(unique(tl$x))
    for (b in seq_along(bx)) {
      lo <- bx[b] - 2; hi <- if (b < length(bx)) bx[b + 1] - 2 else Inf
      bw <- fw[fw$x >= lo & fw$x < hi, , drop = FALSE]
      dh <- bw[bw$text == "Depth", , drop = FALSE]; vh <- bw[bw$text == "Value", , drop = FALSE]
      if (!nrow(dh) || !nrow(vh)) next
      title_w <- bw[bw$gy > min(bw$gy[bw$text == "Field"]) + 1 & bw$gy < dh$gy[1] - 0.5 &
                    !(bw$text %in% c("Depth", "(m)", "Value")), , drop = FALSE]
      title <- paste(title_w$text[order(title_w$gy, title_w$x)], collapse = " ")
      below <- bw[bw$gy > max(dh$gy[1], vh$gy[1]) + 1, , drop = FALSE]
      # depths are numbers; values numbers or codes (effervescence N, S, M, H, V)
      dcol <- below[abs(below$x - dh$x[1]) < 25 & grepl("^[<>]?[0-9.]+$", below$text), , drop = FALSE]
      vcol <- below[abs(below$x - vh$x[1]) < 25 & below$x > dh$x[1] + 25, , drop = FALSE]
      for (k in seq_len(nrow(dcol))) {
        v <- vcol[abs(vcol$gy - dcol$gy[k]) < 1.5, , drop = FALSE]
        ft <- rbind(ft, data.frame(test = title, depth_m = suppressWarnings(as.numeric(dcol$text[k])),
                                   value = if (nrow(v)) v$text[1] else NA_character_, stringsAsFactors = FALSE))
      }
    }
  }
  # notes
  nt <- in_rng(.sali_section_range(sec, "Notes", gy_end))
  notes <- NULL
  if (nrow(nt)) {
    hdr <- nt[nt$gy == min(nt$gy), , drop = FALSE]
    hx <- function(t) { i <- which(hdr$text == t); if (length(i)) hdr$x[i[1]] else NA }
    cols <- list(level = list(x = min(hdr$x)), hname = list(x = hx("Horizon")),
                 hno = list(x = hdr$x[which(hdr$text == "Horizon")[2]] %|NA|% NA), note = list(x = hx("Note")[1]))
    ix <- which(hdr$text == "Note"); if (length(ix)) cols$note$x <- hdr$x[ix[length(ix)]]
    if (!anyNA(vapply(cols, function(z) z$x, 0))) {
      body <- nt[nt$gy > min(nt$gy) + 1, , drop = FALSE]
      notes <- .sali_cells(body, cols, row_col = 1L, row_re = "^(Site|Observation|Horizon|Project)$", up = 1)
    }
  }
  list(obs = obs_all, site = site, asc = asc, table = table, ftests = ft, notes = notes,
       pages = pw$pages, problems = problems, sections = sec$title)
}

# The report's "Laboratory test results" table (samples as columns, methods
# as rows, continued on later pages for more samples) of observation `obs`:
# a data frame of sample, code, value (as printed, "<x" kept, "-" dropped),
# upper_m, lower_m and bulked.
.sali_pdf_lab <- function(pw, obs = 1L) {
  w <- pw$words
  if (is.null(w)) return(NULL)
  w <- w[w$obs %in% obs, , drop = FALSE]
  out <- list()
  started <- FALSE
  for (p in sort(unique(w$page))) {
    x <- w[w$page == p, , drop = FALSE]
    if (!started && !any(x$text == "Laboratory" & c(x$text[-1], "") == "test")) next
    started <- TRUE
    # the disclaimer closes the table
    dis <- x$y[x$text == "Based" & c(x$text[-1], "") == "on"]
    if (length(dis)) x <- x[x$y < min(dis) - 1, , drop = FALSE]
    lab_s <- which(x$text == "Sample" & !(c("", x$text[-nrow(x)]) == "Bulked"))
    if (!length(lab_s)) next
    sy <- x$y[lab_s[1]]; sx <- x$x[lab_s[1]] + x$width[lab_s[1]]
    sn <- x[abs(x$y - sy) < 2 & x$x > sx & grepl("^[0-9]+$", x$text), , drop = FALSE]
    if (!nrow(sn)) next
    cx <- sn$x + sn$width / 2; names(cx) <- sn$text
    half <- if (length(cx) > 1) min(diff(sort(cx))) / 2 else 40
    near <- function(xx, ww) {
      c0 <- xx + ww / 2; k <- which.min(abs(cx - c0))
      if (abs(cx[k] - c0) <= half) names(cx)[k] else NA_character_
    }
    rowvals <- function(y0, xmin) {
      z <- x[abs(x$y - y0) < 2.5 & x$x >= xmin, , drop = FALSE]
      if (!nrow(z)) return(character(0))
      v <- stats::setNames(z$text, vapply(seq_len(nrow(z)), function(i) near(z$x[i], z$width[i]), ""))
      v[!is.na(names(v))]
    }
    up <- x[x$text == "Upper" & x$y > sy & x$y < sy + 40, , drop = FALSE]
    lo <- x[x$text == "Lower" & x$y > sy & x$y < sy + 60, , drop = FALSE]
    bk <- x[x$text == "Bulked" & x$y > sy & x$y < sy + 80, , drop = FALSE]
    xmin <- min(cx) - half
    upv <- if (nrow(up)) rowvals(up$y[1], xmin) else character(0)
    lov <- if (nrow(lo)) rowvals(lo$y[1], xmin) else character(0)
    bkv <- if (nrow(bk)) rowvals(bk$y[1], xmin) else character(0)
    hd <- x[x$text == "Code" & abs(x$y - (if (nrow(bk)) bk$y[1] else sy)) < 3, , drop = FALSE]
    un <- x[x$text == "Units" & abs(x$y - (if (nrow(bk)) bk$y[1] else sy)) < 3, , drop = FALSE]
    if (!nrow(hd)) next
    code_lo <- hd$x[1] - 3; code_hi <- if (nrow(un)) un$x[1] - 3 else hd$x[1] + 60
    y_body <- max(c(sy, up$y[1], lo$y[1], bk$y[1]), na.rm = TRUE) + 3
    incol <- x[x$y > y_body & x$x >= code_lo & x$x < code_hi, , drop = FALSE]
    incol <- incol[order(incol$y, incol$x), , drop = FALSE]
    codes <- incol[grepl("^[0-9P][0-9A-Za-z_/.]*$", incol$text), , drop = FALSE]
    # a code wrapped in its narrow column ("15C1_CE" / "C"): a code-column
    # word on the next line that carries no values continues it
    if (nrow(codes)) for (i in seq_len(nrow(codes))) {
      nxt <- incol[incol$y > codes$y[i] + 2 & incol$y < codes$y[i] + 12, , drop = FALSE]
      if (nrow(nxt) && !length(rowvals(nxt$y[1], xmin)))
        codes$text[i] <- paste0(codes$text[i], nxt$text[1])
    }
    for (i in seq_len(nrow(codes))) {
      v <- rowvals(codes$y[i], xmin)
      v <- v[v != "-" & nzchar(v)]
      if (!length(v)) next
      out[[length(out) + 1L]] <- data.frame(sample = names(v), code = codes$text[i], value = unname(v),
                                            stringsAsFactors = FALSE)
    }
    smp <- unique(names(cx))
    attr_rows <- data.frame(sample = smp, upper_m = suppressWarnings(as.numeric(upv[smp])),
                            lower_m = suppressWarnings(as.numeric(lov[smp])), bulked = unname(bkv[smp]),
                            stringsAsFactors = FALSE)
    out[[length(out) + 1L]] <- structure(attr_rows, kind = "samples")
  }
  if (!length(out)) return(NULL)
  kinds <- vapply(out, function(z) identical(attr(z, "kind"), "samples"), NA)
  vals <- if (any(!kinds)) do.call(rbind, out[!kinds]) else NULL
  smps <- unique(do.call(rbind, out[kinds]))
  if (is.null(vals)) return(NULL)
  merge(vals, smps, by = "sample", all.x = TRUE)
}

# ---- Field Handbook wording -> NatSoil codes -------------------------------------

.sali_abun <- function(s, scale = c("cf", "seg", "mott")) {
  scale <- match.arg(scale)
  s <- tolower(s)
  if (grepl("abundance not recorded", s)) return(NA_character_)
  if (grepl("^no (coarse fragments|segregations|mottles)", s)) return("0")
  if (scale == "cf") {
    if (grepl("^very abundant", s)) return("6")
    if (grepl("^abundant", s)) return("5")
  }
  if (scale == "seg" && grepl("^very many", s)) return("5")
  if (grepl("^very few", s)) return("1")
  if (grepl("^few", s)) return("2")
  if (grepl("^common", s)) return("3")
  if (grepl("^many", s)) return("4")
  NA_character_
}

# The entries of a cell, separated by semicolons.
.sali_entries <- function(s) {
  s <- gsub("\\s+", " ", trimws(s %||% ""))
  if (!nzchar(s)) return(character(0))
  # "(10- 20%)": the report wraps inside parentheses
  s <- gsub("([0-9]+)- ([0-9]+)", "\\1-\\2", s)
  trimws(strsplit(s, ";", fixed = TRUE)[[1]])
}

# Texture names of the report -> Field Handbook codes.
.SALI_TEX <- c(
  "sand" = "S", "fine sand" = "FS", "medium sand" = "MS", "coarse sand" = "KS",
  "loamy sand" = "LS", "loamy fine sand" = "LFS", "loamy medium sand" = "LMS", "loamy coarse sand" = "LKS",
  "clayey sand" = "CS", "clayey fine sand" = "CFS", "clayey medium sand" = "CMS", "clayey coarse sand" = "CKS",
  "sandy loam" = "SL", "fine sandy loam" = "FSL", "medium sandy loam" = "MSL", "coarse sandy loam" = "KSL",
  "loam" = "L", "loam, fine sandy" = "L", "silty loam" = "ZL", "silt loam" = "ZL",
  "sandy clay loam" = "SCL", "fine sandy clay loam" = "FSCL", "medium sandy clay loam" = "MSCL",
  "coarse sandy clay loam" = "KSCL",
  "clay loam" = "CL", "clay loam, sandy" = "CLS", "clay loam, fine sandy" = "CLFS",
  "clay loam, medium sandy" = "CLMS", "clay loam, coarse sandy" = "CLKS", "silty clay loam" = "ZCL",
  "clay" = "C", "sandy clay" = "SC", "silty clay" = "ZC",
  "gravel" = "GR", "stone" = "ST")
.SALI_CLAYS <- c("light clay" = "LC", "light medium clay" = "LMC", "medium clay" = "MC",
                 "medium heavy clay" = "MHC", "heavy clay" = "HC")
.SALI_CLAY_PREF <- c("sandy" = "S", "fine sandy" = "FS", "medium sandy" = "MS", "coarse sandy" = "KS", "silty" = "Z")

# A texture cell -> c(code, qual): "Loam, fine sandy" is L; sandy and silty
# clays are clays of unspecified grade (SC, ZC); heavy or light before a
# grade lighter than a clay is the + or - qualifier; peats by their kind.
.sali_texture <- function(s) {
  t <- tolower(gsub("\\s+", " ", trimws(s %||% "")))
  if (!nzchar(t) || grepl("not recorded", t)) return(c(code = NA_character_, qual = NA_character_))
  if (grepl("peat", t)) {
    k <- if (grepl("sapric", t)) "SP" else if (grepl("hemic", t)) "HP" else if (grepl("fibric", t)) "FP" else "P"
    return(c(code = k, qual = NA_character_))
  }
  if (t %in% names(.SALI_TEX)) return(c(code = unname(.SALI_TEX[t]), qual = NA_character_))
  for (cl in names(.SALI_CLAYS)) {
    if (t == cl) return(c(code = unname(.SALI_CLAYS[cl]), qual = NA_character_))
    for (p in names(.SALI_CLAY_PREF)) if (t == paste(p, cl))
      return(c(code = paste0(.SALI_CLAY_PREF[[p]], .SALI_CLAYS[[cl]]), qual = NA_character_))
  }
  m <- regmatches(t, regexec("^(heavy|light) (.*)$", t))[[1]]
  if (length(m) == 3L && m[3] %in% names(.SALI_TEX) && !(.SALI_TEX[[m[3]]] %in% c("C", "SC", "ZC", "GR", "ST")))
    return(c(code = unname(.SALI_TEX[m[3]]), qual = if (m[2] == "heavy") "+" else "-"))
  c(code = NA_character_, qual = NA_character_)
}

.SALI_PED <- c("angular blocky" = "AB", "subangular blocky" = "SB", "polyhedral" = "PO", "lenticular" = "LE",
               "prismatic" = "PR", "columnar" = "CO", "granular" = "GR", "platy" = "PL", "cast" = "CA")
.SALI_SIZE5 <- c("<2" = "1", "2-5" = "2", "5-10" = "3", "10-20" = "4", "20-50" = "5", "50-100" = "6",
                 "100-200" = "7", "200-500" = "8", ">500" = "9")

# Structures: grade, type ("Blocky" alone names no type) and size class.
.sali_structures <- function(s) {
  lapply(.sali_entries(s), function(e) {
    t <- tolower(e)
    g <- if (grepl("^single grain", t)) "G" else if (grepl("^massive", t)) "V" else if (grepl("^strong", t)) "S"
         else if (grepl("^moderate", t)) "M" else if (grepl("^weak", t)) "W" else NA_character_
    sz <- regmatches(t, regexpr("(<2|>500|[0-9]+-[0-9]+) mm", t))
    sz <- if (length(sz)) .SALI_SIZE5[sub(" mm$", "", sz)] else NA_character_
    ty <- NA_character_
    for (k in names(.SALI_PED)) if (grepl(paste0("\\b", k, "\\b"), t)) { ty <- .SALI_PED[[k]]; break }
    list(str_ped_grade = g, str_ped_type = ty, str_ped_size = unname(sz))
  })
}

.SALI_SEG_NATURE <- c("ferromanganiferous" = "N", "calcareous" = "K", "ferruginous" = "F", "manganiferous" = "M",
                      "gypseous" = "Y", "saline" = "Z", "aluminous" = "A", "argillaceous" = "G",
                      "organic" = "H", "humified" = "H", "sulfurous" = "S", "unidentified" = "U", "other" = "O")
.SALI_SEG_FORM <- c("concretions" = "C", "nodules" = "N", "fragments" = "F", "crystals" = "X",
                    "soft segregations" = "S", "veins" = "V", "root linings" = "R", "tubules" = "T",
                    "laminae" = "L", "residual" = "D")
.SALI_SEG_SIZE <- c("fine" = "1", "medium" = "2", "coarse" = "3", "very coarse" = "4", "extremely coarse" = "5")

.sali_segregations <- function(s) {
  lapply(.sali_entries(s), function(e) {
    t <- tolower(e)
    nat <- NA_character_
    for (k in names(.SALI_SEG_NATURE)) if (grepl(paste0("\\b", k, "\\b"), t)) { nat <- .SALI_SEG_NATURE[[k]]; break }
    frm <- NA_character_
    for (k in names(.SALI_SEG_FORM)) if (grepl(paste0("\\b", k, "\\b"), t)) { frm <- .SALI_SEG_FORM[[k]]; break }
    sz <- regmatches(t, regexpr("(very coarse|extremely coarse|fine|medium|coarse) \\(", t))
    sz <- if (length(sz)) .SALI_SEG_SIZE[[sub(" \\($", "", sz)]] else NA_character_
    list(seg_abun = .sali_abun(t, "seg"), seg_nature = nat, seg_form = frm, seg_size = sz)
  })
}

.SALI_CF_SIZE <- c("small pebbles" = "1", "fine gravel" = "1", "medium pebbles" = "2", "medium gravel" = "2",
                   "large pebbles" = "3", "coarse gravel" = "3", "cobbles" = "4", "stones" = "5",
                   "large boulders" = "7", "boulders" = "6")
.SALI_CF_SHAPE <- "^(angular|subangular|subrounded|rounded|tabular|platy|irregular|spherical|elongate)$"

.sali_coarse <- function(s) {
  lapply(.sali_entries(s), function(e) {
    t <- tolower(e)
    ab <- .sali_abun(t, "cf")
    sz <- NA_character_
    for (k in names(.SALI_CF_SIZE)) if (grepl(paste0("\\b", k, "\\b"), t)) { sz <- .SALI_CF_SIZE[[k]]; break }
    # lithology: what is left once abundance, shape and size are taken out
    rest <- sub("^(very abundant|abundant|very few|few|common|many|no coarse fragments)( \\([^)]*\\))?,? ?", "", t)
    rest <- gsub("\\((size not recorded|[0-9<>. -]+ ?m?m?)\\)", "", rest)
    for (k in names(.SALI_CF_SIZE)) rest <- gsub(paste0("\\b", k, "\\b"), "", rest)
    wd <- strsplit(trimws(gsub("\\s+", " ", rest)), " ")[[1]]
    wd <- wd[!grepl(.SALI_CF_SHAPE, wd) & nzchar(wd)]
    lith_txt <- paste(wd, collapse = " ")
    lith <- if (!nzchar(lith_txt)) NA_character_ else if (grepl("calcrete", lith_txt)) "KC" else lith_txt
    list(cf_abun = ab, cf_size = sz, cf_lith = lith)
  })
}

# Mottles: biological mixing, mechanical mixing and substrate influence are
# types X, Y and Z (not read as mottles); jarosite (J) counts.
.sali_mottles <- function(s) {
  lapply(.sali_entries(s), function(e) {
    t <- tolower(e)
    if (grepl("^no mottles", t)) return(list(mott_type = "M", mott_abun = "0", mott_contrast = NA_character_))
    ty <- if (grepl("biological mixing", t)) "X" else if (grepl("mechanical mixing", t)) "Y"
          else if (grepl("substrate (influence|inclusion)", t)) "Z" else if (grepl("jarosite", t)) "J" else "M"
    ct <- if (grepl("\\bprominent\\b", t)) "P" else if (grepl("\\bdistinct\\b", t)) "D"
          else if (grepl("\\bfaint\\b", t)) "F" else NA_character_
    list(mott_type = ty, mott_abun = .sali_abun(t, "mott"), mott_contrast = ct)
  })
}

.SALI_STRENGTH <- c("very weak" = "1", "very firm" = "4", "very strong" = "6", "loose" = "0", "weak" = "2",
                    "firm" = "3", "strong" = "5", "rigid" = "7")

# Strengths with their moisture ("Dry, firm strength"); one without a
# moisture keeps no moisture status (and is not read by the key).
.sali_strengths <- function(s) {
  out <- list()
  for (e in .sali_entries(s)) {
    t <- tolower(e)
    m <- if (grepl("^moderately moist,", t)) "T" else if (grepl("^moist,", t)) "M" else if (grepl("^dry,", t)) "D"
         else if (grepl("^wet,", t)) "W" else NA_character_
    body <- sub("^[^,]*, ", "", t)
    if (is.na(m)) body <- t
    cl <- NA_character_
    for (k in names(.SALI_STRENGTH)) if (startsWith(body, paste0(k, " strength"))) { cl <- .SALI_STRENGTH[[k]]; break }
    out[[length(out) + 1L]] <- list(strg_moisture_stat = m, strg_class = cl)
  }
  out
}

# Colours ("Dark reddish brown (5YR 3/3) moist", separated by a rule of
# underscores); a colour without its moisture keeps none (NatSoil reads it
# as moist).
.sali_colours <- function(s) {
  s <- gsub("\\s+", " ", trimws(s %||% ""))
  if (!nzchar(s)) return(list())
  parts <- trimws(strsplit(s, "_{3,}")[[1]]); parts <- parts[nzchar(parts)]
  out <- list()
  for (p in parts) {
    m <- regmatches(p, regexec("\\(([0-9.]*[A-Z]+|N) ?([0-9.]+)/([0-9.]*)\\)", p))[[1]]
    if (length(m) != 4L) next
    stat <- if (grepl("\\)\\s*moist\\b", p)) "M" else if (grepl("\\)\\s*dry\\b", p)) "D" else NA_character_
    out[[length(out) + 1L]] <- list(col_hue = m[2], col_value = m[3],
                                    col_chroma = if (nzchar(m[4])) m[4] else if (m[2] == "N") "0" else NA,
                                    col_moisture_stat = stat)
  }
  out
}

.SALI_BOUND <- c(abrupt = "A", clear = "C", diffuse = "D", gradual = "G", sharp = "S")
.SALI_SHAPE <- c(broken = "B", irregular = "I", smooth = "S", tongued = "T", wavy = "W")

.sali_designation <- function(d) {
  d <- gsub("\\s+", "", d %||% "")
  m <- regmatches(d, regexec("^([0-9]*)([A-Z]+)([0-9]*)(.*)$", d))[[1]]
  if (length(m) != 5L) return(list(pref = NA, master = if (nzchar(d)) d else NA, subdiv = NA, suffix = NA))
  nz <- function(x) if (nzchar(x)) x else NA_character_
  list(pref = nz(m[2]), master = nz(m[3]), subdiv = nz(m[4]), suffix = nz(m[5]))
}

# A row of the morphology table -> a horizon of the NatSoil API (coded).
.sali_row_to_natsoil <- function(row, field_ph = NA_real_, eff = NA_character_) {
  num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.]", "", x %||% "")))
  des <- .sali_designation(row[["Name"]])
  tx <- .sali_texture(row[["Textures"]])
  b <- tolower(trimws(row[["Bounds"]] %||% ""))
  bd <- NA_character_; for (k in names(.SALI_BOUND)) if (grepl(paste0("\\b", k, "\\b"), b)) bd <- .SALI_BOUND[[k]]
  bs <- NA_character_; for (k in names(.SALI_SHAPE)) if (grepl(paste0("\\b", k, "\\b"), b)) bs <- .SALI_SHAPE[[k]]
  list(h_desig_num_pref = des$pref, h_desig_master = des$master, h_desig_subdiv = des$subdiv, h_desig_suffix = des$suffix,
       h_depth_upper = num(row[["Upper"]]), h_depth_lower = num(row[["Lower"]]),
       h_texture = tx[["code"]], h_texture_qual = tx[["qual"]],
       h_bound_distinct = bd, h_bound_shape = bs, h_carbonate_eff = eff,
       z_structures = .sali_structures(row[["Structures"]]), z_colours = .sali_colours(row[["Colour"]]),
       z_cutans = list(), z_cracks = list(), z_coarse_frags = .sali_coarse(row[["Coarse"]]),
       z_segregations = .sali_segregations(row[["Segregations"]]), z_mottles = .sali_mottles(row[["Mottles"]]),
       z_strengths = .sali_strengths(row[["Strengths"]]), z_pans = list(),
       z_phs = if (is.na(field_ph)) list() else list(list(ph_value = field_ph)))
}

# ---- Laboratory ----------------------------------------------------------------------

# SALI method codes -> the codes the NatSoil reader reads: a trailing lower-
# case letter marks a laboratory variant of a method (6B2b, 6B3a); the
# particle-size fractions of 2Z2 are written 2Z2_Clay and 2Z2_Silt.
.sali_lab_code <- function(code) {
  c0 <- as.character(code)
  c0 <- ifelse(grepl("^[0-9]+[A-Z][0-9]+[a-z]$", c0), sub("[a-z]$", "", c0), c0)
  c0 <- sub("^2Z2_Clay$", "2Z2_C", c0); c0 <- sub("^2Z2_Silt$", "2Z2_Z", c0)
  toupper(c0)
}

# A MIR prediction: a method code ending in _MC2, _MC91, _MQ01, _MQ02 ...
.SALI_MIR_RE <- "_M[CQ][0-9]*$"

# MIR methods the NatSoil reader has no code for, read by the property the
# method's name gives (Hugo, 2026-10-30): CEC (15C2_CEC, "Cation exchange
# capacity - MIR QLD Calibration 1"), total organic C (6B4) and effective
# CEC (15J2), filed under a code the reader takes for that property.
.SALI_MIR_ALIAS <- c("15C2_CEC" = "15C1_CEC", "6B4" = "6B3", "15J2" = "15J1")

# The samples of observation `obs` (the API's Samples, parsed) as the NatSoil
# reader's samples (depths in metres), filed under the horizon holding their
# middle (SALI does not link samples to horizons): `measured` holds the
# measured results, `predicted` the MIR predictions (by the code of the
# method they predict, with the MIR codes in `raw`). Results flagged invalid
# (ValidFlag N) are left out; "<x" is read as x / 2 with its limit kept.
.sali_lab_samples <- function(js, top_m, bot_m, obs = 1L) {
  out <- list(measured = list(), predicted = list())
  if (is.null(js) || !length(js)) return(out)
  for (smp in js) {
    if (!identical(suppressWarnings(as.integer(smp$ObservationNumber %||% NA)), as.integer(obs))) next
    t <- suppressWarnings(as.numeric(smp$UpperDepth %||% NA)); b <- suppressWarnings(as.numeric(smp$LowerDepth %||% NA))
    mid <- if (!is.na(t) && !is.na(b)) (t + b) / 2 else t
    link <- if (is.na(mid)) NA_integer_ else {
      k <- which(!is.na(top_m) & !is.na(bot_m) & top_m <= mid & bot_m > mid)
      if (!length(k)) k <- which(!is.na(top_m) & top_m <= mid & (is.na(bot_m) | bot_m >= mid))
      if (length(k)) k[length(k)] else NA_integer_
    }
    vals <- list(); lim <- list(); mv <- list(); ml <- list(); raw_codes <- list()
    for (r in smp$SiteLabMethodResults %||% list()) {
      if (identical(r$ValidFlag, "N")) next
      raw <- as.character(r$LabMethodCode %||% NA)
      if (is.na(raw)) next
      is_mir <- grepl(.SALI_MIR_RE, raw)
      code <- .sali_lab_code(sub(.SALI_MIR_RE, "", raw))
      if (is_mir && !is.na(code) && code %in% names(.SALI_MIR_ALIAS)) code <- .SALI_MIR_ALIAS[[code]]
      fv <- trimws(as.character(r$FormattedValue %||% ""))
      v <- suppressWarnings(as.numeric(sub("^[<>]", "", fv)))
      if (is.na(code) || is.na(v)) next
      censored <- startsWith(fv, "<")
      if (is_mir) {
        if (censored) { ml[[code]] <- max(ml[[code]] %||% -Inf, v); v <- v / 2 }
        mv[[code]] <- c(mv[[code]], v); raw_codes[[code]] <- unique(c(raw_codes[[code]], raw))
      } else {
        if (censored) { lim[[code]] <- max(lim[[code]] %||% -Inf, v); v <- v / 2 }
        vals[[code]] <- c(vals[[code]], v)
      }
    }
    # a sample whose middle lies in no horizon is filed under none (0)
    lk <- if (is.na(link)) 0L else link
    bulk <- identical(smp$BulkFlag, "Y")
    out$measured[[length(out$measured) + 1L]] <- list(top = t, bot = b, link = lk, vals = lapply(vals, mean),
                                                      lim = lim, bulk = bulk)
    out$predicted[[length(out$predicted) + 1L]] <- list(top = t, bot = b, link = lk, vals = lapply(mv, mean),
                                                        lim = ml, bulk = bulk, raw = raw_codes)
  }
  out
}

# The report's lab table (.sali_pdf_lab()) as the API's samples, for a site
# the API holds no samples for: the same codes, flags and "<x" reading.
.sali_pdf_lab_samples <- function(df, top_m, bot_m) {
  if (is.null(df) || !nrow(df)) return(list(measured = list(), predicted = list()))
  js <- lapply(split(df, df$sample), function(z) list(
    ObservationNumber = 1L, UpperDepth = z$upper_m[1], LowerDepth = z$lower_m[1], BulkFlag = z$bulked[1],
    SiteLabMethodResults = lapply(seq_len(nrow(z)), function(i)
      list(LabMethodCode = z$code[i], FormattedValue = z$value[i], ValidFlag = "Y"))))
  .sali_lab_samples(unname(js), top_m, bot_m)
}

# The code (and factor) through which the NatSoil reader took attribute
# `attr` from `lab`, as .natsoil_horizon() tries them: the attribute's codes,
# then for organic C the uncorrected Walkley-Black (x 1.3) and the loss on
# ignition (x 0.5, peat), for free iron the oxide (x 0.6994). Used only to
# name the method in the provenance.
.sali_code_used <- function(lab, attr) {
  extra <- switch(attr, oc_pct = list(c("6A1_UC", 1.3), c("2_LOI", 0.5)),
                  fe_dcb_pct = list(c("13C1_FE203", 0.6994)), list())
  tries <- c(lapply(.NATSOIL_LAB[[attr]]$codes, function(cd) c(cd, 1)), extra)
  for (z in tries) if (!is.na(.natsoil_first(lab, z[1], attr, factor = as.numeric(z[2])))) return(toupper(z[1]))
  NA_character_
}

# The MIR codes of the predicted samples that count for horizon k and carry
# a prediction of `code`.
.sali_mir_codes <- function(pred, k, top, bot, code) {
  out <- character(0)
  for (s in pred) {
    if (is.null(s$raw[[code]])) next
    if (length(.natsoil_lab(list(s), k, top, bot)$value)) out <- c(out, s$raw[[code]])
  }
  unique(out)
}

.sali_read_samples <- function(file) {
  js <- jsonlite::fromJSON(paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                           simplifyVector = FALSE)
  # an OData envelope {"value": [...]}
  if (!is.null(names(js)) && !is.null(js$value)) js <- js$value
  js
}

.sali_has_samples <- function(js, obs = 1L) {
  any(vapply(js %||% list(), function(z) identical(suppressWarnings(as.integer(z$ObservationNumber %||% NA)),
                                                  as.integer(obs)), NA))
}

# ---- ASC labels --------------------------------------------------------------------------

# Names of the 3rd-edition key (inst/rules/asc3) -> codes, lower case: the
# orders, the suborders of each order (without the order's name), the great
# groups and the subgroups (their name up to the first comma).
.sali_asc_dict <- function(rules) {
  ord <- vapply(rules$orders, function(o) o$code, ""); names(ord) <- tolower(vapply(rules$orders, function(o) o$name, ""))
  strip <- function(x) tolower(trimws(sub(",.*$", "", x)))
  sub_d <- lapply(names(rules$suborders), function(o) {
    v <- vapply(rules$suborders[[o]], function(t) t$code, "")
    oname <- names(ord)[ord == o]
    names(v) <- tolower(trimws(sub(paste0("\\s*", oname, "$"), "",
                                   tolower(vapply(rules$suborders[[o]], function(t) t$name, "")))))
    v
  })
  names(sub_d) <- names(rules$suborders)
  gg_d <- list(); for (k in names(rules$great_groups)) for (t in rules$great_groups[[k]]) gg_d[[strip(t$name)]] <- t$code
  sg_d <- list(); for (k in names(rules$subgroups)) for (t in rules$subgroups[[k]]) sg_d[[strip(t$name)]] <- t$code
  list(order = ord, sub = sub_d, gg = unlist(gg_d), sg = unlist(sg_d), rules = rules)
}

# A label of the layer ("Vertosol Black Self-Mulching Epicalcareous-
# Endohypersodic") -> codes by position: order, suborder, great group,
# subgroup, each matched by name (case-insensitive) to the 3rd edition; a
# name the 3rd edition does not have stays NA. "No Available Class" is the
# ASC's ZZ, "Class Undetermined" its YY. A suborder with no great groups
# (Hypergypsic, Lutic, Stratic Rudosols) has its subgroup third, kept in
# the great-group column as NatSoil does.
.sali_parse_label <- function(label, dict) {
  na <- NA_character_
  out <- list(order = na, suborder = na, great_group = na, subgroup = na, unmapped = character(0), ntok = 0L)
  t <- gsub("\\s+", " ", trimws(label %||% ""))
  t <- gsub("No Available Class", "ZZ", t, ignore.case = TRUE)
  t <- gsub("Class Undetermined", "YY", t, ignore.case = TRUE)
  tok <- strsplit(t, " ")[[1]]; out$ntok <- length(tok)
  if (!length(tok)) return(out)
  o <- dict$order[tolower(tok[1])]
  if (is.na(o)) { out$unmapped <- tok[1]; return(out) }
  out$order <- unname(o)
  look <- function(x, d) { if (x %in% c("ZZ", "YY")) return(x); v <- d[tolower(x)]; if (is.na(v)) NA_character_ else unname(v) }
  if (length(tok) >= 2) {
    out$suborder <- look(tok[2], dict$sub[[o]] %||% character(0))
    if (is.na(out$suborder)) out$unmapped <- c(out$unmapped, paste0("suborder:", tok[2]))
  }
  key <- paste(out$order, out$suborder, sep = "-")
  no_gg <- is.null(dict$rules$great_groups[[key]]) && !is.null(dict$rules$subgroups[[key]])
  if (length(tok) >= 3) {
    out$great_group <- look(tok[3], if (no_gg) dict$sg else dict$gg)
    if (is.na(out$great_group)) out$unmapped <- c(out$unmapped, paste0("great_group:", tok[3]))
  }
  if (length(tok) >= 4) {
    out$subgroup <- look(tok[4], dict$sg)
    if (is.na(out$subgroup)) out$unmapped <- c(out$unmapped, paste0("subgroup:", tok[4]))
  }
  if (length(tok) > 4) out$unmapped <- c(out$unmapped, paste0("extra:", paste(tok[-(1:4)], collapse = " ")))
  out
}

# The report's confidence phrases -> the Field Handbook's 1-4; any other
# phrase (conversions from another classification, a rapid check of key
# attributes) is "C".
.SALI_CONF <- c("all necessary analytical data is available" = "1",
                "all necessary analytical data are available" = "1",
                "analytical data are incomplete but reasonable confidence" = "2",
                "no analytical data are available but confidence is fair" = "3",
                "no analytical data and little or no knowledge of this soil" = "4")

.sali_confidence <- function(txt) {
  txt <- trimws(txt %||% "")
  k <- .SALI_CONF[tolower(txt)]
  if (!is.na(k)) unname(k) else if (nzchar(txt)) "C" else NA_character_
}

# The edition of the ASC technical reference the report cites.
.sali_edition <- function(ref) {
  r <- tolower(ref %||% "")
  if (grepl("third edition|\\(2021\\)", r)) "3rd (2021)" else if (grepl("second edition|\\(2016\\)", r)) "2nd (2016)"
  else if (grepl("revised edition|\\(2002\\)", r)) "revised (2002)" else if (grepl("first edition|\\(1996\\)", r)) "1st (1996)"
  else if (grepl("no asc technical reference", r)) "none given" else if (!nzchar(r)) "blank" else paste("other:", ref)
}

# ---- Site --------------------------------------------------------------------------------

# Not recorded (also "Not recorded (historical)"), N/A and empty: NA.
.sali_rec <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x) || grepl("^(not recorded|n/a|not applicable)", tolower(x))) NA_character_ else x
}

# Landform words of the report -> the NatSoil vocabulary the key reads.
.sali_landform <- function(x) {
  x <- .sali_rec(x)
  if (is.na(x)) return(x)
  x <- tolower(x)
  x <- gsub("inter-tidal", "intertidal", x); x <- gsub("supra-tidal", "supratidal", x)
  gsub("valley-flat", "valley flat", x)
}

# Surface coarse fragments (all sizes, and gravel), as NatSoil reads them.
.sali_surface_cf <- function(x) {
  x <- .sali_rec(x)
  if (is.na(x)) return(c(cf = NA_real_, gravel = NA_real_))
  mid <- .NATSOIL_CODES$cf_mid
  es <- .sali_coarse(x)
  a <- vapply(es, function(z) if (is.na(z$cf_abun)) NA_real_ else mid[[z$cf_abun]], 0)
  sz <- vapply(es, function(z) z$cf_size, "")
  grav <- is.na(sz) | sz %in% c("1", "2", "3")
  c(cf = if (anyNA(a)) NaN else min(100, sum(a)), gravel = if (anyNA(a[grav])) NaN else min(100, sum(a[grav])))
}

# ---- The pedon -----------------------------------------------------------------------------

.SALI_ORIGIN <- c(api = "SALI soil chemistry API (OData Samples)",
                  `report table` = "SALI site report PDF (SALI4320), Laboratory test results table")

# parsed: .sali_pdf_parse(); lab_js: the API's samples (parsed JSON) or NULL;
# lay: the site's row of the layer; dict: .sali_asc_dict(); lab_pdf: the
# report's lab table (.sali_pdf_lab()), read only when the API holds no
# samples of observation 1.
.sali_build_pedon <- function(parsed, lab_js, lay, dict, lab_pdf = NULL, accessed = NULL) {
  tb <- parsed$table
  if (is.null(tb) || !nrow(tb)) return(NULL)
  num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.]", "", x)))
  top_m <- num(tb$Upper); bot_m <- num(tb$Lower)
  # field pH: Raupach and Tucker, else an electrode in situ, else a 1:5
  # suspension by probe; the first reading within the horizon
  fph <- rep(NA_real_, nrow(tb)); fph_test <- rep(NA_character_, nrow(tb))
  ft <- parsed$ftests
  if (!is.null(ft) && nrow(ft)) {
    for (pat in c("Raupach", "in-situ using electrode", "1:5 soil/water suspension by probe")) {
      z <- ft[grepl(pat, ft$test, fixed = TRUE) & !is.na(ft$depth_m), , drop = FALSE]
      if (!nrow(z)) next
      z <- z[order(z$depth_m), , drop = FALSE]
      for (k in which(is.na(fph))) {
        hit <- which(!is.na(top_m[k]) & !is.na(bot_m[k]) & z$depth_m >= top_m[k] & z$depth_m < bot_m[k])
        if (length(hit)) {
          fph[k] <- suppressWarnings(as.numeric(z$value[hit[1]]))
          if (!is.na(fph[k])) fph_test[k] <- z$test[hit[1]]
        }
      }
    }
  }
  # carbonate effervescence, a field test by depth in some reports: the first
  # reading within the horizon (Field Handbook codes N, S, M, H, V)
  eff <- rep(NA_character_, nrow(tb))
  if (!is.null(ft) && nrow(ft)) {
    z <- ft[grepl("effervescence", ft$test, ignore.case = TRUE) & !is.na(ft$depth_m) &
              ft$value %in% c("N", "S", "M", "H", "V"), , drop = FALSE]
    z <- z[order(z$depth_m), , drop = FALSE]
    for (k in seq_len(nrow(tb))) {
      hit <- which(!is.na(top_m[k]) & !is.na(bot_m[k]) & z$depth_m >= top_m[k] & z$depth_m < bot_m[k])
      if (length(hit)) eff[k] <- z$value[hit[1]]
    }
  }
  S <- .sali_lab_samples(lab_js, top_m, bot_m)
  lab_source <- if (length(S$measured)) "api" else "none"
  # the API holds no samples for some sites whose report prints a lab table
  if (!length(S$measured) && !is.null(lab_pdf) && nrow(lab_pdf)) {
    S <- .sali_pdf_lab_samples(lab_pdf, top_m, bot_m)
    if (length(S$measured)) lab_source <- "report table"
  }
  origin <- if (lab_source %in% names(.SALI_ORIGIN)) .SALI_ORIGIN[[lab_source]] else NA_character_
  attrs <- names(.NATSOIL_LAB)
  rows <- vector("list", nrow(tb)); n_pred <- 0L
  pk <- integer(0); pa <- character(0); ps <- character(0); pn <- character(0)
  add <- function(k, a, source, note) {
    pk <<- c(pk, k); pa <<- c(pa, a); ps <<- c(ps, source); pn <<- c(pn, note)
  }
  for (k in seq_len(nrow(tb))) {
    hz <- .sali_row_to_natsoil(as.list(tb[k, , drop = FALSE]), fph[k], eff[k])
    lab_m <- .natsoil_lab(S$measured, k, top_m[k], bot_m[k])
    h <- .natsoil_horizon(hz, lab_m)
    add(k, "morphology", "measured",
        "SALI site report PDF (SALI4320), observation 1, Profile morphology table; Field Handbook wording read as NatSoil codes")
    lab_ph <- !is.na(.sali_code_used(lab_m, "ph_h2o")) || !is.na(.sali_code_used(lab_m, "ph_cacl2"))
    for (a in attrs) {
      if (is.na(h[[a]])) next
      if (a == "ph_h2o" && !lab_ph) {
        add(k, a, "measured", sprintf("SALI site report PDF (SALI4320), field test: %s", fph_test[k]))
      } else {
        add(k, a, "measured", sprintf("%s, method %s", origin, .sali_code_used(lab_m, a)))
      }
    }
    # MIR predictions, only for an attribute nothing measured gives in this
    # horizon (the field pH is left out of this reading: it is a measurement)
    lab_p <- if (length(S$predicted)) .natsoil_lab(S$predicted, k, top_m[k], bot_m[k]) else list(value = numeric(0))
    if (length(lab_p$value)) {
      hz0 <- hz; hz0$z_phs <- list()
      hp <- .natsoil_horizon(hz0, lab_p)
      for (a in attrs) {
        if (!is.na(h[[a]]) || is.na(hp[[a]])) next
        h[[a]] <- hp[[a]]
        cd <- .sali_code_used(lab_p, a)
        mir <- .sali_mir_codes(S$predicted, k, top_m[k], bot_m[k], cd)
        # the method the MIR predicts, as SALI names it (an alias is read
        # by its property)
        meth <- unique(.sali_lab_code(sub(.SALI_MIR_RE, "", mir)))
        meth_txt <- if (all(meth == cd)) cd else sprintf("%s (read as %s)", paste(meth, collapse = ", "), a)
        add(k, a, "predicted_spectra",
            sprintf("predicted: MIR prediction %s (%s), for method %s; nothing measured for this attribute in this horizon",
                    paste(mir, collapse = ", "), origin, meth_txt))
        n_pred <- n_pred + 1L
      }
    }
    rows[[k]] <- h
  }
  h <- do.call(rbind, rows)
  s <- parsed$site; a <- parsed$asc %||% list()
  lab <- .sali_parse_label(lay$australian_soil_classification, dict)
  conf_txt <- trimws(a$confidence %||% "")
  scf <- .sali_surface_cf(s[["Surface coarse fragments"]])
  sc <- .sali_rec(s[["Surface condition"]])
  low <- function(x) { x <- .sali_rec(x); if (is.na(x)) x else tolower(x) }
  site <- list(
    id = paste("SALI", lay$project_code, lay$site_id, sep = "_"),
    lat = lay$latitude %||% NA_real_, lon = lay$longitude %||% NA_real_, crs = "EPSG:4283", country = "AU",
    designation_system = "asc",
    date = .sali_rec(s[["Date described"]]),
    drainage_class = low(s[["Drainage"]]),
    landform = .sali_landform(s[["Landform pattern"]]), landform_element = .sali_landform(s[["Landform element"]]),
    surface_condition = if (is.na(sc)) sc else tolower(sc),
    microrelief_form = low(s[["Microrelief type"]]),
    reference_asc_order = lab$order, reference_asc_suborder = lab$suborder,
    reference_asc_great_group = lab$great_group, reference_asc_subgroup = lab$subgroup,
    reference_asc_confidence = .sali_confidence(conf_txt), reference_asc_family = NA_character_,
    surface_coarse_fragments_pct = scf[["cf"]], surface_gravel_pct = scf[["gravel"]],
    licence = .SALI_LICENCE,
    citation = sali_citation(accessed),
    source = "SALI (Queensland Government)",
    sali_asc_label = lay$australian_soil_classification, sali_pdf_asc = a$asc %||% NA_character_,
    sali_asc_edition = .sali_edition(a$reference), sali_asc_confidence_text = conf_txt,
    sali_unmapped = paste(lab$unmapped, collapse = "; "),
    sali_observation_class = .sali_rec(s[["Observation class"]]),
    sali_observation_type = .sali_rec(s[["Observation type"]]),
    sali_depth_to_free_water = .sali_rec(s[["Depth to free water"]]), sali_substrate = .sali_rec(s[["Substrate"]]),
    sali_n_lab_samples = length(S$measured), sali_lab_source = lab_source, sali_n_predicted = n_pred)
  pv <- data.table::data.table(horizon_idx = pk, attribute = pa, source = ps, confidence = 1, notes = pn)
  if (!is.null(parsed$notes) && nrow(parsed$notes)) {
    pv <- rbind(pv, data.table::data.table(horizon_idx = suppressWarnings(as.integer(parsed$notes$hno)),
                                           attribute = paste0("note:", tolower(parsed$notes$level)),
                                           source = "measured", confidence = 1,
                                           notes = paste("SALI site report PDF (SALI4320), Notes:", parsed$notes$note)))
  }
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(data.table::as.data.table(h)), provenance = pv)
}

# One site: the report parsed, the lab file read, the pedon (or the reason
# there is none).
.sali_read_site <- function(lay, f_pdf, f_lab, dict, accessed = NULL) {
  pd <- tryCatch(pdftools::pdf_data(f_pdf), error = function(e) e)
  if (inherits(pd, "error")) return(paste("unreadable report:", conditionMessage(pd)))
  pw <- .sali_pdf_words(pd)
  parsed <- .sali_pdf_parse(pw)
  if (is.null(parsed$table) || !nrow(parsed$table)) return("no morphology table")
  js <- tryCatch(.sali_read_samples(f_lab), error = function(e) e)
  if (inherits(js, "error")) return(paste("unreadable laboratory file:", conditionMessage(js)))
  lab_pdf <- if (!.sali_has_samples(js)) tryCatch(.sali_pdf_lab(pw), error = function(e) NULL) else NULL
  .sali_build_pedon(parsed, js, lay, dict, lab_pdf = lab_pdf, accessed = accessed)
}

#' Read Queensland SALI sites as pedons
#'
#' Turns the files saved by \code{\link{fetch_sali}} into
#' \code{\link{PedonRecord}} objects for \code{\link{classify_asc3}} and the
#' other keys: the site layer (\code{layer/layer101_off*.json}, or a data
#' frame saved as \code{layer/layer101.rds}), the site reports
#' (\code{pdf/<PROJECT>_<SITE>.pdf}) and the laboratory samples of the SALI
#' soil chemistry API (\code{lab/<PROJECT>_<SITE>_samples.json}). A site is
#' read when both its report and its laboratory file are on disk; sites
#' whose report was over the download cap, whose report has no morphology
#' table, or whose label names no ASC order (the layer's "Unclassified"
#' sites) are left out.
#'
#' The morphology exists only in the report (a PDF), read from the
#' coordinates of its words (\pkg{pdftools}). Each row of its Profile
#' morphology table is translated into the coded horizon of the NatSoil API
#' (Field Handbook codes) and read by the NatSoil reader's own conventions
#' (see \code{\link{load_natsoil_pedons}}), the laboratory samples too, so
#' both Australian panels are read alike. These readings (the SALI pilot of
#' 2026-10-30, kept by Hugo) apply:
#' \itemize{
#'   \item Observation 1 of the site is read (a report may hold several).
#'   \item The surveyors' label of the layer is read by position (order,
#'     suborder, great group, subgroup) and each name matched,
#'     case-insensitive, to the 3rd edition (\code{inst/rules/asc3}); a name
#'     the 3rd edition does not have stays \code{NA}. "No Available Class"
#'     is \code{ZZ} and "Class Undetermined" \code{YY}. A suborder with no
#'     great groups (Hypergypsic, Lutic, Stratic Rudosols) has its subgroup
#'     in the great-group column, as NatSoil has it. The edition the report
#'     cites is kept in \code{site$sali_asc_edition}, without changing the
#'     codes. The report's confidence phrases are the Field Handbook's 1-4;
#'     other phrases (a conversion from another classification, a rapid
#'     check of key attributes) are \code{"C"}.
#'   \item An empty cell is not recorded; "No mottles", "No segregations"
#'     and "No coarse fragments" are recorded absent. A colour without its
#'     moisture status is moist. Mottles of biological mixing, mechanical
#'     mixing and substrate influence are not mottles; jarosite mottles are.
#'   \item Textures: "Loam, fine sandy" is a loam; sandy and silty clays are
#'     clays of unspecified grade; heavy or light before a grade lighter than
#'     a clay is the + or - qualifier. The first structure listed is read;
#'     "Blocky" alone names no type. "Not recorded (historical)" is
#'     \code{NA}. A strength without its moisture is not read.
#'   \item Field pH, by depth: Raupach and Tucker first, then an electrode in
#'     situ, then a 1:5 suspension by probe (the pH after peroxide and the
#'     field EC are not used); it is read only where the laboratory has no
#'     pH, as in NatSoil. The effervescence field test gives the horizon's
#'     effervescence.
#'   \item Laboratory samples (not linked to horizons in SALI) are filed
#'     under the horizon holding their middle, then read by NatSoil's overlap
#'     rule; bulked samples count; results flagged invalid are dropped; "<x"
#'     is x / 2 with NatSoil's limit rule. A trailing letter of a method
#'     variant is dropped (6B2b is 6B2) and 2Z2_Clay is 2Z2_C.
#'   \item Tidal landform spellings are NatSoil's (inter-tidal is
#'     intertidal); no tidal flooding is inferred, nor the soil water
#'     status; the depth to free water and the substrate are kept in the site
#'     (\code{sali_depth_to_free_water}, \code{sali_substrate}) but not used.
#' }
#' And Hugo's decisions of 2026-10-30:
#' \itemize{
#'   \item MIR predictions (method codes ending in _MC2, _MC91, _MQ01, _MQ02
#'     and the like) are used only where nothing was measured for that
#'     attribute in that horizon (a field pH the horizon reads counts as
#'     measured), and the provenance marks them as predictions (source
#'     \code{"predicted_spectra"}, a note naming the MIR codes; two
#'     calibrations predicting the same method are averaged). MIR methods
#'     the NatSoil reader has no code for are read by the property their
#'     name gives: 15C2_CEC as the CEC, 6B4 as organic carbon and 15J2 as
#'     the effective CEC.
#'   \item The lab table printed in the report is used only when the API
#'     holds no samples for the site's observation.
#' }
#'
#' The site keeps the surveyors' classification
#' (\code{reference_asc_order}, \code{reference_asc_suborder},
#' \code{reference_asc_great_group}, \code{reference_asc_subgroup},
#' \code{reference_asc_confidence}), as \code{\link{benchmark_sali}} and
#' \code{\link{benchmark_natsoil}} read it, the label and the report's ASC
#' name (\code{sali_asc_label}, \code{sali_pdf_asc}), names left unmatched
#' (\code{sali_unmapped}), the drainage, landform, surface condition,
#' microrelief and surface coarse fragments, coordinates (GDA94), the
#' licence and the citation (\code{\link{sali_citation}}), where the
#' laboratory values came from (\code{sali_lab_source}: \code{"api"},
#' \code{"report table"} or \code{"none"}) and how many values are MIR
#' predictions (\code{sali_n_predicted}). The provenance has one row per
#' horizon for its morphology and one per laboratory value, which names the
#' source (the API or the report's table) and the method; the report's
#' notes are kept there too.
#'
#' @param path The folder \code{\link{fetch_sali}} wrote.
#' @param sites Optional sites to read (a data frame with
#'   \code{project_code} and \code{site_id}, or \code{"<PROJECT>_<SITE>"}).
#' @param cores Number of processes (\code{parallel::mclapply}; always 1 on
#'   Windows).
#' @param max_n Optional maximum number of sites read (a random sample).
#' @param seed Optional seed for that sample.
#' @param accessed Optional date the data were downloaded, for the citation.
#' @param verbose Print a summary.
#' @return A named list of \code{\link{PedonRecord}} objects (names
#'   \code{SALI_<PROJECT>_<SITE>}), with an attribute \code{"sali_status"}: a
#'   data frame of the sites considered and what became of each.
#' @references Queensland Government. Soil and Land Information (SALI).
#'   National Committee on Soil and Terrain (2009). Australian Soil and Land
#'   Survey Field Handbook, 3rd edition. CSIRO Publishing.
#' @seealso \code{\link{fetch_sali}}, \code{\link{benchmark_sali}},
#'   \code{\link{sali_citation}}, \code{\link{load_natsoil_pedons}}.
#' @export
read_sali_pedons <- function(path, sites = NULL, cores = 1L, max_n = NULL, seed = NULL, accessed = NULL,
                             verbose = TRUE) {
  if (!requireNamespace("pdftools", quietly = TRUE))
    stop("read_sali_pedons() needs the 'pdftools' package to read the site reports: install.packages('pdftools').",
         call. = FALSE)
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("read_sali_pedons() needs the 'jsonlite' package.", call. = FALSE)
  lay <- .sali_layer_table(path)
  if (!is.null(sites)) {
    want <- if (is.data.frame(sites)) .sali_stem(sites$project_code, sites$site_id) else as.character(sites)
    lay <- lay[lay$stem %in% want, , drop = FALSE]
  }
  f_pdf <- file.path(path, "pdf", paste0(lay$stem, ".pdf"))
  f_lab <- file.path(path, "lab", paste0(lay$stem, "_samples.json"))
  status <- ifelse(is.na(.sali_order_code(lay$asc_label)), "label names no ASC order",
            ifelse(file.exists(f_pdf), ifelse(file.exists(f_lab), NA_character_, "no laboratory file"),
                   ifelse(file.exists(paste0(f_pdf, ".oversize")), "report over the download cap", "no report")))
  todo <- which(is.na(status))
  if (!is.null(max_n) && length(todo) > max_n) {
    if (!is.null(seed)) set.seed(seed)
    todo <- sort(sample(todo, max_n))
  }
  dict <- .sali_asc_dict(load_rules("asc3"))
  one <- function(i) tryCatch(.sali_read_site(lay[i, , drop = FALSE], f_pdf[i], f_lab[i], dict, accessed),
                              error = function(e) paste("failed:", conditionMessage(e)))
  cores <- if (.Platform$OS.type == "windows") 1L else max(1L, as.integer(cores))
  res <- if (cores > 1L && length(todo) > 1L) parallel::mclapply(todo, one, mc.cores = cores) else lapply(todo, one)
  out <- list()
  for (j in seq_along(todo)) {
    r <- res[[j]]
    if (inherits(r, "PedonRecord")) { out[[r$site$id]] <- r; status[todo[j]] <- "read" }
    else status[todo[j]] <- if (inherits(r, "try-error")) paste("failed:", as.character(r)) else as.character(r)
  }
  st <- data.frame(stem = lay$stem, asc_label = lay$asc_label, status = status, stringsAsFactors = FALSE)
  st$status[is.na(st$status)] <- "not read (sample)"
  if (verbose) {
    tb <- table(st$status)
    message(sprintf("SALI: %d pedons from %d sites (%s)", length(out), nrow(st),
                    paste(sprintf("%s %d", names(tb), as.integer(tb))[names(tb) != "read"], collapse = "; ")))
  }
  attr(out, "sali_status") <- st
  out
}

#' Benchmark the Australian key on Queensland SALI
#'
#' Classifies SALI pedons (\code{\link{read_sali_pedons}}) with
#' \code{\link{classify_asc3}} and compares them with the surveyors'
#' classification, with the metrics of \code{\link{benchmark_natsoil}}
#' (which it calls): the order agreement over all pedons and over the decided
#' ones (no order before the assigned one left undecided for missing data),
#' the Arenosols of the 3rd edition counted as Tenosols or Rudosols in the
#' \emph{edition} figures, the suborders given the order, the great groups
#' given the suborder and the subgroups given the great group. It adds a
#' table by surveyed order and one by the ASC edition the report cites.
#'
#' @param pedons A list of \code{\link{PedonRecord}} from
#'   \code{\link{read_sali_pedons}}.
#' @param cores Number of processes (\code{parallel::mclapply}; always 1 on
#'   Windows).
#' @param verbose Print a summary.
#' @param family Also compare the families (see \code{\link{benchmark_natsoil}};
#'   SALI labels carry no family codes, so this adds nothing yet).
#' @return The list of \code{\link{benchmark_natsoil}}, plus \code{by_order}
#'   (by surveyed order: \code{n}, \code{agree} and \code{decided_agree} in
#'   per cent, \code{decided}, and soilKey's most frequent order,
#'   \code{top_prediction}) and \code{by_edition} (the same by the edition of
#'   the ASC reference, \code{site$sali_asc_edition}).
#' @seealso \code{\link{read_sali_pedons}}, \code{\link{benchmark_natsoil}}.
#' @export
benchmark_sali <- function(pedons, cores = 1L, verbose = TRUE, family = FALSE) {
  b <- benchmark_natsoil(pedons, cores = cores, verbose = FALSE, family = family)
  r <- b$results
  pct <- function(x) if (length(x)) round(100 * mean(x), 1) else NA_real_
  tab_by <- function(key, lev, name) {
    rows <- lapply(lev, function(k) {
      s <- key %in% k
      data.frame(level = k, n = sum(s), agree = pct(r$agree[s]), decided = sum(s & r$decided),
                 decided_agree = if (any(s & r$decided)) pct(r$agree[s & r$decided]) else NA_real_,
                 top_prediction = names(sort(table(r$predicted[s]), decreasing = TRUE))[1] %||% NA_character_,
                 stringsAsFactors = FALSE)
    })
    d <- do.call(rbind, rows)
    if (!is.null(d)) names(d)[1] <- name
    d
  }
  b$by_order <- tab_by(r$reference, names(b$recall), "order")
  ids <- vapply(pedons, function(p) as.character(p$site$id %||% NA), "")
  ed <- vapply(pedons, function(p) as.character(p$site$sali_asc_edition %||% NA), "")
  r_ed <- ed[match(r$id, ids)]
  b$by_edition <- tab_by(r_ed, sort(unique(r_ed[!is.na(r_ed)])), "edition")
  if (verbose) {
    message(sprintf(paste("SALI, ASC orders: %d pedons, agreement %.1f %% (%.1f %% counting the",
                          "Arenosols as Tenosols or Rudosols); %d decided, %.1f %% (%.1f %%)"),
                    b$n, b$accuracy, b$accuracy_edition, b$decided, b$decided_accuracy, b$decided_accuracy_edition))
  }
  b
}
