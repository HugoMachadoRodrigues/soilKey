# =============================================================================
# soilKey Pro -- the visitor counter of the footer (v0.9.260).
#
# The footer says how many people are on the site now, how many have come, and
# from which countries, with a small map of them.
#
#   * A person is counted once. On the first visit the browser keeps a mark,
#     "soilkey_seen", in its own storage (no identifier) and tells the server it
#     is a new visitor; a later visit from the same browser counts as a session
#     only. Nothing that identifies the visitor is sent or kept.
#   * The country comes from the address of the request, looked up in DB-IP's
#     "IP to Country Lite" table (CC BY 4.0), which the image downloads when it
#     is built. The address is read for that and kept nowhere.
#   * Automated browsers (headless Chrome, crawlers, the capacity test) are not
#     counted, and neither are pages that never open a Shiny session.
#   * The numbers live in Firestore, in the project the app runs in: one
#     document of totals and one of presence for each running instance, which
#     writes how many sessions it holds every 30 s. "Online now" is the sum of
#     the instances that wrote in the last 90 s.
#   * Only the public site (soilkeypro.com) writes to the real collections; a
#     tagged revision or a local run writes to the "_test" ones, or nowhere when
#     it is not on Cloud Run (no metadata server, no token).
# =============================================================================

.VIS_PUBLIC_HOST <- "soilkeypro.com"
.vis_env <- new.env(parent = emptyenv())

# ---- addresses ---------------------------------------------------------------

# An IPv4 address as a number (a*2^24 + b*2^16 + c*2^8 + d); NA for anything
# else. Vectorised.
.vis_ip4_num <- function(x) {
  x <- as.character(x)
  ok <- !is.na(x) & grepl("^[0-9]{1,3}(\\.[0-9]{1,3}){3}$", x)
  out <- rep(NA_real_, length(x))
  if (any(ok)) {
    p <- matrix(as.numeric(unlist(strsplit(x[ok], ".", fixed = TRUE))), ncol = 4, byrow = TRUE)
    bad <- rowSums(p > 255) > 0
    v <- p[, 1] * 16777216 + p[, 2] * 65536 + p[, 3] * 256 + p[, 4]
    v[bad] <- NA_real_
    out[ok] <- v
  }
  out
}

# The first 48 bits of an IPv6 address as a number (exact in a double); NA for
# anything else. 48 bits tell countries apart in DB-IP's table: a block of a
# country smaller than a /48 is rare. Vectorised.
.vis_ip6_prefix48 <- function(x) {
  x <- tolower(as.character(x))
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x) & grepl(":", x, fixed = TRUE) & grepl("^[0-9a-f:.]+$", x)
  for (i in which(ok)) {
    s <- x[i]
    if (grepl(".", s, fixed = TRUE)) s <- sub(":[0-9.]+$", ":0:0", s)   # IPv4 tail
    halves <- strsplit(s, "::", fixed = TRUE)[[1]]
    if (length(halves) > 2L) next
    left <- if (nzchar(halves[1])) strsplit(halves[1], ":", fixed = TRUE)[[1]] else character(0)
    right <- if (length(halves) == 2L && nzchar(halves[2])) strsplit(halves[2], ":", fixed = TRUE)[[1]] else character(0)
    fill <- if (length(halves) == 2L || grepl("::$", s)) 8L - length(left) - length(right) else 0L
    g <- c(left, rep("0", max(fill, 0L)), right)
    if (length(g) != 8L || !all(grepl("^[0-9a-f]{1,4}$", g))) next
    h <- strtoi(g[1:3], 16L)
    if (anyNA(h)) next
    out[i] <- h[1] * 4294967296 + h[2] * 65536 + h[3]
  }
  out
}

# The client's address: the first of X-Forwarded-For (Cloud Run puts the client
# there), else the address of the connection.
.vis_client_ip <- function(xff = NULL, remote = NULL) {
  a <- if (length(xff) == 1L && !is.na(xff) && nzchar(xff)) trimws(strsplit(xff, ",", fixed = TRUE)[[1]][1]) else remote
  if (length(a) != 1L || is.na(a) || !nzchar(a)) NA_character_ else a
}

# Whether a user agent is an automated browser rather than a person: headless
# Chrome, crawlers, command-line clients, the capacity test, and the browser of
# the Claude app ("Claude/<version>"), which the maintainer's checks use.
.vis_is_automated <- function(ua) {
  is.null(ua) || length(ua) != 1L || is.na(ua) || !nzchar(ua) ||
    grepl("HeadlessChrome|PhantomJS|bot|crawl|spider|slurp|curl|wget|python|Go-http|soilKey-capacity-test|Claude/[0-9]",
          ua, ignore.case = TRUE)
}

# The collection a host writes to: the real one for the public site only.
# Vectorised.
.vis_collection <- function(host) {
  if (is.null(host) || !length(host)) host <- ""
  h <- tolower(sub(":[0-9]+$", "", ifelse(is.na(host), "", host)))
  ifelse(h %in% c(.VIS_PUBLIC_HOST, paste0("www.", .VIS_PUBLIC_HOST)), "visitors", "visitors_test")
}

# ---- DB-IP's country table --------------------------------------------------

# Builds the compact table the app reads from DB-IP's CSV ("first,last,cc").
# Run when the image is built (deploy/fly/Dockerfile).
.vis_geo_build <- function(csv_gz, out_rds, source_label = basename(csv_gz)) {
  # "NA" is Namibia, not a missing value
  d <- utils::read.csv(gzfile(csv_gz), header = FALSE, colClasses = "character",
                       col.names = c("first", "last", "cc"), na.strings = character(0))
  d$cc[d$cc == "ZZ"] <- NA_character_
  v6 <- grepl(":", d$first, fixed = TRUE)
  four <- data.frame(start = .vis_ip4_num(d$first[!v6]), end = .vis_ip4_num(d$last[!v6]),
                     cc = d$cc[!v6], stringsAsFactors = FALSE)
  six <- data.frame(start = .vis_ip6_prefix48(d$first[v6]), end = .vis_ip6_prefix48(d$last[v6]),
                    cc = d$cc[v6], stringsAsFactors = FALSE)
  tidy <- function(t) {
    t <- t[!is.na(t$start) & !is.na(t$end), ]
    t <- t[order(t$start), ]
    t[!duplicated(t$start), ]
  }
  geo <- list(v4 = tidy(four), v6 = tidy(six), source = source_label)
  geo$v4$cc <- factor(geo$v4$cc); geo$v6$cc <- factor(geo$v6$cc)
  saveRDS(geo, out_rds, compress = "xz")
  invisible(geo)
}

# The table, read once per process from SOILKEY_GEOIP; NULL when absent.
.vis_geo <- function() {
  if (!exists("geo", envir = .vis_env, inherits = FALSE)) {
    p <- Sys.getenv("SOILKEY_GEOIP", "")
    .vis_env$geo <- if (nzchar(p) && file.exists(p)) tryCatch(readRDS(p), error = function(e) NULL) else NULL
  }
  .vis_env$geo
}

# The ISO 3166 code of the country of each address; NA when it is private,
# reserved or not in the table. Vectorised.
.vis_country <- function(ip, geo = .vis_geo()) {
  out <- rep(NA_character_, length(ip))
  if (is.null(geo)) return(out)
  look <- function(key, t) {
    i <- findInterval(key, t$start)
    hit <- !is.na(key) & i > 0L
    res <- rep(NA_character_, length(key))
    res[hit] <- ifelse(key[hit] <= t$end[i[hit]], as.character(t$cc[i[hit]]), NA_character_)
    res
  }
  k4 <- .vis_ip4_num(ip); k6 <- .vis_ip6_prefix48(ip)
  out[!is.na(k4)] <- look(k4[!is.na(k4)], geo$v4)
  out[!is.na(k6)] <- look(k6[!is.na(k6)], geo$v6)
  out
}

# ---- Firestore ----------------------------------------------------------------

.vis_metadata <- function(path) {
  r <- tryCatch(
    httr2::request(paste0("http://metadata.google.internal/computeMetadata/v1/", path)) |>
      httr2::req_headers(`Metadata-Flavor` = "Google") |>
      httr2::req_timeout(2) |>
      httr2::req_perform(),
    error = function(e) NULL)
  if (is.null(r)) NULL else r
}

# An access token of the service account, cached until a minute before it
# expires; NULL off Cloud Run.
.vis_token <- function() {
  if (isTRUE(.vis_env$no_metadata)) return(NULL)
  if (!is.null(.vis_env$token) && Sys.time() < .vis_env$token_until) return(.vis_env$token)
  r <- .vis_metadata("instance/service-accounts/default/token")
  if (is.null(r)) { .vis_env$no_metadata <- TRUE; return(NULL) }
  j <- httr2::resp_body_json(r)
  .vis_env$token <- j$access_token
  .vis_env$token_until <- Sys.time() + as.numeric(j$expires_in) - 60
  .vis_env$token
}

.vis_project <- function() {
  if (is.null(.vis_env$project)) {
    r <- .vis_metadata("project/project-id")
    .vis_env$project <- if (is.null(r)) "" else httr2::resp_body_string(r)
  }
  .vis_env$project
}

.vis_doc_root <- function() {
  sprintf("projects/%s/databases/(default)/documents", .vis_project())
}

.vis_request <- function(url, token) {
  httr2::request(url) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_timeout(8)
}

# The body of the commit that counts a session: one more session, and, for a
# new visitor, one more visitor and one more for the country.
.vis_commit_body <- function(doc, new_visitor, country) {
  inc <- function(path) list(fieldPath = path, increment = list(integerValue = "1"))
  tr <- list(inc("sessions"))
  if (isTRUE(new_visitor)) {
    tr <- c(tr, list(inc("visitors")))
    if (!is.na(country) && grepl("^[A-Z]{2}$", country))
      tr <- c(tr, list(inc(paste0("countries.", country))))
  }
  list(writes = list(list(transform = list(document = doc, fieldTransforms = tr))))
}

# Counts a session. Asynchronous: the session does not wait for Firestore.
.vis_record <- function(coll, new_visitor, country) {
  tok <- .vis_token(); if (is.null(tok)) return(invisible(FALSE))
  root <- .vis_doc_root()
  body <- .vis_commit_body(paste0(root, "/", coll, "/totals"), new_visitor, country)
  req <- .vis_request(sprintf("https://firestore.googleapis.com/v1/%s:commit", root), tok) |>
    httr2::req_body_json(body)
  p <- httr2::req_perform_promise(req)
  promises::catch(p, function(e) message("visitors: record failed: ", conditionMessage(e)))
  invisible(TRUE)
}

# The presence of this instance: how many sessions it holds, and when.
.vis_presence_body <- function(count, when = Sys.time()) {
  list(fields = list(
    count = list(integerValue = as.character(as.integer(count))),
    updated = list(timestampValue = format(when, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))))
}

# "Online now": the sessions of the instances that wrote in the last `stale`
# seconds. `docs` is the list of presence documents as Firestore returns them.
.vis_online <- function(docs, now = Sys.time(), stale = 90) {
  n <- 0L
  for (d in docs) {
    f <- d$fields
    t <- tryCatch(as.POSIXct(sub("\\..*Z$|Z$", "", f$updated$timestampValue),
                             format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"), error = function(e) NA)
    if (!is.na(t) && as.numeric(difftime(now, t, units = "secs")) <= stale)
      n <- n + as.integer(f$count$integerValue %||% 0L)
  }
  n
}

# The totals document as plain values.
.vis_totals <- function(doc) {
  f <- doc$fields
  num <- function(x) as.numeric(x$integerValue %||% 0)
  cm <- f$countries$mapValue$fields
  countries <- if (length(cm)) vapply(cm, num, numeric(1)) else numeric(0)
  since <- f$since$timestampValue
  list(visitors = num(f$visitors), sessions = num(f$sessions),
       countries = countries[countries > 0],
       since = if (is.null(since)) NA_character_ else substr(since, 1, 10))
}

# Writes this instance's presence and reads the totals and every presence.
# Asynchronous; the result lands in .vis_env$stats and bumps .vis_env$version.
.vis_tick <- function() {
  tok <- .vis_token(); if (is.null(tok)) return(invisible(NULL))
  root <- .vis_doc_root()
  base <- sprintf("https://firestore.googleapis.com/v1/%s", root)
  for (coll in names(.vis_env$active)) {
    req <- .vis_request(sprintf("%s/%s_presence/%s?updateMask.fieldPaths=count&updateMask.fieldPaths=updated",
                                base, coll, .vis_env$instance), tok) |>
      httr2::req_method("PATCH") |>
      httr2::req_body_json(.vis_presence_body(.vis_env$active[[coll]]))
    promises::catch(httr2::req_perform_promise(req),
                    function(e) message("visitors: presence failed: ", conditionMessage(e)))
  }
  for (coll in unique(c("visitors", names(.vis_env$active)))) local({
    cl <- coll
    tot <- httr2::req_perform_promise(.vis_request(sprintf("%s/%s/totals", base, cl), tok))
    pre <- httr2::req_perform_promise(.vis_request(sprintf("%s/%s_presence?pageSize=50", base, cl), tok))
    promises::then(promises::promise_all(tot = tot, pre = pre), function(r) {
      s <- .vis_totals(httr2::resp_body_json(r$tot))
      s$online <- .vis_online(httr2::resp_body_json(r$pre)$documents %||% list())
      .vis_env$stats[[cl]] <- s
      .vis_env$version <- (.vis_env$version %||% 0L) + 1L
    }) |> promises::catch(function(e) message("visitors: read failed: ", conditionMessage(e)))
  })
  invisible(NULL)
}

# The loop of ticks: every 30 s while this instance holds a counted session.
.vis_loop <- function() {
  if (sum(unlist(.vis_env$active), na.rm = TRUE) <= 0L) { .vis_env$looping <- FALSE; return(invisible()) }
  .vis_tick()
  later::later(.vis_loop, 30)
}

# A counted session begins (`coll`: the collection of its host) and ends.
.vis_session_open <- function(coll) {
  if (is.null(.vis_env$instance))
    # the revision, the process and the clock: no draw from the random
    # number generator, which the sessions' own computations use
    .vis_env$instance <- sprintf("%s-%d-%s", Sys.getenv("K_REVISION", "local"), Sys.getpid(),
                                 format(round(as.numeric(Sys.time()) * 1000), scientific = FALSE))
  .vis_env$active[[coll]] <- (.vis_env$active[[coll]] %||% 0L) + 1L
  if (!isTRUE(.vis_env$looping)) { .vis_env$looping <- TRUE; later::later(.vis_loop, 0) }
}
.vis_session_close <- function(coll) {
  .vis_env$active[[coll]] <- max(0L, (.vis_env$active[[coll]] %||% 1L) - 1L)
}
