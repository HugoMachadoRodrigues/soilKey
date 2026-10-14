# =============================================================================
# soilKey Pro -- fill the visitor counter with the sessions the request logs
# still hold, from before the counter was deployed (v0.9.261). Run once.
#
# Cloud Run keeps 30 days of request logs. A Shiny session is a request to
# /websocket/ answered with 101. From those of the public host, before
# `--cutoff` (the moment the counting revision got the traffic), leaving out the
# automated browsers the app leaves out:
#   sessions  = their number;
#   visitors  = the number of distinct client addresses (the logs know nothing
#               better than the address for a person);
#   countries = the visitors of each country, by the same DB-IP table the app
#               uses;
#   since     = the day of the first of them.
# The counts are ADDED to the totals document (Firestore increments), so the
# sessions the app counted after the cutoff stay. No address is printed or kept.
#
#   gcloud logging read 'resource.type="cloud_run_revision" AND
#     resource.labels.service_name="soilkeypro" AND httpRequest.status=101' \
#     --project soilkeypro --freshness 31d --format=json > ws.json
#   Rscript deploy/cloudrun/visitors-backfill.R --logs ws.json \
#     --geo dbip-country.rds --cutoff 2026-10-14T20:00:00Z [--write]
#
# Without --write it only prints what it would add.
# =============================================================================

args <- commandArgs(TRUE)
arg <- function(name, default = NULL) {
  i <- match(name, args)
  if (is.na(i)) default else args[i + 1L]
}
logs <- arg("--logs"); geo_path <- arg("--geo"); cutoff <- arg("--cutoff")
write <- "--write" %in% args
project <- arg("--project", "soilkeypro")
stopifnot(!is.null(logs), !is.null(geo_path), !is.null(cutoff))

source(file.path("inst", "shiny", "classify_app_pro", "R", "utils_visitors.R"))

d <- jsonlite::fromJSON(logs, simplifyVector = FALSE)
rows <- lapply(d, function(e) {
  r <- e$httpRequest
  data.frame(time = e$timestamp, url = r$requestUrl %||% "", ua = r$userAgent %||% "",
             ip = r$remoteIp %||% NA_character_, stringsAsFactors = FALSE)
})
x <- do.call(rbind, rows)
x$host <- sub("^https?://([^/]+)/.*$", "\\1", x$url)
keep <- .vis_collection(x$host) == "visitors" & grepl("/websocket/", x$url, fixed = TRUE) &
  !vapply(x$ua, .vis_is_automated, logical(1)) & x$time < cutoff & !is.na(x$ip)
x <- x[keep, ]
if (!nrow(x)) stop("no session to add")

geo <- readRDS(geo_path)
first_ip <- x$ip[!duplicated(x$ip)]
cc <- .vis_country(first_ip, geo)
countries <- table(cc[!is.na(cc)])
since <- min(substr(x$time, 1, 10))
cat(sprintf("sessions %d | visitors %d | countries %d (%s) | since %s | countries unknown for %d visitors\n",
            nrow(x), length(first_ip), length(countries),
            paste(sprintf("%s %d", names(countries), as.integer(countries)), collapse = ", "),
            since, sum(is.na(cc))))
if (!write) { cat("dry run: nothing written\n"); quit(save = "no") }

token <- system2("gcloud", c("auth", "print-access-token"), stdout = TRUE)
root <- sprintf("projects/%s/databases/(default)/documents", project)
doc <- paste0(root, "/visitors/totals")
inc <- function(path, n) list(fieldPath = path, increment = list(integerValue = as.character(n)))
tr <- c(list(inc("sessions", nrow(x)), inc("visitors", length(first_ip))),
        lapply(names(countries), function(k) inc(paste0("countries.", k), as.integer(countries[[k]]))))
body <- list(writes = list(
  list(transform = list(document = doc, fieldTransforms = tr)),
  list(update = list(name = doc, fields = list(since = list(timestampValue = paste0(since, "T00:00:00Z")))),
       updateMask = list(fieldPaths = list("since")))))
r <- httr2::request(sprintf("https://firestore.googleapis.com/v1/%s:commit", root)) |>
  httr2::req_auth_bearer_token(token) |>
  httr2::req_body_json(body) |>
  httr2::req_perform()
cat("written:", httr2::resp_status(r), "\n")
