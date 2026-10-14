# v0.9.261: the visitor counter of the Pro app's footer (R/utils_visitors.R,
# R/mod_visitors.R). The parts that do not need Firestore or a browser.

.vis_app_env <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  sys.source(file.path(d, "R", "utils_visitors.R"), envir = e)
  e$app_dir <- d
  e
}

test_that("IPv4 addresses become numbers, and nothing else does", {
  skip_on_cran()
  e <- .vis_app_env()
  expect_equal(e$.vis_ip4_num(c("0.0.0.0", "1.0.0.0", "8.8.8.8", "255.255.255.255")),
               c(0, 16777216, 134744072, 4294967295))
  expect_true(all(is.na(e$.vis_ip4_num(c("256.1.1.1", "1.2.3", "a.b.c.d", "::1", NA)))))
})

test_that("IPv6 addresses become their first 48 bits", {
  skip_on_cran()
  e <- .vis_app_env()
  expect_equal(e$.vis_ip6_prefix48("2804:14c::1"), 0x2804 * 2^32 + 0x014c * 2^16)
  expect_equal(e$.vis_ip6_prefix48("2804:14c:0:0:0:0:0:1"), e$.vis_ip6_prefix48("2804:14c::1"))
  expect_equal(e$.vis_ip6_prefix48("::1"), 0)
  expect_equal(e$.vis_ip6_prefix48("2001:db8:a::"), 0x2001 * 2^32 + 0x0db8 * 2^16 + 0xa)
  expect_true(all(is.na(e$.vis_ip6_prefix48(c("8.8.8.8", "2001:::1:2:3", "xyz:1::", NA)))))
})

test_that("the country table is built from DB-IP's file and read by range", {
  skip_on_cran()
  e <- .vis_app_env()
  csv <- tempfile(fileext = ".csv.gz"); rds <- tempfile(fileext = ".rds")
  on.exit(unlink(c(csv, rds)), add = TRUE)
  con <- gzfile(csv, "w")
  writeLines(c("0.0.0.0,0.255.255.255,ZZ", "8.8.8.0,8.8.8.255,US",
               "200.20.0.0,200.20.255.255,BR",
               "41.182.0.0,41.182.255.255,NA",
               "2804::,2804:ffff:ffff:ffff:ffff:ffff:ffff:ffff,BR",
               "2a00::,2a00:ffff:ffff:ffff:ffff:ffff:ffff:ffff,PT"), con)
  close(con)
  g <- e$.vis_geo_build(csv, rds, "test")
  expect_equal(nrow(g$v4), 4L); expect_equal(nrow(g$v6), 2L)
  expect_equal(e$.vis_country(c("8.8.8.8", "200.20.10.1", "2804:14c::1", "2a00:1::5"), g),
               c("US", "BR", "BR", "PT"))
  # Namibia's code is "NA", not a missing value
  expect_identical(e$.vis_country("41.182.3.4", g), "NA")
  # reserved (ZZ), outside every range, private, not an address
  expect_true(all(is.na(e$.vis_country(c("0.1.2.3", "9.9.9.9", "192.168.0.1", "x"), g))))
  # read from the file named by SOILKEY_GEOIP, once per process
  withr::with_envvar(c(SOILKEY_GEOIP = rds), {
    rm(list = intersect("geo", ls(e$.vis_env)), envir = e$.vis_env)
    expect_equal(e$.vis_country("8.8.8.8"), "US")
  })
  # with no table, no country
  expect_true(is.na(e$.vis_country("8.8.8.8", NULL)))
})

test_that("the client is the first address of X-Forwarded-For", {
  skip_on_cran()
  e <- .vis_app_env()
  expect_equal(e$.vis_client_ip("177.53.1.1, 35.191.0.1", "169.254.1.1"), "177.53.1.1")
  expect_equal(e$.vis_client_ip(NULL, "10.0.0.2"), "10.0.0.2")
  expect_equal(e$.vis_client_ip("", "10.0.0.2"), "10.0.0.2")
  expect_true(is.na(e$.vis_client_ip(NULL, NULL)))
})

test_that("automated browsers are not counted, people are", {
  skip_on_cran()
  e <- .vis_app_env()
  people <- c("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0 Safari/537.36",
              "Mozilla/5.0 (iPhone; CPU iPhone OS 18_7 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Mobile/15E148 Safari/604.1")
  robots <- c("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) HeadlessChrome/155.0.0.0 Safari/537.36",
              "Googlebot/2.1 (+http://www.google.com/bot.html)", "curl/8.4.0",
              "python-requests/2.32", "soilKey-capacity-test",
              "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Claude/2.2645.0 Chrome/138.0 Electron/37.0 Safari/537.36",
              "", NA)
  expect_false(any(vapply(people, e$.vis_is_automated, logical(1))))
  expect_true(all(vapply(robots, e$.vis_is_automated, logical(1))))
  expect_true(e$.vis_is_automated(NULL))
})

test_that("only the public site writes to the real collection", {
  skip_on_cran()
  e <- .vis_app_env()
  expect_equal(e$.vis_collection("soilkeypro.com"), "visitors")
  expect_equal(e$.vis_collection("www.soilkeypro.com"), "visitors")
  expect_equal(e$.vis_collection("SOILKEYPRO.COM:443"), "visitors")
  expect_equal(e$.vis_collection("t0960---soilkeypro-hj7pbml62a-ue.a.run.app"), "visitors_test")
  expect_equal(e$.vis_collection("127.0.0.1:4321"), "visitors_test")
  expect_equal(e$.vis_collection(NULL), "visitors_test")
})

test_that("a session adds one session; a first visit, one visitor and one for its country", {
  skip_on_cran()
  e <- .vis_app_env()
  doc <- "projects/p/databases/(default)/documents/visitors/totals"
  b <- e$.vis_commit_body(doc, TRUE, "BR")
  tr <- b$writes[[1]]$transform
  expect_equal(tr$document, doc)
  expect_equal(vapply(tr$fieldTransforms, `[[`, "", "fieldPath"),
               c("sessions", "visitors", "countries.BR"))
  expect_true(all(vapply(tr$fieldTransforms, function(x) x$increment$integerValue, "") == "1"))
  # a return visit: the session only
  expect_equal(vapply(e$.vis_commit_body(doc, FALSE, "BR")$writes[[1]]$transform$fieldTransforms,
                      `[[`, "", "fieldPath"), "sessions")
  # a first visit with no country known: no country field
  expect_equal(vapply(e$.vis_commit_body(doc, TRUE, NA)$writes[[1]]$transform$fieldTransforms,
                      `[[`, "", "fieldPath"), c("sessions", "visitors"))
  # what is not a country code is not a field path
  expect_false("countries.<a>" %in% vapply(e$.vis_commit_body(doc, TRUE, "<a>")$writes[[1]]$transform$fieldTransforms,
                                            `[[`, "", "fieldPath"))
  # the body is JSON as Firestore takes it
  expect_match(as.character(jsonlite::toJSON(b, auto_unbox = TRUE)), '"increment":\\{"integerValue":"1"\\}')
})

test_that("online now adds up the instances that wrote in the last 90 seconds", {
  skip_on_cran()
  e <- .vis_app_env()
  now <- as.POSIXct("2026-10-14T20:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  doc <- function(n, t) list(fields = list(count = list(integerValue = as.character(n)),
                                           updated = list(timestampValue = t)))
  docs <- list(doc(3, "2026-10-14T19:59:40Z"),          # 20 s ago
               doc(2, "2026-10-14T19:58:45.123456Z"),   # 75 s ago, with a fraction
               doc(7, "2026-10-14T19:55:00Z"))          # 5 min ago: gone
  expect_equal(e$.vis_online(docs, now), 5L)
  expect_equal(e$.vis_online(list(), now), 0L)
  # the body this instance writes
  pb <- e$.vis_presence_body(4, now)
  expect_equal(pb$fields$count$integerValue, "4")
  expect_equal(pb$fields$updated$timestampValue, "2026-10-14T20:00:00Z")
})

test_that("the totals document is read into numbers", {
  skip_on_cran()
  e <- .vis_app_env()
  doc <- list(fields = list(
    visitors = list(integerValue = "21"), sessions = list(integerValue = "70"),
    since = list(timestampValue = "2026-10-07T00:00:00Z"),
    countries = list(mapValue = list(fields = list(
      BR = list(integerValue = "18"), US = list(integerValue = "2"), PT = list(integerValue = "1"))))))
  t <- e$.vis_totals(doc)
  expect_equal(t$visitors, 21); expect_equal(t$sessions, 70); expect_equal(t$since, "2026-10-07")
  expect_equal(t$countries, c(BR = 18, US = 2, PT = 1))
  # an empty document (nothing counted yet)
  t0 <- e$.vis_totals(list(fields = list()))
  expect_equal(t0$visitors, 0); expect_length(t0$countries, 0L); expect_true(is.na(t0$since))
})

test_that("off Cloud Run nothing is sent and nothing breaks", {
  skip_on_cran()
  e <- .vis_app_env()
  e$.vis_env$no_metadata <- TRUE
  expect_null(e$.vis_token())
  expect_false(e$.vis_record("visitors_test", TRUE, "BR"))
  expect_null(e$.vis_tick())
})

test_that("the country table of the map has a point and two names for the countries it places", {
  skip_on_cran()
  e <- .vis_app_env()
  ct <- utils::read.csv(file.path(e$app_dir, "data", "countries.csv"), stringsAsFactors = FALSE,
                        encoding = "UTF-8", na.strings = "")
  expect_true(all(grepl("^[A-Z]{2}$", ct$iso2)))
  expect_false(anyDuplicated(ct$iso2) > 0)
  placed <- ct[!is.na(ct$lon), ]
  expect_gt(nrow(placed), 200L)
  expect_true(all(abs(placed$lon) <= 180 & abs(placed$lat) <= 90))
  expect_equal(ct$name_en[ct$iso2 == "NA"], "Namibia")
  br <- ct[ct$iso2 == "BR", ]
  expect_equal(c(br$name_en, br$name_pt), c("Brazil", "Brasil"))
  expect_true(br$lon > -74 && br$lon < -34 && br$lat > -34 && br$lat < 6)
})

test_that("the footer's strings exist in both languages with the same placeholders", {
  skip_on_cran()
  y <- yaml::read_yaml(system.file("i18n", "translations.yaml", package = "soilKey"))
  if (is.null(y)) y <- yaml::read_yaml(file.path("inst", "i18n", "translations.yaml"))
  k <- grep("^visitors\\.", names(y$en), value = TRUE)
  expect_setequal(k, c("visitors.online", "visitors.people", "visitors.countries",
                       "visitors.since", "visitors.note", "visitors.credit",
                       "visitors.map_show", "visitors.map_title"))
  ph <- function(x) regmatches(x, gregexpr("%[sd]", x))[[1]]
  for (key in k) expect_identical(ph(y$en[[key]]), ph(y$pt[[key]]), info = key)
})

test_that("numbers are written the language's way, with no warning", {
  skip_on_cran()
  e <- .vis_app_env()
  sys.source(file.path(e$app_dir, "R", "mod_visitors.R"), envir = e)
  expect_no_warning(pt <- e$.vis_fmt(c(7, 1234, 1234567), "pt"))
  expect_equal(pt, c("7", "1.234", "1.234.567"))
  expect_equal(e$.vis_fmt(1234567, "en"), "1,234,567")
})

test_that("the footer keeps the map in a panel that opens on a click", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("leaflet")
  d <- .vis_app_env()$app_dir
  # the module's UI, built where shiny's functions are found, as in the app
  e <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(d, "R", "utils_visitors.R"), envir = e)
  sys.source(file.path(d, "R", "mod_visitors.R"), envir = e)
  e$i18n <- function(key, ...) key
  ui <- as.character(e$visitors_ui("visitors"))
  # the panel starts hidden and holds the map, the note and the credit
  expect_match(ui, '<div id="visitors-panel" class="sk-visitors-panel" hidden', fixed = TRUE)
  expect_match(ui, 'id="visitors-map"', fixed = TRUE)
  expect_match(ui, "https://db-ip.com", fixed = TRUE)
  # the script opens it from the numbers' button and draws the map when shown
  expect_match(ui, "var panel = '#visitors-panel', btn = '.sk-visitors-toggle';", fixed = TRUE)
  expect_match(ui, "p.trigger('shown')", fixed = TRUE)
  expect_match(ui, "Shiny.setInputValue('visitors-first_visit'", fixed = TRUE)
})

test_that("the app ships the visitor module, and Classify's key trace is not a fill item", {
  skip_on_cran()
  e <- .vis_app_env()
  cls <- readLines(file.path(e$app_dir, "R", "mod_classify.R"), warn = FALSE)
  expect_true(any(grepl('DT::DTOutput(ns("trace_table"), fill = FALSE)', cls, fixed = TRUE)))
})
