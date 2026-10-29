# v0.9.303: the Pro app moves uploads and downloads over the session's
# websocket (inst/shiny/classify_app_pro/R/utils_transfer.R), because Shiny's
# own HTTP endpoints answer 404 when Cloud Run sends the request to another
# instance. These tests drive the server half with a stand-in session.

.v09303_app_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d))
    d <- file.path("inst", "shiny", "classify_app_pro")
  d
}

.v09303_env <- function() {
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(.v09303_app_dir(), "R"), pattern = "[.]R$",
                       full.names = TRUE)) {
    sys.source(f, envir = env)
  }
  env
}

# A stand-in for the Shiny session: userData, the custom messages it was sent,
# and the callbacks it would run when the session ends.
.v09303_session <- function() {
  s <- new.env()
  s$userData <- new.env()
  s$sent <- list()
  s$ended <- list()
  s$sendCustomMessage <- function(type, message) {
    s$sent[[length(s$sent) + 1L]] <- list(type = type, message = message)
  }
  s$onSessionEnded <- function(fun) s$ended[[length(s$ended) + 1L]] <- fun
  s
}

.v09303_slices <- function(bytes, size) {
  n <- max(1L, ceiling(length(bytes) / size))
  lapply(seq_len(n), function(k) {
    a <- (k - 1L) * size + 1L
    b <- min(length(bytes), k * size)
    if (b >= a) bytes[a:b] else raw(0)
  })
}

test_that("an upload sent in slices is rebuilt byte for byte", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- .v09303_env()
  s <- .v09303_session()
  bytes <- as.raw(rep_len(0:255, 2500L))
  parts <- .v09303_slices(bytes, 1000L)
  for (k in seq_along(parts)) {
    msg <- list(uid = "abc123", k = k - 1L, n = length(parts),
                data = jsonlite::base64_enc(parts[[k]]))
    if (k == 1L) msg$name <- "profile.JPG"
    expect_null(env$.sk_upload_chunk(msg, s))
  }
  acks <- Filter(function(m) m$type == "sk_file_ack", s$sent)
  expect_length(acks, 3L)
  expect_true(all(vapply(acks, function(m) is.null(m$message$error), TRUE)))
  v <- env$.sk_upload_value(list(uid = "abc123", name = "profile.JPG",
                                 size = 2500, type = "image/jpeg"), s)
  expect_s3_class(v, "data.frame")
  expect_identical(names(v), c("name", "size", "type", "datapath"))
  expect_identical(v$name, "profile.JPG")
  expect_identical(v$type, "image/jpeg")
  expect_identical(v$size, 2500)
  # the extension is kept, as in Shiny's datapath
  expect_identical(basename(v$datapath), "0.jpg")
  expect_identical(readBin(v$datapath, "raw", 1e4), bytes)
  # the session end removes the file
  expect_length(s$ended, 1L)
  s$ended[[1L]]()
  expect_false(file.exists(v$datapath))
})

test_that("an empty file arrives as one empty slice", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- .v09303_env()
  s <- .v09303_session()
  expect_null(env$.sk_upload_chunk(
    list(uid = "e1", k = 0L, n = 1L, data = "", name = "empty.csv"), s))
  v <- env$.sk_upload_value(list(uid = "e1", name = "empty.csv"), s)
  expect_identical(v$size, 0)
  expect_identical(basename(v$datapath), "0.csv")
})

test_that("an incomplete, out-of-order or unknown upload gives no value", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- .v09303_env()
  s <- .v09303_session()
  one <- jsonlite::base64_enc(as.raw(1:10))
  expect_null(env$.sk_upload_chunk(
    list(uid = "u1", k = 0L, n = 3L, data = one, name = "a.csv"), s))
  # slice 2 before slice 1
  expect_identical(env$.sk_upload_chunk(
    list(uid = "u1", k = 2L, n = 3L, data = one), s), "failed")
  expect_null(env$.sk_upload_value(list(uid = "u1"), s))
  # a slice of an upload that never started
  expect_identical(env$.sk_upload_chunk(
    list(uid = "zz", k = 1L, n = 3L, data = one), s), "failed")
  # an id that is not plain letters and digits is refused without an ack
  n_sent <- length(s$sent)
  expect_identical(env$.sk_upload_chunk(
    list(uid = "../x", k = 0L, n = 1L, data = one), s), "failed")
  expect_length(s$sent, n_sent)
  expect_null(env$.sk_upload_value(list(uid = "../x"), s))
  expect_null(env$.sk_upload_value(NULL, s))
})

test_that("an upload over shiny.maxRequestSize is refused", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- .v09303_env()
  s <- .v09303_session()
  old <- options(shiny.maxRequestSize = 15)
  on.exit(options(old), add = TRUE)
  b <- jsonlite::base64_enc(as.raw(1:10))
  expect_null(env$.sk_upload_chunk(
    list(uid = "big", k = 0L, n = 2L, data = b, name = "big.csv"), s))
  expect_identical(env$.sk_upload_chunk(
    list(uid = "big", k = 1L, n = 2L, data = b), s), "too_large")
  expect_identical(s$sent[[length(s$sent)]]$message$error, "too_large")
  expect_null(env$.sk_upload_value(list(uid = "big"), s))
  expect_identical(env$.sk_upload_limit(), 15)
})

test_that("a download is sent in base64 slices that rebuild the file", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- .v09303_env()
  bytes <- as.raw(rep_len(c(0:255, 10L), 3000L))
  sl <- env$.sk_slices(bytes, 1024L)
  expect_length(sl, 3L)
  expect_identical(do.call(c, lapply(sl, jsonlite::base64_dec)), bytes)
  expect_length(env$.sk_slices(raw(0), 1024L), 1L)

  s <- .v09303_session()
  ok <- env$.sk_run_download(
    s, function() "soilKey_session_P1.json",
    function(file) writeBin(bytes, file))
  expect_true(ok)
  msgs <- Filter(function(m) m$type == "sk_download", s$sent)
  expect_gte(length(msgs), 1L)
  m1 <- msgs[[1L]]$message
  expect_identical(m1$name, "soilKey_session_P1.json")
  expect_identical(m1$type, "application/json")
  expect_identical(vapply(msgs, function(m) m$message$k, 1L),
                   seq_along(msgs) - 1L)
  expect_true(all(vapply(msgs, function(m) m$message$n, 1L) == length(msgs)))
  expect_identical(do.call(c, lapply(msgs, function(m)
    jsonlite::base64_dec(m$message$data))), bytes)
})

test_that("a download that fails sends nothing", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- .v09303_env()
  s <- .v09303_session()
  # req() stops silently, as it did inside downloadHandler(); no session is
  # needed for that path
  expect_false(env$.sk_run_download(s, "x.csv",
                                    function(file) shiny::req(FALSE)))
  expect_length(s$sent, 0L)
})

test_that("the content type follows the file extension", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- .v09303_env()
  expect_identical(env$.sk_content_type("a.csv"), "text/csv")
  expect_identical(env$.sk_content_type("a.HTML"), "text/html")
  expect_identical(env$.sk_content_type("a.gpkg"),
                   "application/geopackage+sqlite3")
  expect_identical(env$.sk_content_type("a.tif"), "image/tiff")
  expect_identical(env$.sk_content_type("a"), "application/octet-stream")
})

test_that("sk_file_input leaves its file input unbound", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- .v09303_env()
  html <- as.character(env$sk_file_input("pedon-session_file", "Open",
                                         accept = ".json"))
  file_tag <- regmatches(html, regexpr("<input[^>]*type=\"file\"[^>]*>", html))
  expect_length(file_tag, 1L)
  # with no id and no name, Shiny's file binding skips the element, so the
  # browser never posts to /session/<token>/upload/
  expect_false(grepl("\\sid=", file_tag))
  expect_false(grepl("\\sname=", file_tag))
  expect_match(file_tag, "data-sk-file=\"pedon-session_file\"", fixed = TRUE)
  expect_match(file_tag, "accept=\".json\"", fixed = TRUE)
  expect_match(html, "id=\"pedon-session_file\"", fixed = TRUE)
})

test_that("the app uses no HTTP upload or download control", {
  skip_on_cran()
  app_dir <- .v09303_app_dir()
  files <- c(file.path(app_dir, "app.R"),
             list.files(file.path(app_dir, "R"), pattern = "[.]R$",
                        full.names = TRUE))
  files <- files[basename(files) != "utils_transfer.R"]
  code <- unlist(lapply(files, function(f) {
    x <- readLines(f, warn = FALSE)
    sub("#.*$", "", x)
  }))
  for (pat in c("fileInput\\(", "downloadHandler\\(", "downloadButton\\(",
                "downloadLink\\(")) {
    expect_false(any(grepl(pat, code)), info = pat)
  }
  expect_true(file.exists(file.path(app_dir, "www", "soilkey-transfer.js")))
  # a download id is an input now, not an output: outputOptions() or
  # output$<id> on it stops the session ("not in list of output objects")
  for (f in files) {
    x <- sub("#.*$", "", readLines(f, warn = FALSE))
    ids <- regmatches(x, regexpr("sk_download\\(\"[A-Za-z0-9_]+\"", x))
    ids <- sub("^sk_download\\(\"([A-Za-z0-9_]+)\"$", "\\1", ids)
    for (id in ids) {
      expect_false(any(grepl(sprintf("outputOptions\\([^)]*\"%s\"", id), x)),
                   info = paste(basename(f), id))
      expect_false(any(grepl(sprintf("output\\$%s\\b", id), x)),
                   info = paste(basename(f), id))
    }
  }
})

test_that("every module server starts with the new controls", {
  skip_on_cran()
  for (p in c("shiny", "bslib", "DT", "plotly", "shinyWidgets", "leaflet"))
    skip_if_not_installed(p)
  env <- .v09303_env()
  rv <- shiny::reactiveValues(pedon = NULL, example_request = 0L,
                              include_family = FALSE, specifiers = FALSE)
  settings <- shiny::reactive(list())
  # the modules with an upload or a download; each must start without error
  for (srv in c("pedon_server", "report_server", "map_server",
                "map_batch_server", "map_grid_server", "spectra_server",
                "photo_server")) {
    fun <- get(srv, envir = env)
    wrapper <- if ("settings" %in% names(formals(fun))) {
      function(id) fun(id, rv = rv, settings = settings)
    } else {
      function(id) fun(id, rv = rv)
    }
    expect_no_error(
      shiny::testServer(wrapper, args = list(id = "t"), {
        session$flushReact()
      }),
      message = srv)
  }
})

test_that("the transfer strings exist in English and Portuguese", {
  skip_on_cran()
  skip_if_not_installed("yaml")
  path <- system.file("i18n", "translations.yaml", package = "soilKey")
  if (!nzchar(path)) path <- file.path("inst", "i18n", "translations.yaml")
  cat <- yaml::read_yaml(path)
  keys <- c("transfer.upload_complete", "transfer.too_large",
            "transfer.upload_failed", "transfer.download_failed",
            "transfer.lost_title", "transfer.lost_body", "transfer.reload")
  for (lang in c("en", "pt")) {
    expect_true(all(keys %in% names(cat[[lang]])), info = lang)
  }
})
