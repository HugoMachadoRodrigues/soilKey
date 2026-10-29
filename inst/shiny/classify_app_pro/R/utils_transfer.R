# =============================================================================
# soilKey Pro -- uploads and downloads over the session's websocket (v0.9.303).
#
# Shiny's fileInput() and downloadHandler() move the file by plain HTTP, to
# /session/<token>/upload/<id> and /session/<token>/download/<id>. Only the R
# process that holds the session knows those addresses. On Cloud Run the app
# runs on up to three instances and session affinity is best effort: while a
# new instance starts, a request of the page can reach another instance, which
# answers 404 (seen on soilkeypro.com in October 2026, both for uploads and for
# downloads). The websocket stays on the instance that holds the session, so
# these controls move the file over it instead:
#
#   * sk_file_input() draws the markup of fileInput(), but its <input
#     type=file> has no id, so Shiny's own file binding leaves it alone.
#     www/soilkey-transfer.js reads the file in slices and sends each slice as
#     an input value; the server appends it to a temporary file and
#     acknowledges it before the next slice goes. The last message sets
#     input$<id> to the one-row data frame fileInput() gives (name, size,
#     type, datapath), so the modules read it unchanged.
#   * sk_download_button() / sk_download_link() are action buttons. On a
#     click, sk_download() runs the content function into a temporary file and
#     sends the file back in slices as custom messages; the page joins them
#     into a Blob and saves it under the file name.
#
# The same JavaScript shows a notice with a Reload button when the websocket
# closes (the session ended, or the network dropped).
# =============================================================================

# Bytes per slice sent to the page. Base64 adds a third, so each websocket
# message stays under 700 KB.
.sk_transfer_slice <- 512L * 1024L

# The upload limit: Shiny's own option, so the limit is the one fileInput()
# applied (5 MB unless the app sets shiny.maxRequestSize). Inf = no limit.
.sk_upload_limit <- function() {
  lim <- getOption("shiny.maxRequestSize", 5 * 1024^2)
  if (!is.numeric(lim) || length(lim) != 1L || is.na(lim) || lim <= 0) Inf
  else lim
}

# An identifier for one transfer. tempfile() is used because it does not touch
# the R random number generator (the Uncertainty tab draws from it).
.sk_transfer_uid <- function() {
  gsub("[^A-Za-z0-9]", "", basename(tempfile("t")))
}

.sk_uid_ok <- function(uid) {
  is.character(uid) && length(uid) == 1L && grepl("^[A-Za-z0-9]{1,40}$", uid)
}

# ---- upload: UI -------------------------------------------------------------

# fileInput() markup with the file input left unbound: no id and no name on
# the <input type=file> (an element with neither is skipped by every Shiny
# version's binder). The outer container carries the input id.
sk_file_input <- function(inputId, label, accept = NULL) {
  lim <- .sk_upload_limit()
  q <- htmltools::tagQuery(shiny::fileInput(inputId, label, accept = accept))
  q$addAttrs(id = inputId)
  q$find("input")$filter(function(x, i) identical(x$attribs$type, "file"))$
    removeAttrs(c("id", "name"))$
    addAttrs(
      `data-sk-file`      = inputId,
      `data-sk-max`       = if (is.finite(lim)) format(lim, scientific = FALSE)
                            else "-1",
      `data-sk-done`      = i18n("transfer.upload_complete"),
      `data-sk-too-large` = i18n("transfer.too_large",
                                 if (is.finite(lim)) format(round(lim / 1024^2, 1))
                                 else "?"),
      `data-sk-failed`    = i18n("transfer.upload_failed"),
      `aria-labelledby`   = paste0(inputId, "-label"))
  q$allTags()
}

# ---- upload: server ---------------------------------------------------------

# The uploads of one session, kept in session$userData and removed from disk
# when the session ends.
.sk_upload_state <- function(session) {
  st <- session$userData$sk_uploads
  if (is.null(st)) {
    st <- new.env(parent = emptyenv())
    session$userData$sk_uploads <- st
    session$onSessionEnded(function() {
      for (u in ls(st)) unlink(dirname(st[[u]]$path), recursive = TRUE)
    })
  }
  st
}

# One slice: list(uid, k, n, data = base64, name = on the first slice).
# Appends it to the upload's file and acknowledges it ("sk_file_ack"), with
# error = "too_large" or "failed" when it is refused. Returns the error code
# (NULL when the slice was taken), which the tests read.
.sk_upload_chunk <- function(data, session) {
  uid <- as.character(data$uid %||% "")
  k <- suppressWarnings(as.integer(data$k %||% NA))
  n <- suppressWarnings(as.integer(data$n %||% NA))
  ack <- function(error = NULL) {
    if (.sk_uid_ok(uid)) {
      session$sendCustomMessage("sk_file_ack",
                                list(uid = uid, k = k, error = error))
    }
    invisible(error)
  }
  if (!.sk_uid_ok(uid) || length(k) != 1L || length(n) != 1L ||
      is.na(k) || is.na(n) || n < 1L || k < 0L || k >= n)
    return(ack("failed"))
  st <- .sk_upload_state(session)
  if (k == 0L) {
    if (!is.null(st[[uid]])) return(ack("failed"))
    ext <- gsub("[^A-Za-z0-9]", "",
                tools::file_ext(as.character(data$name %||% "")))
    d <- tempfile("sk_upload_")
    dir.create(d)
    # like Shiny's datapath: "0" plus the original extension, which the
    # readers of photos and tables rely on
    st[[uid]] <- list(path = file.path(d, paste0("0", if (nzchar(ext))
                                                   paste0(".", tolower(ext)))),
                      next_k = 0L, n = n, bytes = 0, done = FALSE)
  }
  u <- st[[uid]]
  if (is.null(u) || isTRUE(u$failed) || k != u$next_k || n != u$n)
    return(ack("failed"))
  bytes <- tryCatch(jsonlite::base64_dec(as.character(data$data %||% "")),
                    error = function(e) NULL)
  if (is.null(bytes)) {
    u$failed <- TRUE; st[[uid]] <- u
    return(ack("failed"))
  }
  if (u$bytes + length(bytes) > .sk_upload_limit()) {
    u$failed <- TRUE; st[[uid]] <- u
    unlink(u$path)
    return(ack("too_large"))
  }
  con <- file(u$path, if (k == 0L) "wb" else "ab")
  writeBin(bytes, con)
  close(con)
  u$next_k <- k + 1L
  u$bytes  <- u$bytes + length(bytes)
  u$done   <- u$next_k == u$n
  st[[uid]] <- u
  ack(NULL)
}

# The final message: list(uid, name, size, type). Returns the value
# fileInput() would give, or NULL while the file is incomplete.
.sk_upload_value <- function(data, session) {
  if (is.null(data)) return(NULL)
  uid <- as.character(data$uid %||% "")
  st <- session$userData$sk_uploads
  if (!.sk_uid_ok(uid) || is.null(st)) return(NULL)
  u <- st[[uid]]
  if (is.null(u) || !isTRUE(u$done) || isTRUE(u$failed) || !file.exists(u$path))
    return(NULL)
  data.frame(name     = as.character(data$name %||% basename(u$path))[1L],
             size     = as.numeric(file.size(u$path)),
             type     = as.character(data$type %||% "")[1L],
             datapath = u$path,
             stringsAsFactors = FALSE)
}

shiny::registerInputHandler("sk.file.chunk", function(data, shinysession, name) {
  .sk_upload_chunk(data, shinysession)
  NULL                        # the slice is not kept as an input value
}, force = TRUE)

shiny::registerInputHandler("sk.file", function(data, shinysession, name) {
  .sk_upload_value(data, shinysession)
}, force = TRUE)

# ---- download: UI -----------------------------------------------------------

sk_download_button <- function(inputId, label = "Download",
                               icon = shiny::icon("download"), class = NULL,
                               ...) {
  shiny::actionButton(inputId, label, icon = icon,
                      class = paste(c("sk-download", class), collapse = " "),
                      ...)
}

sk_download_link <- function(inputId, label = "Download", ...) {
  shiny::actionLink(inputId, label, class = "sk-download", ...)
}

# ---- download: server -------------------------------------------------------

.sk_content_type <- function(name) {
  switch(tolower(tools::file_ext(name)),
         csv  = "text/csv",
         json = "application/json",
         html = "text/html",
         pdf  = "application/pdf",
         gpkg = "application/geopackage+sqlite3",
         tif  = , tiff = "image/tiff",
         "application/octet-stream")
}

# Bytes -> base64 slices of at most `slice` bytes each (one slice when empty).
.sk_slices <- function(bytes, slice = .sk_transfer_slice) {
  n <- max(1L, as.integer(ceiling(length(bytes) / slice)))
  vapply(seq_len(n), function(k) {
    a <- (k - 1L) * slice + 1L
    b <- min(length(bytes), k * slice)
    jsonlite::base64_enc(if (b >= a) bytes[a:b] else raw(0))
  }, character(1L))
}

.sk_send_file <- function(session, bytes, name, type = .sk_content_type(name),
                          slice = .sk_transfer_slice) {
  parts <- .sk_slices(bytes, slice)
  uid <- .sk_transfer_uid()
  for (k in seq_along(parts)) {
    session$sendCustomMessage("sk_download", list(
      uid = uid, k = k - 1L, n = length(parts), name = name, type = type,
      data = parts[[k]]))
  }
  invisible(length(parts))
}

# What one click does: the file name, then the content written to a temporary
# file (the order downloadHandler() used), then the bytes to the page. An error
# in either shows its message as a notification; req() stops silently.
.sk_run_download <- function(session, filename, content) {
  notify <- function(e) {
    msg <- conditionMessage(e)
    if (!inherits(e, "shiny.silent.error") || nzchar(msg)) {
      if (!nzchar(msg)) msg <- i18n("transfer.download_failed")
      shiny::showNotification(msg, type = "error", duration = 8,
                              session = session)
    }
    invisible(FALSE)
  }
  name <- tryCatch(if (is.function(filename)) filename() else filename,
                   error = function(e) e)
  if (inherits(name, "error")) return(notify(name))
  name <- basename(as.character(name)[1L])
  ext <- tools::file_ext(name)
  tmp <- tempfile("sk_download_", fileext = if (nzchar(ext)) paste0(".", ext))
  on.exit(unlink(tmp), add = TRUE)
  res <- tryCatch({ content(tmp); NULL }, error = function(e) e)
  if (inherits(res, "error")) return(notify(res))
  if (!file.exists(tmp))
    return(notify(simpleError(i18n("transfer.download_failed"))))
  bytes <- readBin(tmp, "raw", n = file.size(tmp))
  .sk_send_file(session, bytes, name)
  invisible(TRUE)
}

# Server half of a download control: replaces
#   output$<id> <- downloadHandler(filename, content)
# with
#   sk_download("<id>", filename, content)
sk_download <- function(id, filename, content,
                        session = shiny::getDefaultReactiveDomain()) {
  shiny::observeEvent(session$input[[id]], {
    .sk_run_download(session, filename, content)
  }, ignoreInit = TRUE)
  invisible(NULL)
}
