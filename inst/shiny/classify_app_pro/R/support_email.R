# =============================================================================
# Server-side support email for soilKey Pro.
#
# The Support tab lets a user send a message to the maintainer WITHOUT the
# maintainer's address ever reaching the browser. The form posts to the Shiny
# server (this code), which verifies a Cloudflare Turnstile token and sends the
# message through the Resend API. Every secret is read from an environment
# variable and never rendered to the client.
#
# Environment variables (set as Cloud Run secrets, see SUPPORT_EMAIL_SETUP.md):
#   RESEND_API_KEY        Resend API key.                              [required]
#   SOILKEY_SUPPORT_TO    Destination -- use a Cloudflare Email Routing
#                         alias (e.g. support@soilkeypro.com) that forwards to
#                         your private inbox, so the real address is never in
#                         code, the container, or the DOM.             [required]
#   SOILKEY_SUPPORT_FROM  Verified sender on your domain, e.g.
#                         "soilKey Pro <support@soilkeypro.com>".      [required]
#   TURNSTILE_SITE_KEY    Cloudflare Turnstile site key (public).      [optional]
#   TURNSTILE_SECRET_KEY  Cloudflare Turnstile secret key.             [optional]
#
# If the three required vars are unset the form is hidden and the Support tab
# falls back to a GitHub-Issues link, so the app still runs locally and in CI.
# =============================================================================

#' Is server-side support email configured?
#' @keywords internal
support_mail_enabled <- function() {
  nzchar(Sys.getenv("RESEND_API_KEY")) &&
    nzchar(Sys.getenv("SOILKEY_SUPPORT_TO")) &&
    nzchar(Sys.getenv("SOILKEY_SUPPORT_FROM"))
}

#' Turnstile helpers
#' @keywords internal
turnstile_site_key <- function() Sys.getenv("TURNSTILE_SITE_KEY", "")
#' @keywords internal
turnstile_enabled  <- function() {
  nzchar(Sys.getenv("TURNSTILE_SECRET_KEY")) && nzchar(turnstile_site_key())
}

#' Minimal, dependency-free e-mail syntax check.
#' @keywords internal
.sk_valid_email <- function(x) {
  is.character(x) && length(x) == 1L && nzchar(x) && nchar(x) <= 254L &&
    grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$", x)
}

#' Verify a Cloudflare Turnstile token server-side.
#'
#' Returns list(ok = logical, skipped = logical, reason = character). When
#' Turnstile is not configured the check is skipped (ok = TRUE) so the form
#' still works behind the honeypot + rate limit.
#' @keywords internal
verify_turnstile <- function(token, remoteip = NULL) {
  if (!turnstile_enabled()) return(list(ok = TRUE, skipped = TRUE, reason = ""))
  if (!is.character(token) || !nzchar(token))
    return(list(ok = FALSE, skipped = FALSE, reason = "missing-token"))
  body <- list(secret = Sys.getenv("TURNSTILE_SECRET_KEY"), response = token)
  if (!is.null(remoteip) && nzchar(remoteip)) body$remoteip <- remoteip
  resp <- tryCatch(
    httr::POST("https://challenges.cloudflare.com/turnstile/v0/siteverify",
               body = body, encode = "form", httr::timeout(10)),
    error = function(e) NULL)
  if (is.null(resp)) return(list(ok = FALSE, skipped = FALSE, reason = "unreachable"))
  out <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"),
                  error = function(e) list())
  list(ok = isTRUE(out$success), skipped = FALSE,
       reason = paste(unlist(out$`error-codes`), collapse = ","))
}

#' Send one support message via the Resend API.
#'
#' The destination and API key are read from the environment; the user's own
#' address goes into Reply-To (never From, which must be a verified domain
#' sender). Returns list(ok = logical, error = character).
#' @keywords internal
send_support_email <- function(name, reply_to, subject, message,
                               meta = list()) {
  to   <- Sys.getenv("SOILKEY_SUPPORT_TO")
  from <- Sys.getenv("SOILKEY_SUPPORT_FROM")
  key  <- Sys.getenv("RESEND_API_KEY")
  if (!nzchar(to) || !nzchar(from) || !nzchar(key))
    return(list(ok = FALSE, error = "not-configured"))

  name    <- .sk_clip(name, 200L)
  subject <- .sk_clip(subject, 200L)
  message <- .sk_clip(message, 5000L)
  metaTxt <- if (length(meta))
    paste0("\n", paste(sprintf("%s: %s", names(meta), unlist(meta)),
                       collapse = "\n")) else ""

  text <- paste0(
    "New support message from soilKey Pro\n\n",
    "Name:     ", name, "\n",
    "Reply-to: ", reply_to, "\n",
    "Subject:  ", subject, "\n\n",
    "Message:\n", message, "\n",
    metaTxt, "\n\n--- sent from soilkeypro.com\n")

  payload <- list(
    from    = from,
    to      = list(to),
    subject = paste0("[soilKey Pro support] ", subject),
    text    = text)
  # Reply-To so the maintainer can answer the user directly.
  if (.sk_valid_email(reply_to)) payload[["reply_to"]] <- reply_to

  resp <- tryCatch(
    httr::POST("https://api.resend.com/emails",
               httr::add_headers(Authorization = paste("Bearer", key)),
               body = payload, encode = "json", httr::timeout(15)),
    error = function(e) NULL)
  if (is.null(resp)) return(list(ok = FALSE, error = "unreachable"))
  code <- httr::status_code(resp)
  if (code >= 200 && code < 300) return(list(ok = TRUE, error = ""))
  list(ok = FALSE, error = paste0("provider-", code))
}

#' Trim + hard-cap a user string.
#' @keywords internal
.sk_clip <- function(x, n) {
  x <- if (is.null(x) || is.na(x[1])) "" else as.character(x)[1]
  x <- trimws(x)
  if (nchar(x) > n) substr(x, 1L, n) else x
}
