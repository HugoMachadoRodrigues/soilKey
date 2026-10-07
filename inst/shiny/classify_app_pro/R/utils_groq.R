# =============================================================================
# soilKey Pro -- choosing the Groq model at run time.
#
# Groq retires models every few months, and each retirement broke this app
# with an HTTP 404: meta-llama/llama-4-scout-17b-16e-instruct in August 2026
# (reported from ISRIC), then qwen/qwen3.6-27b and llama-3.3-70b-versatile by
# October 2026, which took down the Photo tab and the Assistant together. A
# fixed model name is a time bomb.
#
# So the model is chosen from Groq's own list of what the account can use right
# now (GET /models: free, no tokens spent), by an order of preference, and the
# list is kept for an hour per R process. If a call still comes back with "model
# does not exist" -- a retirement inside that hour -- the list is dropped, a new
# model is chosen and the call is retried once (.groq_model_gone()).
#
# An explicit choice still wins, as before, but only while Groq still offers it:
#   options(soilKey.groq_vision_model = ) / $GROQ_VISION_MODEL
#   options(soilKey.groq_text_model = )   / $GROQ_TEXT_MODEL
# =============================================================================

.GROQ_MODELS_URL <- "https://api.groq.com/openai/v1/models"

# Preference, most wanted first. Each entry is a pattern; among the models that
# match it, the newest version wins (qwen3.10 over qwen3.8 over qwen3.6), so a
# routine version bump is followed without a code change.
#
# Text: on 2026-10-07 the same grounded questions went to both candidates.
# Qwen3 stayed with the evidence; gpt-oss-120b twice put Alisols after Luvisols
# in the WRB key (they come before, and the trace said so) and made up SiBCS
# rules. gpt-oss stays second: it has its own rate limit, so it answers when
# Qwen is at its per-minute limit (.groq_rate_limited()).
.GROQ_PREFS <- list(
  vision = c("^qwen/qwen3\\.[0-9]+-27b$",
             "^meta-llama/llama-4-maverick",
             "^meta-llama/llama-4-scout",
             "vision"),
  text   = c("^qwen/qwen3\\.[0-9]+-27b$",
             "^openai/gpt-oss-120b$",
             "^openai/gpt-oss-20b$",
             "^qwen/qwen3",
             "^llama-3\\.3-70b",
             "^meta-llama/llama-4"))

# Used only when Groq's list cannot be read at all (network down, key rejected):
# the last models verified to work, on 2026-10-07, in order of preference.
.GROQ_FALLBACK <- list(vision = "qwen/qwen3.8-27b",
                       text   = c("qwen/qwen3.8-27b", "openai/gpt-oss-120b"))

.groq_cache <- new.env(parent = emptyenv())

# Newest version first: numbers are compared as numbers, so "3.10" > "3.8".
.groq_newest_first <- function(x) {
  if (length(x) < 2L) return(x)
  pad <- function(s) {
    m <- gregexpr("[0-9]+", s)
    regmatches(s, m) <- lapply(regmatches(s, m),
                               function(d) formatC(as.numeric(d), width = 8,
                                                   format = "d", flag = "0"))
    s
  }
  x[order(vapply(x, pad, character(1)), decreasing = TRUE)]
}

# The ids Groq lists as available to this key, cached for `ttl` seconds.
# NULL when the list cannot be read.
.groq_available_models <- function(key, ttl = 3600) {
  if (!nzchar(key %||% "")) return(NULL)
  fresh <- !is.null(.groq_cache$ids) &&
    as.numeric(difftime(Sys.time(), .groq_cache$at, units = "secs")) < ttl
  if (fresh) return(.groq_cache$ids)
  ids <- tryCatch({
    r <- httr::GET(.GROQ_MODELS_URL,
                   httr::add_headers(Authorization = paste("Bearer", key)),
                   httr::timeout(10))
    if (httr::status_code(r) != 200L) NULL else {
      d <- httr::content(r, as = "parsed", type = "application/json")
      live <- Filter(function(m) !isFALSE(m$active), d$data %||% list())
      vapply(live, function(m) as.character(m$id), character(1))
    }
  }, error = function(e) NULL)
  if (length(ids)) {
    .groq_cache$ids <- ids
    .groq_cache$at  <- Sys.time()
  }
  if (length(ids)) ids else NULL
}

# Forget the cached list, so the next call reads it again.
.groq_forget_models <- function() {
  rm(list = intersect(c("ids", "at"), ls(.groq_cache)), envir = .groq_cache)
  invisible(NULL)
}

# The first preference that matches something available, newest version first.
.groq_pick <- function(ids, prefs) {
  for (p in prefs) {
    hit <- ids[grepl(p, ids)]
    if (length(hit)) return(.groq_newest_first(hit)[1])
  }
  NA_character_
}

# The model to use for `kind` ("vision" or "text"): an explicit choice if Groq
# still offers it, else the best available, else NA when nothing suitable is
# offered. If the list cannot be read, the explicit choice or the last verified
# default is returned and the call itself will say whether it works. `exclude`
# names models not to use this time (one at its rate limit).
.groq_model <- function(kind = c("vision", "text"),
                        key = Sys.getenv("GROQ_API_KEY", ""),
                        exclude = character(0)) {
  kind <- match.arg(kind)
  opt <- getOption(sprintf("soilKey.groq_%s_model", kind), default = NULL)
  env <- Sys.getenv(sprintf("GROQ_%s_MODEL", toupper(kind)), "")
  wanted <- setdiff(c(if (!is.null(opt) && nzchar(opt)) opt, if (nzchar(env)) env),
                    exclude)
  ids <- .groq_available_models(key)
  if (is.null(ids)) {
    pick <- c(wanted, setdiff(.GROQ_FALLBACK[[kind]], exclude))
    return(if (length(pick)) pick[1] else NA_character_)
  }
  ids <- setdiff(ids, exclude)
  for (w in wanted) if (w %in% ids) return(w)
  .groq_pick(ids, .GROQ_PREFS[[kind]])
}

# Did this error come from asking for a model Groq no longer serves?
.groq_model_gone <- function(err) {
  msg <- if (inherits(err, "condition")) conditionMessage(err) else as.character(err)
  grepl("does not exist|decommission|model_not_found|no longer supported",
        msg, ignore.case = TRUE)
}

# Did this error come from Groq's per-minute limits (requests, tokens, or
# output tokens)? Waiting for them would hold the whole R process, and every
# session on it, for up to a minute; another model has its own allowance.
.groq_rate_limited <- function(err) {
  msg <- if (inherits(err, "condition")) conditionMessage(err) else as.character(err)
  grepl("\\b429\\b|rate limit|too many requests|request too large|per minute",
        msg, ignore.case = TRUE)
}

# Sampling for the Assistant. A low temperature keeps the model on the evidence
# it is given; the output cap keeps a reply inside Qwen's 1,000 output tokens a
# minute. gpt-oss reasons before it answers, and that counts as output too.
.groq_text_params <- function(model) {
  list(temperature = 0.2,
       max_tokens  = if (grepl("gpt-oss", model %||% "", ignore.case = TRUE)) 1500L
                     else 800L)
}

# Extra request fields for a model. Qwen3 reasons before answering unless told
# not to. gpt-oss always reasons; "low" keeps a grounded answer from spending
# over a thousand tokens of the free tier's 8,000 a minute on it. Other families
# may reject the field, so it is sent to these two only.
.groq_api_args <- function(model) {
  m <- model %||% ""
  if (grepl("qwen", m, ignore.case = TRUE)) list(reasoning_effort = "none")
  else if (grepl("gpt-oss", m, ignore.case = TRUE)) list(reasoning_effort = "low")
  else list()
}
