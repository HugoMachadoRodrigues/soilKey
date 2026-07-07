# =============================================================================
# soilKey Pro -- "Talk to soilKey Pro" chat (v0.9.176).
#
# Replaces the old Photo tab's Mock/Local/Cloud provider selector with a single
# conversational assistant:
#
#   * With a FREE Groq API key (pasted here or in GROQ_API_KEY) it chats with a
#     real open Llama/Qwen model via ellmer -- no paid key, nothing runs on the
#     user's machine.
#   * With NO key it still answers, from a built-in SCRIPTED assistant grounded
#     in the current pedon and its deterministic classification, so the demo is
#     never dead.
#
# The model NEVER classifies: soilKey's deterministic keys do that, and the
# assistant only explains the result. Photo -> Munsell extraction is folded in:
# attach a soil photo and it reads the colours (a Groq vision model if a key is
# present, else the offline mock), landing them in the pedon exactly like the
# old Photo tab.
# =============================================================================

.GROQ_TEXT_MODEL   <- "llama-3.3-70b-versatile"
.GROQ_VISION_MODEL <- "meta-llama/llama-4-scout-17b-16e-instruct"

# Resolve the Groq key: the in-app field wins, else the GROQ_API_KEY env var.
.chat_groq_key <- function(field) {
  k <- if (!is.null(field) && nzchar(trimws(field))) trimws(field) else ""
  if (nzchar(k)) k else Sys.getenv("GROQ_API_KEY", "")
}

# Build an ellmer Groq chat (or NULL if no key / not available). api_key is
# deprecated in ellmer >= 0.4 in favour of credentials, so suppress that note.
.chat_make_groq <- function(key, model, system_prompt) {
  if (!nzchar(key) || !requireNamespace("ellmer", quietly = TRUE)) return(NULL)
  tryCatch(
    suppressWarnings(ellmer::chat_groq(
      system_prompt = system_prompt, model = model,
      api_key = key, echo = "none")),
    error = function(e) NULL)
}

# A compact, deterministic description of the current pedon + its classification.
# Fed to Groq as context AND consumed by the scripted fallback. Reports what
# classify_all() found; it never asks a model to classify.
.chat_pedon_context <- function(pedon, settings = NULL) {
  if (is.null(pedon)) return(NULL)
  st <- tryCatch(settings, error = function(e) NULL)
  res <- tryCatch(soilKey::classify_all(
    pedon, on_missing = "silent",
    include_familia = isTRUE(st$include_familia),
    include_family  = isTRUE(st$include_family),
    specifiers      = isTRUE(st$specifiers)),
    error = function(e) NULL)
  h <- tryCatch(as.data.frame(pedon$horizons), error = function(e) NULL)
  site <- pedon$site %||% list()
  lines <- c(
    sprintf("Site id: %s", site$id %||% "(unnamed)"),
    if (!is.null(site$lat) && !is.null(site$lon))
      sprintf("Location: lat %s, lon %s", site$lat, site$lon),
    if (!is.null(h) && nrow(h))
      sprintf("Horizons (%d): %s", nrow(h),
              paste(sprintf("%s %s-%s cm",
                            h$designation %||% "?", h$top_cm %||% "?",
                            h$bottom_cm %||% "?"), collapse = "; ")))
  say <- function(r, label) {
    if (is.null(r)) return(NULL)
    sprintf("%s: %s (%s; evidence grade %s)", label,
            r$name %||% "?", r$rsg_or_order %||% "?", r$evidence_grade %||% "?")
  }
  cls <- c(say(res$wrb, "WRB 2022"), say(res$sibcs, "SiBCS 5"),
           say(res$usda, "USDA ST"))
  missing <- tryCatch({
    md <- unique(unlist(lapply(res[c("wrb", "sibcs", "usda")],
                               function(r) if (!is.null(r)) r$missing_data)))
    if (length(md)) paste("Missing data:", paste(md, collapse = ", ")) else NULL
  }, error = function(e) NULL)
  list(text    = paste(c(lines, cls, missing), collapse = "\n"),
       results = res)
}

# Scripted (no-key) assistant: a keyword intent router over the current pedon
# context, so the demo answers real questions without any model.
.chat_scripted_reply <- function(msg, ctx) {
  if (is.null(ctx)) return(i18n("chat.scripted_need_pedon"))
  m <- tolower(msg %||% "")
  r <- ctx$results
  grade <- function(x) if (is.null(x)) "?" else x$evidence_grade %||% "?"
  if (grepl("horizon|camada|perfil|profile", m) && !grepl("wrb|sibcs|usda", m))
    return(paste0(i18n("chat.scripted_horizons"), "\n\n", ctx$text))
  if (grepl("\\bwrb\\b|world reference", m) && !is.null(r$wrb))
    return(sprintf("**WRB 2022:** %s\n\n%s: %s (%s)", r$wrb$name,
                   i18n("chat.reference_group"), r$wrb$rsg_or_order, grade(r$wrb)))
  if (grepl("sibcs|embrapa|brasil", m) && !is.null(r$sibcs))
    return(sprintf("**SiBCS 5:** %s (%s)", r$sibcs$name, grade(r$sibcs)))
  if (grepl("usda|soil taxonomy|order|great group", m) && !is.null(r$usda))
    return(sprintf("**USDA ST:** %s (%s)", r$usda$name, grade(r$usda)))
  if (grepl("missing|falta|why|por que|porque|grade|evid", m))
    return(paste0(i18n("chat.scripted_missing"), "\n\n", ctx$text))
  if (grepl("munsell|colou?r|\\bcor\\b", m))
    return(i18n("chat.scripted_photo_hint"))
  # default: full three-system summary + an offer to enable the live model
  paste0(i18n("chat.scripted_summary"), "\n\n", ctx$text, "\n\n",
         i18n("chat.scripted_addkey_hint"))
}

# One chat bubble (markdown -> HTML when commonmark is available).
.chat_bubble <- function(role, text) {
  cls <- if (identical(role, "user")) "sk-bubble sk-bubble-user"
         else "sk-bubble sk-bubble-bot"
  body <- if (requireNamespace("commonmark", quietly = TRUE))
    shiny::HTML(commonmark::markdown_html(text %||% ""))
  else shiny::HTML(gsub("\n", "<br/>", htmltools::htmlEscape(text %||% "")))
  shiny::div(class = "sk-bubble-row",
             shiny::div(class = cls, body))
}


# The chat now lives in a right-side slide-out DRAWER available on every tab
# (mounted in app.R), not a nav tab. This builds the drawer's inner content: a
# header, the transcript, and the composer. No API-key field and no photo upload
# -- the key comes from GROQ_API_KEY (else the grounded scripted assistant).
chat_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "sk-assistant-inner",
    shiny::div(
      class = "sk-assistant-head",
      shiny::div(
        shiny::span(class = "sk-assistant-title",
                    shiny::tags$img(src = "logo.png", class = "sk-assistant-logo",
                                    alt = "soilKey"), " ",
                    i18n("chat.drawer_title")),
        shiny::uiOutput(ns("backend_status"), inline = TRUE)),
      shiny::tags$button(
        id = "sk_assistant_close", class = "sk-assistant-close",
        type = "button", `aria-label` = i18n("chat.close"),
        shiny::icon("xmark"))),
    shiny::div(id = ns("log"), class = "sk-chat-log",
               role = "log", `aria-live` = "polite",
               shiny::uiOutput(ns("messages"))),
    shiny::div(
      class = "sk-chat-composer",
      shiny::textAreaInput(ns("msg"), NULL, width = "100%", rows = 2,
                           placeholder = i18n("chat.placeholder")),
      bslib::tooltip(
        shiny::actionButton(ns("send"), i18n("chat.send"),
                            icon = shiny::icon("paper-plane"),
                            class = "btn-primary"),
        "Send your message.")),
    shiny::div(class = "sk-assistant-foot small text-muted",
               i18n("chat.grounding_note"))
  )
}


chat_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    history  <- shiny::reactiveVal(list())
    chat_obj <- shiny::reactiveVal(NULL)
    chat_sig <- shiny::reactiveVal("")

    add <- function(role, text) {
      history(c(history(), list(list(role = role, text = text))))
      session$sendCustomMessage("sk_chat_scroll", ns("log"))
    }

    # A persistent Groq chat, rebuilt when the pedon / key / model changes so its
    # system prompt always reflects the current context (kept across turns while
    # those are stable, preserving conversation history).
    get_backend <- function() {
      key   <- .chat_groq_key(input$groq_key)
      model <- getOption("soilKey.groq_text_model", .GROQ_TEXT_MODEL)
      sig   <- paste(rv$pedon$site$id %||% "none", model, nzchar(key), sep = "|")
      if (nzchar(key) &&
          (!identical(sig, chat_sig()) || is.null(chat_obj()))) {
        ctx <- .chat_pedon_context(rv$pedon, tryCatch(settings(), error = function(e) NULL))
        sys <- paste0(i18n("chat.system_prompt"),
                      if (!is.null(ctx)) paste0("\n\n### Current pedon & deterministic classification\n", ctx$text))
        chat_obj(.chat_make_groq(key, model, sys))
        chat_sig(sig)
      }
      if (nzchar(key)) list(kind = "groq", chat = chat_obj())
      else list(kind = "scripted", chat = NULL)
    }

    output$backend_status <- shiny::renderUI({
      key <- .chat_groq_key(input$groq_key)
      if (nzchar(key))
        shiny::div(class = "small mb-2", style = "color:#3f6024;",
                   shiny::icon("circle-check"), " ", i18n("chat.backend_groq"))
      else
        shiny::div(class = "small mb-2 alert alert-light py-1 px-2 border",
                   shiny::icon("robot"), " ", i18n("chat.backend_scripted"))
    })
    # render eagerly so the status shows even while the settings sidebar starts
    # collapsed (otherwise the output stays suspended/pending until first shown)
    shiny::outputOptions(output, "backend_status", suspendWhenHidden = FALSE)

    output$messages <- shiny::renderUI({
      h <- history()
      if (!length(h))
        return(shiny::div(class = "text-muted p-3 text-center",
                          i18n("chat.empty")))
      shiny::tagList(lapply(h, function(m) .chat_bubble(m$role, m$text)))
    })

    # ---- send a text message ---------------------------------------------
    shiny::observeEvent(input$send, {
      msg <- trimws(input$msg %||% "")
      if (!nzchar(msg)) return()
      add("user", msg)
      shiny::updateTextAreaInput(session, "msg", value = "")
      ctx     <- .chat_pedon_context(rv$pedon, tryCatch(settings(), error = function(e) NULL))
      backend <- get_backend()
      if (identical(backend$kind, "scripted") || is.null(backend$chat)) {
        add("assistant", .chat_scripted_reply(msg, ctx))
        return()
      }
      reply <- shiny::withProgress(
        message = i18n("chat.thinking"), value = 0.5,
        tryCatch(as.character(backend$chat$chat(msg)),
                 error = function(e) NULL))
      if (is.null(reply) || !nzchar(reply))
        add("assistant", paste0(i18n("chat.groq_failed"), "\n\n",
                                .chat_scripted_reply(msg, ctx)))
      else add("assistant", reply)
    })
  })
}
