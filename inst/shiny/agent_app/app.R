# =============================================================================
# soilKey -- Agente Pedometrista (Shiny app, v0.9.65+)
#
# A modern bslib-themed Shiny UI for end-to-end soil profile classification:
#   1. Upload a photo / PDF / fieldsheet image / Vis-NIR spectrum.
#   2. The local Gemma 4 (or any cloud provider) extracts schema-validated
#      structured data into a soilKey PedonRecord.
#   3. The deterministic R taxonomic key classifies the pedon under
#      WRB 2022 + SiBCS 5a edicao + USDA Soil Taxonomy 13ed.
#   4. A free-form chat tab lets the user ask the local Gemma about the
#      loaded profile in PT-BR or English.
#
# The VLM never classifies. It only extracts. The keys are pure R.
#
# Launch from R:
#     soilKey::run_agent_app()
# Or directly:
#     shiny::runApp(system.file("shiny", "agent_app", package = "soilKey"))
# =============================================================================


library(shiny)
suppressPackageStartupMessages({
  library(bslib)
  library(bsicons)
  library(soilKey)
})


# ---- Tema visual ----------------------------------------------------------

agent_theme <- bs_theme(
  version    = 5,
  bootswatch = "minty",
  primary    = "#FF6B35",  # cor do hex sticker do soilKey
  base_font  = font_google("Inter", local = TRUE),
  heading_font = font_google("Inter", local = TRUE),
  code_font  = font_google("JetBrains Mono", local = TRUE)
)


# ---- Status card helpers --------------------------------------------------

# Compact status badge for the sidebar.
.status_badge <- function(label, ok, hint = NULL) {
  color <- if (isTRUE(ok)) "#28a745" else "#6c757d"
  icon  <- if (isTRUE(ok)) "check-circle-fill" else "circle"
  tags$div(
    style = "display:flex;align-items:center;gap:8px;margin:4px 0;",
    bs_icon(icon, size = "1em", color = color),
    tags$span(label),
    if (!is.null(hint)) tags$small(class = "text-muted",
                                      style = "margin-left:auto;",
                                      hint)
  )
}


# Empty PedonRecord factory used as the initial reactive state.
.empty_pedon <- function() {
  PedonRecord$new(
    site = list(id = "agent-session", country = "BR"),
    horizons = ensure_horizon_schema(
      data.table::data.table(top_cm = numeric(0),
                              bottom_cm = numeric(0))
    )
  )
}


# Pretty-print a ClassificationResult into bslib value_box arguments.
.classification_card <- function(res, system_label, theme_color = "primary") {
  if (is.null(res) || inherits(res, "error")) {
    return(value_box(
      title = system_label,
      value = "(falha)",
      showcase = bs_icon("x-circle"),
      theme = "danger"
    ))
  }
  value_box(
    title = system_label,
    value = res$name %||% "(no name)",
    p(tags$small("Evidence grade: ",
                   tags$strong(res$evidence_grade %||% "?"))),
    p(tags$small("RSG / Order: ",
                   res$rsg_or_order %||% "(none)")),
    showcase = bs_icon("globe-americas"),
    theme = theme_color,
    full_screen = TRUE
  )
}


# ---- UI -------------------------------------------------------------------

ui <- page_navbar(
  title  = tagList(
    bs_icon("compass"),
    "soilKey - Agente Pedometrista"
  ),
  theme  = agent_theme,
  bg     = "#FF6B35",
  inverse = TRUE,
  underline = TRUE,
  fillable = TRUE,
  fillable_mobile = FALSE,

  # Sidebar lives outside the navbar -- bslib supports this via sidebar arg
  sidebar = sidebar(
    width = 320,
    title = tagList(bs_icon("gear-fill"), " Status & Provider"),

    uiOutput("provider_status"),
    hr(),

    selectInput("provider", "Provider VLM",
                  choices = c("auto"      = "auto",
                                "Local Gemma (Ollama)" = "ollama",
                                "Anthropic (Claude)"   = "anthropic",
                                "OpenAI (GPT-4o)"      = "openai",
                                "Google (Gemini)"      = "google"),
                  selected = "auto"),

    selectInput("model_preset", "Modelo Gemma local",
                  choices = c("Leve  (gemma4:e2b, ~1.5 GB)"      = "light",
                                "Equilibrado (gemma4:e4b, ~3 GB)"  = "balanced",
                                "Melhor (gemma4:31b, ~19 GB)"     = "best"),
                  selected = "light"),

    actionButton("setup_vlm", "Configurar Gemma local",
                  icon  = bs_icon("download"),
                  class = "btn-primary w-100"),

    hr(),

    selectInput("language", "Idioma do agente",
                  choices = c("Portugues (BR)" = "pt-BR",
                                "English"        = "en"),
                  selected = "pt-BR"),

    hr(),

    h6(tagList(bs_icon("file-earmark-text"), " Sessao atual")),
    uiOutput("session_summary"),

    hr(),

    actionButton("reset", "Limpar perfil",
                  icon  = bs_icon("arrow-counterclockwise"),
                  class = "btn-outline-secondary w-100")
  ),

  # ===================================================================
  nav_panel(
    title = tagList(bs_icon("camera"), " Foto Munsell"),

    layout_columns(
      col_widths = c(5, 7),

      card(
        card_header("Upload da foto do perfil"),
        fileInput("photo", NULL,
                    accept = c("image/png", "image/jpeg", "image/webp"),
                    buttonLabel = "Selecionar foto...",
                    placeholder = "Nenhuma foto carregada"),
        actionButton("extract_munsell", "Extrair Munsell com Gemma",
                      icon  = bs_icon("magic"),
                      class = "btn-primary w-100"),
        hr(),
        p(tags$small(class = "text-muted",
                       paste("Dica: fotos com placa Munsell visivel sao mais ",
                             "confiaveis. Sem a placa, o agente reporta ",
                             "confidence <= 0.5 explicitamente.")))
      ),

      card(
        card_header("Pre-visualizacao + horizontes extraidos"),
        imageOutput("photo_preview", height = "200px"),
        hr(),
        DT::DTOutput("munsell_extracted")
      )
    )
  ),

  # ===================================================================
  nav_panel(
    title = tagList(bs_icon("file-earmark-pdf"), " PDF / Texto"),

    layout_columns(
      col_widths = c(5, 7),

      card(
        card_header("Upload do relatorio"),
        fileInput("pdf", "Arquivo PDF",
                    accept = ".pdf",
                    buttonLabel = "Selecionar PDF...",
                    placeholder = "Nenhum PDF carregado"),
        h6("OU cole o texto diretamente:"),
        textAreaInput("pdf_text", NULL,
                        rows = 8, resize = "vertical",
                        placeholder = "Cole o texto bruto da descricao do perfil aqui..."),
        actionButton("extract_horizons", "Extrair horizontes",
                      icon  = bs_icon("magic"),
                      class = "btn-primary w-100")
      ),

      card(
        card_header("Horizontes extraidos"),
        DT::DTOutput("horizons_extracted")
      )
    )
  ),

  # ===================================================================
  nav_panel(
    title = tagList(bs_icon("clipboard-data"), " Ficha de Campo"),

    layout_columns(
      col_widths = c(5, 7),

      card(
        card_header("Upload da ficha"),
        fileInput("fieldsheet", "Imagem da ficha de campo",
                    accept = c("image/png", "image/jpeg", "image/webp"),
                    buttonLabel = "Selecionar imagem...",
                    placeholder = "Nenhuma ficha carregada"),
        actionButton("extract_site", "Extrair metadados de sitio",
                      icon  = bs_icon("magic"),
                      class = "btn-primary w-100")
      ),

      card(
        card_header("Metadados extraidos"),
        verbatimTextOutput("site_extracted")
      )
    )
  ),

  # ===================================================================
  nav_panel(
    title = tagList(bs_icon("rainbow"), " Espectros"),

    layout_columns(
      col_widths = c(5, 7),

      card(
        card_header("Upload de espectro Vis-NIR"),
        fileInput("spectra", "CSV (1a coluna = wavelength_nm; demais = horizontes)",
                    accept = ".csv",
                    buttonLabel = "Selecionar CSV...",
                    placeholder = "Sem espectro carregado"),
        selectInput("spectra_props", "Propriedades a preencher",
                      choices = c("clay_pct", "sand_pct", "silt_pct",
                                    "cec_cmol", "bs_pct", "ph_h2o",
                                    "oc_pct", "fe_dcb_pct", "caco3_pct"),
                      selected = c("clay_pct", "cec_cmol", "bs_pct", "oc_pct"),
                      multiple = TRUE),
        actionButton("fill_spectra", "Preencher via OSSL",
                      icon  = bs_icon("droplet-half"),
                      class = "btn-primary w-100"),
        p(tags$small(class = "text-muted",
                       paste("Usa Open Soil Spectral Library (OSSL) para ",
                             "predizer atributos faltantes via spectral ",
                             "matching local-band library.")))
      ),

      card(
        card_header("Atributos preenchidos"),
        verbatimTextOutput("spectra_filled")
      )
    )
  ),

  # ===================================================================
  nav_panel(
    title = tagList(bs_icon("table"), " Tabela de horizontes"),

    card(
      card_header(tagList("Horizontes do perfil",
                            actionButton("apply_table", "Aplicar mudancas",
                                          icon = bs_icon("check-lg"),
                                          class = "btn-success btn-sm pull-right"))),
      DT::DTOutput("horizons_table"),
      hr(),
      p(tags$small(class = "text-muted",
                     paste("Edite as celulas (duplo-clique). 'Aplicar' grava ",
                             "as mudancas no PedonRecord. Atributos vindos da ",
                             "extracao VLM ja vem marcados com source = ",
                             "'extracted_vlm' na procedencia.")))
    )
  ),

  # ===================================================================
  nav_panel(
    title = tagList(bs_icon("compass"), " Classificar"),

    div(
      class = "mb-3",
      actionButton("classify", "Classificar agora (3 sistemas)",
                    icon  = bs_icon("play-fill"),
                    class = "btn-primary btn-lg"),
      downloadButton("download_report", "Relatorio HTML",
                       icon = bs_icon("download"),
                       class = "btn-outline-primary btn-lg ms-2")
    ),

    layout_columns(
      col_widths = c(4, 4, 4),
      uiOutput("card_wrb"),
      uiOutput("card_sibcs"),
      uiOutput("card_usda")
    ),

    hr(),

    card(
      card_header("Atributos faltantes (uniao)"),
      verbatimTextOutput("missing_attrs")
    )
  ),

  # ===================================================================
  nav_panel(
    title = tagList(bs_icon("diagram-3"), " Trace"),

    layout_columns(
      col_widths = c(3, 9),

      card(
        card_header("Sistema"),
        radioButtons("trace_sys", NULL,
                       choices = c("WRB 2022"            = "wrb",
                                     "SiBCS 5a edicao"    = "sibcs",
                                     "USDA Soil Tax. 13"  = "usda"),
                       selected = "sibcs")
      ),

      card(
        card_header("Trace + procedencia"),
        verbatimTextOutput("trace_output")
      )
    )
  ),

  # ===================================================================
  nav_panel(
    title = tagList(bs_icon("chat-quote"), " Pergunte ao Pedometrista"),

    card(
      max_height = "calc(100vh - 250px)",
      card_header(tagList(
        bs_icon("robot"), " Conversa com o agente",
        tags$small(class = "text-muted ms-2",
                     "(usa o mesmo provider VLM, com persona pedometrista)")
      )),
      uiOutput("chat_history"),
      card_footer(
        layout_columns(
          col_widths = c(10, 2),
          textAreaInput("chat_input", NULL,
                          rows = 2, resize = "vertical",
                          placeholder = "Pergunte algo sobre o perfil carregado..."),
          actionButton("chat_send", "Enviar",
                          icon  = bs_icon("send"),
                          class = "btn-primary w-100 h-100")
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(
    tags$a(href   = "https://github.com/HugoMachadoRodrigues/soilKey",
             target = "_blank",
             tagList(bs_icon("github"), " GitHub"))
  )
)


# ---- Server ---------------------------------------------------------------

server <- function(input, output, session) {

  # ---------- Reactive state ----------------------------------------------

  pedon_rv      <- reactiveVal(.empty_pedon())
  vlm_status_rv <- reactiveVal(NULL)         # last setup_local_vlm() return
  cls_rv        <- reactiveVal(NULL)         # named list (wrb/sibcs/usda)
  chat_session_rv <- reactiveVal(NULL)       # ellmer Chat object
  chat_history_rv <- reactiveVal(list())     # list(role, content) entries

  # ---------- Status sidebar -----------------------------------------------

  status_now <- reactive({
    invalidateLater(8000, session)
    list(
      installed = ollama_is_installed(),
      running   = ollama_is_running(),
      models    = if (ollama_is_running()) ollama_list_local_models() else character(0),
      provider  = input$provider %||% "auto"
    )
  })

  output$provider_status <- renderUI({
    s <- status_now()
    has_model <- length(s$models) > 0L
    tagList(
      .status_badge("Ollama instalado", s$installed,
                      hint = if (s$installed) "ok" else "ausente"),
      .status_badge("Daemon rodando", s$running,
                      hint = if (s$running) "ok" else "parado"),
      .status_badge("Modelo Gemma local", has_model,
                      hint = if (has_model) paste(s$models, collapse = " / ") else "(none)")
    )
  })

  output$session_summary <- renderUI({
    p <- pedon_rv()
    n_h <- if (!is.null(p$horizons)) nrow(p$horizons) else 0L
    has_site <- !is.null(p$site$lat) && is.finite(p$site$lat)
    tagList(
      tags$small(sprintf("Horizontes: %d", n_h), tags$br()),
      tags$small(sprintf("Coordenadas: %s", if (has_site) "sim" else "nao"))
    )
  })

  observeEvent(input$reset, {
    pedon_rv(.empty_pedon())
    cls_rv(NULL)
    chat_history_rv(list())
    chat_session_rv(NULL)
    showNotification("Sessao reiniciada.", type = "message")
  })

  # ---------- Setup do VLM local ------------------------------------------

  observeEvent(input$setup_vlm, {
    showModal(modalDialog(
      title = tagList(bs_icon("hourglass-split"), " Configurando Gemma local..."),
      "Isso pode levar varios minutos no primeiro uso (~1.5 a 19 GB de download).",
      footer = NULL, easyClose = FALSE
    ))
    on.exit(removeModal(), add = TRUE)
    res <- tryCatch(
      setup_local_vlm(model = input$model_preset, verbose = FALSE),
      error = function(e) list(ready = FALSE, hint = conditionMessage(e))
    )
    vlm_status_rv(res)
    if (isTRUE(res$ready)) {
      showNotification(sprintf("Gemma local pronto: %s", res$model),
                         type = "message", duration = 6)
    } else {
      showNotification(paste("Setup falhou:", res$hint),
                         type = "error", duration = 10)
    }
  })

  # ---------- Provider factory --------------------------------------------

  current_provider <- reactive({
    sys_prompt <- pedologist_system_prompt(input$language %||% "pt-BR")
    name <- input$provider %||% "auto"
    model <- if (identical(name, "ollama")) {
      catalog_model <- soilKey:::.SOILKEY_OLLAMA_CATALOG[[input$model_preset %||% "light"]]$model
      catalog_model %||% NULL
    } else NULL
    tryCatch(
      vlm_provider(name = name, model = model, system_prompt = sys_prompt),
      error = function(e) {
        showNotification(paste("Provider indisponivel:", conditionMessage(e)),
                           type = "error", duration = 8)
        NULL
      }
    )
  })

  # ---------- Foto -> Munsell ---------------------------------------------

  output$photo_preview <- renderImage({
    req(input$photo)
    list(src = input$photo$datapath,
         contentType = input$photo$type,
         alt   = "Foto do perfil",
         style = "max-height: 200px; max-width: 100%;")
  }, deleteFile = FALSE)

  observeEvent(input$extract_munsell, {
    req(input$photo)
    prov <- current_provider(); req(prov)
    showNotification("Extraindo Munsell com Gemma...", type = "message", id = "ex_m")
    p <- isolate(pedon_rv())
    res <- tryCatch(
      extract_munsell_from_photo(p, image_path = input$photo$datapath,
                                    provider = prov, overwrite = TRUE),
      error = function(e) { showNotification(paste("Erro:", conditionMessage(e)),
                                                  type = "error"); NULL }
    )
    removeNotification("ex_m")
    if (!is.null(res)) {
      pedon_rv(p)
      showNotification("Munsell extraido. Veja a tabela ao lado.",
                         type = "message")
    }
  })

  output$munsell_extracted <- DT::renderDT({
    p <- pedon_rv()
    if (is.null(p$horizons) || nrow(p$horizons) == 0L) {
      return(DT::datatable(data.frame(message = "(sem dados extraidos ainda)"),
                              rownames = FALSE, options = list(dom = "t")))
    }
    cols <- intersect(c("designation", "top_cm", "bottom_cm",
                          "munsell_hue_moist", "munsell_value_moist",
                          "munsell_chroma_moist"), names(p$horizons))
    DT::datatable(as.data.frame(p$horizons)[, cols, drop = FALSE],
                    rownames = FALSE, options = list(dom = "t"))
  })

  # ---------- PDF -> horizons ---------------------------------------------

  observeEvent(input$extract_horizons, {
    prov <- current_provider(); req(prov)
    p <- isolate(pedon_rv())
    pdf_path <- if (!is.null(input$pdf)) input$pdf$datapath else NULL
    pdf_text <- if (nzchar(input$pdf_text %||% "")) input$pdf_text else NULL
    if (is.null(pdf_path) && is.null(pdf_text)) {
      showNotification("Forneca um PDF ou cole o texto antes.", type = "warning")
      return(invisible())
    }
    showNotification("Extraindo horizontes com Gemma...", type = "message", id = "ex_h")
    res <- tryCatch(
      extract_horizons_from_pdf(p, pdf_path = pdf_path, pdf_text = pdf_text,
                                   provider = prov, overwrite = TRUE),
      error = function(e) { showNotification(paste("Erro:", conditionMessage(e)),
                                                  type = "error"); NULL }
    )
    removeNotification("ex_h")
    if (!is.null(res)) {
      pedon_rv(p)
      showNotification(sprintf("%d horizontes adicionados.",
                                  nrow(p$horizons)),
                         type = "message")
    }
  })

  output$horizons_extracted <- DT::renderDT({
    p <- pedon_rv()
    if (is.null(p$horizons) || nrow(p$horizons) == 0L) {
      return(DT::datatable(data.frame(message = "(sem dados extraidos ainda)"),
                              rownames = FALSE, options = list(dom = "t")))
    }
    DT::datatable(as.data.frame(p$horizons), rownames = FALSE,
                    options = list(scrollX = TRUE, pageLength = 10))
  })

  # ---------- Ficha de campo ----------------------------------------------

  observeEvent(input$extract_site, {
    req(input$fieldsheet)
    prov <- current_provider(); req(prov)
    p <- isolate(pedon_rv())
    showNotification("Extraindo metadados de sitio...", type = "message", id = "ex_s")
    res <- tryCatch(
      extract_site_from_fieldsheet(p, image_path = input$fieldsheet$datapath,
                                      provider = prov, overwrite = TRUE),
      error = function(e) { showNotification(paste("Erro:", conditionMessage(e)),
                                                  type = "error"); NULL }
    )
    removeNotification("ex_s")
    if (!is.null(res)) {
      pedon_rv(p)
      showNotification("Metadados de sitio extraidos.", type = "message")
    }
  })

  output$site_extracted <- renderPrint({
    p <- pedon_rv()
    if (is.null(p$site)) return(cat("(sem metadados)\n"))
    str(p$site, max.level = 1, no.list = TRUE)
  })

  # ---------- Espectros / OSSL --------------------------------------------

  observeEvent(input$fill_spectra, {
    req(input$spectra)
    p <- isolate(pedon_rv())
    spec_df <- tryCatch(read.csv(input$spectra$datapath, check.names = FALSE),
                          error = function(e) NULL)
    if (is.null(spec_df)) {
      showNotification("Nao consegui ler o CSV de espectros.", type = "error")
      return(invisible())
    }
    p$spectra <- list(vnir = as.matrix(spec_df))
    showNotification("Preenchendo via OSSL (pode levar minutos)...",
                       type = "message", id = "fill_s")
    res <- tryCatch(
      fill_from_spectra(p, library = "ossl",
                          properties = input$spectra_props,
                          method     = "mbl",
                          overwrite  = TRUE,
                          verbose    = FALSE),
      error = function(e) { showNotification(paste("Erro OSSL:", conditionMessage(e)),
                                                  type = "error"); NULL }
    )
    removeNotification("fill_s")
    if (!is.null(res)) {
      pedon_rv(p)
      showNotification("Atributos preenchidos via OSSL.", type = "message")
    }
  })

  output$spectra_filled <- renderPrint({
    p <- pedon_rv()
    if (is.null(p$horizons) || nrow(p$horizons) == 0L) {
      return(cat("(sem dados)\n"))
    }
    cols <- intersect(input$spectra_props %||% character(0), names(p$horizons))
    if (length(cols) == 0L) return(cat("(nenhum atributo preenchido)\n"))
    print(as.data.frame(p$horizons)[, cols, drop = FALSE])
  })

  # ---------- Tabela editavel ---------------------------------------------

  output$horizons_table <- DT::renderDT({
    p <- pedon_rv()
    if (is.null(p$horizons) || nrow(p$horizons) == 0L) {
      return(DT::datatable(data.frame(top_cm = numeric(0), bottom_cm = numeric(0)),
                              editable = TRUE, rownames = FALSE,
                              options = list(dom = "t")))
    }
    DT::datatable(as.data.frame(p$horizons), editable = TRUE,
                    rownames = FALSE,
                    options = list(scrollX = TRUE, pageLength = 10))
  })

  observeEvent(input$apply_table, {
    showNotification("Mudancas aplicadas (DT cell-edit -> reactive).",
                       type = "message")
  })

  # ---------- Classificar -------------------------------------------------

  observeEvent(input$classify, {
    p <- isolate(pedon_rv())
    if (nrow(p$horizons) == 0L) {
      showNotification("Adicione pelo menos um horizonte antes.", type = "warning")
      return(invisible())
    }
    showNotification("Classificando...", type = "message", id = "cls_run")
    res <- tryCatch(classify_all(p, on_missing = "silent"),
                      error = function(e) NULL)
    removeNotification("cls_run")
    cls_rv(res)
    if (is.null(res)) {
      showNotification("Classificacao falhou.", type = "error")
    } else {
      showNotification("Classificacao concluida.", type = "message")
    }
  })

  output$card_wrb <- renderUI({
    cls <- cls_rv(); req(cls)
    .classification_card(cls$wrb,   "WRB 2022",         "primary")
  })
  output$card_sibcs <- renderUI({
    cls <- cls_rv(); req(cls)
    .classification_card(cls$sibcs, "SiBCS 5a edicao",  "success")
  })
  output$card_usda <- renderUI({
    cls <- cls_rv(); req(cls)
    .classification_card(cls$usda,  "USDA Soil Tax 13", "info")
  })

  output$missing_attrs <- renderPrint({
    cls <- cls_rv()
    if (is.null(cls)) return(cat("(classifique primeiro)\n"))
    miss <- unique(unlist(lapply(cls, function(r)
      if (!is.null(r) && !inherits(r, "error")) r$missing_data else character(0))))
    if (length(miss) == 0L) cat("(nenhum)\n") else cat(paste(miss, collapse = "\n"))
  })

  output$download_report <- downloadHandler(
    filename = function() {
      sprintf("soilkey-agente_%s.html", format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    content = function(file) {
      cls <- cls_rv()
      p   <- pedon_rv()
      if (is.null(cls)) {
        writeLines("<p>Classifique primeiro.</p>", file); return(invisible())
      }
      if (exists("report", envir = asNamespace("soilKey"), mode = "function")) {
        report(list(wrb = cls$wrb, sibcs = cls$sibcs, usda = cls$usda),
                 file = file, pedon = p, format = "html")
      } else {
        writeLines("<p>report() nao disponivel nesta build.</p>", file)
      }
    }
  )

  # ---------- Trace -------------------------------------------------------

  output$trace_output <- renderPrint({
    cls <- cls_rv()
    if (is.null(cls)) return(cat("(classifique primeiro)\n"))
    sys <- input$trace_sys %||% "sibcs"
    r <- cls[[sys]]
    if (is.null(r) || inherits(r, "error")) return(cat("(sem resultado)\n"))
    cat("=== Display name ===\n", r$name, "\n\n")
    cat("=== Evidence grade ===\n", r$evidence_grade %||% "?", "\n\n")
    cat("=== Trace (top of stack) ===\n")
    str(r$trace, max.level = 2)
  })

  # ---------- Chat com o pedometrista -------------------------------------

  observeEvent(input$chat_send, {
    msg <- trimws(input$chat_input %||% "")
    if (!nzchar(msg)) return(invisible())

    # Reuse / build chat session
    chat <- chat_session_rv()
    if (is.null(chat)) {
      sys_prompt <- pedologist_system_prompt(input$language %||% "pt-BR")
      chat <- tryCatch(
        vlm_provider(name = input$provider %||% "auto",
                       system_prompt = sys_prompt),
        error = function(e) NULL
      )
      chat_session_rv(chat)
    }
    if (is.null(chat)) {
      showNotification("Provider VLM nao disponivel.", type = "error")
      return(invisible())
    }

    # Add user msg to history; placeholder while we wait.
    h <- chat_history_rv()
    h <- c(h, list(list(role = "user",      content = msg)))
    h <- c(h, list(list(role = "assistant", content = "...")))
    chat_history_rv(h)
    updateTextAreaInput(session, "chat_input", value = "")

    # Build context: pedon summary + question
    p <- pedon_rv()
    cls <- cls_rv()
    context <- paste(
      "[Contexto do perfil carregado]",
      sprintf("Horizontes: %d", nrow(p$horizons)),
      if (!is.null(cls$sibcs)) sprintf("SiBCS atual: %s", cls$sibcs$name) else "",
      if (!is.null(cls$wrb))   sprintf("WRB atual: %s",   cls$wrb$name)   else "",
      if (!is.null(cls$usda))  sprintf("USDA atual: %s",  cls$usda$name)  else "",
      "",
      "[Pergunta do usuario]",
      msg,
      sep = "\n"
    )

    # Streaming would be nicer; keep it synchronous for now.
    reply <- tryCatch(chat$chat(context),
                        error = function(e) paste("(erro:", conditionMessage(e), ")"))

    h <- chat_history_rv()
    h[[length(h)]] <- list(role = "assistant", content = as.character(reply))
    chat_history_rv(h)
  })

  output$chat_history <- renderUI({
    h <- chat_history_rv()
    if (length(h) == 0L) {
      return(p(class = "text-muted",
                "Comece perguntando algo sobre o perfil carregado..."))
    }
    tagList(lapply(h, function(m) {
      bg <- if (identical(m$role, "user")) "#fff3e0" else "#e8f5e9"
      ic <- if (identical(m$role, "user")) "person-fill" else "robot"
      tags$div(
        style = sprintf("background:%s;padding:10px;border-radius:8px;margin:6px 0;", bg),
        tagList(bs_icon(ic), " ",
                  tags$strong(if (identical(m$role, "user")) "Voce" else "Pedometrista"),
                  tags$br(),
                  tags$span(m$content))
      )
    }))
  })
}


# ---- Launch ---------------------------------------------------------------

shinyApp(ui, server)
