# =============================================================================
# soilKey Pro -- shared UI helpers (v0.9.97).
#
# Auto-sourced by Shiny from the app's R/ sub-directory.
# =============================================================================

# Catalogue of canonical fixtures, grouped for shinyWidgets::pickerInput.
# Display label -> exported make_*_canonical() function name.
pro_fixture_catalog <- function() {
  list(
    "WRB 2022 reference soil groups" = c(
      "Acrisol"                = "make_acrisol_canonical",
      "Alisol"                 = "make_alisol_canonical",
      "Andosol (volcanic ash)" = "make_andosol_canonical",
      "Anthrosol"              = "make_anthrosol_canonical",
      "Arenosol"               = "make_arenosol_canonical",
      "Calcisol"               = "make_calcisol_canonical",
      "Cambisol"               = "make_cambisol_canonical",
      "Chernozem"              = "make_chernozem_canonical",
      "Cryosol (permafrost)"   = "make_cryosol_canonical",
      "Durisol"                = "make_durisol_canonical",
      "Ferralsol"              = "make_ferralsol_canonical",
      "Fluvisol"               = "make_fluvisol_canonical",
      "Gleysol"                = "make_gleysol_canonical",
      "Gypsisol"               = "make_gypsisol_canonical",
      "Histosol"               = "make_histosol_canonical",
      "Kastanozem"             = "make_kastanozem_canonical",
      "Leptosol"               = "make_leptosol_canonical",
      "Lixisol"                = "make_lixisol_canonical",
      "Luvisol"                = "make_luvisol_canonical",
      "Nitisol"                = "make_nitisol_canonical",
      "Phaeozem"               = "make_phaeozem_canonical",
      "Planosol"               = "make_planosol_canonical",
      "Plinthosol"             = "make_plinthosol_canonical",
      "Podzol"                 = "make_podzol_canonical",
      "Retisol"                = "make_retisol_canonical",
      "Solonchak"              = "make_solonchak_canonical",
      "Solonetz"               = "make_solonetz_canonical",
      "Stagnosol"              = "make_stagnosol_canonical",
      "Technosol"              = "make_technosol_canonical",
      "Umbrisol"               = "make_umbrisol_canonical",
      "Vertisol"               = "make_vertisol_canonical"
    ),
    "SiBCS 5 ordens" = c(
      "Argissolo"   = "make_argissolo_canonical",
      "Cambissolo"  = "make_cambissolo_canonical",
      "Chernossolo" = "make_chernossolo_canonical",
      "Espodossolo" = "make_espodossolo_canonical",
      "Gleissolo"   = "make_gleissolo_canonical",
      "Latossolo"   = "make_latossolo_canonical",
      "Luvissolo"   = "make_luvissolo_canonical",
      "Neossolo"    = "make_neossolo_canonical",
      "Nitossolo"   = "make_nitossolo_canonical",
      "Planossolo"  = "make_planossolo_canonical",
      "Plintossolo" = "make_plintossolo_canonical",
      "Organossolo" = "make_organossolo_canonical",
      "Vertissolo"  = "make_vertissolo_canonical"
    )
  )
}

# Resolve a make_*_canonical() name to a PedonRecord. Works whether the
# fixture is exported or internal.
pro_load_fixture <- function(fn_name) {
  fn <- get(fn_name, envir = asNamespace("soilKey"))
  fn()
}

# Numeric horizon columns surfaced in the editor / profile plot.
pro_numeric_attrs <- function() {
  c("clay_pct", "silt_pct", "sand_pct", "ph_h2o", "ph_kcl", "oc_pct",
    "cec_cmol", "ecec_cmol", "bs_pct", "al_sat_pct", "caco3_pct",
    "coarse_fragments_pct")
}

# A small coloured pill for an evidence grade (A best .. E weakest).
pro_grade_badge <- function(grade) {
  grade <- as.character(grade %||% NA)
  pal <- c(A = "#198754", B = "#0d6efd", C = "#fd7e14",
           D = "#dc3545", E = "#6c757d")
  col <- if (!is.na(grade) && grade %in% names(pal)) pal[[grade]] else "#6c757d"
  lab <- if (is.na(grade)) i18n("ui.na") else grade
  shiny::tags$span(
    class = "badge",
    style = sprintf("background-color:%s;font-size:0.85rem;", col),
    paste(i18n("ui.evidence"), lab)
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# A bslib card summarising one ClassificationResult.
pro_result_card <- function(res, system_label) {
  if (is.null(res) || inherits(res, "error")) {
    msg <- if (inherits(res, "error")) conditionMessage(res) else i18n("ui.not_run")
    return(bslib::card(
      bslib::card_header(system_label),
      bslib::card_body(shiny::tags$em(class = "text-danger",
                                      paste(i18n("ui.no_result"), msg)))
    ))
  }
  princ <- res$qualifiers$principal %||% character(0)
  supp  <- res$qualifiers$supplementary %||% character(0)
  bslib::card(
    bslib::card_header(
      shiny::div(
        class = "d-flex justify-content-between align-items-center",
        shiny::strong(system_label),
        pro_grade_badge(res$evidence_grade)
      )
    ),
    bslib::card_body(
      shiny::tags$h5(res$name %||% i18n("ui.unnamed")),
      shiny::tags$dl(
        class = "row mb-0 small",
        shiny::tags$dt(class = "col-5", i18n("ui.rsg_order")),
        shiny::tags$dd(class = "col-7", res$rsg_or_order %||% i18n("ui.na")),
        if (length(princ)) shiny::tagList(
          shiny::tags$dt(class = "col-5", i18n("ui.principal_qualifiers")),
          shiny::tags$dd(class = "col-7", paste(princ, collapse = ", "))
        ),
        if (length(supp)) shiny::tagList(
          shiny::tags$dt(class = "col-5", i18n("ui.supplementary")),
          shiny::tags$dd(class = "col-7", paste(supp, collapse = ", "))
        )
      )
    )
  )
}

# A depth-profile plot: selected numeric attribute (X) against depth (Y,
# reversed). Each horizon contributes one marker at its mid-depth; horizons
# are joined top-to-bottom. Returns a plotly htmlwidget.
pro_profile_plot <- function(hz_df, attribute) {
  if (is.null(hz_df) || nrow(hz_df) == 0L) {
    return(plotly::plotly_empty(type = "scatter", mode = "markers") |>
             plotly::layout(title = list(text = i18n("ui.add_one_horizon"),
                                         font = list(size = 13))))
  }
  needed <- c("top_cm", "bottom_cm")
  if (!all(needed %in% names(hz_df))) {
    return(plotly::plotly_empty(type = "scatter", mode = "markers") |>
             plotly::layout(title = list(text = i18n("ui.horizons_need_topbot"),
                                         font = list(size = 13))))
  }
  top <- suppressWarnings(as.numeric(hz_df$top_cm))
  bot <- suppressWarnings(as.numeric(hz_df$bottom_cm))
  mid <- (top + bot) / 2
  desig <- if ("designation" %in% names(hz_df)) as.character(hz_df$designation)
           else paste0("H", seq_len(nrow(hz_df)))

  if (!attribute %in% names(hz_df)) {
    return(plotly::plotly_empty(type = "scatter", mode = "markers") |>
             plotly::layout(title = list(
               text = i18n("ui.column_not_present", attribute),
               font = list(size = 13))))
  }
  val <- suppressWarnings(as.numeric(hz_df[[attribute]]))

  plotly::plot_ly(
    x = val, y = mid, type = "scatter", mode = "lines+markers",
    text = sprintf("%s: %s = %s (%g-%g cm)",
                   desig, attribute, ifelse(is.na(val), "NA", val),
                   top, bot),
    hoverinfo = "text",
    marker = list(size = 10, color = "#2c7fb8"),
    line   = list(color = "#2c7fb8")
  ) |>
    plotly::layout(
      xaxis = list(title = attribute),
      yaxis = list(title = i18n("ui.depth_cm"), autorange = "reversed", zeroline = FALSE),
      margin = list(l = 60, r = 20, t = 30, b = 50)
    )
}

# Plot an attached Vis-NIR matrix: one reflectance trace per horizon, X =
# wavelength (nm, parsed from the column names), Y = reflectance. The matrix is
# horizons x wavelengths (the shape fill_from_spectra() expects). Returns a
# plotly htmlwidget, with graceful placeholders when nothing is attached.
# Round every numeric column of a data.frame to `digits` (default 2) for
# display -- so shown values never carry more than 2 decimal places.
.sk_round2 <- function(df, digits = 2L) {
  df <- as.data.frame(df)
  if (!ncol(df)) return(df)
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], function(x) round(x, digits))
  df
}

pro_spectrum_plot <- function(mat, designations = NULL, y_label = NULL) {
  if (is.null(mat) || !is.matrix(mat) || nrow(mat) == 0L || ncol(mat) == 0L) {
    return(plotly::plotly_empty(type = "scatter", mode = "lines") |>
             plotly::layout(title = list(
               text = i18n("ui.attach_spectrum"),
               font = list(size = 13))))
  }
  # Column names are wavelengths; fall back to a 1..ncol index if unparseable.
  wl <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", colnames(mat))))
  if (length(wl) != ncol(mat) || all(is.na(wl))) wl <- seq_len(ncol(mat))
  labs <- if (!is.null(designations) && length(designations) == nrow(mat))
    as.character(designations) else paste0(i18n("ui.horizon_prefix"), seq_len(nrow(mat)))

  p <- plotly::plot_ly()
  for (i in seq_len(nrow(mat))) {
    p <- plotly::add_trace(
      p, x = wl, y = as.numeric(mat[i, ]), type = "scatter", mode = "lines",
      name = labs[i], hoverinfo = "name+x+y")
  }
  plotly::layout(
    p,
    xaxis  = list(title = i18n("ui.wavelength_nm")),
    yaxis  = list(title = y_label %||% i18n("ui.reflectance")),
    legend = list(orientation = "h", y = -0.2),
    margin = list(l = 60, r = 20, t = 20, b = 60)
  )
}

# Standardised "no pedon yet" placeholder used across tabs.
pro_no_pedon_msg <- function() {
  shiny::div(
    class = "text-muted p-4 text-center",
    shiny::icon("circle-info"), " ",
    i18n("ui.build_pedon_on"), shiny::strong(i18n("ui.pedon_tab")), i18n("ui.tab_first")
  )
}


# =============================================================================
# v0.9.162 design-system helpers -- consistent sections, labels + tooltips.
# All three are used across every module so the app reads as one product.
# =============================================================================

# A small info "i" that reveals `text` on hover / keyboard focus. Place it after
# a control label so every input carries a one-line, plain-English explanation.
sk_tip <- function(text, placement = "top") {
  bslib::tooltip(
    shiny::icon("circle-info", class = "sk-tip"),
    text, placement = placement
  )
}

# A control label with an inline help tooltip (returns a tagList for `label=`).
sk_label <- function(label, help = NULL) {
  if (is.null(help) || !nzchar(help)) return(label)
  shiny::tagList(label, " ", sk_tip(help))
}

# A titled panel: icon + title, an optional one-line description, then content.
# Gives every sidebar block a clear, professional header.
sk_section <- function(title, ..., desc = NULL, icon = NULL) {
  shiny::div(
    class = "sk-section",
    shiny::div(
      class = "sk-section-head",
      if (!is.null(icon)) shiny::icon(icon, class = "sk-section-ic"),
      shiny::span(class = "sk-section-title", title)
    ),
    if (!is.null(desc)) shiny::p(class = "sk-section-desc", desc),
    ...
  )
}

# A centred empty-state / hero panel (icon, heading, explanation, extra tags).
sk_empty <- function(icon, title, body = NULL, ...) {
  bslib::card(
    class = "sk-empty-state",
    bslib::card_body(shiny::div(
      class = "text-center",
      shiny::icon(icon, class = "fa-2x text-secondary mb-2"),
      shiny::tags$h5(class = "mb-2", title),
      if (!is.null(body))
        shiny::p(class = "text-body-secondary mx-auto",
                 style = "max-width: 54ch;", body),
      ...
    ))
  )
}

# Resolve a bundled www/ asset (demo photo, demo spectrum) to an on-disk path,
# working both when the app runs from its installed location (cwd = app dir) and
# from source. Returns NULL if the asset cannot be found.
.pro_demo_asset <- function(name) {
  cands <- c(
    file.path("www", name),                                       # cwd = app dir
    system.file("shiny", "classify_app_pro", "www", name,
                package = "soilKey"))                              # installed
  for (p in cands) if (nzchar(p) && file.exists(p))
    return(normalizePath(p, mustWork = FALSE))
  NULL
}

# Build an n-row Vis-NIR demo-spectrum matrix by recycling the bundled 5-row
# demo, so it ALWAYS matches the current pedon's horizon count (a pedon can have
# any number of horizons -- e.g. a photo extraction may add some). Returns NULL
# if the bundled demo cannot be read.
.pro_demo_spectrum <- function(n_horizons) {
  path <- .pro_demo_asset("demo_spectrum.csv")
  if (is.null(path)) return(NULL)
  m <- tryCatch({
    mm <- as.matrix(utils::read.csv(path, check.names = FALSE))
    storage.mode(mm) <- "double"
    mm
  }, error = function(e) NULL)
  if (is.null(m) || nrow(m) == 0L) return(NULL)
  n   <- max(1L, as.integer(n_horizons))
  idx <- ((seq_len(n) - 1L) %% nrow(m)) + 1L      # recycle rows to length n
  m[idx, , drop = FALSE]
}
