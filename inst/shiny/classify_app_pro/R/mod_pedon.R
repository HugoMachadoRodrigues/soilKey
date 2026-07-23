# =============================================================================
# soilKey Pro -- Pedon builder module (v0.9.97).
#
# Three ways to seed a profile: a canonical fixture, a CSV upload, or a blank
# template. The horizon table is editable cell-by-cell (DT). "Build pedon"
# assembles a PedonRecord from the edited table plus the site metadata and
# stores it in the shared rv$pedon.
# =============================================================================

# Blank horizon template -- one empty row spanning the attributes a field
# description usually records: depth, texture, Munsell colour (moist + dry),
# the main chemistry, the exchange complex, bulk density, structure and the
# lower boundary. Built from horizon_column_spec() so every column is valid and
# correctly typed; the table scrolls horizontally, so entry no longer stops at
# bs_pct. (Loading a fixture still exposes the full ~110-column schema.)
.pedon_common_cols <- function() {
  c("top_cm", "bottom_cm", "designation",
    "clay_pct", "silt_pct", "sand_pct", "coarse_fragments_pct",
    "munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist",
    "munsell_hue_dry", "munsell_value_dry", "munsell_chroma_dry",
    "ph_h2o", "ph_kcl", "oc_pct",
    "cec_cmol", "bs_pct", "al_sat_pct",
    "ca_cmol", "mg_cmol", "k_cmol", "na_cmol",
    "bulk_density_g_cm3", "structure_grade", "boundary_distinctness",
    # v0.9.190: the anthropogenic attributes the Technosol key needs. Without
    # these in the starter table a user building a profile from scratch could
    # never specify a Technosol -- the criteria were only reachable by loading a
    # fixture, which exposes the full schema (David Rossiter, pers. comm.).
    # `geomembrane_present` is asked once at site level instead (see the
    # Construction section) and written onto the horizons at build time.
    "artefacts_pct", "technic_hardmaterial_pct")
}

.pedon_blank_template <- function() {
  spec <- soilKey::horizon_column_spec()
  cols <- intersect(.pedon_common_cols(), names(spec))   # valid columns only
  row  <- lapply(cols, function(cn)
    if (identical(spec[[cn]], "character")) NA_character_ else NA_real_)
  names(row) <- cols
  df <- as.data.frame(row, stringsAsFactors = FALSE)
  # Seed the first horizon so the editor is immediately usable.
  df$top_cm <- 0; df$bottom_cm <- 20; df$designation <- "A"
  df
}

.pedon_starter_csv <- paste(
  "top_cm,bottom_cm,designation,clay_pct,silt_pct,sand_pct,ph_h2o,oc_pct,cec_cmol,bs_pct",
  "0,15,A,50,15,35,4.8,2.0,8.0,24",
  "15,35,AB,52,14,34,4.7,1.2,6.5,17",
  "35,65,BA,55,10,35,4.7,0.6,5.5,14",
  "65,130,Bw1,60,8,32,4.8,0.3,5.0,13",
  "130,200,Bw2,60,8,32,4.9,0.2,4.8,13",
  sep = "\n"
)

# ---------------------------------------------------------------------------
# Robust yes/no parsing for logical columns of the editable table (v0.9.190).
#
# DT::editData() coerces an edited cell to the column's storage type, so a
# logical column receives as.logical(<typed text>). as.logical("no") is NA --
# not FALSE -- so a user answering "no" to a yes/no attribute silently produced
# a missing value and the key went on reporting "data needed" for that
# criterion no matter how many times they answered it. We accept the ordinary
# spellings in both app languages and return NULL when the text is genuinely
# uninterpretable, so the caller can flag it instead of swallowing it.
.sk_parse_logical <- function(x) {
  if (is.logical(x)) return(x)
  s <- tolower(trimws(as.character(x)))
  if (!nzchar(s) || s %in% c("na", "null", "-", "?")) return(NA)
  if (s %in% c("true", "t", "yes", "y", "1", "sim", "s", "verdadeiro", "v"))
    return(TRUE)
  if (s %in% c("false", "f", "no", "n", "0", "nao", "não", "falso"))
    return(FALSE)
  NULL                                  # uninterpretable -> caller warns
}

# Write the site-level technic (anthropogenic) answers onto the horizons (v0.9.190).
#
# WRB 2022 keys Technosols on three alternatives -- >=20 % artefacts by volume
# in the upper 100 cm, a constructed geomembrane starting within 100 cm, or
# technic hard material at the surface -- and the engine reads all three from
# horizon columns. Two things made that unusable from the app:
#
#   * a geomembrane is a fact about how the site was built, not about each
#     layer, so answering it layer-by-layer was tedious and error-prone; and
#   * artefacts and technic hard material are *exceptional* features. A field
#     description that does not mention them means they are absent, but leaving
#     the cells empty made every ordinary profile report the Technosol criteria
#     as "data needed" forever (David Rossiter, pers. comm.).
#
# The app therefore asks once, at site level, and materialises the answer:
#   technic = "none"    -> artefacts 0 %, hard material 0 %, geomembrane FALSE.
#                          Technosols are ruled out cleanly and the key stops
#                          asking, which is what a normal field description means.
#   technic = "present" -> the geomembrane answer below is applied and artefacts
#                          / hard material are taken from the horizon table.
#   technic = "unknown" -> nothing is written. NA is the honest state and the key
#                          correctly reports the criteria as still needing data.
# An explicit answer overrides whatever a loaded fixture carried, because the
# user has just answered the question directly.
.sk_apply_site_technic <- function(df, technic = "unknown",
                                   geomembrane = "unknown",
                                   depth_cm = NA_real_) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  technic <- tolower(trimws(technic %||% "unknown"))

  if (identical(technic, "none")) {
    df$artefacts_pct            <- 0
    df$technic_hardmaterial_pct <- 0
    df$geomembrane_present      <- FALSE
    return(df)
  }
  if (!identical(technic, "present")) return(df)   # "unknown" -> untouched

  geomembrane <- tolower(trimws(geomembrane %||% "unknown"))
  if (identical(geomembrane, "unknown")) return(df)
  df$geomembrane_present <- FALSE
  if (identical(geomembrane, "yes")) {
    d   <- suppressWarnings(as.numeric(depth_cm))
    top <- suppressWarnings(as.numeric(df$top_cm))
    bot <- suppressWarnings(as.numeric(df$bottom_cm))
    hit <- if (!is.na(d))
             which(!is.na(top) & !is.na(bot) & top <= d & bot > d)
           else integer(0)
    if (!length(hit)) hit <- 1L        # no/non-matching depth -> topmost horizon
    df$geomembrane_present[hit[1L]] <- TRUE
  }
  df
}

# ---------------------------------------------------------------------------
# WoSIS profile picker (v0.9.190).
#
# ISRIC's WoSIS is the obvious source of real, already-harmonised profiles, and
# its structure maps directly onto the horizon table plus site metadata the app
# needs (David Rossiter, pers. comm.). We browse the stratified WoSIS sample the
# package already ships, so the picker works offline once the sample is cached
# and needs no credentials. Both the pedon list and the summary table are cached
# for the session, because the first call may fetch the sample.
.sk_wosis_cache <- new.env(parent = emptyenv())

.sk_wosis_pedons <- function() {
  if (is.null(.sk_wosis_cache$pedons)) {
    p <- tryCatch(soilKey::load_wosis_stratified_sample()$pedons,
                  error = function(e) NULL)
    .sk_wosis_cache$pedons <- if (is.null(p)) list() else p
  }
  .sk_wosis_cache$pedons
}

# One row per WoSIS profile, in the same order as .sk_wosis_pedons(), so the
# selected table row indexes the pedon directly.
.sk_wosis_catalogue <- function() {
  if (is.null(.sk_wosis_cache$cat)) {
    ps <- .sk_wosis_pedons()
    .sk_wosis_cache$cat <-
      if (!length(ps)) data.frame()
      else do.call(rbind, lapply(seq_along(ps), function(i) {
        s <- ps[[i]]$site
        num <- function(v) {
          v <- suppressWarnings(as.numeric(v %||% NA)); if (length(v)) round(v[1], 3) else NA_real_
        }
        data.frame(
          Profile  = as.character(s$id %||% NA_character_)[1],
          Country  = as.character(s$country %||% NA_character_)[1],
          WRB      = as.character(s$reference_wrb %||% s$wosis_rsg %||% NA_character_)[1],
          Lat      = num(s$lat),
          Lon      = num(s$lon),
          Horizons = nrow(ps[[i]]$horizons),
          stringsAsFactors = FALSE)
      }))
  }
  .sk_wosis_cache$cat
}

# Repopulate the site inputs from a pedon's own `$site` (v0.9.190).
#
# Loading a fixture used to bring in its horizons and silently drop its site
# metadata, so the Site panel kept showing the previous profile's coordinates
# (David Rossiter, pers. comm.). Only fields the source actually carries are
# written -- nothing is invented.
.sk_fill_site_inputs <- function(session, site) {
  if (is.null(site)) return(invisible(NULL))
  num_ok <- function(v) !is.null(v) && length(v) && !all(is.na(v))
  if (!is.null(site$id))
    shiny::updateTextInput(session, "site_id", value = as.character(site$id)[1])
  if (num_ok(site$lat))
    shiny::updateNumericInput(session, "lat",
                              value = suppressWarnings(as.numeric(site$lat)[1]))
  if (num_ok(site$lon))
    shiny::updateNumericInput(session, "lon",
                              value = suppressWarnings(as.numeric(site$lon)[1]))
  if (!is.null(site$country))
    shiny::updateTextInput(session, "country",
                           value = as.character(site$country)[1])
  if (!is.null(site$parent_material))
    shiny::updateTextInput(session, "pm",
                           value = as.character(site$parent_material)[1])
  invisible(NULL)
}

pedon_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      sk_section(
        i18n("pedon.seed_profile"),
        icon = "layer-group",
        desc = "Start from a reference profile, your own CSV, or a blank sheet.",
        shinyWidgets::radioGroupButtons(
          ns("source"), NULL,
          choices = stats::setNames(
            c("fixture", "wosis", "upload", "blank"),
            c(i18n("pedon.source_fixture"), i18n("pedon.source_wosis"),
              i18n("pedon.source_upload"), i18n("pedon.source_blank"))),
          selected = "fixture", justified = TRUE, size = "sm"
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'fixture'", ns("source")),
          shinyWidgets::pickerInput(
            ns("fixture"),
            sk_label(i18n("pedon.canonical_profile"),
                     "A curated, textbook profile you can load and edit as a starting point."),
            choices  = pro_fixture_catalog(),
            selected = "make_ferralsol_canonical",
            options  = list(`live-search` = TRUE)
          )
        ),
        # v0.9.190: browse real ISRIC WoSIS profiles and load one directly.
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'wosis'", ns("source")),
          shiny::helpText(i18n("pedon.wosis_hint")),
          DT::DTOutput(ns("wosis_table"))
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'upload'", ns("source")),
          shiny::fileInput(ns("csv"),
                           sk_label(i18n("pedon.horizons_csv_tsv"),
                                    "One row per horizon, with depth and lab columns. Download the starter file to see the expected layout."),
                           accept = c(".csv", ".tsv", ".txt")),
          shiny::downloadLink(ns("template"), i18n("pedon.download_starter_csv"))
        ),
        bslib::tooltip(
          shiny::actionButton(ns("load"), i18n("pedon.load_horizons"),
                              icon = shiny::icon("upload"),
                              class = "btn-secondary w-100"),
          "Load the chosen source into the editable horizon table below."
        )
      ),
      sk_section(
        i18n("pedon.site_metadata"),
        icon = "location-dot",
        desc = "Where the profile sits and what it formed on — used for regional priors.",
        shiny::textInput(ns("site_id"),
                         sk_label(i18n("pedon.profile_id"),
                                  "A short label for this profile. It names your downloads and appears in the results."),
                         "demo-pedon-01"),
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(
            ns("lat"),
            sk_label(i18n("pedon.latitude"),
                     "Decimal degrees, from -90 to 90. Optional, but enables the SoilGrids and climate priors."),
            -22.5, step = 0.01)),
          shiny::column(6, shiny::numericInput(
            ns("lon"),
            sk_label(i18n("pedon.longitude"),
                     "Decimal degrees, from -180 to 180. Negative is west. Optional but recommended."),
            -43.7, step = 0.01))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::textInput(
            ns("country"),
            sk_label(i18n("pedon.country_iso2"),
                     "Two-letter ISO country code, e.g. BR for Brazil. Helps regional defaults."),
            "BR")),
          shiny::column(6, shiny::textInput(
            ns("pm"),
            sk_label(i18n("pedon.parent_material"),
                     "The rock or deposit the soil formed on, e.g. gneiss, basalt, alluvium."),
            "gneiss"))
        ),
        # v0.9.190: site-level technic (anthropogenic) attributes. These are
        # facts about how the site was built, not per-layer lab values, so they
        # are asked once here and written onto the horizons at build time.
        shiny::selectInput(
          ns("technic"),
          sk_label(i18n("pedon.technic"),
                   paste("Artefacts, a constructed geomembrane or technic hard material",
                         "key a profile to Technosols (WRB 2022). Choose 'None' for an",
                         "ordinary soil so the key stops asking; 'Unknown' leaves the",
                         "criteria open.")),
          choices  = stats::setNames(
            c("unknown", "none", "present"),
            c(i18n("pedon.technic_unknown"), i18n("pedon.technic_none"),
              i18n("pedon.technic_present"))),
          selected = "unknown"),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'present'", ns("technic")),
          shiny::fluidRow(
            shiny::column(6, shiny::selectInput(
              ns("geomembrane"),
              sk_label(i18n("pedon.geomembrane"),
                       "A constructed geomembrane starting within 100 cm keys the profile to Technosols."),
              choices  = stats::setNames(
                c("unknown", "no", "yes"),
                c(i18n("pedon.geo_unknown"), i18n("pedon.geo_no"), i18n("pedon.geo_yes"))),
              selected = "unknown")),
            shiny::column(6, shiny::numericInput(
              ns("geomembrane_depth"),
              sk_label(i18n("pedon.geomembrane_depth"),
                       "Depth in cm at which the geomembrane starts. Used only when the answer is Yes."),
              value = NA, min = 0, step = 1))
          ),
          shiny::helpText(i18n("pedon.technic_hint"))
        )
      ),
      sk_section(
        i18n("pedon.build_update_pedon"),
        icon = "hammer",
        desc = "Assemble the profile so the other tabs can classify it.",
        bslib::tooltip(
          shiny::actionButton(ns("build"), i18n("pedon.build_update_pedon"),
                              icon = shiny::icon("hammer"),
                              class = "btn-primary w-100"),
          "Check the horizon geometry and coordinates, then build the pedon used by every other tab."
        ),
        shiny::uiOutput(ns("status"))
      ),
      # ---- save / reopen the whole working session as one JSON file --------
      sk_section(
        i18n("pedon.save_open_session"),
        icon = "floppy-disk",
        desc = paste("Export the profile (site + horizons) to a JSON file, or",
                     "reopen one to pick up exactly where you left off."),
        bslib::tooltip(
          shiny::downloadButton(
            ns("save_session"), i18n("pedon.save_session"),
            icon = shiny::icon("download"),
            class = "btn-outline-secondary w-100"),
          "Save the current site details and horizon table as a portable .json file."),
        shiny::fileInput(
          ns("session_file"),
          sk_label(i18n("pedon.open_session"),
                   "Load a .json session saved earlier; it repopulates every field and rebuilds the pedon."),
          accept = ".json")
      )
    ),
    bslib::layout_column_wrap(
      width = 1,
      heights_equal = "row",
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
                     shiny::strong(i18n("pedon.horizons_click_edit")),
                     shiny::div(
                       class = "d-flex gap-2",
                       bslib::tooltip(
                         shiny::downloadButton(ns("download_hz"), i18n("pedon.csv"),
                                               icon = shiny::icon("download"),
                                               class = "btn-sm btn-outline-secondary"),
                         "Save the current table as a CSV to edit offline or re-upload later."),
                       bslib::tooltip(
                         shiny::actionButton(ns("add_row"), i18n("pedon.add_row"),
                                             icon = shiny::icon("plus"),
                                             class = "btn-sm btn-outline-secondary"),
                         "Append a new horizon below, continuing from the deepest depth.")))
        ),
        bslib::card_body(
          shiny::helpText("Click any cell to edit it. Depths are in centimetres; leave a lab value blank if unmeasured."),
          DT::DTOutput(ns("hz_table")),
          shiny::uiOutput(ns("geom_status")))
      ),
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
                     shiny::strong(i18n("pedon.depth_profile")),
                     bslib::tooltip(
                       shiny::selectInput(ns("plot_attr"), NULL,
                                          choices = pro_numeric_attrs(),
                                          selected = "clay_pct", width = "180px"),
                       "Choose which lab attribute to plot against depth."))
        ),
        bslib::card_body(plotly::plotlyOutput(ns("profile"), height = "320px"))
      )
    )
  )
}

# Turn validate_horizon_geometry() details into localized lines for the Pedon
# builder (the package function returns English; the app renders pt/en).
.pedon_geom_lines <- function(geom) {
  d <- geom$details; err <- character(0); warn <- character(0)
  jn <- function(x) paste(x, collapse = ", ")
  if (!is.null(d$missing_depth))  err  <- c(err,  i18n("pedon.geom_missing_depth", jn(d$missing_depth)))
  if (!is.null(d$negative_depth)) err  <- c(err,  i18n("pedon.geom_negative",      jn(d$negative_depth)))
  if (!is.null(d$inverted))       err  <- c(err,  i18n("pedon.geom_inverted",      jn(d$inverted)))
  if (!is.null(d$overlap))        err  <- c(err,  i18n("pedon.geom_overlap",       jn(d$overlap)))
  if (!is.null(d$gap))            warn <- c(warn, i18n("pedon.geom_gap",           jn(d$gap)))
  if (!is.null(d$surface_gap))    warn <- c(warn, i18n("pedon.geom_surface_gap",   d$surface_gap))
  if (isTRUE(d$non_monotonic))    warn <- c(warn, i18n("pedon.geom_non_monotonic"))
  if (!is.null(d$duplicate_designation))
    warn <- c(warn, i18n("pedon.geom_duplicate", jn(d$duplicate_designation)))
  # structural errors (no columns / empty) carry no details -> fall back
  if (length(err) == 0L && length(geom$errors) > 0L) err <- geom$errors
  list(errors = err, warnings = warn)
}

pedon_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    hz        <- shiny::reactiveVal(NULL)  # editable horizon data.frame
    hz_reload <- shiny::reactiveVal(0L)    # bumps only on load/add -> re-render

    output$template <- shiny::downloadHandler(
      filename = function() "soilKey_horizons_template.csv",
      content  = function(file) writeLines(.pedon_starter_csv, file)
    )

    # Download the current horizon table as CSV (edit in a spreadsheet, archive,
    # or re-upload later).
    output$download_hz <- shiny::downloadHandler(
      filename = function() sprintf("soilKey_horizons_%s.csv",
                                    input$site_id %||% "pedon"),
      content  = function(file) {
        cur <- hz()
        shiny::validate(shiny::need(!is.null(cur) && nrow(cur) > 0L,
                                    i18n("pedon.no_horizons_download")))
        utils::write.csv(cur, file, row.names = FALSE)
      }
    )

    # ---- save / reopen the whole session as JSON --------------------------
    # The file uses the canonical {site, horizons:[...]} shape so it is also
    # accepted by soilKey::validate_pedon_json(). Saving captures the live
    # editor state (site inputs + edited horizon table), so unsaved tweaks are
    # preserved; reopening repopulates every field and rebuilds rv$pedon.
    output$save_session <- shiny::downloadHandler(
      filename = function()
        sprintf("soilKey_session_%s.json", input$site_id %||% "pedon"),
      content = function(file) {
        if (!requireNamespace("jsonlite", quietly = TRUE))
          stop("Package 'jsonlite' is required to save a session.")
        df <- hz()
        shiny::validate(shiny::need(!is.null(df) && nrow(df) > 0L,
                                    i18n("pedon.no_horizons_download")))
        horizon_rows <- lapply(seq_len(nrow(df)),
                               function(i) as.list(df[i, , drop = FALSE]))
        payload <- list(
          site = list(
            id              = input$site_id %||% "pedon",
            lat             = suppressWarnings(as.numeric(input$lat)),
            lon             = suppressWarnings(as.numeric(input$lon)),
            country         = input$country %||% NA_character_,
            parent_material = input$pm %||% NA_character_,
            # v0.9.190: site-level construction answers, so a reopened session
            # restores them too. The pedon schema does not restrict additional
            # site properties, so this stays valid for validate_pedon_json().
            technic              = input$technic %||% "unknown",
            geomembrane          = input$geomembrane %||% "unknown",
            geomembrane_depth_cm = suppressWarnings(as.numeric(input$geomembrane_depth))
          ),
          horizons = horizon_rows
        )
        jsonlite::write_json(payload, file, pretty = TRUE, auto_unbox = TRUE,
                             null = "null", na = "null", digits = NA)
      }
    )

    shiny::observeEvent(input$session_file, {
      f <- input$session_file
      if (is.null(f)) return(invisible())
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        shiny::showNotification(
          i18n("pedon.session_load_failed", "package 'jsonlite' not available"),
          type = "error", duration = 8)
        return(invisible())
      }
      parsed <- tryCatch(
        jsonlite::fromJSON(f$datapath, simplifyVector = TRUE,
                           simplifyDataFrame = TRUE),
        error = function(e) e)
      if (inherits(parsed, "error") || is.null(parsed$horizons) ||
          NROW(parsed$horizons) == 0L) {
        msg <- if (inherits(parsed, "error")) conditionMessage(parsed)
               else "no horizons found in file"
        shiny::showNotification(i18n("pedon.session_load_failed", msg),
                                type = "error", duration = 8)
        return(invisible())
      }
      hzdf <- as.data.frame(parsed$horizons, stringsAsFactors = FALSE)
      # Keep only columns soilKey understands, in canonical order.
      spec  <- names(soilKey::horizon_column_spec())
      keep  <- intersect(spec, names(hzdf))
      extra <- setdiff(names(hzdf), spec)
      hzdf  <- hzdf[, c(keep, extra), drop = FALSE]

      site <- parsed$site %||% list()
      shiny::updateTextInput(session, "site_id", value = site$id %||% "pedon")
      if (!is.null(site$lat))
        shiny::updateNumericInput(session, "lat",
                                  value = suppressWarnings(as.numeric(site$lat)))
      if (!is.null(site$lon))
        shiny::updateNumericInput(session, "lon",
                                  value = suppressWarnings(as.numeric(site$lon)))
      shiny::updateTextInput(session, "country", value = site$country %||% "")
      shiny::updateTextInput(session, "pm",
                             value = site$parent_material %||% "")
      # v0.9.190: restore the site-level construction answers (absent in
      # sessions saved by earlier versions -> fall back to "unknown").
      shiny::updateSelectInput(session, "technic",
                               selected = site$technic %||% "unknown")
      shiny::updateSelectInput(session, "geomembrane",
                               selected = site$geomembrane %||% "unknown")
      if (!is.null(site$geomembrane_depth_cm))
        shiny::updateNumericInput(
          session, "geomembrane_depth",
          value = suppressWarnings(as.numeric(site$geomembrane_depth_cm)))
      hz(hzdf)
      hz_reload(hz_reload() + 1L)

      # Try to rebuild the pedon immediately so every tab is usable on reopen;
      # if geometry is off, leave the editor populated and ask for a Build.
      built <- tryCatch({
        # Apply the restored site-level construction answers, exactly as the
        # Build button does, so a reopened session classifies identically.
        hz_built <- .sk_apply_site_technic(
          hzdf, site$technic %||% "unknown",
          site$geomembrane %||% "unknown", site$geomembrane_depth_cm)
        h_dt <- soilKey::ensure_horizon_schema(data.table::as.data.table(hz_built))
        soilKey::PedonRecord$new(
          site = list(
            id              = site$id %||% "pedon",
            lat             = site$lat,
            lon             = site$lon,
            country         = site$country,
            parent_material = site$parent_material),
          horizons = h_dt)
      }, error = function(e) NULL)
      if (!is.null(built)) {
        rv$pedon <- built
        shiny::showNotification(
          i18n("pedon.session_loaded_built", site$id %||% "pedon", nrow(hzdf)),
          type = "message", duration = 6)
      } else {
        shiny::showNotification(
          i18n("pedon.session_loaded", nrow(hzdf)),
          type = "message", duration = 6)
      }
    })

    # ---- one-click example profile (bumped by the Help modal / ribbon) -----
    # The canonical Ferralsol fixture is a complete PedonRecord; loading it
    # populates the editor AND builds rv$pedon, so every tab is immediately
    # usable -- the app's on-ramp for first-time users.
    shiny::observeEvent(rv$example_request, {
      p <- tryCatch(pro_load_fixture("make_ferralsol_canonical"),
                    error = function(e) NULL)
      if (is.null(p)) return(invisible())
      hz(as.data.frame(p$horizons))
      hz_reload(hz_reload() + 1L)
      shiny::updateTextInput(session, "site_id", value = p$site$id %||% "ferralsol-demo")
      shiny::updateNumericInput(session, "lat", value = p$site$lat %||% -22.5)
      shiny::updateNumericInput(session, "lon", value = p$site$lon %||% -43.7)
      rv$pedon <- p
      shiny::showNotification(
        i18n("pedon.loaded_example_ferralsol"),
        type = "message", duration = 6)
    }, ignoreInit = TRUE)

    # ---- load horizons from the chosen source -----------------------------
    shiny::observeEvent(input$load, {
      # v0.9.190: carry the source profile's own site metadata across, instead
      # of loading its horizons and leaving the Site panel showing the previous
      # profile's coordinates (David Rossiter, pers. comm.).
      loaded_site <- NULL
      df <- switch(
        input$source,
        fixture = {
          p <- tryCatch(pro_load_fixture(input$fixture),
                        error = function(e) NULL)
          if (is.null(p)) {
            shiny::showNotification(i18n("pedon.could_not_load_fixture"),
                                    type = "error")
            return(invisible())
          }
          loaded_site <- p$site
          as.data.frame(p$horizons)
        },
        wosis = {
          sel <- input$wosis_table_rows_selected
          if (!length(sel)) {
            shiny::showNotification(i18n("pedon.wosis_select_first"),
                                    type = "warning")
            return(invisible())
          }
          ps <- .sk_wosis_pedons()
          if (sel[1] > length(ps)) return(invisible())
          p <- ps[[sel[1]]]
          loaded_site <- p$site        # WoSIS carries real site metadata
          as.data.frame(p$horizons)
        },
        upload = {
          f <- input$csv
          if (is.null(f)) {
            shiny::showNotification(i18n("pedon.choose_csv_first"), type = "warning")
            return(invisible())
          }
          sep <- if (grepl("\\.tsv$", f$name, ignore.case = TRUE)) "\t" else ","
          tryCatch(utils::read.csv(f$datapath, sep = sep,
                                   stringsAsFactors = FALSE),
                   error = function(e) {
                     shiny::showNotification(
                       i18n("pedon.csv_parse_failed", conditionMessage(e)),
                       type = "error")
                     NULL
                   })
        },
        blank = .pedon_blank_template()
      )
      if (is.null(df)) return(invisible())
      # Keep only columns soilKey understands, in canonical order.
      spec  <- names(soilKey::horizon_column_spec())
      keep  <- intersect(spec, names(df))
      extra <- setdiff(names(df), spec)
      df    <- df[, c(keep, extra), drop = FALSE]
      hz(df)
      hz_reload(hz_reload() + 1L)
      .sk_fill_site_inputs(session, loaded_site)
      shiny::showNotification(
        i18n("pedon.loaded_n", nrow(df)), type = "message")
    })

    shiny::observeEvent(input$add_row, {
      cur <- hz()
      if (is.null(cur)) cur <- .pedon_blank_template()[0, , drop = FALSE]
      new <- .pedon_blank_template()
      # Align columns of the blank row to the current table.
      for (cn in setdiff(names(cur), names(new))) new[[cn]] <- NA
      new <- new[, names(cur), drop = FALSE]
      if (nrow(cur) > 0L) {
        new$top_cm    <- max(cur$bottom_cm, na.rm = TRUE)
        new$bottom_cm <- new$top_cm + 20
      }
      hz(rbind(cur, new))
      hz_reload(hz_reload() + 1L)
    })

    # ---- WoSIS profile picker --------------------------------------------
    output$wosis_table <- DT::renderDT({
      cat_df <- .sk_wosis_catalogue()
      shiny::validate(shiny::need(nrow(cat_df) > 0L,
                                  i18n("pedon.wosis_unavailable")))
      DT::datatable(
        cat_df, rownames = FALSE, selection = "single",
        options = list(pageLength = 8, scrollX = TRUE, dom = "ftip")
      )
    })

    # ---- editable table ---------------------------------------------------
    output$hz_table <- DT::renderDT({
      hz_reload()                          # re-render only on load / add
      df <- shiny::isolate(hz())
      if (is.null(df)) df <- .pedon_blank_template()[0, , drop = FALSE]
      DT::datatable(
        df,
        editable  = list(target = "cell"),
        rownames  = FALSE,
        selection = "none",
        options   = list(pageLength = 12, scrollX = TRUE, dom = "tip")
      )
    })

    # v0.9.190: parse yes/no text before DT coerces it. Logical columns used to
    # swallow anything DT could not read as a literal TRUE/FALSE -- "no" became
    # NA -- so an answered criterion stayed "data needed". Now the ordinary
    # spellings are understood in both languages and an unreadable entry is
    # reported instead of being silently discarded.
    shiny::observeEvent(input$hz_table_cell_edit, {
      df <- hz()
      if (is.null(df)) return(invisible())
      info <- input$hz_table_cell_edit
      bad  <- character(0)
      for (k in seq_len(nrow(info))) {
        j <- info$col[k] + 1L            # rownames = FALSE -> 0-based columns
        if (is.na(j) || j < 1L || j > ncol(df)) next
        cn <- names(df)[j]
        if (!is.logical(df[[cn]])) next
        parsed <- .sk_parse_logical(info$value[k])
        if (is.null(parsed)) {
          bad <- c(bad, cn)
          info$value[k] <- NA
        } else {
          info$value[k] <- if (is.na(parsed)) NA else as.character(parsed)
        }
      }
      hz(DT::editData(df, info, rownames = FALSE))
      if (length(bad))
        shiny::showNotification(
          i18n("pedon.logical_not_understood", paste(unique(bad), collapse = ", ")),
          type = "warning", duration = 8)
    })

    # ---- depth profile ----------------------------------------------------
    output$profile <- plotly::renderPlotly({
      pro_profile_plot(hz(), input$plot_attr %||% "clay_pct")
    })

    # ---- live horizon-geometry feedback under the table -------------------
    # Reacts to every cell edit so problems (overlaps, gaps, inverted depths)
    # surface immediately, in the chosen language. AA-contrast colours.
    output$geom_status <- shiny::renderUI({
      df <- hz()
      if (is.null(df) || nrow(df) == 0L) return(NULL)
      lines <- .pedon_geom_lines(validate_horizon_geometry(df))
      if (length(lines$errors) == 0L && length(lines$warnings) == 0L) {
        return(shiny::div(class = "small mt-2", style = "color:#3f6024;",
                          shiny::icon("circle-check"), " ", i18n("pedon.geom_ok")))
      }
      shiny::tagList(
        lapply(lines$errors, function(m)
          shiny::div(class = "small mt-1", style = "color:#b02a37;",
                     shiny::icon("triangle-exclamation"), " ", m)),
        lapply(lines$warnings, function(m)
          shiny::div(class = "small mt-1", style = "color:#7a5b00;",
                     shiny::icon("circle-exclamation"), " ", m))
      )
    })

    # ---- build the PedonRecord -------------------------------------------
    shiny::observeEvent(input$build, {
      df <- hz()
      if (is.null(df) || nrow(df) == 0L) {
        shiny::showNotification(i18n("pedon.load_add_horizon_first"),
                                type = "warning")
        return(invisible())
      }
      # Guard the horizon geometry before it reaches the key: overlaps, inverted
      # or missing depths would build a nonsensical profile. Errors block;
      # warnings (gaps, surface offset, ...) are surfaced but allowed.
      geom <- validate_horizon_geometry(df)
      glines <- .pedon_geom_lines(geom)
      if (!geom$valid) {
        shiny::showNotification(
          i18n("pedon.geom_errors_block", paste(glines$errors, collapse = " ")),
          type = "error", duration = 8)
        return(invisible())
      }
      if (length(glines$warnings)) {
        shiny::showNotification(
          i18n("pedon.geom_warnings", paste(glines$warnings, collapse = " ")),
          type = "warning", duration = 6)
      }
      # Guard the coordinates before they reach the key: an out-of-range
      # lat/lon would silently poison the SoilGrids prior and the inferred
      # temperature regime. Blank is allowed (coords are optional).
      lat <- suppressWarnings(as.numeric(input$lat))
      lon <- suppressWarnings(as.numeric(input$lon))
      if (!is.na(lat) && (lat < -90 || lat > 90)) {
        shiny::showNotification(
          i18n("pedon.latitude_range"), type = "error")
        return(invisible())
      }
      if (!is.na(lon) && (lon < -180 || lon > 180)) {
        shiny::showNotification(
          i18n("pedon.longitude_range"), type = "error")
        return(invisible())
      }
      # v0.9.190: write the site-level construction answers onto the horizons the
      # key actually reads. This is what lets "no geomembrane" resolve the
      # Technosol criterion instead of leaving it permanently "data needed".
      # The editor table itself is left untouched, so nothing is rewritten
      # behind the user's back.
      df <- .sk_apply_site_technic(df, input$technic, input$geomembrane,
                                   input$geomembrane_depth)
      built <- tryCatch({
        h_dt <- soilKey::ensure_horizon_schema(data.table::as.data.table(df))
        soilKey::PedonRecord$new(
          site = list(
            id              = input$site_id %||% "pedon",
            lat             = input$lat,
            lon             = input$lon,
            country         = input$country,
            parent_material = input$pm
          ),
          horizons = h_dt
        )
      }, error = function(e) e)

      if (inherits(built, "error")) {
        shiny::showNotification(
          i18n("pedon.could_not_build_pedon", conditionMessage(built)),
          type = "error", duration = 8)
        return(invisible())
      }
      rv$pedon <- built
      shiny::showNotification(i18n("pedon.pedon_built_ready"),
                              type = "message")
    })

    output$status <- shiny::renderUI({
      if (is.null(rv$pedon)) {
        shiny::div(class = "text-muted small mt-2",
                   shiny::icon("circle-info"), i18n("pedon.no_pedon_yet"))
      } else {
        shiny::div(class = "text-success small mt-2",
                   shiny::icon("circle-check"), " ",
                   i18n("pedon.pedon_ready",
                        rv$pedon$site$id %||% "pedon",
                        nrow(rv$pedon$horizons)))
      }
    })
  })
}
