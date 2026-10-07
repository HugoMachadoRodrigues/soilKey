# =============================================================================
# soilKey Pro -- Acknowledgements & references tab (v0.9.166).
#
# A static credits page: the classification standards soilKey implements, the R
# packages it builds on, the data sources that test it, and the people whose
# review shaped it -- each with the specific contribution it is thanked for.
# Content is drawn from verifiable sources (package authorship, the published
# manuals, documented data services). It is deliberately editable: the closing
# note invites anyone to have their credit corrected, added or removed.
# =============================================================================

# One credited item: a bold lead (name / citation) and the contribution it is
# thanked for.
.ack_item <- function(lead, contribution) {
  shiny::tags$li(
    class = "sk-ack-item",
    shiny::tags$span(class = "sk-ack-lead", lead),
    shiny::tags$span(class = "sk-ack-contrib", contribution))
}

.ack_card <- function(title, icon_name, ...) {
  bslib::card(
    class = "sk-ack-card",
    bslib::card_header(shiny::icon(icon_name), " ", title),
    bslib::card_body(shiny::tags$ul(class = "sk-ack-list", ...)))
}

acknowledgements_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "sk-ack container-fluid py-3",

    shiny::div(
      class = "sk-ack-head",
      shiny::h3(shiny::icon("heart"), " ", i18n("thanks.title")),
      shiny::p(class = "text-muted", i18n("thanks.intro"))),

    bslib::layout_column_wrap(
      width = 1 / 2, heights_equal = "row",

      # ---- 1. Classification standards (the manuals) ----------------------
      .ack_card(
        i18n("thanks.s_standards"), "book",
        .ack_item(
          "IUSS Working Group WRB (2022). World Reference Base for Soil Resources 2022, 4th edition. International Union of Soil Sciences, Vienna.",
          i18n("thanks.c_wrb")),
        .ack_item(
          "Soil Survey Staff (2022). Keys to Soil Taxonomy, 13th edition. USDA-NRCS, Washington, DC.",
          i18n("thanks.c_usda")),
        .ack_item(
          "Santos, H.G. dos, Jacomine, P.K.T., Anjos, L.H.C. dos, et al. (2018). Sistema Brasileiro de Classificacao de Solos (SiBCS), 5th edition. Embrapa, Brasilia.",
          i18n("thanks.c_sibcs"))),

      # ---- 2. R packages soilKey builds on --------------------------------
      .ack_card(
        i18n("thanks.s_packages"), "cubes",
        .ack_item(
          "aqp -- the ncss-tech team (Dylan E. Beaudette, Andrew G. Brown, and colleagues).",
          i18n("thanks.c_aqp")),
        .ack_item(
          "SoilTaxonomy -- Andrew G. Brown, Dylan E. Beaudette and colleagues (ncss-tech).",
          i18n("thanks.c_soiltaxonomy")),
        .ack_item(
          "munsellinterpol -- Glenn Davis.",
          i18n("thanks.c_munsellinterpol")),
        .ack_item(
          "mpspline2 -- Brendan Malone and colleagues.",
          i18n("thanks.c_mpspline2")),
        .ack_item(
          i18n("thanks.l_ecosystem"),
          i18n("thanks.c_ecosystem"))),

      # ---- 3. Data sources & services -------------------------------------
      .ack_card(
        i18n("thanks.s_data"), "database",
        .ack_item(
          "SoilGrids / ISRIC - World Soil Information.",
          i18n("thanks.c_soilgrids")),
        .ack_item(
          "Open Soil Spectral Library (OSSL) -- Woodwell Climate Research Center, ISRIC and partners.",
          i18n("thanks.c_ossl")),
        .ack_item(
          "FEBR -- Free Brazilian Repository for Open Soil Data -- Alessandro Samuel-Rosa and contributors.",
          i18n("thanks.c_febr")),
        .ack_item(
          "Embrapa Solos -- BDSolos and the SmartSolos SiBCS classifier API (Glauber dos S. Vaz).",
          i18n("thanks.c_embrapa")),
        .ack_item(
          "Glauber J. Vaz, Alberto F. Silva Jr & Luis de F. da Silva Neto (2023) -- 'Brazilian soil data for taxonomic classification', Embrapa Redape (DOI 10.48432/PYKKA7).",
          i18n("thanks.c_redape"))),

      # ---- 4. Review & feedback -------------------------------------------
      .ack_card(
        i18n("thanks.s_feedback"), "comments",
        .ack_item(
          i18n("thanks.l_glenn"),
          i18n("thanks.c_glenn")),
        .ack_item(
          i18n("thanks.l_cran"),
          i18n("thanks.c_cran")))
    ),

    shiny::div(
      class = "sk-ack-note text-muted small",
      shiny::icon("circle-info"), " ", i18n("thanks.note"))
  )
}
