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
          "The reference-soil-group key, qualifiers and diagnostic horizons/properties/materials implemented for the WRB system."),
        .ack_item(
          "Soil Survey Staff (2022). Keys to Soil Taxonomy, 13th edition. USDA-NRCS, Washington, DC.",
          "The order-to-subgroup keys and diagnostic criteria for USDA Soil Taxonomy."),
        .ack_item(
          "Santos, H.G. dos, Jacomine, P.K.T., Anjos, L.H.C. dos, et al. (2018). Sistema Brasileiro de Classificacao de Solos (SiBCS), 5th edition. Embrapa, Brasilia.",
          "The ordem-to-subgrupo keys, atributos diagnosticos and familia criteria for the Brazilian system.")),

      # ---- 2. R packages soilKey builds on --------------------------------
      .ack_card(
        i18n("thanks.s_packages"), "cubes",
        .ack_item(
          "aqp -- the ncss-tech team (Dylan E. Beaudette, Andrew G. Brown, and colleagues).",
          "Horizon-geometry algorithms, argillic/cambic boundary detection, and the SoilProfileCollection the app interoperates with."),
        .ack_item(
          "SoilTaxonomy -- Andrew G. Brown, Dylan E. Beaudette and colleagues (ncss-tech).",
          "The vendored Keys to Soil Taxonomy and WRB 2022 criteria tables used to audit soilKey's own predicates."),
        .ack_item(
          "munsellinterpol -- Glenn Davis.",
          "CIE-anchored Munsell <-> XYZ conversion, and identifying the D65->Illuminant C chromatic adaptation and correct roundHVC() usage in soilKey's spectra-to-colour path."),
        .ack_item(
          "mpspline2 -- Brendan Malone and colleagues.",
          "Mass-preserving spline harmonisation of horizon data to standard depths."),
        .ack_item(
          "terra, sf, leaflet, shiny, bslib, DT, plotly, ellmer and the wider R ecosystem.",
          "Spatial analysis, the interactive map, the application framework, and optional vision-language extraction.")),

      # ---- 3. Data sources & services -------------------------------------
      .ack_card(
        i18n("thanks.s_data"), "database",
        .ack_item(
          "SoilGrids / ISRIC - World Soil Information.",
          "Global soil-property and WRB class-probability priors sampled by the Map tab."),
        .ack_item(
          "Open Soil Spectral Library (OSSL) -- Woodwell Climate Research Center, ISRIC and partners.",
          "Vis-NIR / MIR spectra with paired laboratory labels for the spectral gap-fill engine."),
        .ack_item(
          "FEBR -- Free Brazilian Repository for Open Soil Data -- Alessandro Samuel-Rosa and contributors.",
          "Brazilian profiles with Munsell colours used for benchmarking."),
        .ack_item(
          "Embrapa Solos -- BDSolos and the SmartSolos SiBCS classifier API (Glauber dos S. Vaz).",
          "A national soil database and an independent SiBCS reference to cross-check soilKey against."),
        .ack_item(
          "Glauber J. Vaz, Alberto F. Silva Jr & Luis de F. da Silva Neto (2023) -- 'Brazilian soil data for taxonomic classification', Embrapa Redape (DOI 10.48432/PYKKA7).",
          "A curated set of ~96 hand-reviewed Brazilian soil profiles, shared as a gold-standard benchmark used to test and calibrate the classifiers.")),

      # ---- 4. Review & feedback -------------------------------------------
      .ack_card(
        i18n("thanks.s_feedback"), "comments",
        .ack_item(
          "Glenn Davis (author of munsellinterpol).",
          "Colorimetry corrections to the spectra-to-Munsell path (chromatic adaptation and rounding)."),
        .ack_item(
          "CRAN and Uwe Ligges.",
          "Package review and publication."))
    ),

    shiny::div(
      class = "sk-ack-note text-muted small",
      shiny::icon("circle-info"), " ", i18n("thanks.note"))
  )
}
