# =============================================================================
# soilKey Pro -- shared UI helpers (v0.9.97).
#
# Auto-sourced by Shiny from the app's R/ sub-directory.
# =============================================================================

# Catalogue of canonical fixtures, grouped for shinyWidgets::pickerInput.
# Display label -> exported make_*_canonical() function name.
pro_fixture_catalog <- function() {
  cat <- list(
    wrb = c(
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
    sibcs = c(
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
  # the two descriptive labels, in the session's language
  names(cat$wrb)[cat$wrb == "make_andosol_canonical"] <- i18n("fixture.andosol")
  names(cat$wrb)[cat$wrb == "make_cryosol_canonical"] <- i18n("fixture.cryosol")
  out <- stats::setNames(cat, c(i18n("fixture.group_wrb"), i18n("fixture.group_sibcs")))
  # v0.9.317: one example per order (or type) of each national system, under
  # the system's name; each keys to its own class
  for (id in intersect(.sk_national_systems()$id, names(.PRO_NATIONAL_EXAMPLES))) {
    ex <- .PRO_NATIONAL_EXAMPLES[[id]]
    out[[.sk_system_label(id)]] <- stats::setNames(
      paste0("make_", id, "_example:", unname(ex)), names(ex))
  }
  out
}

# The national examples: label (the class the example keys to) -> the order
# or type argument of make_<system>_example().
.PRO_NATIONAL_EXAMPLES <- list(
  cssc3 = c("Cryosolic" = "cryosolic", "Organic" = "organic", "Vertisolic" = "vertisolic",
            "Podzolic" = "podzolic", "Gleysolic" = "gleysolic", "Solonetzic" = "solonetzic",
            "Chernozemic" = "chernozemic", "Luvisolic" = "luvisolic",
            "Brunisolic" = "brunisolic", "Regosolic" = "regosolic"),
  asc3 = c("Anthroposol" = "anthroposols", "Organosol" = "organosols", "Podosol" = "podosols",
           "Vertosol" = "vertosols", "Hydrosol" = "hydrosols", "Kurosol" = "kurosols",
           "Sodosol" = "sodosols", "Chromosol" = "chromosols", "Calcarosol" = "calcarosols",
           "Ferrosol" = "ferrosols", "Dermosol" = "dermosols", "Kandosol" = "kandosols",
           "Arenosol" = "arenosols", "Rudosol" = "rudosols", "Tenosol" = "tenosols"),
  nzsc4 = c("Organic Soils" = "organic", "Gley Soils" = "gley", "Ultic Soils" = "ultic",
            "Podzol Soils" = "podzol", "Allophanic Soils" = "allophanic",
            "Pumice Soils" = "pumice", "Melanic Soils" = "melanic",
            "Semiarid Soils" = "semiarid", "Oxidic Soils" = "oxidic",
            "Granular Soils" = "granular", "Pallic Soils" = "pallic",
            "Brown Soils" = "brown", "Anthropic Soils" = "anthropic",
            "Recent Soils" = "recent", "Raw Soils" = "raw"),
  hscs2024 = stats::setNames(
    c("peat", "anthropogenic", "rocky_skeletal", "rock_influenced", "solonetz", "solonchak",
      "swelling_clay", "meadow", "carbonate", "steppe", "clay_illuvial", "sandy",
      "brown_earth", "deposit", "weakly_developed"),
    c("L\u00e1ptalaj", "Antropog\u00e9n talaj", "K\u00f6ves-szikl\u00e1s v\u00e1ztalaj",
      "K\u0151zethat\u00e1s\u00fa talaj", "Szolonyec talaj", "Szoloncs\u00e1k talaj",
      "Duzzad\u00f3 agyagtalaj", "R\u00e9ti talaj", "Karbon\u00e1t talaj",
      "Mez\u0151s\u00e9gi talaj", "Agyagbemos\u00f3d\u00e1sos talaj", "Homoktalaj",
      "Barnaf\u00f6ld", "Hordal\u00e9ktalaj", "F\u00f6ldeskop\u00e1r")),
  scsj2017 = c("Man-made soils" = "man_made", "Organic Soils" = "organic", "Andosols" = "andosol",
               "Podzols" = "podzol", "Fluvic soils" = "fluvic", "Red-Yellow soils" = "red_yellow",
               "Stagnic soils" = "stagnic", "Eutrosols" = "eutrosol",
               "Brown Forest soils" = "brown_forest", "Regosols" = "regosol"),
  cscj2011 = c("Man-made soils" = "man_made", "Organic soils" = "organic", "Podzols" = "podzol",
               "Andosols" = "andosol", "Dark Red soils" = "dark_red", "Lowland soils" = "lowland",
               "Red-Yellow soils" = "red_yellow", "Stagnic soils" = "stagnic",
               "Brown Forest soils" = "brown_forest", "Regosols" = "regosol"),
  cpcs1967 = c("Sols min\u00e9raux bruts" = "raw_mineral", "Sols peu \u00e9volu\u00e9s" = "weakly_developed",
               "Vertisols" = "vertisol", "Andosols" = "andosol",
               "Sols calcimagn\u00e9siques" = "calcimagnesic", "Sols isohumiques" = "isohumic",
               "Sols brunifi\u00e9s" = "brunified", "Sols podzolis\u00e9s" = "podzolised",
               "Sols \u00e0 sesquioxydes de fer et mangan\u00e8se" = "sesquioxide", "Sols ferrallitiques" = "ferrallitic",
               "Sols hydromorphes" = "hydromorphic", "Sols sodiques" = "sodic"),
  # the Russian examples, one per division, labelled by the division (its
  # English gloss, the name the result gives)
  kidpr2008 = c("Peat soils" = "AA", "Peatozems" = "AB", "Alluvial soils" = "BA",
                "Volcanic soils" = "BB", "Stratozems" = "BC", "Weakly developed soils" = "DA",
                "Texture-differentiated soils" = "CA", "Al-Fe-humus soils" = "CB",
                "Fe-metamorphic soils" = "CC", "Structural-metamorphic soils" = "CD",
                "Cryometamorphic soils" = "CE", "Pale-metamorphic soils" = "CF",
                "Cryogenic soils (cryozems)" = "CG", "Gley soils" = "CH",
                "Humus-accumulative soils" = "CI", "Light-humus carbonate-accumulative soils" = "CJ",
                "Alkaline clay-differentiated soils" = "CK", "Halomorphic soils" = "CL",
                "Hydrometamorphic soils" = "CM", "Organo-accumulative soils" = "CN",
                "Eluvial soils" = "CO", "Lithozems" = "CP", "Abrazems" = "CQ",
                "Agroabrazems" = "CR", "Agrozems" = "CS", "Turbozems" = "CT", "Chemozems" = "CU"),
  # the Chinese examples, one per suborder, labelled by the suborder (the
  # name the result gives)
  cst2001 = c("Permagelic Histosols" = "A1", "Orthic Histosols" = "A2", "Stagnic Anthrosols" = "B1",
              "Orthic Anthrosols" = "B2", "Humic Spodosols" = "C1", "Orthic Spodosols" = "C2",
              "Cryic Andosols" = "D1", "Vitric Andosols" = "D2", "Udic Andosols" = "D3",
              "Udic Ferralosols" = "E1", "Aquic Vertosols" = "F1", "Ustic Vertosols" = "F2",
              "Udic Vertosols" = "F3", "Cryic Aridosols" = "G1", "Orthic Aridosols" = "G2",
              "Alkalic Halosols" = "H1", "Orthic Halosols" = "H2", "Permagelic Gleyosols" = "I1",
              "Stagnic Gleyosols" = "I2", "Orthic Gleyosols" = "I3", "Lithomorphic Isohumosols" = "J1",
              "Ustic Isohumosols" = "J2", "Udic Isohumosols" = "J3", "Ustic Ferrosols" = "K1",
              "Perudic Ferrosols" = "K2", "Udic Ferrosols" = "K3", "Boric Argosols" = "L1",
              "Ustic Argosols" = "L2", "Perudic Argosols" = "L3", "Udic Argosols" = "L4",
              "Gelic Cambosols" = "M1", "Aquic Cambosols" = "M2", "Ustic Cambosols" = "M3",
              "Perudic Cambosols" = "M4", "Udic Cambosols" = "M5", "Anthric Primosols" = "N1",
              "Sandic Primosols" = "N2", "Alluvic Primosols" = "N3", "Orthic Primosols" = "N4"),
  # the examples of the FAO-Unesco Revised Legend (1988), one per major soil
  # grouping, labelled by the grouping (the first level the result gives)
  fao1988 = c("Histosols" = "HS", "Anthrosols" = "AT", "Leptosols" = "LP", "Vertisols" = "VR",
              "Fluvisols" = "FL", "Solonchaks" = "SC", "Gleysols" = "GL", "Andosols" = "AN",
              "Arenosols" = "AR", "Regosols" = "RG", "Podzols" = "PZ", "Plinthosols" = "PT",
              "Ferralsols" = "FR", "Planosols" = "PL", "Solonetz" = "SN", "Greyzems" = "GR",
              "Chernozems" = "CH", "Kastanozems" = "KS", "Phaeozems" = "PH", "Podzoluvisols" = "PD",
              "Gypsisols" = "GY", "Calcisols" = "CL", "Nitisols" = "NT", "Alisols" = "AL",
              "Acrisols" = "AC", "Luvisols" = "LV", "Lixisols" = "LX", "Cambisols" = "CM"),
  # the examples of the FAO-Unesco Legend of 1974, one per major soil group,
  # labelled by the group (the first level the result gives)
  fao1974 = c("Histosols" = "O", "Lithosols" = "I", "Vertisols" = "V", "Fluvisols" = "J",
              "Solonchaks" = "Z", "Gleysols" = "G", "Andosols" = "T", "Arenosols" = "Q",
              "Regosols" = "R", "Rankers" = "U", "Rendzinas" = "E", "Podzols" = "P",
              "Ferralsols" = "F", "Planosols" = "W", "Solonetz" = "S", "Greyzems" = "M",
              "Chernozems" = "C", "Kastanozems" = "K", "Phaeozems" = "H", "Podzoluvisols" = "D",
              "Xerosols" = "X", "Yermosols" = "Y", "Nitosols" = "N", "Acrisols" = "A",
              "Luvisols" = "L", "Cambisols" = "B"),
  # the French examples of the Referentiel pedologique 2008, one per reference
  # implemented, labelled by the reference (the singular, as the name gives it)
  rp2008 = c("alocrisol typique" = "alocrisols_typiques",
             "alocrisol humique" = "alocrisols_humiques", "vitrandosol" = "vitrandosols",
             "silandosol eutrique" = "silandosols_eutriques",
             "silandosol dystrique" = "silandosols_dystriques",
             "silandosol perhydrique" = "silandosols_perhydriques",
             "aluandosol haplique" = "aluandosols_hapliques",
             "aluandosol perhydrique" = "aluandosols_perhydriques",
             "anthroposol transform\u00e9" = "anthroposols_transformes",
             "anthroposol artificiel" = "anthroposols_artificiels",
             "anthroposol reconstitu\u00e9" = "anthroposols_reconstitues",
             "anthroposol construit" = "anthroposols_construits",
             "anthroposol arch\u00e9ologique" = "anthroposols_archeologiques",
             "ar\u00e9nosol" = "arenosols", "brunisol eutrique" = "brunisols_eutriques",
             "brunisol dystrique" = "brunisols_dystriques", "rendosol" = "rendosols",
             "calcosol" = "calcosols", "dolomitosol" = "dolomitosols", "rendisol" = "rendisols",
             "calcisol" = "calcisols", "magn\u00e9sisol" = "magnesisols",
             "calcarisol" = "calcarisols", "chernosol haplique" = "chernosols_hapliques",
             "chernosol typique" = "chernosols_typiques",
             "chernosol m\u00e9lanoluvique" = "chernosols_melanoluviques",
             "colluviosol" = "colluviosols", "cryosol histique" = "cryosols_histiques",
             "cryosol min\u00e9ral" = "cryosols_mineraux",
             "ferrallitisol meuble" = "ferrallitisols_meubles",
             "ferrallitisol nodulaire" = "ferrallitisols_nodulaires",
             "ferrallitisol p\u00e9troxydique" = "ferrallitisols_petroxydiques",
             "ferrallitisol duroxydique" = "ferrallitisols_duroxydiques",
             "oxydisol meuble" = "oxydisols_meubles", "oxydisol nodulaire" = "oxydisols_nodulaires",
             "oxydisol p\u00e9troxydique" = "oxydisols_petroxydiques",
             "ferruginosol meuble" = "ferruginosols_meubles",
             "ferruginosol semiluvique" = "ferruginosols_semiluviques",
             "ferruginosol luvique" = "ferruginosols_luviques",
             "ferruginosol p\u00e9troxydique" = "ferruginosols_petroxydiques",
             "fersialsol carbonat\u00e9" = "fersialsols_carbonates", "fersialsol calcique" = "fersialsols_calciques",
             "fersialsol insatur\u00e9" = "fersialsols_insatures", "fersialsol \u00e9luvique" = "fersialsols_eluviques",
             "fluviosol brut" = "fluviosols_bruts", "fluviosol juv\u00e9nile" = "fluviosols_juveniles",
             "fluviosol typique" = "fluviosols_typiques", "fluviosol brunifi\u00e9" = "fluviosols_brunifies",
             "grisol \u00e9luvique" = "grisols_eluviques", "grisol d\u00e9grad\u00e9" = "grisols_degrades",
             "grisol haplique" = "grisols_hapliques", "gypsosol haplique" = "gypsosols_hapliques",
             "gypsosol p\u00e9trogypsique" = "gypsosols_petrogypsiques",
             "histosol fibrique" = "histosols_fibriques", "histosol m\u00e9sique" = "histosols_mesiques",
             "histosol saprique" = "histosols_sapriques", "histosol composite" = "histosols_composites",
             "histosol leptique" = "histosols_leptiques", "leptismectisol" = "leptismectisols",
             "lithosol" = "lithosols", "n\u00e9oluvisol" = "neoluvisols", "luvisol typique" = "luvisols_typiques",
             "luvisol d\u00e9grad\u00e9" = "luvisols_degrades", "luvisol dernique" = "luvisols_derniques",
             "luvisol tronqu\u00e9" = "luvisols_tronques", "quasi-luvisol" = "quasi_luvisols", "nitosol" = "nitosols",
             "organosol holorganique" = "organosols_holorganiques", "organosol calcaire" = "organosols_calcaires",
             "organosol satur\u00e9" = "organosols_satures", "organosol insatur\u00e9" = "organosols_insatures",
             "p\u00e9losol typique" = "pelosols_typiques", "p\u00e9losol brunifi\u00e9" = "pelosols_brunifies",
             "p\u00e9losol diff\u00e9renci\u00e9" = "pelosols_differencies", "peyrosol" = "peyrosols",
             "ph\u00e6osol haplique" = "phaeosols_hapliques", "ph\u00e6osol m\u00e9lanoluvique" = "phaeosols_melanoluviques",
             "planosol typique" = "planosols_typiques", "planosol distal" = "planosols_distaux",
             "planosol structural" = "planosols_structuraux", "podzosol ocrique" = "podzosols_ocriques",
             "podzosol meuble" = "podzosols_meubles", "podzosol durique" = "podzosols_duriques",
             "podzosol humique" = "podzosols_humiques", "podzosol humo-durique" = "podzosols_humo_duriques",
             "podzosol \u00e9luvique" = "podzosols_eluviques", "podzosol placique" = "podzosols_placiques",
             "post-podzosol" = "post_podzosols", "rankosol" = "rankosols",
             "r\u00e9ductisol typique" = "reductisols_typiques", "r\u00e9ductisol stagnique" = "reductisols_stagniques",
             "r\u00e9doxisol" = "redoxisols", "r\u00e9gosol" = "regosols",
             "salisol chloruro-sulfat\u00e9" = "salisols_chloruro_sulfates",
             "salisol carbonat\u00e9" = "salisols_carbonates",
             "sodisol indiff\u00e9renci\u00e9" = "sodisols_indifferencies",
             "sodisol solonetzique" = "sodisols_solonetziques", "sodisol solodis\u00e9" = "sodisols_solodises",
             "sodisalisol" = "sodisalisols", "salisodisol" = "salisodisols",
             "thalassosol brut" = "thalassosols_bruts", "thalassosol juv\u00e9nile" = "thalassosols_juveniles",
             "thalassosol pold\u00e9ris\u00e9" = "thalassosols_polderises", "thiosol" = "thiosols",
             "sulfatosol" = "sulfatosols", "veracrisol" = "veracrisols", "lithovertisol" = "lithovertisols",
             "topovertisol" = "topovertisols", "paravertisol haplique" = "paravertisols_hapliques",
             "paravertisol planosolique" = "paravertisols_planosoliques"),
  # the South African examples of 1991, one per soil form, labelled by the form
  za1991 = c("Champagne" = "champagne", "Kranskop" = "kranskop", "Magwa" = "magwa", "Inanda" = "inanda",
             "Lusiki" = "lusiki", "Sweetwater" = "sweetwater", "Nomanci" = "nomanci",
             "Rensburg" = "rensburg", "Arcadia" = "arcadia", "Willowbrook" = "willowbrook",
             "Bonheim" = "bonheim", "Steendal" = "steendal", "Immerpan" = "immerpan", "Mayo" = "mayo",
             "Milkwood" = "milkwood", "Inhoek" = "inhoek", "Katspruit" = "katspruit",
             "Kroonstad" = "kroonstad", "Longlands" = "longlands", "Wasbank" = "wasbank",
             "Constantia" = "constantia", "Tsitsikamma" = "tsitsikamma", "Lamotte" = "lamotte",
             "Concordia" = "concordia", "Houwhoek" = "houwhoek", "Estcourt" = "estcourt",
             "Klapmuts" = "klapmuts", "Vilafontes" = "vilafontes", "Kinkelbos" = "kinkelbos",
             "Cartref" = "cartref", "Fernwood" = "fernwood", "Westleigh" = "westleigh",
             "Dresden" = "dresden", "Avalon" = "avalon", "Glencoe" = "glencoe", "Pinedene" = "pinedene",
             "Griffin" = "griffin", "Molopo" = "molopo", "Askham" = "askham", "Clovelly" = "clovelly",
             "Bainsvlei" = "bainsvlei", "Bloemdal" = "bloemdal", "Kimberly" = "kimberly",
             "Plooysburg" = "plooysburg", "Garies" = "garies", "Hutton" = "hutton",
             "Shortlands" = "shortlands", "Jonkersberg" = "jonkersberg", "Witfontein" = "witfontein",
             "Pinegrove" = "pinegrove", "Groenkop" = "groenkop", "Sterkspruit" = "sterkspruit",
             "Sepane" = "sepane", "Valsrivier" = "valsrivier", "Swartland" = "swartland",
             "Tukulu" = "tukulu", "Etosha" = "etosha", "Gamoep" = "gamoep", "Oudtshoorn" = "oudtshoorn",
             "Oakleaf" = "oakleaf", "Montagu" = "montagu", "Addo" = "addo", "Prieska" = "prieska",
             "Trawal" = "trawal", "Augrabies" = "augrabies", "Brandvlei" = "brandvlei", "Coega" = "coega",
             "Knersvlakte" = "knersvlakte", "Glenrosa" = "glenrosa", "Mispah" = "mispah",
             "Dundee" = "dundee", "Namib" = "namib", "Witbank" = "witbank"),
  # the German examples of the KA5 (2005), one per type of Tab. 51, labelled by
  # the type as printed
  ka5 = c("Erdniedermoor" = "KV", "Mulmniedermoor" = "KM", "Erdhochmoor" = "KH", "Hochmoor" = "HH",
          "Niedermoor" = "HN", "Protopedon" = "JP", "Gyttja" = "JG", "Sapropel" = "JS", "Dy" = "JD",
          "Nassstrand" = "IA", "Watt" = "IW", "Strand" = "UEA", "Rohmarsch" = "MR", "Kalkmarsch" = "MC",
          "Organomarsch" = "MO", "Knickmarsch" = "MK", "Dwogmarsch" = "MD", "Haftn\u00e4ssemarsch" = "MH",
          "Kleimarsch" = "MN", "Moorgley" = "GH", "Anmoorgley" = "GM", "Nassgley" = "GN", "Gley" = "GG",
          "Rambla" = "AO", "Tschernitza" = "AT", "Paternia" = "AQ", "Kalkpaternia" = "AZ", "Vega" = "AB",
          "Felshumusboden" = "FF", "Skeletthumusboden" = "FS", "Syrosem" = "OO", "Lockersyrosem" = "OL",
          "Kalktschernosem" = "TC", "Tschernosem" = "TT", "Ranker" = "RN", "Regosol" = "RQ",
          "Rendzina" = "RR", "Pararendzina" = "RZ", "Reduktosol" = "XX", "Kolluvisol" = "YK",
          "Plaggenesch" = "YE", "Rigosol" = "YY", "Treposol" = "YU", "Hortisol" = "YO", "Stagnogley" = "SG",
          "Haftpseudogley" = "SH", "Pseudogley" = "SS", "Podsol" = "PP", "Fahlerde" = "LF",
          "Parabraunerde" = "LL", "Braunerde" = "BB", "Terra rossa" = "CR", "Terra fusca" = "CF",
          "Pelosol" = "DD", "... \u00fcber Fersiallit" = "VV", "... \u00fcber Ferrallit" = "VW")
)

# Resolve a make_*_canonical() name, or a national example written
# "make_<system>_example:<order>", to a PedonRecord. Works whether the
# fixture is exported or internal.
pro_load_fixture <- function(fn_name) {
  parts <- strsplit(fn_name, ":", fixed = TRUE)[[1L]]
  fn <- get(parts[1L], envir = asNamespace("soilKey"))
  if (length(parts) > 1L) fn(parts[2L]) else fn()
}

# The national system of a national example ("make_cssc3_example:brunisolic"
# -> "cssc3"); NA for any other fixture.
pro_fixture_system <- function(fn_name) {
  m <- regmatches(fn_name, regexec("^make_([a-z0-9]+)_example:", fn_name))[[1L]]
  if (length(m)) m[2L] else NA_character_
}

# Numeric horizon columns surfaced in the editor / profile plot.
pro_numeric_attrs <- function() {
  c("clay_pct", "silt_pct", "sand_pct", "ph_h2o", "ph_kcl", "oc_pct",
    "cec_cmol", "ecec_cmol", "bs_pct", "al_sat_pct", "caco3_pct",
    "coarse_fragments_pct")
}

# A small coloured pill for an evidence grade (A best .. E weakest). NA is a
# profile with no soil property at all (v0.9.213): no grade to show.
pro_grade_badge <- function(grade) {
  grade <- as.character(grade %||% NA)
  pal <- c(A = "#198754", B = "#0d6efd", C = "#b35900",  # C: AA with white text
           D = "#dc3545", E = "#6c757d")
  col <- if (!is.na(grade) && grade %in% names(pal)) pal[[grade]] else "#6c757d"
  lab <- if (is.na(grade)) i18n("ui.no_measured_data")
         else paste(i18n("ui.evidence"), grade)
  shiny::tags$span(
    class = "badge",
    style = sprintf("background-color:%s;font-size:0.85rem;", col),
    lab
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
      if (is.na(res$evidence_grade %||% NA))
        shiny::div(class = "alert alert-warning small py-2 mb-2",
                   i18n("ui.no_measured_data_note")),
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


# DT::datatable() in the session's language. DataTables draws its own controls
# ("Search:", "Showing 1 to 10 of 25 entries", "Previous", "Next"); in English
# they are left to DataTables' defaults, in Portuguese they come from the
# catalogue.
sk_datatable <- function(data, ..., options = list()) {
  if (identical(.sk_app_lang(), "pt") && is.null(options$language))
    options$language <- list(
      search         = i18n("dt.search"),
      lengthMenu     = i18n("dt.length_menu"),
      info           = i18n("dt.info"),
      infoEmpty      = i18n("dt.info_empty"),
      infoFiltered   = i18n("dt.info_filtered"),
      zeroRecords    = i18n("dt.zero_records"),
      emptyTable     = i18n("dt.empty_table"),
      loadingRecords = i18n("dt.loading"),
      processing     = i18n("dt.processing"),
      paginate       = list(first = i18n("dt.first"), previous = i18n("dt.previous"),
                            `next` = i18n("dt.next"), last = i18n("dt.last")))
  sk_drop_jquery(DT::datatable(data, ..., options = options))
}

# The jQuery that DT and leaflet widgets bring along, dropped (v0.9.218). The
# page already loads Shiny's own jQuery, which the widgets use. Both widgets
# also carry jquerylib's copy, and when the two are the same version (3.6.0,
# with the Shiny 1.8 of the container image) they share the resource path
# "jquery-3.6.0": rendering a widget re-points it to jquerylib's folder, which
# has no jquery.min.js, so every page that instance served afterwards came up
# without jQuery (HTTP 404). Local installs, on a newer Shiny, never collide.
sk_drop_jquery <- function(widget) {
  widget$dependencies <- Filter(function(d) !identical(d$name, "jquery"),
                                widget$dependencies)
  widget
}

# leaflet::leaflet() without its own jQuery (see sk_drop_jquery()).
sk_leaflet <- function(...) sk_drop_jquery(leaflet::leaflet(...))

# DT::renderDT() with the data sent in the table itself (server = FALSE), over
# the session's websocket (v0.9.218). With server-side processing the browser
# fetches the rows by HTTP from /session/<id>/dataobj/<table>, and Cloud Run
# does not always route that request to the instance holding the session
# (session affinity is best effort): the table stayed empty with a 404. The
# app's tables are small (horizons, key traces, attribute lists).
sk_renderDT <- function(expr, ...) {
  DT::renderDT(substitute(expr), server = FALSE, quoted = TRUE,
               env = parent.frame(), ...)
}


# Colours for the Map's class layers (SoilGrids overlay, predicted grid, batch
# points). leaflet's "Set3" has 12 colours: over more classes leaflet
# interpolated between them, so neighbouring classes got near-identical shades
# (and RColorBrewer warned "n too large" in the log; the SoilGrids overlay of a
# Brazilian state shows ~19 classes). Up to 12 classes keep Set3's colours;
# beyond that each class gets its own colour from Polychrome 36, less its
# near-white grey and pale cream (v0.9.214): on the map and in the legend's
# white box those read as "no class".
.SK_POLYCHROME <- setdiff(unname(grDevices::palette.colors(36, "Polychrome 36")),
                          c("#E4E1E3", "#F7E1A0"))
sk_class_pal <- function(domain, na.color = "transparent") {
  n <- length(unique(domain[!is.na(domain)]))
  cols <- if (n <= 12L) {
    RColorBrewer::brewer.pal(max(3L, n), "Set3")[seq_len(max(1L, n))]
  } else if (n <= length(.SK_POLYCHROME)) {
    .SK_POLYCHROME[seq_len(n)]
  } else {
    grDevices::hcl.colors(n, "Dynamic")
  }
  leaflet::colorFactor(cols, domain = domain, na.color = na.color)
}


# Register the scripts and styles of `page` (jQuery, Bootstrap, bslib, Shiny's
# own, the widgets) in this R process. Shiny registers a dependency only when it
# renders the page, so an R process that had not rendered one answered 404 for
# all of them. On Cloud Run, with more than one instance, the 40-odd requests of
# one page load are not all routed to the instance that rendered the page
# (session affinity is best effort): visitors got the page without its styles
# or scripts, an unstyled list of links and no Shiny. app.R calls this at
# start-up, so every instance can serve every asset.
#
# Outside a running app bslib builds its themed dependencies (selectize,
# shiny-sass, bslib's component CSS) from its global theme, so that is set to
# the app's theme while rendering; without it they come out unthemed, under
# other file names. renderPage() is internal to shiny, hence the fallback to
# the page's own dependencies if it ever changes.
#
# v0.9.218: the app's theme is also made every session's theme from the start.
# Shiny records the current theme (the "bootstrapTheme" option) when it serves
# a page while the app is running, and sessions copy the app's options when
# they open. app.R is sourced before the app state exists (shiny::runApp()), so
# this start-up render did not record it, and a session that Cloud Run opened
# on an instance that had not yet served a page had no theme: DT tables fell
# back to their default style, and selectize and bslib's component CSS were
# rendered unthemed, re-pointing their resource paths to folders without the
# themed files, so pages that instance served afterwards got 404s for them.
# Set before the app state exists, the option is copied into it.
sk_register_page_deps <- function(page, theme) {
  shiny::shinyOptions(bootstrapTheme = theme)
  old_bs <- bslib::bs_global_set(theme)
  on.exit(bslib::bs_global_set(old_bs), add = TRUE)
  ok <- tryCatch({ shiny:::renderPage(page); TRUE }, error = function(e) FALSE)
  if (!ok)
    for (d in htmltools::renderTags(page)$dependencies) shiny::createWebDependency(d)
  sk_register_widget_deps()   # under the theme: DT takes its flavour from it
  invisible(ok)
}

# The scripts that only a drawn widget brings, registered at start-up (v0.9.224).
#
# Not every script of the app is in its page. A widget adds some when it is
# drawn: DT its DataTables core (in the theme's Bootstrap flavour), plotly its
# plotly.js bundle, and a leaflet map with provider tiles the provider list and
# its plugin. Rendering the page did not register those, and Shiny did only in
# the R process that drew the widget. On Cloud Run, with a second instance up,
# the browser's request for them could reach an instance that had drawn none:
# HTTP 404. Seen on 2026-10-10 for the two provider scripts, in the request
# log (page and websocket on one instance, the 404s on another): the Map tab
# came up as a grey box, without tiles or overlay.
#
# One widget of each kind the app draws gives the list. Only prefixes that are
# not registered yet are added, so nothing the page registered is re-pointed,
# and a widget's own jQuery is left out, as in sk_drop_jquery().
sk_register_widget_deps <- function() {
  widgets <- list(
    function() sk_datatable(data.frame(a = 1)),
    function() plotly::plot_ly(x = 1, y = 1, type = "bar"),
    function() sk_provider_tiles(sk_leaflet(), "Esri.WorldGrayCanvas"))
  added <- character(0)
  for (w in widgets) {
    deps <- tryCatch(htmltools::renderTags(w())$dependencies,
                     error = function(e) list())
    for (d in deps) {
      prefix <- paste0(d$name, "-", d$version)
      if (identical(d$name, "jquery") ||
            prefix %in% names(shiny::resourcePaths())) next
      shiny::createWebDependency(d)
      added <- c(added, prefix)
    }
  }
  invisible(added)
}

# leaflet::addProviderTiles(), for a map or a map proxy, with the provider
# scripts taken from sk_provider_deps(), the list the start-up registration
# uses, so both point to the same folders (v0.9.224).
#
# Shiny serves the folder a dependency is in, whole. leaflet takes the
# provider list from the installed leaflet.providers package folder (so in the
# container image, checked inside it), and has a fallback that writes the list
# to the R session's temporary folder. sk_provider_deps() moves a dependency
# found there to a folder that holds its scripts and nothing else, so the
# temporary folder is never served.
sk_provider_tiles <- function(map, provider, layerId = NULL, group = NULL,
                              options = leaflet::providerTileOptions()) {
  if (!provider %in% names(leaflet::providers))
    stop("Unknown tile provider '", provider, "'", call. = FALSE)
  map$dependencies <- c(map$dependencies, sk_provider_deps())
  leaflet::invokeMethod(map, leaflet::getMapData(map), "addProviderTiles",
                        provider, layerId, group, options)
}

.sk_widget_deps <- new.env(parent = emptyenv())

# The dependencies leaflet::addProviderTiles() adds to a map: what a map with
# provider tiles has and a plain one does not.
sk_provider_deps <- function() {
  if (is.null(.sk_widget_deps$providers)) {
    plain <- vapply(sk_leaflet()$dependencies, function(d) d$name, character(1))
    deps <- leaflet::addProviderTiles(sk_leaflet(),
                                      "Esri.WorldGrayCanvas")$dependencies
    deps <- Filter(function(d) !(d$name %in% plain), deps)
    .sk_widget_deps$providers <-
      lapply(htmltools::resolveDependencies(deps), sk_own_dependency)
  }
  .sk_widget_deps$providers
}

# A dependency whose files are in the R session's temporary folder, moved to a
# folder that holds those files only. Any other dependency is returned as is.
sk_own_dependency <- function(dep) {
  dir <- dep$src$file
  if (is.null(dir) || !is.null(dep$package) ||
        !identical(normalizePath(dir, mustWork = FALSE),
                   normalizePath(tempdir(), mustWork = FALSE)))
    return(dep)
  files <- unlist(c(dep$script, dep$stylesheet), use.names = FALSE)
  own <- file.path(tempdir(), "sk-deps", paste0(dep$name, "-", dep$version))
  dir.create(own, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(dir, files), own, overwrite = TRUE)
  dep$src$file <- own
  dep
}
