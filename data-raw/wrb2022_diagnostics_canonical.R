# =============================================================================
# Canonical WRB 2022 diagnostic reference + soilKey crosswalk.
#
# The official diagnostic names come from IUSS Working Group WRB (2022), *World
# Reference Base for Soil Resources 2022, 4th edition*, ISRIC, Chapter 3:
#   * 3.1 Diagnostic horizons   (40)
#   * 3.2 Diagnostic properties (17)
#   * 3.3 Diagnostic materials   (19)
# transcribed from the official document's section headers.
#
# The `soilkey_fn` column crosswalks each canonical diagnostic to the soilKey
# implementing function (namespace-resolved, see inst/rules/wrb2022/). This table
# is the auditable canonical set that coverage_report("wrb_horizons"/... ) diffs
# the implemented functions against -- replacing the hand-asserted README tallies.
#
# Run:  source("data-raw/wrb2022_diagnostics_canonical.R")  (writes the CSV + .rda)
# =============================================================================

# ---- 3.1 Diagnostic horizons (40) -------------------------------------------
horizons <- tibble::tribble(
  ~official,        ~soilkey_fn,
  "Albic",          "albic",
  "Anthraquic",     "anthraquic",
  "Argic",          "argic",
  "Calcic",         "calcic",
  "Cambic",         "cambic",
  "Chernic",        "chernic",
  "Cohesic",        "cohesic",        # v0.9.186 -- Brazilian Latossolo Coeso (Carater coeso)
  "Cryic",          "cryic_horizon",  # v0.9.186 -- horizon wrapper over cryic_conditions
  "Duric",          "duric_horizon",
  "Ferralic",       "ferralic",
  "Ferric",         "ferric",
  "Folic",          "folic",          # v0.9.186 -- well-aerated organic (aerobic Histic)
  "Fragic",         "fragic",
  "Gypsic",         "gypsic",
  "Histic",         "histic_horizon",
  "Hortic",         "hortic",
  "Hydragric",      "hydragric",
  "Irragric",       "irragric",
  "Limonic",        "limonic",
  "Mollic",         "mollic",
  "Natric",         "natric_horizon",
  "Nitic",          "nitic_horizon",
  "Panpaic",        "panpaic",
  "Petrocalcic",    "petrocalcic",
  "Petroduric",     "petroduric",
  "Petrogypsic",    "petrogypsic",
  "Petroplinthic",  "petroplinthic",
  "Pisoplinthic",   "pisoplinthic",
  "Plaggic",        "plaggic",
  "Plinthic",       "plinthic",
  "Pretic",         "pretic",
  "Protovertic",    "protovertic",
  "Salic",          "salic",
  "Sombric",        "sombric",
  "Spodic",         "spodic",
  "Terric",         "terric",
  "Thionic",        "thionic",
  "Tsitelic",       "tsitelic",
  "Umbric",         "umbric_horizon",
  "Vertic",         "vertic_horizon"
)

# ---- 3.2 Diagnostic properties (17) -----------------------------------------
properties <- tibble::tribble(
  ~official,                     ~soilkey_fn,
  "Abrupt textural difference",  "abrupt_textural_difference",
  "Albeluvic glossae",           "albeluvic_glossae",
  "Andic",                       "andic_properties",
  "Anthric",                     "anthric_horizons",
  "Continuous rock",             "continuous_rock",
  "Gleyic",                      "gleyic_properties",
  "Lithic discontinuity",        "lithic_discontinuity",
  "Protocalcic",                 "protocalcic_properties",
  "Protogypsic",                 "protogypsic_properties",
  "Reducing conditions",         "reducing_conditions",
  "Retic",                       "retic_properties",
  "Shrink-swell cracks",         "shrink_swell_cracks",
  "Sideralic",                   "sideralic_properties",
  "Stagnic",                     "stagnic_properties",
  "Takyric",                     "takyric_properties",
  "Vitric",                      "vitric_properties",
  "Yermic",                      "yermic_properties"
)

# ---- 3.3 Diagnostic materials (19) ------------------------------------------
materials <- tibble::tribble(
  ~official,             ~soilkey_fn,
  "Aeolic",              "aeolic_material",
  "Artefacts",           "artefacts",
  "Calcaric",            "calcaric_material",
  "Claric",              "claric_material",
  "Dolomitic",           "dolomitic_material",
  "Fluvic",              "fluvic_material",
  "Gypsiric",            "gypsiric_material",
  "Hypersulfidic",       "hypersulfidic_material",
  "Hyposulfidic",        "hyposulfidic_material",
  "Limnic",              "limnic_material",
  "Mineral",             "mineral_material",
  "Mulmic",              "mulmic_material",
  "Organic",             "organic_material",
  "Organotechnic",       "organotechnic_material",
  "Ornithogenic",        "ornithogenic_material",
  "Soil organic carbon", "soil_organic_carbon",
  "Solimovic",           "solimovic_material",
  "Technic hard",        "technic_hard_material",
  "Tephric",             "tephric_material"
)

wrb2022_diagnostics <- rbind(
  data.frame(category = "horizon",  horizons,   stringsAsFactors = FALSE),
  data.frame(category = "property", properties, stringsAsFactors = FALSE),
  data.frame(category = "material", materials,  stringsAsFactors = FALSE)
)

# ---- sanity + summary --------------------------------------------------------
stopifnot(
  sum(wrb2022_diagnostics$category == "horizon")  == 40L,
  sum(wrb2022_diagnostics$category == "property") == 17L,
  sum(wrb2022_diagnostics$category == "material") == 19L
)
gaps <- wrb2022_diagnostics[is.na(wrb2022_diagnostics$soilkey_fn), ]
cat(sprintf("WRB 2022 canonical diagnostics: %d horizons + %d properties + %d materials = %d\n",
            40, 17, 19, nrow(wrb2022_diagnostics)))
cat(sprintf("Implemented: %d ; GAPS: %d\n",
            sum(!is.na(wrb2022_diagnostics$soilkey_fn)), nrow(gaps)))
if (nrow(gaps)) { cat("GAPS:\n"); print(gaps[, c("category", "official")], row.names = FALSE) }

out_csv <- "inst/extdata/canonical/wrb2022_diagnostics.csv"
dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
utils::write.csv(wrb2022_diagnostics, out_csv, row.names = FALSE, na = "")
cat("wrote", out_csv, "\n")
