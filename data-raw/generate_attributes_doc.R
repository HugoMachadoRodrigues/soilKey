# =============================================================================
# Generate inst/ATTRIBUTES.md -- the canonical soilKey attribute reference.
#
# The document is generated straight from the live code so it can never drift:
#   * canonical names + R types  <- horizon_column_spec()
#   * accepted aliases           <- .horizon_name_aliases()
#   * group headers + per-field descriptions parsed from the R/utils.R source
#     comments (the spec's own inline comments ARE the descriptions).
# A coverage assertion guarantees every canonical column is documented.
#
# Run with:  Rscript data-raw/generate_attributes_doc.R
# =============================================================================

suppressMessages(pkgload::load_all(".", quiet = TRUE))

spec    <- horizon_column_spec()
aliases <- soilKey:::.horizon_name_aliases()
ver     <- as.character(utils::packageVersion("soilKey"))

# ---- reverse alias map: canonical -> character vector of aliases -------------
alias_rev <- split(names(aliases), unname(aliases))

# ---- parse group headers + inline comments from the spec source -------------
src   <- readLines("R/utils.R")
b0    <- grep("^horizon_column_spec <- function\\(\\) \\{", src)[1]
b1    <- which(src == "  )")[which(which(src == "  )") > b0)[1]]  # first `  )` after start
block <- src[b0:b1]

groups <- character(0)   # name -> group label
descs  <- character(0)   # name -> inline description (may be "")
cur_group <- "General"
for (ln in block) {
  gh <- regmatches(ln, regexec("^\\s*#\\s*-+\\s*(.+?)\\s*-+\\s*$", ln))[[1]]
  if (length(gh) == 2) { cur_group <- gh[2]; next }
  m <- regmatches(ln, regexec("^\\s*([A-Za-z0-9_]+)\\s*=\\s*\"[a-z]+\"\\s*,?\\s*(#\\s*(.*))?$", ln))[[1]]
  if (length(m) >= 2 && nzchar(m[2]) && m[2] %in% names(spec)) {
    nm <- m[2]
    groups[nm] <- cur_group
    descs[nm]  <- if (length(m) >= 4) trimws(m[4]) else ""
  }
}

# ---- curated overrides: cleaner descriptions + explicit units for the
#      everyday analytical attributes (specialist fields inherit their
#      source comment). unit is also inferred from the name suffix below. ----
override <- list(
  top_cm        = list(unit = "cm",        desc = "Upper depth of the horizon (from the soil surface)."),
  bottom_cm     = list(unit = "cm",        desc = "Lower depth of the horizon."),
  designation   = list(unit = "-",         desc = "Horizon designation / symbol (e.g. A, Bt, Bw, E, C, R)."),
  munsell_hue_moist   = list(unit = "Munsell", desc = "Munsell hue, moist (e.g. \"7.5YR\", \"10R\", \"N\")."),
  munsell_value_moist = list(unit = "Munsell", desc = "Munsell value, moist (0-10)."),
  munsell_chroma_moist= list(unit = "Munsell", desc = "Munsell chroma, moist."),
  munsell_hue_dry     = list(unit = "Munsell", desc = "Munsell hue, dry."),
  munsell_value_dry   = list(unit = "Munsell", desc = "Munsell value, dry (0-10)."),
  munsell_chroma_dry  = list(unit = "Munsell", desc = "Munsell chroma, dry."),
  clay_pct      = list(unit = "% (mass)",  desc = "Clay content of the fine earth (< 2 um)."),
  silt_pct      = list(unit = "% (mass)",  desc = "Silt content of the fine earth."),
  sand_pct      = list(unit = "% (mass)",  desc = "Sand content of the fine earth."),
  coarse_fragments_pct = list(unit = "% (vol)", desc = "Coarse fragments (> 2 mm) by volume."),
  ph_h2o        = list(unit = "-",         desc = "pH measured in water (1:2.5 or as reported). soilKey's default pH."),
  ph_kcl        = list(unit = "-",         desc = "pH measured in 1 M KCl."),
  ph_cacl2      = list(unit = "-",         desc = "pH measured in 0.01 M CaCl2."),
  oc_pct        = list(unit = "% (mass)",  desc = "Organic carbon of the fine earth."),
  n_total_pct   = list(unit = "% (mass)",  desc = "Total nitrogen."),
  cec_cmol      = list(unit = "cmol(+)/kg", desc = "Cation exchange capacity (of the fine earth)."),
  ecec_cmol     = list(unit = "cmol(+)/kg", desc = "Effective CEC (sum of exchangeable bases + exchangeable Al)."),
  bs_pct        = list(unit = "%",         desc = "Base saturation (SiBCS V%)."),
  al_sat_pct    = list(unit = "%",         desc = "Aluminium saturation (SiBCS m%)."),
  ca_cmol       = list(unit = "cmol(+)/kg", desc = "Exchangeable calcium."),
  mg_cmol       = list(unit = "cmol(+)/kg", desc = "Exchangeable magnesium."),
  k_cmol        = list(unit = "cmol(+)/kg", desc = "Exchangeable potassium."),
  na_cmol       = list(unit = "cmol(+)/kg", desc = "Exchangeable sodium."),
  al_cmol       = list(unit = "cmol(+)/kg", desc = "Exchangeable (KCl) aluminium."),
  caco3_pct     = list(unit = "% (mass)",  desc = "Calcium carbonate equivalent."),
  caso4_pct     = list(unit = "% (mass)",  desc = "Gypsum (calcium sulphate)."),
  bulk_density_g_cm3 = list(unit = "g/cm3", desc = "Bulk density of the fine earth."),
  ec_dS_m       = list(unit = "dS/m",      desc = "Electrical conductivity (saturated paste, 25 C)."),
  # ---- morphology (field description) ----
  boundary_distinctness = list(unit = "-", desc = "Distinctness of the lower boundary (abrupt / clear / gradual / diffuse)."),
  boundary_topography   = list(unit = "-", desc = "Topography of the lower boundary (smooth / wavy / irregular / broken)."),
  structure_grade  = list(unit = "-", desc = "Grade of structure (structureless / weak / moderate / strong)."),
  structure_size   = list(unit = "-", desc = "Size class of structural units (very fine / fine / medium / coarse / very coarse)."),
  structure_type   = list(unit = "-", desc = "Type of structure (granular / blocky / prismatic / platy / single-grain / massive)."),
  consistence_moist = list(unit = "-", desc = "Consistence when moist (loose / very friable / friable / firm / very firm / extremely firm)."),
  consistence_wet   = list(unit = "-", desc = "Consistence when wet (stickiness and plasticity)."),
  clay_films_amount   = list(unit = "-", desc = "Abundance of clay coatings / films (none / few / common / many)."),
  clay_films_strength = list(unit = "-", desc = "Distinctness / thickness of clay films (faint / distinct / prominent)."),
  # ---- oxides & water retention ----
  fe_dcb_pct = list(unit = "% Fe", desc = "Dithionite-citrate-bicarbonate extractable Fe (free/pedogenic iron oxides)."),
  fe_ox_pct  = list(unit = "% Fe", desc = "Acid ammonium-oxalate extractable Fe (active/amorphous iron; andic)."),
  al_ox_pct  = list(unit = "% Al", desc = "Acid ammonium-oxalate extractable Al (active/amorphous aluminium; andic)."),
  si_ox_pct  = list(unit = "% Si", desc = "Acid ammonium-oxalate extractable Si (andic properties)."),
  water_content_33kpa   = list(unit = "% (water)", desc = "Gravimetric water content at 33 kPa (field capacity)."),
  water_content_1500kpa = list(unit = "% (water)", desc = "Gravimetric water content at 1500 kPa (permanent wilting point).")
)

infer_unit <- function(nm) {
  if (grepl("_cm$", nm))        return("cm")
  if (grepl("_pct$", nm))       return("%")
  if (grepl("_cmol$", nm))      return("cmol(+)/kg")
  if (grepl("_g_cm3$", nm))     return("g/cm3")
  if (grepl("_mg_kg$", nm))     return("mg/kg")
  if (grepl("_dS_m$", nm))      return("dS/m")
  if (grepl("_C$", nm))         return("deg C")
  if (grepl("kpa$", nm))        return("% (water)")
  if (grepl("^munsell_", nm))   return("Munsell")
  if (grepl("_days$", nm))      return("days")
  "-"
}

md_escape <- function(x) gsub("\\|", "\\\\|", x)

# ---- assemble grouped tables ------------------------------------------------
out <- c(
  sprintf("# soilKey -- recognised horizon attribute names (v%s)", ver),
  "",
  "This is the authoritative list of the analytical and morphological attribute",
  "names that soilKey reads from a horizon table (CSV upload in soilKey Pro, or a",
  "`data.frame`/`data.table` passed to the R API). Every classification key reads",
  "these columns; anything else is preserved but ignored by the keys.",
  "",
  "## Naming conventions",
  "",
  "* Canonical names are **lower-case `snake_case`**, with an explicit unit/method",
  "  suffix (`_cm`, `_pct`, `_cmol`, `_g_cm3`, `_mg_kg`, `_dS_m`, `_C`).",
  "* Column-name recognition is **case-insensitive** and **separator-insensitive**:",
  "  `pH_H2O`, `PH.H2O`, `ph h2o` and `pH-H2O` are all recognised as `ph_h2o`.",
  "* A small table of **common abbreviations** is also accepted (see the *aliases*",
  "  column below): e.g. `Clay` -> `clay_pct`, `SOC` -> `oc_pct`, `CEC` -> `cec_cmol`.",
  "* An exact canonical name always wins and is never overwritten; unrecognised",
  "  columns are kept verbatim and simply not read by the keys.",
  "* Units are as listed. Percentages are by mass unless noted `(vol)`; exchangeable",
  "  cations and CEC are in cmol(+)/kg (= meq/100 g).",
  "",
  "> Tip (R): `soilKey::horizon_column_spec()` returns the canonical names and types",
  "> programmatically; `soilKey::ensure_horizon_schema(df)` normalises a table to them.",
  ""
)

# cleaner public section titles: drop internal "vX.Y additions:" history
# prefixes and capitalise the first letter (never full title-case -- it would
# mangle WRB / SiBCS / Ki/Kr / COLE acronyms).
clean_label <- function(g) {
  g <- sub("^v[0-9][0-9.A-Za-z]*\\s+additions?:\\s*", "", g)
  substring(g, 1, 1) <- toupper(substring(g, 1, 1))
  g
}

group_order <- unique(unname(groups[names(spec)]))
for (g in group_order) {
  members <- names(spec)[groups[names(spec)] == g]
  members <- members[!is.na(members)]
  if (!length(members)) next
  out <- c(out, sprintf("## %s", clean_label(g)), "",
           "| Attribute | Unit | Type | Description | Accepted aliases |",
           "|-----------|------|------|-------------|------------------|")
  for (nm in members) {
    ov   <- override[[nm]]
    unit <- if (!is.null(ov)) ov$unit else infer_unit(nm)
    desc <- if (!is.null(ov)) ov$desc else descs[nm]
    # strip internal "vX.Y[.Z]:" version tags from inherited source comments
    if (!is.null(desc) && !is.na(desc))
      desc <- sub("^v[0-9][0-9.A-Za-z]*:\\s*", "", desc)
    if (is.na(desc) || !nzchar(desc)) desc <- "-"
    al   <- alias_rev[[nm]]
    als  <- if (is.null(al)) "-" else paste0("`", sort(al), "`", collapse = ", ")
    out <- c(out, sprintf("| `%s` | %s | %s | %s | %s |",
                          nm, unit, spec[[nm]], md_escape(desc), als))
  }
  out <- c(out, "")
}

out <- c(out,
  "---",
  "",
  sprintf("_Generated from `horizon_column_spec()` and `.horizon_name_aliases()` in soilKey v%s._", ver),
  "")

# ---- coverage assertion -----------------------------------------------------
documented <- names(spec)[names(spec) %in% names(groups)]
missing <- setdiff(names(spec), documented)
if (length(missing))
  stop("Undocumented canonical columns (spec parser missed them): ",
       paste(missing, collapse = ", "))

writeLines(out, "inst/ATTRIBUTES.md")
cat(sprintf("Wrote inst/ATTRIBUTES.md: %d attributes across %d groups (v%s).\n",
            length(spec), length(group_order), ver))
