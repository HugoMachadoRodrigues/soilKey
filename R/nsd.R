# =============================================================================
# v0.9.334 -- the National Soils Database of New Zealand (NSD) as a benchmark
# for the New Zealand Soil Classification (classify_nzsc4()).
#
# The NSD is kept by Manaaki Whenua - Landcare Research and served by the
# National Soil Data Repository (NSDR), which exports each soil survey as a
# zip of relational CSV tables (sa_site, sd_soil, sd_horizon, the em_ tables
# of the field description, sa_sample and ob_observation for the laboratory,
# cl_concept for the code lists). soilKey does not ship it: download the
# survey exports and pass them to read_nsd_pedons(). The tables are read in
# memory, straight from the zips; nothing is written to disk.
#
# LICENCE. The NSDR serves the NSD under the Creative Commons Attribution -
# No Derivatives 4.0 International licence (CC BY-ND 4.0): the data may be
# passed on only unchanged and whole, with credit to Manaaki Whenua -
# Landcare Research. The pedons built here are a translation of the records,
# for analysis by the user who downloaded them; do not redistribute them.
# The terms travel with every pedon (site$licence) and nsd_citation() gives
# the acknowledgement. Check the NSDR terms of use for the current terms.
#
# WHAT THE TRANSLATION READS:
#   * Codes are decoded through the survey's cl_concept table; values written
#     as words are read as written.
#   * Designations: the NSD's conversion to the current notation
#     (designation_fao) when it has one, with an FAO H master (peat) read as
#     the New Zealand O; otherwise the surveyor's designation. In the older
#     New Zealand notation (Taylor and Pohlen 1962) of many surveys,
#     old_notation = "translate" (default) reads (B) as Bw, the suffixes fe,
#     ca and cs as s, k and y, an upper-case G suffix as g, and moves the
#     digits written between the master and its suffixes to the end (B2g is
#     Bg2). Hugo's readings of 2026-10-30: A2 is E, with its subdivisions
#     and suffixes (A21 E1, A2g Eg, A22x Ex2; (A2) also E); the old master
#     G, a gley horizon, is Bg when a B horizon lies below it in the
#     profile, otherwise Cg (the usual lowest gleyed horizon), GC likewise
#     BCg or Cg; the old master D, the substratum, is C, its prefixes and
#     suffixes kept (2D 2C, BD BC, DG Cg, Dh Ch) except its old suffixes r
#     and n, which are not read (Hugo, 2026-10-30: the NSD's own conversions
#     give Dr as Cu or C and Dn as Bt, 2Btb, Cm or Cu, so neither is the r
#     or n of the current notations; Dr and Dn are C). The NSD's own conversions
#     (designation_fao) are not touched; the provenance keeps the recorded
#     designation and says what it was read as. A cemented sesquioxidic B
#     written Bms is the Bfm of a placic horizon when 10 mm or less thick,
#     else the Bsm of an ortstein-pan (NZSC Diagnostics).
#   * Matrix colour: moist (the New Zealand convention); of co-dominant
#     matrix colours the first recorded is read and the others noted.
#   * Mottles: the abundance classes of Milne et al. (1995, p. 97), read at
#     their middle (very few 1, few 6, common 17.5, many 37.5, abundant
#     62.5, profuse 87.5 %); with absent_unless_recorded, a described horizon
#     with no mottle record in a survey that records mottles has none, unless
#     its designation marks gleying.
#   * Structure: the pedality degree and the shape of the first ped record;
#     the size class gives the largest ped size (Milne et al. 1995: very
#     fine 6, fine 10, medium 20, coarse 60, very coarse 100 mm). The older
#     NZ term "nut" (and "nutty") is not a shape of Milne et al. (1995,
#     Table 17); the 2024 handbook (Note 12.1) records nut as granular "or
#     PR", most likely a slip for PH (polyhedral). Hugo's reading of
#     2026-10-30: nut is granular or polyhedral, one of the two, not known
#     which. It is kept in structure_type_one_of ("granular or polyhedral")
#     with structure_type left empty: the NZSC key reads a criterion under
#     each shape (one that needs polyhedral peds is undecided, one that takes
#     granular or polyhedral peds holds), and the keys that do not read
#     structure_type_one_of read the shape as not recorded. A second ped
#     record of nut is not read (structure_type_secondary holds one shape).
#   * Strength: the classes of Milne et al. (1995, p. 83) (codes 1 to 6 and
#     their names), moist unless the horizon was described dry. The older
#     words moderately weak, moderately firm and moderately strong (and mod
#     weak, mod firm) belong to the scale of Taylor and Pohlen (1962), which
#     soilKey does not have; they have no class in Milne et al. (1995) and
#     stay unread (undecided), as Hugo decided on 2026-10-30. Failure
#     (brittle, friable, ...) is the moist consistence.
#   * Coatings: clay coats are clay films; the darkest coat is the colour of
#     the coatings. Pans: an ironpan 10 mm or less thick is a placic horizon
#     (a thin iron pan); a thicker one is an iron pan of no named kind.
#   * Laboratory: each sample goes to the horizon the NSD estimates for it
#     (sd_horizon_id_est), else to the horizon holding its mid-depth; several
#     samples are averaged by thickness; a horizon with none takes the
#     samples that overlap it, weighted by the overlap. MIR predictions are
#     not read, nor values below or above a detection limit (CaCO3 below the
#     limit is 0). Clay is the fraction finer than 2 um (fe2); sand (2-0.06
#     mm) and silt (0.06-0.002 mm) as the NSD calculates them; total carbon
#     is organic carbon (it includes any carbonate carbon).
#   * Parent material: the soil note PARENT ROCK is the site's
#     parent_material; Hugo's reading of 2026-10-30 takes it down to each
#     layer whose origin is not recorded (layer_origin, which the NZSC key
#     reads for the fluvial features of the Fluvial Recent and Fluvial Raw
#     Soils, and for pumice, fill and refuse), while a layer with its own
#     recorded origin keeps it.
#     The NSD exports have no origin field per horizon (only free-text
#     horizon notes, which are not read). A note of one material goes to
#     every layer. A note naming a sequence (X over, overlying or on Y; Y
#     under or underlain by X; a slash is not read as a sequence) is split
#     where its parts are as many as the profile's lithologic groups (the
#     numbered breaks, 2C or IIC): the first part to the unnumbered layers,
#     the next to the next group. Otherwise it stays the site's
#     parent_material and goes to no layer (Hugo, 2026-10-30).
#   * Depths start at the soil surface (organic horizons recorded above the
#     mineral surface with negative depths are shifted down).
# =============================================================================

.NSD_DOI <- "https://doi.org/10.26060/95m4-cz25"
.NSD_LICENCE <- paste("CC BY-ND 4.0 (Manaaki Whenua - Landcare Research, NSDR terms of use):",
                      "redistribution only unchanged and whole, with credit to Manaaki Whenua",
                      "- Landcare Research; do not redistribute pedons translated from it")

#' Citation for data read from the New Zealand National Soils Database
#'
#' Returns the acknowledgement for profiles of the National Soils Database
#' (NSD) of Manaaki Whenua - Landcare Research, as exported by the National
#' Soil Data Repository (NSDR) and read by \code{\link{read_nsd_pedons}}.
#'
#' @param accessed Optional date the survey exports were downloaded.
#' @return A character string.
#' @examples
#' nsd_citation()
#' @export
nsd_citation <- function(accessed = NULL) {
  paste0("Manaaki Whenua - Landcare Research (2020). National Soils Database (NSD). ",
         .NSD_DOI, ". Survey exports of the National Soil Data Repository (NSDR)",
         if (!is.null(accessed)) paste0(", downloaded on ", format(as.Date(accessed), "%Y-%m-%d")),
         ". Licensed under CC BY-ND 4.0.")
}

# ---- Reading the exports -----------------------------------------------------

# The surveys under `path`: zip files, or folders holding sd_soil.csv; a
# folder of either is searched one level down.
.nsd_sources <- function(path) {
  out <- character(0)
  for (p in path) {
    if (dir.exists(p)) {
      if (file.exists(file.path(p, "sd_soil.csv"))) { out <- c(out, p); next }
      zips <- list.files(p, pattern = "\\.zip$", full.names = TRUE, ignore.case = TRUE)
      subs <- list.dirs(p, recursive = FALSE)
      subs <- subs[file.exists(file.path(subs, "sd_soil.csv"))]
      out <- c(out, sort(zips), sort(subs))
    } else if (file.exists(p) && grepl("\\.zip$", p, ignore.case = TRUE)) {
      out <- c(out, p)
    } else {
      rlang::abort(sprintf("read_nsd_pedons(): no NSD survey at %s", p))
    }
  }
  unique(out)
}

# One table of a survey (zip or folder), all columns as text; NULL when the
# survey has no such table or it is empty. Headers may carry a byte-order
# mark and spaces; a header longer than its rows (ob_observation) is padded.
.nsd_read_table <- function(src, name) {
  txt <- NULL
  if (dir.exists(src)) {
    f <- file.path(src, name)
    if (!file.exists(f)) return(NULL)
    txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
  } else {
    entries <- utils::unzip(src, list = TRUE)$Name
    hit <- entries[basename(entries) == name]
    if (!length(hit)) return(NULL)
    con <- unz(src, hit[1L])
    on.exit(close(con), add = TRUE)
    txt <- readLines(con, encoding = "UTF-8", warn = FALSE)
  }
  if (length(txt) < 2L) return(NULL)
  txt[1L] <- sub("^\ufeff", "", txt[1L])
  d <- utils::read.csv(text = txt, colClasses = "character", na.strings = c("", "NULL"),
                       check.names = FALSE, strip.white = TRUE, fill = TRUE,
                       quote = "\"", comment.char = "", encoding = "UTF-8")
  names(d) <- trimws(names(d))
  keep <- nzchar(names(d)) & !duplicated(names(d))
  d <- d[, keep, drop = FALSE]
  if (!nrow(d)) return(NULL)
  d
}

# Decoder of the survey's code lists: (table, column, values) -> names; a
# value that is not a code of that column is returned as it is.
.nsd_decoder <- function(cl) {
  if (is.null(cl) || !all(c("tablename", "columnname", "code", "name") %in% names(cl)))
    return(function(table, column, v) as.character(v))
  key <- paste(cl$tablename, cl$columnname, cl$code, sep = "|")
  nm <- stats::setNames(as.character(cl$name), key)
  nm <- nm[!duplicated(names(nm))]
  function(table, column, v) {
    v <- trimws(as.character(v))
    out <- unname(nm[paste(table, column, v, sep = "|")])
    ifelse(is.na(out), v, out)
  }
}

.nsd_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# A Munsell value or chroma: its leading number ("2M" 2, "4/4-3" 4).
.nsd_munsell_num <- function(x) {
  .nsd_num(sub("^\\s*([0-9]+(\\.[0-9]+)?).*$", "\\1", ifelse(is.na(x), NA_character_, x)))
}

# NZTM2000 (EPSG:2193) easting and northing to WGS84 latitude and longitude
# (GRS80, inverse transverse Mercator; Snyder 1987, eqs 8-12 to 8-18).
.nsd_nztm_to_lonlat <- function(E, N) {
  a <- 6378137; f <- 1 / 298.257222101; k0 <- 0.9996
  lon0 <- 173 * pi / 180; FE <- 1600000; FN <- 10000000
  e2 <- 2 * f - f^2; ep2 <- e2 / (1 - e2)
  M <- (N - FN) / k0
  mu <- M / (a * (1 - e2 / 4 - 3 * e2^2 / 64 - 5 * e2^3 / 256))
  e1 <- (1 - sqrt(1 - e2)) / (1 + sqrt(1 - e2))
  phi1 <- mu + (3 * e1 / 2 - 27 * e1^3 / 32) * sin(2 * mu) +
    (21 * e1^2 / 16 - 55 * e1^4 / 32) * sin(4 * mu) +
    (151 * e1^3 / 96) * sin(6 * mu) + (1097 * e1^4 / 512) * sin(8 * mu)
  C1 <- ep2 * cos(phi1)^2; T1 <- tan(phi1)^2
  N1 <- a / sqrt(1 - e2 * sin(phi1)^2)
  R1 <- a * (1 - e2) / (1 - e2 * sin(phi1)^2)^1.5
  D <- (E - FE) / (N1 * k0)
  lat <- phi1 - (N1 * tan(phi1) / R1) * (D^2 / 2 -
    (5 + 3 * T1 + 10 * C1 - 4 * C1^2 - 9 * ep2) * D^4 / 24 +
    (61 + 90 * T1 + 298 * C1 + 45 * T1^2 - 252 * ep2 - 3 * C1^2) * D^6 / 720)
  lon <- lon0 + (D - (1 + 2 * T1 + C1) * D^3 / 6 +
    (5 - 2 * C1 + 28 * T1 - 3 * C1^2 + 8 * ep2 + 24 * T1^2) * D^5 / 120) / cos(phi1)
  list(lat = lat * 180 / pi, lon = lon * 180 / pi)
}

# ---- The surveyor's classes ---------------------------------------------------

# NZSC codes of the 3rd edition (2010) that the 4th edition (2026) recodes or
# renames (its Changelog), and subgroups it withdraws: their soils now key
# elsewhere, BLAM perhaps in another order (Mafic Brown or Mafic Melanic
# Soils), so BLAM is read at no level; AFA and RHI keep their order and group.
.NSD_NZSC_RECODE <- c(MIW = "MIC", MIMW = "MIMC", AFE = "AFT", BFAC = "BFCA", BFAL = "BFLA",
                      PXJM = "PXMJ", PJU = "PJY", PJMU = "PJMY", AFST = "AFX", BAX = "BAC")
.NSD_NZSC_WITHDRAWN <- c(BLAM = 0L, AFA = 2L, RHI = 2L)   # levels still read

.nsd_nzsc_reference <- function(raw) {
  raw <- trimws(as.character(raw))
  none <- list(code = NA_character_, name = NA_character_, order = NA_character_,
               group = NA_character_, subgroup = NA_character_, note = NA_character_)
  if (is.na(raw) || !nzchar(raw)) return(none)
  code <- toupper(trimws(sub("\\s*\\[.*$", "", raw)))
  name <- if (grepl("\\[.*\\]", raw)) trimws(sub("^.*\\[(.*)\\].*$", "\\1", raw)) else NA_character_
  if (!grepl("^[A-Z]{1,4}$", code)) return(utils::modifyList(none, list(name = name)))
  note <- NA_character_
  if (code %in% names(.NSD_NZSC_RECODE)) {
    note <- sprintf("3rd-edition code %s read as %s (NZSC 4th ed., Changelog)", code, .NSD_NZSC_RECODE[[code]])
    code <- .NSD_NZSC_RECODE[[code]]
  }
  keep <- 3L
  if (code %in% names(.NSD_NZSC_WITHDRAWN)) {
    keep <- .NSD_NZSC_WITHDRAWN[[code]]
    note <- sprintf("subgroup %s withdrawn in the NZSC 4th ed. (Changelog): read %s", code,
                    if (keep == 0L) "at no level" else "at order and group level only")
  }
  n <- nchar(code)
  list(code = code, name = name,
       order = if (keep >= 1L) substr(code, 1, 1) else NA_character_,
       group = if (keep >= 2L && n >= 2L) substr(code, 1, 2) else NA_character_,
       subgroup = if (keep >= 3L && n >= 3L) code else NA_character_,
       note = note)
}

# Soil Taxonomy order from the formative element of the great group.
.NSD_USDA_ORDER <- c(ept = "Inceptisols", and = "Andisols", ent = "Entisols", ert = "Vertisols",
                     oll = "Mollisols", alf = "Alfisols", ult = "Ultisols", ist = "Histosols",
                     od = "Spodosols", ox = "Oxisols", id = "Aridisols", el = "Gelisols")

.nsd_usda_reference <- function(raw) {
  none <- list(order = NA_character_, grtgroup = NA_character_, subgroup = NA_character_,
               edition = NA_character_)
  s <- toupper(trimws(gsub("\\s+", " ", as.character(raw))))
  if (is.na(s) || !nzchar(s)) return(none)
  s <- gsub("\\s*-\\s*", "-", sub("^[?]\\s*", "", s))
  tok <- strsplit(s, " ", fixed = TRUE)[[1L]]
  gg <- tolower(tok[length(tok)])
  if (!grepl("^[a-z]+$", gg)) return(none)
  hit <- names(.NSD_USDA_ORDER)[vapply(names(.NSD_USDA_ORDER), function(k) endsWith(gg, k), NA)]
  if (!length(hit)) return(none)
  mods <- tok[-length(tok)]
  sg <- if (length(mods) && all(grepl("^[A-Z-]+$", mods)))
    paste(tolower(c(mods, paste0(gg, "s"))), collapse = " ") else NA_character_
  list(order = unname(.NSD_USDA_ORDER[[hit[1L]]]), grtgroup = paste0(gg, "s"), subgroup = sg,
       # Andepts were Inceptisols until Soil Taxonomy created the Andisols (1990)
       edition = if (grepl("andept$", gg)) "before 1990 (Andepts)" else NA_character_)
}

# ---- Designations ------------------------------------------------------------

# The designation the NZSC key reads, and a note when it differs from the
# record. `fao`: the NSD conversion (designation_fao), else NA; `thick`: the
# horizon thickness in cm; `soil` and `top`: the profile and the top depth of
# each horizon (the old master G is read by what lies below it).
.nsd_designation <- function(orig, fao, old_notation = "translate", thick = rep(NA_real_, length(orig)),
                             soil = rep("1", length(orig)), top = seq_along(orig)) {
  n <- length(orig)
  out <- rep(NA_character_, n); note <- rep(NA_character_, n)
  use_fao <- !is.na(fao) & nzchar(trimws(fao))
  f <- trimws(fao[use_fao])
  # FAO H (peat) is the New Zealand O
  h <- grepl("^([0-9]*[b]?)H([0-9a-z]*)$", f)
  f2 <- sub("^([0-9]*[b]?)H([0-9a-z]*)$", "\\1O\\2", f)
  out[use_fao] <- f2
  note[use_fao] <- ifelse(h, sprintf("NSD designation_fao %s, its FAO H (peat) read as O; recorded %s", f, orig[use_fao]),
                          sprintf("NSD designation_fao (recorded %s)", ifelse(is.na(orig[use_fao]), "with none", orig[use_fao])))
  o <- !use_fao & !is.na(orig) & nzchar(trimws(orig))
  d <- trimws(orig[o]); d0 <- d
  why <- rep(NA_character_, length(d))
  pre <- "^(([0-9]+|[IVX]+)?[a-z]*)"                 # lithologic and lower-case prefixes
  if (identical(old_notation, "translate")) {
    d <- sub("^(([0-9]+|[IVX]+)?[a-z]*)\\(B([0-9]*)\\)([a-z]*)$", "\\1Bw\\4\\3", d)     # (B)g1
    d <- sub("^(([0-9]+|[IVX]+)?[a-z]*)\\(B([0-9]*)([a-z]*)\\)$", "\\1Bw\\4\\3", d)     # (B1g)
    d <- sub("^([0-9]*[LOAEBC])-([0-9]+)$", "\\1\\2", d)                                 # O-2
    d <- gsub("fe", "s", d, fixed = TRUE)
    d <- gsub("ca", "k", d, fixed = TRUE)
    d <- gsub("cs", "y", d, fixed = TRUE)
    # A2 is E, with its subdivisions and suffixes (A21 E1, A2g and A2G Eg,
    # A22x Ex2); (A2), in the parentheses of the old notation, also E
    a2 <- paste0(pre, "\\(?A2([0-9]*)([a-z]*)(G?)([0-9]*)\\)?$")
    k <- grepl(a2, d) & (grepl("\\(A2", d) == grepl("\\)$", d))
    d[k] <- sub("G", "g", sub(a2, "\\1E\\4\\5\\3\\6", d[k]), fixed = TRUE)
    why[k] <- "the A2 is E"
    # the old master D (the substratum) is C, its prefixes and suffixes kept
    # (as a second master too: BD is BC); its r and n are dropped below
    kd <- grepl(paste0(pre, "([AB]?)D"), d)
    d[kd] <- sub(paste0(pre, "([AB]?)D"), "\\1\\3C", d[kd])
    why[kd] <- "the old master D, the substratum, is C"
    d <- sub("^([0-9]*[a-z]*[ABC])([0-9]*)G([0-9]*)$", "\\1g\\2\\3", d)                   # B2G, BG
    d <- sub("^([0-9]*[a-z]*[LOAEBC]{1,2})([0-9]+)([a-z]+)$", "\\1\\3\\2", d)             # B2g -> Bg2
    # the old D's suffixes r and n are not read (Hugo, 2026-10-30): the
    # NSD's own conversions give Dr as Cu or C and Dn as Bt, 2Btb, Cm or Cu,
    # so neither is the r (reduced) or n (sodium) of the current notations;
    # g and h stay (DG Cg, Dh Ch)
    cpat <- paste0(pre, "([AB]?C)([A-Za-z]*)(.*)$")
    if (any(kd)) {
      dk <- d[kd]
      d[kd] <- paste0(sub(cpat, "\\1\\3", dk), gsub("[rnN]", "", sub(cpat, "\\4", dk)), sub(cpat, "\\5", dk))
      why[kd] <- ifelse(d[kd] == sub(cpat, "\\1\\3\\4\\5", dk), why[kd],
                        "the old master D, the substratum, is C; its old suffixes r and n are not read")
    }
  }
  out[o] <- d
  # the old master G (a gley horizon; GC its transition to the C): Bg (BCg)
  # when a B horizon lies below it in the profile, otherwise Cg, the usual
  # lowest gleyed horizon; its subdivisions and suffixes kept (G1 Bg1, Gx Cgx)
  if (identical(old_notation, "translate")) {
    gpat <- paste0(pre, "G(C?)([0-9]*)([a-z]*)([0-9]*)$")
    io <- which(o); out0 <- out
    for (j in which(grepl(gpat, d))) {
      i <- io[j]
      below <- which(soil == soil[i] & !is.na(top) & !is.na(top[i]) & top > top[i])
      b_below <- any(grepl(paste0(pre, "B"), out0[below]))
      p <- regmatches(d[j], regexec(gpat, d[j]))[[1L]]
      master <- if (b_below) { if (nzchar(p[4L])) "BC" else "B" } else "C"
      d[j] <- paste0(p[2L], master, "g", gsub("g", "", p[6L], fixed = TRUE), p[5L], p[7L])
      why[j] <- if (b_below) "the old master G, a gley horizon, is Bg: a B horizon lies below it"
                else "the old master G, a gley horizon, is Cg: no B horizon lies below it"
      out[i] <- d[j]
    }
  }
  note[o] <- ifelse(d == d0, "NSD designation (surveyor)",
                    sprintf("NSD designation %s in the older New Zealand notation, read as %s%s", d0, d,
                            ifelse(is.na(why), "", paste0(" (", why, ")"))))
  # Bms (the NSD's order of the letters) for a cemented sesquioxidic B: the
  # New Zealand notation writes Bfm for a placic horizon (10 mm or less
  # thick) and Bsm for an ortstein-pan (thicker)
  ms <- !is.na(out) & grepl("^([0-9]*[a-z]*)B[mM][sS]([0-9]*)$", out)
  if (any(ms)) {
    thin <- !is.na(thick) & thick <= 1
    new <- ifelse(thin, sub("^([0-9]*[a-z]*)B[mM][sS]([0-9]*)$", "\\1Bfm\\2", out),
                  sub("^([0-9]*[a-z]*)B[mM][sS]([0-9]*)$", "\\1Bsm\\2", out))
    note[ms] <- paste0(note[ms], sprintf("; %s read as %s (%s)", out[ms], new[ms],
                       ifelse(thin[ms], "10 mm or less thick: a placic horizon", "thicker than 10 mm: an ortstein-pan")))
    out[ms] <- new[ms]
  }
  list(designation = out, note = note)
}

# ---- Field description --------------------------------------------------------

.NSD_MOTTLE_PCT <- c("1" = 1, "2" = 6, "3" = 17.5, "4" = 37.5, "5" = 62.5, "6" = 87.5,
                     "very few" = 1, few = 6, common = 17.5, many = 37.5, abundant = 62.5,
                     profuse = 87.5, no = 0, none = 0)
.NSD_PED_SIZE_MM <- c("1" = 1, "2" = 2, "3" = 6, "4" = 10, "5" = 20, "6" = 60, "7" = 100,
                      microfine = 1, "extremely fine" = 2, "very fine" = 6, fine = 10,
                      medium = 20, coarse = 60, "very coarse" = 100)
.NSD_PED_SIZE <- c("1" = "very fine", "2" = "very fine", "3" = "very fine", "4" = "fine",
                   "5" = "medium", "6" = "coarse", "7" = "very coarse")
.NSD_GRADE <- c("weakly developed" = "weak", weakly = "weak", weak = "weak",
                "weakly to moderately developed" = "weak",
                "moderately developed" = "moderate", moderately = "moderate", moderate = "moderate",
                "moderately to strongly developed" = "moderate",
                "strongly developed" = "strong", strongly = "strong", strong = "strong",
                massive = "structureless", masive = "structureless",
                "single grain" = "structureless")
.NSD_SHAPE <- c(polyhedra = "polyhedral", polyheda = "polyhedral", polyhedral = "polyhedral",
                blocky = "blocky", crumb = "crumb", granular = "granular", prismatic = "prismatic",
                platy = "platy", columnar = "columnar", spheroida = "spheroidal",
                spheroidal = "spheroidal")
# The older ped term nut: granular or polyhedral, one of the two, not known
# which (Hugo, 2026-10-30; NZ Soil Description Handbook 2024, Note 12.1,
# records nut as granular "or PR", most likely PH, polyhedral). Not a shape
# of Milne et al. (1995, Table 17).
.NSD_SHAPE_ONE_OF <- c(nut = "granular or polyhedral", nutty = "granular or polyhedral",
                       "nutty ?" = "granular or polyhedral")
# Strength classes of Milne et al. (1995, p. 83). The older words moderately
# weak, moderately firm and moderately strong (mod weak, mod firm) are of the
# Taylor and Pohlen (1962) scale, which soilKey does not have: they are left
# out on purpose and stay unread (undecided), as Hugo decided (2026-10-30).
.NSD_STRENGTH <- c("1" = "very weak", "2" = "weak", "3" = "slightly firm", "4" = "firm",
                   "5" = "very firm", "6" = "hard", loose = "loose", "very weak" = "very weak",
                   weak = "weak", "slightly firm" = "slightly firm", firm = "firm",
                   "very firm" = "very firm", hard = "hard", "very strong" = "very strong",
                   rigid = "rigid")
.NSD_FAILURE <- c(B = "brittle", D = "deformable", F = "friable", S = "semi-deformable",
                  V = "very friable", brittle = "brittle", deformable = "deformable",
                  friable = "friable", "semi-deformable" = "semi-deformable",
                  "very friable" = "very friable")
.NSD_STICKY <- c("1" = "non-sticky", "2" = "slightly sticky", "3" = "moderately sticky",
                 "4" = "very sticky")
.NSD_PLASTIC <- c("1" = "non-plastic", "2" = "slightly plastic", "3" = "moderately plastic",
                  "4" = "very plastic")
# Milne et al. (1995): sharp < 0.5, abrupt 0.5-2, distinct 2-5, indistinct
# 5-10, diffuse >= 10 cm; in the FAO words of the other keys
.NSD_BOUNDARY <- c("1" = "very abrupt", "2" = "abrupt", "3" = "clear", "4" = "gradual",
                   "5" = "diffuse", sharp = "very abrupt", abrupt = "abrupt", distinct = "clear",
                   indistinct = "gradual", diffuse = "diffuse")
.NSD_TOPOGRAPHY <- c(S = "smooth", W = "wavy", I = "irregular", discontinuous = "broken")
.NSD_CEMENT <- c(uncemented = "none", "very weakly cemented" = "very weakly",
                 "weakly cemented" = "weakly", "moderately cemented" = "moderately",
                 "strongly cemented" = "strongly", indurated = "indurated")
.NSD_AMOUNT <- c("1" = "very few", "2" = "few", "3" = "common", "4" = "many", "5" = "many",
                 "6" = "many", no = "none", none = "none", few = "few", common = "common",
                 many = "many")
.NSD_ROOTS <- c("1" = "few", "2" = "few", "3" = "common", "4" = "many", "5" = "abundant",
                no = "none", none = "none")
.NSD_DRAINAGE <- c(well = "well drained", poor = "poorly drained", imperfect = "imperfectly drained",
                   "moderately well" = "moderately well drained",
                   "somewhat excessive" = "somewhat excessively drained",
                   excessive = "excessively drained", "very poor" = "very poorly drained")
.NSD_TEXTURE_QUAL <- c("1" = "coarse", "2" = "medium", "3" = "fine")
# Texture codes of Milne et al. (1995, Table 12) for a survey whose code list
# lacks them (SC as the NSD code lists name it)
.NSD_TEXTURE <- c(S = "sand", LS = "loamy sand", SL = "sandy loam", LZ = "loamy silt", Z = "silt",
                  SCL = "sandy clay loam", SC = "sandy clay loam", CL = "clay loam", ZL = "silt loam",
                  LC = "loamy clay", ZC = "silty clay", C = "clay")

.nsd_lookup <- function(table, v) {
  v <- tolower(trimws(as.character(v)))
  names(table) <- tolower(names(table))
  unname(table[v])
}

# ---- Laboratory ----------------------------------------------------------------

# ob_observation property -> soilKey column, the factor to soilKey units and
# the unit as the NSD records it.
.NSD_LAB <- list(
  oc_pct = list("totalc", 1, "%, total carbon (includes any carbonate carbon)"),
  n_total_pct = list("totaln", 1, "%"),
  ph_h2o = list(c("phh2o", "phh2o_moist"), 1, "pH in water (1:2.5)"),
  ph_kcl = list("phkcl", 1, "pH in 1 M KCl"),
  ph_cacl2 = list("phcacl2", 1, "pH in CaCl2"),
  ph_naf = list("phnaf", 1, "pH in 1 M NaF"),
  phosphate_retention_pct = list("pretention", 1, "%"),
  cec_cmol = list("cec", 1, "me% = cmol(+)/kg, pH 7"),
  ca_cmol = list("exchangeableca", 1, "me% = cmol(+)/kg"),
  mg_cmol = list("exchangeablemg", 1, "me% = cmol(+)/kg"),
  k_cmol = list("exchangeablek", 1, "me% = cmol(+)/kg"),
  na_cmol = list("exchangeablena", 1, "me% = cmol(+)/kg"),
  al_cmol = list("kclextractableal", 1, "me% = cmol(+)/kg, 1 M KCl"),
  al_kcl_cmol = list("kclextractableal", 1, "me% = cmol(+)/kg, 1 M KCl"),
  bs_pct = list("basesaturation", 1, "%"),
  al_ox_pct = list("alacidoxalate", 1, "%"),
  fe_ox_pct = list("feacidoxalate", 1, "%"),
  si_ox_pct = list("siacidoxalate", 1, "%"),
  fe_dcb_pct = list("fedithionitecitrate", 1, "%"),
  fe_pyro_pct = list("fepyrophosphate", 1, "%"),
  al_pyro_pct = list("alpyrophosphate", 1, "%"),
  clay_pct = list("fe2", 1, "% finer than 2 um"),
  sand_pct = list("sand", 1, "% w/w, 2-0.06 mm"),
  silt_pct = list("silt", 1, "% w/w, 0.06-0.002 mm"),
  bulk_density_g_cm3 = list(c("drybulkdensityfineearth", "drybulkdensitywholesoil"), 1, "t/m3 = g/cm3"),
  volcanic_glass_pct = list("volcanicglasssand", 1, "% of the sand grains"),
  coarse_fragments_pct = list("stonesover2mm", 1, "% v/v, > 2 mm"),
  caco3_pct = list("calciumcarbonate", 1, "%"),
  ec_1to5_dS_m = list("k25_1:5", 1, "mS/cm = dS/m, 1:5 water extract"),
  water_content_1500kpa = list("airdriedwaterretention15bar", 1, "% w/w, air-dried"),
  water_content_1500kpa_undried = list("fieldmoistwaterretention15bar", 1, "% w/w, field-moist"),
  cole_value = list("cole", 1, "-"))

# One value per laboratory sample and property: MIR predictions and values
# beyond a detection limit are left out (CaCO3 below its limit is 0); the
# mean of what remains.
.nsd_lab_values <- function(ob) {
  if (is.null(ob) || !all(c("sa_sample_lab_id", "propertytypename", "result_val") %in% names(ob)))
    return(NULL)
  props <- unique(unlist(lapply(.NSD_LAB, `[[`, 1L)))
  ob <- ob[ob$propertytypename %in% props, , drop = FALSE]
  if (!nrow(ob)) return(NULL)
  meth <- tolower(as.character(ob$method %||% rep(NA, nrow(ob))))
  ob <- ob[!grepl("^mir-", meth), , drop = FALSE]
  op <- trimws(as.character(ob$result_op %||% rep("=", nrow(ob))))
  v <- .nsd_num(ob$result_val)
  below <- op %in% "<" & ob$propertytypename == "calciumcarbonate"
  v[below] <- 0
  v[op %in% c("<", ">", "<=", ">=") & !below] <- NA_real_
  ok <- is.finite(v)
  if (!any(ok)) return(NULL)
  d <- data.frame(sample = ob$sa_sample_lab_id[ok], prop = ob$propertytypename[ok], v = v[ok],
                  stringsAsFactors = FALSE)
  stats::aggregate(v ~ sample + prop, data = d, FUN = mean)
}

# Which horizon each laboratory sample belongs to: the NSD's estimate when
# that horizon is kept, else the one holding the sample's mid-depth.
.nsd_sample_hosts <- function(smp, hz) {
  top <- .nsd_num(ifelse(is.na(smp$lab_samplingdepth_minval), smp$field_samplingdepth_minval,
                         smp$lab_samplingdepth_minval))
  bot <- .nsd_num(ifelse(is.na(smp$lab_samplingdepth_maxval), smp$field_samplingdepth_maxval,
                         smp$lab_samplingdepth_maxval))
  est <- as.character(smp$sd_horizon_id_est %||% rep(NA, nrow(smp)))
  host <- ifelse(est %in% hz$sd_horizon_id, est, NA_character_)
  mid <- (top + bot) / 2
  for (i in which(is.na(host) & is.finite(mid))) {
    j <- which(hz$sa_site_id == smp$sa_site_id[i] & hz$top <= mid[i] & hz$bot > mid[i])
    if (length(j)) host[i] <- hz$sd_horizon_id[j[1L]]
  }
  data.frame(sample = smp$sa_laboratorysample_id %||% paste0(smp$sa_sample_id, "-lab"),
             site = smp$sa_site_id, top = top, bot = bot, host = host,
             stringsAsFactors = FALSE)
}

# The parts of a PARENT ROCK note that names a sequence of materials, top
# first: "X over Y", "X overlying Y", "X on Y" (and "Y under X",
# "Y underlying X", "Y underlain by X", read from the bottom). NULL for a
# note of one material, and for a note that mixes the two directions. A
# slash is not read as a sequence: the NSD also writes it in mixtures
# (granite/limestone) and abbreviations (Taup/pum, G/W).
.nsd_note_sequence <- function(note) {
  if (is.na(note) || !nzchar(note)) return(NULL)
  down <- "(?i)\\s+(over|overlying|overlies|on|upon)\\s+"
  up <- "(?i)\\s+(under|underlying|underlies|underlain\\s+by)\\s+"
  has_down <- grepl(down, note, perl = TRUE); has_up <- grepl(up, note, perl = TRUE)
  if (has_down == has_up) return(NULL)
  parts <- trimws(strsplit(note, if (has_down) down else up, perl = TRUE)[[1L]])
  parts <- parts[nzchar(parts)]
  if (length(parts) < 2L) return(NULL)
  if (has_up) rev(parts) else parts
}

# The lithologic group of each designation (the numeral prefix, Arabic or
# Roman; none is 1).
.nsd_litho_number <- function(d) {
  d <- trimws(as.character(d))
  out <- rep(1L, length(d))
  k <- !is.na(d) & grepl("^[0-9]+", d)
  out[k] <- as.integer(sub("^([0-9]+).*", "\\1", d[k]))
  r <- !is.na(d) & !k & grepl("^[IVX]+[A-Z]", d)
  out[r] <- vapply(sub("^([IVX]+)[A-Z].*", "\\1", d[r]),
                   function(x) as.integer(utils::as.roman(x)), integer(1))
  out[is.na(d)] <- NA_integer_
  out
}

# The soil's PARENT ROCK note (`pm`, named by soil) taken down to the layers
# whose origin (`own`) is not recorded; a layer with its own recorded origin
# keeps it (Hugo, 2026-10-30). A note of one material goes to every such
# layer. A note naming a sequence ("loess over outwash gravels") is split
# where its parts are as many as the profile's lithologic groups (the
# numbered breaks of `designation`, 2C, IIC; groups taken in depth order by
# `top`): the first part goes to the unnumbered layers, the second to the
# layers of the next group, and so on. Otherwise a sequence goes to no layer
# and stays the site's parent_material only (Hugo, 2026-10-30). `filled`: the
# layers that took the note or a part of it; `how`: the provenance text.
.nsd_layer_origin <- function(own, soil, pm, designation = rep(NA_character_, length(own)),
                              top = seq_along(own)) {
  own <- as.character(own)
  n <- length(own)
  free <- is.na(own) | !nzchar(trimws(own))
  filled <- rep(FALSE, n); how <- rep(NA_character_, n)
  num <- .nsd_litho_number(designation)
  for (s in unique(as.character(soil))) {
    note <- unname(pm[s])
    if (is.null(note) || is.na(note) || !nzchar(note)) next
    rows <- which(as.character(soil) == s)
    seq_parts <- .nsd_note_sequence(note)
    if (is.null(seq_parts)) {
      r <- rows[free[rows]]
      own[r] <- note; filled[r] <- TRUE
      how[r] <- "NSD sd_soil_note PARENT ROCK of the soil, taken down to a layer whose origin is not recorded"
      next
    }
    g <- num[rows]
    if (anyNA(g)) next
    first_top <- tapply(top[rows], g, min, na.rm = TRUE)
    groups <- as.integer(names(sort(first_top)))
    if (length(groups) != length(seq_parts)) next
    for (k in seq_along(groups)) {
      r <- rows[g == groups[k] & free[rows]]
      own[r] <- seq_parts[k]; filled[r] <- TRUE
      how[r] <- sprintf("NSD sd_soil_note PARENT ROCK of the soil (\"%s\"), part %d of %d, by the layer's lithologic group",
                        note, k, length(groups))
    }
  }
  list(value = own, filled = filled, how = how)
}

# ---- One survey ------------------------------------------------------------------

.nsd_survey_pedons <- function(src, absent_unless_recorded = TRUE, old_notation = "translate",
                               accessed = NULL) {
  rd <- function(n) .nsd_read_table(src, n)
  soil <- rd("sd_soil.csv"); hz <- rd("sd_horizon.csv"); sites <- rd("sa_site.csv")
  if (is.null(soil) || is.null(hz) || is.null(sites)) return(list())
  dec <- .nsd_decoder(rd("cl_concept.csv"))
  survey <- rd("dm_surveydataset.csv")
  survey_name <- if (!is.null(survey)) as.character(survey$name[1L]) else NA_character_
  survey_code <- if (!is.null(survey)) as.character(survey$code[1L]) else NA_character_
  col <- function(d, k) if (!is.null(d) && k %in% names(d)) as.character(d[[k]]) else rep(NA_character_, NROW(d))

  # horizons with depths
  hz$sa_site_id <- soil$sa_site_id[match(hz$sd_soil_id, soil$sd_soil_id)]
  hz$top <- .nsd_num(hz$horizondepth_minval); hz$bot <- .nsd_num(hz$horizondepth_maxval)
  no_depth <- tapply(is.na(hz$top), hz$sd_soil_id, sum)
  hz <- hz[!is.na(hz$top) & !is.na(hz$sa_site_id), , drop = FALSE]
  n <- nrow(hz)
  hid <- hz$sd_horizon_id
  prov <- list()
  note <- function(attr, rows, text) {
    rows <- which(rows)
    if (length(rows)) prov[[length(prov) + 1L]] <<- data.frame(
      hid = hid[rows], attribute = attr, source = "measured", confidence = 1,
      notes = if (length(text) == 1L) text else text[rows], stringsAsFactors = FALSE)
  }
  H <- data.frame(hid = hid, sd_soil_id = hz$sd_soil_id, top_cm = hz$top,
                  bottom_cm = hz$bot, stringsAsFactors = FALSE)
  open_bottom <- col(hz, "horizondepth_maxop") %in% ">"
  note("bottom_cm", open_bottom & !is.na(H$bottom_cm), "NSD: the horizon continues below this depth (>)")

  # designations
  ds <- .nsd_designation(col(hz, "designation"), col(hz, "designation_fao"), old_notation,
                         thick = hz$bot - hz$top, soil = hz$sd_soil_id, top = hz$top)
  H$designation <- ds$designation
  note("designation", !is.na(ds$designation), ds$note)

  # boundaries
  bd <- .nsd_lookup(.NSD_BOUNDARY, dec("sd_horizon", "lowerboundarydistinctness", col(hz, "lowerboundarydistinctness")))
  H$boundary_distinctness <- bd
  note("boundary_distinctness", !is.na(bd), "NSD lowerboundarydistinctness (Milne et al. 1995 classes in FAO words)")
  bt <- .nsd_lookup(.NSD_TOPOGRAPHY, col(hz, "lowerboundaryshape"))
  H$boundary_topography <- bt
  note("boundary_topography", !is.na(bt), "NSD lowerboundaryshape")

  # texture
  tv <- dec("sd_horizon", "texture_value", col(hz, "texture_value"))
  code_tv <- !is.na(tv) & tv %in% names(.NSD_TEXTURE)
  tv[code_tv] <- .NSD_TEXTURE[tv[code_tv]]
  tv <- tolower(tv)
  tq <- .nsd_lookup(.NSD_TEXTURE_QUAL, col(hz, "texture_qualifier"))
  sandy <- !is.na(tv) & grepl("sand", tv) & !grepl("^(fine|medium|coarse|very)", tv)
  tex <- ifelse(!is.na(tq) & sandy, paste(tq, tv), tv)
  mod <- tolower(col(hz, "texture_legacy_texturemodifier"))
  peaty <- !is.na(mod) & grepl("peaty", mod) & !is.na(tex) & !grepl("peat", tex)
  tex[peaty] <- paste("peaty", tex[peaty])
  H$field_texture <- tex
  note("field_texture", !is.na(tex), ifelse(peaty, "NSD texture_value with the texture modifier peaty",
                                             "NSD texture_value (and texture_qualifier for the sand grade)"))

  # consistence
  wet_state <- tolower(dec("sd_horizon", "soilwaterstate", col(hz, "soilwaterstate")))
  dry <- wet_state %in% "dry"
  st_raw <- dec("sd_horizon", "consistence_strength", col(hz, "consistence_strength"))
  st_leg <- dec("sd_horizon", "consistence_legacy_strengthaggregate", col(hz, "consistence_legacy_strengthaggregate"))
  st_raw <- ifelse(is.na(st_raw), st_leg, st_raw)
  st <- .nsd_lookup(.NSD_STRENGTH, st_raw)
  H$strength_moist <- ifelse(dry, NA_character_, st)
  H$strength_dry <- ifelse(dry, st, NA_character_)
  note("strength_moist", !is.na(H$strength_moist), "NSD consistence_strength (Milne et al. 1995 strength classes)")
  note("strength_dry", !is.na(H$strength_dry), "NSD consistence_strength, horizon described dry")
  fl <- .nsd_lookup(.NSD_FAILURE, col(hz, "consistence_failure"))
  fl2 <- .nsd_lookup(.NSD_FAILURE, st_raw)              # friable written as a strength
  fl <- ifelse(is.na(fl), fl2, fl)
  H$consistence_moist <- fl
  note("consistence_moist", !is.na(fl), "NSD consistence_failure (failure class)")
  sk <- .nsd_lookup(.NSD_STICKY, col(hz, "consistence_stickiness"))
  H$stickiness <- sk; note("stickiness", !is.na(sk), "NSD consistence_stickiness")
  pl <- .nsd_lookup(.NSD_PLASTIC, col(hz, "consistence_plasticity"))
  H$plasticity <- pl; note("plasticity", !is.na(pl), "NSD consistence_plasticity")
  cm <- .nsd_lookup(.NSD_CEMENT, col(hz, "consistence_induration"))
  H$cementation_class <- cm; note("cementation_class", !is.na(cm), "NSD consistence_induration")

  # matrix colour (moist): the first recorded; co-dominant colours noted
  mc <- rd("sd_horizon_matrixcolour.csv")
  H$munsell_hue_moist <- NA_character_; H$munsell_value_moist <- NA_real_; H$munsell_chroma_moist <- NA_real_
  if (!is.null(mc)) {
    first <- mc[!duplicated(mc$sd_horizon_id), , drop = FALSE]
    i <- match(hid, first$sd_horizon_id)
    hue <- toupper(gsub("\\s+", "", first$hue[i]))
    two <- !is.na(hue) & grepl("[/-]", hue)
    hue <- sub("[/-].*$", "", hue)
    hue[!grepl("^([0-9.]+(R|YR|Y|GY|G|BG|B|PB|P|RP)|N)$", hue)] <- NA_character_
    H$munsell_hue_moist <- hue
    H$munsell_value_moist <- .nsd_munsell_num(first$value[i])
    H$munsell_chroma_moist <- .nsd_munsell_num(first$chroma[i])
    H$munsell_chroma_moist[hue %in% "N"] <- 0
    nmc <- table(mc$sd_horizon_id)
    others <- as.integer(nmc[hid]); others[is.na(others)] <- 0L
    txt <- ifelse(others > 1L, sprintf("NSD sd_horizon_matrixcolour, moist (the first of %d co-dominant matrix colours)", others),
                  "NSD sd_horizon_matrixcolour, moist (the New Zealand convention)")
    txt <- ifelse(two, paste0(txt, "; a hue between two (", first$hue[i], ") read as the first"), txt)
    note("munsell_hue_moist", !is.na(H$munsell_hue_moist), txt)
    note("munsell_value_moist", !is.na(H$munsell_value_moist), txt)
    note("munsell_chroma_moist", !is.na(H$munsell_chroma_moist), txt)
  }

  # mottles
  mo <- rd("em_mottle.csv")
  H$redoximorphic_features_pct <- NA_real_
  if (!is.null(mo)) {
    ab <- .nsd_lookup(.NSD_MOTTLE_PCT, dec("em_mottle", "abundance", mo$abundance))
    s <- tapply(ab, mo$sd_horizon_id, function(v) if (all(is.na(v))) NA_real_ else min(100, sum(v, na.rm = TRUE)))
    H$redoximorphic_features_pct <- as.numeric(s[hid])
    note("redoximorphic_features_pct", !is.na(H$redoximorphic_features_pct),
         "NSD em_mottle abundance (Milne et al. 1995 p. 97 classes at their middle; several mottle records added)")
    con <- .nsd_num(mo$contrast)
    cmax <- tapply(con, mo$sd_horizon_id, function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE))
    H$mottle_contrast <- c("1" = "faint", "2" = "distinct", "3" = "prominent")[as.character(cmax[hid])]
    note("mottle_contrast", !is.na(H$mottle_contrast), "NSD em_mottle contrast (the strongest)")
    clr <- ifelse(is.na(mo$matrixcolour_munsell_hue %||% NA), NA_character_,
                  paste0(mo$matrixcolour_munsell_hue, " ", mo$matrixcolour_munsell_value, "/", mo$matrixcolour_munsell_chroma))
    cl2 <- tapply(clr, mo$sd_horizon_id, function(v) { v <- v[!is.na(v)]; if (length(v)) paste(v, collapse = "; ") else NA_character_ })
    H$mottle_colors <- as.character(cl2[hid])
    note("mottle_colors", !is.na(H$mottle_colors), "NSD em_mottle colour")
    if (isTRUE(absent_unless_recorded)) {
      described <- !is.na(H$munsell_value_moist)
      gley <- grepl("g|r|\\(f\\)|^([0-9]+|[IVX]+)?[a-z]*G", ifelse(is.na(H$designation), "", H$designation))
      zero <- is.na(H$redoximorphic_features_pct) & described & !gley & !hid %in% mo$sd_horizon_id
      H$redoximorphic_features_pct[zero] <- 0
      note("redoximorphic_features_pct", zero,
           "no mottle record for this described horizon (the survey records mottles): read as none (absent_unless_recorded)")
    }
  }

  # structure: the first ped record (em_ped_id 131-0-...), and the second
  pe <- rd("em_ped.csv")
  if (!is.null(pe)) {
    k <- suppressWarnings(as.integer(sub("^[0-9]+-([0-9]+)-.*$", "\\1", pe$em_ped_id)))
    pe <- pe[order(pe$sd_horizon_id, ifelse(is.na(k), 99L, k)), , drop = FALSE]
    p1 <- pe[!duplicated(pe$sd_horizon_id), , drop = FALSE]
    rest <- pe[duplicated(pe$sd_horizon_id), , drop = FALSE]
    p2 <- rest[!duplicated(rest$sd_horizon_id), , drop = FALSE]
    i <- match(hid, p1$sd_horizon_id)
    gr_raw <- tolower(trimws(p1$legacy_grade[i]))
    gr <- .nsd_lookup(.NSD_GRADE, gr_raw)
    shape <- .nsd_lookup(.NSD_SHAPE, p1$type[i])
    shape <- ifelse(!is.na(gr_raw) & grepl("^mas+ive", gr_raw), "massive",
             ifelse(!is.na(gr_raw) & grepl("single grain", gr_raw), "single grain", shape))
    # nut: one of two shapes, not known which; structure_type stays empty
    one_of <- .nsd_lookup(.NSD_SHAPE_ONE_OF, p1$type[i])
    one_of[!is.na(shape)] <- NA_character_
    H$structure_grade <- gr; H$structure_type <- shape; H$structure_type_one_of <- one_of
    note("structure_grade", !is.na(gr), "NSD em_ped legacy_grade (pedality degree)")
    note("structure_type", !is.na(shape), "NSD em_ped type")
    note("structure_type_one_of", !is.na(one_of),
         "NSD em_ped type nut (an older term): granular or polyhedral, Handbook 2024 Note 12.1 (one of the two, not known which; structure_type left empty)")
    sz <- dec("em_ped", "geometry_size", p1$geometry_size[i])
    H$structure_size <- .nsd_lookup(.NSD_PED_SIZE, p1$geometry_size[i])
    H$structure_size[is.na(H$structure_size)] <- .nsd_lookup(c(fine = "fine", medium = "medium", coarse = "coarse",
                                                                "very fine" = "very fine", "very coarse" = "very coarse"),
                                                              sz)[is.na(H$structure_size)]
    note("structure_size", !is.na(H$structure_size), "NSD em_ped geometry_size")
    H$ped_size_max_mm <- .nsd_lookup(.NSD_PED_SIZE_MM, p1$geometry_size[i])
    H$ped_size_max_mm[is.na(H$ped_size_max_mm)] <- .nsd_lookup(.NSD_PED_SIZE_MM, sz)[is.na(H$ped_size_max_mm)]
    note("ped_size_max_mm", !is.na(H$ped_size_max_mm),
         "upper limit of the NSD em_ped size class (Milne et al. 1995: very fine 2-6, fine 6-10, medium 10-20, coarse 20-60 mm)")
    j <- match(hid, p2$sd_horizon_id)
    s2 <- .nsd_lookup(.NSD_SHAPE, p2$type[j])             # a nut is not one shape: not read
    H$structure_type_secondary <- s2
    note("structure_type_secondary", !is.na(s2), "NSD em_ped, the second ped record of the horizon")
  }

  # coatings: clay coats are clay films; the darkest coat colours the coatings
  sf <- rd("em_surfacefeature.csv")
  if (!is.null(sf)) {
    ty <- toupper(trimws(as.character(sf$type)))
    am <- .nsd_lookup(.NSD_AMOUNT, sf$abundance)
    cc <- ty %in% "CC"
    nocoat <- is.na(ty) & am %in% "none"
    rank <- c(none = 0, "very few" = 1, few = 2, common = 3, many = 4)
    best <- function(rows) {
      v <- tapply(am[rows], sf$sd_horizon_id[rows], function(a) {
        a <- a[!is.na(a)]; if (!length(a)) "few" else a[which.max(rank[a])] })
      v
    }
    films <- best(cc)
    H$clay_films_amount <- as.character(films[hid])
    none <- hid %in% sf$sd_horizon_id[nocoat] & is.na(H$clay_films_amount)
    H$clay_films_amount[none] <- "none"
    note("clay_films_amount", !is.na(H$clay_films_amount) & !none, "NSD em_surfacefeature clay coats (CC) abundance; with no abundance, few")
    note("clay_films_amount", none, "NSD em_surfacefeature: no coatings recorded as such")
    dist <- c("1" = "faint", "2" = "distinct", "3" = "prominent")[as.character(.nsd_num(sf$distinctness))]
    fs <- tapply(dist[cc], sf$sd_horizon_id[cc], function(a) { a <- a[!is.na(a)]; if (length(a)) a[1] else NA_character_ })
    H$clay_films_strength <- as.character(fs[hid])
    note("clay_films_strength", !is.na(H$clay_films_strength), "NSD em_surfacefeature clay coats distinctness")
    val <- .nsd_munsell_num(sf$matrixcolour_munsell_value %||% rep(NA, nrow(sf)))
    coat <- !is.na(ty) & ty %in% c("CC", "OG", "HUMUS-CLAY", "ZC") & !am %in% "none"
    cv <- tapply(val[coat], sf$sd_horizon_id[coat], function(v) if (all(is.na(v))) NA_real_ else min(v, na.rm = TRUE))
    H$coatings_value_moist <- as.numeric(cv[hid])
    note("coatings_value_moist", !is.na(H$coatings_value_moist), "NSD em_surfacefeature: the darkest coat (moist value)")
  }

  # pans
  pn <- rd("em_pan.csv")
  if (!is.null(pn)) {
    ty <- tolower(trimws(as.character(pn$type)))
    j <- match(hid, pn$sd_horizon_id)
    t <- ty[j]
    thick <- H$bottom_cm - H$top_cm
    pt <- ifelse(t %in% c("ironpan", "fe/mn pan", "fe"),
                 ifelse(t %in% "ironpan" & !is.na(thick) & thick <= 1, "thin ironpan (placic)", "ironpan"),
          ifelse(t %in% "humuspan", "humus pan", ifelse(t %in% "fragipan", "fragipan",
          ifelse(t %in% "claypan", "claypan", NA_character_))))
    H$pan_type <- pt
    note("pan_type", !is.na(pt), ifelse(pt %in% "thin ironpan (placic)",
         "NSD em_pan ironpan in a horizon 10 mm or less thick: a placic horizon (a thin iron pan, NZSC Diagnostics)",
         "NSD em_pan type"))
  }

  # roots
  ro <- rd("em_root.csv")
  if (!is.null(ro)) {
    j <- match(hid, ro$sd_horizon_id)
    ra <- .nsd_lookup(.NSD_ROOTS, ro$abundance[j])
    H$roots_abundance <- ra
    note("roots_abundance", !is.na(ra), "NSD em_root abundance")
  }

  # laboratory
  smp <- rd("sa_sample.csv"); ob <- rd("ob_observation.csv")
  lab <- .nsd_lab_values(ob)
  if (!is.null(smp) && !is.null(lab)) {
    hosts <- .nsd_sample_hosts(smp, data.frame(sd_horizon_id = hid, sa_site_id = hz$sa_site_id,
                                               top = hz$top, bot = hz$bot, stringsAsFactors = FALSE))
    lab$host <- hosts$host[match(lab$sample, hosts$sample)]
    w <- hosts$bot - hosts$top
    lab$w <- w[match(lab$sample, hosts$sample)]
    lab$w[!is.finite(lab$w) | lab$w <= 0] <- 1
    # a horizon with no sample of its own: the samples that overlap it
    own <- unique(lab$host[!is.na(lab$host)])
    lone <- which(!hid %in% own)
    ov <- NULL
    if (length(lone)) {
      ss <- hosts[is.finite(hosts$top) & is.finite(hosts$bot), , drop = FALSE]
      rows <- lapply(lone, function(r) {
        k <- which(ss$site == hz$sa_site_id[r])
        if (!length(k)) return(NULL)
        o <- pmin(ss$bot[k], hz$bot[r]) - pmax(ss$top[k], hz$top[r])
        k <- k[is.finite(o) & o > 0]; o <- o[is.finite(o) & o > 0]
        if (!length(k)) return(NULL)
        data.frame(sample = ss$sample[k], hid = hid[r], w2 = o, stringsAsFactors = FALSE)
      })
      ov <- do.call(rbind, rows)
    }
    for (a in names(.NSD_LAB)) {
      spec <- .NSD_LAB[[a]]
      vals <- rep(NA_real_, n); txt <- rep(NA_character_, n)
      for (p in spec[[1L]]) {
        L <- lab[lab$prop == p & !is.na(lab$host), , drop = FALSE]
        if (nrow(L)) {
          m <- tapply(L$v * L$w, L$host, sum) / tapply(L$w, L$host, sum)
          cnt <- tapply(L$v, L$host, length)
          v <- as.numeric(m[hid]) * spec[[2L]]
          fill <- is.na(vals) & is.finite(v)
          vals[fill] <- v[fill]
          txt[fill] <- sprintf("NSD %s (%s)%s", p, spec[[3L]],
                               ifelse(cnt[hid][fill] > 1, sprintf(", mean of %d samples by thickness", cnt[hid][fill]), ""))
        }
        if (!is.null(ov) && nrow(ov)) {
          L2 <- merge(ov, lab[lab$prop == p, c("sample", "v"), drop = FALSE], by = "sample")
          if (nrow(L2)) {
            m <- tapply(L2$v * L2$w2, L2$hid, sum) / tapply(L2$w2, L2$hid, sum)
            v <- as.numeric(m[hid]) * spec[[2L]]
            fill <- is.na(vals) & is.finite(v)
            vals[fill] <- v[fill]
            txt[fill] <- sprintf("NSD %s (%s) of the sample(s) overlapping this horizon, weighted by the overlap", p, spec[[3L]])
          }
        }
      }
      H[[a]] <- vals
      note(a, !is.na(vals), txt)
    }
    # base saturation from the bases when not recorded; ECEC, Al saturation
    sb <- H$ca_cmol + H$mg_cmol + H$k_cmol + H$na_cmol
    fill <- is.na(H$bs_pct) & is.finite(sb) & is.finite(H$cec_cmol) & H$cec_cmol > 0
    H$bs_pct[fill] <- pmin(100, 100 * sb[fill] / H$cec_cmol[fill])
    note("bs_pct", fill, "calculated: 100 x exchangeable bases / CEC (NSD)")
    ec <- sb + H$al_cmol
    H$ecec_cmol <- ifelse(is.finite(ec), ec, NA_real_)
    note("ecec_cmol", is.finite(ec), "calculated: exchangeable bases + KCl-extractable Al (NSD)")
    al <- ifelse(is.finite(H$al_cmol) & is.finite(H$ecec_cmol) & H$ecec_cmol > 0,
                 100 * H$al_cmol / H$ecec_cmol, NA_real_)
    H$al_sat_pct <- al
    note("al_sat_pct", is.finite(al), "calculated: 100 x KCl-extractable Al / ECEC (NSD)")
  }

  # the soil's PARENT ROCK note (the site's parent material), taken down to
  # the layers whose origin is not recorded (the NSD exports record none per
  # horizon); a sequence only where its parts match the lithologic groups
  snote <- rd("sd_soil_note.csv")
  pm <- if (!is.null(snote)) {
    k <- grepl("^\\s*PARENT (ROCK|MATERIAL)\\s*:", snote$note, ignore.case = TRUE)
    stats::setNames(trimws(sub("^\\s*PARENT (ROCK|MATERIAL)\\s*:\\s*", "", snote$note[k], ignore.case = TRUE)),
                    snote$sd_soil_id[k])
  } else character(0)
  lo <- .nsd_layer_origin(H$layer_origin %||% rep(NA_character_, n), H$sd_soil_id, pm,
                          designation = H$designation, top = H$top_cm)
  H$layer_origin <- lo$value
  note("layer_origin", lo$filled, lo$how)

  prov <- if (length(prov)) do.call(rbind, prov) else NULL

  # site tables
  land <- rd("lc_landscape.csv"); use <- rd("lc_landuse.csv")
  clim <- rd("lc_climate.csv")
  by_soil <- split(seq_len(n), H$sd_soil_id)
  out <- list()
  for (r in seq_len(nrow(soil))) {
    so <- soil[r, , drop = FALSE]
    st <- sites[match(so$sa_site_id, sites$sa_site_id), , drop = FALSE]
    id <- as.character(st$identifier %||% so$sd_soil_id)
    if (is.na(id)) id <- as.character(so$sd_soil_id)
    rows <- by_soil[[as.character(so$sd_soil_id)]]
    h <- if (length(rows)) H[rows, , drop = FALSE] else NULL
    site <- list(id = id, country = "NZ", designation_system = "nzsc",
                 nsdr_identifier = as.character(st$nsdridentifier %||% NA),
                 survey = survey_name, survey_code = survey_code)
    x <- .nsd_num(st$location_geometry_x); y <- .nsd_num(st$location_geometry_y)
    if (is.finite(x) && is.finite(y) && identical(as.character(st$location_geometry_srid), "2193")) {
      ll <- .nsd_nztm_to_lonlat(x, y); site$lat <- ll$lat; site$lon <- ll$lon
    }
    if (!is.na(so$date %||% NA)) site$date <- as.character(so$date)
    if (!is.na(st$region %||% NA)) site$province <- as.character(st$region)
    dr <- tolower(trimws(sub("\\.$", "", as.character(so$profiledrainage %||% NA))))
    if (!is.na(dr) && dr %in% names(.NSD_DRAINAGE)) site$drainage_class <- .NSD_DRAINAGE[[dr]]
    if (!is.na(pm[as.character(so$sd_soil_id)])) site$parent_material <- unname(pm[as.character(so$sd_soil_id)])
    if (!is.null(land)) {
      l <- land[match(so$sa_site_id, land$sa_site_id), , drop = FALSE]
      if (!is.na(l$landform %||% NA)) site$landform <- as.character(l$landform)
    }
    if (!is.null(use)) {
      u <- use[match(so$sa_site_id, use$sa_site_id), , drop = FALSE]
      dd <- tolower(as.character(u$drained %||% NA))
      if (dd %in% c("y", "yes")) site$artificially_drained <- TRUE
      if (dd %in% c("n", "no")) site$artificially_drained <- FALSE
    }
    if (!is.null(clim)) {
      cc <- clim[match(so$sa_site_id, clim$sa_site_id), , drop = FALSE]
      t <- .nsd_num(cc$meantemperatureannual_val %||% NA)
      if (is.finite(t) && t > 0) site$mean_annual_air_temp_C <- t
    }
    ref <- .nsd_nzsc_reference(so$classifier_nzsc %||% NA)
    site$reference_nzsc <- ref$code
    site$reference_nzsc_name <- ref$name
    site$reference_nzsc_order <- ref$order
    site$reference_nzsc_group <- ref$group
    site$reference_nzsc_subgroup <- ref$subgroup
    site$reference_nzsc_raw <- as.character(so$classifier_nzsc %||% NA)
    if (!is.na(ref$note)) site$reference_nzsc_note <- ref$note
    if (!is.na(so$classifier_nzsc_alt %||% NA)) site$reference_nzsc_alt <- as.character(so$classifier_nzsc_alt)
    if (!is.na(so$classifier_nz_genetic %||% NA)) site$reference_nz_genetic <- as.character(so$classifier_nz_genetic)
    if (!is.na(so$classifier_nz_series %||% NA)) site$reference_nz_series <- as.character(so$classifier_nz_series)
    us <- .nsd_usda_reference(so$classifier_usda %||% NA)
    site$reference_usda <- us$order
    site$reference_usda_grtgroup <- us$grtgroup
    site$reference_usda_subgroup <- us$subgroup
    if (!is.na(us$edition)) site$reference_usda_edition <- us$edition
    site$reference_usda_raw <- as.character(so$classifier_usda %||% NA)
    site$reference_source <- "NSD (Manaaki Whenua - Landcare Research), the surveyor's classification"
    site$source <- "Manaaki Whenua NSD"
    site$dataset <- paste0("NSD survey ", survey_name, if (!is.na(survey_code)) sprintf(" [%s]", survey_code) else "")
    if (!is.null(accessed)) site$accessed <- format(as.Date(accessed), "%Y-%m-%d")
    site$licence <- .NSD_LICENCE
    site$citation <- nsd_citation(accessed)
    site$attribution <- sprintf("Soil profile %s of the New Zealand National Soils Database (survey %s), Manaaki Whenua - Landcare Research.",
                                id, survey_name)
    notes <- character(0)
    nd <- no_depth[as.character(so$sd_soil_id)]
    if (!is.na(nd) && nd > 0) notes <- c(notes, sprintf("%d NSD horizon record(s) with no depth left out.", nd))
    pv <- NULL
    if (!is.null(h) && nrow(h)) {
      ord <- order(h$top_cm, h$bottom_cm)
      h <- h[ord, , drop = FALSE]
      shift <- -min(h$top_cm, na.rm = TRUE)
      if (is.finite(shift) && shift != 0) {
        h$top_cm <- h$top_cm + shift; h$bottom_cm <- h$bottom_cm + shift
        notes <- c(notes, sprintf("Depths shifted by %g cm so that the profile starts at 0 (organic horizons recorded above the mineral surface).", shift))
      }
      if (!is.null(prov)) {
        pv <- prov[prov$hid %in% h$hid, , drop = FALSE]
        pv$horizon_idx <- match(pv$hid, h$hid)
        pv <- pv[, c("horizon_idx", "attribute", "source", "confidence", "notes")]
      }
      h$hid <- NULL; h$sd_soil_id <- NULL
      h <- ensure_horizon_schema(data.table::as.data.table(h))
    } else {
      h <- NULL
      notes <- c(notes, "The NSD has no horizon with depths for this profile.")
    }
    if (length(notes)) site$depth_note <- paste(notes, collapse = " ")
    p <- tryCatch(PedonRecord$new(site = site, horizons = h,
                                  provenance = if (is.null(pv) || !nrow(pv)) NULL else data.table::as.data.table(pv)),
                  error = function(e) NULL)
    if (!is.null(p)) out[[id]] <- p
  }
  out
}

#' Read New Zealand National Soils Database survey exports
#'
#' Reads the survey exports of the New Zealand National Soils Database (NSD,
#' Manaaki Whenua - Landcare Research), as the National Soil Data Repository
#' (NSDR) serves them (one zip of relational CSV tables per survey), into
#' \code{\link{PedonRecord}} objects. The tables are read in memory from the
#' zips (or from unzipped folders); nothing is written to disk.
#'
#' Each pedon carries the field description (designations, depths, boundary,
#' moist Munsell colour, field texture, strength, failure, stickiness,
#' plasticity, cementation, mottles, structure, clay coats and the colour of
#' coatings, pans, roots) and the laboratory results of its samples (carbon,
#' nitrogen, pH in water, KCl, CaCl2 and NaF, P retention, CEC and
#' exchangeable cations, base saturation, KCl-extractable Al, oxalate,
#' dithionite and pyrophosphate Fe and Al, particle size, bulk density,
#' volcanic glass in the sand, stones, CaCO3, EC of a 1:5 extract, water at
#' 1500 kPa, COLE), with the NSD table and column of every value in the
#' provenance. Codes are decoded through the survey's \code{cl_concept}
#' table. The site holds the surveyor's classes: \code{reference_nzsc} (the
#' NZSC code, e.g. \code{"MOT"}, recoded to the 4th edition where the 4th
#' edition renamed it), \code{reference_nzsc_name}, the order, group and
#' subgroup codes (\code{reference_nzsc_order}, \code{_group},
#' \code{_subgroup}), the record as written (\code{reference_nzsc_raw}),
#' \code{reference_usda} (the Soil Taxonomy order of the recorded name),
#' \code{reference_usda_grtgroup}, \code{reference_usda_subgroup}, the NZ
#' Genetic class and series; and the drainage class, the parent material
#' (the soil note "PARENT ROCK"), the landform, whether the land is drained,
#' the mean annual air temperature, latitude and longitude (from NZTM2000),
#' the licence and the citation (\code{\link{nsd_citation}}). The header of
#' \code{R/nsd.R} lists every reading.
#'
#' Designations: the NSD gives many horizons a designation in the current
#' notation (\code{designation_fao}), which is read (an FAO H master as the
#' New Zealand O); the others keep the surveyor's designation, often in the
#' older New Zealand notation (Taylor and Pohlen 1962). With
#' \code{old_notation = "translate"}, (B) is read as Bw, the suffixes fe, ca
#' and cs as s, k and y, an upper-case G suffix as g, and digits written
#' between the master and its suffixes are moved to the end (B2g as Bg2).
#' A2 is read as E, with its subdivisions and suffixes (A21 as E1, A2g as
#' Eg). The old master G (a gley horizon) is read as Bg when a B horizon
#' lies below it in the profile, otherwise as Cg (GC as BCg or Cg); the old
#' master D (the substratum) as C, its prefixes and suffixes kept (BD as BC,
#' DG as Cg) except its old suffixes r and n, which are not read (Dr and Dn
#' as C; the NSD's own conversions give them as Cu, C, Bt or Cm). A cemented sesquioxidic B written Bms is read as Bfm (a placic
#' horizon) when 10 mm or less thick and as Bsm (an ortstein-pan) when
#' thicker, as the NZSC Diagnostics define them. The provenance keeps the
#' recorded designation and what it was read as.
#'
#' Structure: the older ped term nut (not a shape of Milne et al. 1995) is
#' granular or polyhedral, one of the two (NZ Soil Description Handbook
#' 2024, Note 12.1). It is kept in \code{structure_type_one_of} with
#' \code{structure_type} empty: \code{\link{classify_nzsc4}} reads a
#' criterion under each shape, and the other keys read the shape as not
#' recorded. The older strength words moderately weak, moderately firm and
#' moderately strong (of the Taylor and Pohlen 1962 scale) are not read.
#'
#' Parent material: the soil note PARENT ROCK is also the
#' \code{layer_origin} of each horizon whose origin is not recorded (the
#' NSD exports record none per horizon). A note naming a sequence of
#' materials ("loess over outwash gravels") is split by the profile's
#' lithologic groups (2C, IIC) when its parts are as many; otherwise it
#' stays the site's \code{parent_material} only.
#'
#' The NSD records no field reactive-aluminium (NaF) test, Childs' test,
#' sensitivity, fluidity, packing or Ksat, which the NZSC key reads; the
#' laboratory pH in NaF is kept in \code{ph_naf}.
#'
#' @param path One or more NSD survey zips, folders holding the unzipped
#'   CSV tables of a survey, or folders of either.
#' @param absent_unless_recorded If \code{TRUE} (default), a described
#'   horizon (one with a matrix colour) with no mottle record, in a survey
#'   that records mottles, has none, unless its designation marks gleying.
#' @param old_notation \code{"translate"} (default) or \code{"asis"}: how
#'   designations in the older New Zealand notation are read (see Details).
#' @param classified_only If \code{TRUE}, only profiles with an NZSC class.
#' @param accessed Optional date the exports were downloaded, kept in the
#'   site and the citation.
#' @param verbose Print the number of profiles read per survey.
#' @return A named list of \code{\link{PedonRecord}} objects (names are the
#'   NSD site identifiers).
#' @references Manaaki Whenua - Landcare Research (2020). National Soils
#'   Database (NSD). \doi{10.26060/95m4-cz25}. Milne, J.D.G., Clayden, B.,
#'   Singleton, P.L. and Wilson, A.D. (1995). Soil Description Handbook.
#'   Manaaki Whenua Press.
#' @seealso \code{\link{benchmark_nsd}}, \code{\link{classify_nzsc4}}.
#' @examples
#' \dontrun{
#' peds <- read_nsd_pedons("path/to/nsd/surveys")
#' b <- benchmark_nsd(peds, level = "order")
#' b$accuracy
#' }
#' @export
read_nsd_pedons <- function(path, absent_unless_recorded = TRUE,
                            old_notation = c("translate", "asis"),
                            classified_only = FALSE, accessed = NULL, verbose = FALSE) {
  old_notation <- match.arg(old_notation)
  src <- .nsd_sources(path)
  out <- list()
  for (s in src) {
    p <- .nsd_survey_pedons(s, absent_unless_recorded = absent_unless_recorded,
                            old_notation = old_notation, accessed = accessed)
    if (isTRUE(classified_only))
      p <- Filter(function(x) !is.na(x$site$reference_nzsc %||% NA), p)
    if (isTRUE(verbose)) cat(sprintf("[nsd] %s: %d profile(s)\n", basename(s), length(p)))
    dup <- intersect(names(out), names(p))
    if (length(dup)) names(p)[match(dup, names(p))] <- paste0(dup, "@", basename(s))
    out <- c(out, p)
  }
  out
}

#' Benchmark the New Zealand key against the National Soils Database
#'
#' Classifies NSD pedons (\code{\link{read_nsd_pedons}}) with
#' \code{\link{classify_nzsc4}} and compares the result with the surveyor's
#' NZSC class at the level asked: the order (the first letter of the code),
#' the group (the first two letters) or the subgroup (the whole code). A
#' pedon is compared at a level only when its recorded class reaches that
#' level (a code of one letter names only the order). A result is
#' provisional when a class before the assigned one at the level asked, or
#' a level above it, could not be decided (\code{$provisional_because} of
#' \code{\link{classify_nzsc4}}); \code{accuracy_decided} counts only the
#' results that are not provisional.
#'
#' @param pedons A list of \code{\link{PedonRecord}}s from
#'   \code{read_nsd_pedons}.
#' @param level \code{"order"}, \code{"group"} or \code{"subgroup"}.
#' @param cores Number of processes (\code{parallel::mclapply}; always 1 on
#'   Windows).
#' @param verbose Print a summary.
#' @return A list: \code{level}; \code{accuracy}; \code{accuracy_decided};
#'   \code{n_compared}; \code{n_decided}; \code{confusion} (reference by
#'   prediction, codes); \code{per_class_recall}; \code{predictions} (one row
#'   per pedon with a reference: the recorded and predicted codes at every
#'   level, \code{agree}, \code{provisional} and \code{provisional_because}).
#' @export
benchmark_nsd <- function(pedons, level = c("order", "group", "subgroup"), cores = 1L,
                          verbose = TRUE) {
  level <- match.arg(level)
  if (!is.list(pedons) || !length(pedons))
    stop("benchmark_nsd(): `pedons` must be a non-empty list of PedonRecord objects.", call. = FALSE)
  ref_field <- c(order = "reference_nzsc_order", group = "reference_nzsc_group",
                 subgroup = "reference_nzsc_subgroup")[[level]]
  above <- switch(level, order = "^orders:", group = "^(orders|groups):", subgroup = ".")
  one <- function(p) {
    r <- tryCatch(classify_nzsc4(p, on_missing = "silent"), error = function(e) NULL)
    code_at <- function(l) {
      if (is.null(r)) return(NA_character_)
      v <- r$levels$code[r$levels$level == l]
      if (length(v)) as.character(v[1L]) else NA_character_
    }
    pb <- if (is.null(r)) character(0) else as.character(r$provisional_because)
    data.frame(id = p$site$id %||% NA_character_,
               survey = p$site$survey %||% NA_character_,
               reference_code = p$site$reference_nzsc %||% NA_character_,
               reference = p$site[[ref_field]] %||% NA_character_,
               reference_order = p$site$reference_nzsc_order %||% NA_character_,
               predicted_order = code_at("orders"), predicted_group = code_at("groups"),
               predicted_subgroup = code_at("subgroups"),
               predicted_name = if (is.null(r)) NA_character_ else as.character(r$name),
               provisional = any(grepl(above, pb)),
               provisional_because = paste(pb, collapse = " "),
               error = is.null(r), stringsAsFactors = FALSE)
  }
  has <- vapply(pedons, function(p) !is.na(p$site[[ref_field]] %||% NA), NA)
  pedons <- pedons[has]
  if (!length(pedons)) stop("benchmark_nsd(): no pedon has a reference class at this level.", call. = FALSE)
  cores <- if (.Platform$OS.type == "windows" || !requireNamespace("parallel", quietly = TRUE)) 1L
           else max(1L, as.integer(cores))
  rows <- if (cores > 1L) parallel::mclapply(pedons, one, mc.cores = cores) else lapply(pedons, one)
  pred <- do.call(rbind, rows); rownames(pred) <- NULL
  pcol <- c(order = "predicted_order", group = "predicted_group", subgroup = "predicted_subgroup")[[level]]
  pred$predicted <- pred[[pcol]]
  pred$agree <- !is.na(pred$predicted) & pred$reference == pred$predicted
  d <- !pred$provisional
  conf <- table(reference = pred$reference, predicted = pred$predicted, useNA = "ifany")
  recall <- tapply(pred$agree, pred$reference, mean)
  per_class <- data.frame(reference = names(recall), n = as.integer(table(pred$reference)[names(recall)]),
                          recall = as.numeric(recall), stringsAsFactors = FALSE)
  out <- list(level = level, accuracy = mean(pred$agree),
              accuracy_decided = if (any(d)) mean(pred$agree[d]) else NA_real_,
              n_compared = nrow(pred), n_decided = sum(d), confusion = conf,
              per_class_recall = per_class, predictions = pred)
  if (isTRUE(verbose))
    cat(sprintf("[nsd] %s: %.1f%% agreement on %d profiles (%.1f%% on the %d decided)\n",
                level, 100 * out$accuracy, out$n_compared, 100 * out$accuracy_decided, out$n_decided))
  out
}
