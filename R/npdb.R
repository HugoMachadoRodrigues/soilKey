# =============================================================================
# v0.9.273 -- the National Pedon Database of Canada (NPDB) as a benchmark for
# the Canadian System of Soil Classification (classify_cssc3()).
#
# The NPDB is published by Agriculture and Agri-Food Canada (CanSIS) on the
# Open Government Portal under the Open Government Licence - Canada. soilKey
# does not ship it: download the pre-packaged CSV files (npdb_v3_csv.zip) and
# pass the folder that holds them to load_npdb_pedons().
# =============================================================================

#' Read the National Pedon Database of Canada
#'
#' Reads the pre-packaged CSV files of the National Pedon Database (NPDB,
#' version 3, Agriculture and Agri-Food Canada, CanSIS) into
#' \code{\link{PedonRecord}} objects, with the survey's classification under
#' the Canadian System of Soil Classification as the reference.
#'
#' The NPDB is not distributed with soilKey. It is published on the Open
#' Government Portal of Canada under the Open Government Licence - Canada
#' (dataset "National Pedon Database"); download the pre-packaged CSV files
#' and unzip them into a folder. \code{Info.csv} and \code{Horizons.csv} are
#' required; \code{Morphology.csv}, \code{Physical.csv},
#' \code{Chemical.csv} and \code{Profiles.csv} are read when present.
#'
#' What the reader does with the records:
#' \itemize{
#'   \item records are split on CR-LF only, since some fields hold a lone
#'     carriage return ("subangular\\r blocky");
#'   \item the designation is rebuilt in the Canadian notation from its parts
#'     (lithologic numeral, master, suffixes, numeric modifier: "IICk1"), and
#'     the site declares \code{designation_system = "cssc"};
#'   \item depths: organic and water horizons recorded upward from the
#'     mineral surface (top above bottom) become negative depths; a mineral
#'     horizon with its depths reversed or negative keeps |top| with no bottom
#'     when it is the deepest layer, and no depths otherwise; a missing top is
#'     taken from the layer above; every profile is shifted to begin at 0
#'     cm, and \code{site$npdb_depth_order_mismatch} flags profiles whose
#'     depth order differs from the order of their layers;
#'   \item colours: the codes ("100yr4030", "10yr2010", "025y 4020",
#'     "10.0yr4.0/3.0", "n 50") are read with hue numbers above 0 and up to
#'     10; of the two matrix colours, the one that is never darker within a
#'     profile (and lighter at least once) is read as the dry colour and the
#'     other as the moist one; when they do not separate, \code{COLOUR_1} is
#'     read as the moist colour and no dry colour is read;
#'   \item total N: the value 9 (a no-data code) and a value repeated over
#'     three or more horizons are not read; the units are told apart by the
#'     median C:N of each pedon (of its dataset when the pedon has too few
#'     values): 5 to 40 is read as percent, 0.5 to 4 as g/kg (divided by
#'     10), anything else is not read; a horizon whose C:N then lies outside
#'     3 to 60 is not read either;
#'   \item base saturation: computed from the exchangeable bases and the CEC
#'     (at most 100 \%) when they are recorded, the recorded value being kept
#'     when it agrees with it (one dataset records something else in that
#'     column);
#'   \item pH: a dataset whose pH in CaCl2 is mostly above its pH in water
#'     has the two columns swapped back;
#'   \item values outside their possible range are not read;
#'   \item the survey's order, great group and subgroup words are turned into
#'     the names of the 3rd edition (1998) where they match one
#'     (\code{reference_cssc_order}, \code{reference_cssc_great_group},
#'     \code{reference_cssc_subgroup}), including the subgroups the 1998
#'     edition merged (Solodic into Solonetzic); names of earlier editions
#'     with no 1998 equivalent are left out, and the raw words are kept in
#'     \code{reference_cssc_raw}.
#' }
#' The NPDB records no soil climate (mean annual soil temperature, moisture
#' subclass), the chemistry of the podzolic B (pyrophosphate-extractable Fe
#' and Al) or the contrast of mottles. The Chernozemic order and the Gray
#' Brown Luvisols, which ask for the climate, are undecided on these records
#' (see \code{\link{benchmark_npdb}}).
#'
#' @param dir Folder with the NPDB CSV files.
#' @param max_n Optional maximum number of pedons (a random sample).
#' @param seed Optional seed for that sample.
#' @param verbose Print progress.
#' @return A named list of \code{\link{PedonRecord}} objects (names are the
#'   NPDB \code{PEDON_ID}).
#' @references Agriculture and Agri-Food Canada (2021). National Pedon
#'   Database, version 3. Open Government Portal, Open Government Licence -
#'   Canada.
#' @examples
#' \dontrun{
#' peds <- load_npdb_pedons("path/to/npdb_v3_csv")
#' b <- benchmark_npdb(peds, level = "order")
#' b$accuracy
#' }
#' @export
load_npdb_pedons <- function(dir, max_n = NULL, seed = NULL, verbose = TRUE) {
  if (!dir.exists(dir)) stop("load_npdb_pedons(): folder not found: ", dir, call. = FALSE)
  rd <- function(name, required = FALSE) {
    f <- file.path(dir, name)
    if (!file.exists(f)) {
      if (required) stop("load_npdb_pedons(): ", name, " not found in ", dir, call. = FALSE)
      return(NULL)
    }
    d <- .npdb_read_csv(f)
    for (k in intersect(c("PEDON_ID", "LAYER_ID", "DATASET_ID"), names(d)))
      d[[k]] <- suppressWarnings(as.integer(d[[k]]))
    d
  }
  info <- rd("Info.csv", TRUE); hz <- rd("Horizons.csv", TRUE)
  mo <- rd("Morphology.csv"); ph <- rd("Physical.csv"); ch <- rd("Chemical.csv")
  pr <- rd("Profiles.csv")
  hz <- hz[!is.na(hz$PEDON_ID), , drop = FALSE]
  ids <- unique(hz$PEDON_ID[hz$PEDON_ID %in% info$PEDON_ID])
  if (!is.null(max_n) && length(ids) > max_n) {
    if (!is.null(seed)) set.seed(seed)
    ids <- sample(ids, max_n)
  }
  hz <- hz[hz$PEDON_ID %in% ids, , drop = FALSE]
  hz <- hz[order(hz$PEDON_ID, hz$LAYER_ID), , drop = FALSE]
  if (isTRUE(verbose)) cat(sprintf("[npdb] %d pedons, %d horizons\n", length(ids), nrow(hz)))

  key <- function(d) paste(d$PEDON_ID, d$LAYER_ID)
  pick <- function(d, col) {
    if (is.null(d) || !col %in% names(d)) return(rep(NA, nrow(hz)))
    d[[col]][match(key(hz), key(d))]
  }
  numv <- function(v) suppressWarnings(as.numeric(v))
  num <- function(v, lo, hi) { v <- numv(v); ifelse(!is.na(v) & (v < lo | v > hi), NA_real_, v) }
  ds <- info$DATASET_ID[match(hz$PEDON_ID, info$PEDON_ID)]

  # chemistry
  oc <- num(pick(ch, "CARB_ORG"), 0, 65)
  n_tot <- .npdb_clean_n(numv(pick(ch, "N_TOTAL")), oc, hz$PEDON_ID, ds)
  cat_ <- lapply(c(ca = "EXCH_CA", mg = "EXCH_MG", k = "EXCH_K", na = "EXCH_NA"),
                 function(cn) num(pick(ch, cn), 0, 200))
  cec <- num(pick(ch, "CEC"), 0, 300)
  bs <- .npdb_base_saturation(num(pick(ch, "BASE_SATN"), 0, 100), cat_, cec)
  phs <- .npdb_ph(num(pick(ch, "PH_H2O"), 2, 11), num(pick(ch, "PH_CACL2"), 2, 11), ds)

  # colours: moist and dry
  c1 <- .npdb_munsell(pick(mo, "COLOUR_1")); c2 <- .npdb_munsell(pick(mo, "COLOUR_2"))
  dry2 <- .npdb_colour2_is_dry(c1$value, c2$value, hz$PEDON_ID)
  moist <- list(hue = c1$hue, value = c1$value, chroma = c1$chroma)
  dry <- list(hue = rep(NA_character_, nrow(hz)), value = rep(NA_real_, nrow(hz)),
              chroma = rep(NA_real_, nrow(hz)))
  # COLOUR_2 dry: COLOUR_1 moist, COLOUR_2 dry; COLOUR_1 dry (COLOUR_2 always
  # darker): swap them
  is2 <- dry2 %in% TRUE; is1 <- dry2 %in% FALSE
  dry$hue[is2] <- c2$hue[is2]; dry$value[is2] <- c2$value[is2]; dry$chroma[is2] <- c2$chroma[is2]
  dry$hue[is1] <- c1$hue[is1]; dry$value[is1] <- c1$value[is1]; dry$chroma[is1] <- c1$chroma[is1]
  moist$hue[is1] <- c2$hue[is1]; moist$value[is1] <- c2$value[is1]; moist$chroma[is1] <- c2$chroma[is1]

  # designations
  lit <- suppressWarnings(as.integer(hz$HZN_LIT))
  roman <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")
  part <- function(v) ifelse(is.na(v), "", as.character(v))
  desig <- paste0(ifelse(!is.na(lit) & lit > 1, roman[pmin(lit, 9L)], ""),
                  part(hz$HZN_MAS), part(hz$HZN_SUF), part(hz$HZN_MOD))
  desig[!nzchar(desig)] <- NA_character_
  dep <- .npdb_depths(numv(hz$U_DEPTH), numv(hz$L_DEPTH), hz$HZN_MAS, hz$PEDON_ID, hz$LAYER_ID)

  cons <- tolower(as.character(pick(mo, "CONSIS_DRY")))
  cons[cons %in% "rigid"] <- "extremely hard"
  H <- data.frame(pid = hz$PEDON_ID, layer = hz$LAYER_ID, designation = desig,
                  top_cm = dep$top, bottom_cm = dep$bottom,
                  munsell_hue_moist = moist$hue, munsell_value_moist = moist$value,
                  munsell_chroma_moist = moist$chroma,
                  munsell_hue_dry = dry$hue, munsell_value_dry = dry$value,
                  munsell_chroma_dry = dry$chroma,
                  clay_pct = num(pick(ph, "T_CLAY"), 0, 100),
                  sand_pct = num(pick(ph, "T_SAND"), 0, 100),
                  silt_pct = num(pick(ph, "T_SILT"), 0, 100),
                  bulk_density_g_cm3 = num(pick(ph, "BULK_DEN"), 0.02, 2.6),
                  oc_pct = oc, n_total_pct = n_tot,
                  caco3_pct = num(pick(ch, "CACO3_EQ"), 0, 100),
                  ph_h2o = phs$h2o, ph_cacl2 = phs$cacl2,
                  ca_cmol = cat_$ca, mg_cmol = cat_$mg, k_cmol = num(cat_$k, 0, 50),
                  na_cmol = num(cat_$na, 0, 100), al_cmol = num(pick(ch, "EXCH_AL"), 0, 100),
                  cec_cmol = cec, bs_pct = bs,
                  structure_type = .npdb_structure(pick(mo, "S_PRM_KIND"), pick(mo, "S_PRM_MOD")),
                  rupture_resistance = cons, stringsAsFactors = FALSE)
  rules <- load_rules("cssc3")
  drain <- if (!is.null(pr)) pr$DRAINAGE[match(ids, pr$PEDON_ID)] else rep(NA, length(ids))
  by_id <- split(H, H$pid)
  out <- lapply(seq_along(ids), function(k) {
    id <- ids[k]; h <- by_id[[as.character(id)]]
    if (is.null(h) || nrow(h) == 0L) return(NULL)
    h <- h[order(h$top_cm, h$layer), , drop = FALSE]
    # the depth order of the layers with a depth against their layer order
    mismatch <- is.unsorted(h$layer[!is.na(h$top_cm)])
    shift <- -min(h$top_cm, na.rm = TRUE)
    if (is.finite(shift)) { h$top_cm <- h$top_cm + shift; h$bottom_cm <- h$bottom_cm + shift }
    h$pid <- NULL; h$layer <- NULL
    s <- info[match(id, info$PEDON_ID), , drop = FALSE]
    ref <- .npdb_reference(s$CSSC_ORDER, s$CSSC_GTGRP, s$CSSC_SBGRP, rules)
    site <- list(id = as.character(id), country = "CA",
                 lat = numv(s$DD_LAT_N), lon = numv(s$DD_LONG_N),
                 province = s$PROVINCE, dataset_id = s$DATASET_ID,
                 designation_system = "cssc", drainage_class = drain[k],
                 npdb_depth_order_mismatch = mismatch,
                 reference_cssc_order = ref$order, reference_cssc_great_group = ref$great_group,
                 reference_cssc_subgroup = ref$subgroup,
                 reference_cssc_raw = paste(s$CSSC_ORDER, s$CSSC_GTGRP, s$CSSC_SBGRP, sep = " | "))
    tryCatch(PedonRecord$new(site = site,
                             horizons = ensure_horizon_schema(data.table::as.data.table(h))),
             error = function(e) NULL)
  })
  names(out) <- as.character(ids)
  out <- Filter(Negate(is.null), out)
  if (isTRUE(verbose)) cat(sprintf("[npdb] %d pedon records\n", length(out)))
  out
}

# Read an NPDB CSV file: records end with CR-LF, and a lone CR inside a field
# is a space; the first header may carry a byte-order mark.
.npdb_read_csv <- function(f) {
  txt <- rawToChar(readBin(f, "raw", file.info(f)$size))
  Encoding(txt) <- "latin1"
  txt <- sub("^\xef\xbb\xbf", "", txt, useBytes = TRUE)
  txt <- gsub("\r\n", "\n", txt, fixed = TRUE, useBytes = TRUE)
  txt <- gsub("\r", " ", txt, fixed = TRUE, useBytes = TRUE)
  d <- utils::read.csv(text = txt, stringsAsFactors = FALSE, na.strings = c("", "NA"),
                       check.names = FALSE, strip.white = TRUE, colClasses = "character")
  names(d) <- toupper(gsub("^[^A-Za-z]+", "", names(d)))
  d
}

# Total N in percent: the no-data code 9 and a value copied over three or
# more horizons are dropped; the units come from the median C:N of the pedon
# (of the dataset when the pedon has fewer than two usable values): 5 to 40
# percent, 0.5 to 4 g/kg; a horizon whose C:N is then outside 3 to 60 is
# dropped.
.npdb_clean_n <- function(n, oc, pid, ds) {
  n[!is.na(n) & n == 9] <- NA_real_
  copied <- tapply(n, pid, function(v) { v <- v[!is.na(v)]; length(v) >= 3L && length(unique(v)) == 1L })
  n[as.character(pid) %in% names(copied)[copied %in% TRUE]] <- NA_real_
  usable <- !is.na(n) & n > 0 & !is.na(oc) & oc > 0.3 & oc < 15
  r <- ifelse(usable, oc / n, NA_real_)
  fac <- function(m) ifelse(is.na(m), NA_real_, ifelse(m >= 5 & m <= 40, 1, ifelse(m >= 0.5 & m < 4, 0.1, NA_real_)))
  by_p <- tapply(r, pid, function(v) if (sum(!is.na(v)) >= 2L) stats::median(v, na.rm = TRUE) else NA_real_)
  by_d <- tapply(r, ds, function(v) if (any(!is.na(v))) stats::median(v, na.rm = TRUE) else NA_real_)
  fp <- fac(by_p[as.character(pid)]); fd <- fac(by_d[as.character(ds)])
  f <- ifelse(!is.na(fp), fp, fd)
  n <- n * f
  ratio <- ifelse(!is.na(n) & n > 0 & !is.na(oc) & oc > 0.3, oc / n, NA_real_)
  n[!is.na(ratio) & (ratio < 3 | ratio > 60)] <- NA_real_
  n[!is.na(n) & (n < 0 | n > 5)] <- NA_real_
  as.numeric(n)
}

# Base saturation (%): from the exchangeable bases and the CEC when they are
# recorded (at most 100 %), the recorded value kept when it agrees with it
# within a ratio of 0.8 to 1.25.
.npdb_base_saturation <- function(recorded, bases, cec) {
  sb <- bases$ca + bases$mg + bases$k + bases$na
  calc <- ifelse(!is.na(sb) & !is.na(cec) & cec > 0, pmin(100, 100 * sb / cec), NA_real_)
  agree <- !is.na(recorded) & !is.na(calc) & calc > 0 & recorded / calc >= 0.8 & recorded / calc <= 1.25
  ifelse(is.na(calc), recorded, ifelse(agree, recorded, calc))
}

# pH in water and in CaCl2; a dataset whose pH in CaCl2 is above its pH in
# water on most horizons has the two swapped back.
.npdb_ph <- function(h2o, cacl2, ds) {
  both <- !is.na(h2o) & !is.na(cacl2)
  d <- tapply((cacl2 - h2o)[both], ds[both], stats::median)
  swap <- as.character(ds) %in% names(d)[d > 0]
  list(h2o = ifelse(swap, cacl2, h2o), cacl2 = ifelse(swap, h2o, cacl2))
}

# Which matrix colour is the dry one, per pedon: TRUE when COLOUR_2 is never
# darker than COLOUR_1 and lighter at least once (COLOUR_2 dry), FALSE when
# it is never lighter and darker at least once (COLOUR_1 dry), NA otherwise.
.npdb_colour2_is_dry <- function(v1, v2, pid) {
  both <- !is.na(v1) & !is.na(v2)
  verdict <- tapply(seq_along(pid), pid, function(i) {
    i <- i[both[i]]
    if (!length(i)) return(NA)
    d <- v2[i] - v1[i]
    if (all(d >= 0) && any(d > 0)) TRUE else if (all(d <= 0) && any(d < 0)) FALSE else NA
  })
  as.logical(verdict[as.character(pid)])
}

# Depths below the surface: organic and water horizons recorded upward (top
# above bottom) become negative; a mineral horizon with reversed or negative
# depths keeps |top| with no bottom when it is the deepest layer, and no
# depths otherwise; a missing top is the bottom of the layer above.
.npdb_depths <- function(u, l, master, pid, layer) {
  m <- toupper(gsub("[^A-Za-z]", "", as.character(master)))
  organic <- !is.na(m) & grepl("^[LFHOW]+$", m)
  up <- !is.na(u) & !is.na(l) & u > l
  top <- ifelse(up & organic, -u, u); bot <- ifelse(up & organic, -l, l)
  bad <- !organic & ((up) | (!is.na(u) & u < 0))
  if (any(bad)) {
    last <- stats::ave(layer, pid, FUN = max)
    deepest <- bad & layer == last
    top[deepest] <- abs(u[deepest]); bot[deepest] <- NA_real_
    top[bad & !deepest] <- NA_real_; bot[bad & !deepest] <- NA_real_
  }
  # a missing top: the bottom of the layer above (rows are in layer order)
  prev_bot <- stats::ave(bot, pid, FUN = function(b) c(NA_real_, b[-length(b)]))
  fill <- is.na(top) & !is.na(prev_bot) & !bad
  top[fill] <- prev_bot[fill]
  list(top = top, bottom = bot)
}

# Kind of structure; some datasets write the size class in the kind field
# and the kind in the next one.
.npdb_structure <- function(kind, mod) {
  k <- gsub("\\s+", " ", trimws(tolower(as.character(kind))))
  m <- gsub("\\s+", " ", trimws(tolower(as.character(mod))))
  size <- !is.na(k) & grepl("^(very )?(fine|medium|coarse)( to (very )?(fine|medium|coarse))?$", k)
  kinds <- "blocky|granular|massive|platy|single|prismatic|columnar|cloddy"
  ifelse(size, ifelse(!is.na(m) & grepl(kinds, m), m, NA_character_), k)
}

# NPDB Munsell codes: "100yr4030" (hue 10.0YR, value 4.0, chroma 3.0),
# "10yr2010" (hue 10), "5yr3020", "025y 4020", "10.0yr4.0/3.0" and neutral
# "n 50"; hue numbers above 0 and up to 10 only.
.npdb_munsell <- function(code) {
  s <- tolower(gsub("\\s+", "", as.character(code)))
  n <- length(s)
  hue <- rep(NA_character_, n); val <- rep(NA_real_, n); chr <- val
  m1 <- regmatches(s, regexec("^([0-9]{1,3})([a-z]{1,2})([0-9]{2})([0-9]{2})$", s))
  m2 <- regmatches(s, regexec("^([0-9]+\\.?[0-9]*)([a-z]{1,2})([0-9.]+)/([0-9.]+)$", s))
  m3 <- regmatches(s, regexec("^n([0-9]{2})([0-9]{2})?$", s))
  hnum <- function(txt) {
    d <- nchar(txt); v <- as.numeric(txt)
    if (d == 3L) v / 10 else if (d == 2L) { if (v == 10) 10 else v / 10 } else v
  }
  fmt <- function(h, fam) {
    if (is.na(h) || h <= 0 || h > 10) return(NA_character_)
    paste0(if (h == round(h)) format(round(h)) else format(h), fam)
  }
  for (i in seq_len(n)) {
    if (length(m1[[i]]) == 5L) {
      hue[i] <- fmt(hnum(m1[[i]][2]), toupper(m1[[i]][3]))
      val[i] <- as.numeric(m1[[i]][4]) / 10; chr[i] <- as.numeric(m1[[i]][5]) / 10
    } else if (length(m2[[i]]) == 5L) {
      hue[i] <- fmt(as.numeric(m2[[i]][2]), toupper(m2[[i]][3]))
      val[i] <- as.numeric(m2[[i]][4]); chr[i] <- as.numeric(m2[[i]][5])
    } else if (length(m3[[i]]) >= 2L) {
      hue[i] <- "N"; val[i] <- as.numeric(m3[[i]][2]) / 10; chr[i] <- 0
    }
  }
  bad <- is.na(hue) | is.na(val) | val < 1 | val > 10 | is.na(chr) | chr < 0 | chr > 30
  hue[bad] <- NA_character_; val[bad] <- NA_real_; chr[bad] <- NA_real_
  list(hue = hue, value = val, chroma = chr)
}

# The survey's order, great group and subgroup words as 3rd edition names,
# or NA where they name no class of the 1998 key.
.npdb_reference <- function(order, gg, sg, rules) {
  orders <- c(brunisol = "Brunisolic", luvisol = "Luvisolic", gleysol = "Gleysolic",
              chernozem = "Chernozemic", podzol = "Podzolic", regosol = "Regosolic",
              organic = "Organic", cryosol = "Cryosolic", solonetz = "Solonetzic",
              vertisol = "Vertisolic")
  noun <- c(brunisol = "Brunisol", luvisol = "Luvisol", gleysol = "Gleysol",
            chernozem = "Chernozem", podzol = "Podzol", regosol = "Regosol",
            cryosol = "Cryosol", solonetz = "Solonetz", vertisol = "Vertisol", organic = "")
  gg_names <- unlist(lapply(rules$great_groups, function(g) vapply(g, `[[`, "", "name")))
  sg_names <- unlist(lapply(rules$subgroups, function(g) vapply(g, `[[`, "", "name")))
  title <- function(s) gsub("(^|[ -])([a-z])", "\\1\\U\\2", s, perl = TRUE)
  low <- function(v) trimws(tolower(as.character(v)))
  o <- low(order); g <- low(gg); s <- low(sg)
  ord <- if (!is.na(o) && o %in% names(orders)) orders[[o]] else NA_character_
  gname <- NA_character_
  if (!is.na(ord) && !is.na(g) && nzchar(g)) {
    alias <- c("solidized solonetz" = "solodized solonetz", "fibric" = "fibrisol")
    if (o == "regosol") alias <- c(alias, "rego" = "regosol", "regosolic" = "regosol")
    if (g %in% names(alias)) g <- alias[[g]]
    cand <- unique(c(title(g), trimws(paste(title(g), noun[[o]]))))
    hit <- cand[cand %in% gg_names]
    if (length(hit)) gname <- hit[1]
  }
  sname <- NA_character_
  if (!is.na(gname) && !is.na(s) && nzchar(s)) {
    alias <- c("gleyed cumulous" = "gleyed cumulic")
    # the subgroups merged or renamed in 1998, by great group
    if (gname == "Gray Luvisol")
      alias <- c(alias, "dark" = "dark gray", "gleyed dark" = "gleyed dark gray")
    if (o == "chernozem") alias <- c(alias, "solodic" = "solonetzic", "gleyed solodic" = "gleyed solonetzic")
    if (gname == "Regosol") alias <- c(alias, "gleyed rego" = "gleyed")
    if (o == "cryosol") alias <- c(alias, "rego" = "regosolic")
    if (s %in% names(alias)) s <- alias[[s]]
    cand <- paste(title(s), gname)
    # "Dark Gray" of the Gray Luvisols names the subgroup Dark Gray Luvisol
    if (gname == "Gray Luvisol" && grepl("dark gray", s))
      cand <- c(cand, sub("Gray Gray Luvisol", "Gray Luvisol", cand))
    hit <- cand[cand %in% sg_names]
    if (length(hit)) sname <- hit[1]
  }
  list(order = ord, great_group = gname, subgroup = sname)
}

#' Benchmark the Canadian key against the National Pedon Database
#'
#' Classifies NPDB pedons (\code{\link{load_npdb_pedons}}) with
#' \code{\link{classify_cssc3}} and compares the result with the survey's
#' classification at the level asked.
#'
#' The records hold no soil climate and no dry colour, which the key reads
#' for the Chernozemic order and its great groups (Ch 3, p. 34; Ch 5, p. 63)
#' and for the Gray Brown Luvisols; the orders named in \code{set_aside}
#' (by default the Chernozemic) are therefore reported apart, with where they
#' land, and left out of \code{accuracy}. A result is provisional when an
#' undecided class comes before the one assigned (\code{$provisional_because}
#' of \code{\link{classify_cssc3}}); \code{accuracy_decided} counts only the
#' results that are not provisional at the level asked or above.
#'
#' Pedons from \code{\link{npdb_scenario}} carry estimates as ranges; each
#' is classified at its central estimate and at every combination of the
#' ends of its ranges (and of the moisture subclasses between its two
#' estimates), and a class counts as decided only where every
#' version gives it and none is provisional (otherwise the central class is
#' reported as provisional). Run such pedons with
#' \code{set_aside = character(0)} to include the Chernozems.
#'
#' @param pedons A list of \code{\link{PedonRecord}} from
#'   \code{\link{load_npdb_pedons}}.
#' @param level \code{"order"}, \code{"great_group"} or \code{"subgroup"}.
#' @param set_aside Reference orders reported apart (\code{character(0)} for
#'   none).
#' @param cores Number of processes (\code{parallel::mclapply}; always 1
#'   on Windows).
#' @param verbose Print a summary.
#' @return A list: \code{level}; \code{accuracy} (share of agreement outside
#'   \code{set_aside}); \code{accuracy_decided}; \code{n_compared};
#'   \code{n_decided}; \code{accuracy_all} (all orders); \code{set_aside} (a
#'   table of where those pedons land); \code{confusion};
#'   \code{per_class_recall}; \code{predictions} (one row per pedon;
#'   \code{scenario} marks pedons classified at the ends of their ranges and
#'   \code{scenario_split} those whose versions disagree).
#' @export
benchmark_npdb <- function(pedons, level = c("order", "great_group", "subgroup"),
                           set_aside = "Chernozemic", cores = 1L, verbose = TRUE) {
  level <- match.arg(level)
  if (!is.list(pedons) || length(pedons) == 0L)
    stop("benchmark_npdb(): `pedons` must be a non-empty list of PedonRecord objects.",
         call. = FALSE)
  ref_field <- c(order = "reference_cssc_order", great_group = "reference_cssc_great_group",
                 subgroup = "reference_cssc_subgroup")[[level]]
  lvl <- c(order = "orders", great_group = "great_groups", subgroup = "subgroups")[[level]]
  above <- switch(level, order = "^orders:", great_group = "^(orders|great_groups):",
                  subgroup = ".")
  name_of <- function(r) {
    if (is.null(r)) NA_character_
    else if (level == "order") r$rsg_or_order %||% NA_character_
    else { v <- r$levels$name[r$levels$level == lvl]; if (length(v)) v[1] else NA_character_ }
  }
  one <- function(p) {
    # a pedon of npdb_scenario() is classified at the ends of its ranges too;
    # a class is decided only where every version gives it
    vs <- .npdb_scenario_versions(p)
    rs <- lapply(vs, function(v) tryCatch(classify_cssc3(v, on_missing = "silent"),
                                          error = function(e) NULL))
    preds <- vapply(rs, name_of, character(1))
    prov <- vapply(rs, function(r) !is.null(r) && any(grepl(above, r$provisional_because)),
                   logical(1))
    split <- length(unique(preds)) > 1L
    data.frame(id = p$site$id %||% NA_character_,
               reference_order = p$site$reference_cssc_order %||% NA_character_,
               reference = p$site[[ref_field]] %||% NA_character_,
               predicted = preds[1],
               provisional = any(prov) || split,
               error = is.null(rs[[1]]),
               scenario = length(vs) > 1L, scenario_split = split,
               stringsAsFactors = FALSE)
  }
  cores <- if (.Platform$OS.type == "windows" || !requireNamespace("parallel", quietly = TRUE)) 1L
           else max(1L, as.integer(cores))
  rows <- if (cores > 1L) parallel::mclapply(pedons, one, mc.cores = cores) else lapply(pedons, one)
  pred <- do.call(rbind, rows); rownames(pred) <- NULL
  agree <- !is.na(pred$reference) & !is.na(pred$predicted) & pred$reference == pred$predicted
  has_ref <- !is.na(pred$reference)
  aside <- has_ref & pred$reference_order %in% set_aside
  k <- has_ref & !aside
  d <- k & !pred$provisional
  conf <- table(reference = pred$reference[k], predicted = pred$predicted[k], useNA = "ifany")
  recall <- tapply(agree[k], pred$reference[k], mean)
  per_class <- data.frame(reference = names(recall), n = as.integer(table(pred$reference[k])[names(recall)]),
                          recall = as.numeric(recall), stringsAsFactors = FALSE)
  out <- list(level = level,
              accuracy = if (any(k)) mean(agree[k]) else NA_real_,
              accuracy_decided = if (any(d)) mean(agree[d]) else NA_real_,
              n_compared = sum(k), n_decided = sum(d),
              accuracy_all = if (any(has_ref)) mean(agree[has_ref]) else NA_real_,
              set_aside = if (any(aside)) table(reference = pred$reference[aside],
                                                predicted = pred$predicted[aside], useNA = "ifany")
                          else NULL,
              confusion = conf, per_class_recall = per_class, predictions = pred)
  if (isTRUE(verbose))
    cat(sprintf("[npdb] %s: %.1f%% agreement on %d pedons (%.1f%% on the %d decided)%s\n",
                level, 100 * out$accuracy, out$n_compared, 100 * out$accuracy_decided,
                out$n_decided,
                if (any(aside)) sprintf("; %d set aside (%s)", sum(aside),
                                        paste(set_aside, collapse = ", ")) else ""))
  out
}
