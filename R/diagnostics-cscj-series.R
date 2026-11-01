# =============================================================================
# v0.9.328 -- the 381 soil series groups of the Comprehensive Soil
# Classification System of Japan, First Approximation (CSCJ 2011, chapter
# VI). Each subgroup has its own ordered list (inst/rules/cscj2011/
# series-groups/), drawn from a shared vocabulary: bedrock, a gravel layer or
# a pan layer within 60 cm, the texture of the subsurface, buried peat,
# alluvial or kuroboku layers in the subsoil, the humus classes, the mineral layers and
# fibre of the peat soils, the base-rich dark surface horizon, wetness. The
# terms the SCSJ gates read are their CSCJ wrappers with parameters; the
# others are here. Three-valued.
# =============================================================================

# Bedrock (unweathered or semi-weathered) within `within` cm of the soil
# surface (Hugo, 2026-11-01: the series groups measure from the soil
# surface).
lithic_cscj <- function(pedon, within = 60) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) return(.cscj_result("lithic_cscj", NA, integer(0), list(), "designation", "VI"))
  passed <- .scsj_not(.scsj_no_rock_within(x, within))
  .cscj_result("lithic_cscj", passed, which(x$rock), list(within = within),
               if (is.na(passed)) c("designation", "bottom_cm") else character(0), "VI (series groups)")
}

# The gravel layer of each row (IV.4: 20 % or more gravel by area): 20 % or
# more in gravel_pct or coarse_fragments_pct (Hugo, 2026-11-01); a row with
# neither recorded has none (recorded when present, Hugo, 2026-11-01).
.cscj_gravel_rows <- function(x) {
  gv <- .scsj_col(x$h, "gravel_pct"); cf <- .scsj_col(x$h, "coarse_fragments_pct")
  (!is.na(gv) & gv >= 20) | (!is.na(cf) & cf >= 20)
}

# The top of a gravel layer within `within` cm of the soil surface; with
# `or_rock`, a gravel layer or the bedrock (the Lowland soils' lists and the
# Endofluvic Wet and Allophanic Andosols).
skeletal_cscj <- function(pedon, within = 60, or_rock = FALSE) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) return(.cscj_result("skeletal_cscj", NA, integer(0), list(), "gravel_pct", "VI"))
  g <- .cscj_gravel_rows(x)
  passed <- .scsj_top_in(x, g, -Inf, within, 0)
  if (isTRUE(or_rock)) passed <- .scsj_or(passed, .scsj_not(.scsj_no_rock_within(x, within)))
  .cscj_result("skeletal_cscj", passed, which(g), list(within = within, or_rock = or_rock),
               if (is.na(passed)) c("gravel_pct", "coarse_fragments_pct", "bottom_cm") else character(0),
               "VI (series groups)")
}

# The top of a pan layer (IV.4: hardness 29 mm or more by the Yamanaka-type
# tester, 10 cm or more thick) within `within` cm; hardness not recorded: no
# pan layer (Hugo, 2026-11-01).
petoric_cscj <- function(pedon, within = 60) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) return(.cscj_result("petoric_cscj", NA, integer(0), list(), "hardness_yamanaka_mm", "VI"))
  hd <- .scsj_col(x$h, "hardness_yamanaka_mm")
  pan <- !is.na(hd) & hd >= 29
  passed <- .scsj_top_in(x, pan, -Inf, within, 10)
  .cscj_result("petoric_cscj", passed, which(pan), list(within = within),
               if (is.na(passed)) c("hardness_yamanaka_mm", "bottom_cm") else character(0), "VI (series groups)")
}

# ---- texture of the subsurface ------------------------------------------------------

# The Japanese texture classes (Soil Survey Handbook, ISSS fractions) and
# their words; the undivided sandy loam (SL) is medium or coarse, not fine.
.CSCJ_TEXTURE_CLASS <- c(HC = "fine", SIC = "fine", LIC = "fine", SC = "fine", SICL = "fine", CL = "fine",
                         SCL = "fine", SIL = "medium", L = "medium", FSL = "medium", COSL = "coarse",
                         LS = "coarse", S = "coarse", SL = "not_fine")
.CSCJ_TEXTURE_WORDS <- c("\u91cd\u57f4\u571f" = "HC", "\u30b7\u30eb\u30c8\u8cea\u57f4\u571f" = "SIC",
                         "\u8efd\u57f4\u571f" = "LIC", "\u7802\u8cea\u57f4\u571f" = "SC",
                         "\u30b7\u30eb\u30c8\u8cea\u57f4\u58cc\u571f" = "SICL", "\u7802\u8cea\u57f4\u58cc\u571f" = "SCL",
                         "\u57f4\u58cc\u571f" = "CL", "\u30b7\u30eb\u30c8\u8cea\u58cc\u571f" = "SIL",
                         "\u7d30\u7802\u58cc\u571f" = "FSL", "\u7c97\u7802\u58cc\u571f" = "COSL",
                         "\u58cc\u8cea\u7802\u571f" = "LS", "\u58cc\u571f" = "L", "\u7802\u571f" = "S",
                         "\u7802\u58cc\u571f" = "SL")

# The texture class of each row: "fine", "medium", "coarse" from
# texture_class_jp; without it, clay 15 % or more is fine and less is
# "not_fine" (medium or coarse, undecided; Hugo, 2026-11-01); NA without
# either.
.cscj_texture_rows <- function(x) {
  tc <- gsub("[[:space:]\u3000]", "", as.character(.scsj_col(x$h, "texture_class_jp", NA_character_)), perl = TRUE)
  key <- toupper(tc)
  for (w in names(.CSCJ_TEXTURE_WORDS)) key <- ifelse(!is.na(tc) & tc == w, .CSCJ_TEXTURE_WORDS[[w]], key)
  cls <- unname(.CSCJ_TEXTURE_CLASS[key])
  clay <- .scsj_col(x$h, "clay_pct")
  ifelse(!is.na(cls), cls, ifelse(is.na(clay), NA_character_, ifelse(clay >= 15, "fine", "not_fine")))
}

# The rows the texture of the subsurface is read on (IV.4; Hugo,
# 2026-11-01): among the horizons of the subsurface section, those 15 cm or
# more thick with the largest overlap of 20 to 40 cm, a tie broken by the
# larger part of the section and then by the thicker horizon; none: the
# thickest in the section. A horizon without a bottom there may be one.
.cscj_texture_rep <- function(x) {
  cs <- .scsj_control(x)
  rows <- cs$rows
  if (!length(rows)) return(list(rows = integer(0), maybe = integer(0), covered = cs$covered))
  open <- is.na(x$dbot[rows])
  db <- ifelse(open, Inf, x$dbot[rows])
  th <- ifelse(open, Inf, db - x$dtop[rows])
  ov <- pmin(db, 40) - pmax(x$dtop[rows], 20)
  cand <- which(th >= 15 & ov > 0)
  ov2 <- pmin(db, cs$to) - pmax(x$dtop[rows], cs$from)
  pick <- function(k, v) k[v[k] >= max(v[k]) - 1e-9]
  if (length(cand)) {
    k <- pick(cand, ov); k <- pick(k, ov2); k <- pick(k, th)
    best <- rows[k]
  } else {
    best <- rows[pick(seq_along(rows), ov2)]
  }
  list(rows = setdiff(best, rows[open]), maybe = intersect(best, rows[open]), covered = cs$covered)
}

# The texture of the subsurface is of class `class`: "fine", "medium",
# "coarse" or "coarse_medium" (medium or coarse). The representative rows
# must agree; a section the description may not cover leaves it undecided
# where 15 cm or more could hide another horizon.
texture_cscj <- function(pedon, class = "fine") {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) return(.cscj_result("texture_cscj", NA, integer(0), list(), "texture_class_jp", "VI"))
  tr <- .cscj_texture_rows(x)
  rep <- .cscj_texture_rep(x)
  one <- function(cl) {
    if (is.na(cl)) return(NA)
    switch(class,
      fine = cl == "fine",
      medium = if (cl == "not_fine") NA else cl == "medium",
      coarse = if (cl == "not_fine") NA else cl == "coarse",
      coarse_medium = cl != "fine",
      stop("unknown texture class ", class))
  }
  rows <- c(rep$rows, rep$maybe)
  v <- vapply(tr[rows], one, NA)
  # no subsurface: a criterion on it is not met (Hugo, 2026-11-01)
  passed <- if (!length(rows)) { if (isTRUE(rep$covered)) FALSE else NA } else
            if (all(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA
  if (!is.na(passed) && !isTRUE(rep$covered) && .cscj_undescribed(x, 20, 60, longest = TRUE) >= 15) passed <- NA
  miss <- if (!is.na(passed)) character(0) else if (length(rows) && all(tr[rows] %in% "not_fine")) "texture_class_jp" else
          c("texture_class_jp", "clay_pct", "bottom_cm")
  .cscj_result("texture_cscj", passed, rows, list(class = class, texture = tr[rows]), miss, "VI (series groups); IV.4")
}

# ---- peat soils --------------------------------------------------------------------

# A mineral layer 10 cm or more thick at the surface (epi-mineralic).
epimineralic_cscj <- function(pedon) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) return(.cscj_result("epimineralic_cscj", NA, integer(0), list(), "oc_pct", "VI B"))
  r <- .scsj_horizon_within(x, x$mineral, 10, 0.5)
  .cscj_result("epimineralic_cscj", r$passed, r$rows, list(), if (is.na(r$passed)) "oc_pct" else character(0),
               "VI B, p. 30")
}

# 25 cm or more (cumulative) of mineral layers within 75 cm (endomineralic).
endomineralic_cscj <- function(pedon) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) return(.cscj_result("endomineralic_cscj", NA, integer(0), list(), "oc_pct", "VI B"))
  passed <- .scsj_at_least(.scsj_cum(x, x$mineral, 0, 75), 25)
  .cscj_result("endomineralic_cscj", passed, which(x$mineral %in% TRUE), list(),
               if (is.na(passed)) "oc_pct" else character(0), "VI B, p. 30")
}

# Fibric peat material has the largest share of the peat layers to 50 cm
# (read as the Sapric Peat soils are).
fibric_peat_cscj <- function(pedon) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) return(.cscj_result("fibric_peat_cscj", NA, integer(0), list(), "oc_pct", "VI B"))
  k <- .scsj_peat_kind(x)
  rows <- which(!(x$peat %in% FALSE) & !is.na(x$dtop) & x$dtop < 50)
  if (!length(rows)) return(.cscj_result("fibric_peat_cscj", NA, integer(0), list(), "oc_pct", "VI B"))
  closed <- !is.na(x$dbot[rows])
  th <- ifelse(closed, pmin(x$dbot[rows], 50) - pmax(x$dtop[rows], 0), 0)
  sure_peat <- x$peat[rows] %in% TRUE & closed
  share <- function(kind) sum(th[sure_peat & k[rows] %in% kind])
  unk <- sum(th[!sure_peat | is.na(k[rows])]) + .scsj_cum(x, rep(FALSE, x$n), 0, 50)$poss
  f <- share("fibric"); o <- max(share("hemic"), share("sapric"))
  passed <- if (f > o + unk) TRUE else if (f + unk <= o) FALSE else NA
  .cscj_result("fibric_peat_cscj", passed, rows, list(fibric_cm = f, other_cm = o, unknown_cm = unk),
               if (is.na(passed)) "fiber_content_rubbed_pct" else character(0), "VI B, p. 30")
}

# ---- the base-rich dark surface horizon (mollic) ------------------------------------

# The base-rich dark surface horizon (IV.1): from the soil surface down,
# surface horizons (A or undesignated, not buried, mineral) of moist value
# 3 or less and chroma 3 or less (not 3/3), 0.6 % carbon or more, base
# saturation 50 % or more without fertilising (liming recorded: undecided,
# Hugo, 2026-11-01); 10 cm or more directly on the bedrock, else 25 cm.
mollic_cscj <- function(pedon) {
  x <- .scsj_profile(.cscj_marked(pedon))
  if (is.na(x$surface)) return(.cscj_result("mollic_cscj", NA, integer(0), list(), "bs_pct", "IV.1"))
  v <- x$colour$value; c <- x$colour$chroma
  # three-valued: a value or chroma over 3 rules it out alone; 3/3 is not dark
  dark <- ifelse((!is.na(v) & v > 3) | (!is.na(c) & c > 3), FALSE,
          ifelse(is.na(v) | is.na(c), NA, !(v == 3 & c == 3)))
  oc <- ifelse(is.na(x$oc), NA, x$oc >= 0.6)
  bs <- .scsj_col(x$h, "bs_pct")
  bs50 <- ifelse(is.na(bs), NA, bs >= 50)
  if (isTRUE(.scsj_site_flag(x$site$acid_neutralising_amendment))) bs50 <- ifelse(bs50 %in% TRUE, NA, bs50)
  surf <- ifelse(x$des$buried %in% TRUE, FALSE,
          ifelse(is.na(x$role), TRUE, ifelse(x$role %in% "other", NA, grepl("^A", x$role))))
  meets <- .scsj_and(.scsj_and(.scsj_and(dark, oc), .scsj_and(bs50, x$mineral)), surf)
  rows <- x$ord[!is.na(x$dtop[x$ord]) & x$dtop[x$ord] >= -0.5]
  rt <- x$rock_top
  on_rock <- function(b) !is.na(b) && is.finite(rt %||% NA) && abs(b - rt) <= 0.5
  e <- .scsj_ends(x)
  sure <- 0; poss <- 0; prev <- 0; sure_open <- TRUE; sure_end <- NA_real_; poss_end <- NA_real_; ended <- FALSE
  for (i in rows) {
    gap <- x$dtop[i] - prev
    if (gap > 0.5) { sure_open <- FALSE; poss <- poss + gap; poss_end <- x$dtop[i] }
    m <- meets[i]
    if (m %in% FALSE) { ended <- TRUE; break }
    th <- x$thick[i]
    if (m %in% TRUE && sure_open && !is.na(th)) { sure <- sure + th; sure_end <- x$dbot[i] } else sure_open <- FALSE
    poss <- poss + (if (is.na(th)) Inf else th); poss_end <- x$dbot[i]
    if (is.na(x$dbot[i])) { ended <- TRUE; break }
    prev <- x$dbot[i]
  }
  if (!ended && !(e$on_rock || isTRUE(x$complete))) poss <- Inf
  passed <- if (sure >= 25 || (sure >= 10 && on_rock(sure_end))) TRUE else
            if (poss < 10 || (poss < 25 && !on_rock(poss_end))) FALSE else NA
  .cscj_result("mollic_cscj", passed, integer(0), list(sure_cm = sure),
               if (is.na(passed)) c("bs_pct", "oc_pct", "munsell_value_moist", "munsell_chroma_moist") else character(0),
               "IV.1, p. 21")
}
