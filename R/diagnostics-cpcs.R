# =============================================================================
# v0.9.330 -- the twelve classes of the French soil classification (CPCS
# 1967, Commission de Pedologie et de Cartographie des Sols). The CPCS gives
# no key: soilKey keys the classes in an order built from the text's own
# statements of precedence (Hugo, 2026-11-01; see key.yaml and
# readings.yaml), and each class gate tests only its own definition. Every
# threshold of the text is in % organic matter (OM = 1.72 x organic carbon),
# with depths from the top of the first mineral horizon (Aoo and Ao are
# measured upward from the top of A1, p. 11). Three-valued: TRUE, FALSE, or
# NA when the data leave the class undecided. Names carry the _cpcs suffix.
# =============================================================================

.CPCS_REF <- "CPCS (1967). Classification des sols."
.CPCS_OM <- 1.72

.cpcs_result <- function(name, passed, layers = integer(0), evidence = list(),
                         missing = character(0), where = "") {
  DiagnosticResult$new(name = name, passed = passed, layers = as.integer(layers),
                       evidence = evidence, missing = unique(missing),
                       reference = sprintf("%s %s", .CPCS_REF, where))
}

.cpcs_or <- function(...) { v <- c(...); if (any(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA }
.cpcs_and <- function(...) { v <- c(...); if (any(v %in% FALSE)) FALSE else if (all(v %in% TRUE)) TRUE else NA }
.cpcs_not <- function(a) if (is.na(a)) NA else !a
.cpcs_any <- function(v) if (any(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA
.cpcs_all <- function(v) if (any(v %in% FALSE)) FALSE else if (all(v %in% TRUE)) TRUE else NA
.cpcs_num <- function(h, name) suppressWarnings(as.numeric(.scsj_col(h, name)))

# ---- the profile as the CPCS reads it -------------------------------------------

.cpcs_profile <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  site <- pedon$site %||% list()
  top <- h$top_cm; bot <- h$bottom_cm
  des <- .cpcs_designations(.scsj_col(h, "designation", NA_character_), site)
  oc <- .cpcs_num(h, "oc_pct")
  om <- oc * .CPCS_OM
  thick <- ifelse(is.na(top) | is.na(bot), NA_real_, pmax(0, bot - top))
  ord <- order(top, na.last = NA)
  # organic layers: an Aoo, Ao, L, F, H or O; otherwise 30 % OM or more (the
  # line between Ao and A1, p. 11); unknown in an undesignated row
  organic <- ifelse(des$role %in% "O", TRUE,
             ifelse(!is.na(om), om >= 30,
             ifelse(des$known & !(des$role %in% "other"), FALSE, NA)))
  ground <- if (length(ord)) top[ord[1L]] else NA_real_
  firstmin <- ord[!(organic[ord] %in% TRUE)][1L]
  surface <- if (is.na(firstmin)) ground else top[firstmin]
  x <- list(h = h, n = n, site = site, des = des, role = des$role, top = top, bot = bot,
            thick = thick, ord = ord, oc = oc, om = om, organic = organic,
            mineral = ifelse(is.na(organic), NA, !organic), ground = ground, surface = surface,
            firstmin = firstmin, dtop = top - surface, dbot = bot - surface,
            gtop = top - ground, gbot = bot - ground, complete = FALSE)
  x$rock <- des$role %in% "R"
  r <- which(x$rock & !is.na(x$dtop))
  x$rock_top <- if (length(r)) min(x$dtop[r]) else NA_real_
  x$clay <- .cpcs_num(h, "clay_pct"); x$silt <- .cpcs_num(h, "silt_pct"); x$sand <- .cpcs_num(h, "sand_pct")
  x
}

# The same profile with depths from the ground surface (the peat of class XI).
.cpcs_from_ground <- function(x) {
  y <- x; y$dtop <- x$gtop; y$dbot <- x$gbot; y$surface <- x$ground
  r <- which(x$rock & !is.na(x$gtop)); y$rock_top <- if (length(r)) min(x$gtop[r]) else NA_real_
  y
}

# Mineral rows in depth order whose top lies above `to` cm.
.cpcs_rows_within <- function(x, to) {
  x$ord[!(x$organic[x$ord] %in% TRUE) & !is.na(x$dtop[x$ord]) & x$dtop[x$ord] < to]
}

# ---- the horizons of the profile type (p. 11-13) -----------------------------------

# Is there a B (an enriched B, or a (B)), an enriched B, or an A2? From the
# CPCS designations; where some mineral rows carry no designation, from
# soilKey's diagnostics: a B when one of the argic, cambic, spodic, natric,
# ferralic or nitic horizons passes, none when all fail (an enriched B: the
# same without the cambic); an A2 when the albic horizon passes (Hugo,
# 2026-11-01).
.cpcs_horizon_types <- function(x, pedon) {
  # every mineral row, also one without a depth; a buried horizon belongs to
  # a buried profile, not to this one
  rows <- which(!(x$organic %in% TRUE) & !(x$des$buried %in% TRUE))
  role <- x$role[rows]
  undes <- is.na(role) | role %in% "other"
  isb <- role %in% "B"
  core <- isb & !(x$des$trans[rows] %in% TRUE)
  b_any_des <- any(isb)
  be <- x$des$benriched[rows]
  # an enriched B: one of the core B rows; a transition (BA, BC, B1, B3)
  # alone leaves it open
  b_enr_des <- if (any(be[core] %in% TRUE)) TRUE else
    if (any(is.na(be[isb])) || any(isb & !core & be %in% TRUE)) NA else FALSE
  e_des <- any(role %in% "E")
  pick3 <- function(a, b) if (isTRUE(a) || isTRUE(b)) TRUE else if (isFALSE(a) && isFALSE(b)) FALSE else NA
  out <- list(b_any = b_any_des, b_enriched = b_enr_des, e = e_des, from = "designations")
  if (any(undes) || is.na(b_enr_des)) {
    pass <- function(f) tryCatch(f(pedon)$passed, error = function(e) NA)
    # the podzolic B is read by class VIII itself: soilKey's spodic horizon
    # counts here only when it passes
    d_enr <- .cpcs_any(c(pass(argic), pass(natric_horizon), pass(ferralic), pass(nitic_horizon)))
    if (isTRUE(pass(spodic))) d_enr <- TRUE
    if (any(undes)) {
      d_any <- .cpcs_or(d_enr, pass(cambic))
      out$b_any <- pick3(b_any_des, d_any)
      # an albic layer is an A2 only over a mineral layer that is not albic
      cl <- tryCatch(test_claric_munsell(x$h), error = function(e) NULL)
      st <- vapply(seq_len(x$n), function(i) cl$details[[as.character(i)]]$passed %||% NA, NA)
      ord <- x$ord[!(x$organic[x$ord] %in% TRUE) & !x$rock[x$ord]]
      ue <- vapply(seq_along(ord), function(k) {
        i <- ord[k]; if (!isTRUE(st[i])) return(st[i] %in% NA && FALSE)
        below <- ord[seq_len(length(ord)) > k]
        any(st[below] %in% FALSE)
      }, NA)
      d_e <- if (any(ue)) TRUE else if (any(is.na(st[ord]))) NA else FALSE
      out$e <- pick3(e_des, d_e)
      out$from <- "designations and soilKey diagnostics"
    }
    out$b_enriched <- if (is.na(b_enr_des)) d_enr else pick3(b_enr_des, d_enr)
    if (is.na(b_enr_des) && !is.na(d_enr)) out$from <- "designations and soilKey diagnostics"
  }
  out
}

# ---- the organic matter of the top of the profile (classes I and II) ---------------

# Only traces of organic matter in the top 20 cm: every mineral layer that
# starts within 20 cm has 0.5 % OM or less, the top 3 cm 1.5 % (pp. 17, 21;
# Hugo, 2026-11-01). Without carbon, the designation decides: an (A), C, R
# or G is a raw layer, an A or a B is not.
.cpcs_om_traces <- function(x) {
  rows <- .cpcs_rows_within(x, 20)
  if (!length(rows)) return(if (any(x$organic %in% TRUE)) FALSE else NA)
  v <- vapply(rows, function(i) {
    lim <- if (!is.na(x$dtop[i]) && x$dtop[i] <= 0.5 && !is.na(x$dbot[i]) && x$dbot[i] <= 3) 1.5 else 0.5
    if (!is.na(x$om[i])) return(x$om[i] <= lim + 1e-9)
    r <- x$role[i]
    if (isTRUE(x$des$embryo[i]) || r %in% c("C", "R", "G")) TRUE
    else if (r %in% c("A", "AB", "AC", "E", "B")) FALSE else NA
  }, NA)
  .cpcs_all(v)
}

# ---- recent deposits, erosion and permafrost (the causes of class II) ---------------

.CPCS_DEPOSIT_RE <- paste0("alluv|fluvi|fluvic|colluv|solimov|aeol|eolian|eolien|\u00e9olien|dune|",
                           "tephr|volcanic ash|pyroclast|cendre|ponce|pumice|lapilli|scoria|",
                           "anthrop|fill|remblai|apport|spoil|dredg|recent deposit|d\u00e9p\u00f4t r\u00e9cent")
.CPCS_EROSION_RE <- "erosion|\u00e9rosion|eroded|\u00e9rod\u00e9|gully|ravine|badland|truncat|deflation"

# A recorded deposit in the origin of a layer; the text dates the deposit
# of class II to some years or decades before the observation (p. 21), so a
# glacial or fluvioglacial deposit, or loess, is not one.
.cpcs_recent_deposit <- function(x) {
  txt <- tolower(paste(c(.scsj_txt(x$h, "layer_origin"), .scsj_txt(x$h, "rock_origin")), collapse = " "))
  txt <- gsub("fluvio-?glac[a-z]*|glacio-?[a-z]*|glacial[a-z]*|\\bloess\\b", " ", txt)
  grepl(.CPCS_DEPOSIT_RE, txt)
}

# Negated words taken out of a free text ("no erosion", "not eroded",
# "erosion: none", "non-calcareous").
.cpcs_drop_negated <- function(txt) {
  w <- "[a-z\u00e0-\u00ff]+"
  txt <- gsub("\\b(un|non-?)(eroded|\u00e9rod\u00e9|calcareous|calcaire)", " ", txt)
  txt <- gsub(paste0("\\b(no|not|non|without|sans|pas d[e']|aucune?)[- ]+(", w, "[- ]+){0,3}", w), " ", txt)
  gsub(paste0(w, "[ ]*(class|degree|:|-)?[ ]*(none|nil|absent|aucune?|nulle?|0)\\b"), " ", txt)
}

.cpcs_recent_erosion <- function(x) {
  s <- x$site
  if (isTRUE(.nzsc_yes(s$anthropic_truncation)) || isTRUE(.nzsc_yes(s$wind_deflation))) return(TRUE)
  txt <- tolower(paste(as.character(unlist(c(s$landform, s$surface_condition, s$land_use))), collapse = " "))
  grepl(.CPCS_EROSION_RE, .cpcs_drop_negated(txt))
}

# Permafrost: a gelic or pergelic soil temperature regime, a permafrost
# temperature of 0 C or less, ice, or a frozen layer of the notation (f);
# nothing recorded means none.
.cpcs_permafrost <- function(x) {
  reg <- tolower(as.character(x$site$soil_temperature_regime %||% NA)[1L])
  if (!is.na(reg) && grepl("gelic|pergelic", reg)) return(TRUE)
  pt <- .cpcs_num(x$h, "permafrost_temp_C")
  ice <- .cpcs_num(x$h, "ice_pct")
  # the reader gives each notation its frozen letter (f in the FAO notation,
  # z in the Canadian one); never on an organic master
  frozen <- x$des$frozen %in% TRUE & !(x$role %in% "O")
  any(pt <= 0, na.rm = TRUE) || any(ice > 0, na.rm = TRUE) || any(frozen)
}

# ---- the chemistry of class V ------------------------------------------------------

# The top mineral A rows (the A1 and its transitions), or the first mineral
# row when the profile is undesignated.
.cpcs_a_rows <- function(x) {
  rows <- x$ord[!(x$organic[x$ord] %in% TRUE)]
  if (!length(rows)) return(integer(0))
  out <- integer(0)
  for (i in rows) {
    r <- x$role[i]
    if (r %in% c("A", "AB", "AC")) { out <- c(out, i); next }
    if ((is.na(r) || r %in% "other") && !length(out)) { out <- i; break }
    break
  }
  out
}

# The top run of A1 rows (role A only, not the A3 or AB), for the base of
# the A1 of class XI.
.cpcs_a1_rows <- function(x) {
  a <- .cpcs_a_rows(x)
  a[x$role[a] %in% "A" | is.na(x$role[a])]
}

# Base saturation of each row: bs_pct, else the sum of the bases over the
# CEC.
.cpcs_bs <- function(x) {
  h <- x$h
  bs <- .cpcs_num(h, "bs_pct"); cec <- .cpcs_num(h, "cec_cmol")
  z <- function(v) ifelse(is.na(v), 0, v)
  sb <- .cpcs_num(h, "ca_cmol") + .cpcs_num(h, "mg_cmol") + z(.cpcs_num(h, "k_cmol")) + z(.cpcs_num(h, "na_cmol"))
  ifelse(!is.na(bs), bs, ifelse(!is.na(sb) & !is.na(cec) & cec > 0, pmin(100, 100 * sb / cec), NA_real_))
}

# pH(H2O) above 6.8 and the complex saturated above 90 % by Ca and Mg in the
# A (a calcareous A counts as saturated; base saturation when Ca or Mg is
# not given) (p. 35).
.cpcs_v_chemistry <- function(x) {
  a <- .cpcs_a_rows(x)
  if (!length(a)) return(NA)
  h <- x$h
  ph <- .cpcs_num(h, "ph_h2o")[a]
  cec <- .cpcs_num(h, "cec_cmol")[a]; ca <- .cpcs_num(h, "ca_cmol")[a]; mg <- .cpcs_num(h, "mg_cmol")[a]
  bs <- .cpcs_num(h, "bs_pct")[a]; cc <- .cpcs_num(h, "caco3_pct")[a]
  calc <- ifelse(!is.na(cc), cc > 0, x$des$ca[a] %in% TRUE)
  sat <- ifelse(calc, 100, ifelse(!is.na(cec) & cec > 0 & !is.na(ca) & !is.na(mg), 100 * (ca + mg) / cec, bs))
  ph_ok <- .cpcs_all(ifelse(is.na(ph), NA, ph > 6.8))
  sat_ok <- .cpcs_all(ifelse(is.na(sat), NA, sat > 90))
  .cpcs_and(ph_ok, sat_ok)
}

.CPCS_CAMG_RE <- paste0("limestone|chalk|marl|dolom|calcar|calcaire|craie|marne|travertin|tufa|",
                        "ultrabasic|ultramafic|serpentin|p\u00e9ridot|peridot|dunite")

# A source of Ca or Mg: carbonate in the profile, or a calcareous or very
# basic rock recorded (p. 35).
.cpcs_camg_source <- function(x) {
  cc <- .cpcs_num(x$h, "caco3_pct")
  ef <- .scsj_txt(x$h, "effervescence_class")
  ef_pos <- !is.na(ef) & grepl("slight|weak|moderate|strong|violent|faible|forte|moyenne", ef) &
    !grepl("^(none|no|not|non|absent|nil|0)", ef)
  if (any(cc > 0, na.rm = TRUE) || any(is.na(cc) & x$des$ca %in% TRUE) || any(ef_pos)) return(TRUE)
  txt <- tolower(paste(c(.scsj_txt(x$h, "rock_origin"), .scsj_txt(x$h, "layer_origin"),
                         as.character(unlist(x$site$parent_material %||% character(0)))), collapse = " "))
  if (grepl(.CPCS_CAMG_RE, .cpcs_drop_negated(txt))) return(TRUE)
  if (all(is.na(cc)) && !nzchar(gsub("na| ", "", txt))) NA else FALSE
}

.CPCS_VOLCANIC_RE <- "volcan|tephr|\\bash\\b|cendre|pumic|ponce|scori|lapilli|pyroclast|vitric|glass|verre|andic|andique"

# A sign of volcanic material: tephra or glass named in the origin of a
# layer or in the parent material, or volcanic glass recorded.
.cpcs_volcanic <- function(x) {
  txt <- tolower(paste(c(.scsj_txt(x$h, "layer_origin"), .scsj_txt(x$h, "rock_origin"),
                         as.character(unlist(x$site$parent_material %||% character(0)))), collapse = " "))
  grepl(.CPCS_VOLCANIC_RE, txt) || any(.cpcs_num(x$h, "volcanic_glass_pct") > 0, na.rm = TRUE)
}

# A tropical climate: an isothermic, isohyperthermic or hyperthermic soil
# temperature regime, or a
# latitude of 23.5 degrees or less; not recorded, not tropical (the default
# of the climate readings, Hugo, 2026-11-01).
.cpcs_tropical <- function(x) {
  reg <- tolower(as.character(x$site$soil_temperature_regime %||% NA)[1L])
  if (!is.na(reg) && grepl("isothermic|isohyperthermic|^hyperthermic", reg)) return(TRUE)
  lat <- suppressWarnings(as.numeric(as.character(x$site$lat %||% NA)[1L]))
  !is.na(lat) && abs(lat) <= 23.5
}

# ---- reading a property in the B ----------------------------------------------

# The B rows: designated B rows (with their transitions); in an undesignated
# profile, the mineral rows below the first.
.cpcs_b_rows <- function(x) {
  rows <- x$ord[!(x$organic[x$ord] %in% TRUE)]
  b <- rows[x$role[rows] %in% "B" & !(x$des$buried[rows] %in% TRUE)]
  if (length(b)) return(b)
  # no designated B: the undesignated rows below the A
  a <- .cpcs_a_rows(x)
  rest <- rows[!(rows %in% a) & !x$rock[rows] & (is.na(x$role[rows]) | x$role[rows] %in% "other")]
  if (!length(a) && length(rest)) rest <- rest[-1L]
  rest
}

# A property read in the larger part of the first 100 cm of the B (Hugo's
# reading for the SiBCS, 2026-10-14, kept for the CPCS "en B"): TRUE when
# the rows where it holds cover more than half of that thickness, FALSE when
# the rows where it fails cover half or more, NA otherwise.
.cpcs_larger_part_b <- function(x, v, b = .cpcs_b_rows(x)) {
  b <- b[!is.na(x$dtop[b])]
  if (!length(b)) return(NA)
  t0 <- min(x$dtop[b]); t1 <- t0 + 100
  ov <- vapply(b, function(i) {
    bt <- if (is.na(x$dbot[i])) x$dtop[i] + 1 else x$dbot[i]
    max(0, min(bt, t1) - max(x$dtop[i], t0))
  }, 0)
  tot <- sum(ov)
  if (tot <= 0) return(NA)
  yes <- sum(ov[v[b] %in% TRUE]); no <- sum(ov[v[b] %in% FALSE])
  if (yes > tot / 2) TRUE else if (no >= tot / 2) FALSE else NA
}

# Ki (SiO2/Al2O3, molar, sulfuric attack) of each row.
.cpcs_ki <- function(x) {
  compute_ki(.cpcs_num(x$h, "sio2_sulfuric_pct"), .cpcs_num(x$h, "al2o3_sulfuric_pct"))
}

# The CEC of the clay, cmol(+)/kg clay.
.cpcs_cec_clay <- function(x) {
  cec <- .cpcs_num(x$h, "cec_cmol")
  ifelse(is.na(cec) | is.na(x$clay) | x$clay <= 0, NA_real_, 100 * cec / x$clay)
}

# The ferrallitic B of each row: Ki 2.0 or less; without Ki, a CEC of the
# clay of 16 cmol(+)/kg or less (Hugo, 2026-11-01).
.cpcs_ferrallitic_rows <- function(x) {
  ki <- .cpcs_ki(x); cc <- .cpcs_cec_clay(x)
  ifelse(!is.na(ki), ki <= 2.0, ifelse(!is.na(cc), cc <= 16, NA))
}

# ---- hydromorphy (class XI) ----------------------------------------------------

# A reduced (gley) matrix: chroma 2 or less with value 4 or more in a hue of
# 5Y or bluer, or neutral (grey, greenish, bluish, p. 75); a G of the
# notation; or a positive dipyridyl test.
.cpcs_reduced_rows <- function(x) {
  h <- x$h
  hue <- as.character(.scsj_col(h, "munsell_hue_moist", NA_character_))
  v <- .cpcs_num(h, "munsell_value_moist"); cr <- .cpcs_num(h, "munsell_chroma_moist")
  neu <- .scsj_neutral(hue)
  cr <- ifelse(neu & is.na(cr), 0, cr)
  hn <- .scsj_hue_num(hue)
  blue <- neu | (!is.na(hn) & hn >= 25)
  known <- !is.na(v) & !is.na(cr)
  grey <- known & v >= 4 & cr <= 2 & !(x$role %in% c("E", "O"))
  dip <- .scsj_dipyridyl(h)
  # the recorded colour decides; a G of the notation is a grey of any hue,
  # or stands alone when no colour is recorded (Hugo, 2026-11-01)
  G <- x$des$G %in% TRUE
  (grey & (blue | G)) | (G & !known) | dip %in% TRUE
}

# A hydromorphic layer: 5 % or more of redox features, a g or G of the
# notation, or a reduced matrix. A mottle that is not recorded is absent
# (Hugo, 2026-11-01).
.cpcs_hydromorphic_rows <- function(x) {
  # recorded data first: a recorded mottle percentage decides, with the
  # matrix; a g or G of the notation only where no mottles are recorded
  # (Hugo, 2026-11-01)
  rf <- .cpcs_num(x$h, "redoximorphic_features_pct")
  red <- .cpcs_reduced_rows(x)
  ifelse(!is.na(rf), rf >= 5 | red, x$des$g %in% TRUE | red)
}

# The base of the A1: the bottom of the top run of A rows, 0 when the
# mineral profile does not start with an A.
.cpcs_a1_base <- function(x) {
  a <- .cpcs_a1_rows(x)
  if (!length(a)) return(0)
  b <- x$dbot[a[length(a)]]
  if (is.na(b)) NA_real_ else b
}

# ---- texture ---------------------------------------------------------------------

# Sandy (clay + silt under 15 %, p. 39), clayey (clay above 40 %) or medium.
.cpcs_texture <- function(x, i) {
  cl <- x$clay[i]; si <- x$silt[i]; sa <- x$sand[i]
  if (is.na(si) && !is.na(cl) && !is.na(sa)) si <- 100 - cl - sa
  if (!is.na(cl) && cl > 40) return("clayey")
  if (!is.na(cl) && !is.na(si)) return(if (cl + si < 15) "sandy" else "medium")
  if (!is.na(cl) && cl >= 15) return("medium")
  NA_character_
}

# The texture minimum of OM for the isohumic soils: 0.5 % sandy, 0.75 %
# medium, 1 % clayey (Hugo, 2026-11-01).
.cpcs_om_min <- function(tex) switch(tex %||% "na", sandy = 0.5, medium = 0.75, clayey = 1, NA_real_)

# Organic matter falling sharply below the A (p. 35): the horizon under the
# A rows has a third of the OM of the last of them or less; rock under the A
# is a sharp transition. The isohumic soils need the contrary (a gradual
# decrease) (Hugo, 2026-11-01).
.cpcs_sharp_drop <- function(x) {
  a <- .cpcs_a_rows(x)
  if (!length(a)) return(NA)
  mrows <- x$ord[!(x$organic[x$ord] %in% TRUE)]
  nxt <- mrows[match(a[length(a)], mrows) + 1L]
  om_a <- x$om[a[length(a)]]
  if (is.na(nxt)) return(NA)
  if (x$rock[nxt]) return(TRUE)
  if (is.na(om_a) || is.na(x$om[nxt])) return(NA)
  x$om[nxt] <= om_a / 3
}

# Peat (XI-1): more than 30 % OM (more than 20 % in a sandy material) over
# 40 cm or more of the top 80 cm of the ground (p. 73). An H with no carbon
# is peat in the FAO notation; in the CPCS notation H is a layer of the Ao
# (p. 11), undecided.
.cpcs_peat_rows <- function(x) {
  des <- vapply(as.character(.scsj_col(x$h, "designation", NA_character_)), .cpcs_symbol, "")
  vapply(seq_len(x$n), function(i) {
    om <- x$om[i]
    if (is.na(om)) {
      if (!is.na(des[i]) && grepl("^[IVX0-9]*H", des[i])) return(if (isTRUE(x$des$cpcs)) NA else TRUE)
      if (x$role[i] %in% "O") return(NA)
      return(if (x$des$known[i]) FALSE else NA)
    }
    if (om > 30) return(TRUE)
    if (om <= 20) return(FALSE)
    tex <- .cpcs_texture(x, i)
    if (is.na(tex)) NA else tex == "sandy"
  }, NA, USE.NAMES = FALSE)
}

.cpcs_peat_route <- function(x) {
  g <- .cpcs_from_ground(x)
  to <- min(80, g$rock_top, na.rm = TRUE)
  .scsj_at_least(.scsj_cum(g, .cpcs_peat_rows(x), 0, to), 40)
}

# A peat soil is a hydromorphic soil: the peat excludes the mineral classes
# keyed before class XI (Hugo, 2026-11-01). A gate's result with that
# exclusion.
.cpcs_not_peat <- function(res, x) {
  if (isTRUE(.cpcs_peat_route(x)) && !isFALSE(res$passed)) {
    res$passed <- FALSE
    res$evidence$peat_soil <- TRUE
  }
  res
}

# ---- the class gates ---------------------------------------------------------------

#' CPCS 1967 class gates (internal)
#'
#' The tests of the twelve classes of the French soil classification
#' (CPCS 1967), read by \code{\link{classify_cpcs1967}} in the order of
#' \code{inst/rules/cpcs1967/key.yaml}. Each returns a
#' \code{\link{DiagnosticResult}}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @keywords internal
#' @name cpcs1967_classes
NULL

#' @rdname cpcs1967_classes
#' @export
raw_mineral_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  om <- .cpcs_om_traces(x)
  no_evol <- .cpcs_and(.cpcs_not(ty$b_any), .cpcs_not(ty$e))
  # a profile (A)C, (A)R or R: no organic layer over the mineral soil (p. 17)
  no_org <- !any(x$organic %in% TRUE)
  passed <- .cpcs_and(om, no_evol, no_org)
  .cpcs_result("raw_mineral_soils_cpcs", passed, .cpcs_rows_within(x, 20),
               list(om_traces_top_20 = om, no_A2_no_B = no_evol, no_organic_layer = no_org,
                    horizons_from = ty$from),
               if (is.na(passed)) c("oc_pct", "designation") else character(0), "I, p. 17")
}

#' @rdname cpcs1967_classes
#' @export
weakly_developed_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  pf <- .cpcs_permafrost(x)
  ac <- .cpcs_and(.cpcs_not(ty$b_any), .cpcs_not(ty$e))
  # an organic layer over the mineral soil is more than traces
  om_more <- if (any(x$organic %in% TRUE)) TRUE else .cpcs_not(.cpcs_om_traces(x))
  # an AC soil with the chemistry of class V and a source of Ca or Mg is a
  # calcimagnesic soil, unless a cause of class II is recorded (Hugo,
  # 2026-11-01): a recent deposit or recent erosion
  v_soil <- .cpcs_and(.cpcs_v_chemistry(x), .cpcs_camg_source(x))
  cause <- .cpcs_recent_deposit(x) || .cpcs_recent_erosion(x)
  not_v <- if (cause) TRUE else .cpcs_not(v_soil)
  passed <- .cpcs_or(pf, .cpcs_and(ac, om_more, not_v))
  .cpcs_result("weakly_developed_soils_cpcs", passed, .cpcs_rows_within(x, 20),
               list(permafrost = pf, ac_profile = ac, om_above_traces = om_more,
                    calcimagnesic_chemistry_and_source = v_soil, recent_deposit_or_erosion = cause),
               if (is.na(passed)) c("oc_pct", "designation", "ph_h2o", "ca_cmol", "mg_cmol") else character(0),
               "II, p. 21")
}

#' @rdname cpcs1967_classes
#' @export
vertisols_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  h <- x$h
  rows <- .cpcs_rows_within(x, 150)
  sl <- tolower(as.character(.scsj_col(h, "slickensides", NA_character_)))
  st <- .scsj_txt(h, "structure_type")
  # slickensides or wedge-shaped aggregates, common or more, in a layer
  # starting within 150 cm; one that is not recorded is absent
  slick <- (!is.na(sl) & grepl("common|many|continuous|abundant|frequent", sl)) |
           (!is.na(st) & grepl("wedge|lenticul|cuneiform|sphenoid", st)) | x$des$ss %in% TRUE
  srow <- rows[slick[rows]]
  has_slick <- length(srow) > 0L
  # clay 30 % or more through the (B) (the B rows, else the rows with
  # slickensides)
  b <- .cpcs_b_rows(x); b <- if (length(b)) intersect(b, rows) else srow
  if (!length(b)) b <- srow
  clay_ok <- if (!has_slick) FALSE else .cpcs_all(ifelse(is.na(x$clay[b]), NA, x$clay[b] >= 30))
  cw <- .cpcs_num(h, "cracks_width_cm")[rows]
  # shrinkage cracks of 0.5 cm or more, as soilKey reads the WRB (Hugo,
  # 2026-11-01), when recorded
  cracks_ok <- if (all(is.na(cw))) TRUE else any(cw >= 0.5, na.rm = TRUE)
  passed <- .cpcs_and(has_slick, clay_ok, cracks_ok, .cpcs_not(ty$b_enriched))
  .cpcs_not_peat(.cpcs_result("vertisols_cpcs", passed, srow,
               list(slickensides = has_slick, clay_30_in_B = clay_ok, cracks_0_5_cm = cracks_ok,
                    no_enriched_B = .cpcs_not(ty$b_enriched)),
               if (is.na(passed)) c("clay_pct", "designation") else character(0), "III, p. 27"), x)
}

#' @rdname cpcs1967_classes
#' @export
sodic_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  h <- x$h
  rows <- .cpcs_rows_within(x, 100)
  rows <- rows[!x$rock[rows]]
  if (!length(rows)) return(.cpcs_result("sodic_soils_cpcs", FALSE, where = "XII, p. 77"))
  # the EC is read in the solum (the A and B horizons to 100 cm; in a
  # profile without a B, the horizons to 50 cm); the C below is the parent
  # material (Hugo, 2026-11-01). In an undesignated profile the horizons to
  # 50 cm; deeper ones under 7 dS/m leave it undecided unless there is no B.
  undes <- all(is.na(x$role[rows]) | x$role[rows] %in% "other")
  sol <- if (undes) rows[x$dtop[rows] < 50] else
    if (any(x$role[rows] %in% "B")) rows[!(x$role[rows] %in% c("C", "R", "G"))] else
      rows[x$dtop[rows] < 50 | x$role[rows] %in% c("A", "AB", "AC", "E")]
  if (!length(sol)) sol <- rows
  ec <- .cpcs_num(h, "ec_dS_m")[sol]
  na <- .cpcs_num(h, "na_cmol"); cec <- .cpcs_num(h, "cec_cmol"); esp0 <- .cpcs_num(h, "esp_pct")
  esp <- ifelse(!is.na(na) & !is.na(cec) & cec > 0, 100 * na / cec, esp0)[rows]
  ec_route <- if (all(is.na(ec))) FALSE else if (any(ec <= 7, na.rm = TRUE)) FALSE else
    if (all(!is.na(ec))) TRUE else NA
  if (undes && isTRUE(ec_route)) {
    deep <- .cpcs_num(h, "ec_dS_m")[setdiff(rows, sol)]
    if (any(deep <= 7, na.rm = TRUE) && !isFALSE(.cpcs_horizon_types(x, pedon)$b_any)) ec_route <- NA
  }
  # exchangeable Na above 10 % of the CEC with a massive, diffuse (or
  # columnar) structure and a compact consistence (Na only: the Mg threshold
  # is not known)
  st <- .scsj_txt(h, "structure_type")[rows]; gr <- .scsj_txt(h, "structure_grade")[rows]
  cm <- .scsj_txt(h, "consistence_moist")[rows]; rr <- .scsj_txt(h, "rupture_resistance")[rows]
  degraded <- ifelse(is.na(st) & is.na(gr), NA,
                     (!is.na(st) & grepl("massive|massif|columnar|colonn", st)) |
                     (!is.na(gr) & grepl("structureless", gr)))
  compact <- ifelse(is.na(cm) & is.na(rr), NA,
                    (!is.na(cm) & grepl("very firm|extremely firm", cm)) |
                    (!is.na(rr) & grepl("very hard|extremely hard", rr)))
  sod <- ifelse(is.na(esp), NA, esp > 10)
  sod_rows <- mapply(function(a, b, c) .cpcs_and(a, b, c), sod, degraded, compact)
  sod_route <- if (all(is.na(esp))) FALSE else .cpcs_any(sod_rows)
  # a salinity of gypsum alone, with little Na, belongs to class V (p. 79)
  cs4 <- .cpcs_num(h, "caso4_pct")[rows]
  gyp <- any(cs4 > 0, na.rm = TRUE) || any(is.na(cs4) & x$des$cs[rows] %in% TRUE) ||
    any(.cpcs_num(h, "gypsum_visible_pct")[rows] > 0, na.rm = TRUE)
  if (isTRUE(ec_route) && gyp && !any(esp > 10, na.rm = TRUE))
    ec_route <- if (all(!is.na(esp))) FALSE else NA
  # clean sands keep no lasting trace of sodium (p. 77, 79)
  sandy <- vapply(rows, function(i) {
    if (is.na(x$sand[i]) || is.na(x$clay[i])) return(NA)
    si <- if (is.na(x$silt[i])) 100 - x$sand[i] - x$clay[i] else x$silt[i]
    x$sand[i] >= 85 && si + 1.5 * x$clay[i] < 15
  }, NA)
  sands <- .cpcs_all(sandy)
  # with no value, salts seen in the field leave the class undecided
  seen <- any(x$des$sa[rows] %in% TRUE) || any(.cpcs_num(h, "halite_visible_pct")[rows] > 0, na.rm = TRUE)
  if (isFALSE(ec_route) && all(is.na(ec)) && seen) ec_route <- NA
  passed <- .cpcs_and(.cpcs_or(ec_route, sod_route), .cpcs_not(sands))
  .cpcs_not_peat(.cpcs_result("sodic_soils_cpcs", passed, rows,
               list(ec_above_7_in_solum = ec_route, na_above_10_degraded = sod_route, clean_sands = sands,
                    gypsum = gyp),
               if (is.na(passed)) c("ec_dS_m", "na_cmol", "cec_cmol", "structure_type", "consistence_moist")
               else character(0), "XII, p. 77"), x)
}

#' @rdname cpcs1967_classes
#' @export
andosols_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  an <- tryCatch(andic_properties(pedon), error = function(e) NULL)
  rows <- .cpcs_rows_within(x, 100)
  # andic layers within 100 cm: the certain thickness, and the possible one
  # with the layers soilKey leaves undecided
  st <- as.logical(an$evidence$layer_status %||% rep(NA, x$n))
  if (length(st) != x$n) st <- rep(NA, x$n)
  st[as.integer(an$layers %||% integer(0))] <- TRUE
  cut <- function(i) { b <- if (is.na(x$dbot[i])) x$dtop[i] else min(x$dbot[i], 100); max(0, b - max(x$dtop[i], 0)) }
  lay <- rows[st[rows] %in% TRUE]
  th <- sum(vapply(lay, cut, 0))
  poss <- th + sum(vapply(rows[is.na(st[rows])], cut, 0))
  andic <- if (th >= 30) TRUE else if (poss < 30) FALSE else NA
  # without the andic data, a soil with no sign of volcanic material
  # (tephra, glass, pumice) is not an andosol
  if (is.na(andic) && !.cpcs_volcanic(x)) andic <- FALSE
  # the thickness-weighted mean CEC of the andic A and (B) within 100 cm
  sol <- lay[!(x$role[lay] %in% c("C", "R", "G"))]
  if (!length(sol)) sol <- lay
  cec <- .cpcs_num(x$h, "cec_cmol")[sol]
  w <- vapply(sol, function(i) { b <- if (is.na(x$dbot[i])) x$dtop[i] + 1 else min(x$dbot[i], 100)
                                 max(0, b - max(x$dtop[i], 0)) }, 0)
  k <- !is.na(cec) & w > 0
  cec_ok <- if (!length(sol) || !any(k)) TRUE else sum(cec[k] * w[k]) / sum(w[k]) > 25
  passed <- .cpcs_and(andic, cec_ok, ty$b_any)
  .cpcs_not_peat(.cpcs_result("andosols_cpcs", passed, lay,
               list(andic_cm_within_100 = th, cec_above_25 = cec_ok, B_present = ty$b_any),
               if (is.na(passed)) c("al_ox_pct", "fe_ox_pct", "bulk_density_g_cm3", "phosphate_retention_pct")
               else character(0), "IV, pp. 31-32"), x)
}

#' @rdname cpcs1967_classes
#' @export
podzolised_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  h <- x$h
  n <- .cpcs_num(h, "n_total_pct")
  rows <- x$ord[!(x$organic[x$ord] %in% TRUE)]
  e <- rows[x$role[rows] %in% "E"]
  oc_e <- if (length(e) && any(!is.na(x$oc[e]))) max(x$oc[e], na.rm = TRUE) else NA_real_
  b <- .cpcs_b_rows(x)
  undes <- all(is.na(x$role[rows]) | x$role[rows] %in% "other")
  cl_all <- tryCatch(test_claric_munsell(h), error = function(err) NULL)
  albic_row <- function(i) isTRUE(cl_all$details[[as.character(i)]]$passed)
  # in an undesignated profile, a layer of albic material is an A2, not a B
  if (undes) b <- b[!vapply(b, albic_row, NA)]
  # criterion 2: OM above 0.5 %, C/N above 14, more carbon than the A2;
  # without C or N, a Bh, Bs or Bfe of the notation (Hugo, 2026-11-01). The
  # notation gives the kind of B: a (B), Bw or Bt is not a podzolic B.
  bz <- vapply(b, function(i) {
    if (isFALSE(x$des$h[i]) && isFALSE(x$des$fe[i]) &&
        (isTRUE(x$des$bstruct[i]) || isTRUE(x$des$t[i]) || isTRUE(x$des$trans[i]))) return(FALSE)
    if (!is.na(x$om[i]) && !is.na(n[i]) && n[i] > 0) {
      ok <- x$om[i] > 0.5 && x$oc[i] / n[i] > 14
      if (length(e)) ok <- ok && (if (is.na(oc_e)) NA else x$oc[i] > oc_e)
      return(ok)
    }
    if (!is.na(x$om[i]) && x$om[i] <= 0.5) return(FALSE)
    if (isTRUE(x$des$h[i]) || isTRUE(x$des$fe[i])) return(TRUE)
    # a B of the notation without h, s or fe is not podzolic, except a plain
    # B of the FAO notation (no symbol at all), which stays undecided
    if (x$role[i] %in% "B") {
      other <- any(vapply(c("t", "g", "G", "ca", "cs", "sa", "ss", "natric", "cn", "p", "x", "m", "trans"),
                          function(f) isTRUE(x$des[[f]][i]), NA))
      return(if (is.na(x$des$benriched[i]) && !other) NA else FALSE)
    }
    NA
  }, NA)
  podz_b <- if (length(b)) .cpcs_any(bz) else FALSE
  # an undesignated profile: soilKey's spodic horizon decides when it can
  # (the C/N of an undesignated layer may be that of a C)
  if (undes) {
    sp <- tryCatch(spodic(pedon)$passed, error = function(err) NA)
    if (!is.na(sp)) podz_b <- sp
  }
  # an ashy A2 alone (sols humo-cendreux): an A2 of albic material, no B; not
  # a g master nor a hydromorphic A2 (p. 51)
  rf <- .cpcs_num(h, "redoximorphic_features_pct")
  e2 <- e[!(x$des$g[e] %in% TRUE) & !(!is.na(rf[e]) & rf[e] >= 5)]
  ashy <- FALSE
  if (length(e2)) {
    st <- vapply(e2, function(i) cl_all$details[[as.character(i)]]$passed %||% NA, NA)
    ashy <- .cpcs_any(st)
  }
  ashy_route <- .cpcs_and(ashy, .cpcs_not(ty$b_any))
  passed <- .cpcs_or(podz_b, ashy_route)
  .cpcs_not_peat(.cpcs_result("podzolised_soils_cpcs", passed, b[bz %in% TRUE],
               list(podzolic_B = podz_b, ashy_A2_without_B = ashy_route),
               if (is.na(passed)) c("oc_pct", "n_total_pct", "designation", "munsell_value_dry") else character(0),
               "VIII, p. 49"), x)
}

#' @rdname cpcs1967_classes
#' @export
ferrallitic_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  fr <- .cpcs_larger_part_b(x, .cpcs_ferrallitic_rows(x))
  passed <- .cpcs_and(ty$b_any, fr)
  .cpcs_not_peat(.cpcs_result("ferrallitic_soils_cpcs", passed, .cpcs_b_rows(x),
               list(B_present = ty$b_any, ki_2_or_cec_clay_16_in_B = fr),
               if (is.na(passed)) c("sio2_sulfuric_pct", "al2o3_sulfuric_pct", "cec_cmol", "clay_pct") else character(0),
               "X, pp. 63-65"), x)
}

#' @rdname cpcs1967_classes
#' @export
sesquioxide_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  h <- x$h
  ki <- .cpcs_ferrallitic_rows(x)
  ki_gt <- .cpcs_larger_part_b(x, ifelse(is.na(ki), NA, !ki))
  bs <- .cpcs_bs(x)
  v50 <- .cpcs_larger_part_b(x, ifelse(is.na(bs), NA, bs > 50))
  # iron individualised: a reddened B (5YR or redder, chroma 4 or more, the
  # fersiallitic soils), or iron concretions; a 7.5YR or 10YR B with chroma 4
  # or more and value 5 or more only under a tropical climate, the
  # ferruginous tropical soils (Hugo, 2026-11-01); a neutral B is not
  hue <- as.character(.scsj_col(h, "munsell_hue_moist", NA_character_))
  hn <- .scsj_hue_num(hue); neu <- .scsj_neutral(hue)
  cr <- .cpcs_num(h, "munsell_chroma_moist"); va <- .cpcs_num(h, "munsell_value_moist")
  nod <- .cpcs_num(h, "ferric_nodules_pct")
  trop <- .cpcs_tropical(x)
  fe <- ifelse((!is.na(nod) & nod > 0) | (is.na(nod) & x$des$cn %in% TRUE), TRUE,
        ifelse(neu, FALSE,
        ifelse(is.na(hn) | is.na(cr), NA,
        ifelse(hn <= 15 & cr >= 4, TRUE,
        ifelse(hn <= 20 & cr >= 4 & !is.na(va) & va >= 5 & trop, TRUE, FALSE)))))
  fe_ind <- .cpcs_larger_part_b(x, fe)
  not_iso <- .cpcs_not(isohumic_soils_cpcs(pedon)$passed)
  passed <- .cpcs_and(ty$b_any, ki_gt, v50, fe_ind, not_iso)
  .cpcs_not_peat(.cpcs_result("sesquioxide_soils_cpcs", passed, .cpcs_b_rows(x),
               list(B_present = ty$b_any, ki_above_2_in_B = ki_gt, bs_above_50_in_B = v50,
                    iron_individualised_in_B = fe_ind, not_isohumic = not_iso),
               if (is.na(passed)) c("sio2_sulfuric_pct", "al2o3_sulfuric_pct", "bs_pct", "munsell_hue_moist")
               else character(0), "IX, pp. 57-60"), x)
}

#' @rdname cpcs1967_classes
#' @export
hydromorphic_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  pf <- .cpcs_permafrost(x)
  if (pf) return(.cpcs_result("hydromorphic_soils_cpcs", FALSE, evidence = list(permafrost = TRUE),
                              where = "XI, p. 73"))
  # peat: more than 30 % OM (more than 20 % in a sandy material) over 40 cm
  # or more of the top 80 cm of the ground (p. 73; Hugo, 2026-11-01)
  peat_route <- .cpcs_peat_route(x)
  # anmoor or hydromor: 8 to 30 % OM over 20 cm or more from the mineral
  # surface, directly over a gley or stagnogley (a reduced matrix)
  red <- .cpcs_reduced_rows(x)
  mrows <- x$ord[!(x$organic[x$ord] %in% TRUE)]
  run <- integer(0); anm <- FALSE; stop_na <- FALSE
  for (i in mrows) {
    om <- x$om[i]
    if (is.na(om)) { stop_na <- TRUE; break }
    if (om >= 8 && om < 30) run <- c(run, i) else break
  }
  if (stop_na && (!length(run) || sum(x$thick[run], na.rm = TRUE) < 20)) anm <- NA
  if (length(run) && !is.na(anm)) {
    th <- sum(x$thick[run], na.rm = TRUE)
    nxt <- mrows[match(run[length(run)], mrows) + 1L]
    if (th >= 20 && !is.na(nxt)) {
      known <- !is.na(.cpcs_num(x$h, "munsell_chroma_moist")[nxt]) || x$des$known[nxt] ||
        !is.na(.scsj_dipyridyl(x$h)[nxt])
      anm <- if (isTRUE(red[nxt])) TRUE else if (known) FALSE else NA
    }
  }
  # the mineral route: hydromorphic layers over the larger part of the
  # profile from the base of the A1 to 100 cm (p. 73, 75)
  hyd <- .cpcs_hydromorphic_rows(x)
  base <- .cpcs_a1_base(x)
  to <- min(100, x$rock_top, na.rm = TRUE)
  min_route <- if (is.na(base)) NA else if (base >= to) FALSE else {
    L <- to - base
    cu <- .scsj_cum(x, hyd, base, to)
    if (cu$sure > L / 2) TRUE else if (cu$poss <= L / 2) FALSE else NA
  }
  passed <- .cpcs_or(peat_route, anm, min_route)
  .cpcs_result("hydromorphic_soils_cpcs", passed, which(hyd),
               list(peat_40_cm = peat_route, anmoor_over_gley = anm, hydromorphic_majority = min_route,
                    a1_base_cm = base),
               if (is.na(passed)) c("oc_pct", "redoximorphic_features_pct", "munsell_chroma_moist") else character(0),
               "XI, pp. 73-75")
}

#' @rdname cpcs1967_classes
#' @export
isohumic_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  h <- x$h
  rows <- x$ord[!(x$organic[x$ord] %in% TRUE) & !is.na(x$dtop[x$ord])]
  if (!length(rows)) return(.cpcs_result("isohumic_soils_cpcs", NA, where = "VI, p. 39"))
  # OM in the top 15 cm at least the texture minimum
  top <- rows[x$dtop[rows] < 15]
  w <- vapply(top, function(i) max(0, min(if (is.na(x$dbot[i])) 15 else x$dbot[i], 15) - max(x$dtop[i], 0)), 0)
  om15 <- if (!length(top) || any(is.na(x$om[top]))) NA_real_ else sum(x$om[top] * w) / max(sum(w), 1e-9)
  tex15 <- .cpcs_texture(x, top[1L])
  min15 <- .cpcs_om_min(tex15)
  om15_ok <- if (is.na(om15)) NA else if (!is.na(min15)) om15 >= min15 else
    if (om15 >= 1) TRUE else if (om15 < 0.5) FALSE else NA
  # marked horizons (OM at least their texture minimum, 0.75 % when the
  # texture is not known) from the top, over half the depth to the C or
  # more, with no drop to a third or less from one to the next, nor below
  # the A (Hugo, 2026-11-01)
  crow <- rows[x$role[rows] %in% c("C", "R", "G")]
  bots <- x$dbot[rows][!is.na(x$dbot[rows])]
  dc <- if (length(crow)) min(x$dtop[crow]) else if (length(bots)) max(bots) else NA_real_
  marked <- integer(0); gradual <- TRUE; stop_na <- FALSE
  for (k in seq_along(rows)) {
    i <- rows[k]
    if (x$rock[i]) break
    mn <- .cpcs_om_min(.cpcs_texture(x, i)); if (is.na(mn)) mn <- 0.75
    if (is.na(x$om[i])) { stop_na <- TRUE; if (!length(marked) || is.na(dc) || x$dtop[i] < dc) gradual <- NA; break }
    if (x$om[i] < mn) break
    if (length(marked) && x$om[i] <= x$om[marked[length(marked)]] / 3) { gradual <- FALSE; break }
    marked <- c(marked, i)
  }
  gradual <- .cpcs_and(gradual, .cpcs_not(.cpcs_sharp_drop(x)))
  half <- if (is.na(dc) || dc <= 0) NA else {
    inside <- marked[x$dtop[marked] < dc]
    mth <- sum(vapply(inside, function(i) (if (is.na(x$dbot[i])) dc else min(x$dbot[i], dc)) - x$dtop[i], 0))
    if (mth >= dc / 2) TRUE else if (stop_na) NA else FALSE
  }
  # the complex saturated: S/T of 50 % or more in the upper horizons, rising
  # to 80 % or more in the B, read in the larger part of its first 100 cm
  # (p. 39; a calcareous horizon counts as saturated)
  bs <- .cpcs_bs(x); cc <- .cpcs_num(h, "caco3_pct")
  calc <- ifelse(!is.na(cc), cc > 0, x$des$ca %in% TRUE)
  sat50 <- ifelse(calc, TRUE, ifelse(is.na(bs), NA, bs >= 50))
  sat80 <- ifelse(calc, TRUE, ifelse(is.na(bs), NA, bs >= 80))
  a <- .cpcs_a_rows(x); if (!length(a)) a <- rows[1L]
  b <- .cpcs_b_rows(x)
  st50 <- .cpcs_and(.cpcs_all(sat50[a]), if (length(b)) .cpcs_larger_part_b(x, sat80, b) else
    .cpcs_all(sat50[rows[is.na(dc) | x$dtop[rows] < dc]]))
  passed <- .cpcs_and(om15_ok, half, gradual, st50)
  .cpcs_result("isohumic_soils_cpcs", passed, marked,
               list(om_top_15 = om15, om_top_15_ok = om15_ok, marked_half_of_solum = half,
                    gradual_decrease = gradual, saturation_50_A_80_B = st50),
               if (is.na(passed)) c("oc_pct", "clay_pct", "bs_pct") else character(0), "VI, p. 39")
}

#' @rdname cpcs1967_classes
#' @export
calcimagnesic_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  chem <- .cpcs_v_chemistry(x)
  # the isohumic soils, which class V excludes (p. 35), are keyed before it,
  # so the fall of organic matter is not tested here (Hugo, 2026-11-01)
  passed <- .cpcs_and(.cpcs_not(ty$b_enriched), chem)
  .cpcs_result("calcimagnesic_soils_cpcs", passed, .cpcs_a_rows(x),
               list(no_enriched_B = .cpcs_not(ty$b_enriched), ph_and_ca_mg_saturation = chem),
               if (is.na(passed)) c("ph_h2o", "ca_cmol", "mg_cmol", "cec_cmol") else character(0),
               "V, p. 35")
}

#' @rdname cpcs1967_classes
#' @export
brunified_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  # the developed soils that remain: a B, a (B) or an A2 (Hugo, 2026-11-01)
  passed <- .cpcs_or(ty$b_any, ty$e)
  .cpcs_result("brunified_soils_cpcs", passed, .cpcs_b_rows(x),
               list(B_present = ty$b_any, A2_present = ty$e, horizons_from = ty$from),
               if (is.na(passed)) "designation" else character(0), "VII, p. 45")
}
