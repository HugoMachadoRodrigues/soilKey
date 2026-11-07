# =============================================================================
# v0.9.363 -- example profiles of the 138 subtypes of the terrestrial KA5
# types (pp. 202-237), for make_ka5_example(subtype = ...). Each profile is
# written in the KA5 notation, from a printed formula of its subtype with
# depths inside its windows; its data are soilKey's typical values for each
# horizon (.ka5_hz_data()), so that the data twin (route = "data") can be
# keyed where the data route reaches the subtype. They are not real
# profiles.
# =============================================================================

# The profiles: "designation top-bottom" joined by " / "; the extra columns
# and site fields where a subtype asks for them.
.KA5_SUBTYPE_PROFILES <- list(
  # F, O
  FFn = "O 0-8 / mC 8-30",
  FSn = "O 0-5 / xC+O 5-40 / mC 40-60",
  OOn = "Ai 0-1 / mC 1-30",
  OOp = "mAi 0-2 / mC 2-30",
  OLn = "Ai 0-2 / lC 2-100",
  # R: Ranker, Regosol, Rendzina, Pararendzina
  RNn = list("Ah< 0-20 / imC 20-60", bs_pct = c(30, NA)),
  RNr = list("Ah> 0-20 / imC 20-60", bs_pct = c(70, NA)),
  `OO-RN` = "Aih 0-2 / imC 2-40",
  `OL-RN` = "Aih 0-2 / ilCv 2-15 / imC 15-40",
  `BB-RN` = "Ah 0-15 / Bv-ilCv 15-22 / imC 22-50",
  `PP-RN` = "Ahe 0-8 / Ae 8-12 / Bhs-ilCv 12-20 / imC 20-50",
  RQn = list("Ah< 0-20 / ilC 20-100", bs_pct = c(30, NA)),
  RQr = list("Ah> 0-20 / ilC 20-100", bs_pct = c(70, NA)),
  `OL-RQ` = "Aih 0-2 / ilC 2-100",
  `BB-RQ` = "Ah 0-20 / Bv-ilCv 20-35 / ilC 35-100",
  `PP-RQ` = "Ahe 0-8 / Ae 8-15 / Bhs-ilCv 15-25 / ilC 25-100",
  `SS-RQ` = "Ah 0-20 / ilC 20-50 / ilC-Sw 50-70 / IIilC-Sd 70-100",
  `GG-RQ` = "Ah 0-20 / ilC 20-50 / Go 50-90 / Gr 90-150",
  RRn = list("Ah> 0-20 / cmC 20-60", bs_pct = c(90, NA)),
  RRs = list("Ah< 0-20 / cmC 20-60", bs_pct = c(30, NA)),
  `OO-RR` = "Aih 0-2 / cmC 2-40",
  `OL-RR` = "Aih 0-2 / clC 2-60",
  `BB-RR` = "Ah 0-20 / Bv+cCv 20-30 / cC 30-60",
  `CF-RR` = list("Ah 0-15 / Tv+cCv 15-30 / cC 30-60", terra_material_pct = c(NA, 60, NA)),
  `GG-RR` = "Ah 0-20 / cCv 20-50 / Go 50-90 / Gr 90-150",
  RZn = list("Ah> 0-25 / elC 25-150", bs_pct = c(90, NA)),
  RZs = list("Ah< 0-25 / elC 25-150", bs_pct = c(30, NA)),
  `OO-RZ` = "Aih 0-2 / emC 2-40",
  `OL-RZ` = "Aih 0-2 / elC 2-100",
  `BB-RZ` = "Ah 0-20 / Bv-eCv 20-30 / elC 30-100",
  `SS-RZ` = "Ah 0-20 / eCv 20-50 / C-Sw 50-70 / IIC-Sd 70-100",
  `GG-RZ` = "Ah 0-20 / eCv 20-50 / Go 50-90 / Gr 90-150",
  # T
  TTn = "Axh 0-60 / Axh+lCc 60-80 / lCc 80-150",
  `DD-TT` = "Axh 0-20 / P-Axh 20-50 / P-Axh+lC 50-70 / lC 70-120",
  `BB-TT` = "Axh 0-30 / Bv-Axh 30-50 / Bv-Axh+Bv 50-70 / C 70-120",
  `LL-TT` = "Axh 0-20 / Al-Axh 20-40 / Bt-Axh 40-60 / Axh+lC 60-80 / lC 80-120",
  `SS-TT` = "Axh 0-30 / Sw-Axh 30-45 / Axh-Sw 45-60 / Sw 60-75 / IISd 75-120",
  `GG-TT` = "Axh 0-30 / Go-Axh 30-50 / Go 50-90 / Gr 90-150",
  TCn = "Acxh 0-55 / Acxh+elCc 55-75 / elCc 75-150",
  `DD-TC` = "Acxh 0-20 / P-Acxh 20-50 / P-Acxh+elCc 50-70 / eCc 70-120",
  `BB-TC` = "Acxh 0-30 / Bv-Acxh 30-50 / Acxh+elCc 50-70 / eCc 70-120",
  `LL-TC` = "Al-Acxh 0-20 / Bt-Acxh 20-45 / Acxh+elCc 45-65 / eCc 65-120",
  `GG-TC` = "Acxh 0-30 / eGco-Acxh 30-50 / eGco 50-90 / eGr 90-150",
  # D
  DDn = "Ah 0-15 / P 15-60 / eC 60-100",
  DDh = "Ah 0-20 / Ah-P 20-45 / P 45-70 / eC 70-100",
  `RN-DD` = "Ah 0-15 / ilCv-P 15-35 / imC 35-60",
  `RQ-DD` = "Ah 0-15 / ilCv-P 15-30 / ilCv 30-60 / imC 60-80",
  `RZ-DD` = "Ah 0-15 / elCv-P 15-40 / eC 40-100",
  `BB-DD` = "Ah 0-15 / Bv-P 15-40 / P 40-60 / eC 60-100",
  `SS-DD` = "Ah 0-20 / Sw-P 20-45 / P-Sd 45-70 / eC 70-100",
  `GG-DD` = "Ah 0-20 / Go-P 20-45 / P-Go 45-70 / Go 70-90 / Gr 90-150",
  # B
  BBn = "Ah 0-15 / Bv 15-50 / C 50-100",
  BBc = "Ah 0-15 / Bcv 15-50 / Cc 50-100",
  BBh = "Ah 0-20 / Ah-Bv 20-45 / Bv 45-70 / C 70-100",
  BBl = "Ah 0-15 / Ah-Bfv 15-45 / IIC 45-100",
  `DD-BB` = "Ah 0-10 / Bv 10-25 / IIBv-P 25-50 / IIP 50-70 / IIeC 70-100",
  `LL-BB` = list("Ah 0-20 / Al-Bv 20-35 / Btv 35-90 / C 90-130", clay_pct = c(15, 15, 18, 16), silt_pct = 60),
  `LF-BB` = list("Ah 0-15 / Bv 15-40 / Ael 40-80 / Ael+Bt 80-100 / Bt 100-130 / C 130-150",
                 clay_pct = c(12, 13, 12, 18, 26, 20), silt_pct = 60),
  `PP-BB` = "Ahe 0-5 / Ae 5-8 / Bhs 8-12 / Bv 12-50 / C 50-100",
  `SS-BB` = "Ah 0-20 / Bv 20-45 / Bv-Sw 45-60 / IISd 60-100",
  `GG-BB` = "Ah 0-20 / Go-Bv 20-45 / Go 45-90 / Gr 90-150",
  # L
  LLn = "Ah 0-25 / Al 25-45 / Bt 45-90 / C 90-130",
  LLd = "Ah 0-25 / Al 25-45 / Bv+Bbt 45-80 / ilCv+Bbt 80-100 / C 100-130",
  LLh = "Ah 0-20 / Alh 20-45 / Ah-Bt 45-80 / C 80-120",
  `TT-LL` = "Ah 0-20 / Axh-Al 20-40 / Axh-Bt 40-70 / C 70-110",
  `BB-LL` = "Ah 0-20 / Bv-Al 20-40 / Bt 40-80 / C 80-120",
  `PP-LL` = "Ahe 0-5 / Ae 5-8 / Al-Bhs 8-13 / Bt 13-60 / C 60-100",
  `CF-LL` = list("Ah 0-20 / Al 20-35 / Bt 35-50 / IIBt-Tv 50-70 / IITv 70-90 / IIcC 90-120"),
  `SS-LL` = "Ah 0-20 / Sw-Al 20-40 / Sd-Bt 40-60 / Sd 60-90 / C 90-120",
  `GG-LL` = "Ah 0-20 / Al 20-40 / Bt 40-60 / Bt-Go 60-90 / Gr 90-150",
  LFn = "Ah 0-20 / Ael 20-45 / Ael+Bt 45-60 / Bt 60-100 / C 100-130",
  LFd = "Ah 0-20 / Ael 20-40 / Ael+Bt 40-55 / Bv+Bbt 55-80 / C 80-110",
  `BB-LF` = "Ah 0-20 / Bv-Ael 20-40 / Ael+Bt 40-60 / Bt 60-100 / C 100-130",
  `PP-LF` = "Ahe 0-5 / Ae 5-8 / Ael-Bhs 8-13 / Ael+Bt 13-40 / Bt 40-80 / C 80-110",
  `SS-LF` = "Ah 0-20 / Sw-Ael 20-40 / Sd-Bt 40-60 / Sd 60-90 / C 90-120",
  `GG-LF` = "Ah 0-20 / Ael 20-40 / Bt 40-60 / Bt-Go 60-90 / Gr 90-150",
  # P
  PPn = "Ahe 0-10 / Ae 10-25 / Bsh 25-30 / Bhs 30-50 / C 50-100",
  PPe = "Ahe 0-5 / Ae 5-20 / Bs 20-45 / C 45-100",
  PPh = "Ahe 0-5 / Ae 5-20 / Bh 20-35 / C 35-100",
  PPd = "Ahe 0-5 / Ae 5-20 / Bh 20-30 / Bbms 30-32 / Bs 32-50 / C 50-100",
  `BB-PP` = "Ahe 0-5 / Ae 5-15 / Bsh 15-20 / Bhs 20-35 / Bv 35-60 / C 60-100",
  `LL-PP` = "Ahe 0-5 / Ae 5-15 / Bsh 15-20 / Bhs 20-35 / Al 35-50 / Bt 50-80 / C 80-110",
  `SS-PP` = "Ahe 0-5 / Ae 5-15 / Bsh 15-20 / Bhs 20-45 / Sw 45-60 / IISd 60-100",
  `SG-PP` = "Srw-Ahe 0-10 / Srw-Ae 10-25 / Sd-Bhms 25-40 / C 40-80",
  `GG-PP` = "Ahe 0-5 / Ae 5-15 / Bsh 15-20 / Bhs 20-45 / Bhs-Go 45-70 / Gr 70-150",
  `YK-PP` = "Ahe 0-5 / Ae 5-15 / Bsh 15-20 / Bhs 20-35 / M 35-60 / IIfAh 60-80 / IIBv 80-110",
  `YE-PP` = "Ahe 0-5 / Ae 5-15 / Bsh 15-20 / Bhs 20-35 / E 35-60 / IIfAh 60-80 / IIBhs 80-100",
  # C
  CFn = "Ah 0-10 / Tv 10-45 / cC 45-80",
  CFc = "Ah 0-10 / Tcv 10-45 / cC 45-80",
  `BB-CF` = list("Ah 0-10 / Bv-Tv 10-45 / cC 45-80", clay_pct = c(40, 55, NA)),
  `LL-CF` = list("Ah 0-10 / Al 10-25 / IIBt-Tv 25-50 / IITv 50-70 / IIC 70-100",
                 carbonate_whole_soil_pct = c(NA, NA, NA, NA, 95), caco3_pct = c(0, 0, 0, 0, NA)),
  `SS-CF` = list("Ah 0-10 / Sw-Tv 10-40 / Tv-Sd 40-70 / C 70-100",
                 carbonate_whole_soil_pct = c(NA, NA, NA, 95), caco3_pct = c(0, 0, 0, NA)),
  CRn = "Ah 0-10 / Tu 10-40 / cC 40-60",
  # S
  SSn = "Ah 0-20 / Sw 20-40 / IISd 40-100",
  SSc = "Ah 0-20 / Scw 20-40 / IISd 40-100",
  SSg = list("Ah 0-20 / sSw 20-40 / IIsSd 40-100", site = list(slope_pct = 12)),
  SSh = "Ah 0-20 / Ah-Sw 20-45 / Sw 45-60 / IISd 60-100",
  SSm = "Sw-Aa 0-15 / Sw 15-35 / IISd 35-100",
  `TT-SS` = "Axh 0-30 / Axh-Sw 30-45 / Sw 45-60 / IISd 60-100",
  `DD-SS` = "Ah 0-20 / P-Sw 20-40 / P-Sd 40-80 / C 80-100",
  `BB-SS` = "Ah 0-15 / Bv 15-30 / Bv-Sw 30-45 / Sw 45-60 / IISd 60-100",
  `LL-SS` = "Ah 0-20 / Al 20-30 / Al-Sw 30-50 / IIBt-Sd 50-100",
  `LF-SS` = list("Ah 0-20 / Ael 20-30 / Ael-Sw 30-50 / Bt-Sd 50-100", clay_pct = c(12, 12, 12, 28), silt_pct = 60),
  `PP-SS` = "Ahe 0-5 / Ae 5-15 / Bhs 15-25 / Sw 25-45 / IISd 45-100",
  `CF-SS` = "Ah 0-15 / Ah-Sw 15-35 / IITv-Sd 35-70 / IIcC 70-100",
  `YK-SS` = "Ah 0-20 / M 20-30 / M-Sw 30-50 / IISd 50-100",
  `YE-SS` = "Ah 0-20 / E 20-30 / E-Sw 30-50 / IISd 50-100",
  `GG-SS` = "Ah 0-20 / Sw 20-40 / IISd 40-60 / IIGo 60-90 / IIGr 90-150",
  SHn = "Ah 0-20 / Sg 20-90 / IIC 90-120",
  SHh = "Ah 0-20 / Ah-Sg 20-45 / Sg 45-90 / IIC 90-120",
  `BB-SH` = "Ah 0-20 / Bv 20-35 / Sg 35-90 / IIC 90-120",
  `LL-SH` = "Ah 0-20 / Al-Sg 20-35 / Bt-Sg 35-70 / Bt 70-100",
  `LF-SH` = list("Ah 0-20 / Ael-Sg 20-35 / Bt-Sg 35-70 / Bt 70-100", clay_pct = c(12, 12, 28, 26), silt_pct = 60),
  `GG-SH` = "Ah 0-20 / Sg 20-50 / Go-Sg 50-70 / IIGo 70-90 / IIGr 90-120",
  SGn = "Sw-Ah 0-15 / Srw 15-40 / IISrd 40-100",
  SGd = "Sw-Ahe 0-10 / Srw 10-35 / Bbms-Srd 35-37 / C 37-80",
  SGm = "Sw-Aa 0-15 / Srw 15-40 / IISrd 40-100",
  `HN-SG` = "nH 0-20 / IIAh-Srw 20-30 / IISrw 30-50 / IIISrd 50-100",
  `HH-SG` = "hH 0-20 / IIAh-Srw 20-30 / IISrw 30-50 / IIISrd 50-100",
  `GG-SG` = "Sw-Ah 0-15 / Srw 15-40 / IISrd 40-60 / IIIGo 60-100",
  # X
  XXn = "Ah 0-15 / Yo 15-30 / Yr 30-100",
  XXt = "Yo-Ai 0-5 / Yo 5-20 / Yr 20-100",
  XXx = "Ah 0-15 / Yo 15-45 / Yr 45-100",
  XXu = "Ah 0-15 / Yr 15-100",
  # Y
  YKn = "Ah 0-30 / M 30-70 / IIfAh 70-90 / IIBv 90-120",
  `PP-YK` = "Ahe 0-5 / Ae 5-8 / Bhs 8-12 / M 12-60 / IIfAh 60-80 / IIBv 80-110",
  `SS-YK` = "Ah 0-20 / M 20-45 / M-Sw 45-60 / IISd 60-100",
  `GG-YK` = "Ah 0-20 / M 20-45 / M-Go 45-60 / IIGo 60-90 / IIGr 90-150",
  YEn = "Ah 0-30 / E 30-70 / IIfAh 70-80 / IIBhs 80-100",
  `PP-YE` = "Ahe 0-5 / Ae 5-8 / Bhs 8-12 / E 12-60 / IIfAh 60-80 / IIBhs 80-100",
  `SS-YE` = "Ah 0-20 / E 20-45 / E-Sw 45-60 / IISw 60-70 / IISd 70-100",
  `GG-YE` = "Ah 0-20 / E 20-45 / E-Go 45-60 / IIGo 60-90 / IIGr 90-150",
  YOn = "Ap 0-30 / Ex 30-60 / C 60-100",
  YYn = "R-Ap 0-30 / R 30-70 / C 70-120",
  `PP-YU` = "R-Ap 0-30 / R+Bhs 30-60 / C 60-100",
  `LL-YU` = "R-Ap 0-30 / R+Bt 30-60 / C 60-100",
  `LF-YU` = "R-Ap 0-30 / R+Ael+Bt 30-60 / C 60-100",
  `GG-YU` = "R-Ap 0-30 / Go+R 30-50 / G 50-100"
)

# The rows of a profile string: designations and depths.
.ka5_parse_profile_string <- function(s) {
  it <- trimws(strsplit(s, "/", fixed = TRUE)[[1L]])
  m <- regmatches(it, regexec("^(\\S+)\\s+([0-9.]+)-([0-9.]*)$", it))
  if (any(lengths(m) != 4L)) rlang::abort(sprintf("A KA5 example profile does not read: '%s'.", s))
  top <- as.numeric(vapply(m, `[`, "", 3L))
  bot <- suppressWarnings(as.numeric(vapply(m, `[`, "", 4L)))
  bot[is.na(bot)] <- top[is.na(bot)] + 40
  list(des = vapply(m, `[`, "", 2L), top = top, bot = bot)
}

#' soilKey's typical values of a KA5 horizon (for the example profiles): by
#' the carrying part, then the other parts and the prefixes. Not a rule of
#' the KA5.
#' @noRd
.ka5_hz_data <- function(d) {
  r <- .ka5_parse_one(d)
  car <- r$code %||% NA_character_
  pre <- r$prefixes %||% character(0)
  with <- sub("^[a-z]*", "", r$with %||% character(0))
  ms <- substr(sub("^[a-z]*", "", car), 1L, 1L)
  o <- list(oc_pct = 0.2, clay_pct = 15, silt_pct = 40, munsell_hue_moist = "10YR", munsell_value_moist = 5,
            munsell_chroma_moist = 4, caco3_pct = 0, redoximorphic_features_pct = 0, reduction_colours_pct = 0,
            bleached_pct = 0)
  set <- function(...) { a <- list(...); for (k in names(a)) o[[k]] <<- a[[k]] }
  col <- function(h, v, c) set(munsell_hue_moist = h, munsell_value_moist = v, munsell_chroma_moist = c)
  s_feat <- function(lvl) switch(lvl,
    w = set(redoximorphic_features_pct = 30, bleached_pct = 55, ksat_mm_h = 20, air_capacity_pct = 8, rh_value = 17),
    d = set(redoximorphic_features_pct = 40, bleached_pct = 30, ksat_mm_h = 1, air_capacity_pct = 2, clay_pct = 30),
    part = set(redoximorphic_features_pct = 15, bleached_pct = 25))
  if (is.na(car)) return(o)
  if (ms == "A") {
    if (car %in% c("Ah", "Ap", "Aih")) { set(oc_pct = if (car == "Aih") 1.5 else 2.5); col("10YR", 3, 2) }
    else if (car == "Ai") set(oc_pct = 0.2)
    else if (car == "Ahe") { set(oc_pct = 3, clay_pct = 3, silt_pct = 5); col("10YR", 4, 1) }
    else if (car == "Ae") { set(oc_pct = 0.3, clay_pct = 3, silt_pct = 5); col("10YR", 6, 1) }
    else if (car == "Al") { set(oc_pct = 0.4, clay_pct = 15, silt_pct = 60) }
    else if (car == "Alh") { set(oc_pct = 1.5, clay_pct = 14, silt_pct = 60); col("10YR", 4, 3) }
    else if (car == "Ael") { set(oc_pct = 0.3, clay_pct = 12, silt_pct = 60); col("10YR", 7, 2) }
    else if (grepl("^Ac?x", car)) {
      set(oc_pct = 2.5, clay_pct = 22, silt_pct = 65, bs_pct = 95, worm_holes_pct = 10, structure_type = "granular")
      col("10YR", 2, 1)
      if (grepl("^Acx", car)) set(secondary_carbonates_pct = 2, caco3_pct = 5)
    } else if (car == "Aa") { set(oc_pct = 12); col("10YR", 2, 1) }
  } else if (ms == "B") {
    if (grepl("t", car)) {
      set(oc_pct = 0.3, clay_pct = if (grepl("v", car)) 18 else 24, silt_pct = 60,
          clay_films_amount = if (grepl("v", car)) "few" else "common")
      col("7.5YR", 4, 4)
    } else if (grepl("[hs]", car)) {
      if (car == "Bh") { set(oc_pct = 3, c_pyro_pct = 2, fe_pyro_pct = 0.1); col("5YR", 2, 2) }
      else if (car == "Bs") { set(oc_pct = 0.5, c_pyro_pct = 0.2, fe_pyro_pct = 0.3); col("5YR", 4, 8) }
      else if (car == "Bsh") { set(oc_pct = 2, c_pyro_pct = 1.5, fe_pyro_pct = 0.3); col("5YR", 3, 3) }
      else { set(oc_pct = 1, c_pyro_pct = 0.9, fe_pyro_pct = 0.2); col("7.5YR", 4, 6) }
      set(clay_pct = 3, silt_pct = 5)
      if (grepl("m", car)) set(cementation_class = "strongly cemented")
    } else if (car == "Bfv") { set(oc_pct = 2, bulk_density_g_cm3 = 0.7, total_porosity_pct = 70); col("7.5YR", 3, 4) }
    else { set(oc_pct = 0.3); col("7.5YR", 4, 6); if (grepl("c", car)) set(secondary_carbonates_pct = 2, caco3_pct = 3) }
  } else if (ms == "C") {
    set(oc_pct = 0.1, clay_pct = 10, rock_diggable = !("m" %in% pre))
    if ("m" %in% pre) set(oc_pct = NA, clay_pct = NA, silt_pct = NA)
    if ("x" %in% pre) set(coarse_fragments_pct = 98, rock_diggable = NA)
    if ("e" %in% pre) set(caco3_pct = 15)
    if ("c" %in% pre) set(caco3_pct = 90, carbonate_whole_soil_pct = 95)
    if (grepl("c$", car)) set(secondary_carbonates_pct = 5, caco3_pct = max(o$caco3_pct, 15))
  } else if (ms == "G") {
    if (grepl("^Gr", car)) { set(redoximorphic_features_pct = 1, reduction_colours_pct = 99, rh_value = 12); col("5GY", 5, 1) }
    else if (car == "G") { set(redoximorphic_features_pct = 10, reduction_colours_pct = 60); col("5Y", 5, 2) }
    else { set(redoximorphic_features_pct = 20, reduction_colours_pct = 5); col("2.5Y", 5, 3) }
    if (grepl("c", car)) set(secondary_carbonates_pct = 3, caco3_pct = 8)
  } else if (ms == "S") {
    if (grepl("^S[ce]?r?w$|^Sw$", car)) s_feat("w") else if (grepl("d$", car)) s_feat("d")
    else if (car == "Sg") set(redoximorphic_features_pct = 40, bleached_pct = 45, air_capacity_pct = 2, clay_pct = 12,
                              silt_pct = 60, sand_ffs_pct = 10)
    if (grepl("r", car)) set(reduction_colours_pct = if (grepl("d$", car)) 20 else 5,
                             water_saturation_days = if (grepl("d$", car)) 330 else 250)
    if (grepl("c", car)) set(secondary_carbonates_pct = 2, caco3_pct = 3)
    col("10YR", 6, 2)
  } else if (ms == "P") {
    set(clay_pct = 55, silt_pct = 30, slickensides = "common", structure_type = "prismatic", clay_films_amount = "none")
  } else if (ms == "T") {
    set(clay_pct = 70, silt_pct = 20, structure_type = "subangular blocky", clay_films_amount = "none")
    col(if (car == "Tu") "2.5YR" else "7.5YR", 4, 6)
    if (grepl("c", car)) set(secondary_carbonates_pct = 2)
  } else if (ms == "M") {
    set(oc_pct = 1.2, layer_origin = "colluvic", stratification_pattern = "continuous")
  } else if (ms == "E") {
    if (car == "Ex") set(oc_pct = 2, p2o5_al_mg_kg = 300, worm_holes_pct = 5)
    else set(oc_pct = 1.5, clay_pct = 5, silt_pct = 10, p2o5_al_mg_kg = 250)
  } else if (ms == "R") {
    set(oc_pct = 1)
  } else if (ms == "Y") {
    if (car == "Yr") set(soil_gas_ch4_co2_pct = 40, reduction_colours_pct = 90)
    else set(soil_gas_ch4_co2_pct = 20, redoximorphic_features_pct = 10, reduction_colours_pct = 5)
  } else if (ms == "H") {
    set(oc_pct = 45, clay_pct = NA, silt_pct = NA, peat_moor_type = if ("h" %in% pre) "high-moor" else "low-moor")
  } else if (ms == "O") {
    set(oc_pct = 40, clay_pct = NA, silt_pct = NA, fine_organic_matter_pct = 60)
  }
  # the other parts of a transitional or interfingering horizon
  for (w in with) {
    wm <- substr(w, 1L, 1L)
    if (wm == "S") s_feat("part")
    if (wm == "G") set(redoximorphic_features_pct = max(o$redoximorphic_features_pct, 3))
    if (w %in% c("Ah", "Axh", "Acxh") && !(ms %in% c("A"))) set(oc_pct = 1.5)
    if (w == "P") set(clay_pct = max(o$clay_pct %||% 0, 50, na.rm = TRUE))
    if (wm == "T") set(clay_pct = max(o$clay_pct %||% 0, 55, na.rm = TRUE))
    if (w == "M") set(layer_origin = "colluvic", stratification_pattern = "continuous")
    if (w == "E") set(p2o5_al_mg_kg = 250)
  }
  if (!is.na(o$clay_pct %||% NA) && !is.na(o$silt_pct %||% NA)) o$sand_pct <- max(0, 100 - o$clay_pct - o$silt_pct)
  o
}

# The site of an example profile from its horizons: the water table under a
# G, perched water over an S, the record of a deep cultivation or a gas.
.ka5_example_site <- function(code, des, top) {
  car <- vapply(des, function(d) .ka5_parse_one(d)$code %||% NA_character_, "")
  ms <- substr(sub("^[a-z]*", "", car), 1L, 1L)
  parts <- unlist(lapply(des, function(d) c(.ka5_parse_one(d)$code, sub("^[a-z]*", "", .ka5_parse_one(d)$with))))
  site <- list()
  if (any(ms %in% "G")) {
    gr <- which(grepl("^Gr", car))
    site <- c(site, list(groundwater_kind = "observed",
                         groundwater_depth_cm = if (length(gr)) top[gr[1L]] else top[which(ms %in% "G")[1L]] + 20))
  } else if (any(substr(parts, 1L, 1L) %in% "S")) site$groundwater_kind <- "perched"
  if (any(ms %in% "Y")) site$reductgas_source <- "refuse_sludge_harbour_mud"
  ty <- .ka5_subtype_entry(code)$type
  if (ty == "YY") site <- c(site, list(deep_cultivation_kind = "repeated_trenching", mechanical_mixing_depth_cm = 70, land_use = "vineyard"))
  if (ty == "YU") site <- c(site, list(deep_cultivation_kind = "single_deep_ploughing", mechanical_mixing_depth_cm = 60, land_use = "arable"))
  if (ty == "YE") site <- c(site, list(deep_cultivation_kind = "plaggen", mechanical_mixing_depth_cm = 30))
  if (ty == "YO") site <- c(site, list(deep_cultivation_kind = "garden", mechanical_mixing_depth_cm = 30, land_use = "arable"))
  site
}

#' The example profile of a terrestrial KA5 subtype
#' @param code The subtype code as printed (BBn, GG-BB ...).
#' @param route "notation" or "data".
#' @noRd
.ka5_subtype_example <- function(code, route = c("notation", "data")) {
  route <- match.arg(route)
  spec <- .KA5_SUBTYPE_PROFILES[[code]]
  if (is.null(spec)) rlang::abort(sprintf("No KA5 subtype example '%s' (the terrestrial subtypes have one each).", code))
  if (is.character(spec)) spec <- list(spec)
  p <- .ka5_parse_profile_string(spec[[1L]])
  n <- length(p$des)
  rows <- lapply(p$des, .ka5_hz_data)
  cols <- unique(unlist(lapply(rows, names)))
  h <- list(designation = p$des, top_cm = p$top, bottom_cm = p$bot)
  for (k in cols) {
    vals <- lapply(rows, function(r) if (is.null(r[[k]])) NA else r[[k]])
    h[[k]] <- if (any(vapply(vals, is.character, NA))) vapply(vals, function(z) if (is.na(z)) NA_character_ else as.character(z), "")
              else if (all(vapply(vals, is.logical, NA))) vapply(vals, as.logical, NA)
              else vapply(vals, as.numeric, 0)
  }
  extra <- spec[-1L]
  site_extra <- extra$site %||% list()
  extra$site <- NULL
  for (k in names(extra)) h[[k]] <- if (length(extra[[k]]) == 1L) rep(extra[[k]], n) else extra[[k]]
  site <- utils::modifyList(list(id = paste0("ka5-", code), country = "DE", designation_system = "ka5"),
                            utils::modifyList(.ka5_example_site(code, p$des, p$top), site_extra))
  if (route == "data") {
    h$designation <- rep(NA_character_, n)
    site$designation_system <- NULL
  }
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}

# The subtype examples that key to their subtype on the data as well
# (v0.9.363; the others need the notation: transitions and interfingering
# the data key does not build, prefixes it does not write).
.KA5_DATA_ROUTE_SUBTYPES <- c("FFn", "OOn", "OLn", "RNn", "RQn", "SS-RQ", "GG-RQ", "RRn", "RRs", "RZn", "RZs", "TTn",
                              "BB-TT", "GG-TT", "TCn", "BBn", "BBl", "GG-BB", "LLn", "GG-LL", "LFn", "GG-LF", "PPn",
                              "PPe", "PPh", "LL-PP", "SS-PP", "GG-PP", "YK-PP", "CFn", "CFc", "CRn", "SSn", "SSc",
                              "SSg", "PP-SS", "SHn", "SHh", "BB-SH", "XXn", "XXt", "XXx", "XXu", "YKn", "PP-YK",
                              "SS-YK", "GG-YK", "YEn", "GG-YE", "YOn", "YYn")
