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

# v0.9.364: the example profiles of the 85 subtypes of the semiterrestrial,
# subhydric and Moore types (pp. 238-262) and of soilKey's two open
# subtypes of Q22, written as the terrestrial ones; their data come from
# .ka5_hz_data(wet = TRUE) and their site from the prefixes (tidal, floodplain,
# slope or spring water, a drained moor).
.KA5_SUBTYPE_PROFILES_WET <- list(
  # A: Auenboeden
  AOn = "aAi 0-3 / alC 3-90 / aG 90-150",
  `GG-AO` = "aAi 0-3 / alC 3-50 / aGo 50-90 / aG 90-150",
  AQn = "aAh 0-20 / ailC 20-90 / aG 90-150",
  `GG-AQ` = "aAh 0-20 / ailC 20-50 / aGo 50-90 / aG 90-150",
  AZn = "aeAh 0-20 / aelC 20-90 / aG 90-150",
  `GG-AZ` = "aeAh 0-20 / aelC 20-50 / aGo 50-90 / aG 90-150",
  ATn = "aAxh 0-50 / aM 50-90 / aG 90-150",
  `GG-AT` = "aAxh 0-45 / aGo 45-90 / aG 90-150",
  ABn = "aAh 0-20 / aM 20-90 / aG 90-150",
  `AQ-AB` = "aAh 0-15 / ailC 15-35 / IIaM 35-90 / IIaG 90-150",
  `SS-AB` = "aAh 0-20 / aM 20-45 / aSw 45-60 / aSd 60-90 / aG 90-150",
  `GG-AB` = "aAh 0-20 / aM 20-50 / aGo 50-90 / aG 90-150",
  # G: Gleye
  GGn = "Ah 0-25 / Go 25-60 / Gr 60-120",
  GGx = "Ah 0-25 / Go 25-140",
  GGe = "Ah 0-25 / Go 25-45 / Gso 45-80 / Gr 80-120",
  GGi = "Ah 0-25 / Gw 25-60 / Gr 60-120",
  GGw = "Ah 0-25 / Go 25-90 / Gor 90-130 / Gr 130-150",
  GGc = "Ah 0-25 / Gco 25-60 / Gcr 60-120",
  GGh = "Ah 0-25 / Ah-Go 25-45 / Go 45-70 / Gr 70-120",
  GGg = "Ah 0-25 / sGo 25-60 / sGr 60-120",
  GGq = "Ah 0-25 / qGo 25-60 / qGr 60-120",
  GGa = "aAh 0-25 / aGo 25-60 / aGr 60-120",
  `RQ-GG` = "Ah 0-20 / ilC 20-35 / Go 35-90 / Gr 90-150",
  `RR-GG` = "Ah 0-20 / cC 20-35 / Go 35-90 / Gr 90-150",
  `RZ-GG` = "Ah 0-20 / eC 20-35 / Go 35-90 / Gr 90-150",
  `TT-GG` = "Axh 0-30 / Axh-Go 30-50 / Go 50-90 / Gr 90-150",
  `DD-GG` = "Ah 0-20 / P-Go 20-45 / Go 45-90 / Gr 90-150",
  `BB-GG` = "Ah 0-20 / Bv-Go 20-45 / Go 45-90 / Gr 90-150",
  `LL-GG` = "Ah 0-20 / Al 20-30 / Bt-Go 30-50 / Go 50-90 / Gr 90-150",
  `LF-GG` = list("Ah 0-15 / Ael 15-25 / Ael+Bt 25-35 / Bt-Go 35-55 / Go 55-90 / Gr 90-150",
                 clay_pct = c(12, 12, 18, 26, 26, 20), silt_pct = 60),
  `PP-GG` = "Ahe 0-5 / Ae 5-15 / Bhs-Go 15-40 / Go 40-90 / Gr 90-150",
  `SS-GG` = "Ah 0-20 / Sw 20-30 / IISd-Go 30-50 / IIGo 50-90 / IIGr 90-150",
  `YK-GG` = "Ah 0-20 / M-Go 20-45 / IIGo 45-90 / IIGr 90-150",
  `YE-GG` = "Ah 0-20 / E-Go 20-45 / IIGo 45-90 / IIGr 90-150",
  `AO-GG` = "aAi 0-3 / alC 3-30 / aGo 30-60 / aGr 60-120",
  `AQ-GG` = "aAh 0-20 / ailC 20-30 / aGo 30-60 / aGr 60-120",
  `AZ-GG` = "aeAh 0-20 / aelC 20-30 / aeGo 30-60 / aGr 60-120",
  `AT-GG` = "aAxh 0-30 / aAxh-Go 30-50 / aGo 50-90 / aGr 90-150",
  `AB-GG` = "aAh 0-20 / aM-Go 20-45 / IIaGo 45-90 / IIaGr 90-150",
  GNn = "Go-Ah 0-25 / Gr 25-100",
  GNc = "Gco-Ah 0-25 / Gcr 25-100",
  GNh = "Go-Ah 0-25 / Ah-Gr 25-45 / Gr 45-100",
  GNg = "sGo-Ah 0-25 / sGr 25-100",
  GNq = "qGo-Ah 0-25 / qGr 25-100",
  GMn = "Go-Aa 0-20 / Gr 20-100",
  GMc = "Gco-Aa 0-20 / Gcr 20-100",
  GMg = "sGo-Aa 0-20 / sGr 20-100",
  GMq = "qGo-Aa 0-20 / qGr 20-100",
  GHn = "nH 0-20 / IIGr 20-100",
  GHh = "hH 0-20 / IIGr 20-100",
  GHg = "uH 0-20 / IIsGr 20-100",
  GHq = "uH 0-20 / IIqGr 20-100",
  `[GHu]` = "uH 0-20 / IIGr 20-100",
  # M: Marschen
  MRn = "tmzeGo-Ah 0-10 / tmzeGo 10-30 / tmzeGr 30-100",
  MRb = "tbGo-Ah 0-10 / tbGo 10-30 / tbGr 30-100",
  MRf = "tpeGo-Ah 0-10 / tpeGo 10-30 / tpeGr 30-100",
  MCn = "tmeAh 0-20 / tmeGo 20-50 / tmzeGr 50-120",
  MCb = "tbeAh 0-20 / tbeGo 20-50 / tbeGr 50-120",
  MCf = "tpeAh 0-20 / tpeGo 20-50 / tpeGr 50-120",
  MNn = "tmAh 0-20 / tmGo 20-60 / tmzeGr 60-120",
  MNb = "tbAh 0-20 / tbGo 20-60 / tbeGr 60-120",
  MNf = "tpAh 0-20 / tpGo 20-60 / tpeGr 60-120",
  MHn = "tmAh 0-20 / tmSg-Go 20-60 / tmzeGr 60-120",
  MHb = "tbAh 0-20 / tbSg-Go 20-60 / tbGr 60-120",
  MHf = "tpAh 0-20 / tpSg-Go 20-60 / tpGr 60-120",
  MDn = "tpAh 0-20 / tpSw 20-35 / IIftpAh\u00b0Sd 35-50 / IIftpGo\u00b0Sd 50-70 / IItpGo 70-90 / IItpGr 90-150",
  MKn = "tbAh 0-20 / tbSw 20-35 / tbSq 35-60 / tbGr 60-120",
  MOn = list("tmoAh 0-20 / tmoGo 20-50 / tmoGr 50-120", oc_pct = c(6, 1, 0.5), clay_pct = 50),
  # Ue: Strandboeden
  "\u00dcAn" = "tmzAi 0-5 / tmzlC 5-40 / tmzeGo 40-100",
  "\u00dcAb" = "tbzAi 0-5 / tbzlC 5-40 / tbzeGo 40-100",
  "\u00dcAf" = "tpAi 0-5 / tplC 5-40 / tpGo 40-100",
  # I: Semisubhydrische Boeden
  IWn = "tmzeFo 0-15 / tmzeFr 15-80",
  IWb = "tbzeFo 0-15 / tbzeFr 15-80",
  IWf = "tpFo 0-15 / tpFr 15-80",
  # H, K: Moore
  HNn = "nHw 0-50 / nHr 50-120 / IIfF 120-140",
  HNc = "nHcw 0-40 / nHcr 40-120 / IIfF 120-140",
  HNu = "uHw 0-40 / uHr 40-110 / IInHr 110-150",
  HHn = "hHw 0-40 / hHr 40-150 / IInHr 150-200",
  KVn = "nHv 0-20 / nHt 20-35 / nHw 35-80 / nHr 80-150",
  KVc = "nHcv 0-20 / nHct 20-35 / nHw 35-80 / nHcr 80-150",
  KVu = "uHv 0-20 / uHt 20-35 / uHw 35-80 / uHr 80-120 / IIfF 120-130 / IIIC 130-150",
  KMn = "nHm 0-15 / nHa 15-30 / nHt 30-45 / nHw 45-100",
  KMc = "nHcm 0-15 / nHca 15-30 / nHcw 30-100",
  KMu = "uHm 0-15 / uHa 15-30 / uHt 30-45 / uHw 45-100",
  `KV-KM` = "nHvm 0-15 / nHa 15-30 / nHt 30-45 / nHw 45-100",
  KHn = "hHv 0-15 / hHw 15-60 / hHr 60-150",
  `[KHm]` = "hHm 0-15 / hHw 15-60 / hHr 60-150"
)

# The types of the wet subtypes (their examples take the wet data and site).
.KA5_WET_TYPES <- c("AO", "AQ", "AZ", "AT", "AB", "GG", "GN", "GM", "GH", "MR", "MC", "MO", "MK", "MD", "MH", "MN",
                    "\u00dcA", "IA", "IW", "JP", "JG", "JS", "JD", "HH", "HN", "KV", "KM", "KH")

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
.ka5_hz_data <- function(d, wet = FALSE) {
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
    # v0.9.364: a podzolic B part (Bhs-Go): the Cp and Fep of a Bhs, sand
    if (isTRUE(wet) && grepl("^B[hms]*(h|s)[hms]*$", w)) set(c_pyro_pct = 0.9, fe_pyro_pct = 0.2, oc_pct = max(o$oc_pct, 1), clay_pct = 3, silt_pct = 5)
  }
  if (isTRUE(wet)) o <- .ka5_hz_data_wet(o, car, pre, with, ms)
  if (!is.na(o$clay_pct %||% NA) && !is.na(o$silt_pct %||% NA)) o$sand_pct <- max(0, 100 - o$clay_pct - o$silt_pct)
  o
}

#' soilKey's typical values of the horizons of the wet subtypes (v0.9.364):
#' the tidal, floodplain, salt, marl and organic prefixes, the kinds of the G,
#' the H and the F, read by the order of tests on the data. Not a rule of the
#' KA5.
#' @noRd
.ka5_hz_data_wet <- function(o, car, pre, with, ms) {
  set <- function(...) { a <- list(...); for (k in names(a)) o[[k]] <<- a[[k]] }
  col <- function(h, v, c) set(munsell_hue_moist = h, munsell_value_moist = v, munsell_chroma_moist = c)
  tp <- intersect(pre, c("tm", "tb", "tp"))
  if (length(tp)) set(layer_origin = c(tm = "tidal-marine", tb = "tidal-brackish", tp = "tidal-fluvial")[[tp[1L]]])
  if ("a" %in% pre || any(startsWith(with, "a"))) set(layer_origin = "fluvic")
  set(ec_dS_m = if ("z" %in% pre) 5 else 0.3)
  if ("e" %in% pre && ms %in% c("A", "G", "S", "F")) set(caco3_pct = max(o$caco3_pct, 5))
  if ("o" %in% pre) set(oc_lithogenic_pct = 3, oc_pct = max(o$oc_pct, 3), clay_pct = 50)
  if (ms == "G") {
    if (car == "Gor") { set(redoximorphic_features_pct = 10, reduction_colours_pct = 60); col("5Y", 5, 2) }
    else if (car == "Gw") { set(redoximorphic_features_pct = 0, reduction_colours_pct = 0, dipyridyl_test = FALSE); col("5Y", 6, 1) }
    else if (car == "Gso") { set(redoximorphic_features_pct = 40, reduction_colours_pct = 5, ferric_nodules_pct = 20); col("7.5YR", 5, 8) }
  }
  if (ms == "S" && car == "Sq") set(knick_degree = "strong", ksat_mm_h = 0.4)
  if (ms == "M") set(stratification_pattern = "continuous")
  if (ms == "H") {
    sfx <- sub("^H", "", car)
    if ("u" %in% pre) set(peat_moor_type = "transitional")
    set(munsell_hue_moist = "10YR", munsell_value_moist = 2, munsell_chroma_moist = 1,
        ph_cacl2 = if (grepl("c", sfx)) 6.5 else if ("h" %in% pre) 3.5 else if ("u" %in% pre) 4.2 else 5.5)
    if (grepl("c", sfx)) set(secondary_carbonates_pct = 3, caco3_pct = 5)
    if (grepl("m", sfx)) set(von_post_index = 8, water_repellence = "repellent", structure_type = "single grain")
    else if (grepl("v", sfx)) set(structure_type = "crumb")
    else if (grepl("a", sfx)) set(structure_type = "subangular blocky")
    else if (grepl("t", sfx)) set(structure_type = "prismatic")
    else if (grepl("w", sfx)) set(redoximorphic_features_pct = 10, water_saturation_days = 200, structure_type = "massive")
    else if (grepl("r", sfx)) set(water_saturation_days = 365, structure_type = "massive")
  }
  if (ms == "F") {
    set(clay_pct = 20, silt_pct = 40)
    switch(car,
      Fo = { set(oc_pct = 2, redoximorphic_features_pct = 10, rh_value = 15); col("5Y", 4, 2) },
      Fr = { set(oc_pct = 2, rh_value = 10); col("N", 2, 0) },
      Fw = { set(oc_pct = 0.4); col("2.5Y", 6, 2) },
      Fh = { set(oc_pct = 8, rh_value = 16); col("7.5YR", 3, 2) },
      Fi = { set(oc_pct = 0.2); col("2.5Y", 6, 2) },
      NULL)
  }
  o
}

# The site of an example profile from its horizons: the water table under a
# G, perched water over an S, the record of a deep cultivation or a gas.
.ka5_example_site <- function(code, des, top, wet = FALSE) {
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
  if (isTRUE(wet)) {
    pre <- unlist(lapply(des, function(d) .ka5_parse_one(d)$prefixes))
    if (any(ms %in% "G")) site <- c(site, list(
      groundwater_source = if ("s" %in% pre) "slope_water" else if ("q" %in% pre) "spring_water" else "groundwater",
      slope_pct = if ("s" %in% pre) 12 else 2))
    if ("a" %in% pre) site$landform <- "floodplain"
    if (length(ms) && ms[1L] %in% "F") site$tidal_zone <- "intertidal"
    if (ty == "\u00dcA") site$tidal_zone <- "supratidal"
    if (ty %in% c("KV", "KM", "KH")) site$artificially_drained <- TRUE
  }
  site
}

#' The example profile of a terrestrial KA5 subtype
#' @param code The subtype code as printed (BBn, GG-BB ...).
#' @param route "notation" or "data".
#' @noRd
.ka5_subtype_example <- function(code, route = c("notation", "data")) {
  route <- match.arg(route)
  code <- sub("^UEA", "\u00dcA", code)
  spec <- .KA5_SUBTYPE_PROFILES[[code]] %||% .KA5_SUBTYPE_PROFILES_WET[[code]]
  if (is.null(spec)) rlang::abort(sprintf("No KA5 subtype example '%s' (every printed subtype has one).", code))
  wet <- !is.null(.KA5_SUBTYPE_PROFILES_WET[[code]])
  if (is.character(spec)) spec <- list(spec)
  p <- .ka5_parse_profile_string(spec[[1L]])
  n <- length(p$des)
  rows <- lapply(p$des, .ka5_hz_data, wet = wet)
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
                            utils::modifyList(.ka5_example_site(code, p$des, p$top, wet), site_extra))
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

# The wet subtype examples that key to their subtype on the data as well
# (v0.9.364; the others need the notation: the Gw, Gso, Gco and Gcr, the Sg
# and Sq of the Marschen, the Dwog, the transitions the data key does not
# build, the Hvm).
.KA5_DATA_ROUTE_SUBTYPES_WET <- c("AOn", "GG-AO", "AQn", "GG-AQ", "AZn", "GG-AZ", "ATn", "GG-AT", "ABn", "AQ-AB", "GG-AB",
                                  "GGn", "GGx", "GGw", "GGh", "GGg", "GGq", "GGa", "RQ-GG", "RR-GG", "RZ-GG", "AO-GG",
                                  "AQ-GG", "AZ-GG", "GNn", "GNg", "GNq", "GMn", "GMg", "GMq", "GHn", "GHh", "GHg", "GHq",
                                  "[GHu]", "MRn", "MRb", "MRf", "MCn", "MCb", "MCf", "MNn", "MNb", "MNf", "MOn",
                                  "\u00dcAn", "\u00dcAb", "\u00dcAf", "IWn", "IWb", "IWf", "HNn", "HNc", "HNu", "HHn", "KVn",
                                  "KVc", "KVu", "KMn", "KMc", "KMu", "KHn", "[KHm]")
