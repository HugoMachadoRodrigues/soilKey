# Writes inst/rules/za1991/key.yaml: the 73 soil forms of the South African
# soil classification of 1991 (and Lichtenburg, outside the AGIS key, Hugo's
# decision Q1) in the order of van Huyssteen (2020), which follows the book's
# (V 9; Q6), each with its sequence of diagnostic horizons and materials (the
# tokens of horizons.yaml) and the `closed_on` list: the codes that would
# extend the form's sequence into another form (Q3, the longest prefix;
# Fernwood is closed on every subsoil code, K 32 note i). Codes from SASRI
# (1999), else the 2018 code of the same name (van Tol and Bouwer 2024); Inanda
# and Plooysburg have none in any open source and take soilKey's [Ia] and
# [Py]. Only codes, names, page numbers and token sequences are written; no
# text of the sources.
#
# Run from the package root: Rscript data-raw/za1991_key.R

forms <- read.table(header = TRUE, stringsAsFactors = FALSE, text = "
code   name         src       seq              pages
Ch     Champagne    sasri1999 OO               'K 17; V 5.1'
Kp     Kranskop     sasri1999 HA-YAB-RAB       'K 4; V 5.2'
Ma     Magwa        sasri1999 HA-YAB           'K 5; V 5.3'
[Ia]   Inanda       soilkey   HA-RAB           'K 6; V 5.4'
Lu     Lusiki       sasri1999 HA-PCU           'K 7; V 5.5'
Sr     Sweetwater   sasri1999 HA-NCU           'K 8; V 5.6'
No     Nomanci      sasri1999 HA-LCU           'K 9; V 5.7'
Rg     Rensburg     sasri1999 VA-G             'K 77; V 5.8'
Ar     Arcadia      sasri1999 VA               'K 76; V 5.9'
Wo     Willowbrook  sasri1999 MA-G             'K 10; V 5.10'
Bo     Bonheim      sasri1999 MA-PCU           'K 16; V 5.11'
Sn     Steendal     sat2018   MA-SCH           'K 15; V 5.12'
Im     Immerpan     sat2018   MA-HCH           'K 13; V 5.13'
My     Mayo         sasri1999 MA-LCU           'K 12; V 5.14'
Mw     Milkwood     sasri1999 MA-HRK           'K 14; V 5.15'
Ik     Inhoek       sasri1999 MA               'K 11; V 5.16'
Ka     Katspruit    sasri1999 OA-G             'K 18; V 5.17'
Kd     Kroonstad    sasri1999 OA-E-G           'K 19; V 5.18'
Lo     Longlands    sat2018   OA-E-SPL         'K 20; V 5.19'
Wa     Wasbank      sasri1999 OA-E-HPL         'K 21; V 5.20'
Ct     Constantia   sasri1999 OA-E-YAB         'K 22; V 5.21'
Ts     Tsitsikamma  sat2018   OA-E-PZB-PLP     'K 23; V 5.22'
Lt     Lamotte      sat2018   OA-E-PZB-UMW     'K 24; V 5.23'
Cc     Concordia    sat2018   OA-E-PZB-UMN     'K 25; V 5.24'
Hh     Houwhoek     sat2018   OA-E-PZB-SAP     'K 26; V 5.25'
Es     Estcourt     sasri1999 OA-E-PRC         'K 27; V 5.26'
Km     Klapmuts     sasri1999 OA-E-PCU         'K 28; V 5.27'
Vf     Vilafontes   sasri1999 OA-E-NCU         'K 29; V 5.28'
Kk     Kinkelbos    sat2018   OA-E-NCA         'K 30; V 5.29'
Cf     Cartref      sat2018   OA-E-LCU         'K 31; V 5.30'
Fw     Fernwood     sasri1999 OA-E             'K 32; V 5.31'
We     Westleigh    sasri1999 OA-SPL           'K 33; V 5.32'
Dr     Dresden      sasri1999 OA-HPL           'K 34; V 5.33'
Av     Avalon       sasri1999 OA-YAB-SPL       'K 35; V 5.34'
Gc     Glencoe      sasri1999 OA-YAB-HPL       'K 35-36; V 5.35'
Pn     Pinedene     sasri1999 OA-YAB-USW       'K 36; V 5.36'
Gf     Griffin      sasri1999 OA-YAB-RAB       'K 37; V 5.37'
Mp     Molopo       sat2018   OA-YAB-SCH       'K 39; V 5.38'
Ak     Askham       sat2018   OA-YAB-HCH       'K 41; V 5.39'
Cv     Clovelly     sasri1999 OA-YAB           'K 42; V 5.40'
Bv     Bainsvlei    sasri1999 OA-RAB-SPL       'K 43; V 5.41'
Lc     Lichtenburg  sat2018   OA-RAB-HPL       'V 5.42'
Bd     Bloemdal     sasri1999 OA-RAB-USW       'K 40; V 5.43'
Ky     Kimberly     sat2018   OA-RAB-SCH       'K 43-44; V 5.44'
[Py]   Plooysburg   soilkey   OA-RAB-HCH       'K 44; V 5.45'
Gr     Garies       sat2018   OA-RAB-DOR       'K 45; V 5.46'
Hu     Hutton       sasri1999 OA-RAB           'K 46; V 5.47'
Sd     Shortlands   sasri1999 OA-RSB           'K 47; V 5.48'
Jb     Jonkersberg  sat2018   OA-PZB-PLP       'K 48; V 5.49'
Wf     Witfontein   sat2018   OA-PZB-UMW       'K 49; V 5.50'
Pg     Pinegrove    sat2018   OA-PZB-UMN       'K 50; V 5.51'
Gk     Groenkop     sat2018   OA-PZB-SAP       'K 51; V 5.52'
Ss     Sterkspruit  sasri1999 OA-PRC           'K 52; V 5.53'
Se     Sepane       sasri1999 OA-PCU-UMW       'K 53; V 5.54'
Va     Valsrivier   sasri1999 OA-PCU-UMN       'K 54; V 5.55'
Sw     Swartland    sasri1999 OA-PCU-SAP       'K 55-56; V 5.56'
Tu     Tukulu       sasri1999 OA-NCU-USW       'K 57; V 5.57'
Et     Etosha       sat2018   OA-NCU-SCH       'K 58; V 5.58'
Gm     Gamoep       sat2018   OA-NCU-HCH       'K 59; V 5.59'
Ou     Oudtshoorn   sat2018   OA-NCU-DOR       'K 60-61; V 5.60'
Oa     Oakleaf      sasri1999 OA-NCU           'K 62; V 5.61'
Mu     Montagu      sat2018   OA-NCA-USW       'K 62-63; V 5.62'
Ad     Addo         sat2018   OA-NCA-SCH       'K 63-64; V 5.63'
Pr     Prieska      sat2018   OA-NCA-HCH       'K 65; V 5.64'
Tr     Trawal       sat2018   OA-NCA-DOR       'K 66; V 5.65'
Ag     Augrabies    sasri1999 OA-NCA           'K 66-67; V 5.66'
Br     Brandvlei    sat2018   OA-SCH           'K 68; V 5.67'
Cg     Coega        sat2018   OA-HCH           'K 69; V 5.68'
Kn     Knersvlakte  sat2018   OA-DOR           'K 70; V 5.69'
Gs     Glenrosa     sat2018   OA-LCU           'K 71-72; V 5.70'
Ms     Mispah       sasri1999 OA-HRK           'K 73; V 5.71'
Du     Dundee       sasri1999 OA-SAL           'K 74; V 5.72'
Nb     Namib        sasri1999 OA-RSA           'K 75; V 5.73'
Wb     Witbank      sasri1999 OA-MMD           'K 38; V 5.74'
")

# the English names of the tokens (soilKey's gloss of the printed names)
cat_ <- yaml::read_yaml("inst/rules/za1991/horizons.yaml")$horizons
nm <- stats::setNames(vapply(cat_, function(e) e$name, ""), vapply(cat_, function(e) e$token, ""))
subsoil <- vapply(cat_, function(e) e$token, "")[vapply(cat_, function(e) e$group, "") == "subsoil"]
slots <- strsplit(forms$seq, "-", fixed = TRUE)

# the gates of plan 3.3 beyond the sequence
extra <- list(
  Sw = list(list(below_horizon_za = list(horizon = "PCU", material = "SAP", max_cm = 50))),
  Va = list(list(below_horizon_za = list(horizon = "PCU", material = "non_saprolite", min_cm = 50))),
  Se = list(list(below_horizon_za = list(horizon = "PCU", material = "non_saprolite", min_cm = 50))),
  Lt = list(list(below_horizon_za = list(horizon = "PZB", from = "B1", material = "non_saprolite", min_cm = 50))),
  Cc = list(list(below_horizon_za = list(horizon = "PZB", from = "B1", material = "non_saprolite", min_cm = 50))),
  Wf = list(list(below_horizon_za = list(horizon = "PZB", from = "B1", material = "non_saprolite", min_cm = 50))),
  Pg = list(list(below_horizon_za = list(horizon = "PZB", from = "B1", material = "non_saprolite", min_cm = 50))),
  Hh = list(list(below_horizon_za = list(horizon = "PZB", from = "B1", material = "SAP", max_cm = 50))),
  Gk = list(list(below_horizon_za = list(horizon = "PZB", from = "B1", material = "SAP", max_cm = 50))),
  Wb = list(list(deposit_thickness_za = list(material = "MMD", min_cm = 50))),
  Du = list(list(deposit_thickness_za = list(material = "SAL", min_cm = 50))),
  Nb = list(list(deposit_thickness_za = list(material = "RSA", min_cm = 50))))

entries <- lapply(seq_len(nrow(forms)), function(i) {
  s <- slots[[i]]
  k <- length(s)
  longer <- vapply(slots, function(t) length(t) > k && identical(t[seq_len(k)], s), NA)
  closed <- unique(vapply(slots[longer], function(t) t[k + 1L], ""))
  if (forms$code[i] == "Fw") closed <- subsoil
  e <- list(code = forms$code[i], name = forms$name[i],
            name_en = paste(nm[s], collapse = " / "),
            code_source = forms$src[i], sequence = forms$seq[i],
            pages = forms$pages[i], closed_on = as.list(closed))
  # D1 (K 55 note ii, K 26 note i, K 51 note i; Hugo, 2026-11-06): the
  # saprolite may lie within 500 mm under the B, so the third slot also takes
  # the unconsolidated materials (Swartland also hard rock, Q4), and
  # below_horizon_za() decides
  if (forms$code[i] == "Sw") e$alternatives <- list(SAP = list("HRK", "UMN", "UMW"))
  if (forms$code[i] %in% c("Hh", "Gk")) e$alternatives <- list(SAP = list("UMN", "UMW"))
  if (forms$code[i] == "Lc") e$outside_1991_key <- TRUE
  if (forms$name[i] == "Kimberly") e$aliases <- list("Kimberley")
  if (forms$name[i] == "Vilafontes") e$aliases <- list("Villafontes")
  e$tests <- list(all_of = c(list(list(form_sequence_za = list(form = forms$code[i]))), extra[[forms$code[i]]] %||% list()))
  e
})
`%||%` <- function(a, b) if (is.null(a)) b else a

head <- c(
  "# The 73 soil forms of the South African soil classification of 1991 (Soil",
  "# Classification Working Group, 1991), and Lichtenburg (V 5.42, outside the",
  "# AGIS key; Hugo's decision Q1: keyed, always provisional, no family), in",
  "# the order of van Huyssteen (2020), which follows the book's (V 9; Q6).",
  "# Written by data-raw/za1991_key.R; edit there.",
  "#",
  "#   code         SASRI (1999), else the 2018 code of the same name; [Ia] and",
  "#                [Py] are soilKey's (no code in any open source)",
  "#   name         as printed (Kimberly, Vilafontes; aliases as V and W write them)",
  "#   name_en      soilKey's gloss: the sequence of printed horizon names",
  "#   sequence     the tokens of horizons.yaml, from the top",
  "#   closed_on    the codes that would extend the sequence into another form:",
  "#                such a code directly below the last horizon of the sequence",
  "#                makes the form fail (Q3, the longest prefix; Fernwood is",
  "#                closed on every subsoil code, K 32 note i)",
  "#   alternatives codes read in place of a slot: the unconsolidated materials",
  "#                above saprolite within 500 mm (Swartland, Houwhoek, Groenkop;",
  "#                D1), and hard rock under the pedocutanic B as very weakly",
  "#                weathered saprolite (Swartland, Q4)",
  "#   tests        form_sequence_za (the sequence) and the 500 mm rules of K",
  "#                (below_horizon_za, deposit_thickness_za; plan 3.3)",
  "")
yaml::write_yaml(list(forms = entries), "inst/rules/za1991/key.yaml.tmp")
body <- readLines("inst/rules/za1991/key.yaml.tmp")
unlink("inst/rules/za1991/key.yaml.tmp")
writeLines(c(head, body), "inst/rules/za1991/key.yaml")
cat("wrote", length(entries), "forms\n")
