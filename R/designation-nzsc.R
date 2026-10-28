# =============================================================================
# v0.9.293 -- horizon designations for the New Zealand Soil Classification
# (NZSC, 4th ed., Manaaki Whenua 2026), in the New Zealand notation (Clayden
# and Hewitt; the NZSC names the letters it reads in its Diagnostics): a
# lithologic prefix (2, 3 ...), lower-case prefixes (e anthropic soil
# material, b buried), a master horizon (L litter, O peat, A, E, B, C, R, and
# CR for the material below a paralithic contact; two masters for a
# transition, AB, BC, A/B), suffix letters, a suffix in parentheses for a
# weak expression ((g) and (f) redox-mottled, (h) cutanic, (x) brittle), and
# trailing digits (Bw1, Bw2).
#
# The suffixes the key reads: w weathered-B, t argillic, h humus (Bh;
# cutanic in parentheses), s sesquioxides (Bs), o oxidic, g and r
# reductimorphic, (g) and (f) redox-mottled, x fragipan, (x) brittle-B,
# k calcareous, d densipan, qm duripan, hm humus-pan, lm ironstone-pan, sm
# ortstein-pan, fm placic, i nodular, n sodic, z saline, p ploughed; f, m and
# h after L or O the decomposition of litter and peat. The FAO/USDA notation
# is read by the same letters where they agree (Ah, Bw, Bt, Bg, C, R; Oi, Oe
# and Oa as fibric, mesic and humified peat; a trailing b as buried).
# =============================================================================

# The parse of one designation: prefix digits, lower-case prefixes, the master
# horizon(s), the suffix letters (strong and weak, apart), trailing digits.
# NULL when it does not fit.
.parse_designation_nzsc1 <- function(d) {
  d <- trimws(as.character(d))
  if (is.na(d) || !nzchar(d)) return(NULL)
  d <- sub("[[:space:]].*$", "", d)          # a note after the designation
  d <- sub("\\?+$", "", d)
  m <- regmatches(d, regexec("^([0-9]*)([beu]*)(CR|[LOAEBCR])(/?)([LOAEBC]?)(.*)$", d))[[1]]
  if (!length(m)) return(NULL)
  # the ss of slickensides (FAO/USDA) is not an s
  rest <- gsub("ss", "", m[7L], fixed = TRUE)
  digits <- sub("^.*?([0-9]*)$", "\\1", rest)
  rest <- sub("[0-9]*$", "", rest)
  weak <- regmatches(rest, gregexpr("\\(([a-z]+)\\)", rest))[[1]]
  weak <- unlist(strsplit(gsub("[()]", "", paste(weak, collapse = "")), ""))
  strong_s <- gsub("\\([a-z]+\\)", "", rest)
  if (grepl("[^a-z]", strong_s)) return(NULL)
  list(prefix = m[2L], pre = strsplit(m[3L], "")[[1]], master = m[4L],
       intermingled = nzchar(m[5L]), master2 = m[6L],
       strong = strsplit(strong_s, "")[[1]], weak = weak, digits = digits,
       raw = strong_s)
}

# Is a set of designations in the New Zealand (or FAO-like) notation that
# this parser reads? Declared in site$designation_system, otherwise yes.
# `nz` when the notation is the New Zealand one (declared, or a site in New
# Zealand): a Cr is then a reductimorphic C, as the NZSC reads r; elsewhere a
# Cr is weathered rock (FAO/USDA).
.nzsc_notation <- function(designation, site = list()) {
  declared <- tolower(as.character(site$designation_system %||% NA)[1L])
  country <- toupper(as.character(site$country %||% NA)[1L])
  if (!is.na(declared) && nzchar(declared) && declared %in% c("asc", "australian", "cssc", "canadian"))
    return(list(ok = FALSE, nz = FALSE, why = sprintf("declared: %s", declared)))
  nz <- (!is.na(declared) && declared %in% c("nzsc", "nz", "new zealand")) ||
        ((is.na(declared) || !nzchar(declared)) && !is.na(country) && country %in% c("NZ", "NZL", "NEW ZEALAND"))
  list(ok = TRUE, nz = nz, why = if (is.na(declared)) "New Zealand or FAO notation" else sprintf("declared: %s", declared))
}

# The role and flags of each horizon. role: L, O, A, E, B, BC (and the other
# transitions as written: AB, BA, CB, A/B, B/C ...), C, CR, R, "other" (not
# readable) or NA (no designation).
.nzsc_designations <- function(designation, site = list()) {
  d <- as.character(designation %||% character(0))
  n <- length(d)
  nota <- .nzsc_notation(d, site)
  role <- rep(NA_character_, n); strong <- vector("list", n); weak <- vector("list", n)
  raw <- rep("", n); pre <- vector("list", n); prefix <- rep("", n)
  for (i in seq_len(n)) {
    strong[[i]] <- character(0); weak[[i]] <- character(0); pre[[i]] <- character(0)
    if (is.na(d[i]) || !nzchar(trimws(d[i]))) next
    p <- if (nota$ok) .parse_designation_nzsc1(d[i]) else NULL
    if (is.null(p)) { role[i] <- "other"; next }
    role[i] <- if (nzchar(p$master2) && p$master2 != p$master)
                 paste0(p$master, if (p$intermingled) "/" else "", p$master2)
               else p$master
    strong[[i]] <- p$strong; weak[[i]] <- p$weak; raw[i] <- p$raw
    pre[[i]] <- p$pre; prefix[i] <- p$prefix
  }
  has <- function(l) vapply(strong, function(s) l %in% s, NA)
  has_w <- function(l) vapply(weak, function(s) l %in% s, NA)
  pair <- function(a, b) vapply(seq_len(n), function(i) grepl(paste0(a, ".*", b), raw[i]), NA)
  buried <- vapply(pre, function(s) "b" %in% s, NA) |
            (has("b") & !role %in% c("L", "O"))
  known <- !is.na(role) & role != "other"
  # L and O decomposition letters (f, m, h; FAO i, e, a) are not suffixes
  org <- role %in% c("L", "O")
  decomp <- ifelse(org & (has("f") | has("i")), "fibric", ifelse(org & (has("m") | has("e")), "mesic",
            ifelse(org & (has("h") | has("a")), "humified", NA_character_)))
  list(notation = nota, role = role, known = known, strong = strong, weak = weak, raw = raw,
       prefix = prefix, buried = buried, anthropic = vapply(pre, function(s) "e" %in% s, NA),
       decomp = decomp,
       w = has("w") & !org, t = has("t") & !org, h = has("h") & !org & !pair("h", "m"),
       s = has("s") & !org & !pair("s", "m"), o = has("o"), g = has("g"), r = has("r") & (!role %in% "C" | nota$nz),
       g_weak = has_w("g"), f_weak = has_w("f"), h_weak = has_w("h"), x_weak = has_w("x"),
       x = has("x"), k = has("k"), d = has("d") & !org, n = has("n"), z = has("z"), i = has("i") & !org,
       p = has("p"), m = has("m") & !org,
       duripan = pair("q", "m"), humus_pan = pair("h", "m") & !org, ironstone_pan = pair("l", "m"),
       ortstein = pair("s", "m"), placic = pair("f", "m") & !org)
}
