# =============================================================================
# v0.9.277 -- horizon designations for the Australian Soil Classification
# (ASC, 3rd ed., Isbell & NCST 2021), in the notation of the Australian Soil
# and Land Survey Field Handbook: a lithologic prefix (2, 3 ...), a master
# horizon (O, P, A, B, C, D, R, or two of them for a transition: AB, BC ...;
# with a slash for intermingled parts: B/C, C/B), a subdivision digit and a
# further digit (A1, A2, B21, B22), and suffix letters (t silicate clay,
# k carbonate, s and h sesquioxides and organic matter of the Podosol B,
# m cemented, e conspicuously bleached, j sporadically bleached, p ploughed,
# b buried, g gleyed, w weak development ...).
#
# Designations in the FAO or USDA notation are read by the role of each
# horizon: an A as an A1 (an Ap as a ploughed A1), an E as an A2, a B with no
# transition as a B2 (a Bt as a B2t), a BC as a BC.
# =============================================================================

.ASC_MASTERS <- c("O", "P", "A", "B", "C", "D", "R")

# A designation as written, made ready to parse: the first word only (a note
# such as "lamellae" or "(weak)" after it is dropped), a trailing "?" (the
# surveyor's doubt) dropped, and one written all in capitals given lower-case
# suffixes: the letters after the masters that are not masters ("CK",
# "B2WT", "BHS"; BC stays a BC), and a P, D or R after an A, B or C, or a
# master repeated without a digit (AP, A1P, B2D, 2B2B), as the notation
# writes no transition to a P, D or R horizon.
.asc_clean_designation <- function(d) {
  d <- trimws(as.character(d))
  if (is.na(d) || !nzchar(d)) return(d)
  d <- sub("[[:space:](].*$", "", d)
  d <- sub("\\?+$", "", d)
  if (!nzchar(d) || grepl("[a-z]", d)) return(d)
  m <- regmatches(d, regexec("^([0-9]*)([ABC])([0-9]*)([PDR])([A-Z]*)$", d))[[1]]
  if (length(m)) return(paste0(m[2L], m[3L], m[4L], tolower(m[5L]), tolower(m[6L])))
  m <- regmatches(d, regexec("^([0-9]*)([OPABCDR])([0-9]+)([A-Z]+)$", d))[[1]]
  if (length(m) && substr(m[5L], 1L, 1L) == m[3L])
    return(paste0(m[2L], m[3L], m[4L], tolower(m[5L])))
  m <- regmatches(d, regexec("^([0-9]*[OPABCDR]{1,2}[0-9]*)([EFGHIJKLMNQSTUVWXYZ][A-Z]*)([0-9]*)$", d))[[1]]
  if (length(m) == 0L) return(d)
  paste0(m[2L], tolower(m[3L]), m[4L])
}

# Parse one designation in the Australian notation: a lithologic prefix, one
# master or two (a transition, AB, BC, also written with digits as in the
# older A2B, B3C, A1A2; with a slash for intermingled parts, B/C, A1/A2), the
# subdivision digits, the suffix letters and any trailing digits. NULL when
# it does not fit the notation.
.parse_designation_asc1 <- function(d) {
  d <- .asc_clean_designation(d)
  if (is.na(d) || !nzchar(d)) return(NULL)
  m <- regmatches(d, regexec("^([0-9]*)([OPABCDR])([0-9]*)(/?)([OPABCDR]?)([0-9]*)([a-z]*)([0-9]*)$", d))[[1]]
  if (length(m) == 0L) return(NULL)
  if (nzchar(m[5L]) && !nzchar(m[6L])) return(NULL)   # a slash with no second master
  list(prefix = m[2L], master = m[3L], digits = m[4L], intermingled = nzchar(m[5L]),
       master2 = m[6L], digits2 = m[7L], transition = nzchar(m[6L]),
       digit = if (nzchar(m[4L])) substr(m[4L], 1L, 1L) else "",
       digit2 = if (nzchar(m[7L])) substr(m[7L], 1L, 1L) else "",
       subdiv = m[4L], suffixes = strsplit(gsub("ss", "", m[8L]), "")[[1]], trailing = m[9L])
}

# The role of a parsed Australian designation.
.asc_role1 <- function(p) {
  if (p$transition && p$master2 != p$master) {
    return(paste0(p$master, if (p$intermingled) "/" else "", p$master2))
  }
  if (p$transition) {
    # two parts of the same master (A1A2, A1/A2, A2A3, B1B2): an A with an A2
    # part counts as an A2 (its colour change is there), otherwise the
    # higher subdivision; any other master takes the first part
    dg <- c(p$digit, p$digit2); dg <- dg[nzchar(dg)]
    if (p$master == "A" && length(dg)) return(if ("2" %in% dg) "A2" else paste0("A", max(dg)))
    if (p$master %in% c("B", "O") && length(dg)) return(paste0(p$master, dg[1L]))
  }
  if (p$master %in% c("A", "B", "O") && nzchar(p$digit)) return(paste0(p$master, p$digit))
  switch(p$master, A = "A1", B = "B2", O = "O1", p$master)
}

# The roles the gates know; any other (A0, B5 ...) is read as a designation
# that cannot be read.
.ASC_ROLES <- c("O1", "O2", "P", "A1", "A2", "A3", "AB", "BA", "A/B", "B/A", "AC", "CA",
                "B1", "B2", "B3", "BC", "CB", "B/C", "C/B", "C", "D", "R")

# Is a set of designations written in the Australian notation? Declared in
# site$designation_system ("asc" or another notation), otherwise yes when a
# master is followed directly by its subdivision digit (A1, A2, B21, B2t), or
# a P or D master appears, and no sign of the FAO/USDA notation does (an E
# master, a digit after the suffixes as in Bt1, O horizons written Oi, Oe,
# Oa).
.asc_notation <- function(designation, site = list()) {
  declared <- tolower(as.character(site$designation_system %||% NA)[1L])
  d <- gsub("\\s+", "", as.character(designation))
  d <- d[!is.na(d) & nzchar(d)]
  if (!is.na(declared) && nzchar(declared)) {
    ok <- declared %in% c("asc", "australian", "field handbook", "nchs", "australian field handbook")
    return(list(ok = ok, why = sprintf("declared: %s", declared)))
  }
  if (length(d) == 0L) return(list(ok = FALSE, why = "no designations"))
  aus <- grepl("^[0-9]*[OPABCDR]{1,2}[0-9]", d) | grepl("^[0-9]*[PD]", d)
  usda <- grepl("^[0-9]*E", d) | grepl("^[0-9]*[ABC][A-Z]?[a-z]+[0-9]+$", d) |
          grepl("^O[iea]", d)
  if (any(aus) && !any(usda)) return(list(ok = TRUE, why = "Australian notation (A1, B2t ...)"))
  if (any(usda)) return(list(ok = FALSE, why = "FAO/USDA notation"))
  country <- toupper(as.character(site$country %||% NA)[1L])
  if (!is.na(country) && country %in% c("AU", "AUS", "AUSTRALIA"))
    return(list(ok = TRUE, why = "site in Australia, no sign of another notation"))
  list(ok = FALSE, why = "no sign of the Australian notation")
}

# The role of each horizon for the ASC, from its designation in either
# notation: role is one of O1, O2, P, A1, A2, A3, AB, B1, B2, B3, BC, B/C,
# C/B, C, D, R, other, or NA (no designation); flags for the suffixes the
# order key reads.
.asc_designations <- function(designation, site = list()) {
  d <- as.character(designation %||% character(0))
  n <- length(d)
  nota <- .asc_notation(d, site)
  role <- rep(NA_character_, n)
  sfx <- vector("list", n)
  buried <- rep(FALSE, n); prefix <- rep("", n)
  lamellae <- grepl("lamell", tolower(d))
  for (i in seq_len(n)) {
    di <- gsub("\\s+", "", d[i])
    if (is.na(di) || !nzchar(di)) { sfx[[i]] <- character(0); next }
    # clay lamellae (argic horizons): B material in thin bands, not a B2
    if (lamellae[i] && is.null(.parse_designation_asc1(d[i]))) {
      role[i] <- "lamellae"; sfx[[i]] <- character(0); next
    }
    if (nota$ok) {
      p <- .parse_designation_asc1(d[i])
      if (is.null(p)) { role[i] <- "other"; sfx[[i]] <- character(0); next }
      prefix[i] <- p$prefix
      sfx[[i]] <- p$suffixes
      role[i] <- .asc_role1(p)
      if (!role[i] %in% .ASC_ROLES) role[i] <- "other"
    } else {
      # FAO/USDA notation: master letters (and a lithologic prefix), then
      # the suffix letters; the trailing digits are vertical subdivisions.
      # The ss of slickensides and the se of sulfides are not an s, and the
      # a, e and i of an O tell its decomposition, not a bleached horizon.
      di <- sub("\\?+$", "", sub("[[:space:](].*$", "", trimws(d[i])))
      m <- regmatches(di, regexec("^([0-9]*)([OAEBCRHLW])(/?)([OAEBCRHLW]?)([a-z]*)", di))[[1]]
      if (length(m) == 0L) { role[i] <- "other"; sfx[[i]] <- character(0); next }
      prefix[i] <- m[2L]
      letters_s <- gsub("ss|se", "", m[6L])
      s <- strsplit(letters_s, "")[[1]]
      a <- m[3L]; b <- m[5L]
      dec <- if (a == "O" && !nzchar(b)) intersect(s, c("a", "e", "i")) else character(0)
      sfx[[i]] <- setdiff(s, dec)
      s <- c(sfx[[i]], dec)
      role[i] <- if (nzchar(m[4L])) paste0(sub("E", "A", a), "/", sub("E", "A", b))
                 else if (nzchar(b)) paste0(sub("E", "A", a), sub("E", "A", b))
                 else switch(a, A = "A1", E = "A2", B = "B2", C = "C", R = "R",
                             O = if (any(s %in% c("a", "e"))) "O2" else "O1",
                             H = "P", L = "O1", W = "other", "other")
      # an AE, EA or A/E holds the E: read as an A2
      if (role[i] %in% c("AA", "A/A")) role[i] <- "A2"
      if (!role[i] %in% .ASC_ROLES) role[i] <- "other"
    }
    buried[i] <- "b" %in% sfx[[i]]
  }
  has <- function(letter) vapply(sfx, function(s) letter %in% s, logical(1))
  # a designation that cannot be read ("other") tells nothing, like none
  known <- !is.na(role) & role != "other"
  list(notation = nota, role = role, suffixes = sfx, prefix = prefix, buried = buried,
       known = known, lamellae = lamellae,
       t = has("t"), k = has("k"), s = has("s"), h = has("h"), m = has("m"),
       e = has("e"), p = has("p"))
}
