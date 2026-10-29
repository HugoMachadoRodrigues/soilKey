# =============================================================================
# v0.9.305 -- horizon designations for the Hungarian diagnostic classification
# (HSCS 2024, Micheli et al.), which adopts the FAO (2006) notation for its
# genetic horizons: a lithologic prefix (2, 3 ...), a master horizon (H peat,
# O, A, E, B, C, R; I ice, L limnic, W water), a second master for a
# transition (AB, BC) or an intermingled one (A/B, C/R), suffix letters, a
# suffix in parentheses for a weak expression (C(k)), and trailing digits
# (A2, Bt1).
#
# The FAO (2006) suffixes the key reads: b buried, g stagnic (perched water),
# i slickensides, k carbonates, l capillary fringe (groundwater) mottling,
# m cementation, n sodium, p ploughing, r strong reduction, t clay
# accumulation, u artefacts (urban), w colour or structure, x fragipan,
# y gypsum, z soluble salts. Unlike the New Zealand notation, r is strong
# reduction (a Cr is a reduced C, not weathered rock) and i is slickensides.
# =============================================================================

# The parse of one designation: prefix digits, the master horizon(s), the
# suffix letters (strong and weak, apart), trailing digits. NULL when it does
# not fit.
.parse_designation_hscs1 <- function(d) {
  d <- trimws(as.character(d))
  if (is.na(d) || !nzchar(d)) return(NULL)
  d <- sub("[[:space:]].*$", "", d)          # a note after the designation
  d <- sub("\\?+$", "", d)
  m <- regmatches(d, regexec("^([0-9]*)(b?)([HOLAEBCRIW])(/?)([HOLAEBCRIW]?)(.*)$", d))[[1]]
  if (!length(m)) return(NULL)
  rest <- m[7L]
  digits <- sub("^.*?([0-9]*)$", "\\1", rest)
  rest <- sub("[0-9]*$", "", rest)
  weak <- regmatches(rest, gregexpr("\\(([a-z]+)\\)", rest))[[1]]
  weak <- unlist(strsplit(gsub("[()]", "", paste(weak, collapse = "")), ""))
  strong_s <- gsub("\\([a-z]+\\)", "", rest)
  # USDA writes slickensides ss; FAO writes i
  strong_s <- gsub("ss", "i", strong_s, fixed = TRUE)
  if (grepl("[^a-z@]", strong_s)) return(NULL)
  list(prefix = m[2L], buried_prefix = nzchar(m[3L]), master = m[4L],
       intermingled = nzchar(m[5L]), master2 = m[6L],
       strong = strsplit(gsub("@", "", strong_s), "")[[1]], weak = weak, digits = digits)
}

# The role and flags of each horizon. role: H, O, A, E, B, C, R (and the
# transitions as written: AB, BC, A/B, C/R ...), "other" (not readable) or
# NA (no designation).
.hscs_designations <- function(designation) {
  d <- as.character(designation %||% character(0))
  n <- length(d)
  role <- rep(NA_character_, n); strong <- vector("list", n); weak <- vector("list", n)
  prefix <- rep("", n); bpre <- rep(FALSE, n)
  for (i in seq_len(n)) {
    strong[[i]] <- character(0); weak[[i]] <- character(0)
    if (is.na(d[i]) || !nzchar(trimws(d[i]))) next
    p <- .parse_designation_hscs1(d[i])
    if (is.null(p)) { role[i] <- "other"; next }
    role[i] <- if (nzchar(p$master2) && p$master2 != p$master)
                 paste0(p$master, if (p$intermingled) "/" else "", p$master2)
               else p$master
    strong[[i]] <- p$strong; weak[[i]] <- p$weak
    prefix[i] <- p$prefix; bpre[i] <- p$buried_prefix
  }
  has <- function(l) vapply(strong, function(s) l %in% s, NA)
  has_any <- function(l) vapply(seq_len(n), function(i) l %in% c(strong[[i]], weak[[i]]), NA)
  known <- !is.na(role) & role != "other"
  org <- role %in% c("H", "O")
  list(role = role, known = known, strong = strong, weak = weak, prefix = prefix,
       buried = bpre | (has("b") & !org),
       g = has_any("g"), i = has("i"), k = has_any("k"), l = has_any("l"), m = has("m"),
       n = has("n"), p = has("p"), r = has_any("r"), t = has("t"), u = has("u"),
       w = has("w"), x = has("x"), y = has("y"), z = has_any("z"))
}
