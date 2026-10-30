# ============================================================================
# WRB 2022 (4th ed.) -- Specifier infrastructure (v0.9.2.B + v0.9.3.A)
#
# Specifiers are prefixes that compose with a base qualifier to
# constrain WHERE / HOW the qualifier applies in the profile.
#
#   Ano-    upper 50 cm  (alias of Epi-)
#   Epi-    upper 50 cm
#   Endo-   50 - 100 cm
#   Bathy-  > 100 cm
#   Panto-  whole profile
#   Kato-   lower part of the profile (top_cm >= 50 cm)
#   Amphi-  feature in BOTH the upper (0-50) AND lower (50-100) bands
#   Poly-   multiple non-contiguous occurrences (>= 2 disjoint runs)
#   Supra-  above a continuous-rock / petric / technic-hard barrier
#   Thapto- in a buried soil (designation suffix \\code{b})
#
# Composition example: "Endogleyic" = gleyic features in the 50-100
# cm band only; "Thaptohistic" = histic horizon in a buried soil.
#
# Each specifier is described by a kind:
#   "depth"  -> simple depth band (min_top_cm, max_top_cm)
#   "filter" -> custom layer-filter function with signature
#               function(pedon, base_layers) -> integer(0..n)
# The dispatcher in resolve_wrb_qualifiers picks up any YAML name that
# starts with one of these prefixes and routes accordingly.
# ============================================================================


# ---- v0.9.3.A custom layer-filter helpers ---------------------------------

# Upper limit (cm) of the shallowest limiting layer, NA when the data show
# none. WRB 2022 Ch 2.1, rule 8 (p. 23): "The term limiting layer used in
# definitions comprises continuous rock, technic hard material, petrocalcic,
# petroduric, petrogypsic and petroplinthic horizons and other cemented
# layers" that are at least moderately cemented and continuous. Until
# v0.9.321 the other cemented layers (a cemented spodic Bsm, ortstein, a
# cemented layer whose agent is not recorded) were left out; they are read
# by .wrb_cemented_limiting(). .wrb_limiting_layers() gives the layers.
# `other_cemented = FALSE` keeps the reading before v0.9.322, for the USDA
# kandic horizon, which uses this as its contact and is not a WRB criterion.
.barrier_top_cm <- function(pedon, other_cemented = TRUE) {
  layers <- .wrb_limiting_layers(pedon, other_cemented = other_cemented)
  if (length(layers) == 0L) return(NA_real_)
  min(pedon$horizons$top_cm[layers])
}

# The layers that are a limiting layer (Ch 2.1, rule 8, p. 23), with a
# recorded upper limit.
.wrb_limiting_layers <- function(pedon, other_cemented = TRUE) {
  rk <- continuous_rock(pedon)
  pc <- petrocalcic(pedon); pd <- petroduric(pedon)
  pg <- petrogypsic(pedon); pp <- petroplinthic(pedon)
  th <- technic_hard_material(pedon)
  h <- pedon$horizons
  layers <- unique(c(unlist(lapply(list(rk, pc, pd, pg, pp, th),
                                   function(d) if (isTRUE(d$passed)) d$layers else integer(0))),
                     if (other_cemented) which(.wrb_cemented_limiting(h) %in% TRUE)))
  sort(layers[!is.na(h$top_cm[layers])])
}

# The "other cemented layers" of the limiting layer (WRB 2022 Ch 2.1, rule 8,
# p. 23), layer by layer: TRUE / FALSE / NA. The rule asks for both
# "cementation with a class of at least moderately cemented and a
# continuity" with vertical fractures, if present, >= 10 cm apart and < 20%
# of the volume.
#  * The class is read from cementation_class: moderately, strongly or
#    indurated (and very / extremely strongly) is TRUE; none or a weakly
#    class (very, extremely weakly) is FALSE.
#  * Where no class is recorded, from the designation: the suffix m is
#    "Pedogenic cementation in >= 50% of the volume ...; cementation class:
#    at least moderately cemented" and is "only used if following another
#    suffix (k, l, q, s, v, y, z) that indicates the cementing agent" (Annex
#    3, Table 10.2, p. 227). So Bsm, Bhsm, Bkm, Bqm, Ckm, Bvm count; a bare m
#    (Bm, Cm) does not, as Canadian Bm is a slightly altered B. Every part of
#    an interfingered designation (Bsm/Btsm) must carry it, since a layer
#    whose dominant part is not cemented (Bk/Bkm) is not continuous. A
#    designation without such an m is FALSE.
#  * soilKey has no field for the fractures, so continuity is read as met,
#    as petrocalcic(), petroduric(), petrogypsic() and petroplinthic() read
#    their own continuity criterion. A fragipan or densipan (pan_type) is not
#    continuous by definition: "the soil material in between the soil
#    aggregates and >= 50% of the volume of the aggregated soil are not
#    cemented" (fragic horizon, Ch 3.1.13, p. 46), so it is FALSE whatever
#    its class.
.wrb_cemented_limiting <- function(h) {
  n <- nrow(h)
  if (!n) return(logical(0))
  cc <- tolower(trimws(as.character(h$cementation_class %||% rep(NA_character_, n))))
  cc[cc %in% ""] <- NA_character_
  cc_no  <- !is.na(cc) & (grepl("weak", cc) |
              grepl("^(none|no|non|not|un)", cc))
  cc_yes <- !is.na(cc) & !cc_no & grepl("moderat|strong|indurat", cc)
  by_class <- ifelse(cc_yes, TRUE, ifelse(cc_no, FALSE, NA))
  d <- as.character(h$designation %||% rep(NA_character_, n))
  d[!is.na(d) & !nzchar(trimws(d))] <- NA_character_
  m_sfx <- vapply(d, function(x) {
    if (is.na(x)) return(NA)
    parts <- strsplit(x, "/", fixed = TRUE)[[1]]
    parts <- .desg_master(trimws(parts))
    parts <- parts[nzchar(parts)]
    if (!length(parts)) return(NA)
    all(grepl("^[A-Z]+[a-z0-9']*[klqsvyz]m", parts))
  }, logical(1), USE.NAMES = FALSE)
  out <- ifelse(!is.na(by_class), by_class, m_sfx)
  pan <- tolower(trimws(as.character(h$pan_type %||% rep(NA_character_, n))))
  out[!is.na(pan) & grepl("fragipan|densipan", pan)] <- FALSE
  out
}

# Filter for Supra-: keep layers whose bottom_cm <= the shallowest
# barrier top.
.supra_filter <- function(pedon, base_layers) {
  bar <- .barrier_top_cm(pedon)
  if (is.na(bar)) return(integer(0))
  h <- pedon$horizons
  base_layers[!is.na(h$bottom_cm[base_layers]) &
                  h$bottom_cm[base_layers] <= bar]
}

# Filter for Thapto-: keep layers whose designation ends with the
# lowercase buried-soil "b" suffix.
.thapto_filter <- function(pedon, base_layers) {
  h <- pedon$horizons
  d <- h$designation[base_layers]
  ok <- !is.na(d) & grepl("b$|/b$|^[A-Z][a-z]*b\\b", d)
  base_layers[ok]
}

# Filter for Kato-: layers whose top_cm >= 50 (lower part).
.kato_filter <- function(pedon, base_layers) {
  h <- pedon$horizons
  base_layers[!is.na(h$top_cm[base_layers]) &
                  h$top_cm[base_layers] >= 50]
}

# Filter for Amphi-: keep base_layers ONLY if they span both the
# 0-50 band AND the 50-100 band; otherwise return integer(0).
.amphi_filter <- function(pedon, base_layers) {
  h <- pedon$horizons
  tops <- h$top_cm[base_layers]
  has_upper <- any(!is.na(tops) & tops <  50)
  has_lower <- any(!is.na(tops) & tops >= 50 & tops < 100)
  if (has_upper && has_lower) base_layers else integer(0)
}

# Filter for Poly-: count non-contiguous runs in `base_layers`. Two
# layers are "contiguous" if one's bottom_cm equals the next's top_cm.
.poly_filter <- function(pedon, base_layers) {
  if (length(base_layers) < 2L) return(integer(0))
  h <- pedon$horizons
  ord <- order(h$top_cm[base_layers])
  ly  <- base_layers[ord]
  prev_bot <- NA_real_
  runs <- 1L
  for (i in seq_along(ly)) {
    top <- h$top_cm[ly[i]]
    if (!is.na(prev_bot) && abs(top - prev_bot) > 1e-6) runs <- runs + 1L
    prev_bot <- h$bottom_cm[ly[i]]
  }
  if (runs >= 2L) ly else integer(0)
}


# Specifier table: name -> (kind, params). The first 5 are simple
# depth-band specifiers, the next 5 use custom layer filters.
.wrb_specifiers <- list(
  Ano    = list(kind = "depth",  min_top_cm =   0, max_top_cm =  50),
  Epi    = list(kind = "depth",  min_top_cm =   0, max_top_cm =  50),
  Endo   = list(kind = "depth",  min_top_cm =  50, max_top_cm = 100),
  Bathy  = list(kind = "depth",  min_top_cm = 100, max_top_cm = Inf),
  Panto  = list(kind = "depth",  min_top_cm =   0, max_top_cm = Inf),
  Kato   = list(kind = "filter", filter = .kato_filter),
  Amphi  = list(kind = "filter", filter = .amphi_filter),
  Poly   = list(kind = "filter", filter = .poly_filter),
  Supra  = list(kind = "filter", filter = .supra_filter),
  Thapto = list(kind = "filter", filter = .thapto_filter)
)


# Detect a specifier prefix on a qualifier name. Returns a list with
# `prefix` and `base` (the un-prefixed qualifier name with its first
# letter restored to upper case). Returns NULL when no specifier
# prefix matches.
.detect_specifier <- function(qname) {
  for (sp in names(.wrb_specifiers)) {
    if (startsWith(qname, sp) && nchar(qname) > nchar(sp)) {
      base <- substring(qname, nchar(sp) + 1L)
      # Re-capitalise the base (e.g. "Endogleyic" -> "Gleyic").
      base <- paste0(toupper(substring(base, 1, 1)),
                     substring(base, 2))
      return(list(prefix = sp, base = base,
                  spec = .wrb_specifiers[[sp]]))
    }
  }
  NULL
}


# Apply a specifier to a base qualifier function and return a
# DiagnosticResult for the prefixed name.
#
# For a "depth"-kind specifier the returned layers from the base
# qualifier are intersected with [min_top_cm, max_top_cm]. For a
# "filter"-kind specifier the custom filter is called with the base
# layer set; only layers it returns are kept.
.apply_specifier <- function(pedon, prefix, base_qname, spec) {
  full_name <- paste0(prefix,
                       paste0(toupper(substring(base_qname, 1, 1)),
                              substring(base_qname, 2)))
  fn_name <- paste0("qual_", tolower(base_qname))
  fn <- tryCatch(get(fn_name, envir = asNamespace("soilKey")),
                   error = function(e) NULL)
  if (is.null(fn)) {
    return(DiagnosticResult$new(
      name = full_name, passed = NA, layers = integer(0),
      evidence = list(),
      missing = sprintf("base qualifier %s not implemented", base_qname),
      reference = "WRB (2022) Ch 5/6, Specifier"
    ))
  }
  base_res <- tryCatch(fn(pedon), error = function(e) NULL)
  if (is.null(base_res)) {
    return(DiagnosticResult$new(
      name = full_name, passed = NA, layers = integer(0),
      evidence = list(),
      missing = sprintf("base qualifier %s threw error", base_qname),
      reference = "WRB (2022) Ch 5/6, Specifier"
    ))
  }
  if (!isTRUE(base_res$passed)) {
    return(DiagnosticResult$new(
      name = full_name, passed = base_res$passed,
      layers = integer(0),
      evidence = list(base = base_res),
      missing = base_res$missing %||% character(0),
      reference = "WRB (2022) Ch 5/6, Specifier"
    ))
  }
  h <- pedon$horizons
  ly <- base_res$layers
  if (identical(spec$kind, "depth")) {
    keep_mask <- !is.na(h$top_cm[ly]) &
                  h$top_cm[ly] >= spec$min_top_cm &
                  h$top_cm[ly] <  spec$max_top_cm
    kept <- ly[keep_mask]
    extra <- list(depth_band = c(min = spec$min_top_cm,
                                  max = spec$max_top_cm))
  } else if (identical(spec$kind, "filter")) {
    kept <- spec$filter(pedon, ly)
    extra <- list(filter = sprintf("%s.filter", prefix))
  } else {
    rlang::abort(sprintf("Unknown specifier kind: %s", spec$kind))
  }
  passed <- length(kept) > 0L
  DiagnosticResult$new(
    name = full_name, passed = passed,
    layers = if (passed) kept else integer(0),
    evidence = c(list(base = base_res), extra),
    missing = base_res$missing %||% character(0),
    reference = sprintf("WRB (2022) Ch 5/6, %s (%s of %s)",
                          full_name, prefix, base_qname)
  )
}


# ---------------------------------------------------------------------------
# v0.9.105 -- automatic depth-specifier attachment.
#
# WRB 2022 Ch 5 lets a depth specifier (Epi-/Endo-/Bathy-/Amphi-/Panto-/Kato-)
# be prefixed to a qualifier when its diagnostic feature is confined to a depth
# band. The pieces above only fire when a name ARRIVES prefixed; the helpers
# below COMPUTE the right specifier from the feature's actual layers so it can
# be attached automatically (opt-in via classify_wrb2022(specifiers = TRUE)).
# ---------------------------------------------------------------------------

# Qualifiers that take a depth specifier: the subsurface feature/horizon
# qualifiers (those whose qual_*() uses .q_presence). Epipedons and
# surface-by-definition qualifiers (Mollic, Umbric, Chernic, Histic, Hortic,
# Plaggic, Pretic, Terric, Anthric, Anthraquic, Hydragric, Irragric, Mulmic,
# Ornithic, Takyric, Yermic) and the thermal Cryic are EXCLUDED -- their depth
# is definitional, so a specifier (e.g. "Epimollic") would be invalid.
.WRB_DEPTH_SPECIFIABLE <- c(
  "Abruptic", "Albic", "Arenic", "Calcaric", "Calcic", "Cambic", "Dolomitic",
  "Duric", "Ferralic", "Ferric", "Fluvic", "Gleyic", "Gypsic", "Gypsiric",
  "Leptic", "Natric", "Nitic", "Petrocalcic", "Petroduric", "Petrogypsic",
  "Petroplinthic", "Pisoplinthic", "Plinthic", "Protocalcic", "Protogypsic",
  "Retic", "Salic", "Spodic", "Stagnic", "Tephric", "Thionic", "Vertic",
  "Vitric")

# Do the merged depth intervals cover [min .. max] with no gap?
.wrb_layers_contiguous <- function(tops, bots) {
  o <- order(tops)
  t <- tops[o]; b <- bots[o]
  cur_end <- b[1L]
  for (i in seq_along(t)[-1L]) {
    if (t[i] > cur_end) return(FALSE)   # a gap between this layer and the run
    cur_end <- max(cur_end, b[i])
  }
  TRUE
}

# Compute the depth specifier prefix for a feature occupying `layers`.
# Returns "" when no specifier is warranted (the canonical, within-100 cm
# contiguous case, or when depth is indeterminate).
.compute_depth_specifier <- function(pedon, layers) {
  if (length(layers) == 0L) return("")
  h    <- pedon$horizons
  tops <- suppressWarnings(as.numeric(h$top_cm[layers]))
  bots <- suppressWarnings(as.numeric(h$bottom_cm[layers]))
  ok   <- !is.na(tops) & !is.na(bots) & bots > tops
  tops <- tops[ok]; bots <- bots[ok]
  if (!length(tops)) return("")
  overlaps <- function(lo, hi) any(tops < hi & bots > lo)
  in_epi   <- overlaps(0,   50)
  in_endo  <- overlaps(50,  100)
  in_bathy <- overlaps(100, Inf)
  bands <- sum(in_epi, in_endo, in_bathy)
  if (bands == 0L) return("")
  if (in_epi && in_endo && in_bathy) {
    # Panto- ("throughout") only when the feature truly spans the whole
    # profile -- from the surface (top <= 5 cm) down past 100 cm with no
    # gap. A broad subsurface feature that merely overlaps all three bands
    # is the canonical (bare-name) case, not Panto.
    return(if (min(tops) <= 5 && .wrb_layers_contiguous(tops, bots))
             "Panto" else "")
  }
  if (bands == 1L)
    return(if (in_epi) "Epi" else if (in_endo) "Endo" else "Bathy")
  # exactly two bands
  if (in_epi && in_endo)
    return(if (.wrb_layers_contiguous(tops, bots)) "" else "Amphi")
  if (in_endo && in_bathy) return("Kato")   # lower part only
  if (in_epi && in_bathy)  return("Amphi")  # split with a mid gap
  ""
}

# Attach depth specifiers to a set of matched qualifier names, using a
# name -> layers map. Only names in .WRB_DEPTH_SPECIFIABLE are touched.
.apply_depth_specifiers <- function(pedon, qnames, layers_map) {
  vapply(qnames, function(qn) {
    if (!(qn %in% .WRB_DEPTH_SPECIFIABLE)) return(qn)
    sp <- .compute_depth_specifier(pedon, layers_map[[qn]] %||% integer(0))
    if (!nzchar(sp)) return(qn)
    paste0(sp, tolower(substring(qn, 1, 1)), substring(qn, 2))
  }, character(1), USE.NAMES = FALSE)
}
