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

# Layers above the deepest "barrier": continuous_rock, any petro-
# cemented horizon, or technic_hard_material.
.barrier_top_cm <- function(pedon) {
  rk <- continuous_rock(pedon)
  pc <- petrocalcic(pedon); pd <- petroduric(pedon)
  pg <- petrogypsic(pedon); pp <- petroplinthic(pedon)
  th <- technic_hard_material(pedon)
  layers <- unique(unlist(lapply(list(rk, pc, pd, pg, pp, th),
                                  function(d) if (isTRUE(d$passed)) d$layers else integer(0))))
  if (length(layers) == 0L) return(NA_real_)
  h <- pedon$horizons
  min(h$top_cm[layers], na.rm = TRUE)
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
