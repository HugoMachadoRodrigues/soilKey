# ============================================================================
# WRB 2022 (4th ed.) -- Specifier infrastructure (v0.9.2.B)
#
# Specifiers are depth-position prefixes that compose with a base
# qualifier to constrain WHERE the qualifier applies in the profile:
#
#   Ano-    upper 50 cm  (alias of Epi-)
#   Epi-    upper 50 cm
#   Endo-   50 - 100 cm
#   Bathy-  > 100 cm
#   Panto-  whole profile (no depth restriction; just clarifies that
#           the qualifier applies throughout)
#
# Composition example: "Endogleyic" = gleyic-properties layers in the
# 50-100 cm range only.
#
# v0.9.2.B implements 5 specifiers (Ano-, Epi-, Endo-, Bathy-, Panto-).
# The remaining specifiers from WRB 2022 Ch 6 (Kato-, Poly-, Supra-,
# Thapto-, Amphi-) require designation-chain analysis or buried-soil
# flags that are not yet first-class in the horizon schema; they are
# scheduled for v0.9.3.
#
# Specifiers are dispatched by `resolve_wrb_qualifiers` automatically
# whenever it sees a YAML name that starts with one of the recognised
# prefixes. There is no need to define a `qual_endogleyic` function;
# the dispatcher strips the prefix, calls the base qual_*, and
# intersects the returned layer set with the specifier's depth band.
# ============================================================================


# Specifier table: prefix -> (min_top_cm, max_top_cm).
.wrb_specifiers <- list(
  Ano   = c(min =   0, max =  50),
  Epi   = c(min =   0, max =  50),
  Endo  = c(min =  50, max = 100),
  Bathy = c(min = 100, max = Inf),
  Panto = c(min =   0, max = Inf)
)


# Detect a specifier prefix on a qualifier name. Returns a list with
# `prefix`, `base` (the un-prefixed qualifier), and the depth band.
# Returns NULL when no specifier matches.
.detect_specifier <- function(qname) {
  for (sp in names(.wrb_specifiers)) {
    if (startsWith(qname, sp) && nchar(qname) > nchar(sp)) {
      base <- substring(qname, nchar(sp) + 1L)
      # Re-capitalise the base (e.g. "Endogleyic" -> "Gleyic").
      base <- paste0(toupper(substring(base, 1, 1)),
                     substring(base, 2))
      return(list(prefix = sp, base = base,
                  min_top_cm = .wrb_specifiers[[sp]][["min"]],
                  max_top_cm = .wrb_specifiers[[sp]][["max"]]))
    }
  }
  NULL
}


# Apply a specifier (prefix + depth band) to a base qualifier function
# and return a DiagnosticResult for the prefixed name. Layers from
# the base qualifier are intersected with the specifier depth range.
.apply_specifier <- function(pedon, prefix, base_qname,
                                min_top_cm, max_top_cm) {
  full_name <- paste0(prefix, tolower(base_qname))
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
  in_band <- !is.na(h$top_cm[ly]) &
              h$top_cm[ly] >= min_top_cm &
              h$top_cm[ly] <  max_top_cm
  kept <- ly[in_band]
  passed <- length(kept) > 0L
  DiagnosticResult$new(
    name = full_name, passed = passed,
    layers = if (passed) kept else integer(0),
    evidence = list(base = base_res,
                    depth_band = c(min = min_top_cm, max = max_top_cm)),
    missing = base_res$missing %||% character(0),
    reference = sprintf("WRB (2022) Ch 5/6, %s (%s of %s)",
                          full_name, prefix, base_qname)
  )
}
