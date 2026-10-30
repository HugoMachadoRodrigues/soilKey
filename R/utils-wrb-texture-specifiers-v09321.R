# =============================================================================
# v0.9.315 -- WRB 2022 Ch 2.1 and 2.3.1: texture qualifiers of one slash group
# at different depths.
#
# Ch 2.1 (p. 24-25): "Qualifiers that are mutually exclusive may apply to the
# same soil at different depths. In this case, they can be used both, each one
# with the respective specifier (see Chapter 2.3). If no specifier is used,
# only the first applicable qualifier can be used." Ch 2.3.1, rule 2 (p. 27)
# says it again for the qualifiers that refer to a layer, and places the
# texture qualifiers "in the sequence from the top to the bottom of the
# profile"; the example of p. 26 is "(Anoloamic, Endoclayic, ...)".
#
# The texture qualifiers (Arenic, Clayic, Loamic, Siltic; Ch 5, p. 128, 130,
# 140, 146) refer to a layer and take the specifiers of rule 2, "no
# subqualifier if a limiting layer starts < 60 cm from the mineral soil
# surface". resolve_wrb_qualifiers() uses the helpers below only when it is
# asked for specifiers (specifiers = TRUE); without them the first texture
# qualifier that applies is the only one, as before.
# =============================================================================

# The specifier of Ch 2.3.1 rule 2 (p. 27) for a qualifier whose layer is made
# of the horizons `layers`, depths counted from the surface `ms` (the mineral
# soil surface, or the soil surface where the definition says so); `lim` is
# the upper limit of a limiting layer (NA if none). With one run of adjoining
# horizons starting within 100 cm, upper limit t and lower limit b:
#   Epi-   b <= 50 ("and no such layer occurs between 50 and 100 cm");
#   Endo-  t >= 50 ("and no such layer occurs < 50 cm");
#   Ano-   t = 0 and 50 < b < 100, nothing between 99 and 100 cm and not
#          directly above a limiting layer;
#   Amphi- 0 < t < 50 (none of it < 1 cm) and 50 < b < 100, as Ano-;
#   Kato-  0 < t < 50 (none of it < 1 cm) and b >= 100 or at a limiting layer
#          starting > 50 cm;
#   Panto- t = 0 and b >= 100 or at a limiting layer starting > 50 cm.
# Two or more runs within 100 cm, interrupted by layers that do not qualify:
# Poly-; Epi- (all of them above 50 cm) and Endo- (all below 50 cm) may also
# fit them. Runs that start deeper than 100 cm (a qualifier whose depth range
# goes further, as Spodic's 200 cm) count for Endo- only. The specifier is
# returned when exactly one fits, NA otherwise (no specifier, or a choice the
# text leaves to the user).
.wrb_layer_specifier <- function(h, layers, ms, lim = NA_real_) {
  runs <- .q_layer_runs(h, layers)
  if (!length(runs)) return(NA_character_)
  eps <- 1e-6
  lr <- if (is.na(lim)) NA_real_ else lim - ms
  at_lim <- function(b) !is.na(lr) && abs(b - lr) < eps
  t <- vapply(runs, function(r) r$top - ms, numeric(1))
  b <- vapply(runs, function(r) r$bottom - ms, numeric(1))
  in100 <- t < 100
  n100 <- sum(in100)
  one <- n100 == 1L
  k <- if (one) which(in100) else 1L
  deep_end  <- b[k] >= 100 || (at_lim(b[k]) && lr > 50)
  mid_end   <- b[k] > 50 && b[k] <= 99 && !at_lim(b[k])
  at_top    <- abs(t[k]) < eps
  below_top <- t[k] >= 1 && t[k] < 50
  fits <- c(Epi = n100 >= 1L && all(b[in100] <= 50), Endo = all(t >= 50),
            Ano = one && at_top && mid_end, Amphi = one && below_top && mid_end,
            Kato = one && below_top && deep_end, Panto = one && at_top && deep_end,
            Poly = n100 >= 2L)
  if (sum(fits) == 1L) names(fits)[fits] else NA_character_
}

# The members of a texture slash group that apply (`passed`, in list order,
# their horizons in `layers_map`), each with its specifier, when the text lets
# them be used together: no limiting layer < 60 cm from the mineral soil
# surface; the layers described, with a texture class, to 100 cm or to the
# limiting layer (Ch 2.3.1, p. 26: depth subqualifiers "are only used if the
# relevant soil characteristics are reported until >= 100 cm"); the members at
# different depths (no horizon in two of them); and one specifier for each.
# Returns a named character vector (member -> name with its specifier), or
# NULL when only the first member can be used.
.wrb_texture_group_specified <- function(pedon, passed, layers_map) {
  if (length(passed) < 2L) return(NULL)
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) return(NULL)
  lim <- tryCatch(.barrier_top_cm(pedon), error = function(e) NA_real_)
  if (!is.na(lim) && lim - ms < 60) return(NULL)
  to <- ms + (if (is.na(lim)) 100 else min(100, lim - ms))
  # the window described without gaps, every mineral layer with its class
  win <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & h$bottom_cm > ms &
                 h$top_cm < to)
  if (!length(win)) return(NULL)
  win <- win[order(h$top_cm[win])]
  cur <- ms
  for (i in win) {
    if (h$top_cm[i] > cur + 1e-6) return(NULL)
    cur <- max(cur, h$bottom_cm[i])
  }
  if (cur < to - 1e-6) return(NULL)
  tex <- .wrb_texture_class(h)
  known <- !is.na(tex) | (!is.na(h$clay_pct) & h$clay_pct >= 40) |
    !.wrb_mineral_layer(h)
  if (!all(known[win])) return(NULL)
  ly <- lapply(passed, function(q) layers_map[[q]] %||% integer(0))
  all_ly <- unlist(ly)
  if (!length(all_ly) || anyDuplicated(all_ly)) return(NULL)
  sp <- vapply(ly, function(l) .wrb_layer_specifier(h, l, ms, lim), character(1))
  if (anyNA(sp)) return(NULL)
  stats::setNames(paste0(sp, tolower(substring(passed, 1, 1)), substring(passed, 2)),
                  passed)
}
