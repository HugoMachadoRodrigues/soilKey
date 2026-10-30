# =============================================================================
# v0.9.314: the WRB 2022 mollic and umbric horizons read layer by layer.
#
# WRB 2022 Ch 2.1 (p. 23): "If a diagnostic horizon consists of several
# subhorizons, the diagnostic criteria (except thickness) must be fulfilled in
# every subhorizon separately (averages are not calculated), unless stated
# otherwise." In the mollic horizon (3.1.20, p. 52) and the umbric horizon
# (3.1.39, p. 69) only the base saturation is "on a weighted average". So the
# structure (criterion 1), the soil organic carbon (2) and the colour (3) are
# read in each layer, and the horizon is the run of layers from the mineral
# soil surface that meet them; thickness and base saturation are then read on
# that run. Before v0.9.314:
#   * the carbon test passed when any one layer had 0.6%;
#   * the structure was a share of the whole dark stack;
#   * the colour accepted proxies: a dry value of 5 or less with no moist
#     colour, a carbon content of 1.5% or more with no colour at all, and the
#     dry chroma for the moist chroma.
# =============================================================================

# How a colour that is not wholly recorded is read:
#   dry_from_moist  a moist value recorded and a dry value not: the dry value
#                   is read as the moist value + 1, as soilKey has read it since
#                   v0.9.18 (a soil is lighter dry than moist);
#   proxies         which of the three older proxies stand where the text
#                   leaves a layer undecided: "dry_only" (a dry value <= 5 and
#                   no moist colour), "carbon" (>= 1.5% SOC and no colour at
#                   all), "dry_chroma" (the dry chroma for the moist chroma).
# Hugo (2026-10-29): the dry value from the moist one, and the carbon proxy
# only, where no colour at all is recorded.
.WRB_MOLLIC_COLOUR_READING <- list(dry_from_moist = TRUE, proxies = "carbon")

.wrb_le <- function(x, k) ifelse(is.na(x), NA, x <= k)
.wrb_ge <- function(x, k) ifelse(is.na(x), NA, x >= k)

#' Criterion 3 of the WRB 2022 mollic horizon (3.1.20, p. 52) or the umbric
#' horizon (3.1.39, p. 69), per layer, TRUE / FALSE / NA:
#'   a. value <= 3 moist and <= 5 dry, chroma <= 3 moist;
#'   b. (mollic) CaCO3 equivalent + gypsum >= 15 and < 40%, value and chroma
#'      <= 3 moist, >= 1% SOC;
#'   c. (mollic; umbric b) CaCO3 + gypsum >= 40% (mollic only) or a texture of
#'      loamy sand or coarser, value <= 5 and chroma <= 3 moist, >= 2.5% SOC.
#' "In >= 90% of the exposed area": the matrix colour of the layer is read.
#' Below a plough layer the subhorizons decide (see .wrb_mollic_layer_status()).
#' @noRd
.wrb_mollic_colour_layers <- function(h, umbric = FALSE,
                                      reading = .WRB_MOLLIC_COLOUR_READING) {
  n <- nrow(h)
  num <- function(col) suppressWarnings(as.numeric(h[[col]] %||% rep(NA_real_, n)))
  vm <- num("munsell_value_moist"); cm <- num("munsell_chroma_moist")
  vd <- num("munsell_value_dry");   cd <- num("munsell_chroma_dry")
  oc <- num("oc_pct")
  carb <- num("caco3_pct") + ifelse(is.na(num("caso4_pct")), 0, num("caso4_pct"))
  vd_ok <- .wrb_le(vd, 5)
  inferred_dry <- is.na(vd) & !is.na(vm)
  if (isTRUE(reading$dry_from_moist))
    vd_ok <- ifelse(inferred_dry, vm + 1 <= 5, vd_ok)
  a <- .and3v(.and3v(.wrb_le(vm, 3), .wrb_le(cm, 3)), vd_ok)
  tx <- .wrb_texture_class(h)
  coarse <- ifelse(is.na(tx), NA, tx %in% c("S", "LS"))
  c_gate <- if (umbric) coarse else .any3v(coarse, .wrb_ge(carb, 40))
  c_ <- .and3v(.and3v(c_gate, .and3v(.wrb_le(vm, 5), .wrb_le(cm, 3))), .wrb_ge(oc, 2.5))
  out <- .any3v(a, c_)
  if (!umbric) {
    b_gate <- ifelse(is.na(carb), NA, carb >= 15 & carb < 40)
    b_ <- .and3v(.and3v(b_gate, .and3v(.wrb_le(vm, 3), .wrb_le(cm, 3))), .wrb_ge(oc, 1))
    out <- .any3v(out, b_)
  }
  source <- ifelse(is.na(out), NA_character_,
                   ifelse(inferred_dry & isTRUE(reading$dry_from_moist), "moist colour, dry value as moist + 1",
                          "colour as recorded"))
  # the older proxies, only where the text leaves the layer undecided
  px <- reading$proxies %||% character(0)
  if (isTRUE(px)) px <- c("dry_only", "carbon", "dry_chroma")
  use <- function(rule, v, label) {
    if (!rule %in% px) return(invisible())
    hit <- is.na(out) & !is.na(v)
    out[hit] <<- v[hit]; source[hit] <<- label
  }
  ch <- ifelse(!is.na(cm), cm, cd)
  use("dry_only", ifelse(is.na(vm) & !is.na(vd), vd <= 5 & (is.na(ch) | ch <= 3), NA),
      "dry colour only (proxy)")
  use("carbon", ifelse(is.na(vm) & is.na(vd) & !is.na(oc) & oc >= 1.5, TRUE, NA),
      "no colour recorded, >= 1.5% SOC (proxy)")
  use("dry_chroma", ifelse(!is.na(vm) & is.na(cm) & !is.na(cd), cd <= 3 & vm <= 3, NA),
      "dry chroma for the moist chroma (proxy)")
  out <- as.logical(out)
  attr(out, "source") <- source
  out
}

#' The layers of the WRB 2022 mollic or umbric horizon, each read on criteria 1
#' (structure; not recorded does not block it, Hugo 2026-10-29), 2 (>= 0.6%
#' SOC) and 3 (colour), and on being mineral material (< 20% SOC). A plough
#' layer takes the colour of the subhorizon below it when that one meets
#' criterion 3 ("or of the subhorizons below any plough layer").
#' @return A list: `status` (TRUE / FALSE / NA per layer), the three criteria
#'   per layer, and `missing` per layer.
#' @noRd
.wrb_mollic_layer_status <- function(h, umbric = FALSE, min_oc = 0.6,
                                     reading = .WRB_MOLLIC_COLOUR_READING) {
  n <- nrow(h)
  oc <- suppressWarnings(as.numeric(h$oc_pct %||% rep(NA_real_, n)))
  colour0 <- .wrb_mollic_colour_layers(h, umbric = umbric, reading = reading)
  colour <- as.logical(colour0)
  plough <- .wrb_plough_layer(h)
  ord <- order(h$top_cm)
  for (k in rev(seq_along(ord))) {
    i <- ord[k]
    if (!plough[i] || k == length(ord)) next
    j <- ord[k + 1L]
    if (is.na(h$bottom_cm[i]) || is.na(h$top_cm[j]) || abs(h$top_cm[j] - h$bottom_cm[i]) > 1) next
    colour[i] <- .any3v(colour[i], colour[j])
  }
  carbon <- .wrb_ge(oc, min_oc)
  mineral <- ifelse(is.na(oc), TRUE, oc < 20)
  structure <- .wrb_aggregate_structure_le_10cm(h)
  structure[is.na(structure)] <- TRUE
  status <- .and3v(.and3v(.and3v(colour, carbon), structure), mineral)
  missing <- lapply(seq_len(n), function(i) c(
    if (is.na(colour[i])) c("munsell_value_moist", "munsell_chroma_moist", "munsell_value_dry"),
    if (is.na(carbon[i])) "oc_pct"))
  list(status = as.logical(status), colour = as.logical(colour),
       colour_source = attr(colour0, "source"),
       carbon = as.logical(carbon), structure = as.logical(structure),
       missing = missing)
}

#' The run of layers of a WRB 2022 mollic or umbric horizon: from the mineral
#' soil surface down, while each layer meets the per-layer criteria
#' (.wrb_mollic_layer_status()). `layers` meet them; `maybe` goes on through
#' layers that are undecided until one fails, so that a thickness made up by
#' them is undecided.
#' @noRd
.wrb_mollic_run <- function(h, status, missing, surface_top_cm = 5) {
  ms <- .q_mineral_surface_cm(h)
  out <- list(layers = integer(0), maybe = integer(0), first = NA_integer_,
              mineral_surface_cm = ms, missing = character(0))
  if (is.na(ms)) return(out)
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord]) & h$top_cm[ord] >= ms - 1e-6]
  undecided <- FALSE
  last_bottom <- NA_real_
  for (i in ord) {
    if (!length(out$maybe)) {
      if (h$top_cm[i] > ms + surface_top_cm) break
      out$first <- i
    } else if (is.na(last_bottom) || abs(h$top_cm[i] - last_bottom) > 1) {
      break
    }
    if (isFALSE(status[i])) break
    out$maybe <- c(out$maybe, i)
    last_bottom <- h$bottom_cm[i]
    if (isTRUE(status[i]) && !undecided) {
      out$layers <- c(out$layers, i)
    } else {
      undecided <- TRUE
      out$missing <- unique(c(out$missing, missing[[i]]))
    }
  }
  out
}

#' The per-layer criteria of the mollic or umbric horizon as subtests over the
#' run: colour, organic carbon and structure pass on the layers of the run,
#' are undecided where only undecided layers could make it, and fail where the
#' surface layer fails.
#' @noRd
.wrb_mollic_layer_subtests <- function(h, ls, run) {
  L <- run$layers
  one <- function(v, missing, note, source = NULL) {
    # with no layer in the run, the surface layer tells which criterion fails
    passed <- if (length(L)) TRUE
              else if (is.na(run$first)) FALSE
              else v[run$first]
    .subtest_result(
      passed  = passed, layers = L,
      missing = if (is.na(passed)) missing else character(0),
      details = if (is.null(source)) data.frame(layer = run$maybe, passed = v[run$maybe])
                else data.frame(layer = run$maybe, passed = v[run$maybe], source = source[run$maybe],
                                stringsAsFactors = FALSE),
      notes   = note)
  }
  list(
    color = one(ls$colour, c("munsell_value_moist", "munsell_chroma_moist", "munsell_value_dry"),
                "WRB 2022 criterion 3, in every layer of the horizon (Ch 2.1, p. 23)",
                source = ls$colour_source),
    organic_carbon = one(ls$carbon, "oc_pct",
                         "WRB 2022 criterion 2, >= 0.6% SOC in every layer of the horizon"),
    structure = one(ls$structure, c("structure_type", "structure_size"),
                    "WRB 2022 criterion 1, in every layer of the horizon; not recorded does not block it"))
}
