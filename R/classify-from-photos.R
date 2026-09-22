# =============================================================================
# v0.9.99 -- field-photo-only classification.
#
# classify_from_photos() assembles a PedonRecord entirely from vision-language
# extraction of field photographs (Munsell colour per horizon, plus optional
# site metadata from a field sheet), optionally back-fills the missing horizon
# attributes from a SoilGrids depth prior, and runs the three deterministic
# keys. No laboratory data is required.
#
# The taxonomic key is never delegated to a model: the VLM only fills the
# PedonRecord, and the resulting classification carries a low evidence grade
# (D for VLM-extracted, C where a SoilGrids prior contributed) so the user
# always knows the result rests on photo evidence, not measurement.
# =============================================================================


#' Classify a soil profile from field photographs alone
#'
#' A no-lab-data pipeline: profile photographs are sent to a vision-language
#' model for Munsell-colour and (optionally) site-metadata extraction; the
#' missing horizon attributes are back-filled from a SoilGrids depth prior;
#' and the WRB 2022, SiBCS 5 and USDA Soil Taxonomy keys are run on the
#' assembled \code{\link{PedonRecord}}.
#'
#' Because every value originates from a photograph or a spatial prior, the
#' classification's evidence grade is low by construction (\code{D} for
#' VLM-extracted attributes, \code{C} where a SoilGrids prior contributed).
#' The result is a screening estimate, not a substitute for a described and
#' sampled profile.
#'
#' @param images Either a character vector of profile-photo paths, or a
#'        named list with elements \code{profile} (character vector,
#'        required) and \code{fieldsheet} (character vector, optional).
#' @param lat,lon Optional decimal-degree coordinates. When supplied they
#'        seed \code{pedon$site} and are used for the SoilGrids fetch; a
#'        field sheet can also supply them through extraction.
#' @param country Optional ISO-2 country code; passed through to the
#'        constructed pedon's site metadata.
#' @param provider A vision-language provider: an \pkg{ellmer} chat object
#'        for live use, or a \code{MockVLMProvider} for testing and
#'        offline demos. Required -- there is no default, so a real
#'        classification is never produced from canned data by accident.
#' @param systems Character vector, any subset of \code{c("wrb", "sibcs",
#'        "usda")}.
#' @param soilgrids If \code{TRUE} (default) missing horizon attributes are
#'        back-filled from a SoilGrids depth prior via
#'        \code{\link{apply_soilgrids_depth_prior}}.
#' @param depth_profiles Optional named list of six-slice SoilGrids depth
#'        profiles, forwarded to \code{\link{apply_soilgrids_depth_prior}}.
#'        Supplying it skips the network call.
#' @param on_missing Forwarded to the classifiers; default \code{"silent"}.
#' @return A named list with one \code{\link{ClassificationResult}} per
#'         requested system (\code{$wrb}, \code{$sibcs}, \code{$usda}),
#'         the constructed \code{$pedon}, its \code{$provenance} ledger,
#'         and a one-row \code{$summary} data frame. If extraction yields
#'         no horizons the list instead carries \code{$error} and a
#'         \code{NULL} pedon.
#' @seealso \code{\link{extract_munsell_from_photo}},
#'          \code{\link{apply_soilgrids_depth_prior}},
#'          \code{\link{compute_per_attribute_evidence_grade}}.
#' @examples
#' \dontrun{
#' # Live use with an ellmer chat:
#' res <- classify_from_photos(
#'   images   = list(profile = "profile.jpg", fieldsheet = "sheet.jpg"),
#'   lat = -22.7, lon = -43.6, country = "BR",
#'   provider = ellmer::chat_anthropic())
#' res$wrb$name
#' res$wrb$evidence_grade   # "D" or "C"
#' }
#' @export
classify_from_photos <- function(images,
                                 lat = NULL, lon = NULL, country = NULL,
                                 provider = NULL,
                                 systems = c("wrb", "sibcs", "usda"),
                                 soilgrids = TRUE,
                                 depth_profiles = NULL,
                                 on_missing = "silent") {

  if (is.null(provider)) {
    rlang::abort(paste0("classify_from_photos() needs a `provider`: an ",
                        "ellmer chat object for live use, or ",
                        "MockVLMProvider$new() for testing."))
  }
  systems <- match.arg(systems, c("wrb", "sibcs", "usda"), several.ok = TRUE)

  # Normalise `images` to a list(profile=, fieldsheet=).
  if (is.character(images)) images <- list(profile = images)
  if (!is.list(images)) {
    rlang::abort("`images` must be a character vector or a named list")
  }
  profile_imgs <- images$profile %||% character(0)
  sheet_imgs   <- images$fieldsheet %||% character(0)
  if (length(profile_imgs) == 0L) {
    rlang::abort("`images` must include at least one profile photo")
  }

  fail <- function(msg) {
    list(wrb = NULL, sibcs = NULL, usda = NULL,
         pedon = NULL, provenance = NULL, error = msg)
  }

  # Empty pedon seeded with whatever site metadata the caller gave.
  pedon <- PedonRecord$new(
    site = list(id = "photo-pedon", lat = lat, lon = lon, country = country),
    horizons = make_empty_horizons(0L)
  )

  # --- 1. Munsell colour from each profile photo -------------------------
  for (img in profile_imgs) {
    if (!file.exists(img)) return(fail(sprintf("image not found: %s", img)))
    ok <- tryCatch({
      extract_munsell_from_photo(pedon, img, provider)
      TRUE
    }, error = function(e) conditionMessage(e))
    if (!isTRUE(ok)) {
      return(fail(sprintf("Munsell extraction failed: %s", ok)))
    }
  }

  # --- 2. Site metadata from any field-sheet image -----------------------
  for (img in sheet_imgs) {
    if (!file.exists(img)) return(fail(sprintf("image not found: %s", img)))
    tryCatch(
      extract_site_from_fieldsheet(pedon, img, provider),
      error = function(e)
        rlang::warn(sprintf("field-sheet extraction failed: %s",
                            conditionMessage(e))))
  }

  if (is.null(pedon$horizons) || nrow(pedon$horizons) == 0L) {
    return(fail("VLM extraction returned no horizons"))
  }

  # --- 3. SoilGrids depth prior for the missing attributes ---------------
  if (isTRUE(soilgrids)) {
    have_coords <- !is.null(pedon$site$lat) && !is.null(pedon$site$lon) &&
                   !is.na(pedon$site$lat) && !is.na(pedon$site$lon)
    if (have_coords || !is.null(depth_profiles)) {
      pedon <- apply_soilgrids_depth_prior(pedon,
                                           depth_profiles = depth_profiles)
    } else {
      rlang::warn(paste0("classify_from_photos(): soilgrids = TRUE but no ",
                         "coordinates available; skipping the depth prior"))
    }
  }

  # --- 4. Run the requested deterministic keys ---------------------------
  classifiers <- list(
    wrb   = function(p) classify_wrb2022(p, on_missing = on_missing),
    sibcs = function(p) classify_sibcs(p,   on_missing = on_missing),
    usda  = function(p) classify_usda(p,    on_missing = on_missing)
  )
  out <- list(wrb = NULL, sibcs = NULL, usda = NULL)
  for (sys in systems) {
    out[[sys]] <- tryCatch(classifiers[[sys]](pedon),
                           error = function(e) e)
  }

  pick_name <- function(r) {
    if (is.null(r) || inherits(r, "error")) NA_character_
    else r$name %||% NA_character_
  }
  pick_grade <- function(r) {
    if (is.null(r) || inherits(r, "error")) NA_character_
    else r$evidence_grade %||% NA_character_
  }

  out$pedon      <- pedon
  out$provenance <- pedon$provenance
  out$summary    <- data.frame(
    system         = c("wrb", "sibcs", "usda"),
    name           = c(pick_name(out$wrb), pick_name(out$sibcs),
                       pick_name(out$usda)),
    evidence_grade = c(pick_grade(out$wrb), pick_grade(out$sibcs),
                       pick_grade(out$usda)),
    stringsAsFactors = FALSE
  )
  out$error <- NULL
  out
}
