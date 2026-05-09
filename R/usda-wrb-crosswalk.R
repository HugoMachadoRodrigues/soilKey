# =============================================================================
# USDA Soil Taxonomy <-> WRB Reference Soil Group cross-walk (v0.9.74)
#
# Based on the IUSS Working Group WRB (2022) "World Reference Base for
# Soil Resources" 4th edition, Annex 6 ("Correlation between WRB
# Reference Soil Groups and Keys to Soil Taxonomy") and the long-
# standing FAO/USDA correlation literature.
#
# This is a many-to-many mapping: a USDA Suborder typically maps to
# 1-3 plausible WRB RSGs depending on context (mineralogy, climate,
# diagnostic horizons not captured in higher-level taxonomy). The
# function below returns the SINGLE most common WRB RSG for benchmark
# purposes -- the "reasonable expectation" given USDA classification.
#
# Coverage gates:
#   - Order alone: 12 USDA Orders, ~20 WRB RSGs covered
#   - Order + Suborder: ~70 USDA Suborders, finer disambiguation
# =============================================================================


#' USDA Soil Taxonomy <-> WRB Reference Soil Group correlation table
#'
#' Returns the single most-common WRB RSG for a given USDA Order +
#' optional Suborder. Based on IUSS WRB (2022) Annex 6.
#'
#' @section Caveat:
#' This is a "best-guess" cross-walk for benchmark validation only.
#' Real-world correlation requires per-pedon evaluation of WRB
#' diagnostic horizons. Use this function to derive a reasonable
#' \emph{expected} WRB classification from a USDA-classified pedon
#' (e.g.\ from KSSL/NASIS) so that \code{classify_wrb2022()} can be
#' validated against an external taxonomy on the same profiles.
#'
#' @param usda_order Character vector of USDA Order names. Case-
#'        insensitive; trailing 's' stripped (e.g.\ both "Mollisols"
#'        and "Mollisol" accepted).
#' @param usda_suborder Optional character vector of USDA Suborder
#'        names (case-insensitive) used to refine the mapping.
#'        Same length as \code{usda_order} or recycled.
#'
#' @return Character vector of WRB Reference Soil Group names
#'   (singular, no plural 's'). \code{NA} for unrecognised inputs.
#'
#' @section References:
#' IUSS Working Group WRB (2022). \emph{World Reference Base for
#' Soil Resources}, 4th edition, Annex 6. International Union of
#' Soil Sciences, Vienna.
#'
#' @examples
#' usda_to_wrb_rsg("Mollisols")
#' #> "Phaeozem"
#' usda_to_wrb_rsg("Aridisols", "Salids")
#' #> "Solonchak"
#' usda_to_wrb_rsg(c("Spodosols", "Oxisols", "Vertisols"))
#' #> c("Podzol", "Ferralsol", "Vertisol")
#'
#' @export
usda_to_wrb_rsg <- function(usda_order, usda_suborder = NULL) {
  norm <- function(x) {
    if (is.null(x) || all(is.na(x))) return(NA_character_)
    s <- tolower(trimws(as.character(x)))
    sub("s$", "", s)
  }
  ord <- norm(usda_order)
  sub <- if (!is.null(usda_suborder)) norm(usda_suborder)
          else rep(NA_character_, length(ord))
  if (length(sub) == 1L && length(ord) > 1L)
    sub <- rep(sub, length(ord))

  # Order-level default mapping (most common WRB RSG)
  order_map <- c(
    histosol    = "Histosol",
    andisol     = "Andosol",
    gelisol     = "Cryosol",
    spodosol    = "Podzol",
    oxisol      = "Ferralsol",
    vertisol    = "Vertisol",
    aridisol    = "Calcisol",   # default; refined by suborder below
    ultisol     = "Acrisol",
    mollisol    = "Phaeozem",   # default; refined by suborder
    alfisol     = "Luvisol",
    inceptisol  = "Cambisol",
    entisol     = "Regosol"     # default; refined by suborder
  )
  out <- order_map[ord]
  names(out) <- NULL

  # Suborder-level refinement for the major Orders that disambiguate
  # heavily on suborder.
  refine <- function(o, s) {
    if (is.na(o) || is.na(s)) return(NA_character_)
    key <- paste(o, s, sep = "/")
    refinements <- c(
      # Aridisols
      "aridisol/salid"   = "Solonchak",
      "aridisol/calcid"  = "Calcisol",
      "aridisol/gypsid"  = "Gypsisol",
      "aridisol/argid"   = "Solonetz",   # argillic + sodic; could also be Luvisol
      "aridisol/cambid"  = "Cambisol",
      "aridisol/durid"   = "Durisol",
      # Mollisols
      "mollisol/aquoll"  = "Phaeozem",   # gleyic-like Phaeozem
      "mollisol/cryoll"  = "Phaeozem",
      "mollisol/udoll"   = "Phaeozem",
      "mollisol/rendoll" = "Leptosol",   # rendzic Leptosol
      "mollisol/ustoll"  = "Kastanozem",
      "mollisol/xeroll"  = "Kastanozem",
      "mollisol/alboll"  = "Albeluvisol",
      "mollisol/gelisol" = "Cryosol",
      # Entisols
      "entisol/aquent"   = "Fluvisol",   # also Gleysol
      "entisol/arent"    = "Anthrosol",
      "entisol/fluvent"  = "Fluvisol",
      "entisol/orthent"  = "Regosol",
      "entisol/psamment" = "Arenosol",
      # Inceptisols
      "inceptisol/aquept" = "Gleysol",
      "inceptisol/anthrept" = "Anthrosol",
      "inceptisol/cryept"   = "Cambisol",
      "inceptisol/udept"    = "Cambisol",
      "inceptisol/ustept"   = "Cambisol",
      "inceptisol/xerept"   = "Cambisol",
      # Alfisols
      "alfisol/aqualf"  = "Planosol",     # also Luvisol with stagnic
      "alfisol/cryalf"  = "Luvisol",
      "alfisol/udalf"   = "Luvisol",
      "alfisol/ustalf"  = "Lixisol",
      "alfisol/xeralf"  = "Luvisol",
      # Ultisols
      "ultisol/aquult"  = "Acrisol",     # also Plinthosol
      "ultisol/humult"  = "Alisol",
      "ultisol/udult"   = "Acrisol",
      "ultisol/ustult"  = "Acrisol",
      "ultisol/xerult"  = "Acrisol",
      # Vertisols (all subgroups go to Vertisol)
      "vertisol/aquert" = "Vertisol",
      "vertisol/cryert" = "Vertisol",
      "vertisol/torrert" = "Vertisol",
      "vertisol/udert"  = "Vertisol",
      "vertisol/ustert" = "Vertisol",
      "vertisol/xerert" = "Vertisol",
      # Spodosols
      "spodosol/aquod"  = "Podzol",      # also Stagnosol with podzic
      "spodosol/cryod"  = "Podzol",
      "spodosol/humod"  = "Podzol",
      "spodosol/orthod" = "Podzol",
      # Oxisols
      "oxisol/aquox"    = "Plinthosol",  # plinthitic Ferralsol
      "oxisol/torrox"   = "Ferralsol",
      "oxisol/udox"     = "Ferralsol",
      "oxisol/ustox"    = "Ferralsol",
      "oxisol/perox"    = "Ferralsol",
      # Histosols
      "histosol/fibrist" = "Histosol",
      "histosol/folist"  = "Histosol",
      "histosol/hemist"  = "Histosol",
      "histosol/saprist" = "Histosol",
      # Gelisols
      "gelisol/histel"  = "Cryosol",
      "gelisol/orthel"  = "Cryosol",
      "gelisol/turbel"  = "Cryosol",
      # Andisols
      "andisol/aquand"  = "Andosol",
      "andisol/cryand"  = "Andosol",
      "andisol/udand"   = "Andosol",
      "andisol/ustand"  = "Andosol",
      "andisol/vitrand" = "Andosol",
      "andisol/xerand"  = "Andosol",
      "andisol/torrand" = "Andosol"
    )
    refinements[key]
  }
  for (i in seq_along(out)) {
    r <- refine(ord[i], sub[i])
    if (!is.na(r)) out[i] <- r
  }
  unname(out)
}


#' Annotate KSSL/NASIS pedons with a derived WRB Reference Soil Group
#'
#' Applies \code{\link{usda_to_wrb_rsg}} to each pedon's USDA
#' classification (preserved as \code{site$reference_usda} +
#' \code{site$reference_usda_suborder} by
#' \code{\link{load_kssl_pedons_gpkg}}) and writes the result to
#' \code{site$reference_wrb_from_usda} -- a "best-guess" expected WRB
#' label for benchmark comparison.
#'
#' Pedons that already have \code{site$reference_wrb} populated (e.g.\
#' from external sources) are left untouched.
#'
#' @param pedons List of \code{\link{PedonRecord}} objects.
#' @return The same list, with \code{site$reference_wrb_from_usda}
#'   populated where USDA classification is present.
#'
#' @export
annotate_wrb_from_usda <- function(pedons) {
  for (i in seq_along(pedons)) {
    pr <- pedons[[i]]
    if (!is.null(pr$site$reference_wrb_from_usda)) next
    ord <- pr$site$reference_usda %||% NA_character_
    sub <- pr$site$reference_usda_suborder %||% NA_character_
    if (is.na(ord) || !nzchar(ord)) next
    pr$site$reference_wrb_from_usda <- usda_to_wrb_rsg(ord, sub)
    pedons[[i]] <- pr
  }
  pedons
}


#' Benchmark soilKey WRB predictions against a USDA-derived ground truth
#'
#' Convenience wrapper: applies \code{\link{annotate_wrb_from_usda}}
#' to attach derived WRB labels, runs \code{\link{classify_wrb2022}}
#' on each pedon, and returns top-1 accuracy + per-RSG recall.
#'
#' @param pedons List of \code{\link{PedonRecord}} objects with
#'        \code{site$reference_usda} populated (typically from
#'        \code{\link{load_kssl_pedons_gpkg}}).
#' @param verbose Print progress.
#'
#' @return A list with \code{accuracy}, \code{n_compared},
#'   \code{confusion}, \code{per_class_recall}.
#'
#' @export
benchmark_wrb_vs_usda <- function(pedons, verbose = TRUE) {
  pedons <- annotate_wrb_from_usda(pedons)
  if (isTRUE(verbose))
    cat(sprintf("[wrb-vs-usda] benchmarking %d pedons\n", length(pedons)))

  preds <- vapply(pedons, function(pr) {
    res <- tryCatch(classify_wrb2022(pr, on_missing = "silent"),
                     error = function(e) NULL)
    if (is.null(res)) NA_character_ else sub("s$", "", res$rsg_or_order)
  }, character(1))
  refs <- vapply(pedons, function(p)
                   p$site$reference_wrb_from_usda %||% NA_character_,
                 character(1))

  in_scope <- !is.na(refs) & !is.na(preds)
  n_correct <- sum(in_scope & refs == preds)
  n_total   <- sum(in_scope)
  acc <- if (n_total > 0L) n_correct / n_total else NA_real_

  conf <- table(reference = refs[in_scope], predicted = preds[in_scope])
  per_class <- data.frame(
    reference_rsg = rownames(conf),
    n             = rowSums(conf),
    n_correct     = vapply(rownames(conf),
                            function(r) if (r %in% colnames(conf)) conf[r, r] else 0L,
                            integer(1)),
    stringsAsFactors = FALSE
  )
  per_class$recall <- per_class$n_correct / per_class$n

  if (isTRUE(verbose))
    cat(sprintf("[wrb-vs-usda] accuracy = %.1f%% on n = %d\n",
                 100 * acc, n_compared = n_total))

  list(accuracy = acc, n_compared = n_total, n_total = length(pedons),
       confusion = conf, per_class_recall = per_class,
       refs = refs, preds = preds)
}
