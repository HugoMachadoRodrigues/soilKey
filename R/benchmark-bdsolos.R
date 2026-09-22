# =============================================================================
# v0.9.60 -- benchmark_bdsolos(): triple-system validation against the
# Embrapa BDsolos national reference set.
#
# BDsolos exports (loaded via load_bdsolos_csv() v0.9.55--v0.9.60) carry
# THREE per-pedon reference labels at the site level:
#   * reference_sibcs  -- "Classificacao Atual" (dense; ~82% nation-wide)
#   * reference_wrb    -- "Classificacao FAO/WRB" (sparse; varies by UF)
#   * reference_st     -- "Classificacao Soil Taxonomy" (sparse)
#
# This wrapper runs classify_all() on each pedon, compares each predicted
# system against its corresponding BDsolos reference using the FEBR
# normalisers (normalise_febr_sibcs / normalise_febr_wrb /
# normalise_febr_usda) -- BDsolos and FEBR share the all-caps Portuguese
# SiBCS convention, the period-terminated singular WRB convention, and
# the suffix-encoded USDA Subgroup convention -- and reports per-system
# accuracy + confusion matrices + label coverage in one call.
#
# Returns NA accuracy (with a "no_reference_labels" message) for systems
# where the BDsolos export has no ground-truth, so the same call is safe
# to run on UFs whose surveyors only filled in SiBCS.
# =============================================================================


#' Benchmark soilKey classifiers against BDsolos national reference labels
#'
#' Runs \code{\link{classify_wrb2022}}, \code{\link{classify_sibcs}}, and
#' \code{\link{classify_usda}} on each \code{\link{PedonRecord}} loaded
#' from a BDsolos CSV via \code{\link{load_bdsolos_csv}}, then compares
#' each predicted classification against the corresponding BDsolos
#' reference label (\code{reference_sibcs}, \code{reference_wrb},
#' \code{reference_st}) and reports per-system accuracy, per-class
#' recall, and a confusion matrix.
#'
#' @section Reference label coverage:
#'
#' BDsolos densely populates \code{reference_sibcs} (~82% nation-wide as
#' of the v0.9.59 audit) but sparsely populates \code{reference_wrb} and
#' \code{reference_st} (UF-dependent; ~5% on RJ, higher in some other
#' states). The function always reports the per-system label coverage
#' (\code{$coverage}) so the caller can judge how representative each
#' accuracy figure is.
#'
#' @section Comparison level:
#'
#' SiBCS comparison is at \code{level = "order"} by default, which
#' converts the BDsolos all-caps Portuguese label (e.g.
#' \code{"ARGISSOLO VERMELHO Tb EUTROFICO ..."}) to the soilKey plural
#' Title Case form (\code{"Argissolos"}) via
#' \code{\link{normalise_febr_sibcs}}. Set \code{sibcs_level =
#' "subordem"} to compare the first two SiBCS tokens (Ordem + Subordem).
#'
#' WRB and USDA comparisons are at the Reference Soil Group / Order
#' level: \code{normalise_febr_wrb()} strips qualifier parens and
#' pluralises the bare RSG (\code{"Xanthic Ferralsol"} ->
#' \code{"Ferralsols"}); \code{normalise_febr_usda()} maps the suffix of
#' the last subgroup token to the USDA Order (\code{"Typic
#' Haplorthox"} -> \code{"Oxisols"}).
#'
#' @section Errors and missing-label handling:
#'
#' Pedons without a reference label for a given system are silently
#' excluded from THAT system's comparison (but still classified for the
#' other two systems). If a system has zero pedons with a reference
#' label, the corresponding \code{$per_system} entry has
#' \code{accuracy = NA_real_} and \code{message = "no_reference_labels"}.
#' Classifier errors are caught per-pedon and recorded in
#' \code{n_errors}; they do not abort the run.
#'
#' @param pedons A list of \code{\link{PedonRecord}} objects, typically
#'        produced by \code{\link{load_bdsolos_csv}}.
#' @param systems Character vector. Any subset of \code{c("wrb2022",
#'        "sibcs", "usda")}. Default runs all three.
#' @param sibcs_level One of \code{"order"} (default) or
#'        \code{"subordem"}. Forwarded to
#'        \code{\link{normalise_febr_sibcs}}.
#' @param max_n Optional integer; cap classification at the first
#'        \code{max_n} pedons. \code{NULL} (default) classifies every
#'        pedon.
#' @param verbose If \code{TRUE} (default), emits cli progress messages.
#' @return A list with elements:
#'   \itemize{
#'     \item \code{per_system} -- named list (one entry per requested
#'           system) of \code{list(accuracy, n_compared, n_correct,
#'           n_errors, confusion, per_class)} (or
#'           \code{list(accuracy = NA_real_, message)} when no
#'           reference labels were present).
#'     \item \code{coverage} -- named list of
#'           \code{list(n_with_ref, n_total, pct)} per system.
#'     \item \code{config} -- named list capturing
#'           \code{n_pedons, systems, sibcs_level, soilKey_version,
#'           timestamp}.
#'   }
#' @seealso \code{\link{load_bdsolos_csv}},
#'   \code{\link{benchmark_lucas_2018}}, \code{\link{classify_all}},
#'   \code{\link{normalise_febr_sibcs}},
#'   \code{\link{normalise_febr_wrb}},
#'   \code{\link{normalise_febr_usda}}.
#' @examples
#' \dontrun{
#' # Single UF -- typical SiBCS-dense slice
#' peds <- load_bdsolos_csv("RJ.csv")
#' bench <- benchmark_bdsolos(peds, systems = c("sibcs", "wrb2022", "usda"))
#' bench$coverage      # how many pedons had each reference label
#' bench$per_system$sibcs$accuracy
#' bench$per_system$sibcs$confusion
#'
#' # Subordem level
#' bench2 <- benchmark_bdsolos(peds, systems = "sibcs",
#'                                sibcs_level = "subordem")
#' }
#' @export
benchmark_bdsolos <- function(pedons,
                                 systems     = c("wrb2022", "sibcs", "usda"),
                                 sibcs_level = c("order", "subordem"),
                                 max_n       = NULL,
                                 verbose     = TRUE) {
  if (!is.list(pedons) || length(pedons) == 0L) {
    stop("benchmark_bdsolos(): `pedons` must be a non-empty list of ",
         "PedonRecord objects.", call. = FALSE)
  }
  systems     <- match.arg(systems, several.ok = TRUE)
  sibcs_level <- match.arg(sibcs_level)

  if (!is.null(max_n) && length(pedons) > max_n) {
    pedons <- pedons[seq_len(max_n)]
  }

  ref_field <- c(wrb2022 = "reference_wrb",
                  sibcs   = "reference_sibcs",
                  usda    = "reference_st")
  norm_fn <- list(
    wrb2022 = function(x) normalise_febr_wrb(x),
    sibcs   = function(x) normalise_febr_sibcs(x, level = sibcs_level),
    usda    = function(x) normalise_febr_usda(x)
  )

  pred_field <- function(sys, cls) {
    if (is.null(cls)) return(NA_character_)
    # WRB/USDA: rsg_or_order is plural Title Case ("Ferralsols",
    # "Oxisols"). SiBCS: rsg_or_order is plural Title Case Order
    # ("Argissolos") at level = "order"; for level = "subordem", the
    # full $name carries Order + Subordem ("Argissolos Vermelhos") and
    # we trim back to the first two tokens to align with the
    # normalised reference.
    if (sys != "sibcs" || sibcs_level == "order")
      return(cls$rsg_or_order %||% NA_character_)
    nm <- cls$name %||% NA_character_
    if (is.na(nm)) return(NA_character_)
    toks <- strsplit(trimws(nm), "\\s+")[[1L]]
    if (length(toks) < 2L) return(toks[1L])
    paste(toks[1:2], collapse = " ")
  }

  out_per_system <- list()
  out_coverage   <- list()
  n_errors_total <- 0L

  for (sys in systems) {
    rf <- ref_field[[sys]]
    refs_raw <- vapply(pedons, function(p) {
      v <- p$site[[rf]] %||% NA_character_
      if (length(v) != 1L) NA_character_ else as.character(v)
    }, character(1L))
    has_ref <- !is.na(refs_raw) & nzchar(trimws(refs_raw))
    out_coverage[[sys]] <- list(
      n_with_ref = sum(has_ref),
      n_total    = length(pedons),
      pct        = if (length(pedons) > 0L)
                       round(100 * sum(has_ref) / length(pedons), 1)
                   else NA_real_
    )
    if (sum(has_ref) == 0L) {
      out_per_system[[sys]] <- list(
        accuracy   = NA_real_,
        n_compared = 0L,
        n_correct  = 0L,
        n_errors   = 0L,
        confusion  = NULL,
        per_class  = NULL,
        message    = "no_reference_labels"
      )
      next
    }
    if (isTRUE(verbose))
      cli::cli_alert_info(sprintf(
        "benchmark_bdsolos[%s]: %d / %d pedons carry a reference label",
        sys, sum(has_ref), length(pedons)))

    classify <- switch(sys,
                         wrb2022 = function(p) classify_wrb2022(p, on_missing = "silent"),
                         sibcs   = function(p) classify_sibcs(p,   on_missing = "silent"),
                         usda    = function(p) classify_usda(p,    on_missing = "silent"))

    n_compared <- 0L; n_correct <- 0L; n_errors <- 0L
    rows <- vector("list", sum(has_ref))
    idx_pedon <- which(has_ref)
    for (j in seq_along(idx_pedon)) {
      p   <- pedons[[idx_pedon[j]]]
      ref <- norm_fn[[sys]](refs_raw[idx_pedon[j]])
      cls <- tryCatch(classify(p),
                       error = function(e) { n_errors <<- n_errors + 1L; NULL })
      pred_raw <- pred_field(sys, cls)
      pred <- if (is.na(pred_raw)) NA_character_ else pred_raw
      rows[[j]] <- list(id   = p$site$id,
                          ref  = ref,
                          pred = pred)
      if (!is.na(pred) && !is.na(ref)) {
        n_compared <- n_compared + 1L
        if (identical(pred, ref)) n_correct <- n_correct + 1L
      }
    }
    n_errors_total <- n_errors_total + n_errors
    pp <- do.call(rbind, lapply(rows, as.data.frame))
    # NA-safe: replace NA in ref / pred with the literal string
    # "<unmapped>" so table() and downstream data.frame() do not
    # choke on missing factor levels (some BDsolos USDA labels do
    # not match normalise_febr_usda's suffix table -- e.g. "Soil
    # Taxonomy 13ed" or non-Soil-Taxonomy notes).
    pp$ref [is.na(pp$ref)]  <- "<unmapped>"
    pp$pred[is.na(pp$pred)] <- "<unmapped>"
    cm <- if (nrow(pp) > 0L) table(reference = pp$ref, predicted = pp$pred)
           else NULL
    per_class <- if (!is.null(cm)) {
      ref_totals <- rowSums(cm)
      data.frame(
        reference = rownames(cm),
        n_ref     = as.integer(ref_totals),
        n_correct = vapply(rownames(cm), function(r)
                            if (r %in% colnames(cm)) as.integer(cm[r, r]) else 0L,
                            integer(1L)),
        recall    = vapply(rownames(cm), function(r)
                            if (r %in% colnames(cm) && ref_totals[r] > 0)
                              cm[r, r] / ref_totals[r] else 0,
                            numeric(1L)),
        stringsAsFactors = FALSE
      )
    } else NULL

    out_per_system[[sys]] <- list(
      accuracy   = if (n_compared > 0L) n_correct / n_compared else NA_real_,
      n_compared = n_compared,
      n_correct  = n_correct,
      n_errors   = n_errors,
      confusion  = cm,
      per_class  = per_class,
      message    = NA_character_
    )
    if (isTRUE(verbose))
      cli::cli_alert_success(sprintf(
        "benchmark_bdsolos[%s]: accuracy = %.1f%% (%d / %d), errors = %d",
        sys,
        100 * (out_per_system[[sys]]$accuracy %||% NA_real_),
        n_correct, n_compared, n_errors))
  }

  list(
    per_system = out_per_system,
    coverage   = out_coverage,
    config     = list(
      n_pedons        = length(pedons),
      systems         = systems,
      sibcs_level     = sibcs_level,
      n_errors_total  = n_errors_total,
      soilKey_version = as.character(utils::packageVersion("soilKey")),
      timestamp       = Sys.time()
    )
  )
}
