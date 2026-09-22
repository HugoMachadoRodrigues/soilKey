# =============================================================================
# v0.9.83 -- argic strong-films audit helpers.
#
# The SiBCS Cap 18 latossolic-vs-argic precedence rule, wired into
# B_latossolico() since v0.9.61, says:
#
#   * Latossolic features dominate when ferralic + CTC argila <= 17 +
#     thickness >= 50 AND argic-passing layers have only "ausente",
#     "pouca", or "fraca" clay films.
#   * Strong clay films ("comum"/"abundante") in argic-passing layers
#     reroute the profile to Argissolo regardless of latossolic features.
#
# v0.9.83 extracts the strong-films decision into a standalone helper so
# that (a) the rule can be audited on any benchmark dataset and (b)
# B_latossolico() can call the same logic without duplicating the
# Portuguese accent-stripping / token-matching code.
#
# The audit on BDsolos RJ (n = 722, 115 Latossolo references) shows
# 0 / 115 Latossolos are excluded by the strong-films rule -- the
# 17 / 115 misclassified-as-Argissolos pedons are routed by other
# constraints (ferralic CTC > 17, texture failure, etc.) NOT by the
# strong-films exclusion.
# =============================================================================


#' Detect strong clay-film qualifier strings (Portuguese / English)
#'
#' Internal helper used by \code{argic_with_strong_clay_films()} and
#' the v0.9.61 \code{B_latossolico()} latossolic-vs-argic precedence
#' rule. Strips Portuguese accents and matches the standard SiBCS Cap
#' 18 "strong" terminology: \emph{comum}, \emph{abundante},
#' \emph{common}, \emph{abundant}.
#'
#' "Pouca", "fraca", "few", "weak" do NOT count as strong (they are
#' the weak end of the SiBCS clay-film scale that allows latossolic
#' features to dominate).
#'
#' @param films_chr Character vector of \code{clay_films_amount}
#'        values (Portuguese or English).
#' @return Logical scalar: \code{TRUE} if any element matches a strong
#'         qualifier; \code{FALSE} for empty input or weak-only
#'         qualifiers.
#' @noRd
.argic_strong_films_match <- function(films_chr) {
  if (length(films_chr) == 0L) return(FALSE)
  films_chr <- films_chr[!is.na(films_chr) & nzchar(films_chr)]
  if (length(films_chr) == 0L) return(FALSE)
  norm <- tolower(trimws(films_chr))
  norm <- gsub("[\u00C1\u00C0\u00C2\u00C3\u00E1\u00E0\u00E2\u00E3]",
                "a", norm)
  any(grepl("\\babunda|\\bcomu|\\bcommon|\\babundan", norm))
}


#' Test whether a pedon's argic horizon has strong clay films
#'
#' Wraps \code{\link{argic}()} and inspects the
#' \code{clay_films_amount} field at the argic-passing layers. Returns
#' a structured result that \code{\link{B_latossolico}()} uses to
#' decide whether the SiBCS Cap 18 strong-films exclusion fires.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A list with:
#' \itemize{
#'   \item \code{passed} -- logical, \code{TRUE} only when argic passes
#'         AND at least one argic-passing layer has a strong
#'         (\emph{comum} / \emph{abundante}) film qualifier.
#'   \item \code{layers} -- integer vector of argic-passing layer
#'         indices (empty when \code{passed} is \code{FALSE}).
#'   \item \code{argic} -- the underlying \code{\link{DiagnosticResult}}
#'         from \code{\link{argic}()}.
#'   \item \code{films} -- character vector of the
#'         \code{clay_films_amount} values at the argic-passing layers.
#' }
#' @export
argic_with_strong_clay_films <- function(pedon) {
  bt <- argic(pedon)
  if (!isTRUE(bt$passed)) {
    return(list(passed = FALSE, layers = integer(0),
                  argic  = bt,    films  = character(0)))
  }
  cf <- pedon$horizons$clay_films_amount[bt$layers]
  cf <- cf[!is.na(cf) & nzchar(cf)]
  if (length(cf) == 0L) {
    return(list(passed = FALSE, layers = integer(0),
                  argic  = bt,    films  = character(0)))
  }
  has_strong <- .argic_strong_films_match(cf)
  list(passed = has_strong,
       layers = if (has_strong) bt$layers else integer(0),
       argic  = bt,
       films  = cf)
}


#' Audit the strong-clay-films exclusion across a list of pedons
#'
#' Applies \code{\link{argic_with_strong_clay_films}()} to every
#' pedon in \code{pedons} and returns a per-pedon table summarising
#' how the v0.9.61 \code{B_latossolico()} latossolic-vs-argic rule
#' resolves on the benchmark sample.
#'
#' Useful for empirical validation of the SiBCS Cap 18 precedence
#' rule on field-described datasets such as BDsolos and Redape, where
#' clay-film qualifiers are recorded in mixed Portuguese / English
#' tokenisation. The audit is read-only and never invokes
#' \code{\link{classify_sibcs}()}.
#'
#' @param pedons List of \code{\link{PedonRecord}} objects.
#' @param reference_filter Optional regex applied to
#'        \code{p$site$reference_sibcs} to keep only pedons whose
#'        reference matches (case-sensitive, ICU). Default \code{NULL}
#'        keeps every pedon.
#' @return A \code{data.frame} with columns
#'        \code{id}, \code{reference_sibcs},
#'        \code{argic_passed},
#'        \code{has_films_at_argic},
#'        \code{strong_films_at_argic},
#'        and \code{would_exclude_from_latossolo}.
#' @examples
#' \dontrun{
#' peds <- load_bdsolos_csv("RJ.csv")
#' a <- audit_argic_strong_films(peds, reference_filter = "LATOSSOLO")
#' table(a$would_exclude_from_latossolo)
#' }
#' @export
audit_argic_strong_films <- function(pedons, reference_filter = NULL) {
  if (!is.list(pedons) || length(pedons) == 0L) {
    stop("audit_argic_strong_films(): `pedons` must be a non-empty list ",
         "of PedonRecord objects.", call. = FALSE)
  }
  refs <- vapply(pedons,
                  function(p) as.character(p$site$reference_sibcs %||%
                                              NA_character_)[1L],
                  character(1L))
  keep <- if (is.null(reference_filter)) {
    rep(TRUE, length(pedons))
  } else {
    !is.na(refs) & grepl(reference_filter, refs)
  }
  rows <- vector("list", sum(keep))
  k <- 0L
  for (i in which(keep)) {
    pr <- pedons[[i]]
    res <- tryCatch(argic_with_strong_clay_films(pr),
                     error = function(e) list(passed = NA, layers = integer(0),
                                                argic  = NULL, films = character(0)))
    has_films <- length(res$films) > 0L
    k <- k + 1L
    rows[[k]] <- data.frame(
      id                            = pr$site$id %||% NA_character_,
      reference_sibcs               = refs[i],
      argic_passed                  = isTRUE(res$argic$passed %||% FALSE),
      has_films_at_argic            = has_films,
      strong_films_at_argic         = isTRUE(res$passed),
      would_exclude_from_latossolo   = isTRUE(res$passed),
      stringsAsFactors              = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(data.frame(id = character(0),
                       reference_sibcs = character(0),
                       argic_passed = logical(0),
                       has_films_at_argic = logical(0),
                       strong_films_at_argic = logical(0),
                       would_exclude_from_latossolo = logical(0),
                       stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}
