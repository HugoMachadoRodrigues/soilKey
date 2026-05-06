# =============================================================================
# v0.9.60 -- benchmark_bdsolos_sibcs(): Brazilian SiBCS benchmark.
#
# Mirror of v0.9.49 benchmark_lucas_2018() but for BDsolos:
#
#   load_bdsolos_csv(...) -> list of PedonRecord with site$reference_sibcs
#       + site$reference_nivel_1/2/3
#       (the surveyor's SiBCS classification, BDsolos canonical fields)
#       |
#       v
#   benchmark_bdsolos_sibcs(pedons)
#       |
#       v
#   for each pedon:
#     predicted = classify_sibcs(p)$rsg_or_order  (Brazilian Ordem)
#     reference = .normalize_bdsolos_ordem(p$site$reference_nivel_1)
#     compare at 4 levels (Ordem / Subordem / Grande Grupo / Subgrupo)
#       |
#       v
#   confusion matrix + per-Ordem recall + L0..L4 summary
#
# The audit on Hugo's full 27-UF download (May 2026) showed:
#   8,995 perfis, 7,369 (82%) with surveyor's SiBCS reference.
# This benchmark validates the soilKey SiBCS classifier on the real
# Brazilian surveyor-labeled corpus.
# =============================================================================


#' Map BDsolos ALL-CAPS Ordem singular -> soilKey Title-Case plural
#'
#' BDsolos exports SiBCS classes in ALL CAPS singular form
#' (e.g. \code{"LATOSSOLO"}); soilKey returns Title Case plural
#' (e.g. \code{"Latossolos"}). This helper aligns the two.
#'
#' Also handles the legacy / folk Ordem names that appear in older
#' BDsolos surveys (1970s-90s pre-SiBCS-1ª-edição):
#'
#' \itemize{
#'   \item \code{PODZOLICO}, \code{PODZOLCIO}, \code{LATOSOL}
#'         -> \code{Argissolos} (the 1999 SiBCS rename)
#'   \item \code{GLEI} -> \code{Gleissolos}
#'   \item \code{BRUNIZEM} -> \code{Chernossolos}
#'   \item \code{AREIA(S)} -> \code{Neossolos} (Quartzarenicos)
#'   \item \code{ALUVIAL} -> \code{Neossolos} (Fluvicos)
#'   \item \code{BRUNO}, \code{RENDZINA} -> \code{Chernossolos}
#'   \item \code{SOLONCHAK}, \code{SOLONETZ} -> \code{Planossolos}
#'         (Naticos / Solodicos in SiBCS)
#' }
#'
#' Returns \code{NA_character_} when the input is NA or unrecognised.
#'
#' @keywords internal
.bdsolos_normalize_ordem <- function(s) {
  if (is.null(s) || length(s) == 0L || is.na(s) || !nzchar(trimws(s))) {
    return(NA_character_)
  }
  raw <- toupper(trimws(as.character(s)))
  raw <- strsplit(raw, "[ ,;]")[[1L]][1L]
  # Modern SiBCS Ordens (1999+, Embrapa SiBCS 1a -> 5a edicao)
  modern <- c(
    ARGISSOLO    = "Argissolos",
    CAMBISSOLO   = "Cambissolos",
    CHERNOSSOLO  = "Chernossolos",
    ESPODOSSOLO  = "Espodossolos",
    GLEISSOLO    = "Gleissolos",
    LATOSSOLO    = "Latossolos",
    LUVISSOLO    = "Luvissolos",
    NEOSSOLO     = "Neossolos",
    NITOSSOLO    = "Nitossolos",
    ORGANOSSOLO  = "Organossolos",
    PLANOSSOLO   = "Planossolos",
    PLINTOSSOLO  = "Plintossolos",
    VERTISSOLO   = "Vertissolos"
  )
  if (raw %in% names(modern)) return(unname(modern[raw]))
  # Legacy / folk names (pre-1999 surveys)
  legacy <- c(
    "PODZOLICO" = "Argissolos",
    "PODZOLCIO" = "Argissolos",
    "PODZOLICA" = "Argissolos",
    "LATOSOL"   = "Latossolos",
    "GLEI"      = "Gleissolos",
    "GLEISOLO"  = "Gleissolos",
    "PODZOL"    = "Espodossolos",
    "BRUNIZEM"  = "Chernossolos",
    "RENDZINA"  = "Chernossolos",
    "AREIA"     = "Neossolos",
    "AREIAS"    = "Neossolos",
    "ALUVIAL"   = "Neossolos",
    "ATERRO"    = "Neossolos",
    "REGOSOLO"  = "Neossolos",
    "PLINTOSOL" = "Plintossolos",
    "SOLONETZ"  = "Planossolos",
    "SOLONCHAK" = "Planossolos",
    "VERTISOLO" = "Vertissolos",
    "BRUNO"     = "Chernossolos",
    "ORGANOSSOL"= "Organossolos",
    "NEO0SSOLO" = "Neossolos",     # typo seen in BDsolos
    "SOLO"      = NA_character_    # generic; cannot map
  )
  ascii <- chartr(
    intToUtf8(c(0xc1, 0xc0, 0xc2, 0xc3, 0xc4, 0xc9, 0xc8, 0xca, 0xcb,
                 0xcd, 0xcc, 0xce, 0xcf, 0xd3, 0xd2, 0xd4, 0xd5, 0xd6,
                 0xda, 0xd9, 0xdb, 0xdc, 0xc7, 0xd1)),
    "AAAAAEEEEIIIIOOOOOUUUUCN",
    raw
  )
  ascii_short <- gsub("[^A-Z]", "", ascii)
  if (ascii_short %in% names(legacy)) return(unname(legacy[ascii_short]))
  # Unknown
  NA_character_
}


#' Run the BDsolos / SiBCS surveyor-reference benchmark
#'
#' Runs \code{\link{classify_sibcs}} on each pedon and tabulates
#' agreement with the surveyor's SiBCS classification embedded in
#' the BDsolos export (\code{site$reference_nivel_1} when
#' available, fall back to parsing \code{site$reference_sibcs}).
#'
#' Compared to the v0.9.49 \code{\link{benchmark_lucas_2018}}, this
#' uses the SURVEYOR's reference (richer than the WRB-1km raster):
#' the BDsolos pedologist who described the profile assigns the
#' Ordem / Subordem / Grande Grupo / Subgrupo. This is the
#' authoritative Brazilian benchmark.
#'
#' @param pedons List of \code{\link{PedonRecord}} objects, typically
#'        from \code{\link{load_bdsolos_csv}}.
#' @param classify_with Internal: classifier (default
#'        \code{classify_sibcs}). Pass \code{classify_via_smartsolos_api}
#'        to benchmark the Embrapa PROLOG classifier instead.
#' @param classify_args List of additional arguments passed to the
#'        classifier (e.g. \code{list(api_key = ...,
#'        post_fn = ...)} for SmartSolos).
#' @param max_n Optional integer cap on pedons benchmarked.
#' @param verbose If \code{TRUE} (default), prints a summary line.
#' @return A list with elements:
#'   \describe{
#'     \item{\code{predictions}}{data.frame: \code{point_id,
#'           predicted_ordem, reference_ordem, agree_ordem,
#'           predicted_subordem, reference_subordem (when
#'           parseable), reference_raw}.}
#'     \item{\code{confusion}}{Ordem-level confusion table.}
#'     \item{\code{accuracy}}{Overall Ordem-level match fraction.}
#'     \item{\code{per_ordem}}{data.frame: per-Ordem recall.}
#'     \item{\code{summary}}{n_total, n_in_scope, n_matched,
#'           n_errors, n_unmapped (reference Ordem string we
#'           cannot normalise to a SiBCS Ordem).}
#'   }
#'
#' @examples
#' \dontrun{
#' pedons <- load_bdsolos_csv("soil_data/embrapa_bdsolos/BD_solos/RJ.csv")
#' bench <- benchmark_bdsolos_sibcs(pedons)
#' bench$accuracy
#' bench$per_ordem
#' bench$confusion
#' }
#' @seealso \code{\link{load_bdsolos_csv}},
#'          \code{\link{benchmark_lucas_2018}},
#'          \code{\link{classify_sibcs}},
#'          \code{\link{compare_smartsolos}}.
#' @export
benchmark_bdsolos_sibcs <- function(pedons,
                                      classify_with  = classify_sibcs,
                                      classify_args  = list(on_missing = "silent"),
                                      max_n          = NULL,
                                      verbose        = TRUE) {
  if (!is.list(pedons) || length(pedons) == 0L) {
    stop("benchmark_bdsolos_sibcs(): 'pedons' must be a non-empty list of PedonRecord.")
  }
  if (!all(vapply(pedons, inherits, logical(1L), "PedonRecord"))) {
    stop("benchmark_bdsolos_sibcs(): every element of 'pedons' must be a PedonRecord.")
  }
  if (!is.null(max_n) && length(pedons) > max_n) {
    pedons <- pedons[seq_len(as.integer(max_n))]
  }
  if (isTRUE(verbose)) {
    cli::cli_alert_info(sprintf(
      "Running %s on %d pedons...",
      ifelse(identical(classify_with, classify_sibcs), "classify_sibcs",
              deparse(substitute(classify_with))),
      length(pedons)))
  }

  predicted_ordem <- character(length(pedons))
  predicted_subordem <- character(length(pedons))
  predicted_gg       <- character(length(pedons))
  predicted_sg       <- character(length(pedons))
  reference_raw      <- character(length(pedons))
  reference_ordem    <- character(length(pedons))
  reference_subordem <- character(length(pedons))
  reference_gg       <- character(length(pedons))
  errors <- list()

  for (i in seq_along(pedons)) {
    p <- pedons[[i]]
    res <- tryCatch(
      do.call(classify_with, c(list(p), classify_args)),
      error = function(e) {
        errors[[length(errors) + 1L]] <<- list(
          i = i, id = p$site$id %||% i,
          error = conditionMessage(e)
        )
        NULL
      }
    )
    predicted_ordem[i]    <- if (is.null(res)) NA_character_ else
                                as.character(res$rsg_or_order %||% NA_character_)
    if (!is.null(res) && !is.null(res$trace)) {
      predicted_subordem[i] <- as.character(res$trace$subordem_assigned$name        %||% NA_character_)
      predicted_gg[i]       <- as.character(res$trace$grande_grupo_assigned$name    %||% NA_character_)
      predicted_sg[i]       <- as.character(res$trace$subgrupo_assigned$name        %||% NA_character_)
    }
    # Reference: prefer site$reference_nivel_1 (already-parsed Ordem),
    # fall back to first word of site$reference_sibcs.
    ref_n1 <- p$site$reference_nivel_1 %||% NA_character_
    ref_n2 <- p$site$reference_nivel_2 %||% NA_character_
    ref_n3 <- p$site$reference_nivel_3 %||% NA_character_
    ref_full <- p$site$reference_sibcs %||% NA_character_
    reference_raw[i]      <- ref_full
    reference_ordem[i]    <- .bdsolos_normalize_ordem(ref_n1 %||% ref_full)
    reference_subordem[i] <- if (!is.na(ref_n2 %||% NA_character_)) ref_n2 else NA_character_
    reference_gg[i]       <- if (!is.na(ref_n3 %||% NA_character_)) ref_n3 else NA_character_
  }

  ids <- vapply(pedons, function(p) as.character(p$site$id %||% NA_character_),
                  character(1L))
  agree_ordem <- !is.na(predicted_ordem) & !is.na(reference_ordem) &
                  predicted_ordem == reference_ordem
  comparison <- data.frame(
    point_id           = ids,
    predicted_ordem    = predicted_ordem,
    reference_ordem    = reference_ordem,
    agree_ordem        = agree_ordem,
    predicted_subordem = predicted_subordem,
    reference_subordem = reference_subordem,
    predicted_gg       = predicted_gg,
    reference_gg       = reference_gg,
    reference_raw      = reference_raw,
    stringsAsFactors   = FALSE
  )

  in_scope <- !is.na(comparison$predicted_ordem) & !is.na(comparison$reference_ordem)
  n_in_scope <- sum(in_scope)
  n_matched  <- sum(comparison$agree_ordem)
  accuracy <- if (n_in_scope > 0L) n_matched / n_in_scope else NA_real_

  conf <- if (n_in_scope > 0L) {
    table(
      Predicted = comparison$predicted_ordem[in_scope],
      Reference = comparison$reference_ordem[in_scope]
    )
  } else NULL

  per_ordem <- if (n_in_scope > 0L) {
    sub_in <- comparison[in_scope, ]
    refs <- sort(unique(sub_in$reference_ordem))
    do.call(rbind, lapply(refs, function(o) {
      sub <- sub_in[sub_in$reference_ordem == o, ]
      data.frame(
        reference_ordem = o,
        n               = nrow(sub),
        n_correct       = sum(sub$agree_ordem),
        recall          = mean(sub$agree_ordem),
        stringsAsFactors = FALSE
      )
    }))
  } else NULL

  n_unmapped <- sum(is.na(reference_ordem) & !is.na(reference_raw) &
                       nzchar(trimws(reference_raw)))

  if (isTRUE(verbose)) {
    cli::cli_alert_success(sprintf(
      "benchmark_bdsolos_sibcs(): Ordem accuracy = %.1f%% over %d in-scope pedons (%d matched / %d total / %d errors / %d unmapped).",
      100 * (accuracy %||% NA_real_),
      n_in_scope, n_matched, length(pedons), length(errors), n_unmapped
    ))
  }

  list(
    predictions = comparison,
    confusion   = conf,
    accuracy    = accuracy,
    per_ordem   = per_ordem,
    summary = list(
      n_total     = length(pedons),
      n_in_scope  = n_in_scope,
      n_matched   = n_matched,
      n_errors    = length(errors),
      n_unmapped  = n_unmapped
    ),
    errors = errors
  )
}
