# =============================================================================
# soilKey -- Report rendering (PDF)
#
# Companion to `report-html.R`. The PDF path goes through a temporary
# Rmd file rendered by `rmarkdown::render(... output_format =
# "pdf_document")`, which in turn requires a working LaTeX install
# (or an alternative engine accepted by rmarkdown, such as `tinytex`).
#
# We chose this approach over a "render HTML and shell to wkhtmltopdf"
# pipeline because:
#
#   - rmarkdown is already in Suggests (used by the vignettes), so
#     adding the PDF path costs nothing in dependencies.
#   - LaTeX-rendered PDFs handle long key traces and tables better
#     than HTML-to-PDF at typical paper sizes.
#   - Pedologists who archive laudos in PDF generally already have a
#     TeX install for tables/figures.
#
# When neither rmarkdown nor LaTeX is available, the function
# degrades gracefully with an actionable error pointing to
# `tinytex::install_tinytex()`.
# =============================================================================


#' Render a soilKey classification report as PDF
#'
#' See \code{\link{report}} for the generic dispatcher. This function
#' assembles a temporary `.Rmd` file with the same content as
#' \code{\link{report_html}} (site, cross-system summary, classification
#' cards, horizons, provenance) and renders it via
#' \code{rmarkdown::render()}.
#'
#' @param x      A \code{ClassificationResult}, list of results, or
#'               \code{PedonRecord}.
#' @param file   Output \code{.pdf} path.
#' @param pedon  Optional \code{PedonRecord}.
#' @param title  Report title.
#' @param ...    Passed to \code{rmarkdown::render()}.
#' @return       The output path, invisibly.
#' @export
report_pdf <- function(x,
                       file,
                       pedon = NULL,
                       title = NULL,
                       ...) {
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Package 'rmarkdown' is required for PDF reports.\n",
         "  install.packages('rmarkdown')")

  norm <- .normalise_results(x, pedon = pedon)
  results <- norm$results
  pedon   <- norm$pedon

  if (is.null(title)) {
    pedon_id <- if (!is.null(pedon) && !is.null(pedon$site$id))
                  pedon$site$id else "soilKey report"
    title <- paste0("soilKey -- ", pedon_id)
  }

  rmd <- .build_report_rmd(results, pedon = pedon, title = title)
  tmp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(rmd, tmp_rmd, useBytes = TRUE)
  on.exit(unlink(tmp_rmd), add = TRUE)

  out_dir <- dirname(normalizePath(file, mustWork = FALSE))
  out_fn  <- basename(file)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  out <- tryCatch(
    rmarkdown::render(
      tmp_rmd,
      output_format = rmarkdown::pdf_document(toc       = FALSE,
                                               number_sections = FALSE,
                                               latex_engine    = "xelatex"),
      output_file   = out_fn,
      output_dir    = out_dir,
      quiet         = TRUE,
      ...
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("(?i)pdflatex|xelatex|LaTeX|tinytex", msg)) {
        stop("PDF rendering failed -- a LaTeX engine appears to be missing.\n",
             "  Install one with `tinytex::install_tinytex()` or by",
             " installing TeX Live / MacTeX system-wide.\n",
             "  Underlying error: ", msg, call. = FALSE)
      }
      stop(e)
    }
  )

  invisible(out)
}


# ---- internal: build an Rmd from the assembled data --------------------------
#' Internal helper: .escape_latex


#' @keywords internal
.escape_latex <- function(x) {
  if (is.null(x)) return("")
  x <- as.character(x)
  x[is.na(x)] <- ""
  # Replace order matters: backslash first, then the rest.
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~",  "\\\\textasciitilde{}",  x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}
#' Internal helper: .rmd_header

#' @keywords internal
.rmd_header <- function(title) {
  paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "date: \"", format(Sys.time(), "%Y-%m-%d"), "\"\n",
    "output:\n",
    "  pdf_document:\n",
    "    toc: false\n",
    "    number_sections: false\n",
    "  html_document:\n",
    "    toc: false\n",
    "    self_contained: true\n",
    "geometry: margin=2cm\n",
    "fontsize: 10pt\n",
    "header-includes:\n",
    "  - \\usepackage{longtable}\n",
    "  - \\usepackage{booktabs}\n",
    "  - \\usepackage{xcolor}\n",
    "---\n\n",
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)\n",
    "```\n\n"
  )
}
#' Internal helper: .rmd_classification_block

#' @keywords internal
.rmd_classification_block <- function(res) {
  qual_principal <- res$qualifiers$principal     %||% character()
  qual_suppl     <- res$qualifiers$supplementary %||% character()

  trace_lines <- if (length(res$trace) == 0) {
    "_(no trace)_"
  } else {
    paste(vapply(seq_along(res$trace), function(i) {
      t <- res$trace[[i]]
      sym <- if (isTRUE(t$passed))      "**PASSED**"
             else if (isFALSE(t$passed)) "failed"
             else                        "indeterminate"
      sprintf("%2d. `%-3s` %-20s -- %s%s",
                i,
                t$code %||% "?",
                t$name %||% "",
                sym,
                if (!isTRUE(t$passed) &&
                       length(t$missing %||% character()) > 0)
                  sprintf(" (%d attrs missing)",
                            length(t$missing))
                else "")
    }, character(1)), collapse = "  \n")
  }

  paste0(
    "## ", res$system %||% "?", "\n\n",
    "**", res$name %||% "(unnamed)", "**\n\n",
    "* RSG/Order: `", res$rsg_or_order %||% "?", "`\n",
    "* Evidence grade: **", res$evidence_grade %||% "NA", "**\n",
    if (length(qual_principal) > 0)
      paste0("* Principal qualifiers: ",
               paste(qual_principal, collapse = ", "), "\n"),
    if (length(qual_suppl) > 0)
      paste0("* Supplementary qualifiers: ",
               paste(qual_suppl, collapse = ", "), "\n"),
    "\n### Key trace\n\n",
    trace_lines,
    "\n\n",
    if (length(res$ambiguities) > 0)
      paste0("### Ambiguities\n\n",
               paste(vapply(res$ambiguities, function(a)
                 sprintf("- **%s**: %s",
                           a$rsg_code %||% "?",
                           a$reason   %||% ""),
                 character(1)), collapse = "\n"),
               "\n\n")
      else "",
    if (length(res$missing_data %||% character()) > 0)
      paste0("### Missing data that would refine the result\n\n",
               paste(res$missing_data, collapse = ", "),
               "\n\n")
      else "",
    if (length(res$warnings %||% character()) > 0)
      paste0("### Warnings\n\n",
               paste(vapply(res$warnings, function(w)
                 sprintf("- %s", w),
                 character(1)), collapse = "\n"),
               "\n\n")
      else ""
  )
}
#' Internal helper: .rmd_summary_block

#' @keywords internal
.rmd_summary_block <- function(results) {
  if (length(results) < 2) return("")
  rows <- vapply(results, function(r)
    sprintf("| %s | %s | %s |",
              r$system          %||% "?",
              r$name            %||% "(unnamed)",
              r$evidence_grade  %||% "NA"),
    character(1))
  paste0(
    "## Cross-system summary\n\n",
    "| System | Name | Grade |\n",
    "|---|---|---|\n",
    paste(rows, collapse = "\n"),
    "\n\n"
  )
}
#' Internal helper: .rmd_horizons_block

#' @keywords internal
#' @param pedon A \code{\link{PedonRecord}}.
.rmd_horizons_block <- function(pedon) {
  if (is.null(pedon) || is.null(pedon$horizons) ||
        nrow(pedon$horizons) == 0) {
    return("")
  }
  cols <- intersect(
    c("top_cm", "bottom_cm", "designation",
      "munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist",
      "clay_pct", "silt_pct", "sand_pct",
      "ph_h2o", "oc_pct", "cec_cmol", "bs_pct"),
    names(pedon$horizons)
  )
  if (length(cols) == 0) return("")
  h <- as.data.frame(pedon$horizons)[, cols, drop = FALSE]

  paste0(
    "## Horizons\n\n",
    "```{r horizons}\n",
    "knitr::kable(",
    paste(deparse(h), collapse = ""),
    ", booktabs = TRUE, format.args = list(big.mark = ''))\n",
    "```\n\n"
  )
}
#' Internal helper: .rmd_site_block

#' @keywords internal
#' @param pedon A \code{\link{PedonRecord}}.
.rmd_site_block <- function(pedon) {
  if (is.null(pedon) || is.null(pedon$site)) return("")
  s <- pedon$site
  bits <- list()
  if (!is.null(s$id))              bits[["ID"]]              <- s$id
  if (!is.null(s$lat) && !is.null(s$lon))
    bits[["Coords"]] <- sprintf("%.4f, %.4f", s$lat, s$lon)
  if (!is.null(s$country))         bits[["Country"]]         <- s$country
  if (!is.null(s$parent_material)) bits[["Parent material"]] <- s$parent_material
  if (!is.null(s$elevation_m))     bits[["Elevation (m)"]]   <- s$elevation_m
  if (!is.null(s$slope_pct))       bits[["Slope (%)"]]       <- s$slope_pct
  if (!is.null(s$date))            bits[["Date"]]            <- s$date
  if (length(bits) == 0) return("")
  rows <- vapply(seq_along(bits), function(i)
    sprintf("| %s | %s |", names(bits)[i], bits[[i]]),
    character(1))
  paste0(
    "## Site\n\n",
    "| Field | Value |\n",
    "|---|---|\n",
    paste(rows, collapse = "\n"),
    "\n\n"
  )
}
#' Internal helper: .build_report_rmd

#' @keywords internal
.build_report_rmd <- function(results, pedon, title) {
  paste0(
    .rmd_header(title),
    sprintf("_Generated %s by soilKey v%s. The taxonomic key was executed deterministically from versioned YAML rules; no language model was used in the classification step._\n\n",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
              .soilkey_version()),
    .rmd_site_block(pedon),
    .rmd_summary_block(results),
    "# Classification results\n\n",
    paste(vapply(results, .rmd_classification_block, character(1)),
            collapse = "\n"),
    .rmd_horizons_block(pedon)
  )
}
