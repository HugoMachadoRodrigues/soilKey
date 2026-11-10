# =============================================================================
# soilKey -- reading a South African soil label (v0.9.393).
#
# South African profiles carry their local classification as words: the
# binomial of 1977 ("<Form> <Series>", e.g. "Hutton Farningham", sometimes
# with a comma after the form), the form and family of 1991 ("<Form> Form,
# <Family> Family", e.g. "Hutton Form, Lillieburn Family") or a form named
# alone ("Sterkspruit"). .za_match_label() reads such a label and returns the
# code of its form in za1991 and, for a label of 1991, of its family; it
# serves the offline AfSP benchmark (inst/benchmarks/za1991_afsp.R) and never
# classifies.
#
# How a label is read (Hugo's decisions of 2026-11-10):
#   1. the shape gives the era of the label, not the survey year:
#      "<Form> Form, <Family> Family" is "1991"; "<Form>[,] <Series ...>" is
#      "1977" (a binomial, scored at the form level as its 1991 namesake);
#      "<Form>[ Form][,]" alone is "form_only"; any other shape (a rest
#      after the form that starts with "Form" but is not "<Family> Family")
#      is not read;
#   2. the form word is matched exactly (case ignored) to a form name of
#      inst/rules/za1991/key.yaml; the series of a binomial is reported, never
#      scored;
#   3. the family of a 1991 label is matched exactly (case ignored) to a
#      family name of its form in inst/rules/za1991/families.yaml; a family
#      name that is not one of the form's is reported in family_unread;
#   4. exact names only: no other spelling is accepted unless the caller's
#      `variants` (regular expression = replacement, case ignored) rewrite
#      the label first, only when its form is not read as it stands; such a
#      match is flagged "variant".
# =============================================================================

.za_bench_cache <- new.env(parent = emptyenv())

# The forms and families of za1991 as tables, read once from the rules.
.za_label_tables <- function() {
  if (!is.null(.za_bench_cache$tab)) return(.za_bench_cache$tab)
  r <- load_rules("za1991")
  forms <- data.frame(code = vapply(r$forms, function(f) as.character(f$code), ""),
                      name = vapply(r$forms, function(f) as.character(f$name), ""),
                      stringsAsFactors = FALSE)
  fam <- r$families
  families <- do.call(rbind, lapply(names(fam), function(fc) data.frame(
    form = fc, code = vapply(fam[[fc]], function(e) as.character(e$code), ""),
    name = vapply(fam[[fc]], function(e) as.character(e$name), ""), stringsAsFactors = FALSE)))
  tab <- list(forms = forms, families = families)
  assign("tab", tab, envir = .za_bench_cache)
  tab
}

# The names of form codes ("Hu" -> "Hutton", "[Ia]" -> "Inanda"); NA where
# the code is no form of za1991.
.za_form_name <- function(code) {
  f <- .za_label_tables()$forms
  f$name[match(as.character(code), f$code)]
}

#' Read a South African soil label (1977 binomial or 1991 form and family)
#'
#' @param x Character vector of labels ("Hutton Farningham", "Hutton Form,
#'   Lillieburn Family", "Sterkspruit").
#' @param variants Optional named character vector: regular expressions
#'   (case ignored) and their replacements, applied only to a label whose
#'   form is not read as it stands.
#' @return A data frame, one row per label: \code{label}, \code{era}
#'   ("1977", "1991", "form_only" or NA when the shape is none of these),
#'   \code{form_code}, \code{form_name}, \code{series} (the words after the
#'   form of a binomial), \code{family_code}, \code{family_name},
#'   \code{family_unread} (a family named by a 1991 label that is no family
#'   of its form) and \code{matched_by} ("name", "variant" or NA).
#' @keywords internal
#' @noRd
.za_match_label <- function(x, variants = NULL) {
  x <- as.character(x)
  tab <- .za_label_tables()
  shape <- function(s) {
    s <- trimws(gsub("\\s+", " ", s))
    m <- regmatches(s, regexec("(?i)^(\\S+?) Form, (.+) Family$", s, perl = TRUE))[[1]]
    if (length(m) == 3L) return(list(era = "1991", form = m[2], rest = trimws(m[3])))
    m <- regmatches(s, regexec("(?i)^([A-Za-z]+)(?: Form)?,?$", s, perl = TRUE))[[1]]
    if (length(m) == 2L) return(list(era = "form_only", form = m[2], rest = NA_character_))
    m <- regmatches(s, regexec("^([A-Za-z]+),? (.+)$", s, perl = TRUE))[[1]]
    # a rest that starts with "Form" is a 1991 label out of shape ("Hutton
    # Form Lillieburn Family", "Avalon Form, Woodburn"), not a series
    if (length(m) == 3L && !grepl("(?i)^form\\b", m[3], perl = TRUE))
      return(list(era = "1977", form = m[2], rest = trimws(m[3])))
    NULL
  }
  form_of <- function(w) {
    i <- match(tolower(w), tolower(tab$forms$name))
    if (is.na(i)) NULL else tab$forms[i, ]
  }
  one <- function(lab) {
    out <- data.frame(label = lab, era = NA_character_, form_code = NA_character_,
                      form_name = NA_character_, series = NA_character_,
                      family_code = NA_character_, family_name = NA_character_,
                      family_unread = NA_character_, matched_by = NA_character_,
                      stringsAsFactors = FALSE)
    if (is.na(lab) || !nzchar(trimws(lab))) return(out)
    sh <- shape(lab)
    f <- if (!is.null(sh)) form_of(sh$form) else NULL
    by <- "name"
    if (is.null(f) && length(variants)) {
      s <- trimws(gsub("\\s+", " ", lab))
      for (i in seq_along(variants))
        s <- gsub(names(variants)[i], variants[[i]], s, ignore.case = TRUE, perl = TRUE)
      if (!identical(s, lab)) {
        sh2 <- shape(s)
        f2 <- if (!is.null(sh2)) form_of(sh2$form) else NULL
        if (!is.null(f2)) { sh <- sh2; f <- f2; by <- "variant" }
      }
    }
    if (!is.null(sh)) out$era <- sh$era
    if (is.null(f)) return(out)
    out$form_code <- f$code
    out$form_name <- f$name
    out$matched_by <- by
    if (identical(sh$era, "1977")) out$series <- sh$rest
    if (identical(sh$era, "1991")) {
      fam <- tab$families[tab$families$form == f$code, , drop = FALSE]
      j <- match(tolower(sh$rest), tolower(fam$name))
      if (is.na(j)) {
        out$family_unread <- sh$rest
      } else {
        out$family_code <- fam$code[j]
        out$family_name <- fam$name[j]
      }
    }
    out
  }
  if (!length(x)) return(one(NA_character_)[0, ])
  res <- do.call(rbind, lapply(x, one))
  rownames(res) <- NULL
  res
}
