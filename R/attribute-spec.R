# =============================================================================
# v0.9.266 -- metadata of the horizon attributes.
#
# horizon_column_spec() keeps the names and R types of the canonical horizon
# columns. Their section, unit and description now live in one file,
# inst/schemas/horizon-attributes.yaml, which also has room for what the
# national systems will need: the keys that read an attribute (systems), the
# values of a categorical one (allowed) and the range of a numeric one (min,
# max). horizon_attribute_spec() joins the two with the accepted aliases, and
# the attribute reference inst/ATTRIBUTES.md is generated from it. Before, the
# generator parsed the comments of R/utils.R and kept its own table of units.
# =============================================================================

.ATTRIBUTE_FIELDS <- c("name", "section", "unit", "description")

# The entries of inst/schemas/horizon-attributes.yaml, checked against
# horizon_column_spec(): one entry per column, in the same order.
.attribute_metadata <- function() {
  path <- system.file("schemas", "horizon-attributes.yaml", package = "soilKey")
  meta <- yaml::read_yaml(path)$attributes
  nms  <- vapply(meta, function(e) as.character(e$name %||% ""), character(1))
  spec <- names(horizon_column_spec())
  if (!identical(nms, spec)) {
    rlang::abort(sprintf(paste(
      "horizon-attributes.yaml and horizon_column_spec() disagree.",
      "Missing from the YAML: %s. Not in the spec: %s."),
      paste(setdiff(spec, nms), collapse = ", ") %|e% "none",
      paste(setdiff(nms, spec), collapse = ", ") %|e% "none"))
  }
  for (e in meta) {
    lacking <- setdiff(.ATTRIBUTE_FIELDS, names(e))
    if (length(lacking) > 0L) {
      rlang::abort(sprintf("horizon-attributes.yaml, '%s' lacks: %s",
                           e$name, paste(lacking, collapse = ", ")))
    }
  }
  stats::setNames(meta, nms)
}

# "" -> fallback, for messages.
`%|e%` <- function(a, b) if (length(a) == 0L || !nzchar(a)) b else a


#' Metadata of the horizon attributes
#'
#' One row per canonical horizon column (see
#' \code{\link{horizon_column_spec}}), in its order, with the section it
#' belongs to, its unit, a description and the alternative column names that
#' are recognised for it. The metadata come from
#' \code{inst/schemas/horizon-attributes.yaml}, which also records, where
#' known, which classification keys read an attribute, the values a
#' categorical attribute takes and the range of a numeric one.
#'
#' @return A \code{data.frame} with the columns \code{name}, \code{type}
#'   (\code{"numeric"} or \code{"character"}), \code{section}, \code{unit},
#'   \code{description}, \code{aliases}, \code{systems} and \code{allowed}
#'   (comma-separated, empty when none), and \code{min} and \code{max}
#'   (\code{NA} when not recorded).
#' @seealso \code{\link{horizon_column_spec}}, \code{\link{ensure_horizon_schema}}.
#' @examples
#' a <- horizon_attribute_spec()
#' a[a$name %in% c("clay_pct", "cec_cmol"), c("name", "unit", "description")]
#' @export
horizon_attribute_spec <- function() {
  spec <- horizon_column_spec()
  meta <- .attribute_metadata()
  aliases <- .horizon_name_aliases()
  alias_rev <- split(names(aliases), unname(aliases))
  field <- function(f) vapply(meta, function(e)
    as.character(e[[f]] %||% ""), character(1), USE.NAMES = FALSE)
  joined <- function(f) vapply(meta, function(e)
    paste(as.character(unlist(e[[f]])), collapse = ", "), character(1),
    USE.NAMES = FALSE)
  number <- function(f) vapply(meta, function(e)
    as.numeric(e[[f]] %||% NA_real_), numeric(1), USE.NAMES = FALSE)
  data.frame(
    name        = names(spec),
    type        = unlist(spec, use.names = FALSE),
    section     = field("section"),
    unit        = field("unit"),
    description = field("description"),
    aliases     = vapply(names(spec), function(n)
                    paste(sort(alias_rev[[n]]), collapse = ", "),
                    character(1), USE.NAMES = FALSE),
    systems     = joined("systems"),
    allowed     = joined("allowed"),
    min         = number("min"),
    max         = number("max"),
    stringsAsFactors = FALSE
  )
}


# The lines of inst/ATTRIBUTES.md: one table per section, in the order the
# sections first appear in the spec. data-raw/generate_attributes_doc.R writes
# them; a test checks that the file is up to date.
.attributes_markdown <- function(version = as.character(utils::packageVersion("soilKey"))) {
  a <- horizon_attribute_spec()
  md_escape <- function(x) gsub("\\|", "\\\\|", x)
  out <- c(
    sprintf("# soilKey -- recognised horizon attribute names (v%s)", version),
    "",
    "This is the authoritative list of the analytical and morphological attribute",
    "names that soilKey reads from a horizon table (CSV upload in soilKey Pro, or a",
    "`data.frame`/`data.table` passed to the R API). Every classification key reads",
    "these columns; anything else is preserved but ignored by the keys.",
    "",
    "## Naming conventions",
    "",
    "* Canonical names are **lower-case `snake_case`**, with an explicit unit/method",
    "  suffix (`_cm`, `_pct`, `_cmol`, `_g_cm3`, `_mg_kg`, `_dS_m`, `_C`).",
    "* Column-name recognition is **case-insensitive** and **separator-insensitive**:",
    "  `pH_H2O`, `PH.H2O`, `ph h2o` and `pH-H2O` are all recognised as `ph_h2o`.",
    "* A small table of **common abbreviations** is also accepted (see the *aliases*",
    "  column below): e.g. `Clay` -> `clay_pct`, `SOC` -> `oc_pct`, `CEC` -> `cec_cmol`.",
    "* An exact canonical name always wins and is never overwritten; unrecognised",
    "  columns are kept verbatim and simply not read by the keys.",
    "* Units are as listed. Percentages are by mass unless noted `(vol)`; exchangeable",
    "  cations and CEC are in cmol(+)/kg (= meq/100 g).",
    "",
    "> Tip (R): `soilKey::horizon_attribute_spec()` returns this table (names, types,",
    "> sections, units, descriptions and aliases); `soilKey::horizon_column_spec()`",
    "> the canonical names and types; `soilKey::ensure_horizon_schema(df)` normalises",
    "> a table to them.",
    ""
  )
  for (s in unique(a$section)) {
    rows <- a[a$section == s, , drop = FALSE]
    out <- c(out, sprintf("## %s", s), "",
             "| Attribute | Unit | Type | Description | Accepted aliases |",
             "|-----------|------|------|-------------|------------------|")
    for (i in seq_len(nrow(rows))) {
      al <- if (nzchar(rows$aliases[i]))
              paste0("`", strsplit(rows$aliases[i], ", ", fixed = TRUE)[[1]],
                     "`", collapse = ", ")
            else "-"
      out <- c(out, sprintf("| `%s` | %s | %s | %s | %s |", rows$name[i],
                            rows$unit[i], rows$type[i],
                            md_escape(rows$description[i]), al))
    }
    out <- c(out, "")
  }
  c(out,
    "---",
    "",
    sprintf(paste0("_Generated from `inst/schemas/horizon-attributes.yaml`, ",
                   "`horizon_column_spec()` and `.horizon_name_aliases()` in ",
                   "soilKey v%s._"), version),
    "")
}
