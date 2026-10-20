# =============================================================================
# v0.9.263 -- the system registry.
#
# Every classification system is a folder inst/rules/<id>/ holding its key
# and a system.yaml entry (id, order, aliases, classifier, levels, display
# names, edition, citation, status). The code that runs "every system"
# (load_rules, .validate_rules, classify_all) reads this registry instead of
# repeating the three ids, so a new system is added by adding its folder.
#
# Ids are resolved exactly, ids first and then aliases. For the three systems
# that predate the registry (legacy: true) the unique partial match that
# match.arg() used to give still works ("w" -> "wrb2022"), so existing calls
# keep their behaviour; new systems are matched exactly only, because a
# prefix such as "s" would otherwise become ambiguous.
# =============================================================================

.registry_cache <- new.env(parent = emptyenv())

.REGISTRY_REQUIRED <- c("id", "order", "classifier", "top_level", "levels",
                        "display", "edition", "citation", "status")

# Read and check every inst/rules/<id>/system.yaml of a package (or of the
# folder `root`, which tests use).
.read_system_registry <- function(package = "soilKey",
                                  root = system.file("rules", package = package)) {
  if (!nzchar(root)) {
    rlang::abort(sprintf("No rules folder found in package '%s'.", package))
  }
  files <- file.path(list.dirs(root, full.names = TRUE, recursive = FALSE),
                     "system.yaml")
  files <- files[file.exists(files)]
  entries <- lapply(files, function(f) {
    e <- yaml::read_yaml(f)
    lacking <- setdiff(.REGISTRY_REQUIRED, names(e))
    if (length(lacking) > 0L) {
      rlang::abort(sprintf("%s lacks: %s", f, paste(lacking, collapse = ", ")))
    }
    if (!identical(e$id, basename(dirname(f)))) {
      rlang::abort(sprintf("%s: id '%s' differs from its folder name.",
                           f, e$id))
    }
    for (k in c("id", "classifier", "top_level", "edition", "status",
                "citation")) {
      v <- e[[k]]
      if (!is.character(v) || length(v) != 1L || is.na(v) || !nzchar(v)) {
        rlang::abort(sprintf("%s: '%s' must be a single non-empty string.",
                             f, k))
      }
    }
    if (!is.numeric(e$order) || length(e$order) != 1L || is.na(e$order)) {
      rlang::abort(sprintf("%s: 'order' must be a single number.", f))
    }
    en <- if (is.list(e$display)) e$display$en else NULL
    if (!is.character(en) || length(en) != 1L || is.na(en) || !nzchar(en)) {
      rlang::abort(sprintf("%s: 'display: en' must be a single non-empty string.",
                           f))
    }
    e$aliases <- as.character(unlist(e$aliases))
    e$levels  <- as.character(unlist(e$levels))
    if (length(e$levels) == 0L || !identical(e$levels[1L], e$top_level)) {
      rlang::abort(sprintf("%s: 'levels' must start with the top level '%s'.",
                           f, e$top_level))
    }
    e$legacy  <- isTRUE(e$legacy)
    e$default <- isTRUE(e$default)
    e$result_slot <- e$result_slot %||% e$id
    e$classify_all_options <- e$classify_all_options %||% list()
    e
  })
  ids <- vapply(entries, function(e) e$id, character(1))
  ord <- vapply(entries, function(e) as.numeric(e$order), numeric(1))
  entries <- entries[order(ord, ids)]
  names(entries) <- ids[order(ord, ids)]

  keys <- c(names(entries), unlist(lapply(entries, function(e) e$aliases)))
  if (anyDuplicated(keys)) {
    rlang::abort(sprintf("System ids and aliases must be unique; repeated: %s",
                         paste(unique(keys[duplicated(keys)]), collapse = ", ")))
  }
  slots <- vapply(entries, function(e) e$result_slot, character(1))
  if (anyDuplicated(slots)) {
    rlang::abort(sprintf("Result slots must be unique; repeated: %s",
                         paste(unique(slots[duplicated(slots)]), collapse = ", ")))
  }
  entries
}

# The registry of a package, read once per session.
.system_registry <- function(package = "soilKey") {
  if (is.null(.registry_cache[[package]])) {
    .registry_cache[[package]] <- .read_system_registry(package)
  }
  .registry_cache[[package]]
}

# Registered ids in run order; default_only = the systems classify_all()
# runs when asked for "all".
.system_ids <- function(default_only = FALSE) {
  reg <- .system_registry()
  if (default_only) reg <- reg[vapply(reg, function(e) e$default, logical(1))]
  names(reg)
}

# The registry entry of one id (not an alias; resolve first).
.system_get <- function(id) {
  e <- .system_registry()[[id, exact = TRUE]]
  if (is.null(e)) rlang::abort(sprintf("Unknown classification system '%s'.", id))
  e
}

# Resolve user-supplied system names to registry ids. With several.ok = TRUE
# unknown names are dropped as long as one name resolves, as match.arg() did.
# Two more match.arg() rules are kept for the calls that relied on them: NULL
# gives the first system (WRB 2022), and load_rules()'s old default vector,
# c("wrb2022", "usda", "sibcs5"), gives its first element.
.system_resolve <- function(x, several.ok = FALSE) {
  reg <- .system_registry()
  if (is.null(x)) return(names(reg)[1L])
  if (!several.ok && identical(x, c("wrb2022", "usda", "sibcs5"))) {
    return("wrb2022")
  }
  if (!is.character(x) || length(x) == 0L) {
    rlang::abort("'system' must be a non-empty character vector.")
  }
  if (!several.ok && length(x) != 1L) {
    rlang::abort("'system' must be of length 1.")
  }
  alias_ids <- unlist(lapply(reg, function(e)
    stats::setNames(rep(e$id, length(e$aliases)), e$aliases)), use.names = FALSE)
  alias_keys <- unlist(lapply(reg, function(e) e$aliases), use.names = FALSE)
  lookup <- c(stats::setNames(names(reg), names(reg)),
              stats::setNames(alias_ids, alias_keys))
  legacy <- lookup[lookup %in% names(reg)[vapply(reg, function(e) e$legacy,
                                                 logical(1))]]

  one <- function(s) {
    if (is.na(s) || !nzchar(s)) return(NA_character_)
    if (s %in% names(lookup)) return(unname(lookup[[s]]))
    hit <- unique(unname(legacy[startsWith(names(legacy), s)]))
    if (length(hit) == 1L) hit else NA_character_
  }
  ids <- vapply(x, one, character(1), USE.NAMES = FALSE)
  if (all(is.na(ids)) || (!several.ok && anyNA(ids))) {
    rlang::abort(sprintf("'system' should be one of %s (aliases: %s).",
                         paste0("'", names(reg), "'", collapse = ", "),
                         paste0("'", alias_keys, "'", collapse = ", ")))
  }
  ids[!is.na(ids)]
}


#' Classification systems known to soilKey
#'
#' Lists the classification systems registered in the package, one row per
#' system, in the order \code{\link{classify_all}} runs them. Each system is a
#' folder \code{inst/rules/<id>/} holding its key and a \code{system.yaml}
#' entry. The \code{id} is what \code{\link{load_rules}} and
#' \code{\link{classify_all}} accept; the aliases are accepted too.
#'
#' @return A \code{data.frame} with one row per system and the columns
#'   \code{id}, \code{name} (English display name), \code{edition},
#'   \code{status}, \code{classifier} (the function that classifies a
#'   \code{\link{PedonRecord}} in that system), \code{levels} and
#'   \code{aliases} (both comma-separated), and \code{default} (whether
#'   \code{classify_all(systems = "all")} runs the system).
#' @seealso \code{\link{classify_all}}, \code{\link{load_rules}}.
#' @examples
#' soilkey_systems()
#' @export
soilkey_systems <- function() {
  reg <- .system_registry()
  chr <- function(f) vapply(reg, f, character(1), USE.NAMES = FALSE)
  data.frame(
    id         = names(reg),
    name       = chr(function(e) e$display$en),
    edition    = chr(function(e) e$edition),
    status     = chr(function(e) e$status),
    classifier = chr(function(e) e$classifier),
    levels     = chr(function(e) paste(e$levels, collapse = ", ")),
    aliases    = chr(function(e) paste(e$aliases, collapse = ", ")),
    default    = vapply(reg, function(e) e$default, logical(1),
                        USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}
