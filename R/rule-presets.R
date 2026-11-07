# =============================================================================
# v0.9.375 -- named presets of a rule set, expanded at load time.
#
# A system whose rule files hold a `presets:` block (the Chinese Soil
# Taxonomy, inst/rules/cst2001/presets.yaml) may write a test of a class as
# a call of a preset with its parameters:
#
#   tests:
#     all_of:
#       - lithic: {depth: 50, contact: lithic}
#
# load_rules() replaces every such call with the preset's template, its
# "$name" placeholders filled in with the parameters (or the preset's
# defaults), so the rule engine reads ordinary all_of / any_of / none_of
# blocks and predicates and does not change. A parameter whose value is null
# drops the argument it fills. A template may call another preset (the
# complex subgroups are two presets keyed together). A preset is defined as
#
#   presets:
#     lithic:
#       params: {depth: required, contact: lithic_or_paralithic}
#       template: {contact_sg_cst: {top_max: $depth, kind: $contact}}
#
# A parameter whose default is the word "required" must be given. Calls with
# an unknown parameter or without a required one, and templates naming a
# placeholder that is not a parameter, are recorded in the attribute
# "preset_problems" of the rules, which .validate_rules() reports. Systems
# without presets are read exactly as before.
# =============================================================================

# The preset names a spec may call: the names of the presets block.
.preset_is_call <- function(spec, presets) {
  is.list(spec) && length(spec) >= 1L && !is.null(names(spec)) &&
    names(spec)[1L] %in% names(presets)
}

# Fill the "$name" placeholders of a template with `vals`; a placeholder whose
# value is NULL drops the element that holds it. Unknown placeholders are
# returned in attr "unknown".
.preset_fill <- function(node, vals) {
  unknown <- character(0)
  fill <- function(v) {
    if (is.character(v) && length(v) == 1L && startsWith(v, "$")) {
      k <- substring(v, 2L)
      if (!k %in% names(vals)) {
        unknown <<- c(unknown, k)
        return(v)
      }
      return(vals[[k]])
    }
    if (!is.list(v)) return(v)
    nms <- names(v)
    out <- list()
    for (i in seq_along(v)) {
      fv <- fill(v[[i]])
      if (is.null(fv)) {
        # a named argument with a null value is dropped; an item of a list
        # keeps its place as an empty mapping
        if (!is.null(nms) && nzchar(nms[i])) next
        fv <- list()
      }
      if (!is.null(nms) && nzchar(nms[i])) out[[nms[i]]] <- fv
      else out[[length(out) + 1L]] <- fv
    }
    if (is.null(nms) && !length(out) && length(v)) return(list())
    out
  }
  res <- fill(node)
  attr(res, "unknown") <- unique(unknown)
  res
}

# Expand every preset call in a rule set (see the header). Returns the rules
# without the presets block, with attributes "presets" (the block) and
# "preset_problems".
.expand_presets <- function(rules) {
  presets <- rules$presets
  if (is.null(presets)) return(rules)
  rules$presets <- NULL
  problems <- character(0)
  call_preset <- function(spec, where, depth) {
    nm <- names(spec)[1L]
    p <- presets[[nm]]
    args <- spec[[1L]]
    if (is.null(args)) args <- list()
    defaults <- p$params %||% list()
    unknown <- setdiff(names(args) %||% character(0), names(defaults))
    if (length(unknown)) {
      problems <<- c(problems, sprintf("%s: preset %s has no parameter %s", where, nm,
                                        paste(unknown, collapse = ", ")))
    }
    vals <- defaults
    for (k in names(args)) vals[k] <- list(args[[k]])
    req <- names(vals)[vapply(vals, function(v) identical(v, "required"), NA)]
    if (length(req)) {
      problems <<- c(problems, sprintf("%s: preset %s needs %s", where, nm, paste(req, collapse = ", ")))
    }
    tpl <- .preset_fill(p$template, vals)
    if (length(attr(tpl, "unknown"))) {
      problems <<- c(problems, sprintf("%s: the template of preset %s names %s, not a parameter", where, nm,
                                        paste(attr(tpl, "unknown"), collapse = ", ")))
    }
    attr(tpl, "unknown") <- NULL
    walk(tpl, where, depth + 1L)
  }
  walk <- function(x, where, depth = 0L) {
    if (!is.list(x)) return(x)
    if (depth > 20L) {
      problems <<- c(problems, sprintf("%s: presets nested too deep", where))
      return(x)
    }
    nms <- names(x) %||% rep("", length(x))
    for (i in seq_along(x)) {
      el <- x[[i]]
      if (!is.list(el)) next
      lab <- if (nzchar(nms[i])) nms[i] else if (!is.null(el$code)) as.character(el$code) else as.character(i)
      if (nms[i] %in% .RULE_COMBINATORS) {
        el <- lapply(el, function(spec) {
          if (.preset_is_call(spec, presets)) call_preset(spec, paste0(where, "/", lab), depth)
          else walk(spec, paste0(where, "/", lab), depth)
        })
        x[i] <- list(el)
      } else {
        x[i] <- list(walk(el, paste0(where, "/", lab), depth))
      }
    }
    x
  }
  out <- walk(rules, "")
  # every template names only its own parameters, the presets no row calls
  # included
  for (nm in names(presets)) {
    pv <- presets[[nm]]$params %||% list()
    u <- attr(.preset_fill(presets[[nm]]$template, pv), "unknown")
    if (length(u) || is.null(presets[[nm]]$template)) {
      problems <- c(problems, sprintf("presets/%s: the template %s", nm,
                                      if (is.null(presets[[nm]]$template)) "is missing"
                                      else sprintf("names %s, not a parameter", paste(u, collapse = ", "))))
    }
  }
  attr(out, "presets") <- presets
  attr(out, "preset_problems") <- unique(problems)
  out
}
