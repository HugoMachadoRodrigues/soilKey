# Package-init helpers for soilKey. The .onLoad hook auto-detects
# PROJ_LIB / GDAL_DATA on macOS Homebrew, conda, and standard Linux
# installs so terra::rast(crs = "EPSG:4326") finds proj.db without
# requiring the user to set environment variables manually. This is
# the layperson on-ramp the v0.9.14 ARCHITECTURE flagged as the most
# common installation foot-gun on non-Linux platforms.

.onLoad <- function(libname, pkgname) {
  auto_set_proj_env(verbose = FALSE)
  invisible(NULL)
}


# v0.9.66 -- .onAttach() suggests / triggers local Gemma setup.
#
# Behaviour matrix (interactive sessions only; non-interactive R is
# always silent):
#
#   Ollama not installed             -> silent (no startup spam)
#   Ollama installed but stopped     -> silent (avoid fork on attach)
#   Daemon running, model present    -> brief "ready" hint
#   Daemon running, model missing    -> hint to call setup_local_vlm("light")
#                                       OR auto-pull when the user opted in
#                                       via options(soilKey.auto_setup_vlm = TRUE).
#
# Suppress everything with options(soilKey.suggest_local_vlm = FALSE).
#
# CRAN-compliance: never auto-modifies the user's machine without an
# explicit options() opt-in (CRAN Repository Policy 1.1 forbids
# packages writing to the system on attach).
.onAttach <- function(libname, pkgname) {
  if (!interactive()) return(invisible())
  if (isFALSE(getOption("soilKey.suggest_local_vlm", default = TRUE))) {
    return(invisible())
  }
  msg <- .suggest_local_vlm_message(target_model = "gemma4:e2b")
  if (nzchar(msg)) packageStartupMessage(msg)

  # Opt-in auto-pull. Default OFF; user enables via either:
  #   options(soilKey.auto_setup_vlm = TRUE)
  #   Sys.setenv(SOILKEY_AUTO_SETUP_VLM = "1")
  auto <- isTRUE(getOption("soilKey.auto_setup_vlm",
                              default = identical(Sys.getenv("SOILKEY_AUTO_SETUP_VLM"),
                                                    "1")))
  if (!auto) return(invisible())
  if (!ollama_is_installed() || !ollama_is_running()) return(invisible())
  models <- tryCatch(ollama_list_local_models(), error = function(e) character(0))
  if ("gemma4:e2b" %in% models) return(invisible())
  packageStartupMessage(
    "soilKey: auto_setup_vlm = TRUE -- pulling gemma4:e2b in background..."
  )
  # Background pull so package attach is not blocked. The user sees
  # progress next time they open `ollama ps` or call setup_local_vlm().
  tryCatch(
    system2("ollama", args = c("pull", "gemma4:e2b"),
              stdout = FALSE, stderr = FALSE, wait = FALSE),
    error = function(e) invisible()
  )
}


#' Build the local-VLM suggestion shown by .onAttach
#'
#' Pure function (no side effects). Returns the multi-line string
#' that .onAttach would print, given the current Ollama state.
#' Factored out for testability: the unit tests exercise this with
#' stubbed inputs instead of touching the real Ollama daemon.
#'
#' @param target_model Ollama model identifier soilKey wants to see.
#' @return Character scalar -- the message body, or `""` when no
#'   message is appropriate (e.g. Ollama not installed at all -- no
#'   point nagging the user).
#' @keywords internal
.suggest_local_vlm_message <- function(target_model = "gemma4:e2b") {
  if (!ollama_is_installed()) return("")  # silent: avoid uninstalled-tool nagging
  if (!ollama_is_running()) {
    return(paste0(
      "soilKey: Ollama installed but daemon stopped. Run ",
      "`soilKey::setup_local_vlm(\"light\")` (or `ollama serve`) to ",
      "enable the local VLM agent. Suppress with ",
      "options(soilKey.suggest_local_vlm = FALSE)."
    ))
  }
  models <- tryCatch(ollama_list_local_models(),
                       error = function(e) character(0))
  if (target_model %in% models) {
    return(paste0(
      "soilKey: local VLM ready (", target_model,
      "). Launch the agent with `soilKey::run_agent_app()`."
    ))
  }
  paste0(
    "soilKey: Ollama detected, but `", target_model, "` (~1.5 GB) is ",
    "not yet pulled. Run `soilKey::setup_local_vlm(\"light\")` once to ",
    "enable the local VLM agent, or set ",
    "options(soilKey.auto_setup_vlm = TRUE) to let soilKey pull it ",
    "next time the package is attached. Suppress this hint with ",
    "options(soilKey.suggest_local_vlm = FALSE)."
  )
}


#' Auto-detect PROJ_LIB and GDAL_DATA directories
#'
#' Probes the common system locations for PROJ \code{proj.db} and
#' GDAL data directories, on macOS Homebrew (Apple silicon and
#' Intel), Linuxbrew, conda / mamba environments, and Debian /
#' Ubuntu / Fedora apt or dnf installs. Sets the corresponding
#' environment variables only when they are not already set, so a
#' user-provided value always wins. Idempotent: safe to call
#' repeatedly.
#'
#' Called automatically from \code{.onLoad}; call manually after
#' installing PROJ / GDAL via Homebrew if you want to refresh the
#' env without restarting R.
#'
#' @param verbose If \code{TRUE}, emits a \code{cli} message
#'        confirming what was detected.
#' @return Invisibly, a named list with \code{PROJ_LIB} and
#'         \code{GDAL_DATA} (the values that were set, or
#'         \code{NA_character_} if a value was already present
#'         or no candidate was found).
#' @export
auto_set_proj_env <- function(verbose = FALSE) {
  proj_candidates <- c(
    "/opt/homebrew/share/proj",                   # macOS Homebrew (Apple silicon)
    "/usr/local/share/proj",                      # macOS Homebrew (Intel)
    "/home/linuxbrew/.linuxbrew/share/proj",      # Linuxbrew
    file.path(Sys.getenv("CONDA_PREFIX"), "share", "proj"),  # conda / mamba
    "/usr/share/proj",                            # apt / dnf
    "/usr/lib/x86_64-linux-gnu/proj"
  )
  gdal_candidates <- c(
    "/opt/homebrew/share/gdal",
    "/usr/local/share/gdal",
    "/home/linuxbrew/.linuxbrew/share/gdal",
    file.path(Sys.getenv("CONDA_PREFIX"), "share", "gdal"),
    "/usr/share/gdal",
    "/usr/lib/gdal"
  )

  pick_first_existing <- function(candidates) {
    for (d in candidates) {
      if (nzchar(d) && dir.exists(d)) return(d)
    }
    NA_character_
  }

  set <- list(PROJ_LIB = NA_character_, GDAL_DATA = NA_character_)
  if (!nzchar(Sys.getenv("PROJ_LIB"))) {
    p <- pick_first_existing(proj_candidates)
    if (!is.na(p)) {
      Sys.setenv(PROJ_LIB = p)
      set$PROJ_LIB <- p
    }
  }
  if (!nzchar(Sys.getenv("GDAL_DATA"))) {
    g <- pick_first_existing(gdal_candidates)
    if (!is.na(g)) {
      Sys.setenv(GDAL_DATA = g)
      set$GDAL_DATA <- g
    }
  }

  if (isTRUE(verbose)) {
    if (!is.na(set$PROJ_LIB))
      cli::cli_alert_success("Auto-detected PROJ_LIB: {.path {set$PROJ_LIB}}")
    if (!is.na(set$GDAL_DATA))
      cli::cli_alert_success("Auto-detected GDAL_DATA: {.path {set$GDAL_DATA}}")
    if (is.na(set$PROJ_LIB) && is.na(set$GDAL_DATA))
      cli::cli_alert_info("PROJ_LIB / GDAL_DATA already set or no candidate dir found.")
  }
  invisible(set)
}
