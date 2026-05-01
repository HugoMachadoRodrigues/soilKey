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
