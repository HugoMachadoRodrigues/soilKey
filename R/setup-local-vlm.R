# =============================================================================
# v0.9.64 -- setup_local_vlm() and Ollama lifecycle helpers.
#
# Goal: make the local-VLM path "just work" so the agent_app() Shiny UI
# (v0.9.65) can offer a one-click "Configurar Gemma local" button that:
#
#   1. Detects whether the Ollama daemon is installed and running.
#   2. Starts `ollama serve` in the background if installed but stopped.
#   3. Pulls the requested model (default: gemma4:e2b -- the smallest
#      multimodal Gemma 4 edge variant, ~1.5 GB) if not already on disk.
#   4. Reports back ready / not-ready with actionable next steps.
#
# CRAN policy forbids shipping LLM weights inside an R package (5 MB
# source-tarball cap, plus binary-blob policy). We therefore SHIP THE
# DOWNLOADER, not the weights -- the user runs `setup_local_vlm()` once
# after install and Ollama caches the model in `~/.ollama/models/`.
# =============================================================================


# ---- Constants ------------------------------------------------------------

#' Canonical Ollama model catalog used by setup_local_vlm()
#'
#' Maps short labels ("light", "balanced", "best") to multimodal Gemma
#' tags pullable via `ollama pull`. Lightest first; "balanced" is the
#' soilKey default for the agent app (small enough to run on a laptop,
#' large enough to read Munsell colors and parse PT-BR field reports
#' reliably).
#'
#' @keywords internal
.SOILKEY_OLLAMA_CATALOG <- list(
  light    = list(model = "gemma4:e2b", size_gb = 1.5,
                   note  = "Gemma 4 edge 2B -- smallest multimodal, ~1.5 GB."),
  balanced = list(model = "gemma4:e4b", size_gb = 3.0,
                   note  = "Gemma 4 edge 4B -- recommended default, ~3 GB."),
  best     = list(model = "gemma4:31b", size_gb = 19.0,
                   note  = "Gemma 4 31B dense -- best quality, ~19 GB.")
)


# ---- Detection ------------------------------------------------------------

#' Is the Ollama CLI installed?
#'
#' Returns TRUE when `ollama` resolves on the system PATH. Does NOT
#' check whether the daemon is running (use [ollama_is_running()] for
#' that).
#'
#' @return Logical scalar.
#' @export
ollama_is_installed <- function() {
  out <- tryCatch(Sys.which("ollama"), error = function(e) "")
  isTRUE(nzchar(out) && file.exists(unname(out[1L])))
}


#' Print the right install-Ollama incantation for the user's OS
#'
#' macOS -> Homebrew formula; Linux -> upstream curl-pipe-sh script;
#' Windows -> winget. Always points to the official installers
#' page <https://ollama.com/download>. Used by [setup_local_vlm()]
#' as the actionable error path when Ollama is not installed.
#'
#' @keywords internal
.print_ollama_install_hint <- function() {
  os <- Sys.info()[["sysname"]]
  cli::cli_h2("Install Ollama")
  if (identical(os, "Darwin")) {
    cli::cli_alert_info("macOS:")
    cli::cli_text("  {.code brew install --cask ollama}")
    cli::cli_text("  -- or download from {.url https://ollama.com/download/mac}")
  } else if (identical(os, "Linux")) {
    cli::cli_alert_info("Linux (any distro):")
    cli::cli_text("  {.code curl -fsSL https://ollama.com/install.sh | sh}")
  } else if (identical(os, "Windows")) {
    cli::cli_alert_info("Windows 10/11:")
    cli::cli_text("  {.code winget install Ollama.Ollama}")
    cli::cli_text("  -- or download from {.url https://ollama.com/download/windows}")
  } else {
    cli::cli_alert_info("Unknown OS -- get the installer from {.url https://ollama.com/download}.")
  }
}


# ---- Daemon lifecycle -----------------------------------------------------

#' Ensure the Ollama daemon is running, starting it if needed
#'
#' If [ollama_is_running()] already returns TRUE, this is a no-op. Else
#' tries to launch `ollama serve` in the background and polls until the
#' HTTP API answers (or `timeout_s` seconds elapse). Requires the
#' `ollama` binary to be on PATH; call [ollama_is_installed()] first.
#'
#' On success, the daemon keeps running for the rest of the R session
#' (and survives the R session, since it forks via `system2(..., wait
#' = FALSE)`). The user can stop it later with `pkill ollama` or
#' equivalent.
#'
#' @param timeout_s Polling deadline in seconds (default 30).
#' @param verbose Logical (default TRUE). Prints CLI status updates.
#' @return Logical scalar: TRUE iff the daemon is reachable when this
#'   function returns. Never throws -- returns FALSE on any failure so
#'   callers can route to [.print_ollama_install_hint()].
#' @export
ollama_ensure_running <- function(timeout_s = 30, verbose = TRUE) {
  if (ollama_is_running()) {
    if (isTRUE(verbose)) {
      cli::cli_alert_success("Ollama daemon already running.")
    }
    return(TRUE)
  }
  if (!ollama_is_installed()) {
    if (isTRUE(verbose)) {
      cli::cli_alert_warning("Ollama is not installed.")
      .print_ollama_install_hint()
    }
    return(FALSE)
  }
  if (isTRUE(verbose)) {
    cli::cli_alert_info("Ollama installed but not running -- starting `ollama serve`...")
  }
  log_path <- tempfile(pattern = "ollama_serve_", fileext = ".log")
  ok <- tryCatch({
    system2("ollama", args = "serve",
              stdout = log_path, stderr = log_path,
              wait = FALSE)
    TRUE
  }, error = function(e) {
    if (isTRUE(verbose)) {
      cli::cli_alert_danger("Failed to spawn `ollama serve`: {conditionMessage(e)}")
    }
    FALSE
  })
  if (!ok) return(FALSE)

  deadline <- Sys.time() + timeout_s
  while (Sys.time() < deadline) {
    if (ollama_is_running()) {
      if (isTRUE(verbose)) {
        cli::cli_alert_success("Ollama daemon ready (log: {.path {log_path}}).")
      }
      return(TRUE)
    }
    Sys.sleep(0.5)
  }
  if (isTRUE(verbose)) {
    cli::cli_alert_danger("Ollama daemon did not become reachable within {timeout_s}s.")
    cli::cli_alert_info("Check the log: {.path {log_path}}")
  }
  FALSE
}


# ---- Model catalog --------------------------------------------------------

#' List models currently pulled to the local Ollama
#'
#' Queries the `/api/tags` endpoint on the running daemon. Returns an
#' empty character vector when the daemon is not reachable or when no
#' models are pulled. Never throws.
#'
#' @return Character vector of model identifiers (e.g.
#'   `c("gemma4:e2b", "gemma4:e4b")`).
#' @export
ollama_list_local_models <- function() {
  if (!ollama_is_running()) return(character(0))
  if (!requireNamespace("httr", quietly = TRUE) ||
        !requireNamespace("jsonlite", quietly = TRUE)) {
    return(character(0))
  }
  url <- getOption("soilKey.ollama_url",
                     default = "http://127.0.0.1:11434/api/tags")
  out <- tryCatch({
    resp <- httr::GET(url, httr::timeout(2))
    if (httr::status_code(resp) != 200L) return(character(0))
    body <- httr::content(resp, as = "text", encoding = "UTF-8")
    parsed <- jsonlite::fromJSON(body, simplifyVector = TRUE)
    models <- parsed$models %||% data.frame()
    if (is.data.frame(models) && "name" %in% names(models)) {
      as.character(models$name)
    } else character(0)
  }, error = function(e) character(0))
  out
}


#' Pull a model into the local Ollama
#'
#' Wraps `ollama pull <model>` via [system2()]. The pull is potentially
#' large (1-20 GB depending on the model) and may take many minutes
#' over a slow connection; this function blocks until completion.
#' Skipped (no-op) when the model is already present in
#' [ollama_list_local_models()].
#'
#' @param model Ollama model identifier (e.g. `"gemma4:e2b"`).
#' @param verbose Logical (default TRUE). Streams `ollama pull` output
#'   to the console.
#' @return Logical scalar: TRUE iff the model is on-disk after this
#'   function returns.
#' @export
ollama_pull_model <- function(model, verbose = TRUE) {
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
        !nzchar(model)) {
    rlang::abort("ollama_pull_model(): 'model' must be a non-empty character scalar.")
  }
  if (!ollama_is_installed()) {
    if (isTRUE(verbose)) {
      cli::cli_alert_warning("Ollama is not installed; cannot pull {.field {model}}.")
      .print_ollama_install_hint()
    }
    return(FALSE)
  }
  already <- ollama_list_local_models()
  if (model %in% already) {
    if (isTRUE(verbose)) {
      cli::cli_alert_success("Model {.field {model}} already pulled.")
    }
    return(TRUE)
  }
  if (isTRUE(verbose)) {
    cli::cli_alert_info("Pulling {.field {model}} (this may take several minutes)...")
  }
  rc <- tryCatch(
    system2("ollama", args = c("pull", model),
              stdout = if (isTRUE(verbose)) "" else FALSE,
              stderr = if (isTRUE(verbose)) "" else FALSE),
    warning = function(w) {
      # system2() raises a warning when the command exits non-zero.
      attr(w, "rc") %||% 1L
    },
    error = function(e) 1L
  )
  ok <- isTRUE(identical(as.integer(rc), 0L)) ||
          model %in% ollama_list_local_models()
  if (isTRUE(verbose)) {
    if (ok) cli::cli_alert_success("Model {.field {model}} ready.")
    else    cli::cli_alert_danger("Failed to pull {.field {model}}.")
  }
  isTRUE(ok)
}


# ---- Top-level setup ------------------------------------------------------

#' One-call setup for the local VLM (Ollama + Gemma)
#'
#' Idempotent end-to-end bootstrap of the local VLM stack used by the
#' soilKey agent app. Detects the Ollama installation, starts the
#' daemon if needed, pulls the requested model and returns a status
#' list the caller can render in a Shiny UI.
#'
#' @param model One of `"light"` (gemma4:e2b, ~1.5 GB), `"balanced"`
#'   (gemma4:e4b, ~3 GB; default), `"best"` (gemma4:31b, ~19 GB), OR
#'   any explicit Ollama model identifier (e.g. `"qwen2.5vl:7b"`).
#' @param ensure_running Logical (default TRUE). When TRUE, also
#'   starts the daemon via [ollama_ensure_running()] when needed.
#' @param verbose Logical (default TRUE). Streams CLI status messages.
#'
#' @return Invisibly, a list with elements:
#'   \describe{
#'     \item{`ready`}{Logical -- TRUE iff the model can be used now.}
#'     \item{`model`}{Character -- the model identifier resolved.}
#'     \item{`ollama_url`}{Character -- daemon endpoint.}
#'     \item{`installed`}{Logical -- whether the Ollama CLI is on PATH.}
#'     \item{`running`}{Logical -- whether the daemon answers /api/tags.}
#'     \item{`pulled`}{Logical -- whether the model is on local disk.}
#'     \item{`hint`}{Character -- one-line next-step hint for the user
#'           (empty when `ready = TRUE`).}
#'   }
#'
#' @section What this does NOT do:
#' - Does NOT install Ollama (requires `sudo` / admin); the function
#'   prints OS-specific install hints instead.
#' - Does NOT ship the model weights inside the R package (CRAN
#'   policy); the model is pulled from the Ollama registry on first run
#'   and cached in `~/.ollama/models/`.
#' - Does NOT classify anything; once setup succeeds, call
#'   [vlm_provider("ollama", model = ...)] then the
#'   [extract_horizons_from_pdf()] / [extract_munsell_from_photo()] /
#'   [extract_site_from_fieldsheet()] family.
#'
#' @examples
#' \dontrun{
#' # Default: pull the balanced 3 GB model, start the daemon if needed.
#' status <- setup_local_vlm()
#' status$ready  # TRUE on a healthy machine with disk + bandwidth
#'
#' # Lightweight option for laptops:
#' setup_local_vlm("light")    # gemma4:e2b, ~1.5 GB
#'
#' # Best quality (server / workstation):
#' setup_local_vlm("best")     # gemma4:31b, ~19 GB
#'
#' # Any other multimodal model the user prefers:
#' setup_local_vlm("qwen2.5vl:7b")
#' }
#' @seealso [vlm_provider()], [ollama_is_running()],
#'   [ollama_pull_model()].
#' @export
setup_local_vlm <- function(model        = "balanced",
                              ensure_running = TRUE,
                              verbose      = TRUE) {
  catalog <- .SOILKEY_OLLAMA_CATALOG
  resolved <- if (model %in% names(catalog)) catalog[[model]]$model else model
  size_hint <- if (model %in% names(catalog)) catalog[[model]]$note else
                  paste0("Custom model: ", model)

  if (isTRUE(verbose)) {
    cli::cli_h1("soilKey -- local VLM setup")
    cli::cli_alert_info(size_hint)
  }

  installed <- ollama_is_installed()
  if (!installed) {
    if (isTRUE(verbose)) .print_ollama_install_hint()
    return(invisible(list(
      ready = FALSE, model = resolved,
      ollama_url = getOption("soilKey.ollama_url",
                                default = "http://127.0.0.1:11434"),
      installed = FALSE, running = FALSE, pulled = FALSE,
      hint = "Install Ollama, then re-run setup_local_vlm()."
    )))
  }

  running <- ollama_is_running()
  if (!running && isTRUE(ensure_running)) {
    running <- ollama_ensure_running(verbose = verbose)
  }
  if (!running) {
    return(invisible(list(
      ready = FALSE, model = resolved,
      ollama_url = getOption("soilKey.ollama_url",
                                default = "http://127.0.0.1:11434"),
      installed = TRUE, running = FALSE, pulled = FALSE,
      hint = "Start the Ollama daemon: `ollama serve` (or set ensure_running = TRUE)."
    )))
  }

  pulled <- ollama_pull_model(resolved, verbose = verbose)
  ready  <- isTRUE(pulled)

  if (isTRUE(verbose)) {
    if (ready) {
      cli::cli_alert_success("Local VLM ready: provider {.field ollama}, model {.field {resolved}}.")
    } else {
      cli::cli_alert_danger("Local VLM setup failed; see messages above.")
    }
  }

  invisible(list(
    ready = ready, model = resolved,
    ollama_url = getOption("soilKey.ollama_url",
                              default = "http://127.0.0.1:11434"),
    installed = TRUE, running = running, pulled = pulled,
    hint = if (ready) "" else
             paste0("Pull failed; check disk space + network and retry ",
                     "ollama_pull_model('", resolved, "').")
  ))
}
