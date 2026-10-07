# v0.9.209: the Pro app's slow handlers (Classify, Uncertainty, the Map's point
# prior and batch, the Assistant) are shiny::ExtendedTask objects. Their results
# arrive through promises, even when no background worker runs the job, so a
# testServer() block lets the event loop run before it reads them.
.settle <- function(session, timeout = 60) {
  t0 <- Sys.time()
  repeat {
    session$flushReact()
    later::run_now(0.02)
    session$flushReact()
    if (later::loop_empty() ||
        difftime(Sys.time(), t0, units = "secs") > timeout) break
  }
  invisible(NULL)
}
