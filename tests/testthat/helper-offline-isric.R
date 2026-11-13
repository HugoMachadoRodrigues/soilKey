# v0.9.404: the test suite never queries ISRIC. Every request to its GraphQL
# services (WoSIS and ISIS) goes through .wosis_post(), which is replaced here
# for the whole run by an offline stand-in that answers as an unreachable
# server does. Before, the WoSIS picker of the app and two tests of the live
# reader sent real requests to graphql.isric.org, so the suite depended on
# ISRIC and could wait on its server. Tests that need a reply mock
# .wosis_post themselves (local_mocked_bindings), which takes precedence; the
# LIVE tests, run with SOILKEY_TEST_WOSIS_LIVE=true, get the real one.
# Only inside a test run: load_all() also sources the helpers, outside one.
if (testthat::is_testing() &&
    !isTRUE(as.logical(Sys.getenv("SOILKEY_TEST_WOSIS_LIVE", "false")))) {
  testthat::local_mocked_bindings(
    .wosis_post = function(query, variables = NULL, timeout = 30, endpoint = NULL)
      list(data = NULL, error = "ISRIC is not queried in the tests (offline stand-in)"),
    .package = "soilKey",
    .env = testthat::teardown_env()
  )
}
