# Extracted from test-v0981-sibcs-subordem.R:105

# test -------------------------------------------------------------------------
skip_if_not(file.exists("/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/redape_geotab"),
                "redape_geotab dataset not available")
peds <- suppressMessages(suppressWarnings(load_redape_pedons(
    "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/redape_geotab",
    verbose = FALSE)))
res <- suppressMessages(suppressWarnings(benchmark_redape(peds,
                                                            level = "order",
                                                            verbose = FALSE)))
expect_gt(res$accuracy, 0.56)
expect_lt(res$accuracy, 0.63)
