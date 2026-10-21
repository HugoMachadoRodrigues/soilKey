# =============================================================================
# Generate inst/ATTRIBUTES.md -- the canonical soilKey attribute reference.
#
# Since v0.9.266 the document comes from horizon_attribute_spec(), which joins
#   * the canonical names + R types   <- horizon_column_spec()
#   * section, unit and description   <- inst/schemas/horizon-attributes.yaml
#   * accepted aliases                <- .horizon_name_aliases()
# A test (test-v09266-attribute-spec.R) fails when the file is out of date.
#
# Run with:  Rscript data-raw/generate_attributes_doc.R
# =============================================================================

suppressMessages(pkgload::load_all(".", quiet = TRUE))

out <- soilKey:::.attributes_markdown()
writeLines(out, "inst/ATTRIBUTES.md")
a <- horizon_attribute_spec()
cat(sprintf("Wrote inst/ATTRIBUTES.md: %d attributes across %d sections (v%s).\n",
            nrow(a), length(unique(a$section)),
            as.character(utils::packageVersion("soilKey"))))
