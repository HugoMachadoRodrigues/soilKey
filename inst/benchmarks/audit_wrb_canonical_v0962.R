#!/usr/bin/env Rscript
# inst/benchmarks/audit_wrb_canonical_v0962.R
#
# Audit soilKey's WRB 2022 implementation against the canonical
# parsed WRB_4th_2022 dataset (vendored from
# ncss-tech/SoilTaxonomy at v0.2.8). Produces a markdown report
# listing:
#   * RSGs in canonical NOT implemented in soilKey
#   * Principal qualifiers in canonical NOT implemented in soilKey
#   * Supplementary qualifiers in canonical NOT implemented in soilKey
#   * RSGs / qualifiers implemented in soilKey but NOT in canonical
#     (legitimate? or drift?)

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

DATE_TAG <- format(Sys.Date(), "%Y-%m-%d")
OUT <- file.path("inst/benchmarks/reports",
                  sprintf("audit_wrb_canonical_v0962_%s.md", DATE_TAG))

cat("[audit-wrb] loading canonical WRB_4th_2022 ...\n")
wrb <- wrb2022_canonical(prefer_pkg = FALSE)

# Canonical RSGs (32 distinct, multi-row per criteria clause)
canonical_rsgs <- sort(unique(toupper(wrb$rsg$reference_soil_group)))
canonical_pqs  <- sort(unique(wrb$pq$principal_qualifiers))
canonical_sqs  <- sort(unique(wrb$sq$supplementary_qualifiers))
cat(sprintf("[audit-wrb] canonical: %d RSGs, %d PQs, %d SQs\n",
            length(canonical_rsgs), length(canonical_pqs),
            length(canonical_sqs)))

# soilKey-side: scan exported function names + YAML rule names.
ns <- getNamespaceExports("soilKey")
sk_fn_names <- ns
yaml_rules_path <- system.file("rules", "wrb2022", package = "soilKey")
if (!nzchar(yaml_rules_path) || !dir.exists(yaml_rules_path))
  yaml_rules_path <- file.path("inst", "rules", "wrb2022")
yaml_files <- list.files(yaml_rules_path, pattern = "\\.yaml$",
                          full.names = TRUE, recursive = TRUE)

# Convert canonical RSG name to a soilKey-style identifier.
# WRB RSGs are pluralised in soilKey (e.g. "FERRALSOLS" -> "Ferralsols"),
# but the canonical text uses singular ALL-CAPS ("FERRALSOL").
sk_rsg_pred <- function(canonical) {
  # Canonical: "FERRALSOLS", "ARENOSOLS", etc.; some are already plural.
  # Normalise: lowercase, strip trailing s.
  base <- tolower(canonical)
  base <- sub("s$", "", base)
  # Plural Title Case (soilKey standard)
  paste0(toupper(substr(base, 1, 1)),
         substr(base, 2, nchar(base)), "s")
}

# Build src_files BEFORE rsg_status (rsg_status uses src_files).
src_files <- c(
  list.files("R", pattern = "qualifiers-wrb", full.names = TRUE),
  list.files("R", pattern = "diagnostics-(properties|materials|horizons|rsg)-wrb",
              full.names = TRUE),
  list.files("R", pattern = "diagnostics-rsg",   full.names = TRUE),
  list.files("R", pattern = "diagnostics-horizons-sibcs", full.names = TRUE),
  list.files("R", pattern = "^key-",            full.names = TRUE)
)
src_files <- unique(src_files)

rsg_status <- vapply(canonical_rsgs, function(rsg) {
  pred_name <- sk_rsg_pred(rsg)
  # Match against ALL these candidate forms (case-insensitive):
  #   * canonical singular ALL-CAPS    -- "SOLONETZ"
  #   * canonical with trailing s      -- "SOLONETZS"
  #   * soilKey plural Title Case      -- "Solonetz"  (heuristic)
  #   * lowercase singular             -- "solonetz"
  base_lc <- tolower(sub("s$", "", rsg))
  candidates <- c(rsg, paste0(rsg, "S"), pred_name,
                    base_lc, paste0(base_lc, "s"))
  rgx <- paste0("\\b(", paste(unique(candidates), collapse = "|"),
                  ")\\b")
  any(grepl(rgx, sk_fn_names, ignore.case = TRUE)) ||
    any(vapply(c(yaml_files, src_files), function(f) {
      if (!file.exists(f)) return(FALSE)
      txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
      grepl(rgx, txt, ignore.case = TRUE)
    }, logical(1L)))
}, logical(1L))

# Qualifier-name implementations: scan R/qualifiers-wrb2022*.R sources
# (src_files already built above for rsg_status detection)
src <- character(0)
for (f in src_files) src <- c(src, readLines(f, warn = FALSE))
src_blob <- tolower(paste(src, collapse = "\n"))

qual_present <- function(name) {
  # Canonical names like "Mawic", "Cryic". soilKey uses lowercase
  # function names (sometimes prefixed with q_ or qual_).
  grepl(paste0("\\b", tolower(name), "\\b"), src_blob)
}

pq_status <- vapply(canonical_pqs, qual_present, logical(1L))
sq_status <- vapply(canonical_sqs, qual_present, logical(1L))

n_rsg_impl <- sum(rsg_status); n_rsg_miss <- length(rsg_status) - n_rsg_impl
n_pq_impl  <- sum(pq_status);  n_pq_miss  <- length(pq_status)  - n_pq_impl
n_sq_impl  <- sum(sq_status);  n_sq_miss  <- length(sq_status)  - n_sq_impl

# --- write markdown report -------------------------------------------
sink(OUT)
cat("# soilKey WRB 2022 audit vs canonical (NCSS-tech)\n\n")
cat(sprintf("**Date**: %s  \n", DATE_TAG))
cat(sprintf("**soilKey version**: %s  \n",
            as.character(utils::packageVersion("soilKey"))))
cat(sprintf("**Canonical source**: NCSS-tech/SoilTaxonomy, ",
            "WRB_4th_2022 (vendored ~8 KB)  \n"))
cat(sprintf("**Reference**: IUSS Working Group WRB (2022). ",
            "*World Reference Base for Soil Resources*, 4th edition.\n\n"))

cat("## Coverage summary\n\n")
cat(sprintf("| Element                | Canonical | Implemented | Missing |\n"))
cat(sprintf("|------------------------|----------:|------------:|--------:|\n"))
cat(sprintf("| Reference Soil Groups  | %9d | %11d | %7d |\n",
            length(canonical_rsgs), n_rsg_impl, n_rsg_miss))
cat(sprintf("| Principal qualifiers   | %9d | %11d | %7d |\n",
            length(canonical_pqs), n_pq_impl, n_pq_miss))
cat(sprintf("| Supplementary qualif.  | %9d | %11d | %7d |\n",
            length(canonical_sqs), n_sq_impl, n_sq_miss))

cat("\n## Reference Soil Groups\n\n")
cat("### Implemented\n\n")
for (rsg in names(rsg_status)[rsg_status])
  cat(sprintf("- `%s`\n", rsg))
cat("\n### Missing (canonical NOT in soilKey)\n\n")
miss_rsgs <- names(rsg_status)[!rsg_status]
if (length(miss_rsgs) == 0L) {
  cat("(none -- all 32 canonical RSGs are implemented)\n")
} else {
  for (rsg in miss_rsgs) cat(sprintf("- `%s`\n", rsg))
}

cat("\n## Principal qualifiers\n\n")
cat(sprintf("Total canonical: %d  \n", length(canonical_pqs)))
cat(sprintf("Implemented (heuristic match in R/qualifiers-wrb*.R): %d (%.1f%%)\n\n",
            n_pq_impl, 100 * n_pq_impl / length(canonical_pqs)))
cat("### Missing principal qualifiers (top 50)\n\n")
miss_pqs <- names(pq_status)[!pq_status]
for (q in head(sort(miss_pqs), 50L)) cat(sprintf("- `%s`\n", q))
if (length(miss_pqs) > 50L)
  cat(sprintf("... and %d more (see RDS file).\n", length(miss_pqs) - 50L))

cat("\n## Supplementary qualifiers\n\n")
cat(sprintf("Total canonical: %d  \n", length(canonical_sqs)))
cat(sprintf("Implemented (heuristic match): %d (%.1f%%)\n\n",
            n_sq_impl, 100 * n_sq_impl / length(canonical_sqs)))
cat("### Missing supplementary qualifiers (top 50)\n\n")
miss_sqs <- names(sq_status)[!sq_status]
for (q in head(sort(miss_sqs), 50L)) cat(sprintf("- `%s`\n", q))
if (length(miss_sqs) > 50L)
  cat(sprintf("... and %d more (see RDS file).\n", length(miss_sqs) - 50L))

cat("\n## Caveats\n\n")
cat("- Heuristic matching: a qualifier is 'implemented' if its name\n")
cat("  appears (case-insensitive, word-boundary) anywhere in the\n")
cat("  WRB-related R sources. False positives possible if the name\n")
cat("  collides with an unrelated identifier; false negatives if the\n")
cat("  qualifier was implemented under a different identifier.\n")
cat("- Canonical RSG names are singular ALL-CAPS in WRB 2022 text;\n")
cat("  soilKey uses plural Title Case. The detector tries both.\n")
cat("- 'Missing' here means 'not detected by the heuristic'. Manual\n")
cat("  review needed to confirm a real coverage gap before opening\n")
cat("  v0.9.63 implementation tickets.\n")
sink()
cat(sprintf("[audit-wrb] wrote %s\n", OUT))

# Also write the raw status as RDS for downstream consumption
saveRDS(list(
  date = DATE_TAG,
  rsg_status = rsg_status,
  pq_status  = pq_status,
  sq_status  = sq_status
), file.path("inst/benchmarks/reports",
              sprintf("audit_wrb_canonical_v0962_%s.rds", DATE_TAG)))
cat("[audit-wrb] DONE\n")
