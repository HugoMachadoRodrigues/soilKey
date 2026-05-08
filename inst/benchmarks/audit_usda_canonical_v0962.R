#!/usr/bin/env Rscript
# inst/benchmarks/audit_usda_canonical_v0962.R
#
# Audit soilKey's USDA Soil Taxonomy 13ed implementation against
# the canonical parsed ST_criteria_13th + ST_features datasets
# (vendored from ncss-tech/SoilTaxonomy at v0.2.8).

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

DATE_TAG <- format(Sys.Date(), "%Y-%m-%d")
OUT <- file.path("inst/benchmarks/reports",
                  sprintf("audit_usda_canonical_v0962_%s.md", DATE_TAG))

cat("[audit-usda] loading canonical ST_criteria_13th + ST_features ...\n")
kst <- kst13_canonical(prefer_pkg = FALSE)
feat <- st_features_canonical(prefer_pkg = FALSE)
cat(sprintf("[audit-usda] canonical: %d KST clauses, %d diagnostic features\n",
            length(kst), nrow(feat)))

# Diagnostic features by group
feat_groups <- table(feat$group)
cat("\nFeatures by group:\n")
print(feat_groups)

# Source files to scan
src_files <- c(
  list.files("R", pattern = "diagnostics-(epipedons|horizons|conditions)-usda",
              full.names = TRUE),
  list.files("R", pattern = "diagnostics-orders-usda", full.names = TRUE),
  list.files("R", pattern = "diagnostics-(alfisols|andisols|aridisols|entisols|gelisols|histosols|inceptisols|mollisols|oxisols|spodosols|ultisols|vertisols)-usda",
              full.names = TRUE),
  list.files("R", pattern = "key-usda", full.names = TRUE)
)
src <- character(0)
for (f in src_files) src <- c(src, readLines(f, warn = FALSE))
src_blob <- tolower(paste(src, collapse = "\n"))

# --- Diagnostic feature coverage ---
feat_status <- vapply(feat$name, function(name) {
  # Strip parentheticals; lowercase; word-boundary match
  tag <- tolower(gsub("\\s*\\(.*", "", name))
  tag <- gsub("[^a-z0-9_]+", "_", tag)
  # Try a few forms
  forms <- c(tag, gsub("_$", "", tag),
              gsub("^_", "", tag),
              sub("_+$", "", tag))
  forms <- unique(forms[nzchar(forms)])
  rgx <- paste0("\\b(", paste(forms, collapse = "|"), ")\\b")
  grepl(rgx, src_blob, perl = TRUE)
}, logical(1L))
n_feat_impl <- sum(feat_status); n_feat_miss <- length(feat_status) - n_feat_impl

# --- USDA Order coverage ---
canonical_orders <- c("Gelisols", "Histosols", "Spodosols", "Andisols",
                       "Oxisols", "Vertisols", "Aridisols", "Ultisols",
                       "Mollisols", "Alfisols", "Inceptisols", "Entisols")
order_status <- vapply(canonical_orders, function(o) {
  rgx <- paste0("\\b", tolower(o), "\\b")
  grepl(rgx, src_blob)
}, logical(1L))
n_order_impl <- sum(order_status)

# --- KST clause-name coverage (informational only) ---
# Each kst entry has $taxon (Subgroup name typically). Count distinct taxa.
all_kst_taxa <- unlist(lapply(kst, function(b) b$taxon))
all_kst_taxa <- unique(all_kst_taxa[!is.na(all_kst_taxa) & all_kst_taxa != "*"])
cat(sprintf("[audit-usda] %d distinct KST taxa in canonical\n",
            length(all_kst_taxa)))

# Subgroup-name coverage (very heuristic: just match the FIRST word of
# each canonical subgroup, e.g. "Typic", "Aquic"... in our YAML).
subgroup_word1 <- unique(vapply(all_kst_taxa, function(t) {
  parts <- strsplit(t, " ", fixed = TRUE)[[1L]]
  if (length(parts) == 0L) NA_character_
  else tolower(parts[1L])
}, character(1L)))
subgroup_word1 <- subgroup_word1[!is.na(subgroup_word1) &
                                    nzchar(subgroup_word1)]
yaml_paths <- list.files(
  c(file.path("inst", "rules", "usda"),
    file.path("inst", "rules", "usda", "subgroups")),
  pattern = "\\.yaml$", recursive = TRUE, full.names = TRUE)
yaml_blob <- if (length(yaml_paths) > 0L) {
                tolower(paste(unlist(lapply(yaml_paths, function(f)
                                  readLines(f, warn = FALSE))),
                                  collapse = "\n"))
              } else ""
sg_word_status <- vapply(subgroup_word1, function(w) {
  grepl(paste0("\\b", w, "\\b"), yaml_blob, perl = TRUE)
}, logical(1L))

sink(OUT)
cat("# soilKey USDA Soil Taxonomy 13ed audit vs canonical (NCSS-tech)\n\n")
cat(sprintf("**Date**: %s  \n", DATE_TAG))
cat(sprintf("**soilKey version**: %s  \n",
            as.character(utils::packageVersion("soilKey"))))
cat("**Canonical source**: NCSS-tech/SoilTaxonomy ",
    "(`ST_criteria_13th`, `ST_features`)\n")
cat("**Reference**: USDA-NRCS Soil Survey Staff (2022). ",
    "*Keys to Soil Taxonomy*, 13th edition.\n\n")

cat("## Coverage summary\n\n")
cat("| Element                       | Canonical | Implemented | Missing |\n")
cat("|-------------------------------|----------:|------------:|--------:|\n")
cat(sprintf("| USDA Soil Orders              | %9d | %11d | %7d |\n",
            length(canonical_orders), n_order_impl,
            length(canonical_orders) - n_order_impl))
cat(sprintf("| Diagnostic features (canonical) | %7d | %11d | %7d |\n",
            length(feat_status), n_feat_impl, n_feat_miss))
cat(sprintf("| Distinct KST taxa             | %9d | n/a (~%d via YAML) | n/a |\n",
            length(all_kst_taxa), sum(sg_word_status)))

cat("\n## USDA Soil Orders\n\n")
cat(sprintf("Implemented: %d / %d\n\n", n_order_impl,
            length(canonical_orders)))
for (o in canonical_orders) {
  cat(sprintf("- `%s` : %s\n", o,
              if (order_status[[o]]) "implemented" else "**MISSING**"))
}

cat("\n## Diagnostic features (84 canonical) by group\n\n")
for (grp in unique(feat$group)) {
  grp_feats <- feat[feat$group == grp, ]
  grp_impl  <- feat_status[feat$group == grp]
  cat(sprintf("\n### %s (%d / %d implemented)\n\n",
              grp, sum(grp_impl), length(grp_impl)))
  for (i in seq_len(nrow(grp_feats))) {
    nm <- grp_feats$name[i]
    cat(sprintf("- `%s` : %s\n", nm,
                if (grp_impl[i]) "OK" else "**not detected**"))
  }
}

cat("\n## Caveats\n\n")
cat("- Heuristic name-matching: a feature is 'implemented' if its\n")
cat("  name (lowercased + tokenised) appears anywhere in the USDA\n")
cat("  R sources or YAML rules. False positives possible (collision\n")
cat("  with other identifiers); false negatives if the feature was\n")
cat("  implemented under a different name.\n")
cat("- Subgroup coverage uses first-word matching as a proxy. The\n")
cat("  detailed Subgroup audit (matching all canonical subgroup\n")
cat("  names against the YAML rules) is a v0.9.63 task.\n")
cat("- USDA Order names are pluralised in canonical text and in\n")
cat("  soilKey output.\n")
sink()
cat(sprintf("[audit-usda] wrote %s\n", OUT))

saveRDS(list(
  date = DATE_TAG,
  feat_status = feat_status,
  order_status = order_status,
  sg_word_status = sg_word_status
), file.path("inst/benchmarks/reports",
              sprintf("audit_usda_canonical_v0962_%s.rds", DATE_TAG)))
cat("[audit-usda] DONE\n")
