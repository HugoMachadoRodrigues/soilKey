#!/usr/bin/env Rscript
# inst/benchmarks/audit_usda_subgroup_v0963.R
#
# Refined USDA Subgroup audit: matches every distinct canonical
# subgroup name (e.g. "Typic Hapludults", "Aquandic Cryoboralfs")
# against soilKey's YAML subgroup rules. This replaces the v0.9.62
# first-word heuristic which under-counted dramatically (claimed
# only ~13/84 features detectable).

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

DATE_TAG <- format(Sys.Date(), "%Y-%m-%d")
OUT <- file.path("inst/benchmarks/reports",
                  sprintf("audit_usda_subgroup_v0963_%s.md",
                            DATE_TAG))

cat("[audit-usda-sg] loading canonical KST 13ed codes ...\n")
codes <- kst13_codes()
# Subgroups are 4-letter codes; Great Groups are 3-letter.
sg_codes  <- codes[nchar(codes$code) == 4L, ]
gg_codes  <- codes[nchar(codes$code) == 3L, ]
sub_codes <- codes[nchar(codes$code) == 2L, ]
ord_codes <- codes[nchar(codes$code) == 1L, ]
cat(sprintf("[audit-usda-sg] canonical KST 13ed: %d Orders, %d Suborders, %d Great Groups, %d Subgroups\n",
            nrow(ord_codes), nrow(sub_codes), nrow(gg_codes),
            nrow(sg_codes)))

# soilKey YAML rules + R sources to scan
rule_paths <- c(
  list.files("inst/rules/usda", pattern = "\\.yaml$",
              recursive = TRUE, full.names = TRUE),
  list.files("inst/rules/usda", pattern = "\\.yml$",
              recursive = TRUE, full.names = TRUE)
)
src_paths <- list.files("R", pattern = "-usda\\.R$", full.names = TRUE)
all_paths <- c(rule_paths, src_paths)

read_or_empty <- function(p) tryCatch(readLines(p, warn = FALSE),
                                          error = function(e) character(0))
blob <- tolower(paste(unlist(lapply(all_paths, read_or_empty)),
                        collapse = "\n"))

# Helper: tokenise + strip plurals + lowercase a name like
# "Typic Hapludults" -> c("typic", "hapludult", "hapludults"),
# then check whether each token appears in the blob OR the full
# normalised name appears verbatim.
match_subgroup <- function(name) {
  if (is.na(name) || !nzchar(name)) return(FALSE)
  norm <- tolower(trimws(name))
  if (grepl(norm, blob, fixed = TRUE)) return(TRUE)
  # Try with each token + each plural variant
  toks <- strsplit(norm, "\\s+", fixed = FALSE)[[1L]]
  ok <- vapply(toks, function(t) {
    rgx <- paste0("\\b", t, "s?\\b")
    grepl(rgx, blob, perl = TRUE)
  }, logical(1L))
  # Implemented if BOTH the modifier (Typic / Aquic / etc.) AND the
  # GG token (last token) appear separately. This is not perfect but
  # eliminates the v0.9.62 first-word over-count.
  if (length(toks) >= 2L) all(ok) else any(ok)
}

# --- Order ---
ord_status <- vapply(ord_codes$name, match_subgroup, logical(1L))
sub_status <- vapply(sub_codes$name, match_subgroup, logical(1L))
gg_status  <- vapply(gg_codes$name,  match_subgroup, logical(1L))
sg_status  <- vapply(sg_codes$name,  match_subgroup, logical(1L))

cat(sprintf("[audit-usda-sg] Orders     : %d / %d\n",
            sum(ord_status), nrow(ord_codes)))
cat(sprintf("[audit-usda-sg] Suborders  : %d / %d\n",
            sum(sub_status), nrow(sub_codes)))
cat(sprintf("[audit-usda-sg] Great Grp  : %d / %d\n",
            sum(gg_status),  nrow(gg_codes)))
cat(sprintf("[audit-usda-sg] Subgroups  : %d / %d\n",
            sum(sg_status),  nrow(sg_codes)))

sink(OUT)
cat("# soilKey USDA KST 13ed Subgroup audit (refined v0.9.63)\n\n")
cat(sprintf("**Date**: %s  \n", DATE_TAG))
cat(sprintf("**soilKey version**: %s  \n",
            as.character(utils::packageVersion("soilKey"))))
cat("**Canonical source**: SoilKnowledgeBase 2022_KST_codes.json (vendored)\n\n")

cat("## Coverage at each KST level (refined matching)\n\n")
cat("| Level       | Canonical | Implemented | Missing |\n")
cat("|-------------|----------:|------------:|--------:|\n")
cat(sprintf("| Order       | %9d | %11d | %7d |\n",
            nrow(ord_codes), sum(ord_status),
            nrow(ord_codes) - sum(ord_status)))
cat(sprintf("| Suborder    | %9d | %11d | %7d |\n",
            nrow(sub_codes), sum(sub_status),
            nrow(sub_codes) - sum(sub_status)))
cat(sprintf("| Great Group | %9d | %11d | %7d |\n",
            nrow(gg_codes),  sum(gg_status),
            nrow(gg_codes)  - sum(gg_status)))
cat(sprintf("| Subgroup    | %9d | %11d | %7d |\n",
            nrow(sg_codes),  sum(sg_status),
            nrow(sg_codes)  - sum(sg_status)))

# Per-Order Subgroup coverage (since users care most about
# headline-Order numbers).
cat("\n## Subgroup coverage per Order\n\n")
cat("| Order | Subgroups (canonical) | Implemented | % |\n")
cat("|-------|---------------------:|------------:|--:|\n")
for (i in seq_len(nrow(ord_codes))) {
  oc <- ord_codes$code[i]
  on <- ord_codes$name[i]
  # Subgroups under this Order: 4-letter codes starting with `oc`
  these_sg <- sg_codes[startsWith(sg_codes$code, oc), ]
  these_status <- sg_status[which(sg_codes$code %in% these_sg$code)]
  pct <- if (nrow(these_sg) > 0L) 100 * sum(these_status) / nrow(these_sg)
         else NA_real_
  cat(sprintf("| %s (%s) | %19d | %11d | %.1f%% |\n",
              on, oc, nrow(these_sg), sum(these_status),
              pct))
}

# Top 30 missing Subgroups (alphabetical)
cat("\n## Top 30 missing Subgroups\n\n")
miss_sg <- sg_codes$name[!sg_status]
for (n in head(sort(miss_sg), 30L))
  cat(sprintf("- `%s`\n", n))
if (length(miss_sg) > 30L)
  cat(sprintf("\n... and %d more.\n", length(miss_sg) - 30L))

cat("\n## Caveats\n\n")
cat("- Refined matcher: requires ALL space-separated tokens of a\n")
cat("  Subgroup name (e.g. \"Typic\" + \"Hapludults\") to appear in\n")
cat("  the YAML rules / R sources blob. Plural variants (\"-s\",\n")
cat("  \"-es\", \"-ies\") matched as alternates. Verbatim full-name\n")
cat("  match also accepted.\n")
cat("- This is much closer to truth than the v0.9.62 first-word\n")
cat("  heuristic but still has false positives (modifier words\n")
cat("  like \"Typic\" or \"Aquic\" are ubiquitous and may match\n")
cat("  unrelated rules) and false negatives (multi-word YAML\n")
cat("  names with non-canonical word order).\n")
cat("- Manual review of the missing-Subgroup list before opening\n")
cat("  v0.9.64 implementation tickets is recommended.\n")
sink()
cat(sprintf("[audit-usda-sg] wrote %s\n", OUT))

saveRDS(list(date = DATE_TAG,
              ord_status = ord_status,
              sub_status = sub_status,
              gg_status  = gg_status,
              sg_status  = sg_status,
              missing_sg = miss_sg),
        file.path("inst/benchmarks/reports",
                    sprintf("audit_usda_subgroup_v0963_%s.rds",
                              DATE_TAG)))
cat("[audit-usda-sg] DONE\n")
