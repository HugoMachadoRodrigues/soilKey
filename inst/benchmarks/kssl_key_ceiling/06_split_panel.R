# Split the full panel into chunks the classifier can hold in memory.
# Usage: Rscript 06_split_panel.R <cache_full> <chunk_root> [chunk_size]
args <- commandArgs(trailingOnly = TRUE)
src <- args[1]; root <- args[2]; size <- if (length(args) >= 3) as.integer(args[3]) else 3000L
suppressMessages(pkgload::load_all("/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/Packages/soilKey", quiet = TRUE))
pan <- readRDS(file.path(src, "kssl_panel.rds"))
ids <- vapply(pan$nasis, function(p) p$site$id, "")
grp <- split(seq_along(ids), ceiling(seq_along(ids) / size))
for (g in names(grp)) {
  d <- file.path(root, sprintf("c%02d", as.integer(g))); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  i <- grp[[g]]
  saveRDS(list(lab = pan$nasis[i], nasis = pan$nasis[i],
               meta = pan$meta[pan$meta$pedon_key %in% ids[i]]),
          file.path(d, "kssl_panel.rds"), compress = FALSE)
  message("chunk ", g, ": ", length(i))
}
