#!/bin/zsh
# Resumable full run in chunks: split once, classify each chunk (checkpointed
# per scenario), merge, summarise.
cd "${0:A:h}"
CH=../cache/full_chunks; RF=../results_full
[ -d $CH/c01 ] || Rscript 06_split_panel.R ../cache/full $CH 3000 > full_split.log 2>&1 || exit 1
for c in $CH/c*; do
  n=${c:t}; out=$RF/chunks/$n; mkdir -p $out
  cp -n $RF/kssl_climate_regimes.csv $out/ 2>/dev/null
  [ -f $out/kssl_scenarios.csv ] && continue
  Rscript 02_classify_scenarios.R $c $out 4 > full_classify_$n.log 2>&1 || echo "chunk $n failed" >> full_errors.log
  echo "chunk $n done $(date)" >> full_progress.log
done
Rscript -e 'library(data.table); for (s in LETTERS[1:8]) { f <- Sys.glob(sprintf("../results_full/chunks/c*/kssl_scenarios_part_%s.csv", s)); if (length(f)) fwrite(rbindlist(lapply(f, fread, colClasses=list(character="pedon_key"))), sprintf("../results_full/kssl_scenarios_part_%s.csv", s)) }' > full_merge.log 2>&1
Rscript 04_summarise.R $RF > $RF/summary_full.txt 2>&1
Rscript 05_diagnose.R $RF H > $RF/diagnosis_H.txt 2>&1
Rscript 05_diagnose.R $RF F > $RF/diagnosis_F.txt 2>&1
echo "exit $? $(date)" > full_done.flag
