#!/bin/zsh
# SOLUS-only scenarios (S, U, T) on every chunk, resumable; then merge and summarise.
cd "${0:A:h}"
CH=../cache/full_chunks; RF=../results_full
for c in $CH/c*; do
  n=${c:t}; out=$RF/chunks/$n
  [ -f $out/kssl_scenarios_part_S.csv ] && [ -f $out/kssl_scenarios_part_U.csv ] && [ -f $out/kssl_scenarios_part_T.csv ] && continue
  Rscript 02_classify_scenarios.R $c $out 4 S,U,T > solus_classify_$n.log 2>&1 || echo "chunk $n failed" >> solus_errors.log
  echo "chunk $n done $(date)" >> solus_progress.log
done
Rscript -e 'library(data.table); for (s in c("S","U","T")) { f <- Sys.glob(sprintf("../results_full/chunks/c*/kssl_scenarios_part_%s.csv", s)); if (length(f)) fwrite(rbindlist(lapply(f, fread, colClasses=list(character="pedon_key"))), sprintf("../results_full/kssl_scenarios_part_%s.csv", s)) }' > solus_merge.log 2>&1
Rscript 04_summarise.R $RF > $RF/summary_full_solus.txt 2>&1
Rscript 05_diagnose.R $RF S > $RF/diagnosis_S.txt 2>&1
Rscript 05_diagnose.R $RF U > $RF/diagnosis_U.txt 2>&1
echo "exit $? $(date)" > solus_done.flag
