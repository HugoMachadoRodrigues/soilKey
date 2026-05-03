# KSSL+NASIS v0.9.25 multi-level benchmark — 2026-05-03

A/B comparison of v0.9.24 vs v0.9.25 on the same KSSL+NASIS sample
(n=865), identical quality filter. Only the KST 13ed Great Group
canonicaliser changed between the two runs.

## Results

| Level         | v0.9.24 | v0.9.25 | Delta |
|---------------|---:|---:|---:|
| **Order**     | 37.23 % | 37.23 % | 0.00 pp |
| **Suborder**  | 17.84 % | 17.84 % | 0.00 pp |
| **Great Group** | 6.50 % (CI 5.1-8.4) | **10.34 %** (CI 8.4-12.4) | **+3.84 pp (+59 % rel.)** |
| **Subgroup**  | 3.82 % (CI 2.8-5.0) | **4.97 %** (CI 3.7-6.4) | **+1.15 pp (+30 % rel.)** |

## Why this works

KSSL `samp_taxgrtgroup` mixes pre-KST-13ed labels (e.g. Haplaquolls,
Pellusterts, Camborthids, Vitrandepts) with modern KST 13ed labels.
The classifier emits modern names. Direct string equality therefore
produces false-negative Great Group misses for every profile with
a pre-13ed reference label. The canonicaliser
`canonicalise_kst13ed_gg()` collapses both editions to a shared key.

## Confusion-pair table BEFORE canonicaliser (top 10)

```
       haplargids -> haplocambids        :  8
         pellusterts -> hapluderts       :  8  (RESOLVED by canon)
 udipsamments -> quartzipsamments        :  7  (still missing -- requires Quartzipsamment test)
ustipsamments -> quartzipsamments        :  7  (same)
         hapludalfs -> paleudalfs        :  6
        medisaprists -> udifolists       :  6  (RESOLVED by canon)
       chromusterts -> hapluderts        :  5  (RESOLVED by canon)
       haplargids -> haplocalcids        :  5
        hapludalfs -> glossudalfs        :  5
       torriorthents -> udorthents       :  5
```

## Roadmap for v0.9.26+

The remaining 2/3 of the 266 GG misses fall into two categories:

1. **Argillic horizon detection** (~25 cases): Haplargids missing
   argillic distinction (-> Haplocambids/Haplocalcids); Hapludalfs
   missing pale/glossic refinements (-> Paleudalfs/Glossudalfs);
   Argiustolls/Argiudolls missing argillic (-> Hapludolls).

2. **Specialised Great Group tests** (~15 cases): Quartzipsamments
   (>95% quartz mineralogy), Fragiudults (fragipan), specific
   Endo-/Epi-aquic distinctions where saturation pattern matters.

## Reproducibility

```r
library(soilKey)
peds <- load_kssl_pedons_with_nasis(
  gpkg   = "<path>/ncss_labdata.gpkg",
  sqlite = "<path>/NASIS_Morphological_09142021.sqlite",
  head   = 1000)

# Same filter as this report:
keep <- vapply(peds, function(p) {
  hz <- p$horizons
  if (is.null(hz) || nrow(hz) == 0) return(FALSE)
  if (!any(!is.na(hz$clay_pct))) return(FALSE)
  !is.null(p$site$reference_usda_subgroup) &&
    !is.na(p$site$reference_usda_subgroup) &&
    nzchar(p$site$reference_usda_subgroup)
}, logical(1))
peds <- peds[keep]

for (lvl in c("order", "suborder", "great_group", "subgroup")) {
  res <- benchmark_run_classification(peds, system = "usda",
                                         level = lvl, boot_n = 200L)
  cat(sprintf("%-12s n=%d  top1=%.4f  CI=[%.3f, %.3f]\n",
                lvl, res$n_evaluated, res$accuracy_top1,
                res$accuracy_ci[1], res$accuracy_ci[2]))
}
```
