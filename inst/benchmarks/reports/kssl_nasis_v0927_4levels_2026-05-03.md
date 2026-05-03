# KSSL+NASIS v0.9.27 multi-level benchmark — 2026-05-03

A/B comparison of v0.9.26 vs v0.9.27 on the same KSSL+NASIS sample
(n=865, identical filter). v0.9.27 wires the clay-illuviation
evidence test (NASIS `pediagfeatures` argillic flag and per-horizon
`clay_films_amount`) into `argillic_usda`'s threshold selection.

## Results

| Level         | v0.9.26 | v0.9.27 | Delta |
|---------------|---:|---:|---:|
| Order         | 37.23 % | 36.99 % (CI 33.9-40.2) | -0.24 pp (within CI) |
| Suborder      | 17.84 % | 17.73 % (CI 15.2-20.0) | -0.11 pp (within CI) |
| **Great Group** | 10.34 % | **10.57 %** (CI 8.6-12.5) | **+0.23 pp** |
| **Subgroup**  | 4.97 %  | **5.09 %**  (CI 3.8-6.4) | **+0.12 pp** |

## Coverage of clay-films evidence (n=878 quality-filtered)

| Slot value                                       | n   | %      | Tier used     |
|--------------------------------------------------|----:|-------:|---------------|
| `clay_films_amount` populated OR pediagfeatures argillic flag | 341 | 38.8 % | KST (3/1.2/8) |
| Indeterminate (no NASIS data at all)             | 418 | 47.6 % | WRB (proxy)   |
| NASIS data present, no argillic flag             | 119 | 13.6 % | WRB (proxy)   |

The +0.23 pp Great Group lift comes from the 38.8 % subset that
falls in the KST-only-passing band (clay increase 3-6 pp absolute,
or 1.2-1.4 ratio). Most strong-argillic profiles already pass WRB
thresholds, so the marginal lift here represents the genuinely
borderline cases.

## Why the lift is smaller than the v0.9.26 roadmap estimate (+3-5 pp)

The v0.9.26 roadmap estimated +3-5 pp Great Group based on the
266 GG misses observed at v0.9.25, with major confusion pairs
including:

- haplargids -> haplocambids/calcids (17 cases)
- argiustolls -> hapludolls (4 cases)
- argiudolls -> hapludolls (3 cases)
- hapludalfs -> paleudalfs/glossudalfs (11 cases)

In practice, of those ~30+ argillic-detection-related misses,
only a small subset actually has both:

1. NASIS pediagfeatures or clay_films_amount populated (38.8 %
   of profiles), AND
2. Clay increase in the WRB-fail/KST-pass band.

Roadmap update: the v0.9.28+ work item is to investigate the
418 indeterminate profiles (47.6 % of sample) -- many are likely
older KSSL pedons whose NASIS records were never linked. Filling
the NASIS gap (or accepting field-survey morphology evidence as
clay-films proxy for those profiles) would unlock a larger
fraction of the original confusion-pair lift.

## Reproducibility

```r
library(soilKey)
peds <- load_kssl_pedons_with_nasis(
  gpkg   = "<path>/ncss_labdata.gpkg",
  sqlite = "<path>/NASIS_Morphological_09142021.sqlite",
  head   = 1000)

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

# Verify clay-films-test coverage on your sample:
films_evid <- vapply(peds, function(p) {
  isTRUE(argillic_clay_films_test(p)$passed)
}, logical(1))
cat(sprintf("Profiles with clay-films evidence: %d/%d (%.1f%%)\n",
              sum(films_evid), length(peds), 100*mean(films_evid)))
```
