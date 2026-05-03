# KSSL+NASIS v0.9.27 large-scale validation (n=3000) — 2026-05-03

Validation benchmark at 3x the n=865 sample size used throughout
v0.9.24-v0.9.27 development, to confirm the gains and produce
tighter confidence intervals for paper-quality claims.

## Configuration

- Loader: `load_kssl_pedons_with_nasis(head = 3000)` (3x v0.9.27
  development sample).
- Quality filter: same as the n=865 reports (clay_pct populated +
  reference_usda_subgroup non-empty).
- After filter: **n = 2638 pedons**.
- Bootstrap replicates: 500 (vs. 200 in development reports).

## Headline numbers (CI ±1.7 pp)

| Level         | n     | top-1            | 95 % CI            |
|---------------|------:|-----------------:|--------------------|
| **Order**     | 2638  | **34.19 %**      | [32.4 %, 36.0 %]   |
| **Suborder**  | 2636  | **13.85 %**      | [12.5 %, 15.2 %]   |
| **Great Group** | 2633 | **7.94 %**      | [7.0 %, 8.9 %]    |
| **Subgroup**  | 2638  | **4.17 %**       | [3.5 %, 4.9 %]     |

## Comparison with n=865 development numbers

| Level       | n=865 (dev) | n=2638 (validation) | Δ (validation − dev) |
|-------------|---:|---:|---:|
| Order       | 36.99 % | 34.19 % | -2.80 pp |
| Suborder    | 17.73 % | 13.85 % | -3.88 pp |
| Great Group | 10.57 % | 7.94 %  | -2.63 pp |
| Subgroup    | 5.09 %  | 4.17 %  | -0.92 pp |

The n=2638 numbers are **uniformly lower** than the n=865 numbers
because the n=865 sample (the first 1000-row chunk of KSSL with
quality filter) was a higher-than-average draw. Larger samples
include more difficult profiles -- thin Bt's, missing chemistry,
ambiguous moisture regime designations, and field-survey labels
that span KST editions 8-12 unevenly.

## Interpretation

The headline numbers for paper claims should be the
**n=2638** values, NOT the n=865 development numbers. The
**+3.84 pp Great Group lift from the v0.9.25 KST canonicaliser**
was measured A/B on the same n=865 sample so it remains valid as
a relative gain. To produce the equivalent A/B at n=2638 would
require re-running the v0.9.24 baseline at n=3000 (~75 min on
this machine); deferred to v0.9.28+.

## Time profile

- Loader: ~30 s (3000-row gpkg + NASIS sqlite join).
- Order benchmark: ~3 min (`classify_usda` x 3000 + bootstrap).
- Suborder benchmark: ~3 min (re-runs classifier; bootstrap).
- Great Group benchmark: ~3 min (same).
- Subgroup benchmark: ~65 min (the dominant cost; subgroup
  machinery touches all KST 13ed sub-tests on every profile).

Total: ~75 min wall clock on a single CPU. Subgroup-level
classification dominates 87 % of the runtime; if subgroup is
not needed, an Order/Suborder/Great-Group-only run completes in
~10 minutes.

## Reproducibility

```r
library(soilKey)
peds <- load_kssl_pedons_with_nasis(
  gpkg   = "<path>/ncss_labdata.gpkg",
  sqlite = "<path>/NASIS_Morphological_09142021.sqlite",
  head   = 3000)

keep <- vapply(peds, function(p) {
  hz <- p$horizons
  if (is.null(hz) || nrow(hz) == 0) return(FALSE)
  if (!any(!is.na(hz$clay_pct))) return(FALSE)
  !is.null(p$site$reference_usda_subgroup) &&
    !is.na(p$site$reference_usda_subgroup) &&
    nzchar(p$site$reference_usda_subgroup)
}, logical(1))
peds <- peds[keep]
stopifnot(length(peds) > 2500)  # ~2638 expected on the 2021 NASIS snapshot

for (lvl in c("order", "suborder", "great_group", "subgroup")) {
  res <- benchmark_run_classification(peds, system = "usda",
                                         level = lvl, boot_n = 500L)
  cat(sprintf("%-12s n=%d  top1=%.4f  CI=[%.3f, %.3f]\n",
                lvl, res$n_evaluated, res$accuracy_top1,
                res$accuracy_ci[1], res$accuracy_ci[2]))
}
```
