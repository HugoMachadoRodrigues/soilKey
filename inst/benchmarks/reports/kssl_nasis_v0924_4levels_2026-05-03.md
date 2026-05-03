# KSSL+NASIS v0.9.24 multi-level benchmark — 2026-05-03

Apples-to-apples A/B at 4 levels of the USDA Soil Taxonomy
hierarchy on KSSL+NASIS (n=865, 1000-head load with quality and
subgroup-label filter). Both runs use the same pedons and the
same `benchmark_run_classification` driver. The only difference
between baseline and v0.9.24 is the tightening of
`aquic_conditions_usda` and `oxyaquic_subgroup_usda`.

## Results

| Level         | v0.9.23 baseline | v0.9.24 (tightening) | Delta |
|---------------|---:|---:|---:|
| **Order**     | 37.23 % (CI 34.0-40.3) | 37.23 % (CI 34.0-40.3) | 0.00 pp |
| **Suborder**  | -- | 17.84 % (CI 15.4-20.2) | (new measurement) |
| **Great Group** | -- | 6.50 % (CI 5.1-8.4) | (new measurement) |
| **Subgroup**  | 3.24 % | **3.82 %** (CI 2.8-5.0) | **+0.58 pp** |

## Filter

- `head = 1000` (1000-row pedon load).
- Quality: `any(!is.na(clay_pct))` per profile.
- Subgroup label: `reference_usda_subgroup` non-NA, non-empty.
- After filter: **n = 865 pedons**.

## Tightening summary

`aquic_conditions_usda` (KST 13ed Ch 3, pp 41-44): now requires
both reduction evidence (chroma <= 2 OR 'g' designation suffix)
AND redoximorphic-feature evidence (redox features >= min_redox_pct
OR a chroma-2-with-g matrix).

`oxyaquic_subgroup_usda` (KST 13ed Ch 14): now requires either
(a) measured redox >= 2 % AND chroma <= 4, or (b) 'g' designation
AND chroma <= 3.

## Subgroup miss diagnosis

Of the 865 pedons:
- 322 are correct at Order level (37.23 %).
- 33 are correct at Subgroup level (3.82 %).
- 289 (89.8 % of 322) are correct at Order but wrong at Subgroup.
- Of those 289, **132 (45.7 %)** have a Typic-reference subgroup.
- Of those 132, **114 (86.4 %) actually fire as Typic in the
  predictor** -- the Subgroup modifier is correct; the Great
  Group is wrong.

This identifies the **Great Group machinery** (one level above
the Subgroup modifier) as the next-leverage zone for v0.9.25+.
Adding more qualifying-modifier tests (Pachic, Cumulic, Mollic,
Lithic, etc.) would help only the remaining 132 - 114 = 18
typic-modifier-wrong cases (~6 % of correct-Order Subgroup misses).

## Companion: WoSIS GraphQL refresh

`run_wosis_benchmark_graphql` re-validation: **5/30 (16.67 %)**
on continent = "South America", page_size = 10. Compares to
v0.9.13 baseline of ~13 % WRB top-1 on a 50-profile pull. Sample
limited to 30 because the WoSIS GraphQL server returns
"canceling statement due to statement timeout" beyond ~40
profiles per session. See
`inst/benchmarks/reports/wosis_graphql_2026-05-03.md` for the
per-RSG breakdown and confusion matrix.

## Reproducibility

```r
library(soilKey)
peds <- load_kssl_pedons_with_nasis(
  gpkg = "<path>/ncss_labdata.gpkg",
  sqlite = "<path>/NASIS_Morphological_09142021.sqlite",
  head = 1000)

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
