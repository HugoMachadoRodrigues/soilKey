# KSSL + NASIS: multi-level USDA Soil Taxonomy benchmark

This vignette describes the **multi-level USDA Soil Taxonomy 13ed
benchmark** that ships with `soilKey` (versions 0.9.24 and later). It is
the package’s headline real-data validation: the first public USDA Soil
Taxonomy implementation that resolves classifier accuracy at every level
of the hierarchy (Order / Suborder / Great Group / Subgroup) on real,
published lab data.

## 1. The KSSL + NASIS join (load_kssl_pedons_with_nasis)

The Kellogg Soil Survey Laboratory ([NCSS lab data,
USDA-NRCS](https://ncsslabdatamart.sc.egov.usda.gov)) ships ~36 000
pedons with full analytical chemistry (clay, sand, silt, OC, pH, CEC,
BS, …) but minimal field morphology. The NASIS Morphological database (a
separate sqlite distribution) carries the matching **field-survey
morphology**: Munsell colors, structure grade / size / type, clay films
(`phpvsf`), slickensides, and the **`pediagfeatures` diagnostic-features
table** (~13 500 entries identifying argillic, mollic, cambic, spodic,
fragipan, etc.).

[`load_kssl_pedons_with_nasis()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_pedons_with_nasis.md)
joins the two by `peiid` and depth-overlap matching, returning one
PedonRecord per profile with both lab and morphology populated:

``` r

peds <- load_kssl_pedons_with_nasis(
  gpkg   = "<path>/ncss_labdata.gpkg",
  sqlite = "<path>/NASIS_Morphological_09142021.sqlite",
  head   = 3000)

length(peds)
#> [1] 2964   # after the 3000-row gpkg slice and the join

peds[[1]]$horizons[, c("designation", "clay_pct", "munsell_chroma_moist",
                          "clay_films_amount")]
#>    designation clay_pct munsell_chroma_moist clay_films_amount
#> 1: Ap          22.5     3                    NA
#> 2: Bt1         34.7     4                    common
#> 3: Bt2         38.3     4                    many
```

NASIS-derived attributes coverage on the 2021 snapshot (n=865
quality-filtered):

| Slot                               | Coverage |
|------------------------------------|---------:|
| `nasis_diagnostic_features` (any)  |    ~52 % |
| `pediagfeatures` argillic flag     |    ~11 % |
| Per-horizon `clay_films_amount`    |    ~28 % |
| Per-horizon `munsell_chroma_moist` |    ~99 % |
| Per-horizon `structure_grade`      |    ~93 % |

The high Munsell + structure coverage powers the **pediagfeatures
tie-breaker** (v0.9.21) – when a canonical lab + morphology gate returns
`passed = NA`, the surveyor’s diagnostic flag flips it to `TRUE`. The
argillic / fragipan / clay-films paths (v0.9.27, v0.9.28, v0.9.31)
leverage the same NASIS slot for direct positive evidence.

## 2. The four levels (benchmark_run_classification)

Once you have a list of PedonRecords with `reference_usda`,
`reference_usda_subgroup`, `reference_usda_grtgroup`, and
`reference_usda_suborder` populated (the gpkg loader handles all four),
[`benchmark_run_classification()`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_run_classification.md)
measures top-1 accuracy at any of the four hierarchy levels:

``` r

# Quality filter: profiles with usable clay data and a non-empty
# subgroup reference label.
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
                                         level = lvl, boot_n = 500L)
  cat(sprintf("%-12s n=%d  top1=%.4f  CI=[%.3f, %.3f]\n",
                lvl, res$n_evaluated, res$accuracy_top1,
                res$accuracy_ci[1], res$accuracy_ci[2]))
}
```

## 3. Headline numbers (v0.9.31, n=2638, ±1.7 pp CI)

| Level           |     n |   top-1 | 95 % CI            |
|-----------------|------:|--------:|--------------------|
| **Order**       | 2 638 | 34.19 % | \[32.4 %, 36.0 %\] |
| **Suborder**    | 2 636 | 13.85 % | \[12.5 %, 15.2 %\] |
| **Great Group** | 2 633 |  7.94 % | \[7.0 %, 8.9 %\]   |
| **Subgroup**    | 2 638 |  4.17 % | \[3.5 %, 4.9 %\]   |

These are the values reported in the methodology paper. They were
measured on the n=2638 large-scale validation sample (3x the n=865
development sample) with 500 bootstrap reps for ±1.7 pp confidence
intervals.

## 4. Release-by-release trajectory

The v0.9.24 → v0.9.31 release series progressively closed key reasoning
gaps. Apples-to-apples on the n=865 development sample:

| Release | Change | GG (n=865) | Subgroup (n=865) |
|----|----|---:|---:|
| v0.9.22 | (baseline before this series) | – | 3.0 % |
| v0.9.23 | argic clay-increase canonicalisation | – | – |
| v0.9.24 | aquic / oxyaquic subgroup tightening + new GG/Suborder benchmark levels | 6.5 % | 3.8 % |
| v0.9.25 | KST 13ed Great Group canonicaliser (Pellusterts → Hapluderts; etc.) | **10.3 % (+3.84)** | **5.0 % (+1.15)** |
| v0.9.26 | per-system argic threshold API (infrastructure) | 10.3 % | 5.0 % |
| v0.9.27 | clay-films test via NASIS pediagfeatures + phpvsf | 10.6 % (+0.23) | 5.1 % (+0.12) |
| v0.9.28 | designation-based clay-films proxy (‘t’ suffix) | 10.6 % | 5.1 % |
| v0.9.30 | bundled WoSIS sample + WoSIS retry bug fix | 10.6 % | 5.1 % |
| v0.9.31 | Quartzipsamment proxy broadened + Fragipan NASIS path | **10.92 % (+0.35)** | **5.32 % (+0.23)** |

The v0.9.25 KST 13ed Great Group canonicaliser remains the largest
single-version GG lift (+3.84 pp, +59 % relative). It required no
classifier changes — the predictor is already correct for KST 13ed; only
the comparison was unfair to legacy KSSL labels (which span Soil
Taxonomy editions 8 through 13).

## 5. The four levels are independently measurable

`level = "great_group"` reads `pedon$site$reference_usda_grtgroup` and
compares the LAST token of `classify_usda(pedon)$name` after the v0.9.25
KST canonicaliser collapses edition-driven renames.

`level = "suborder"` reads `pedon$site$reference_usda_suborder` and
applies `.gg_to_suborder()` (a ~70-Suborder canonical-suffix mapping
derived from KST 13ed Ch 4) to the predicted Great Group name.

`level = "subgroup"` reads `pedon$site$reference_usda_subgroup` and
compares the full subgroup name token-by-token (Subgroup modifier kept
intact; Great Group canonicalised).

`level = "order"` reads `pedon$site$reference_usda` and compares the
top-level Order name directly.

## 6. v0.9.25 – the KST canonicaliser story

KSSL `samp_taxgrtgroup` is populated from historical pedon descriptions
spanning Soil Taxonomy editions 8 through 13. Several Great Group names
changed between editions, and KSSL did NOT retroactively update them.
soilKey’s classifier follows KST 13ed (the current edition), so direct
string equality between predicted (13ed) and reference (mixed editions)
Great Group names produces **false-negative misses** for every profile
whose KSSL label is a pre-13ed name.

The 16 most common edition-driven renames in KSSL:

| Pre-13ed name (KSSL) | KST 13ed equivalent | Reason |
|----|----|----|
| Haplaquolls | Endoaquolls / Epiaquolls | KST 11 split Hapl- into endo (deep) / epi (perched) saturation |
| Haplaquepts | Endoaquepts / Epiaquepts | same |
| Haplaquerts | Endoaquerts / Epiaquerts | same |
| Haplaquents | Endoaquents / Epiaquents | same |
| Haplaqualfs | Endoaqualfs / Epiaqualfs | same |
| Haplaquods | Endoaquods / Epiaquods | same |
| Pellusterts | Hapluderts / Salusterts / Calciusterts | KST 12: dark-colour Pellu split by chemistry |
| Chromusterts | Hapluderts | KST 12: bright-colour Chromu merged |
| Pelluderts / Chromuderts | (same Pellu/Chromu reorg, udic) | – |
| Dystrochrepts | Dystrudepts | KST 11: Ochrept suborder retired; Udept created |
| Eutrochrepts | Eutrudepts | same |
| Camborthids | Haplocambids | KST 11: Orthid suborder retired; Cambid created |
| Calciorthids | Haplocalcids | same |
| Paleorthids | Haplodurids / Durargids | (KST 11 Aridisol reshuffling) |
| Vitrandepts | Vitrudands | KST 11: Andisols promoted to its own Order |
| Medisaprists / Medihemists / Medifibrists | Haplosaprists / Haplohemists / Haplofibrists | “medi-” temperature regime moved to Subgroup |

The canonicaliser
[`canonicalise_kst13ed_gg()`](https://hugomachadorodrigues.github.io/soilKey/reference/canonicalise_kst13ed_gg.md)
collapses each pair (or triple, for the Hapl-/Endo-/Epi- splits) to a
shared canonical key. Apply to BOTH ref and pred before comparing – the
Subgroup modifier (Typic / Aquic / …) is left intact and the
canonicalisation only affects the Great Group token.

## 7. What’s missing (roadmap)

The 4.2 % Subgroup top-1 leaves substantial headroom. The v0.9.27
confusion analysis identified the dominant remaining gaps:

- **Pale- / Glossic Alfisol prefixes** (~11 misses, deferred to
  v0.9.32+): current
  [`pale_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/pale_qualifying_usda.md)
  proxy is too strict (clay \>= 35 %); KST 13ed actually defines Pale-
  by clay-stability within 150 cm. Tightening here without regressing on
  Hapludalfs is delicate work.
- **NASIS data sparsity** (~47 % of profiles lack any pediagfeatures /
  phpvsf record): roadmap candidate is field-survey morphology +
  soilKey’s pedometric proxies as a third evidence path.
- **Endo/Epi-aquic precise distinction**: currently the v0.9.25
  canonicaliser collapses these for fair comparison; a future release
  could restore the distinction by inferring saturation depth from
  `redoximorphic_features_pct` distribution.

## Summary

The KSSL + NASIS join is `soilKey`’s headline real-data benchmark. It
exercises every level of the USDA Soil Taxonomy 13ed hierarchy on n=2638
profiles with full lab + morphology data, with confidence intervals
tight enough (±1.7 pp) for paper-grade claims. The v0.9.24 → v0.9.31
release series moved Great Group accuracy from a 6.5 % baseline to 10.92
%, primarily through the KST 13ed name canonicaliser (v0.9.25) which
collapsed edition-driven renames in the historical KSSL labels.

For the WRB and SiBCS benchmark axes, see vignettes
`v06_wosis_benchmark` and the FEBR / Embrapa report at
`inst/benchmarks/reports/embrapa_v0929_2026-05-03.md`.
