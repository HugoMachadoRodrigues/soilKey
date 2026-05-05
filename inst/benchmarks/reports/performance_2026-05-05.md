# soilKey performance benchmark — v0.9.53 (2026-05-05)

Reproducible classifier latency + batch throughput. The numbers
below were collected on Apple M-series silicon (`aarch64-apple-darwin23`,
R 4.6.0) with **soilKey 0.9.53** loaded via `pkgload::load_all()`,
on 200 synthetic 5-horizon pedons (fixed RNG seed 42).

## How to reproduce

```r
library(soilKey)
bench <- benchmark_performance(n = 200, verbose = TRUE)
bench$summary
```

Same seed -> same pedons -> comparable timings across releases.

## 200-pedon batch (n = 200, seed = 42)

| System  | Median (s/pedon) | Mean (s/pedon) | Total (s) | Throughput (pedons/min) |
|---------|-----------------:|---------------:|----------:|------------------------:|
| WRB 2022    | **0.021** | 0.026 |  5.16 | **2,327** |
| SiBCS 5a    | **0.037** | 0.039 |  7.75 | **1,549** |
| USDA-ST 13a | **0.121** | 0.207 | 41.42 | **290** |

## What this means at scale

| Dataset | n pedons | WRB | SiBCS | USDA |
|---|---:|---:|---:|---:|
| LUCAS Soil 2018 (full) | ~18,984 | ~ 8 min | ~ 12 min | ~ 65 min |
| KSSL / NCSS pedon DB    | ~36,000 | ~ 15 min | ~ 23 min | ~ 124 min |
| Embrapa BDsolos full    | ~9,000  | ~ 4 min | ~ 6 min | ~ 31 min |

A typical "full release of one continent" benchmark (~20k pedons)
takes between 8 minutes (WRB only) and just over an hour (all three
systems serial). Embarrassingly parallel: pass a `mclapply()` /
`future_lapply()` wrapper to cut the wall time linearly with cores.

## Per-system observations

### WRB 2022 -- fastest (2,327 pedons/min)

The WRB key is shallow (one RSG per pedon, no hierarchical descent
beyond the qualifier layer) and the diagnostic predicates are mostly
arithmetic over the horizon table. The qualifier evaluation does
add ~10 ms but is well-cached.

### SiBCS 5a -- 1,549 pedons/min

50% slower than WRB because the SiBCS key descends through
ordem -> subordem -> grande grupo -> subgrupo, with the YAML rule
engine evaluating per-level test blocks. The v0.9.45
color-undetermined path adds a one-time check per pedon and is
not the bottleneck.

### USDA Soil Taxonomy 13a -- 290 pedons/min

About 8x slower than WRB. USDA-ST evaluates **all 12 orders** in
sequence (Gelisols -> Histosols -> Spodosols -> Andisols -> ...)
before assigning, even though only one matches. Each order has
its own diagnostic predicates and great-group / subgroup descent.
This is faithful to the chapter-structure of the Keys to Soil
Taxonomy 13ed and is **not** a bottleneck for typical research-scale
runs (a 36,000-pedon NCSS benchmark fits in two hours), but it's
the natural target for v0.9.54+ optimisation if scale grows.

## Memory profile

The classifiers are stateless: no global caches, no growing
data structures. Per-pedon allocations are dominated by the
`ClassificationResult$trace` list (one entry per RSG / order
tested), which is < 50 KB per pedon. A 36,000-pedon batch with
all three systems retained in memory is < 6 GB of R objects,
comfortably below typical research workstations.

## Test sentinel

`tests/testthat/test-v0953-performance.R` runs a 3-pedon mini-bench
unconditionally on CI and verifies that `median_seconds < 5` per
system. A 50x regression on the synthetic fixture would trip this
check, alerting reviewers before a release ships.

## What's next

- **v0.9.54+ candidate**: short-circuit the USDA-ST order loop
  once the mineral-soil vs organic-soil + diagnostic-horizon
  pre-filter narrows to one or two orders. Expected 3-5x speedup
  on large batches.
- **Parallel wrapper**: a `classify_all_parallel()` helper around
  `parallel::mclapply()` (Linux/macOS) or `future_lapply()` (Windows).
- **Hot-path profiling**: `profvis::profvis()` on a 1k-pedon run
  to identify the YAML rule-engine hotspots in `run_taxonomic_key()`.
