# Benchmarking soilKey against WoSIS

This vignette describes the benchmark protocol that measures `soilKey`’s
WRB 2022 classification agreement against an external reference set,
with **WoSIS** (the World Soil Information Service, ISRIC) as the
canonical target. It also runs a fully reproducible mini-benchmark on
the package’s own canonical fixtures so the protocol can be exercised
offline.

The full WoSIS run (see §5) is the basis of the agreement statistics
reported in the methodological paper accompanying `soilKey` v1.0.

## 1. Protocol overview

Each benchmark profile is a triple $`(P, T, C)`$:

- $`P`$ – a `PedonRecord` assembled from the source dataset’s horizons +
  chemistry;
- $`T`$ – the **target** RSG name from the source’s own classification
  (e.g. WoSIS’ `cwrb_reference_soil_group`);
- $`C`$ – the **candidate** classification produced by
  `classify_wrb2022(P)`.

The benchmark reports four numbers per source:

- **Top-1 agreement** – fraction of profiles where $`C\$rsg = T`$.
- **Top-3 agreement** – fraction where $`T`$ is among the three
  highest-likelihood RSGs in the trace (key trace + spatial prior, when
  available).
- **Coverage** – fraction of profiles where $`C`$ is a definite RSG (not
  “Regosols” catch-all).
- **Indeterminate-rate** – fraction of profiles where $`C\$rsg`$ is NA
  owing to missing diagnostic attributes.

Confusion matrices are reported alongside the headline numbers: which
RSGs `soilKey` confuses with which (e.g. Acrisol vs Lixisol differ only
by base saturation, so disagreement on this border is informative).

## 2. Mini-benchmark on the canonical fixtures

The package’s 31 canonical fixtures provide a self-contained benchmark:
each fixture is *constructed* to map to a specific RSG, so the expected
target is known. The mini-benchmark verifies that
[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
agrees with the construction intent on every fixture – i.e. top-1
agreement is 100% on this set.

``` r

expected <- c(
  HS = "Histosols", AT = "Anthrosols", TC = "Technosols", CR = "Cryosols",
  LP = "Leptosols", SN = "Solonetz",   VR = "Vertisols", SC = "Solonchaks",
  GL = "Gleysols",  AN = "Andosols",   PZ = "Podzols",   PT = "Plinthosols",
  PL = "Planosols", ST = "Stagnosols", NT = "Nitisols",  FR = "Ferralsols",
  CH = "Chernozems", KS = "Kastanozems", PH = "Phaeozems", UM = "Umbrisols",
  DU = "Durisols",  GY = "Gypsisols", CL = "Calcisols", RT = "Retisols",
  AC = "Acrisols",  LX = "Lixisols",   AL = "Alisols",   LV = "Luvisols",
  CM = "Cambisols", AR = "Arenosols",  FL = "Fluvisols"
)

fixfns <- list(
  HS = make_histosol_canonical,  AT = make_anthrosol_canonical,
  TC = make_technosol_canonical, CR = make_cryosol_canonical,
  LP = make_leptosol_canonical,  SN = make_solonetz_canonical,
  VR = make_vertisol_canonical,  SC = make_solonchak_canonical,
  GL = make_gleysol_canonical,   AN = make_andosol_canonical,
  PZ = make_podzol_canonical,    PT = make_plinthosol_canonical,
  PL = make_planosol_canonical,  ST = make_stagnosol_canonical,
  NT = make_nitisol_canonical,   FR = make_ferralsol_canonical,
  CH = make_chernozem_canonical, KS = make_kastanozem_canonical,
  PH = make_phaeozem_canonical,  UM = make_umbrisol_canonical,
  DU = make_durisol_canonical,   GY = make_gypsisol_canonical,
  CL = make_calcisol_canonical,  RT = make_retisol_canonical,
  AC = make_acrisol_canonical,   LX = make_lixisol_canonical,
  AL = make_alisol_canonical,    LV = make_luvisol_canonical,
  CM = make_cambisol_canonical,  AR = make_arenosol_canonical,
  FL = make_fluvisol_canonical
)

bench <- do.call(rbind, lapply(names(expected), function(code) {
  fx <- fixfns[[code]]()
  res <- classify_wrb2022(fx, on_missing = "silent")
  data.frame(
    fixture  = code,
    target   = expected[[code]],
    assigned = res$rsg_or_order,
    name     = res$name,
    grade    = res$evidence_grade,
    match    = res$rsg_or_order == expected[[code]]
  )
}))

knitr::kable(bench[, c("fixture", "target", "assigned", "match", "grade")])
```

| fixture | target      | assigned    | match | grade |
|:--------|:------------|:------------|:------|:------|
| HS      | Histosols   | Histosols   | TRUE  | A     |
| AT      | Anthrosols  | Anthrosols  | TRUE  | A     |
| TC      | Technosols  | Technosols  | TRUE  | A     |
| CR      | Cryosols    | Cryosols    | TRUE  | A     |
| LP      | Leptosols   | Leptosols   | TRUE  | A     |
| SN      | Solonetz    | Solonetz    | TRUE  | A     |
| VR      | Vertisols   | Vertisols   | TRUE  | A     |
| SC      | Solonchaks  | Solonchaks  | TRUE  | A     |
| GL      | Gleysols    | Gleysols    | TRUE  | A     |
| AN      | Andosols    | Andosols    | TRUE  | A     |
| PZ      | Podzols     | Podzols     | TRUE  | A     |
| PT      | Plinthosols | Plinthosols | TRUE  | A     |
| PL      | Planosols   | Planosols   | TRUE  | A     |
| ST      | Stagnosols  | Stagnosols  | TRUE  | A     |
| NT      | Nitisols    | Nitisols    | TRUE  | A     |
| FR      | Ferralsols  | Ferralsols  | TRUE  | A     |
| CH      | Chernozems  | Chernozems  | TRUE  | A     |
| KS      | Kastanozems | Kastanozems | TRUE  | A     |
| PH      | Phaeozems   | Phaeozems   | TRUE  | A     |
| UM      | Umbrisols   | Umbrisols   | TRUE  | A     |
| DU      | Durisols    | Durisols    | TRUE  | A     |
| GY      | Gypsisols   | Gypsisols   | TRUE  | A     |
| CL      | Calcisols   | Calcisols   | TRUE  | A     |
| RT      | Retisols    | Retisols    | TRUE  | A     |
| AC      | Acrisols    | Acrisols    | TRUE  | A     |
| LX      | Lixisols    | Lixisols    | TRUE  | A     |
| AL      | Alisols     | Alisols     | TRUE  | A     |
| LV      | Luvisols    | Luvisols    | TRUE  | A     |
| CM      | Cambisols   | Cambisols   | TRUE  | A     |
| AR      | Arenosols   | Arenosols   | TRUE  | A     |
| FL      | Fluvisols   | Fluvisols   | TRUE  | A     |

Headline numbers:

``` r

data.frame(
  metric = c("n_profiles", "top1_agreement",
             "indeterminate_rate", "evidence_grade_A",
             "evidence_grade_B"),
  value  = c(nrow(bench),
             mean(bench$match),
             mean(is.na(bench$assigned)),
             mean(bench$grade == "A"),
             mean(bench$grade == "B"))
)
#>               metric value
#> 1         n_profiles    31
#> 2     top1_agreement     1
#> 3 indeterminate_rate     0
#> 4   evidence_grade_A     1
#> 5   evidence_grade_B     0
```

Confusion matrix on the mini set:

``` r

table(target = bench$target, assigned = bench$assigned)
#>              assigned
#> target        Acrisols Alisols Andosols Anthrosols Arenosols Calcisols
#>   Acrisols           1       0        0          0         0         0
#>   Alisols            0       1        0          0         0         0
#>   Andosols           0       0        1          0         0         0
#>   Anthrosols         0       0        0          1         0         0
#>   Arenosols          0       0        0          0         1         0
#>   Calcisols          0       0        0          0         0         1
#>   Cambisols          0       0        0          0         0         0
#>   Chernozems         0       0        0          0         0         0
#>   Cryosols           0       0        0          0         0         0
#>   Durisols           0       0        0          0         0         0
#>   Ferralsols         0       0        0          0         0         0
#>   Fluvisols          0       0        0          0         0         0
#>   Gleysols           0       0        0          0         0         0
#>   Gypsisols          0       0        0          0         0         0
#>   Histosols          0       0        0          0         0         0
#>   Kastanozems        0       0        0          0         0         0
#>   Leptosols          0       0        0          0         0         0
#>   Lixisols           0       0        0          0         0         0
#>   Luvisols           0       0        0          0         0         0
#>   Nitisols           0       0        0          0         0         0
#>   Phaeozems          0       0        0          0         0         0
#>   Planosols          0       0        0          0         0         0
#>   Plinthosols        0       0        0          0         0         0
#>   Podzols            0       0        0          0         0         0
#>   Retisols           0       0        0          0         0         0
#>   Solonchaks         0       0        0          0         0         0
#>   Solonetz           0       0        0          0         0         0
#>   Stagnosols         0       0        0          0         0         0
#>   Technosols         0       0        0          0         0         0
#>   Umbrisols          0       0        0          0         0         0
#>   Vertisols          0       0        0          0         0         0
#>              assigned
#> target        Cambisols Chernozems Cryosols Durisols Ferralsols Fluvisols
#>   Acrisols            0          0        0        0          0         0
#>   Alisols             0          0        0        0          0         0
#>   Andosols            0          0        0        0          0         0
#>   Anthrosols          0          0        0        0          0         0
#>   Arenosols           0          0        0        0          0         0
#>   Calcisols           0          0        0        0          0         0
#>   Cambisols           1          0        0        0          0         0
#>   Chernozems          0          1        0        0          0         0
#>   Cryosols            0          0        1        0          0         0
#>   Durisols            0          0        0        1          0         0
#>   Ferralsols          0          0        0        0          1         0
#>   Fluvisols           0          0        0        0          0         1
#>   Gleysols            0          0        0        0          0         0
#>   Gypsisols           0          0        0        0          0         0
#>   Histosols           0          0        0        0          0         0
#>   Kastanozems         0          0        0        0          0         0
#>   Leptosols           0          0        0        0          0         0
#>   Lixisols            0          0        0        0          0         0
#>   Luvisols            0          0        0        0          0         0
#>   Nitisols            0          0        0        0          0         0
#>   Phaeozems           0          0        0        0          0         0
#>   Planosols           0          0        0        0          0         0
#>   Plinthosols         0          0        0        0          0         0
#>   Podzols             0          0        0        0          0         0
#>   Retisols            0          0        0        0          0         0
#>   Solonchaks          0          0        0        0          0         0
#>   Solonetz            0          0        0        0          0         0
#>   Stagnosols          0          0        0        0          0         0
#>   Technosols          0          0        0        0          0         0
#>   Umbrisols           0          0        0        0          0         0
#>   Vertisols           0          0        0        0          0         0
#>              assigned
#> target        Gleysols Gypsisols Histosols Kastanozems Leptosols Lixisols
#>   Acrisols           0         0         0           0         0        0
#>   Alisols            0         0         0           0         0        0
#>   Andosols           0         0         0           0         0        0
#>   Anthrosols         0         0         0           0         0        0
#>   Arenosols          0         0         0           0         0        0
#>   Calcisols          0         0         0           0         0        0
#>   Cambisols          0         0         0           0         0        0
#>   Chernozems         0         0         0           0         0        0
#>   Cryosols           0         0         0           0         0        0
#>   Durisols           0         0         0           0         0        0
#>   Ferralsols         0         0         0           0         0        0
#>   Fluvisols          0         0         0           0         0        0
#>   Gleysols           1         0         0           0         0        0
#>   Gypsisols          0         1         0           0         0        0
#>   Histosols          0         0         1           0         0        0
#>   Kastanozems        0         0         0           1         0        0
#>   Leptosols          0         0         0           0         1        0
#>   Lixisols           0         0         0           0         0        1
#>   Luvisols           0         0         0           0         0        0
#>   Nitisols           0         0         0           0         0        0
#>   Phaeozems          0         0         0           0         0        0
#>   Planosols          0         0         0           0         0        0
#>   Plinthosols        0         0         0           0         0        0
#>   Podzols            0         0         0           0         0        0
#>   Retisols           0         0         0           0         0        0
#>   Solonchaks         0         0         0           0         0        0
#>   Solonetz           0         0         0           0         0        0
#>   Stagnosols         0         0         0           0         0        0
#>   Technosols         0         0         0           0         0        0
#>   Umbrisols          0         0         0           0         0        0
#>   Vertisols          0         0         0           0         0        0
#>              assigned
#> target        Luvisols Nitisols Phaeozems Planosols Plinthosols Podzols
#>   Acrisols           0        0         0         0           0       0
#>   Alisols            0        0         0         0           0       0
#>   Andosols           0        0         0         0           0       0
#>   Anthrosols         0        0         0         0           0       0
#>   Arenosols          0        0         0         0           0       0
#>   Calcisols          0        0         0         0           0       0
#>   Cambisols          0        0         0         0           0       0
#>   Chernozems         0        0         0         0           0       0
#>   Cryosols           0        0         0         0           0       0
#>   Durisols           0        0         0         0           0       0
#>   Ferralsols         0        0         0         0           0       0
#>   Fluvisols          0        0         0         0           0       0
#>   Gleysols           0        0         0         0           0       0
#>   Gypsisols          0        0         0         0           0       0
#>   Histosols          0        0         0         0           0       0
#>   Kastanozems        0        0         0         0           0       0
#>   Leptosols          0        0         0         0           0       0
#>   Lixisols           0        0         0         0           0       0
#>   Luvisols           1        0         0         0           0       0
#>   Nitisols           0        1         0         0           0       0
#>   Phaeozems          0        0         1         0           0       0
#>   Planosols          0        0         0         1           0       0
#>   Plinthosols        0        0         0         0           1       0
#>   Podzols            0        0         0         0           0       1
#>   Retisols           0        0         0         0           0       0
#>   Solonchaks         0        0         0         0           0       0
#>   Solonetz           0        0         0         0           0       0
#>   Stagnosols         0        0         0         0           0       0
#>   Technosols         0        0         0         0           0       0
#>   Umbrisols          0        0         0         0           0       0
#>   Vertisols          0        0         0         0           0       0
#>              assigned
#> target        Retisols Solonchaks Solonetz Stagnosols Technosols Umbrisols
#>   Acrisols           0          0        0          0          0         0
#>   Alisols            0          0        0          0          0         0
#>   Andosols           0          0        0          0          0         0
#>   Anthrosols         0          0        0          0          0         0
#>   Arenosols          0          0        0          0          0         0
#>   Calcisols          0          0        0          0          0         0
#>   Cambisols          0          0        0          0          0         0
#>   Chernozems         0          0        0          0          0         0
#>   Cryosols           0          0        0          0          0         0
#>   Durisols           0          0        0          0          0         0
#>   Ferralsols         0          0        0          0          0         0
#>   Fluvisols          0          0        0          0          0         0
#>   Gleysols           0          0        0          0          0         0
#>   Gypsisols          0          0        0          0          0         0
#>   Histosols          0          0        0          0          0         0
#>   Kastanozems        0          0        0          0          0         0
#>   Leptosols          0          0        0          0          0         0
#>   Lixisols           0          0        0          0          0         0
#>   Luvisols           0          0        0          0          0         0
#>   Nitisols           0          0        0          0          0         0
#>   Phaeozems          0          0        0          0          0         0
#>   Planosols          0          0        0          0          0         0
#>   Plinthosols        0          0        0          0          0         0
#>   Podzols            0          0        0          0          0         0
#>   Retisols           1          0        0          0          0         0
#>   Solonchaks         0          1        0          0          0         0
#>   Solonetz           0          0        1          0          0         0
#>   Stagnosols         0          0        0          1          0         0
#>   Technosols         0          0        0          0          1         0
#>   Umbrisols          0          0        0          0          0         1
#>   Vertisols          0          0        0          0          0         0
#>              assigned
#> target        Vertisols
#>   Acrisols            0
#>   Alisols             0
#>   Andosols            0
#>   Anthrosols          0
#>   Arenosols           0
#>   Calcisols           0
#>   Cambisols           0
#>   Chernozems          0
#>   Cryosols            0
#>   Durisols            0
#>   Ferralsols          0
#>   Fluvisols           0
#>   Gleysols            0
#>   Gypsisols           0
#>   Histosols           0
#>   Kastanozems         0
#>   Leptosols           0
#>   Lixisols            0
#>   Luvisols            0
#>   Nitisols            0
#>   Phaeozems           0
#>   Planosols           0
#>   Plinthosols         0
#>   Podzols             0
#>   Retisols            0
#>   Solonchaks          0
#>   Solonetz            0
#>   Stagnosols          0
#>   Technosols          0
#>   Umbrisols           0
#>   Vertisols           1
```

## 3. Confusion at the qualifier level

Beyond the RSG,
[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
returns the principal- and supplementary-qualifier lists. The benchmark
can be deepened to track agreement on the **qualifier prefix** of the
soil name (the strongest principal that fired). For the canonical
fixtures:

``` r

qualifier_prefix <- vapply(names(expected), function(code) {
  fx <- fixfns[[code]]()
  res <- classify_wrb2022(fx, on_missing = "silent")
  if (length(res$qualifiers$principal) == 0) NA_character_
  else res$qualifiers$principal[1]
}, character(1))

knitr::kable(
  data.frame(fixture          = names(qualifier_prefix),
             principal_prefix = qualifier_prefix),
  caption = "Most-specific principal qualifier per canonical fixture."
)
```

|     | fixture | principal_prefix |
|:----|:--------|:-----------------|
| HS  | HS      | Floatic          |
| AT  | AT      | Hortic           |
| TC  | TC      | Mollic           |
| CR  | CR      | Cambic           |
| LP  | LP      | Lithic           |
| SN  | SN      | Albic            |
| VR  | VR      | Protocalcic      |
| SC  | SC      | Sodic            |
| GL  | GL      | Haplic           |
| AN  | AN      | Vitric           |
| PZ  | PZ      | Albic            |
| PT  | PT      | Plinthic         |
| PL  | PL      | Albic            |
| ST  | ST      | Albic            |
| NT  | NT      | Luvic            |
| FR  | FR      | Geric            |
| CH  | CH      | Vermic           |
| KS  | KS      | Hypocalcic       |
| PH  | PH      | Cambic           |
| UM  | UM      | Cambic           |
| DU  | DU      | Duric            |
| GY  | GY      | Gypsic           |
| CL  | CL      | Calcic           |
| RT  | RT      | Albic            |
| AC  | AC      | Albic            |
| LX  | LX      | Albic            |
| AL  | AL      | Hyperalic        |
| LV  | LV      | Albic            |
| CM  | CM      | Eutric           |
| AR  | AR      | Protic           |
| FL  | FL      | Haplic           |

Most-specific principal qualifier per canonical fixture. {.table}

## 4. Benchmark protocol against WoSIS (run-once, paper-grade)

The mini-benchmark above proves the package’s internal consistency. The
paper-grade benchmark runs the same flow against ISRIC WoSIS, a global
archive of ~118 000 georeferenced pedon descriptions with WRB 2014 /
2022 classifications.

The protocol is intentionally separated from the package itself (no live
download in vignettes) so it can be re-run at a controlled date. The
driver script is shipped at `inst/benchmarks/run_wosis_benchmark.R`.
Skeleton:

``` r

library(soilKey)

# 1. Pull a snapshot of WoSIS profiles via the WoSIS API.
profiles <- read_wosis_profiles(
  url     = "https://wosis.isric.org/api/v3/profiles?format=json",
  page_size = 500L
)

# 2. Build a PedonRecord per profile.
pedons <- lapply(profiles, build_pedon_from_wosis)

# 3. Classify each through the v0.9.3 key.
classifications <- lapply(pedons, classify_wrb2022, on_missing = "silent")

# 4. Compare against the WoSIS-recorded RSG.
bench <- mapply(function(c, p) {
  data.frame(
    profile_id  = p$site$id,
    target_rsg  = p$site$wosis_rsg,
    assigned    = c$rsg_or_order,
    grade       = c$evidence_grade,
    match       = c$rsg_or_order == p$site$wosis_rsg
  )
}, classifications, pedons, SIMPLIFY = FALSE)
bench <- do.call(rbind, bench)

# 5. Headline numbers.
list(
  n              = nrow(bench),
  top1           = mean(bench$match, na.rm = TRUE),
  indeterminate  = mean(is.na(bench$assigned)),
  pct_grade_A    = mean(bench$grade == "A"),
  by_rsg         = table(bench$target_rsg, bench$assigned)
)
```

The same flow extends naturally to:

- **WoSIS subsets by region** – e.g. South America only, or Brazilian
  profiles only (most relevant for the SiBCS cross-system check).
- **WoSIS profiles missing one or more attributes** – exercise the
  gap-filling pipeline (vignette 05) and report the agreement uplift
  from `predicted_spectra`.
- **WoSIS profiles labelled with v2014 vs v2022 RSGs** – the benchmark
  separates them so `soilKey`’s v2022-only key is not penalised when
  WoSIS used a deprecated RSG name.

## 5. Reporting standards

Each benchmark run produces a versioned report at
`inst/benchmarks/reports/wosis_<DATE>.md` containing:

- Snapshot date and WoSIS subset used.
- Top-1 / Top-3 agreement, with 95% CI from a non-parametric bootstrap.
- Confusion matrix (RSG-level) and qualifier-prefix agreement.
- Per-RSG agreement and the most common confused pair for each.
- Evidence-grade distribution.
- Notable disagreements with diagnostic-level traces, for QA.

The same script is used for the periodic regression run that ships with
each minor `soilKey` release.

## 6. Offline canonical-fixture run (release-time sanity check)

The package ships an offline benchmark driver alongside the WoSIS one —
`run_canonical_benchmark()` in `inst/benchmarks/run_wosis_benchmark.R` —
that loops over the 31 canonical fixtures under `inst/extdata/`, runs
all three keys, compares to the known target encoded in the filename and
the cross-system correspondence table (Schad 2023 Annex Table 1; SiBCS
5ª ed. Annex A), and writes a versioned report to
`inst/benchmarks/reports/canonical_<DATE>.md`.

Sourcing the file and running it from the package root produces real,
reproducible numbers without any network call:

``` r

source(system.file("benchmarks", "run_wosis_benchmark.R",
                    package = "soilKey"))
bench <- run_canonical_benchmark()
```

The most recent canonical run (committed under
`inst/benchmarks/reports/`) achieved:

| System     |   n | match | top-1 |
|:-----------|----:|------:|------:|
| WRB 2022   |  31 |    31 | 1.000 |
| SiBCS 5    |  20 |    20 | 1.000 |
| USDA ST 13 |  31 |    31 | 1.000 |

The canonical run is the release-time CI check that the key + qualifier
system + cross-system mapping all hold up; the WoSIS run is the
paper-grade external validation.

## Summary

`soilKey`’s benchmarking layer is intentionally minimal:

1.  The mini-benchmark on canonical fixtures (this vignette) is fully
    reproducible inside the rendered HTML and acts as a CI check that
    the key + qualifier system holds 100% top-1 agreement on profiles
    constructed to match each RSG.
2.  The offline `run_canonical_benchmark()` driver runs all three
    systems against multi-valued cross-system targets and writes a
    versioned report — invoked once per release, network-free.
3.  The full WoSIS benchmark is run-once-per-release, off-line for the
    paper, with the driver script and reports versioned under
    `inst/benchmarks/`.
4.  All three layers report by the same metrics so comparison across
    releases is direct.

This closes the methodological loop opened in the architecture document:
the deterministic key, the side modules (VLM, spatial, OSSL), and the
benchmark are integrated without any of them being able to overrule the
others.

## 7. v0.9.27 – per-page retry + graceful degradation

The ISRIC WoSIS GraphQL endpoint occasionally returns *“canceling
statement due to statement timeout”* under load — we observed this
consistently after ~40 profiles per session in May 2026. A naïve
`read_wosis_profiles_graphql()` would abort the whole pull and surface a
hard error; that is unfriendly when 30+ profiles already came back fine.

`v0.9.27` adds a **per-page retry with exponential backoff** (1 s, 2 s,
4 s, 8 s; up to 4 attempts) plus **graceful degradation**: once at least
one page has succeeded, transient failures on later pages return the
partial pull rather than aborting. This makes the live WoSIS benchmark
robust enough to run from CI and from casual user sessions without
surprise crashes.

``` r

source(system.file("benchmarks", "run_wosis_benchmark.R",
                    package = "soilKey"))
profs <- read_wosis_profiles_graphql(
  continent = "South America",
  n_max     = 100L,
  page_size = 10L,
  verbose   = TRUE
)
length(profs)
#> [1] 40   # 100 was requested but server timed out at offset = 40;
#>          # the partial pull was returned cleanly.
```

## 8. v0.9.30 – bundled WoSIS sample for offline / CI testing

For tests, CI, and casual users who want to exercise the WRB benchmark
path without depending on ISRIC server availability, `v0.9.30` ships a
frozen 40-profile snapshot under `inst/extdata/wosis_sa_sample.rds` (49
KB). Helper:

``` r

sample <- load_wosis_sample()
length(sample$pedons)
#> [1] 40

sample$pulled_on
#> [1] "2026-05-03"

# Classify offline:
classify_wrb2022(sample$pedons[[1]])$rsg_or_order
```

The bundled snapshot is for **reproducible tests, not current
ground-truth claims**. For up-to-date paper-grade benchmarks, callers
should still use `run_wosis_benchmark_graphql()` against the live
endpoint.

For the **multi-level USDA Soil Taxonomy benchmark** (Order / Suborder /
Great Group / Subgroup) on the KSSL gpkg + NASIS sqlite join, see
vignette `v08_kssl_nasis_multilevel`.
