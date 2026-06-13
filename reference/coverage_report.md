# Honest taxonomic-completeness report

Measures, by NAME, exactly which canonical taxa/qualifiers the package's
deterministic rule base registers, replacing hand-maintained coverage
claims with an auditable, reproducible diff. For `"usda_subgroup"` the
canonical reference is the Soil Taxonomy 13th-edition subgroup set from
[`kst13_codes`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_codes.md);
for `"wrb_qualifiers"` it is the WRB 2022 principal + supplementary
qualifier set from
[`wrb2022_canonical`](https://hugomachadorodrigues.github.io/soilKey/reference/wrb2022_canonical.md).

## Usage

``` r
coverage_report(
  system = c("usda_subgroup", "usda_great_group", "usda_suborder", "wrb_qualifiers",
    "sibcs"),
  write = FALSE,
  report_dir = NULL
)
```

## Arguments

- system:

  Which axis to measure. USDA taxon levels against the Soil Taxonomy
  13th-edition code set
  ([`kst13_codes`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_codes.md)):
  `"usda_subgroup"` (default), `"usda_great_group"`, `"usda_suborder"`.
  WRB 2022 qualifiers against
  [`wrb2022_canonical`](https://hugomachadorodrigues.github.io/soilKey/reference/wrb2022_canonical.md):
  `"wrb_qualifiers"` – here "covered" means the `qual_*` function exists
  *and* is a genuine implementation (not an unconditional `passed = NA`
  stub), and the inert ones are returned in `$stubs`. `"sibcs"` has no
  external canonical class list, so it honestly reports registered class
  counts per level only (no percentage).

- write:

  If `TRUE`, also write a Markdown summary to `report_dir`. Default
  `FALSE`.

- report_dir:

  Directory for the Markdown report when `write = TRUE`. Defaults to
  `inst/benchmarks/reports` inside the installed package.

## Value

Invisibly, a list with `$overall` (one-row data frame: `system`,
`level`, `canonical_n`, `registered_n`, `covered_n`, `missing_n`,
`pct`), `$by_group` (per order, or per principal/supplementary),
`$missing` (canonical names not registered), `$extra` (registered names
absent from the canonical set), and – for `"wrb_qualifiers"` – `$stubs`
(functions that exist but are inert). A compact summary is printed as a
side effect.

## Examples

``` r
cov <- coverage_report("usda_subgroup")
#> 
#> ── Coverage: usda subgroup ──
#> 
#> ℹ 2003 / 2715 canonical subgroups registered (73.8%); 712 missing.
#>        group canonical_n covered_n missing_n   pct
#>     Gelisols         129       129         0 100.0
#>    Histosols          75        75         0 100.0
#>    Spodosols         121       121         0 100.0
#>     Andisols         218       188        30  86.2
#>      Oxisols         213       182        31  85.4
#>    Vertisols         158       134        24  84.8
#>     Ultisols         215       158        57  73.5
#>     Alfisols         352       242       110  68.8
#>  Inceptisols         349       238       111  68.2
#>    Mollisols         376       256       120  68.1
#>     Entisols         239       156        83  65.3
#>    Aridisols         270       124       146  45.9
cov$overall
#>   system    level canonical_n registered_n covered_n missing_n  pct
#> 1   usda subgroup        2715         2199      2003       712 73.8
head(cov$missing)
#> [1] "Abruptic Argiaquolls"             "Abruptic Argicryolls"            
#> [3] "Abruptic Argiduridic Durixerolls" "Abruptic Argidurids"             
#> [5] "Abruptic Argiudolls"              "Abruptic Durixeralfs"            
```
