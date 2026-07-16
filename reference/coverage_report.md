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
    "wrb_horizons", "wrb_properties", "wrb_materials", "wrb_rsg", "sibcs"),
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
  WRB 2022 against
  [`wrb2022_canonical`](https://hugomachadorodrigues.github.io/soilKey/reference/wrb2022_canonical.md)
  and the canonical diagnostic reference
  (`inst/extdata/canonical/wrb2022_diagnostics.csv`):
  `"wrb_qualifiers"`, `"wrb_horizons"` (40 diagnostic horizons),
  `"wrb_properties"` (17 diagnostic properties), `"wrb_materials"` (19
  diagnostic materials), and `"wrb_rsg"` (32 Reference Soil Groups,
  diffed against `inst/rules/wrb2022/key.yaml`). For the qualifier and
  diagnostic axes "covered" means the mapped function exists *and* is a
  genuine implementation (not an unconditional `passed = NA` stub); the
  inert ones are returned in `$stubs`. `"sibcs"` has no external
  canonical class list, so it honestly reports registered class counts
  per level only (no percentage).

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
#> ℹ 2049 / 2715 canonical subgroups registered (75.5%); 666 missing.
#>        group canonical_n covered_n missing_n   pct
#>     Gelisols         129       129         0 100.0
#>    Histosols          75        75         0 100.0
#>    Spodosols         121       121         0 100.0
#>      Oxisols         213       190        23  89.2
#>     Andisols         218       189        29  86.7
#>    Vertisols         158       136        22  86.1
#>  Inceptisols         349       262        87  75.1
#>     Ultisols         215       161        54  74.9
#>     Alfisols         352       247       105  70.2
#>    Mollisols         376       257       119  68.4
#>     Entisols         239       157        82  65.7
#>    Aridisols         270       125       145  46.3
cov$overall
#>   system    level canonical_n registered_n covered_n missing_n  pct
#> 1   usda subgroup        2715         2245      2049       666 75.5
head(cov$missing)
#> [1] "Abruptic Argiaquolls"             "Abruptic Argicryolls"            
#> [3] "Abruptic Argiduridic Durixerolls" "Abruptic Argidurids"             
#> [5] "Abruptic Argiudolls"              "Abruptic Durixeralfs"            
```
