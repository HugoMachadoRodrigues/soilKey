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
  system = c("usda_subgroup", "wrb_qualifiers"),
  write = FALSE,
  report_dir = NULL
)
```

## Arguments

- system:

  Which axis to measure: `"usda_subgroup"` (default) or
  `"wrb_qualifiers"`.

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
`$missing` (canonical names not registered) and `$extra` (registered
names absent from the canonical set). A compact summary is printed as a
side effect.

## Examples

``` r
cov <- coverage_report("usda_subgroup")
#> 
#> ── Coverage: usda subgroup ──
#> 
#> ℹ 1921 / 2715 canonical subgroups registered (70.8%); 794 missing.
#>        group canonical_n covered_n missing_n   pct
#>     Gelisols         129       129         0 100.0
#>    Histosols          75        75         0 100.0
#>    Spodosols         121       121         0 100.0
#>     Andisols         218       183        35  83.9
#>     Ultisols         215       158        57  73.5
#>      Oxisols         213       146        67  68.5
#>  Inceptisols         349       238       111  68.2
#>    Mollisols         376       256       120  68.1
#>     Alfisols         352       238       114  67.6
#>     Entisols         239       156        83  65.3
#>    Vertisols         158        98        60  62.0
#>    Aridisols         270       123       147  45.6
cov$overall
#>   system    level canonical_n registered_n covered_n missing_n  pct
#> 1   usda subgroup        2715         2117      1921       794 70.8
head(cov$missing)
#> [1] "Abruptic Argiaquolls"             "Abruptic Argicryolls"            
#> [3] "Abruptic Argiduridic Durixerolls" "Abruptic Argidurids"             
#> [5] "Abruptic Argiudolls"              "Abruptic Durixeralfs"            
```
