# End-to-end WRB 2022 classification with full names

This vignette walks the full WRB 2022 (4th edition) classification flow
on the canonical Ferralsol fixture, end to end – from a raw
`PedonRecord` to the complete name, built by the rules of Chapter 2.2,
with both **principal** and **supplementary** qualifiers in the
canonical parenthesised form.

The Ferralsol fixture represents a typical Brazilian *Latossolo*
(gneiss-derived, Mata Atlântica). Since v0.9.217 the qualifier lists are
those of WRB 2022 Chapter 4 and the name is built by the rules of
Chapter 2.2, and
[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
resolves it to:

    Geric Rhodic Ferralsol (Clayic, Epic, Eutric, Ferric, Humic)

We will inspect each step that produces that name.

## 1. Build the pedon

The canonical fixture exposes a published-quality profile. Use it as the
working pedon.

``` r

pr <- make_ferralsol_canonical()
pr
#> 
#> ── PedonRecord ──
#> 
#> Site: id=FR-canonical-01 | (-22.5000, -43.7000) | BR | 2024-03-10 | on gneiss
#> Horizons (5):
#> 1) A 0-15 cm clay=50.0 silt=15.0 sand=35.0 CEC=8.0 pH=4.8 OC=2.0
#> 2) AB 15-35 cm clay=52.0 silt=14.0 sand=34.0 CEC=6.5 pH=4.7 OC=1.2
#> 3) BA 35-65 cm clay=55.0 silt=10.0 sand=35.0 CEC=5.5 pH=4.7 OC=0.6
#> 4) Bw1 65-130 cm clay=60.0 silt=8.0 sand=32.0 CEC=5.0 pH=4.8 OC=0.3
#> 5) Bw2 130-200 cm clay=60.0 silt=8.0 sand=32.0 CEC=4.8 pH=4.9 OC=0.2
```

A glance at the horizons and chemistry:

``` r

knitr::kable(
  pr$horizons[, .(top_cm, bottom_cm, designation,
                  munsell_hue_moist, munsell_value_moist, munsell_chroma_moist,
                  clay_pct, oc_pct, cec_cmol, bs_pct,
                  ph_h2o, ph_kcl)]
)
```

| top_cm | bottom_cm | designation | munsell_hue_moist | munsell_value_moist | munsell_chroma_moist | clay_pct | oc_pct | cec_cmol | bs_pct | ph_h2o | ph_kcl |
|---:|---:|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| 0 | 15 | A | 2.5YR | 3 | 4 | 50 | 2.0 | 8.0 | 24 | 4.8 | 4.0 |
| 15 | 35 | AB | 2.5YR | 3 | 4 | 52 | 1.2 | 6.5 | 17 | 4.7 | 4.0 |
| 35 | 65 | BA | 2.5YR | 3 | 6 | 55 | 0.6 | 5.5 | 14 | 4.7 | 4.0 |
| 65 | 130 | Bw1 | 2.5YR | 4 | 6 | 60 | 0.3 | 5.0 | 13 | 4.8 | 4.1 |
| 130 | 200 | Bw2 | 2.5YR | 4 | 6 | 60 | 0.2 | 4.8 | 13 | 4.9 | 4.2 |

Notable features for WRB key:

- Clay 50-60 % throughout, hue 2.5YR, low chroma -\> ferralic-like with
  reddish tint;
- OC 2.0 % at the surface, decreasing with depth;
- Low CEC (5-8 cmol+/kg fine earth) and low BS (13-24 %);
- pH H2O 4.7-4.9, pH KCl 4.0-4.2 -\> delta pH negative (no Posic).

## 2. Run the WRB key

[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
walks the canonical Ch 4 RSG order (HS -\> AT -\> … -\> RG) and returns
the first RSG whose tier-2 gate is satisfied.

``` r

res <- classify_wrb2022(pr)
res
#> 
#> ── ClassificationResult (WRB 2022) ──
#> 
#> Name: Geric Rhodic Ferralsol (Clayic, Epic, Eutric, Ferric, Humic)
#> RSG/Order: Ferralsols
#> Qualifiers: Geric, Rhodic, Clayic, Epic, Eutric, Ferric, Humic, FALSE, NA,
#> gibbsite_clay_fraction_pct, TRUE, FALSE, TRUE, FALSE, structure_type,
#> structure_grade, clay_films_amount, fe_dcb_pct, fe_ox_pct, FALSE,
#> p_mehlich3_mg_kg, FALSE, redoximorphic_features_pct, FALSE,
#> redoximorphic_features_pct, FALSE, FALSE, top_cm, bottom_cm, FALSE, FALSE,
#> FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, al_ox_pct,
#> fe_ox_pct, phosphate_retention_pct, volcanic_glass_pct, FALSE, NA,
#> rupture_resistance, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE,
#> TRUE, TRUE, NA, bioturbation_density, bulk_density_g_cm3, particles_630um_pct,
#> FALSE, FALSE, NA, water_saturation_days, redoximorphic_features_pct, FALSE, NA,
#> visible black carbon, % of exposed area (not in the schema), FALSE, NA,
#> saprolite_pct, FALSE, rock_origin, FALSE, FALSE, artefacts_pct,
#> geomembrane_present, technic_hardmaterial_pct, NA, artefacts_pct, NA,
#> contamination_type, NA, layer_origin, artefacts_pct
#> Evidence grade: A
#> 
#> ── Ambiguities
#> - TC: Indeterminate -- missing 3 attribute(s): artefacts_pct,
#> geomembrane_present, technic_hardmaterial_pct
#> - CR: Indeterminate -- missing 1 attribute(s): permafrost_temp_C
#> - VR: Indeterminate -- missing 1 attribute(s): slickensides
#> - SC: Indeterminate -- missing 1 attribute(s): ec_dS_m
#> - PZ: Indeterminate -- missing 2 attribute(s): al_ox_pct, fe_ox_pct
#> - PT: Indeterminate -- missing 1 attribute(s): plinthite_pct
#> - ST: Indeterminate -- missing 1 attribute(s): redoximorphic_features_pct
#> - NT: Indeterminate -- missing 5 attribute(s): structure_type, structure_grade,
#> clay_films_amount, fe_dcb_pct, fe_ox_pct
#> 
#> ── Missing data that would refine result
#> artefacts_pct, geomembrane_present, technic_hardmaterial_pct,
#> permafrost_temp_C, slickensides, ec_dS_m, redoximorphic_features_pct,
#> al_ox_pct, fe_ox_pct, phosphate_retention_pct, volcanic_glass_pct,
#> plinthite_pct, structure_type, structure_grade, clay_films_amount, fe_dcb_pct
#> 
#> ── Warnings
#> ! 16 distinct attribute(s) missing across the key trace -- see $missing_data
#> 
#> ── Key trace
#> (16 RSGs tested before assignment)
#> 1. HS Histosols -- failed
#> 2. AT Anthrosols -- failed
#> 3. TC Technosols -- NA (3 attrs missing)
#> 4. CR Cryosols -- NA (1 attrs missing)
#> 5. LP Leptosols -- failed
#> 6. SN Solonetz -- failed
#> 7. VR Vertisols -- NA (1 attrs missing)
#> 8. SC Solonchaks -- NA (1 attrs missing)
#> 9. GL Gleysols -- failed (1 attrs missing)
#> 10. AN Andosols -- failed (4 attrs missing)
#> 11. PZ Podzols -- NA (2 attrs missing)
#> 12. PT Plinthosols -- NA (1 attrs missing)
#> 13. PL Planosols -- failed
#> 14. ST Stagnosols -- NA (1 attrs missing)
#> 15. NT Nitisols -- NA (5 attrs missing)
#> 16. FR Ferralsols -- PASSED
```

The returned `ClassificationResult` carries:

- `$rsg_or_order` – the assigned Reference Soil Group (here,
  **Ferralsols**);
- `$name` – the full name with principal and supplementary qualifiers;
- `$qualifiers` – the resolved principal and supplementary lists, plus
  the per-qualifier trace;
- `$trace` – the RSG-by-RSG key trace, including which RSGs failed
  before the assignment;
- `$evidence_grade` – A through E (or NA when no horizon carries a soil
  property), summarising the provenance of the classification.

## 3. Inspect the principal qualifier resolution

After the RSG is assigned, the resolver walks the RSG’s list of
principal qualifiers from WRB 2022 Chapter 4, in its ranked order. For
Ferralsols (p. 110) the list is
`Ferritic, Gibbsic, Rhodic/Xanthic, Geric, Nitic, Pretic, Gleyic, Stagnic, Profundihumic, Mollic/Umbric, Acric/Lixic, Skeletic, Haplic`.
Qualifiers separated by a slash are mutually exclusive, or the later
ones are redundant, so only the first that applies is used.

``` r

qres <- resolve_wrb_qualifiers(pr, "FR")
qres$principal
#> [1] "Geric"  "Rhodic"
```

Principal qualifiers are written right to left: “the uppermost qualifier
in the list is placed closest to the name of the RSG” (Chapter 2.2).
Rhodic ranks above Geric, so the name reads *Geric Rhodic Ferralsol*.
Their ranks in the list:

    #>   Qualifier Rank in Chapter 4
    #> 1     Geric                 4
    #> 2    Rhodic                 3

The `trace` slot keeps every Ch 4 principal that was tested, including
those that failed. Useful for diagnostic debugging:

``` r

trace_df <- do.call(
  rbind,
  lapply(names(qres$trace), function(q) {
    t <- qres$trace[[q]]
    data.frame(qualifier = q,
               passed    = if (is.null(t$passed)) NA else t$passed,
               note      = t$note %||% "")
  })
)
head(trace_df, 12)
#>        qualifier passed note
#> 1       Ferritic  FALSE     
#> 2        Gibbsic     NA     
#> 3         Rhodic   TRUE     
#> 4        Xanthic  FALSE     
#> 5          Geric   TRUE     
#> 6          Nitic  FALSE     
#> 7         Pretic  FALSE     
#> 8         Gleyic  FALSE     
#> 9        Stagnic  FALSE     
#> 10 Profundihumic  FALSE     
#> 11        Mollic  FALSE     
#> 12        Umbric  FALSE
```

## 4. Inspect the supplementary qualifier resolution

Supplementary qualifiers go in brackets after the RSG name. They are not
ranked: the texture qualifiers come first, the others “in the order of
the alphabet”, by qualifier rather than subqualifier (Chapter 2.2).

``` r

qres$supplementary
#> [1] "Clayic" "Epic"   "Eutric" "Ferric" "Humic"
```

Ochric does not appear: the Ferralsol list has `Humic/Ochric`, and Humic
applies first. Each tag, with the source the resolver cites:

    #>   Qualifier
    #> 1    Clayic
    #> 2      Epic
    #> 3    Eutric
    #> 4    Ferric
    #> 5     Humic
    #>                                                                                                                                                                                                                               Reference
    #> 1                                                                                                                                                                                                               WRB (2022) Ch 5, Clayic
    #> 2                                                                                                                                                                                                                 WRB (2022) Ch 5, Epic
    #> 3 WRB (2022) Ch 5 (Dystric p.130-131, Eutric p.131-132): exchangeable Al vs exchangeable bases over 20-100 cm; Dystric = Al > bases in >= half, Eutric = bases >= Al in the major part. Not base saturation (that was WRB 2014). Eutric
    #> 4                                                                                                                                                                                                                       WRB (2022) Ch 5
    #> 5                                                                                                                                                                                                                WRB (2022) Ch 5, Humic

## 5. Compose the name

[`format_wrb_name()`](https://hugomachadorodrigues.github.io/soilKey/reference/format_wrb_name.md)
glues principal and supplementary into the canonical form:

``` r

format_wrb_name(
  rsg_name      = "Ferralsols",
  principal     = qres$principal,
  supplementary = qres$supplementary
)
#> [1] "Geric Rhodic Ferralsol (Clayic, Epic, Eutric, Ferric, Humic)"
```

This is exactly the string returned by `classify_wrb2022()$name`.

## 6. Redundant and sibling qualifiers

“Qualifiers conveying redundant information are not added … For example,
Eutric is not added if the Calcaric qualifier applies” (Chapter 2.2).
The resolver drops Eutric, and its subqualifiers such as Hypereutric,
when Calcaric or Dolomitic applies:

``` r

soilKey:::.drop_redundant_qualifiers(c("Calcaric", "Hypereutric"),
                                     c("Calcaric", "Hypereutric"))
#> [1] "Calcaric"
```

When several qualifiers of one family pass (Hypercalcic, Calcic,
Protocalcic), only the most specific is kept, in the principal and in
the supplementary list:

``` r

str(soilKey:::.wrb_qualifier_families)
#> List of 7
#>  $ salinity: chr [1:2] "Hypersalic" "Salic"
#>  $ calcic  : chr [1:3] "Hypercalcic" "Calcic" "Protocalcic"
#>  $ gypsic  : chr [1:3] "Hypergypsic" "Gypsic" "Protogypsic"
#>  $ vertic  : chr [1:2] "Vertic" "Protovertic"
#>  $ eutric  : chr [1:2] "Hypereutric" "Eutric"
#>  $ dystric : chr [1:2] "Hyperdystric" "Dystric"
#>  $ alic    : chr [1:2] "Hyperalic" "Alic"
soilKey:::.suppress_qualifier_siblings(
  c("Mollic", "Hypercalcic", "Calcic", "Protocalcic", "Cambic")
)
#> [1] "Mollic"      "Hypercalcic" "Cambic"
```

## 7. Evidence grade

[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
reports an `evidence_grade` summarising the provenance of every
attribute used in the classification. **A** means every value was
measured; **B** to **E** mark spectra-predicted, prior-inferred,
VLM-extracted and user-assumed values; **NA** means no horizon carries a
soil property at all.

``` r

res$evidence_grade
#> [1] "A"
```

The Ferralsol fixture has all measured values, so the grade is **A**.
The `v01_getting_started` vignette shows how `pedon$add_measurement()`
with `source = "extracted_vlm"` or `source = "predicted_spectra"` lowers
the grade – so you always know how robust the classification is.

## 8. Render a self-contained pedologist-facing report

The
[`report()`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md)
generic takes a `ClassificationResult` (or a list of them, or a
`PedonRecord` – in which case all three keys are run automatically) and
writes a single-file HTML report with inline CSS, no external network
requests, suitable for archiving with a laudo. The PDF path goes through
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
and requires a working LaTeX engine.

``` r

# Pass the three classifications as a list:
results <- list(
  classify_wrb2022(pr),
  classify_sibcs(pr, include_familia = TRUE),
  classify_usda(pr)
)
report(results, file = "perfil_ferralsol.html", pedon = pr)

# Or pass the pedon directly and let report() run the three keys:
report(pr, file = "perfil_ferralsol.html")

# Same content as PDF (requires LaTeX):
# report(pr, file = "perfil_ferralsol.pdf")
```

The HTML output includes: the cross-system summary, the full key trace
per system, qualifiers (principal + supplementary), evidence grade,
ambiguities, missing data, the horizons table, and the per-source
provenance summary. `ClassificationResult$report(file)` is the
R6-method-style equivalent and delegates to the same code.

## 9. Tier-3 strict mode for borderline pedons

By default the per-RSG numerical gates apply soilKey’s regionally
calibrated thresholds. For pedons that sit close to an RSG boundary,
[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
accepts a `strict` argument that strengthens seven Tier-3 gates toward
the canonical WRB 2022 Chapter 4 intent (e.g. the Vertisol
overlying-clay floor rises from 30 % to 35 %, the Chernozem
base-saturation floor from 50 % to 80 %).

``` r

# A profile with 32 % clay above a vertic horizon: a Vertisol under the
# default gate, but below the 35 % strict floor.
classify_wrb2022(pr, strict = FALSE)$rsg_or_order  # default
classify_wrb2022(pr, strict = TRUE)$rsg_or_order   # Tier-3 strict
```

`strict = FALSE` (the default) is fully backward compatible – every
canonical fixture classifies identically with and without it. Strict
mode only changes genuinely borderline profiles, which makes it a useful
sensitivity probe: if a classification is stable across both modes, the
assignment is robust. The toggle can also be set globally with
`options(soilKey.rsg_strict = TRUE)` (this is what the Shiny Pro app’s
Settings tab does), and each RSG gate records the effective threshold in
its `DiagnosticResult` evidence.

## Summary

    #> WRB 2022 name : Geric Rhodic Ferralsol (Clayic, Epic, Eutric, Ferric, Humic)
    #> Assigned RSG  : Ferralsols
    #> Principal     : Geric, Rhodic
    #> Supplementary : Clayic, Epic, Eutric, Ferric, Humic
    #> Evidence grade: A

The `v03_cross_system_correlation` vignette runs the same profile
through the Brazilian SiBCS and the USDA Soil Taxonomy keys and shows
the alignment between the three classifications.
