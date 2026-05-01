# End-to-end WRB 2022 classification with Ch 6 names

This vignette walks the full WRB 2022 (4th edition) classification flow
on the canonical Ferralsol fixture, end to end – from a raw
`PedonRecord` to the complete Chapter 6 name with both **principal** and
**supplementary** qualifiers in the canonical parenthesised form.

The Ferralsol fixture represents a typical Brazilian *Latossolo*
(gneiss-derived, Mata Atlântica). After v0.9.3,
[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
resolves it to:

    Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)

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
#> Name: Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric,
#> Rubic)
#> RSG/Order: Ferralsols
#> Qualifiers: Geric, Ferric, Rhodic, Chromic, Clayic, Humic, Dystric, Ochric,
#> Rubic, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
#> al_ox_pct, fe_ox_pct, phosphate_retention_pct, volcanic_glass_pct, FALSE,
#> volcanic_glass_pct, FALSE, FALSE, plinthite_pct, FALSE, plinthite_pct, FALSE,
#> plinthite_pct, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE,
#> redoximorphic_features_pct, FALSE, redoximorphic_features_pct, FALSE, FALSE,
#> p_mehlich3_mg_kg, FALSE, p_mehlich3_mg_kg, FALSE, FALSE, FALSE, FALSE, FALSE,
#> FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
#> TRUE, FALSE
#> Evidence grade: A
#> 
#> ── Ambiguities
#> - TC: Indeterminate -- missing 3 attribute(s): artefacts_pct,
#> geomembrane_present, technic_hardmaterial_pct
#> - CR: Indeterminate -- missing 1 attribute(s): permafrost_temp_C
#> - VR: Indeterminate -- missing 1 attribute(s): slickensides
#> - SC: Indeterminate -- missing 1 attribute(s): ec_dS_m
#> - AN: Indeterminate -- missing 4 attribute(s): al_ox_pct, fe_ox_pct,
#> phosphate_retention_pct, volcanic_glass_pct
#> - PZ: Indeterminate -- missing 2 attribute(s): al_ox_pct, fe_ox_pct
#> - PT: Indeterminate -- missing 1 attribute(s): plinthite_pct
#> - ST: Indeterminate -- missing 1 attribute(s): redoximorphic_features_pct
#> 
#> ── Missing data that would refine result
#> artefacts_pct, geomembrane_present, technic_hardmaterial_pct,
#> permafrost_temp_C, slickensides, ec_dS_m, redoximorphic_features_pct,
#> al_ox_pct, fe_ox_pct, phosphate_retention_pct, volcanic_glass_pct,
#> plinthite_pct
#> 
#> ── Warnings
#> ! 12 distinct attribute(s) missing across the key trace -- see $missing_data
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
#> 10. AN Andosols -- NA (4 attrs missing)
#> 11. PZ Podzols -- NA (2 attrs missing)
#> 12. PT Plinthosols -- NA (1 attrs missing)
#> 13. PL Planosols -- failed
#> 14. ST Stagnosols -- NA (1 attrs missing)
#> 15. NT Nitisols -- failed
#> 16. FR Ferralsols -- PASSED
```

The returned `ClassificationResult` carries:

- `$rsg_or_order` – the assigned Reference Soil Group (here,
  **Ferralsols**);
- `$name` – the full Ch 6 name with principal and supplementary
  qualifiers;
- `$qualifiers` – the resolved principal and supplementary lists, plus
  the per-qualifier trace;
- `$trace` – the RSG-by-RSG key trace, including which RSGs failed
  before the assignment;
- `$evidence_grade` – A through D, summarising the provenance of the
  classification.

## 3. Inspect the principal qualifier resolution

After the RSG is assigned, the resolver walks the canonical Ch 4
principal-qualifier list for that RSG (e.g. for Ferralsols:
`Vetic, Posic, Acric, Lixic, Geric, Hyperdystric, ...`) and tests each
against the pedon.

``` r

qres <- resolve_wrb_qualifiers(pr, "FR")
qres$principal
#> [1] "Geric"   "Ferric"  "Rhodic"  "Chromic"
```

The four principals that pass the Ferralsol fixture, in canonical Ch 4
order:

    #>   Qualifier
    #> 1     Geric
    #> 2    Ferric
    #> 3    Rhodic
    #> 4   Chromic
    #>                                                                                                                                               Why
    #> 1 ECEC = sum of bases + Al_KCl <= 1.5 cmol+/kg fine earth in some layer of the upper 100 cm. Layer 4 (Bw1, top = 65 cm) has ECEC = 1.18 cmol+/kg.
    #> 2                                                                         Iron-rich subsoil (Fe_dcb >= 5%); fe_dcb_pct hits 8-9% in this fixture.
    #> 3                                   Hue 2.5YR moist, value < 4 in 25-150 cm. Bw1 has value = 4 (failing in some layers but BA satisfies value 3).
    #> 4                                                              Hue redder than 7.5YR + chroma > 4 in 25-150 cm subsoil. Bw1 chroma = 6 satisfies.

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
#>       qualifier passed note
#> 1         Vetic  FALSE     
#> 2         Posic  FALSE     
#> 3         Acric  FALSE     
#> 4         Lixic  FALSE     
#> 5         Geric   TRUE     
#> 6  Hyperdystric  FALSE     
#> 7   Hypereutric  FALSE     
#> 8        Histic  FALSE     
#> 9         Folic  FALSE     
#> 10        Andic  FALSE     
#> 11       Vitric  FALSE     
#> 12      Sombric  FALSE
```

## 4. Inspect the supplementary qualifier resolution

Supplementary qualifiers are the parenthesised tags in the WRB Ch 6
name. They refine the soil description with texture / chemistry / colour
information that is not strong enough to be a principal but still
informative.

``` r

qres$supplementary
#> [1] "Clayic"  "Humic"   "Dystric" "Ochric"  "Rubic"
```

What each tag captures for this Ferralsol:

    #>   Qualifier
    #> 1    Clayic
    #> 2     Humic
    #> 3   Dystric
    #> 4    Ochric
    #> 5     Rubic
    #>                                                                                                Why
    #> 1 Clay >= 60 % over a layer thicker than 30 cm in the upper 100 cm; Bw1 has clay = 60% over 65 cm.
    #> 2                                 Weighted OC >= 1 % in the upper 50 cm; weighted OC ~ 1.1 % here.
    #> 3                       BS < 50 % throughout 20-100 cm; BS = 13-24 % across all four upper layers.
    #> 4                      OC >= 0.2 % in upper 10 cm + no mollic + no umbric; surface has OC = 2.0 %.
    #> 5         Hue <= 5YR + chroma >= 4 in upper 100 cm (less strict than Rhodic). 2.5YR / 6 satisfies.

## 5. Compose the Ch 6 name

[`format_wrb_name()`](https://hugomachadorodrigues.github.io/soilKey/reference/format_wrb_name.md)
glues principal and supplementary into the canonical form:

``` r

format_wrb_name(
  rsg_name      = "Ferralsols",
  principal     = qres$principal,
  supplementary = qres$supplementary
)
#> [1] "Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)"
```

This is exactly the string returned by `classify_wrb2022()$name`.

## 6. Family suppression

When several qualifiers from the same WRB family (e.g. Calcic /
Hypocalcic / Protocalcic) pass the same RSG, only the most-specific
sibling appears in the name. The suppression is applied **after** all
candidates are evaluated and works on both the principal and
supplementary lists.

The internal table:

``` r

str(soilKey:::.wrb_qualifier_families)
#> List of 10
#>  $ salinity: chr [1:3] "Hypersalic" "Salic" "Hyposalic"
#>  $ sodicity: chr [1:3] "Hypersodic" "Sodic" "Hyposodic"
#>  $ calcic  : chr [1:4] "Hypercalcic" "Calcic" "Hypocalcic" "Protocalcic"
#>  $ gypsic  : chr [1:4] "Hypergypsic" "Gypsic" "Hypogypsic" "Protogypsic"
#>  $ vertic  : chr [1:2] "Vertic" "Protovertic"
#>  $ albic   : chr [1:2] "Hyperalbic" "Albic"
#>  $ skeletic: chr [1:2] "Hyperskeletic" "Skeletic"
#>  $ eutric  : chr [1:2] "Hypereutric" "Eutric"
#>  $ dystric : chr [1:2] "Hyperdystric" "Dystric"
#>  $ alic    : chr [1:2] "Hyperalic" "Alic"
```

A worked example: a synthetic Calcisol that satisfies Calcic,
Hypocalcic, and Protocalcic simultaneously will collapse to just
**Calcic**.

``` r

soilKey:::.suppress_qualifier_siblings(
  c("Mollic", "Calcic", "Hypocalcic", "Protocalcic", "Cambic")
)
#> [1] "Mollic" "Calcic" "Cambic"
```

## 7. Evidence grade

[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
reports an `evidence_grade` summarising the provenance of every
attribute used in the classification. **A** means every used value was
lab-measured; **D** means the result rests on VLM-extracted or
user-assumed values.

``` r

res$evidence_grade
#> [1] "A"
```

The Ferralsol fixture has all measured values, so the grade is **A**.
The `v01_getting_started` vignette shows how `pedon$add_measurement()`
with `source = "extracted_vlm"` or `source = "predicted_spectra"` lowers
the grade – so you always know how robust the classification is.

## 6. Render a self-contained pedologist-facing report

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

## Summary

    #> WRB 2022 name : Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)
    #> Assigned RSG  : Ferralsols
    #> Principal     : Geric, Ferric, Rhodic, Chromic
    #> Supplementary : Clayic, Humic, Dystric, Ochric, Rubic
    #> Evidence grade: A

The `v03_cross_system_correlation` vignette runs the same profile
through the Brazilian SiBCS and the USDA Soil Taxonomy keys and shows
the alignment between the three classifications.
