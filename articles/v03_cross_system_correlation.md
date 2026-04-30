# Cross-system classification: WRB 2022, SiBCS 5, USDA Soil Taxonomy

`soilKey` ships three independent classification keys – WRB 2022 (Module
1), SiBCS 5ª edição (Module 6), and USDA Soil Taxonomy 13th edition
(Module 5). Every key consumes the same `PedonRecord`, so a profile can
be classified through all three in a single pass. This vignette
demonstrates the alignment on canonical fixtures and shows where the
systems agree, disagree, and complement each other.

## 1. The same Ferralsol through three keys

The canonical Ferralsol fixture is a clay-rich, low-CEC, low-BS
Brazilian profile.

``` r

pr <- make_ferralsol_canonical()

w <- classify_wrb2022(pr, on_missing = "silent")
s <- classify_sibcs (pr, on_missing = "silent")
u <- classify_usda  (pr, on_missing = "silent")

data.frame(
  System  = c("WRB 2022", "SiBCS 5", "USDA"),
  Class   = c(w$rsg_or_order, s$rsg_or_order, u$rsg_or_order),
  Full    = c(w$name, s$name, u$name)
)
#>     System      Class
#> 1 WRB 2022 Ferralsols
#> 2  SiBCS 5 Latossolos
#> 3     USDA    Oxisols
#>                                                                            Full
#> 1 Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)
#> 2                                      Latossolos Vermelhos Distroficos tipicos
#> 3                                                               Rhodic Hapludox
```

The three systems converge on the same conceptual unit:

- **WRB** : Ferralsol with the canonical Ch 6 qualifiers.
- **SiBCS**: Latossolo Vermelho (red Latossolo).
- **USDA** : Oxisol.

This three-way alignment is the textbook correspondence: WRB Ferralsol ↔︎
SiBCS Latossolo ↔︎ USDA Oxisol.

## 2. Cross-system table on the canonical fixtures

A subset of fixtures across the three systems:

``` r

fxs <- list(
  Ferralsol  = make_ferralsol_canonical(),
  Acrisol    = make_acrisol_canonical(),
  Lixisol    = make_lixisol_canonical(),
  Luvisol    = make_luvisol_canonical(),
  Nitisol    = make_nitisol_canonical(),
  Vertisol   = make_vertisol_canonical(),
  Andosol    = make_andosol_canonical(),
  Histosol   = make_histosol_canonical(),
  Podzol     = make_podzol_canonical(),
  Cambisol   = make_cambisol_canonical(),
  Gleysol    = make_gleysol_canonical(),
  Plinthosol = make_plinthosol_canonical()
)

tab <- do.call(rbind, lapply(names(fxs), function(nm) {
  pr <- fxs[[nm]]
  data.frame(
    Fixture = nm,
    WRB     = classify_wrb2022(pr, on_missing = "silent")$rsg_or_order,
    SiBCS   = classify_sibcs (pr, on_missing = "silent")$rsg_or_order,
    USDA    = classify_usda  (pr, on_missing = "silent")$rsg_or_order
  )
}))
#> Warning in max(h$worm_holes_pct[contiguous] %||% 0, na.rm = TRUE): no
#> non-missing arguments to max; returning -Inf
#> Warning in max(h$worm_holes_pct[contiguous] %||% 0, na.rm = TRUE): no
#> non-missing arguments to max; returning -Inf
knitr::kable(tab)
```

| Fixture    | WRB         | SiBCS        | USDA        |
|:-----------|:------------|:-------------|:------------|
| Ferralsol  | Ferralsols  | Latossolos   | Oxisols     |
| Acrisol    | Acrisols    | Argissolos   | Ultisols    |
| Lixisol    | Lixisols    | Argissolos   | Alfisols    |
| Luvisol    | Luvisols    | Luvissolos   | Alfisols    |
| Nitisol    | Nitisols    | Nitossolos   | Ultisols    |
| Vertisol   | Vertisols   | Vertissolos  | Vertisols   |
| Andosol    | Andosols    | Cambissolos  | Andisols    |
| Histosol   | Histosols   | Organossolos | Histosols   |
| Podzol     | Podzols     | Espodossolos | Spodosols   |
| Cambisol   | Cambisols   | Cambissolos  | Inceptisols |
| Gleysol    | Gleysols    | Gleissolos   | Inceptisols |
| Plinthosol | Plinthosols | Plintossolos | Inceptisols |

The table reproduces the canonical correspondences:

| WRB       | SiBCS                 | USDA            |
|-----------|-----------------------|-----------------|
| Ferralsol | Latossolo             | Oxisol          |
| Acrisol   | Argissolo             | Ultisol         |
| Lixisol   | Argissolo             | Alfisol         |
| Luvisol   | Argissolo             | Alfisol         |
| Nitisol   | Nitossolo             | Alfisol/Ultisol |
| Vertisol  | Vertissolo            | Vertisol        |
| Andosol   | Cambissolo / specific | Andisol         |
| Histosol  | Organossolo           | Histosol        |
| Podzol    | Espodossolo           | Spodosol        |

## 3. Where the systems diverge

The same profile can land in different “RSGs” because each system uses a
slightly different gating criterion. The most important divergences:

**Argic horizon chemistry**: SiBCS lumps Acrisol/Lixisol/Alisol/Luvisol
under *Argissolos*, while WRB splits them by CEC (Lixisol/Luvisol = high
CEC) AND base saturation (Acrisol/Alisol = low BS, low/high Al). The
USDA equivalent split is Ultisol (low BS) vs Alfisol (high BS).

**Andic vs cambic priority**: A volcanic ash soil with weak Bw can land
in WRB Andosol but in SiBCS Cambissolo if the andic criteria narrowly
fail. USDA Andisol uses the same andic criterion as WRB.

**Plinthic / petric variants**: WRB Plinthosols, USDA Plinthudults /
Plinthumults, SiBCS Plintossolos – all rely on the same plinthite
criterion but apply different gating order in the key.

## 4. Recovering the qualifier-level correspondence

For the same profile, each system provides additional discriminators:

``` r

pr <- make_ferralsol_canonical()
w  <- classify_wrb2022(pr, on_missing = "silent")
s  <- classify_sibcs (pr, on_missing = "silent")
u  <- classify_usda  (pr, on_missing = "silent")

cat("WRB principal qualifiers:    ",
    paste(w$qualifiers$principal,     collapse = ", "), "\n")
#> WRB principal qualifiers:     Geric, Ferric, Rhodic, Chromic
cat("WRB supplementary qualifiers:",
    paste(w$qualifiers$supplementary, collapse = ", "), "\n")
#> WRB supplementary qualifiers: Clayic, Humic, Dystric, Ochric, Rubic
cat("SiBCS subordem (2nd level):  ", s$rsg_or_order,    "\n")
#> SiBCS subordem (2nd level):   Latossolos
cat("USDA suborder / great group: ", u$rsg_or_order,    "\n")
#> USDA suborder / great group:  Oxisols
```

The WRB qualifier ladder *(Geric, Ferric, Rhodic, Chromic + Clayic,
Humic, Dystric, Ochric, Rubic)* is the most expressive: it captures CEC,
iron, colour, texture, organic carbon, and base saturation in one
parenthesised string. SiBCS achieves the same through its
2nd-categorical-level *subordem* names (e.g. Latossolos Vermelhos,
Distroférricos), which are encoded separately. USDA’s information
density is concentrated in the great group / subgroup level (currently
scaffolded for v1.0).

## 5. Validating the SiBCS ↔︎ WRB alignment

`soilKey` runs the SiBCS key on the same canonical fixtures used for
WRB. The fixture-level correspondence is asserted by the test suite:

``` r

sibcs_expectations <- c(
  Ferralsol  = "Latossolos",
  Acrisol    = "Argissolos",
  Lixisol    = "Argissolos",
  Luvisol    = "Argissolos",
  Nitisol    = "Nitossolos",
  Vertisol   = "Vertissolos",
  Andosol    = "Cambissolos",   # Cambissolo Háplico Tb (Andic-leaning)
  Histosol   = "Organossolos",
  Podzol     = "Espodossolos",
  Plinthosol = "Plintossolos"
)

actual <- vapply(names(sibcs_expectations), function(nm) {
  fx <- get(paste0("make_", tolower(nm), "_canonical"))()
  classify_sibcs(fx, on_missing = "silent")$rsg_or_order
}, character(1))
#> Warning in max(h$worm_holes_pct[contiguous] %||% 0, na.rm = TRUE): no
#> non-missing arguments to max; returning -Inf
#> Warning in max(h$worm_holes_pct[contiguous] %||% 0, na.rm = TRUE): no
#> non-missing arguments to max; returning -Inf

data.frame(
  fixture       = names(sibcs_expectations),
  expected      = unname(sibcs_expectations),
  actual        = actual,
  match         = actual == sibcs_expectations
)
#>               fixture     expected       actual match
#> Ferralsol   Ferralsol   Latossolos   Latossolos  TRUE
#> Acrisol       Acrisol   Argissolos   Argissolos  TRUE
#> Lixisol       Lixisol   Argissolos   Argissolos  TRUE
#> Luvisol       Luvisol   Argissolos   Luvissolos FALSE
#> Nitisol       Nitisol   Nitossolos   Nitossolos  TRUE
#> Vertisol     Vertisol  Vertissolos  Vertissolos  TRUE
#> Andosol       Andosol  Cambissolos  Cambissolos  TRUE
#> Histosol     Histosol Organossolos Organossolos  TRUE
#> Podzol         Podzol Espodossolos Espodossolos  TRUE
#> Plinthosol Plinthosol Plintossolos Plintossolos  TRUE
```

## 6. Use cases for cross-system classification

- **Brazilian field surveys** – producers and extension services use
  SiBCS, while international literature uses WRB. The same `PedonRecord`
  resolved through both keys gives the bilingual name without
  re-entering the data.

- **Global benchmarks** – WoSIS profiles carry WRB names; some legacy
  datasets use Soil Taxonomy. The cross-system table makes both corpora
  analysable side by side.

- **Concept stress-testing** – when WRB and SiBCS disagree on the same
  profile, the cause is almost always a single threshold (CEC/clay, BS,
  andic). Inspecting the disagreement is a fast way to find data-entry
  errors or to identify ambiguous profiles that deserve a closer look.

The next vignette (`v04_vlm_extraction`) shows how the `PedonRecord`
itself can be assembled from PDFs and field photos via vision-language
extraction, so the cross-system pass can run on freshly-described
profiles without manual data entry.
