# soilKey -- recognised horizon attribute names (v0.9.192)

This is the authoritative list of the analytical and morphological attribute
names that soilKey reads from a horizon table (CSV upload in soilKey Pro, or a
`data.frame`/`data.table` passed to the R API). Every classification key reads
these columns; anything else is preserved but ignored by the keys.

## Naming conventions

* Canonical names are **lower-case `snake_case`**, with an explicit unit/method
  suffix (`_cm`, `_pct`, `_cmol`, `_g_cm3`, `_mg_kg`, `_dS_m`, `_C`).
* Column-name recognition is **case-insensitive** and **separator-insensitive**:
  `pH_H2O`, `PH.H2O`, `ph h2o` and `pH-H2O` are all recognised as `ph_h2o`.
* A small table of **common abbreviations** is also accepted (see the *aliases*
  column below): e.g. `Clay` -> `clay_pct`, `SOC` -> `oc_pct`, `CEC` -> `cec_cmol`.
* An exact canonical name always wins and is never overwritten; unrecognised
  columns are kept verbatim and simply not read by the keys.
* Units are as listed. Percentages are by mass unless noted `(vol)`; exchangeable
  cations and CEC are in cmol(+)/kg (= meq/100 g).

> Tip (R): `soilKey::horizon_column_spec()` returns the canonical names and types
> programmatically; `soilKey::ensure_horizon_schema(df)` normalises a table to them.

## Geometry & boundaries

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `top_cm` | cm | numeric | Upper depth of the horizon (from the soil surface). | `depth_top`, `top`, `top_depth`, `upper_depth` |
| `bottom_cm` | cm | numeric | Lower depth of the horizon. | `bottom`, `bottom_depth`, `depth_bottom`, `lower_depth` |
| `designation` | - | character | Horizon designation / symbol (e.g. A, Bt, Bw, E, C, R). | `horizon`, `horizon_designation`, `hzn`, `hzn_desgn` |
| `boundary_distinctness` | - | character | Distinctness of the lower boundary (abrupt / clear / gradual / diffuse). | - |
| `boundary_topography` | - | character | Topography of the lower boundary (smooth / wavy / irregular / broken). | - |

## Color (Munsell)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `munsell_hue_moist` | Munsell | character | Munsell hue, moist (e.g. "7.5YR", "10R", "N"). | - |
| `munsell_value_moist` | Munsell | numeric | Munsell value, moist (0-10). | - |
| `munsell_chroma_moist` | Munsell | numeric | Munsell chroma, moist. | - |
| `munsell_hue_dry` | Munsell | character | Munsell hue, dry. | - |
| `munsell_value_dry` | Munsell | numeric | Munsell value, dry (0-10). | - |
| `munsell_chroma_dry` | Munsell | numeric | Munsell chroma, dry. | - |

## Structure & consistence

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `structure_grade` | - | character | Grade of structure (structureless / weak / moderate / strong). | - |
| `structure_size` | - | character | Size class of structural units (very fine / fine / medium / coarse / very coarse). | - |
| `structure_type` | - | character | Type of structure (granular / blocky / prismatic / platy / single-grain / massive). | - |
| `consistence_moist` | - | character | Consistence when moist (loose / very friable / friable / firm / very firm / extremely firm). | - |
| `consistence_wet` | - | character | Consistence when wet (stickiness and plasticity). | - |
| `clay_films_amount` | - | character | Abundance of clay coatings / films (none / few / common / many). | - |
| `clay_films_strength` | - | character | Distinctness / thickness of clay films (faint / distinct / prominent). | - |
| `coarse_fragments_pct` | % (vol) | numeric | Coarse fragments (> 2 mm) by volume. | `cf`, `coarse_fragments`, `gravel`, `rock_fragments` |

## Texture

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `clay_pct` | % (mass) | numeric | Clay content of the fine earth (< 2 um). | `clay` |
| `silt_pct` | % (mass) | numeric | Silt content of the fine earth. | `silt` |
| `sand_pct` | % (mass) | numeric | Sand content of the fine earth. | `sand` |

## Acidity

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `ph_h2o` | - | numeric | pH measured in water (1:2.5 or as reported). soilKey's default pH. | `ph`, `ph_h20`, `ph_water`, `phw` |
| `ph_kcl` | - | numeric | pH measured in 1 M KCl. | - |
| `ph_cacl2` | - | numeric | pH measured in 0.01 M CaCl2. | - |

## Organics

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `oc_pct` | % (mass) | numeric | Organic carbon of the fine earth. | `c_org`, `corg`, `oc`, `org_c`, `organic_carbon`, `soc` |
| `n_total_pct` | % (mass) | numeric | Total nitrogen. | `n_tot`, `n_total`, `nitrogen`, `tn`, `total_n` |

## Exchange complex

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `cec_cmol` | cmol(+)/kg | numeric | Cation exchange capacity (of the fine earth). | `cec`, `ctc` |
| `ecec_cmol` | cmol(+)/kg | numeric | Effective CEC (sum of exchangeable bases + exchangeable Al). | `ecec` |
| `bs_pct` | % | numeric | Base saturation (SiBCS V%). | `base_saturation`, `bs`, `v_pct` |
| `al_sat_pct` | % | numeric | Aluminium saturation (SiBCS m%). | `al_sat`, `al_saturation`, `m_pct` |
| `ca_cmol` | cmol(+)/kg | numeric | Exchangeable calcium. | `ca`, `exch_ca` |
| `mg_cmol` | cmol(+)/kg | numeric | Exchangeable magnesium. | `exch_mg`, `mg` |
| `k_cmol` | cmol(+)/kg | numeric | Exchangeable potassium. | `exch_k`, `k` |
| `na_cmol` | cmol(+)/kg | numeric | Exchangeable sodium. | `exch_na`, `na` |
| `al_cmol` | cmol(+)/kg | numeric | Exchangeable (KCl) aluminium. | `al`, `exch_al` |

## Carbonates / sulphates

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `caco3_pct` | % (mass) | numeric | Calcium carbonate equivalent. | `caco3`, `carbonate`, `carbonates` |
| `secondary_carbonates_pct` | % | numeric | identifiable SECONDARY carbonates by volume (soft masses / pseudomycelia / pendents / nodules). The morphological OR-path of the calcic horizon: WRB 2022 3.1.4 protocalcic / USDA KST >= 5% by-volume secondary carbonates, beside the +5%-vs-underlying enrichment | - |
| `caso4_pct` | % (mass) | numeric | Gypsum (calcium sulphate). | `caso4`, `gypsum` |

## Iron / aluminum oxides

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `fe_dcb_pct` | % Fe | numeric | Dithionite-citrate-bicarbonate extractable Fe (free/pedogenic iron oxides). | - |
| `fe_ox_pct` | % Fe | numeric | Acid ammonium-oxalate extractable Fe (active/amorphous iron; andic). | - |
| `al_ox_pct` | % Al | numeric | Acid ammonium-oxalate extractable Al (active/amorphous aluminium; andic). | - |
| `si_ox_pct` | % Si | numeric | Acid ammonium-oxalate extractable Si (andic properties). | - |

## Physical

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `bulk_density_g_cm3` | g/cm3 | numeric | Bulk density of the fine earth. | `bd`, `bulk_density`, `db` |
| `water_content_33kpa` | % (water) | numeric | Gravimetric water content at 33 kPa (field capacity). | - |
| `water_content_1500kpa` | % (water) | numeric | Gravimetric water content at 1500 kPa (permanent wilting point). | - |

## Salinity, redoximorphism, vertic

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `ec_dS_m` | dS/m | numeric | Electrical conductivity (saturated paste, 25 C). | `conductivity`, `ec`, `ece` |
| `plinthite_pct` | % | numeric | volume % of plinthite (Fe-rich nodules / mottles) | - |
| `redoximorphic_features_pct` | % | numeric | volume % of Fe/Mn redox features | - |
| `slickensides` | - | character | absent / few / common / many / continuous | - |

## Technic, duric

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `artefacts_pct` | % | numeric | volume % of human artefacts (for Technosols) | - |
| `geomembrane_present` | - | logical | WRB 2022 Ch 5 Technosols: continuous geomembrane within 100 cm | - |
| `technic_hardmaterial_pct` | % | numeric | WRB 2022 Ch 5 Technosols: % concrete/asphalt/mine spoil at surface (>= 95% within 5 cm) | - |
| `duripan_pct` | % | numeric | volume % of Si-cemented duripan (for Durisols) | - |

## Completing WRB Ch 3.1 / 3.2 / 3.3 coverage

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `cementation_class` | - | character | 'none' / 'weakly' / 'moderately' / 'strongly' / 'indurated' (for petric variants) | - |
| `p_mehlich3_mg_kg` | mg/kg | numeric | plant-available P (anthric / hortic / pretic / ornithogenic) | - |
| `worm_holes_pct` | % | numeric | volume % of worm holes / casts / coprolites (chernic / vermic / arenicolic) | - |
| `water_dispersible_clay_pct` | % | numeric | WDC / total clay (Ferralsols 'activic' check) | - |
| `sulfidic_s_pct` | % | numeric | inorganic sulfidic S (hypersulfidic / hyposulfidic / thionic) | - |
| `volcanic_glass_pct` | % | numeric | % volcanic glass in 0.02-2 mm fraction (vitric / tephric) | - |
| `phosphate_retention_pct` | % | numeric | P retention (vitric / andic threshold) | - |
| `artefacts_industrial_pct` | % | numeric | subset of artefacts: industrial-process glasses, slag, ash (organotechnic) | - |
| `artefacts_urbic_pct` | % | numeric | subset of artefacts: rubble / refuse from human settlements (Technosols urbic) | - |
| `rock_origin` | - | character | 'fluviatile' / 'marine' / 'lacustrine' / 'aeolian' / 'colluvial' / 'pyroclastic' / NA | - |
| `permafrost_temp_C` | deg C | numeric | mean annual soil temp at this depth (gelic / cryic) | - |
| `cracks_width_cm` | cm | numeric | width of shrink-swell cracks when soil dry (vertic horizon / shrink_swell_cracks) | - |
| `cracks_depth_cm` | cm | numeric | depth to which the crack extends from the surface | - |
| `polygonal_cracks_spacing_cm` | cm | numeric | avg horizontal spacing of polygonal cracks (takyric properties) | - |
| `desert_pavement_pct` | % | numeric | % surface coverage by coarse fragments (yermic properties) | - |
| `varnish_pct` | % | numeric | % of coarse fragments with desert varnish (yermic) | - |
| `ventifact_pct` | % | numeric | % of coarse fragments wind-shaped (yermic) | - |
| `vesicular_pores` | - | character | 'absent' / 'few' / 'common' / 'many' (yermic) | - |
| `rupture_resistance` | - | character | 'loose' / 'soft' / 'slightly hard' / 'hard' / 'very hard' / 'extremely hard' | - |
| `plasticity` | - | character | 'non-plastic' / 'slightly plastic' / 'moderately plastic' / 'very plastic' | - |
| `al_kcl_cmol` | cmol(+)/kg | numeric | KCl-extractable Al (Alisols criterion) | - |
| `layer_origin` | - | character | 'aeolic' / 'fluvic' / 'solimovic' / 'tephric' / 'organic' etc (for material gating) | - |

## SiBCS pendentes (von Post, Ki/Kr, COLE, sulfuric attack)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `fiber_content_rubbed_pct` | % | numeric | SiBCS Cap 14: % fibras apos esfregamento (Saprico < 17, Hemico 17-40, Fibrico >= 40) | - |
| `fiber_content_unrubbed_pct` | % | numeric | SiBCS Cap 14: % fibras antes do esfregamento (auxiliar) | - |
| `von_post_index` | - | integer | Indice de decomposicao von Post 1924 (H1-H10): H1-H4 Fibrico / H5-H6 Hemico / H7-H10 Saprico | - |
| `cole_value` | - | numeric | Coefficient of Linear Extensibility (1500 kPa moist -> oven dry); SiBCS retratil >= 0.06 | - |
| `sio2_sulfuric_pct` | % | numeric | SiO2 por ataque sulfurico-NaOH (Embrapa Manual de Metodos), para Ki/Kr | - |
| `al2o3_sulfuric_pct` | % | numeric | Al2O3 por ataque sulfurico, para Ki = (SiO2/60.08)/(Al2O3/101.96) molar | - |
| `fe2o3_sulfuric_pct` | % | numeric | Fe2O3 por ataque sulfurico, para Kr = SiO2/(Al2O3+Fe2O3) molar (Latossolos Acriferricos) | - |

## SiBCS Cap 18 mineralogia da fracao areia

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `sand_mica_pct` | % | numeric | SiBCS Cap 18 p 286: % volume de micas na fracao areia (>= 15% -> Familia "micacea") | - |
| `sand_amphibole_pct` | % | numeric | SiBCS Cap 18 p 286: % volume de anfibolios (>= 15% -> Familia "anfibolitica") | - |
| `sand_feldspar_pct` | % | numeric | SiBCS Cap 18 p 286: % volume de feldspatos (>= 15% -> Familia "feldspatica") | - |
| `sand_mineralogy` | - | character | SiBCS Cap 18 p 286 fallback: 'micacea' / 'anfibolitica' / 'feldspatica' / 'quartzosa' / NA (atalho qualitativo) | - |

## SiBCS Cap 18 Organossolos

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `woody_fragments_pct` | % | numeric | SiBCS Cap 18 p 288: % volume de galhos/troncos >= 2 cm em horizontes organicos (Organossolos lenhosos / muito lenhosos / extremamente lenhosos) | - |

## Tier-3 schema fields for WRB SQ qualifiers

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `surface_crust_type` | - | character | WRB Ch 5 (Biocrustic / Pelocrustic / Evapocrustic / Puffic): biological / clay / evaporite / puffed crust morphology | - |
| `bioturbation_density` | - | character | WRB Ch 5 (Arenicolic / Isopteric): faunal burrow density (none / few / common / many) -- proxy for invertebrate-driven mixing | - |
| `cordic_horizon` | - | logical | WRB Ch 5 (Cordic): presence of cordic horizon (cemented but not duripan/petrocalcic) | - |
| `microrelief_form` | - | character | WRB Ch 5 (Dorsic / Gilgaic): microrelief form (gilgai / dorsal-ridge / hummocky / smooth) | - |
| `weathering_stage` | - | character | WRB Ch 5 (Saprolithic / Naramic / Lapiadic): weathering stage of parent material (fresh / moderately weathered / saprolite / completely weathered) | - |
| `salt_crust_pattern` | - | character | WRB Ch 5 (Naramic): salt crust morphology (efflorescent / crusty / hardpan) | - |
| `contamination_type` | - | character | WRB Ch 5 (Immissic): pollution / contamination class (heavy_metals / hydrocarbons / atmospheric_immission / NA) | - |
| `stratification_pattern` | - | character | WRB Ch 5 (Litholinic / Raptic): stratification description (continuous / interrupted / lithologic_break / NA) | - |
| `aeolian_morphology` | - | character | WRB Ch 5 (Nechic): aeolian / loess deposition pattern (loess / dune / sandsheet / NA) | - |
| `mottle_morphology` | - | character | WRB Ch 5 (Mochipic): mottle pattern qualitative (mochi / banded / patchy / NA) | - |
| `surface_puff_layer` | - | logical | WRB Ch 5 (Kalaic / Puffic): seasonal puffed surface layer (TRUE / FALSE / NA) | - |
| `thixotropic_index` | - | numeric | WRB Ch 5 (Thixotropic): thixotropic-behaviour index (0-100) from slurry test | - |
| `saprolite_pct` | % | numeric | WRB Ch 5 (Saprolithic): % by volume of in-situ weathered saprolite material | - |
| `water_regime_pattern` | - | character | WRB Ch 5 (Uterquic): bidirectional / single / aquic regime classification | - |

## Fields that unlock schema-blocked predicates

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `water_content_1500kpa_undried` | - | numeric | 1500 kPa water retention on UNDRIED samples; Vitrands/Vitrandic need < 30% undried beside < 15% air-dried (KST 13ed Ch 6) | - |
| `particles_002_2mm_pct` | % | numeric | % of the FINE-EARTH fraction in the 0.02-2.0 mm size class; Vitrandic subgroup crit 2 needs >= 30% (KST 13ed Ch 9) | - |
| `cracks_top_cm` | cm | numeric | depth (cm) of the UPPER boundary of shrink-swell cracks; Vertic subgroup needs cracks within 125 cm (KST 13ed) | - |
| `incubation_ph` | - | numeric | pH after the WRB 8-week aerobic incubation test; hypersulfidic drops < 4, hyposulfidic stays >= 4 (WRB 2022 Ch 3.3.8/3.3.9) | - |
| `ice_pct` | % | numeric | volume % ice (related to whole soil); WRB 2022 Glacic needs >= 75% (Ch 5) | - |
| `water_saturation_days` | days | numeric | cumulative days/year water-saturated; WRB 2022 Mochipic needs >= 300 days (Ch 5) | - |
| `particles_630um_pct` | % | numeric | % particles >= 630 um; WRB 2022 Isopteric needs < 5% (Ch 5) | - |
| `jarosite_present` | - | logical | jarosite mineral present; WRB 2022 Aceric requires it beside pH 3.5-5 (Ch 5) | - |

---

_Generated from `horizon_column_spec()` and `.horizon_name_aliases()` in soilKey v0.9.192._

