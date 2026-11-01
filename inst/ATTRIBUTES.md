# soilKey -- recognised horizon attribute names (v0.9.328)

This is the authoritative list of the analytical and morphological attribute
names that soilKey reads from a horizon table (CSV upload in soilKey Pro, or a
`data.frame`/`data.table` passed to the R API). Every classification key reads
these columns; anything else is preserved but ignored by the keys. The site
properties (`PedonRecord$site`) follow the horizon attributes, under
[Site properties](#site-properties).

Since v0.9.294, `PedonRecord$new()` and `read_pedon_csv()` warn about horizon
columns and site properties that are not recognised, with the closest
recognised names; `unrecognised_attributes()` returns the same list as a
table, and `options(soilKey.warn_unrecognised = FALSE)` turns the warning off.
The loaders (BDsolos, FEBR, WoSIS, NatSoil, ...) keep extra columns on purpose
and do not warn.

## Naming conventions

* Canonical names are **lower-case `snake_case`**, with an explicit unit/method
  suffix (`_cm`, `_pct`, `_cmol`, `_g_cm3`, `_mg_kg`, `_dS_m`, `_C`).
* Column-name recognition is **case-insensitive** and **separator-insensitive**:
  `pH_H2O`, `PH.H2O`, `ph h2o` and `pH-H2O` are all recognised as `ph_h2o`.
* A small table of **common abbreviations** is also accepted (see the *aliases*
  column below): e.g. `Clay` -> `clay_pct`, `SOC` -> `oc_pct`, `CEC` -> `cec_cmol`.
* An exact canonical name always wins and is never overwritten; unrecognised
  columns are kept verbatim, not read by the keys, and named in a warning.
* Units are as listed. Percentages are by mass unless noted `(vol)`; exchangeable
  cations and CEC are in cmol(+)/kg (= meq/100 g).

> Tip (R): `soilKey::horizon_attribute_spec()` returns this table (names, types,
> sections, units, descriptions and aliases); `soilKey::horizon_column_spec()`
> the canonical names and types; `soilKey::ensure_horizon_schema(df)` normalises
> a table to them.

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
| `pressure_faces` | - | character | Abundance of pressure faces on the surfaces of the soil aggregates (none / few / common / many): shiny surfaces of aggregates pressed against each other, without the striations of slickensides and of the colour of the matrix (WRB 2022 Annex 1, Ch 8.4.14). The WRB 2022 nitic horizon (Ch 3.1.22) asks for pressure faces on 25 % or more of the aggregate surfaces, read as common or more. Clay coatings are recorded in clay_films_amount, not here. | - |
| `structure_type_secondary` | - | character | Type of the second-level structure into which the first-level structural units break (e.g. angular blocky breaking into polyhedral or flat-edged elements). Read only by the WRB 2022 nitic horizon (Ch 3.1.22, criterion 2.a); structure_type is the first level. | - |
| `structure_type_one_of` | - | character | The type of structure when the record names one of several, not known which, as words joined by 'or' (the older New Zealand term nut is granular or polyhedral, NZ Soil Description Handbook 2024, Note 12.1: 'granular or polyhedral'); structure_type is then left empty, so the keys that do not read this attribute read the type as not recorded. The NZSC key reads a criterion under each type: it holds when it holds for every one, fails when it fails for every one, and is undecided otherwise. | - |

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
| `ph_naf` | - | numeric | pH in 1 M NaF (the sodium fluoride test of Fieldes and Perrott, 1966). WRB 2022 Ch 3.2.3: a pH in NaF of 9.5 or more indicates allophane or organo-aluminium complexes. andic_properties() reads Alox + 1/2 Feox >= 2 % with a pH NaF >= 9.5 as andic where bulk density is not measured (a proxy, recorded in the evidence). The USDA keys do not read it. | - |

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
| `secondary_carbonates_form` | - | character | Form of the secondary carbonates, FAO (2006): pseudomycelia, coatings (on ped faces), soft concretions, hard concretions, powdery, masses. HSCS 2024 subtype mészlepedékes (whitish coatings, pseudomycelia, on ped faces). | - |

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
| `mottle_size_class` | - | character | Size class of the mottles, FAO (2006): very fine (< 2 mm), fine (2-6 mm), medium (6-20 mm), coarse (> 20 mm). HSCS 2024 iron accumulation horizon (vaskiválásos): large mottles over 20 mm. | - |
| `ec_max_dS_m` | dS/m | numeric | Upper bound of the electrical conductivity of the saturated paste, when only a more dilute extract was measured (ISIS: EC 1:2.5 x 12.5, for a saturation percentage of 20 % or more). Read only by the salic test, to rule a layer out; ec_dS_m wins when it is known. | - |

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
| `water_dispersible_clay_pct` | % | numeric | water-dispersible clay, % of the fine earth (a clay content, WRB 2022 Annex 2 Ch 9.4; not a share of the total clay); Ferralsols argic exception | - |
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

## The colours a horizon has beside its matrix colour

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `variegated_colors` | - | character | Munsell notations (moist) of the colours of a variegated horizon (coloracao variegada), which has no single matrix colour, separated by ";" ("10YR 5/3; 2.5YR 4/6"); SiBCS Plintossolos (Cap 3, p. 104) | - |
| `mottle_colors` | - | character | Munsell notations (moist) of the colours of the mottles (mosqueados) of the horizon, separated by ";" ("10YR 6/2; 5YR 5/8"); SiBCS Plintossolos (Cap 3, p. 104) | - |

## Canadian system (CSSC 3rd ed.)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `fe_pyro_pct` | % Fe | numeric | Sodium-pyrophosphate-extractable Fe (organically complexed iron); CSSC podzolic B, Bf, Bhf and Bh horizons. | - |
| `al_pyro_pct` | % Al | numeric | Sodium-pyrophosphate-extractable Al (organically complexed aluminium); CSSC podzolic B, Bf and Bhf horizons. | - |
| `mottle_contrast` | - | character | Contrast of the mottles with the matrix colour (faint / distinct / prominent); CSSC gleying, ASC mottled horizon. | - |

## Australian system (ASC 3rd ed.)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `field_texture` | - | character | Field texture grade (NCST Field Handbook codes: S, LS, CS, SL, L, ZL, SCL, CL, CLS, ZCL, LC, LMC, MC, MHC, HC; sand-size prefixes F/M/K and + or - accepted); ASC textural B, Arenosols, Kandosols and Vertosols. | - |
| `subplasticity` | - | character | Kneading test of clays (none / subplastic / strongly subplastic / superplastic); ASC Sodosols and Chromosols. | - |
| `carbonate_soft_pct` | % | numeric | Soft, finely divided pedogenic carbonate (soft segregations and the like), visual abundance; ASC carbonate classes (Calcarosol suborders). | - |
| `carbonate_hard_pct` | % | numeric | Hard pedogenic carbonate (nodules, concretions, calcrete fragments, carbonate-coated gravel), visual abundance; ASC carbonate classes (Calcarosol suborders). | - |
| `ec_1to5_dS_m` | dS/m | numeric | Electrical conductivity of a 1:5 soil:water extract; ASC 'highly saline' (EC > 2 dS/m: Hypersalic, Hypergypsic and Stratic suborders). ec_dS_m is the saturated paste. | - |
| `strength_dry` | - | character | Soil strength of the dry soil (Field Handbook classes: loose, very weak, weak, firm, very firm, strong, very strong, rigid); ASC Arenic Rudosols and the melanic horizon. | - |
| `strength_moist` | - | character | Soil strength of the moist soil, same classes as strength_dry; ASC Arenic and Lutic Rudosols. | - |
| `ped_size_max_mm` | mm | numeric | Upper limit of the size class of the most common peds (Field Handbook classes: 2, 5, 10, 20, 50 ... mm); ASC melanic horizon (10-20 mm or less). | - |
| `ferric_nodules_pct` | % | numeric | Ferruginous (and ferromanganiferous) nodules or concretions, visual abundance; ASC ferric horizon (more than 20 %), Sesqui-Nodular Tenosols. | - |
| `bauxitic_nodules_pct` | % | numeric | Bauxite (aluminous) nodules or concretions, visual abundance; ASC bauxitic horizon (more than 20 %), Sesqui-Nodular Tenosols. | - |
| `manganiferous_nodules_pct` | % | numeric | Black manganiferous nodules or concretions, mostly uncemented, visual abundance; ASC manganic horizon (more than 20 %), Manganic Arenosols. | - |
| `gypsum_visible_pct` | % | numeric | Visible gypsum (segregations, crystals) of apparently pedogenic origin, visual abundance; ASC gypsic horizon (more than 20 %, at least 0.1 m). | - |
| `halite_visible_pct` | % | numeric | Halite crystals, visual abundance; ASC Halic (more than 50 %) and Subhalic (less than 50 %) great groups of the Hypersalic suborders. | - |
| `gritty` | - | logical | A distinct sharp gritty feel from gritty coarse sand (0.2-2 mm) and fine gravel of angular or subangular quartz or other hard rock fragments; ASC Gritty great groups. Not recorded is read as absent. | - |
| `heavy_minerals_pct` | % | numeric | Visible heavy or opaque minerals, identifiable by wet panning in the field; ASC Gravic Arenosols (more than 3 %). Not recorded is read as absent. | - |
| `pan_type` | - | character | Kind of pan in the horizon (Field Handbook: red-brown hardpan, calcrete, ferricrete, alcrete, thin ironpan, silcrete, duripan, fragipan, densipan, organic pan, ortstein, other pan; 'none' when recorded absent); ASC Duric, Petroferric, Petrocalcic, Placic great groups. | - |
| `esp_pct` | % | numeric | Exchangeable sodium percentage as the laboratory reports it, read when exchangeable Na or the CEC is not given; ASC sodic. | - |
| `soil_water_status` | - | character | Moisture of the horizon when it was described (Field Handbook: dry / moderately moist / moist / wet); ASC Vertosols, whose cracks are not required in a solum described moist. | - |
| `effervescence_class` | - | character | Field reaction of the fine earth with 1 M HCl (none / slight / moderate / strong / very strong); ASC calcareous soil. | - |
| `gravel_pct` | % (vol) | numeric | Gravel-sized (2-60 mm) coarse fragments, visual abundance; ASC family gravel classes (glossary 'Gravel'). Without it the family reads coarse_fragments_pct. | - |
| `water_repellence` | - | character | Water repellence of the horizon, Field Handbook ethanol test (non-repellent / repellent / strongly repellent); ASC family water repellence of the surface soil. | - |

## New Zealand system (NZSC 4th ed.)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `naf_test` | - | character | Reactive-aluminium (NaF) field test on a moist sample (none, slight, moderate, strong; New Zealand codes N, L, M, H); NZSC allophanic soil material, podzolic-B horizon, Brown Soils. | - |
| `childs_test` | - | character | Childs' test (2,2'-bipyridine) for ferrous iron, positive or negative; NZSC Gley Soils (ironstone-pan route) and reductimorphic horizons. | - |
| `sensitivity_class` | - | character | Sensitivity class (smeary, greasy feel of a remoulded sample): insensitive, slightly sensitive, moderately sensitive, sensitive, strongly sensitive; NZSC allophanic soil material. | - |
| `fluidity_class` | - | character | Fluidity class: non-fluid, slightly fluid, moderately fluid, very fluid; NZSC Gley and Recent Soils (no moderately or very fluid horizon within 30 cm). | - |
| `stickiness` | - | character | Stickiness class: non-sticky, slightly sticky, moderately sticky, very sticky; NZSC allophanic soil material and Melanic Soils. | - |
| `packing_degree` | - | character | Degree of packing of the soil material (low, moderate, high, very high, extremely high); NZSC slowly permeable layer, with the ped size or the texture. | - |
| `ksat_mm_h` | mm/h | numeric | Measured vertical saturated hydraulic conductivity; NZSC slowly permeable layer (below 4 mm/h), which it decides when measured. | - |
| `penetration_resistance_kpa` | kPa | numeric | Soil penetration resistance, moist; NZSC densipan (more than 4,000 kPa) and Pan Podzol Soils (3,100 kPa or more). | - |
| `redox_segregations_location` | - | character | Where the high-chroma redox segregations (mottles) mainly are: within peds, matrix (of an apedal horizon), ped faces, voids or skeletans; NZSC perch-gley features (within peds or the matrix) and Groundwater-gley Podzol Soils (coatings on voids or skeletans). | - |
| `sulfate_s_pct` | % (mass) | numeric | Sulfate sulfur of the fine earth, dry mass; NZSC sulfuric soil material (0.05 % or more). | - |
| `roots_abundance` | - | character | Abundance of roots in the horizon (none, very few, few, common, many); NZSC Impeded Pumice Soils (a compact, brittle layer with no roots). | - |
| `maori_cultural_layer` | - | logical | TRUE for a horizon modified through Māori cultural practice, with additions of rock fragments, sand, shells, charcoal, ash, seaweed or similar (a cultural layer, as at midden sites); NZSC Māori Anthropic Soils. | - |
| `coatings_value_moist` | Munsell | numeric | Moist Munsell value of the clay or clay-organic coatings on ped faces; NZSC Solonetzic Semiarid Soils (value 4 or less); since 0.9.299 also the dark coatings of the Humose and Placic-humose Acid Gley and Podzolic Yellow Ultic Soils. | - |
| `sphagnum_fibres_pct` | % | numeric | Share of the organic fibres that are Sphagnum (moss peat); NZSC Sphagnic Fibric Organic Soils (more than 70 % to 60 cm). Recorded when met: not recorded reads as none. | - |
| `shelly_layer` | - | logical | TRUE for a layer of shells (shell beds, shelly sand); NZSC Calcareous Recent Gley Soils (a shelly layer within 60 cm). Recorded when met: not recorded reads as none. | - |
| `pore_coatings_hue_moist` | Munsell | character | Moist Munsell hue of the coatings on pores (not root linings), or of the gel-like masses bridging sand grains or coating coarse fragments; NZSC Acidic and Vitric-acidic Orthic Allophanic Soils (hue 7.5YR or redder, value 5 or less, chroma 3 or more). Recorded when met: not recorded reads as none. | - |
| `pore_coatings_value_moist` | Munsell | numeric | Moist Munsell value of the coatings on pores or gel-like masses (see pore_coatings_hue_moist). | - |
| `pore_coatings_chroma_moist` | Munsell | numeric | Moist Munsell chroma of the coatings on pores or gel-like masses (see pore_coatings_hue_moist). | - |
| `weathered_fragments_pct` | % | numeric | Share of the coarse rock fragments that are weathered enough to break easily by hammer or spade; NZSC Weathered Aged-argillic Semiarid, Weathered-argillic Perch-gley, Mottled-weathered and Weathered Argillic Pallic Soils (the majority, more than 50 %). Recorded when met: not recorded reads as none. | - |

## Hungarian system (HSCS 2024)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `soluble_salts_pct` | % | numeric | Total water-soluble salts, % by mass of the soil, as Hungarian laboratories report them (estimated from the electrical conductivity of the saturated paste, MSZ-08-0206-2:1978). HSCS 2024 solonchak horizon (0.15 % or more). Not converted from ec_dS_m: the text itself finds that conversion unreliable. | - |
| `soluble_salts_sulfate_pct` | % | numeric | Sulphate share of the total water-soluble salts, % (of the salts, not of the soil). HSCS 2024 subtype karbonát-szulfátos: at least 50 %. | - |
| `soluble_salts_chloride_pct` | % | numeric | Chloride share of the total water-soluble salts, % (of the salts, not of the soil). HSCS 2024 subtype karbonát-kloridos: at least 30 %. | - |
| `p2o5_al_mg_kg` | mg/kg | numeric | P2O5 extracted by the ammonium-lactate (AL) method, mg/kg. HSCS 2024 hortikus (anthropogenic horizon and subtype): at least 500 mg/kg in the upper 20 cm. | - |
| `digo_pct` | % | numeric | Visible traces of applied digó earth (a calcareous ameliorant), % of the soil matrix. HSCS 2024 variety digózott: at least 5 % within 50 cm. | - |
| `coarse_fragments_shape` | - | character | Shape of the coarse fragments: rounded (gravel) or angular (rock debris). HSCS 2024 varieties kavicsos (rounded) and kőzettörmelékes (angular). | - |
| `soluble_salts_carbonate_pct` | % | numeric | Carbonate and bicarbonate share of the total water-soluble salts, % (of the salts, not of the soil). HSCS 2024 subtypes karbonát-szulfátos and karbonát-kloridos: at least 20 % beside the sulphate or chloride share (Hugo's reading, 2026-10-30). | - |

## Japanese system (SCSJ 2017)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `phosphate_absorption_coef` | mg P2O5/100 g | numeric | Phosphate absorption coefficient (Japanese method), mg P2O5 per 100 g of dry soil; the SCSJ 2017 writes it in mg P2O5/g (15.00 = 1500). The kuroboku feature needs 1500 or more and the immature kuroboku feature 300 to under 1500; P retention (phosphate_retention_pct, 85 % and 25 %) or Alo + 1/2 Feo (2.0 % and 0.4 %) replace it when it is not measured. Not the Blakemore P retention. | - |
| `active_al_test` | - | character | Japanese field active-aluminium test (NaF with phenolphthalein paper, Soil Survey Handbook, JSP 1997), graded by how fast and how strongly the paper turns red: -, +-, +, 2+, 3+. SCSJ 2017: a vivid red within a minute (2+ or more) indicates the kuroboku feature; none or +- (no colour, or colour after 5 minutes) indicates no kuroboku feature. Read only when no laboratory value decides. The New Zealand NaF test (naf_test) uses other grades. | - |
| `n_value` | - | numeric | n value of a water-saturated deposit (Pons and Zonneveld 1965): (field water content % of dry soil - 0.2 x silt and sand %) / (clay % + 3 x organic matter %). SCSJ 2017 physical immaturity: 1 or more (or the material squeezes between the fingers, fluidity_class). | - |
| `exchange_acidity_y1` | mL | numeric | Exchange acidity y1 (Daikuhara's method): mL of 0.1 M NaOH needed to titrate the 1 M KCl extract of 100 g of dry soil. SCSJ 2017 non-allophanic kuroboku feature: 5 or more (or Sio under 0.6 %, or Alp/Alo 0.5 or more); not read on farmland, where fertilising and liming change it. | - |
| `ph_h2o2` | - | numeric | pH of the soil after oxidation with hydrogen peroxide. SCSJ 2017 potential sulfuric material: pH(H2O) 4.0 or more with pH(H2O2) under 3.0, in material saturated unless drained (the Thionic subgroups of the Gley and Gray Fluvic soils). | - |
| `peat_moor_type` | - | character | Peat material by the plants it is made of (SCSJ 2017 IV.3): high-moor (Sphagnum, Carex middendorffii, Vaccinium oxycoccus, Rhynchospora, Scheuchzeria the largest share), transitional (Moliniopsis, Eriophorum, Myrica gale, Picea glehnii), low-moor (other). The High-moor and Transitional-moor Peat soils read the uppermost 25 cm of the peat. | - |
| `artefacts_organic` | - | logical | TRUE where the artificial material of the horizon is mainly organic (household refuse and the like), FALSE where mainly inorganic. SCSJ 2017 Organic Artifactual soils (also read from the site). | - |
| `leached_spots` | - | logical | Leached (bleached) spots in the horizon (the Japanese youdatsu-han), TRUE or FALSE. SCSJ 2017 forest soil phases of the Podzols: a podzolic accumulation horizon by analysis with one by morphology and leached spots is moderately developed. | - |
| `texture_class_jp` | - | character | The Japanese soil texture class of the Soil Survey Handbook (ISSS fractions): HC, SiC, LiC, SC, SiCL, CL, SCL (fine-textured), SiL, L, FSL (medium-textured), CoSL, LS, S (coarse-textured), or the Japanese words. CSCJ 2011 soil series groups; without it, clay_pct of 15 % or more reads as fine-textured and medium vs coarse is undecided. | - |
| `hardness_yamanaka_mm` | mm | numeric | Soil hardness by the Yamanaka-type hardness tester (penetration in mm). CSCJ 2011 pan layer (29 mm or more, 10 cm or more thick); not recorded: no pan layer. | - |

# Site properties

The named elements of `PedonRecord$site` that soilKey recognises. Give them in
the `site` list of `PedonRecord$new()` (or of `read_pedon_csv()`), not as
horizon columns. The keys that read a property are given by their registry
ids (`wrb2022`, `usda`, `sibcs5`, `cssc3`, `asc3`, `nzsc4`); the properties of
the last section are stored by the loaders and reports and read by no key.

> Tip (R): `soilKey::site_attribute_spec()` returns this table.

## Identity & location

| Property | Unit | Type | Description | Values | Read by |
|----------|------|------|-------------|--------|---------|
| `id` | - | character | Identifier of the pedon. Written by every loader and shown by print(), the reports, the app and the benchmarks; no key reads it. | - | - |
| `lat` | decimal degrees | numeric | Latitude (-90 to 90; validate() flags a value outside). When the country is not Brazil, \|lat\| <= 23.5 marks the site as tropical, which lets a Bw or Bo designation stand for the WRB ferralic horizon (and so the USDA oxic horizon) and the SiBCS B latossolico; the USDA family soil temperature class estimates the regime from it when soil_temperature_regime is absent. Also read by the SoilGrids prior, the maps and the reports. | - | `wrb2022`, `usda`, `sibcs5` |
| `lon` | decimal degrees | numeric | Longitude (-180 to 180; validate() flags a value outside). Read by the SoilGrids prior, the maps and the reports; no key reads it. | - | - |
| `crs` | EPSG code | numeric | Coordinate reference system of lat and lon (4326, WGS 84, when absent). Read by the SoilGrids buffer and the GeoPackage export; no key reads it. | - | - |
| `country` | - | character | Country of the site, as an ISO code or a name. BR, BRA, Brazil or Brasil marks the site as tropical for the WRB ferralic horizon (and the USDA oxic horizon) and the SiBCS B latossolico. Without designation_system, CA, AU and NZ (or their ISO-3 codes and English names) tell the CSSC, ASC and NZSC keys that the designations may be in their notation, and another country tells the CSSC key that they are not. | - | `wrb2022`, `usda`, `sibcs5`, `cssc3`, `asc3`, `nzsc4` |
| `date` | - | character | Date of the description (ISO 8601, YYYY-MM-DD; the fixtures store a Date). Shown by print() and the reports; no key reads it. | - | - |
| `year` | - | numeric | Year of the description. Read only by the GeoPackage export (pedon_point layer); no loader writes it. | - | - |

## Landscape & surface

| Property | Unit | Type | Description | Values | Read by |
|----------|------|------|-------------|--------|---------|
| `elevation_m` | m | numeric | Elevation above sea level. Read by the USDA family soil temperature class, which estimates the regime from lat and elevation (0 m when absent) when soil_temperature_regime is not given; also shown by the reports. | - | `usda` |
| `slope_pct` | % | numeric | Slope gradient in percent, not degrees. Read by the WRB Inclinic qualifier (5 % or more) and the USDA Cumulic subgroups of the Mollisols (less than 25 %, or not recorded); also shown by the reports. | - | `wrb2022`, `usda` |
| `aspect_deg` | deg | numeric | Slope aspect in degrees clockwise from true north (0 to 360). Filled by the field-sheet extraction (VLM); no key reads it. | - | - |
| `landform` | - | character | Landform, as text (the landform pattern in NatSoil). Read by the ASC Hydrosol settings (tidal, coastal, lake, playa and subaqueous landforms) and by the WRB Gilgaic and Escalic qualifiers (gilgai, built terraces). | - | `wrb2022`, `asc3`, `scsj2017` |
| `landform_element` | - | character | Landform element, as text (NatSoil). Read with landform by the ASC Hydrosol settings; a playa element on a plain pattern counts as a playa. | - | `asc3` |
| `microrelief_form` | - | character | Microrelief at the soil surface, as text (NatSoil writes its microrelief types); also a horizon column of the same name. Read by the WRB Gilgaic qualifier (a recorded microrelief that names no gilgai rules it out) and the Escalic qualifier (built terraces). | - | `wrb2022` |
| `gilgai_presence` | - | logical | Gilgai microrelief present (TRUE) or absent (FALSE). Read first by the WRB Gilgaic qualifier (Vertisols only), before the relief texts. | - | `wrb2022` |
| `forma_relevo` | - | character | Relief form, as text (Portuguese field name). Read beside microrelief_form by the WRB Gilgaic and Escalic qualifiers; no loader writes it. | - | `wrb2022` |
| `relevo_local` | - | character | Local relief, as text (Portuguese field name). Read beside microrelief_form by the WRB Gilgaic qualifier; no loader writes it. | - | `wrb2022` |
| `relief_form` | - | character | Relief form, as text. Read beside microrelief_form by the WRB Gilgaic and Escalic qualifiers; no loader writes it. | - | `wrb2022` |
| `surface_condition` | - | character | Condition of the soil surface, as text with items separated by ';' ('cracking; self-mulching'). Read by the ASC key: surface cracks for the Vertosols, self-mulching and crusts for the Vertosol great groups, and a saline or salt surface for the Salic Hydrosols and the Hypersalic and Hypergypsic suborders. | - | `asc3` |
| `surface_gravel_pct` | % | numeric | Gravel on the ground surface (NaN when recorded with no abundance). Read by the ASC family gravel class of the surface and A1, which takes the larger of it and the gravel of the A1. | - | `asc3` |
| `surface_coarse_fragments_pct` | % | numeric | Coarse fragments of all sizes on the ground surface (NaN when recorded with no abundance). Read by the ASC family gravel class in place of surface_gravel_pct when that is not given. | - | `asc3` |
| `salinised` | - | logical | The site is salinised (TRUE) or not (FALSE). Read by the ASC key: the Salic Hydrosol setting, and the salinity of the Hypersalic and Hypergypsic suborders when no electrical conductivity is recorded. | - | `asc3` |
| `parent_material` | - | character | Parent material, as text. Read by the WRB Rendzic qualifier (limestone, chalk, dolomite, carbonate or marl) and the NZSC Melanic Soils (calcareous, mafic or ultramafic rock); also shown by print() and the reports. | - | `wrb2022`, `nzsc4`, `scsj2017` |
| `land_use` | - | character | Land use, as text. Read by the WRB Drainic (artificial drainage), Tidalic (tidal, salt marsh, mangrove) and Escalic (built terraces) qualifiers. | - | `wrb2022`, `scsj2017` |
| `vegetation` | - | character | Vegetation, as text. Written by the fixtures and the field-sheet extraction (VLM); no key reads it. | - | - |
| `contaminated` | - | logical | A layer within 50 cm exceeds a contamination or toxicity limit of the soil regulation in force. HSCS 2024 subtype szennyezett (Anthropogenic soils). | - | `hscs2024` |
| `terraced` | - | logical | The surface is an artificial terrace. HSCS 2024 variety teraszolt. | - | `hscs2024` |
| `flood_irrigated` | - | logical | The soil is under flood-irrigated cultivation (e.g. rice). HSCS 2024 variety árasztott. | - | `hscs2024`, `scsj2017` |
| `wind_deflation` | - | logical | The surface is bare, structureless single grains with marks of wind reworking (deflation). HSCS 2024 subtype futó (Sandy soils). | - | `hscs2024` |

## Water, climate & regimes

| Property | Unit | Type | Description | Values | Read by |
|----------|------|------|-------------|--------|---------|
| `drainage_class` | - | character | Drainage class, in English or Portuguese; a range ('imperfectly to poorly drained') is read by the end each test asks for. Read by the WRB Drainic, Subaquatic and Tidalic qualifiers (which also look for words such as 'artificial', 'submerged' and 'tidal'), the SiBCS horizonte glei (after drainage), the CSSC Folisol and Vertisol great groups, the ASC key (Hydrosols, organic materials, Folic Organosols, Aquic Vertosols, the Podosol and Hypersalic suborders) and the NZSC Organic Soils. | `excessively drained`, `somewhat excessively drained`, `rapidly drained`, `well drained`, `moderately well drained`, `imperfectly drained`, `somewhat poorly drained`, `poorly drained`, `very poorly drained`, `excessivamente drenado`, `fortemente drenado`, `acentuadamente drenado`, `bem drenado`, `moderadamente drenado`, `imperfeitamente drenado`, `mal drenado`, `muito mal drenado` | `wrb2022`, `sibcs5`, `cssc3`, `asc3`, `nzsc4`, `scsj2017` |
| `drainage` | - | character | Drainage class as the BDsolos export writes it (classe de drenagem, in Portuguese), on the same scale as drainage_class. The SiBCS horizonte glei reads it before drainage_class; the CSSC drainage groups read it when drainage_class is absent. | `excessively drained`, `somewhat excessively drained`, `rapidly drained`, `well drained`, `moderately well drained`, `imperfectly drained`, `somewhat poorly drained`, `poorly drained`, `very poorly drained`, `excessivamente drenado`, `fortemente drenado`, `acentuadamente drenado`, `bem drenado`, `moderadamente drenado`, `imperfeitamente drenado`, `mal drenado`, `muito mal drenado` | `sibcs5`, `cssc3`, `scsj2017` |
| `soil_moisture_regime` | - | character | USDA soil moisture regime, compared in lower case (aridic and torric are the same). Read by the USDA key (the Aridisols, and the ustic, xeric, udic and torric suborders, great groups and subgroups) and by the CSSC key when soil_moisture_subclass is absent (Chernozemic climate, the subhumid Solonetzic subgroups). | `aquic`, `aridic`, `torric`, `udic`, `perudic`, `ustic`, `xeric` | `usda`, `cssc3` |
| `soil_moisture_subclass` | - | character | Canadian soil moisture subclass (letters only are compared, so 'sub-humid' reads as subhumid; the aquic subclasses leave the tests undecided). Read by the CSSC key: the Chernozemic climate (subhumid or drier passes, humid and perhumid fail) and the A-horizon colour of the Solonetzic subgroups (subhumid). Filled by npdb_scenario() for NPDB pedons. | `arid`, `subarid`, `semiarid`, `subhumid`, `humid`, `perhumid` | `cssc3` |
| `soil_temperature_regime` | - | character | USDA soil temperature regime, compared in lower case. Read by the USDA key (the cryic suborders and great groups, and the family temperature class, which is otherwise estimated from lat and elevation_m) and by the CSSC key when mean_annual_soil_temp_C is absent (Gray Brown Luvisols: mesic or thermic; Chernozemic climate: not pergelic or gelic). | `gelic`, `cryic`, `frigid`, `mesic`, `thermic`, `hyperthermic`, `isofrigid`, `isomesic`, `isothermic`, `isohyperthermic`, `pergelic` | `usda`, `cssc3`, `scsj2017` |
| `mean_annual_soil_temp_C` | deg C | numeric | Mean annual soil temperature. Read by the CSSC key: the Chernozemic climate (0 degrees or more) and the Gray Brown Luvisols (8 degrees or more). Filled by npdb_scenario() for NPDB pedons. | - | `cssc3`, `scsj2017` |
| `soil_temp_30cm_C` | deg C | numeric | Mean annual soil temperature at 30 cm from the mineral soil surface. Read by the NZSC Hydrothermal Recent and Raw Soils (2.5 degrees or more above the mean annual air temperature). | - | `nzsc4` |
| `mean_annual_air_temp_C` | deg C | numeric | Mean annual air temperature. Read by the NZSC Hydrothermal Recent and Raw Soils, against soil_temp_30cm_C. | - | `nzsc4` |
| `surface_water` | - | character | How often water stands on the soil surface. Read by the NZSC Hydric Raw Soils (surface water for the majority of the time, but not constantly). | `constant`, `most of the time`, `occasional`, `none` | `nzsc4` |
| `artificially_drained` | - | logical | The site is artificially drained. Read by the NZSC peaty topsoil, whose saturation requirement then does not apply. | - | `nzsc4`, `scsj2017` |
| `groundwater_kind` | - | character | Kind of water table recorded at the site (FAO): none observed, observed, apparent, perched or flooded. Read with groundwater_depth_cm by the WRB gleyic and stagnic properties: mottles over a layer with no reductimorphic matrix are gleyic where groundwater that is not perched lies within 150 cm, stagnic where none was observed or it is perched, and undecided where the site does not say. Written by isis_profile_to_pedon(). | `none observed`, `observed`, `apparent`, `perched`, `flooded` | `wrb2022` |
| `groundwater_depth_cm` | cm | numeric | Depth of the water table below the soil surface (0 at the surface). Read with groundwater_kind by the WRB gleyic and stagnic properties (within 150 cm, or deeper). Written by isis_profile_to_pedon() from depthGroundwater, or groundwaterTop where ISIS writes 0 for the depth. | - | `wrb2022`, `scsj2017` |
| `water_table_cm_above_surface` | cm | numeric | Height of a water table above the soil surface (positive above it). Read by the USDA key: the Wassents (above 0) and the Wassists (2 cm or more). | - | `usda` |
| `tidal_zone` | - | character | Tidal zone of the site. Read by the ASC Hydrosol settings (Subtidal, Intertidal, Supratidal and Extratidal suborders); a zone named in landform is read too. | `subtidal`, `intertidal`, `supratidal`, `extratidal` | `asc3` |
| `tidal_inundation` | - | logical | The site is subject to tidal flooding. Read by the ASC key: it makes the soil a Hydrosol, rules out the Organosols, and without tidal_zone leaves the tidal Hydrosol settings undecided. | - | `asc3` |
| `subaqueous` | - | logical | An inland water body lies over the soil. Read by the ASC Subaqueous Hydrosol setting, before the landform text. | - | `asc3` |

## Description & notation

| Property | Unit | Type | Description | Values | Read by |
|----------|------|------|-------------|--------|---------|
| `designation_system` | - | character | Notation of the horizon designations. Read by the CSSC, ASC and NZSC keys to decide whether the designations are in their notation (otherwise judged from the designations and the country). Written by the NatSoil ('asc') and NPDB ('cssc') loaders. | `cssc`, `cssc3`, `canadian`, `canada`, `asc`, `australian`, `field handbook`, `nchs`, `australian field handbook`, `nzsc`, `nz`, `new zealand` | `cssc3`, `asc3`, `nzsc4`, `scsj2017` |
| `nasis_diagnostic_features` | - | character | Diagnostic features the surveyor recorded in NASIS (pediagfeatures featkind, one string per feature). Read by the USDA key: the argillic clay-films test and the fragipan, and as a tie-breaker that turns an undecided order test (histic, spodic, andic, vertic, argillic, kandic, natric, mollic, cambic) into a pass. Written by the KSSL NASIS loader. | - | `usda` |
| `anthropic_truncation` | - | logical | The soil has been truncated by human activity (TRUE, or 'yes', 'y', 't', '1'). Read by the NZSC Anthropic Soils, where it meets the first condition without 30 cm of anthropic soil material. | - | `nzsc4`, `scsj2017` |
| `mechanical_mixing_depth_cm` | cm | numeric | Depth from the mineral soil surface to which people mechanically disturbed the original horizons (mixing, deep ripping, subsoil lifting). Read by the NZSC Mixed Anthropic Soils (more than 30 cm). | - | `nzsc4`, `scsj2017` |
| `methane_odour` | - | logical | A methane content that can be detected by odour. Read by the NZSC Refuse Anthropic Soils. | - | `nzsc4` |
| `mechanical_mixing_purpose` | - | character | Why the original horizons were mechanically mixed: tephra flipping (a modified tephra stratigraphy), breaking impeding horizons (to break up drainage-impeding horizons such as placic horizons or pans, often with hump-and-hollow recontouring), or other. Read by the NZSC Inverted-tephric and Inverted-impeded Mixed Anthropic Soils; recorded when met. | `tephra flipping`, `breaking impeding horizons`, `other` | `nzsc4`, `scsj2017` |
| `acid_neutralising_amendment` | - | logical | TRUE where acid-neutralising amendments (lime or similar) were added to the soil material. Read by the NZSC Treated-sulfidic Fill Anthropic Soils; recorded when met. | - | `nzsc4`, `scsj2017` |
| `overthickened_a` | - | logical | The surveyor recorded an overthickened A horizon (TRUE, or 'yes', 'y', 't', '1'). Read by the NZSC key: with its base at 45 cm or more, under 30 cm of anthropic material and no mottles within 30 cm, the soil enters the key at the Recent Soils. | - | `nzsc4` |
| `jp_edition` | - | character | Internal: set by classify_cscj2011() on its copy of the pedon ('cscj2011') so that the diagnostics the CSCJ 2011 shares with the SCSJ 2017 read the CSCJ's own limits. Not for user data. | - | `cscj2011` |
| `scsj_part` | - | logical | Internal: set by the SCSJ key on a part of a profile it keys on its own (the fill of Man-made soils, or the soil under it); the fill's own material is then read. Not for user data. | - | `scsj2017` |
| `scsj_part_complete` | - | logical | Internal: with scsj_part, whether nothing lies below the part's last horizon (the fill; the soil under it only where the original profile ends on rock). Not for user data. | - | `scsj2017` |
| `artefacts_organic` | - | logical | TRUE where the artificial material of the soil is mainly organic (household refuse and the like). SCSJ 2017 Organic Artifactual soils, read when no horizon records artefacts_organic. | - | `scsj2017` |
| `forest_soil_type` | - | character | The soil type of the Japanese forest soil classification (1975) recorded for the site: BA, BB, BC, BD, BD(d), BE, BF, or PDI, PDII, PDIII for the Podzols; the types of the other 1975 groups (BlD, lBlD, RC, YD(d), dRB ...) are read by their moisture letter, and PW (wet podzols) gives no development phase. SCSJ 2017 forest soil phases (chapter V.3), read before the phases soilKey derives from the profile; a recorded type also says forest land when no land use is recorded. | - | `scsj2017` |

## Soil construction (soilKey Pro)

| Property | Unit | Type | Description | Values | Read by |
|----------|------|------|-------------|--------|---------|
| `technic` | - | character | The soilKey Pro answer on artefacts, technic hard material and geomembranes, saved in the session file. 'none' writes 0 % artefacts, 0 % technic hard material and no geomembrane on every horizon; 'present' applies the geomembrane answer; 'unknown' writes nothing. The keys read the horizon columns, not this. | `unknown`, `none`, `present` | - |
| `geomembrane` | - | character | With technic 'present': 'yes' marks geomembrane_present on the horizon that holds geomembrane_depth_cm (the top one when no depth matches), 'no' marks it absent on every horizon, 'unknown' writes nothing. | `unknown`, `no`, `yes` | - |
| `geomembrane_depth_cm` | cm | numeric | Depth of the geomembrane from the soil surface; selects the horizon on which soilKey Pro marks geomembrane_present. | - | - |

## Reference classes & provenance (stored, read by no key)

| Property | Unit | Type | Description | Values | Read by |
|----------|------|------|-------------|--------|---------|
| `reference_source` | - | character | Name of the dataset the reference classes come from ('Embrapa BDsolos', 'KSSL / NCSS Lab Data Mart', ...). Written by the benchmark loaders. | - | - |
| `reference_wrb` | - | character | Published WRB class (a Reference Soil Group, or the name the dataset gives). Written by the LUCAS-ESDB, FEBR, BDsolos and AfSP (from WRB 2006) loaders and the spectral libraries, or copied from wosis_rsg or reference_wrb_from_usda; read by the benchmarks as the WRB reference. | - | - |
| `reference_wrb_full` | - | character | Full WRB name the data provider gives the profile, with its qualifiers. Written by isis_profile_to_pedon() (ISRIC's name); its edition is in reference_wrb_edition. | - | - |
| `reference_wrb_edition` | - | character | Edition (year) of the WRB name in reference_wrb and reference_wrb_full, e.g. '2022'. Written by isis_profile_to_pedon(); benchmark_isis() scores only the 2022 names. | - | - |
| `reference_usda_edition` | - | character | Edition (year) of the USDA names in reference_usda and its suborder, great group and subgroup fields, e.g. '1999'. Written by isis_profile_to_pedon(). read_nsd_pedons() writes 'before 1990 (Andepts)' for a great group of the Andepts, Inceptisols until Soil Taxonomy created the Andisols. | - | - |
| `reference_wrb06_full` | - | character | Full WRB 2006 name of an AfSP profile. Written by load_afsp_pedons(). | - | - |
| `reference_fao88` | - | character | FAO 1988 class of an AfSP profile. Written by load_afsp_pedons(). | - | - |
| `reference_wrb_from_usda` | - | character | WRB Reference Soil Group derived from reference_usda and reference_usda_suborder by annotate_wrb_from_usda(); read by benchmark_wrb_vs_usda() and copied into reference_wrb when that is missing. | - | - |
| `wosis_rsg` | - | character | WRB Reference Soil Group that WoSIS reports for the profile; copied into reference_wrb when that is missing. | - | - |
| `reference_usda` | - | character | Published USDA Soil Taxonomy class (the order, or a subgroup or series name, depending on the loader). Written by the KSSL, FEBR and AfSP loaders; read by the benchmarks and annotate_wrb_from_usda(). | - | - |
| `reference_usda_suborder` | - | character | Published USDA suborder (KSSL samp_taxsuborder). Read by the suborder benchmark and annotate_wrb_from_usda(). | - | - |
| `reference_usda_grtgroup` | - | character | Published USDA great group (KSSL samp_taxgrtgroup). Read by the great-group benchmark. | - | - |
| `reference_usda_subgroup` | - | character | Published USDA subgroup (KSSL samp_taxsubgrp). Read by the subgroup benchmark. | - | - |
| `reference_st` | - | character | Published Soil Taxonomy class under another name: the BDsolos Soil Taxonomy column, or the USDA order of a spectral library. Read as the USDA reference by the BDsolos and spectral benchmarks (the other loaders use reference_usda). | - | - |
| `wosis_usda_order` | - | character | USDA order that WoSIS reports for the profile. | - | - |
| `reference_sibcs` | - | character | Published SiBCS class, as the dataset writes it. Written by the FEBR, BDsolos, Embrapa and Redape loaders and the spectral libraries; read by the benchmarks and by build_taxon_profiles(). | - | - |
| `reference_sibcs_order` | - | character | SiBCS order of a Redape profile (ORDEM). Read by benchmark_redape(), which composes the reference at each level. | - | - |
| `reference_sibcs_subordem` | - | character | SiBCS suborder of a Redape profile (SUBORDEM). Read by benchmark_redape(). | - | - |
| `reference_sibcs_gg` | - | character | SiBCS great group of a Redape profile (GDE_GRUPO). Read by benchmark_redape(). | - | - |
| `reference_sibcs_subgrupo` | - | character | SiBCS subgroup of a Redape profile (SUBGRUPO). Read by benchmark_redape(). | - | - |
| `reference_asc_order` | - | character | Surveyors' ASC order code of a NatSoil profile. Read by benchmark_natsoil(). | - | - |
| `reference_asc_suborder` | - | character | Surveyors' ASC suborder code of a NatSoil profile. Read by benchmark_natsoil(). | - | - |
| `reference_asc_great_group` | - | character | Surveyors' ASC great group code of a NatSoil profile. Read by benchmark_natsoil(). | - | - |
| `reference_asc_subgroup` | - | character | Surveyors' ASC subgroup code of a NatSoil profile. Read by benchmark_natsoil(). | - | - |
| `reference_asc_family` | - | character | Surveyors' ASC family codes of a NatSoil profile, by position and separated by ';'. Read by benchmark_natsoil(). | - | - |
| `reference_asc_confidence` | - | character | Surveyors' confidence code for the ASC class of a NatSoil profile. Reported by benchmark_natsoil(). | - | - |
| `reference_cssc_order` | - | character | CSSC order of an NPDB profile, matched to the classes of the key. Read by benchmark_npdb(). | - | - |
| `reference_cssc_great_group` | - | character | CSSC great group of an NPDB profile, matched to the classes of the key. Read by benchmark_npdb(). | - | - |
| `reference_cssc_subgroup` | - | character | CSSC subgroup of an NPDB profile, matched to the classes of the key. Read by benchmark_npdb(). | - | - |
| `reference_cssc_raw` | - | character | NPDB order, great group and subgroup as recorded, joined by ' \| '. Written by load_npdb_pedons(). | - | - |
| `reference_nzsc` | - | character | NZSC code of an NSD profile as the surveyor recorded it (e.g. 'MOT'), recoded to the 4th edition where the 4th edition renamed the code (MIW as MIC). Written by read_nsd_pedons(). | - | - |
| `reference_nzsc_name` | - | character | NZSC class name of an NSD profile as recorded (e.g. 'Typic Orthic Pumice Soil'). Written by read_nsd_pedons(). | - | - |
| `reference_nzsc_order` | - | character | NZSC order code (one letter) of an NSD profile; NA for a subgroup the 4th edition withdrew and moved to other orders (BLAM). Read by benchmark_nsd(). | - | - |
| `reference_nzsc_group` | - | character | NZSC group code (two letters) of an NSD profile, when the record names a group. Read by benchmark_nsd(). | - | - |
| `reference_nzsc_subgroup` | - | character | NZSC subgroup code (three or four letters) of an NSD profile, when the record names a subgroup the 4th edition keeps. Read by benchmark_nsd(). | - | - |
| `reference_nzsc_raw` | - | character | NSD classifier_nzsc as recorded (code and name). Written by read_nsd_pedons(). | - | - |
| `reference_nzsc_note` | - | character | How a 3rd-edition NZSC code of an NSD profile was read (recoded, or withdrawn in the 4th edition). Written by read_nsd_pedons(). | - | - |
| `reference_nzsc_alt` | - | character | NSD classifier_nzsc_alt: an alternative NZSC class the surveyor recorded. Written by read_nsd_pedons(). | - | - |
| `reference_nz_genetic` | - | character | NZ Genetic Soil Classification class of an NSD profile (classifier_nz_genetic). Written by read_nsd_pedons(). | - | - |
| `reference_nz_series` | - | character | Soil series of an NSD profile (classifier_nz_series). Written by read_nsd_pedons(). | - | - |
| `reference_usda_raw` | - | character | Soil Taxonomy subgroup of an NSD profile as recorded (classifier_usda, e.g. 'TYPIC UDIVITRAND'). Written by read_nsd_pedons(). | - | - |
| `nsdr_identifier` | - | character | Identifier of the site in the National Soil Data Repository of New Zealand (nsdridentifier). Written by read_nsd_pedons(). | - | - |
| `survey` | - | character | Name of the NSD soil survey the profile belongs to. Written by read_nsd_pedons(). | - | - |
| `survey_code` | - | character | Code of the NSD soil survey the profile belongs to. Written by read_nsd_pedons(). | - | - |
| `sali_asc_label` | - | character | Surveyors' ASC label of a Queensland SALI site as the site layer gives it (order, suborder, great group, subgroup names). Written by read_sali_pedons(); its codes are in reference_asc_order and the following fields. | - | - |
| `sali_pdf_asc` | - | character | ASC name of a SALI site as its site report prints it (the book's order, with the family). Written by read_sali_pedons(). | - | - |
| `sali_asc_edition` | - | character | Edition of the ASC technical reference the SALI site report cites, e.g. '3rd (2021)', '2nd (2016)', '1st (1996)'. Written by read_sali_pedons(); benchmark_sali() reports the agreement by edition. | - | - |
| `sali_asc_confidence_text` | - | character | Confidence phrase of the SALI site report for its ASC class, as printed (reference_asc_confidence gives it as 1-4, or C). Written by read_sali_pedons(). | - | - |
| `sali_unmapped` | - | character | Names of the SALI label that match no class of the ASC 3rd edition (left NA), separated by '; '. Written by read_sali_pedons(). | - | - |
| `sali_observation_class` | - | character | Observation class of the SALI site (e.g. 'Class IIIb (core analytical suite)'). Written by read_sali_pedons(). | - | - |
| `sali_observation_type` | - | character | Observation type of the SALI site (e.g. 'Soil pit', 'Hand auger boring'). Written by read_sali_pedons(). | - | - |
| `sali_depth_to_free_water` | - | character | Depth to free water as the SALI site report records it (text). Written by read_sali_pedons(); not used. | - | - |
| `sali_substrate` | - | character | Substrate as the SALI site report records it (text). Written by read_sali_pedons(); not used. | - | - |
| `sali_n_lab_samples` | - | numeric | Number of laboratory samples of observation 1 read for the SALI site. Written by read_sali_pedons(). | - | - |
| `sali_lab_source` | - | character | Where the laboratory values of the SALI site come from: 'api' (the SALI soil chemistry API), 'report table' (the site report's table, read only when the API holds no samples) or 'none'. Written by read_sali_pedons(). | - | - |
| `sali_n_predicted` | - | numeric | Number of horizon values of the SALI site that are MIR predictions, used where nothing was measured (marked predicted_spectra in the provenance). Written by read_sali_pedons(). | - | - |
| `curation_note` | - | character | Curation note of a Redape profile (CURADORIA). Written by load_redape_pedons(). | - | - |
| `curation_authors` | - | character | Authors of the curation of a Redape profile (AUTOR_CURADORIA). Written by load_redape_pedons(). | - | - |
| `source` | - | character | Provider of a profile read from a live service ('ISRIC WoSIS', 'NatSoil (CSIRO)'); soilKey Pro keeps it with the other provenance fields. | - | - |
| `dataset` | - | character | Code of the WoSIS source dataset of the profile; shown by soilKey Pro. | - | - |
| `dataset_id` | - | character | NPDB dataset identifier (DATASET_ID). Written by load_npdb_pedons(). | - | - |
| `wosis_profile_id` | - | character | Internal WoSIS identifier of the profile (the profile code is the id). | - | - |
| `wosis_profile_code` | - | character | WoSIS profile code, which soilKey Pro keeps with the provenance fields of a WoSIS profile and shows in its data-source note. | - | - |
| `accessed` | - | character | Date the WoSIS data were read (YYYY-MM-DD); shown by soilKey Pro. | - | - |
| `licence` | - | character | Licence of the profile data (WoSIS: the more restrictive of the profile and layer licences; NatSoil: its usage licence); shown by soilKey Pro. | - | - |
| `licence_short` | - | character | Short label of the WoSIS licence ('CC BY 4.0', 'CC BY-NC 3.0', 'Public domain'); shown by soilKey Pro. | - | - |
| `citation` | - | character | Citation the provider asks for (WoSIS, with the download date; NatSoil). Printed by the HTML and PDF reports. | - | - |
| `attribution` | - | character | One-line attribution of a WoSIS profile (profile code, dataset, date read, licence). Printed by the HTML and PDF reports. | - | - |
| `depth_note` | - | character | Note that the WoSIS depths were shifted so the organic surface layer starts at 0 cm; shown by soilKey Pro. | - | - |
| `wosis_error` | - | character | Why the WoSIS layers could not be read (the pedon then has no horizons); shown by soilKey Pro. | - | - |
| `sampling_type` | - | character | Kind of BDsolos sampling point ('Tipo': Perfil completo, Perfil parcial or Amostra extra). The BDsolos benchmarks keep the complete profiles ('perfil completo') and the records that do not say. | - | - |
| `state` | - | character | Brazilian state of the profile (FEBR estado_id, BDsolos UF). | - | - |
| `municipality` | - | character | Municipality of a BDsolos profile. | - | - |
| `province` | - | character | Province or region of the profile: the Canadian province of an NPDB profile, the New Zealand region of an NSD profile. | - | - |
| `altitude_m` | m | numeric | Altitude of a BDsolos profile. The USDA family reads elevation_m, not this. | - | - |
| `land_cover` | - | character | Land cover or vegetation, as text (BDsolos current use or vegetation column; LUCAS LC0_Desc). The WRB qualifiers read land_use, not this. | - | - |
| `survey_date` | - | character | Survey date of a LUCAS 2018 point (SURVEY_DATE). The reports show date, not this. | - | - |
| `npdb_depth_order_mismatch` | - | logical | TRUE when the depth order of an NPDB profile's layers differs from their layer order. Written by load_npdb_pedons(). | - | - |
| `npdb_cmi` | - | numeric | Thornthwaite climate moisture index of an NPDB pedon, set by npdb_scenario() with soil_moisture_subclass. | - | - |
| `npdb_scenario` | - | list | A list of the ranges npdb_scenario() estimated (rows and error bands of the dry colours, the soil temperature range, the moisture subclasses between the two PET methods). Read by benchmark_npdb(), which classifies the pedon at every combination of their ends. | - | - |

---

_Generated from `inst/schemas/horizon-attributes.yaml`, `inst/schemas/site-attributes.yaml`, `horizon_column_spec()` and `.horizon_name_aliases()` in soilKey v0.9.328._

