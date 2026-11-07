# soilKey -- recognised horizon attribute names (v0.9.373)

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
| `fe_dcb_pct` | % Fe | numeric | Dithionite-citrate-bicarbonate extractable Fe (free/pedogenic iron oxides), % Fe (not Fe2O3). Against a total iron given as Fe2O3 (fe_total_pct) the free over total ratio reads fe_dcb_pct / (0.6994 x fe_total_pct). | - |
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
| `layer_origin` | - | character | 'aeolic' / 'fluvic' / 'solimovic' / 'tephric' / 'organic' etc (for material gating). The French Référentiel pédologique 2008 also reads 'colluvic' (the colluviosols and the qualifier colluvial), 'marine' (the thalassosols) and 'transported' (the anthroposols reconstitués) (Hugo's decisions Q14 and Q15). The German KA5 (ka5) reads 'tidal-marine', 'tidal-brackish' and 'tidal-fluvial' (the prefixes tm, tb, tp of the Marschen, Watt and Strand, and the subtype letters n, b, f) and 'limnic' (the subhydric soils). | - |

## SiBCS pendentes (von Post, Ki/Kr, COLE, sulfuric attack)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `fiber_content_rubbed_pct` | % | numeric | SiBCS Cap 14: % fibras apos esfregamento (Saprico < 17, Hemico 17-40, Fibrico >= 40) | - |
| `fiber_content_unrubbed_pct` | % | numeric | SiBCS Cap 14: % fibras antes do esfregamento (auxiliar) | - |
| `von_post_index` | - | integer | Indice de decomposicao von Post 1924 (H1-H10): H1-H4 Fibrico / H5-H6 Hemico / H7-H10 Saprico | - |
| `cole_value` | - | numeric | Coefficient of Linear Extensibility (moist at 33 kPa -> oven dry); SiBCS retratil >= 0.06; the French Référentiel pédologique 2008 measures it at -33 kPa too (vertisols, pp. 320-321) | - |
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
| `woody_fragments_pct` | % | numeric | SiBCS Cap 18 p 288: % volume de galhos/troncos >= 2 cm em horizontes organicos (Organossolos lenhosos / muito lenhosos / extremamente lenhosos). Also read by the humus forms of the French Référentiel pédologique 2008 (Annex 1, p. 340): recognisable woody debris in the O horizons, lignique 35-80 %, xylique over 80 %. | - |

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
| `peat_moor_type` | - | character | Peat material by the plants it is made of (SCSJ 2017 IV.3): high-moor (Sphagnum, Carex middendorffii, Vaccinium oxycoccus, Rhynchospora, Scheuchzeria the largest share), transitional (Moliniopsis, Eriophorum, Myrica gale, Picea glehnii), low-moor (other). The High-moor and Transitional-moor Peat soils read the uppermost 25 cm of the peat. The Russian Field Guide of 2008 adds mesophilic: the litter of non-mire herbs, dwarf shrubs or krummholz, the dry peat TJ (Hugo's decision Q22); its oligotrophic peat TO is high-moor, its eutrophic peat TE transitional or low-moor. | - |
| `artefacts_organic` | - | logical | TRUE where the artificial material of the horizon is mainly organic (household refuse and the like), FALSE where mainly inorganic. SCSJ 2017 Organic Artifactual soils (also read from the site). | - |
| `leached_spots` | - | logical | Leached (bleached) spots in the horizon (the Japanese youdatsu-han), TRUE or FALSE. SCSJ 2017 forest soil phases of the Podzols: a podzolic accumulation horizon by analysis with one by morphology and leached spots is moderately developed. | - |

## Japanese system (CSCJ 2011)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `texture_class_jp` | - | character | The Japanese soil texture class of the Soil Survey Handbook (ISSS fractions): HC, SiC, LiC, SC, SiCL, CL, SCL (fine-textured), SiL, L, FSL (medium-textured), CoSL, LS, S (coarse-textured), or the Japanese words. CSCJ 2011 soil series groups; without it, clay_pct of 15 % or more reads as fine-textured and medium vs coarse is undecided. | - |
| `hardness_yamanaka_mm` | mm | numeric | Soil hardness by the Yamanaka-type hardness tester (penetration in mm). CSCJ 2011 pan layer (29 mm or more, 10 cm or more thick); not recorded: no pan layer. | - |

## French system (CPCS 1967)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `ice_form` | - | character | Form of the ground ice in the layer, as text: lenses (large or small), veins, wedges, pore ice, massive ice, or none. Read by the CPCS groups of the permafrost soils (II/11-13): ice lenses or segregated ice, or 20 % ice or more (ice_pct), is a strong ice segregation. Not recorded with ice_pct: the group is undecided. | - |
| `caco3_active_pct` | % | numeric | Active carbonate (calcaire actif, the Drouineau ammonium-oxalate method), % of the fine earth. Read by the CPCS rendzinas rich in active carbonate (V/112): 10 % or more in the A; not recorded, a very strong effervescence decides. | - |
| `coarse_sand_pct` | % | numeric | Coarse sand (0.2-2 mm, the French size class), % of the fine earth. Read by the CPCS ferrallitic subgroups: 'faiblement remanié' (with fine_sand_pct, the coarse to fine sand ratio of the A and of the B differing by 20 % or more) and 'éluvié' (twice the coarse sand at the A/(B) boundary, as twice the coarse fragments and nodules). | - |
| `fine_sand_pct` | % | numeric | Fine sand (0.05-0.2 mm, the French size class), % of the fine earth. Read with coarse_sand_pct by the CPCS ferrallitic subgroups 'faiblement remanié' (X/124, X/235, X/335). | - |
| `gibbsite_pct` | % | numeric | Gibbsite, % of the clay fraction. Read by the CPCS subgroup of the dark brown, very acid, gibbsitic ferrallitic soils (X/322): 25 % or more in the larger part of the B; not recorded, Ki under 1.0 where Ki exists, else the colour and the pH alone. | - |

## Russian system (Field Guide 2008)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `phys_clay_pct` | % | numeric | Physical clay (particles under 0.01 mm, the Kachinsky analysis), % of the fine earth. The Russian Field Guide of 2008 writes texture in the Kachinsky gradations of the podzolic scale (sand under 10, loamy sand 10-20, light loam 20-30, medium loam 30-40, heavy loam 40-50, light clay 50-65, medium clay 65-80, heavy clay over 80); one gradation lighter or heavier than the horizon below or above separates E from EL and BT from BI. Not recorded: texture_class_ru, else the USDA texture class, flagged approximate (Hugo's decision Q7). | - |
| `texture_class_ru` | - | character | Texture class of the Russian (Kachinsky) scale, in English or in Russian: sand (песок), loamy sand (супесь), light, medium or heavy loam (легкий, средний, тяжелый суглинок), light, medium or heavy clay (легкая, средняя, тяжелая глина), the Russian words written with е or ё. Read for the gradations of the Russian Field Guide of 2008 where phys_clay_pct is not recorded (Hugo's decision Q7). | - |
| `loi_pct` | % | numeric | Loss on ignition, % of the dry mass. The Russian Field Guide of 2008 bounds its organic horizons by it: the peat horizons over 35 %, the peregnoi H over 25 %, the agro-peat-mineral PTR under 35 %, the coarse-humus AO 15-35 %, AH 10-25 %. Not recorded: 1.724 x oc_pct (35 % is then 20.3 % organic carbon; Hugo's decision Q6). | - |
| `peat_decomposition_pct` | % | numeric | Degree of decomposition of peat, % (the Russian method: the share of the plant tissue that has lost its cell structure). The Russian Field Guide of 2008 reads 50 % or more as the peregnoi H and at most 50 % as a peat horizon (O, T, TO, TE, TJ, PT). Not recorded: von_post_index (H7 or more decomposed, H4 or less not), then fiber_content_rubbed_pct (under 17 % decomposed, 40 % or more not); between them undecided (Hugo's decision Q6). | - |
| `peat_botanical_composition` | - | character | The plants whose remains make the peat, with their share in % where known, joined by ';' (sedge 50; wood 30; sphagnum 20), in English or Russian: wood (древесный), reed (тростниковый), Scheuchzeria (шейхцериевый), horsetail (хвощовый), herb (травяной), cotton-grass (пушицевый), Sphagnum (сфагновый), sedge (осоковый), Hypnum (гипновый). The Russian Field Guide of 2008 names the species of peat of its organogenic soils by the plant whose remains are more than 40 % of the peat, by two when two dominate (древесно-осоковый, p. 136); a record with no shares names its dominant plants. Not recorded: the species is undecided. | - |
| `foreign_fragments_pct` | % | numeric | Fragments of other horizons mixed into the layer (deep ploughing, plantage, trenching, burrowing), % of the vertical section. The Russian Field Guide of 2008 reads 80 % or more (under 20 % of the section undisturbed) over 40 cm as the turbated horizon TUR, a thinner mixed layer as the feature tr, over 25 % of an agro horizon as agr, fragments of the horizon below in an agro horizon as pb. Not recorded: the site's mechanical_mixing_depth_cm, flagged (Hugo's decision Q20). | - |
| `turbated_fragments` | - | character | The horizons whose fragments make up a turbated layer, as Russian indices joined by ';' (SEL;ASN). Read for the post-solonetz turbozems and the solonetzic agrozems of the Russian Field Guide of 2008, which the formulas write TUR[SEL+ASN]; a designation written that way gives them too (Hugo's decision Q20). | - |
| `contamination_hazard` | - | character | Hazard category of the pollutants in the layer by the Russian sanitary standards: permissible, moderately hazardous, highly hazardous or extremely hazardous. The Russian Field Guide of 2008 makes a layer starting within 30 cm with extremely hazardous pollutants the contaminated horizon X, and the soil a chemozem; the site's contaminated or contamination_type without a category leave X undecided, and nothing recorded means no X (Hugo's decision Q23). | - |
| `pollutants` | - | character | The pollutants of the layer, as text (Cu, Ni, Co; oil products; radionuclides). Named in the name of a chemozem of the Russian Field Guide of 2008, which keeps the name of the soil it was before the pollution (Hugo's decision Q23). | - |
| `uncoated_grains_pct` | % | numeric | Share of the sand and silt grains with no colouring (iron or humus) film, % (seen with a hand lens). Key position 30 of the Russian Field Guide of 2008: films on under 20 % of the grains (over 80 % uncoated) count as no films; continuous films make BHF, BFM, BAN or F. Not recorded: soilKey's spodic horizon stands in for the films of BHF, flagged (Hugo's decisions Q4 and Q24). | - |
| `effervescence_extent` | - | character | Extent of the effervescence with 10 % HCl over the horizon: none, local (spots, single fragments or morphons) or continuous (the whole mass). Key position 32 of the Russian Field Guide of 2008: continuous effervescence leads to BCA, CAT, ML or the feature ca. Not recorded: caco3_pct (1 % or more is continuous, under 0.5 % none) or effervescence_class (strong or very strong is continuous; none is none) (Hugo's decision Q24). | - |
| `plant_residues_pct` | % (vol) | numeric | Plant residues in the layer (live roots excluded), % by volume. Key positions 10 and 11 of the Russian Field Guide of 2008: over two thirds of the volume makes an organic horizon (peat or peregnoi), coarse residues under one third an AH, AO or PTR. Not recorded: the organic matter (loi_pct, else oc_pct) stands in (Hugo's decisions Q6 and Q24). | - |
| `sulfide_odour` | - | logical | Smell of hydrogen sulfide from the layer, TRUE or FALSE. Key position 3 of the Russian Field Guide of 2008: a black silty layer smelling of H2S under a salt crust is the sulfidic solonchak SS, otherwise the solonchak S. Not recorded: sulfidic_s_pct (Hugo's decision Q24). | - |
| `slickensides_size` | - | character | Size of the slickensides in the layer: small (chaotic, under about 20 cm), large (over 20-30 cm) or both. The vertic horizon V of the Russian Field Guide of 2008 needs small and large slickensides with wedge-shaped peds; one or two of those signs make the feature v. Not recorded: slickensides with the wedge-shaped structure (Hugo's decision Q24). | - |
| `cryoturbation` | - | character | Cryoturbation in the layer (bent or torn horizons, involutions, organic material pulled down by frost): none or present. The feature @ of the Russian Field Guide of 2008, and evidence of its cryogenic horizon CR. Not recorded: an irregular or broken boundary with frost in the profile (Hugo's decision Q24). | - |

## French system (Référentiel pédologique 2008)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `coarse_fragments_wt_pct` | % | numeric | Coarse elements (over 2 mm), % by weight of the air-dried whole soil, as the French Référentiel pédologique 2008 measures them (pp. 24, 249), not by volume. The RP reads them for the peyric horizons X (over 60 % over more than 10 cm), the peyrosols (over 60 % in the upper 50 cm), the rankosols and the qualifiers caillouteux, graveleux and pierreux (over 40 %). Not recorded: coarse_fragments_pct (by volume) converted with a rock density of 2.65 and the recorded bulk density of the fine earth, flagged; with no bulk density, a volume at or above the threshold reads yes (the weight share is never lower, except for porous rocks), a volume under 35 % (for 60) or under 20 % (for 40) no, and between them the test is undecided (Hugo's decision Q11). | - |
| `gravel_wt_pct` | % | numeric | Gravel (graviers, 0.2-2 cm), % by weight of the whole soil (Référentiel pédologique 2008, p. 249). The RP gravelly horizon Xgr has over 50 % gravel; also the qualifier graveleux. Not recorded: gravel_pct (by volume), flagged (Hugo's decision Q11). | - |
| `cobbles_wt_pct` | % | numeric | Cobbles (cailloux, 2-7.5 cm), % by weight of the whole soil (Référentiel pédologique 2008, p. 249). The RP cobbly horizon Xc has over 50 % cobbles, stones and boulders with cobbles dominant; also the qualifier caillouteux. Not recorded: the coarse fragments by volume, flagged (Hugo's decision Q11). | - |
| `stones_wt_pct` | % | numeric | Stones (pierres, 7.5-20 cm; not the 7.5-25 cm of other French guides), % by weight of the whole soil (Référentiel pédologique 2008, p. 249). The RP stony horizon Xp has over 50 % cobbles, stones and boulders with stones or boulders dominant; also the qualifier pierreux. Not recorded: the coarse fragments by volume, flagged (Hugo's decision Q11). | - |
| `boulders_wt_pct` | % | numeric | Boulders (blocs, over 20 cm), % by weight of the whole soil (Référentiel pédologique 2008, p. 249), counted with the stones for the RP stony horizon Xp. Not recorded: the coarse fragments by volume, flagged (Hugo's decision Q11). | - |
| `cec_method` | - | character | Method of the CEC and the exchangeable bases of the layer: acetate_ph7 (ammonium acetate at pH 7), cobaltihexammine (at the pH of the soil, common in French laboratories), bacl2, kcl, nh4cl or oxalate. The French Référentiel pédologique 2008 reads S/CEC with the CEC by ammonium acetate at pH 7 (p. 50); a CEC by another method stands in for it, flagged, with a lower grade of evidence; not recorded, the CEC is read as reported, with a note (Hugo's decision Q12). | - |
| `redox_relict` | - | logical | TRUE when the redox features of the layer are relict (fossil hydromorphy, the qualifier paléorédoxique), FALSE when they are recorded as functional. The French Référentiel pédologique 2008 asks for a functional hydromorphy (pp. 279, 281) for g, G and the doubles with the rédoxisols: redox features count as functional unless recorded as relict, and reductic features always (Hugo's decision Q13). | - |
| `anthropic_material` | - | character | Kind of anthropic material of the layer, as the materials Z of the French Référentiel pédologique 2008 (pp. 27-28): earthy (Ztr), technological (Ztc), holorganic (ZO), archaeological (Zar), or transported (pedological or geological material moved by people, the suffix -tp). Read for the anthroposols artificiels, construits, archéologiques and reconstitués. Not recorded: the Z codes of the designation, then the artefacts_* attributes (Hugo's decision Q15). | - |
| `anthropic_material_pct` | % (vol) | numeric | Share of the anthropic material in the layer, % by volume. French Référentiel pédologique 2008 (pp. 27-28, 88-98): the anthroposols artificiels have over 50 %, the archéologiques 20 % or more of anthropic debris and artefacts over 50 cm, the qualifier anthropisé under 50 %. Not recorded: artefacts_pct (Hugo's decision Q15). | - |
| `o_zoogenic` | - | character | For an OF or OH horizon: zoogenic (zo: the fine organic matter is made of faecal pellets of the litter fauna) or non-zoogenic (noz), French Référentiel pédologique 2008, Annex 1 (pp. 331-333). Reads OFzo, OFnoz, OHzo and OHnoz, and the humus forms (moder and amphimus against mor). The suffix zo or noz of the designation gives it too; not recorded, the humus form stays undecided between its candidates (Hugo's decision Q22). | - |
| `fine_organic_matter_pct` | % (vol) | numeric | Fine organic matter (holorganic material with no plant debris recognisable by eye), % by volume of an O horizon, fine roots and mycelia excluded (French Référentiel pédologique 2008, pp. 8, 329-333): OL under 10 %, OF 10-70 % (OFr under 30, OFm 30-70), OH over 70 % (OHr 70-90; OHf with recognisable debris under 10 %). Not recorded: the designation (OL, OF, OH; the FAO Oi, Oe, Oa as hints), else 100 minus plant_residues_pct, flagged (Hugo's decision Q23). | - |
| `biogenic_aggregates_pct` | % (vol) | numeric | Biogenic crumb or granular aggregates, % by volume of an A horizon (French Référentiel pédologique 2008, pp. 10-11, 334): over 25 % makes a biomacrostructured or biomesostructured A, 25 % or less a non-crumb A; read by Aca, Aci, Ado, the veracrisols and the humus forms. Not recorded: a moderate or strong crumb or granular structure with worm holes, or a mull humus form, flagged; otherwise undecided (Hugo's decision Q23). | - |
| `biogenic_aggregate_size` | - | character | Size of most biogenic aggregates of an A horizon (French Référentiel pédologique 2008, p. 10): macro (more than half over about 3 mm: a biomacrostructured A) or meso (more than half 1-3 mm: a biomesostructured A). Not recorded: structure_size, flagged (Hugo's decision Q23). | - |
| `effervescence_hot` | - | logical | TRUE when the fine earth effervesces with hot HCl and not, or very weakly, with cold HCl: dolomite. French Référentiel pédologique 2008 (pp. 12, 16): the dolomitic Ado and Sdo and the dolomitic qualifiers. With neither this nor mgco3_pct, Ado and Sdo stay undecided against Aca and Sca (Hugo's decision Q23). | - |
| `mgco3_pct` | % | numeric | Magnesium carbonate, % of the fine earth. With caco3_pct it gives the molar ratio CaCO3/MgCO3 of the French Référentiel pédologique 2008 (pp. 12, 15-16): calcaire over 8, dolomiteux 1.5 to 8, dolomitique under 1.5 (Ado, Sdo). Not recorded: effervescence_hot (Hugo's decision Q23). | - |
| `fe_total_pct` | % Fe2O3 | numeric | Total iron as Fe2O3 (HF digestion, alkaline fusion or XRF), % of the fine earth. French Référentiel pédologique 2008: the ferruginous FE at most 12 % (p. 171), the ND of the ferruginosols 12-20 % and their OXc and OXm over 20 % (p. 172), the FS by free over total iron (pp. 179-180), the Sn by free over total iron 0.60-0.90 (p. 17). The ratios to free iron are read on one basis: Fe = 0.6994 x Fe2O3, so the free over total iron of the Sn (0.60-0.90) and of the FS (above 0.50) is fe_dcb_pct / (0.6994 x fe_total_pct), and FFE over total iron (above 0.20) fe_ffe_pct / (0.6994 x fe_total_pct). fe2o3_sulfuric_pct is not total iron (Hugo's decision Q23). | - |
| `fe_ffe_pct` | % Fe | numeric | Free iron by the method of Ségalen (FFE), % Fe of the fine earth (as fe_dcb_pct, not as Fe2O3). The fersiallitic FS of the French Référentiel pédologique 2008 has FFE over total iron above 0.20 (pp. 179-180), read as fe_ffe_pct / (0.6994 x fe_total_pct) when the total iron is given as Fe2O3. Not recorded: fe_dcb_pct over fe_total_pct above 0.50 (about 0.30 on iron-rich parents) is evidence, flagged (Hugo's decision Q23). | - |
| `weatherable_minerals_pct` | % | numeric | Weatherable minerals other than quartz and muscovite, % of the 20-200 µm fraction. The oxidic OX of the French Référentiel pédologique 2008 has under 10 % (p. 151); also read for the ferrallitisols against the ferruginosols and for the nitosols (Hugo's decision Q23). | - |
| `oxides_triacid_pct` | % | numeric | Sum of the iron, aluminium and titanium oxides (triacid attack or alkaline fusion), % of the fraction under 2 µm. French Référentiel pédologique 2008 (pp. 149-151): the ferrallitic F under 60 % (usually 3-18 %), Fox 30-60 %, the oxidic OX over 60 % (Hugo's decision Q23). | - |
| `clay_mineralogy` | - | character | Dominant clay mineral of the layer: smectite, halloysite, kaolinite, illite, vermiculite (the hydroxy-Al vermiculite of Sal), interstratified or mixed (2:1). French Référentiel pédologique 2008: the nitic Sn (halloysite), the ferrallitic F and Fhy, the fersiallitic FS, Sal, the vertic horizons, the pélosols against the vertisols, the leptismectisols, the qualifiers kaolinitique and smectitique. Not recorded: Ki and Kr and the CEC of the clay as evidence (Hugo's decision Q23). | - |
| `halloysite_pct` | % | numeric | Halloysite, % of the clay fraction. French Référentiel pédologique 2008: the nitic Sn over 50 % (p. 16), the halloysitic Fhy 10-50 % (p. 149); over 50 % confirms the nitosols, 50 % or less rules them out. Not recorded: clay_mineralogy (Hugo's decision Q23). | - |
| `kaolinite_pct` | % | numeric | Kaolinite, % of the clay fraction. French Référentiel pédologique 2008: the ferrallitic F has 1:1 clays 90-100 % (p. 151); the qualifiers kaolinitique. Not recorded: clay_mineralogy (Hugo's decision Q23). | - |
| `smectite_pct` | % | numeric | Smectite, % of the clay fraction. French Référentiel pédologique 2008: swelling clays dominant in the vertic horizons (p. 22), the leptismectisols, the qualifiers smectitique. Not recorded: clay_mineralogy (Hugo's decision Q23). | - |
| `degradation_form` | - | character | Form of the morphological degradation of a BT (French Référentiel pédologique 2008, p. 18): diffuse, interdigitations (E penetrating along the vertical ped faces, too narrow for tongues), tongues, or planosolic. Read by BTd and the luvisols dégradés and derniques, and the qualifiers glossique and à dégradation diffuse. Not recorded: soilKey's retic properties and albeluvic glossae stand in, flagged (Hugo's decision Q23). | - |
| `tongue_vol_pct` | % (vol) | numeric | Tongues of E material in a BT, % of the volume of its affected part. The tongues of the degraded BTd of the French Référentiel pédologique 2008 make over 15 % of it (p. 18). Not recorded: degradation_form (Hugo's decision Q23). | - |
| `tongue_width_mm` | mm | numeric | Smallest horizontal size of the tongues of E material in a BT, mm. The tongues of the degraded BTd of the French Référentiel pédologique 2008 are at least 5 mm (clayey) to 15 mm (silty to sandy) wide (p. 18). Not recorded: degradation_form (Hugo's decision Q23). | - |
| `lamellae_cumulative_cm` | cm | numeric | Cumulative thickness of the clay bands (lamellae) of the layer, cm. A BT of the French Référentiel pédologique 2008 made only of bands needs each band 0.5 cm or more and more than 15 cm in all (p. 17); also the qualifier lamellique. Not recorded: the designation (Hugo's decision Q23). | - |
| `c_pyro_pct` | % | numeric | Organic carbon extractable with 0.1 M sodium pyrophosphate, % of the fine earth. The podzolic BP of the French Référentiel pédologique 2008 has most of its organic matter so extractable (p. 19). Not recorded: soilKey's spodic horizon stands in, flagged (Hugo's decision Q23). | - |
| `pyrophosphate_index` | - | numeric | Pyrophosphate index of a peat (Kaila). French Référentiel pédologique 2008, the histic horizons (pp. 205-206): Hf 7-10, Hm 10-25, Hs over 10 (with the exceptions of its table), Ha and LH over 50. Not recorded: the rubbed fibres, then the von Post index (Hugo's decision Q23). | - |
| `s_total_pct` | % | numeric | Total sulfur, % of the dry mass. French Référentiel pédologique 2008: the thionic material TH 0.75 % or more (p. 27), the sulfated U and the thiosols over 0.75 % (p. 308). Not recorded: sulfidic_s_pct plus sulfate_s_pct as a lower bound (Hugo's decision Q23). | - |
| `nodules_wt_pct` | % | numeric | Metal-oxide (sesquioxidic) nodules and concretions, % by weight of the whole soil. French Référentiel pédologique 2008: the nodular ND over 60 % (p. 154), the suffix -nod and Anod 30-60 % (pp. 151, 155), the qualifier nodulaire. Not recorded: ferric_nodules_pct and bauxitic_nodules_pct (by volume), flagged (Hugo's decision Q23). | - |
| `lateral_cover_pct` | % | numeric | Share of the ground surface the horizon covers, %: the lateral continuity of the OL, OF and OH horizons of the French Référentiel pédologique 2008, Annex 1 (a discontinuous horizon is written in parentheses; the hémimoder has an OH on under 10 %). Not recorded: the parentheses of a recorded sequence, else undecided (Hugo's decision Q22). | - |

## South African system (1991)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `diagnostic_za` | - | character | Name of the diagnostic horizon or material of the South African soil classification of 1991 (Soil Classification Working Group, 1991) that the surveyor gave the layer, as South African descriptions write it beside the master horizon: one of the 5 topsoil horizons (organic O, humic A, vertic A, melanic A, orthic A) or the 25 subsoil horizons and materials (E horizon, G horizon, red apedal B, yellow-brown apedal B, red structured B, neocutanic B, neocarbonate B, pedocutanic B, prismacutanic B, lithocutanic B, podzol B, placic pan, soft plinthic B, hard plinthic B, soft carbonate horizon, hardpan carbonate horizon, dorbank, saprolite, hard rock, stratified alluvium, regic sand, man-made soil deposit, unconsolidated material with or without signs of wetness, unspecified material with signs of wetness), in any case, with or without 'horizon', E or G alone, or soilKey's token (OA, RAB, SPL ...); 'none' or 'non-diagnostic' for a layer that holds none. A negated name (not, no, without, absent) is not read; one written with doubt (?, possibly, probably) is a candidate only; calcrete is read by its adjective (hardpan calcrete the hardpan carbonate horizon, powder or nodular calcrete the soft carbonate horizon, calcrete alone undecided between them). A name of 1977 is read as its 1991 equivalent, flagged (ferrihumic B: podzol B; gleycutanic B: G horizon under an A or E, unspecified material with signs of wetness under a B; calcareous apedal or neocutanic B: neocarbonate B). The recorded name gives the horizon, the recorded data its properties; not recorded, the names written in designation are read, and the identification on the data decides (Hugo's decision Q5). | - |
| `plasticity_index` | % | numeric | Plasticity index (Atterberg: liquid limit minus plastic limit, % water). South African soil classification of 1991: a vertic A has a plasticity index over 32 with the liquid limit by the South African Casagrande cup, over 36 by the British Standard cone; a melanic A 32 or less (36 or less); the method is liquid_limit_method. Not the field class plasticity. Not recorded: the slickensides decide vertic against melanic, flagged (Hugo's decision Q17). | - |
| `liquid_limit_method` | - | character | Method of the liquid limit behind plasticity_index: casagrande_cup (the South African standard) or bs_cone (British Standard cone). South African soil classification of 1991: it picks the vertic A limit, 32 (cup) or 36 (cone). Not recorded: the Casagrande cup, flagged. | - |
| `bedrock_pct` | % (vol) | numeric | Rock in place, fresh or partly weathered, % by volume of the layer. South African soil classification of 1991, family criterion 14: a hard lithocutanic B or saprolite has more than 70 % rock with a hard consistence dry, moist and wet. Not recorded: coarse_fragments_pct or saprolite_pct over 70 with rupture_resistance hard or harder, flagged, the consistence read in the state recorded (Hugo's decision Q19). | - |
| `coatings_hue_moist` | Munsell | character | Moist Munsell hue of the clay or clay-organic coatings on ped faces (with coatings_value_moist and coatings_chroma_moist). South African soil classification of 1991: the continuous black cutans of the Estcourt families (family criterion 12) have a moist value of 2 or less and a chroma of 1 or less, or a neutral hue N; also the dark cutans of a wet prismacutanic B. | - |
| `coatings_chroma_moist` | Munsell | numeric | Moist Munsell chroma of the clay or clay-organic coatings on ped faces. South African soil classification of 1991: the black cutans of family criterion 12 have chroma 1 or less. Not recorded: coatings_value_moist alone, the chroma undecided. | - |
| `coatings_continuity` | - | character | Continuity of the coatings on ped faces: continuous, discontinuous or patchy. South African soil classification of 1991: the black cutans of the Estcourt families (family criterion 12) are continuous on the vertical ped faces. Not recorded: clay_films_amount many, flagged. | - |
| `recent_deposit` | - | logical | TRUE when the layer is a recent aeolian, alluvial or man-made deposit over the soil. South African soil classification of 1991, general rules: a recent deposit under 500 mm thick over a classifiable buried soil is set aside and the buried soil classified; 500 mm or thicker, or over material that is not classifiable soil, the deposit is classified. Not recorded: layer_origin (aeolian, fluvic, colluvial, transported), anthropic_material, or a buried horizon marked (suffix b) directly below, flagged; none of them recorded, not a recent deposit (Hugo's decision Q8). | - |
| `cross_bedding` | - | logical | TRUE when the layer shows cross bedding. South African soil classification of 1991: regic sand may be cross-bedded, an E horizon may not, and cross-bedded aeolian dune sands are regic sand, not an apedal B. Not recorded: stratification_pattern and an aeolian layer_origin, else absent. | - |
| `sand_grade` | - | character | Grade of the sand of the layer: fine, medium or coarse (the sand grades of 1977, which the 1991 system keeps). South African soil classification of 1991: added to the texture class of the A in the name of a soil (Hu1200 medium sandy loam). Read only for the name; not recorded, the texture class without its grade (Hugo's decision Q23). | - |

## German system (KA5)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `bleached_pct` | % | numeric | Wet bleaching (Nassbleichung, KA5 code rb), % of the area of the horizon. German KA5 (Bodenkundliche Kartieranleitung, 5th ed., 2005; ka5): with the rust features it gives the 80 % of the Sw and Sg and the 50 % of the Sd, and the transitions Bv-Sw (10 to < 80 %) and Sw-Bv (< 10 %) (pp. 101-103). Read from hydromorphic_features_ka5 when only the field code is recorded (the area class by the lower end for minima). Not recorded: those tests undecided. | - |
| `reduction_colours_pct` | % | numeric | Reduction colours (KA5 codes rg, rt, rs, and a reduction-coloured matrix), % of the area of the horizon. German KA5 (ka5): Go < 10, Gro 10 to 50, Gor > 50, Gr > 95 %, and Yr (pp. 104-108). Not recorded: a matrix Munsell colour in the Gr colours reads 100, flagged; else undecided. | - |
| `rh_value` | - | numeric | rH value (redox potential with pH). German KA5 (ka5): Sw, Gr, Yo and Yr need rH 19 or less in wet phases (pp. 101-108); the subhydric Gyttja (13 or more), Sapropel (< 13) and Dy (< 19) (p. 256). Not recorded: a positive childs_test (the dipyridyl test) stands in for 'rH 19 or less' only. | - |
| `air_capacity_pct` | % vol | numeric | Air capacity (pores > 50 um drained at field capacity), % by volume. German KA5 (ka5): the Sd and the Sg have under 3 % (pp. 102-103). Not recorded: undecided. | - |
| `total_porosity_pct` | % vol | numeric | Total pore volume, % by volume. German KA5 (ka5): the loose Bfv of the Lockerbraunerde has 60 % or more (or a dry bulk density under 0.8 g/cm3; p. 95). Not recorded: 100 x (1 - bulk density / 2.65), flagged. | - |
| `muscovite_pct` | % | numeric | Muscovite, % of the fine earth. German KA5 (ka5): supporting criterion of the Bv (6 % or more; else the Bu, p. 95). Not recorded: not used. | - |
| `clay_coatings_thin_section_pct` | % | numeric | Clay coatings in thin section, % of the area. German KA5 (ka5): a Bt has more than 1 % (p. 97). Not recorded: clay_films_amount, flagged. | - |
| `clay_bridges` | - | logical | Clay bridges between sand grains visible with a hand lens (TRUE / FALSE). German KA5 (ka5): one of the three signs of the Bt (p. 97). Not recorded: clay_films_amount, flagged. | - |
| `bands_area_pct` | % | numeric | Clay, humus or sesquioxide bands, % of the area of the horizon. German KA5 (ka5): Bbt+Bv, Bbh+Bv, Bbs+Bv and Bbt+Cv 10 to < 50 %, Bv+Bbt and lCv+Bbt over 50 %, Cbtv under 10 % (pp. 96-100); the band subtypes LLd and LFd. Not recorded: lamellae_cumulative_cm over the thickness, flagged. | - |
| `kultotroph` | - | logical | Base and nutrient status lastingly changed by regular fertilising (TRUE / FALSE). German KA5 (ka5): the suffix k of Akh, Aka and Hk (pp. 85, 91-94) and the variety ku. Not recorded: undecided. | - |
| `carbonate_whole_soil_pct` | % | numeric | Carbonate (CaCO3 equivalent) of the whole material, skeleton included, % by mass. German KA5 (ka5): the rock classes i (< 2), e (2 to < 75) and c (75 or more) that separate Ranker, Regosol, Pararendzina and Rendzina (pp. 84, 204-211). Not recorded: the prefix written, else caco3_pct of the fine earth, flagged (Hugo's decision Q15). | - |
| `rock_diggable` | - | logical | The substrate can be dug with a spade when moist (TRUE / FALSE). German KA5 (ka5): the prefixes l (loose) against m and x (pp. 84, 99). Not recorded: cementation_class and rupture_resistance, flagged. | - |
| `parent_rock_ka5` | - | character | Rock of the substrate as the KA5 codes it (field 47a). German KA5 (ka5): the P horizon forms from clay or clay-marl stone, the T horizon from carbonate rock (pp. 100-101). Not recorded: site parent_material. | - |
| `caco3_lithogenic` | - | logical | Lithogenic (primary) carbonate in the fine earth (TRUE / FALSE). German KA5 (ka5): B and T horizons are free of it (pp. 95, 101). Not recorded: caco3_pct 0 reads FALSE, any other value undecided. | - |
| `oc_lithogenic_pct` | % | numeric | Sedimentary (lithogenic) organic carbon, %, the coal classes k1 to k6 of field 45 (Tab. 38). German KA5 (ka5): the prefix o needs 2 % sedimentary organic substance or more (pp. 84, 93, 99). Not recorded: layer_origin with oc_pct, flagged. | - |
| `knick_degree` | - | character | Degree of the Knick horizon of the Knickmarsch: 'strong' (Knick-Horizont) or 'weak' (knickiger Horizont). German KA5 (ka5): the Sq (p. 103) and the type MK. Not recorded: esp_pct and a columnar or prismatic structure, flagged. | - |
| `sand_ffs_pct` | % | numeric | Finest sand (ffS, 63 to < 125 um), % of the fine earth, the KA5 fraction (p. 141); not the USDA very fine sand (50-100 um). German KA5 (ka5): silt plus finest sand 50 % or more in the Sg, the Haftpseudogley and the Haftnaessemarsch. Not recorded: silt 50 % or more reads yes, silt plus sand under 50 % no, the rest undecided (Hugo's decision Q11). | - |
| `soil_gas_ch4_co2_pct` | % vol | numeric | CH4 and CO2 in the soil air, % by volume (the highest measured). German KA5 (ka5): the Y horizons of the Reduktosol have at times 10 % or more (pp. 107-108). Not recorded: site methane_odour, flagged. | - |
| `horizon_continuity` | - | character | Lateral continuity of the horizon: 'continuous' or 'patchy'. German KA5 (ka5): the Ai is patchy and under 2 cm or continuous with humus below the Ah minimum (p. 93); the Aih of the Syrosem-Ranker is continuous. Not recorded: lateral_cover_pct, else undecided. | - |
| `terra_material_pct` | % | numeric | Terra material (solution residue of carbonate rock), % of the fine soil. German KA5 (ka5): 50 % or more in the (Bv-)Tv+cCv of the Terra fusca-Rendzina (p. 209). Not recorded: a Tv written in the interfingering horizon. | - |
| `ec_soil_solution_dS_m` | dS/m | numeric | Electrical conductivity of the soil solution, dS/m (= mS/cm). German KA5 (ka5): the marine Watt has 15 or more (p. 255), against the estuarine. Not recorded: ec_dS_m of the saturation extract, flagged. | - |
| `humus_class_ka5` | - | character | Humus class of field 29 (Tab. 15, p. 112), % organic substance: h0 0, h1 < 1, h2 1 to < 2, h3 2 to < 4, h4 4 to < 8, h5 8 to < 15, h6 15 to < 30 (anmoorig), h7 30 or more (organic). German KA5 (ka5): the Ah minimum, the Aa and the H when no organic carbon is measured; read by its range (lower end for minima, upper end for maxima). | - |
| `carbonate_class_ka5` | - | character | Carbonate class of field 46 (Tab. 40, pp. 168-169), % CaCO3 equivalent of the fine earth: c0 0, c1 < 0.5, c2 0.5 to < 2, c3 2 to < 10 (c3.2 2 to < 4, c3.3 4 to < 7, c3.4 7 to < 10), c4 10 to < 25, c5 25 to < 50, c6 50 to < 75, c7 75 or more. German KA5 (ka5): the classes i, e, c and the decalcification depths when caco3_pct is not measured; read by its range. | - |
| `texture_class_ka5` | - | character | Texture class (Feinbodenart) of the KA5 (Tab. 30, Abb. 17): Ss, Sl2, Sl3, Sl4, Slu, St2, St3, Su2, Su3, Su4, Ls2, Ls3, Ls4, Lt2, Lt3, Lts, Lu, Uu, Uls, Us, Ut2, Ut3, Ut4, Tt, Tl, Tu2, Tu3, Tu4, Ts2, Ts3, Ts4. German KA5 (ka5): the Bv 'one subgroup more clayey', the pure sand of the Strand and the soil-water tables. Not recorded: clay_pct, silt_pct and sand_pct. | - |
| `colour_ka5` | - | character | Colour as the words of Liste 18 code it (bl, bn, ge, gn, ro, gr, sw, we, vi, or, ol, tk, oc; before them h, hh, d, dd, le, fa, sm; after them li; up to three, the last the main colour: roligrbn). German KA5 (ka5): kept as recorded; the book forbids converting the words into Munsell (p. 110), so a Munsell test with only a word recorded stays undecided. | - |
| `hydromorphic_features_ka5` | - | character | Hydromorphic features as the KA5 field codes write them (fields 30 and 31, Tab. 16, p. 113; the rules of 5.3, pp. 50-55): kind (e, ed, eh, eo, es oxidative; rb, rg, rt, rs reductive), form, degree g1-g5, area class f1-f9, size; features separated by ';' (ed,fl,g3,f4;rb,f6). German KA5 (ka5): read into bleached_pct, reduction_colours_pct and the rust share (redoximorphic_features_pct) as the ranges of the area classes. | - |

## Chinese system (CST 2001)

| Attribute | Unit | Type | Description | Accepted aliases |
|-----------|------|------|-------------|------------------|
| `fine_clay_pct` | % | numeric | Fine clay (< 0.2 um), % of the fine earth. Chinese Soil Taxonomy 2001, the argic horizon (pp. 35-36): with 60 % clay or more the illuvial horizon has 8 % more fine clay than the eluvial one. Not recorded: that path stays undecided (Hugo's decision Q20). | - |
| `clay_films_thickness_mm` | mm | numeric | Thickness of the clay films (argillans) on ped faces and in pores, mm. Chinese Soil Taxonomy 2001: the argic horizon (films thicker than 0.5 mm on 5 % or more of the faces, p. 35), the clay-pan and the Argosols order (0.5 mm or more, pp. 38, 75). Not recorded: distinct or prominent films (clay_films_strength) read as thicker than 0.5 mm, flagged (Hugo's decision Q20). | - |
| `humus_clay_films_pct` | % | numeric | Humus-clay films (dark coatings of humus and clay), % of the ped faces and pore walls. Chinese Soil Taxonomy 2001: the agric horizon (5 % or more; evidence 3-5 %, pp. 33-34) and the hydragric horizon. Not recorded: clay_films_amount with a dark coating colour (coatings_value_moist), flagged (Hugo's decision Q20). | - |
| `humus_clay_films_mm` | mm | numeric | Thickness of the humus-clay films, mm. Chinese Soil Taxonomy 2001, the agric horizon (0.5 mm or more, p. 33). Not recorded: undecided (Hugo's decision Q20). | - |
| `micromorph_argillans_pct` | % (area) | numeric | Oriented clay (argillans) in thin section, % of the area. Chinese Soil Taxonomy 2001: the argic horizon (1 % or more, p. 36), the clay-pan (item 5, p. 38) and the secondary clayification of the argic horizon. Not recorded: a required thin-section item counts as met, with the result provisional, unless a record contradicts it (Hugo's decision Q17). | - |
| `micromorph_features` | - | character | Features seen in thin section, words joined by ';': clay_stripes, pseudomorphs (clay pseudomorphs of weathered minerals), freeze_thaw_aggregates, vermiform_voids, fibrous_oriented_clay, layered_sorting, grain_clusters, humus_clay_films. Chinese Soil Taxonomy 2001: the argic horizon, the clay-pan (item 5), the agric horizon and the frost-thawic features (pp. 33-38, 57). Not recorded: as micromorph_argillans_pct (Hugo's decision Q17). | - |
| `tongue_depth_cm` | cm | numeric | Depth to which tongues of albic material reach into the horizon below, cm. Chinese Soil Taxonomy 2001: the albic horizon (tongues under 5 cm, p. 28), the glossic horizon (5 cm or more, p. 29) and the alkalic horizon (2.5 cm or more, p. 38). Not recorded: no tongues (a field observation not recorded is absent). | - |
| `albic_materials_pct` | % (vol) | numeric | Albic materials (bleached, uncoated), % by volume of the horizon. Chinese Soil Taxonomy 2001, the albic horizon (85 % or more, p. 28). Not recorded: the horizon's own colours and data read as 100 %, flagged (Hugo's decision Q20). | - |
| `spodic_materials_pct` | % (vol) | numeric | Spodic materials, % by volume of the horizon. Chinese Soil Taxonomy 2001, the spodic horizon (85 % or more; evidence 50-85 %, pp. 32-33). Not recorded: the horizon's own colours and data read as 100 %, flagged (Hugo's decision Q20). | - |
| `cracked_coatings_pct` | % | numeric | Sand grains with cracked organic-metallic coatings, % of the grains. Chinese Soil Taxonomy 2001, the spodic materials (p. 32). Not recorded: absent. | - |
| `odoe` | - | numeric | Optical density of the acid oxalate extract (ODOE). Chinese Soil Taxonomy 2001, the spodic materials (0.25 or more, p. 33). Not recorded: that path stays undecided. | - |
| `pyrophosphate_color` | Munsell | character | Munsell value and chroma of the sodium pyrophosphate extract on white chromatographic or filter paper, as value/chroma ('7/2') or with a hue ('10YR 7/2'). Chinese Soil Taxonomy 2001, the fibric and sapric soil materials (Fig. 3.1, pp. 43-45). Not recorded: the rubbed fibre content alone decides (Hugo's decision Q20). | - |
| `root_mat_pct` | % (vol) | numeric | Interwoven live and dead roots, % by volume. Chinese Soil Taxonomy 2001: the mattic epipedon (50 % or more; evidence 10-50 %, pp. 22-23) and mattic soil material (10 % or more, p. 45). Not recorded: undecided. | - |
| `root_mat` | - | logical | An elastic, felt-like mat of interwoven roots at the surface (TRUE / FALSE). Chinese Soil Taxonomy 2001, the mattic epipedon of alpine meadows (p. 22). Not recorded: absent. | - |
| `reduced_matrix_pct` | % (vol) | numeric | Matrix in the reduced (gley) colours of the gleyic features, % by volume of the horizon. Chinese Soil Taxonomy 2001: the gleyic features (more than 50 %, p. 55), the gleyic evidence (30-50 %, p. 55) and the redoxic features (under 30 %, p. 56). Not recorded: the horizon's matrix colour read as 100 %, flagged (Hugo's decision Q20). | - |
| `ferricyanide_test` | - | character | Potassium ferricyanide test for ferrous iron on a freshly broken field-wet surface: positive (solid dark blue) or negative. Chinese Soil Taxonomy 2001, the gleyic features (item 3a, p. 55); item 3b, alpha,alpha'-dipyridyl, is childs_test. Not recorded: gley colours without a test give the gleyic features provisionally (Hugo's decision Q11). | - |
| `eh_mv` | mV | numeric | Field redox potential (Eh), mV. Chinese Soil Taxonomy 2001, the gleyic features: rH = Eh/29 + 2 pH, 19 or less (item 4); gleyic evidence 20-25 (pp. 55-56). Not recorded: undecided (Hugo's decision Q11). | - |
| `humic_infillings` | - | logical | Cracks or pores filled with humic material or films fallen from the horizons above (TRUE / FALSE). Chinese Soil Taxonomy 2001: the humic property (p. 59) and the vertic features (self-swallowing, p. 49). Not recorded: absent. | - |
| `humus_coatings` | - | character | Humus coatings on ped faces or pore walls in the B: none, few, common or many. Chinese Soil Taxonomy 2001, the humic property (item 1, p. 59). Not recorded: a dark coating colour on the B faces (coatings_value_moist 3 or less) or humic_infillings, flagged (Hugo's decision Q20). | - |
| `silting_sieve_test` | - | character | Sieve test of irrigation silting (laminae of silt deposited by irrigation water): positive or negative. Chinese Soil Taxonomy 2001: the siltigic epipedon (pp. 24-25) and the anthro-silting materials (pp. 48-49). Not recorded: absent; the WRB irragric horizon may stand in, flagged (Hugo's decision Q20). | - |
| `puddled` | - | logical | The cultivated layer is puddled (tilled under water) for rice (TRUE / FALSE). Chinese Soil Taxonomy 2001, the anthrostagnic epipedon (item 3, p. 26). Not recorded: site$flood_irrigated or a paddy land use, flagged (Hugo's decisions Q12, Q20). | - |
| `p_olsen_mg_kg` | mg/kg | numeric | Olsen phosphorus (0.5 M NaHCO3 at pH 8.5), mg P per kg. Chinese Soil Taxonomy 2001: the fimic epipedon (35 or more; evidence 18-35, pp. 25-26) and the phos-agric horizon (18 or more and much higher than below, p. 34). Mehlich-3 P (p_mehlich3_mg_kg) is never read in its place (Hugo's decision Q16). | - |
| `p_total_pct` | % | numeric | Total phosphorus as P (not P2O5; P = 0.437 x P2O5), % of the fine earth. Chinese Soil Taxonomy 2001: the phosphipan (30 g/kg or more, p. 41) and the phosphic property (15-130 g/kg; evidence 1.5-15 g/kg, p. 63). Not recorded: undecided. | - |
| `k_total_pct` | % | numeric | Total potassium as K (not K2O; K = 0.83 x K2O), % of the fine earth. Chinese Soil Taxonomy 2001, the ferralic horizon (under 8 g/kg, item 4, p. 31). Not recorded: the weatherable minerals decide (weatherable_minerals_pct, flagged for its fraction, Hugo's decision Q16). | - |
| `ec_1to1_dS_m` | dS/m | numeric | Electrical conductivity of the 1:1 soil:water extract, dS/m. Chinese Soil Taxonomy 2001, the salic horizon (30 or more in arid regions, 15 or more elsewhere; product with the thickness 900 or more, p. 42). The saturated-paste EC (ec_dS_m) and the 1:5 EC are never converted to it (Hugo's decision Q13). | - |
| `sio2_al2o3_clay` | - | numeric | SiO2/Al2O3 molar ratio of the clay fraction (alkali fusion), or of the fine earth by HCl-HNO3-H2SO4 digestion. Chinese Soil Taxonomy 2001, the allitic property (2.0 or less, p. 62). Not recorded: the Ki of the sulfuric attack (sio2_sulfuric_pct, al2o3_sulfuric_pct), flagged (Hugo's decision Q16). | - |
| `sio2_al2o3_naoh` | - | numeric | SiO2/Al2O3 molar ratio extracted from the fine earth by hot 0.5 M NaOH. Chinese Soil Taxonomy 2001, the allitic property (1.0 or less, item 2, p. 62). Not recorded: that path stays undecided. | - |
| `trb_cmol` | cmol(+)/kg | numeric | Total reserve in bases (exchangeable and mineral Ca, Mg, K and Na) of the soil, cmol(+)/kg. Chinese Soil Taxonomy 2001, the alic property (item 2, p. 62), with trb_clay_cmol. Not recorded: the silt/clay ratio path decides. | - |
| `trb_clay_cmol` | cmol(+)/kg | numeric | The part of the total reserve in bases held in the clay fraction, cmol(+) per kg of soil. Chinese Soil Taxonomy 2001, the alic property: 80 % or more of trb_cmol (item 2, p. 62). Not recorded: the silt/clay ratio path decides. | - |
| `gravel_carbonate_coating` | - | character | Carbonate coating on gravel and stones: none, partial, bottom (pendants under the stones) or complete. Chinese Soil Taxonomy 2001, the argic horizon formed by secondary clayification (p. 37). Not recorded: that item is skipped (Hugo's decision Q20). | - |
| `plinthite_hardens` | - | logical | The plinthite hardens irreversibly on repeated wetting and drying (TRUE / FALSE). Chinese Soil Taxonomy 2001, the plinthic horizon (p. 32). Not recorded: recorded plinthite (plinthite_pct) is read as meeting it. | - |
| `rock_fragment_max_cm` | cm | numeric | Size of the largest rock fragments, cm. Chinese Soil Taxonomy 2001, the lithologic character of purplish sandstones and shales (fragments under 4 cm, p. 47). Not recorded: undecided. | - |
| `shajiang_pct` | % (vol) | numeric | Shajiang (irregular calcareous nodules of the North China plains), % by volume. Chinese Soil Taxonomy 2001: the Shajiang groups and subgroups (F1.1.2, L4.3.2, M2.2). Not recorded: hard carbonate concretions (carbonate_hard_pct, or secondary_carbonates_pct with a hard form), flagged (default 13 of 2026-11-04). | - |
| `fe_mn_nodules_pct` | % (vol) | numeric | Iron-manganese nodules and concretions, % by volume. Chinese Soil Taxonomy 2001, the Concretic subgroups (30 % or more, L4.1.1). Not recorded: the sum of ferric_nodules_pct and manganiferous_nodules_pct, flagged. | - |
| `pyroclastic_fragments_pct` | % (vol) | numeric | Cinders, scoria and pumice, % by volume of the layer. Chinese Soil Taxonomy 2001: the cindery and pumiceous materials of the Histosols order (p. 72) and the Cinderic subgroups (35 % or more over 0-25 cm). Not recorded: a tephric layer_origin or volcanic_glass_pct, flagged (default 21 of 2026-11-04). | - |
| `soluble_salts_sodium_pct` | % | numeric | Sodium share of the cations of the water-soluble salts, % of the sum of the soluble cations (in charge). Chinese Soil Taxonomy 2001, the Marinic subgroups (chloride the main anion, sodium the main cation, pp. 110-111). Not recorded: na_cmol or esp_pct, flagged (Hugo's decision Q18). | - |

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
| `lat` | decimal degrees | numeric | Latitude (-90 to 90; validate() flags a value outside). When the country is not Brazil, \|lat\| <= 23.5 marks the site as tropical, which lets a Bw or Bo designation stand for the WRB ferralic horizon (and so the USDA oxic horizon) and the SiBCS B latossolico; the USDA family soil temperature class estimates the regime from it when soil_temperature_regime is absent. Also read by the SoilGrids prior, the maps and the reports. | - | `wrb2022`, `usda`, `sibcs5`, `cpcs1967` |
| `lon` | decimal degrees | numeric | Longitude (-180 to 180; validate() flags a value outside). Read by the SoilGrids prior, the maps and the reports; no key reads it. | - | - |
| `crs` | EPSG code | numeric | Coordinate reference system of lat and lon (4326, WGS 84, when absent). Read by the SoilGrids buffer and the GeoPackage export; no key reads it. | - | - |
| `country` | - | character | Country of the site, as an ISO code or a name. BR, BRA, Brazil or Brasil marks the site as tropical for the WRB ferralic horizon (and the USDA oxic horizon) and the SiBCS B latossolico. Without designation_system, CA, AU and NZ (or their ISO-3 codes and English names) tell the CSSC, ASC and NZSC keys that the designations may be in their notation, and another country tells the CSSC key that they are not. | - | `wrb2022`, `usda`, `sibcs5`, `cssc3`, `asc3`, `nzsc4` |
| `date` | - | character | Date of the description (ISO 8601, YYYY-MM-DD; the fixtures store a Date). Shown by print() and the reports; no key reads it. | - | - |
| `year` | - | numeric | Year of the description. Read only by the GeoPackage export (pedon_point layer); no loader writes it. | - | - |

## Landscape & surface

| Property | Unit | Type | Description | Values | Read by |
|----------|------|------|-------------|--------|---------|
| `elevation_m` | m | numeric | Elevation above sea level. Read by the USDA family soil temperature class, which estimates the regime from lat and elevation (0 m when absent) when soil_temperature_regime is not given; also shown by the reports. The CPCS reads 1,500 m or more as the altitude of the alpine podzols (VIII/22). | - | `usda`, `cpcs1967` |
| `slope_pct` | % | numeric | Slope gradient in percent, not degrees. Read by the WRB Inclinic qualifier (5 % or more) and the USDA Cumulic subgroups of the Mollisols (less than 25 %, or not recorded); also shown by the reports. | - | `wrb2022`, `usda`, `cpcs1967` |
| `aspect_deg` | deg | numeric | Slope aspect in degrees clockwise from true north (0 to 360). Filled by the field-sheet extraction (VLM); no key reads it. | - | - |
| `landform` | - | character | Landform, as text (the landform pattern in NatSoil). Read by the ASC Hydrosol settings (tidal, coastal, lake, playa and subaqueous landforms) and by the WRB Gilgaic and Escalic qualifiers (gilgai, built terraces). The Russian Field Guide of 2008 reads a river floodplain (floodplain, levee, backswamp, пойма) for its alluvial soils; a valley, an alluvial plain or a delta leaves them undecided, and a terrace (above the floodplain or not) or another landform recorded rules them out. | - | `wrb2022`, `asc3`, `scsj2017`, `cpcs1967`, `kidpr2008` |
| `landform_element` | - | character | Landform element, as text (NatSoil). Read with landform by the ASC Hydrosol settings; a playa element on a plain pattern counts as a playa. Read with landform by the Russian Field Guide of 2008 (a river floodplain). | - | `asc3`, `cpcs1967`, `kidpr2008` |
| `microrelief_form` | - | character | Microrelief at the soil surface, as text (NatSoil writes its microrelief types); also a horizon column of the same name. Read by the WRB Gilgaic qualifier (a recorded microrelief that names no gilgai rules it out) and the Escalic qualifier (built terraces). | - | `wrb2022`, `cpcs1967` |
| `patterned_ground` | - | character | Patterned ground of frost action at the site, as text: polygons (earth, stone, ice-wedge, low-centre), circles, nets, stripes, mud boils, ice-cored mounds, hummocks, thufurs, palsas, or none. Read by the CPCS groups of the cold-desert raw soils (I/22-23) and of the permafrost soils (II/11-14): a network of polygons, circles, stripes, mud boils or cracks is an organised form; hummocks, thufurs and palsas alone are not. Not recorded: the group is undecided. | - | `cpcs1967` |
| `gilgai_presence` | - | logical | Gilgai microrelief present (TRUE) or absent (FALSE). Read first by the WRB Gilgaic qualifier (Vertisols only), before the relief texts. | - | `wrb2022` |
| `forma_relevo` | - | character | Relief form, as text (Portuguese field name). Read beside microrelief_form by the WRB Gilgaic and Escalic qualifiers; no loader writes it. | - | `wrb2022` |
| `relevo_local` | - | character | Local relief, as text (Portuguese field name). Read beside microrelief_form by the WRB Gilgaic qualifier; no loader writes it. | - | `wrb2022` |
| `relief_form` | - | character | Relief form, as text. Read beside microrelief_form by the WRB Gilgaic and Escalic qualifiers; no loader writes it. | - | `wrb2022` |
| `surface_condition` | - | character | Condition of the soil surface, as text with items separated by ';' ('cracking; self-mulching'). Read by the ASC key: surface cracks for the Vertosols, self-mulching and crusts for the Vertosol great groups, and a saline or salt surface for the Salic Hydrosols and the Hypersalic and Hypergypsic suborders. | - | `asc3`, `cpcs1967` |
| `surface_gravel_pct` | % | numeric | Gravel on the ground surface (NaN when recorded with no abundance). Read by the ASC family gravel class of the surface and A1, which takes the larger of it and the gravel of the A1. | - | `asc3` |
| `surface_coarse_fragments_pct` | % | numeric | Coarse fragments of all sizes on the ground surface (NaN when recorded with no abundance). Read by the ASC family gravel class in place of surface_gravel_pct when that is not given. | - | `asc3` |
| `salinised` | - | logical | The site is salinised (TRUE) or not (FALSE). Read by the ASC key: the Salic Hydrosol setting, and the salinity of the Hypersalic and Hypergypsic suborders when no electrical conductivity is recorded. | - | `asc3` |
| `parent_material` | - | character | Parent material, as text. Read by the WRB Rendzic qualifier (limestone, chalk, dolomite, carbonate or marl) and the NZSC Melanic Soils (calcareous, mafic or ultramafic rock); the Russian Field Guide of 2008 reads alluvium for its alluvial soils (glaciofluvial material, outwash, moraine and marine material are none); also shown by print() and the reports. | - | `wrb2022`, `nzsc4`, `scsj2017`, `cpcs1967`, `kidpr2008` |
| `land_use` | - | character | Land use, as text. Read by the WRB Drainic (artificial drainage), Tidalic (tidal, salt marsh, mangrove) and Escalic (built terraces) qualifiers; the French Référentiel pédologique 2008 reads crops or grassland for the open-land humus forms of Annex 1 (agri-mull, mull de pelouse ...; p. 337). | - | `wrb2022`, `scsj2017`, `cpcs1967`, `rp2008` |
| `vegetation` | - | character | Vegetation, as text. Written by the fixtures and the field-sheet extraction (VLM); read only beside land_use by the open-land humus forms of the French Référentiel pédologique 2008 (p. 337). | - | - |
| `contaminated` | - | logical | A layer within 50 cm exceeds a contamination or toxicity limit of the soil regulation in force. HSCS 2024 subtype szennyezett (Anthropogenic soils). For the Russian Field Guide of 2008, contamination recorded without the hazard category of the layers (contamination_hazard) leaves the contaminated horizon X, and the chemozems, undecided. | - | `hscs2024`, `kidpr2008` |
| `terraced` | - | logical | The surface is an artificial terrace. HSCS 2024 variety teraszolt. | - | `hscs2024` |
| `flood_irrigated` | - | logical | The soil is under flood-irrigated cultivation (e.g. rice). HSCS 2024 variety árasztott. | - | `hscs2024`, `scsj2017` |
| `wind_deflation` | - | logical | The surface is bare, structureless single grains with marks of wind reworking (deflation). HSCS 2024 subtype futó (Sandy soils). | - | `hscs2024`, `cpcs1967` |

## Water, climate & regimes

| Property | Unit | Type | Description | Values | Read by |
|----------|------|------|-------------|--------|---------|
| `drainage_class` | - | character | Drainage class, in English or Portuguese; a range ('imperfectly to poorly drained') is read by the end each test asks for. Read by the WRB Drainic, Subaquatic and Tidalic qualifiers (which also look for words such as 'artificial', 'submerged' and 'tidal'), the SiBCS horizonte glei (after drainage), the CSSC Folisol and Vertisol great groups, the ASC key (Hydrosols, organic materials, Folic Organosols, Aquic Vertosols, the Podosol and Hypersalic suborders) and the NZSC Organic Soils. | `excessively drained`, `somewhat excessively drained`, `rapidly drained`, `well drained`, `moderately well drained`, `imperfectly drained`, `somewhat poorly drained`, `poorly drained`, `very poorly drained`, `excessivamente drenado`, `fortemente drenado`, `acentuadamente drenado`, `bem drenado`, `moderadamente drenado`, `imperfeitamente drenado`, `mal drenado`, `muito mal drenado` | `wrb2022`, `sibcs5`, `cssc3`, `asc3`, `nzsc4`, `scsj2017` |
| `drainage` | - | character | Drainage class as the BDsolos export writes it (classe de drenagem, in Portuguese), on the same scale as drainage_class. The SiBCS horizonte glei reads it before drainage_class; the CSSC drainage groups read it when drainage_class is absent. | `excessively drained`, `somewhat excessively drained`, `rapidly drained`, `well drained`, `moderately well drained`, `imperfectly drained`, `somewhat poorly drained`, `poorly drained`, `very poorly drained`, `excessivamente drenado`, `fortemente drenado`, `acentuadamente drenado`, `bem drenado`, `moderadamente drenado`, `imperfeitamente drenado`, `mal drenado`, `muito mal drenado` | `sibcs5`, `cssc3`, `scsj2017` |
| `soil_moisture_regime` | - | character | USDA soil moisture regime, compared in lower case (aridic and torric are the same). Read by the USDA key (the Aridisols, and the ustic, xeric, udic and torric suborders, great groups and subgroups) and by the CSSC key when soil_moisture_subclass is absent (Chernozemic climate, the subhumid Solonetzic subgroups). The Chinese Soil Taxonomy 2001 adds the stagnic (perched water), anthrostagnic (irrigated paddy) and peraquic (water table always at or near the surface) regimes (pp. 51-55); the USDA key reads all three as aquic. | `aquic`, `aridic`, `torric`, `udic`, `perudic`, `ustic`, `xeric`, `stagnic`, `anthrostagnic`, `peraquic` | `usda`, `cssc3`, `cpcs1967` |
| `soil_moisture_subclass` | - | character | Canadian soil moisture subclass (letters only are compared, so 'sub-humid' reads as subhumid; the aquic subclasses leave the tests undecided). Read by the CSSC key: the Chernozemic climate (subhumid or drier passes, humid and perhumid fail) and the A-horizon colour of the Solonetzic subgroups (subhumid). Filled by npdb_scenario() for NPDB pedons. | `arid`, `subarid`, `semiarid`, `subhumid`, `humid`, `perhumid` | `cssc3` |
| `soil_temperature_regime` | - | character | USDA soil temperature regime, compared in lower case. Read by the USDA key (the cryic suborders and great groups, and the family temperature class, which is otherwise estimated from lat and elevation_m) and by the CSSC key when mean_annual_soil_temp_C is absent (Gray Brown Luvisols: mesic or thermic; Chernozemic climate: not pergelic or gelic). The Chinese Soil Taxonomy 2001 adds the permagelic regime (soil perennially at or below 0 C, p. 56), which the USDA key reads as gelic. | `gelic`, `cryic`, `frigid`, `mesic`, `thermic`, `hyperthermic`, `isofrigid`, `isomesic`, `isothermic`, `isohyperthermic`, `pergelic`, `permagelic` | `usda`, `cssc3`, `scsj2017`, `cpcs1967` |
| `frost_regime` | - | character | Frost regime of the soil: none, seasonal (frozen for part of the winter), long-seasonal (frozen for most of the year, no permafrost) or permafrost. For the Russian Field Guide of 2008, whose key position 26 asks for long-seasonal or perennial frost in or under the profile (the cryometamorphic, cryogenic, humus-cryometamorphic and pale-metamorphic horizons; the cryo-humus AK with permafrost). Not recorded: permafrost from the horizon data (permafrost_temp_C, ice_pct, ice_form, the permafrost sign of the notation) or a gelic or pergelic soil_temperature_regime; long-seasonal frost is then undecided (Hugo's decision Q24). | `none`, `seasonal`, `long-seasonal`, `permafrost` | `kidpr2008` |
| `aridity_index` | - | numeric | Annual aridity by Penman's empirical formula as the Chinese Soil Taxonomy 2001 gives it: D = ET / P, with ET the annual potential evapotranspiration (pan evaporation times 0.6 from November to February, 0.8 from May to August, 0.7 in the other months) and P the annual precipitation (p. 52). Over 3 is the aridic regime (3 to 3.5 too, where the ustic range of 1 to 3.5 overlaps it), 1 to 3 ustic, under 1 udic or perudic. Read by the CST moisture regime when soil_moisture_regime is not recorded, and for the arid regions of the salic horizon (Hugo's decisions Q5 and Q14). | - | - |
| `aridity_index_max_month` | - | numeric | The highest monthly Penman aridity of the year (the same formula, month by month). The Chinese Soil Taxonomy 2001 separates the perudic regime (every month under 1) from the udic one (annual aridity under 1, not every month) by it (p. 53); not recorded, an annual aridity under 1 leaves the two undecided (Hugo's decision Q5). | - | - |
| `climatic_moisture_regime` | - | character | The moisture regime of the region (the climate), as distinct from the soil's own: aridic (or torric), ustic, udic or perudic. The Chinese Soil Taxonomy 2001 reads arid regions for the salic horizon and some Parasalic subgroups (p. 42) and the ustic region of the F1 groups; read before aridity_index and soil_moisture_regime for the region (Hugo's decision Q14). | `aridic`, `torric`, `ustic`, `udic`, `perudic` | - |
| `udic_perudic_border` | - | logical | A udic regime bordering on the perudic one (TRUE / FALSE), which the Chinese Soil Taxonomy 2001 asks for in the Xanthi- and Xanthic taxa (E1.2, E1.3.4, K3, L4.4.4, M5.4.4) and does not define. A perudic soil moisture regime also passes; not recorded, the test is undecided (Hugo's decision Q8). | - | - |
| `mean_summer_soil_temp_C` | deg C | numeric | Mean soil temperature of the summer (June to August) at 50 cm, or at a lithic or paralithic contact if shallower. The Chinese Soil Taxonomy 2001 separates the cryic regime from the frigid one by it (under 15 C, 8 C with an O horizon; 13 C and 6 C when saturated in summer, p. 57); not recorded, the two are told apart only by a recorded regime (Hugo's decision Q6). | - | - |
| `summer_saturated` | - | logical | The soil is saturated with water during some period of the summer (TRUE / FALSE), for the cryic regime of the Chinese Soil Taxonomy 2001 (p. 57). Not recorded: the summer temperature decides only where both limits agree. | - | - |
| `cracks_open_days` | days per year | numeric | Cumulative days per year with cracks 5 mm or more wide through a layer 25 cm or more thick within 50 cm of the mineral soil surface. The Chinese Soil Taxonomy 2001 reads it for the Ustic Vertosols (90 days or more) and the Aridic subgroups (210 days or more, pp. 53, 96-99). Not recorded: a recorded ustic regime with observed cracks passes the 90 days, provisionally; the 210 days only with an aridic regime (Hugo's decision Q15). | - | - |
| `irrigated` | - | logical | The land is irrigated (TRUE / FALSE). The Chinese Soil Taxonomy 2001 keys the Ustic Cambosols of aridic soils made ustic by irrigation (M3). flood_irrigated TRUE implies it (default 10 of 2026-11-04). | - | - |
| `freeze_thaw_days_pct` | % of days | numeric | Days of the year whose air temperature passes through 0 C (diurnal freezing and thawing), % of the days. The Chinese Soil Taxonomy 2001, the frost-thawic features (70 % or more, item 4, p. 57). Not recorded: that item is undecided. | - | - |
| `surface_crust_thickness_cm` | cm | numeric | Thickness of the surface crust (the vesicular or takyric crust), cm. The Chinese Soil Taxonomy 2001: the aridic epipedon (a vesicular crust of 0.5 cm or more over a platy layer, pp. 27-28) and the takyric features (a crust 1-4 cm thick, G1.4.2, G2.5.4, H1.1). Not recorded: the thickness of the top horizon when it has vesicular pores or cracks, flagged (default 14 of 2026-11-04). | - | - |
| `silting_rate_cm_yr` | cm per year | numeric | Rate of deposition of anthropic silt, cm per year. The Chinese Soil Taxonomy 2001, the anthro-silting materials (0.5 cm or more per year by irrigation, 10 cm or more by interception of runoff, pp. 48-49). Not recorded: undecided. | - | - |
| `silting_kind` | - | character | Kind of anthropic silting: irrigation (sediment-rich irrigation water) or interception (runoff intercepted behind dams or terraces). The Chinese Soil Taxonomy 2001, the anthro-silting materials (pp. 48-49). | `irrigation`, `interception` | - |
| `paddy_years_frac` | fraction | numeric | Share of the years under submerged rice cultivation (0 to 1). The Chinese Soil Taxonomy 2001 gives the anthrostagnic evidence to a shorter paddy history than the anthrostagnic epipedon (pp. 26-27). Not recorded: undecided. | - | - |
| `lithic_contact_extent_pct` | % of the plane | numeric | Share of the horizontal plane at the depth of the lithic contact that the contact occupies, %. The Chinese Soil Taxonomy 2001 asks for a lithic contact in more than half of the plane in the karst taxa (M4.3.1 and others). Not recorded: a recorded contact counts, provisionally (default 22 of 2026-11-04). | - | - |
| `mean_annual_soil_temp_C` | deg C | numeric | Mean annual soil temperature. Read by the CSSC key: the Chernozemic climate (0 degrees or more) and the Gray Brown Luvisols (8 degrees or more). Filled by npdb_scenario() for NPDB pedons. | - | `cssc3`, `scsj2017` |
| `soil_temp_30cm_C` | deg C | numeric | Mean annual soil temperature at 30 cm from the mineral soil surface. Read by the NZSC Hydrothermal Recent and Raw Soils (2.5 degrees or more above the mean annual air temperature). | - | `nzsc4` |
| `mean_annual_air_temp_C` | deg C | numeric | Mean annual air temperature. Read by the NZSC Hydrothermal Recent and Raw Soils, against soil_temp_30cm_C. | - | `nzsc4` |
| `koppen_climate` | - | character | Koppen climate of the site, as its code (Af, Aw, BWh, BSk, Csa, Cfb, Dfa, Dfc, ET ...). Read first by the CPCS sub-classes, whose classes are split by pedoclimate (cold or hot deserts, cold or tropical countries, humid, continental or boreal temperate climates); without it the CPCS reads the USDA soil temperature and moisture regimes, then the latitude. | - | `cpcs1967` |
| `mean_annual_precip_mm` | mm | numeric | Mean annual precipitation. Recorded for the CPCS climate (the ferrallitic sub-classes describe their rainfall); not yet a criterion of any key. | - | `cpcs1967` |
| `surface_water` | - | character | How often water stands on the soil surface. Read by the NZSC Hydric Raw Soils (surface water for the majority of the time, but not constantly). | `constant`, `most of the time`, `occasional`, `none` | `nzsc4` |
| `artificially_drained` | - | logical | The site is artificially drained. Read by the NZSC peaty topsoil, whose saturation requirement then does not apply. | - | `nzsc4`, `scsj2017` |
| `groundwater_kind` | - | character | Kind of water table recorded at the site (FAO): none observed, observed, apparent, perched or flooded. Read with groundwater_depth_cm by the WRB gleyic and stagnic properties: mottles over a layer with no reductimorphic matrix are gleyic where groundwater that is not perched lies within 150 cm, stagnic where none was observed or it is perched, and undecided where the site does not say. Written by isis_profile_to_pedon(). The CPCS groups read a perched water table, or none, as a stagnogley (XI/33, VIII/32) and any other as a gley (XI/31). | `none observed`, `observed`, `apparent`, `perched`, `flooded` | `wrb2022`, `cpcs1967` |
| `groundwater_depth_cm` | cm | numeric | Depth of the water table below the soil surface (0 at the surface). Read with groundwater_kind by the WRB gleyic and stagnic properties (within 150 cm, or deeper). Written by isis_profile_to_pedon() from depthGroundwater, or groundwaterTop where ISIS writes 0 for the depth. | - | `wrb2022`, `scsj2017`, `cpcs1967` |
| `water_table_cm_above_surface` | cm | numeric | Height of a water table above the soil surface (positive above it). Read by the USDA key: the Wassents (above 0) and the Wassists (2 cm or more). | - | `usda` |
| `tidal_zone` | - | character | Tidal zone of the site. Read by the ASC Hydrosol settings (Subtidal, Intertidal, Supratidal and Extratidal suborders); a zone named in landform is read too. | `subtidal`, `intertidal`, `supratidal`, `extratidal` | `asc3` |
| `tidal_inundation` | - | logical | The site is subject to tidal flooding. Read by the ASC key: it makes the soil a Hydrosol, rules out the Organosols, and without tidal_zone leaves the tidal Hydrosol settings undecided. | - | `asc3` |
| `subaqueous` | - | logical | An inland water body lies over the soil. Read by the ASC Subaqueous Hydrosol setting, before the landform text. | - | `asc3` |

## Description & notation

| Property | Unit | Type | Description | Values | Read by |
|----------|------|------|-------------|--------|---------|
| `designation_system` | - | character | Notation of the horizon designations. Read by the CSSC, ASC and NZSC keys to decide whether the designations are in their notation (otherwise judged from the designations and the country), and by the CPCS key ('cpcs': the old French notation of 1967, otherwise recognised by its symbols, never by the country). The Russian notation of the Field Guide of 2008 ('kidpr', 'ru', 'russian', any value starting with kidpr, ru, rfg or rsc) is otherwise recognised by an index only it writes (AY, EL, BHF, BCA, PU, TUR ...), never by the country: Russian legacy data use the Soviet notation of 1977, read only as hints. The notation of the French Référentiel pédologique 2008 ('rp', 'rp2008', 'referentiel', 'afes', any value starting with rp, referentiel or afes; 'rp1995' or 'rp95' for the RP of 1995, where FE is the ferric horizon) is otherwise recognised by a code only it writes (OL, OF, OH, BP, LA, SV, FS, Sca, Aca, Kc, Js ...) or two kinds that it alone usually writes, with no symbol only the CPCS, the FAO or the USDA writes, in a profile not written all in capitals, never by the country; 'french' and 'fr' stay the CPCS. The South African notation of 1991 ('za', 'za1991', 'sat', 'sat1991', 'south africa', 'south african', any value starting with south africa) lets the names of its diagnostic horizons written in the designations be read ('Ap orthic A', 'B1 red apedal'), as they are when the profile records a name in diagnostic_za or when a designation holds a name only that system writes (orthic A, red apedal, yellow-brown apedal, neocutanic, dorbank ...); 'SA' and 'RSA' name the country, not a notation, and declare nothing; never inferred from the country, since South African sites are also described in the FAO notation. The notation of the German Bodenkundliche Kartieranleitung (KA5, 2005: 'ka5', 'kartieranleitung', 'bodensystematik', 'agboden', 'dbg', any value starting with them; 'ka3' and 'ka4' read as KA5 with a note, the G horizons having been redefined in 2005; 'ka6' read through the KA6 symbols with a one-to-one KA5 reading) is otherwise recognised by one strong sign (the degree sign, a lower-case prefix before a master: lC, fAh, aM, tmGo; a base-saturation mark; Sw, Sd, Axh, Ael, Bfv, Gor, Tv ...) or two likely ones of two kinds (Bv, Cv, Cn, Al, Aa, Hm, Hw, Hr, P, T), with no symbol only the FAO, USDA, CSSC, RP, CPCS or Russian notation writes, never by the country. Written by the NatSoil ('asc') and NPDB ('cssc') loaders. | `cssc`, `cssc3`, `canadian`, `canada`, `asc`, `australian`, `field handbook`, `nchs`, `australian field handbook`, `nzsc`, `nz`, `new zealand`, `cpcs`, `cpcs1967`, `french`, `kidpr`, `kidpr2008`, `ru`, `ru2004`, `ru2008`, `russian`, `russia`, `rfg2008`, `rsc2004`, `rp`, `rp2008`, `rp1995`, `referentiel`, `afes`, `za`, `za1991`, `sat`, `sat1991`, `south africa`, `south african`, `south_africa`, `southafrican`, `ka5`, `ka4`, `ka3`, `ka6`, `kartieranleitung`, `bodensystematik`, `agboden`, `dbg` | `cssc3`, `asc3`, `nzsc4`, `scsj2017`, `cpcs1967`, `kidpr2008`, `ka5` |
| `humus_form` | - | character | Humus form of the site, as the French and European typologies name it: mull, moder, mor (humus brut), hydromull, hydromoder, hydromor, anmoor, tourbe (peat). Read by the CPCS subgroups of the rankers (II/211-213: mor or moder; not recorded, the subgroup is undecided) and of the humic gley soils (XI/212-214: anmoor or hydromor decides before the C/N). The French Référentiel pédologique 2008 (Annex 1) adds its forms: eumull, mésomull, oligomull, dysmull, hémimoder, eumoder, dysmoder, hémimor, humimor, amphimus, euamphimus, the forms without fine earth (peyromull, lithomoder, peyromoder, lithomor, peyromor, tangel, peyro-amphimus) and of open land (agri-mull, agri-moder, agri-mor, mull, moder and mor de pelouse, amphimus de pelouse); a recorded form prevails where the horizons leave the form of humus_form_rp() undecided, and a conflict is reported (Hugo's decision Q22). The German KA5 adds the l-mull and f-mull (pp. 298-303); its German names are read as these values (Rohhumus mor, Feuchtmull hydromull, Feuchtmoder hydromoder, Feuchtrohhumus hydromor, Tangel tangel, Moder moder, Mull mull). | `mull`, `moder`, `mor`, `hydromull`, `hydromoder`, `hydromor`, `anmoor`, `tourbe`, `eumull`, `mésomull`, `oligomull`, `dysmull`, `hémimoder`, `eumoder`, `dysmoder`, `hémimor`, `humimor`, `amphimus`, `euamphimus`, `peyromull`, `lithomoder`, `peyromoder`, `lithomor`, `peyromor`, `tangel`, `peyro-amphimus`, `agri-mull`, `agri-moder`, `agri-mor`, `mull de pelouse`, `moder de pelouse`, `mor de pelouse`, `amphimus de pelouse`, `l-mull`, `f-mull` | `cpcs1967`, `rp2008` |
| `nasis_diagnostic_features` | - | character | Diagnostic features the surveyor recorded in NASIS (pediagfeatures featkind, one string per feature). Read by the USDA key: the argillic clay-films test and the fragipan, and as a tie-breaker that turns an undecided order test (histic, spodic, andic, vertic, argillic, kandic, natric, mollic, cambic) into a pass. Written by the KSSL NASIS loader. | - | `usda` |
| `anthropic_truncation` | - | logical | The soil has been truncated by human activity (TRUE, or 'yes', 'y', 't', '1'). Read by the NZSC Anthropic Soils, where it meets the first condition without 30 cm of anthropic soil material. | - | `nzsc4`, `scsj2017`, `cpcs1967` |
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
| `reference_za_form` | - | character | Soil form of the South African soil classification recorded for the profile (Soil Classification Working Group 1991, za1991): the form name or its two-letter code (Hutton, Hu), with its edition in reference_za_edition; never resolved across editions (a 1991 family name can be a 2018 form name). For benchmarks only; never used to classify. | - | - |
| `reference_za_family` | - | character | Soil family of the South African soil classification recorded for the profile: the form code and the four digits (Hu1200), or the family name, with its edition in reference_za_edition (a 2018 family code means something else). For benchmarks only; never used to classify. | - | - |
| `reference_za_raw` | - | character | The South African class of the profile as the source gives it (the 1977 binomial 'Hutton Shorrocks' of AfSP LocalCls, or a 1991 code such as Hu1200 medium sandy loam). For benchmarks only. | - | - |
| `reference_za_edition` | - | character | Edition of the South African class in reference_za_form, reference_za_family and reference_za_raw: 1977 (the binomial system), 1991 (the taxonomic system soilKey keys, za1991) or 2018 (the natural and anthropogenic system). | `1977`, `1991`, `2018` | - |
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

## French system (Référentiel pédologique 2008)

| Property | Unit | Type | Description | Values | Read by |
|----------|------|------|-------------|--------|---------|
| `valley_position` | - | character | Position of the site in its valley, for the fluviosols of the French Référentiel pédologique 2008 (pp. 183-188): minor bed (lit mineur), major bed (lit majeur), terrace, alluvial fan or dried marsh. Read when recorded; not recorded, an attachment that needs it is provisional, landform being evidence only (Hugo's decision Q14). | `minor bed`, `major bed`, `terrace`, `alluvial fan`, `dried marsh` | - |
| `flood_frequency` | - | character | Frequency of flooding of the site: none, rare, occasional, frequent or annual. The third condition of the fluviosols of the French Référentiel pédologique 2008 (an alluvial water table, usually floodable, p. 183). Read when recorded; not recorded, the attachment is provisional (Hugo's decision Q14). | `none`, `rare`, `occasional`, `frequent`, `annual` | - |
| `poldered` | - | logical | TRUE when the site is a polder: embanked, drained or reclaimed land won from the sea. The thalassosols poldérisés of the French Référentiel pédologique 2008 (p. 304). Read when recorded; not recorded, artificially_drained with a marine origin is evidence (Hugo's decision Q14). | - | - |
| `lateral_bp_downslope` | - | logical | TRUE when a podzolic BP is present laterally, downslope of the profile. The podzosols éluviques of the French Référentiel pédologique 2008 have an E with no BP in the solum and a BP downslope (p. 270); not recorded, they stay a candidate (Hugo's decision Q14). | - | - |
| `rp_anthropic_history` | - | character | The history of the soil recorded in the terms of the French Référentiel pédologique 2008, joined by ';' (sablage; défoncement; amendements organiques; terrassettes; nivellement; décapage ...). Read for the anthroposols transformés (a history recorded over 50 cm or more) and the qualifiers of human history; never inferred from the profile (Hugo's decision Q15). | - | - |
| `rp_qualifiers_recorded` | - | character | Qualifiers of the French Référentiel pédologique 2008 recorded by the surveyor, the names as printed joined by ';' (drainé; colluvial; alpin ...). The qualifiers of landscape, climate, history and management enter the name of the type only when recorded here, land_use or landform (Hugo's decision Q21). | - | - |
| `parent_material_relation` | - | character | How the parent material enters the name of a type of the French Référentiel pédologique 2008 (pp. 61, 401): 'issu de' (the solum formed in it) or 'sur' (the solum lies on it). The phrase is written only when this and parent_material are recorded (Hugo's decision Q21). | `issu de`, `sur` | - |
| `intertropical` | - | logical | TRUE when the site is declared in the intertropical domain, FALSE when declared outside it. The French Référentiel pédologique 2008 calls the horizons F, OX, RT, ND, FE, OXc and OXm specific to that domain (p. 6) and soilKey tests them only there: declared here, else a Köppen climate of group A or a latitude between the tropics; neither recorded, they stay undecided (Hugo's decision Q18). | - | - |
| `rp_paleo` | - | character | The terms of the paleosolum vocabulary of the French Référentiel pédologique 2008 (pp. 58-63) declared by the surveyor, joined by ';': paléo- (with the reference: paléoluvisol), développé dans, superposé à, multi-, vétuste, fossilisé, bigénétique, polygénétique. These are named only when declared; bilithique, polylithique, recouvert par and enfoui are read from the profile (Hugo's decision Q1). | - | - |

## German system (KA5)

| Property | Unit | Type | Description | Values | Read by |
|----------|------|------|-------------|--------|---------|
| `particle_size_scheme` | - | character | Fraction limits of the recorded texture: 'ka5' (silt 2-63 um, sand 63-2000 um, the German fractions), 'usda' (silt 2-50 um) or 'iso'. German KA5: with silt not in the KA5 fractions, silt 50 % or more counts as KA5 silt 50 % or more, under 45 % as under 50, 45 to < 50 is undecided (Hugo's decision Q11). Not recorded: read as 2-50 um. | `ka5`, `usda`, `iso` | `ka5` |
| `embanked` | - | logical | The site lies behind a dyke (TRUE / FALSE). German KA5: embanked floodplain soils may still be flooded by seepage (Qualmwasser); a Go-Ah is a Rohmarsch unless the site is embanked (Hugo's decision Q20). poldered is not a stand-in. | - | `ka5` |
| `groundwater_source` | - | character | Source of the water of the G horizons: 'groundwater', 'slope_water' or 'spring_water'. German KA5: the Hang- and Quellen- subtypes of the Gleye (prefixes s and q, pp. 243-248). Not recorded: the prefixes written. | `groundwater`, `slope_water`, `spring_water` | `ka5` |
| `groundwater_oxygen_rich` | - | logical | The groundwater is oxygen-rich (TRUE / FALSE). German KA5: the Oxigley GGx (p. 242; Hugo's decision Q16). Not recorded: no Gr in a profile described to 13 dm or more, else undecided between GGn and GGx. | - | `ka5` |
| `groundwater_mhgw_cm` | cm | numeric | Mean high groundwater level, cm below the surface (negative above it), the MHGW of Tab. 59 (pp. 310-313). German KA5: about the top of the Go; a stand-in for undescribed G horizons, flagged and provisional (Hugo's decision Q13). | - | `ka5` |
| `groundwater_mgw_cm` | cm | numeric | Mean groundwater level, cm below the surface (negative above it), the MGW of Tab. 59. German KA5: informative beside the MHGW and MNGW. | - | `ka5` |
| `groundwater_mngw_cm` | cm | numeric | Mean low groundwater level, cm below the surface, the MNGW of Tab. 59. German KA5: about the top of the Gr; a stand-in for undescribed G horizons, flagged and provisional (Hugo's decision Q13). | - | `ka5` |
| `groundwater_step` | - | character | Groundwater step (Grundwasserstufe) of Tab. 59: GWS1 to GWS6, GWS5.1, GWS5.2. German KA5: read through its MHGW and MNGW ranges when the levels are not recorded. | `GWS1`, `GWS2`, `GWS3`, `GWS4`, `GWS5`, `GWS5.1`, `GWS5.2`, `GWS6` | `ka5` |
| `vegetation_cover` | - | character | Cover of higher plants: 'none', 'patchy' or 'closed'. German KA5: the Strand, Watt, Nassstrand and Rohmarsch (pp. 249, 254-256). Not recorded: the vegetation text. | `none`, `patchy`, `closed` | `ka5` |
| `erosion_degree` | - | character | Degree of erosion Eg0 to Eg5 of the KA5. German KA5: the variety e. Not recorded: anthropic_truncation. | `Eg0`, `Eg1`, `Eg2`, `Eg3`, `Eg4`, `Eg5` | `ka5` |
| `rewetted` | - | logical | The site was rewetted after drainage (TRUE / FALSE). German KA5: the variety w of the Moore. Not recorded: undecided. | - | `ka5` |
| `reductgas_source` | - | character | Source of the reducing gas of the Y horizons: 'mofette', 'pipeline_leak', 'refuse_sludge_harbour_mud' or 'natural_wetness'. German KA5: a written Y gives the Reduktosol unless the source is natural wetness (pp. 232-233; Hugo's decision Q20). | `mofette`, `pipeline_leak`, `refuse_sludge_harbour_mud`, `natural_wetness` | `ka5` |
| `deep_cultivation_kind` | - | character | Kind of deep cultivation: 'repeated_trenching' (Rigosol), 'single_deep_ploughing' (Treposol), 'plaggen', 'garden', 'peat_sand_culture'. German KA5: separates YY from YU, and the cultivated Moore (pp. 236-237, 262-263; Hugo's decision Q23). Not recorded: mechanical_mixing_purpose; else YY and YU both candidates. | `repeated_trenching`, `single_deep_ploughing`, `plaggen`, `garden`, `peat_sand_culture` | `ka5` |

---

_Generated from `inst/schemas/horizon-attributes.yaml`, `inst/schemas/site-attributes.yaml`, `horizon_column_spec()` and `.horizon_name_aliases()` in soilKey v0.9.373._

