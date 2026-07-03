# Package index

## Core data structures

The unified profile carrier. Every classification, every provenance tag,
every key-trace lives on one of these objects.

- [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  : PedonRecord: structured representation of a single pedon
- [`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
  : DiagnosticResult: structured outcome of a diagnostic test
- [`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
  : ClassificationResult: structured outcome of running a key
- [`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md)
  : Classe S4-like para atributos de Familia (5o nivel SiBCS)
- [`horizon_column_spec()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizon_column_spec.md)
  : Canonical horizon column specification
- [`make_empty_horizons()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_empty_horizons.md)
  : Build an empty horizons data.table with the canonical schema
- [`ensure_horizon_schema()`](https://hugomachadorodrigues.github.io/soilKey/reference/ensure_horizon_schema.md)
  : Coerce a horizons-like data.frame to the canonical schema
- [`validate_horizon_geometry()`](https://hugomachadorodrigues.github.io/soilKey/reference/validate_horizon_geometry.md)
  : Validate horizon depth geometry

## Classification entry points

One function per system. Each returns a `ClassificationResult` with the
assigned name, the full key trace, the per-attribute provenance log, and
the evidence grade.

- [`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
  : Classify a pedon under WRB 2022
- [`classify_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md)
  : Classifica um pedon segundo o SiBCS 5a edicao (1o + 2o + 3o + 4o
  niveis)
- [`classify_sibcs_familia()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs_familia.md)
  : Classifica um perfil no 5o nivel categorico do SiBCS (Familia)
- [`classify_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md)
  : Classify a pedon under USDA Soil Taxonomy (13th edition)
- [`classify_all()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md)
  : Classify a pedon across all three taxonomic systems
- [`classify_csv()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_csv.md)
  : Classify a horizon spreadsheet in all three systems - one file, one
  line
- [`classify_from_documents()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_from_documents.md)
  : Build a fully-classified \`PedonRecord\` from documents in one call
- [`classify_from_photos()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_from_photos.md)
  : Classify a soil profile from field photographs alone
- [`classify_by_spectral_neighbours()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_by_spectral_neighbours.md)
  : Classify a soil by spectral similarity to OSSL reference profiles
- [`soil_classes_at_location()`](https://hugomachadorodrigues.github.io/soilKey/reference/soil_classes_at_location.md)
  : Likely soil classes at a geographic location (spatial classification
  aid)
- [`apply_soilgrids_depth_prior()`](https://hugomachadorodrigues.github.io/soilKey/reference/apply_soilgrids_depth_prior.md)
  : Fill missing horizon attributes from a SoilGrids depth prior
- [`gapfill_within_pedon()`](https://hugomachadorodrigues.github.io/soilKey/reference/gapfill_within_pedon.md)
  : Fill interior missing horizon attributes by within-pedon depth
  interpolation
- [`gapfill_derive_horizon()`](https://hugomachadorodrigues.github.io/soilKey/reference/gapfill_derive_horizon.md)
  : Fill horizon attributes derivable BY DEFINITION from the same
  horizon
- [`build_taxon_profiles()`](https://hugomachadorodrigues.github.io/soilKey/reference/build_taxon_profiles.md)
  : Build per-taxon mean depth profiles for predicted-taxon gap-fill
- [`gapfill_by_predicted_taxon()`](https://hugomachadorodrigues.github.io/soilKey/reference/gapfill_by_predicted_taxon.md)
  : Fill missing horizon attributes from the predicted taxon's mean
  profile
- [`compute_per_attribute_evidence_grade()`](https://hugomachadorodrigues.github.io/soilKey/reference/compute_per_attribute_evidence_grade.md)
  : Per-attribute provenance-aware evidence grade

## Interoperability

Conversion to / from the canonical R soil-data ecosystem:
[`aqp::SoilProfileCollection`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-class.html).
Round-trip preserving.

- [`as_aqp()`](https://hugomachadorodrigues.github.io/soilKey/reference/as_aqp.md)
  : Convert one or more PedonRecord objects to an aqp
  SoilProfileCollection
- [`from_aqp()`](https://hugomachadorodrigues.github.io/soilKey/reference/from_aqp.md)
  : Convert an aqp SoilProfileCollection back to a list of PedonRecord

## Interactive Shiny app

Drag-and-drop CSV web interface; renders all three classifications
side-by-side and exports a self-contained HTML report.

- [`run_classify_app()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_classify_app.md)
  : Launch the soilKey interactive classification Shiny app

## USDA Soil Taxonomy 13ed – diagnostic helpers

Soil Taxonomy diagnostic helpers used by the v0.9.27+ argillic /
clay-films logic.

- [`argillic_clay_films_test()`](https://hugomachadorodrigues.github.io/soilKey/reference/argillic_clay_films_test.md)
  : Test for clay-illuviation evidence (KST 13ed Ch 3 p 4)

## Benchmark utilities

Benchmark drivers and KSSL/NASIS label normalisers.

- [`canonicalise_kst13ed_gg()`](https://hugomachadorodrigues.github.io/soilKey/reference/canonicalise_kst13ed_gg.md)
  : Canonicalise a USDA Great Group label to a KST 13ed-compatible key
- [`normalise_kssl_subgroup()`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_kssl_subgroup.md)
  : Normalise KSSL USDA subgroup labels for benchmark comparison

## WRB 2022 – diagnostic horizons (Ch 3.1)

32/32 diagnostic horizons per IUSS Working Group WRB (2022) Ch 3.1.

- [`argic()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
  : Argic horizon (WRB 2022)

- [`albic()`](https://hugomachadorodrigues.github.io/soilKey/reference/albic.md)
  : Albic horizon (WRB 2022)

- [`cambic()`](https://hugomachadorodrigues.github.io/soilKey/reference/cambic.md)
  : Cambic horizon (WRB 2022)

- [`calcic()`](https://hugomachadorodrigues.github.io/soilKey/reference/calcic.md)
  : Calcic horizon (WRB 2022)

- [`duric_horizon()`](https://hugomachadorodrigues.github.io/soilKey/reference/duric_horizon.md)
  : Duric horizon (WRB 2022)

- [`ferralic()`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md)
  : Ferralic horizon (WRB 2022)

- [`ferric()`](https://hugomachadorodrigues.github.io/soilKey/reference/ferric.md)
  : Ferric horizon (WRB 2022)

- [`fragic()`](https://hugomachadorodrigues.github.io/soilKey/reference/fragic.md)
  :

  Fragic horizon (WRB 2022): a high-bulk-density horizon with restricted
  rooting. v0.3.3: detects via bulk_density_g_cm3 \>= 1.65 AND structure
  grade massive/very firm OR designation pattern `x`/`Bx`.

- [`gypsic()`](https://hugomachadorodrigues.github.io/soilKey/reference/gypsic.md)
  : Gypsic horizon (WRB 2022)

- [`histic_horizon()`](https://hugomachadorodrigues.github.io/soilKey/reference/histic_horizon.md)
  : Histic horizon (WRB 2022)

- [`mollic()`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md)
  : Mollic horizon (WRB 2022)

- [`natric_horizon()`](https://hugomachadorodrigues.github.io/soilKey/reference/natric_horizon.md)
  : Natric horizon (WRB 2022)

- [`nitic_horizon()`](https://hugomachadorodrigues.github.io/soilKey/reference/nitic_horizon.md)
  : Nitic horizon (WRB 2022)

- [`petrocalcic()`](https://hugomachadorodrigues.github.io/soilKey/reference/petrocalcic.md)
  : Petrocalcic horizon (WRB 2022)

- [`petroduric()`](https://hugomachadorodrigues.github.io/soilKey/reference/petroduric.md)
  : Petroduric horizon (WRB 2022): cemented duric.

- [`petrogypsic()`](https://hugomachadorodrigues.github.io/soilKey/reference/petrogypsic.md)
  : Petrogypsic horizon (WRB 2022): cemented gypsic.

- [`petroplinthic()`](https://hugomachadorodrigues.github.io/soilKey/reference/petroplinthic.md)
  : Petroplinthic horizon (WRB 2022): cemented plinthic.

- [`pisoplinthic()`](https://hugomachadorodrigues.github.io/soilKey/reference/pisoplinthic.md)
  :

  Pisoplinthic horizon (WRB 2022): pisolitic plinthic. v0.3.3 detects
  via designation pattern `Bspl` / `Bvpi` or via plinthite \\= 15% AND
  structure_type containing 'pisol'.

- [`plinthic()`](https://hugomachadorodrigues.github.io/soilKey/reference/plinthic.md)
  : Plinthic horizon (WRB 2022)

- [`salic()`](https://hugomachadorodrigues.github.io/soilKey/reference/salic.md)
  : Salic horizon (WRB 2022)

- [`sombric()`](https://hugomachadorodrigues.github.io/soilKey/reference/sombric.md)
  : Sombric horizon (WRB 2022): subsurface accumulation of humus that
  qualified neither as spodic nor as a true mollic-like horizon
  (low-base-saturation cool tropical highlands). v0.3.3 detects via
  designation pattern + OC criteria (BS \< 50, OC \> 0.6, depth \> 25
  cm).

- [`spodic()`](https://hugomachadorodrigues.github.io/soilKey/reference/spodic.md)
  : Spodic horizon (WRB 2022)

- [`thionic()`](https://hugomachadorodrigues.github.io/soilKey/reference/thionic.md)
  : Thionic horizon (WRB 2022): post-oxidation acid sulfate horizon.
  Requires sulfidic_s_pct \>= 0.01 AND pH(H2O) \<= 4.

- [`umbric_horizon()`](https://hugomachadorodrigues.github.io/soilKey/reference/umbric_horizon.md)
  : Umbric horizon (WRB 2022)

- [`vertic_horizon()`](https://hugomachadorodrigues.github.io/soilKey/reference/vertic_horizon.md)
  : Vertic horizon (WRB 2022 Ch 3.1)

- [`chernic()`](https://hugomachadorodrigues.github.io/soilKey/reference/chernic.md)
  : Chernic horizon (WRB 2022): the cherozemic-style mollic with very
  high biological activity (worm holes, casts, coprolites). v0.3.3:
  delegates to mollic + worm_holes_pct \>= 50 (proxy for "biological
  homogenization").

- [`tsitelic()`](https://hugomachadorodrigues.github.io/soilKey/reference/tsitelic.md)
  : Tsitelic horizon (WRB 2022 Ch 3.1)

- [`panpaic()`](https://hugomachadorodrigues.github.io/soilKey/reference/panpaic.md)
  : Panpaic horizon (WRB 2022 Ch 3.1)

- [`limonic()`](https://hugomachadorodrigues.github.io/soilKey/reference/limonic.md)
  : Limonic horizon (WRB 2022 Ch 3.1)

- [`protovertic()`](https://hugomachadorodrigues.github.io/soilKey/reference/protovertic.md)
  : Protovertic horizon (WRB 2022 Ch 3.1)

- [`anthraquic()`](https://hugomachadorodrigues.github.io/soilKey/reference/anthraquic.md)
  :

  Anthraquic horizon (WRB 2022): puddled-rice / paddy plough layer.
  v0.3.3 detects via designation pattern `Apl|Ap|Hh`.

- [`hydragric()`](https://hugomachadorodrigues.github.io/soilKey/reference/hydragric.md)
  :

  Hydragric horizon (WRB 2022): subsoil hydric horizon under anthraquic.
  v0.3.3 detects via designation pattern `Bg|Brg` immediately below an
  anthraquic-like topsoil.

- [`hortic()`](https://hugomachadorodrigues.github.io/soilKey/reference/hortic.md)
  : Hortic horizon (WRB 2022): garden / kitchen-midden topsoil.
  Diagnostic criteria: thickness \\= 20 cm, dark colour (mollic-like),
  high P (Mehlich-3 P \>= 100 mg/kg or P2O5_1pct_citric \>= 175 mg/kg),
  high SOC.

- [`irragric()`](https://hugomachadorodrigues.github.io/soilKey/reference/irragric.md)
  :

  Irragric horizon (WRB 2022): topsoil thickened by irrigation deposits.
  v0.3.3: thickness \>= 20 cm + sediment-derived structure proxied via
  designation `Apk|Apg|Au`.

- [`plaggic()`](https://hugomachadorodrigues.github.io/soilKey/reference/plaggic.md)
  : Plaggic horizon (WRB 2022): sod-derived topsoil \>= 20 cm with low
  BD AND independent evidence of human input.

- [`pretic()`](https://hugomachadorodrigues.github.io/soilKey/reference/pretic.md)
  : Pretic horizon (WRB 2022): "Amazonian Dark Earth" (terra preta de
  indio) horizon – thick anthropogenic surface with high P, SOC, and
  incorporated charcoal / pottery.

- [`terric()`](https://hugomachadorodrigues.github.io/soilKey/reference/terric.md)
  : Terric horizon (WRB 2022): topsoil thickened by long-term
  application of mineral material (sediment / sand additions). v0.3.3:
  thickness \>= 20 cm + designation Au / Apc.

## WRB 2022 – diagnostic properties (Ch 3.2)

- [`abrupt_textural_difference()`](https://hugomachadorodrigues.github.io/soilKey/reference/abrupt_textural_difference.md)
  : Abrupt textural difference (WRB 2022 Ch 3.2.1)

- [`albeluvic_glossae()`](https://hugomachadorodrigues.github.io/soilKey/reference/albeluvic_glossae.md)
  : Albeluvic glossae (WRB 2022 Ch 3.2.2)

- [`andic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/andic_properties.md)
  : Andic properties (WRB 2022)

- [`anthric_horizons()`](https://hugomachadorodrigues.github.io/soilKey/reference/anthric_horizons.md)
  : Anthric horizons (WRB 2022)

- [`continuous_rock()`](https://hugomachadorodrigues.github.io/soilKey/reference/continuous_rock.md)
  : Continuous rock (WRB 2022 Ch 3.2.5)

- [`gleyic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/gleyic_properties.md)
  : Gleyic properties (WRB 2022)

- [`lithic_discontinuity()`](https://hugomachadorodrigues.github.io/soilKey/reference/lithic_discontinuity.md)
  : Lithic discontinuity (WRB 2022 Ch 3.2.7)

- [`protocalcic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/protocalcic_properties.md)
  : Protocalcic properties (WRB 2022 Ch 3.2.8)

- [`protogypsic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/protogypsic_properties.md)
  : Protogypsic properties (WRB 2022 Ch 3.2.9): visible secondary gypsum
  \\= 1% but below the gypsic gate.

- [`reducing_conditions()`](https://hugomachadorodrigues.github.io/soilKey/reference/reducing_conditions.md)
  :

  Reducing conditions (WRB 2022 Ch 3.2.10) – per-pedon test wrapping
  `test_reducing_conditions`.

- [`retic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/retic_properties.md)
  : Retic properties (WRB 2022)

- [`shrink_swell_cracks()`](https://hugomachadorodrigues.github.io/soilKey/reference/shrink_swell_cracks.md)
  :

  Shrink-swell cracks (WRB 2022 Ch 3.2.12) – per-pedon test wrapping
  `test_shrink_swell_cracks`.

- [`sideralic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/sideralic_properties.md)
  : Sideralic properties (WRB 2022 Ch 3.2.13)

- [`stagnic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/stagnic_properties.md)
  : Stagnic properties (WRB 2022)

- [`takyric_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/takyric_properties.md)
  :

  Takyric properties (WRB 2022 Ch 3.2.15) – per-pedon test wrapping
  `test_takyric_surface`.

- [`vertic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/vertic_properties.md)
  : Vertic properties (WRB 2022)

- [`vitric_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/vitric_properties.md)
  : Vitric properties (WRB 2022 Ch 3.2.16)

- [`yermic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/yermic_properties.md)
  :

  Yermic properties (WRB 2022 Ch 3.2.17) – per-pedon test wrapping
  `test_yermic_surface`.

## WRB 2022 – diagnostic materials (Ch 3.3)

- [`aeolic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/aeolic_material.md)
  : Aeolic material (WRB 2022 Ch 3.3.1)

- [`artefacts()`](https://hugomachadorodrigues.github.io/soilKey/reference/artefacts.md)
  : Artefacts (WRB 2022 Ch 3.3.2)

- [`calcaric_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/calcaric_material.md)
  : Calcaric material (WRB 2022 Ch 3.3.3): \\= 2% CaCO3 throughout the
  fine earth, primary carbonates from the parent material.

- [`claric_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/claric_material.md)
  : Claric material (WRB 2022 Ch 3.3.4): light-coloured fine earth with
  Munsell criteria.

- [`dolomitic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/dolomitic_material.md)
  :

  Dolomitic material (WRB 2022 Ch 3.3.5): \\= 2% Mg-rich carbonate,
  CaCO3/MgCO3 \< 1.5. v0.3.3: detects via designation pattern
  `kdo|do|magn` as proxy when ratio data missing.

- [`fluvic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/fluvic_material.md)
  : Fluvic material (WRB 2022)

- [`gypsiric_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/gypsiric_material.md)
  : Gypsiric material (WRB 2022 Ch 3.3.7): \\= 5% gypsum that is primary
  (not secondary). Without a "secondary fraction" schema column, v0.3.3
  treats any layer with caso4_pct \>= 5 as gypsiric unless it explicitly
  carries gypsic-horizon designation.

- [`hypersulfidic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/hypersulfidic_material.md)
  : Hypersulfidic material (WRB 2022 Ch 3.3.8): \\= 0.01% inorganic
  sulfidic S, pH \\= 4, capable of severe acidification on aerobic
  incubation.

- [`hyposulfidic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/hyposulfidic_material.md)
  :

  Hyposulfidic material (WRB 2022 Ch 3.3.9): same inorganic sulfidic S
  and field pH as hypersulfidic but does NOT consist of hypersulfidic
  (criterion 3 – does not acidify to pH \< 4 on aerobic incubation,
  usually self-neutralised by carbonate). Reachable from v0.9.128: when
  `incubation_ph` is measured, a sulfidic + pH\>=4 layer that stays \>=
  4 on incubation is the set-complement of `hypersulfidic_material` and
  is reported here. Without an incubation pH the two cannot be told
  apart, so this returns empty (the layer is reported as potential
  hypersulfidic instead).

- [`limnic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/limnic_material.md)
  :

  Limnic material (WRB 2022 Ch 3.3.10): subaquatic deposits (coprogenous
  earth, diatomaceous earth, marl, gyttja). v0.3.3: detects via
  `rock_origin %in% c("lacustrine", "marine")` or designation pattern.

- [`mineral_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/mineral_material.md)
  : Mineral material (WRB 2022 Ch 3.3.11): \< 20% SOC AND \< 35% volume
  artefacts containing \>= 20% organic carbon. The complement of
  organic_material / organotechnic_material.

- [`mulmic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/mulmic_material.md)
  : Mulmic material (WRB 2022 Ch 3.3.12): mineral material developed
  from organic material; \\= 8% SOC, with low BD, structural / chroma
  criteria.

- [`organic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/organic_material.md)
  : Organic material (WRB 2022 Ch 3.3.13): \\= 20% SOC + recognisability
  criteria. v0.3.3: SOC threshold only.

- [`organotechnic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/organotechnic_material.md)
  : Organotechnic material (WRB 2022 Ch 3.3.14): \\= 35% volume of
  artefacts that themselves contain \\= 20% organic C. Soil itself has
  \< 20% SOC.

- [`ornithogenic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/ornithogenic_material.md)
  :

  Ornithogenic material (WRB 2022 Ch 3.3.15): bird-influenced topsoil.
  Mehlich-3 P \>= 750 mg/kg + designation pattern `Aornit|Bornit`.

- [`soil_organic_carbon()`](https://hugomachadorodrigues.github.io/soilKey/reference/soil_organic_carbon.md)
  : Soil organic carbon (WRB 2022 Ch 3.3.16): organic C that does NOT
  belong to artefacts. v0.3.3: any layer with oc_pct \>= 0.1 and
  artefacts_industrial_pct \< 35.

- [`solimovic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/solimovic_material.md)
  :

  Solimovic material (WRB 2022 Ch 3.3.17): hetero genous mass-movement
  material on slopes / footslopes (formerly "colluvic"). v0.3.3: detects
  via `rock_origin == "colluvial"` OR `layer_origin == "solimovic"`.

- [`technic_features()`](https://hugomachadorodrigues.github.io/soilKey/reference/technic_features.md)
  : Technic features (WRB 2022)

- [`tephric_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/tephric_material.md)
  : Tephric material (WRB 2022 Ch 3.3.19): \\= 30% volcanic glass in
  0.02-2 mm fraction AND no andic / vitric properties.

## WRB 2022 – RSG-level gates

- [`acrisol()`](https://hugomachadorodrigues.github.io/soilKey/reference/acrisol.md)
  : Acrisol RSG diagnostic (WRB 2022)
- [`alisol()`](https://hugomachadorodrigues.github.io/soilKey/reference/alisol.md)
  : Alisol RSG diagnostic (WRB 2022)
- [`lixisol()`](https://hugomachadorodrigues.github.io/soilKey/reference/lixisol.md)
  : Lixisol RSG diagnostic (WRB 2022)
- [`luvisol()`](https://hugomachadorodrigues.github.io/soilKey/reference/luvisol.md)
  : Luvisol RSG diagnostic (WRB 2022)
- [`chernozem()`](https://hugomachadorodrigues.github.io/soilKey/reference/chernozem.md)
  : Chernozem RSG diagnostic (WRB 2022)
- [`kastanozem()`](https://hugomachadorodrigues.github.io/soilKey/reference/kastanozem.md)
  : Kastanozem RSG diagnostic (WRB 2022)
- [`phaeozem()`](https://hugomachadorodrigues.github.io/soilKey/reference/phaeozem.md)
  : Phaeozem RSG diagnostic (WRB 2022)
- [`andosol()`](https://hugomachadorodrigues.github.io/soilKey/reference/andosol.md)
  : Andosol RSG gate (WRB 2022 Ch 4, p 104)
- [`chernozem_strict()`](https://hugomachadorodrigues.github.io/soilKey/reference/chernozem_strict.md)
  : Chernozem RSG gate (strengthened, WRB 2022 Ch 4, p 111)
- [`kastanozem_strict()`](https://hugomachadorodrigues.github.io/soilKey/reference/kastanozem_strict.md)
  : Kastanozem RSG gate (strengthened, WRB 2022 Ch 4, p 112)

## SiBCS 5ª ed. – horizontes B diagnósticos (Cap 2)

Os gates de horizonte B públicos. As demais gates da chave SiBCS
(carater\_*, horizonte\_*, atributos, qualifiers WRB, e os dispatchers
por Ordem) são o motor determinístico interno, despachado por
[`classify_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md)
/
[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md);
veja a seção “Internal – motor taxonômico” no fim da referência.

- [`B_textural()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_textural.md)
  : Horizonte B textural (SiBCS Cap 2, p 54-57; v0.7 strict)
- [`B_latossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_latossolico.md)
  : Horizonte B latossolico (SiBCS Cap 2, p 57-59; v0.7 strict)
- [`B_nitico()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_nitico.md)
  : Horizonte B nitico (SiBCS Cap 2, p 61-62; v0.7)
- [`B_espodico()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_espodico.md)
  : Horizonte B espodico (SiBCS Cap 2, p 62-65; v0.7)
- [`B_incipiente()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_incipiente.md)
  : Horizonte B incipiente (SiBCS Cap 2, p 59-61; v0.7)
- [`B_planico()`](https://hugomachadorodrigues.github.io/soilKey/reference/B_planico.md)
  : Horizonte B planico (SiBCS Cap 2, p 65-66; v0.7)

## SiBCS 5ª ed. – Família (5º nível, Cap 18)

O 5º nível categórico (família), via
`classify_sibcs(include_familia = TRUE)`.

- [`classify_sibcs_familia()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs_familia.md)
  : Classifica um perfil no 5o nivel categorico do SiBCS (Familia)

## USDA Soil Taxonomy 13ed – Família (5º nível)

Os modificadores de família (particle-size, mineralogy, CEC-activity,
reaction, temperature, depth) via
`classify_usda(pedon, include_family = TRUE)`. Os gates de Ordem -\>
Subgrupo são internos (veja “Internal”).

- [`classify_usda_family()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda_family.md)
  : Classify the USDA family (5th level) of a pedon

## Module 2 – VLM extraction

Schema-validated multimodal extraction via `ellmer`. The VLM never
classifies; every extracted value carries provenance = `extracted_vlm`.

- [`vlm_provider()`](https://hugomachadorodrigues.github.io/soilKey/reference/vlm_provider.md)
  : Construct a VLM provider chat object
- [`MockVLMProvider`](https://hugomachadorodrigues.github.io/soilKey/reference/MockVLMProvider.md)
  : Mock VLM provider for testing
- [`extract_horizons_from_pdf()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_horizons_from_pdf.md)
  : Extract horizons from a soil description PDF
- [`extract_munsell_from_photo()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_munsell_from_photo.md)
  : Extract Munsell color from a profile photo
- [`extract_site_from_fieldsheet()`](https://hugomachadorodrigues.github.io/soilKey/reference/extract_site_from_fieldsheet.md)
  : Extract site metadata from a field-sheet image

## Module 3 – Spatial prior

SoilGrids + Embrapa raster. Used as a sanity-check; never overrides the
deterministic key.

- [`spatial_prior()`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior.md)
  : Spatial prior over RSGs (or Orders) at a pedon's location
- [`spatial_prior_soilgrids()`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior_soilgrids.md)
  : SoilGrids spatial prior
- [`spatial_prior_embrapa()`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior_embrapa.md)
  : Embrapa national soil-class spatial prior (Brazil only)
- [`prior_consistency_check()`](https://hugomachadorodrigues.github.io/soilKey/reference/prior_consistency_check.md)
  : Check consistency between a deterministic RSG assignment and a
  spatial prior

## Module 4 – OSSL spectroscopy

Vis-NIR / MIR gap-fill. Predicted attributes carry provenance =
`predicted_spectra`, which downgrades the evidence grade from A to B.

- [`preprocess_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/preprocess_spectra.md)
  : Pre-process Vis-NIR or MIR spectra
- [`predict_ossl_mbl()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md)
  : Memory-based learning prediction against the OSSL library
- [`predict_ossl_plsr_local()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_plsr_local.md)
  : Local PLSR prediction against the OSSL library
- [`predict_ossl_pretrained()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_pretrained.md)
  : Pre-trained OSSL prediction
- [`fill_from_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md)
  : Fill missing soil attributes from spectra via OSSL
- [`read_spectral_library()`](https://hugomachadorodrigues.github.io/soilKey/reference/read_spectral_library.md)
  : Read a Vis-NIR / MIR reflectance + lab table into an OSSL-shaped
  library
- [`pedons_from_spectral_table()`](https://hugomachadorodrigues.github.io/soilKey/reference/pedons_from_spectral_table.md)
  : Build PedonRecords with attached Vis-NIR/MIR spectra from a table
- [`benchmark_spectral_fill()`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_spectral_fill.md)
  : Benchmark the accuracy lift of spectral gap-fill (ON vs OFF), k-fold
- [`pi_to_confidence()`](https://hugomachadorodrigues.github.io/soilKey/reference/pi_to_confidence.md)
  : Map a 95% prediction interval to a \[0, 1\] confidence score

## Reporting

Render a complete pedologist-facing report from one or more
`ClassificationResult` objects.

- [`report()`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md)
  : Render a soilKey classification report
- [`report_html()`](https://hugomachadorodrigues.github.io/soilKey/reference/report_html.md)
  : Render a soilKey classification report as self-contained HTML
- [`report_pdf()`](https://hugomachadorodrigues.github.io/soilKey/reference/report_pdf.md)
  : Render a soilKey classification report as PDF
- [`report_to_qgis()`](https://hugomachadorodrigues.github.io/soilKey/reference/report_to_qgis.md)
  : Export a classification result + pedon to a QGIS GeoPackage
- [`key_trace_table()`](https://hugomachadorodrigues.github.io/soilKey/reference/key_trace_table.md)
  : Flatten a classification key trace into a tabular form

## Layperson on-ramp

Zero-code Shiny GUI plus auto-detect helpers that remove the friction
non-coders hit on a fresh install.

- [`run_demo()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_demo.md)
  : Launch the soilKey Shiny demo (one-screen GUI)
- [`read_pedon_csv()`](https://hugomachadorodrigues.github.io/soilKey/reference/read_pedon_csv.md)
  : Read a horizon spreadsheet (CSV/TSV) into a PedonRecord
- [`auto_set_proj_env()`](https://hugomachadorodrigues.github.io/soilKey/reference/auto_set_proj_env.md)
  : Auto-detect PROJ_LIB and GDAL_DATA directories
- [`vlm_pick_provider()`](https://hugomachadorodrigues.github.io/soilKey/reference/vlm_pick_provider.md)
  : Pick the best available VLM provider
- [`ollama_is_running()`](https://hugomachadorodrigues.github.io/soilKey/reference/ollama_is_running.md)
  : Is the local Ollama HTTP API reachable?

## Real-data benchmarks (v0.9.15+)

Loaders for the three external validation sets soilKey is benchmarked
against in the v1.0 methods paper, plus FEBR / BDsolos
label-normalisation helpers (v0.9.16).

- [`load_kssl_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_pedons.md)
  : Load NCSS / KSSL pedons with reference USDA Soil Taxonomy
  classification
- [`load_kssl_pedons_gpkg()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_pedons_gpkg.md)
  : Load KSSL / NCSS pedons from the ncss_labdata GeoPackage
- [`load_kssl_pedons_with_nasis()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_pedons_with_nasis.md)
  : Load KSSL pedons enriched with NASIS morphology
- [`load_lucas_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_lucas_pedons.md)
  : Load EU-LUCAS / ESDB pedons with reference WRB classification
- [`load_embrapa_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_embrapa_pedons.md)
  : Load Embrapa dadosolos pedons with reference SiBCS classification
- [`load_febr_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_febr_pedons.md)
  : Load the Embrapa FEBR superconjunto into a list of PedonRecords
- [`normalise_febr_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_sibcs.md)
  : Canonicalise FEBR SiBCS names to match soilKey rule outputs.
- [`normalise_febr_wrb()`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_wrb.md)
  : Normalise FEBR WRB taxon strings to RSG-only
- [`normalise_febr_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_usda.md)
  : Normalise FEBR USDA taxon strings to USDA Soil Taxonomy Order
- [`benchmark_run_classification()`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_run_classification.md)
  : Run a benchmark across one of the loaded pedon lists

## Canonical fixtures

One-call constructors for the 31 canonical fixtures (one per WRB RSG,
plus auxiliaries) used in the test suite.

- [`make_acrisol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_acrisol_canonical.md)
  : Build the canonical Acrisol fixture
- [`make_alisol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_alisol_canonical.md)
  : Build the canonical Alisol fixture
- [`make_andosol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_andosol_canonical.md)
  : Build the canonical Andosol fixture
- [`make_anthrosol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_anthrosol_canonical.md)
  : Build the canonical Anthrosol fixture
- [`make_arenosol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_arenosol_canonical.md)
  : Build the canonical Arenosol fixture
- [`make_argissolo_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_argissolo_canonical.md)
  : Perfil canonico de Argissolo (SiBCS 5a ed., Cap 5)
- [`make_calcisol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_calcisol_canonical.md)
  : Build the canonical Calcisol fixture
- [`make_cambisol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_cambisol_canonical.md)
  : Build the canonical Cambisol fixture
- [`make_cambissolo_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_cambissolo_canonical.md)
  : Perfil canonico de Cambissolo (SiBCS 5a ed., Cap 6)
- [`make_chernossolo_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_chernossolo_canonical.md)
  : Perfil canonico de Chernossolo (SiBCS 5a ed., Cap 7)
- [`make_chernozem_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_chernozem_canonical.md)
  : Build the canonical Chernozem fixture
- [`make_cryosol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_cryosol_canonical.md)
  : Build the canonical Cryosol fixture
- [`make_durisol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_durisol_canonical.md)
  : Build the canonical Durisol fixture
- [`make_espodossolo_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_espodossolo_canonical.md)
  : Perfil canonico de Espodossolo (SiBCS 5a ed., Cap 8)
- [`make_ferralsol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_ferralsol_canonical.md)
  : Build the canonical Ferralsol fixture
- [`make_fluvisol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_fluvisol_canonical.md)
  : Build the canonical Fluvisol fixture
- [`make_gleissolo_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_gleissolo_canonical.md)
  : Perfil canonico de Gleissolo (SiBCS 5a ed., Cap 9)
- [`make_gleysol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_gleysol_canonical.md)
  : Build the canonical Gleysol fixture
- [`make_gypsisol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_gypsisol_canonical.md)
  : Build the canonical Gypsisol fixture
- [`make_histosol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_histosol_canonical.md)
  : Build the canonical Histosol fixture
- [`make_kastanozem_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_kastanozem_canonical.md)
  : Build the canonical Kastanozem fixture
- [`make_latossolo_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_latossolo_canonical.md)
  : Perfil canonico de Latossolo (SiBCS 5a ed., Cap 10)
- [`make_leptosol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_leptosol_canonical.md)
  : Build the canonical Leptosol fixture
- [`make_lixisol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_lixisol_canonical.md)
  : Build the canonical Lixisol fixture
- [`make_luvisol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_luvisol_canonical.md)
  : Build the canonical Luvisol fixture
- [`make_luvissolo_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_luvissolo_canonical.md)
  : Perfil canonico de Luvissolo (SiBCS 5a ed., Cap 11)
- [`make_neossolo_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_neossolo_canonical.md)
  : Perfil canonico de Neossolo Litolico (SiBCS 5a ed., Cap 12)
- [`make_nitisol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_nitisol_canonical.md)
  : Build the canonical Nitisol fixture
- [`make_nitossolo_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_nitossolo_canonical.md)
  : Perfil canonico de Nitossolo Vermelho (SiBCS 5a ed., Cap 13)
- [`make_organossolo_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_organossolo_canonical.md)
  : Perfil canonico de Organossolo (SiBCS 5a ed., Cap 14)
- [`make_phaeozem_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_phaeozem_canonical.md)
  : Build the canonical Phaeozem fixture
- [`make_planosol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_planosol_canonical.md)
  : Build the canonical Planosol fixture
- [`make_planossolo_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_planossolo_canonical.md)
  : Perfil canonico de Planossolo (SiBCS 5a ed., Cap 15)
- [`make_plinthosol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_plinthosol_canonical.md)
  : Build the canonical Plinthosol fixture
- [`make_plintossolo_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_plintossolo_canonical.md)
  : Perfil canonico de Plintossolo (SiBCS 5a ed., Cap 16)
- [`make_podzol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_podzol_canonical.md)
  : Build the canonical Podzol fixture
- [`make_retisol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_retisol_canonical.md)
  : Build the canonical Retisol fixture
- [`make_solonchak_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_solonchak_canonical.md)
  : Build the canonical Solonchak fixture
- [`make_solonetz_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_solonetz_canonical.md)
  : Build the canonical Solonetz fixture
- [`make_stagnosol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_stagnosol_canonical.md)
  : Build the canonical Stagnosol fixture
- [`make_technosol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_technosol_canonical.md)
  : Build the canonical Technosol fixture
- [`make_umbrisol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_umbrisol_canonical.md)
  : Build the canonical Umbrisol fixture
- [`make_vertisol_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_vertisol_canonical.md)
  : Build the canonical Vertisol fixture
- [`make_vertissolo_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_vertissolo_canonical.md)
  : Perfil canonico de Vertissolo (SiBCS 5a ed., Cap 17)

## Helpers and miscellaneous

- [`run_all_benchmarks()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_all_benchmarks.md)
  : Run the full soilKey benchmark suite and (optionally) write a report

- [`run_classify_app()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_classify_app.md)
  : Launch the soilKey interactive classification Shiny app

- [`run_demo()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_demo.md)
  : Launch the soilKey Shiny demo (one-screen GUI)

- [`run_sibcs_grande_grupo()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_sibcs_grande_grupo.md)
  : Resolve o grande grupo (3o nivel) de um pedon classificado em uma
  subordem SiBCS

- [`run_sibcs_key()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_sibcs_key.md)
  : Roda a chave SiBCS 5a edicao sobre um pedon

- [`run_sibcs_subgrupo()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_sibcs_subgrupo.md)
  : Resolve o subgrupo (4o nivel) de um pedon classificado em um Grande
  Grupo SiBCS

- [`run_sibcs_subordem()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_sibcs_subordem.md)
  : Resolve a subordem de um pedon ja classificado em uma ordem SiBCS

- [`run_taxa_list()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_taxa_list.md)
  : Iterate a flat taxa list and evaluate tests in canonical order

- [`run_taxonomic_key()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_taxonomic_key.md)
  : Run a taxonomic key (system-agnostic engine)

- [`run_usda_great_group()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_usda_great_group.md)
  : Run the USDA Great Group key for a given Suborder

- [`run_usda_key()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_usda_key.md)
  : Run the USDA Soil Taxonomy Order key over a pedon

- [`run_usda_subgroup()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_usda_subgroup.md)
  : Run the USDA Subgroup key for a given Great Group

- [`run_usda_suborder()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_usda_suborder.md)
  : Run the USDA Suborder key for a given Order

- [`run_wrb_key()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_wrb_key.md)
  : Run the WRB 2022 key over a pedon

- [`compute_ki()`](https://hugomachadorodrigues.github.io/soilKey/reference/compute_ki.md)
  : Ki (silica:alumina molar) – SiBCS Cap 1, p 32

- [`compute_kr()`](https://hugomachadorodrigues.github.io/soilKey/reference/compute_kr.md)
  : Kr (silica:sesquioxidos molar) – SiBCS Cap 1, p 32

- [`compute_per_attribute_evidence_grade()`](https://hugomachadorodrigues.github.io/soilKey/reference/compute_per_attribute_evidence_grade.md)
  : Per-attribute provenance-aware evidence grade

- [`load_afsp_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_afsp_pedons.md)
  : Load Africa Soil Profiles (AfSP) v1.2 as PedonRecord objects

- [`load_afsp_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_afsp_sample.md)
  : Load the bundled AfSP stratified sample (v0.9.77)

- [`load_bdsolos_csv()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_bdsolos_csv.md)
  : Load a BDsolos CSV export as a list of PedonRecord objects

- [`load_embrapa_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_embrapa_pedons.md)
  : Load Embrapa dadosolos pedons with reference SiBCS classification

- [`load_febr_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_febr_pedons.md)
  : Load the Embrapa FEBR superconjunto into a list of PedonRecords

- [`load_kssl_nasis_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_nasis_sample.md)
  : Load the bundled KSSL + NASIS morphological-enriched sample
  (v0.9.75)

- [`load_kssl_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_pedons.md)
  : Load NCSS / KSSL pedons with reference USDA Soil Taxonomy
  classification

- [`load_kssl_pedons_gpkg()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_pedons_gpkg.md)
  : Load KSSL / NCSS pedons from the ncss_labdata GeoPackage

- [`load_kssl_pedons_with_nasis()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_pedons_with_nasis.md)
  : Load KSSL pedons enriched with NASIS morphology

- [`load_kssl_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_sample.md)
  : Load the bundled KSSL/NCSS lab-data sample (v0.9.74)

- [`load_lucas_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_lucas_pedons.md)
  : Load EU-LUCAS / ESDB pedons with reference WRB classification

- [`load_lucas_soil_2018()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_lucas_soil_2018.md)
  : Load the LUCAS Soil 2018 Topsoil release as a list of PedonRecord
  objects

- [`load_redape_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_redape_pedons.md)
  : Load curated soil profiles from the Embrapa Redape GeoTab dataset

- [`load_rules()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_rules.md)
  : Load a soilKey rule set (YAML)

- [`load_wosis_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_wosis_sample.md)
  : Load the bundled WoSIS South-America sample

- [`load_wosis_stratified_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_wosis_stratified_sample.md)
  : Load the bundled WoSIS stratified RSG-balanced sample (v0.9.73)

- [`save_ossl_models()`](https://hugomachadorodrigues.github.io/soilKey/reference/save_ossl_models.md)
  [`load_ossl_models()`](https://hugomachadorodrigues.github.io/soilKey/reference/save_ossl_models.md)
  : Save / load trained OSSL-backed PLSR models

- [`apply_soilgrids_depth_prior()`](https://hugomachadorodrigues.github.io/soilKey/reference/apply_soilgrids_depth_prior.md)
  : Fill missing horizon attributes from a SoilGrids depth prior

- [`format_wrb_name()`](https://hugomachadorodrigues.github.io/soilKey/reference/format_wrb_name.md)
  : Format a WRB 2022 soil name with qualifiers

- [`validate_horizon_geometry()`](https://hugomachadorodrigues.github.io/soilKey/reference/validate_horizon_geometry.md)
  : Validate horizon depth geometry

- [`validate_pedon_json()`](https://hugomachadorodrigues.github.io/soilKey/reference/validate_pedon_json.md)
  : Validate a PedonRecord against the JSON schema

- [`leptic_features()`](https://hugomachadorodrigues.github.io/soilKey/reference/leptic_features.md)
  : Leptic features (WRB 2022)

- [`planic_features()`](https://hugomachadorodrigues.github.io/soilKey/reference/planic_features.md)
  : Planic features (WRB 2022)

- [`technic_features()`](https://hugomachadorodrigues.github.io/soilKey/reference/technic_features.md)
  : Technic features (WRB 2022)

- [`andic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/andic_properties.md)
  : Andic properties (WRB 2022)

- [`gleyic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/gleyic_properties.md)
  : Gleyic properties (WRB 2022)

- [`protocalcic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/protocalcic_properties.md)
  : Protocalcic properties (WRB 2022 Ch 3.2.8)

- [`protogypsic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/protogypsic_properties.md)
  : Protogypsic properties (WRB 2022 Ch 3.2.9): visible secondary gypsum
  \\= 1% but below the gypsic gate.

- [`retic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/retic_properties.md)
  : Retic properties (WRB 2022)

- [`sideralic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/sideralic_properties.md)
  : Sideralic properties (WRB 2022 Ch 3.2.13)

- [`stagnic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/stagnic_properties.md)
  : Stagnic properties (WRB 2022)

- [`takyric_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/takyric_properties.md)
  :

  Takyric properties (WRB 2022 Ch 3.2.15) – per-pedon test wrapping
  `test_takyric_surface`.

- [`vertic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/vertic_properties.md)
  : Vertic properties (WRB 2022)

- [`vitric_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/vitric_properties.md)
  : Vitric properties (WRB 2022 Ch 3.2.16)

- [`yermic_properties()`](https://hugomachadorodrigues.github.io/soilKey/reference/yermic_properties.md)
  :

  Yermic properties (WRB 2022 Ch 3.2.17) – per-pedon test wrapping
  `test_yermic_surface`.

- [`` `%||%` ``](https://hugomachadorodrigues.github.io/soilKey/reference/grapes-or-or-grapes.md)
  : Default-value-for-NULL operator

- [`arenic_texture()`](https://hugomachadorodrigues.github.io/soilKey/reference/arenic_texture.md)
  : Arenic texture (WRB 2022)

- [`cerosidade()`](https://hugomachadorodrigues.github.io/soilKey/reference/cerosidade.md)
  : Cerosidade quantitativa (SiBCS Cap 13, p 207; Cap 1)

- [`combine_priors()`](https://hugomachadorodrigues.github.io/soilKey/reference/combine_priors.md)
  : Combine multiple spatial priors via weighted geometric mean

- [`cryic_conditions()`](https://hugomachadorodrigues.github.io/soilKey/reference/cryic_conditions.md)
  : Cryic conditions (WRB 2022)

- [`distrofico()`](https://hugomachadorodrigues.github.io/soilKey/reference/distrofico.md)
  : Solo distrofico (SiBCS Cap 1, p 30)

- [`duripa()`](https://hugomachadorodrigues.github.io/soilKey/reference/duripa.md)
  : Duripa (SiBCS Cap 2, p 74; v0.7)

- [`eutrofico()`](https://hugomachadorodrigues.github.io/soilKey/reference/eutrofico.md)
  : Solo eutrofico (SiBCS Cap 1, p 30)

- [`evaluate_rsg_tests()`](https://hugomachadorodrigues.github.io/soilKey/reference/evaluate_rsg_tests.md)
  : Evaluate the test block of a single RSG

- [`fibrico()`](https://hugomachadorodrigues.github.io/soilKey/reference/fibrico.md)
  : Material organico fibrico (SiBCS Cap 14)

- [`fragipa()`](https://hugomachadorodrigues.github.io/soilKey/reference/fragipa.md)
  : Fragipa (SiBCS Cap 2, p 73-74; v0.7)

- [`hemico()`](https://hugomachadorodrigues.github.io/soilKey/reference/hemico.md)
  : Material organico hemico (SiBCS Cap 14)

- [`oxic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/oxic_usda.md)
  : Oxic horizon (USDA Soil Taxonomy)

- [`vertic_horizon()`](https://hugomachadorodrigues.github.io/soilKey/reference/vertic_horizon.md)
  : Vertic horizon (WRB 2022 Ch 3.1)

- [`protovertic()`](https://hugomachadorodrigues.github.io/soilKey/reference/protovertic.md)
  : Protovertic horizon (WRB 2022 Ch 3.1)

- [`panpaic()`](https://hugomachadorodrigues.github.io/soilKey/reference/panpaic.md)
  : Panpaic horizon (WRB 2022 Ch 3.1)

- [`tsitelic()`](https://hugomachadorodrigues.github.io/soilKey/reference/tsitelic.md)
  : Tsitelic horizon (WRB 2022 Ch 3.1)

- [`limonic()`](https://hugomachadorodrigues.github.io/soilKey/reference/limonic.md)
  : Limonic horizon (WRB 2022 Ch 3.1)

- [`make_synthetic_pedon_with_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/make_synthetic_pedon_with_spectra.md)
  : Build a synthetic PedonRecord with attached spectra (testing aid)

- [`mudanca_textural_abrupta()`](https://hugomachadorodrigues.github.io/soilKey/reference/mudanca_textural_abrupta.md)
  : Mudanca textural abrupta (SiBCS Cap 1, p 30-31)

- [`ossl_library_template()`](https://hugomachadorodrigues.github.io/soilKey/reference/ossl_library_template.md)
  : Canonical schema for an \`ossl_library\` object

- [`clear_ossl_cache()`](https://hugomachadorodrigues.github.io/soilKey/reference/clear_ossl_cache.md)
  : Clear the soilKey OSSL cache

- [`download_ossl_subset()`](https://hugomachadorodrigues.github.io/soilKey/reference/download_ossl_subset.md)
  : Download an OSSL subset and return an \`ossl_library\` artefact

- [`download_ossl_subset_with_labels()`](https://hugomachadorodrigues.github.io/soilKey/reference/download_ossl_subset_with_labels.md)
  : Download an OSSL subset and attach WRB / SiBCS / USDA labels

- [`familia_mineralogia_argila_geral()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_mineralogia_argila_geral.md)
  : Familia: mineralogia da fracao argila (geral, nao-Latossolos)

- [`ossl_demo_sa`](https://hugomachadorodrigues.github.io/soilKey/reference/ossl_demo_sa.md)
  : Synthetic OSSL South America demo subset

- [`planosol()`](https://hugomachadorodrigues.github.io/soilKey/reference/planosol.md)
  : Planosol RSG gate (WRB 2022 Ch 4, p 107)

- [`posterior_classify()`](https://hugomachadorodrigues.github.io/soilKey/reference/posterior_classify.md)
  : Bayesian posterior classifier (optional)

- [`resolve_wrb_qualifiers()`](https://hugomachadorodrigues.github.io/soilKey/reference/resolve_wrb_qualifiers.md)
  : Resolve WRB 2022 qualifiers for a Reference Soil Group

- [`saprico()`](https://hugomachadorodrigues.github.io/soilKey/reference/saprico.md)
  : Material organico saprico (SiBCS Cap 14)

- [`soilgrids_usda_lut()`](https://hugomachadorodrigues.github.io/soilKey/reference/soilgrids_usda_lut.md)
  : SoilGrids -\> USDA Soil Order lookup table (placeholder)

- [`soilgrids_wrb_lut()`](https://hugomachadorodrigues.github.io/soilKey/reference/soilgrids_wrb_lut.md)
  : SoilGrids -\> WRB code lookup table

- [`subgrupo_planossolo_espessos()`](https://hugomachadorodrigues.github.io/soilKey/reference/subgrupo_planossolo_espessos.md)
  : Subgrupo "espessos" de Planossolos (B planico profundo, \> 100 cm)

- [`subgrupo_planossolo_mesicos()`](https://hugomachadorodrigues.github.io/soilKey/reference/subgrupo_planossolo_mesicos.md)
  : Subgrupo "mesicos" de Planossolos (B planico topo em \[50, 100\] cm)

- [`subgrupo_plintossolo_endico_concrecionario()`](https://hugomachadorodrigues.github.io/soilKey/reference/subgrupo_plintossolo_endico_concrecionario.md)
  : Subgrupo "endico" de Plintossolos Concrecionarios (topo de horizonte
  concrecionario \>= 40 cm)

- [`subgrupo_plintossolo_endico_litoplintico()`](https://hugomachadorodrigues.github.io/soilKey/reference/subgrupo_plintossolo_endico_litoplintico.md)
  : Subgrupo "endico" de Plintossolos Litoplinticos (topo de horizonte
  litoplintico \>= 40 cm)

- [`subgrupo_plintossolo_espessos()`](https://hugomachadorodrigues.github.io/soilKey/reference/subgrupo_plintossolo_espessos.md)
  : Subgrupo "espessos" de Plintossolos (horizonte plintico topo \> 100
  cm)

## RSG-level gates and other materials

- [`ferralsol()`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralsol.md)
  : Ferralsol RSG gate (WRB 2022 Ch 4, p 110)

- [`gleysol()`](https://hugomachadorodrigues.github.io/soilKey/reference/gleysol.md)
  : Gleysol RSG gate (WRB 2022 Ch 4, p 103)

- [`vertisol()`](https://hugomachadorodrigues.github.io/soilKey/reference/vertisol.md)
  : Vertisol RSG gate (WRB 2022 Ch 4, p 101)

- [`acrisol()`](https://hugomachadorodrigues.github.io/soilKey/reference/acrisol.md)
  : Acrisol RSG diagnostic (WRB 2022)

- [`alisol()`](https://hugomachadorodrigues.github.io/soilKey/reference/alisol.md)
  : Alisol RSG diagnostic (WRB 2022)

- [`lixisol()`](https://hugomachadorodrigues.github.io/soilKey/reference/lixisol.md)
  : Lixisol RSG diagnostic (WRB 2022)

- [`luvisol()`](https://hugomachadorodrigues.github.io/soilKey/reference/luvisol.md)
  : Luvisol RSG diagnostic (WRB 2022)

- [`chernozem()`](https://hugomachadorodrigues.github.io/soilKey/reference/chernozem.md)
  : Chernozem RSG diagnostic (WRB 2022)

- [`chernozem_strict()`](https://hugomachadorodrigues.github.io/soilKey/reference/chernozem_strict.md)
  : Chernozem RSG gate (strengthened, WRB 2022 Ch 4, p 111)

- [`kastanozem()`](https://hugomachadorodrigues.github.io/soilKey/reference/kastanozem.md)
  : Kastanozem RSG diagnostic (WRB 2022)

- [`kastanozem_strict()`](https://hugomachadorodrigues.github.io/soilKey/reference/kastanozem_strict.md)
  : Kastanozem RSG gate (strengthened, WRB 2022 Ch 4, p 112)

- [`phaeozem()`](https://hugomachadorodrigues.github.io/soilKey/reference/phaeozem.md)
  : Phaeozem RSG diagnostic (WRB 2022)

- [`andosol()`](https://hugomachadorodrigues.github.io/soilKey/reference/andosol.md)
  : Andosol RSG gate (WRB 2022 Ch 4, p 104)

- [`calcaric_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/calcaric_material.md)
  : Calcaric material (WRB 2022 Ch 3.3.3): \\= 2% CaCO3 throughout the
  fine earth, primary carbonates from the parent material.

- [`dolomitic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/dolomitic_material.md)
  :

  Dolomitic material (WRB 2022 Ch 3.3.5): \\= 2% Mg-rich carbonate,
  CaCO3/MgCO3 \< 1.5. v0.3.3: detects via designation pattern
  `kdo|do|magn` as proxy when ratio data missing.

- [`hypersulfidic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/hypersulfidic_material.md)
  : Hypersulfidic material (WRB 2022 Ch 3.3.8): \\= 0.01% inorganic
  sulfidic S, pH \\= 4, capable of severe acidification on aerobic
  incubation.

- [`hyposulfidic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/hyposulfidic_material.md)
  :

  Hyposulfidic material (WRB 2022 Ch 3.3.9): same inorganic sulfidic S
  and field pH as hypersulfidic but does NOT consist of hypersulfidic
  (criterion 3 – does not acidify to pH \< 4 on aerobic incubation,
  usually self-neutralised by carbonate). Reachable from v0.9.128: when
  `incubation_ph` is measured, a sulfidic + pH\>=4 layer that stays \>=
  4 on incubation is the set-complement of `hypersulfidic_material` and
  is reported here. Without an incubation pH the two cannot be told
  apart, so this returns empty (the layer is reported as potential
  hypersulfidic instead).

- [`solimovic_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/solimovic_material.md)
  :

  Solimovic material (WRB 2022 Ch 3.3.17): hetero genous mass-movement
  material on slopes / footslopes (formerly "colluvic"). v0.3.3: detects
  via `rock_origin == "colluvial"` OR `layer_origin == "solimovic"`.

- [`technic_hard_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/technic_hard_material.md)
  : Technic hard material (WRB 2022 Ch 3.3.18): consolidated human-made
  material (asphalt, concrete, worked stones).

- [`tephric_material()`](https://hugomachadorodrigues.github.io/soilKey/reference/tephric_material.md)
  : Tephric material (WRB 2022 Ch 3.3.19): \\= 30% volcanic glass in
  0.02-2 mm fraction AND no andic / vitric properties.

## Engine selection and dispatch (v0.9.65)

Per-pedon engine-selection heuristic, engine-aware dispatch (soilKey vs
aqp), and side-by-side comparators.

- [`pick_engine()`](https://hugomachadorodrigues.github.io/soilKey/reference/pick_engine.md)
  : Choose the best diagnostic engine for a single pedon
- [`pick_engine_batch()`](https://hugomachadorodrigues.github.io/soilKey/reference/pick_engine_batch.md)
  : Per-pedon batch engine recommendation
- [`classify_with_engine_heuristic()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_with_engine_heuristic.md)
  : Classify a pedon with the engine chosen by \`pick_engine()\`
- [`compare_engines()`](https://hugomachadorodrigues.github.io/soilKey/reference/compare_engines.md)
  : Side-by-side comparison of soilKey vs aqp diagnostic engines
- [`argic_aqp()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic_aqp.md)
  : Argic / argillic horizon via aqp::getArgillicBounds()
- [`cambic_aqp()`](https://hugomachadorodrigues.github.io/soilKey/reference/cambic_aqp.md)
  : Cambic horizon via aqp::getCambicBounds()

## Canonical references (v0.9.62 – v0.9.65)

Vendored WRB 2022 / KST 13 / Soil Taxonomy criteria and shared lookup
helpers for pkg vs vendored data sources.

- [`canonical_reference()`](https://hugomachadorodrigues.github.io/soilKey/reference/canonical_reference.md)
  : Load a canonical reference dataset from soilKey or SoilTaxonomy
- [`wrb2022_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/wrb2022_canonical.md)
  : WRB 2022 canonical reference (parsed IUSS Working Group WRB 2022)
- [`kst13_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_canonical.md)
  : Keys to Soil Taxonomy 13th edition canonical reference
- [`kst13_codes()`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_codes.md)
  : Load the canonical KST 13ed code -\> taxon-name lookup table
- [`kst13_criteria()`](https://hugomachadorodrigues.github.io/soilKey/reference/kst13_criteria.md)
  : Load the canonical KST 13ed criteria for a single taxon code
- [`st_features_canonical()`](https://hugomachadorodrigues.github.io/soilKey/reference/st_features_canonical.md)
  : USDA Soil Taxonomy diagnostic features canonical table
- [`clear_kst13_cache()`](https://hugomachadorodrigues.github.io/soilKey/reference/clear_kst13_cache.md)
  : Clear the in-memory KST13 cache
- [`coverage_report()`](https://hugomachadorodrigues.github.io/soilKey/reference/coverage_report.md)
  : Honest taxonomic-completeness report

## SmartSolos / Embrapa AgroAPI integration (v0.9.54)

Cross-validation against Embrapa’s PROLOG SiBCS classifier.

- [`classify_via_smartsolos_api()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_via_smartsolos_api.md)
  : Classify a PedonRecord via Embrapa's SmartSolosExpert REST API
- [`compare_smartsolos()`](https://hugomachadorodrigues.github.io/soilKey/reference/compare_smartsolos.md)
  : Cross-validate the local SiBCS classifier against the
  SmartSolosExpert API

## BDsolos loader and benchmarks (v0.9.55 – v0.9.60)

- [`benchmark_bdsolos()`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_bdsolos.md)
  : Benchmark soilKey classifiers against BDsolos national reference
  labels
- [`download_bdsolos()`](https://hugomachadorodrigues.github.io/soilKey/reference/download_bdsolos.md)
  : Download the BDsolos consulta-publica CSV (experimental, requires
  chromote)
- [`inspect_bdsolos_csv()`](https://hugomachadorodrigues.github.io/soilKey/reference/inspect_bdsolos_csv.md)
  : Diagnostic inspection of a BDsolos CSV before loading

## Argic strong-films audit (v0.9.83)

Empirical audit of the SiBCS Cap 18 latossolic-vs-argic precedence rule.
Extracts the strong-clay-films decision into a reusable helper so the
rule can be validated on any benchmark dataset (BDsolos RJ: 0.9%
Latossolo false-positive exclusion rate, 37.6% Argissolo correct
retention rate).

- [`argic_with_strong_clay_films()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic_with_strong_clay_films.md)
  : Test whether a pedon's argic horizon has strong clay films
- [`audit_argic_strong_films()`](https://hugomachadorodrigues.github.io/soilKey/reference/audit_argic_strong_films.md)
  : Audit the strong-clay-films exclusion across a list of pedons

## Lazy-fetch benchmark caches (v0.9.94)

The four large benchmark caches (AfSP, KSSL, KSSL+NASIS, WoSIS
stratified, ~1 MB each) are no longer bundled in the CRAN source
tarball. They are downloaded on demand from a versioned GitHub Release
into the user cache (~/Library/Application Support/…/soilKey/data on
macOS) on first call.

- [`download_extdata_cache()`](https://hugomachadorodrigues.github.io/soilKey/reference/download_extdata_cache.md)
  : Download one or more soilKey lazy-fetch caches from GitHub Release

## FEBR loader (v0.9.57)

UFSM Free Brazilian Repository readers (~249 datasets, ~36k horizons).

- [`read_febr_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/read_febr_pedons.md)
  : Load FEBR datasets as a list of PedonRecord objects
- [`febr_index_munsell()`](https://hugomachadorodrigues.github.io/soilKey/reference/febr_index_munsell.md)
  : Curated index of FEBR datasets that carry Munsell colors

## Redape curated GeoTab dataset (v0.9.71)

Embrapa Redape repository (DOI 10.48432/PYKKA7) – 96 hand- reviewed
Brazilian soil profiles, suitable as a gold-standard benchmark for SiBCS
classification.

- [`download_redape_dataset()`](https://hugomachadorodrigues.github.io/soilKey/reference/download_redape_dataset.md)
  : Download the curated Redape GeoTab dataset (Vaz et al 2023)
- [`load_redape_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_redape_pedons.md)
  : Load curated soil profiles from the Embrapa Redape GeoTab dataset
- [`benchmark_redape()`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_redape.md)
  : Benchmark soilKey SiBCS predictions against the Redape gold standard

## WoSIS stratified WRB benchmark (v0.9.73)

ISRIC WoSIS bundled cache, 130 profiles balanced across 26 WRB Reference
Soil Groups (5 per RSG). The first WRB benchmark with profile depth
(analog of Redape but for WRB), pulled via RSG-filtered GraphQL queries.

- [`load_wosis_stratified_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_wosis_stratified_sample.md)
  : Load the bundled WoSIS stratified RSG-balanced sample (v0.9.73)

## USDA Soil Taxonomy \<-\> WRB cross-walk (v0.9.74)

KSSL/NCSS Lab Data Mart benchmark via IUSS WRB 2022 Annex 6 USDA
Order/Suborder -\> WRB RSG correlation. Provides the richest WRB
benchmark to date (CEC 65%, Ca/Mg/K/Na 40-56% vs WoSIS-stratified ~30%).

- [`usda_to_wrb_rsg()`](https://hugomachadorodrigues.github.io/soilKey/reference/usda_to_wrb_rsg.md)
  : USDA Soil Taxonomy \<-\> WRB Reference Soil Group correlation table
- [`annotate_wrb_from_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/annotate_wrb_from_usda.md)
  : Annotate KSSL/NASIS pedons with a derived WRB Reference Soil Group
- [`benchmark_wrb_vs_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_wrb_vs_usda.md)
  : Benchmark soilKey WRB predictions against a USDA-derived ground
  truth
- [`load_kssl_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_sample.md)
  : Load the bundled KSSL/NCSS lab-data sample (v0.9.74)

## KSSL + NASIS morphological-enriched sample (v0.9.75)

Joins KSSL lab gpkg with NASIS Morphological sqlite – adds Munsell
colours, structure, clay films, slickensides (Munsell 0% -\> 89.6% vs
lab-only).

- [`load_kssl_nasis_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_nasis_sample.md)
  : Load the bundled KSSL + NASIS morphological-enriched sample
  (v0.9.75)

## Africa Soil Profiles (AfSP) WRB benchmark (v0.9.77)

ISRIC AfSP v1.2 (Leenaars et al. 2014) – 18,533 georeferenced African
profiles, ~7000 with WRB 2006 RSG. Loader + bundled 120-pedon stratified
sample (5 per RSG x 24 RSGs). Achieves 28% Order accuracy with strong
Cambisol/Histosol (100%) and Ferralsol/Solonetz (80%) recall.

- [`load_afsp_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_afsp_pedons.md)
  : Load Africa Soil Profiles (AfSP) v1.2 as PedonRecord objects
- [`load_afsp_sample()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_afsp_sample.md)
  : Load the bundled AfSP stratified sample (v0.9.77)
- [`benchmark_afsp()`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_afsp.md)
  : Benchmark soilKey WRB predictions against AfSP ground truth
- [`wrb06_code_to_rsg()`](https://hugomachadorodrigues.github.io/soilKey/reference/wrb06_code_to_rsg.md)
  : WRB 2006 RSG code -\> 2022 RSG name

## LUCAS Soil 2018 (v0.9.49 – v0.9.50)

- [`benchmark_lucas_2018()`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_lucas_2018.md)
  : Run the LUCAS Soil 2018 / ESDB WRB benchmark
- [`attach_lucas_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/attach_lucas_spectra.md)
  : Attach LUCAS 2018 Vis-NIR spectra to a list of PedonRecord objects

## Unified benchmark and robustness (v0.9.61)

- [`benchmark_unified()`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_unified.md)
  : Unified cross-dataset benchmark across SiBCS / WRB / USDA
- [`run_all_benchmarks()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_all_benchmarks.md)
  : Run the full soilKey benchmark suite and (optionally) write a report
- [`benchmark_performance()`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_performance.md)
  : Run the soilKey performance benchmark
- [`batch_robustness()`](https://hugomachadorodrigues.github.io/soilKey/reference/batch_robustness.md)
  : Batch robustness across many pedons
- [`classification_robustness()`](https://hugomachadorodrigues.github.io/soilKey/reference/classification_robustness.md)
  : Robustness of classification under input perturbation
- [`classify_with_uncertainty()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_with_uncertainty.md)
  : Posterior distribution over classification outcomes
- [`get_perturbation_scale()`](https://hugomachadorodrigues.github.io/soilKey/reference/get_perturbation_scale.md)
  : Monte-Carlo perturbation scale for an evidence grade

## OSSL spectra: PLS training and prediction

Build PLS regressors from OSSL Vis-NIR/SWIR spectra and predict lab
properties / Munsell / XYZ / generic targets.

- [`train_pls_from_ossl()`](https://hugomachadorodrigues.github.io/soilKey/reference/train_pls_from_ossl.md)
  : Train pre-trained PLSR models from an OSSL library
- [`predict_from_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_from_spectra.md)
  : Predict soil properties from spectra
- [`predict_lab_from_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_lab_from_spectra.md)
  : Predict CIE Lab from Vis-NIR reflectance spectra
- [`predict_munsell_from_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_munsell_from_spectra.md)
  : Predict Munsell hue / value / chroma from Vis-NIR reflectance
  spectra
- [`predict_xyz_from_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_xyz_from_spectra.md)
  : Predict CIE XYZ tristimulus values from Vis-NIR reflectance spectra
- [`predict(`*`<soilKey_pls_model>`*`)`](https://hugomachadorodrigues.github.io/soilKey/reference/predict.soilKey_pls_model.md)
  : Predict from a soilKey_pls_model
- [`print(`*`<soilKey_pls_model>`*`)`](https://hugomachadorodrigues.github.io/soilKey/reference/print.soilKey_pls_model.md)
  : Print method for soilKey_pls_model
- [`fill_munsell_from_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_munsell_from_spectra.md)
  : Fill missing Munsell colors on a PedonRecord from Vis-NIR spectra

## Spatial / database lookups

ESDB attributes, SoilGrids and MapBiomas-Solos pulls.

- [`available_esdb_attributes()`](https://hugomachadorodrigues.github.io/soilKey/reference/available_esdb_attributes.md)
  : List ESDB Raster Library attributes available at a given root
- [`lookup_esdb()`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_esdb.md)
  : Look up an ESDB raster value at WGS84 coordinates
- [`lookup_soilgrids()`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_soilgrids.md)
  : Look up a SoilGrids 250m soil property at WGS84 coordinates
- [`lookup_mapbiomas_solos()`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_mapbiomas_solos.md)
  : Look up a MapBiomas Solos raster value at WGS84 coordinates

## GlobalSoilMap depth harmonisation and aqp helpers

- [`GSM_DEPTHS`](https://hugomachadorodrigues.github.io/soilKey/reference/GSM_DEPTHS.md)
  : Default GlobalSoilMap depth intervals (cm)
- [`harmonize_to_gsm()`](https://hugomachadorodrigues.github.io/soilKey/reference/harmonize_to_gsm.md)
  : Harmonise pedons to GlobalSoilMap depth intervals
- [`pedon_to_spc()`](https://hugomachadorodrigues.github.io/soilKey/reference/pedon_to_spc.md)
  : Convert a soilKey PedonRecord to an aqp SoilProfileCollection
- [`pedon_json_schema()`](https://hugomachadorodrigues.github.io/soilKey/reference/pedon_json_schema.md)
  : JSON Schema for a soilKey PedonRecord
- [`texture_class_from_pct()`](https://hugomachadorodrigues.github.io/soilKey/reference/texture_class_from_pct.md)
  : NRCS texture-class shorthand from clay / silt / sand percent

## Internal – motor taxonômico

Os preditores atômicos do motor determinístico – qualifiers WRB
(`qual_*`), gates de Subgrupo/Grande-Grupo USDA (`*_usda`), atributos e
horizontes diagnósticos SiBCS (`carater_*`, `horizonte_*`), e os
dispatchers por Ordem. Continuam exportados e chamáveis, mas são
despachados por nome pela chave (`classify_*`); não fazem parte da API
pública curada.

- [`MockVLMProvider`](https://hugomachadorodrigues.github.io/soilKey/reference/MockVLMProvider.md)
  : Mock VLM provider for testing
- [`.BDSOLOS_COLUMN_PATTERNS`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-BDSOLOS_COLUMN_PATTERNS.md)
  : Canonical mapping from BDsolos column-name variants to soilKey
  schema
- [`.BDSOLOS_SITE_PATTERNS`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-BDSOLOS_SITE_PATTERNS.md)
  : Site-level columns (BDsolos full export). Mapped at the site, not
  horizon, level.
- [`.FEBR_TO_HORIZON_MAP`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-FEBR_TO_HORIZON_MAP.md)
  : Map FEBR layer-table columns to soilKey horizon attributes
- [`.GLEYIC_HUE_REGEX`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-GLEYIC_HUE_REGEX.md)
  : Gleyic Munsell hue patterns (WRB 2022, Ch 3.1.13 redoximorphic
  features)
- [`.KST13_CACHE`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-KST13_CACHE.md)
  : Package-level cache for the parsed KST 13ed JSON files
- [`.REDAPE_API_BASE`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-REDAPE_API_BASE.md)
  : Embrapa Redape Dataverse API endpoint
- [`.REDAPE_GEOTAB_DOI`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-REDAPE_GEOTAB_DOI.md)
  : Default DOI for the Vaz et al. 2023 curated GeoTab dataset
- [`.SIBCS_LEGACY_ORDER_MAP`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-SIBCS_LEGACY_ORDER_MAP.md)
  : Pre-2018 SiBCS Order names -\> SiBCS 5a edicao plural Title Case map
- [`.SMARTSOLOS_DRAINAGE_SCALE`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-SMARTSOLOS_DRAINAGE_SCALE.md)
  : SmartSolos drainage class scale (DRENAGEM, 1-8)
- [`.SOILGRIDS_TO_HORIZON_MAP`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-SOILGRIDS_TO_HORIZON_MAP.md)
  : Mapping of SoilGrids 250m property names to soilKey horizon columns
- [`.SOILKEY_LAZY_FETCH_CACHES`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-SOILKEY_LAZY_FETCH_CACHES.md)
  : Caches managed by the v0.9.94 lazy-fetch system
- [`.SOILKEY_LAZY_FETCH_RELEASE`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-SOILKEY_LAZY_FETCH_RELEASE.md)
  : Versioned GitHub Release tag where the lazy-fetch caches are pinned
- [`.WRB_LV1_NAME_BY_CODE`](https://hugomachadorodrigues.github.io/soilKey/reference/dot-WRB_LV1_NAME_BY_CODE.md)
  : WRB Reference Soil Group code-to-name table
- [`soilKey`](https://hugomachadorodrigues.github.io/soilKey/reference/soilKey-package.md)
  [`soilKey-package`](https://hugomachadorodrigues.github.io/soilKey/reference/soilKey-package.md)
  : soilKey: Automated Soil Profile Classification per WRB 2022 and
  SiBCS
