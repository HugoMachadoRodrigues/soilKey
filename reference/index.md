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
- [`classify_from_documents()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_from_documents.md)
  : Build a fully-classified \`PedonRecord\` from documents in one call
- [`classify_by_spectral_neighbours()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_by_spectral_neighbours.md)
  : Classify a soil by spectral similarity to OSSL reference profiles
- [`soil_classes_at_location()`](https://hugomachadorodrigues.github.io/soilKey/reference/soil_classes_at_location.md)
  : Likely soil classes at a geographic location (spatial classification
  aid)

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
  : Hyposulfidic material (WRB 2022 Ch 3.3.9): same S and pH as
  hypersulfidic but does NOT consist of hypersulfidic (i.e. not capable
  of severe acidification). v0.3.3: returns sulfidic layers that don't
  meet hypersulfidic.

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

## WRB 2022 – qualifiers and specifiers (Ch 4–6)

Resolved by the rule-engine in canonical Ch 4 order; the sub-qualifier
suppression machinery (Hyper- / Hypo- / Proto-) and the specifier prefix
dispatch (Ano- / Epi- / Endo- / Bathy- / Panto- / Kato- / Amphi- / Poly-
/ Supra- / Thapto-) run inside
[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md).

- [`qual_abruptic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_abruptic.md)
  : Abruptic qualifier (ap): abrupt textural difference within 100 cm.

- [`qual_aceric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_aceric.md)
  :

  Aceric qualifier (ae): pH (1:1 H2O) \<= 5 in some layer within the
  upper 50 cm. Used for sub-aerially exposed acid-sulfate soils
  (Solonchaks, Gleysols on former tidal flats). v0.9.1: numeric pH gate
  only; v0.9.2 adds the cross-check against `thionic` / sulfidic
  material to disambiguate from naturally acidic Histosols.

- [`qual_acric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_acric.md)
  : Acric qualifier (ac): argic horizon + low CEC + high Al. v0.9:
  argic + CEC \< 24 cmolc/kg clay + exch Al \> Ca+Mg+K+Na.

- [`qual_acroxic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_acroxic.md)
  : Acroxic qualifier (ax): andic + extremely low effective exchange
  complex (Ca + Mg + K + Na exch + 1 N KCl Al-exch \<= 2 cmol+/kg fine
  earth) in some layer of the andic part within 100 cm.

- [`qual_albic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_albic.md)
  : Albic qualifier (ab): albic horizon \<= 100 cm.

- [`qual_alic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_alic.md)
  : Alic qualifier (al): argic + high CEC + high Al saturation.

- [`qual_aluandic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_aluandic.md)
  : Aluandic qualifier (aa): andic properties + Al-dominant active
  component (Al / (Al + 0.5 Si) \>= 0.5 in mass).

- [`qual_andic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_andic.md)
  : Andic qualifier (an): andic OR vitric properties combined \>= 30 cm.
  v0.9 simplification: passes if andic_properties or vitric_properties
  passes within 100 cm.

- [`qual_anthraquic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_anthraquic.md)
  : Anthraquic qualifier (aq): anthraquic horizon (puddled-rice
  surface).

- [`qual_anthric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_anthric.md)
  : Anthric qualifier (ak): anthric properties.

- [`qual_arenic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_arenic.md)
  : Arenic qualifier (ar): texture sand or loamy sand \>= 30 cm in \<=
  100 cm.

- [`qual_aric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_aric.md)
  :

  Aric qualifier (ar): mineral surface horizon homogenised by ploughing
  – designation pattern `Ap`, `Apk`, `Apc`, etc., starting within the
  upper 30 cm.

- [`qual_brunic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_brunic.md)
  :

  Brunic qualifier (br): *incipient-only* subsurface alteration – cambic
  horizon within the upper 100 cm AND no argic, spodic, ferralic, or
  nitic horizon present. Used by WRB 2022 Ch 4 for Arenosols that have
  begun to develop a weak Bw without crossing into Cambisol / Acrisol /
  Lixisol / Ferralsol territory; in those RSGs the cambic alone is the
  gating diagnostic and Brunic would be redundant.

- [`qual_calcaric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_calcaric.md)
  : Calcaric qualifier (cl): calcaric material \>= 25 cm in upper 100
  cm.

- [`qual_calcic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_calcic.md)
  : Calcic qualifier (cc): calcic horizon \<= 100 cm.

- [`qual_cambic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_cambic.md)
  : Cambic qualifier (cm): cambic horizon \<= 50 cm.

- [`qual_carbic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_carbic.md)
  : Carbic qualifier (cb): spodic horizon dominated by humus
  illuviation. v0.9.1: spodic + OC \>= 6% in some spodic layer (the WRB
  threshold for Carbic / "humus-Podzol" expression).

- [`qual_chernic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_chernic.md)
  : Chernic qualifier (ch): chernic horizon (intensely worm-mixed
  mollic-like) within 100 cm.

- [`qual_chromic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_chromic.md)
  : Chromic qualifier (cr): hue redder than 7.5YR + chroma \> 4 (in
  upper subsoil 25-150 cm).

- [`qual_clayic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_clayic.md)
  : Clayic qualifier (ce): clay \>= 60% texture for a layer \>= 30 cm in
  the upper 100 cm.

- [`qual_cryic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_cryic.md)
  : Cryic qualifier (cy): cryic horizon \<= 100 cm.

- [`qual_cumulic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_cumulic.md)
  :

  Cumulic qualifier (cu): a layer of recent depositional material added
  on top of an existing soil. v0.9.3.B proxy: `layer_origin` is fluvic /
  aeolic / solimovic at the top of the profile, OR the uppermost mineral
  horizon's designation matches `^[AC]u?\d?` (cumulic-style suffix).

- [`qual_cutanic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_cutanic.md)
  :

  Cutanic qualifier (ct): visible illuvial clay coatings on argic-
  horizon ped surfaces (the "Cutanic Luvisol" / "Cutanic Argissol"
  signature). v0.9.1: argic horizon passes AND the schema column
  `clay_films_amount` contains "common", "many", or "continuous" (or
  "shiny" – common Brazilian descriptor for nitic surfaces) in some
  argic layer.

- [`qual_densic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_densic.md)
  : Densic qualifier (dn): bulk density \>= 1.8 g/cm3 in some root-
  restricting layer within 100 cm.

- [`qual_dolomitic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_dolomitic.md)
  : Dolomitic qualifier (do): dolomitic material in upper 100 cm.

- [`qual_drainic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_drainic.md)
  :

  Drainic qualifier (dr): artificially drained organic soil. v0.9.1:
  site\$drainage_class or site\$land_use carries an explicit
  *artificial* drainage marker AND organic_material passes. Natural
  drainage classes (e.g. "very poorly drained", "well drained") do NOT
  trigger Drainic on their own.

- [`qual_duric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_duric.md)
  : Duric qualifier (du): duric horizon \<= 100 cm.

- [`qual_dystric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_dystric.md)
  : Dystric qualifier (dy): low base saturation throughout. v0.9: BS \<
  50% from 20 to 100 cm in mineral material.

- [`qual_ekranic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_ekranic.md)
  : Ekranic qualifier (ek): impervious cover (asphalt, concrete)
  starting within 5 cm of the surface. v0.9.1: technic_hard_material
  with top depth \<= 5 cm.

- [`qual_eutric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_eutric.md)
  : Eutric qualifier (eu): high base saturation. v0.9: BS \>= 50%
  throughout 20-100 cm.

- [`qual_eutrosilic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_eutrosilic.md)
  : Eutrosilic qualifier (es): silandic + base saturation \>= 50% in
  some layer of the silandic part within 100 cm.

- [`qual_ferralic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_ferralic.md)
  : Ferralic qualifier (fl): ferralic horizon \<= 150 cm.

- [`qual_ferric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_ferric.md)
  : Ferric qualifier (fr): ferric horizon \<= 100 cm.

- [`qual_fibric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_fibric.md)
  : Fibric qualifier (fi): organic material whose dominant decomposition
  class in the upper 100 cm is fibric (\>= 2/3 fiber). v0.9.1:
  thickness-weighted dominance via Oi designation.

- [`qual_fluvic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_fluvic.md)
  : Fluvic qualifier (fv): fluvic material \>= 25 cm thick starting \<=
  75 cm.

- [`qual_folic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_folic.md)
  : Folic qualifier (fo): folic horizon at the soil surface. v0.9
  delegates to histic_horizon with surface-only filter.

- [`qual_garbic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_garbic.md)
  :

  Garbic qualifier (ga): \>= 20% organic-waste artefacts (landfill
  refuse) in the upper 100 cm. v0.9.1 proxy: designation pattern
  (`Cgarb|garb|landfill|refuse`). Hard schema column
  `artefacts_garbic_pct` scheduled for v0.9.2.

- [`qual_geric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_geric.md)
  : Geric qualifier (gr): in some layer at \<= 100 cm, the effective
  exchange complex (sum of bases + 1 N KCl Al-exchangeable) does not
  exceed 1.5 cmol+/kg fine earth, OR the soil shows net positive charge
  (delta pH = pH_KCl - pH_H2O \> 0). The "or" path makes Geric / Posic
  overlap by design (per WRB Ch 5).

- [`qual_glacic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_glacic.md)
  :

  Glacic qualifier (gc): \>= 75% ice by volume within 100 cm. v0.9.1
  proxy: cryic conditions + designation pattern (`ice|gel|glac`). Schema
  column `ice_pct` scheduled for v0.9.2.

- [`qual_gleyic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_gleyic.md)
  : Gleyic qualifier (gl): gleyic properties throughout a layer \>= 25
  cm starting \<= 75 cm + reducing conditions.

- [`qual_glossic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_glossic.md)
  : Glossic qualifier (gs): mollic horizon penetrated by albeluvic
  tongues (glossae). Diagnostic of Glossic Chernozems / Phaeozems on the
  steppe / forest-steppe transition.

- [`qual_grumic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_grumic.md)
  : Grumic qualifier (gr): strong fine granular surface horizon
  (self-mulching Vertisol).

- [`qual_gypsic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_gypsic.md)
  : Gypsic qualifier (gy): gypsic horizon \<= 100 cm.

- [`qual_gypsiric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_gypsiric.md)
  : Gypsiric qualifier (gc): gypsiric material \>= 25 cm in upper 100
  cm.

- [`qual_haplic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_haplic.md)
  : Haplic qualifier (ha): no other principal qualifier of the RSG
  applies. Always passes; the qualifier resolution machinery uses it as
  the default when no other qualifier matched.

- [`qual_hemic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hemic.md)
  : Hemic qualifier (hc): organic material whose dominant decomposition
  class in the upper 100 cm is hemic (1/6 - 2/3 fiber). v0.9.1:
  thickness-weighted dominance via Oe designation.

- [`qual_histic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_histic.md)
  : Histic qualifier (hi): histic horizon at or near the surface.

- [`qual_hortic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hortic.md)
  : Hortic qualifier (ht): hortic horizon (long-cultivated dark
  surface).

- [`qual_humic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_humic.md)
  : Humic qualifier (hu): \>= 1% SOC in upper 50 cm (weighted average).

- [`qual_hydragric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hydragric.md)
  : Hydragric qualifier (hg): hydragric horizon (puddled-rice
  subsurface).

- [`qual_hydric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hydric.md)
  :

  Hydric qualifier (hy): water content at 1500 kPa \>= 100% (undried
  fine earth, WRB 2022). v0.9.1 accepts the air-dried equivalent (\>=
  70%) when the lab protocol pre-dries; the result is flagged as
  "potentially over-permissive" via the `notes` field when the value
  falls in the 70-100% band.

- [`qual_hyperalbic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hyperalbic.md)
  :

  Hyperalbic qualifier (ha): albic horizon thicker than 100 cm in a
  *contiguous* run (extremely deep eluvial bleaching, common in giant
  Podzols of tropical white-sand systems and the deepest Stagnosol /
  Planosol profiles). Non-contiguous albic layers separated by an
  illuvial Bs / Bt do NOT count toward the threshold.

- [`qual_hyperalic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hyperalic.md)
  : Hyperalic qualifier (yl): argic horizon with Al saturation \>= 50%
  in some layer of the argic part within 100 cm. Stronger version of
  Alic.

- [`qual_hyperartefactic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hyperartefactic.md)
  : Hyperartefactic qualifier (yr): \>= 80% artefacts (any type) in the
  upper 100 cm.

- [`qual_hypercalcic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hypercalcic.md)
  : Hypercalcic qualifier (yc): calcic horizon AND CaCO3 \>= 50% in some
  calcic layer.

- [`qual_hyperdystric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hyperdystric.md)
  : Hyperdystric qualifier (yd): base saturation \< 5% throughout the
  upper 100 cm (mineral soil layers only). Stronger than Dystric (BS \<
  50%).

- [`qual_hypereutric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hypereutric.md)
  : Hypereutric qualifier (ye): base saturation \>= 80% throughout the
  upper 100 cm. Stronger than Eutric (BS \>= 50%).

- [`qual_hypergypsic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hypergypsic.md)
  : Hypergypsic qualifier (yg): gypsic horizon AND gypsum \>= 60% in
  some gypsic layer.

- [`qual_hypersalic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hypersalic.md)
  : Hypersalic qualifier (yz): EC (1:5 H2O extract) \>= 30 dS/m in some
  layer within the upper 100 cm. Stronger than the Salic horizon
  (default \>= 15 dS/m).

- [`qual_hyperskeletic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hyperskeletic.md)
  : Hyperskeletic qualifier (hk): coarse fragments \>= 90% throughout
  the upper 100 cm.

- [`qual_hypersodic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hypersodic.md)
  : Hypersodic qualifier (yo): ESP \>= 50% in some layer within 100 cm.
  Stronger than Sodic (default ESP \>= 6%).

- [`qual_hyperspodic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hyperspodic.md)
  : Hyperspodic qualifier (hp): spodic horizon with very strong active
  Al + Fe accumulation (Al_ox + 0.5 \* Fe_ox \>= 1.5%) – twice the
  minimum spodic threshold per WRB Ch 3.1. v0.9.1 also requires
  p-retention \>= 85% in the same layers when available.

- [`qual_hypocalcic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hypocalcic.md)
  : Hypocalcic qualifier (jc): CaCO3 \>= 5% AND \< 15% in some layer
  within 100 cm (between protocalcic 0.5% and the calcic-horizon 15%
  threshold). Marks the broad "carbonate-bearing" middle band that
  doesn't meet the Calcic horizon.

- [`qual_hypogypsic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hypogypsic.md)
  : Hypogypsic qualifier (jg): gypsum \>= 1% AND \< 5% in some layer
  within 100 cm (below the gypsic-horizon threshold but above the
  protogypsic-properties bare-detection bar).

- [`qual_hyposalic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hyposalic.md)
  : Hyposalic qualifier (jz): EC (1:5 H2O extract) \>= 4 dS/m AND \< 15
  dS/m in some layer within the upper 100 cm. Used for soils too weak to
  qualify as Solonchak but still carrying a salinity tag.

- [`qual_hyposodic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_hyposodic.md)
  : Hyposodic qualifier (jo): ESP \>= 6% AND \< 15% in some layer within
  100 cm. Marginal sodicity tag.

- [`qual_irragric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_irragric.md)
  : Irragric qualifier (ir): irragric horizon (irrigation-deposited
  surface).

- [`qual_lamellic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_lamellic.md)
  :

  Lamellic qualifier (ll): thin (\\\<\\ 5 cm) clay-enriched lamellae,
  typical of sandy Luvisols / Alisols / Acrisols. v0.9.3.B proxy:
  designation pattern `lamell` / `E&Bt` / `&Bt` / `Bt(t)?\d?lam` in any
  subsurface layer.

- [`qual_leptic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_leptic.md)
  : Leptic qualifier (le): continuous rock \<= 100 cm.

- [`qual_limnic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_limnic.md)
  : Limnic qualifier (lm): limnic material (lacustrine / marine
  subaquatic deposits) anywhere in the profile.

- [`qual_linic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_linic.md)
  :

  Linic qualifier (li): continuous artificial geomembrane within 100 cm.
  v0.9.1 proxy: designation pattern (`linic|geomemb|liner`).

- [`qual_lithic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_lithic.md)
  : Lithic qualifier (lt): continuous rock starting within 10 cm.
  Tighter depth gate than Leptic (which is \<= 100 cm) and Nudilithic
  (== 0 cm).

- [`qual_lixic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_lixic.md)
  : Lixic qualifier (lx): argic + low CEC, low Al.

- [`qual_loamic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_loamic.md)
  : Loamic qualifier (lo): loam-class texture \>= 30 cm in the upper 100
  cm.

- [`qual_luvic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_luvic.md)
  : Luvic qualifier (lv): argic + high CEC, low Al saturation.

- [`qual_magnesic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_magnesic.md)
  : Magnesic qualifier (mg): exchangeable Ca/Mg \< 1 in upper 100 cm.

- [`qual_mazic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_mazic.md)
  : Mazic qualifier (mz): structureless / massive surface horizon
  (Vertisol). Diagnostic of slaked, crusted Vertisol surfaces.

- [`qual_melanic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_melanic.md)
  : Melanic qualifier (me): andic + dark high-OC surface horizon.
  v0.9.1: thickness \>= 30 cm within upper 50 cm, OC weighted \>= 6%,
  Munsell value \<= 2 and chroma \<= 2 (moist). Melanic Index \>= 1.7
  (the canonical UV-OD ratio) is deferred to v0.9.2.

- [`qual_mollic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_mollic.md)
  : Mollic qualifier (mo): mollic horizon.

- [`qual_mulmic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_mulmic.md)
  : Mulmic qualifier (ml): mulmic material in upper 100 cm.

- [`qual_natric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_natric.md)
  : Natric qualifier (na): natric horizon \<= 100 cm.

- [`qual_nitic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_nitic.md)
  : Nitic qualifier (ni): nitic horizon \<= 100 cm.

- [`qual_nudilithic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_nudilithic.md)
  : Nudilithic qualifier (nt): continuous rock at the soil surface
  (top_cm == 0).

- [`qual_ochric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_ochric.md)
  : Ochric qualifier (oh): SOC \>= 0.2% upper 10 cm + no mollic/umbric.

- [`qual_organotechnic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_organotechnic.md)
  : Organotechnic qualifier (ot): organotechnic material in upper 100
  cm.

- [`qual_ornithic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_ornithic.md)
  : Ornithic qualifier (oc): ornithogenic material (bird-influenced
  topsoil) in the upper 50 cm.

- [`qual_ortsteinic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_ortsteinic.md)
  : Ortsteinic qualifier (os): cemented spodic horizon. v0.9.1: spodic
  horizon + cementation_class strongly OR indurated.

- [`qual_pachic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_pachic.md)
  : Pachic qualifier (pc): mollic OR umbric horizon \>= 50 cm thick.

- [`qual_pellic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_pellic.md)
  : Pellic qualifier (pe): in the upper 30 cm, Munsell value \<= 4 moist
  AND chroma \<= 2 moist. Diagnostic of "black" (dark) Vertisols.

- [`qual_petric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_petric.md)
  : Petric qualifier (pt): any petro-cemented horizon (petrocalcic /
  petroduric / petrogypsic / petroplinthic) within 100 cm.

- [`qual_petrocalcic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_petrocalcic.md)
  : Petrocalcic qualifier (pc): petrocalcic horizon \<= 100 cm.

- [`qual_petroduric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_petroduric.md)
  : Petroduric qualifier (pd): petroduric horizon \<= 100 cm.

- [`qual_petrogypsic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_petrogypsic.md)
  : Petrogypsic qualifier (pg): petrogypsic horizon \<= 100 cm.

- [`qual_petroplinthic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_petroplinthic.md)
  : Petroplinthic qualifier (pp): petroplinthic horizon \<= 100 cm.

- [`qual_pisoplinthic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_pisoplinthic.md)
  : Pisoplinthic qualifier (px): pisoplinthic horizon within 100 cm.

- [`qual_placic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_placic.md)
  : Placic qualifier (pi): thin (\<= 25 mm = 2.5 cm) cemented Fe pan,
  typically inside or just above a spodic horizon. v0.9.1: a layer with
  cementation_class strongly or indurated AND thickness \<= 2.5 cm,
  anywhere in the upper 100 cm.

- [`qual_plaggic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_plaggic.md)
  : Plaggic qualifier (pa): plaggic horizon (sod-amended surface).

- [`qual_plinthic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_plinthic.md)
  : Plinthic qualifier (pl): plinthic horizon \<= 100 cm.

- [`qual_posic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_posic.md)
  : Posic qualifier (po): net positive permanent charge (pH_KCl \>
  pH_H2O) in some layer at \<= 100 cm. Diagnostic of the most weathered
  Ferralsols where free Fe / Al oxides dominate the surface charge.

- [`qual_pretic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_pretic.md)
  : Pretic qualifier (pt): pretic (pre-Columbian Amerindian dark earth)
  horizon.

- [`qual_profondic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_profondic.md)
  :

  Profondic qualifier (pf): argic horizon that continues, with no clay
  decrease, down to or below 150 cm. v0.9.3.B: requires `argic` to pass
  AND at least one argic layer with `bottom_cm >= 150`.

- [`qual_protic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_protic.md)
  : Protic qualifier (pr): Arenosol (or Regosol) with NO incipient
  subsurface horizon – i.e. an A-over-C profile where no cambic, no
  argic, no spodic, no ferralic, no nitic horizon is present in the
  upper 100 cm. v0.9.1 implements as the conjunction of the "no B
  horizon" diagnostics.

- [`qual_protocalcic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_protocalcic.md)
  :

  Protocalcic qualifier (qc): protocalcic properties (incipient
  carbonate accumulation) within the upper 100 cm. Wraps
  `protocalcic_properties`.

- [`qual_protogypsic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_protogypsic.md)
  :

  Protogypsic qualifier (qg): protogypsic properties (incipient gypsum
  accumulation) within the upper 100 cm. Wraps `protogypsic_properties`.

- [`qual_protovertic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_protovertic.md)
  :

  Protovertic qualifier (qv): protovertic horizon (vertic-spectrum lower
  bound, no slickensides yet but the clay + structure / shrink-swell
  signal is already present) within the upper 100 cm. Wraps
  `protovertic` and is mutually exclusive with the strict Vertic
  qualifier.

- [`qual_reductic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_reductic.md)
  : Reductic qualifier (rd): permanently reducing conditions caused by
  anthropogenic gas / liquid emissions (typical of Technosols on
  landfills). v0.9.1: reducing_conditions + Technic context.

- [`qual_rendzic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_rendzic.md)
  : Rendzic qualifier (rz): mollic horizon directly over calcaric
  material (or limestone), shallow. Defined as Mollic + (Calcaric OR
  continuous rock with carbonate parent material).

- [`qual_retic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_retic.md)
  : Retic qualifier (rt): retic properties \<= 100 cm.

- [`qual_rhodic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_rhodic.md)
  : Rhodic qualifier (ro): hue redder than 5YR + value \< 4 + dry no
  more than 1 unit higher than moist (in upper subsoil 25-150 cm).

- [`qual_rubic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_rubic.md)
  : Rubic qualifier (rb): red Munsell hue \\\le\\ 5YR AND chroma \\\ge\\
  4 in some layer within the upper 100 cm. Less strict than Rhodic
  (which requires \\\le\\ 2.5YR + value \< 4); useful as a supplementary
  tag for tropical soils with reddish colours that don't reach the
  Rhodic threshold.

- [`qual_rustic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_rustic.md)
  : Rustic qualifier (rs): iron-dominated spodic illuviation. v0.9.1:
  spodic + OC \< 1% AND active iron (Fe_ox) \>= 0.5% in the same spodic
  layer (humus-poor, Fe-rich ortstein / Bs).

- [`qual_salic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_salic.md)
  : Salic qualifier (sz): salic horizon \<= 100 cm.

- [`qual_sapric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_sapric.md)
  : Sapric qualifier (sa): organic material whose dominant decomposition
  class in the upper 100 cm is sapric (rubbed fiber \< 1/6). v0.9.1:
  thickness-weighted dominance via Oa designation.

- [`qual_silandic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_silandic.md)
  : Silandic qualifier (sn): andic properties + Si-dominant active
  component (Al / (Al + 0.5 Si) \< 0.5 in mass; allophane-rich).

- [`qual_siltic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_siltic.md)
  : Siltic qualifier (sl): silt or silt-loam texture \>= 30 cm in the
  upper 100 cm.

- [`qual_skeletic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_skeletic.md)
  : Skeletic qualifier (sk): coarse fragments \>= 40% averaged over 100
  cm.

- [`qual_sodic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_sodic.md)
  : Sodic qualifier (so): ESP \>= 6% (incl. SAR-derived).

- [`qual_solimovic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_solimovic.md)
  : Solimovic qualifier (sv): solimovic material (mass-movement
  deposits).

- [`qual_sombric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_sombric.md)
  : Sombric qualifier (sm): sombric horizon (humus-illuviated layer at
  depth) within 200 cm. WRB excludes layers that simultaneously meet
  spodic or ferralic criteria from being Sombric – those have specific
  qualifiers of their own. v0.9.1 enforces both exclusions.

- [`qual_spodic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_spodic.md)
  : Spodic qualifier (sd): spodic horizon \<= 200 cm.

- [`qual_spolic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_spolic.md)
  :

  Spolic qualifier (sp): \>= 20% mineral spoil artefacts (mining /
  industrial-process slag) in the upper 100 cm. v0.9.1 proxy:
  designation pattern (`Cspol|spoil|slag|mine`) or
  `rock_origin == "spoil"`. Hard schema column `artefacts_spolic_pct`
  scheduled for v0.9.2.

- [`qual_stagnic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_stagnic.md)
  : Stagnic qualifier (st): stagnic properties \<= 75 cm.

- [`qual_subaquatic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_subaquatic.md)
  : Subaquatic qualifier (sq): permanently under water. v0.9.1:
  site\$drainage_class == "subaquatic" or "submerged".

- [`qual_sulfidic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_sulfidic.md)
  : Sulfidic qualifier (sf): hyper- OR hyposulfidic material in upper
  100 cm (the WRB Sulfidic qualifier covers either acidification class).

- [`qual_takyric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_takyric.md)
  : Takyric qualifier (ty): takyric properties in upper 50 cm.

- [`qual_technic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_technic.md)
  : Technic qualifier (tc): \>= 20% artefacts in upper 100 cm OR
  equivalent geomembrane / technic-hard cover.

- [`qual_tephric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_tephric.md)
  : Tephric qualifier (tf): tephric material \>= 30 cm in upper 100 cm.

- [`qual_terric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_terric.md)
  : Terric qualifier (te): terric horizon (anthropogenic added mineral
  material on top of cultivated land).

- [`qual_thionic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_thionic.md)
  : Thionic qualifier (tn): thionic horizon within 100 cm.

- [`qual_tidalic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_tidalic.md)
  : Tidalic qualifier (td): subject to tidal flooding. v0.9.1:
  site\$drainage_class contains "tidal".

- [`qual_turbic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_turbic.md)
  :

  Turbic qualifier (tb): cryoturbation features within 100 cm. v0.9.1
  proxy: cryic conditions + designation pattern (`turb|jj|cryot`) OR
  slickensides "common"/"many" in a cryic profile.

- [`qual_umbric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_umbric.md)
  : Umbric qualifier (um): umbric horizon.

- [`qual_urbic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_urbic.md)
  : Urbic qualifier (ub): \>= 20% urbic artefacts (rubble, refuse) in
  the upper 100 cm.

- [`qual_vermic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_vermic.md)
  :

  Vermic qualifier (vm): \>= 50% bioturbation by worm casts / krotovinas
  in the upper 100 cm. v0.9.1: `worm_holes_pct >= 50`.

- [`qual_vertic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_vertic.md)
  : Vertic qualifier (vr): vertic horizon \<= 100 cm.

- [`qual_vetic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_vetic.md)
  : Vetic qualifier (vt): CEC (1 N NH4OAc, pH 7) by clay does not exceed
  6 cmol+/kg clay in some layer at \<= 100 cm. Stronger than the
  ferralic-CEC threshold (\<= 16 cmol+/kg clay).

- [`qual_vitric()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_vitric.md)
  : Vitric qualifier (vi): vitric properties \>= 30 cm within 100 cm.

- [`qual_xanthic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_xanthic.md)
  : Xanthic qualifier (xa): ferralic + hue 7.5YR or yellower + value \>=
  4 + chroma \>= 5.

- [`qual_yermic()`](https://hugomachadorodrigues.github.io/soilKey/reference/qual_yermic.md)
  : Yermic qualifier (ye): yermic properties in upper 50 cm.

## SiBCS 5ª ed. – atributos diagnósticos (Cap 1)

- [`carater_acrico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_acrico.md)
  : Carater acrico (SiBCS Cap 1, p 31)

- [`carater_alitico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_alitico.md)
  : Carater alitico (SiBCS Cap 1, p 32)

- [`carater_arenico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_arenico.md)
  : Carater arenico (SiBCS Cap 5)

- [`carater_argiluvico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_argiluvico.md)
  : Carater argiluvico (SiBCS Cap 1; Cap 6)

- [`carater_cambissolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_cambissolico.md)
  : Carater cambissolico (SiBCS Cap 14)

- [`carater_cambissolico_arg()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_cambissolico_arg.md)
  : Carater cambissolico (Argissolos – Cap 5)

- [`carater_carbonatico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_carbonatico.md)
  : Carater carbonatico (SiBCS Cap 1, p 33)

- [`carater_chernossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_chernossolico.md)
  : Carater chernossolico (SiBCS Cap 5; A chernozemico + Ta alta)

- [`carater_coeso()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_coeso.md)
  : Carater coeso (SiBCS Cap 1, pp 32-33)

- [`carater_durico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_durico.md)
  : Carater durico (SiBCS Cap 1)

- [`carater_ebanico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_ebanico.md)
  : Carater ebanico (SiBCS Cap 1; Cap 7 e Cap 17)

- [`carater_espessarenico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_espessarenico.md)
  : Carater espessarenico (SiBCS Cap 5)

- [`carater_espodico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_espodico.md)
  : Carater espodico (SiBCS Cap 1, p 35; Cap 8)

- [`carater_espodico_profundo()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_espodico_profundo.md)
  : Carater B espodico profundo (SiBCS Cap 8)

- [`carater_eutrico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_eutrico.md)
  : Carater eutrico (SiBCS Cap 1, p 35)

- [`carater_ferrico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_ferrico.md)
  : Carater ferrico (SiBCS Cap 1, p 35; Cap 5 e Cap 10)

- [`carater_fluvico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_fluvico.md)
  : Carater fluvico (SiBCS Cap 1, p 35-36): camadas estratificadas +
  distribuicao irregular de C organico. Reuso de fluvic_material (WRB).

- [`carater_gleissolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_gleissolico.md)
  : Carater gleissolico (SiBCS Cap 5; horizonte_glei em posicao
  nao-Gleissolo)

- [`carater_hidromorfico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_hidromorfico.md)
  : Carater hidromorfico (SiBCS Cap 8)

- [`carater_hipocarbonatico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_hipocarbonatico.md)
  : Carater hipocarbonatico (SiBCS Cap 1, p 33): CaCO3 entre 50 e 150
  g/kg.

- [`carater_humico_espesso()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_humico_espesso.md)
  : Carater espesso-humico (SiBCS Cap 5, p 119)

- [`carater_latossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_latossolico.md)
  : Carater latossolico (SiBCS Cap 5)

- [`carater_leptico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_leptico.md)
  : Carater leptico (SiBCS Cap 5; contato litico em 50-100 cm)

- [`carater_leptofragmentario()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_leptofragmentario.md)
  : Carater leptofragmentario (SiBCS Cap 5; Cr / fragmentary 50-100 cm)

- [`carater_luvissolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_luvissolico.md)
  : Carater luvissolico (SiBCS Cap 5; Ta + S alta)

- [`carater_nitossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_nitossolico.md)
  : Carater nitossolico (SiBCS Cap 5)

- [`carater_palico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_palico.md)
  : Carater palico (SiBCS Cap 11)

- [`carater_perferrico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_perferrico.md)
  : Carater perferrico (SiBCS Cap 1; Cap 6 CX Perferricos)

- [`carater_petroplintico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_petroplintico.md)
  : Carater petroplintico (SiBCS Cap 5)

- [`carater_placico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_placico.md)
  : Carater placico (SiBCS Cap 5; horizonte placico cementado por Fe/Mn)

- [`carater_planossolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_planossolico.md)
  : Carater planossolico (SiBCS Cap 5)

- [`carater_plintico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_plintico.md)
  : Carater plintico (SiBCS Cap 1, p 36): plintita \>= 5% em quantidade
  insuficiente para horizonte plintico.

- [`carater_psamitico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_psamitico.md)
  : Carater psamitico (SiBCS Cap 10)

- [`carater_redoxico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_redoxico.md)
  :

  Carater redoxico (SiBCS Cap 1, p 36-37): feicoes redoximorficas em
  quantidade pelo menos comum, dentro da secao de controle.
  `epirredoxico` se dentro de 50 cm; `endorredoxico` se 50-150 cm.

- [`carater_retratil()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_retratil.md)
  : Carater retratil (SiBCS Cap 1, p 33)

- [`carater_rubrico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_rubrico.md)
  : Carater rubrico (SiBCS Cap 1; Cap 10 Latossolos Brunos)

- [`carater_salico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_salico.md)
  : Carater salico (SiBCS Cap 1, p 38): CE \>= 7 dS/m em alguma epoca.

- [`carater_salino()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_salino.md)
  : Carater salino (SiBCS Cap 1, p 39): 4 \<= CE \< 7 dS/m.

- [`carater_saprolitico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_saprolitico.md)
  : Carater saprolitico (SiBCS Cap 5)

- [`carater_sodico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_sodico.md)
  : Carater sodico (SiBCS Cap 1, p 39): saturacao por sodio (PST) \>=
  15%.

- [`carater_solodico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_solodico.md)
  : Carater solodico (SiBCS Cap 1, p 39): PST entre 6% e \< 15%.

- [`carater_sombrico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_sombrico.md)
  : Carater sombrico (SiBCS Cap 1; Cap 5 PV)

- [`carater_terrico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_terrico.md)
  : Carater terrico (SiBCS Cap 14)

- [`carater_tionico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_tionico.md)
  : Carater tionico (SiBCS Cap 9; Cap 1 thionic-related)

- [`carater_vertissolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_vertissolico.md)
  : Carater vertissolico (SiBCS Cap 6)

- [`atividade_argila_alta()`](https://hugomachadorodrigues.github.io/soilKey/reference/atividade_argila_alta.md)
  : Atividade da fracao argila (SiBCS Cap 1, p 30)

## SiBCS 5ª ed. – horizontes diagnósticos (Cap 2)

- [`horizonte_A_antropico()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_A_antropico.md)
  : Horizonte A antropico (SiBCS) (SiBCS Cap 2, p 53)
- [`horizonte_A_chernozemico()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_A_chernozemico.md)
  : Horizonte A chernozemico (SiBCS Cap 2, p 50-51)
- [`horizonte_A_fraco()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_A_fraco.md)
  : Horizonte A fraco (SiBCS Cap 2, p 53): cor clara + estrutura grao
  simples/maciça + OC \< 6 g/kg; OR espessura \< 5 cm.
- [`horizonte_A_humico()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_A_humico.md)
  : Horizonte A humico (SiBCS Cap 2, p 51-52)
- [`horizonte_A_moderado()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_A_moderado.md)
  : Horizonte A moderado (SiBCS Cap 2, p 53-54): catch-all. Returns TRUE
  quando o solo tem horizonte superficial mas nao se enquadra nas demais
  classes diagnosticas superficiais.
- [`horizonte_A_proeminente()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_A_proeminente.md)
  : Horizonte A proeminente (SiBCS Cap 2, p 52-53)
- [`horizonte_E_albico()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_E_albico.md)
  : Horizonte E albico (SiBCS Cap 2, p 66-67; v0.7)
- [`horizonte_calcico()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_calcico.md)
  : Horizonte calcico (SiBCS Cap 2, p 71-72; v0.7)
- [`horizonte_concrecionario()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_concrecionario.md)
  : Horizonte concrecionario (SiBCS Cap 2, p 68-69; v0.7)
- [`horizonte_glei()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_glei.md)
  : Horizonte glei (SiBCS Cap 2, p 69-71; v0.7)
- [`horizonte_histico()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_histico.md)
  : Horizonte hístico (SiBCS Cap 2, p 49-50)
- [`horizonte_litoplintico()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_litoplintico.md)
  : Horizonte litoplintico (SiBCS Cap 2, p 69; v0.7)
- [`horizonte_petrocalcico()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_petrocalcico.md)
  : Horizonte petrocalcico (SiBCS Cap 2, p 72; v0.7)
- [`horizonte_plintico()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_plintico.md)
  : Horizonte plintico (SiBCS Cap 2, p 67-68; v0.7)
- [`horizonte_sulfurico()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_sulfurico.md)
  : Horizonte sulfurico (SiBCS Cap 2, p 72-73; v0.7)
- [`horizonte_vertico()`](https://hugomachadorodrigues.github.io/soilKey/reference/horizonte_vertico.md)
  : Horizonte vertico (SiBCS Cap 2, p 73; v0.7)
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

## SiBCS 5ª ed. – chave (1º–4º níveis, Caps 3–17)

Per-Ordem dispatchers walk Subordem -\> Grande Grupo -\> Subgrupo.
Triggered by `classify_sibcs(pedon)`.

- [`argissolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/argissolo.md)
  : Argissolos (SiBCS Cap 4, p 114; conceito Cap 3, p 86-88)
- [`argissolo_acinzentado()`](https://hugomachadorodrigues.github.io/soilKey/reference/argissolo_acinzentado.md)
  : Argissolos Acinzentados (SiBCS Cap 5)
- [`argissolo_amarelo()`](https://hugomachadorodrigues.github.io/soilKey/reference/argissolo_amarelo.md)
  : Argissolos Amarelos (SiBCS Cap 5)
- [`argissolo_bruno_acinzentado()`](https://hugomachadorodrigues.github.io/soilKey/reference/argissolo_bruno_acinzentado.md)
  : Argissolos Bruno-Acinzentados (SiBCS Cap 5)
- [`argissolo_vermelho()`](https://hugomachadorodrigues.github.io/soilKey/reference/argissolo_vermelho.md)
  : Argissolos Vermelhos (SiBCS Cap 5)
- [`argissolo_vermelho_amarelo()`](https://hugomachadorodrigues.github.io/soilKey/reference/argissolo_vermelho_amarelo.md)
  : Argissolos Vermelho-Amarelos (catch-all dos Argissolos)
- [`cambissolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/cambissolo.md)
  : Cambissolos (SiBCS Cap 4, p 113; conceito Cap 3, p 88-89)
- [`cambissolo_fluvico()`](https://hugomachadorodrigues.github.io/soilKey/reference/cambissolo_fluvico.md)
  : Cambissolos Fluvicos (Cap 6): carater fluvico.
- [`cambissolo_haplico()`](https://hugomachadorodrigues.github.io/soilKey/reference/cambissolo_haplico.md)
  : Cambissolos Haplicos (catch-all).
- [`cambissolo_histico()`](https://hugomachadorodrigues.github.io/soilKey/reference/cambissolo_histico.md)
  : Cambissolos Histicos (Cap 6): horizonte histico sem espessura para
  Organossolo.
- [`cambissolo_humico()`](https://hugomachadorodrigues.github.io/soilKey/reference/cambissolo_humico.md)
  : Cambissolos Humicos (Cap 6): horizonte A humico.
- [`chernossolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/chernossolo.md)
  : Chernossolos (SiBCS Cap 4, p 113; conceito Cap 3, p 89-90)
- [`chernossolo_argiluvico()`](https://hugomachadorodrigues.github.io/soilKey/reference/chernossolo_argiluvico.md)
  : Chernossolos Argiluvicos (Cap 7): B textural abaixo do A
  chernozemico.
- [`chernossolo_ebanico()`](https://hugomachadorodrigues.github.io/soilKey/reference/chernossolo_ebanico.md)
  : Chernossolos Ebanicos (Cap 7): caracter ebanico em B. v0.7.1:
  detecta via Munsell em B - hue 7.5YR ou mais amarelo: V\<4 + C\<3
  umido; OR hue mais vermelho 7.5YR: preto/cinza muito escuro.
- [`chernossolo_haplico()`](https://hugomachadorodrigues.github.io/soilKey/reference/chernossolo_haplico.md)
  : Chernossolos Haplicos (catch-all).
- [`chernossolo_rendzico()`](https://hugomachadorodrigues.github.io/soilKey/reference/chernossolo_rendzico.md)
  : Chernossolos Rendzicos (Cap 7): A chernozemico +
  (calcico/petrocalcico OR carater carbonatico).
- [`espodossolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/espodossolo.md)
  : Espodossolos (SiBCS Cap 4, p 112; conceito Cap 3, p 90-91)
- [`espodossolo_ferri_humiluvico()`](https://hugomachadorodrigues.github.io/soilKey/reference/espodossolo_ferri_humiluvico.md)
  : Espodossolos Ferri-humiluvicos (Cap 8): B espodico tipo Bhs OR
  catch-all dos espodossolos.
- [`espodossolo_ferriluvico()`](https://hugomachadorodrigues.github.io/soilKey/reference/espodossolo_ferriluvico.md)
  : Espodossolos Ferriluvicos (Cap 8): B espodico tipo Bs (Fe + Al,
  baixo OC iluvial).
- [`espodossolo_humiluvico()`](https://hugomachadorodrigues.github.io/soilKey/reference/espodossolo_humiluvico.md)
  : Espodossolos Humiluvicos (Cap 8): B espodico tipo Bh (org. + Al,
  pouco/sem Fe).
- [`gleissolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/gleissolo.md)
  : Gleissolos (SiBCS Cap 4, p 112-113; conceito Cap 3, p 91-93)
- [`gleissolo_haplico()`](https://hugomachadorodrigues.github.io/soilKey/reference/gleissolo_haplico.md)
  : Gleissolos Haplicos (catch-all).
- [`gleissolo_melanico()`](https://hugomachadorodrigues.github.io/soilKey/reference/gleissolo_melanico.md)
  : Gleissolos Melanicos (Cap 9): horizonte hístico \< 40 cm OR A
  humico, proeminente, chernozemico.
- [`gleissolo_salico()`](https://hugomachadorodrigues.github.io/soilKey/reference/gleissolo_salico.md)
  : Gleissolos Salicos (Cap 9): caracter salico em \< 100 cm.
- [`gleissolo_tiomorfico()`](https://hugomachadorodrigues.github.io/soilKey/reference/gleissolo_tiomorfico.md)
  : Gleissolos Tiomorficos (Cap 9): materiais sulfidricos OR horizonte
  sulfurico em \< 100 cm.
- [`latossolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/latossolo.md)
  : Latossolos (SiBCS Cap 4, p 113; conceito Cap 3, p 93-94)
- [`latossolo_amarelo()`](https://hugomachadorodrigues.github.io/soilKey/reference/latossolo_amarelo.md)
  : Latossolos Amarelos (Cap 10): matiz \\= 7.5YR (mais amarelo).
- [`latossolo_bruno()`](https://hugomachadorodrigues.github.io/soilKey/reference/latossolo_bruno.md)
  : Latossolos Brunos (Cap 10): matiz \\= 7.5YR + valor \\= 4 + croma
  \\= 5 (cores brunadas) OR caracter retratil.
- [`latossolo_ki_kr()`](https://hugomachadorodrigues.github.io/soilKey/reference/latossolo_ki_kr.md)
  : Ki/Kr para Latossolos (SiBCS Cap 10, p 173-176)
- [`latossolo_vermelho()`](https://hugomachadorodrigues.github.io/soilKey/reference/latossolo_vermelho.md)
  : Latossolos Vermelhos (Cap 10): matiz \\= 2.5YR (mais vermelho).
- [`latossolo_vermelho_amarelo()`](https://hugomachadorodrigues.github.io/soilKey/reference/latossolo_vermelho_amarelo.md)
  : Latossolos Vermelho-Amarelos (catch-all).
- [`luvissolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/luvissolo.md)
  : Luvissolos (SiBCS Cap 4, p 113; conceito Cap 3, p 95-96)
- [`luvissolo_cromico()`](https://hugomachadorodrigues.github.io/soilKey/reference/luvissolo_cromico.md)
  : Luvissolos Cromicos (Cap 11): caracter cromico (cores fortes em B).
  Aplicado pela presenca de Munsell vermelho-amarelado em B com cromas
  altos.
- [`luvissolo_haplico()`](https://hugomachadorodrigues.github.io/soilKey/reference/luvissolo_haplico.md)
  : Luvissolos Haplicos (catch-all).
- [`neossolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/neossolo.md)
  : Neossolos (SiBCS Cap 4, p 111-112; conceito Cap 3, p 96-97)
- [`neossolo_fluvico()`](https://hugomachadorodrigues.github.io/soilKey/reference/neossolo_fluvico.md)
  : Neossolos Fluvicos (Cap 12): caracter fluvico em \< 150 cm.
- [`neossolo_litolico()`](https://hugomachadorodrigues.github.io/soilKey/reference/neossolo_litolico.md)
  : Neossolos Litolicos (Cap 12): contato litico ou litico fragmentario
  \\= 50 cm.
- [`neossolo_quartzarenico()`](https://hugomachadorodrigues.github.io/soilKey/reference/neossolo_quartzarenico.md)
  : Neossolos Quartzarenicos (Cap 12): textura areia/areia franca em
  todos os horizontes ate 150 cm + 95% quartzo.
- [`neossolo_regolitico()`](https://hugomachadorodrigues.github.io/soilKey/reference/neossolo_regolitico.md)
  : Neossolos Regoliticos (catch-all dos Neossolos).
- [`nitossolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/nitossolo.md)
  : Nitossolos (SiBCS Cap 4, p 114; conceito Cap 3, p 97-98)
- [`nitossolo_bruno()`](https://hugomachadorodrigues.github.io/soilKey/reference/nitossolo_bruno.md)
  : Nitossolos Brunos (Cap 13): matiz \\= 7.5YR + valor \<= 4 + croma
  \<= 5.
- [`nitossolo_haplico()`](https://hugomachadorodrigues.github.io/soilKey/reference/nitossolo_haplico.md)
  : Nitossolos Haplicos (catch-all).
- [`nitossolo_vermelho()`](https://hugomachadorodrigues.github.io/soilKey/reference/nitossolo_vermelho.md)
  : Nitossolos Vermelhos (Cap 13): matiz \\= 2.5YR.
- [`organossolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/organossolo.md)
  : Organossolos (SiBCS Cap 4, chave do 1o nivel; conceito Cap 3, p
  99-101)
- [`organossolo_folico()`](https://hugomachadorodrigues.github.io/soilKey/reference/organossolo_folico.md)
  : Organossolos Folicos (Cap 14): horizonte O histico (drenado).
  Detectado via designation pattern \\^O\\.
- [`organossolo_haplico()`](https://hugomachadorodrigues.github.io/soilKey/reference/organossolo_haplico.md)
  : Organossolos Haplicos (catch-all).
- [`organossolo_tiomorfico()`](https://hugomachadorodrigues.github.io/soilKey/reference/organossolo_tiomorfico.md)
  : Organossolos Tiomorficos (Cap 14): materiais sulfidricos OR
  horizonte sulfurico em \< 100 cm.
- [`planossolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/planossolo.md)
  : Planossolos (SiBCS Cap 4, p 112; conceito Cap 3, p 101-102)
- [`planossolo_haplico()`](https://hugomachadorodrigues.github.io/soilKey/reference/planossolo_haplico.md)
  : Planossolos Haplicos (catch-all).
- [`planossolo_natrico()`](https://hugomachadorodrigues.github.io/soilKey/reference/planossolo_natrico.md)
  : Planossolos Natricos (Cap 15): caracter sodico em \\ 100 cm.
- [`plintossolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/plintossolo.md)
  : Plintossolos (SiBCS Cap 4, p 113; conceito Cap 3, p 102-104)
- [`plintossolo_argiluvico()`](https://hugomachadorodrigues.github.io/soilKey/reference/plintossolo_argiluvico.md)
  : Plintossolos Argiluvicos (Cap 16): horizonte plintico + B textural
  OR carater argiluvico.
- [`plintossolo_haplico()`](https://hugomachadorodrigues.github.io/soilKey/reference/plintossolo_haplico.md)
  : Plintossolos Haplicos (catch-all).
- [`plintossolo_petrico()`](https://hugomachadorodrigues.github.io/soilKey/reference/plintossolo_petrico.md)
  : Plintossolos Petricos (Cap 16): horizonte concrecionario OR
  litoplintico (sem horizonte plintico precedendo).
- [`vertissolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/vertissolo.md)
  : Vertissolos (SiBCS Cap 4, p 112; conceito Cap 3, p 105-106)
- [`vertissolo_ebanico()`](https://hugomachadorodrigues.github.io/soilKey/reference/vertissolo_ebanico.md)
  : Vertissolos Ebanicos (Cap 17): caracter ebanico em B (cores escuras
  dominantes).
- [`vertissolo_haplico()`](https://hugomachadorodrigues.github.io/soilKey/reference/vertissolo_haplico.md)
  : Vertissolos Haplicos (catch-all).
- [`vertissolo_hidromorfico()`](https://hugomachadorodrigues.github.io/soilKey/reference/vertissolo_hidromorfico.md)
  : Vertissolos Hidromorficos (Cap 17): horizonte glei OR caracter
  redoxico.

## SiBCS 5ª ed. – Família (5º nível, Cap 18)

15 dimensões adjectivais ortogonais (grupamento textural, mineralogia,
atividade da argila, óxidos, ândico, …).

- [`familia_andico()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_andico.md)
  : Familia: propriedades andicas (Cap 1, p 42-43)

- [`familia_atividade_argila()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_atividade_argila.md)
  : Familia: subgrupamento de atividade da fracao argila (Cap 18, p 287)

- [`familia_constituicao_esqueletica()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_constituicao_esqueletica.md)
  : Familia: constituicao esqueletica (Cap 1, p 48)

- [`familia_distribuicao_cascalhos()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_distribuicao_cascalhos.md)
  : Familia: distribuicao de cascalhos no perfil (Cap 1, p 47-48)

- [`familia_grupamento_textural()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_grupamento_textural.md)
  : Familia: grupamento textural (Cap 1, p 46)

- [`familia_label()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_label.md)
  :

  Constroi label textual de Familia a partir de `classify_sibcs_familia`

- [`familia_mineralogia_areia()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_mineralogia_areia.md)
  : Familia: mineralogia da fracao areia (Cap 18, p 286)

- [`familia_mineralogia_argila_geral()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_mineralogia_argila_geral.md)
  : Familia: mineralogia da fracao argila (geral, nao-Latossolos)

- [`familia_mineralogia_argila_latossolo()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_mineralogia_argila_latossolo.md)
  : Familia: mineralogia da fracao argila para Latossolos (Cap 18, p
  286-287)

- [`familia_organossolo_espessura()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_organossolo_espessura.md)
  : Familia: espessura \> 100 cm de material organico em Organossolos
  (Cap 18, p 287)

- [`familia_organossolo_lenhosidade()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_organossolo_lenhosidade.md)
  : Familia: lenhosidade em Organossolos (Cap 18, p 288)

- [`familia_organossolo_material_subjacente()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_organossolo_material_subjacente.md)
  : Familia: material subjacente em Organossolos (Cap 18, p 287)

- [`familia_oxidos_ferro()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_oxidos_ferro.md)
  : Familia: teor de oxidos de ferro (Cap 1, p 42)

- [`familia_prefixo_profundidade()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_prefixo_profundidade.md)
  : Familia: prefixo de profundidade epi-/meso-/endo- (Cap 18, p
  284-285)

- [`familia_saturacao_aluminio()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_saturacao_aluminio.md)
  : Familia: saturacao por aluminio – "alico" (Cap 18, p 285)

- [`familia_saturacao_bases()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_saturacao_bases.md)
  : Familia: saturacao por bases (Cap 18, p 285)

- [`familia_subgrupamento_textural()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_subgrupamento_textural.md)
  : Familia: subgrupamento textural (Cap 18, p 283; em validacao)

- [`familia_tipo_horizonte_superficial()`](https://hugomachadorodrigues.github.io/soilKey/reference/familia_tipo_horizonte_superficial.md)
  : Familia: tipo de horizonte diagnostico superficial (Cap 2)

- [`classify_sibcs_familia()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs_familia.md)
  : Classifica um perfil no 5o nivel categorico do SiBCS (Familia)

## USDA Soil Taxonomy 13ed – Path C (Order -\> Subgroup)

Per-Order subgroup dispatchers; triggered by `classify_usda(pedon)`.

- [`acric_andisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/acric_andisol_usda.md)
  : Acric Subgroup helper (Andisols Acrudoxic / Acraquoxic / Acrustoxic
  / etc.)

- [`acric_oxisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/acric_oxisol_usda.md)
  : Acric Oxisol Suborder helper (Acroperox/Acrudox/Acrustox/Acraquox)
  Pass when oxic or kandic horizon has ECEC \< 1.5 cmol/kg clay AND pH
  (KCl) \>= 5.0.

- [`aeric_oxisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aeric_oxisol_usda.md)
  : Aeric Subgroup (for Oxisols Aquox) – chroma-3 below epipedon Already
  defined for Aquods; here we add Oxisol-specific variant (any 10+ cm
  horizon below A with chroma \>= 3 in 50%+ peds).

- [`aeric_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aeric_subgroup_usda.md)
  : Aeric Subgroup helper (Aquods) Pass when ochric epipedon is present
  (vs. histic/umbric/etc).

- [`al_rich_spodic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/al_rich_spodic_usda.md)
  : Aluminum-rich spodic helper (Alaquods, Alorthods, KST Ch 14)

- [`albaquult_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/albaquult_qualifying_usda.md)
  : Albic-over-argillic qualifying (Albaquults) Pass when albic horizon
  overlies an argillic horizon directly.

- [`albic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/albic_horizon_usda.md)
  : Albic horizon (USDA, KST 13ed Ch 3)

- [`albic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/albic_subgroup_usda.md)
  : Albic Subgroup helper (Albaquultic / Albaquic)

- [`alboll_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/alboll_qualifying_usda.md)
  : Albolls qualifier: mollic + albic + argillic.

- [`alfic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/alfic_subgroup_usda.md)
  : Alfic Subgroup helper (Spodosols): argillic or kandic with BS \>=
  35%

- [`alfisol_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/alfisol_qualifying_usda.md)
  : Alfisol Order qualifier Pass when argillic OR kandic horizon
  present + BS \>= 35% in some part.

- [`alfisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/alfisol_usda.md)
  : Alfisols (USDA Cap 5): argillic/kandic/natric horizon + base
  saturation \>= 35% at the implicit reference depth.

- [`alic_andisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/alic_andisol_usda.md)
  : Alic Subgroup helper (Andisols) Pass when al_kcl_cmol \> 2.0 in a
  10+ cm layer between 25 and 50 cm.

- [`andic_soil_properties_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/andic_soil_properties_usda.md)
  : Andic soil properties (USDA, KST 13ed Ch 3, p 32)

- [`andic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/andic_subgroup_usda.md)
  : Andic Subgroup helper (USDA, KST 13ed)

- [`andisol_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/andisol_qualifying_usda.md)
  : Andisol Order qualifier (USDA, KST 13ed Ch 3, p 7)

- [`andisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/andisol_usda.md)
  : Andisols (USDA Cap 6): andic soil properties \>= 60% of thickness.

- [`anhydrous_conditions_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/anhydrous_conditions_usda.md)
  : Anhydrous conditions (USDA Soil Taxonomy, 13th edition)

- [`anionic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/anionic_subgroup_usda.md)
  : Anionic Subgroup helper (Oxisols)

- [`aqualf_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aqualf_qualifying_usda.md)
  : Aqualf Suborder qualifier (aquic conditions in argillic Alfisol).

- [`aquand_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aquand_qualifying_usda.md)
  : Aquands Suborder qualifier (Cap 6, p 117) Pass when histic OR aquic
  conditions in 40-50 cm with redox features. Simplified: histic OR
  aquic_conditions(max_top=50).

- [`aquandic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aquandic_subgroup_usda.md)
  : Aquandic Subgroup helper (Spodosols / others) Aquic + Andic.

- [`aquent_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aquent_qualifying_usda.md)
  : Aquent Suborder qualifier (Entisol with aquic conditions \<50 cm).

- [`aquept_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aquept_qualifying_usda.md)
  : Aquept Suborder qualifier

- [`aquert_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aquert_qualifying_usda.md)
  : Aquerts qualifier (Vertisols with aquic conditions) Pass when
  aquic_conditions within 50 cm.

- [`aquic_conditions_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aquic_conditions_usda.md)
  : Aquic conditions (USDA Soil Taxonomy, 13th edition)

- [`aquic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aquic_subgroup_usda.md)
  : Aquic Subgroup helper (within 100 cm of mineral soil surface)

- [`aquoll_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aquoll_qualifying_usda.md)
  : Aquolls qualifier (aquic conditions in mollic).

- [`aquult_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aquult_qualifying_usda.md)
  : Aquult Suborder qualifier Pass when aquic_conditions within 50 cm.

- [`arenic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/arenic_subgroup_usda.md)
  : Arenic / Grossarenic Subgroup helper (Spodosols)

- [`argic_aridisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic_aridisol_usda.md)
  : Argic Aridisol helper – argillic-or-kandic in Argids/Cryids/etc.

- [`argic_mollisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic_mollisol_usda.md)
  : Argic Mollisol Suborder helper – delegates argillic_within_usda.

- [`argic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic_subgroup_usda.md)
  : Argic Subgroup helper (Endoaquods/Fragiaquods): argillic or kandic.
  Synonym of ultic at this level. Re-exported for naming clarity.

- [`argillic_or_kandic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/argillic_or_kandic_usda.md)
  : Argillic-or-Kandic helper (USDA, used in Spodosols Subgroups)

- [`argillic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/argillic_usda.md)
  : Argillic horizon (USDA Soil Taxonomy)

- [`argillic_within_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/argillic_within_usda.md)
  : Argillic horizon helper (USDA, KST 13ed Ch 3)

- [`aridisol_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aridisol_qualifying_usda.md)
  : Aridisol Order qualifier (USDA, KST 13ed, Ch 2) Pass when the soil
  has aridic SMR AND any one of: argillic, natric, kandic, calcic,
  petrocalcic, gypsic, petrogypsic, salic, duripan, cambic, sulfuric
  horizon. Also requires no other prior order match.

- [`aridisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/aridisol_usda.md)
  : Aridisols (USDA Cap 7): aridic moisture regime + ochric/anthropic +
  subsurface diagnostic. v0.8 simplification: detected via aridity
  proxies (low EC OR salic OR caracter combinations) + non-mollic
  surface + low OC (no organic accumulation).

- [`calcic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/calcic_horizon_usda.md)
  : Calcic horizon (USDA, delegates to WRB calcic).

- [`calcic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/calcic_subgroup_usda.md)
  :

  Calcic Subgroup helper – delegates to calcic_horizon_usda within
  `max_top_cm`.

- [`classify_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md)
  : Classify a pedon under USDA Soil Taxonomy (13th edition)

- [`cryoturbation_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/cryoturbation_usda.md)
  : Cryoturbation (USDA Soil Taxonomy, 13th edition)

- [`cumulic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/cumulic_subgroup_usda.md)
  : Cumulic Subgroup helper (Mollorthels / Umbrorthels)

- [`densiaquept_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/densiaquept_qualifying_usda.md)
  : Densiaquept qualifying (densic contact within 100 cm)

- [`duric_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/duric_subgroup_usda.md)
  : Duric Subgroup helper (USDA Spodosols)

- [`duripan_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/duripan_usda.md)
  : Duripan (USDA, KST 13ed Ch 3, pp 36-37)

- [`dystric_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/dystric_subgroup_usda.md)
  : Dystric Subgroup helper (Vertisols Dystr\*) Pass when BS (NH4OAc) \<
  50% in some part of the upper 100 cm.

- [`entic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/entic_subgroup_usda.md)
  : Entic Subgroup helper (Spodosols)

- [`entisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/entisol_usda.md)
  : Entisols (USDA Cap 8): catch-all for soils that don't match any
  other Order. Always passes.

- [`episaturation_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/episaturation_usda.md)
  : Episaturation helper (USDA, KST 13ed Ch 3, p 41) Pass when aquic
  conditions PLUS perched water (saturation type "episaturation").

- [`eutric_inceptisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/eutric_inceptisol_usda.md)
  : Eutric Inceptisol Suborder helper (Eutrudepts) Pass when BS (NH4OAc)
  \>= 60% in some part of upper 75 cm.

- [`eutric_oxisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/eutric_oxisol_usda.md)
  : Eutric Oxisol Suborder helper (Eutroperox/Eutrudox/etc.) Pass when
  BS (NH4OAc) \>= 35% in all layers within 125 cm.

- [`eutric_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/eutric_subgroup_usda.md)
  : Eutric Subgroup helper (Andisols) Pass when base_saturation
  (sum-of-cations) \>= 50% in some part.

- [`ferric_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/ferric_subgroup_usda.md)
  : Ferric Subgroup helper (Ferrudalfs) Pass when iron-rich (fe_dcb_pct
  \>= 4%) horizon present in B.

- [`fibric_predominant_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/fibric_predominant_usda.md)
  : Fibric_predominant_usda: Fibrists Suborder qualifier

- [`fibric_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/fibric_subgroup_usda.md)
  : Fibric Subgroup helper (Haplohemists / Haplowassists /
  Sulfiwassists) Pass when fibric layers cumulative thickness \>= 25 cm
  in control section below surface tier.

- [`fluvaquentic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/fluvaquentic_usda.md)
  : Fluvaquentic Subgroup helper (irregular OC decrease + aquic)

- [`fluvent_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/fluvent_qualifying_usda.md)
  : Fluvent Suborder qualifier (irregular OC decrease in 25-125 cm, OR
  layered alluvial designation).

- [`fluventic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/fluventic_usda.md)
  : Fluventic Subgroup helper (irregular OC decrease, NO aquic req.)

- [`folist_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/folist_qualifying_usda.md)
  : Folists Suborder qualifier (KST 13ed, Ch 10, p 200)

- [`folistic_epipedon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/folistic_epipedon_usda.md)
  : Folistic epipedon (USDA Soil Taxonomy, 13th edition)

- [`folistic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/folistic_subgroup_usda.md)
  : Folistic Subgroup helper (folistic_epipedon present)

- [`fragipan_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/fragipan_usda.md)
  : Fragipan (USDA, KST 13ed Ch 3, p 38)

- [`frasic_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/frasic_qualifying_usda.md)
  : Frasiwassists Subgroup helper (Wassists)

- [`fulvic_andisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/fulvic_andisol_usda.md)
  : Fulvic Andisols: similar to melanic but with melanic_index \> 1.70
  (more humic acid). v0.8: detected via OC \>= 6 in cumulative 30 cm but
  WITHOUT melanic_epipedon (since melanic requires index \<= 1.70).

- [`gelisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/gelisol_usda.md)
  : Gelisols (USDA Cap 9): gelic conditions / permafrost.

- [`glacic_layer_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/glacic_layer_usda.md)
  : Glacic layer (USDA Soil Taxonomy, 13th edition)

- [`glossic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/glossic_subgroup_usda.md)
  : Glossic Subgroup helper (Glossaqualfs, Glossocryalfs, Glossudalfs)
  Pass when interfingering of albic materials into argillic horizon is
  detected. v0.8 proxy: albic + argillic + lateral chroma \<= 2 on
  argillic boundary.

- [`grossarenic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/grossarenic_subgroup_usda.md)
  : Grossarenic Subgroup helper: sandy throughout, spodic \>= 125 cm.

- [`gypsic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/gypsic_horizon_usda.md)
  : Gypsic horizon (USDA, delegates to WRB gypsic).

- [`gypsic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/gypsic_subgroup_usda.md)
  : Gypsic Subgroup helper – delegates to gypsic_horizon_usda.

- [`halaquept_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/halaquept_qualifying_usda.md)
  : Halic helper for Halaquepts Pass when EC \>= 8 dS/m within 100 cm.

- [`halic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/halic_subgroup_usda.md)
  : Halic Subgroup helper (Haplosaprists)

- [`hemic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/hemic_subgroup_usda.md)
  : Hemic Subgroup helper

- [`histel_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/histel_qualifying_usda.md)
  : Histels Suborder qualifier (USDA, KST 13ed)

- [`histic_epipedon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/histic_epipedon_usda.md)
  : Histic epipedon (USDA Soil Taxonomy, 13th edition)

- [`histic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/histic_subgroup_usda.md)
  : Histic Subgroup helper (in Spodosols, Aquods) Pass when
  histic_epipedon_usda passes.

- [`histosol_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/histosol_qualifying_usda.md)
  : Histosols Order qualifier (USDA, KST 13ed, Ch 2, p 7)

- [`histosol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/histosol_usda.md)
  : Histosols (USDA Cap 10): organic materials \>= 40 cm in 0-100.
  Refined v0.8.4 – now uses histosol_qualifying_usda (40 cm threshold)
  instead of WRB histic_horizon (10 cm).

- [`humic_andisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/humic_andisol_usda.md)
  : Humic Andisols Subgroup helper Pass when mollic OR umbric epipedon
  present.

- [`humic_inceptisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/humic_inceptisol_usda.md)
  : Humic Inceptisol Suborder helper (Hum\*) Pass when umbric or mollic
  epipedon present + thick (\>= 25 cm).

- [`humic_oxisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/humic_oxisol_usda.md)
  : Humic-Oxisol Subgroup helper Pass when cumulative organic carbon
  mass is \>= 16 kg/m2 between surface and 100 cm (computed as SUM(OC%
  \* bulk_density \* dz)). v0.8 proxy: uses default bulk_density 1.0
  g/cm3 if unavailable.

- [`humic_spodic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/humic_spodic_usda.md)
  : Humic-spodic Suborder/GG check (\>= 6% OC in 10+ cm of spodic)

- [`humic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/humic_subgroup_usda.md)
  : Humic Subgroup helper (Humic Duricryods / Humic Placocryods) Pass
  when spodic horizon has \>= 6% OC in 10+ cm.

- [`humilluvic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/humilluvic_subgroup_usda.md)
  : Humilluvic Subgroup helper (Luvihemists)

- [`humult_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/humult_qualifying_usda.md)
  : Humult Suborder qualifier (Ultisols with thick humus accumulation)
  Pass when 0.9% OC weighted average in 0-15 cm AND/OR organic carbon
  mass \>= 12 kg/m2 in 0-100 cm (proxy via humic_oxisol_usda with lower
  threshold).

- [`hydraquent_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/hydraquent_qualifying_usda.md)
  : Hydric Aquent helper (Hydraquents) Pass when surface 0-50 has high
  water content (n value high). v0.8 proxy: water_content_1500kpa \>=
  80% in surface.

- [`hydric_andisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/hydric_andisol_usda.md)
  : Hydric (Andisols): 1500 kPa water retention \>= 70% on undried
  samples throughout a 35+ cm layer within 100 cm.

- [`hydric_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/hydric_subgroup_usda.md)
  : Hydric Subgroup helper (Histosols Cryofibrists / Sphagnofibrists /
  etc.)

- [`inceptisol_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/inceptisol_qualifying_usda.md)
  : Inceptisol Order qualifier Pass when a cambic horizon is present (no
  argillic, no spodic, no mollic, etc. – enforced by prior order
  exclusion).

- [`inceptisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/inceptisol_usda.md)
  : Inceptisols (USDA Cap 11): cambic horizon (or several alternative
  subsurface diagnostics: folistic/histic/mollic with thin sub, salic,
  sodium-affected sub).

- [`kandic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/kandic_horizon_usda.md)
  : Kandic horizon (USDA, KST 13ed Ch 3, p 45)

- [`kandic_oxisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/kandic_oxisol_usda.md)
  : Kandic Suborder helper for Oxisols (Kandiperox/Kandiudox/Kandiustox)
  Delegates to kandic_horizon_usda.

- [`kanhapl_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/kanhapl_qualifying_usda.md)
  : Kanhapl qualifying helper (Kanhapludults / Kanhaplustults / etc.)
  Pass when kandic horizon present BUT NOT meeting Pale criteria (i.e.
  younger / less developed kandic).

- [`lamellic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/lamellic_subgroup_usda.md)
  : Lamellic Subgroup helper (Spodosols Haplorthods)

- [`limnic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/limnic_usda.md)
  : Limnic Subgroup helper (Histels)

- [`lithic_contact_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/lithic_contact_usda.md)
  : Lithic contact within X cm of the surface (USDA Subgroup helper)

- [`melanic_andisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/melanic_andisol_usda.md)
  : Melanic Andisols: melanic_epipedon present.

- [`melanic_epipedon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/melanic_epipedon_usda.md)
  : Melanic epipedon (USDA Soil Taxonomy, 13th edition)

- [`mollic_epipedon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic_epipedon_usda.md)
  : Mollic epipedon (USDA Soil Taxonomy, 13th edition)

- [`mollisol_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/mollisol_qualifying_usda.md)
  : Mollisol Order qualifier (USDA, KST 13ed, Ch 12) Pass when
  mollic_epipedon AND BS (NH4OAc) \>= 50% in upper 100 cm.

- [`mollisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/mollisol_usda.md)
  : Mollisols (USDA Cap 12): mollic epipedon + base saturation \>= 50%.

- [`natric_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/natric_horizon_usda.md)
  : Natric horizon helper (USDA, KST 13ed Ch 3)

- [`natric_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/natric_subgroup_usda.md)
  : Natric Subgroup helper for Natraquerts.

- [`nitric_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/nitric_subgroup_usda.md)
  : Nitric Subgroup helper (Anhyturbels / Anhyorthels)

- [`normalise_febr_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_usda.md)
  : Normalise FEBR USDA taxon strings to USDA Soil Taxonomy Order

- [`ochric_epipedon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/ochric_epipedon_usda.md)
  : Ochric epipedon (USDA Soil Taxonomy, 13th edition)

- [`oxic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/oxic_horizon_usda.md)
  :

  Oxic horizon (USDA, KST 13ed, Ch 3) Delegates to WRB `ferralic`.

- [`oxic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/oxic_usda.md)
  : Oxic horizon (USDA Soil Taxonomy)

- [`oxisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/oxisol_usda.md)
  : Oxisol (USDA Cap 13): oxic horizon, excluding profiles with an
  argillic horizon overlying the oxic.

- [`oxyaquic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/oxyaquic_subgroup_usda.md)
  : Oxyaquic Subgroup helper (Spodosols, Mollisols, etc.)

- [`pachic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/pachic_subgroup_usda.md)
  : Pachic Subgroup helper (Andisols, Mollisols) Pass when mollic OR
  umbric epipedon is \>= 50 cm thick.

- [`pale_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/pale_qualifying_usda.md)
  : Pale qualifying helper (Paleudults / Paleustults / Palexerults /
  Palehumults / Paleaquults)

- [`paleargid_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/paleargid_qualifying_usda.md)
  : Paleargid qualifying helper Pass when argillic horizon has
  continuous clay films AND clay \>\> 35% in upper 10 cm (proxy for old,
  well-developed argillic). v0.8 proxy: argillic + clay_pct \>= 35 in
  upper 30 cm.

- [`permafrost_within_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/permafrost_within_usda.md)
  : Permafrost (USDA Soil Taxonomy, 13th edition)

- [`petrocalcic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/petrocalcic_subgroup_usda.md)
  : Petrocalcic Subgroup helper (Aridisols Petrocalcids) Cemented calcic
  horizon with cementation_class \>= "strongly".

- [`petroferric_contact_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/petroferric_contact_usda.md)
  : Petroferric contact helper (USDA, KST 13ed Ch 3, p 48)

- [`petrogypsic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/petrogypsic_horizon_usda.md)
  : Petrogypsic horizon helper (USDA)

- [`petrogypsic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/petrogypsic_subgroup_usda.md)
  : Petrogypsic Subgroup helper – delegate to petrogypsic_horizon_usda

- [`petronodic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/petronodic_subgroup_usda.md)
  : Petronodic Subgroup helper (Aridisols) Pass when 5%+ rock fragments
  cemented by carbonates within 100 cm. v0.8 proxy: caco3_pct \>= 15 AND
  coarse_fragments_pct \>= 5.

- [`placic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/placic_horizon_usda.md)
  : Placic horizon (USDA, KST 13ed Ch 3, pp 47-48)

- [`plinth_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/plinth_subgroup_usda.md)
  : Plinth qualifying helper (Plinth\*ults) Pass when plinthite \>= 5%
  in 50%+ of layers within 150 cm.

- [`plinthaquox_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/plinthaquox_qualifying_usda.md)
  : Plinthaquox qualifying helper (Aquox: continuous plinthite phase)
  Pass when plinthite \>= 50% in some 10+ cm layer (continuous phase
  proxy).

- [`plinthic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/plinthic_subgroup_usda.md)
  : Plinthic Subgroup helper (Oxisols) Pass when plinthite \>= 5% in any
  horizon within 125 cm.

- [`psamment_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/psamment_qualifying_usda.md)
  : Psamment Suborder qualifier (sandy texture: clay + 2\*silt \< 30 AND
  no clay films / argillic).

- [`psammentic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/psammentic_subgroup_usda.md)
  : Psammentic Subgroup helper (Aquorthels)

- [`quartzipsamment_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/quartzipsamment_qualifying_usda.md)
  : Quartzipsamment helper (Quartzipsamments: \>= 95% silica) v0.8
  proxy: clay \<= 5% AND coarse_fragments_pct \<= 5%.

- [`rendoll_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/rendoll_qualifying_usda.md)
  : Rendolls qualifier: shallow soil over carbonate parent material.
  Pass when CaCO3 \>= 40% in subsurface AND profile depth \< 100 cm to a
  contact.

- [`rhodic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/rhodic_subgroup_usda.md)
  : Rhodic Subgroup helper (Oxisols, Mollisols, etc.) Pass when 50%+
  colors have hue \<= 2.5YR AND value \<= 3 in B horizons 25-125 cm.

- [`ruptic_histic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/ruptic_histic_subgroup_usda.md)
  : Ruptic-Histic Subgroup helper

- [`ruptic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/ruptic_subgroup_usda.md)
  : Ruptic Subgroup helper (Histoturbels / Historthels)

- [`salic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/salic_horizon_usda.md)
  : Salic horizon (USDA, delegates to WRB salic).

- [`salic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/salic_subgroup_usda.md)
  : Salic Subgroup helper Wraps salic_horizon_usda. Used for
  Salaquerts/Salitorrerts/etc.

- [`sapric_predominant_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/sapric_predominant_usda.md)
  : Sapric_predominant_usda: Saprists Suborder qualifier Pass when
  thickness of sapric \> thickness of fibric+hemic in 0-130 cm.

- [`sapric_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/sapric_subgroup_usda.md)
  : Sapric Subgroup helper (Sphagnofibrists)

- [`smr_aridic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/smr_aridic_usda.md)
  : Aridic soil moisture regime (USDA)

- [`smr_torric_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/smr_torric_usda.md)
  : Torric soil moisture regime (USDA)

- [`smr_udic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/smr_udic_usda.md)
  : Udic soil moisture regime (USDA)

- [`smr_ustic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/smr_ustic_usda.md)
  : Ustic soil moisture regime (USDA)

- [`smr_xeric_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/smr_xeric_usda.md)
  : Xeric soil moisture regime (USDA)

- [`sodic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/sodic_subgroup_usda.md)
  : Sodic Subgroup helper – delegate to natric_horizon (USDA)

- [`soil_moisture_regime_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/soil_moisture_regime_usda.md)
  : Soil moisture regime helper (USDA, KST 13ed Ch 3, pp 50-52)

- [`soil_temperature_regime_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/soil_temperature_regime_usda.md)
  : Soil temperature regime helper (USDA, KST 13ed Ch 3, pp 53-58)

- [`sombric_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/sombric_subgroup_usda.md)
  : Sombric Subgroup helper (Oxisols Sombri-) Pass when sombric horizon
  (humus illuviation in tropics) is present. v0.8: detects via 'sombric'
  designation OR a B horizon with V\<=4 + V\<=4 + chroma\<=2 + OC\>1 in
  50-150 cm.

- [`sphagnic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/sphagnic_usda.md)
  : Sphagnic Subgroup helper (Histels Fibristels)

- [`spodic_andisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/spodic_andisol_usda.md)
  : Spodic-Andisols Subgroup helper Pass when albic horizon overlies a
  cambic OR spodic horizon, OR when a spodic horizon is present in 50%+
  of the pedon.

- [`spodic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/spodic_horizon_usda.md)
  : Spodosols Order qualifier (USDA, KST 13ed)

- [`spodic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/spodic_subgroup_usda.md)
  : Spodic Subgroup helper for Psammorthels/Psammoturbels

- [`spodosol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/spodosol_usda.md)
  : Spodosols (USDA Cap 14): spodic horizon (illuvial Al/Fe/OC).

- [`str_cryic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/str_cryic_usda.md)
  : Cryic soil temperature regime (USDA)

- [`str_gelic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/str_gelic_usda.md)
  : Gelic soil temperature regime (USDA)

- [`sulfic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/sulfic_subgroup_usda.md)
  : Sulfic Subgroup helper (Haplowassists) Pass when sulfidic materials
  within 100 cm.

- [`sulfidic_materials_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/sulfidic_materials_usda.md)
  : Sulfidic materials helper (USDA, KST 13ed Ch 3, p 49)

- [`sulfuric_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/sulfuric_horizon_usda.md)
  : Sulfuric horizon helper (USDA, KST 13ed Ch 3)

- [`terric_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/terric_usda.md)
  : Terric Subgroup helper (Histels)

- [`thaptic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/thaptic_subgroup_usda.md)
  : Thaptic Subgroup helper (Andisols) Pass when, between 25 and 100 cm,
  a 10+ cm layer with OC \> 3.0% and mollic colors exists, underlying
  lighter horizons.

- [`thapto_humic_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/thapto_humic_usda.md)
  : Thapto-Humic Subgroup helper

- [`turbic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/turbic_subgroup_usda.md)
  : Turbic Subgroup helper (Gelods) Pass when gelic materials are
  present within 200 cm. Implementation: cryoturbation + permafrost
  within 200 cm.

- [`ultic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/ultic_subgroup_usda.md)
  : Ultic Subgroup helper: argillic or kandic (any BS).

- [`ultisol_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/ultisol_qualifying_usda.md)
  : Ultisol Order qualifier (USDA, KST 13ed, Ch 2) Pass when argillic OR
  kandic horizon present + BS \< 35% in some part of the upper 200 cm.

- [`ultisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/ultisol_usda.md)
  : Ultisols (USDA Cap 15): argillic/kandic horizon + base saturation \<
  35%.

- [`umbric_epipedon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/umbric_epipedon_usda.md)
  : Umbric epipedon (USDA Soil Taxonomy, 13th edition)

- [`umbric_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/umbric_subgroup_usda.md)
  : Umbric Subgroup helper (in Spodosols) Pass when umbric_epipedon_usda
  passes.

- [`vermic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/vermic_subgroup_usda.md)
  : Vermic Subgroup helper (Vermudolls / Vermustolls) Pass when
  worm_holes_pct \>= 50% in some horizon (KST 13ed worm burrow
  criterion).

- [`vertic_aridisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/vertic_aridisol_usda.md)
  : Vertic Aridisols helper – delegates to vertic_subgroup_usda

- [`vertic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/vertic_subgroup_usda.md)
  : Vertic Subgroup helper (USDA, KST 13ed)

- [`vertisol_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/vertisol_qualifying_usda.md)
  :

  Vertisol Order qualifier (USDA, KST 13ed, Ch 2 / Ch 3 vertic horizon)
  Pass when a vertic horizon (clay \>= 30, cracks, slickensides, LE) is
  present. Delegates to WRB `vertic_horizon`.

- [`vertisol_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/vertisol_usda.md)
  : Vertisols (USDA Cap 16): slickensides + cracks. Delegates to
  vertic_horizon.

- [`vitrand_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/vitrand_qualifying_usda.md)
  : Vitrands qualifier (Cap 6, pp 117-118) Pass when 1500 kPa water
  retention \< 15% (air-dried) and \< 30% (undried) throughout 60%+ of
  the thickness. v0.8 proxy: uses water_content_1500kpa \< 15%.

- [`vitrandic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/vitrandic_subgroup_usda.md)
  : Vitrandic Subgroup helper (USDA, KST 13ed)

- [`vitric_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/vitric_subgroup_usda.md)
  : Vitric Subgroup helper (Andisols) Pass when volcanic_glass_pct \>=
  30 in a 25+ cm layer within 100 cm.

- [`wassent_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/wassent_qualifying_usda.md)
  : Wassent Suborder qualifier (subaqueous Entisol). Pass when
  site\$water_table_cm_above_surface \> 0 (water column permanently
  above the surface).

- [`wassist_qualifying_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/wassist_qualifying_usda.md)
  : Wassists Suborder qualifier (KST 13ed, Ch 10, p 203)

- [`xanthic_subgroup_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/xanthic_subgroup_usda.md)
  : Xanthic Subgroup helper (Oxisols) Pass when 50%+ colors have hue \>=
  7.5YR AND value \>= 6 in B horizons.

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

## Layperson on-ramp

Zero-code Shiny GUI plus auto-detect helpers that remove the friction
non-coders hit on a fresh install.

- [`run_demo()`](https://hugomachadorodrigues.github.io/soilKey/reference/run_demo.md)
  : Launch the soilKey Shiny demo (one-screen GUI)
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
- [`load_lucas_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_lucas_pedons.md)
  : Load EU-LUCAS / ESDB pedons with reference WRB classification
- [`load_embrapa_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_embrapa_pedons.md)
  : Load Embrapa dadosolos pedons with reference SiBCS classification
- [`load_febr_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_febr_pedons.md)
  : Load the Embrapa FEBR superconjunto into a list of PedonRecords
- [`normalise_febr_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_sibcs.md)
  : Normalise a FEBR SiBCS taxon string to soilKey's plural Title Case
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

- [`test_abrupt_textural_change()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_abrupt_textural_change.md)
  : Test for an abrupt textural change between adjacent horizons

- [`test_al_saturation_above()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_al_saturation_above.md)
  : Test that aluminium saturation is at or above a threshold

- [`test_al_saturation_below()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_al_saturation_below.md)
  : Test that aluminium saturation is below a threshold

- [`test_andic_alfe()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_andic_alfe.md)
  : Test the andic Al/Fe oxalate criterion: (al_ox + 0.5\*fe_ox) \>=
  2.0%

- [`test_artefacts_concentration()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_artefacts_concentration.md)
  : Test that artefacts_pct \>= threshold within the upper max_top_cm

- [`test_bs_above()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_bs_above.md)
  : Test that base saturation is at or above a threshold

- [`test_bs_below()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_bs_below.md)
  : Test that base saturation is below a threshold

- [`test_bulk_density_below()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_bulk_density_below.md)
  : Test that bulk density is at or below a threshold

- [`test_caco3_concentration()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_caco3_concentration.md)
  : Test for CaCO3 concentration above threshold (per layer)

- [`test_carbonates_present()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_carbonates_present.md)
  : Test for any layer with caco3_pct above a (low) threshold

- [`test_caso4_concentration()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_caso4_concentration.md)
  : Test for CaSO4 (gypsum) concentration above threshold (per layer)

- [`test_cec_per_clay()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_cec_per_clay.md)
  : Test CEC (1M NH4OAc, pH 7) per kg clay \<= threshold

- [`test_cec_per_clay_above()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_cec_per_clay_above.md)
  : Test that CEC per kg clay is at or above a threshold

- [`test_chernic_color()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_chernic_color.md)
  : Test for chroma \<= 2 (moist) within the upper part of the profile

- [`test_clay_above()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_clay_above.md)
  : Test that clay_pct is at or above a threshold

- [`test_clay_increase_argic()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_clay_increase_argic.md)
  : Test the argic clay-increase criterion (WRB 2022)

- [`test_coarse_texture_throughout()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_coarse_texture_throughout.md)
  : Test for coarse texture throughout the upper part of the profile

- [`test_designation_pattern()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_designation_pattern.md)
  : Test that a horizon designation matches a regex pattern

- [`test_duripan_concentration()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_duripan_concentration.md)
  : Test that duripan_pct \>= threshold (Si-cemented nodules)

- [`test_ec_concentration()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_ec_concentration.md)
  : Test for electrical conductivity above threshold (per layer)

- [`test_ecec_per_clay()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_ecec_per_clay.md)
  : Test effective CEC (sum of bases + Al) per kg clay \<= threshold

- [`test_esp_above()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_esp_above.md)
  : Test exchangeable sodium percentage above threshold

- [`test_fe_dcb_above()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_fe_dcb_above.md)
  :

  Test for high free-iron content (`fe_dcb_pct` \>= threshold)

- [`test_ferralic_texture()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_ferralic_texture.md)
  : Ferralic texture: sandy loam or finer (same predicate as argic)

- [`test_ferralic_thickness()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_ferralic_thickness.md)
  : Ferralic minimum thickness \>= 30 cm (WRB 2022)

- [`test_fluvic_stratification()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_fluvic_stratification.md)
  : Test for fluvic stratification: irregular OC pattern + texture
  variability across consecutive horizons

- [`test_gleyic_features()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_gleyic_features.md)
  : Test for gleyic redoximorphic features within top 50 cm

- [`test_minimum_thickness()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_minimum_thickness.md)
  : Test minimum horizon thickness

- [`test_mollic_base_saturation()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_mollic_base_saturation.md)
  : Mollic base-saturation test (NH4OAc, pH 7, default \>= 50%)

- [`test_mollic_color()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_mollic_color.md)
  : Mollic Munsell color test (WRB 2022)

- [`test_mollic_organic_carbon()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_mollic_organic_carbon.md)
  : Mollic organic-carbon test (WRB 2022, default \>= 0.6%)

- [`test_mollic_structure()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_mollic_structure.md)
  : Mollic structure test (WRB 2022)

- [`test_mollic_thickness()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_mollic_thickness.md)
  : Mollic thickness test (default \>= 20 cm in v0.1)

- [`test_oc_above()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_oc_above.md)
  : Test that organic carbon is at or above a threshold

- [`test_ph_below()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_ph_below.md)
  : Test that ph_h2o is at or below a threshold

- [`test_plinthite_concentration()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_plinthite_concentration.md)
  : Test for plinthite concentration above threshold (per layer)

- [`test_salic_product()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_salic_product.md)
  : Test the salic horizon EC \* thickness product (WRB 2022)

- [`test_slickensides_present()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_slickensides_present.md)
  : Test for slickensides at or above a presence level

- [`test_spodic_aluminum_iron()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_spodic_aluminum_iron.md)
  : Test the spodic Al/Fe oxalate criterion: (al_ox + 0.5\*fe_ox) \>=
  threshold

- [`test_stagnic_pattern()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_stagnic_pattern.md)
  : Test for stagnic redox features (perched water signature)

- [`test_texture_argic()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_texture_argic.md)
  : Test sandy-loam-or-finer texture (used by argic, ferralic)

- [`test_top_at_or_above()`](https://hugomachadorodrigues.github.io/soilKey/reference/test_top_at_or_above.md)
  : Test that a candidate layer starts at or above a top_cm threshold

- [`compute_ki()`](https://hugomachadorodrigues.github.io/soilKey/reference/compute_ki.md)
  : Ki (silica:alumina molar) – SiBCS Cap 1, p 32

- [`compute_kr()`](https://hugomachadorodrigues.github.io/soilKey/reference/compute_kr.md)
  : Kr (silica:sesquioxidos molar) – SiBCS Cap 1, p 32

- [`contato_litico()`](https://hugomachadorodrigues.github.io/soilKey/reference/contato_litico.md)
  :

  Contato litico (SiBCS Cap 1, p 40): rocha continua dura. Reuso de
  `continuous_rock` via designacao R / Cr.

- [`contato_litico_fragmentario()`](https://hugomachadorodrigues.github.io/soilKey/reference/contato_litico_fragmentario.md)
  : Contato litico fragmentario (SiBCS Cap 1, p 40): rocha fragmentada.

- [`load_embrapa_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_embrapa_pedons.md)
  : Load Embrapa dadosolos pedons with reference SiBCS classification

- [`load_febr_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_febr_pedons.md)
  : Load the Embrapa FEBR superconjunto into a list of PedonRecords

- [`load_kssl_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_pedons.md)
  : Load NCSS / KSSL pedons with reference USDA Soil Taxonomy
  classification

- [`load_kssl_pedons_gpkg()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_pedons_gpkg.md)
  : Load KSSL / NCSS pedons from the ncss_labdata GeoPackage

- [`load_lucas_pedons()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_lucas_pedons.md)
  : Load EU-LUCAS / ESDB pedons with reference WRB classification

- [`load_rules()`](https://hugomachadorodrigues.github.io/soilKey/reference/load_rules.md)
  : Load a soilKey rule set (YAML)

- [`format_wrb_name()`](https://hugomachadorodrigues.github.io/soilKey/reference/format_wrb_name.md)
  : Format a WRB 2022 soil name with qualifiers

- [`fibric_predominant_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/fibric_predominant_usda.md)
  : Fibric_predominant_usda: Fibrists Suborder qualifier

- [`sapric_predominant_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/sapric_predominant_usda.md)
  : Sapric_predominant_usda: Saprists Suborder qualifier Pass when
  thickness of sapric \> thickness of fibric+hemic in 0-130 cm.

- [`albic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/albic_horizon_usda.md)
  : Albic horizon (USDA, KST 13ed Ch 3)

- [`calcic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/calcic_horizon_usda.md)
  : Calcic horizon (USDA, delegates to WRB calcic).

- [`gypsic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/gypsic_horizon_usda.md)
  : Gypsic horizon (USDA, delegates to WRB gypsic).

- [`kandic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/kandic_horizon_usda.md)
  : Kandic horizon (USDA, KST 13ed Ch 3, p 45)

- [`natric_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/natric_horizon_usda.md)
  : Natric horizon helper (USDA, KST 13ed Ch 3)

- [`oxic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/oxic_horizon_usda.md)
  :

  Oxic horizon (USDA, KST 13ed, Ch 3) Delegates to WRB `ferralic`.

- [`petrogypsic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/petrogypsic_horizon_usda.md)
  : Petrogypsic horizon helper (USDA)

- [`placic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/placic_horizon_usda.md)
  : Placic horizon (USDA, KST 13ed Ch 3, pp 47-48)

- [`salic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/salic_horizon_usda.md)
  : Salic horizon (USDA, delegates to WRB salic).

- [`spodic_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/spodic_horizon_usda.md)
  : Spodosols Order qualifier (USDA, KST 13ed)

- [`sulfuric_horizon_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/sulfuric_horizon_usda.md)
  : Sulfuric horizon helper (USDA, KST 13ed Ch 3)

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

- [`prompt_path()`](https://hugomachadorodrigues.github.io/soilKey/reference/prompt_path.md)
  : Path to a packaged prompt template

- [`schema_path()`](https://hugomachadorodrigues.github.io/soilKey/reference/schema_path.md)
  : Path to a packaged JSON schema file

- [`default_model()`](https://hugomachadorodrigues.github.io/soilKey/reference/default_model.md)
  : Default VLM model per provider

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
  : Hyposulfidic material (WRB 2022 Ch 3.3.9): same S and pH as
  hypersulfidic but does NOT consist of hypersulfidic (i.e. not capable
  of severe acidification). v0.3.3: returns sulfidic layers that don't
  meet hypersulfidic.

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
