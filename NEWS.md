# soilKey 0.9.403

* CRAN check time. The CRAN check runs a smoke set of tests: one profile
  through each key of the registry, the fixture tests of the WRB 2022,
  SiBCS, USDA, CSSC and ASC keys, the `PedonRecord` class, the registry
  and `classify_all()`. The full suite still runs wherever `NOT_CRAN` is
  `"true"` (devtools and the GitHub checks).
* The vignettes run their code only where `NOT_CRAN` is `"true"`. The
  vignettes shipped with the package and on the website are built with
  every chunk run; the CRAN check rebuilds them without running the code.
* No classification changes.

# soilKey 0.9.402 (2026-11-11)

## Undecided first levels are named; granular structure and the vertic A

* **No class of the first level passed, some undecided: the candidates are named.** An ordered key (`classify_hierarchical()`) whose first level has no class that passes but some left undecided for lack of data now names them, in the order of the key, as the FAO-Unesco legends already did: `"[Undecided in the South African soil classification (1991): Inanda ([Ia]), Arcadia (Ar), or not classified]"`. The levels stay empty, `rsg_or_order` is `NA` and `provisional_because` lists the candidates, so the app's card shows the name instead of "Not classified". The name stays `NA` ("Not classified") only when every class of the first level fails. The two warnings that contradicted each other ("No class of the forms key passed, so the profile is not classified" and "Provisional: 2 class(es) tested before the assigned one could not be decided") are replaced by one that says the key cannot decide between the candidates. Of the national keys, only the South African key of 1991 and the CPCS 1967 have no residual class at their first level, so only they reach this case; the CSSC, ASC, NZSC, HSCS, SCSJ, CSCJ, the Russian key of 2008, KA5 and CST 2001 end their first level with a residual class, the RP 2008 names its undecided references already, and the FAO-Unesco legends of 1988 and 1974 keep their own wording. Under the South African key, 18 of the 44 canonical profiles that gave no name now name their candidate forms (Hugo's decision, 2026-11-11).
* **South Africa 1991: a strong granular structure refuses the vertic A.** With no plasticity index, vertic A criterion (ii) is left undecided (B1s, 0.9.397) only when the strong structure is blocky (angular or subangular), prismatic or columnar, or its type is not recorded; a strong granular or crumb structure refuses the vertic A, and the slickensides decide as before B1s (Q17), the melanic A (ii)-(iii) with them (`thresholds.yaml` `vertic_pi_clay`, `readings.yaml`). The canonical Ferralsol (and the Latossolo), whose A is strong granular with 50 % clay, read as humic A over a red apedal B and key to Inanda, decided (`"Inanda, [Ia]1100 Himeville, clay"`), instead of no name; the Nitossolo keys to Shortlands with its family undecided (`"Shortlands, [Sdx110], clay"`), and the Nitisol loses Arcadia from its candidates (Hugo's decision, 2026-11-11).
* **The canonical Ferralsol records weatherable minerals and water-dispersible clay.** `make_ferralsol_canonical()` (and `make_latossolo_canonical()`, built from it) now has `weatherable_minerals_pct` (2, 2, 1, 1, 1) and `water_dispersible_clay_pct` (20, 12, 4, 0, 0), values a Latossolo has, which the ferralic B of the FAO-Unesco legend of 1988 asks for. The legend of 1988 keys it to Haplic Ferralsols (Podzols left undecided before it) instead of undecided between six groups; the legend of 1974 decides the oxic B of its units and keys it to Humic Ferralsols (organic matter 1.42 % weighted over 100 cm, at least 1.35 %), instead of Orthic Ferralsols with Humic undecided. No other key changes for these two profiles (Hugo's decision, 2026-11-11).
* No other key changes on the 44 canonical profiles. On the 641 scored South African profiles of AfSP the form, family and agreement figures do not change (21.5 % agreement, 210 decided).

# soilKey 0.9.401 (2026-11-10)

## Preparing the CRAN submission

No classification changes; every profile keys as in 0.9.400.

* **Test time on CRAN.** The tests that take more than 0.03 s of CPU when run as CRAN runs them (`NOT_CRAN` unset) are skipped on CRAN: 164 tests in 84 files. One test per classification system still runs there: the new `test-v09401-cran-core-keys.R` classifies one example through each of the SiBCS key and the twelve national keys that had none on CRAN (NZSC, HSCS, SCSJ, CSCJ, CPCS, the Russian 2008 key, RP 2008, KA5, the South African 1991 key, CST 2001, FAO 1988 and FAO 1974); WRB 2022, USDA, CSSC and ASC keep their own fixture tests. The CRAN-mode test run goes from 42 s to 33 s on this Mac (elapsed, the tests and the loading of each file). With `NOT_CRAN=true` every test runs, as before.
* **Examples.** The example of `classify_with_uncertainty()` draws 5 perturbations instead of 50. The examples that reach the network (`read_wosis_profiles_graphql()`, `read_wosis_layers_graphql()` and the USDA axis of `coverage_report()`) run only in an interactive session with a connection.
* **Check notes.** Three `switch()` calls name their first argument `EXPR`, as an alternative named `E` partially matched it; `head()` and `modifyList()` are called as `utils::head()` and `utils::modifyList()`.
* **NEWS.** NEWS.md keeps the entries from 0.9.259 (the version on CRAN) onwards. The older entries are in `NEWS-archive.md` in the source repository, which is not part of the package.

# soilKey 0.9.400 (2026-11-10)

## Faster classifiers, with the same results

The classifiers do less repeated work; no name, code, qualifier, trace, evidence, warning or other part of a result changes.

* **Rule sets read once.** `load_rules()` reads the YAML files of a system once per session, and `resolve_wrb_qualifiers()` its qualifier lists (`qualifiers.yaml`) likewise; before, every classification read them again (and looked for them on disk). A rule set is an R list, so a caller that changes its copy does not change the cache.
* **A memo for one classification.** `classify_wrb2022()`, `classify_usda()`, `classify_sibcs()`, the national keys (through `classify_hierarchical()`), `run_taxonomic_key()`, `run_taxa_list()`, `resolve_wrb_qualifiers()` and the family functions open a memo for the length of their call. Inside it, the diagnostics that the keys call again and again are computed once per pedon, arguments and `soilKey.*` options: the twelve USDA orders (each order calls every order before it, so a USDA classification called `gelisol_usda()` 512 times), `cryic_conditions()`, `spodic()`, `albic()`, `.argic_impl()`, `mollic()`, `ferralic()`, the WRB limiting layers (`.wrb_limiting_layers()`, read by many qualifiers) and the diagnostics they rest on, and the SiBCS B horizons and attributes that its levels call several times. The memo keeps a copy of the horizon table and the site each pedon had: a pedon whose horizons or site are replaced or changed in place inside the call starts afresh, and the memo is dropped when the call returns, so a pedon modified after a classification is read anew by the next one. A diagnostic called on its own is not memoised. Each call still gets a new `DiagnosticResult` (a copy, with the results nested in its evidence copied too), as the keys change some results they receive, and the warnings and messages of the first call are given again at each later one.
* **Designations read once.** The reading of the horizon designations (`.desg_master()`, the older notation, the declared `designation_system`) is kept in a bounded session cache, each entry checked against the designations and the notation it was read with.
* **`classify_with_uncertainty()`** perturbs each column in a plain vector written back once, with the same random draws in the same order, instead of writing each cell into the horizon table.

All in `R/memo.R`, except the perturbation. `coverage_report()` follows a memoised diagnostic to its `.<name>_uncached()` computation, so the coverage it reports is unchanged.

The results were compared before and after, the whole result object of each classification (with its trace, evidence, warnings and messages) serialised: 29,738 results, 0 differences: the 44 canonical profiles under every key (with the WRB specifiers and the USDA and SiBCS families, 880), every example of every key (1,252), `classify_with_uncertainty()` on five profiles under three systems (15), the 974 ISIS reference profiles under the 17 keys (16,558) and 649 AfSP profiles under the 17 keys (11,033). The memo has its own tests (`test-v09400-memo.R`): a pedon modified after a classification, or whose declared notation changes, is classified as a new one.

CPU time per profile (this Mac, the same load for both):

| | 0.9.399 | 0.9.400 |
|---|--:|--:|
| WRB 2022, the 31 canonical WRB profiles | 0.118 s | 0.052 s |
| USDA, the same | 0.468 s | 0.036 s |
| SiBCS, the same | 0.088 s | 0.027 s |
| `classify_with_uncertainty()`, Ferralsol, n = 10 | 24.7 s | 13.9 s |
| the 17 keys on the 974 ISIS profiles (first version of the memo) | 3,216 s | 1,943 s |
| the test suite as CRAN runs it (sum over the tests) | 51.4 s | 43.1 s |

The national keys gain less: their own readers do most of their work.

# soilKey 0.9.399 (2026-11-10)

## The AfSP loader reads drainage, parent material and landform; two old regular expressions fixed

`load_afsp_pedons()` now reads the site fields of AfSP by the words of its dictionary (`AfSP012Qry_DictioClassValues.dbf`, when it is in `afsp_dir`; without it ParMat and LndForm are not read):

* `Drain` into `drainage_class` (excessively, somewhat excessively, well, moderately well, imperfectly, poorly, very poorly drained).
* `ParMat` into `parent_material`, the words of the code composed with those of its ancestors ("volcanic rocks, acid"; "clayey, fluvial, calcareous"; an ancestor whose words the next one repeats is left out, so MA1 is "acid metamorphic rocks, quartzite"). The codes of the unconsolidated deposits that the dictionary does not list (U<texture><carbonates><origin>: U00C, U00L, U00M, U00FC, US00, Ux0F, U02F, UT00, UY2 ...) are decoded by the dictionary's own pattern; "unknown" and "unspecified" are left out. Of the 8082 AfSP profiles that record a ParMat, 40 stay unread (A, Hilly, IS1C, MI1, U000, UC0F, UF02, UQUC, US11, UY2A, UYOC); of the 1752 that also have a WRB 2006 group (the profiles `load_afsp_pedons()` returns), 2 (A, U000).
* `LndForm` into `landform`, only the forms that say something of the origin of the material: depression (LD), low-gradient footslope (LF) and valley floor (LV). The general relief classes (level, sloping and steep land, plain, plateau, dissected plain, hill, mountain, escarpment zone, valley) are not read: 295 of the 2739 profiles with a WRB 2006 group and a landform recorded.

Two regular expressions that read the parent material are fixed:

* **KA5, the P horizon** (`R/diagnostics-ka5-horizons.R`): "ton" matched inside limestone, sandstone, siltstone, ironstone and plutonic rocks. The clay and clay-marl stone is now read by whole words (Ton, Tonstein, Tonschiefer, Geschiebemergel, clay, claystone, marl, shale, pelite). On the AfSP profiles below it no longer matches 42 parent materials (plutonic rocks, sandstone, limestone, ironstone) and changes no KA5 name.
* **SCSJ and NZSC, volcanic ejecta and tephra** (`R/diagnostics-scsj.R`, `R/diagnostics-nzsc-groups.R`, `R/diagnostics-nzsc-subgroups-zlme.R`): a pyroclastic breccia or another consolidated pyroclastic rock is rock, not ejecta or tephra. On AfSP this changes 2 SCSJ names (Allophanic Andosols to Calcareous Eutrosols), the same 2 CSCJ names (which share the SCSJ reader) and 1 NZSC name (Typic Tephric Recent to Weathered Orthic Recent Soils).

The 926 example and canonical profiles of the package (the 31 canonical WRB profiles, the examples of every national key and the 503 FAO examples) keep their names under KA5, SCSJ and NZSC.

Measured on AfSP (offline; the 1149 profiles of 0.9.397: all 649 South African profiles with a WRB 2006 group and 500 others from 31 countries, seed 397), 0.9.397 against 0.9.399, names changed per key. The field that drives each change was found by removing the drainage, the parent material or the landform from the new profiles one at a time; the landform drives none.

| Key | Names changed | Of them South African | What drives it |
|---|--:|--:|---|
| WRB 2022 | 2 | 0 | parent material (the Fluvic qualifier) |
| SiBCS | 34 | 21 | drainage (23 to Gleissolos, 9 gleissolicos) |
| USDA | 0 | 0 | |
| FAO 1988 | 51 | 4 | parent material (the lists of groups left undecided) |
| FAO 1974 | 64 | 15 | drainage 33, parent material 22, the two together 9 |
| CPCS 1967 | 7 | 0 | parent material (5 to alluvial or volcanic apport soils) |
| RP 2008 | 26 | 5 | drainage 24, parent material 2 |
| CSSC | 0 | 0 | |
| ASC | 39 | 8 | drainage: 22 to Hydrosols, 17 Podosols (13 aquic or semiaquic, 4 aeric) |
| NZSC | 4 | 3 | parent material 3, drainage 1 |
| SCSJ 2017 | 27 | 1 | parent material (23 to Fluvic soils or Andosols) |
| CSCJ 2011 | 24 | 1 | parent material (19 to Lowland soils or Andosols) |
| CST 2001 | 45 | 26 | drainage 43 (26 Gleyosols no longer read so), parent material 2 |
| Russia 2008 | 2 | 0 | parent material |
| KA5 | 1 | 1 | drainage |
| HSCS 2024 | 17 | 0 | parent material |
| ZA 1991 | 0 | 0 | |

Agreement with the AfSP labels at the level of the reference group (major soil group for FAO; the AfSP label codes read by the unit and group codes of the edition), the same 1149 profiles, Wilson 95 % intervals:

| Key | Labels | Agree before | Agree after | Decided before | Decided after | Agree among decided, before | after |
|---|--:|--:|--:|--:|--:|--:|--:|
| WRB 2022 RSG vs WRB06rg | 1149 | 333, 29.0 % (26.4-31.7) | 333, 29.0 % (26.4-31.7) | 1149 | 1149 | 29.0 % | 29.0 % |
| FAO 1988 group vs FAO88 | 1145 | 210, 18.3 % (16.2-20.7) | 210, 18.3 % (16.2-20.7) | 477 (41.7 %) | 477 (41.7 %) | 44.0 % (39.6-48.5) | 44.0 % (39.6-48.5) |
| FAO 1974 group vs FAO74 | 404 | 102, 25.2 % (21.3-29.7) | 102, 25.2 % (21.3-29.7) | 239 (59.2 %) | 240 (59.4 %) | 42.7 % (36.6-49.0) | 42.5 % (36.4-48.8) |

No WRB reference group changes. The FAO 1988 grouping changes on 2 profiles (labelled Vertisols and Cambisols: Fluvisols to Regosols), the FAO 1974 grouping on 3 (unlabelled: Fluvisols to Regosols; labelled Podzols: none to Cambisols; labelled Cambisols: Fluvisols to Regosols); none agrees before or after.

# soilKey 0.9.398 (2026-11-10)

## soilKey Pro: the national systems added since 0.9.337 in the app

The systems added since the last published app (Germany KA5, South Africa 1991, China CST 2001, the FAO-Unesco legends of 1988 and 1974) were already offered on the Classify and Pedon tabs; four gaps are closed.

* **The FAO phases and map symbol on the card.** The card of a FAO-Unesco result now lists the phases present or undecided and the texture and slope classes of the map symbol, as the report's section "Phases and map symbol" does (the same helper, `.fao_phase_lines()`), in the session's language.
* **The FAO edition line and the CST equivalents in Portuguese.** In a Portuguese report, and on a card of the app in Portuguese, the words around the codes and class names are translated: "Legenda de 1974", "Legenda Revisada (1988)", the kind of change ("renomeada", "definição alterada" ...), "Ap. 4 p.", "na maior parte", "em parte", "inteiramente", "Tabela 4.2". The conditions of the crosswalk (`also_when` of `inst/rules/fao1988/crosswalk.yaml`) are translated in a new file, `inst/i18n/fao_crosswalk_pt.yaml`. Class names stay as the books print them. In English every line is the one of before, byte for byte; the words now come from `inst/i18n/report_translations.yaml` (22 new keys in each language).
* **The Thanks tab credits every national system.** A new card, "National classification systems", lists each national system of the registry by its short reference, with what soilKey reads from it and the experimental pill while the registry marks it so; a system added to the registry is credited without a change to the app. Every `system.yaml` gains a field `citation_short` (authors, year, title, publisher, no editorial notes); the Thanks card falls back to the full `citation`, then to the name, and the report keeps the full citation.
* **No repeated correlation in the report.** The report's warnings no longer repeat the CST correlation ("Correlation (a note, never a class): ...") when the national section shows it; in a Portuguese report the repeat was in English. The warnings stored in the result are unchanged.
* **Wording (Hugo).** "Textura arenosa" for the coarse texture class of the FAO map symbol (`name_pt` of `phases.yaml`, both editions) and of the crosswalk conditions; "[parte dos ...]" for the 1974 classes that only partly go to a 1988 class; no "chaveado" in the Portuguese texts of the crosswalk.
* **The app test covers every national system.** `test-app-national-systems.R` now checks that every national system of the registry has its examples on the Pedon tab and that a sample of them (first, middle, last) loads and keys to its class with the registry's classifier; new tests cover the FAO phases on the card, the Portuguese notes, and the Thanks card in both languages.

Every classification is unchanged.

# soilKey 0.9.397 (2026-11-10)

## South African classification 1991: the vertic A without a plasticity index, and the AfSP loader reads mottles, clay mineralogy and stickiness

Three confusions of the AfSP benchmark of 0.9.394 were examined and decided by Hugo (2026-11-10). The decisions are recorded in `inst/rules/za1991/readings.yaml`.

* **B1s: Q17 revised for the vertic A.** AfSP records neither slickensides nor a plasticity index, so Q17 (without the plasticity index the slickensides decide) refused the vertic A on every profile, and Arcadia and Rensburg (0 of 29) were not even among the forms left undecided. A measurement not recorded is undecided, so now, with no plasticity index and no diagnostic slickensides, criterion (ii) of the vertic A, and with it (ii)-(iii) of the melanic A, is undecided when the layer has a strong structure (or none recorded) and 30 % clay or more (or none recorded). The 30 % is `vertic_pi_clay` in `thresholds.yaml`, flagged as a stand-in (all 71 vertic A of the land type survey have 30 % clay or more; van Huyssteen 2020). Elsewhere Q17 stands, and a plasticity index recorded decides as before. Without the structure gate the melanic A of the 35 melanic-form examples became undecided; with it the examples are unchanged.
* **A-i: D8 kept for the neocutanic B.** A uniform red or yellow-brown layer with a weak blocky structure stays an apedal B. Reading it as a neocutanic B cost 19 Huttons for 6 Oakleafs on AfSP; reading it so only when the layer is not red gained 4 Oakleafs, lost a Kranskop and moved Avalon profiles to Oakleaf. AfSP records no variegation, cutans or origin that could evidence the neocutanic B, so Oakleaf read as Hutton (6) stays.
* **MP1: mottles recorded only as present.** The soft plinthic B needs more than 10 % mottles; AfSP records mottles only as present or absent (field `Mottling`, 99 South African layers, none in the 5 Bainsvlei profiles), and the loader did not read it. A new horizon attribute, `mottles_present` (logical), holds it: `TRUE` with no amount or colour leaves the soft plinthic B, the low-chroma mottles of the signs of wetness and the signs of wetness of a soft carbonate horizon (FC 15) undecided, and then only the amount of the mottles (`redoximorphic_features_pct`) decides: plinthite or ferric nodules recorded, even as 0, do not stand in for it. `FALSE` or not recorded, no mottles. The key is not changed; Bainsvlei stays unreached from the AfSP data.

`load_afsp_pedons()` now reads: `Mottling` into `mottles_present`; `ClyMinera` into `clay_mineralogy` (montmorillonitic and SM as smectite, kaolinitic, illitic, vermiculitic, interstratified; chloritic, sesquioxidic, allophane, KA/IL and gibbsite have no `clay_mineralogy` word and are read as not recorded, with a provenance note); and `Sticknss` into `stickiness` (sticky as moderately sticky). Each code read goes to the provenance. ClyMinera is an estimate (a dominant clay mineral given to the layer), not a measurement: it is recorded with the provenance `inferred_prior` (confidence 0.5), so the evidence grade of a result on such a profile drops to C, as for the other estimated values; the names are not changed by it (383 of the 1149 profiles below carry it). The site fields `Drain`, `ParMat` and `LndForm` are not read in this version. `inst/schemas/horizon-attributes.yaml`, `inst/schemas/pedon-schema.json` and `inst/ATTRIBUTES.md` carry the new attribute.

Measured on AfSP (offline; 1149 profiles: all 649 South African profiles with a WRB 2006 group and 500 others from 31 countries, sampled with seed 397), classified with the 17 keys before and after: names change only in three keys.

| Key | Names changed | Of them South African | Cause |
|---|--:|--:|---|
| FAO 1974 | 9 | 8 | clay mineralogy (Chromic Cambisols and Orthic Solonchaks to Orthic Luvisols 2 each, Haplic to Luvic Phaeozems 2, Chromic to Eutric Cambisols 1, Eutric Cambisols to Orthic Luvisols 1, an undecided profile to Eutric Nitosols 1) |
| RP 2008 | 12 | 7 | clay mineralogy (fersialsol insature to non rattache 8, to brunisol dystrique or eutrique 3; non rattache to brunisol dystrique 1) |
| ZA 1991 | 13 | 6 | B1s 9, mottles present 4 |

WRB 2022, SiBCS, USDA, FAO 1988, CPCS, CSSC, ASC, NZSC, SCSJ, CSCJ, CST, the Russian, KA5 and HSCS keys give the same names. Recorded as an estimate, the clay mineralogy changes the same names (the FAO 1974 Luvisols included): the provenance lowers the evidence grade, it does not change a test. The soft carbonate horizon and the rule on plinthite with mottles present change no name on these profiles. The AfSP benchmark row of the suite (`benchmark_afsp()` on `load_afsp_sample()`) reads a snapshot built by the loader of v0.9.77, so the loader change does not reach it.

The AfSP run of the South African benchmark (641 scored) with the profiles of the new loader:

* Main figure: 138 of 641 agree, 21.5 % (0.9.394: 139, 21.7 %). soilKey names a form for 210 (was 216), and among those the form agrees for 65.7 % (was 64.4 %). 429 stop with forms undecided and the label's form is among them for 340 (was 315 of 423): Arcadia now for 15 of 19 and Rensburg for 8 of 10 (both 0 before). Two Arcadia keyed as Inhoek and one Milkwood keyed right now stop with the vertic A undecided. Mottles recorded as present change no South African form.
* Second figure, the humic A of 1977: 151 of 641 agree (was 154).
* The 73 form examples, the 404 family examples and the special cases (555 runs) keep their names and provisional status.

# soilKey 0.9.396 (2026-11-10)

## FAO-Unesco legend: the WoSIS benchmark

A second offline benchmark of the FAO-Unesco legends, `inst/benchmarks/fao_wosis.R`, compares both editions with the FAO-Unesco names of ISRIC's WoSIS profiles (fields `faoMajorGroup`, `faoSoilUnit`, `faoPublicationYear`): a label of 1974 with `classify_fao(edition = "1974")`, a label of the year 1997 (ISRIC's reprint of the Revised Legend, read as fao1988, Hugo's decision Q1) with `classify_fao()`. Like the ISIS benchmark it downloads nothing: it reads a cache folder saved by the user (`listing.rds`, the profile records; `raw/<profileId>.rds`, the layers of each profile; optionally `manifest.csv`) from its first argument, the option `soilKey.wosis_fao_cache`, `SOILKEY_WOSIS_FAO_CACHE` or the user cache folder. Run with no cache found it says so and exits cleanly; a cache path given explicitly that does not exist stops with an error. WoSIS data are not shipped; the report cites them as ISRIC asks (`wosis_citation()`, Batjes, Calisto and de Sousa 2024) with the date of the download.

The layers of the cache are read exactly as the live reader reads them: the conversion step of `read_wosis_layers_graphql()` (depth order, the shift of an organic surface to 0 cm, units, provenance, the most restrictive licence of the layers) is now an internal function, `.wosis_layers_to_horizons()`, which the live reader calls after its download. The live path is unchanged: on synthetic replies and on 300 saved profiles its output is byte-identical before and after.

How the labels are scored (Hugo's decisions of 2026-11-10). A label "<unit> <group>" is read by `.fao_match_label()` in the edition of its year, exact names only; a label of another year is counted and not scored. A unit that is no unit of its edition (the national adaptations: "Chromic Regosol" of 1974, "Plinthic Calcisol" of 1997) is scored at grouping level only and listed as an unmatched unit (Q3). WoSIS codes every Kastanozem of 1974 "KS" and every Solonchak "SC", the codes of 1988, under names that are valid 1974 names: they are read by name as K and Z and scored with the rest of 1974. Profiles of the dataset WD-ISIS are left out, since the ISIS benchmark scores them (Q7). All licences are scored, with the figures on the CC BY profiles beside them (Q4). The Legend of 1974 is also given for Mexico and for the rest. The headline gives four figures together (Q6): agreement over all labels (a fall-through counts as a disagreement), coverage (soilKey names a grouping), agreement among the covered labels, and "not contradicted": the grouping agrees, or the label's grouping is one soilKey left undecided, the candidates of a fall-through or a grouping tested before a provisional class. Beside it, a shuffled-label baseline (the same rate with the labels permuted within the subset, 200 permutations, fixed seed, the caller's random numbers untouched), and the agreement among firm classifications (covered and not provisional).

The sample was drawn proportionally with a minimum of one profile per stratum (edition x country x group code), which over-represents the small strata: in 1988 the 996 strata that owe their one profile to the minimum make 33 % of the sample but 12 % of the profiles with layers. The benchmark reads the design back from the cache and weighs each profile N_h / n_h; the weighted figures are the headline, with Wilson intervals on Kish's effective sample size (an approximation), and the unweighted figures beside them.

The run of 2026-11-10 on 6,000 WoSIS profiles with layers, 3,000 per edition (38,695 profiles carry an FAO-Unesco label, 33,203 with layers; no profile of the sample had more than 40 layers), read with the 15 properties of soilKey's WoSIS map that the keys use (pH KCl, P retention and the 1500 kPa water content added to the first download; no profile of the sample has the last); 139 WD-ISIS profiles left out, 5,861 scored. Weighted (unweighted in brackets):

- Revised Legend, 2,925 labels: agreement 2.4 % (1.8-3.1) [3.4 %]; coverage 7.7 % (6.6-8.9) [10.1 %]; among covered 30.9 % (25.0-37.5) [33.8 %]; not contradicted 62.3 % (60.2-64.4) [60.9 %] against a shuffled-label baseline of 34.2 % [32.9 %]; among the 99 firm classifications 60.4 % (48.2-71.5) [64.6 %]. Unit 45 of 2,514 (1.8 %), 51.1 % where the grouping agrees. CC BY only (1,824): agreement 3.1 %, coverage 10.6 %, among covered 28.9 %, not contradicted 63.1 % (baseline 31.3 %).
- Legend of 1974, 2,936 labels: agreement 10.0 % (9.0-11.1) [10.0 %]; coverage 26.0 % (24.5-27.6) [26.1 %]; among covered 38.5 % (35.1-42.0) [38.4 %]; not contradicted 62.1 % (60.3-63.8) [62.2 %] against a baseline of 39.8 %; only 13 classifications are firm, 5 of them agree. Unit 178 of 2,471 (7.2 %), 70.1 % where the grouping agrees. Mexico (2,712): agreement 10.5 %, coverage 26.9 %, among covered 39.2 %; outside Mexico (224): 3.6 %, 15.4 %, 23.6 %. By group, the Solonchaks (Z, coded SC) agree on 67 of 111 (98.5 % of the 68 covered), the Kastanozems (K, coded KS) on none of 70 (11 covered). No CC BY profile of 1974 remains once WD-ISIS is left out.
- Labels: every group name is read; 61 labels of 1997 and 418 of 1974 name a unit outside their edition (Chromic Regosol 155, Chromic Phaeozem 41, Vitric Gleysol 40 of 1974; Luvic Regosol, Plinthic Calcisol and Sodic Cambisol 6 each of 1997).

Most disagreements are fall-throughs: WoSIS serves no base saturation, Munsell colour, Fe and Al fractions or gypsum, often no horizon designation and no carbonate, so most tests stay undecided as Hugo's rule asks and the soil names its candidates (the Gypsisols are a candidate in 2,300 of the 2,629 revised fall-throughs). But where soilKey names a grouping it is more often wrong than right: 196 of the 296 covered revised labels and 471 of the 765 covered labels of 1974 disagree. In 1988 the wrong ones are mostly Cambisols for Vertisols (24), Chernozems (20), Phaeozems (16), Calcisols and Fluvisols (12 each), and Regosols for Leptosols (11); in 1974 Regosols for Rendzinas (55), Phaeozems (46) and Lithosols (43), Phaeozems for Rendzinas (41), and Luvisols for Xerosols (35) and Acrisols (29). Almost all covered classifications are provisional (a grouping tested before them was left undecided); among the firm ones the revised groupings agree for about 60 %, while 1974 has too few (13) to say. Every classification is unchanged.

# soilKey 0.9.395 (2026-11-10)

## The declared notation decides how the older designations are read

Since 0.9.240, 0.9.250 and 0.9.256 the WRB, SiBCS, FAO-Unesco and Chinese diagnostics read the older notation of the horizon designations (in use until about 1990: the A2 is the eluvial horizon, the E of today; the B3 the transition to the C, the BC of today; the "(B)" the B incipiente, the Bi of today) when a profile shows a mark of it (a B followed by a number, an A or a C followed by two figures, a "(B)", "ir" in a B, "pl" in a B or C). The reading took no account of a notation declared in `site$designation_system`, so a profile declared in the FAO notation of 2006, which writes "B1, B2, B3" for three parts of a B, had its A2 read as E and its B3 as BC, and "(Bw)R" read as "BiwR". Found while building the FAO-Unesco module.

As Hugo decided (Q1 to Q9, 2026-11-10), a declared notation now decides before the marks (`.desg_declared_notation()`, `R/utils-wrb-ch3-v09220.R`):

- a declared older notation has its A2, B3 and "(B)" read, marks or not: the Australian Field Handbook of the ASC (`asc` with or without an edition, as `asc4` or `asc2021`, and its aliases: A1, A2, A3, B1, B21, B3), the French CPCS of 1967 (`cpcs`, `cpcs1967`, `french`, `fr`), the FAO-Unesco Legend of 1974 and the FAO Guidelines of 1977 (`fao1974`, `fao1977`, the only older ones among the FAO values), the USDA Soil Survey Manual of 1951 and its supplement of 1962 (`usda1951`, `ssm1951`, `usda1962`, `ssm1962`, `soilsurveymanual1951`, `soilsurveymanual1962`) and the Brazilian notation in use until 1988 (`br_pre1988`). The USDA of 1975 and the Soviet notation of 1977 are not old here;
- a declared present notation has none of them read, whatever the marks: `fao`, `fao2006`, `wrb` and its editions, `usda`, `kst`, `sibcs` (the notation of Santos et al., 2015), the CSSC, NZSC, SCSJ and CSCJ values, and the South African, German, Russian and French notations as their own readers recognise them (so "zambia", "satellite" or "dbg-wrb" are not taken for a notation). In a present notation a "(B)" loses its parentheses and keeps its letters ("(Bw)R" is read "BwR", "(B)km" "Bkm"), where the older or undeclared notation reads it "Bi" as before;
- with no declaration, a value that declares nothing (`NA`, `none`, `SA` ...), `nz_legacy` or a value soilKey does not know, the marks decide as before; `PedonRecord$validate()` notes a value it does not know. Declared values are compared lower-cased, accents folded and all but letters and digits dropped, a value that is not UTF-8 read as CP1252.

The site reaches every reader as an explicit argument: `.desg_master()`, `.desg_old_notation()` and `.h_master()` take `site`, and so do the helpers that received only the horizons (the FAO readers of the master, the solum, the buried horizons, the rock structure and the texture, the WRB plough, subsurface, cemented and secondary carbonate and gypsum readers, the SiBCS readers of the B in the first 100 cm, the abrupt textural change, the histic and organic layers and the textural ratio, the Chinese horizon kind). The readers that read one designation at a time now read it with the notation of its whole profile: the cemented layers of the WRB qualifiers (each part of "Bt/Cr" read with the parts of the profile) and the plough designation of the Brunic qualifier.

The NSD reader (`read_nsd_pedons()`) now declares `designation_system = "nz_legacy"` (it declared `"nzsc"`): the National Soils Database surveys are written in the New Zealand notation, many of them in its older form (A11, A12, B21, B22, B3 beside Bw). Every reader that compares the notation reads `nz_legacy` (or "NZ legacy", compared as a key) as it read `nzsc`: the NZSC, SCSJ, CSSC, ASC and FAO 1988 notation tests, the frozen-layer pattern and the CPCS, which reads the f of a New Zealand B as iron, not frozen. The older-notation reading is judged from each profile's own marks. The ASC key also reads `asc` with an edition (`asc3`, `asc2021`) as its notation. The ISIS reader keeps `fao2006` (Q6). `inst/schemas/site-attributes.yaml` lists the values the code compares (`fao`, `fao2006`, `wrb` and its editions, `usda`, `sibcs`, `fao1977`, `usda1951`, `ssm1951`, `usda1962`, `br_pre1988`, `nz_legacy`) and `inst/ATTRIBUTES.md` is regenerated.

Measured on 2026-11-10, before and after:

- Every example and fixture keeps its names: 1,302 profiles (the canonical WRB and SiBCS fixtures, every `make_*_example()` choice and near miss, the FAO 1988 and 1974 examples, the ZA forms and the 225 KA5 subtypes) under 17 keys, 22,134 names identical.
- BDsolos, FEBR, Redape, AfSP, WoSIS, KSSL and LUCAS: their loaders declare no notation, so nothing changes.
- ISIS (974 profiles, `fao2006`): readings change in 8 profiles (present-notation designations such as "Ah, B1, B2, B3, B4", "A1, A2, BA, Bt1, Bt2, B3" no longer read as the older notation, "(Bw)R" read "BwR"); names change for 3: SiBCS on 2 (CL006 Argissolos Bruno-Acinzentados to Neossolos Fluvicos, GH008 Cambissolos Haplicos to Neossolos Regoliticos) and the candidates of 1 undecided FAO 1988 profile. WRB, USDA and FAO 1974 unchanged, so the ISIS benchmarks of 0.9.392 are unchanged.
- NatSoil (9,703 profiles, `asc`): readings change in 437 (an Australian A2 read as E, a B3 as BC, where the profile has no other mark); names change on 306 profiles: the FAO 1988 candidates of 300 undecided profiles (299 stay undecided with another list, 1 becomes not classified), SiBCS on 10 (mostly Neossolos Regoliticos to Argissolos, 2 to Espodossolos) and WRB on 4. The ASC key does not read these designations, so the NatSoil benchmark is unchanged.
- SALI, Queensland (5,716 profiles, `asc`): readings change in 34; names change on 20 profiles (22 names): the FAO 1988 candidates of 18 undecided profiles, WRB on 2 (a Cambic qualifier gained) and SiBCS on 2. The SALI benchmark (ASC key) is unchanged.
- NSD (1,330 profiles, now `nz_legacy`): no reading changes, and all 1,330 profiles give the same names under the 17 keys as before (22,610 names; the 183 profiles with no horizons stop with the same error before and after).

# soilKey 0.9.394 (2026-11-10)

## South African classification 1991: four readings after the AfSP benchmark

Four readings examined on the AfSP benchmark of 0.9.393 and decided by Hugo (2026-11-10). They are recorded in `inst/rules/za1991/readings.yaml`; the 73 form examples, the 404 family examples and the special cases (555 runs) keep their names and provisional status.

* **B1: a B at the surface needs more than 100 mm.** The reading D7 (a red or yellow-brown apedal or neocarbonate surface layer with under 0.3 % organic carbon directly over hard rock, a lithocutanic B or a dorbank is that B, the profile truncated) now holds only when the layer is more than 100 mm thick (`surface_b_thick` in `thresholds.yaml`, flagged). A thinner layer is not read as a B at the surface, so a thin carbon-poor topsoil on hard rock is tested as a topsoil and keys to Mispah, as the surveyors wrote it. The limit is over 100 mm while the lithosol rule of the general rules takes an orthic A under 100 mm, so a topsoil of exactly 100 mm is neither a lithosol nor read as a B. A layer whose thickness is not recorded (top or bottom missing) leaves the reading undecided, the orthic A or the B; no AfSP profile is affected. On AfSP, D7 fired on 6 profiles: the 5 labelled Mispah (A of 50-100 mm, read as Clovelly) and one labelled Hutton (300 mm, red), which keeps its Hutton. Of the 5, one is now Mispah and 4 stop with Mispah among the forms left undecided (their A stays undecided as organic O or melanic A; examined apart, for Hugo).
* **C1: cutans not recorded leave the pedocutanic B undecided.** When clay films are not recorded, soilKey's argic horizon stood in for the cutans both ways; but the pedocutanic B asks for prominent cutans, not a clay increase, so argic not holding refused the B. Now, in the pedocutanic test only, argic not holding leaves the cutans undecided; argic holding still stands in, flagged. The other refusals of the cutans reader (no peds, carbonate neoformations, a layer outside the zone of the clay maximum of an argic horizon) stand. The shared reader of cutans used by the other national systems is unchanged. On AfSP (no layer records clay films), the 5 Bonheim keyed as Inhoek now stop with Bonheim among the candidates; one Rensburg keyed as Willowbrook stops too, and 11 Swartland and Valsrivier profiles that stopped now have their label among the candidates.
* **A1: the humic A of 1977 in the benchmark (the key unchanged).** The thin humic A came with the second edition; the humic A of 1977 had to be thicker than 450 mm. The AfSP report gives a second figure, flagged, beside the main one: for labels from a survey before 1991 or written as a 1977 binomial (a label with neither is not read so), soilKey's humic form with a humic A of 450 mm or less is scored as its orthic counterpart (Inanda as Hutton, Magwa as Clovelly, Kranskop as Griffin, Sweetwater as Oakleaf, Nomanci as Glenrosa; internal `.za_humic_1977()` in `R/za-benchmark.R`). Lusiki is left out: it has no single orthic counterpart (Valsrivier or Swartland by what lies below the pedocutanic B). The organic carbon is not corrected: AfSP gives no method for the South African SOTER profiles, their values read as reported by the laboratory, and the same institute's WISE profiles are coded Walkley-Black.

* **M1: no dry colour and a moist value over 3 is not melanic.** The melanic A needs a dry value and chroma of 3 or less. A layer with no dry colour was left undecided for the melanic colour (default 9), even with a light moist colour; but the dry value is never darker than the moist, so a moist value over 3 now refuses the colour, flagged (`dry_value_from_moist`). A moist value of 3 or less stays undecided, as before. The 200 mm window of the organic O below a description that ends in rock is not changed (O1 not adopted). Two older tests that exercised default 9 on a layer of moist value 4 or 5 now give that layer a moist value of 3. On AfSP, 30 profiles that stopped are now decided: 16 agree (Clovelly 8, Mispah 5, Griffin, Hutton and Katspruit 1 each) and 14 do not, where the B or the material below is not recognised from the data (Avalon as Clovelly 5 and as Pinedene 1, Kroonstad as Klapmuts 2, Westleigh 2, Clovelly as Hutton, Glenrosa as Hutton, Longlands as Pinedene and Valsrivier as Katspruit 1 each).

The AfSP run of 2026-11-10 (641 scored), with all four readings:

* Main figure: 139 of 641 agree, 21.7 % (Wilson 95 % 18.7-25.0; 0.9.393: 122, 19.0 %). soilKey names a form for 216 (coverage 33.7 %, 30.1-37.4; was 196, 30.6 %), and among those the form agrees for 64.4 % (57.8-70.4; was 62.2 %). 423 stop with forms undecided and the label's form is among them for 315 (74.5 %; was 316 of 442, 71.5 %). The confusions Mispah to Clovelly (5) and Bonheim to Inhoek (5) are gone; Mispah now agrees for 8 of 18 (was 2).
* Without M1 (B1, C1 and A1 only) the main figure was 123 of 641 (19.2 %), 186 decided (29.0 %), 66.1 % of them.
* Second figure, the humic A of 1977 (surveys before 1991 or 1977 binomials): 154 of 641 agree, 24.0 % (20.9-27.5); 71.3 % of the decided (64.9-76.9). 25 profiles are read so; 19 agreements gained (Hutton read as Inanda 8, Clovelly as Magwa 6, Griffin as Kranskop 5) and 4 lost: binomials of surveys of 1999-2000 whose surveyors wrote a thin humic A as humic (Kranskop 3, Inanda 1). Left: a Griffin of 1982 whose humic A is 500 mm.

# soilKey 0.9.393 (2026-11-10)

## South African classification 1991: the AfSP benchmark

A new script, `inst/benchmarks/za1991_afsp.R`, compares `classify_za1991()` with the local classification of the South African profiles of the Africa Soil Profiles database (AfSP 1.2, ISRIC; field `LocalCls` of `AfSP012Qry_Profiles.dbf`). The script is offline: it reads the AfSP DBF files from the directory given as its first argument, the option `soilKey.afsp_dir` or the environment variable `SOILKEY_AFSP_DIR`, stops (from R) or exits cleanly (as a script) when there is none, and downloads nothing. AfSP is CC BY-NC; no profile or label of it is shipped with soilKey, and the tests use synthetic labels and profiles only. Every classification is unchanged.

The labels are read by a new internal reader, `.za_match_label()` in `R/za-benchmark.R`, as Hugo decided (2026-11-10):

* **The era from the shape of the label, not the survey year.** A binomial `"<Form> <Series>"` (with or without a comma after the form) is a label of 1977 and is scored at the form level as its 1991 namesake; `"<Form> Form, <Family> Family"` is a label of 1991; a form named alone is reported apart. The report gives the agreement overall and by era, and notes that some forms were redefined in 1991.
* **Exact names only.** The form word is matched to a form name of `inst/rules/za1991/key.yaml` and the family to a family name of that form in `families.yaml` (case ignored). The script's explicit list of misspellings (Avanlon, Houhoek, Houwbroek and Kraanskop, with or without a comma) is tried only when a label is not read as it stands, and such matches are flagged; anything else is unmatched, never guessed.
* **No form counts as a disagreement** in the main figure, as in the other benchmarks. The report also gives the agreement among the profiles soilKey decides, the coverage, the provisional share among them, and how often the label's form is among the forms left undecided (the `forms:<code>` entries of `provisional_because`).
* **Families** are scored only where a 1991 label names a family of its form and soilKey reaches a family; the series of 1977 are not scored.

The run of 2026-11-10 on AfSP 1.2: 659 South African profiles, 646 labelled, all 646 read (4 by the listed misspellings), 641 scored (5 labelled profiles have no WRB 2006 group and are not built by `load_afsp_pedons()`). 624 are binomials of 1977, 13 labels of 1991 and 4 forms named alone; 108 of the binomials were surveyed in 1991 or later, and all 13 labels in the 1991 shape come from surveys before 1991 (converted to the 1991 system later, by the compilers).

* Form: 122 of 641 agree, 19.0 % (Wilson 95 % 16.2-22.3); 1977 binomials 19.6 % (16.6-22.8, n 624), 1991 labels 0 of 13, forms named alone 0 of 4.
* soilKey names a form for 196 profiles (coverage 30.6 %, 27.1-34.3), and among those the form agrees for 62.2 % (55.3-68.7); 121 of the 196 are provisional (104 at the form level). 442 stop with forms undecided, and the label's form is among them for 316 (71.5 %); 3 get no form at all (nothing undecided).
* Candidates: the lists of forms left undecided hold a median of 8.5 forms. With the labels shuffled across the 442 profiles that stop (1000 permutations, fixed seed) the hit rate is 40.9 % (95 % range 38.0-44.1); by size of the list, 56 of 103 with 1-5 forms (54.4 %), 116 of 158 with 6-20 (73.4 %), none with 21-40, 144 of 181 with over 40 (79.6 %). Not in the rate: 8 of the 74 decided disagreements have the label's form among the forms left undecided before the one assigned.
* Hutton, the largest class, agrees for 84 of 185 labels (84 of the 93 soilKey decides).
* Family: 10 of the 13 labels of 1991 name a family of their form; soilKey reaches a family for 2 of them, and neither agrees (in both the form differs). Of the 3 family names not read, 2 (Glengazi, Cleveland) are 1977 series names and 1 (Greendal) looks like a misspelling of Greendale (Rg1000); it is not read (no family misspelling list, Hugo's decision).

The main loss is the profiles that stop: most AfSP descriptions do not record what the key needs to separate the forms (the matrix colour of the lower B, cutans, slickensides, signs of wetness). Among the decided disagreements, the most frequent are a humic A where the label has an orthic A (Hutton to Inanda 8, Clovelly to Magwa 6, Griffin to Kranskop 6), Oakleaf read as Hutton (6), Bonheim keyed as Inhoek (5; in 4 of them Bonheim, tested before, was left undecided), and a thin, carbon-poor A on hard rock read as a truncated profile, Mispah to Clovelly (5).


# soilKey 0.9.392 (2026-11-09)

## FAO-Unesco legend: the ISIS benchmark

Twelfth and last part of the FAO-Unesco soil legend. A new script, `inst/benchmarks/fao_isis.R`, compares both editions with the FAO-Unesco names ISRIC gave its reference profiles (ISIS, field `faoClassifications`): the label of 1974 with `classify_fao(edition = "1974")`, the label of the Revised Legend (stored under the year of the ISRIC reprint, 1997) with `classify_fao()`. As Hugo decided (Q27), it is offline and does not change the ISIS reader: it reads a saved cache of ISIS profiles from its first argument, the option `soilKey.isis_cache`, `SOILKEY_ISIS_CACHE` or the user cache folder, downloads nothing, and run as a script with no cache found says so and exits cleanly. ISIS data are not shipped. WoSIS is left for later, with Hugo's go.

The labels are read by a new internal reader, `.fao_match_label()` in `R/fao-benchmark.R`: the edition from the publication year (1974 to fao1974; 1988, 1990 and 1997 to fao1988), the group word singular or plural, the 1974 Nitosols and the 1988 Nitisols read in the other edition too (flagged), ISRIC's third-level subunit ("Ferri-", "Orthi-") cut off and reported apart (the legend defines no third level, so it is never scored), the words after the group matched to the phases of the edition, exact names only; a variants list, empty today, is tried only on a label that is not read. Every one of the 1,515 labels of the cache of 2026-10-29 (803 of 1974, 712 revised, on 814 profiles) is read as it stands, with its grouping and unit; 593 revised labels carry a subunit, 212 labels a phase.

The run of 2026-11-09 on that cache, before Hugo's decisions below (13 labels on 7 profiles with no horizons in the record are not scored):

- Legend of 1974, 796 labels: grouping 250 agree, 31.4 % (Wilson 95 % 28.3-34.7); unit 120, 15.1 % (12.8-17.7), 48.0 % (41.9-54.2) where the grouping agrees. soilKey names a group for 529 labels, and among those the group agrees for 47.3 % (43.0-51.5); 260 stop with candidates undecided and 7 are not classified (no residual group, Q5). Of the 529, 267 are provisional (a group tested before it left undecided) and 134 rest on a stand-in; among the 207 firm labels (classified, neither provisional nor on a stand-in) the group agrees for 43.0 % (36.4-49.8).
- Revised Legend, 706 labels: grouping 155 agree, 22.0 % (19.1-25.2); unit 102, 14.4 % (12.0-17.2), 65.8 % (58.0-72.8) where the grouping agrees. soilKey names a grouping for 353 labels, and among those it agrees for 43.9 % (38.8-49.1); the other 353 stop with candidates undecided. Of the 353, 288 are provisional and 3 rest on a stand-in; among the 62 firm labels the grouping agrees for 32.3 % (22.0-44.6).
- Phases: of the labelled phases soilKey finds present the sodic phase 25 of 29 (1974) and 8 of 11 (1988), the duripan 6 of 7 and 3 of 3, the fragipan 5 of 6 and 2 of 2, the petric 9 of 14, the skeletic 8 of 15, the gilgai 9 of 9 and the phreatic 6 of 11 in 1988; the field-only phases ISIS does not record in the profile data (cerrado, stony, rudic, anthraquic, gelundic, takyric) are absent, and the inundic phase undecided, as Hugo's rule for a field observation not recorded asks. soilKey finds the sodic phase on far more profiles than the labels name (113 and 106), which suggests the labels do not list every phase.

The fall-throughs are the main loss: most profiles that stop do so because a measurement the key needs is not in the record, most often the gypsum content (the Gypsisols are a candidate in 271 of the 353 revised fall-throughs; the gypsiric and gypsic tests of the Regosols, Fluvisols and Arenosols of 1974 likewise), the pyrophosphate and oxalate Al and Fe of the spodic B, the clay skins and the electrical conductivity. Following Hugo's rule, a measurement not recorded leaves its test undecided and the soil names its candidates instead of a guess. The weak spots the plan expected show in the confusion tables: Vertisols keyed as Cambisols (13 of the 1974 labels, 17 of the revised ones), Andosols as Cambisols (20 and 15), and Ferralsols as Acrisols or Cambisols (24 and 20). The report gives the agreement by edition and level with Wilson intervals, the provisional, stand-in and fall-through counts, the candidates left undecided, the phases and the confusion of the groupings. soilKey's readings of this part (the edition from the publication year, the reading of labels, what counts in the benchmark, and that a phase soilKey finds but a label omits is reported, not counted as an error) are in `inst/rules/fao1988/readings.yaml` and `data-raw/fao_readings.yaml`, confirmed by Hugo (2026-11-10); the vignette on cross-system classification gains a section on the two editions. Every classification is unchanged (the fixtures of the other systems byte-identical).

Fixed on the review (2026-11-10). The firm agreement counts only the labels soilKey classifies (it counted the fall-throughs as firm), and the provisional and stand-in columns of the agreement table say so; a profile that could not be run no longer drops an element of the results list. The crosswalk of 0.9.391: the Lithosols, Rendzinas and Rankers as units are renamed (p. 117 prints "formerly"), the Vertisols and Histosols groups the same (no group comment on pp. 118 and 126), the Calcic Cambisols split, the Dystric Podzoluvisols amended (the gleyic comment runs onto their row, p. 123), the Rhodic Nitisols new with no comment printed (bracketed, and an alternative of the Eutric and Dystric Nitosols), the Luvic Calcisols an alternative of the Calcic Luvisols (keyed before the Luvisols, p. 119); a condition may be given per target, so a Eutric Fluvisol of 1988 shows only the fluvic clause of the Gleysols of 1974, and a 1988 unit shows the alternatives that name its unit, else those of the group rows (a Calcaric Cambisol no longer shows the Haplic Xerosols). Hugo confirmed these readings (2026-11-10). The tests read the benchmark script from the installed package (so they run under R CMD check), run it end to end on a small synthetic cache, and check the edition line on constructed results instead of classifying every example.

Decided by Hugo (2026-11-10), on the causes the benchmark showed. The ISIS reader reads the profile's large cracking and, without a cracking record, gilgai as cracks of 1 cm or more (the lower limit of the medium surface cracks, 1-2 cm, of the FAO field guidelines' classes that ISIS's own cracksSurface uses); small cracking stays 0.5 cm, since a small crack may be a fine one. With neither gypsum nor an ECe recorded, an upper bound of the ECe below 2 dS/m (the reader derives it from the EC 1:2.5) reads as no gypsum, flagged, the rule already used for an ECe below 2; this serves the gypsic horizon of both editions. Andic properties with Alox + 1/2 Feox of 2 % or more and a bulk density of 0.9 or less and no P retention measured are undecided, not absent; the rule that undecided andic properties are absent unless volcanic material is recorded applies only when fewer criteria are met (the amorphous complex of 1974 has no such rule and was not changed). The oxic B of 1974 keeps Q13: clay films recorded as few still count as evidence of illuviation, a known cause of Ferralsols keyed as Acrisols or Luvisols in the benchmark. The other consumers of the ISIS reader are unchanged: the CST 2001 and KA5 ISIS benchmarks give the same scores before and after, and on the 38 ISIS profiles whose cracks change, the WRB and USDA results of `benchmark_isis()` are the same (both ask 0.5 cm). No result of `make_fao_example()` changes, and every other classification is unchanged (the fixtures of the other systems byte-identical).

The benchmark after these decisions, same cache:

- Legend of 1974, 796 labels: grouping 276 agree, 34.7 % (31.4-38.0); unit 143, 18.0 % (15.5-20.8), 51.8 % (45.9-57.6) where the grouping agrees. soilKey names a group for 563 labels (529 before), and among those it agrees for 49.0 % (44.9-53.1); 226 stop with candidates undecided (260 before), 7 are not classified. Among the 254 firm labels the group agrees for 46.9 % (40.8-53.0). The Vertisols now agree on 22 labels (none before).
- Revised Legend, 706 labels: grouping 186 agree, 26.3 % (23.2-29.7); unit 123, 17.4 % (14.8-20.4), 66.1 % (59.1-72.5) where the grouping agrees. soilKey names a grouping for 383 labels (353 before), and among those it agrees for 48.6 % (43.6-53.6); 323 stop with candidates undecided. Among the 195 firm labels the grouping agrees for 51.3 % (44.3-58.2). The Vertisols agree on 21 labels (none before), and the Gypsisols are a candidate in 166 fall-throughs (271 before).

# soilKey 0.9.391 (2026-11-09)

## FAO-Unesco legend: the crosswalk between the 1974 and 1988 editions

Eleventh part of the FAO-Unesco soil legend. `inst/rules/fao1988/crosswalk.yaml` holds the correspondence of Appendix 4 of the Revised Legend ("Main changes in the revised legend", pp. 113-126) between the 26 major soil groups and 106 soil units of the Legend of 1974 and the 28 major soil groupings and 153 soil units of 1988. Every 1974 group and unit has one row: the 1988 class or classes it goes to, or none where it is deleted, with its relation (same name, renamed, amended, renamed and amended, merged, split, deleted), the printed page and the comment of the appendix in soilKey's words. The 1988 classes with no 1974 class are listed as new: the 6 new groupings beside the Leptosols, which take the Lithosols, Rendzinas and Rankers, and 59 units. Appendix 4 prints only the 1988 names; the 1974 class of each row is read from the 1974 list by its name or by the "formerly" the appendix prints. Codes only: the names are read from the rule files of each edition.

As Hugo decided (Q8), the three changes the appendix does not flag are added in brackets: Calcaric Gleysols (Gc) to Calcic Gleysols (GLk) is a change of definition, the Plinthic Luvisols (Lp) are deleted with no 1988 row, and the Thionic Histosols (HSt) are new. Where a group comment of the appendix sends part of a 1974 group elsewhere, soilKey adds bracketed alternatives with their condition: the Luvisol units to the Lixisols and the Acrisol units to the Alisols by the CEC of the argic B, the Calcic Cambisols to the Calcisols or Gypsisols, the Calcic Kastanozems to the Gypsic Kastanozems, the plinthic units to the Plinthosols, and the units of the deleted Xerosols and Yermosols by their names. One-to-many rows stay one-to-many (Pellic and Chromic Vertisols to the four Vertisol units, Eutric and Dystric Histosols to the Folic, Terric and Fibric ones).

`fao_crosswalk()` (exported) returns the table, one row per 1974 class, or with `from = "1988"` the 1974 classes behind a 1988 code. `print()` of a result of either edition, the HTML and PDF reports and the Pro app add one line with the other edition: a Lixisol of 1988 is new in 1988, some Orthic, Chromic, Calcic and Vertic Luvisols of 1974 when their argic B has a CEC below 24 cmol(+)/kg clay; an Orthic Luvisol of 1974 is a Haplic Luvisol of 1988 (renamed and amended, p. 122), or a Haplic Lixisol. The line is a note: the table never classifies, each edition is keyed by its own text, and every classification is unchanged (the fixtures of the other systems byte-identical; no FAO result changes, the line is computed when printed). soilKey's readings of this part (the 1974 side of each row, two comments printed between rows, the alternatives read from the group comments, the targets of the Xerosol and Yermosol units, two slips of the layout on pp. 115 and 124) are in `inst/rules/fao1988/readings.yaml` and `data-raw/fao_readings.yaml`. Decided by Hugo (2026-11-10): all of them are confirmed as proposed.

# soilKey 0.9.390 (2026-11-09)

## FAO-Unesco legend: the phases of 1974

Tenth part of the FAO-Unesco soil legend, the fourth of the Legend of 1974. `classify_fao(edition = "1974")` now gives the 12 phases of pp. 5-7 in `$phases`, beside the soil unit: stony, lithic, petric, petrocalcic, petrogypsic, petroferric, phreatic, fragipan, duripan, saline, sodic and cerrado (the list of p. 5 prints "stony lithic" without a comma: two phases). Phases subdivide the units by what matters for the use of the land and are not diagnostic for the units (p. 5), so the unit, the group and the key trace are exactly those of 0.9.389. Each phase is three-valued, in the same form as the phases of 1988: `status` is present, absent, undecided, or not shown where the text excludes the unit, with `criterion`, the page, the criterion in soilKey's words, the fallbacks used and what would decide an undecided phase. Two more rows give the classes of the map symbol (pp. 4-5): the texture class of the upper 30 cm with the classes of 1974 (1 coarse, below 18 % clay and above 65 % sand; 2 medium; 3 fine, above 35 % clay) and the slope class (a up to 8 %, b to 30 %, c above 30 %). The phases, their pages, criteria, numbers and Portuguese names are in `inst/rules/fao1974/phases.yaml`, read by `R/fao-phases.R`.

The numbers are the printed ones. Lithic: continuous coherent hard rock within 50 cm. Petric: 40 % or more coarse fragments by volume, oxidic concretions, hardened plinthite or ironstone included, 25 cm thick, the top within 100 cm, not continuously cemented. Petrocalcic and petrogypsic: the cemented calcic or gypsic horizon of Soil Taxonomy, or a pan recorded as calcrete or gypcrete, the top within 100 cm. Petroferric, fragipan and duripan: the upper level within 100 cm, read by the readers of the same phases of 1988. Phreatic: a water table between 3 and 5 m. Saline: an ECe above 4 mmhos/cm in some horizon within 100 cm. Sodic: more than 6 % exchangeable sodium within 100 cm. Stony: the record of fragments or outcrops that make mechanised equipment impracticable (`rudic_recorded`, the same condition as the rudic phase of 1988). Cerrado: the vegetation recorded at the site. The "not shown for" rules are applied as printed: no lithic phase for the Lithosols, no saline phase for the Solonchaks, no sodic phase for the Solonetz; when such a group is only a candidate, a phase whose criterion is met is undecided.

Hugo's decisions of 2026-11-08 are applied: the phases are a separate list per edition, three-valued, with the "not shown for" rules, and the cerrado phase is read from the recorded vegetation (Q25); the texture and slope classes of the map symbol are given when the data allow (Q3); the field-only phases (stony, cerrado) are absent when not recorded, with no substitute threshold (default 16); the pans of Soil Taxonomy read soilKey's USDA predicates, the WRB horizons only as stand-ins, flagged (default 17). soilKey's readings of this part (the stony phase on the rudic record, the petric phase as the skeletic reader at 100 cm, the petrocalcic and petrogypsic horizons of the shared reader, the 3 to 5 m of the phreatic phase with both ends included and no morphology test, no unit-level exclusion) are in `inst/rules/fao1974/readings.yaml` and `data-raw/fao_readings.yaml` for his word. `print()` and the HTML and PDF reports show the phases of 1974 as they show those of 1988. The reader of the "not shown for" rules takes the length of the group code (two letters in 1988, one in 1974); every result of 1988, its phases included, is unchanged, and every other classification is unchanged (the fixtures of the other systems byte-identical).

Decided by Hugo on the review (2026-11-10). The stony phase is read on the rudic record (`rudic_recorded`), as proposed.

# soilKey 0.9.389 (2026-11-09)

## FAO-Unesco legend: the 26 major soil groups and 106 units of 1974

Ninth part of the FAO-Unesco soil legend, the third of the Legend of 1974 (Soil Map of the World, Vol. I, Unesco Paris). The legend of 1974 is now registered as `fao1974` (aliases `fao74`, `fao_unesco_1974`; experimental and opt-in; order 17 in the registry, namespace `fao74`). `classify_fao(pedon, edition = "1974")` keys it, and `classify_fao1974()` is its thin wrapper in the registry, as Hugo decided (Q1); `fao` stays the Revised Legend (Q2). The key of pp. 43-53 is read in its own order, not that of the list of pp. 12-13: Histosols, Lithosols, Vertisols, Fluvisols, Solonchaks, Gleysols, Andosols, Arenosols, Regosols, Rankers, Rendzinas, Podzols, Ferralsols, Planosols, Solonetz, Greyzems, Chernozems, Kastanozems, Phaeozems, Podzoluvisols, Xerosols, Yermosols, Nitosols, Acrisols, Luvisols and Cambisols. Under its group a soil keys to the first of the 106 soil units whose gate passes, the last unit of each group being the key's "Other ..." residual (Orthic, Eutric, Haplic, Chromic, Vitric or Cambic as printed); the Lithosols, Rankers and Rendzinas are single units, so their result has one level. The groups are in `inst/rules/fao1974/key.yaml`, the units in `inst/rules/fao1974/units/`, each with its code and name as printed, soilKey's Portuguese gloss, its page and its gate in soilKey's words.

The gates read the diagnostic horizons (0.9.387) and properties (0.9.388) of 1974. A new file, `R/key-fao74.R`, holds the predicates that combine them where a gate reads more than one in one place, or reads a window, a B or a record the diagnostics do not read: the H horizon of the Histosols, the rock within 10 cm of the Lithosols, the clay mixed over 20 cm, cracks and gilgai of the Vertisols, "no diagnostic horizons other than", the material of the Andosols to 35 cm, the coarse texture, albic material and B features of the Arenosols, the thickness of the umbric A of the Rankers and Humic Cambisols, the calcareous material under the mollic A of the Rendzinas, the uppermost diagnostic B, the albic E over a slowly permeable horizon of the Planosols, the chroma of the mollic A to 15 cm, the 125, 90 or 75 cm of the calcic and gypsic horizons by texture, the shallower of a calcic and a gypsic horizon of the Xerosols and Yermosols, the tongues or nodules of the Podzoluvisols, the clay distribution of the Nitosols, and the unit gates of the windows, the B horizons, the Podzols, the Ferralsols and the Planosols. No predicate of 1988 is redefined; the shared readers run on the 1974 context.

Hugo's decisions of 2026-11-08 are applied: the key governs where the definitions differ (Q6: a mollic A with bleached coatings and hydromorphic properties keys as a Mollic Gleysol, the Acrisols read the base saturation in some part of the B, the Mollic and Humic Andosols ask no smeary consistence, the Nitosols do not exclude an albic E); there is no residual group after the Cambisols (Q5): a soil that no gate takes is `[Not classified in the FAO-Unesco Legend (1974)]`, and one with gates undecided names them as candidates; a diagnostic with no depth in the key is read within 125 cm (Q10); the regime of the Xerosols and Yermosols is the one recorded, and the shallower of a calcic and a gypsic horizon decides (Q26); calcareous between 20 and 50 cm is an alternative of the Calcic Cambisols (default 15). Where the 1974 text repeats a rule Hugo decided for 1988 (the uppermost diagnostic B decides at the key, the Kastanozems as the exact complement of the Chernozems, the thin H horizon over rock, the Rendzic calcareous rock, volcanic glass not recorded as none, the B of the unit gates, the colours of a red, yellow or brown B, the slowly permeable horizon), the same reading is used and recorded for his word. soilKey's readings of this part (among them: "coarse texture" as the coarse class of p. 5 to 100 cm, albic material from the surface to 50 cm, the B characteristics of the Arenosols as the features their units name, the single-unit groups with one level) are in `inst/rules/fao1974/readings.yaml` and `data-raw/fao_readings.yaml`.

`make_fao_example(code, edition = "1974")` gives one synthetic profile per group (26, each keying to its own group without an undecided gate) and per unit (106, each keying to its own unit), with near misses that cross one printed number: 26 at group level (a rock at 11 cm, a calcic horizon at 91 cm in a medium texture, a base saturation of 45 % in the lower B) and 52 for the unit gates with a printed number (permafrost at 201 cm, a pH of 5.5 between 20 and 50 cm, 1.64 me per 100 g clay for the Acric Ferralsols, a base saturation of 50 % in the B to 100 cm for the Humic Ferralsols, a sandy loam to 100 cm for the Ochric Andosols). The edition pairs of plan section 11 are tested: a Lixisol of 1988 is an Orthic Luvisol of 1974, a Calcisol a Calcic Cambisol or, in an aridic regime, a Calcic Xerosol, gleyic properties at 70 cm make a Gleyic Luvisol of 1988 and an Orthic Luvisol of 1974, and a Nitisol of 1988 with a mollic A is a Luvic Phaeozem of 1974. The app lists the Legend of 1974 among the national systems with the 26 examples; the i18n catalogues name the system and its edition in English and Portuguese. The thresholds table is shipped as `inst/rules/fao1974/thresholds.yaml` with a new `unit_key` block, and the list of the 26 groups and 106 units as `data-raw/fao1974_units.csv`. Every result of 1988 is unchanged (the 296 examples of `make_fao_example()`, with their phases and traces), and every other classification is unchanged (the fixtures of the other systems byte-identical).

The review (2026-11-09) fixed these defects: the lime waiver of the mollic A of 1974 (more than 40 % finely divided lime waives the limits of the dry value, 74 p. 24) now waives its dry contrast with the C too, so a light calcareous A over a chalky C is a Rendzina; the Humic Ferralsols and Ochric Andosols have near misses; and the tests build a cambic B over an argillic B (a Cambisol, not a Luvisol) and a Vertisol whose clay mixed over 20 cm passes where 18 cm would fail.

Decided by Hugo on the review (2026-11-10). The albic E and albic material of 1974 ask a chroma below 2.5 at the middle values (a moist value of 4 or 5, a dry value of 5 or 6; 3 over a red parent material), the limit the 5YR clause of p. 27 presupposes, flagged; the reading "as printed" of 0.9.387 and 0.9.388 is superseded. In 1974 an E with none of the kinds p. 27 names below it (a spodic, argillic or natric B, a fragipan, an impervious layer) is albic material, not an albic E, so a white sand keys to the Albic Arenosols and a wet A / Eg / Cg to the Gleysols whatever the letter of the E; 1988 is unchanged. The coarse texture of the Arenosols is read to 100 cm or to the rock where it is shallower. The Nitosols lack the ferric properties as printed, the CEC by NH4Cl below 24 me per 100 g clay included, flagged. "At less than" is strict for the Thionic Fluvisols (125 cm) and for the Gypsic and Calcic Xerosols and Yermosols (125, 90 or 75 cm); their examples sit at 124 and 89 cm, the near misses at 125 and 90. An aridic regime not recorded stays undecided, so a soil keyed after the Xerosols and Yermosols is provisional until the regime is recorded. The uppermost B decides at the key only, as in 1988: the Luvic units accept any argillic B. soilKey's other readings of this part are accepted (the single-unit groups with one level, albic material from the surface to 50 cm, the Kastanozems as the complement of the Chernozems, the uppermost B at the key, the slowly permeable horizon of the Planosols, the vitric material of the Andosols). The lime waiver of the mollic A, which waives the limits of the dry value, now waives the dry contrast with the C in 1988 too (the same words, p. 26): a correction to 0.9.382, which applies to the umbric A as well; none of the 296 examples of 1988 changes.

# soilKey 0.9.388 (2026-11-09)

## FAO-Unesco legend: the diagnostic properties of 1974

Eighth part of the FAO-Unesco soil legend, the second of the Legend of 1974. `R/diagnostics-fao74.R` now also reads the diagnostic properties of pp. 27-32 and the terms the 1974 key uses without a definition section, as 28 internal predicates returning a `DiagnosticResult` that passes, fails or stays undecided, with the layers, the per-layer answers, what would decide it, the fallbacks used and any stand-in: `abrupt_textural_change_fao74()`, `albic_material_fao74()`, `aridic_regime_fao74()`, `amorphous_complex_fao74()`, `ferralic_properties_fao74()`, `ferric_properties_fao74()`, `gilgai_fao74()`, `high_om_b_fao74()` (with `kind = "acrisol"`), `high_salinity_fao74()`, `hydromorphic_properties_fao74()`, `interfingering_fao74()`, `permafrost_fao74()`, `plinthite_fao74()`, `slickensides_fao74()`, `smeary_fao74()`, `soft_powdery_lime_fao74()`, `sulfidic_materials_fao74()`, `takyric_features_fao74()`, `thin_iron_pan_fao74()`, `tonguing_fao74()`, `vertic_properties_fao74()`, `weatherable_minerals_fao74()`, `recent_alluvial_deposits_fao74()`, `calcareous_fao74()`, `bleached_coatings_fao74()`, `heavy_clay_fao74()`, `exchangeable_sodium_fao74()` and `acric_cec_fao74()`. A property with no printed depth is read within 125 cm (Q10). Nothing is registered or classified yet.

The numbers are the printed ones. The abrupt textural change sits at the base of an A or E and asks the clay to double within 8 cm, or, from 20 % clay, 20 points more with double the clay somewhere in the horizon below. The exchange complex dominated by amorphous material asks a bulk density below 0.85 and one of its other marks (Q20). The ferralic and ferric properties read a CEC by NH4Cl below 24 me per 100 g clay in the cambic B, right below the A or in the argillic B. The B of high organic matter asks 1.35 % organic matter over 100 cm (1.5 % in the upper B of the Acrisols). High salinity is an ECe above 15 within 125, 90 or 75 cm by the texture of the upper 30 cm, or above 4 within 25 cm at a pH above 8.5. The hydromorphic properties are the test of the text (Q22): groundwater reaching the surface with its capillary fringe, a histic H, neutral or bluish hues, or the colours of reduction in five branches for soils with a spodic B, a mollic A, an argillic B under the plough layer, an oxic B, and other soils, within 50 cm or the depth the caller gives. The recent alluvial deposits ask 0.35 % organic matter to 125 cm, an irregular decrease, stratification or sulfidic materials. Sulfidic materials ask 0.75 % sulfur and no jarosite; vertic properties ask cracks of 1 cm within 50 cm of the top of the B. The Acric CEC is 1.5 me per 100 g clay or less, the sodium of the Solodic Planosols more than 6 %, and the heavy clay 60 % clay (soilKey's number, flagged).

Where Appendix 4 of 1988 calls a property unchanged (interfingering, permafrost, plinthite, slickensides, smeary consistence, soft powdery lime, tonguing, weatherable minerals), the reader of 1988 runs on the 1974 context, where the argillic and cambic B are those of 1974; calcareous and soft powdery lime read the numbers of 1988, flagged (Q23). Those 1988 readers were moved into named functions with no change: every `_fao88` result is the same. Where the 1974 answer stays undecided, a USDA reader of soilKey stands in, then a WRB one, marked as a stand-in (the amorphous complex, high salinity, the hydromorphic properties, sulfidic materials, takyric features). soilKey's readings of this part (among them: the 8 cm of the abrupt change as an abrupt or clear boundary, the albic colours as printed, the branches of the hydromorphic test, sulfidic materials absent where sulfur is measured nowhere and no marine setting is recorded) are in `data-raw/fao_readings.yaml` for his word. Every classification is unchanged (the fixtures of the other systems and the 1988 examples byte-identical).

Decided by Hugo on the review (2026-11-09). soilKey's readings of this part are accepted. The ferralic properties read "respectively": `kind = "cambisol"` the cambic B, `kind = "arenosol"` the horizon right below the A. In the hydromorphic properties an undecided spodic, argillic or oxic B is read both ways, as the mollic A already was: a layer is decided where the readings agree, else it stays undecided (the designation fallback is dropped). The sulfidic criterion of the recent alluvial deposits stays absent, flagged, where sulfur is measured nowhere and no marine, tidal or brackish setting is recorded. The review also fixed these defects: in 4.2 the shift to 75 cm when a calcic horizon intervenes belongs to the chroma 1 branch only; a hue nearest to 2.5Y starts at 1.25Y; an iron pan not named thin or placic asks the 40 mm limit; and the 8 cm of the abrupt textural change cites the boundary classes of the 1967 Guidelines.

# soilKey 0.9.387 (2026-11-09)

## FAO-Unesco legend: the diagnostic horizons of 1974

Seventh part of the FAO-Unesco soil legend, the first of the Legend of 1974 (Soil Map of the World, Vol. I, Unesco Paris). A new file, `R/diagnostics-fao74.R`, reads the H horizon of the master horizons (p. 21) and the 13 diagnostic horizons of pp. 24-27 as internal predicates, each returning a `DiagnosticResult` that passes, fails or stays undecided, with the layers, what would decide it, the fallbacks used and any stand-in: `h_horizon_fao74()`, `histic_h_fao74()` (with `base = "eutric"` or `"dystric"`), `mollic_a_fao74()`, `umbric_a_fao74()`, `ochric_a_fao74()` (with `grade = "very_weak"` or `"weak"`) and `ochric_grade_fao74()`, `argillic_b_fao74()`, `natric_b_fao74()`, `cambic_b_fao74()`, `spodic_b_fao74()`, `oxic_b_fao74()`, `calcic_fao74()`, `gypsic_fao74()`, `sulfuric_fao74()` and `albic_e_fao74()`. Depths are read from the soil surface, horizons buried under 50 cm of new material are not diagnostic (p. 32), and a horizon with no printed depth is read within 125 cm (Q10). Nothing is registered or classified yet.

The 1974 text writes organic matter, read as organic carbon x 1.724, flagged (Q11). The H horizon has 20 % organic matter without clay to 30 % at 60 % clay and is saturated; the histic H is more than 20 cm of it, or a thin surface or plough layer that, mixed to 25 cm, holds 14 to 28 % organic matter. The mollic and umbric A ask 1 % organic matter (4 % with the lime waiver) and have no strongly humic waiver of the colour contrast; the umbric A drops the consistence rule where the A never dries and excludes plaggen layers. The ochric A has two grades for the Yermosols and Xerosols: very weak below a line of organic matter in the upper 40 cm that falls from 1 % at a sand/clay ratio of 1 to 0.5 % at 13 (1.2 and 0.6 % in 18 cm over a shallow pan), and weak otherwise. The argillic B asks the clay increase of 3 points, a ratio of 1.2 or 8 points within 30 cm and, unlike the 1988 argic B, evidence of illuviation (clay skins, oriented clay or clay bridges; a Bt designation counts, flagged, Q13), with one tenth of the overlying horizons, 7.5 cm (15 cm in sand) or 15 cm of lamellae, and no texture limit. The natric B adds columnar structure and more than 15 % sodium. The cambic B asks a CEC above 16 me per 100 g clay or weatherable minerals, an alteration (colour, clay, carbonates or the colours of reduction) and a base at 25 cm, with no minimum thickness. The oxic B asks 30 cm, an NH4Cl retention of 10 or an ECEC below 10 and a CEC of 16 me per 100 g clay or less, more than 15 % clay, only traces of primary minerals and rock structure below 5 %. The sulfuric horizon asks jarosite mottles beside a pH below 3.5 (Q18).

Where Appendix 4 of 1988 calls a horizon unchanged (the albic E it calls amended), the readers of 1988 are shared through the edition of the profile context and branch only where the printed text differs (histic H, mollic, umbric and ochric A, spodic B below 12.5 cm and below an Ap, gypsic more than 15 cm, albic E without a chroma limit at the middle values); every `_fao88` result is unchanged. Where a 1974 horizon stays undecided, the USDA reader of soilKey stands in, then the WRB one, marked as a stand-in. soilKey's readings of this part (among them: the saturation of the H horizon, the weak ochric A as every other ochric A, fine clay compared only when recorded, sand and loamy sand as coarser than very fine sand, "traces" of primary minerals as 3 % or less, no thickness for the sulfuric horizon, and the stand-ins) are in `data-raw/fao_readings.yaml` for his word. Every classification is unchanged (the fixtures of the other systems and the 1988 examples byte-identical).

Decided by Hugo on the review (2026-11-09). soilKey's readings of this part are accepted. The weak ochric A is every ochric A that is not very weak, and a rock or pan shallower than 18 cm reads the 18 cm limits, flagged. The spodic B is read below the deeper of 12.5 cm and the base of an Ap, and a layer reaching below that depth counts by its part below. The review also fixed these defects: the histic H of 1974 is built from the saturated H horizon (a well-drained O no longer passes), with its thin surface and plough layer paths kept; the swelling 2:1 clays of the argillic B take the evidence of pressure, a third more fine clay or 8 points more fine clay as alternatives; where only the natric exclusion of the argillic B is undecided, the USDA and then the WRB natric horizon decide that exclusion alone, keeping the 1974 layers; a stand-in that passes keeps only its layers within the upper limit and not buried; and a cambic B with a CEC by NH4Cl of 16 or less leaves that criterion undecided.

# soilKey 0.9.386 (2026-11-09)

## FAO-Unesco legend: the phases of 1988

Sixth part of the FAO-Unesco soil legend (the Revised Legend of 1988, read in the ISRIC reprint of 1997). `classify_fao()` now gives the 16 phases of chapter VIII (pp. 67-71) in `$phases`, beside the soil unit: anthraquic, duripan, fragipan, gelundic, gilgai, inundic, lithic, petroferric, phreatic, placic, rudic, salic, skeletic, sodic, takyric and yermic. Phases are features of the land that may limit its use; they cut across the units and never define one (p. 67), so the unit, the grouping and the key trace are exactly those of 0.9.385. Each phase is three-valued: `status` is present, absent, undecided, or not shown where the text excludes the unit, with `criterion` (the test alone), the page, the criterion in soilKey's words, the fallbacks used and, for an undecided phase, the attributes that would decide it. Two more rows give the classes of the map symbol (Appendix 2, p. 107): the texture class of the upper 30 cm (1 coarse, 2 medium, 3 fine, read by the reader of 0.9.381) and the slope class (a up to 8 %, b to 30 %, c above 30 %, from `slope_pct`). The phases, their pages, criteria, numbers and Portuguese names are in `inst/rules/fao1988/phases.yaml`, read by a new file, `R/fao-phases.R`; the numbers are those of the `phases` block of the thresholds table.

The numbers are the printed ones. Lithic: continuous hard rock within 50 cm (`continuous_hard_rock_fao88()`). Salic: an ECe above 4 dS/m in a layer within 100 cm. Sodic: more than 6 % exchangeable sodium within 100 cm. Skeletic: 40 % or more coarse fragments by volume, 25 cm thick, the top within 50 cm, not continuously cemented. Inundic: more than 10 days of surface water in the growing period. Phreatic: a water table within 500 cm with no gleyic properties. Anthraquic: stagnic properties within 50 cm under long continued irrigation, read from the site's flood irrigation, years under rice or a land use naming rice or paddy. Yermic: less than 0.6 % organic carbon in the surface 18 cm mixed (0.20 % when coarser than sandy loam, waived when the salic criterion is met) and one of the six desert features. The duripan, fragipan, petroferric and placic phases are a layer within 100 cm read by soilKey's USDA predicates (Soil Taxonomy 1975, p. 67), by the kind of pan recorded in the field, or by the designations qm and x in the FAO notation, flagged; the WRB petroduric, fragic and petroplinthic horizons stand in only where the attributes of the USDA predicate are not recorded, flagged. The "not shown for" rules are applied: no salic phase for the Solonchaks and the Salic Fluvisols, no sodic phase for the Solonetz and the Sodic Solonchaks, no phreatic phase for the Fluvisols and Gleysols; when such a unit is only a candidate, a phase whose criterion is met is undecided.

Hugo's decisions of 2026-11-08 are applied: the phases are a separate list per edition, three-valued, with the "not shown for" rules, and the phreatic phase is a water table within 500 cm without gleyic properties outside the Fluvisols and Gleysols (Q25); the texture and slope classes of the map symbol are given when the data allow (Q3); field-only phases (gelundic, gilgai, rudic, takyric, the desert features) are absent when not recorded, with no substitute threshold (default 16); the four pans of Soil Taxonomy read the USDA predicates, the WRB horizons only as stand-ins, flagged (default 17). The readings of this part are in `inst/rules/fao1988/readings.yaml` and `data-raw/fao_readings.yaml`. `print()` shows the phases present, those undecided, those not shown and the map symbol; the HTML and PDF reports add a section "Phases and map symbol" (English and Portuguese). Every other classification is unchanged (the fixtures of the other systems byte-identical), and the unit and grouping of every example of `make_fao_example()` (296 profiles, near misses included) are those of 0.9.385.

Decided by Hugo on the review (2026-11-09). soilKey's readings of this part are accepted, with these changes. A moist rupture resistance of firm or more (the layers of `fragipan_usda()`) is a sign of a fragipan only, flagged and undecided, unless a second sign holds in the layer: very or extremely firm with a massive structure, or a bulk density of 1.65 g/cm3 or more (the number of soilKey's WRB fragic horizon; the text has none); a designation with x (E/Bx too) or a recorded fragipan is one by itself. The petroferric phase needs iron: 50 % plinthite or more, a recorded ironstone or ferricrete, or a designation with m and v, c or s; a layer cemented by carbonates, gypsum or silica is not petroferric. The salic phase is not shown for the Salic Fluvisols either (the printed reason of p. 69). Heavy textured, for the takyric phase, is the fine class of the upper 30 cm (p. 107). A layer whose coarse fragments are not recorded is not skeletic, flagged. A rice land use alone makes the irrigation of the anthraquic phase, flagged, unless the site records no irrigation. The lithic phase stays shown for the Leptosols. The review also fixed these defects: the USDA pan layers count only when the predicate passes (a firm layer too thin for a fragipan is none); a NASIS fragipan without its depth is undecided; a recorded "not irrigated" decides before the land use; the yermic carbon that fails is undecided while the salic criterion is, and its blown sand is a sand sheet or cover sand, not a dune, shifting sand or a negation; `classify_fao(gapfill = TRUE)` gap-fills once, so the key and the phases read the same pedon; a phase left open by a candidate unit names it (`major_groups:SC`) in `missing`, with a note; a pan undecided by a short description carries no "absent" flag; the slope class is given for a pedon without horizons; and a phase that fails to read records the error as a flag.

# soilKey 0.9.385 (2026-11-09)

## FAO-Unesco legend: the 153 soil units of 1988

Fifth part of the FAO-Unesco soil legend (the Revised Legend of 1988, read in the ISRIC reprint of 1997). `fao1988` now has its second level, the 153 soil units of the key (chapter X, pp. 75-89), registered as `units` after the major soil groupings. `classify_fao()` keys a soil to its grouping and then, under it, to the first unit whose gate passes, in the key's order, not the order of the lists (which start with the Eutric or Haplic unit): `name` is the unit as printed ("Chromic Luvisols"), `rsg_or_order` the grouping, and `levels` has one row per level with its code, name, page and soilKey's Portuguese gloss. The last unit of each grouping is the key's "Other ..." residual: Haplic in 17 groupings, Eutric in 9, and the Terric Histosols and Urbic Anthrosols, whose descriptions are shown as information and not tested (default 9). A unit the data leave undecided before the one assigned makes the unit provisional, listed as `units:<code>` in `provisional_because` with the grouping still decided. The units are in `inst/rules/fao1988/units/`, one file per grouping, each with its code, name, gloss, page, gate and a summary of the gate in soilKey's words; their numbers are in the `unit_key` block of the thresholds table.

Most unit gates read the diagnostics of 0.9.382 and 0.9.383 directly: permafrost within 200 cm, gleyic properties within 100 cm, stagnic properties within 50 cm, plinthite, calcic and gypsic horizons within 125 cm, a sulfuric horizon or sulfidic material, the mollic, umbric and ochric A, the eutric and dystric histic H, the argic B, calcareous or gypsiferous from 20 to 50 cm, sodic, salic, andic, vertic and ferric properties, tonguing, strongly humic, the petrocalcic and petrogypsic horizons within 100 cm. A new file, `R/key-fao-units.R`, holds the predicates of the gates that read more than one diagnostic in one place or a record the diagnostics do not read: the drainage and fibres of the Folic and Fibric Histosols, deep cultivation and man-made sediments of the Aric and Cumulic Anthrosols, the rock within 10 cm and the calcareous material under the mollic A of the Lithic and Rendzic Leptosols, the base saturation of a window or of a B, the smeary consistence and texture of the Vitric Andosols, the thick albic E, clay increase, colour below the A and cambic alteration of the Arenosols, the ignition, Fe/C ratio and thin albic E of the Podzols, the plinthite horizon of the Dystric Plinthosols, the geric and ferralic properties inside a B, the rubbed colours of the Rhodic, Xanthic and Chromic units, and the slowly permeable horizon of the Dystric Planosols.

Hugo's decisions of 2026-11-08 are applied: the key governs where chapter VI differs (Q6), the two passages in italics of this part are applied as printed (soft powdery lime in the Calcic Solonetz, p. 83; "in at least some part of the B" in the Humic Cambisols, p. 89; Q4), and the Gypsiric Regosols keep the key's name with Gypsic Regosols, the name of the lists, as an alias (Q7). Every name of the final list resolves to exactly one unit. `make_fao_example(code)` now takes a unit code too: 153 synthetic profiles, each keying to its own unit without an undecided grouping or unit before it, and 87 near misses, one for each unit gate with a printed number, each crossing that number (a rock at 11 cm, a calcic horizon at 126 cm, a base saturation of 50 % from 20 to 50 cm, a rubbed chroma of 4 at 7.5YR) and keying to another unit. The i18n catalogues name the level in English and Portuguese ("Soil unit", "Unidade de solo"); the app keeps its 28 examples, one per grouping. The grouping of the 56 examples of 0.9.384 is unchanged, and every other classification is unchanged (the fixtures of the other systems byte-identical).

Decided by Hugo on the review (2026-11-09). soilKey's readings of this part are accepted, with these changes. The fimic A itself (`fimic_a_fao88()`) now needs a sign of manuring: citric P2O5 above 250 mg/kg, a manuring term (plaggen, hortic, manure, compost, midden, kitchen garden) in the layer origin, anthropic material, designation or the site's land use, or the WRB plaggic or hortic horizon, always tried; artefacts neither make nor exclude it, so fills, wastes and man-made sediments stay Anthrosols and key to the Cumulic or Urbic units, and the unit-level guard of the first draft is gone. A Rhodic B whose dry value is not recorded is undecided. A smeary consistence not recorded leaves the Vitric Andosols undecided unless the texture is coarser than silt loam. A calcareous rock under the mollic A (CaCO3 measured, or limestone, chalk, marble or dolomite recorded) makes a Rendzic Leptosol. The colour of a red, yellow or brown B is read in every layer of it. The Fibric Histosols are very poorly drained or recorded undrained, not artificially drained; a drainage range that spans the limit, or no drainage record, leaves the Folic and Fibric units undecided. The other readings are accepted as proposed: a depth missing from a unit gate is read within 125 cm, "fibre content dominant" is the fibric material of Soil Taxonomy, the Cumulic sediments are sandy loam or finer, silt loam or finer excludes loam, the Luvic Arenosols compare a layer with any layer above it, the carbon enrichment of a Podzol B is the h of its designation, the plinthite horizon is the upper run of the gate, "the B" is the upper diagnostic B of the grouping's kind, and the slowly permeable horizon is the layer below the E with the adjoining layers of its master horizon. The review also fixed these defects: one vocabulary of the anthropic record now serves the Anthrosols gate, the fimic A and the Cumulic unit (plain "irrigation sediment", "irragric", "terric", a raised surface or a positive silting test alone were not read; "mine" is read as a word, not inside "mineral"); a drainage range is read by both ends, with the codes VP, P, W and Portuguese terms; an undecided B above a sure one decides where their answers differ; free iron and carbon both 0 fail the Ferric Podzols; and the Gelic Leptosols have a near miss at 201 cm on a deep Leptosol with less than 20 % fine earth (87 near misses).

# soilKey 0.9.384 (2026-11-09)

## FAO-Unesco legend: the 28 major soil groupings of 1988

Fourth part of the FAO-Unesco soil legend (the Revised Legend of 1988, read in the ISRIC reprint of 1997). The legend is now registered as `fao1988` (aliases `fao`, `fao88`, `revised_legend`; experimental and opt-in; order 16 in the registry, namespace `fao88`), with one level for now, the major soil groupings. `classify_fao(pedon, edition = "1988")` is the user function of the module and `classify_fao1988()` its thin wrapper in the registry, as Hugo decided (Q1); `edition = "1974"` stops with a message until the Legend of 1974 is keyed in a later part. The key of chapter X (pp. 75-89) is read in its own order, not that of the lists: Histosols, Anthrosols, Leptosols, Vertisols, Fluvisols, Solonchaks, Gleysols, Andosols, Arenosols, Regosols, Podzols, Plinthosols, Ferralsols, Planosols, Solonetz, Greyzems, Chernozems, Kastanozems, Phaeozems, Podzoluvisols, Gypsisols, Calcisols, Nitisols, Alisols, Acrisols, Luvisols, Lixisols and Cambisols. The gates read the 16 diagnostic horizons (0.9.382) and the 27 diagnostic properties (0.9.383); `R/key-fao.R` adds the predicates that combine them where a gate reads more than one in one place: the organic layers of the Histosols, the record of a profound human modification, the rock, highly calcareous or cemented layer or the fine earth of the Leptosols, the clay, cracks and slickensides of the Vertisols, "no diagnostic horizons other than", the coarse texture and coarse fragments of the Arenosols, the plinthite under an albic E or a stagnic horizon, the E over a slowly permeable horizon, the chroma of the mollic A to 15 cm, the uncoated grains of the Greyzems, the base saturation to 125 cm, the tongues or nodules of the Podzoluvisols, the clay distribution and boundaries of the Nitisols, and the CEC and base saturation of the argic B.

The tests are three-valued: an undecided gate before the one that passes makes the result provisional. The key governs where the descriptions of chapter VI differ (Q6), and the passage in italics of the Regosols gate in the 1997 text (lacking soft powdery lime) is applied as printed (Q4). There is no residual grouping after the Cambisols (Q5): a soil that no gate takes is `[Not classified in the FAO-Unesco Revised Legend (1988)]`, with the 28 failed gates in the warnings, and one that no gate takes while some stay undecided names them as the candidates, in brackets. The names are as printed, with soilKey's Portuguese gloss in `levels$name_pt` and the page of the key in `levels$page`. `make_fao_example(code)` gives one synthetic profile per grouping (28, each keying to its own grouping without an undecided gate) and, with `near_miss = TRUE`, the same profile with one printed number of its gate crossed (28, each keying elsewhere). The app lists the Revised Legend among the national systems with the 28 examples; the i18n catalogues name the system, its edition and the level in English and Portuguese. The thresholds table is shipped as `inst/rules/fao1988/thresholds.yaml`, the readings of the 1988 edition as `inst/rules/fao1988/readings.yaml`, and the list of the 28 groupings and 153 units as `data-raw/fao1988_units.csv`. Every other classification is unchanged (the fixtures of the other systems byte-identical).

Decided by Hugo on the review (2026-11-09). soilKey's readings of this part are accepted, with these changes. The uppermost diagnostic B within 125 cm decides at the key (p. 42): the gate of a grouping defined by an argic, natric, cambic, spodic or ferralic B passes only when its B is the uppermost one, undecided when an undecided B lies above it (`upper_b_fao88()`), so an argic B over a ferralic B is no Ferralsol and a cambic B over an argic B no Luvisol. The CEC of the argic B is read per layer (ch. VI p. 52): 24 cmol(+)/kg clay or more in every layer for the Alisols and Luvisols, below 24 in at least one for the Acrisols and Lixisols. An organic layer on rock or fragmental material makes a Histosol when more than 20 cm thick (the histic H minimum), and a bulk density not measured is read as 0.1 or more, flagged. The deep path of the Plinthosols needs gleyic or stagnic properties in a horizon above the plinthite within 100 cm (ch. VI p. 60), not in the plinthite itself. The 35 cm of andic properties are measured from the mineral soil surface, flagged when an O or H is skipped. The Kastanozems are the exact complement of the Chernozems: a chroma above 2 in any layer of the upper 15 cm of the A. The boundaries of the Nitisols are tested at the A-to-B transition only. A crack with its depth but not its width leaves the Vertisols undecided. The review also fixed the plinthite runs (read layer by layer), the cracks (width and depth of the same record), the missing attributes of the Planosols trace, the NT example (pressure faces, which the nitic properties read) and the near-miss tests (every gate evaluated directly).

# soilKey 0.9.383 (2026-11-09)

## FAO-Unesco legend: the diagnostic properties of 1988

Third part of the FAO-Unesco soil legend (the Revised Legend of 1988, read in the ISRIC reprint of 1997). A new file, `R/diagnostics-fao88-properties.R`, reads the 27 diagnostic properties of chapter V (pp. 34-41) as internal predicates, each returning a `DiagnosticResult` that passes, fails or stays undecided, with the layers, the per-layer answers, what would decide it and the fallbacks used: `abrupt_textural_change_fao88()`, `andic_properties_fao88()`, `calcareous_fao88()`, `calcaric_fao88()`, `continuous_hard_rock_fao88()`, `ferralic_properties_fao88()`, `ferric_properties_fao88()`, `fluvic_properties_fao88()`, `geric_properties_fao88()`, `gleyic_properties_fao88()`, `stagnic_properties_fao88()`, `gypsiferous_fao88()`, `interfingering_fao88()`, `nitic_properties_fao88()`, `organic_soil_materials_fao88()`, `permafrost_fao88()`, `plinthite_fao88()`, `salic_properties_fao88()`, `slickensides_fao88()`, `smeary_fao88()`, `sodic_properties_fao88()`, `soft_powdery_lime_fao88()`, `strongly_humic_fao88()`, `sulfidic_materials_fao88()`, `tonguing_fao88()` (albic or Chernozem tongues), `vertic_properties_fao88()` and `weatherable_minerals_fao88()`. A property with no depth in the key is read in the layers starting within 125 cm (p. 42); `max_top_cm` reads "within X cm" and `window_cm` "throughout X-Y cm", as the key will ask. Nothing is registered or classified yet.

The numbers are the printed ones. The abrupt textural change doubles the clay below 20 % or adds 20 points from 20 %, over an abrupt or clear boundary. The andic properties read their three items, with the volcanic glass sliding from 30 % at 0.4 % Alox + 1/2 Feox to 5 % at 2.0 %. Calcareous is more than 2 % CaCO3 or strong effervescence, calcaric calcareous throughout 20-50 cm. The ferralic properties ask a CEC below 24 cmol(+)/kg clay or 4 per kg soil in the cambic B or right below the A; the geric properties 1.5 cmol(+)/kg clay of bases and KCl acidity, or a delta pH of +0.1. The salic properties ask an ECe above 15 dS/m within 30 cm, or above 4 at a pH (1:1) above 8.5; the sodic properties 15 % sodium or 50 % sodium and magnesium; the sulfidic materials 0.75 % sulfur with less than three times as much CaCO3 and a pH above 3.5; strongly humic more than 1.4 % organic carbon over 100 cm. The fluvic properties read organic carbon above 0.20 % to 125 cm or irregular with depth, or stratification in a quarter of the soil, on sediments where their origin is recorded. Gleyic properties are two of three signs (reduction, groundwater whose capillary fringe reaches the surface, gley colours in more than 95 % of the matrix); stagnic properties are reduction with the mottling pattern within 50 cm. The ferric properties, soft powdery lime, tonguing, interfingering, nitic and vertic properties, permafrost, plinthite, slickensides and smeary consistence read the field record; what an unrecorded observation means for each (absent or undecided) is listed below and in the readings.

Hugo's decisions of 2026-11-08 are applied: the colour pattern stands in for a reduction test that is not recorded, flagged, and the chroma increasing with depth supports the stagnic properties without being required (Q21); "many" coarse mottles are 15 % and more than 20 mm (Q22); and the defaults on the abrupt textural change, fluvic, nitic and vertic properties. The andic properties of p. 34 move to this file, and item 7 of the ferralic B reads them as before (absent, flagged, unless volcanic material is recorded). They also decide now whether a mantle of 35 to 50 cm buries the horizons below it (p. 42), which was left undecided in 0.9.382.

Decided by Hugo on the review (2026-11-09). A boundary over less than 5 cm is one recorded abrupt or clear (abrupta, clara), read by one shared reader for the abrupt textural change and item 6 of the argic B; not recorded, it is absent, flagged, in both, and an argic B that fails on it keeps the flag. The abrupt textural change also compares a layer starting less than 5 cm below the upper one, across a thin transitional layer. Without CaCO3, strong effervescence makes a layer calcareous and a water pH below 6.5 makes it not calcareous, flagged (both recorded: undecided, flagged); anything else leaves it undecided. An increase of organic carbon of more than 0.2 points is an irregular decrease for the fluvic properties. The three reduction tests are read as an OR, a negative one alone does not refute, and the gley colour confirms reduction but never refutes it, never in an E. Below a buried horizon only the properties bound to a horizon (ferralic, ferric, nitic, tonguing, interfingering) are removed. The frozen suffix f and the plinthite suffix v are read only in the notation that writes them (z for frozen in the Canadian one); elsewhere they leave the test undecided. Fe-Mn nodules alone, carbonate coatings named without an amount, a sulfur lower bound against CaCO3 and Chernozem tongues without a width are undecided; skeletans not recorded are absent; any recorded slickensides that are not none count. A petrocalcic pan is never continuous hard rock, strongly humic passes when the carbon described already exceeds the 100 cm mean, the Podzol ignition route shows stagnic as well as gleyic properties, and the oxalate iron of the nitic properties is compared as Fe (0.2 % Fe2O3 = 0.14 % Fe). The readings are in `data-raw/fao_readings.yaml`. Every classification is unchanged (the fixtures of the other systems byte-identical).

# soilKey 0.9.382 (2026-11-09)

## FAO-Unesco legend: the diagnostic horizons of 1988

Second part of the FAO-Unesco soil legend (the Revised Legend of 1988, read in the ISRIC reprint of 1997). A new file, `R/diagnostics-fao88-horizons.R`, reads the 16 diagnostic horizons of chapter IV (pp. 25-33) as internal predicates, each returning a `DiagnosticResult` that passes, fails or stays undecided, with the layers, what would decide it, the fallbacks used and any stand-in: `histic_h_fao88()` (with `base = "eutric"` or `"dystric"`), `mollic_a_fao88()`, `fimic_a_fao88()`, `umbric_a_fao88()`, `ochric_a_fao88()`, `argic_b_fao88()`, `natric_b_fao88()`, `cambic_b_fao88()`, `spodic_b_fao88()`, `ferralic_b_fao88()`, `calcic_fao88()`, `petrocalcic_fao88()`, `gypsic_fao88()`, `petrogypsic_fao88()`, `sulfuric_fao88()` and `albic_e_fao88()`. They build on the readers of 0.9.381: depths from the soil surface, H and O included; horizons buried under 50 cm of new material are not diagnostic; the subsurface horizons have their upper limit within 125 cm (p. 42, argument `max_top_cm`). Nothing is registered or classified yet.

The numbers are the printed ones. The histic H is more than 20 cm of organic soil material within 40 cm, or a thin surface layer or a plough layer that, mixed to 25 cm, has at least 8 + 8 x clay / 60 % organic carbon. The mollic and umbric A mix their upper 18 cm and ask a moist chroma and value below 3.5, a dry value below 5.5, a value one unit darker than the C (waived when strongly humic), 0.6 % organic carbon (2.5 % with more than 40 % lime), a base saturation of 50 % or more (the umbric A less), and a thickness of 10 cm on rock or a cemented pan, else 18 cm and a third of a solum under 75 cm, or more than 25 cm. The argic B asks sandy loam or finer with 8 % clay, the increase of 3 points, a ratio of 1.2 or 8 points (breaks at 15 and 40 %) within 30 cm with clay skins and 15 cm without, one tenth of the overlying horizons and 7.5 cm (lamellae 15 cm), and without clay skins a coarser horizon of 18 cm above it (5 cm over an abrupt transition); it is not ferralic and not natric. The natric B adds columnar or prismatic structure and more than 15 % exchangeable sodium in its upper 40 cm. The ferralic B asks a CEC of 16 or an ECEC of 12 cmol(+)/kg clay or less, 30 cm, under 10 % weatherable minerals and water-dispersible clay, a silt/clay ratio of 0.2 or less, no andic properties and under 5 % rock structure. The spodic B reads the pyrophosphate chemistry of Soil Taxonomy 1975 with its index of accumulation of 65. The calcic, gypsic and sulfuric horizons ask 15 cm; the petrocalcic and petrogypsic horizons have no minimum thickness.

Hugo's decisions of 2026-11-08 are applied: a recorded Bt designation counts as the clay-skin observation, flagged, and "few" clay films are 1 % (Q13); the citric P2O5 of the mollic and umbric A decides when measured, else the anthropic layers it targets do, absent if not recorded (Q14); the eutric and dystric histic H by pH (1:5) 5.5, and organic soil materials undecided between the saturated limit and 20 % unless the drainage is recorded (Q15); the ferralic B asks 8 % clay and reads the 20-200 um weatherable minerals, flagged (Q16); without the 1975 spodic chemistry, `spodic_horizon_usda()` and then the WRB `spodic()` stand in, flagged (Q17); the sulfuric horizon does not ask for jarosite (Q18). soilKey's own readings of this part (among them: the mixing of the mollic A, the layers that can hold a B, carbonate and gypsum read as absent from the pH, the HCl reaction or the ECe when not measured, volcanic glass read as none unless volcanic material is recorded, and the albic E read on its moist colour when no dry colour is recorded) are in `data-raw/fao_readings.yaml` for his word. Every classification is unchanged (the fixtures of the other systems byte-identical).

Decided by Hugo on the review (2026-11-09). soilKey's readings of this part are accepted, with these changes. The spodic B is ruled out without its chemistry only by free carbonate or a water pH of 7 or more; a designation without h or s no longer rules it out, the older suffix ir counts as an illuvial sign, and the stand-ins now decide only the chemistry and are recorded only when they decide. An albic E without a dry colour takes the moist value + 1 as its dry value, flagged; an undesignated layer is read as albic only below a surface layer and above a B or C. Andic properties left undecided are absent, flagged, unless volcanic material is recorded. The thickness of the mollic A and its solum are both measured from the mineral surface. Moderately well drained leaves the saturation of organic materials undecided. The review also fixed these defects: an A layer straddling 18 cm is now read on its own values; the lime waiver of the mollic A is an alternative to the printed colour, with 2.5 % organic carbon on its branch only, and undecided when CaCO3 is not known; a plough layer may be an Hp or Op; lamellae count only when recorded above 0; argic item 6 is undecided when a clay on the way up is missing; an A no more than a third of an open solum fails; "closer to 2 than to 3" is a chroma below 2.5; the petrocalcic and petrogypsic horizons read their cementing agent (km, ym) from the designation; and the Bt of an E&Bt counts as a clay-skin observation.

# soilKey 0.9.381 (2026-11-08)

## FAO-Unesco legend: the attributes and the shared readers

First part of the FAO-Unesco soil legend in its two editions, the Revised Legend of 1988 (World Soil Resources Report 60, read in the ISRIC reprint of 1997) and the Legend of 1974 (Soil Map of the World, Vol. I). Both will be one module with an edition switch, registered as two systems in a later part; nothing is registered or classified yet.

New horizon attributes, from the plan's list as Hugo decided (Q19, 2026-11-08): `p2o5_citric_mg_kg`, `extractable_acidity_ph82_cmol`, `cec_ph82_cmol`, `al_dcb_pct`, `exch_acidity_kcl_cmol`, `ph_h2o_1to1`, `ph_h2o_1to5`, `rock_structure_pct`, `smeary_consistence`, `bleached_ped_coatings`, `ignition_redder`, `spodic_pellets`, `skeletans_pct`, `skeletan_thickness_mm`, `carbonate_coatings_faces_pct`, `nodule_size_max_cm`, `dta_low_endotherm`, the rubbed moist colour (`munsell_hue_rubbed`, `munsell_value_rubbed`, `munsell_chroma_rubbed`), `pitted_quartz_pct`, `palygorskite_pct` and `aeolian_crack_infill`. New site attributes: `flooding_days_growing_period`, `rudic_recorded` and `reference_fao74`. Each names the criterion it serves, with its page, and its fallback when not recorded. `inst/ATTRIBUTES.md` and `inst/schemas/pedon-schema.json` are regenerated.

A new file, `R/fao-readers.R`, holds the internal readers the diagnostics of both editions will share, all three-valued, each saying what it read, what would decide it and the fallbacks it used: depths from the literal soil surface, H and O horizons included (Q9); windows read "throughout" or "in some part", and "within X cm" by the top of the feature; thickness-weighted means and the mixed surface, with the depth below a shallow soil counted as zero where the strongly humic test of 1988 says so; organic matter as organic carbon x 1.724, flagged (Q11); pH at 1:1 and 1:5, the CEC per kg clay by NH4OAc or NH4Cl, the effective CEC of each edition and the base saturation, each flagged where the method differs (Q12); the three texture classes of the map symbol in each edition, the class of the upper 30 cm and the 1974 depth of 125, 90 or 75 cm by that class (Q10), sandy loam or finer and the texture groups of tonguing; the solum, buried horizons under 50 cm of new material (35 cm over an andic mantle in 1988); the clay increase in three bands with its numbers as arguments, equal to the USDA branch of `test_clay_increase_argic()` on a grid of clay contents, within a vertical distance and undecided across a lithological discontinuity; the hue rank and hue comparisons; the rubbed colour, the moist colour standing in, flagged (Q24); and the fallbacks of smeary consistence, the ignition test, rock structure and palygorskite.

The numbers of both editions, with their pages, are in `data-raw/fao_thresholds_1988.yaml` and `data-raw/fao_thresholds_1974.yaml`, and the readings in `data-raw/fao_readings.yaml` (Hugo's decisions of 2026-11-08 and 2026-11-09, his word on every reading of this part); the later parts move them to the rule folders. No text of the books is shipped. Every classification is unchanged (the fixtures of the other systems byte-identical).
# soilKey 0.9.380 (2026-11-08)

## Germany KA5: FAO horizon designations read on the data route

Ninth part of the German soil systematics of the KA5 (Bodenkundliche Kartieranleitung, 5th ed., 2005; system id `ka5`). A profile that declares the FAO notation in `site$designation_system` (as `isis_profile_to_pedon()` does) was read on the data alone: its FAO letters were thrown away, so an E was often read as an Ah, a Bt or a Bw as a loose C, and the class tests of the Ah/C-Böden and the Braunerden stayed undecided. A new file, `R/ka5-fao.R`, keeps the letters where an FAO letter and a KA5 symbol mean the same (the KA5 prints no correlation of horizon symbols; the pairs are read from the two definitions, KA5 pp. 92-105 and WRB 2022 Annex 3, pp. 225-228). Each FAO designation bounds the KA5 horizons its row may be: an E is an eluvial A (Al, Ael, Ae, Ahe), an A an A that is not eluvial, a Bt a horizon with a Bt part, a Bw one with a Bv part, a Bs or Bh one with a Bs or Bh part, a plain B a B, P or T, a C a C, an R a solid rock (mC), an O an L, Of or Oh; the redox letters open the S and G horizons (g both, by the water regime the KA5 asks; l and r the G). Where a field descriptor is not recorded, a letter states what its definition names: t the clay coatings of the Bt, w the browning of the Bv, p the tillage of the Ap, k the secondary carbonate, g and l redox mottling present with its share open. Measured data come first and decide against a letter: recorded clay films, the clay difference, Munsell colours.

Within the family the order of tests decides three-valued. When the data leave one horizon undecided, it is the letter's own translation (Bt for Bt, Bv for Bw) and the open question is one the letter answers, the FAO designation reads it, as a KA5 designation would be read; such a row has a lower grade of evidence, listed in `provisional_because` as `horizons:<code>`. The letter does not answer whether water is groundwater or perched water (Q13), the organic band of Q10 (H or Aa), or the origin of carbonate measured in a B: those rows stay undecided. A finer horizon the letter does not name (an Axp for an FAO Ap) stays undecided beside it. The letter and the data are in conflict, and the row undecided with a note, when no horizon of the family holds on the measured data (an FAO Bt with few clay films, an FAO E not lighter than its Bt), or when a horizon outside the family holds on the measured data first (an FAO C browner than the C below, an FAO Bw with the coatings and clay of a Bt, an FAO Bt with a measured Sd or Go, an FAO A over a measured Ah-Bv); the redox translations the data rule out are dropped from the candidates. Numbered sub-horizons with the same defining letter are one horizon (Bt1/Btg2, Bw1/Bwg2): each Bt sub-horizon must show the clay enrichment on its own clay against the eluvial horizon above, a Bw1 is compared with the horizon under the Bw, an E looks for its Bt under its sub-horizons and an EB. The A1, A2 and B3 of the 1977 numbering get no FAO reading. Rows with no designation, profiles that do not declare the FAO notation and profiles in the KA5 notation are read as before. Hugo confirmed the readings (`readings.yaml`); the reading of the 1977 numbering is soilKey's, for him to confirm.

On the ISIS profiles from Germany (`inst/benchmarks/ka5_isis.R`, which now also counts a level as provisional when a horizon rests on a lower grade of evidence): class 2 of 16 (12.5 %, 3.5-36.0) becomes 4 of 16 (25.0 %, 10.2-49.5) and type 1 of 15 (6.7 %, 1.2-29.8) becomes 3 of 15 (20.0 %, 7.0-45.2); one class and one type are firm, the rest provisional. Profiles that stop at the division fall from 13 to 9 of the 16 labelled; DE007 (Parabraunerde) and DE011 (Braunerde) now key to their type. DE002 (a Pseudogley in its label) keys as a Normparabraunerde, its S class now closed by the measured rust; DE009 (Rosterde) as a Normbraunerde. The nine that still stop do so on what the data do not say: groundwater or perched water (DE003, DE006, DE019), bioturbation for an Axh (DE013, DE014, Q17), the origin of the carbonate measured in the Bt (DE015, DE016), a clay difference under the Bt minimum (DE012), the Bg of DE008. One expectation of `test-v09361-ka5-data-key.R` changes: an FAO Ap in a profile declared fao2006 is now read Ap, not Ah. Every other KA5 test and every classification of the other systems is unchanged (byte-identical).

Decided by Hugo on the review (2026-11-08 and 2026-11-09). soilKey's readings of this part are accepted, with these choices: rock is an R designation, or 90 % bedrock or more with a hard rupture resistance recorded (bedrock alone is not rock); free iron below 0.5 % reads as not turning redder on ignition, 0.5 % or more leaves the test undecided; the texture of the upper 30 cm under an organic surface starts at the first mineral horizon, flagged; the strongly humic mean of a soil shallower than 50 cm over rock is divided by 100 cm, flagged; and a horizon without a designation between the last A, E or B and the first C leaves the solum undecided. A window is read only where the depths fill it: a gap or a layer without depths leaves it undecided, overlapping layers are flagged, and "within X cm" fails only when the description goes below X or reaches rock at X. A vector answer with an undecided element now names what would decide it, buried horizons are read after a figure (Bt2b), a rock layer has full rock structure, and a base saturation computed over another CEC method carries both flags.

# soilKey 0.9.379 (2026-11-08)

## Germany KA5: older German names and an offline benchmark on the ISIS profiles from Germany

Eighth part of the German soil systematics of the KA5 (Bodenkundliche Kartieranleitung, 5th ed., 2005; system id `ka5`). A new file, `inst/rules/ka5/legacy_names.yaml`, maps older German soil names to KA5 codes, for benchmarks only: the second names of the DBG systematics of 1998 (Schwarzerde, Kalkschwarzerde, Braunauenboden, Auenregosol, Tiefumbruchboden and others), Haftnässepseudogley (the KA5's own word for the Haftpseudogley, p. 313), the older names the KA5 prints beside its own (Rosterde, Borowina, Latosole, the Marschen subtypes such as Typische Kalkmarsch), and Chernozem. Every row names its source and page and resolves to a class, type or subtype of the key. A name the KA5 prints under two types (Typische Seemarsch, Übergangsbrackmarsch) is read at the class M; Amphigley, printed for two classes (pp. 229, 245), is excluded. Three leading modifiers are read: Acker- (the variety v, p. 267), Typischer (the Norm subtype of a type name, pp. 191, 238, 250-253; the KA5's own exceptions, Typische Kleimarsch and Typische Flussmarsch, are rows of their own) and Primärer (dropped before a Pseudogley only). A name of exactly two type names joined by a hyphen is read with the older names put in; one the key does not print is the type of its last part (p. 191).

A new internal matcher, `.ka5_match_label()`, reads a label against the names of the key (21 classes, 54 types with a name, 223 subtypes; soilKey's bracketed codes left out) and the legacy file: exact names only, with case, umlaut spelling, the sharp s and the spacing around a hyphen alike. A name with brackets (but the modifier "(Acker)"), a slash, a comma, a full stop or a digit, two names joined by a space and names of three or more parts are not read. There is no generic tolerance of misspellings; a name not read is NA, never a guess. In a label of several parts the deepest part wins when the others are its ancestors, else their common ancestor. A new script, `inst/benchmarks/ka5_isis.R`, keys the 24 ISIS profiles from Germany with `classify_ka5()` and scores them at the class, the type and, where both sides give one, the subtype, with Wilson 95 % intervals. It downloads nothing: it reads the saved ISIS profiles from its first argument, `SOILKEY_ISIS_CACHE` or `tools::R_user_dir("soilKey", "cache")/isis/profiles.rds`, and stops if there is none. Its explicit variant list reads two names of the labels with no printed source, flagged: a soil like a Schwarzerde as the class T only (DE008), and a Braunerde of low base content as the type BB with the variety dy noted (DE011). ISIS data are not shipped.

First run (soilKey 0.9.379): all 16 local names are read (one at class level, nine at type level, six at subtype level); 8 profiles have no local name and 6 have no horizons. Class 2 of 16 (12.5 %, 3.5-36.0), type 1 of 15 (6.7 %, 1.2-29.8), subtype 0 of the 1 profile where both sides give one. The low figures come from how far soilKey keys, not from disagreements: on the data route 13 of the 16 labelled profiles stop at the division, the class test of the Ah/C-Böden undecided in 11 of them and that of the Braunerden in 8, because the FAO-style E and Bt horizons are mostly left unread or read as Ah or lC. Only DE001 (Parabraunerde, keyed Normparabraunerde), DE005 (Nassgley, keyed Normgley: the class agrees) and DE004 reach a type. DE004 is a Gley-Podsol in its label and a Luvisol in ISIS's WRB; soilKey keys it as a Normgley. The label is kept and scored as printed, a disagreement. Hugo's decisions on the readings of this part are in `readings.yaml`. Every classification is unchanged (the fixtures of the other systems byte-identical).

# soilKey 0.9.378 (2026-11-08)

## China CST 2001: an offline benchmark on the ISIS profiles from China

Eighth part of the Chinese Soil Taxonomy (2001, English edition; system id `cst2001`). A new script, `inst/benchmarks/cst2001_isis.R`, keys the 51 ISIS reference profiles from China (CN001-CN051) with `classify_cst2001()` and scores them against the CST name of their local classification. It downloads nothing: it reads a saved list of raw ISIS profiles from its first argument, the environment variable `SOILKEY_ISIS_CACHE` or `tools::R_user_dir("soilKey", "cache")/isis/profiles.rds`, and stops if there is none. The markdown report goes where the second argument says; ISIS data are not shipped.

The labels carry the genetic name of the profile (the Genetic Soil Classification of China) beside the CST one. A new internal matcher, `.cst_match_label()`, finds the CST part and its taxon at any level among the 779 names of the key (14 orders, 39 suborders, 138 groups, 588 subgroups; soilKey's bracketed residuals left out): exact names only, singular and plural, case and hyphenation alike, and the book's aliases. There is no generic tolerance of misspellings. The script adds an explicit, documented list of the two known slips of the labels, tried only when no part matches exactly and flagged in the report: Acidi-Undic for Acidi-Udic Cambosols (CN008), and the order name Isohumisols of the 1991 and 1995 proposals, read as Isohumosols (CN049, scored at the order). 50 labels match: 48 at group level, one at subgroup level (CN014, Panic Sali-Orthic Aridosols) and one at order level (CN049); CN043 gives the genetic name only. Each level is scored on the labels that reach it, with a Wilson 95 % interval; a profile keyed to a bracketed residual (CN020, [E2] [Other Ferralosols]) counts as a disagreement and is reported apart.

First run (soilKey 0.9.378): order 26 of 50 (52.0 %, 38.5-65.2), suborder 22 of 49 (44.9 %, 31.9-58.7), group 14 of 49 (28.6 %, 17.8-42.4), the one subgroup label not reached. The Ferralosols agree 7 of 7, the Argosols 6 of 7 and the Primosols 3 of 3; the four Anthrosols and the two Aridosols are missed (keyed as Ferrosols, Argosols, Cambosols or Primosols), three of the five Isohumosols go to the Primosols, three of the seven Ferrosols to the Ferralosols, and the Cambosols agree 5 of 13. Most names are provisional: at the order 46 of the 51 profiles have a class tested before theirs that could not be decided, and 48 at the group (the book's own provisional marks are not counted). Most of these are the Halosols (H), undecided in 27 of the 46, and the Andosols (D), undecided in 24. The labels were given by ISSAS, probably on the Chinese edition, so the group is the fair target and differences below it are not errors by themselves. The genetic names have no automatic crosswalk to the 2001 system and are not scored; WoSIS was not queried (no national classification field is known in its schema). Every classification is unchanged (the fixtures of the other systems byte-identical).

# soilKey 0.9.377 (2026-11-08)

## China CST 2001: gley colours in well-drained soils and the fine clay of the argic horizon

Two order-level fixes of the Chinese Soil Taxonomy (2001; system id `cst2001`), decided by Hugo on 2026-11-08.

Gley colours without a ferrous-iron test or Eh gave the gleyic features provisionally (Q11) in any layer but an E or albic one, so a humus-grey A horizon (10YR 4/2, 4/1, 5/1) of a well-drained profile keyed as Gleyosols. A recorded drainage class of well drained or better, with no saturation regime recorded, now rules the colour-only reading out in every layer without redox features of its own, as it already did for the low chroma of the redoxic features. Moderately well drained does not. Redox features are read per layer: mottles (a mottle recorded as none, absent, nil, - or 0 is none), redox concentrations, nodules, or a g or r suffix of the designation (Bg, Cr). A positive test or rH 19 or less still gives the features in any layer. With no drainage class recorded, such an A stays provisional Gleyosols with the alternative in `$candidates`. The drainage reader now also takes "excessively well drained" and "somewhat excessively well drained" (the ISIS wordings) as well drained or better, reads hyphens and underscores as spaces ("Well-drained"), and takes the worst class of a range ("well drained to moderately well drained" is not well drained); it serves the saturation regimes, the redoxic features and the Foli- groups too. A mottle recorded as none no longer counts as a redox sign beside a shallow water table, nor as a mottle in item 3 of the redoxic features.

Item 2b of the argic horizon (p. 36) asks more total clay and more fine clay than the eluvial horizon; soilKey read the fine clay only at 60 % clay or more. Now, in the lower bands, the fine clay is compared with the eluvial layers the total clay increases over: if every one with fine clay measured has as much or more, item 2b fails; if the fine clay is not measured in both layers (in one or neither), the total-clay increase passes provisionally. The whole key is then run again with that increase failing, no gate switched off: the first level whose class differs names the fine clay as what would confirm it, even when it was assigned firmly (an argic layer can block an earlier class, the Spodosols for instance), and the class of that run is the alternative (a 1.2 times increase with no clay films: Argosols, provisional, with Cambosols as the alternative). The clay-pan and secondary clayification keep the total clay alone. Clay films and the thin section remain alternatives to the increase, as printed, and the fine-clay ratio a third higher stays a "normally". The alkalic horizon reads the same items, so it follows the argic horizon; its evidence grade, read on its own items, is unaffected. A block of alternatives in the key (any_of) is now provisional only when every alternative that passed is.

Names are unchanged except the H2.2 example, a well-drained Halosol that was Gleyic on colours alone and is now Typic Aqui-Orthic Halosols. The suborder examples G2, H1, K1 and K2, whose argic or alkalic horizon rests on item 2b, now record their fine clay, so they and the group and subgroup examples built on them (the Alkalic Halosols, the Argi- and Argic Ferrosols, the Argic Aridosols) keep their names, not provisionally. On the 51 ISIS profiles from China no name changes; seven Argosols, Ferrosols and Isohumosols with an argic horizon by the clay increase alone now carry the fine-clay warning and an alternative. Every other classification is unchanged (the fixtures of the other systems byte-identical).

# soilKey 0.9.376 (2026-11-07)

## China CST 2001: the subgroups of orders H to N

Seventh part of the Chinese Soil Taxonomy (2001, English edition; system id `cst2001`). `classify_cst2001()` now keys a pedon of orders H to N (Halosols, Gleyosols, Isohumosols, Ferrosols, Argosols, Cambosols, Primosols) below its group to the 412 subgroups of key 5.2 (pp. 108-180), in the book's order, in `inst/rules/cst2001/subgroups/{H..N}.yaml`, with the page, the name as printed (the canonical spelling of Q2, the other printed forms and the singular as aliases), the adjective and soilKey's Portuguese gloss. Every group of the book now has its subgroups: the 588 names of Appendix 1 each resolve to one row, and the note that the name stops at the group is gone (soilKey's bracketed residuals keep theirs). J1.2.1, K1.1.1 and M3.5.1 carry the book's own provisional mark.

New presets in `presets.yaml`, with their criteria in `R/key-cst-subgroups-hn.R`: the albic horizon, or a glossic horizon or glossic evidence, anywhere, within a depth, below the mollic epipedon or above the argic horizon; the calcic horizon alone, with soft powdery lime, or with the hypercalcic horizon and the calcipan, with its upper boundary within or between depths; Eutric and pH 7.0 as the converse tests, Leachic as a share of the window, the ferric property throughout the argic horizon or claypan or the B, and Dystric throughout the claypan or the argic horizon; a property in some subhorizon of the B (allitic, alic, ferric, spodic evidence); features in a share of the window (Phosphic) or in every layer of it (Epigleyic); salic evidence with its duration; para-alkalic evidence; gleyic evidence of perched water (Stagnic); a hue in a subhorizon of the upper half of the B (Xanthic of M5.4.4); Permic. Written directly: the kind of the histic epipedon (Fibric, Sapric), the window below the top of the claypan (L1.1.2), the Fe-Mn nodules of an indurated subhorizon (Concretic), cinders over 0-25 cm (Cinderic), carbon over 0-20 cm (Grayic), vertic evidence within 25 cm (Verti-lithic), and the torrential and marine salt profiles of the Halosols (Dilluvic, Marinic). The presets of 0.9.375 and the subgroups of orders A to G are unchanged. A new site attribute, `salic_months_per_year`, gives the months a year with salic evidence the Parasalic subgroups of M2.2 and M3.3 ask for; not recorded, the evidence observed passes provisionally (Q15).

`make_cst2001_example()` gives one profile per subgroup of orders H to N, each keying to its own subgroup, not provisionally (588 subgroup examples in all); the suborder and group examples keep their order, suborder and group and now reach a subgroup. Every other classification is unchanged (the fixtures of the other systems byte-identical).

Decided by Hugo on the review (2026-11-08). The nine defects of the review are fixed: shajiang in a run (and the M2.2 group key), a B subhorizon or the whole B measured within 125 cm of the mineral soil surface, the upper half of the B of M5.4.4 capped at 125 cm, Dilluvic with a layer not measured or a description ending above 200 cm, Marinic with the carbonate not measured, and a Lithic contact or a horizon with its upper boundary within a depth (gypsic horizon, salipan, alkalic evidence, argic, sulfuric and plinthic horizons) are undecided when the description stops above the depth asked, not on rock or a contact, as the feature windows already were (the Lithic rows of orders A to G too); phosphic evidence of N4.7.1 reads the mineral soil surface (`phosphic_property_cst(datum =)`); M1.6.2 takes a calcic horizon starting at 100 cm; Verti-lithic reads a half-open window. Alkalic evidence is now read on its printed items (p. 39): the weakly developed structure of the alkalic horizon and an ESP of 5 or more within 40 cm in a subsurface layer, without the argic items. With a short paddy history the anthrostagnic evidence passes its regime item on the paddy record, and the saturation axis reads the anthrostagnic regime from paddy records only when the history is not recorded or longer than half of the years. The lithologic characters of sandy deposits are read from the mineral soil surface, litter left out. The argillans of a claypan (item 4) qualify the first mineral layer, so a claypan can be exposed (L4.3.1). With these readings every subgroup is reachable and has an example. soilKey's other readings of this part are accepted, with these choices: Stagnic undecided under a flooded record, Recalcaric undecided when the description ends above 100 cm, Concretic indurated only (indurated or extremely hard) below the base of the albic run, and M5.4.4 a run whose top lies in the upper half of the B. A class assigned on a provisional reading now names what would confirm it (a ferrous-iron test, a deeper description, the extent of the contact, the months with salic evidence, the duration of the cracks).

# soilKey 0.9.375 (2026-11-07)

## China CST 2001: the subgroups of orders A to G

Sixth part of the Chinese Soil Taxonomy (2001, English edition; system id `cst2001`). The registry now names the level subgroups, and `classify_cst2001()` keys a pedon of orders A to G (Histosols, Anthrosols, Spodosols, Andosols, Ferralosols, Vertosols, Aridosols) below its group to the 176 subgroups of key 5.2 (pp. 76-108), in the book's order, one file per order in `inst/rules/cst2001/subgroups/` keyed by group, with the page, the name as printed (the canonical spelling of Q2, the other printed forms and the singular as aliases), the adjective and soilKey's Portuguese gloss in `levels$name_pt`. Typic, the residual of each list, passes by default. `[A1.3.x]` Lithic Hemi-Permagelic Histosols, listed in Appendix 1 but not in the key, is keyed at its Appendix position with the test of A1.2.3 (Q3), and the result notes it. C1.1.1 and D1.1.1 carry the book's own provisional mark. The groups of orders H to N stop at the group, with a note, until their subgroups follow in the next part.

The same adjective carries different depths, thicknesses and extents in different groups, so each subgroup row calls a preset of the new `inst/rules/cst2001/presets.yaml` with the numbers printed for it (Lithic, Mottlic, Gleyic, Parasalic, Sodic, Sulfic, Dystric, Eutrophic, Trunic, Xanthic, Dark, Takyric, the cracks of the Aridic and Ustic subgroups ...). `load_rules()` expands the calls into ordinary blocks when the rules are loaded (`R/rule-presets.R`), so the rule engine does not change; a complex subgroup (Calcari-mottlic, Aridi-trunic, Gypsi-panic, Sodi-takyric) is both of its parts and is keyed before them, as printed. `.validate_rules()` reports a preset called with an unknown parameter or without a required one. The criteria the subgroup keys define are written in `R/key-cst-subgroups.R`: a lithic contact within or between depths (with the fragments and plane-extent alternatives of the later orders), a feature in a run of layers, in layers totalling a thickness or anywhere in a window, the Buric and Terric mineral layers and the tier kinds of the Histosols, a lithic contact within the control section, the carbonate pattern of anthro-recalcification, the soluble salts of the Parasalic Siltigi-Orthic Anthrosols, base saturation or pH in a window, base saturation in a share of it, hues in a run, a share or the upper part of the B, the order's horizon at the surface, the dark mixed surface of the Aridosols and an argic horizon with lime infiltrates. Every test is three-valued as before: an undecided subgroup before the assigned one makes the name provisional, and a subgroup assigned on a provisional reading (gley colours without a test, a window cut by the base of the description) comes with the alternative the key would reach.

`make_cst2001_example()` takes a subgroup code of orders A to G as well and gives one profile per subgroup (176), each keying to its own subgroup, not provisionally; the suborder and group examples keep their order, suborder and group. The readings of this part, soilKey's own for Hugo's word, are in `inst/rules/cst2001/readings.yaml`. Every other classification is unchanged (the fixtures of the other systems byte-identical).

Decided by Hugo on the review (2026-11-07): a window that runs below the base of the description is undecided there unless the description ends on rock or a contact; the thickness not described joins the upper bound only, so a feature already found above is a firm pass and otherwise the subgroup is undecided and the name provisional (Mottlic, Endogleyic, Gleyic, Sulfic, Parasalic, Sodic, Paracalcic, Paragypsic, Calcic, Shajiang, and the tiers of the Histosols). soilKey's other readings of this part accepted as proposed. Trunic: a recorded A, AB, AC or E at the mineral soil surface fails before anything else, and the anthropic truncation flag is read only when the first layer has no designation; BA and B/A count as a B; the ferralic horizon is tested on the first layer whatever its designation. Where a subgroup names the gypsic horizon alone (G2.1.7, G2.2.3, G2.2.5) a hypergypsic horizon does not count. The preset expander also expands a preset written directly as a mapping and reports a call with extra keys. New aliases Fe-leachic-Gleyi-Stagnic Anthrosols (p. 83) and Gypsi-Panic Sali-Orthic Aridosols (p. 105). Gypsic evidence left open by an undescribed layer below now names the gypsum to measure.

# soilKey 0.9.374 (2026-11-07)

## China CST 2001: the groups

Fifth part of the Chinese Soil Taxonomy (2001, English edition; system id `cst2001`). The registry now names the level groups, and `classify_cst2001()` keys a pedon below its suborder to the 138 groups of key 5.2 (pp. 75-176), in the book's order, one file per order in `inst/rules/cst2001/groups/` keyed by suborder, with the page, the name as printed (the canonical spelling of Q2, the other printed forms as aliases) and soilKey's Portuguese gloss in `levels$name_pt`. Where the book prints no residual, soilKey's `[A1.4] [Other Permagelic Histosols]` and `[A2.5] [Other Orthic Histosols]` close the list (Q4); the result notes that the name stops there, as it now also does for `[E2] [Other Ferralosols]`, which has no groups. C1.1 and C2.1, all the Spodosols, carry the book's own provisional mark. The groups read the horizons, characteristics and regimes of the earlier parts and the criteria the group keys define, written in `R/key-cst-groups.R`: the Foli- groups and the kind that predominates in the surface tier of the Histosols, the grey iron-percoleached subhorizon and the iron accumulation of the Stagnic Anthrosols, the hydromorphic cumulic epipedon, the rhodic and xanthic colours of the Ferralosols by the thickness of the window, the udic regime bordering on the perudic one, a calcic horizon starting between 50 and 125 cm, the thickness of the mollic epipedon, an albic horizon above the claypan, folic histic evidence, shajiang, the takyric surface of the Halosols, coral sands within 100 cm, carbonate rocks or coral sands in more than half of the horizons, and a property in some subhorizon of the B or throughout it (allitic, highly weathered, alic, acid). Every test is three-valued as before: an undecided group before the assigned one makes the name provisional, and a group assigned on a provisional reading comes with the alternative the key would reach. A residual group named for a regime that the soil does not have (the Udi- groups; the Aqui-Orthic Halosols, named for the aquic regime) keeps its class, with a note.

`make_cst2001_example()` takes a group code as well and gives one profile per group (140, the two bracketed residuals included), each keying to its own group, not provisionally; the 39 suborder examples still key to their suborder, now with a group (a few were given the measurements their group asks for, so that it is decided). The readings of this part, soilKey's own for Hugo's word, are in `inst/rules/cst2001/readings.yaml`. Every other classification is unchanged (the fixtures of the other systems byte-identical).

Decided by Hugo on the review (2026-11-07): soilKey's readings of this part accepted as proposed (J3.1 and M4.2 included). B1.2 reads the grey colour alone (moist chroma 2 or less, as printed), with no test of the iron. The Foli- groups take a well-drained record for the days of saturation only as a stand-in, flagged in the trace and named in a warning. Where a group names the calcic horizon alone (F1.1, F3.2, G1.1, J2.4, L2.2) a hypercalcic horizon does not count. H1.1 reads the depth of the polygonal cracks (`cracks_depth_cm`, 1-4 cm) first, the crust thickness standing in, flagged. Fixed on the review: the depth below the base of the description is undecided in the colour shares of the Ferralosols and the coral share of M5.2, and a share that passes only on the part described passes provisionally, with the alternative; the thickness of the mollic epipedon (J2.3) is undecided while a layer below it may still belong to it; `layer_run_cst()` takes `grade = "at_least_evidence"`, so gleyic evidence counts in M4.2.

# soilKey 0.9.373 (2026-11-06)

## China CST 2001: the registry, the orders and the suborders

Fourth part of the Chinese Soil Taxonomy (2001, English edition). The system is registered as `cst2001` (aliases `cst`, `china`, `chinese`; experimental and opt-in; order 15 in the registry, after the German KA5 and South Africa planned at 13 and 14), with the levels orders and suborders for now. `classify_cst2001()` keys a pedon to the 14 orders of key 5.1 (pp. 72-75) and the 39 suborders of key 5.2, in the book's order, on the 33 diagnostic horizons (0.9.371), the 25 diagnostic characteristics (0.9.372) and the regimes (0.9.370), plus the criteria the key defines where it uses them (the share of the upper 60 cm with andic property, the organic soil materials of the Histosols, an argic horizon above the spodic one, the base saturation window of the Isohumosols, the claypan with illuviation argillans, the humic spodic subhorizons, the texture of the Vitric Andosols, the crack days of the Ustic Vertosols, the low n value or clay of the Cambosols, irrigation and anthro-silting within 50 cm). Three-valued: an undecided class before the assigned one makes the name provisional; a class assigned on a reading the book leaves open (gley colours without a test, cracks for their duration) carries a warning. soilKey's residual `[E2] [Other Ferralosols]` closes the suborders of the Ferralosols, and E1 takes the udic or perudic regime (Q4). Names in English as printed, with soilKey's Portuguese gloss in `levels$name_pt`; the book's own provisional marks are read from `book_provisional` (none falls on an order or suborder). A residual named for the udic regime reached under another recorded regime keeps its class, with a note. The approximate equivalents of each order in US Soil Taxonomy (1999) and the WRB (1998), Tables 4.2 and 4.3, are a note in the result, the report and the app (`inst/rules/cst2001/correlation.yaml`; Q1), never a class. The horizon panel of `identify_horizons(pedon, "cst2001")` is in the trace (phase `horizons`).

`make_cst2001_example()` gives one profile per suborder (39, each keying to its own suborder, not provisionally) and, with `near_miss = TRUE`, fourteen near misses, one per order. The app lists China among the national systems with the 39 examples; `data-raw/cst2001_taxa.csv` holds the 779 names of Appendix 1 in the canonical spelling of Q2 (with soilKey's `[E2]`), the other printed forms as aliases. A recorded drainage class of well drained or better now rules out the saturation regimes of the CST (stagnic, anthrostagnic, aquic, peraquic). The readings of this part are in `inst/rules/cst2001/readings.yaml`. Every other classification is unchanged (the fixtures of the other systems byte-identical).

Decided by Hugo on the review (2026-11-06): soilKey's readings of this part accepted, with these changes. The spodic materials by colour alone only directly below an albic horizon or an E layer, elsewhere with a sign (cementation, cracked coatings, Alo + 1/2 Feo 0.50 % with half or less above, ODOE); measured oxalate without a sign gives spodic evidence at most, no data leaves a red B undecided (red Ferralosols, Ferrosols and Argosols no longer key as Spodosols). The base saturation of the Isohumosols below the description is read as saturated under a calcareous lowest horizon, otherwise the window stops at the base of the description and the class is provisional. Runs of gleyic and redoxic features skip rock and organic layers. The permafrost of the Cambosols (M 4), the contacts of the Vertosols (F 2) and the anthroturbic layer (N1) are read from the mineral soil surface (`datum = "mineral"` of `permafrost_layer_cst()`, `lithic_contact_cst()`, `paralithic_contact_cst()`, `anthroturbic_layer_cst()`). The anthrostagnic epipedon and the agric horizon on a plain A are ruled out when an item fails, instead of undecided. M 3 keeps the printed clay under 80 g/kg and reads an unmeasured n value under 0.7 in a layer not saturated. The Vitric Andosols take sand, loamy sand and sandy loam only. Histosol item b is literal. A recorded non-volcanic parent material rules out the andic property; pH(H2O) under 5.5 or a perudic regime rules out the salic horizon where salts are not measured; under an aridic regime a crust not recorded leaves the aridic epipedon undecided. A class assigned on a provisional reading (gley colours without a test, Q11) comes with the class the key would reach without it, in `candidates` (kind "alternative"), a warning, the trace and the printed result, never as the class.

# soilKey 0.9.372 (2026-11-06)

## China CST 2001: the diagnostic characteristics

Third part of the Chinese Soil Taxonomy (2001, English edition; system id `cst2001`, registered in a later part). The 25 diagnostic characteristics of the book (pp. 43-64) read on the data (`R/diagnostics-cst-properties.R`), three-valued, with the layers, the missing attributes and the fallbacks used: organic soil materials with their five kinds (fibric, hemic, sapric, folic, mattic) and the control section and tiers of the Histosols; the seven lithologic characters within 125 cm (`lithologic_characters_cst(kind =)` and `lc_<kind>_cst()`); lithic and paralithic contacts; anthro-silting materials; vertic features and vertic evidence; the anthroturbic layer; gleyic features and gleyic evidence (gley colours without a ferrous-iron test or Eh met provisionally, Q11); redoxic features; the permafrost layer; frost-thawic features; the n value; the isohumic property (Rh 0.4 or less, the printed "4" read as 0.4, Q9; the book's default bulk densities where none was measured, flagged, Q10); the humic, andic, ferric (every layer of the B, Q23), allitic (the Ki of the sulfuric attack flagged, Q16), alic, phosphic, sodic and calcaric properties with their evidences; base saturation (eutric, dystric, eutrophic, dystrophic in a window); sulfidic materials (no number in the book: the WRB number and stand-ins, always with a lower grade). The two regimes are the readers of 0.9.370. Layer characteristics take the grades of the horizons (full, evidence, at least evidence) and join the horizon panel (`characteristics`, `characteristic_evidences`, `characteristic_stand_ins`); the sulfuric horizon now reads the sulfidic materials under its base. Numbers in `inst/rules/cst2001/thresholds.yaml` with their pages; Hugo's decisions as applied and soilKey's own readings of this part, for his word, in `inst/rules/cst2001/readings.yaml`. Fixtures of the differences from the WRB 2022 (sodic ESP 30, the isohumic ratio, the Chinese gley colours, alic pH(KCl) 4.0, andic of KST 1996, ferric). No new attribute. Every other classification is unchanged (the fixtures of the other systems byte-identical).

Decided by Hugo on the review (2026-11-06): rock layers carry no soil colour; carbonate rocks by rock nouns only, undecided where the rock is not named; the 60 cm Histosol tier needs fibric moss materials; frozen designations read after a discontinuity numeral, and frozen ground not recorded is absent where the description reaches the depth asked (only a frigid or warmer regime rules permafrost out); the scaly structure in an A or B only; provisional gleyic features marked in the panel, and gley colours alone not read in an E or albic layer without redox features or a saturation regime; the allitic fine-earth ratio (the sulfuric Ki) ORed with the clay ratio; redoxic item 3 without a carbon exclusion, false under a well-drained record; northern red earths by a red earth or red clay parent material; the calcaric property on measured CaCO3, else effervescence, else pH under 6.5 non-calcaric; the isohumic depths scaled to the contact; the sulfuric horizon's lower grade only on the WRB stand-ins; the phosphic pattern with no reversal over 10 %; the sandy carbon as a weighted mean; every purplish layer with fragments under 4 cm.

# soilKey 0.9.371 (2026-11-06)

## China CST 2001: the diagnostic horizons

Second part of the Chinese Soil Taxonomy (2001, English edition; system id `cst2001`, registered in a later part). The 33 diagnostic horizons of the book (pp. 20-43), each read layer by layer on the data (`R/diagnostics-cst-horizons.R`, `R/diagnostics-cst-anthropic.R`): a grade per layer, full, evidence (the book's weaker grade, for the 15 horizons that have one), none or undecided; readers `<horizon>_cst(pedon, grade = "full" | "evidence" | "at_least_evidence", top_max)` for the key, `phos_agric_horizon_cst()` for the phos-agric kind of the agric horizon and `argic_horizon_cst(natro = TRUE)` for the natro-argic horizon of the Aridosols. A WRB or USDA diagnostic of soilKey stands in only where a CST descriptor is missing, flagged with a lower grade. `.cst_identify()` gives the horizon panel (the horizons, evidences, kinds and stand-ins of each layer); `identify_horizons(pedon, "cst2001")` reaches it once the system is registered. The numbers are in `inst/rules/cst2001/thresholds.yaml` with their pages; soilKey's readings of this part, for Hugo's word, in `inst/rules/cst2001/readings.yaml`. Fixtures of the differences from the WRB 2022: the argic clay increase (+3 / x1.2 / +8 / fine clay +8), plinthite 10 %, the alkalic horizon at ESP 30 and pH 9.0, the salic horizon on salts or the 1:1 EC. Every other classification is unchanged (the fixtures of the other systems byte-identical).

# soilKey 0.9.370 (2026-11-05)

## China CST 2001: the attributes, the shared readers and the regimes

First part of the Chinese Soil Taxonomy (2001, English edition; system id `cst2001`, registered in a later part). New horizon attributes (36, section "Chinese system (CST 2001)", among them `fine_clay_pct`, `clay_films_thickness_mm`, `reduced_matrix_pct`, `ferricyanide_test`, `eh_mv`, `p_olsen_mg_kg`, `ec_1to1_dS_m`, `shajiang_pct`, `pyroclastic_fragments_pct`) and site attributes (14: `aridity_index`, `aridity_index_max_month`, `climatic_moisture_regime`, `udic_perudic_border`, `mean_summer_soil_temp_C`, `summer_saturated`, `cracks_open_days`, `irrigated` and others), each with its fallback. `soil_moisture_regime` takes stagnic, anthrostagnic and peraquic, which the USDA key reads as aquic; `soil_temperature_regime` takes permagelic, read by the USDA key as gelic. The shared readers (`R/cst-readers.R`: units, clay activity, free iron, texture and particle-size classes, hue ranks, the mixed surface, windows and shares, storage with the book's default bulk densities, the arid region) and the regime readers (`R/cst-regimes.R`, each regime with its source in the trace) read the numbers of `inst/rules/cst2001/thresholds.yaml`. The moisture regime is read on two axes, climatic (aridic, ustic, udic, perudic) and saturation (stagnic, anthrostagnic, aquic, peraquic), so a paddy recorded udic is anthrostagnic and a value on one axis says nothing of the other; permafrost within 50 cm gives the permagelic regime ahead of an estimate from the air; Cr is a paralithic contact only with a contact recorded or an FAO, WRB or USDA notation declared (otherwise the redox r of the Chinese field notation); an O horizon without a measured bulk density takes the range 0.1-0.4 Mg/m3, so a storage is a range. Hugo's decisions of 2026-11-04 and 2026-11-05 are in `inst/rules/cst2001/readings.yaml`. Every other classification is unchanged (the fixtures of the other systems byte-identical).

# soilKey 0.9.365 (2026-11-07)

## Germany (KA5, 2005): the varieties and subvarieties

`classify_ka5()` now keys the fifth and sixth levels of the German systematics (section 5.7.1.4, pp. 263-282), an additive level after the subtype (Hugo's decision Q1): the 38 variety codes of Tab. 49 by the 22 criteria of Tab. 46, offered where Tab. 50 marks the subtype, plus the six of its note 1 (og, c, z, k, v, ku) everywhere except where the subtype's own formula writes the feature; the subvarieties as a digit on the letter (s and g flach 2 or tief 4, p schwach 2, mäßig 3 or stark 4; the base-saturation class BS1-BS5 of Tab. 48 on the potential or effective CEC is reported in the trace). Tab. 50 is in `inst/rules/ka5/varieties.yaml` with its pages, all 223 subtype rows read from the page images (the terrestrial rows of pp. 274-279 for the first time), with the codes, criteria, thresholds, subvariety classes, symbol rules I-VII and the printed examples. Each criterion is read on the horizons, three-valued: on the notation by the symbols written (Aeh, Ahe, Sew, rGo, eGo, zAh, oAh, Bms, Gkso, Cbtv, Ctv, Akh, Ap, the base-saturation marks), on the data by the record (base saturation, colours, organic substance, carbonate, EC, jarosite, cementation, clay films and bands, erosion degree, rewetting, land use); the humus forms only as recorded. A variety the data leave open is not named: it is listed as undecided in `trace$varieties` and one warning, the result not provisional (p. 263). The result gains rows of level `variety` (the code with its digit, the word of the name, the gloss, the full symbol in a `symbol` column), the full name in `name` ("podsolige pseudovergleyte Normbraunerde", "(Acker)Gley") and the symbol in `trace$variety$symbol` (`p2.s2BBn`, `vGGn`); the development depth of Tab. 47 is reported in the trace. The further subtypes of the type that pass join as a combination (Q21; Tab. 46 no. 17, rules II-IV: `PP-GG-BB` Podsol-Gley-Braunerde, `BB-GGe`, `GGh-GGc` (Humus)Kalkgley); the overlays of Tab. 46 no. 18 stay out of the levels and the name, reported in the trace (`trace$variety$overlay`); r and e stand before the letter they qualify (`BB-rGG`, `r.gLFn`, `LF-eBB`). An Aeh, Ahe, Akh or Aka takes a subtype formula's plain Ah slot, so a podzolic or cultotrophic topsoil keeps the Norm subtype. `make_ka5_example(variety = ...)` gives a profile for each of the 38 codes; every printed example of pp. 268-272 reads back to its name from its symbol. The levels above the variety of every KA5 example are unchanged; their names gain the varieties their data carry (the default Bv and Tv colours of the examples are no longer iron-rich or reddish; the podzolic B of the subtype examples keeps its colour, so their data twins key as before, and is read as reddish). New horizon attribute `schwertmannite_present` (the variety sf). soilKey's readings are in `readings.yaml`. Every other classification is unchanged.

Decided by Hugo on the review (2026-11-07): the name carries the varieties, the overlays do not (the decision of 0.9.364 stands); open varieties stay out of `provisional_because`; rt is read on the type's own B or T horizon (Bv, Bs, Tv, Bt); a feature the subtype's formula already writes is not named (an optional (e) counts as written; no note-1 c on the Kalkpaternia, Pararendzina, Rendzina and Kalktschernosem); on the notation route the carbonate data stand in for the prefix e only on rows with no symbol, and a Tschernosem's carbonate is not geogenic by default; an Ae under an Ahe is stark podsolig; rule IV allows a hydromorphic type last (BB-LL-SS); a rewetting not recorded is absent; the base-saturation class goes to the trace only; a and g on the same deep aGo name a only; no anthropic-truncation stand-in for e; eu, m and dy are open where no horizon outside the C reaches into 2 to < 5 dm, and the O/C soils read their O horizon; the relict r qualifies a g or s only when that one is named, otherwise the type (rMCn); b on the data needs a brown colour; the aG of the variety a lies strictly deeper than 8 dm, and a description ending above it leaves a open; sf accepts schwertmannite.

# soilKey 0.9.364 (2026-11-06)

## Germany (KA5, 2005): the semiterrestrial, subhydric and Moor subtypes; the overlays

`classify_ka5()` now keys the subtypes of the semiterrestrial, subhydric and Moore types from the type texts (pp. 238-262): 85 subtypes (A 12, G 40, M 15, Ü 3, I 3, H 4, K 7 and the unit KV-KM; the Nassstrand and the four subhydric types of class J have none), in `inst/rules/ka5/subtypes/{A,G,M,UE,I,J,H,K}.yaml`, with the order and fallback of Q21: the Auenböden by their aGo at 4 to < 8 dm (Gley-X) or their aG at 8 dm or deeper (the Norm, an aG under the description allowed); the Gley transitions of the terrestrial and Auen types (the Gr typically 8 to 13 dm, not required), the Hang- and Quellen- subtypes by the prefix s or q (on the data by `groundwater_source`, the slope for the s), the Oxigley by the oxygen-rich groundwater or no Gr in a profile described to 13 dm (Q16), the Moorgley with no Norm subtype; the Marschen, the Strand and the Watt by their tidal prefixes tm, tb, tp; the Niedermoor and Hochmoor by the pH of their peat where measured; the Erd- and Mulmmoore by their suffixes, with the transitional unit KV-KM. Two open subtypes of Q22, in brackets: "[Übergangsmoorgley]" and "[vermulmtes Erdhochmoor]". The Rohmarsch keeps Q20: a Go-Ah is a Rohmarsch unless the site is embanked or not tidally flooded.

The overlays (Tab. 46 no. 18) are reported in `trace$overlay` and the warnings, as varieties that change neither the levels nor the name: a mineral cover on a Moor keeps the Moor (Q7), named as substrate only under 2 dm, "flacher Gley über Niedermoor" (`GG\HN`) at 2 to < 3 dm and "Gley über Niedermoor" (`GG/HN`) at 3 to < 4 dm, the cover keyed as the mineral soil it would be; a fersiallitic or ferrallitic palaeosol under the soil is named "... über Fersiallit / Ferrallit" (Q19); a buried soil (fossil horizons of a layer II or deeper) is keyed alone from its own top and named by the thickness of the soil above, under 3 dm "flach", 3 to < 7 dm, the Organomarsch over a Podsol by its own steps (1.5 and 3 dm, p. 254). `make_ka5_example(subtype = ...)` gives a profile for each of the 85 subtypes and the two open ones, with a data twin (63 key on the data alone); the wet type examples key to their Norm subtype and keep their types. The Paternia gate now leaves a Paternia-Vega (an aM under the ilC) to the Vega, and the Auenböden take an S under an aM anywhere above it (the Pseudogley-Vega). soilKey's readings are in `readings.yaml`. Every other classification is unchanged.

* After the review (Hugo, 2026-11-06): the readings accepted; only an aM (prefix a) beginning above 8 dm takes a profile out of the Paternia, aM bands in the ilC keeping it; the Gley-Rambla, Gley-Paternia and Gley-Kalkpaternia allow their aG under the description; the Wechselgley takes no q; a Moorgley whose peat is of no known kind leaves the subtype undecided, and the Hang-, Quellen- and Hochmoorgley are tested before the Niedermoorgley; the Normhochmoor and the Hochmoor gate read the pH in the hH(w); on the data route a horizon among a row's candidates, or one the data key does not build (Gso, Hvm, an unrecorded secondary carbonate, a layer II, an A or M part), leaves a subtype undecided rather than failed; a Moor under a cover of less than 4 dm reads its type from the top of the peat (Ah/Go over an nHv: GG/KV).

* The bleached and podzolic horizons on the data (Hugo, 2026-11-06): the order of tests on the data reads a pale first row over a pale Ae over a Bh or Bs as an Ahe; a B with the Cp/Fep of a Bh or Bs is no Bt when no clay coatings are recorded; illuvial humus (the Ah's humus minimum under a pale horizon without it) or oxalate Fe and Al accumulated under a pale horizon leave the Bt and the podzolic B both undecided; under a podzolic B a clay increase with no clay coatings recorded is no Bt, and the eluvial Al of a Parabraunerde under a podzolic B is read; a G with a podzolic B part is a Bhs-Go, not an Ah-Go. On the data the Podsol-Gley twin now keys PP-GG (it read Ah/Al/Bt-Go before), the Podsol-Braunerde PP-BB (was a Parabraunerde), the Braunerde-Podsol BB-PP (was LL-PP), the Plaggenesch-Podsol a Podsol (was a Parabraunerde); LL-PP, GG-PP and YK-PP keep their subtypes with fewer candidates. The types of the type examples and of the notation route are unchanged.

# soilKey 0.9.363 (2026-11-06)

## Germany (KA5, 2005): the subtypes of the 28 terrestrial types

`classify_ka5()` now keys the subtypes (Subtypen) of the terrestrial types from the type texts (pp. 202-237): 138 subtypes of the 13 terrestrial classes (F 2, O 3, R 27, T 11, D 8, B 10, L 15, P 11, C 6, S 27, X 4, Y 14; the palaeosols VV and VW have none), in `inst/rules/ka5/subtypes/`, and the registry lists the level `subtypes`. Each subtype is its printed formula or formulas, matched on the horizons with the sequence engine, its depth windows read from the GOF (the Gley-X subtypes by the first G at 4 to < 8 dm, the Pseudogley-X by the first S, the Podsol-X and X-Podsol by the bottom of the podzol B, the humic and anthropogenic parts by 4 dm ...) and its numbers (the base-saturation marks of the Ah/C-Boeden on the subtype scale, the clay band of the Parabraunerde-Braunerde, the Fahlerde's clay difference in the LF-X subtypes, Cp/Fep, the slope of the Hangpseudogley, the Terra material ...), with the limits in `thresholds.yaml`. Following Hugo's decisions: inside a type the transitional subtypes come first in printed order, then the deviation subtypes, the Norm subtype last as the fallback, and every subtype that passes is listed in `trace$subtypes_passing` (Q21); the Treposol has no Norm subtype, and the Rigosol and Treposol take an open subtype for the type still recognisable under the R, written in brackets ("[BB-YY]", "[Treposol aus Braunerde]", Q22). A subtype the data leave open makes the result provisional, with the others as candidates; the Fahlerde-X subtypes and the Parabraunerde-Braunerde hold on the designation, provisionally, when their clay is not measured (C13). `make_ka5_example(subtype = ...)` gives one profile per subtype in the KA5 notation and its data twin (51 of the 138 key on the data alone); the 56 type examples keep their types and key to their Norm subtype. The Pseudogley gate now takes the Scw and Scd of the Kalkpseudogley, and the Ranker gate a thin loose layer over solid rock under 3 dm. The readings are in `readings.yaml`. Every other classification is unchanged.

* After the review (Hugo, 2026-11-06): the horizons under a formula are free and its final C is met by any deeper row, undecided when nothing is described under it; a slot (X-)Y takes a run of X-Y and Y horizons; the Lessives gate takes a Bt-Sd (the first forms of SS-LL and SS-LF); the Fahlerde's clay difference is the Ael against the Bt, not the Ael+Bt (also for the type); the Euranker and Euregosol need no Bv; the Norm subtype tests its formula and the marks that complement a deviation and holds provisionally, the deviation a candidate, when they fail (R1); an Sw written without s on a slope of 9 % or more leaves the Hangpseudogley undecided (R2); the open subtypes need a Bt for LF, the printed window for a G or S, and never repeat a printed code; a solid rock with no recorded top leaves the Ranker undecided; the base-saturation marks read every horizon of the topsoil.


# soilKey 0.9.362 (2026-11-06)

## Germany (KA5, 2005): the registry and the key to the 56 soil types

`classify_ka5()` keys a profile to the divisions (Abteilungen), classes (Klassen) and types (Typen) of the German soil systematics by Tab. 51 of the Bodenkundliche Kartieranleitung (pp. 283-288): the Moore, the subhydric, semiterrestrial and terrestrial soils, 21 classes and 56 types, read on the horizons of `identify_horizons(pedon, "ka5")` (the KA5 notation, else the order of tests on the data). The system is registered as `ka5` (aliases `germany`, `kartieranleitung`, `bodensystematik`), experimental and opt-in. Following Hugo's decisions: Tab. 51 with the numbers of the type texts (Q6) and two rules of the figures (Q5), the Moor under a cover of less than 4 dm (Q7), the last symbol carrying the horizon (Q8), the rock of the Ah/C-Boeden (Q15, Q18), a thick Ah without Axh undecided (Q17), the upper soil over a palaeosol (Q19), the Rigosol and Treposol by the record (Q23). A gate the data leave open makes the name provisional; the Fahlerde on its designation without the clay (C13) and the Rigosol without the kind of cultivation are listed as provisional too. The names are as printed (German), with English and Portuguese glosses in the rules; the horizon panel is in `trace$horizon_panel`, the figures' consistency flags in `trace$figures`. `make_ka5_example()` gives one profile per type in the KA5 notation, its near miss and its data twin (51 of the 56 key on the data alone; the Knick-, Dwog- and Haftnaessemarsch and the palaeosols need the notation). The Pro app offers the 56 examples. The subtypes, overlays and varieties come next. Every other classification is unchanged.

* After the review (Hugo, 2026-11-06, the 19 defects): buried soils no longer decide the present soil; tidal prefixes count only in the present solum, z as tidal; the Axh window and the colour conflict of the Schwarzerden; truncated descriptions and missing depths undecided; the peat body across thin interbeds; the Kalkmarsch by its carbonatic horizon under 4 dm; one strict R bound; the Moore types on the topsoil; stand-ins and a class without type shown as provisional; the Organomarsch without o, marl under a Terra and an Ai on block debris (Syrosem) as Hugo decided.


# soilKey 0.9.361 (2026-11-06)

## Germany (KA5, 2005): the horizons on the data

The KA5 horizons read on the recorded data, used by nothing yet: `.ka5_identify()` (the identification `identify_horizons(pedon, "ka5")` will reach once the system is registered) reads every layer by soilKey's fixed order of tests (Hugo's decision Q9: F, H and O, Y, R, E, M, G, S, A, B with T and P, C, then the prefixes and suffixes and the transitions Bv-Sw, Sw-Bv, Bv-Go, Go-Bv, Ah-Bv, Ah-Go). Every test is three-valued: an undecided one gives candidates and the attributes that would decide them. A layer written in the KA5 notation keeps its horizon and the data's reading goes to the evidence. The readers (`R/ka5-readers.R`): the depth datum GOF (p. 79), organic substance with the 15 to < 17.4 % band (Q10), the Ah minimum with the silt of other fractions (Q11), the Munsell pages (Q12), the area classes and field codes, the carbonate of the rock (Q15), the clay differences of the Bt and the Fahlerde. The numbers are in `inst/rules/ka5/thresholds.yaml` with their pages; soilKey's new readings, for Hugo to confirm, in `readings.yaml`. Every classification is unchanged.

* After the review (Hugo, 2026-11-06): a recorded water table makes a horizon G only within its fluctuation zone; a notation C or H without its prefix takes it from the data or leaves it open; the Ael without colour is undecided against the Al; no topsoil under 30 cm of peat; the Srd and Srw conventions and the F colour cuts (a lower grade) recorded.


# soilKey 0.9.360 (2026-11-05)

## Germany (KA5, 2005): the horizon notation and the formulas

The first piece of the German soil systematics of the Bodenkundliche Kartieranleitung, 5th edition (Ad-hoc-AG Boden 2005), used by nothing yet: the notation reader, its catalogue, the formula dialect of the types and subtypes, and the new attributes. Every classification is unchanged (the fixtures of the other systems byte-identical).

* **The catalogue** `inst/rules/ka5/horizons.yaml`: the 15 masters, 23 prefixes, the suffixes with their meaning by host (a, f, k, m, t, v, w), the base-saturation marks, the joiners, Tab. 12 of the permitted combinations, the 166 symbols 5.6.3.3 defines (checked on the page images) and the transitions it names, the KA6 symbols with a one-to-one reading, the field codes that are not horizons and the collisions with the other notations.
* **The reader** (`R/designation-ka5.R`): the last part carries the horizon (Ah-Bv is a Bv, fAh°Sd an Sd in a buried Ah), lower case before the master is a prefix and after it a suffix, Roman numerals, sub-horizon digits, letters in parentheses as sets of readings (B(h)s), the typing slips IC, AI and the degree sign typed o. A profile is in the KA5 notation when declared (ka5, ka4, ka3, kartieranleitung, bodensystematik, agboden, dbg; ka6 through its one-to-one symbols) or recognised by one strong sign or two likely ones, with no symbol only the FAO, USDA, Canadian, RP, CPCS or Russian notation writes; a mixed profile is undecided; never by the country (Hugo's decisions Q2, Q3, Q4, Q8).
* **The formulas** of Liste 31 and of the 223 subtypes (`.ka5_parse_formula()`) read into the sequence engine's formula object, with no unknown token; the strings, transcribed from the page images, are in `tests/testthat/fixtures/ka5-formulas.tsv`.
* **New attributes** (Hugo's decision Q24): 26 horizon columns (`bleached_pct`, `reduction_colours_pct`, `rh_value`, `air_capacity_pct`, `total_porosity_pct`, `sand_ffs_pct`, `carbonate_whole_soil_pct`, the KA5 field codes `humus_class_ka5`, `carbonate_class_ka5`, `texture_class_ka5`, `colour_ka5`, `hydromorphic_features_ka5` ...) and 13 site properties (`particle_size_scheme`, `embanked`, the groundwater levels of Tab. 59, `reductgas_source`, `deep_cultivation_kind` ...); the field codes are read into them by their class ranges.
* **Matching a profile to a formula** (`.ka5_horizon_frame()`): the last part carries the horizon and the whole symbol is the engine's token, so Bv-Ah over Ah stays two horizons; Ap takes a plain Ah; the buried soil of `II...` runs from the first fossil row of a layer II; a geogenic prefix written in front (a, tb, tm, tp, e, z, s, q, o) holds for the whole horizon, f and r only for their part. Before the engine reads the table, a KA5 formula's own hook (`prepare_table`, which no other system carries) lets a master written without suffix (C, G, aG, O) or a slot with commas take a run of rows (Ah/Bv/Cv/Cn is Ah/Bv/C, p. 199), and merges a transitional row into its neighbour with the same carrying code unless the formula names the transition (Al+Bt into the Bt, Go-Bv into the Bv).
* Hugo's decisions on soilKey's readings and on the review (2026-11-04) are in `inst/rules/ka5/readings.yaml`: the strong signs exactly as his card lists them (Btv is none); the Canadian j forms (Btj, Aej) and Ck and Oe written as designations veto the KA5; IIIC is II and lC with no layer II above it; the degree sign typed o only in a KA5 profile.

# soilKey 0.9.359 (2026-11-06)

## South Africa 1991: the families

`classify_za1991()` now keys the second and last level of the South African soil classification of 1991: the 404 families of the 73 forms, by the 19 family criteria. The name follows the identification of the book (Q23): the form, the family code and name, and the USDA texture class of the A when its clay, silt and sand are recorded (`"Hutton, Hu1200 Kelvin, sandy loam"`; the sand grade with `sand_grade`). Every other classification is unchanged (the fixtures of the other systems byte-identical).

* **The families** (`inst/rules/za1991/families.yaml`, written by `data-raw/za1991_families.R`): codes of the form and the four printed digits, names as printed, the slips of the key pages read as in the plan (Plooysburg 1000 and 2000, the 1110 headers, the Shortlands 3000 families on the B1, the signs of wetness of the soft carbonate horizon). The first family whose classes all hold is the family; a criterion undecided for lack of data leaves the result provisional, with the families still possible and soilKey's partial code, an x for each undecided digit (`"Hutton, [Hux200]"`).
* **The 19 criteria** (`R/families-za.R`), each read once on the horizon its family names, as the form key identified it (the B1 of Q20, the A, the E, the upper G, the upper C, the layer under the placic pan or the podzol B, the profile within 1500 mm): the base status of the B1 (S-value x 100 / clay %), the luvic B1 (Q21), hard lithocutanic B and saprolite (Q19), calcareous (Q22), the colours (Q9-Q12), podzolic character (Q13), the humic A, fibrous O, lamellae, structure, black cutans, ortstein, the consistence of the C and the material under the O. `qualifiers$za_family` lists every criterion read: its class, the layers, the stand-ins and the attributes missing.
* **Examples**: every form example holds the classes of its first family, and `make_za1991_example(form, family = "Hu1200")` gives a profile of any of the 404 families, by the data and by the notation.
* **Decided by Hugo on the review (2026-11-06)**, in `readings.yaml`: an organic O of unrecorded lower limit no longer stops the key; the absence of wetness or carbonates is undecided on a description that stops above 1500 mm or has nothing under the A; a pale soft carbonate horizon shows signs of wetness only by its mottles or a gley hue; hardness follows the USDA rupture resistance (dry hard, moist very firm, moderately cemented or more); the 500 mm under the organic O is the book's (K 17); one pair of brackets around a partial code, and none when no class of a digit holds; a redder E is grey; Dundee reads its wetness from the alluvium down; a calcareous part anywhere in the B makes the calcareous Shortlands families; a fibrous O needs one run of 200 mm; the B1 is the upper layer of the first B; on a B of several colours FC 5 takes the larger share of dark or red; the other readings of this part confirmed.
* **A diagnostic horizon of several layers** on the data: the lower layers of a red structured B (and of a placic pan) under a layer of the same horizon were read as no horizon, and those of a lithocutanic B as saprolite; they are now read as the horizon, as the other B horizons, the E and the G already were (a layer under a lithocutanic B is saprolite only by its own rock).
* The criteria names in English and Portuguese, the families level in the app and the report.


# soilKey 0.9.358 (2026-11-05)

## South Africa 1991: the registry and the 73 soil forms

The South African soil classification of 1991 joins the registry as `za1991` (aliases `za`, `sat`, `sat1991`, `south_africa`), experimental and opt-in: `classify_za1991()` keys a profile to its soil form, `make_za1991_example()` gives one synthetic profile per form, and `identify_horizons(pedon, "za1991")` reads every layer as one of the 30 diagnostic horizons and materials. Every other classification is unchanged (the fixtures of the other systems byte-identical).

* **The 73 forms** (`inst/rules/za1991/key.yaml`, written by `data-raw/za1991_key.R`) in the order of van Huyssteen (2020), each a sequence of diagnostic horizons and materials with the horizons that would make it another form (Q3: Hutton over hard rock is Hutton, over a soft plinthic B Bainsvlei); Lichtenburg keyed, always provisional (Q1); no form gives the nearest forms, an orthic A under 100 mm over nothing the land class rock and lithosols (Q2).
* **The general rules**: 1500 mm (Q7), a recent deposit over a buried soil (Q8), truncated profiles; a description stopping above 1500 mm leaves the longer forms undecided (Q4); the 500 mm rules of the form pages (Valsrivier, Sepane, Swartland, the podzol forms, Witbank, Dundee, Namib).
* **Decided by Hugo on the review (2026-11-06)**, in `readings.yaml`: the saprolite of Swartland, Houwhoek and Groenkop may lie within 500 mm under the B, below unconsolidated material; the match is one pass over the horizons, however many are undecided; a topsoil of several layers is a vertic, melanic or humic A when that covers half of it or its upper 200 mm, for every completion of its undecided layers; a topsoil over a recent deposit is part of the deposit; a red over a yellow-brown apedal layer is one horizon unless each is 20 cm or more; a B reaching 1500 mm reads the layer described below it; registry order 14.
* The form names and soilKey's glosses in English and Portuguese, and the examples in the Pro app. The families follow in the next part.


# soilKey 0.9.357 (2026-11-05)

## South Africa 1991: the other horizons and materials on the data

The third part of the South African soil classification of 1991: three-valued tests on the recorded data for the other seventeen diagnostic horizons and materials, the position pass and the identification of every layer (`R/diagnostics-za-horizons.R`). Nothing is classified yet; every other classification is unchanged (the fixtures of the other systems byte-identical).

* **The tests**: the E horizon (grey dry, eluvial), the podzol B and the placic pan, the soft and hard plinthic B, the soft and hardpan carbonate horizons, the dorbank, saprolite, hard rock, stratified alluvium, regic sand, the man-made soil deposit and the unconsolidated material; the podzol B, placic pan, dorbank and carbonate horizons by the WRB numbers standing in, flagged (Q13-Q16), in `thresholds.yaml`.
* **The position pass**: a grey low-chroma matrix is the G horizon under a topsoil or E, the unspecified material with signs of wetness under an apedal, neocutanic or neocarbonate B, the unconsolidated material with signs of wetness under a podzol or pedocutanic B; G or prismacutanic under an E by the 500 mm of K 19 and 27; lithocutanic B under the topsoil, saprolite under a B; the placic pan only under a podzol B; T-humic (iv) on the horizon identified below.
* **`.za_identify()`**, the identification of the registry namespace `za` for `identify_horizons()` (registered with the system in the next part): the notation where a layer is named, else the data, with the stand-ins and fallbacks in the trace.
* **Decided by Hugo on the review (2026-11-06)** and recorded in `readings.yaml`: the B horizons by position (an apedal or structured B only under a topsoil, an E or the same B; under a podzol or pedocutanic B such a layer is unconsolidated material); the E with less clay than the layer below or at most 2 points over the one above, only under an orthic A or an E, and a grey eluvial layer never a G; a grey pedocutanic B is the G only with one more sign of wetness; a B1 soft plinthic only with grey of its own or as many mottles as the layer below; organic carbon on the oxalate route of the podzol B; saprolite under the topsoil or an E is the lithocutanic B; a red or yellow apedal surface layer with under 0.3 % organic carbon over rock, a lithocutanic B or a dorbank is that B; a surface layer designated C is a topsoil.

# soilKey 0.9.356 (2026-11-05)

## South Africa 1991: the topsoils and the B horizons on the data

The second part of the South African soil classification of 1991: three-valued tests on the recorded data for the five topsoil horizons and the eight apedal and structured B horizons (`R/diagnostics-za-horizons.R`). Nothing is classified yet; every other classification is unchanged (the fixtures of the other systems byte-identical).

* **The topsoils** (organic O, vertic A, melanic A, humic A, orthic A), in that order (default 7): the organic O by 10 % organic carbon over 200 mm and its saturation, recorded or by its evidence, flagged (Q18); vertic against melanic by the plasticity index, else the slickensides, flagged (Q17); humic against melanic by the 4 cmol(+) bases per kg clay per 1 % organic carbon; the humic A tested twice, the second time on the layer below it, and orthic when that fails.
* **The B horizons**: red apedal, yellow-brown apedal, red structured, neocutanic, neocarbonate, pedocutanic, prismacutanic (the clay rule of S 32, 20 % exactly by the +20 rule, Q21) and lithocutanic B, with the CEC under 11 cmol(+)/kg soil only for a borderline structure (default 16) and calcareous by effervescence or CaCO3 (Q22).
* **The colours** of Hugo's decisions Q9-Q12: grey dry, yellow-brown, red (moist hue, the dry one flagged) and low chroma.
* **`inst/rules/za1991/thresholds.yaml`**: every number with its source and a `stand_in` flag; the WRB and soilKey stand-ins are flagged in every answer that reads them ("stand_in:<name>"), so the book's numbers can replace them without a change to the code. The readings of this part were decided by Hugo on its review (2026-11-05), with his amendments: low chroma outside the gley hues only at a moist value of 4 or more, 10Y a gley hue; red not grey (5YR 6/1 is not red); with no dry colour, a moist chroma of 4 or more is not grey; an A2 under an A1 is a candidate E; a borderline grade (weak to moderate) is apedal under 11 cmol(+)/kg soil and moderate at 11 or more; no B horizon on rock; the uniform dark brown weak B is neocutanic; red structured and red pedocutanic stay candidates together when the transition is not recorded; a calcareous stratified layer stays the material (Q6). `readings.yaml` records them.

# soilKey 0.9.355 (2026-11-05)

## South Africa 1991: the diagnostic horizons and their names

The first part of the South African soil classification of 1991 (Soil Classification Working Group, 1991; the "Blue Book", read from its open pages, SASRI 1999 and van Huyssteen 2020, without the book): the catalogue of its 30 diagnostic horizons and materials, the reader of their names, and the attributes the later parts read. Nothing is classified yet; every other classification is unchanged (the fixtures of the other systems byte-identical).

* **The catalogue** (`inst/rules/za1991/horizons.yaml`): the 5 topsoil horizons (organic O, humic A, vertic A, melanic A, orthic A) and the 25 subsoil horizons and materials, each with its printed name, soilKey's English gloss, a token in capitals (OA, RAB, SPL ...), what the preface says of it in 1977, and its pages; the names of 1977 the preface changed; the other words for the hard and carbonate horizons (calcrete, ferricrete, ouklip, silcrete); the hints of the FAO letters.
* **The reader** (`R/designation-za.R`; Hugo's decision Q5). A new horizon attribute `diagnostic_za` holds the name as surveys write it ("red apedal B", "Soft plinthic B horizon", "B2 soft carbonate", "none"), in any case and with or without "horizon", or soilKey's token (RAB); E and G alone are the E and G horizons. A 1977 name is read as its 1991 equivalent, flagged (ferrihumic B: podzol B; gleycutanic B: G horizon or unspecified material with signs of wetness by the horizon above; calcareous apedal or neocutanic B: neocarbonate B). A negated name (not red apedal B, E horizon absent) is not read; a non-diagnostic one (saprolite (non-diagnostic)) records no diagnostic horizon; one written with doubt (E?, possibly red apedal B) stays a candidate; calcrete is read by its adjective. The same names written inside `designation` ("Ap orthic A", "B1 red apedal") are read when `site$designation_system` declares the notation (`za`, `za1991`, `sat`, `sat1991`, `south africa` ...; `SA` and `RSA` declare nothing), when the profile records a name in `diagnostic_za`, or when a designation holds a name only this system writes; never from the country, never when the site declares another notation. FAO and USDA letters are hints only (Bt: pedocutanic, prismacutanic, red structured or neocutanic B; Cr: saprolite, the wet materials, G or lithocutanic B); a word (Clay, Hutton) gives none.
* **New attributes**: on the horizons `diagnostic_za`, `plasticity_index`, `liquid_limit_method`, `bedrock_pct`, `coatings_hue_moist`, `coatings_chroma_moist`, `coatings_continuity`, `recent_deposit`, `cross_bedding` and `sand_grade`; on the site `reference_za_form`, `reference_za_family`, `reference_za_raw` and `reference_za_edition` (benchmarks only).

### Readings decided on the review (2026-11-04)

Hugo decided the nine readings of this part with the fixes of its review: soilKey's two-letter short forms (ot, re, ye ...) are not read until checked on a survey sheet; calcrete by its adjective (alone undecided between the soft and the hardpan carbonate horizon); Cr hints at the saprolite, the wet materials, the G horizon and the lithocutanic B; the other six (recognition by name, the gleycutanic B by position, Afrikaans names, words of no diagnostic horizon, the master letter, the declared values) as recommended. `inst/rules/za1991/readings.yaml` records them with the four decisions of 2026-11-04 this part applies.

# soilKey 0.9.354 (2026-11-04)

## France: the correspondence tables of the Référentiel pédologique 2008

The annexes of the RP 2008 that correlate it with other systems, and the labels of the RP of 1995, are shipped as data (Hugo's decision Q1): codes and names as the book prints them, with the page, its doubts ("?") kept as a flag and its slips noted; no sentence of the book. They are notes for display and benchmarks: no classifier reads them, and every classification is unchanged (the fixtures of all systems byte-identical).

* **Annex 5, the WRB 2006** (pp. 378-392; `inst/rules/rp2008/crosswalk_wrb2006.yaml`). The 110 references with their WRB 2006 name (the brunisols eutriques in two rows by their saturation, the lithosols stricts in a row of their own), the 12 reference horizons beside a WRB 2006 diagnostic horizon or FAO letter, the qualifiers (65 rows for the 64 of the book: the row of the calcium chapter, p. 380, holds for that chapter only) and the 15 "faux amis". The WRB is the 2006 edition the book cites, not soilKey's WRB 2022.
* **Annex 6, the CPCS 1967 horizons** (p. 393; `crosswalk_cpcs_horizons.yaml`). The table row for row, exported as data; the notation reader keeps its own copy in `horizons.yaml` for the hints of Q9, and a test keeps the two equal. BTfe, FSj and the Ha under the A1 are flagged as slips.
* **Annex 7, the CPCS 1967 categories** (pp. 394-398; `crosswalk_cpcs_classes.yaml`). Each row keyed by the `cpcs1967` code of its category (class, subclass, group or subgroup), its RP entries resolved to reference codes or groups, with their qualifiers, the double attachments and the "???" of the categories with no equivalent; the 14 categories the book calls new.
* **The labels of the RP 1995** (p. VII; `legacy_rp1995.yaml`). The 13 references removed in 2008 and the names of 2008 the book gives for such solums.
* `rp2008_crosswalk()` returns each table as a data frame; `rp2008_from_label()` reads labels of French datasets (a reference, a type, a double, a label of 1995) as the references of 2008, for benchmarks.
* The report and the Pro app show the WRB 2006 names of the references of an RP result (Annex 5) and the RP equivalents of a CPCS result (Annex 7), as notes.

### Readings confirmed (2026-11-04)

Hugo confirmed the readings of this release, with the points of its review: all the tables of Annex 5 shipped; the table of Annex 6 as data beside the reader's copy, FE, OX and F of row B intertropical and BTfe, FSj and the Ha under the A1 flagged as slips; a CPCS result shown the row of the most specific category of its chain; the entries of Annex 7 that name no reference read through their chapter (fersialsols calcaires and saturés as the carbonatés and calciques, luvisols-rédoxisols as the double of any luvisol reference); the brunisols oligo-saturés read as dystriques, the histosols recouverts and the humiferous andosols of 1995 as any reference of their group. Entries of `readings.yaml` record them.

# soilKey 0.9.353 (2026-11-04)

## France: the humus forms of the Référentiel pédologique 2008 (Annex 1)

The new `humus_form_rp()` attaches the épisolum of a profile to a humus form of Annex 1 of the RP 2008 (pp. 327-355), and `classify_rp2008()` reports it beside the name: the new field `$humus_form` (one row: the form, its name, type, group, the route, the candidates, the recorded form and any conflict, the qualifiers) and `$trace$humus_form` (the whole reading). The "à <form>" part of the name waits for the name builder of a later part, so the name, the levels and the candidates of the references do not change. Every other classification is unchanged (the fixtures of the other systems byte-identical).

* **The order of the reading** (Hugo's decision Q22; `inst/rules/rp2008/humus_forms.yaml`). H horizons at the top leave the épisolum to the histosols, with no form (p. 351); an OHta makes a tangel (p. 349) and an An as the first mineral horizon an anmoor (p. 351), the codes the references read (organosols à tangel, réductisols à anmoor); O horizons directly over an X, R or D give the forms without fine mineral earth, the type read on the O horizons alone (peyromull, lithomoder, peyromoder, lithomor, peyromor, peyro-amphimus; pp. 348-349); the land use or a ploughed L gives the open-land forms (agri-mull, mull de pelouse, agri-moder, moder de pelouse, agri-mor, mor de pelouse, amphimus de pelouse; p. 337); then the key of Figure 2 (p. 354), walked by `.walk_key()` on the O and A horizons to its eleven outcomes (eumull, mésomull, oligomull, dysmull, hémimoder, eumoder, dysmoder, hémimor, humimor, mor, euamphimus); a G, AG or g under the O horizons makes the outcome hydromorphic (hydromull, hydromoder, hydromor; p. 350).
* **Three-valued.** The key reads the kind of the O horizons (the notation, or `fine_organic_matter_pct`), their zoogenic character (zo, noz, or `o_zoogenic`), their lateral cover (`lateral_cover_pct`, 90 % or more continuous, or the parentheses of the RP notation: an O horizon written in parentheses is now read as that horizon, discontinuous, p. 340), the thickness of the OH and the biological structure of the A. What is not recorded leaves the form undecided among its candidates (when they share a type, the type is decided); with no O horizon recorded the form is undecided.
* **The recorded form** (Q22). A recorded `site$humus_form` (the forms and types of the annex, accents and case free) decides when the horizons leave the form undecided, a type keeping the candidates of that type; outside the forms the horizons leave possible it still decides, and the conflict is reported; when the horizons decide another form they prevail, the conflict reported. The schema lists the four forms de pelouse.
* **The qualifiers of the humus forms** (pp. 338-340), read on the first A: the pH and S/CEC classes (half-open, a neighbouring class within the tolerance of p. 64 listed as imperfect), carbonatée, the Ca/Mg classes on a saturated or subsaturated non-carbonated A (hyper-magnésique under 1/3), sodisée, lignique and xylique by the woody debris of the O horizons. They are listed beside the form, not yet in the name.
* `print()` of a result of the RP shows the humus form, and `key_trace_table()` gives it one row of information.
* **An O horizon written in parentheses** ("(OLn)", "(OHzo)": discontinuous, p. 340) was an unread designation; it is now read as its code. This also reaches the references: such a profile is now recognised as written in the RP notation, and an "(OLn)" litter is left out of the depths as an OLn is (p. 64), so "(OLn)/A/R" with the R at 12 cm is a lithosol, as "OLn/A/R" was, instead of "[non rattaché]". Other designations in parentheses, as the "(B)" of the CPCS, are read as before.

### Readings confirmed, and the review (2026-11-04)

Hugo confirmed the eight readings of this release (the order of the reading, no O horizon recorded, the continuity of the O horizons, the wet épisolum, the open land, the forms without fine earth, the recorded form, the qualifiers). An independent review then found the key itself right and fourteen defects around it, all fixed: a form is no longer decided when the horizons may also fit no form (a dead end of the key, a gate with no form after an undecided one, a top layer that may be an H), the new `no_form_possible` saying so; the recorded form no longer stands in for the biological structure of the A inside the key; the wet test reads the layer under the organo-mineral one for a mor only; the forms without fine earth keep every type an undecided OH or an undecided R, D or X leaves possible; the amphimus de pelouse needs an OHzo; the land uses are read as whole words, the present use first (a cleared forest now grazed is grassland); a qualifier that needs a non-carbonated A is undecided while the carbonates are; the continuity of an O of open kind is undecided. Hugo decided its four open points: a continuous OLn gives a mésomull as the key does, with a note on the early-winter eumull (p. 342); only continuous OH horizons count toward the 1 cm of the dysmoder (p. 344); the label of a decided type is the name of the type; `woody_fragments_pct` stands in for the woody debris, flagged. Twelve entries of `readings.yaml` record them.

# soilKey 0.9.352 (2026-11-04)

## France: the Référentiel pédologique 2008 attaches its last 18 references

`classify_rp2008()` now tests the 110 references of the 34 groups of references (GER): the ninety-two of 0.9.346 to 0.9.351 and the six chapters that close the book, pp. 291-326: régosols (1), salisols et sodisols (7), thalassosols (3), thiosols et sulfatosols (2), veracrisols (1) and vertisols (4), in `inst/rules/rp2008/references/<ger>.yaml`. They are plugged into the attachment of 0.9.346 as parts 5 to 7 were; their signposts leave `key.yaml`, which no longer holds any, and no double or precedence waits for a later release. Every other classification is unchanged (the fixtures of the other systems byte-identical).

* **Régosols** (pp. 291-293). Within 10 cm (+/- 5; the OL not counted) an M or D layer or a C horizon under one organo-mineral or holorganic horizon at most, the holorganic O counting as one, read as the lithosols are (`lithic_obstacle_rp(window = TRUE)`: an undecided layer deeper than the window is no obstacle); the hardness from the horizon frame (the notation, the rupture resistance, soilKey's `continuous_rock()`; not recorded, undecided). H of 10 cm or less over M, D or C is the régosol épihistique (p. 209); the young alluvial and marine deposits, the fluviosols bruts and juvéniles and the thalassosols bruts and juvéniles, take the name before it (p. 291), and an A over 10 cm makes the arénosol (pp. 101, 292).
* **Salisols et sodisols** (pp. 294-302). The salic horizon within 60 cm (+/- 10), chloruro-sulfatés in the neutral series and carbonatés in the alkaline one (`salic_series_rp()`: the anions of the salts, else the pH in water standing in for the paste extract, Q12); the sodic horizon within 60 cm with no salic one: indifférenciés with no E and no BT, solonetziques an E over the NaBT, solodisés an Ea or an E at pH 5 or less (+/- 0.2) over it; the sodisalisols and salisodisols by the order of the two horizons (`superposed_rp()`). A code written with its Na or Sa (NaBT, SaA) needs the overlay. With a G or g within 50 cm the doubles with the réductisols and rédoxisols (pp. 298-299); too sandy for a paste, an arénosol salin (exclusions).
* **Thalassosols** (pp. 303-306). A marine or fluvio-marine material recorded (`marine_rp()`, Q14; such a material is no longer an alluvium of the fluviosols), the closed list of horizons of p. 304, no TH, U, salic or sodic horizon within 60 cm; bruts, juvéniles and poldérisés by the horizons, the site confirming (`tidal_site_rp()`: a polder fails the bruts and juvéniles, a tidal flooding the poldérisés, `artificially_drained` stands in for `poldered`; not recorded, provisional). A G within 50 cm makes the double réductisol-thalassosol, a g the rédoxisol-thalassosol poldérisé (pp. 285, 305; Q8).
* **Thiosols et sulfatosols** (pp. 307-312). A TH within 50 cm (+/- 10) and no U, its consistency n over 1.4 and its pH after drying under 3.5 tested when recorded (`maturation_rp()`, `acidification_rp()`); a U within 50 cm with a TH directly under it (`sulfidic_under_rp()`) and n under 1.4. Sulfur at a shallow depth attaches the solum first to them, in every case (p. 310): a salisol with their characters is a sulfatosol or thiosol salique.
* **Veracrisols** (pp. 313-316). The vermihumic Ah (`vermihumic_rp()`): written Ah, or the dark A layers of the surface run on the data (moist value 3 or less, chroma 2 or less), biomacrostructured; under heath over 50 cm (+/- 10), S/CEC under 30 % and Al/S over 1; under culture an Ah under the LAh or LA with its base deeper than 50 cm; Ah of 30 to 50 cm the veracrisol leptique, imperfect (p. 315); with an alocrisol humique the double of p. 316. The climate is reported, not tested.
* **Vertisols** (pp. 317-326). An SV over a V starting within 100 cm (strict), 40 % clay or more in every SV and V, smectitic when the mineralogy is recorded; in the order of Q20 the paravertisols planosoliques (an E or Eg directly over the SV, abruptly bounded), hapliques (an A under 40 % clay directly over the SV), then the litho- and topovertisols with a vertic surface (`vertic_surface_rp()`: on the data the characters of the Av read on the surface A), told apart by the site, else by the SV (`vertisol_site_rp()`). The vertisols take the name before a salisol or sodisol (VSa, VNa) and a planosol; the doubles with the réductisols and rédoxisols of p. 325 are written.
* **The rows that waited** (pp. 253, 282, 285): the peyrosols over 50 cm and a G within 50 cm now rank before the régosols, salisols, sodisols, thalassosols, veracrisols and vertisols as before every other reference, the thiosols and sulfatosols kept first; with E, BT, BP or Sp beside the redox features the other reference takes the name before the rédoxisol, the doubles still written.
* `make_rp2008_example()` gives the 18 new profiles (the chalk of Champagne, a sebkha of Tunisia, the natron of the Kanem, the puszta, the prairies of Alberta and the Riverina, the bay of the Mont-Saint-Michel and the Marais poitevin, the mangroves of the Casamance, the touyas of Béarn, the marls of the Gâtinais, the basalts of Uruguay and the clay plains of the north of Cameroon), and the Pro app offers them. Each of the 110 examples attaches simply to its own reference, and the test of this part keeps every candidate of each as a snapshot.

### The review, and the readings confirmed (2026-11-04)

Hugo confirmed the eleven readings of this part as recommended, amended by his decisions on its independent review, and the defects it found are fixed:

* **VSa and VNa** are the salic and sodic horizons too (the overlay beside the feature; on the data a V with Na over 15 % of the CEC is a VNa), so the salisols and sodisols are among the candidates of such a vertisol (Q20). A written A, LA or SaA over the SV reads its vertic characters on the data, undecided when they are not recorded; a dark, humic SV under the A run stays an SV candidate; a written SV or V carries its clay, tested only when recorded (decision 7).
* **The sodic horizon** known by its ESP only (the Na derived with the CEC, or an ESP of 15 % or more as evidence) counts a massive structure. The solodisés need an Ea written, or an E at pH 5 or less (`solodic_e_rp()`: a written E with no pH is none, on the data undecided); an A directly on the NaBT is the sodisol solonetzique imperfect, and a sodic BT names the sodisol before a luvisol; the indifférenciés need no E or BT within 60 cm.
* **The régosols** count adjacent layers of one code as one horizon (A1/A2; the lithosols too) and exclude a recorded recent alluvium (`alluvial_rp()`).
* **The thalassosols** forbid R, D, X and Z only within the solum (100 cm); a polder, reclaimed, embanked or former tidal land is read before the tidal words, and the marine words are read as words (not submarine, not non-tidal). A provisional thalassosol or vertisol says why.
* **The veracrisols** whose description ends in the Ah are undecided; a vermihumic Ah of 30 cm or more names the veracrisol before a luvisol tronqué (decision 1).
* **Hugo's decisions**: an unsaturated SV makes the lithovertisol even in a low position, and with no site recorded the litho- or topovertisol is provisional (2); a U with no TH under it is the sulfatosol imperfect, before the réductisol (3); only the réductisols typiques make the double with the thalassosols (4); the salsodic doubles need a réductisol that holds strictly (5); the thiosols and sulfatosols come before the peyrosols (6). Seven entries of `readings.yaml` record them.
* The example fluviosol brut no longer lists the régosol among its candidates (the snapshot of the test updated).

# soilKey 0.9.351 (2026-11-04)

## France: the Référentiel pédologique 2008 attaches 21 more references

`classify_rp2008()` now tests the 92 references of twenty-eight groups of references (GER): the twenty-one of 0.9.346 to 0.9.349 and the seven chapters that follow in the book, pp. 243-290: pélosols (3), peyrosols (1), phæosols (2), planosols (3), podzosols (8), rankosols (1) and réductisols et rédoxisols (3), in `inst/rules/rp2008/references/<ger>.yaml`. They are plugged into the attachment of 0.9.346 as parts 5 and 6 were; their signposts leave `key.yaml`. The 18 references of the other 6 GER (régosols, salisols et sodisols, thalassosols, thiosols et sulfatosols, veracrisols, vertisols) come later. Every other classification is unchanged (the fixtures of the other systems byte-identical).

* **Pélosols** (pp. 243-248). The Sp over 30 cm (+/- 5), starting within 40 cm for the three references (D6, strict), never acid: a pH in water of 5.0 or more (+/- 0.2) in every Sp layer (D5); no V or SV. Typiques the Sp under the A or ploughed at the surface, brunifiés under a thin S, différenciés under an Ae and E or an LE under 30 % clay and 30 cm thick at most (else a planosol). On the data a prismatic over-structure alone, with no slickensides or wedge-shaped aggregates, over a layer that meets the Sp is the Sp, not an SV. A pélosol typique with redox features from under 50 cm is the double pélosol typique-rédoxisol of p. 285; a différencié includes them, no double.
* **Peyrosols** (pp. 249-254). Peyric horizons from the surface over more than 50 cm (+/- 10), or 10 to 50 cm directly on M, R or D (leptique) (`peyric_rp()`): an X (by weight, Q11), or a horizon of the notation with its coarse elements recorded over 60 % by weight. Over 50 cm the peyrosol takes the name whatever the context (p. 253), in the doubles the book names: an alocrisol typique, the anthroposols artificiels and reconstitués, the andosols, a colluviosol, a fluviosol brut, the podzosols (the peyrosol-podzosol meuble of p. 252), the fersialsols, the réductisols, a rankosol. A peyric X of the data whose fine earth holds an E, BP, FS or G serves those horizons' tests in the run from the surface.
* **Phæosols** (pp. 255-258). An Aso of 30 cm or more (D8), its S/CEC 50 to 80 % under permanent vegetation read on its layers (over 80 undecided against the chernosols, D9; under 50 the phæosol dystrique, imperfect), over an Sh or BTh: written with h, or on the data over 0.6 % organic carbon and over 80 % S/CEC (`humic_under_rp()`); an E under 5 cm only, no Km. Hapliques with an index under 1.3, mélanoluviques 1.3 to 3 (gradual) or 1.3 to 2 (abrupt), read against the adjacent Sh or BTh only (`idt_rp(adjacent = TRUE)`). With an Aso of 30 cm or more the phæosol, dystrique or within the tolerance, takes the name before a brunisol. The climate is reported, not tested (D10); the solums the book places in no reference (an Aso over an E of 5 to 15 cm over a BT; an index of 2 to 3 with an abrupt transition) are reported in `$trace$book_gaps`.
* **Planosols** (pp. 259-264). The four characters of the textural planosols (`planosol_rp()`): a clay difference of 20 points (D1), the least clayey E 30 % at most, the most clayey horizon 25 % at least, the abrupt change within 8 cm (under 20 % more than the double, from 20 % 20 points more, D2) on the lower boundary of the E, a subhorizontal contact; the Eg, [Ae + E + Eg] of 15 cm, the floor within 120 cm, no Eg on a BP. Typiques the Eg within 50 cm, distaux at 50 cm or deeper (D4), structuraux over a compacted or indurated floor; the truncated typiques of p. 263 imperfect, tronqué (D3). They take the name before the rédoxisols (no double, p. 263) and before very planosolic luvisols dégradés or derniques (p. 227); the pélosols différenciés before them (p. 244).
* **Podzosols** (pp. 265-275). The BP of the frame directly under some E or the A (`below_rp(layer = "any")`: the matcher reads a slot as a run of one code), cemented (written with c, recorded, or an m of another notation on the data, Bhsm) or loose (`cemented_rp()`), the humus no mull (`podzolic_humus_rp()`: an A at pH 5.0 or more with no O recorded leaves it undecided), the placic Femp directly above or inside the BP (`placic_rp()`); ocriques (no E; a mor directly on the BP imperfect, as an A over a cemented BP), meubles and duriques (E), humiques and humo-duriques (Eh), éluviques (an E on C or R, the BP downslope recorded, `lateral_bp_downslope`, Q14: else a candidate), placiques, post-podzosols (a ploughed layer on the BP, or LE/E/BP; on the data L/E/BP undecided), in soilKey's order of D11. E and BP over 10 cm under an Aho make any podzosol, the organosol a candidate (p. 241).
* **Rankosols** (pp. 276-278). (O)/A/R: [O+A] over 10 and under 35 cm on a non-carbonated R, no C, M or D, coarse elements over 25 % and under 60 % by weight in the A (`coarse_band_rp()`; not recorded, the rankosol undecided), organic carbon under 8 % (+/- 2), nothing calcareous, calcic or magnesic, no E, BT, S or g over 5 cm, a Sal under 10 cm (p. 73), a BP under 10 cm: 5 cm or less the rankosol, over 5 the double rankosol-podzosol ocrique (D14); 60 % or more the double rankosol-peyrosol of p. 277 (D15).
* **Réductisols et rédoxisols** (pp. 279-290). Functional reductic or redox features (g over 5 %, Q17; a relict one recorded is not functional, Q13; a drained site keeps the attachment) starting within 50 cm and continuing over 50 cm (`hydromorphy_rp()`); a G within 50 cm makes a réductisol whatever lies above (default 20); stagnique with an R or M under the G or a perched water table recorded, typique with the G to the base or with a C or D under it and a phreatic water table recorded (`reductic_base_rp()`); the redox g or -g alone a rédoxisol. The depth class of the hydromorphy is in `$trace$hydromorphy`.
* **The hydromorphic doubles** that earlier parts waited for are written (Q8, Q13): luvisol typique, dégradé or dernique-rédoxisol (pp. 226-230, 285), brunisol-rédoxisol with an aerated S over the Sg (p. 107), colluviosol-rédoxisol from 20 to 50 cm (p. 133), cryosol minéral-rédoxisol and -podzosol (pp. 141-142), the podzosols with the rédoxisols and réductisols (pp. 273, 285); a double with the rédoxisol only when it holds strictly. H over 50 cm over a G is the histosol, the réductisol a candidate (no double, the histosols réductiques of p. 209). In alluvium the rédoxisol fluvique takes the name and the double fluviosol-rédoxisol stays among the candidates (`status: candidate`, marked `double_possible`). The pairs with the thalassosols wait for them.
* `make_rp2008_example()` gives the 21 new profiles (the marls of Lorraine, a scree of the mont Ventoux, the loess of Uruguay, the Champagne humide, the Sologne and the boulbènes, the Vosges and the Landes, a granite of the Massif central, valley bottoms and a clayey colluvium), and the Pro app offers them.
* The precedence pairs and the doubles are read only among the references that may attach, and the order of the rapid key once per rules object: the 44 canonical fixtures classify as fast as with 0.9.350, the results identical.

### Readings confirmed, and the review (2026-11-04)

Hugo confirmed the twelve readings of this part, amended by his decisions on its review: the 50 cm of hydromorphic features, the base of the réductisols (and the substrate and water, decision 5), the doubles the book names with the rédoxisols and réductisols, the aerated S of the brunisol-rédoxisol, the doubles of the podzosols named by the GER, no histosol-réductisol (decision 2), the 8 cm of the planosols, the planosols tronqués, the pH and 40 cm of the pélosols, the humus and the post-podzosols, the rankosols (decision 7), the Sh and BTh of the phæosols. Nine entries record the decisions of the review.

# soilKey 0.9.350 (2026-11-04)

## France: the FE of the ferruginosols on the data

On the data route the FE horizon of the ferruginosols was read as a BT whenever a clay increase over the A let soilKey's `argic()` stand in for clay films that were not recorded, so ferruginous tropical profiles described without designations went to the fersialsols or the luvisols. On the notation route nothing changes.

* **FE against BT** (pp. 17, 170-171). In the intertropical domain the illuviation cutans separate the zones of clay illuviation from those of weathering, and the A over an FE is depleted of clay by sheet runoff. Recorded clay films decide: common or many a BT, even in a ferruginous context, none or few no BT; a BT mark of another notation (Bt, B2t) keeps the BT. With the clay films not recorded, a layer that holds FE is no BT at 5YR or redder (outside the BT colours of the ferruginosols, 10YR to 7.5YR) and the fixed order goes on (an FE, or an F tested before it); it stays undecided between FE and BT at 7.5YR, when the FE holds only within the tolerance of p. 64, or when the FE itself is undecided. A layer under an FE is a BT by `argic()` only with more clay than the FE directly above it: the BT is more clayey than the A, E, S or C of the solum (p. 17), and the increase of the FE over the depleted A is not its own (Hugo's decision 4); the BTss reads the BT above it the same way. With the domain not recorded nothing changes.
* **FS against a kaolinitic clay** (pp. 150, 171, 179). In the domain a CEC of 16 cmol+/kg of clay or less, the upper bound of the ferrallitic F, is no FS when no clay mineralogy is recorded (a recorded smectite or other 2:1 clay keeps the FS): the BT under an FE stays the BT of the ferruginosols instead of an FSt.
* **The field route of the FS** (pp. 171, 179; Hugo's decision 3, everywhere). With no FFE measured an FS needs an angular blocky (or cubic) structure: of strong grade it makes the FS, of moderate grade the FS imperfect (the fersialsol attaches as imperfect); a recorded subangular, prismatic, granular or massive structure, or a weak grade, is no FS by the field route, and the grade or the angularity not recorded leaves it undecided. With the FFE measured the structure is read as before.
* Outside the intertropical domain only the field route of the FS changes, and none of the probes of parts 4 to 6 moves. The four ferruginosol probes of part 6 now attach (meubles, semiluviques twice, luviques) and the book's sequences of pp. 173-174 written as data attach to their references; the tropical fersialsols, luvisols and ferrallitisols of the twins keep theirs. Every other classification is unchanged (the fixtures of the other systems byte-identical).

### Readings confirmed (2026-11-04)

Hugo confirmed the two readings of this release with the amendments of its review (the BT mark of another notation, the FE within the tolerance, the clay mineralogy before the CEC), and decided the field route of the FS and the clay increase of a BT under an FE; four entries of `readings.yaml` record them.

# soilKey 0.9.349 (2026-11-04)

## France: the Référentiel pédologique 2008 attaches 18 more references

`classify_rp2008()` now tests the 71 references of twenty-one groups of references (GER): the fifteen of 0.9.346 and 0.9.348 and the six chapters that follow in the book, pp. 201-242: histosols (5), leptismectisols (1), lithosols (1), luvisols (6), nitosols (1) and organosols (4), in `inst/rules/rp2008/references/<ger>.yaml`. They are plugged into the attachment of 0.9.346 (registry, precedence, doubles, rapid key) as part 5 was; their signposts leave `key.yaml`. The 39 references of the other 13 GER come later. Every other classification is unchanged (the fixtures of the other systems byte-identical).

* **Histosols** (pp. 201-216). Almost only H horizons (Hf, Hm, Hs, Ha, LH), the first within 50 cm (a cover is the qualifier recouvert), and under them only G, M, D or R, or thin limnic and terric materials (`histic_solum_rp()`; a C read as the M). The predominant horizon is the thickest kind of H, thicknesses cumulated, between 40 and 120 cm, or 0 and 50 cm over an M, D or R within 50 cm; a tie is no kind truly predominant, the Ha and LH count for no kind (D3); the 60 cm of the fibriques and 40 of the mésiques and sapriques are the thickness of that kind in the solum (D1, `histic_kind_rp()`); over 50 cm of H by the figure of p. 209, the composites the remainder; the leptiques 10 to 50 cm of H over M, D or R. H of 10 cm or less over R is a lithosol, over M, D or a C read as the M a régosol (a later release), under 50 cm over G a réductisol (a later release), with a permafrost within 2 m a cryosol histique (p. 211; `precedence.yaml`). The kinds on the data by the rubbed fibres, then von Post (D4); H against O by the designation, then the water (Q19): an organic layer the data leave between the two leaves both GER undecided. The habitat notes are reported, not tested (D18).
* **Leptismectisols** (pp. 217-218). Av over SV, [Av + SV] 50 cm or less, no V, every horizon 40 % clay or more, the complex saturated over 90 % by Ca and Mg (+/- 5, default 9), over R, Kc, Km or C; the smectites tested only when the clay mineralogy is recorded (`clay_mineral_rp()`). With no V read the leptismectisol takes the name before the signpost of the vertisols.
* **Lithosols** (pp. 219-220). An R, Km, Fem or Sim at 10 cm or less (10 to 15 cm imperfect), the OL not counted, or an OXc or OXm there in the intertropical domain (Q18), with at most one horizon over it, the holorganic O horizons counting as one (`lithic_obstacle_rp()`; Hugo's decisions 1 and 2: A-Sal-R with the R at 12 cm is an alocrisol typique, imperfect). A cuirasse or carapace of the tropical chapters stays a ferrallitisol, oxydisol or ferruginosol only when an F, OX, ND, RT, FE, Ea or BT lies under it within 120 cm (`under_rp()`; the ferruginosol pétroxydique tronqué of p. 175); without them it is a lithosol pétroxydique or duroxydique (decision 3).
* **Luvisols** (pp. 221-232). The texture contrast (`luvic_contrast_rp()`): the band set by the mean clay of the E (with no E, the ploughed LA or LE), against the most clayey BT: néoluvisols from 3 points, an index of 1.3 or 9 points more, to under the typiques' 8 points, index over 1.8 or 24 points (D11); in the band of 10 to 30 % and for the derniques the index is the lexicon's, the most clayey BT over the poorest E or Ae (p. 401; decision 4). When recorded, a BT of 5YR or redder or micro-polyhedral fails them (`siallitic_rp()`, p. 230; decision 6), and an h on the E or the BT bars them (p. 194; decision 7). Dégradés a BTd under the E; derniques an Ea directly on the BTd and the index over 1.8, named before the dégradés (D9); tronqués no E over 10 cm under forest, named before a typique or néoluvisol that also attaches (decision 8), an LBT or a ploughed horizon with no contrast left under crops (D12); quasi-luvisols an E over a II BT (`discontinuity_rp()`), the others read in the first material. The FFE over total iron of the BT over 0.20, measured, fails them (p. 230; `iron_release_rp(low = TRUE)`). The grisols come before the néoluvisols and luvisols typiques (p. 194). The doubles with the rédoxisols wait for them (Q8); a cryoturbated luvisol with a permafrost is a luvisol-cryosol (p. 141).
* **Nitosols** (pp. 233-237). The nitic Sn, under an A or a ploughed horizon; on the data its halloysite recorded decides, else Ki and Kr, else soilKey's `nitic_horizon()` stands in for the mineralogy as well, flagged (D14). The nitosol takes the name before the brunisols, whose S slot reads the Sn (p. 236), and yields to a ferrallitisol when an F lies with the Sn (the ferrallitisol meuble polylithique of p. 157). The chapter is a first version of the book (p. VI) and its climate is reported, not tested.
* **Organosols** (pp. 238-242). OF, OH, OHta and Aho over 10 cm (+/- 5), no H, thin organo-mineral horizons 10 cm or less in all and the organic ones at least twice as thick (`organic_dominance_rp()`); an S over 10 cm fails them, strict, and OF/OH over an S of 15 cm is a brunisol, imperfect, with no A (a new imperfect case of `brunisols.yaml`; decision 9). Holorganiques OF and/or OH and/or OHta and no A; calcaires an Acaho; saturés the Aho over 80 % S/CEC and insaturés under 80 % (no carbonated Acaho), the Aho read as a whole (p. 239), the insaturés with 20 cm of O and Aho from the surface and well drained to 50 cm. An S, an Sca or Sci or E and BP over 10 cm make a brunisol, calcosol or calcisol or podzosol humique; a Sal over 10 cm an alocrisol humique, under 10 an organosol insaturé, the other a candidate, and the double organosol insaturé-alocrisol humique only at 10 cm (decision 5); an Acaho with OF and OH over 40 cm an organosol before a rendosol (p. 241).
* `make_rp2008_example()` gives the 18 new profiles (peats of the Jura, the Beauce, the Causses, the loess of the Paris basin, Poland, Guadeloupe, the Jura and the Vosges), and the Pro app offers them.

### Readings confirmed, and the review (2026-11-04)

Hugo confirmed the nine readings of this part, amended by his decisions on its review: the lithosol obstacle at 10 cm or less, with one horizon over it; the tropical references before the lithosols only with their horizons under the cuirasse; the index of the lexicon in the middle band and for the derniques; the BT of 5YR or redder or micro-polyhedral, and an h on the E or the BT, barring the luvisols; the tronqués before the typiques; an S over 10 cm under the O barring the organosols; the Sal of the organosols and alocrisols humiques. Six entries record those decisions. An unread mineral designation (ASa, SSa) is no longer read as a possible organic horizon, and an O of undecided kind may be an OHta.

# soilKey 0.9.348 (2026-11-03)

## France: the Référentiel pédologique 2008 attaches 24 more references

`classify_rp2008()` now tests the 53 references of fifteen groups of references (GER): the nine of 0.9.346 and the six chapters that follow in the book, pp. 146-200: ferrallitisols et oxydisols (7), ferruginosols (4), fersialsols (4), fluviosols (4), grisols (3) and gypsosols (2), in `inst/rules/rp2008/references/<ger>.yaml`. They are plugged into the attachment of 0.9.346 (registry, precedence, doubles, rapid key) without changing it; their signposts leave `key.yaml`. The 57 references of the other 19 GER come later. Every other classification is unchanged (the fixtures of the other systems byte-identical).

* **Ferrallitisols et oxydisols** (pp. 146-168). Only in the intertropical domain (Q18, the new `tropical_rp()`: declared, a Köppen climate A or a latitude between the tropics; outside, they fail). The hardness precedence OXm > OXc > ND > meuble sets the reference (D3, in `precedence.yaml` with its pages); the thicker loose horizon within 120 cm, F or OX (their nodular forms included), makes a ferrallitisol or an oxydisol, neither a ferrallitisol (D4, `loose_rp()`); an OX with an OXc and no OXm is a ferrallitisol duroxydique, with an ND as well (A-OX-ND-OXc, the hardness precedence: Hugo's decision R2), since the book has no oxydisol duroxydique (D5). No FE, E or BT (pp. 162-163). The proximal (0-60 cm, the chapter, Q17) and distal (80-120 cm) types are reported with the depth of the first ND, OXm or OXc in `trace$position` and the warnings; between 60 and 80 cm the reference stays, with no type (D2).
* **Ferruginosols** (pp. 169-177). Intertropical; an FE, or an E or BT under an OXm or an OXc (the pétroxydiques accept the OXc, D10, D11); the meubles without an S are the imperfect case of p. 173, "ferruginosol meuble sans horizon S". The climate is reported, not tested.
* **Fersialsols** (pp. 178-182). An FS, its iron release read again at the reference (`iron_release_rp()`: FFE over total iron above 0.20 when measured, the free iron as evidence, the field route otherwise, D14); carbonated over the larger part of the FS thickness (`carbonated_rp()`; a local effervescence is not carbonated), calcic (not effervescent but locally, S/CEC over 80 %, Ca/Mg over 5), unsaturated (S/T under 80 %) or eluvic (an E over an FSt, IDT over 1.3). A saturated, non-effervescent FS with Ca/Mg 5 or less is a fersialsol calcique, imperfect, calcimagnésique or magnésique (D13); some FS carbonated but not the larger part, a calcique, imperfect, bathycarbonaté or recarbonaté (Hugo's decision D3). On the data the t of an FS needs common or many clay films; an FSt with no E is an éluvique, imperfect, tronqué (IDT from the A over 1.3), or a calcique or insaturé, imperfect, luvique (R1).
* **Fluviosols** (pp. 183-191). The alluvial origin is required (Q14, `alluvial_rp()`); the position in the valley and the water table are read when recorded (`valley_position`, `flood_frequency`, `groundwater_kind`, `groundwater_depth_cm`): a terrace or an alluvial fan fail, and the water table only when the flooding and the groundwater are both recorded as none (an embanked valley, or "none observed" in the pit, leave it not recorded: Hugo's decision D1); the French words lit mineur, lit majeur, terrasse, cône and marais are read, another position leaves it undecided; not recorded, the fluviosol attaches and is provisional, said in the warnings (`provisional_unless` in its file, "references:<code>" in `provisional_because`). No E, BT, BP, sodic, salic, sulfated or thionic horizon, no G or H within 50 cm. A structureless surface layer (single grain, massive) is no Js (p. 25) but the raw alluvium of the bruts, with no Jp under it, which the data now reach; a C stands for the M of the typiques (A-Jp-C). A g within 50 cm keeps the fluviosol in the name, provisional by the rédoxisols of a later release (Q8; the doubles fluviosol-rédoxisol and peyrosol-fluviosol brut wait in `doubles.yaml`). The bruts come before the régosols (p. 188), the colluviosols yield to them (p. 132), and they take the solum before the brunisols, calcosols, calcisols and arénosols by the rapid key (5a).
* **Grisols** (pp. 192-195). The Aso by the chapter (Q17, D15, `sombric_rp()`): value under 4.5 and chroma under 4, 4/3 excluded, not calcareous, S/CEC 50-80 % and pH 5.5-6.5 under permanent vegetation, the general definition of p. 13 in the strict pass only (met by the chapter alone, imperfect); cultivated (an L or the land use) the complex is not tested (D19); a written Aso with no colour still reads its recorded S/CEC, pH and carbonates. On the data a layer that continues the A run and reads E is the Eh when it is lighter than the layers above and below, with less clay than below and a weak structure; otherwise it stays undecided, its h possible. Clinohumic, S/CEC under 95 % in the Eh, BTh and BThd (not in a deep non-humic BT), the texture differentiation index 1.3 itself hapliques (S19). Still in soilKey's 7c (Q6), and before the chernosols and phaeosols (p. 194), so that the signpost of the phaeosols no longer makes a grisol provisional.
* **Gypsosols** (pp. 196-200). A Ys from the surface or under a Ym, its top within 10 +/- 5 cm (D18); a Yp makes no gypsosol (it gives "à horizon gypsique" to the other references, with the qualifiers).
* **The first version of the book.** The results of the ferrallitisols, oxydisols and ferruginosols say that the book calls these chapters a first version (p. VI, Q18; `first_draft` in `key.yaml`); `result_note` adds what a chapter reports without testing (the climates of the ferruginosols and gypsosols).
* `make_rp2008_example()` gives the 24 new profiles (in Côte d'Ivoire, New Caledonia, Burkina Faso, Provence, the Loire valley, Romania and Tunisia), and the Pro app offers them.

### Fixed

* **The parent material written last.** A last optional slot such as `(C|M|R)` now reads one optional slot per layer, the C, M and D in any order and the R last: the matcher reads a slot as a run of one code, so A-S-C-R was [non rattaché] in 0.9.346 and is now a brunisol, A-S-M-C too, Aca-Sca-C-R a calcosol.
* **The iron of the FS on the data** is read on one basis, the free iron as Fe against the total as Fe2O3 (Q23, as `fe_total_pct` documents it): FFE / (0.6994 x Fe2O3) above 0.20, free / (0.6994 x Fe2O3) above 0.50.

* **Timing.** The sequences are matched on the table already read, the completions of the undecided layers stop at the first sign of disagreement, the strict and tolerant passes share one match when no layer is read within tolerance, and the memberships of the frame are memoised: the 44 canonical fixtures classify in about the time of 0.9.346, every result unchanged.

### Readings confirmed (2026-11-03)

Hugo confirmed the eight readings of `readings.yaml` for this part (the intertropical domain of a profile written in the RP notation with no climate or latitude; F against OX, the hardness and the types; the sequences of the ferruginosols; the A, the FSt, the carbonates and the iron of the fersialsols; the origin, the site and the sequences of the fluviosols; the Aso, Eh, BTh, BThd and the index of the grisols; the crust of the gypsosols; the parent material written last), those of the fersialsols and fluviosols amended by his decisions D1, D2, D3 and R1, and three entries record his decisions D3, R1 and R2.

# soilKey 0.9.347 (2026-11-03)

## Russia: the species, and the 1977 analogue as a note

The Russian soil classification (Field Guide of 2008) now names its soils down to the species (вид) of chapter 5.1, after the subtypes, and gives the analogue of the type in the USSR classification of 1977 (chapter 6) as a note that never changes the classification (Hugo's decision Q1). The varieties and ranks of 5.2 and 5.3 are not keyed, and a technogenic surface formation still stops at trunk E.

* **The twelve tables of 5.1** (pp. 135-137), in the new `inst/rules/kidpr2008/species.yaml`: the thickness of the grey-humus and light-humus horizons (маломощные, среднемощные, мощные) and of the dark-humus horizon (мелкие to сверхмощные), of the peat horizon of a mineral soil, of the agro horizon; the depth and place of gleying (глубоко-, поверхностно-, профильно-оглеенные); the lower boundary of the bleached horizon; the layer over the solonetzic horizon (корковые to глубокие); the depth of the carbonates (карбонатные to сильновыщелоченные) and of the soluble salts (солончаковые to глубокозасоленные); and, in the organogenic trunk, the decomposition of the peat, its botanical composition and the thickness of the peat body. Each table says what it measures, on which horizon, for which types, and its class limits as numbers.
* **The species level.** Every table that applies to the type is read; the class that holds is a row of level `species` of `levels` (code `<table>:<class>`, the English gloss in `name`, the Russian in `name_native`), named after the subtypes in the order of the tables: "Sod-podzolic [typical], medium-thick, deep-eluvial", "Дерново-подзолистые [типичные], среднемощные, глубокоэлювиальные". The bleached species takes the prefix form of p. 136 with the term of its horizon (мелкоподзолистые for the E of a podzol, глубокоэлювиальные for an EL, неглубокоосолоделые in the solods); two dominant plants give the double name of the peat (древесно-осоковые). `qualifiers$species` and `species_native` list them, and the trace has every table read (phase `species`) with its measure, layers and evidence.
* **Three-valued, as intervals.** A depth not recorded, a layer whose horizon the data leave undecided at the edge of a horizon, or a horizon that reaches the bottom of the description when a deeper class is possible, leaves the species undecided: it is not named, it is listed in `qualifiers$species_undecided` with a note, and the name is not made provisional. A value at a limit is read by the owner's rule (the lower end of a range for a minimum, the upper end for a maximum).
* **The 1977 analogue** (pp. 139-161), in the new `inst/rules/kidpr2008/correlation_1977.yaml`: the 268 rows of the correlation table, each a 2004 type with its 2004 formula, the type of 2008 it stands for, whether the soils were distinguished in 1977, and the 1977 units it names (their short printed names, level and extent: full, approximately, partly, mostly), with soilKey's English paraphrase. 267 rows resolve to a type of 2008, each to a different one (one 2004 type has no type of 2008); 144 say the soils were not distinguished; eight types of 2008 have no row. `classify_kidpr2008()` adds the analogue of the assigned type as a line in `warnings` and as `qualifiers$analogue_1977`; the note says so where the soils were not distinguished or chapter 6 has no row, and a soil whose type is unassigned, or a chemozem, gets none. The report and the Pro app show it.
* **Two formulas of chapter 6** that the text layer had hidden (AY-AEL-BM-C on p. 143, printed letter-spaced, and P-TUR-C on p. 156, printed on the line of its name) join the fixture of the formulas: 268 rows of chapter 6, not 266.
* **A new horizon attribute**, `peat_botanical_composition` (the plants of the peat with their shares, in English or Russian), for the botanical species.
* `classify_kidpr2008()` gains `species` (default: as `subtypes`); `subtypes = FALSE` still stops at the type. New i18n keys `classify.level_species`, `classify.levels_species`, `classify.kidpr_1977`, `report.level_species`, `report.kidpr_1977` and `report.kidpr_1977_desc` (EN, PT).
* The other systems are unchanged (their fixtures byte-identical).

### Hugo's decisions (2026-11-03)

* **Gleying by its continuity:** surface-gleyed is gleying in the upper horizons (those starting above 50 cm) that does not continue below 50 cm; profile-gleyed is gleying from a horizon starting above 50 cm down to the bottom, only a humus or agro horizon that is not gleyed allowed over it (AU-G-CG and PU-G-CG are profile-gleyed); deep-gleyed is unchanged (a gleyed layer from 100-130 cm, nothing gleyed above 100 cm). A layer whose gleying is undecided is read both ways.
* **Depths from the mineral surface:** the depths of the species are measured under the organic horizons O and T that start the profile, since the litter-peat O does not count (p. 136); the species of the peat itself still measure the peat. A podzolic soil O 0-8 over EL 8-23 is shallow-eluvial.
* **A double botanical name puts the larger share last**, as Russian writes it: wood 45 with sedge 50 is древесно-осоковые.
* **The other readings are accepted:** the class limits (the lower end of a range for a minimum, the upper end for a maximum, the last carbonate class 80-120); what each table measures; an undecided species not named and the name not provisional; the rows of chapter 6 resolved to the types of 2008; the readings of its two columns. Transitions such as AUBCA stay out of the thickness of the dark-humus horizon. No reading of the Russian classification waits for confirmation.

### Fixed after the review (2026-11-03)

* Cotton-grass is no longer read as herb: "grass" after "cotton" is not the herb, and the spellings of one plant (cotton-grass, cottongrass, Eriophorum, пушица) add up, in a layer and across layers.
* A transition one of whose masters is a carbonate horizon (AUBCA) has carbonates: a chernozem AU 0-40 over AUBCA 40-70 is slightly leached, no longer medium leached.
* The bleached species needs only the lower boundary of its horizon: an EL whose top is not recorded no longer leaves it undecided. Whether a horizon reaches the bottom of the description is read on the depths, not on the order of the layers.
* The adjective мелкие, printed by two tables (the dark-humus horizon under 30 cm and the layer of 5-10 cm over the solonetzic horizon, p. 136), is named once; both rows stay in `levels`.
* The 1977 note keeps what the table adds to a unit (a new `qualifier` of the analogues): the varieties of CE03, the genus рихковые of CP10, the erosion of CS13, the reclaimed peat soils of raised and fen bogs of CS05 and CS06 (now the 1977 types торфяные болотные верховые and низинные).
* The PDF report escapes the marks of the formulas (~~, the backtick) in the 1977 section.

# soilKey 0.9.346 (2026-11-03)

## France: the Référentiel pédologique 2008 attaches its first 29 references

The French Référentiel pédologique 2008 (AFES; Baize and Girard coord.; Éditions Quæ, 2009) enters the registry as `rp2008` (namespace `rp`, aliases `rp` and `referentiel`; `france`, `french` and `fr` stay with the CPCS 1967), experimental and opt-in: `classify_all()` runs it only when asked. `classify_rp2008()` tests the 29 references of the first nine groups of references (GER): alocrisols (2), andosols (6), anthroposols (5), arénosols (1), brunisols (2), the solums dominated by calcium or magnesium (7: rendosols, calcosols, dolomitosols, rendisols, calcisols, magnésisols, calcarisols), chernosols (3), colluviosols (1) and cryosols (2). Every other classification is unchanged.

* **Not a key: an attachment.** Every reference is tested every time (Q3): its sequences of reference horizons, matched with `match_horizon_sequence()` on the horizon frame (the RP notation where written, else the horizons on the data of 0.9.343), and the side tests of its chapter (depths, thicknesses, forbidden horizons, saturation, carbonates, permafrost, colluvial and anthropic materials), in `inst/rules/rp2008/references/<ger>.yaml`, each with its page, its WRB 2006 name (Annex 5, display only) and its distinctions in the comments.
* **Degrees.** Each reference is evaluated strictly and with the tolerances of p. 64 (`tolerances.yaml`: depth and thickness 10 to 200 cm, pH 0.2, CEC 1, S/CEC 5, organic carbon 8 and 30; every other threshold strict, Q5): simple, imperfect (met only within tolerance, never promoted, Q4), simple or imperfect, undecided or failed. The imperfect cases the chapters name (calcosol leptique, rendosol pachique, arénosol rédoxique, anthroposol lithique, calcisol insaturé en surface ...) attach imperfectly, their qualifier written in the name ("calcosol leptique").
* **One main name and the candidates.** The precedence of the chapters first (`precedence.yaml`: a colluviosol only when no other reference attaches, a calcarisol before the other six of its GER, the arénosols before the rendosols, calcosols and dolomitosols, the perhydric silandosols first, the calcisol against the brunisol eutrique by the carbonate reserve of Q16 ...), then the degree, then the order of the rapid key (`rapid_key.yaml`, pp. 66-68; the grisols in soilKey's 7c), Q6. Doubles only for the pairs the book prescribes (`doubles.yaml`: calcisol-calcosol, anthroposol transformé-brunisol eutrique when the transformé is imperfect, calcosol-anthroposol transformé; the pairs with references of later releases listed and waiting), Q7. With no reference, "[non rattaché]" and the three nearest references. The new slot `candidates` holds every reference that may attach, with its degree, the criteria that fail, pass only within tolerance or are undecided, and the book's statements that rank it after the name (`precedence`). The name is French, as printed, in the singular, the English gloss in `levels$name_en`, the GER a label (`rsg_or_order`).
* **The other 25 GER.** Their 81 references come in later releases. `key.yaml` lists all 34 GER and 110 references, with a signpost for each GER not implemented (the horizons or materials of its chapter: BT for the luvisols, BP for the podzosols, G or g within 50 cm for the réductisols and rédoxisols ...): a profile that shows them gets the GER among the candidates, undecided and not implemented yet, and the name is provisional when it ranks first. No profile is attached elsewhere without saying so.
* `identify_horizons(pedon, "rp2008")` now reaches the RP identification; `make_rp2008_example()` gives one profile per reference, in the RP notation; the examples are in the Pro app.
* `load_rules()` reads the folders and files a registry entry names besides its levels (`rule_folders`, `rule_files`); no other system names any.

### Hugo's decisions read here (2026-11-02)

* Q1 (references and types, the GER a label), Q3 (one name and the candidates), Q4 (the tolerance band is imperfect), Q5 (the thresholds not in the table strict), Q6 (the book, the degree, the rapid key), Q7 (the doubles the book prescribes), Q14 (the colluvial origin required), Q15 (the anthroposols by their materials and the recorded history), Q16 (the carbonate reserve by a positive proof), and the defaults 1, 2, 9, 13 and 21.

### Hugo's answers on the readings (2026-11-03)

* The 17 readings of this release (`readings.yaml`) are accepted, revised by the review: the imperfect cases (D1), the open sequences (D2), the perhydric andosols (D2, D3), the artificiels and construits (R3), the arénosols (D9 and decision 3), the brunisols (D6), the carbonate reserve (D7), the calcium GER (D4, D9), the chernosols (D5, R2) and the cryosols (decision 1).

### After the review (2026-11-03)

* **D1.** A reference attached imperfectly within the tolerance of p. 64 is named by the imperfect case of its chapter whose test holds strictly: calcosol leptique at [A + Sca] 32 cm, rendosol pachique at Aca 38 cm, arénosol rédoxique with -g at 70 cm, arénosol leptique with clay at 110 cm, vitrandosol leptique at Avi 27 cm, anthroposol artificiel leptique at Z 45 cm, anthroposol reconstitué leptique at Ltp 40 cm.
* **D2, D3, R5.** The perhydric silandosols and aluandosols take the name only when the perhydric test holds; the aluandosols hapliques no longer test it; an And, Snd, Alu or Slu under an Avi makes no vitrandosol (an épivitrique silandosol); the leptique case of the andosols needs the andic set thinner than 30 cm.
* **D4.** The calcium and magnesium GER excludes a V or SV within 100 cm only (bathyvertique deeper, p. 116), and a Ys or Ym within 10 cm (a deeper Ys no longer excludes).
* **D5, R2.** The chernosols typiques forbid only a BT directly under the Ach, or a humic BTh; the mélanoluviques need over 0.6 % organic carbon in the first BT; a cultivated LAh or LAch counts for the 40 cm of Ach.
* **D6.** The brunisols read their forbidden horizons, and their sequence, in the first material (the first lithological unit of 50 cm or more): A-S-IISca-IIRca is a brunisol eutrique; under crops their sequence is LS-S; S/CEC over 50 % eutric and under 50 % dystric as printed, both imperfect at exactly 50 %, the eutrique named.
* **D7.** The carbonate reserve reads a closed vocabulary of calcareous rocks and materials, whole words, and not when denied (non-calcareous, decalcified, décarbonaté); a tuff is no longer a reserve.
* **D8.** A test on the first layer of a set is undecided when an undecided layer above it could belong to the set and would give another verdict.
* **D9.** The arénosols come before the rendosols, calcosols and dolomitosols only when the A is not biomacrostructured, and are excluded by them when it is; the calcarisols come first only when they attach strictly (a Km within tolerance: a calcosol).
* **R3, R4.** The artificiels need a technological material dominant (an earthy Ztr or a holorganic ZO makes none); the construits have a leptique case (30 to 50 cm), and a recorded construction keeps them first; the signpost of the rankosols lets a Sal under 10 cm through.
* The nearest references of a solum attached to none include the colluviosols when colluvial material is recorded; the print of an attachment and the report's provisional section speak of an attachment, not of a key (the report's footer, common to every system, waits for the name builder).
* Undecided layers that leave the alternatives of a reference undecided are completed with their candidates (up to 32 ways); a layer of 50 % carbonates and gypsum or more, or indurated, is mineral even with no carbon recorded (a calcrete Km).

### Hugo's decisions after the review (2026-11-03)

* **1.** A cryosol minéral is simple with the cryoturbation recorded, imperfect when the data show the permafrost within 200 cm and record no cryoturbation, and then takes the name before the brunisols and the other references of later entries of the rapid key.
* **2.** The double anthroposol transformé-brunisol eutrique is written only when the transformé is imperfect (the intermediate case of p. 91); a simple transformé takes the name and the brunisol stays among the candidates.
* **3.** An Ach, And, Alu, Avi, Aso or Sal in the profile excludes the arénosols, so that a texture not recorded no longer makes those names provisional.

### Hugo's decisions on the readings of the review (2026-11-03)

* The first material (a cover under 50 cm skipped); the completion of undecided layers in the sequences; a calcrete with no carbon recorded is mineral; the first layer of a set under an undecided one (D8); the colluviosols among the nearest references; the signposts of the rankosols (a thin Sal) and gypsosols (a Ys within 20 cm).
* The carbon of a layer holds over its whole depth: a single Ach of 0-45 cm with 3 % carbon is clinohumic (0.6 % or more below 40 cm). No reading of the Référentiel waits for confirmation.

# soilKey 0.9.345 (2026-11-02)

## Russia: the subtypes and the full name

The Russian soil classification (Field Guide of 2008) names its soils down to the subtype (подтип): each division has one list of subtypes in chapter 4, and a soil is named by its type and every subtype of the list that passes, in the order of the list, comma-separated and in the plural as printed: "Подзолистые глееватые, языковатые" (Hugo's decision Q16).

* **The 26 lists, 558 entries** (the chemozems CU have none), in the new `inst/rules/kidpr2008/subtypes.yaml`, transcribed in the book's order from pp. 79-133: AA 15, AB 10, BA 22, BB 12, BC 14, [DA] 19, CA 35, CB 30, CC 16, CD 24, CE 16, CF 17, CG 15, CH 27, CI 27, CJ 18, CK 16, CL 11, CM 22, CN 31, CO 22, CP 23, CQ 30, CR 30, CS 35, CT 21. Each entry has soilKey's ASCII key (g, ELf, hh for [hh], e_hf for [e-hf], akl, cryoturb for @, perm for the permafrost sign, TTal for TT~~, TTash for TT"), the index as printed, the Russian name, an English gloss, the hosts, the page and notes; the universal subtypes are marked.
* **The subtype level** after the type, read by the feature tests of 0.9.344 (data first, then the Russian notation, then the defaults): the subtypes are rows of level `subtype` in `levels` (as the Hungarian subtypes), the full name in English in `name` ("Podzolic gleyic, tongued") and in Russian in `qualifiers$name_native`; every entry, with its evidence, route and hosts, is in the trace (phase `subtype`).
* **Typical and provisional.** When every entry is FALSE the soil is of the typical subtype, soilKey's [типичные] ([typical]), which the lists leave out (p. 76); when some are undecided the name is provisional, the undecided entries in `provisional_because` and in a warning, the passing ones still named.
* **Not named again** when the formula of the type writes the feature: g in the gley types, q in the quasigley ones, ELg in CA03, BF in CB05, s and cs in the BCAs,cs of the solonetzes, the residual carbonates in a type written with Cca; a feature in parentheses (C(ca), Q(s)) is optional and does not count. A V or an S laid over a horizon (BCA+V, AU+S) names the soil vertic or saline. Where the list has ELg, g is read on the layers other than EL.
* **Single-type sentences.** The list decides where a feature applies; a sentence of 3.2 restricts it only when it names a single type: te in the oligotrophic peat soils, [akl] in the brown arid soils, crm in the podzolic soils, ct in the sod-podzolic soils, cf in the gleyzems, ocr in the peaty cryozems.
* **The carbonate profile** of CI, CJ and CQ to CT is one of a scale (default 19): nodules over pseudomycelium over diffuse carbonates, the form with the migrating carbonates of the humus horizon (AUlc-BCAmc, AUlc-BCAnc, AJlc-BCAnc) where lc holds. The double el of CI and CM is read by its place; [BHF-BAN]>3 of the volcanic soils is a test of its own (more than three elementary profiles buried under the surface soil).
* **Buried soils and thin deposits.** A soil keyed by its buried horizons under a deposit of 40 cm or less keeps them for its subtypes, and the deposit written as a horizon of 5-40 cm over a bracketed humus horizon is rh (RU, RY, RJ) or r (C~~); only where the formula writes the buried soil ([ABC], {ABC}) is it left out.
* **Reachability.** On profiles built from the formulas of each division, every entry is named somewhere except those its division cannot have: s and cs in CK (written in every formula) and x in the divisions of trunk C keyed after the chemozems (an X within 30 cm makes a chemozem); pb in CC, CE and CG and agr, ad, pa in the abrazems are reached only by the feature tests, not in a classified soil.
* **The book's own names** are reproduced: the sod-pale-podzolic gleyic soil and the podzolic soil with a podzol microprofile (p. 60), the podzolised podbur (p. 60), the contact-bleached podzol (p. 67), the dark-humus water-accumulative stratozem (p. 129), and the chemozem of p. 134 keeps its name.
* `classify_kidpr2008()` gains `subtypes = TRUE`; `FALSE` stops at the type (the tests of the keys and types use it). A soil whose type is unassigned gets no subtypes.
* On the 27 examples the classification takes about 5 s instead of 3; on the 44 canonical fixtures 5 s instead of 4. The other systems are unchanged (their fixtures byte-identical).

### Hugo's decisions (2026-11-02 and 2026-11-03)

Nine readings of the subtypes, accepted by Hugo and recorded in `readings.yaml` as his decisions, as revised after the review of the subtypes: no subtypes for a soil whose type is unassigned; the six single-type sentences; what counts as written in the formula (the horizon that is the feature in full: G, Q, V, ML, S, F, CR; Cca keeps the residual-carbonate subtype out of the solods; an optional (ca) does not; a V or S overlay is v or s); the hosts the lists narrow (h, mr, Th, [e-hf], el of the weakly developed soils on W, hi of the agrozems under PT, ae of CJ on AJ, crm of the cryozems on or over CR, ox, fn and ff of the eluvial soils on their D or EL); a buried soil does not name the soil over it where the formula writes it, and a thin deposit over a buried soil is r or rh; the misprints corrected and the glosses kept; the layered-ochric soils (buried profiles only); the subtypes their division cannot have (pb in CC, CE and CG; agr, ad and pa in the abrazems; s and cs in CK; x in trunk C after the chemozems; the contact-eluviated podzol of p. 67, since the CB list has no el; no v for CH04 with a V on its G; a solonchak under a small deposit loses its division in the key); and the form of the full name. A tenth, of 2026-11-03: where the list has ELg, g is read on the layers other than EL (gleying in EL alone is ELg alone, in BT too both). No reading of the Russian classification waits for confirmation.

The report's note on the undecided classes now covers the undecided subtypes too, and `print()` no longer lists the Russian full name and the formula of the type among the qualifiers.

# soilKey 0.9.344 (2026-11-02)

## Russia: the tests of the genetic features

The Russian soil classification (Field Guide of 2008) gains the tests of its genetic features (признаки, section 3.2 and the subtype lists of chapter 4): one test per feature, shared by every division that lists it, for the subtypes that follow (the 26 lists and the full name come next). Nothing is named yet, so every classification is unchanged.

* **67 tests, the 73 index forms.** One internal function per feature of `features.yaml` (the 64 of 3.2 and w, x, rx of the lists); the aliases of the book (tu, ko, ag, as, ael, hh, akl) and the host forms of the lists (ELf, ELg, Th, Oh, AUh, AUel, Gel, Pw, PUw, TEmr, Tote, Di, Del, AJ[akl], and the pairs AUlc-BCAmc, AUlc-BCAnc, AJlc-BCAnc, which need both parts) are read through them. Each test is three-valued and gives its evidence and the attributes that would decide it.
* **Data first, then the notation, then the defaults.** A feature is read on the recorded data where an attribute records what it describes (the proxies of Q17: g, q, v, s, cs, nn, i, the columnar sn, BH and BF by the carbon of the BHF, an by the oxalates, the sapric h, lc, mc, nc and dc by the form of the carbonates, ml, the permafrost sign, @, tr, y, [hh], ro, r, rh, ur, rr), and the key to horizons gives the small horizons and the features it ends in (ao, e, el, r, rh, tr, u, ca). Where the data say nothing, the Russian notation decides: a feature not written on a layer of a profile written in it is absent; a letter of another notation can add one (an FAO Bg suggests g). Where neither does, a field observation not recorded is absent and a measurement not recorded leaves the feature undecided. hi, pl, crm, f, el on a horizon, ic, agr, ad, pr, ff, fn, ocr and yu come from the notation only (Q17). Recorded data that contradict the notation decide, with a note.
* **Hosts and places.** A feature holds on the horizons the text puts it on, with the analogies of Q17 (pl in CRM, cf at the edge of CR) and the horizons of the lists that name it; a host the data leave undecided leaves a passing feature undecided, and with no host the feature is absent. The double el of CI and CM is read by its place: in the AU, solodised (осолоделые); under the AU, eluviated (элювиированные).
* **x, rx and ur (Q17).** x is an X horizon (pollutants at the extremely hazardous level within 30 cm) in a soil the key did not send to the chemozems; rx a toxic layer of 5-40 cm over a buried humus horizon; ur the same layer with 20 % or more of artefacts. pb is read in the agro horizon with over 1.5 % humus, and in the light-humus carbonate soils (CJ) on a natural soil with deflation or truncation recorded (Q19).
* **The dispatcher** (internal) runs a list of (feature, host, place) entries on a pedon and returns one row per entry: the feature, its code, the host, the place, the layers, the verdict, the route (data, notation or both), the evidence and what is missing. Its metadata are in the new `inst/rules/kidpr2008/feature_tests.yaml` (the test, its route, the hosts, the place, whether it is a field observation or a measurement, the attributes it reads); `features.yaml` keeps the hosts the text gives.
* On the 27 examples every feature is decided; on the 44 canonical fixtures of the other systems 2,849 of 2,948 feature tests are decided, the undecided ones mostly the salts and the permafrost, which those profiles do not record.

### Hugo's decisions (2026-11-02)

* **Features read on the data where Q17 is silent:** te (high-moor over a thinner low-moor peat within 50 cm), e (also bleached spots recorded in a humus or middle horizon), el on the data only as the band of key 52a, m (a weak pedogenic structure in the top C that is not sand), tk (a clay crust with polygonal cracks), ox (redox features of 50 % or more in G), ek (a whitish band of 5 cm or less over a heavier lithological layer or D), gr (a granular BFM), d (a platy surface layer of a natural soil), pa (abandoned or fallow land on a ploughed horizon), md (a sapric surface layer over the peat), pir (a pyrogenic layer of over 2 cm on the peat), aq and ae (the origin of the stratified horizon), TT~~ and TT" (alluvial or ash bands in the peat), w (a W over the host) and ca (also local effervescence with no carbonate neoformation).
* **From the notation only:** mr, [e-hf], [akl], cf and ct, like the features of Q17's list; in a profile not written in the Russian notation these features are absent, not undecided.
* **Hosts and places:** the hosts of 3.2 and of the lists that name a feature elsewhere (f, i, crm, fn, hi, ic, sn, pa, e, w on the horizon under the W); a small horizon counts for the host beside it; AUel is el in the AU, the eluviated el is el under the AU, Gel el on G; an undecided host leaves a passing feature undecided, no host makes it absent.
* **Deposited layers and cut-points:** r, rh, ur, rr, rx need 5-40 cm over a buried humus horizon; ur 20 % to under half of artefacts, rr half or more and not toxic, rx a hazard above permissible; ro every mineral horizon 5YR or redder; cs visible gypsum (CaSO4 0 % none); s by the EC of chapter 9 (ECe 4, EC 1:5 0.2 dS/m); ml hard concretions; g not on olive mottles alone, q olive mottles of 5Y chroma 3-4; [hh] darker by a value and richer in carbon than the layer above; y with the retic or albeluvic stand-in, flagged.
* The four readings are in `readings.yaml` as his decisions; no reading of the Russian classification waits for confirmation.

### After the review (2026-11-02)

* **Fixes.** y only under an eluvial horizon (E, EL, AEL, BEL), dark tongues from a humus horizon being yu; ca not where any form of secondary carbonates is recorded, nor on an FAO k layer, and the FAO Ck no longer suggests ca; v and sn not ruled out by a structure alone (the notation decides), q not by mottles of other colours; pa ruled out only by current tillage on an agro horizon; ro leaves out the organic and peat layers by their horizon and reads the gley, blue, purple and neutral hues as not red, RP as red; x ruled out by a hazard category recorded within 30 cm, and written only on a layer starting within 30 cm; g not on mottles recorded as "none"; m undecided when the texture of the rock is not recorded, a weak no when the grade is not; d read on the first mineral layer under an organic one.
* **The salts:** over 1 % in the top 20 cm makes the salt horizon S and no feature s in the profile; ECe decides over EC 1:5 where both are recorded.
* **Limits of the definitions:** te may be a small horizon; crm only over BT; i on the parent rock only at its top; nn 10-20 %; a crust layer of tk 0.5 cm or less; d not on a fine platy structure; the muck band of h on top of a humus horizon only on the AU of the dark-humus gley soils; ml in the middle or lower profile only; @ reads yes and TRUE; [e-hf] in or beside EL (the example of p. 60, O-[e-hf]-EL-BEL-BT-C).
* The dispatcher stops on an unknown host or place and returns an empty table for no entries.

### Hugo's decisions after the review (2026-11-02)

* **Accepted edits:** crm over BT only; [e-hf] in or beside EL.
* **New readings** (`readings.yaml`, "The feature tests after the review"): ca a field observation (effervescence not recorded is absent; an FAO k layer a weak no); m with 15 % clay or more as a loamy or clayey rock when no texture class is recorded; tk fails on a surface layer of 0.5-2 cm (a crust thicker than 0.5 cm); d fails on a fine or very fine platy structure; [hh] by the darker colour, confirmed by the carbon; q ruled out by mottles recorded as none, other colours only a weak no; sn never ruled out by the data; s ruled out for the whole profile by over 1 % salts within the top 20 cm; the muck band h on top of a humus horizon only on an AU in a profile with G.

# soilKey 0.9.343 (2026-11-02)

## France: the reference horizons of the Référentiel pédologique 2008 on the data

The Référentiel pédologique 2008 has no key to its reference horizons: they are concepts read with the whole sequence (p. XII). This release reads them on the recorded data, layer by layer, for profiles not written in the RP notation, and gives the horizon frame the references will be matched on. Nothing is classified yet, so every classification of the other systems is unchanged; the registry entry and the references follow.

* **A fixed order of tests** (internal, `.rp_identify()`), soilKey's, from the book's order of presentation (p. 6) and its statements of dominance: the layers and materials (R, M, D, P, TH, Z and its kinds, Mli, Mt); the holorganic horizons (H with Hf, Hm, Hs and Ha; O with OL, OFr, OFm, OHr, OHf and OHta); the coarse elements (ND, X with Xgr, Xc, Xp); G (Gr, Go, AG), which dominates every other character of the layer, so no SG or BTG; the indurated and accumulation horizons (U, K, Kc, Km, Si, Sim, Fe, Fem, Femp, Y, Ys, Yp, Ym, OXm, OXc); the surface A horizons (An, Ach, Aso, Aca, Ado, Aci, Amg, Alu, And, Avi, Av, Anod, Ae, A, Js); the subsurface ones (E, BP, BPh, BPs, BT, BTß, V, SV, Snd, Slu, F, Fox, Fhy, OX, RT, FE, Sn, FS, Sp, Sca, Sdo, Sci, Smg, Sal, S, Jp, C); then the features of every layer (g, ho, k, t, v, x, y, d, cn, j, cry, nod, c, b, ca, a, h of Scah) on the hosts the book lists, and the overlays L, Na and Sa (LA, NaBT, SaA, JZ). Every test is three-valued: the first that holds gives the code, the other codes that hold or stay undecided go to the candidates, and a layer the data leave open is undecided with the attributes that would decide it.
* **The frame** has the columns of `identify_horizons()` and the RP ones: the family of the code, the code written with its prefixes, features and Roman numeral (`composed`: LAch, NaBTg, II Scag), the kinds a code may be refined to (`variants`: H to Hf, Hm, Hs), the thresholds met only within the tolerance of p. 64 (`imperfect`), the WRB stand-ins and the other substitutes used, a lower grade where they were, the depths from the datum (`top_rp`, `bottom_rp`), the lithological discontinuity and the hints of other notations.
* **The notation and the data.** A layer written in the RP notation keeps its code; the order of tests runs on its data as well, and a different reading goes to the evidence (route "both" when they agree). A layer in the CPCS or FAO notation is read on the data, its designation narrowing the candidates through Annex 6 and the FAO letters.
* **The readers** (`R/rp-readers.R`): the moist colour, the carbon classes, the carbonates and effervescence (cold, hot, the molar CaCO3/MgCO3), S/CEC with its CEC method, the sodium over the sum of the bases, the coarse elements by weight with the volume band, the redox features (location, relict), the reduced matrix, the salts, the intertropical and andosol gates, ploughing, the depth datum (the top of the first layer that is not a litter OL, FAO Oi or CPCS A00) and the hydromorphy of a profile from its frame.
* **WRB stand-ins**, flagged and with a lower grade, where a field descriptor is missing: argic() for the clay coatings of BT, spodic() for BP, calcic() and petrocalcic() for K, vertic_horizon() and vertic_properties() for V and SV, nitic_horizon() for the pressure faces of Sn, ferralic() for F and OX, chernic() and mollic() for Ach, umbric_horizon() for Aso (never sombric()), albic() for E, stagnic_properties() for g, andic_properties() and vitric_properties() for the andosol domain, retic_properties() and albeluvic_glossae() for BTd, continuous_rock() for R, and some others.
* **New site field** `intertropical`: the site declared in or outside the intertropical domain, where F, OX, RT, ND, FE, OXc and OXm are tested (Q18).
* The identification is internal until the registry entry `rp2008` exists: `identify_horizons()` does not reach it yet.

### Hugo's decisions read here (2026-11-02)

* **The order of tests (Q10):** soilKey's order from the book's order of presentation; the first test that holds gives the code, the others go to the candidates; a missing field descriptor is read through a WRB diagnostic, flagged, never the faux amis of p. 392.
* **Coarse elements (Q11), methods (Q12), functional redox (Q13), the most specific text (Q17), the tropical domain (Q18), H against O (Q19)** and the defaults 3, 6, 7, 8, 10, 11, 14, 15, 17, 18 and 20, as the readers and `inst/rules/rp2008/readings.yaml` state them.
* **The new site field `intertropical` (Q18)** is kept: the site declared in or outside the intertropical domain, before the Köppen climate and the latitude.
* **The readings of the horizons on the data, accepted** (`inst/rules/rp2008/readings.yaml`): an undecided G leaves the layer undecided against the code read after it (G dominates, p. 280); the designation of another notation narrows the candidates and decides a single one left, never an H; a grey matrix is reductic only with a sign of water excess (Gr with 10 % redox features or less, Go with segregations at the voids); the holorganic layers by the loss on ignition, else carbon 30 +/- 5 %, and the kinds of O by the fine organic matter; the surface run of the A horizons and the A material (0.5 % carbon or more, not bleached), Js and Jp, Ae; Ach (value 3.5 or less, chroma 2 or less, carbon 3 % or more for every texture), Aso (the list of p. 13 as the test, the grisols of p. 193 as evidence, imperfect when only one holds) and An; the carbonates (a slight effervescence not generalised, no carbonate recorded at pH under 6.5 not carbonated) and the calcimagnesian variant of Aci and Sci, imperfect; the stand-in of K only below the surface and with more CaCO3 than every layer below; BT (the increase of the WRB argic horizon, clay films common or many), BTss, BTd and E on the data; BP (the oxalate, else spodic() under a bleached layer or with more carbon above); V, SV and Av; the intertropical horizons (F by the CEC of the clay, OX, RT, FE; OXm and OXc, not Fem, in the domain); Sn, FS, Sp, Sal and the andosol gate; the layers and materials (R, M, D, P, TH, Z, Mli, Mt); coarse elements not recorded are none; gypsum, silica and iron (Ys, Yp, Ym, Si, Sim, Fe, Fem, Femp); the overlays L, Na and Sa; the features.

# soilKey 0.9.342 (2026-11-02)

## Russia: the types of trunks A, B and D, and the name of the chemozems

The Russian soil classification (Field Guide of 2008) gains the types of the six divisions of the organogenic, synlithogenic and weakly developed trunks: 58 types, matched as the types of trunk C are (0.9.338, 0.9.339). The chemozems (CU), which have no types, are now named by their pollutants and the soil they were. Every division has its type level; the subtypes follow. Every other classification is unchanged.

* **The 58 types** in `inst/rules/kidpr2008/types/`: AA 5 (peat soils), AB 4 (peatozems), BA 18 (alluvial soils), BB 4 (volcanic soils), BC 10 (stratozems) and [DA] 17 (the weakly developed soils of trunk D, the division the book does not code). Each has soilKey's code in brackets ([AA01] ... [DA17], in the order of chapter 4, the natural types first), the Russian name as printed (two slips of the diagrams of the alluvial soils, глеевы and the second мергелистые, as chapter 6 writes them), an English gloss, the formula as the fixture of the formulas transcribes it, and the page.
* **What the formulas test (Q14).** The buried soil [ABC] names the stratozems on a buried soil and is tested, a buried profile starting within 100 cm (p. 128); the substrate D is not. The substrate names the types of trunk D and is tested: the marks of the loose rock (C= loamy and clayey, C.. sandy, C~~ alluvial, C" ash, C^^ aeolian) and the hard rock R, with its carbonate or gypsum mark where written (Rca, Rcs); the C of the solonchaks is not. The rock under the dry-peat soils (AA05) is tested: the hard rock R or loose material C (p. 43); a rock under the peat closes no other formula of the peat soils. The alluvial and ash marks of the parent material in BA and BB, and the buried profiles {ABC} of the volcanic soils, are not tested; the buried humus horizons of the alluvial soils are not in the formula (p. 124).
* **The peat of trunk A.** A peat horizon that runs across the line 50 cm under the top of the peat body is TT below that line (Q22): TO 0-120 cm is TO-TT (AA01), PT over T 25-125 cm is PT-TT (AB01); a peat horizon that starts 50 cm or more under the top of the body is TT (TO 0-50 over TO 50-120 is AA01). A T of the notation in the peat soils is TO or TE by the botany of its layer, undecided between them when the layer records none.
* **The substrate of trunk D.** The layer directly under the O or W names the type; a deeper rock is not tested (W-C..-R is DA04). On the data route a layer with no horizon under the O or W is the loose rock, and an undesignated layer that names a rock and records no texture is undecided between the R and the C types. A C whose mark is not written takes it from the data, the layer's own origin first and the site's parent material only when the layer records none: an alluvial, ash or aeolian origin (loess is not aeolian here), else the texture (sand and loamy sand are sandy, finer is loamy and clayey), undecided with no texture recorded; the legend's aeolian mark "" is the ^^ of the diagrams; a proluvial body (an alluvial fan) has no type. A rock R whose marks are not written, on the data route or a bare R of the notation, takes its carbonate mark from a carbonate rock named (marble included) or effervescence and its gypsum mark from a gypsum rock named; another rock named has neither; a rock not named leaves them undecided.
* **The stratozems.** Of several stratified horizons in the body, the thickest names the type and the others are its features (RY 20 over RU 30 is BC02); a tie leaves the type undecided.
* **The alluvial soils on the data route.** The surface humus layer the key to horizons reads as RU or RY over a buried humus horizon is, for the types, the humus horizon AU or AY of the floodplain, and a layer with no diagnostic horizon the alluvium: AU over alluvium over Ab over alluvium is BA02, as its twin in the notation. The fix is in the reading of the alluvial types; key 2.1 and the divisions are unchanged.
* **The BH of the ochric soils.** The notation reads BH and BF as BHF, so the BH of BB01 and BB03 is read as a BHF of either kind, as Q15 does for CR07 and CS10; without it the two types could not be reached.
* **The name of a chemozem.** The book divides the chemozems by the kind of pollution, keeping the name of the original soil (p. 134). The type level is now built: soilKey's code `[CU:<code of the original soil>]` and the names "Chemozems polluted with Cu, Ni, Co; original soil: Sod-podzolic" and "Хемоземы, загрязненные Cu, Ni, Co, по исходной почве: Дерново-подзолистые". The pollutants are those recorded in `pollutants` for the layers of the X horizon, split on commas, semicolons and "and"; the original soil is the profile keyed again with the X masked (the X overlays the notation reader parses, AY+Xpb as well as AY+X, taken out, a layer that is only X dropped, and the contamination records removed), down to its type, and is in the trace (phase `chemozem`, an assigned step). An original soil with no division is named "..., division not provided by the Field Guide of 2008"; a masked profile that is a chemozem again leaves the original soil undecided ([CU:?]). Pollutants not recorded are named so and asked in `$missing_data`; an original soil undecided at its type or division is named so and makes the name provisional (`original_soil:` entries in `provisional_because`). This changes a result of 0.9.337 to 0.9.339: a chemozem had the division as its name.
* **Contaminated soils outside trunk C.** The X horizon is no horizon of the profile for step 5 of the key to trunks, so a contaminated soil of trunk A, B or D keeps its division and type (W+X over C.. is DA04, not a chemozem; the subtype x will name it). This changes a division result of 0.9.337. The X horizon counts at C.3b also when it lies on a surface horizon the thickest-horizon rule leaves out (O 15 over AY+X 10 over EL-BT is a chemozem, [CU:CA01]); in 0.9.337 to 0.9.341 it was lost.
* **Doubles.** Two formulas match one profile only where the rock names what the other formula leaves out: O-Rca is DA07 with DA05 its intergrade, W-Rca DA08 with DA06, O-Rcs DA09 with DA05, W-Rcs DA10 with DA06; the more specific names the type, as decided for trunk C.
* Every chapter 4 formula of trunks A, B and D, written as a profile in the Russian notation, reaches its own division and its own type, decided. One undesignated profile per division reaches its type through the key to horizons on the data.
* **Profiles with a division and no type** under the literal Q13 and Q14 (`readings.yaml`): TJ-TT over G, H at the top of a peat body, RU or RY on the floodplain, AY over Q, AO directly over BAN, PU over RY, O or W over D.
* The tests of the oracle of the keys read only the trunk and the division as undecided: an undecided type, or the original soil of a chemozem, is no answer of keys 2.2 and 2.3.
* A tie of recorded thicknesses at the surface, which leaves a type undecided, no longer asks for the depths in `$missing_data`.

### Hugo's decisions (2026-11-02)

* **The name of a chemozem:** as designed: soilKey's code `[CU:<code of the original soil>]`, the pollutants as recorded, the original soil in the nominative after "по исходной почве:", "pollutants not recorded" when none are, and the name provisional when the original soil is undecided.
* **The rock under the dry peat (AA05):** by p. 43 the hard rock R or loose material C may lie under the TJ body: TJ-TT over C is AA05, as TJ-TT over R; the C closes no other formula of the peat soils (TO-TT over C is AA01).
* **After the review:** a peat horizon starting 50 cm or more under the top of the peat body is TT, as the data route reads it; in trunk D the substrate directly under the O or W names the type and a deeper R is not tested; of several stratified horizons in a stratozem the thickest names the type; in the peat soils an R under the peat body does not block AA01 to AA04 (TO-TT over R is AA01), only AA05 requires R or C under the TJ; the chemozems follow Q23, a contaminated soil of trunk A, B or D keeping its division and type; a bare R of the notation takes its marks from the data as a C does, and an undesignated layer that names a rock and records no texture is undecided between the R and the C types.
* **A named rock that is neither carbonate nor gypsum** (granite, sandstone ...) has neither mark: O over an R of granite with no effervescence recorded is DA05, decided.
* **The readings of this release, accepted** (`readings.yaml`): TT below the upper 50 cm of the peat body and a T of the notation as TO or TE by its botany; the substrate of the weakly developed soils (no horizon is loose rock, the mark from the origin or the texture, the legend's "" read as ^^, a proluvial body with no type, the marks of R from the rock recorded); the BH of BB01 and BB03 a BHF of either kind; the profiles the literal formulas leave without a type. Every reading in `readings.yaml` is again one of Hugo's decisions.

### After the review (2026-11-02)

* **Fixes.**
  * A grey matrix outside the bluish, greenish and neutral hues is reductic only with a positive dipyridyl test, recorded water saturation or a water table at the layer (or water above the surface), never the drainage class alone; an A is AG only in a bluish or greenish hue; Gr only with 5 % of redox features or less, 5-10 % with no location leaving Go and g undecided.
  * The A run at the surface continues only under a layer of A material, with more than 1 % carbon (a transition such as BA is no A designation); a layer that continues it is read by the subsurface tests too, and is undecided between the A and the subsurface code when both hold.
  * E: bleached is a moist value of 5 or more with chroma 2 or less; a lower chroma counts as lighter only at an equal or higher value; clay films common or many, or more clay than the layer above, exclude it; above a rock or a coarse layer with no fine earth only a bleached layer is an E.
  * Aso holds only on the list of p. 13 (the grisols reading of p. 193 is evidence), and only in a well aerated A (no functional redox features, no reduced matrix).
  * A BT that is also fersiallitic is FSt; a BT or FE with ferric nodules under 40 % is BTcn or FEcn, not Fe; -cn only on BT and FE, -j only on S, Sci and BT, on an angular (not subangular) structure.
  * TH never with jarosite or a pH under 4 (the sulfated U).
  * Jp under an A or LA in a young deposit as well as under a Js, before the S.
  * A G candidate is never narrowed away by a designation.
  * The andosol domain opens only on volcanic material recorded or on the andic or vitric properties holding; an Alox + 1/2 Feox under 2 % closes it.
  * Na only on a columnar, prismatic or coarse structure, or a massive one with 1 cmol+/kg of sodium or more; never undecided on a layer with no degraded structure.
  * Ach: the 3 % carbon in the top 10 cm of the A run, a lighter mineral layer below and a gradual decrease of the carbon, chernic() alone as the stand-in; the moist value stays 3.5 or less (the chapter, p. 126, Q17).
  * Ae not on a ploughed layer; M without a designation (a soft layer with no fine earth and its carbonates recorded, or an FAO Cr); OL in a profile written in capitals still the litter of the depth datum, and OF, OH as hints; a light layer under 1 % carbon under an O read as an E, not an A; a mineral designation with no data makes the layer mineral; OX with pH 5.5-6.5; Sci with pH over 6.5; a loss on ignition contradicted by the carbon flagged, and no A over 30 % carbon; an imperfectly drained site as a sign of water excess.

### Hugo's decisions after the review (2026-11-02)

* **The revised readings** (`inst/rules/rp2008/readings.yaml`): the designations narrowing the candidates (never a G away; BA is no A; OL, OF and OH in capitals; a mineral designation makes the layer mineral), the reductic matrix, the holorganic layers, the A run at the surface, Ach, Aso and An, BT and E, the intertropical horizons, Sn, FS, Sp, Sal and the andosol gate, the layers and materials, gypsum, silica and iron, the overlays and the features, as the fixes above read them.
* **soilKey's additions, accepted:** Aso needs a well aerated A (no functional redox features, no reduced matrix, p. 13); tidal flooding, water above the surface or a subaqueous site count as a water table for a grey matrix; the A run continues only under a layer of A material; a light layer low in carbon is read as an E only directly under an O; an E is undecided when the BP below it is undecided; Fe only with ferric nodules of 40 % or more where a BT or FE also holds, otherwise BTcn or FEcn.
* **Aso:** with the list of p. 13 as the only test, the "imperfect when only one holds" clause of Q17 is empty: when the list of p. 13 holds, the grisols reading of p. 193 always holds too.
* **The biomacrostructured A (d3):** without biogenic_aggregates_pct, a crumb or granular structure with a recorded biological activity (worm holes, burrows, earthworms, a mull) reads the biomacrostructured A, flagged; a polyhedral, massive or other structure reads the A that is not; nothing recorded stays undecided.

# soilKey 0.9.341 (2026-11-02)

## France: the notation of the Référentiel pédologique 2008

The French Référentiel pédologique 2008 (AFES) writes its reference horizons in a notation of its own (LAch, NaBTg2, II Scag, E&BT, OFrzo) and defines its references by sequences of them. This release adds the reader of that notation, the catalogue of its horizons and marks, and the fields its references will read. Nothing is classified yet, so every classification is unchanged; the horizons on the data, then the registry entry with the references, follow.

* **The notation reader** (internal). A designation is read in a closed, case-sensitive vocabulary by the longest match (Snd is the silandic S, not Sn with d; Sal the aluminic S, not Sa with l; Kc, Xc, OXc and Femp are codes of their own) into its code, the listed code a variant belongs to (OF for OFr), its family, the overlays L, Na, Sa, the paleo π and J (LA, NaBT, πRT, JZtc), its suffixes as features with their reading on that host (c is indurated, but the mycogenic structure after OF or OH; x is fragic, but mixed rocks after D), a Roman numeral for a lithological discontinuity, the digits of a sub-horizon, transitions (A-S, and BC or ACk written without the hyphen) and juxtaposed volumes (E&BT). The writings the book uses for another code are read with a note (Lav, Vna, Vsa, OFz, OHz, OHmzo), and the old codes Ahs, FEm, FEmp and Ap as Aso, Fem, Femp and L. A designation it cannot read is left unread with a note; text that is not UTF-8 is read as Latin-1; nothing stops with an error.
* **The notation of a profile** is decided before any letter is read, since the RP shares A1, A2, Cca, G, Go and Gr with the CPCS 1967, with opposite meanings for A2 and Cca. A profile is in the RP notation when `site$designation_system` declares it, or when it has one code only the RP writes (OL, OF, OH, BP, LA, SV, FS, OX, RT, FE, Sca, Sal, Aca, Ach, Aso, Kc, Km, Js, Ys, Ztc, Mli, BTß, E&BT, NaBT ...) or two kinds of code that it alone usually writes (Fe, K, Si, Hf, Hm, Hs, X, Y, U, V, Sn, ND, Sg, Cca, a BT with a lower-case suffix where another designation has lower case), and no symbol only the CPCS writes (A00, A0, (A), (B), Bfe, Can, an A2 used as an E) and no sign of the Russian notation. A profile with both is undecided: no designation is read as an RP code, and both readings go to the evidence. The country is never read. In a profile declared `rp1995`, or written with FEm, FEmp or Ahs, FE is the ferric Fe of the RP of 1995, not the ferruginous FE of 2008.
* **Hints from other notations.** A CPCS designation gives the RP codes of Annex 6 (A2 gives E or Ea, Cca gives Ck, K, Kc or Km, (B) the S horizons, with the slips of the table noted), and an FAO designation the codes of soilKey's table of the FAO letters (Bt gives BT or FSt, Bw the S horizons, Oi OL). They are candidates for the identification on the data, never the RP code. The CPCS and Russian readers do not change.
* **The catalogue** `inst/rules/rp2008/horizons.yaml`: the 75 codes of the list of reference horizons, the 4 layers and 8 materials, 11 variants printed in the definitions and chapters (Fox, Fhy, the five kinds of OX, Gr, Go, Mma, Marg), the 7 O sub-horizons of Annex 1, 4 old codes and the B of the transitions (each with the name as printed, an English gloss, its family, the page and its class for the recognition), the 5 prefixes and 32 suffixes with the hosts the book lists, the writings of the book read as another code, the Annex 6 table and the table of the FAO letters. Every sequence of reference horizons printed in the 34 chapters and in Annex 1 (359 rows, 308 distinct strings) reads with no unknown code.
* **New fields.** Horizon: `coarse_fragments_wt_pct` and the four classes by weight `gravel_wt_pct` (graviers, 0.2-2 cm), `cobbles_wt_pct` (cailloux, 2-7.5 cm), `stones_wt_pct` (pierres, 7.5-20 cm) and `boulders_wt_pct` (blocs, over 20 cm); `cec_method`; `redox_relict`; `anthropic_material` and `anthropic_material_pct`; `o_zoogenic`; and the 21 fields that decide a reference horizon: `fine_organic_matter_pct`, `biogenic_aggregates_pct`, `biogenic_aggregate_size`, `effervescence_hot`, `mgco3_pct`, `fe_total_pct`, `fe_ffe_pct`, `weatherable_minerals_pct`, `oxides_triacid_pct`, `clay_mineralogy`, `halloysite_pct`, `kaolinite_pct`, `smectite_pct`, `degradation_form`, `tongue_vol_pct`, `tongue_width_mm`, `lamellae_cumulative_cm`, `c_pyro_pct`, `pyrophosphate_index`, `s_total_pct` and `nodules_wt_pct`. Site: `valley_position`, `flood_frequency`, `poldered`, `lateral_bp_downslope`, `rp_anthropic_history`, `rp_qualifiers_recorded` and `parent_material_relation`. The values `rp`, `rp2008`, `rp1995`, `referentiel` and `afes` of `site$designation_system`, the humus forms of Annex 1 in `humus_form`, and the origins `colluvic`, `marine` and `transported` of `layer_origin`.
* The description of `cole_value` now says what it measures: from moist at 33 kPa to oven dry, not from 1500 kPa.

### Hugo's decisions (2026-11-02)

* **Registry names (Q2):** the system will be `rp2008`, namespace `rp`, aliases `rp` and `referentiel`; `france`, `french` and `fr` stay with the CPCS, so nothing changes for those who use it.
* **The notation (Q9):** declared, or one symbol only the RP writes, or two that it alone usually writes, with no symbol only the CPCS or the Russian notation writes; a mixed profile is undecided; the CPCS and FAO designations give hints through Annex 6, and the data decide; never by the country.
* **The vocabulary (default 12):** closed and case-sensitive, by the longest match; L, Na, Sa, π and J are overlays and the suffixes features; -c is indurated except in Kc, in Xc and after OF or OH; the variants of the book are read; FE is read as Fe only in an RP 1995 profile or with FEm, FEmp or Ahs.
* **The new fields:** the coarse elements by weight in the book's four classes (Q11), the method of the CEC (Q12), relict redox features (Q13), the position in the valley, the floods, the polders and a BP downslope (Q14), the anthropic materials and history (Q15), the qualifiers recorded and the relation to the parent material (Q21), the zoogenic O and the humus forms (Q22), and the 21 fields that decide a reference horizon (Q23).
* **The readings of the reader, accepted** (`inst/rules/rp2008/readings.yaml`): ND, the nodular horizon, is only a likely sign of the notation, since databases write ND for "not determined"; the suffixes read only on the hosts the book lists completely; Ga, Ea and the V variants read as their code with a feature; the frozen mark * of the cryosols; transitions written without the hyphen; Roman numerals without a space and a numeral written alone; the old codes in any RP profile and as signs of the notation; the classes of the symbols beyond those of Q9; the CPCS A2 used as an E; the sign of the Russian notation; the patterns of Annex 6 and the table of the FAO letters; the writing of the sequences; Latin-1 text.

### After the review (Hugo, 2026-11-02)

* **Fixes.**
  * V and X written alone are the vertic and peyric horizons, not Roman numerals (SV/V, O/X).
  * Ap (formerly L) inside a transition or written with a slash (Ap/C) is no sign of the notation and no longer makes the profile one of the RP of 1995.
  * A juxtaposition is a sign of the notation only as E&BT or with a part that is one: the New Zealand L&F and F&H are not.
  * L is a prefix only before the A, E, S, O, H, BT and BP horizons: LF, LC, LR, LM and LD are no RP codes.
  * -ho is a sign only on Aho, Acaho, Sho, Zho, THho and Xho with nothing after it, and the frozen * only a likely one, so the FAO runs Ahoc and A* do not make an RP profile.
  * A prefix written apart joins its master (Na BT, Sa A, p. 296), and "+" between two horizons is a juxtaposition (E + A, p. 315).
  * The (B)Ca row of Annex 6 is read, and A1g takes the A1 row.
  * Sub-horizons are numbered from 1: A0, A00, BT0 and S01 are no RP writing, and A00 keeps its Annex 6 hint OL.
  * Cs is C with -s, not a symbol of the CPCS only; an Australian A2e is no longer called CPCS in the explanation.
  * A mark written twice (Sgg, Rcaca) or -c or -m after a code that says indurated (Kcc, OXmc) leaves the designation unread, with a note; the suffixes of the book's complete lists are read only on the codes it names (St, Ct, FSt; BTd, Sd; Ea, Ga, ga; noz and zo on OF and OH).
  * A transition keeps its sub-horizon digits and the features of each part.
  * Thin and narrow spaces, the Windows-1252 en dash, the capital Π, "pi" and the capital ẞ are read.
  * Pages corrected in the catalogue: Amg p. 12, Alu p. 78, OXfr p. 151.
  * The units of iron: `fe_total_pct` in % Fe2O3, `fe_dcb_pct` and `fe_ffe_pct` in % Fe, and the factor 0.6994 for the ratios of free to total iron (Sn 0.60-0.90, FFE over total iron above 0.20).
  * The two fields of the plan that were missing: `lateral_cover_pct` (horizon: the share of the ground an O horizon covers, Annex 1) and `rp_paleo` (site: the paleosolum terms declared, pp. 58-63).
* **New decisions.**
  * An FAO or USDA symbol beside an RP symbol leaves the notation undecided, as a CPCS or Russian symbol does: a B with a lower-case suffix (Bw, Bs, Bt ...), Oh, Oi, Oe, Oa, Cr, AB or BA, and an Arabic numeral of discontinuity (2Cg, 3Bt). The RP transitions A-S, AS and BC are not such symbols.
  * In a profile written all in capitals the case is lost: the designations an upper-cased FAO, USDA or Canadian table also writes (OH, OF, OL, AH, AP, BT, BW, CK, CR ...) stop counting as signs, while the codes only the RP writes in capitals (LA, LE, LS, LBT, SV, FS, OX, RT, FE, BP, ND ...) still count. OH/AH/E/BT/C is not the RP; LA/BT, SV/V and A/FE/C are. This refines the first reading of the decision, under which only a declaration recognised a profile in capitals, which lost 48 of the 308 sequences of the book.
  * A lone g, the redox horizon written alone, is a likely sign of the notation.
  * `site$designation_system` is read by prefix: a value that starts with rp, referentiel or afes declares the RP (AFES 2009, RP08, Référentiel pédologique français), rp1995 or rp95 the RP of 1995. The Russian reader reads its own declarations the same way (a value that starts with kidpr, ru, rfg or rsc); nothing else in it changes.

# soilKey 0.9.340 (2026-11-02)

## Attachment: engine pieces for systems that are not keys

This release prepares the French Referentiel pédologique 2008, which is not a key: a soil is attached to one, two or none of its references, simply or imperfectly. It adds the generic pieces that system will use. No system uses them yet, and every other classification is unchanged.

* **`within_tolerance(value, threshold, direction, tol)`** compares a value with a threshold that has a tolerance. It returns "pass", "imperfect" (beyond the threshold but within the tolerance), "fail" or NA. The edge of the tolerance band is inside it.
* **Tolerance tables.** A system lists its tolerances by kind of threshold (depth, pH, CEC, ...), for one threshold or for every threshold of a kind. A threshold the table does not list is strict.
* **A tolerant mode of the rule engine.** A class can be evaluated twice, strictly and then tolerantly. In the tolerant pass, the predicates that read their thresholds through the new helpers see each listed threshold moved by its tolerance, in the direction that favours the class. Inside a `none_of` block the direction flips: "no BT within 120 cm" is read within 100 cm, not 140. The two verdicts give the degree: simple, imperfect, simple or imperfect (undecided between the two), undecided, or fail. The criteria that fail, pass only within tolerance, or are missing are named. The mode is off unless a system asks for it, and it belongs to that system: a predicate of another system, even one run from inside the tolerant pass, reads its thresholds as printed. So every other system runs as before. A class that passes strictly is simple. A threshold must be one known number; a tolerance table is always checked.
* **The attachment combiner** chooses the main name among the classes that attach. Exclusions come first, resolved together: only a class that is not excluded excludes another. The explicit precedence rules come next, then the degree (simple before imperfect), then an order list. A double or multiple name is written only for a listed group, in the group's order. A class that could take the name once its data are known makes the name provisional, read at the best degree it can still reach. When nothing attaches, the name says so and the three nearest classes are listed. Codes in the rules that name no class are noted.
* **A `candidates` slot** in the classification result holds every class that may attach the soil, with its degree, rank and criteria. It is empty for every other system, and printing and the summary are unchanged unless it is filled.
* **An additive level with no typical class.** `typical = NULL` adds no "typical" label when nothing is named, for systems that have none.

### Hugo's decisions (2026-11-02)

* **The band edge counts inside.** Against "no BT within 120 cm" (120 ± 20), a BT at exactly 100 cm is imperfect; at 99 cm it fails.
* **Provisional.** An undecided class that could take the name makes the name provisional.
* **Exclusion by an undecided class.** The excluded class stays a candidate, and the name is provisional. Classes that exclude each other are all kept, with a note.
* **Precedence cycles.** Pairs on a cycle are dropped, with a note; the degree and the order decide. When the pairs and the degrees disagree, a note says the order is a convention.
* **Distance.** The nearest classes are ranked by the number of failed criteria, then by the sum of the failures in tolerance units.
* **Candidates.** Undecided classes are always in the candidates; failed ones only as the nearest, when nothing attaches.

# soilKey 0.9.339 (2026-11-02)

## Russia: the types of the divisions CK to CT

The Russian soil classification (Field Guide of 2008) gains the types of the ten divisions CK to CT: 111 types, matched as the types of CA to CJ are (0.9.338). Every division of trunk C now has its types but the chemozems (CU), which have none. Every other classification is unchanged.

* **The 111 types** in `inst/rules/kidpr2008/types/`: CK 12 (solonetzes), CL 3 (solonchaks), CM 4 (hydrometamorphic), CN 9 (organo-accumulative), CO 11 (eluvozems), CP 13 (lithozems), CQ 10 (abrazems), CR 13 (agroabrazems), CS 19 (agrozems), CT 17 (turbozems). Each has soilKey's code in brackets ([CK01] to [CT17], in the order of chapter 4), the Russian name as printed (two slips of the diagrams, Агрослонцы and Сухотофяно-литоземы, as chapter 6 writes them), an English gloss, the formula as the fixture of the formulas transcribes it, and the page.
* **What the formulas test (Q14).** The BCA under the solonetz horizon (BCAs,cs) is optional, in the solonetzes, the solonetzic abrazems, agrozems and turbozems. The hard rock R of the lithozems is tested, with its carbonate mark where the formula writes it without parentheses (Rca: the carbolithozems CP10, CP11). The gleyed substrate DG of the gley eluvozems is tested; the D of the others is not. The fragments of a TUR (TUR[SEL+ASN]) are tested.
* **The fragments of a TUR (Q20)** come from the notation or from the column `turbated_fragments` ("SEL;ASN"); with neither, the solonetzic types are undecided when the TUR has a high exchangeable sodium, and the TUR is not theirs otherwise.
* **Near-identity readings (Q15):** S over Q is a gley solonchak (CL01); a BT exposed or under PB is read as BI (CQ01, CR04, CR05); the BH of CR07 and CS10 is a BHF of either kind.
* **Doubles.** Two formulas match one profile only where one names what the other leaves out: O-EL-DG is CO02 with CO01 its intergrade (likewise CO04 and CO03, CO10 and CO09), AU-Cca-Rca is CP11 with CP06 its intergrade (likewise CP10 and CP09), PB-BM-BCA is CR09 with CR08 its intergrade.
* Every chapter 4 formula of CK to CT, written as a profile in the Russian notation, reaches its own division and its own type, decided, with and without its optional horizons, except CS16 and CS17 without their BCA: a thin TUR of solonetz fragments under P or PU over the parent material has no division (C.6а). One undesignated profile per division reaches its type through the key to horizons on the data.
* **An exposed BT is an abrazem.** At C.20 a BT at the surface, with no humus, organic, agro or eluvial horizon above it and the description starting at 0 cm, counts as BI. This changes a division result of 0.9.337 and 0.9.338: BT over C, which had no division ("not provided"), is now an abrazem (CQ), type CQ01.
* **The secondary solonchaks.** In the types of the halomorphic soils an S or SS laid over another horizon counts as the S of the formula: AY+S over C is CL03, AU+S over G or Q is CL01. The key to divisions sends these profiles to CL as before; outside CL an overlay stays a feature of its layer.
* **The review of the types of CK to CT.** On the data route the gleyed substrate of the gley eluvozems and the carbonate rock of the carbolithozems are read from the data (a gley colour or a redox sign; a carbonate rock named or effervescence), undecided when not recorded, so CO02, CO04, CO05, CO08, CO10, CO11, CP10 and CP11 are reachable there. The missing data of an undecided type name `turbated_fragments` when the fragments of a TUR are what is unknown. A formula that starts with SEL, S, a middle horizon or TUR leaves out a natural organic or humus horizon at the top that no formula of its division writes (a thin O or an AY over SEL-ASN is CK01) and is closed on one another formula writes (AU-SEL-BSN has no type). The post-solonetzic turbozems are tried with the agro types.
* **Known limitation** (as in 0.9.338): a description that ends in a horizon decides the type that needs nothing below it (O-EL with no substrate described is an eluvozem, PU-TUR a dark turbozem).

### Hugo's decisions (2026-11-02)

* **Secondary solonchaks:** the secondary solonchaks form over any profile (pp. 56, 105), so in the types of CL an overlay S is the S of the formula; the [ABC] of CL03 is any formed profile, not tested.
* **Exposed BT:** the near-identity of Q15 (a BT exposed is BI) holds at the key to divisions too, at C.20, with the guard of Q19.
* **The fragments of TUR:** every fragment a formula writes must be among those of the TUR (TUR[SEL+ASN] at the surface is no CT14); with no record of them, the type is undecided when the TUR has an ESP of 15 % or more and the TUR is not one of these otherwise.
* **The five doubles** (CO02 and CO01, CO04 and CO03, CO10 and CO09, CP10 and CP09, CP11 and CP06): the more specific names the type, the other is an intergrade.
* **A division and no type:** the profiles the literal rules leave without a type stay so, with the note of Q15: TJ over C (CN), V at the surface (CQ), O, T or PT over Q, AU-SEL-BSN, AU-ASN without SEL, PB or PU over BSN; a horizon foreign to the division under the plough is ignored (PB over BFM is CR01); CS16 and CS17 without their BCA have no division; on the data a carbonate layer directly under the plough is read as AJ.
* **On the review (2026-11-02):** in the turbozems every middle horizon of C.5 closes the formulas (P-TUR-BHF-C, PU-TUR-V-C and PU-TUR-BM-C have no type) and BT counts as BI in CT05 to CT08; a BCA is ignored in the formulas of CQ, CR, CS and CT that have a middle horizon and do not write BCA (BI-BCA-Cca is CQ01, PU-BM-BCA CS12, PU-BI-BCA CS13), as in CD; a plain TUR (CT01 to CT13) is no TUR of solonetz fragments; the G or Q of CL01 and CL02 lies directly under the S; CT13 takes PTR as well as PT; P directly over BCA is CS14. A surface horizon over a formula that starts with no surface horizon closes it when another formula of the division writes it (AU, AJ in CK) and is left out when none does (O, AY): AU-SEL-ASN is CK05 only, AU-SEL-BSN has no type. The BCA rule brings the double CR09 and CR08, CR09 naming the type.

# soilKey 0.9.338 (2026-11-02)

## Russia: the types of the divisions CA to CJ

The Russian soil classification (Field Guide of 2008) names its types by the formula of the profile, drawn in chapter 4. This release adds the type level for the ten divisions CA to CJ: 106 types, matched on the diagnostic horizons the divisions read. The other divisions keep the division as the name until their types follow. Every other classification is unchanged.

* **The type level.** After the division, `classify_kidpr2008()` matches the profile against every formula of the division with `match_horizon_sequence()`, three-valued. The profile is the one the divisions read: fresh litter left out, the profile under a deposit of 40 cm or less, the surface horizons by the thickest-horizon rule (p. 37), sub-horizons merged, transitions and small horizons left out, an overlay a feature of its layer. The type is the third level of the result, with soilKey's English gloss in `name` and the Russian name in `levels$name_native`. An undecided type tried before the one assigned makes the name provisional; with none matched and some undecided, the name stops at the division and the warnings name the candidates.
* **The 106 types** in `inst/rules/kidpr2008/types/`: CA 27, CB 17, CC 3, CD 13, CE 7, CF 4, CG 3, CH 9, CI 20, CJ 3. Each has a code of soilKey's in brackets ([CA01] to [CJ03], in the order of chapter 4, the natural types first), the Russian name as printed, an English gloss, the formula as the fixture of the formulas transcribes it, and the page. The test of each is the new predicate `type_formula_kidpr()`.
* **A combination no formula provides for** keeps its division. The type is unassigned, a warning says the Field Guide of 2008 does not provide for it, and the three nearest formulas by edit distance are in that warning and in the trace (phase `types_nearest`).
* **Intergrades.** When two formulas match, the more specific names the type; the other is named in the evidence and in the warnings. Among the formulas of CA to CJ this happens only for AY-BEL-BM, where CD09 names the type and CD01 is the intergrade, and for AU-BM-BCA, where CD08 names it and CD03 is the intergrade.
* **Fixes of the review.** A tie or a missing thickness among the organic and humus horizons at the surface, or an undecided layer that could make one of them the thickest, now leaves the type undecided between its candidates (O 10 cm over AY 10 cm over EL-BEL-BT is CA01 or CA04); the type no longer follows one reading silently. On the data route an FAO transition (BC, B/C) or an FAO C layer between two horizons is passed over, as the divisions do: Ah-Eg-Btg-BC-Bg is CA05. A nearest formula that only a gap blocks says so in the note.
* **The stratified body of the stratozems** counts P and PU only over the first RJ, RY or RU, as the agro stratozems draw them (P-RY-D, PU-RU-D). This changes a division result of 0.9.337: RY 12 cm over P-EL-BEL-BT, or over PU-AU-BCA, is no longer a stratozem (BC) but CA17 or CI11.
* **A V laid over another horizon** is a feature of its layer for the key to divisions too. This changes a division result of 0.9.337: AU over G+V is a gley soil (CH, type CH04), not a humus-accumulative soil.
* **The matcher** takes a new option, `agro_remnant = "thickest"`: under an agro horizon only the thickest natural horizon of the surface run stays in the sequence, as its remnant, and the others are features. The default keeps the old behaviour. A list of schemes may also be closed on a vocabulary per scheme.
* Every chapter 4 formula of CA to CJ, written as a profile in the Russian notation, reaches its own division and its own type, decided. So do the profiles without their optional horizons, except CA24 without its EL: P directly over BT is an agrozem. One undesignated profile per division reaches its type through the key to horizons on the data.

### Hugo's decisions (2026-11-02)

* **Closed formulas (Q13):** every horizon that some formula of the division writes must be in the type's formula when it is in the profile; a horizon foreign to the division is ignored and may name a subtype. Most specific first, agro before natural; a second match is an intergrade.
* **BEL closes only where it is diagnostic:** BEL blocks a formula that does not write it only in the types where page 44 makes it diagnostic (CA07, CA08, CA11, CA20, CA21, CD09). In every other formula a BEL is read as part of the passage from the eluvial to the middle horizon: it neither blocks the formula nor is required by it. So AU-EL-BEL-BT-G-CG is CA10, AY-EL-BEL-BT-BCA-Cca CA12, PU-EL-BEL-BT-G-CG CA23 and AY-EL-BEL-BM-C CD07, while AY-EL-BEL-BT-C stays CA04, AU-BEL-BT-C CA07 and AY-BEL-BT-C CA11. The key to divisions already keeps a BEL apart from the clay-illuvial chernozems: AU-BEL-BT is CA07, AU-BI is CI02, AU-BEL-BI has no division.
* **What a formula tests (Q14):** a horizon in parentheses is optional; BEL is required only where it is the diagnostic eluvial horizon (CA07, CA08, CA11, CA20, CA21, CD09); the g and q marks are expected, not required; the parent material is not tested; directly under passes over transitions and sub-horizons, with up to 15 cm with no diagnostic horizon between AU and BCA.
* **No type (Q15):** the division stays and the type is unassigned, with the nearest formulas and a note. The near-identity readings: the BF of CB05, CE06 and CE07, and the BH of the gley Al-Fe-humus soils, are a BHF of either kind; CJ01 takes BCA or CAT; CH02 takes O, T or H over G-CRM.
* **A layer with no horizon** on the data, between two horizons of a formula, may be a transition or loose rock: the type is undecided, with its candidates, not unassigned. Up to 15 cm of it between AU and BCA is still passed over.
* **E and EL in CB and CE:** an EL counts as an eluvial horizon with E. It fills the E of a formula (O-EL-BHF-C is CB07, O-EL-CRM-C is CE05) and blocks the formulas without E (no podbur CB01, no CE01); with no formula with E, the type is unassigned (Q15).
* **BCA in CD:** like BEL, a BCA neither blocks a formula of CD that does not write it nor is required by it (p. 85): AY-BM-BCA-Cca is CD01, AO-BM-BCA CD02; CD08 and CD13 still win where they match.
* **Overlays at the division key:** only a V that is the horizon of its own layer is a middle horizon; a V laid over another horizon is a feature (G+V under AU is CH04). The overlays S and X count as before.
* **The stratified body:** P and PU count only over the stratified horizons.
* **The readings of the types** (`readings.yaml`): the T of a formula takes TO and TE; a natural humus or organic horizon of the surface run that a formula does not write stays out of the closure, and under the plough the thickest natural horizon is the remnant; an overlay (S, X or V over a host) is a feature for the types, not a horizon of the formula; at equal specificity the agro types come before the natural ones, then the order of chapter 4.

# soilKey 0.9.337 (2026-11-02)

## Russia: the registry entry, the trunks and the 27 divisions

The Russian soil classification (Field Guide of 2008) joins the registry as `kidpr2008` (aliases kidpr, russia, ru2008), experimental and opt-in: `classify_all()` does not run it unless asked, so every other classification is unchanged. This release keys the five trunks (key 2.2) and the 27 divisions (key 2.3) on the diagnostic horizons of the profile, from the Russian notation where the profile is written in it, else from the key to horizons on the data (0.9.335-0.9.336); the types and subtypes follow.

* **`classify_kidpr2008()`** and **`make_kidpr2008_example()`** (one small profile in the Russian notation per division). The result names the division (or the trunk), with an English gloss in `name` and the Russian of the book in `levels$name_native`; a profile that is not a soil (trunk E), a thin layer of loose rock over hard rock, and a combination of horizons the Field Guide does not provide for are noted in the warnings, the division unassigned.
* **The keys flattened** (`inst/rules/kidpr2008/key.yaml` and `divisions/`): each trunk and division is a first-match entry whose gate is the conjunction of the conditions of the book's path to it (less what an earlier entry of the list excludes), written with the path (C.4а-C.5а-C.6б-C.7а); the steps of the key to divisions are written once and named by the gates. The trunks E, D, A, B, C; trunk A: AB, AA; trunk B: BA, BB, BC; trunk C: CP, CL, CU, CQ, CO, CN, CT, CR, CS, CC, CF, CG, CH, CE, CJ, CK, CB, CA, CD, CI, CM; trunk D: [DA], the division the book does not code.
* **The predicates** (internal, `_kidpr`): a horizon present, starting the profile, the surface horizon, directly over another, below another, the profile made only of some horizons, hard rock within 30 cm, a TUR over 40 cm, the peat body, the deposit at the surface, the stratified humus body, a soil horizon within 1 m, a river floodplain on modern alluvium, volcanic ash and the contaminated horizon X, three-valued, reading the frame of `identify_horizons(pedon, "kidpr2008")` once per classification.
* A literal walker of keys 2.2 and 2.3, with its own reading of the profile, is the test oracle of the flattened lists: on every combination of the tested horizon-presence space they agree once the repairs below are applied, and every difference from the literal key is one of them. A sample of that space is also classified end to end as pedons in the Russian notation, and a set of common soils of Russia on the data route. Every profile formula of chapter 4 reaches its own division.
* The level labels Trunk and Division (EN, PT) in the app and the report; the app offers the 27 examples; the site attributes `frost_regime` and `designation_system`, and `landform`, `landform_element`, `parent_material` and `contaminated`, name the system that reads them.

### Hugo's decisions (2026-11-02)

* **Levels and registry (Q1, Q2):** trunks and divisions now, types and subtypes later; trunk E is not a soil (rock outcrops, technogenic surface formations, a deposit that is not humified over a buried soil), with a note, and the chapter 8 groups are not keyed; id `kidpr2008`, namespace `kidpr`; the codes of the book, [DA] for the uncoded division of trunk D.
* **Precedence (Q9):** the printed order of the steps and of their alternatives (C.7: BFM, BPL, CR, CRM, BMK, ASN or BSN; C.8: BHF, BT, BM); C.19в before C.19а (P with AY and PU with AU are organo-accumulative soils); C.6б over C.6а when BFM, BPL, CRM, CR, CRH or BMK is in the profile (P-G-CRM is a gley soil); an agro horizon with no natural remnant on alluvium is an agrozem (p. 124).
* **Gaps (Q10):** only those the book's types prove are repaired: AY and AK at C.2, PT and PTR at C.19а, AK at C.19в, a TUR over 40 cm tested at C.6 and a TUR at the surface counted at C.4, trunk D taking an S alone over loose rock, AY or AO directly over G to the gley soils; every other combination with no exit is "not provided by the Field Guide of 2008", the division unassigned, with a note.
* **The surface, the sources (Q11, Q12):** the agro horizon, else the thickest of the natural organic and humus horizons that start the profile (a W of 5 cm or less over another horizon is a feature); division names from the chapter 4 headings, the index and key forms as synonyms.
* **Truncated, turbated, synlithogenic, peat, chemozems (Q19-Q23):** a middle horizon at the surface counts only when the description starts at 0 cm; the turbozems need a TUR over 40 cm; trunk B takes only what one of its divisions takes (a floodplain with alluvial material; BAN; a stratified humus body of 40 cm or more, P and PU counted), and the deposit of step 2 is the material at the surface that is not a horizon formed in it; the peat body of more than 50 cm sums the contiguous organic layers, H, PT and PTR included, a description ending in peat at 50 cm or less undecided; the chemozems only at C.3б, after the lithozems and the halomorphic soils, X from the extremely hazardous category within 30 cm.
* **soilKey's readings, accepted** (`readings.yaml`): V and CRH count among the middle horizons of C.5 (the types CI04, CI05, CI14, CI15 have no other); a profile C.6 gives no answer (an agro horizon on ASN or BSN, the agro-solonetzes CK09-CK12) goes on to C.7; the peat T of the key's lists includes TO and TE; the surface S or SS of C.3 starts within 10 cm; loose rock belongs to the deposit only over a buried soil or an organic or humus horizon (TPO and the deposit of the data always), and a buried soil directly under a thin deposit is the soil keyed; a valley, alluvial plain or delta landform leaves the floodplain undecided; P or PU at C.19а is read as present; each gate leaves out the exclusions an earlier entry of the list supplies, so that an undecided class makes the one after it provisional. Every reading in `readings.yaml` is now one of Hugo's decisions.

### After the review (Hugo, 2026-11-02)

* **Fixes.**
  * TPO, and the deposit the key to horizons finds on the data, belong to the deposit at the surface: TPO or a raw deposit over 40 cm on a soil is trunk E, not a soil.
  * A layer the data leave undecided between peat horizons counts in the peat body, and step 6 no longer asks for the decomposition or the von Post value it does not need.
  * A layer the data leave undecided between soil horizons is a soil horizon for step 1, so such profiles are no longer provisional on trunk E.
  * A layer in which the key on the data finds no diagnostic horizon may be a transition: it no longer proves that the profile has no soil horizon, so the canonical plinthosol is no longer trunk E. Rock, TPO and a deposit still do.
  * FAO and WRB transitions (AB, BC, E/B and the like) are passed over as those of the Russian notation are. A chernozem Ah-AB-Bk is a humus-accumulative soil on both routes.
  * A missing depth or thickness leaves the step that needs it undecided: hard rock, a surface salt horizon or a contaminated layer of unknown depth, and the thickest-horizon rule with a thickness missing. No R warning any more.
  * The texts of the site are read as words. A terrace is no floodplain, glaciofluvial material and outwash are no alluvium, and outwash or washed sand is no volcanic ash.
  * The peat body is more than 50 cm, as the book says. `classify_kidpr2008()` says it needs a PedonRecord when it is given something else.
  * A description that starts with fresh litter at 0 cm starts at 0 cm for a truncated soil.
  * The test oracle reads the profile on its own. Its space now holds TPO, the deposit of the data, transitions, layers with no diagnostic horizon and an O thicker than the humus horizon under it.
* **New decisions.**
  * BAN is formed in volcanic ash by its definition, so BAN alone makes the volcanic soils: H-BAN-C is BB, by the notation or by the key on the data.
  * Step 5 of the key to trunks reads the profile after the thickest-horizon rule, as C.2 and C.19 do. O over a thinner humus horizon is only O, a weakly developed soil, and O with W needs no repair.
  * Every ploughed soil of the iron-metamorphic (CC) and light-humus carbonate-accumulative (CJ) divisions is an agrozem (CS), or an agroabrazem (CR) under PB, with or without a remnant of the natural horizon (pp. 83, 99). P-AJ-BCA, P-AY-BFM and P-AJ-BM-BCA are agrozems, PB-BFM an agroabrazem. This replaces the reading of an agro horizon directly over BFM or BMK.
  * On the data route an FAO or WRB transition counts as a transition only when the key to horizons finds no diagnostic horizon in it. AE read as AEL and EB read as BEL keep their horizon, so the grey forest soil keeps its division (CA06).
  * On the data route a layer designated C (C, Cg, 2Ck) in which the key to horizons finds no diagnostic horizon is loose rock, one of the readings key position 66 lists.
  * C.14 reads "directly under", as C.13 does. The key prints "under", but the text of the gley soils (p. 92) puts G directly under the organic or humus horizon and leaves out a profile with any other mineral horizon, and the text decides. O-BI-G is not provided by the Field Guide of 2008.
  * BMK stays out of C.5: AJ-BMK-C is not provided.

# soilKey 0.9.336 (2026-11-02)

## Russia: the diagnostic horizons on the data

The Russian soil classification (Field Guide of 2008) identifies the diagnostic horizons of a profile by a key of 66 positions (section 2.1). This release runs that key on the recorded data, so that every layer gets its Russian horizon where the profile is not written in the Russian notation, with soilKey's WRB diagnostics standing in where a field descriptor of the key is missing. Nothing is classified yet, so every classification is unchanged; the registry entry with the trunks and divisions follows.

* **The key 2.1** (internal; the horizon identification `identify_horizons()` dispatches to once the registry entry exists). The 66 positions in the book's order, each a question on the layer and on the whole profile (the layers above and below, the depth of a boundary, frost in the profile). It reaches the 50 horizons, the modifications AV, BH and BF, the peat as rock TT, the features u, tr, rh, r, ao, e, el and ca, and no diagnostic horizon (position 66). The overlays S, X and V are kept beside the horizon of the layer; one left undecided leaves only the overlay undecided. Two answers the key gives no exit (a mixed layer neither TUR nor tr; continuous effervescence with cutans and no white-eye) leave the layer undecided between their neighbours. In a profile dark from a dark rock, a middle horizon gets the feature u and goes on at 31.
* **One row per layer**: the horizon and its modification, overlays, features, small horizons (e, el, ao, r, rh), the route (notation, data or both), the path of the key in the book's notation, the evidence, the WRB diagnostics that stood in and the fallbacks used, and, when undecided, the candidates and the attributes that would decide it. A layer written in the Russian notation keeps its horizon; the key runs on its data too, and a reading that differs goes to the evidence. Designations in other notations give hints only; fresh litter is no horizon; hard rock is R.
* **The readers** (internal): the colour words of the key in Munsell, the Kachinsky gradations of texture and the clay ratio, acid or not, the salts and the pollutants, the organic matter, the decomposition, the carbonates, frost, ploughing and the surface deposit, each three-valued and noting the fallback it used. soilKey's cambic horizon also stands in for the transformed structure of BM, and its andic properties for the volcanic ash of BAN; a layer under pH(H2O) 7.0 is read as holding no free carbonates when no effervescence is recorded, and a layer with a particle-size analysis, CaCO3 or a mineral bulk density but no carbon as mineral.
* **The fixes of the review.** A grey layer under an organic layer, or one continuing the surface horizon, is a humus horizon only when it looks like one (not gley or olive, not massive, single-grained or stratified, not frozen, darker than the parent material), so the CRM, CR or G under the O of a cryozem or a gleyzem, a pale soil's BPL and layered alluvium are no longer read as AY or AJ. The light horizon is compared with the middle horizon under the BEL. The argic stand-in gives no cutans to massive layers, carbonate layers or layers outside the clay maximum; the solonetz structure comes only from a columnar structure recorded in the layer's run; the carbonate neoformations not recorded are read by calcic(), protocalcic_properties() or the carbonate maximum of the solum. The Soviet plough layer (Apakh, Ap with a Cyrillic p) and a clear plough boundary are read; the site's mixing depth no longer makes the plough layer mixed. With equal values the lower chroma is the lighter layer; CRM is dull (chroma 3 or less) with a cryogenic structure; BMK needs the effervescence; a rich granular AU is no AH; a film colour must be brighter than the parent material; AK must be strictly the darkest; a natural surface layer lighter than the humus horizon under it is a deposit (criterion 3 of position 8); Russian texture words are read in either order.

### Hugo's decisions (2026-11-02)

* **Without the notation (Q4):** the key runs on the attributes; where a field descriptor is missing a soilKey WRB diagnostic stands in, flagged: spodic() for the iron films of BHF, argic() for the cutans of BT and BI, calcic() for the carbonate neoformations of BCA, the structure of natric_horizon() for ASN and BSN (the exchangeable sodium never decides). An undecided question follows every branch: one outcome gives the horizon, several leave it undecided with the candidates.
* **Colours (Q5):** dry Munsell, one cut-point per binary choice of the key (dark: value 4 or less, chroma 2 or less; grey: chroma 3 or less in 10YR, 2.5Y, 5Y or N, 7.5YR only at chroma 2 or less; whitish: value 7 or more, chroma 2 or less); dry value = moist value + 1 when only the moist colour is recorded, flagged; the gley, olive and solonetz colours read moist, the gley set the whole gley chart.
* **Organic matter (Q6):** humus = 1.724 x OC; LOI from `loi_pct`, else 1.724 x OC; decomposition from `peat_decomposition_pct`, else von Post (H7 or more, H4 or less), else the rubbed fibre (under 17 %, 40 % or more); between them undecided.
* **Texture (Q7):** the Kachinsky gradations from `phys_clay_pct` or `texture_class_ru`, else the USDA classes in order of fineness, one step one gradation, flagged; BT against BI by the clay ratio of 1.4 to the overlying humus or eluvial horizon.
* **Salts and pH (Q8):** `soluble_salts_pct`, else the EC of chapter 9 (ECe 16 dS/m, EC 1:5 2 dS/m), else a salt crust or efflorescences recorded at the surface; pH(KCl) alone acid under 5.0, not acid from 6.2.
* **The surface (Q11), the sources (Q12):** a horizon starts at the surface at the top of the mineral soil, under the agro layers or under the deposit; exact boundary values follow the key's inequality (e 2 cm or less, O 10 cm or less, el 5 cm or less, salts over 1 %, a humified deposit of 40 cm a horizon, a raw one a feature).
* **Ploughing (Q18):** the Russian index, else the plough suffix p in the layers from the surface, else a tilled land use with an abrupt boundary within 40 cm; a designated profile with no plough mark is natural, a profile with no designation and no land use undecided.
* **TUR (Q20), the deposit (Q21), peat (Q22), X (Q23):** TUR from `foreign_fragments_pct` (80 % or more) or the site's mixing depth, flagged; the deposit is the mineral material over a buried soil, or a layered deposit at the surface, never a humus horizon formed in it; TO and TE in the upper 50 cm of the peat, TT below, TJ by the value mesophilic; X from `contamination_hazard`, the site's contamination without a category undecided.
* **Frost (position 26, the review):** `frost_regime` first; else frozen ground in the profile (ice, a temperature of 0 C or less, the permafrost sign, the FAO frozen suffix, cryoturbation) yes; else the Koppen climate (D with c or d, ET, EF yes; A, C, D with a or b, BWh, BSh no; BWk and BSk undecided); else the soil temperature regime (cryic, gelic, pergelic yes; mesic or warmer no; frigid undecided); else a mean annual air temperature over 5 C no; nothing recorded, undecided. Latitude does not decide; the route is flagged.
* **After the review (Hugo, 2026-11-02):** no free carbonates under pH(H2O) 7.0 when no effervescence is recorded; carbonate neoformations not recorded read as the carbonate maximum of the solum (calcic() or protocalcic_properties() passing, or the layer with more carbonate than every layer above, at least as much as those below except the parent material, and 1 % or more), not for the parent material, undecided otherwise; Koppen BWk and BSk fall through to the soil temperature regime and the air temperature; a film horizon neither BFM nor fading is BHF; GY, G, BG, B and PB above chroma 1 need a structure or a redox sign as 2.5Y does; plant residues count above 5 %; LOI 10-15 % with a crumb or granular structure is AU; the clay maximum is 90 % of the highest argic clay; the site's mixing depth is undecided when ploughing is.
* **Litter (Hugo, 2026-11-02):** an FAO Oi or L is fresh litter only in a run of such layers that starts the description and is 5 cm thick or less (a run with no recorded thickness stays litter); thicker, or lower in the profile, it is an organic layer the key reads on the data, so a raised bog described Oi/Oe is peat and 40 cm of Oi over a gleyed layer is T. The Soviet A00 stays litter.
* **The peat body of positions 41 and 44 (the review):** only weakly decomposed peat, without H, PT or PTR, as the key writes it (the 51 cm body of the organogenic trunk keeps them).
* **Gley in 2.5Y (the review):** with 2.5Y at chroma 2-3 a G needs a massive or structureless structure or a recorded redox sign (mottles, redox features, a dipyridyl test); the rest of the gley chart (N, and GY, G, BG, B, PB at chroma 1) suffices by colour.
* **Defaults:** the agro horizon by the colour of position 19 (PU dark; P grey as Q5 reads it, chroma 3 or less in 10YR, 2.5Y, 5Y or N, with a dry value of 5 or more, so 10YR 5/3 is P; PB the others; 15a allows a brown tint); V by slickensides, wedge-shaped peds and clay 30 % or more within 80 cm, one or two of the signs the feature v; continuous effervescence by `effervescence_extent`, else CaCO3, else the effervescence class; white-eye = soft concretions; field observations not recorded are absent, measurements not recorded leave their question undecided; field-only criteria are evidence.

# soilKey 0.9.335 (2026-11-02)

## Russia: the notation of the Field Guide of 2008

The Russian soil classification (Field Guide of 2008) writes its horizons in a notation of its own (AY, EL, BEL, BTg, BCAs,cs) and names its types by profile formulas in it. This release adds the reader of that notation, the catalogues of its horizons and features, and the fields its horizon key will read. Nothing is classified yet, so every classification is unchanged; the horizon key on the data, then the registry entry with the trunks and divisions, follow.

* **The notation reader** (internal). Each designation is read into its horizon and features: small horizons written alone (O-e-BHF-C), bracketed features ([hh], [e-hf], [akl], also on a host: AJ[akl]), turbated mixtures ([AY, EL]tr, TUR[SEL+ASN]), a remnant in parentheses ((EL)), the modifications BH, BF and AV, the rock flags (CG, CQ, DG), the rock marks (C~~, C", C^^, C"", C=, C..), a lithological layer (2BM, IIBT), sub-horizons (BT1, BT2), transitions (AUBM, AU/BM, BC), a buried horizon in brackets ([AU], [BTg]) and an overlay written after a "+" (AU+S). A row it cannot read gets the hints of the FAO letters, and each reading it had to make is noted (a note after a space, a trailing "?", a bracket that is no feature of the catalogue read by its parts). Text that is not valid UTF-8 is read as Windows-1251, the Cyrillic code page, when it can be, else left unread; nothing stops with an error.
* **The catalogues** `inst/rules/kidpr2008/horizons.yaml` and `features.yaml`: the 50 diagnostic horizons (Russian name as printed, an English gloss, the chapter 3 group, the path of the horizon key, the page), the modifications AV, BH and BF, the alias PC, the rock symbols C, D, CLM, R, TT and TPO (technogenic surface formations), the marks and the four tokens of the chapter 6 formulas (AM, AKL, PY, TR); the 64 features of section 3.2 and the three indices only the chapter 4 lists use (w, x, rx), with their aliases, their host horizons and the host-bound forms of the lists (ELf, Th, Tote, AUlc-BCAmc ...). Every profile formula of chapter 4 (275) and of the chapter 6 correlation (266) parses with them, with no unknown token; 478 of them are recognised as Russian with no declaration (the other 63 are written only with letters other notations share).
* **The WRB cryic conditions** read a profile declared in the Russian notation by its permafrost sign, not by an f, which is ferruginated there (ELf); no other profile changes.
* The split of a run of features written without commas (internal, the formula parser) takes linear time; a long run that could not be split took seconds to minutes.
* **New fields.** Horizon: `phys_clay_pct` and `texture_class_ru` (the Kachinsky texture), `loi_pct` and `peat_decomposition_pct` (the organic horizons), `plant_residues_pct`, `uncoated_grains_pct`, `effervescence_extent`, `sulfide_odour`, `slickensides_size` and `cryoturbation` (positions of the horizon key), `foreign_fragments_pct` and `turbated_fragments` (turbated layers), `contamination_hazard` and `pollutants` (the contaminated horizon X). Site: `frost_regime`. The value `mesophilic` of `peat_moor_type`, and the Russian names of `site$designation_system` (kidpr, kidpr2008, ru, ru2004, ru2008, russian, russia, rfg2008, rsc2004).

### Hugo's decisions (2026-11-02)

* **Notations (Q3):** only the Russian notation of 2004/2008 gives the profile type: declared in `site$designation_system`, or recognised by an index only it writes (AY, AU, EL, BEL, BHF, BCA, PU, TUR ...) or a sign only it writes (the permafrost sign, a rock mark, a comma-separated feature list, a bracketed feature), once the Cyrillic letters that look like Latin ones are read as Latin; never by the country. FAO letters (Ap, Bt, Bs, Bk) only give hints to the key on the data, and so does the Soviet notation of 1977 (A0, A1, A2, Bca); in a profile with its numbered tokens, AO is the Soviet A0. In a profile written all in capitals, the indices an upper-cased FAO designation could spell (AY, EL, BEL ...) count while the rows that read as FAO ones (AP, BW, BTG) are not more numerous, and the capitals after an index are then features (BTG = BT + g); PU, PT, TUR, SEL, TO ... always count. AH, AK, AO, BH and BF count only where the case of the letters is kept, or beside an index no FAO master starts (P, G, T ...).
* **The notation (defaults 8 to 13):** the Cyrillic look-alikes are folded and the permafrost sign is read in both its forms; the aliases tr = tu, ek = ko, agr = ag, ae = ael, cs = as and PB = PC are read as the first; Tote is TO with te; the tokens AM and AKL of chapter 6 are read; a lithological layer is numbered in Arabic or Roman figures; a layer may hold a horizon and an overlay S, X or V; small horizons are marked rows, sub-horizons are merged and transitions are not diagnostic; a feature string without commas is read by the longest match, with a note where it could be read otherwise (hi, gr, md, mr, rh, ur, yu, hh, ff, rr, rx, hf, agr); a feature written alone is a small horizon only where the text allows it (e, el, ao, ek, r ..., not g, s, ca or v); CG, CQ and DG are a gleyed or quasi-gleyed rock, not a G or Q horizon; the printing slips of the keys are corrected.
* **The new fields (Q6, Q7, Q20, Q22, Q23, Q24):** the Kachinsky texture (`phys_clay_pct`, `texture_class_ru`), the loss on ignition and the degree of decomposition, the fragments of a turbated layer and their horizons, the hazard category and the pollutants of the contaminated horizon, `mesophilic` peat, and the seven descriptors that decide positions of the horizon key (uncoated grains, the extent of the effervescence, the frost regime, plant residues, the smell of hydrogen sulfide, the size of the slickensides, cryoturbation). The descriptive ones (skeletans, the place and layering of the clay films, the mineral admixture, carbonate pendants, an erosion mark) are not added: the notation or the evidence carries them.

# soilKey 0.9.334 (2026-11-02)

## Russia: the engine for profile formulas

The Russian soil classification (Field Guide of 2008) names its types by profile formulas ("O-EL-BEL-BT-C") and its subtypes by a list of features per division. This release adds the generic pieces it needs; no system uses them yet, so every classification is unchanged. The Russian notation, horizons, divisions and types follow.

* **Profile formulas.** An internal parser reads a formula into slots: alternative codes ("RY(RJ)", "CG~~ (CQ~~)", and soilKey's "O|T|H"), optional horizons in parentheses, features (typical, or required where they name the type, as Rca), the role of each slot (surface, middle, parent), buried profiles ([ABC], {ABC}), turbated compositions (TUR[AU+SEL+ASN]), small horizons ([e-hf], e), rock flags (CG, CQ, DG) and rock marks (C~~, C", C^^, C"", C=, C..). Given a vocabulary of horizons and features, an unknown token stops it with its name.
* **`match_horizon_sequence()`** matches the identified horizons of a profile against a formula: TRUE, FALSE or NA when an undecided layer could change the answer, with the slots matched, the slots missing, the extra horizons and the edit distance. A horizon fills a slot by its code or by an overlay (an AU with an S overlay fills an S slot); surface and agro codes the formula does not name stay out of the closure unless `closure_surface = TRUE`, so a natural remnant under the plough does not block an agro type. Internal helpers run a division's schemes, pick the most specific match (the others noted as intergrades) and, when none matches, rank the nearest schemes by edit distance.
* **`identify_horizons()`** gives the reading of every layer as a diagnostic horizon (code, overlays, features, small and transitional rows, buried profiles, route, verdict, evidence, the candidates when undecided), by the system's own identification; a system without one stops with an error, as every system does in this release.
* **A walker of horizon keys** (internal): a key written as a tree of questions answered TRUE, FALSE or NA.
* **An additive level** (internal): after the type, every listed feature that passes is named, in list order. The Hungarian subtype properties keep their own code.
* `print()` of a result lists the classes of one level together ("CA01 Podzolic > g gleyic, y tongued"), so the Hungarian subtype and variety properties are no longer chained with ">" as if each were below the one before.

### Hugo's decisions (2026-11-02)

* **Undecided key questions (Q4):** an undecided question follows every branch; one outcome gives the horizon, several leave it undecided with the candidates.
* **The surface horizon (Q11):** the agro horizon, else the thickest natural organic or humus horizon starting at the surface or under a deposit under 40 cm; the others are read as features.
* **Templates (Q13):** a type's formula is closed on the vocabulary of its division: a horizon that appears in any scheme of the division must be matched when present; a horizon foreign to the division is ignored. The most specific scheme is tried first, agro before natural; when two match, the more specific wins and the other is an intergrade in the trace.
* **Formulas (Q14):** a horizon in parentheses is optional; terminal symbols are tested only where they name the type; "directly under" skips transitional rows and sub-horizons, with an allowed gap (up to 15 cm between AU and BCA).
* **No type (Q15):** when no scheme matches, the name stops at the division and the trace lists the nearest schemes.
* **Subtypes (Q16):** every listed feature that passes is named, in list order; typical when every one is FALSE; provisional when some are undecided, which are listed; a feature already written in the type's formula is not named again.
* **Also accepted:** sub-horizons are merged (BT1, BT2 = BT); transitions are not diagnostic; small horizons are marked rows, not diagnostic horizons; one layer may hold a horizon and an overlay (S, X, V).

# soilKey 0.9.333 (2026-11-01)

## France: the 276 subgroups of the CPCS 1967

`classify_cpcs1967()` now keys a pedon to its subgroup, the fourth and last registry level, `subgroups`. A subgroup the text names in full stands alone ("Podzols humo-ferrugineux", "Chernozem modal"); one it names by an adjective follows its group ("Rankers, à mor", "Sols ferrallitiques moyennement désaturés en (B) typiques, Faiblement appauvri"). `name` is now the deepest level.

* **Subgroups.** The text names most subgroups by a secondary process or by one side of a measure; soilKey's readings are listed in `readings.yaml`.
  * A subgroup the data cannot decide leaves the name provisional.
  * When no listed subgroup of a group fits (27 groups have no modal), the name stops at the group and the trace says why; 22 groups have no subgroups.
  * The other subgroups whose tests pass are noted in the trace as "intergrade vers ...", and a brown soil with hydromorphic signs only below its (B) as a "faciès hydromorphe".
* **Five new fields:** `humus_form` (site: mull, moder, mor, hydromull, hydromoder, hydromor, anmoor, tourbe), `caco3_active_pct`, `coarse_sand_pct`, `fine_sand_pct` and `gibbsite_pct` (horizon).

### Hugo's decisions (2026-11-01)

* **Inventory:** the main text of the AFES edition, 276 subgroups; I/16 has no common subgroups; the two peat subgroups under each peat group (XI/111 to XI/132); "Sans horizon B" is XI/221; VII/115 and VII/413 included; IV/22 takes the subgroups of IV/21. Codes: the class and the text's three digits (I/111 under I/11).
* **Order:** the printed order, modal last; a subgroup that splits the group by a measure after those of a secondary process, and a composite subgroup before its parts.
* **Hydromorphy:** signs (redox features above 0 %, mottle colours, a reduced matrix, or a g or G where no mottles are recorded) in a layer starting within 100 cm; also a water table within 1 m for "à nappe"; pronounced signs or concretions within 50 cm for the vertisols; signs from 50 cm and none above for II/142.
* **Shallow profiles:** by the profile type: hard rock directly under the A for the lithosols, C or R within 45 cm for IV x13 and 50 cm for VII/411, a (B) on hard non-calcareous rock within 50 cm for VII/115.
* **Vertisols:** "vertiques" with slickensides common at most; self-mulching with fine structure to 10 or 20 cm.
* **Salt and sodium:** EC 4 dS/m or more, exchangeable Na 6 % of the CEC or more, within 100 cm.
* **Rendzinas:** a very strong effervescence or 10 % active carbonate (V/112), under 10 % carbonate with 15 % coarse fragments (V/114), a dry climate (V/115).
* **Crusts:** an encroûtement is a recorded accumulation (5 % secondary carbonates, 5 % more carbonate than the layer below, or a Ca of the notation); a croûte is a cemented layer.
* **Isohumic soils:** vertique = protovertic or a coarse prismatic (B); à B textural = the textural B of VII/12; rubéfié = the reddened B of class IX.
* **Brunified soils:** the measures decide (allophane, clay index, pH of the B); the colour words of the A2 and the humus are descriptions, except the ashy A2 over an ochre B of VII/124.
* **Podzolised soils:** Bh and Bfe by the notation, then the colour; deep hydromorphy below the base of the B with none above it.
* **Classes IX and X:** the accepted IX/X table; "faiblement rajeuni" at 3 % weatherable minerals; "faiblement remanié" by the evidence of the reworked groups; X/322 by a dark brown B, pH under 4.5 and gibbsite (or Ki under 1.0).
* **Class XI:** the peats by their pH (5.5), else the moor type; anmoor by a C/N under 20, hydromor by the humus form, else a C/N of 20 or more; XI/321, 322, 341 and 342 by the morphology.
* **Class XII:** XII/21 by the surface structure, then the EC route; XII/23 by the Na of the first B; XII/114 by a thionic horizon, or pH under 4 with jarosite or sulfidic S.
* **After the review** (Hugo, 2026-11-01):
  * the sub-class IX-1 takes the nearly saturated tropical sands of IX/114 (8 % clay or less to 100 cm, under 1 % OM in the A) whatever their base saturation, and IX/114 reads V 80 % or more in the larger part of the B;
  * an encroûtement needs a recorded accumulation, not soilKey's calcic horizon alone;
  * the yellow and brown subgroups of classes IX and X come after the subgroups of a process, and the thick calcic brown soils (V/213) before the brunified rendzinas;
  * IX/213 and IX/222 take any hydromorphic sign;
  * a subgroup left undecided before the assigned one makes it provisional, as at the other levels;
  * the detailed readings soilKey made where the plan was silent are kept, marked as soilKey's in readings.yaml.

# soilKey 0.9.332 (2026-11-01)

## France: the 101 groups of the CPCS 1967

`classify_cpcs1967()` now keys a pedon to its group, a third registry level, `groups`, for example "Podzols", "Sols lessivés" or "Sols ferrallitiques fortement désaturés en (B) typiques". The subgroups follow.

* **Groups.** The text defines most groups by a profile type, a colour word or a property with no number; soilKey's readings are listed in `readings.yaml`.
  * A group the data cannot decide leaves the name provisional.
  * When no group of a sub-class fits (VIII-1, VIII-2, XI-1 have no default), the name stops at the sub-class and the trace says why.
* **Two new fields:** `patterned_ground` (site: polygons, circles, stripes, mud boils, hummocks, palsas) and `ice_form` (horizon: ice lenses, wedges). They decide the groups of the cold-desert and permafrost soils; not recorded, those groups are undecided.
* The FAO and USDA suffixes i, e and a of an O horizon give the fibre class of a peat when no fibre content is recorded.

### Hugo's decisions (2026-11-01)

* **Deposits and erosion (I-1, II-4):** the origin recorded for the material under the A within 50 cm, else the parent material; anthropic, volcanic, alluvial, colluvial, aeolian, in that order; no origin recorded, the erosion group.
* **Classes I to IV:**
  * the lithosols of the deserts: hard rock within 10 cm;
  * II/23 allophanic first (pH NaF 9.5 or more, or Alo + 1/2 Feo 2 % or more), II/22 only on a calcareous rock;
  * II/31: under 1 % OM and an arid climate;
  * the vertisol groups by the structure of the top 15 cm (self-mulching is angular, massive undecided);
  * IV/21: base saturation of 50 % or more in the (B), pH 6 or more without it.
* **Classes V and VI:**
  * V/13 cryptorendzinas first (dolomite), then V/11 rendzinas (no (B)), V/12 the others;
  * V/22 (OM above 20 % on hard carbonate rock), V/23 (fine polyhedral A, CEC of the clay 50 or more), V/21 the others;
  * the isohumic groups by the mean OM of the top 20 cm (chernozems above 5 %, chestnut soils 3 % or more, marron soils 1 % or more).
* **Classes VII and VIII:**
  * the leached soils: the clay index (largest clay of the B over smallest of the A) above 1.4 with 5 % clay more; films recorded as none or few rule it out;
  * the sod-podzolic soils: an A1 of 25 cm or more and tongues;
  * one colour table for the podzolised soils (ashy = albic, pale = one Munsell value above the A1 and the B, ochre = 10YR to 5YR with chroma 6, brown = chroma 4 or less);
  * boreal podzols (Köppen Dc or Dd, or latitude 50 or more) before alpine ones (ET or EF below 60 degrees, or 1,500 m).
* **Classes IX and X:**
  * IX/12 leached, IX/13 impoverished, IX/11 the others, by the clay index, the thickness of the A, the C and the clay films;
  * IX/21: a calcium reserve within 100 cm, or a (B) with V 95 % or more and pH 7 to 7.5;
  * class X in the order humic, leached, impoverished, reworked, rejuvenated, typical (X/24 read as reworked).
* **Classes XI and XII:**
  * the peat groups by the fibre class over the greatest thickness;
  * XI-3 in the order 35, 36, 34, 31, 33, 32 (the specific groups first);
  * XII-2 in the order solodised, Solonetz, Solonchak-Solonetz.
* **After the review** (Hugo, 2026-11-01):
  * the deep gley of the amphigley starts at 80 cm; in a declared FAO notation, g is a pseudogley and l the fringe of a gley;
  * a water table recorded as none, or not recorded, makes a stagnogley of a profile with its morphology (undecided when not recorded);
  * VIII/13 allows an A2 under 5 cm and no Bh over 3 cm; a site at 1,500 m or more below 60 degrees is alpine;
  * V/31 is the AR or ACR profile, its thinness a description; a plain B of the CPCS notation is a textural B when clay is not recorded;
  * ice-wedge polygons are a strong ice segregation; XII/23 without a pH is undecided; a columnar B is compact;
  * names carry soilKey's insertions in brackets: "[possible]" for III/21-22 (the text prints "nul"), "[remaniés]" for X/24.

# soilKey 0.9.331 (2026-11-01)

## France: the 35 sub-classes of the CPCS 1967

`classify_cpcs1967()` now keys a pedon to its sub-class, a second registry level, `subclasses`, for example "Sols ferrallitiques fortement désaturés en (B)" or "Sols brunifiés des climats tempérés humides". The groups and subgroups follow.

* **The climate.** The sub-classes rest on the pedoclimate where they can. A climatic sub-class needs its climate and the morphology of one of its groups.
  * The climate is read from a new site field, `koppen_climate` (the Köppen code), else from the USDA soil temperature and moisture regimes, else from the latitude.
  * With no climate recorded, the soil falls to the class's non-climatic or temperate sub-class, and the trace says so.
  * A second new field, `mean_annual_precip_mm`, is recorded but decides nothing yet.
* **The other sub-classes** read the profile:
  * the external drainage of the vertisols, from the slope or the landform;
  * the organic matter of the andosols;
  * gypsum and carbonate in the A for the calcimagnesic soils;
  * hydromorphy reaching the A2 or the top of the B for the podzolised soils;
  * the base saturation of the B for the sesquioxide and ferrallitic soils;
  * the peat and the anmoor for the hydromorphic soils;
  * the structure for the sodic soils.
* The app and the report label the new level "Subclass".

### Hugo's decisions (2026-11-01)

* **Climate:** Köppen first, then the USDA regimes, then the latitude. With no climate, the default sub-class. The isohumic soils, whose four sub-classes are all climatic, stay undecided unless the property test of VI-1 holds.
* **Classes I and II:**
  * I-2 cold deserts and I-3 hot deserts;
  * II-2 humic: 5 % OM or more in the A, pH under 6.8, not arid;
  * II-3 xeric: a dry climate, under 2 % OM, no calcareous accumulation.
* **Classes III and IV:**
  * III-1: a slope under 2 % or a flat landform; nothing recorded leaves it undecided;
  * IV-1: OM above 10 % in the A over 25 cm.
* **Classes V, VII and VIII:**
  * V-3 gypseous first, then V-1 carbonated, then V-2;
  * VII-4 tropical, VII-3 boreal and VII-2 continental, each with the morphology of its groups;
  * VIII-3 hydromorphic, then VIII-2 cold.
* **After the review:**
  * every Köppen climate falls in one isohumic sub-class (VI-1 Cf, Dfa, Af, Am; VI-2 the other D, BSk, BWk, E; VI-3 Cs; VI-4 BSh, BWh, Aw, As, Cw);
  * VII-1 and VIII-1 hold under a temperate climate or none recorded; under another recorded climate, a soil without the morphology of the climatic sub-class is undecided rather than named temperate.
* **Classes IX to XII:**
  * IX-1 when base saturation is under 65 %;
  * X by the thickness-weighted base saturation of the (B), at 40 % and 20 %;
  * XI-1 peat, XI-2 anmoor;
  * XII-2 by the sodium route.

# soilKey 0.9.330 (2026-11-01)

## France: the twelve classes of the French soil classification (CPCS 1967)

`classify_cpcs1967()` keys a pedon to the twelve classes of the French soil classification of 1967 (Commission de Pédologie et de Cartographie des Sols, *Classification des sols*), a system of its own beside the other national systems. `make_cpcs1967_example()` gives one profile per class. The names are French, as the text writes them, in `name` and `levels$name_native`. The sub-classes, groups and subgroups follow.

* **An order of classes.** The CPCS gives no key. soilKey keys the classes in an order built from the text's statements of precedence: I sols minéraux bruts, III vertisols, XII sols sodiques, IV andosols, VIII sols podzolisés, X sols ferrallitiques, IX sols à sesquioxydes de fer et manganèse, XI sols hydromorphes, II sols peu évolués, VI sols isohumiques, V sols calcimagnésiques, VII sols brunifiés.
* **The old French notation** (Aoo, Ao, A1, A2, A3, (B), B, Bh, Bfe, Bt, Cca, Can, Cs, Cn, Sa, g, G, R, a Roman numeral for a change of parent material), declared with `site$designation_system = "cpcs"` or recognised by its own symbols, beside the FAO letters. In a profile without designations, soilKey's diagnostic horizons stand in for the profile type.
* **The class tests** read organic matter as 1.72 × organic carbon, with depths from the top of the first mineral horizon:
  * the intensity of hydromorphy and halomorphy that makes classes XI and XII;
  * Ki (or the CEC of the clay) for the ferrallitic soils, read in the larger part of the B;
  * the podzolic B by its C/N;
  * the andic properties with a (B);
  * the sharp or gradual fall of organic matter that separates the calcimagnesic soils from the isohumic soils.
* In the Pro app, the CPCS is a national system of its own; the level is labelled "Class".

### Hugo's decisions (2026-11-01)

* The order of the classes above. Class XI excludes permafrost soils (they stay in class II); the sesquioxide soils exclude the isohumic soils.
* **Class XI:** peat (30 % OM, 20 % in sandy material) over 40 cm of the top 80 cm; 8 to 30 % OM over 20 cm on a gley; or hydromorphic layers over most of the profile from the base of the A1 to 100 cm. A deep gley alone does not count, and a mottle that is not recorded is absent.
* **Class XII:** EC above 7 dS/m in every horizon to 100 cm, or Na above 10 % of the CEC with a massive or columnar, compact structure. Na only; clean sands and gypsum alone excluded.
* **Conventions:** OM = 1.72 × C; depth zero at the top of the A1; ranges read as in the SiBCS; data first, the notation where the data do not decide.
* **Names:** French in both name fields.
* **After the canonical profiles** (the 44 WRB, SiBCS and USDA examples keyed under the CPCS):
  * class IX's individualised iron is a reddened B (5YR or redder, chroma 4) or iron concretions, a yellower B only under a tropical climate;
  * the gradual fall of OM of class VI is read at a third, and class V drops its own fall test (the isohumic soils are keyed before it);
  * the EC of class XII is read in the solum;
  * the cracks of class III are 0.5 cm, as soilKey reads the WRB.
* **Classes I, II and IV:**
  * class I has traces of OM, 0.5 % (1.5 % in the top 3 cm);
  * class II has permafrost, or an AC profile with more than traces;
  * an AC soil with the chemistry of class V and a source of Ca or Mg is calcimagnesic unless a recent deposit or recent erosion is recorded;
  * class IV needs the andic properties over 30 cm, a CEC above 25 (the thickness-weighted mean of the andic A and (B)) and a (B).
* **Classes III and VIII:** the CPCS's own vertisol reading (slickensides to 150 cm, 30 % clay in the (B)); the podzolic B by criterion 2 (OM above 0.5 %, C/N above 14), or an ashy A2 without a B.
* **Classes X, IX, V, VI and VII:**
  * X: Ki of 2.0 or less, else a CEC of the clay of 16 or less;
  * IX: Ki above 2, base saturation above 50 % and individualised iron;
  * V: no B, pH above 6.8 and Ca + Mg above 90 %;
  * VI: a gradual fall over half the solum, OM minima by texture, S/T of 50 % or more in the upper horizons and 80 % in the B (p. 39);
  * VII: the remaining soils with a B or (B).
* **Supporting readings:** the ashy A2 is albic material; in an undesignated profile, the B is read from soilKey's diagnostics.
* **After the reviews:**
  * the notation gives the profile type (B, (B), A2), and the recorded data the properties: a g with 0 % mottles recorded is not a hydromorphic layer;
  * class VII takes the soils with an A2 and no B;
  * a peat excludes the mineral classes keyed before class XI, and class I takes no organic layer (an organic layer counts as more than traces for class II);
  * the Canadian, New Zealand and Australian letters keep their own meanings (f iron, v vertic, z frozen), and the recorded data come before the Ca, Cs and Cn of the notation.

The other systems do not change.

# soilKey 0.9.329 (2026-11-01)

## Japan: the soil phases of the Comprehensive Soil Classification System of Japan (the CSCJ now complete)

`classify_cscj2011()` now gives the soil phases of chapter VIII in `ClassificationResult$phases`, beside the class name, as `classify_scsj2017()` does. With them, the CSCJ 2011 is complete in soilKey: 10 great groups, 27 groups, 116 subgroups, 381 soil series groups and the phases.

* **Temperature phase:** from the soil temperature, else the USDA regime, else estimated from the mean annual air temperature plus 2 C, as the CSCJ says.
* **Man-made phases**, with the CSCJ's names:
  * a fill is named by its texture or, for a kuroboku fill, its humus class, and by its kind of soil or material (多腐植質黒ボク土盛土造成相, 粗粒質浚渫土砂盛土造成相, as the text's examples);
  * the Reformed soils: the fill over the original soil's group (細粒質低地土／粘土集積赤黄色土造成相).
  * a fill in the plough layer alone takes no phase (chapter IX, step A);
  * a fill with none of the five kinds that the materials name is keyed on its own and named by its great group when 25 cm or more, by its texture alone when thinner;
  * a kuroboku fill on other material is a natural combination (chapter IX, step D; also on #133): the text's example keys as 腐植質下層低地アロフェン質黒ボク土 with the phase 腐植質黒ボク土盛土造成相.
* No forest phases: the CSCJ has none.

### Hugo's decisions (2026-11-01)

* The air temperature plus 2 C for the CSCJ temperature phase, marked as estimated.
* A fill's name puts the texture word, or the humus class of a kuroboku fill, before the soil kind; for the Reformed soils, the fill's texture and great group over the original soil's group.
* A kuroboku fill is a natural combination in the CSCJ only: it does not make the soil Man-made (on #133 and merged up the stack).
* A fill with none of the five kinds: 25 cm or more is classified on its own and named by its great group; a thinner one is named by its texture word, when decided.

The SCSJ 2017 and the other systems do not change.

# soilKey 0.9.328 (2026-11-01)

## Japan: the 381 soil series groups of the Comprehensive Soil Classification System of Japan

`classify_cscj2011()` now keys a pedon down to its soil series group (土壌統群), the CSCJ's lowest category, for example "fine-textured Reddish Argic Red-Yellow soils" / 細粒質赤色粘土集積赤黄色土.

* **Lists.** Each subgroup has its own ordered list, drawn from the text's vocabulary:
  * bedrock, a gravel layer or a pan layer within 60 cm;
  * the texture of the subsurface;
  * peat, alluvial or kuroboku layers in the subsoil, and the non-allophanic feature;
  * the humus classes;
  * the mineral layers and fibre of the peat soils;
  * the base-rich dark surface horizon;
  * wetness.
* **New horizon attributes:** `texture_class_jp` (the Japanese texture classes, codes or words) and `hardness_yamanaka_mm` (soil hardness, for the pan layer).
* **App and report** label the new level "Soil series group".

### Hugo's decisions (2026-11-01)

* Series groups are named by their term, in lower case, before the subgroup's name.
* **Bedrock, gravel and pan layers:**
  * bedrock and the gravel layer are measured from the soil surface;
  * a gravel layer is 20 % or more in either record; with no record, there is no gravel layer;
  * hardness not recorded means no pan layer.
* **Texture:**
  * it is read from `texture_class_jp`;
  * without it, clay of 15 % or more is fine, and medium vs coarse stays undecided;
  * it is read on the horizon of 15 cm or more with the largest overlap of 20 to 40 cm; a tie goes to the larger part of the section, then to the thicker horizon;
  * with no subsurface, no texture class is met.
* **The base-rich dark surface horizon** is undecided where liming is recorded.

The SCSJ 2017 and the other systems do not change.

# soilKey 0.9.327 (2026-11-01)

## Japan: the 116 subgroups of the Comprehensive Soil Classification System of Japan

`classify_cscj2011()` now keys a pedon to its subgroup (chapter VI), named by its word before the group's name, for example "Argic Calcaric Dark Red soils" and 粘土集積石灰性暗赤色土.

* **Subgroups shared with the SCSJ** are its generic gates, read by the CSCJ edition under CSCJ names with the same depths and features. Within them, the Andic subgroups have P retention as their only substitute, and the Endoaeric Lowland Paddy soils read the CSCJ's yellowish brown.
* **New or different:**
  * the Cumulic Fulvic Andosols, by the brown polyhumic surface horizon over 50 cm within 60 cm;
  * the Low-humic Allophanic Andosols;
  * the Dark Red groups' Argic and Haplic subgroups;
  * the Brown Forest soils in the CSCJ's order, with Aquic by either wetness, Eutric on every subhorizon at pH 6.5 and the new Terrace subgroup.

### Hugo's decisions (2026-11-01)

* Terrace by recorded words for Pleistocene deposits, negations taken out, a Holocene or alluvial terrace and a rock not counting; nothing recorded means not.
* Eutric as written, with no liming clause.

The SCSJ 2017 and the other systems do not change.

# soilKey 0.9.326 (2026-11-01)

## Japan: the 27 groups of the Comprehensive Soil Classification System of Japan

`classify_cscj2011()` now keys a pedon to its group (chapter VI): for example the Fulvic Andosols, the Calcaric, Dystric and Eutric Dark Red soils, and the Brown Lowland soils.

* **Groups shared with the SCSJ** are its gates, read by the CSCJ edition under CSCJ names.
* **New groups and readings:**
  * the Fulvic Andosols, by the brown polyhumic kuroboku surface horizon: layers with the kuroboku feature, a moist value and chroma of 3 or more and 4 % carbon or more each, 30 cm in 40 cm, with a weighted mean of 6 % or more;
  * the Calcaric and Dystric Dark Red soils;
  * the Brown Lowland soils, by the CSCJ's own yellowish-brown colour, which leaves out 5YR.

### Hugo's decisions (2026-11-01)

* The brown polyhumic horizon adds up the layers that meet it, with no allowance for layers that fail between them.
* The Brown Lowland soils use the CSCJ's yellowish brown.
* Where liming is recorded, the Dystric and Calcaric Dark Red groups are undecided (the soil keys to the Eutric group, provisional).
* The Non-allophanic Andosols by the key alone.
* The Volcanogenous Regosols' undeveloped horizons leave buried horizons out, in both Japanese systems (the SCSJ change is on #130 and merged up).
* With no subsurface section, a criterion on its subhorizons is not met, in both systems (on #129 and merged up).

The appendix's typos "Fuluvic" and "Volcangenous" are written Fulvic and Volcanogenous. The other systems do not change.

# soilKey 0.9.325 (2026-11-01)

## Japan: the Comprehensive Soil Classification System of Japan, the ten great groups

`classify_cscj2011()` keys a pedon to the ten great groups of the Comprehensive Soil Classification System of Japan, First Approximation (Obara et al. 2011, NIAES), a system of its own beside the SCSJ 2017. `make_cscj2011_example()` gives one profile per great group. Names are the English names of the text's appendix 2, with the Japanese ones in `levels$name_native`. The 27 groups, 116 subgroups, 381 soil series groups and the phases follow.

* **Shared code, the CSCJ's own limits.** The diagnostics the two Japanese systems share are read with the SCSJ code, by the CSCJ edition:
  * the argic horizon by the CSCJ's clay increase (the WRB 2006 values);
  * the immature kuroboku feature with P retention as its only substitute;
  * the heterogeneous fill of the Man-made soils by the CSCJ great groups, without the SCSJ's Regosol-group rule;
  * a fill that keys as Andosols is a combination that occurs in nature (chapter IX, step D), so it does not make the soil Man-made.
* **Podzols before Andosols**, as the CSCJ keys them.
* **New great group:** the Dark Red soils, by a dark red subsurface (the new colour word) or an origin from limestone or calcareous deposits with a base-rich subsurface.
* **The CSCJ's Red-Yellow soils**, without the SCSJ's base-status criterion.
* In the Pro app, the CSCJ is a national system of its own.

### Hugo's decisions (2026-11-01)

* The soil surface, which the CSCJ does not define, is read as in the SCSJ.
* The CSCJ's own argic limits.
* The subsurface section excludes the plough layer, as in the SCSJ.
* The Dark Red colour is that of the thickest B horizon in the subsurface section (a buried B included), else the first B below it, else the thickest layer of the section.
* Limestone or calcareous deposits come from recorded words or 40 % CaCO3 or more; nothing recorded means not. Liming leaves a high base status undecided.
* The Podzols: one horizon may meet both the analysis and the morphology criteria; the single parent material is read on the bleached and accumulation horizons.
* The Red-Yellow soils' 25 cm are those of one horizon, in the CSCJ and in the SCSJ alike.
* A kuroboku fill is a natural combination in the CSCJ only: the text's example of a humic kuroboku fill on alluvium keys as an Andosol, not as a Man-made soil. The SCSJ still reads every recorded fill as not natural.
* Decided for the later parts:
  * a Japanese texture-class attribute;
  * the series-group names with their term first;
  * the air temperature plus 2 C for the CSCJ temperature phase;
  * the texture layer in 20 to 40 cm and the non-allophanic test by the key.

The other systems do not change; the SCSJ changes only in its Red-Yellow soils' count (also on #129).

# soilKey 0.9.324 (2026-10-31)

## Japan: the soil phases of the Soil Classification System of Japan

`classify_scsj2017()` now gives the soil phases of chapter V in `ClassificationResult$phases`, a new field (kind, code, English and Japanese name, basis), which `print()` lists. The class name is left as it is.

* **Temperature phase:** frigid, mesic, thermic, hyperthermic, from the mean annual soil temperature or the USDA soil temperature regime.
* **Man-made phases:** mixed-layer, cut and fill phases for the other great groups, the fill named by its materials; the gley, wet, gravelly or plain phase of the Artifactual soils, named by the artificial material; the fill over the original soil, with the same endings, for the Reformed soils.
* **Forest phases**, on forest or grazing land: the 1975 forest soil type when recorded (new site field `forest_soil_type`, read in any spelling and for the other 1975 groups by their moisture letter); otherwise the three development phases of the Podzols on the key's own pairings (new horizon attribute `leached_spots`), and the seven moisture phases of the other forest soils from soilKey's reading of their descriptions.
* A phase is given only when it is decided: a test earlier in its order that the data leave undecided gives none.

### Hugo's decisions (2026-10-31 and 2026-11-01)

* The phases sit beside the class name, not in it.
* The temperature phase reads the soil temperature or the USDA regime, with no estimate from the air.
* One man-made phase for the Man-made soils: gley before wet before gravelly; the Reformed soils take it too, as the supplement's example.
* The forest moisture phases: the recorded type, else rules from the structures, undecided when ambiguous. Slightly wet: no O, a crumb A of 25 cm or more, a B without blocky, nutty or granular structure. Moderately dry: nutty structure in the lower A and the upper B. Moderately moist, drier type: granular upper A or nutty lower A, no crumb required. Angular blocky is nutty.
* The 1975 types of the other groups are read by their moisture letter.
* A thin fill is named by its materials; mixing counts only below the plough layer; land consolidation alone is not a cut.
* Undecided data give no phase.

Other systems do not change.

# soilKey 0.9.323 (2026-10-31)

## Japan: the 120 subgroups of the Soil Classification System of Japan

`classify_scsj2017()` now keys a pedon to the subgroups of chapter III: 120 under the 26 groups (Haplic, Humic, Cumulic, Brown-humic, Thapto-humic, Thapto-upland, Endofluvic, Aquic, Peaty, Anthraquic, Albic, Epi-gleyed, Thionic, Strong and Mottled, Reddish, Andic, Podzolic, Eutric, Calcaric, Granitic, Para-lithic ...). Each is named by its word before the group's name, in English and in Japanese (`levels$name_native`; the generic classifier now fills `{parent}` in the native name too).

* New horizon attributes: `ph_h2o2` (potential sulfuric material), `peat_moor_type` (high-moor, transitional or low-moor peat) and `artefacts_organic` (mainly organic artificial material).
* The subgroups also read the humic and polyhumic surface horizons, the buried humic layer, the buried non-kuroboku upland soil, the paddy reverse gley layer, jarosite, carbonates, the site's landform and groundwater depth, and the parent rock.
* Hugo's decision (2026-10-31): the mottle class 'contains' of the Strong Gley Fluvic soils, for which the text gives no figure, is 5 % or more.
* On paddy land the Aquic subgroups do not read the paddy horizons' mottles as groundwater wetness, as the groups do; the groundwater gley layers are adjacent gley horizons reaching 75 cm.
* Hugo's decisions (2026-10-31): the Brown-humic colour is a moist value and chroma both 3 or more (chapter VI); a paddy greyed horizon within 30 cm rules the Aeric Pseudogley soils out; the Endoaeric brown layer is below the plough layer and the surface horizons; the Podzolic Brown Forest soils read the podzolic accumulation horizon as the text gives it (its colour suffices).
* Corrections to the shared definitions: "at A to B cm" windows no longer pass a layer that may start above A; the humic surface horizon starts at the soil surface and reads gaps and the end of the description as possible; a buried humic layer may be several horizons; an H horizon with carbon but no kuroboku data is peat material; a buried plough layer is not the plough layer; Japanese drainage classes are read.

The readings are in `inst/rules/scsj2017/readings.yaml`. Other systems do not change.

# soilKey 0.9.322 (2026-10-31)

## Japan: the 26 groups of the Soil Classification System of Japan

`classify_scsj2017()` now keys a pedon past its great group to the 26 groups of chapter III, among them Podzolic, Regosolic, Gleyed, Wet, Non-allophanic and Allophanic Andosols, Fluvic Paddy, Gley, Gray, Brown and Regosolic Fluvic soils, Argic and Cambic Red-Yellow soils, Stagnogley and Pseudogley soils, Magnesian and Calcareous Eutrosols, and Volcanogenous and Sandy Regosols, Lithosols and Terrestrial Regosols. Each has its English and Japanese name.

* New horizon attribute `exchange_acidity_y1` (the exchange acidity y1 of the non-allophanic kuroboku feature, not read on farmland). The groups also read Sio (`si_ox_pct`), Alp/Alo (`al_pyro_pct`, `al_ox_pct`) and exchangeable Ca and Mg.
* The gley layers are subdivided as the text does (groundwater gley layers reach 75 cm or deeper, taken together).
* Man-made soils: a fill and the soil under it that both key to Regosols are compared by their groups; a part of a profile keyed on its own is read as complete.
* Corrections to 0.9.321: a layer with the kuroboku feature no longer also has the immature one (an ash layer that is certainly one of the two still counts for the Andosols); a fill keeps the material it records, so a fill of the same material is read as part of an ordinary profile (chapter VI, the fill phase); colour words decided by value or chroma alone; a run's certain depth and the end of the description read in the depth tests.

### Hugo's decisions (2026-10-31)

* The podzolic accumulation horizon's criterion (2) is read for the soil, as its verb says (as in 0.9.321).
* "Several gley layers" are adjacent gley horizons taken together; an oxidised layer separates two gley layers.
* On paddy land the paddy horizons' mottles are not the groundwater wetness feature of the Wet Andosols and Gray Fluvic soils.
* The mottle class "contains" (the Strong Gley Fluvic soils, next release) is 5 % or more.
* Later (2026-11-01): the Volcanogenous Regosols' undeveloped horizons leave buried horizons out.

The readings are in `inst/rules/scsj2017/readings.yaml`. Other systems do not change.

# soilKey 0.9.321 (2026-10-31)

## Japan: the great groups of the Soil Classification System of Japan (2017)

A new, experimental and opt-in system: the Soil Classification System of Japan (SCSJ 2017) of the Japanese Society of Pedology, Fifth Committee for Soil Classification and Nomenclature. This release keys the ten great groups of chapter II with the definitions of chapter IV; groups, subgroups and the soil phases follow, then the Comprehensive Soil Classification System of Japan (2011) as a system of its own.

* **`classify_scsj2017()`** keys a pedon to Man-made soils, Organic Soils, Andosols, Podzols, Fluvic soils, Red-Yellow soils, Stagnic soils, Eutrosols, Brown Forest soils or Regosols. Each level gives the official English name of the SCSJ table and, in `levels$name_native`, the Japanese one (a new column that the generic classifier fills for any system whose classes carry `name_native`).
* **`make_scsj2017_example()`** gives one made-up profile per great group.
* The key reads the Japanese horizon notation of the Soil Survey Handbook (H, O, A, E, B, C, G with Go and Gr, R; p, g, t, w, s, h, ir, mn; a 2 or II prefix; a trailing b), colours by the colour words of the text, the subsurface control section (20 to 60 cm), paddy land from `site$land_use` or `site$flood_irrigated`, and the recorded origin of the layers (fill, volcanic ejecta, alluvial deposits).
* New horizon attributes: `phosphate_absorption_coef` (mg P2O5/100 g; the kuroboku feature needs 1500 or more), `active_al_test` (the Japanese field active-aluminium test, -, +-, +, 2+, 3+) and `n_value` (physical immaturity). The dipyridyl test is the existing `childs_test`.

### Hugo's decisions (2026-10-31)

* The SCSJ 2017 complete (great groups, groups, subgroups and phases), then the CSCJ 2011 as a system of its own.
* English names with the Japanese ones beside them.
* The phosphate absorption coefficient is a new attribute; P retention (85 %) or Alo + 1/2 Feo (2.0 %) replace it when it is not measured, as the text allows, and the field active-aluminium test after them.
* Paddy land is read from the land use; with none recorded the paddy horizons are undecided, not absent.
* Podzols: the bleached horizon must lie above the podzolic accumulation horizon, the sequence of the great group's concept.
* The subsurface section (20 to 60 cm) counts the horizons with 5 cm or more inside it.
* An undesignated layer of 20 % carbon or more is peat on a poorly or very poorly drained site and a forest floor on any other; undecided without a drainage class.
* In the forest-floor notation (L, F, H), an H under L or F is the humus layer of the floor, not peat.
* Later (2026-11-01): the Red-Yellow soils' 25 cm are those of one horizon, not added up over separate horizons; with no subsurface section, neither base-status criterion is met; a gravel layer (in either record) that reaches into the subsurface section cuts it at its top.

The choices where the text is silent are in `inst/rules/scsj2017/readings.yaml`. Other systems do not change: the 44 canonical profiles key as before under WRB, SiBCS and USDA, and the Canadian, Australian, New Zealand and Hungarian examples as before.

# soilKey 0.9.320 (2026-10-30)

## Australia: Queensland's SALI read from its site reports and its soil chemistry API

Queensland's Soil and Land Information system (SALI) publishes about 21,000 sites with the surveyors' ASC label and laboratory data, anonymously and under CC BY 4.0 (State of Queensland). Its morphology exists only in a per-site report (a PDF), so the reader reads the PDF. The data are not shipped with soilKey.

* **`fetch_sali()`** downloads the site layer (Queensland Spatial, MapServer layer 101: the sites with an ASC label and laboratory data), then per site the report (SALI4320) and the samples with their results from the SALI soil chemistry API (OData). It is resumable, waits between requests, retries with a backoff, stops on any login, key, terms or firewall page, and talks only to the layer and, per site, to the report host and the API.
* **`read_sali_pedons()`** reads the folder into pedons. The report is read from the coordinates of its words (pdftools, in Suggests), and each row of its morphology table is translated into the coded horizon of the NatSoil API and read by the NatSoil reader's own code, the laboratory samples too, so the two Australian panels are read alike. The provenance has a row per horizon for the morphology and one per laboratory value, naming its source and method. The site fields it adds (`sali_*`) are listed in `site_attribute_spec()` and `inst/ATTRIBUTES.md`.
* **`benchmark_sali()`** gives the metrics of `benchmark_natsoil()` (which it calls), with a table by surveyed order and one by the ASC edition the report cites. **`sali_citation()`** gives the CC BY 4.0 attribution.

### Hugo's decisions

* **Reports over 150 KB are not downloaded** (`cap_bytes = 150e3`): those with embedded site photographs are aborted at the cap, marked and not tried again.
* **MIR predictions** (method codes ending in _MC2, _MC91, _MQ01, _MQ02 and the like) are used only where nothing was measured for that attribute in that horizon. The provenance marks them as predictions (source `predicted_spectra`, a note naming the MIR codes), and the evidence grade of such a pedon is B. The pilot left them out. MIR methods the NatSoil reader has no code for are read by the property their name gives: 15C2_CEC ("Cation exchange capacity - MIR QLD Calibration 1") as the CEC, 6B4 (total organic carbon) as organic carbon and 15J2 as the effective CEC.
* **The lab table printed in the report** is used only when the API holds no samples for the site's observation; the provenance says which of the two each value came from.

### The readings (the pilot's, kept)

* Sites labelled "Unclassified" are left out; observation 1 of a site is read.
* The label is read by position (order, suborder, great group, subgroup), each name matched, case-insensitive, to the 3rd edition. A name the 3rd edition does not have stays NA; "No Available Class" is ZZ and "Class Undetermined" YY. The edition the report cites is kept in `site$sali_asc_edition`, without changing the codes. The report's confidence phrases are 1 to 4; conversions from another classification and rapid checks of key attributes are "C".
* An empty cell is not recorded; "No mottles", "No segregations" and "No coarse fragments" are recorded absent. A colour without its moisture status is moist. Mottles of biological mixing, mechanical mixing and substrate influence are not mottles; jarosite mottles are.
* Textures: "Loam, fine sandy" is a loam, sandy and silty clays are clays of unspecified grade, and heavy or light before a grade lighter than a clay is the + or - qualifier. The first structure listed is read; "Blocky" alone names no type; "Not recorded (historical)" is NA. A strength without its moisture is not read.
* Field pH by depth: Raupach and Tucker first, then an electrode in situ, then a 1:5 suspension by probe; the pH after peroxide and the field EC are not used. The effervescence field test gives the horizon's effervescence.
* Laboratory samples go to the horizon holding their middle, then NatSoil's overlap rule applies; bulked samples count; results flagged invalid are dropped; "<x" is x/2 with NatSoil's limit rule. A trailing letter of a method variant is dropped (6B2b is 6B2), and 2Z2_Clay is 2Z2_C.
* Tidal landform spellings are NatSoil's; no tidal flooding is inferred, nor the soil water status. The depth to free water and the substrate are kept in the site but not used.

### The run (6,911 sites, 5,600 pedons)

Of the layer's sites with an ASC order, 18,557 are not in NatSoil already (same project and site, or a NatSoil site within 100 m). They were taken in a fixed random order, in stages of 2,000. The run stopped in the fourth stage, when the report server began refusing requests (after some 14,000, from 2 processes pausing 1 s): 6,911 sites were fetched. Reports over 150 KB (those with embedded site photos) were not downloaded: 1,286 sites (18.6%), unevenly by order (Organosols 82%, Calcarosols 41%, Dermosols 29%, Tenosols 27%; Vertosols 11%, Hydrosols 15%). 25 reports have no morphology table, which leaves 5,600 pedons. Against the surveyors' labels:

| ASC level | Profiles compared | Agreement | Where the key decides |
|---|---|---|---|
| order | 5,600 | 34.8% (36.6% counting a keyed Arenosol for a surveyed Tenosol or Rudosol) | 83.3% of 1,017 |
| suborder, where the order agrees | 1,854 | 82.6% | 91.8% of 571 |
| great group, where the suborder agrees | 1,106 | 42.8% | 73.5% of 340 |
| subgroup, where the great group agrees | 446 | 40.1% | |

The order agreement stayed within 34% to 35% from the first stage (1,588 pedons) to the last. The Hydrosols (1,377 pedons) agree in 4.9%, mostly keyed as Kandosols: the key confirms a Hydrosol by its drainage, which the reports seldom record (the depth to free water, observed once, is kept but not used). The Ferrosols (156) agree in 11.5%; most of the rest are keyed as Dermosols, as free iron was measured in only 30 of them. The laboratory data come from the API for 5,008 pedons and from the report's table for 562; 30 have none. The MIR predictions fill 243 values in 30 pedons. A MIR water pH never stands beside a measured CaCl2 pH; two MIR calibrations behind one value occur in 4 pedons (16 values).

A pilot of 200 sites stratified by order (186 pedons) gave 37.6% at the order and 83.3% of 36 where the key decides. For comparison, the Queensland profiles of NatSoil (agencies 301, 302, 304 and 399; 4,412 profiles) give 54.3% at the order, 82.6% of 783 where the key decides. The 44 canonical fixtures are byte-identical under WRB, SiBCS and USDA.

### Ferrosols: vertic properties that are not recorded

* The Ferrosol test keeps a soil out when at least 0.3 m of its B2 has vertic properties. A B2 horizon whose vertic properties are not recorded now counts as without them; only recorded slickensides or lenticular peds keep a soil out. Slickensides are seldom recorded (Queensland's site reports have no column for them; NatSoil reads them as absent only when other cutans are recorded), so a measured free iron above 5% in the major part of the B2 had decided the order in 1 of 30 SALI Ferrosols and 2 of 35 NatSoil Queensland ones. The trace gives the thickness read this way (`vertic_unrecorded_b2_cm`), and the test names slickensides as missing only when recorded vertic properties leave the answer open.
* SALI: 18 pedons change to Ferrosols, 17 of them surveyed Ferrosols and 1 a Kandosol (order 34.5% to 34.8%). NatSoil: 19 change, 16 surveyed Ferrosols and 3 Kandosols (all 9,700 profiles 43.9% to 44.0%; Queensland 54.0% to 54.3%).
* Gley colours and mottles were looked at for the Hydrosols and left out of the key. The ASC says they may or may not be present and are not universally indicative of saturation. A reading by low chroma, gley colours or mottles in most of the upper metre would raise SALI (where a quarter of the pedons are Hydrosols) to 52.9% but lower NatSoil Queensland (where 2% are) from 54.0% to 52.1%.

# soilKey 0.9.319 (2026-10-30)

## New Zealand: the fluvial features read river deposits and the spelling aluvium

* **NZSC fluvial features** (Fluvial Recent and Fluvial Raw Soils): a recorded origin naming a river deposit (river, rivers, riverine, riverbed) is fluvial, and alluvium is also read in the spelling aluvium, both as NSD parent-rock notes write them (Hugo). A place name such as Riversdale is not read as a river.
* **WRB Raptic** reads riverine and riverbed like river.
* On the NSD (94 surveys) the NZSC group agreement goes from 26.7% to 27.0% (303 to 306 of 1,134). Of the 73 profiles the surveyors call Fluvial Recent Soils, 37 are keyed as such (34 in 0.9.318). Order (43.6%) and subgroup (14.7%) are unchanged, as are WRB and USDA on the NSD and the 44 canonical fixtures.

## Australia: the agreement on NatSoil, measured again

The Australian key has been measured since 0.9.278 on CSIRO's NatSoil (`benchmark_natsoil()`, profiles with the surveyor's ASC label, CC BY 4.0, not shipped). On 9,700 profiles with the current code no prediction has changed since the last measurement (2026-10-27), at any level:

| ASC level | Profiles compared | Agreement | Where the key decides |
|---|---|---|---|
| order | 9,700 | 43.9% (45.8% counting the Arenosols, absent from the older labels, as Tenosols or Rudosols) | 74.5% of 1,305 |
| suborder, where the order agrees | 3,694 | 75.9% | 95.5% of 694 |
| great group, where the suborder agrees | 2,669 | 32.8% | 77.5% of 413 |
| subgroup, where the great group agrees | 852 | 33.3% | 25.0% of 20 |

# soilKey 0.9.318 (2026-10-30)

## The New Zealand National Soils Database read by Hugo's readings

### NSD reader (`read_nsd_pedons()`)

* **The older ped term nut** (and nutty) is granular or polyhedral, one of the two, not known which (NZ Soil Description Handbook 2024, Note 12.1; Milne et al. 1995 has no nut). It goes to a new horizon attribute, `structure_type_one_of` ("granular or polyhedral"), and `structure_type` stays empty.
  * `classify_nzsc4()` reads a criterion under each shape: it holds when it holds for both, fails when it fails for both, and is undecided otherwise. A criterion that needs polyhedral peds is undecided for a nut horizon.
  * The other keys read the shape as not recorded.
* **The older strength words** moderately weak, moderately firm and moderately strong belong to the Taylor and Pohlen (1962) scale, which has no class in Milne et al. (1995). They stay unread (undecided).
* **The older notation**, for designations the NSD has not converted itself:
  * A2 is E, with its subdivisions and suffixes (A21 E1, A2g Eg, A22x Ex2).
  * The old master G (a gley horizon) is Bg when a B horizon lies below it in the profile, otherwise Cg (GC: BCg or Cg).
  * The old master D (the substratum) is C, its prefixes and suffixes kept (BD BC, DG Cg, Dh Ch), except its old suffixes r and n. The NSD's own conversions give Dr as Cu or C and Dn as Bt, 2Btb, Cm or Cu, so neither is the r (reduced) or n (sodium) of the current notations: Dr and Dn are C.
  * The provenance keeps the recorded designation and says what it was read as.
* **The soil note PARENT ROCK** goes down to the layers whose origin is not recorded (`layer_origin`, read by the NZSC fluvial features); a layer with its own origin keeps it.
  * A note of one material goes to every such layer.
  * A note naming a sequence ("loess over outwash gravels"; over, overlying or on, or under and underlain by read from the bottom) is split by the profile's lithologic groups (2C, IIC) when its parts are as many: the first part to the unnumbered layers, the next to the next group. Otherwise it stays the site's `parent_material` and goes to no layer. A slash is not read as a sequence: the NSD also writes it in mixtures and abbreviations.
* A survey whose soil table has no `classifier_usda` (or `classifier_nzsc`) column is read.

### WRB 2022

* **Raptic:** the recorded origin of the layers on each side of a lithic discontinuity also reads loess (and wind-blown material) as aeolic, volcanic ash, pumice, lapilli and scoria as tephric, and river and estuarine deposits (and alluvium spelt aluvium) as fluvic (Hugo). Such a discontinuity is not Raptic.

### soilKey Pro

* A loaded example without coordinates no longer keeps the previous profile's. The coordinates, country and parent material are the loaded profile's, and blank where it has none, as the moisture and temperature regimes already were. Coordinates feed the inferred temperature regime. The 44 WRB and SiBCS examples carry all four; the national examples carry no coordinates.

### Agreement on the NSD (94 surveys)

| NZSC level | 0.9.317 | 0.9.318 |
|---|---|---|
| order | 43.2% (492/1,139) | **43.6% (497/1,139)** |
| group | 24.3% (276/1,134) | **26.7% (303/1,134)** |
| subgroup | 13.8% (154/1,114) | **14.7% (164/1,114)** |

Of the 73 profiles the surveyors call Fluvial Recent Soils, 34 are now keyed as such (9 in 0.9.317). The 44 canonical fixtures are byte-identical under WRB, SiBCS and USDA.

# soilKey 0.9.317 (2026-10-30)

## The national systems in soilKey Pro, and their agreement measured

### soilKey Pro

* **The Classify tab offers the four national systems:** Canada (CSSC, 3rd ed.), Australia (ASC, 3rd ed.), New Zealand (NZSC, 4th ed.) and Hungary (HSCS 2024). They sit under WRB, SiBCS and USDA, each with its edition and an "experimental" label, and run only when ticked.
  * Each ticked system gets a card with the class at each level and its code (and the English gloss for Hungary).
  * A collapsed note lists the classes the key could not decide before the assigned one, with the attributes each needs.
  * For Hungary, a table lists the subtype and variety properties in the name and those undecided, with what would decide them.
  * The key trace and the Missing data panel include the national systems.
* **The Pedon tab's canonical profiles include one example per order (or type) of each national system:** 10 Canadian, 15 Australian, 15 New Zealand and 15 Hungarian, each keying to the class it is labelled with. Loading one ticks its system and keeps the site details its key reads (the horizon notation, the drainage).
* **The report and the session file:**
  * `report()`, `report_html()` and `report_pdf()` take `systems`. The default runs WRB, SiBCS and USDA as before (byte-identical reports); a registry id or alias adds that system.
  * A saved session keeps the ticked systems, and reopening it ticks them again.
* The WoSIS data-source line is shown only for WoSIS data, not for the site details a session or an example carries.

### Canada: designations in the FAO notation (CSSC)

* A profile that declares the FAO notation (`site$designation_system` "fao2006", "fao", "wrb") is read by the Canadian key through a translation of its designations where the letters mean the same. The source is the book's Table 6 (p. 154), checked against the definitions of Ch 2 and WRB 2022 Annex 3.
  * Masters: E = Ae, AE = Ahe, EB = AB.
  * B suffixes: Bw = Bm, Bs = Bf, Bhs = Bhf, Btn = Bnt.
  * Other suffixes: f (permafrost) = z, y and z (salts) = s, i = ss, m after a cementing agent = c, a 2 prefix = II.
  * The letters with no Canadian equivalent are not read, and what the FAO notation cannot write (the h of a plain A, the m of a plain B, the vertic v) stays undecided.
  * The designations are kept, and the evidence lists what each one was read as.
* A bare m (Bm, Cm) is not a cementation, as in WRB since 0.9.315.
* A fragipan (Bx) is no Bm: the m "lacks a brittle consistence when moist" (Ch 2, p. 14).
* `isis_profile_to_pedon()` declares the FAO notation, so the Canadian key no longer reads an FAO z (salts) as frozen.

### Hungary (HSCS 2024)

* **The clay ratio of the clay accumulation horizon (8.2)** passes over a transitional AB, BA, EB or BE horizon with no clay recorded (Hugo), and reads the row against the horizon above it.

### New Zealand: the National Soils Database

* **`read_nsd_pedons()`** reads the survey exports of Manaaki Whenua's National Soils Database (NSDR; zips or folders of CSV tables) in memory into pedon records. It reads:
  * the field description, decoded through each survey's code lists;
  * the laboratory samples, given to their horizons;
  * the provenance of every value;
  * the surveyor's NZSC class, NZ Genetic class and Soil Taxonomy name. The 3rd-edition codes are recoded to the 4th: MIW becomes MIC, and BLAM (withdrawn) is read at no level.
* The older notation is translated: (B) = Bw, and fe, ca, cs = s, k, y.
* **`benchmark_nsd()`** compares `classify_nzsc4()` with the surveyor's code at order, group and subgroup level.
* **`nsd_citation()`** gives the acknowledgement. The NSD is licensed CC BY-ND 4.0 and is not shipped.

### WRB and USDA

* **The frozen suffix follows the declared notation** (`cryic_conditions()` and the mollic criterion 6a):
  * FAO and USDA write a frozen layer f.
  * The Canadian notation writes it z (CSSC Ch 2, p. 16); its f is the Fe and Al of a podzolic B.
  * The New Zealand and Australian notations have no frozen suffix (their f is fibric peat and faunal accumulation).

  On the NSD, 12 New Zealand profiles with fibric peat had keyed to the Gelisols; 7 are now Histosols, as the surveyors named them.

### Agreement measured

| System | Profiles | Level | 0.9.316 | 0.9.317 |
|---|---|---|---|---|
| CSSC | ISIS Canada, 20 | order | 8 | **13** |
| | | great group | 5 | **9** |
| | | subgroup | 3 | **4** |
| HSCS | ISIS Hungary, 20 | type (genetic name via the guide's section 9) | 6 (8 lenient) | **7 (9 lenient)** |
| NZSC | NSD, 1,139 with a class | order | (no reader) | **43.2%** |
| | 1,134 | group | | **24.3%** |
| | 1,114 | subgroup | | **13.8%** |
| USDA | NSD, 600 with a class | order | | **38.5%** |

* On the NSD, 77% of the NZSC results are provisional at order level. Where every earlier order was decided (265 profiles), the order agrees on 72.8%.
* Most disagreements come from data the NSD does not hold: glass in sand (Pumice Soils), carbonates (Semiarid), bulk density (Allophanic) and moist strength (Pallic). Others come from old descriptions (agreement 26% in the 1960s surveys, 61% in the 1980s) and from 4th-edition criteria that older classes do not meet.
* WRB, SiBCS and USDA are unchanged on every panel, and the 44 fixtures keep their names. The new readings concern declared national notations and the national keys.

# soilKey 0.9.316 (2026-10-30)

## The Hungarian system (HSCS 2024) names its subtypes and varieties

* `classify_hscs2024()` now names, besides the 15 types, the subtype properties (altípus tulajdonságok, 8.5, p. 29-33) and the variety properties (változati tulajdonságok, 8.6, p. 34-36) of each type's table (section 7, p. 8-22), with the modifiers of 8.7 (p. 37). As the guide writes it (p. 7), the subtypes come before the type, the first with a capital letter, and the varieties after it, in parentheses, in the table's order. The guide's own example, "Savanyú, humuszszegény Agyagbemosódásos talaj (sekélyen kőzettörmelékes)", is a test.
* The tables are in `inst/rules/hscs2024/properties.yaml`. Each of the 74 properties is a three-valued function, and the result carries every property with its verdict, depth and label (`$qualifiers$status`) and one `levels` row per property named.
* **Readings decided by Hugo (2026-10-30):**
  * Every listed property whose definition holds is named. A row of degrees of one property (carbonate degrees, humus classes, base status, peat decomposition, sekély/mély, művelt/rigolozott) names only the strongest, and none when a stronger one is undecided.
  * A property defined by a layer takes the depth modifier of 8.7 (felszíntől, sekélyen, középmélyen, mélyben) from where its layer starts. The intensity words are used only where the text defines them.
  * The humus classes are read layer by layer. The surface horizon of humuszszegény, lápföldes and nyers is the weighted A horizon.
  * Rozsdabarna needs hue 10YR or redder and chroma 5 or more. Fakó is read in an eluvial horizon only. A g suffix shows the reductive conditions of pangóvizes.
  * In a Láptalaj every organic horizon is peat.
  * The measured CaCO3 decides the carbonate degree, and 15 % or more without secondary carbonates is közepesen.
  * Karbonát-szulfátos and -kloridos need carbonate making 20 % of the salts.
* The other readings where the text is silent are listed in `inst/rules/hscs2024/readings.yaml` (five new entries).
* **New attributes**, undecided when not recorded:
  * horizon: `soluble_salts_sulfate_pct`, `soluble_salts_chloride_pct`, `soluble_salts_carbonate_pct`, `p2o5_al_mg_kg`, `digo_pct`, `coarse_fragments_shape`, `secondary_carbonates_form`, `mottle_size_class`;
  * site: `contaminated`, `terraced`, `flood_irrigated`, `wind_deflation`.
* The Antropogén key reads the hortic route of the anthropogenic horizon (8.1, criterion 1) from `p2o5_al_mg_kg`. The ISIS reader reads the mottle size (`mottlesSizeclass`).
* **On ISRIC's ISIS profiles from Hungary, Slovakia and Romania** the names are what the data show. For example:
  * "Középmélyen erősen karbonátos, telített, sekélyen magnéziumos, kérges Szolonyec talaj (iszapos agyag)" for HU014, a Columnic Solonetz in WRB terms;
  * "Telítetlen, sekélyen podzolos, sekélyen fakó Agyagbemosódásos talaj (középmélyen duzzadó, sekélyen vörös, iszapos vályog)" for RO008.

  No property errs on the 974 ISIS profiles. The type of every profile is unchanged.

# soilKey 0.9.315 (2026-10-30)

## The last WRB 2022 and USDA readings of the ISIS triage

### WRB 2022

* **Gleysols, item 2 (Ch 4, p. 103):** a mollic or umbric horizon more than 40 cm thick, with reducing conditions from 40 cm below the mineral soil surface, over a gleyic layer 10 cm or more thick whose lower limit is at 65 cm or deeper.
* **Retisols (Ch 4, p. 118):** the argic horizon starts within 100 cm of the mineral soil surface and has retic properties at its upper boundary. A natric horizon no longer counts, since the text names the argic one only.
* **The limiting layer (Ch 2.1, rule 8, p. 23)** includes the other cemented layers. A recorded cementation class of moderately cemented or stronger counts, or the suffix m after a cementing agent (Bsm, Bkm, Bqm). Continuity is read as met, as for the petric horizons. A limiting layer has no texture class, so a shallow soil over rock no longer leaves Arenic, Clayic and Siltic undecided.
* **Mollic and umbric horizons:** criterion 6a, 10 cm or more when directly over continuous rock, technic hard material or the horizons the text names. Rock, technic hard material and petric layers end the run of mollic layers.
* **Qualifiers (Ch 5):**
  * Rendzic (p. 145): a mollic horizon that contains or directly overlies material or rock with 40% CaCO3 equivalent or more. Any layer of the mollic horizon counts. Rock recorded as limestone, chalk, dolomite or marble counts without a measured CCE (Hugo).
  * Calcaric material and Endocalcaric read the HCl reaction where CaCO3 is not measured. Endocalcaric and Endodolomitic count from the mineral soil surface.
  * Arenic and Clayic are read as Loamic and Siltic are: from the mineral soil surface, with the limiting-layer alternative and the sand limit of > 45% of Table 8.39.
  * Hypercalcic: 50% CCE or more in every layer of the calcic horizon (Ch 2.1, p. 23).
  * The texture group without specifiers gives its first applicable member. When an earlier member is undecided, which one is first is not known, so the group gives none (Hugo).
  * With `specifiers = TRUE`, the texture qualifiers of one group that apply at different depths are all given, each with its specifier (Ch 2.1, p. 24-26). The depth specifiers follow Ch 2.3.1 rules 1 and 2 (p. 27).
* **The Technosols lists** of Ch 4 (p. 97) are read as printed, in two columns.
* **ISIS reader, surface cracks:**
  * Medium cracks are cracks of 1 cm.
  * "No cracks observed" means no cracks only in a profile described dry at the surface (cracks "may be evident only when the soil is dry", 3.2.12, p. 80); described moist, they stay unknown (Hugo).

### USDA Soil Taxonomy

* **Aridisols (KST 13, item G, p. 69):**
  * No aridic moisture regime is inferred when none is recorded. soilKey had read low surface carbon as aridic, which KST does not give. With no regime recorded, the order is undecided and the key goes on.
  * The order follows item G point by point: an ochric or anthropic epipedon, the diagnostic horizons KST lists within 100 cm, the cambic horizon's 25 cm, and item G.2, the salic path.
  * The canonical Calcisol, Gypsisol, Solonchak and Durisol fixtures record an aridic moisture regime (Hugo).
* **The kandic horizon:** a layer with no recorded lower boundary is undecided, not an error (KSSL 2523).
* **`texture_class_from_pct()`** follows the texture triangle (WRB 2022 Annex 1, Table 8.39, after Soil Science Division Staff 2017) at four boundaries it had wrong:
  * sandy clay needs more than 45% sand;
  * clay 35 to < 40% with more than 45% sand is sandy clay;
  * silt 50 to < 80% with < 12% clay is silt loam;
  * clay < 7% with < 50% silt and more than 43% sand is sandy loam.

  It now matches `aqp::ssc_to_texcl()` on the whole grid. 707 horizons of the panels change class, and no name changes.

### Results against 0.9.314

| panel | 0.9.314 | 0.9.315 |
|---|---|---|
| FEBR USDA (886) | 44.5 | 50.9 |
| KSSL USDA (497) | 38.4 | 38.8 |
| ISIS USDA (748) | 47.5 | 47.9 |
| ISIS WRB 2022 (481) | 61.1 | 61.3 |
| FEBR WRB (218), WoSIS (130), AfSP (120) | 22.0, 16.2, 28.3 | unchanged (qualifiers change) |
| BDsolos, FEBR and Redape SiBCS | 57.5, 37.6, 87.1 | unchanged |

* In FEBR and KSSL no profile records a moisture regime, so no profile there is an Aridisol now. The 127 FEBR profiles move to the Ultisols, Alfisols and others. KSSL loses 34 reference Aridisols and gains 36 other profiles.
* ISIS KE069 (an indurated Ckm at 30 cm) is now a Phaeozem, as ISRIC names it.
* **Fixtures:** the WRB and SiBCS names of the 44 canonical fixtures do not change. In USDA, the Durisol is now a Typic Torriorthents (its duripan is not cemented) and the Luvissolo a Typic Hapludalfs (no moisture regime).

# soilKey 0.9.314 (2026-10-29)

## WRB 2022 mollic and umbric horizons: criteria 1-3 in every layer

* WRB 2022 Ch 2.1 (p. 23): when a diagnostic horizon consists of several subhorizons, every criterion except the thickness must be met in each subhorizon, unless the text says otherwise. For the mollic (3.1.20) and umbric (3.1.39) horizons, only the base saturation is "on a weighted average". The horizon is now the run of layers from the mineral soil surface that each meet:
  * criterion 1, the structure (not recorded still does not block it);
  * criterion 2, >= 0.6% SOC;
  * criterion 3, the colour, items a-c. A plough layer takes the colour of the subhorizon below it.

  The thickness and the weighted base saturation are read on that run.
* Before, the carbon test passed when any one layer had 0.6%, and the structure was a share of the whole dark stack. An Ah of subangular blocky structure over thick massive dark layers therefore had no horizon. Bounding the stack by structure alone would have given ISIS CN012 a mollic horizon from its massive 6 cm Ah, since the colour and carbon tests let its C layers in; reading every criterion in every layer gives it none.
* **Colour where it is not wholly recorded:**
  * A moist value with no dry value still reads the dry value as the moist value + 1.
  * Of the older proxies, only one stays (Hugo, 2026-10-29): a layer with no colour recorded at all and >= 1.5% SOC is read as dark. The FEBR and WoSIS panels record no colour.
  * A dry colour alone now leaves criterion 3 undecided unless its value is above 5, and the dry chroma no longer stands in for the moist chroma.
  * The colour subtest records, for each layer, where its reading came from.
* A criterion that fails now decides the horizon even when another one is undecided. Until now, an undecided colour beside a massive surface layer left the horizon undecided.
* **Results against 0.9.313:**
  * FEBR WRB (22.0%) and WoSIS (16.2%): no name changes.
  * AfSP: 27.5 to 28.3%. BI wasp_2, an Ah of subangular blocky structure over massive dark layers, is an Umbrisol again.
  * ISIS: 61.5 to 61.1% (294 of 481), with 3 gained and 5 lost, each by the text:
    * gained: two profiles that record a dry colour only, which the dry-only proxy made Umbrisols, and an Arenosol;
    * lost: three that record a dry colour only or lose carbon below the first layer, a Kastanozem whose Ap is recorded as massive, and a Luvisol whose plough layer now takes the colour of the Ah below it (a Phaeozem).
  * The SiBCS and USDA panels and the 44 canonical fixtures are unchanged.

# soilKey 0.9.313 (2026-10-29)

## ISIS: slickensides and pressure faces are read as absent only where the cutans field is free

* ISIS has one cutans field per layer. With `absent_unless_recorded = TRUE`, slickensides and pressure faces that a described horizon does not mention were read as absent. Where that field records clay cutans, or cutans of no kind, they could not have been recorded there, so they now stay unknown (Hugo, 2026-10-29). Where the field is free or records no cutans, they are still read as absent.
* The seven ISIS Nitisols that record clay cutans only (KE015, KE016 and others) no longer read as having no pressure faces. KE016's nitic horizon becomes undecided instead of absent.
* **Results:** the WRB and USDA names of the ISIS profiles do not change (481 and 748, rebuilt with this reader). The other panels do not read ISIS.

# soilKey 0.9.312 (2026-10-29)

## The AfSP reader reads its structure codes

* AfSP keeps the structure type, grade and size as class codes of the FAO-ISRIC (1990) and FAO (2006) guidelines. The reader passed them on unread, and the diagnostic tests look for words. A massive "M" or a single-grain "N" read as structure not recorded, so since 0.9.311 it did not block the mollic and umbric horizons. In the 120 profiles of the AfSP sample, 190 layers in 50 profiles record a structure type and 59 layers in 23 profiles a massive, single-grain or rock structure.
* The codes are now read by the AfSP dictionary (`AfSP012Qry_DictioClassValues`):
  * a range of grades by its lower end, as the ISIS reader does ("weak to moderate" is weak);
  * a range of sizes with both ends, since soilKey reads a size range by its upper end;
  * a code the dictionary does not define as not recorded (in AfSP 1.2: grade V, A and R, in 83 of 74,961 layers, and size S, in 5).
* Each code goes to the provenance. The bundled sample (`load_afsp_sample()`, built in 0.9.77) and profiles built before are read when they are loaded or benchmarked.
* **AfSP WRB:** 33 of 120 profiles agree with the reference, as before, with 12 RSGs changed: 2 gained and 2 lost.
  * Gained: a Solonetz whose Btn records columnar structure, and a Cambisol.
  * Lost: an Umbrisol whose Ah over 116 cm of massive dark layers falls short of the structure criterion; a Regosol whose B records subangular blocky structure, a Cambisol now.
  * The other panels do not read AfSP and are unchanged.

# soilKey 0.9.311 (2026-10-29)

## WRB 2022 qualifiers, the argic, nitic, mollic and irragric horizons, and two ISIS reader guards by the text

These are the remaining corrections from the triage of the ISIS profiles where soilKey and ISRIC disagree.

* **Qualifiers (Ch 5):**
  * Calcaric needs a layer of calcaric material 30 cm thick within 100 cm, and no calcic horizon. A moderate or stronger HCl reaction counts when CaCO3 is not measured.
  * Skeletic needs 40% coarse fragments averaged over 100 cm.
  * Geric no longer reads a missing Al as 0.
  * Aric needs a ploughed layer of 10 cm or more with an abrupt lower boundary.
  * Sodic needs a layer of 20 cm and no natric horizon.
  * Loamic and Siltic use the texture classes of the triangle.
  * Brunic needs a layer that fails cambic criterion 1 and meets criteria 3 and 4.
  * Pellic is tested in the upper 30 cm.
* **Argic horizon (3.1.3):**
  * Item 2.a.ii: when the coarser layer directly over the argic horizon is a plough layer, the clay increase does not count. The argic horizon is then shown by illuvial clay (2.b).
  * Item 2.a.i: a lithic discontinuity between the two layers also voids the clay increase. Readings decided by Hugo: the discontinuity counts when a material number records it or the coarse fragments show it (3.2.7, criteria 3-6). The particle-size ratios of criterion 2 are not read, since its item 2.b (not the original variation of the parent material) is not shown by the data.
* **Nitic horizon (3.1.22):** the shiny ped faces are pressure faces, not clay films. ISIS cutans of kind pressure are read into the new attribute `pressure_faces`, and the second-level structure into `structure_type_secondary`.
* **Mollic and umbric horizons:**
  * Criterion 1 needs aggregates of 10 cm or less, or cloddy structure, in half of the horizon. A recorded massive or single-grain structure fails it; structure or size not recorded does not block it (Hugo).
  * Criterion 5 is the weighted base saturation.
* **Irragric horizon:** follows its criteria (3.1.18). Since none of the data record a raised land surface, it is never shown, only ruled out or left open.
* **ISIS reader:**
  * An R row inside a described horizon is not read as continuous rock (IT017: an "R 0-5" row inside an Apkc 0-35).
  * A particle-size record of zeros only is missing.
* **Results against 0.9.310:**
  * FEBR WRB goes from 20.2 to 22.0%. AfSP goes from 28.3 to 27.5% (1 profile), and WoSIS is unchanged.
  * ISIS goes from 64.7 to 61.5%, with 4 profiles gained and 19 lost:
    * 7 Nitisols that record clay cutans only;
    * 4 Phaeozems whose Ah is recorded as massive;
    * 8 argic horizons under a plough layer with patchy clay films.
  * The SiBCS and USDA panels are unchanged.
* **Fixtures:** 7 of the 44 change qualifiers only: Skeletic, Calcaric and Irragric dropped where the text does not support them, Loamic added where it does, and Hypereutric now shown.

# soilKey 0.9.310 (2026-10-29)

## WRB 2022 diagnostics by the text: six groups

Found by running soilKey on ISRIC's World Soil Reference Collection (ISIS). Six investigations sorted the profiles where soilKey and ISRIC disagree. Many came from soilKey readings that did not follow the 2022 text, and those are fixed here, group by group. Each group was measured on the BDsolos, FEBR, KSSL, Redape, AfSP, WoSIS and ISIS panels. The SiBCS and USDA keys keep their readings: where they shared a WRB diagnostic, it now has a separate reading for them.

* **Mollic family:**
  * Phaeozems need base saturation of 50% or more throughout to 100 cm or to a limiting layer, and carbonates are allowed (Ch 4, p. 113).
  * Chernozems and Kastanozems need a protocalcic layer or a calcic horizon at the depth the key gives, with base saturation of 50% or more above it.
  * Protocalcic properties are visible secondary carbonates (3.2.8), not laboratory CaCO3.
  * The chernic horizon follows its seven criteria and no longer needs worm holes.
  * Umbrisols take an umbric, mollic or hortic horizon, and the umbric horizon is the contiguous stack from the mineral surface.
  * Where base saturation is missing, the pH stands in, by Annex 2 Table 9.2.
* **Argic family:**
  * The argic horizon gains criterion 1 (8% clay or more), the transition of 15 cm or less, and criterion 2.b.ii (clay films common or more). It is a subsurface horizon.
  * Acrisols, Lixisols, Alisols and Luvisols use the 2022 tests: CEC below 24 per kg clay, and exchangeable Al above the bases in half of 50-100 cm. Where the cations are missing, the pH stands in, by Table 9.3.
  * The natric horizon gets its structure and ESP criteria, and Solonetz need a natric horizon within 100 cm.
  * The ferralic thickness counts adjoining sub-horizons. Ferralsols follow the text on the argic horizon above the ferralic.
* **Vertic horizon:**
  * Wedge-shaped aggregates count as well as slickensides.
  * The 25 cm counts adjoining layers, and the COLE path stays in the USDA reading only.
  * Vertisols need the vertic horizon within 100 cm, 30% clay above it, and cracks; missing crack data is undecided.
* **Redox and plinthite:**
  * Gleyic and stagnic properties are told apart by colour and by the site's groundwater. This replaces a "mottles decreasing with depth" test that is not in the text. ISIS groundwater is now read into the new site attributes `groundwater_kind` and `groundwater_depth_cm`.
  * Stagnosols need stagnic properties on a third of 0-60 cm, and Planosols an abrupt textural difference within 75 cm with a transition of 2 cm or less.
  * Plinthosols take a plinthic, pisoplinthic or petroplinthic horizon. The pisoplinthic horizon reads nodules, and the petroplinthic horizon is at least strongly cemented.
* **Organic material and Andosols:**
  * Organic material is 20% organic carbon, not 12%. Histosols follow the key: 40 cm of organic material within 100 cm, or 10 cm over rock or ice.
  * Andosols sum the andic layers within 100 cm.
  * Where bulk density is missing, Alox + 1/2 Feox of 2% or more with a pH NaF of 9.5 or more counts as andic, as a field test. The new attribute `ph_naf` holds the pH NaF, and ISIS phnf is read into it.
  * Cryosols gain item 2: a cryic horizon within 200 cm with cryoturbation.
* **Weakly developed and carbonate groups:**
  * The cambic horizon excludes only the layers of another diagnostic horizon. Cambisols, Calcisols, Gypsisols and Fluvisols get their depth tests.
  * The calcic horizon follows criterion 2.
  * Arenosols use a weighted average of texture.
  * Fluvic material needs a water origin, and its strata are a texture reversal or irregular organic carbon. ISIS deposition origin is now read into `rock_origin`.
* **Readings decided by Hugo after the measurement:**
  * A profile described above 100 cm is decided on the part that is described (Phaeozems, Arenosols).
  * With no secondary carbonates recorded, a layer with CaCO3 >= 15% meets calcic criterion 2 by the laboratory.
  * Gleyic criterion 1 (>= 95% reductimorphic features) needs at most 5% oximorphic features, and criterion 3 needs a layer below that meets criterion 1.
* **Results:** on the 481 ISIS profiles with an ISRIC WRB 2022 name, the Reference Soil Group agrees for 310 (64.4%), up from 221 (45.9%) in 0.9.308. Other WRB panels: AfSP 25.0 -> 28.3%, FEBR 19.3 -> 20.2%, WoSIS 16.9 -> 16.2% (one profile, a Histosol whose H horizons have under 20% organic carbon). BDsolos SiBCS goes from 57.2 to 57.5%; Redape and the USDA panels are unchanged against 0.9.309.
* **Fixtures:** 10 of the 44 canonical fixtures change their WRB name, each by the text:
  * the Planossolo becomes a Planosol;
  * the Argissolo and the Nitossolo become Lixisols;
  * Cambic is dropped where the cambic layers belong to another horizon;
  * the Phaeozem fixture becomes Chernic.

  The Chernozem, Kastanozem, Acrisol, Fluvisol, Stagnosol, Planosol, Retisol and Gleysol fixtures gained or changed recorded properties so that they keep showing the soil they were written for (the Gleysol: the groundwater its drainage class names).

# soilKey 0.9.309 (2026-10-29)

## ISRIC's KE009: four WRB qualifiers and the key to the Oxisols by the texts

Jelle Janssen's profile KE009 (ISRIC, Kenya) is a Rhodic Ferralsol (Clayic, Epic, Eutric, Humic) and a Rhodic Eutrustox for ISRIC. soilKey gave Rhodic Ferralsol (Clayic, Epic, Ferric, Ochric) and Typic Paleustolls. Each difference was a soilKey reading that did not follow the text. KE009 now gets ISRIC's names in both systems.

* **Humic** (WRB 2022 Ch 5, p. 136): the weighted average of soil organic carbon to 50 cm from the mineral soil surface, with the range below a limiting layer counting 0. soilKey counted every layer that starts above 50 cm whole, from 0 cm, so a deep layer with little carbon pulled the average down (KE009: 0.83% for 1.15%).
* **Ochric** (p. 141) also requires that the Humic criteria are not met.
* **Eutric, Dystric and their subqualifiers** use the pH when the exchangeable cations are not recorded. The text allows it ("pH values may be used according to Annex 2 (Chapter 9.13)"), by Table 9.3: exchangeable bases equal the Al at pH (water) 4.6, 4.9 or 5.4 for organic carbon below 2, 2 to 7.5 and 7.5 to 20 %. With the carbon unknown, the pH decides only where the three classes agree.
* **The ferric horizon** (Ch 3.1.11, p. 45) is read from its criteria: coarse red or black masses, or 5 % or more of nodules over 2 mm (`ferric_nodules_pct`), in 15 cm or more, outside a plinthic, pisoplinthic or petroplinthic horizon. soilKey had read it from Fe-dithionite of 5 % or more, which is not a criterion, so red Ferralsols with no nodules became Ferric. The argument `min_fe_dith_pct` of `ferric()` becomes `min_nodules_pct`.
* **The kandic horizon** follows Keys to Soil Taxonomy (13th ed., pp. 26-27):
  * the clay increase within 15 cm: 3 % absolute, 20 % relative or 8 % absolute, by the clay of the overlying horizon;
  * the depth of its upper boundary;
  * a thickness of 30 cm;
  * a texture of loamy very fine sand or finer;
  * an apparent CEC of 16 cmol(+) per kg clay or less in half of the 100 cm below the clay increase;
  * no fine stratification.

  soilKey had taken the WRB argic horizon with a low CEC.
* **The key to the Oxisols** (KST 13, Ch 8, item E, pp. 68-69) reads both of its items. One is an oxic horizon within 150 cm with no kandic horizon within that depth. The other is 40 % or more clay in the upper 18 cm with a kandic horizon within 100 cm. Since v0.9.17 soilKey had used "no argillic horizon above the oxic" instead, which made clayey Oxisols with a kandic horizon into Mollisols, Ultisols or Alfisols. soilKey records no weatherable minerals, and the kandic horizon of the second item is read as its oxic horizon is.
* **Fixtures:** 20 of the 44 canonical fixtures change qualifiers only.
  * Humic for Ochric in seven of them, where the 50 cm average is 1 % or more.
  * Eutric added in six of them, from the pH.
  * Ferric removed from four, which have no nodules.
  * The Nitossolo fixture becomes a Rhodic Kandiudox (USDA), since it has a kandic horizon.
* Against 0.9.308, no WRB Reference Soil Group changes on any panel (qualifiers only). The USDA order agrees with ISRIC for 47.5% of the ISIS profiles (from 47.3%); KSSL is unchanged; FEBR goes from 44.8 to 44.5%, as Latosols with a textural gradient now have a kandic horizon by KST 13 (their reference names predate the kandic horizon).

# soilKey 0.9.308 (2026-10-29)

## soilKey reads ISRIC's World Soil Reference Collection (ISIS)

Asked for by Jelle Janssen (ISRIC), who had matched the ISIS properties to soilKey's by hand.

* `read_isis()` reads profiles live from ISRIC's ISIS GraphQL service, by profile code (`read_isis(c("KE009", "ZW005"))`) or by country (`read_isis(country = "KE")`). It returns soilKey pedons ready for `classify_all()`, and writes nothing to disk.
* `isis_profile_to_pedon()` does the translation:
  * Laboratory values go to soilKey units: organic carbon, nitrogen, CaCO3 and gypsum from g/kg; water at 1500 kPa from volume to mass; water-dispersible clay as a clay content.
  * FAO field codes go to soilKey's words: structure, consistence, stickiness, plasticity, cutans, mottles, boundaries, effervescence, cementation and pans.
  * Base saturation, ECEC, aluminium saturation and silt (100 - sand - clay) are calculated from the parts.
  * Every value names its ISIS field in the provenance.
* The site carries the names ISRIC gives the profile: `reference_wrb`, the new `reference_wrb_full` and `reference_wrb_edition`, `reference_usda` and its subgroup, and the new `reference_usda_edition`.
* It also carries the soil moisture and temperature regimes when ISIS records them (KE009: ustic, isohyperthermic), gilgai, the licence and the citation (`isis_citation()`).
* Readings decided by Hugo:
  * ISIS keeps the field description and the laboratory samples in separate rows in more than half its profiles. Each sample goes to the described horizon that holds its mid-depth, as a thickness-weighted mean that fills only what the horizon lacks. A described horizon left with no laboratory value takes the samples that overlap it.
  * Clay films are the FAO (1977) cutans of kind clay: patchy, broken and continuous become few, common and many. Clay cutans with no abundance count as few, and so do cutans of no kind in a horizon whose designation has t.
  * Cutans of kind slickensides are the slickensides, counted as common when patchy, broken or of no recorded abundance.
  * The profile's cracking (large or small) or, without it, gilgai gives cracks of 0.5 cm or more in the horizons starting above 100 cm.
  * Coarse fragments come from the middle of the FAO class when cfvo is not measured.
  * Calcareous inclusions are secondary carbonates. Hard Fe/Mn concretions over 2 mm are nodules. In a v horizon or an iron pan, mottles and soft Fe/Mn concretions are plinthite.
  * EC 1:2.5 is used only where it decides the salic horizon: 15 dS/m or more is a lower bound of the paste EC; below that, EC 1:2.5 x 12.5 is an upper bound. It goes into the new attribute `ec_max_dS_m`, which only the salic test reads, to rule a layer out.
  * With `absent_unless_recorded = TRUE` (the default), mottles, slickensides, plinthite, a duripan and artefacts that a horizon's field description does not mention are absent in that horizon. Mottles stay unknown in a g, l or r horizon with no mottle field.
* `benchmark_isis()` sets soilKey's WRB and USDA names beside ISRIC's. On the 481 profiles with an ISRIC WRB 2022 name, soilKey now gives the same Reference Soil Group for 221 (45.9%); with the first draft of the translation it was 149 (31.0%). The USDA order agrees for 354 of 748 (47.3%; ISRIC's USDA names are from the 1975 and 1999 editions). The median number of close calls per profile fell from 7 to 4.
* The investigation behind these numbers also found deviations of soilKey's WRB 2022 diagnostics from the text, on the mollic, argic, redox, vertic, organic and weakly developed groups. They are fixed in the PRs that follow.

# soilKey 0.9.307 (2026-10-29)

## A soil temperature regime warmer than gelic rules out permafrost

Found on Jelle Janssen's profile KE009 (ISRIC, Kenya), which kept the Cryosols open for want of a permafrost temperature.

* `cryic_conditions()` (WRB Cryosols, the cryic horizon and the Cryic qualifier) and the USDA permafrost test (Gelisols, the Histosols' exclusion, the Gelods) now read `site$soil_temperature_regime`. A regime warmer than gelic (cryic, frigid, mesic, thermic, hyperthermic and their iso forms) means no permafrost, so the answer is no instead of undecided. KST 13 (Ch. 3, p. 50) gives the cryic regime a mean annual soil temperature between 0 and 8 C and no permafrost; the others are warmer.
* Gelic, the older pergelic, a missing regime or a word that is not a regime leave the question open. A measured `permafrost_temp_C` at or below 0 C, or a frozen designation, still wins.
* The Pro app has a field "Soil temperature regime (USDA)" beside the moisture regime. It is saved with the session and restored when the session is opened again.
* No name changes on the 44 canonical fixtures; profiles whose site states a warm regime lose the Cryosols (and Gelisols) from their close calls.

# soilKey 0.9.306 (2026-10-29)

## Pro app: the close calls name their classes, and a field for the soil moisture regime

Both reported by Jelle Janssen (ISRIC).

* The Ambiguities tab of the Classify page showed "?" in place of each
  reference soil group that could not be ruled out. The panel read the
  fields `name` and `code`, while the results carry `rsg_name` and
  `rsg_code`. It now shows the group and its code, for example
  "Cryosols (CR): missing permafrost_temp_C".
* The Pedon page has a "Soil moisture regime (USDA)" field in the site
  section (unknown, aquic, aridic (torric), udic, perudic, ustic, xeric).
  The Soil Taxonomy key reads it: ustic leads to the Ustolls and Ustalfs,
  xeric to the Xerolls. It is saved with the session and restored when the
  session is reopened. A profile loaded from a fixture or from WoSIS sets
  it from its own site data, or back to unknown, so a regime is never
  carried over from the previous profile.

# soilKey 0.9.305 (2026-10-29)

## The Hungarian diagnostic soil classification: the 15 soil types

`classify_hscs2024()` keys a profile through the key to the soil types of
the Hungarian diagnostic classification (Michéli et al. 2024, *A
diagnosztikus szemléletű hazai talajosztályozási rendszer*, second
approximation), from the Láptalaj (peat soil) to the Földeskopár (other
soils). Experimental and opt-in, like the other national systems:
`classify_all(p, systems = "hscs")`.

* The key and its gates follow the text's own order: the types after the
  first are "other soils that ...", so each gate tests only its own
  requirements (`inst/rules/hscs2024/key.yaml`, `R/diagnostics-hscs-types.R`).
* The diagnostic horizons, properties and materials of section 8 are in
  `R/diagnostics-hscs.R`: organic, humic, clay accumulation, swelling,
  cambic, solonetz and solonchak horizons; continuous hard rock, reductive
  conditions and the groundwater gley colour pattern; carbonate classes,
  artefacts and stratified material.
* The horizon notation is the FAO (2006) one the text adopts
  (`R/designation-hscs.R`): an l or r suffix shows reductive conditions, i
  slickensides, k carbonates, n sodium, p ploughing, t clay accumulation, u
  artefacts, w a cambic horizon.
* The texture triangle the text prints (p. 36) is the USDA triangle with
  Hungarian names, so Hungarian field textures (homok, vályogos homok,
  homokos vályog ...) are read on it, as are English names and lab
  particle sizes.
* Names are the Hungarian singular, as the text's naming example writes
  them; `levels$name_en` gives an English gloss. The codes (LAP ... FOL)
  are soilKey's. The subtype and variety properties are not keyed yet.
* New horizon attribute `soluble_salts_pct`: total water-soluble salts as
  Hungarian laboratories report them (the solonchak horizon). The EC is not
  converted, as the text advises.
* `make_hscs2024_example()` gives one example profile per type.
* `classify_hierarchical()` adds a `name_en` column to `levels` when the
  classes of a system carry an English gloss; the other systems keep their
  three columns.

Hugo decided seven readings (2026-10-29):

* reductive conditions (Réti talajok) are shown by a dipyridyl test,
  methane, iron sulfides, or an l or r suffix within 50 cm, and are
  undecided when none is recorded;
* the solonchak horizon reads the new `soluble_salts_pct` only;
* the sodium shares of the solonetz horizon are of the CEC;
* an organic horizon with no bulk density measured counts as 1 g/cm3 or
  less;
* slickensides common, many or continuous meet the 10 % of the swelling
  horizon, and they stand for the cracks when none are recorded;
* names in the Hungarian singular with an English gloss;
* stratified material reads 10 points of USDA sand (2-0.05 mm).

The other choices where the text is silent are in
`inst/rules/hscs2024/readings.yaml` (17 topics).

# soilKey 0.9.304 (2026-10-29)

## New Zealand Soil Classification: rows of unknown role under the A horizon and in the B

Two places where `classify_nzsc4()` read a mineral row with no designation
one way only now try every reading and keep a verdict only when all agree.

* The base of the A horizon. A row with no designation directly under the A
  may be A too, so every test measured from the base of the A is read at
  each base the A may have (its designated base, then the bottom of each
  such row in turn): the sandy groups (GS, RS, BS), part 1 of the Ultic
  Soils, part 2 of the Granular Soils, parts 1 and 2(a) of the Pallic Soils,
  the Rocky Recent Soils and the Perch-gley Typic Ultic Soils. Until now
  such a row was read as below the A.
* Part 2(b) of the Pallic Soils (also read by the Mottled-pallic Orthic
  Recent Soils). Rows with no designation below the topsoil may be part of
  the B or BC, so every choice of them is tried. Until now a B thinner than
  10 cm over such rows was read as not meeting part 2(b).

On the 6,351 profiles of the NZSC review set, 6 change: one Argillic
Perch-gley Pallic Soil becomes undecided at the Pallic order (its platy row
under the A may be A, and then no fine texture lies between the A and 60
cm), and five Sandy Recent Soils become undecided at the sandy group,
because a row with no designation runs below 60 cm and an A reaching 60 cm
leaves the sandy window empty, which the sandy groups already read as
undecided. Profiles with every row designated are unchanged. The readings
file `inst/rules/nzsc4/readings.yaml` gains one topic (81 in all).

# soilKey 0.9.303 (2026-10-29)

## Pro app: uploads and downloads over the websocket, and a notice when the connection is lost

On soilkeypro.com some uploads and downloads failed with "Not Found". Shiny
moves files by plain HTTP requests to `/session/<token>/upload/` and
`/session/<token>/download/`, which only the R process holding the session
can answer. Cloud Run keeps a visitor on one instance on a best-effort basis,
so while a new instance was starting a request could reach another one. The
websocket stays on the instance of the session, so the files now travel over
it.

* `sk_file_input()` replaces `shiny::fileInput()` (7 controls). It draws the
  same box and button; `www/soilkey-transfer.js` sends the file in slices of
  512 KB, each one acknowledged before the next, and the server rebuilds it.
  `input$<id>` keeps the shape `fileInput()` gave (name, size, type,
  datapath), so the tabs read it as before. The size limit is still
  `shiny.maxRequestSize` (5 MB by default); drag and drop still works.
* `sk_download_button()`, `sk_download_link()` and `sk_download()` replace
  `downloadButton()`, `downloadLink()` and `downloadHandler()` (13
  downloads). A click runs the same file name and content functions; the file
  comes back in slices and the browser saves it under the same name. An error
  shows its message as a notification.
* When the connection to the server drops (the session ended, a phone put the
  page to sleep, or the network failed), a bar at the top of the page says so
  and offers a Reload button, in English or Portuguese, instead of a grey page
  that no longer responds. A profile saved with "Save session" can be reopened
  after the reload.

# soilKey 0.9.302 (2026-10-29)

## New Zealand Soil Classification: the subgroups of the Brown, Anthropic, Recent and Raw Soils

`classify_nzsc4()` now keys the 20 groups of the Brown, Anthropic, Recent
and Raw Soils to their 109 subgroups, following the keys to subgroups of the
New Zealand Soil Classification, 4th edition: from the Mottled Allophanic
Brown Soils to the Typic Sandy Raw Soils. With this release every group of
the 15 orders is keyed to its subgroups (336 subgroups of 70 groups); the
Hydrothermal Recent Soils and six groups of Raw Soils have none and keep the
group name.

* The keys live in `inst/rules/nzsc4/subgroups/`, one file per order, in the
  book's order. The last subgroup of each group is the default (in the
  Allophanic Brown Soils the key gives the Typic subgroup criteria and makes
  the Pedal one the last).
* The gates are in `R/diagnostics-nzsc-subgroups-barw.R`: earthy or light B
  horizons, firm horizons beneath them, pallic colours, magnesic Brown
  criteria, impeded Acid Brown Soils, humose and immature Orthic Brown
  Soils, the Maori, refuse, mixing and fill subgroups of the Anthropic
  Soils, acidic windows, weathered-B horizons reaching 30 cm, sulfuric soil
  material, tephric material, buried soils, and the sandy Gley Raw Soils.
* Part 2(b) of the Pallic Soils is now one helper that the order key and
  the Mottled-pallic Orthic Recent Soils share.
* Shared gates gained arguments: fluid and saline horizons within 30 cm,
  scoria to 60 cm, contacts from the soil surface, and cemented horizons of
  a least thickness.
* New site fields: `mechanical_mixing_purpose` and
  `acid_neutralising_amendment`.

Hugo decided seven readings:

* the Maori Anthropic subgroups read the existing fields (artefacts and the
  layer origin);
* the purpose of mechanical mixing is a new site field, absent when not
  recorded;
* a coarse fill layer has 80 % or more coarse fragments and artefacts;
* acid-neutralising amendments are a new site field, absent when not
  recorded;
* the Humose Orthic Brown Soils have 10 % or less clay in every horizon to
  90 cm;
* a compacted fill has one fill horizon of bulk density 1.5 or more;
* a placic horizon is left to the Placic Firm Brown Soils, not counted as
  the cemented horizon of the Cemented and Mottled-cemented Firm Brown
  Soils, as the Acid Brown key does.

The other readings are in `inst/rules/nzsc4/readings.yaml` (80 topics).

Two independent reviews were made, and their findings are fixed. The review
against the book confirmed every code, position and default, and found the
Maori names without their macron, scoria that need not reach 60 cm in the
Scoriaceous Tephric Recent Soils, a coarse fill layer of natural gravel, and
word lists that matched inside other words. The logic review ran 6,000
random profiles and refilled their missing values; it found no error and no
change against 0.9.301, and verdicts decided where a horizon of unknown role
leaves them open (the firm horizon and the B above it, the earthy B, the
greater part of the B, the thin weathered-B, the subsurface horizons). The
fill of the Compacted Fill Anthropic Soils is now the anthropic soil
material the order key reads, and only the uppermost buried soil counts.
Outside the four orders, rock (R, CR) with no pH is no longer read as
possibly acid in the acidic subgroups of 0.9.299, which can only decide an
open verdict.

# soilKey 0.9.301 (2026-10-29)

## New Zealand Soil Classification: the subgroups of the Semiarid, Oxidic, Granular and Pallic Soils

`classify_nzsc4()` now keys the 17 groups of the Semiarid, Oxidic, Granular
and Pallic Soils to their 78 subgroups, following the keys to subgroups of
the New Zealand Soil Classification, 4th edition: from the Mottled
Aged-argillic Semiarid Soils to the Typic Immature Pallic Soils. Eleven of
the 15 orders now reach subgroup level (227 subgroups of 50 groups); the
Brown, Anthropic, Recent and Raw Soils follow.

* The keys live in `inst/rules/nzsc4/subgroups/`, one file per order, in the
  book's order. The last subgroup of each group is the default.
* The gates are in `R/diagnostics-nzsc-subgroups-sxnp.R`: alkaline horizons,
  weathered coarse fragments, lithic and paralithic contacts, the clay
  decrease of the Brown Oxidic subgroups, cutanoxidic horizons below 25 cm,
  allophanic layers apart from the bulk density, sodic features or a
  calcareous horizon within or next to an argillic horizon or a fragipan,
  argillic horizons over the fragipan, acid or reddish parts of the argillic
  horizon, lamellae, cemented horizons and pedal B horizons.
* Shared gates gained arguments: the acidic B or BC subgroups read a pH
  limit and the major part (Granular Soils), the argillic subgroups a
  thickness, and the calcareous subgroups any depth.
* New horizon attribute: `weathered_fragments_pct`, the share of the coarse
  rock fragments weathered enough to break easily by hammer or spade.

Hugo decided two readings:

* the weathered coarse fragments are recorded in the new attribute, and not
  recorded reads as none;
* sodic features that are not measured are absent from a designated horizon
  without the n suffix, as the z suffix marks saline soil material (the
  group key of the Argillic Pallic Soils keeps its reading).

The other readings are in `inst/rules/nzsc4/readings.yaml` (69 topics).

Two independent reviews were made, and their findings are fixed. The review
against the book confirmed every code, name and position. The logic review
ran 6,000 random profiles and refilled their missing values; it found no
error and no change against 0.9.300. The fixes:

* a clay decrease of the Brown Oxidic subgroups is decided when the missing
  clays cannot undo it;
* the pedal B of the Immature Pallic subgroups and the major part of the B
  or BC try each choice of the rows of unknown role (the topsoil aside), as
  now also the B or BC of the acidic, allophanic and magnesic subgroups of
  0.9.300;
* lamellae with no bottom are weighed both ways (also in the Laminar Pallic
  group key);
* fragipans and densipans recorded as a pan type are not cemented;
* a fragipan or argillic horizon the description ends in leaves "immediately
  beneath" open;
* an undesignated row with no fine-earth record may be rock;
* exchangeable Na below 0.7 with no CEC is not a measure of the ESP;
* recorded coatings with no colour value may be dark, so the cutanic
  horizon they mark stays open.

# soilKey 0.9.300 (2026-10-28)

## New Zealand Soil Classification: the subgroups of the Podzol, Allophanic, Pumice and Melanic Soils

`classify_nzsc4()` now keys the 17 groups of the Podzol, Allophanic, Pumice
and Melanic Soils to their 79 subgroups, following the keys to subgroups of
the New Zealand Soil Classification, 4th edition: from the Humus-pan
Densipan Podzol Soils to the Typic Orthic Melanic Soils. With the Organic,
Gley and Ultic Soils of 0.9.299, the subgroups of seven of the 15 orders are
keyed (149 subgroups of 33 groups); the other eight orders follow.

* The keys live in `inst/rules/nzsc4/subgroups/`, one file per order, in the
  book's order. The last subgroup of each group is the default.
* The gates are in `R/diagnostics-nzsc-subgroups-zlme.R`: Bh horizons and
  their thickness, an argillic horizon beneath a Bh or a pan, fluid horizons
  above the podzolic-B, silt-mantled eluvial horizons, acidic and allophanic
  B or BC horizons, vitric allophanic material, buried granular B horizons,
  compacted ignimbrite, the Orthic Pumice subgroups (podzolic, allophanic,
  buried-allophanic, scoriaceous, immature), redox segregations within 60
  cm, and pedal weathered-B horizons under the A.
* Shared gates gained arguments: pans at any depth (the Podzol subgroups name
  no depth) or with their top less than 90 cm down (the ironstone-pans and
  duripans "within 90 cm"), a calcareous horizon within or at 90 cm
  (Melanic Soils), a peaty topsoil at the surface only (where the text names
  no buried one), and the magnesic criteria in the B or BC horizon (Mafic
  Melanic Soils).
* New horizon attributes: `pore_coatings_hue_moist`,
  `pore_coatings_value_moist` and `pore_coatings_chroma_moist`, the colour
  of coatings on pores or gel-like masses (Acidic and Vitric-acidic Orthic
  Allophanic Soils).

Hugo decided seven readings:

* the colour of coatings on pores is recorded in the three new attributes,
  and not recorded reads as none, so the pH decides alone;
* "hue 7.5YR or more" (Podzolic Orthic Pumice Soils) is 7.5YR or redder;
* a silty layer (Peaty-silt-mantled and Silt-mantled Perch-gley Podzol
  Soils) is one of the silt, silt loam, silty clay loam and silty clay
  classes;
* any ignimbrite counts as the Taupo ignimbrite of the Mottled-compacted and
  Compacted Impeded Pumice Soils;
* the Bh of the Ultic-humose Densipan Podzol Soils tops a B horizon that is
  clayey and prismatic as a whole;
* the layer of the Allophanic Orthic Pumice Soils is itself vitric and
  allophanic;
* a scoria of no named composition is basaltic (Scoriaceous Orthic Pumice
  Soils), and "predominantly" is read by the thickness-weighted gravel from
  the mineral soil surface to 25 cm.

The other readings are in `inst/rules/nzsc4/readings.yaml` (61 topics). The
key gives the last Orthic Pumice subgroup (MOT) no name; the chapter's
summary table names it Typic Orthic Pumice Soils.

Two independent reviews were made, and their findings are fixed. The review
against the book confirmed every code, name and position, and found eight
defects, among them a Bh read only under an eluvial horizon, "within 90 cm"
read as "at 90 cm or less" for the ironstone-pans and duripans, and the
buried B of the Buried-granular Orthic Allophanic Soils decided while its
allophanic status was open. The logic review ran 6,000 random profiles and
filled their missing values 185,000 times: it found two walks that never
ended on a horizon of no thickness (in two new gates and in the acidic
subgroups of 0.9.299), and eleven verdicts decided where the data leave
them open, or open where they decide.

Outside the four orders, four readings change; none of the 6,000 random
profiles changes order, group, or subgroup of the Organic, Gley and Ultic
Soils against 0.9.299:

* the acidic subgroups of the Gley Soils no longer hang on a horizon of no
  thickness under the A;
* recorded coatings of moist value 5 may be cutanic (the Diagnostics admit
  value 5 with chroma 3 or less, and the coatings chroma is not recorded),
  so the Granular Acid Gley Soils leave such a B open;
* the Magnesic Yellow Ultic Soils read a mineral row of unknown role as a
  possible B;
* the vertic criteria of the Vertic Melanic Soils read a recorded grade
  below moderate, or a structure type other than blocky or prismatic, as
  ruling the pedality out on its own (it was undecided).

# soilKey 0.9.299 (2026-10-28)

## New Zealand Soil Classification: the subgroups of the Organic, Gley and Ultic Soils

`classify_nzsc4()` now keys the 16 groups of the Organic, Gley and Ultic
Soils to their 70 subgroups, following the keys to subgroups of the New
Zealand Soil Classification, 4th edition: from the Buried-podzol Litter
Organic Soils to the Buried-granular and Typic Yellow Ultic Soils. The name of
such a profile is now its subgroup (for example "Peaty Orthic Gley Soils",
"Mottled Yellow Ultic Soils"); the group and the order stay in `levels`. The
subgroups of the other 12 orders follow.

* The keys live in `inst/rules/nzsc4/subgroups/`, one file per order, in the
  book's order. The last subgroup of each group is the default, except under
  the Densipan and Perch-gley Ultic Soils, which the text gives none.
* The gates are in `R/diagnostics-nzsc-subgroups.R`. Criteria the groups
  share use one gate with arguments: sulfidic, saline, fluid, peaty, acidic,
  concretionary, calcareous, pan, mottled, nodular, argillic, and the
  horizons below a densipan or an eluvial horizon.
* New diagnostics: sulfidic and saline soil material, the mottled profile
  form, buried peaty topsoils, coatings on 10 % or more of the ped faces and
  dark coatings.
* New horizon attributes: `sphagnum_fibres_pct` (the Sphagnic Fibric Organic
  Soils) and `shelly_layer` (the Calcareous Recent Gley Soils).

Hugo decided four readings:

* sulfidic soil material that is not measured is absent when no horizon in
  the window can look sulfidic in the field (a reductimorphic horizon with a
  hue of 5Y or less red, or black), as the text describes it; otherwise it is
  undecided;
* saline soil material that is not measured is absent when the horizons are
  designated and none carries the z suffix; with no designations it is
  undecided;
* the Sphagnum share of the fibres and a shelly layer are recorded when met,
  so not recorded reads as none;
* a Densipan or Perch-gley Ultic Soil that passes no subgroup keeps the group
  name, with a warning.

The other readings are in `inst/rules/nzsc4/readings.yaml`. Two subgroup names follow their groups' spelling: Typic Sulfuric Gley
Soils (the text writes "Sulphuric") and Typic Yellow Ultic Soils (the text
writes "Soil").

Outside the subgroups, two readings change:

* "in some part" now finds a run of 10 cm that passes when it lies directly
  below a horizon left undecided (it was read as undecided);
* the peaty topsoil reads a horizon with a recorded mineral field texture and
  no measured carbon as not peat (it was undecided), as the text defines it
  by peat or peaty textures.

Both can only turn an undecided order or group into a decided one.

An independent review against the text of the book found six defects, all
fixed: (f) horizons counted in the mottled profile form, which the 4th
edition excludes; the window of the organic material measured from its top
rather than from the soil surface; a buried podzol written with the buried
prefix missed under litter; a B horizon ending above 60 cm left undecided in
the Granular Acid Gley Soils; the peat reading above; and coatings of value
5 left open in the Podzolic Yellow Ultic Soils. Recorded dark coatings now
make a cutanic horizon without the (h) notation in the Granular Acid Gley
Soils.

A second review checked the logic with 12,500 random profiles: no errors, no
row-order effects, and every order or group that changes against 0.9.298
goes from undecided to decided through "in some part". Its findings are
fixed: a buried peat under a peaty horizon that is not a topsoil was missed;
an undesignated row under the A, nothing described under litter, a window
the description does not reach (for sulfidic and saline material), a gap
above the clay of the Buried-granular Yellow Ultic Soils and a densipan with
no depths were read as absent rather than undecided; one decisive lab value
did not settle "less silt than clay"; and a pan could pass as the argillic
horizon of the Argillic Orthic Gley Soils. The readings file now has 50
topics.

# soilKey 0.9.298 (2026-10-28)

## New Zealand Soil Classification: the 77 groups

`classify_nzsc4()` now keys each of the 15 orders to its groups, following
the keys to groups of the New Zealand Soil Classification, 4th edition: 77
groups, from the Litter, Fibric, Mesic and Humic Organic Soils to the Hydric,
Gley, Hydrothermal, Cryic, Rocky, Sandy, Fluvial, Tephric and Orthic Raw
Soils. The name of a classified profile is now its group (for example
"Orthic Brown Soils", "Fragic Pallic Soils"); the order stays in
`rsg_or_order` and in `levels`. Subgroups follow.

The groups read diagnostics the order key did not need: sulfuric and tephric
soil material, densipans, perch-gley features, redox-mottled horizons, peaty
topsoils, slowly permeable layers, nodular horizons and fluvial features.
They also read new horizon attributes: `packing_degree`, `ksat_mm_h`,
`penetration_resistance_kpa`, `redox_segregations_location`,
`sulfate_s_pct`, `roots_abundance`, `maori_cultural_layer` and
`coatings_value_moist`. On the site they read `soil_temp_30cm_C`,
`mean_annual_air_temp_C`, `surface_water`, `methane_odour`,
`mechanical_mixing_depth_cm` and `artificially_drained`.

Hugo decided four readings:

* the new attributes are created, as above;
* features that a describer records when met are absent when not recorded:
  stratification, nodules, cracks, refuse and Maori cultural layers,
  permafrost, surface water and mechanical mixing;
* the pans that the text names as often working as slowly permeable layers
  count as such: fragipans, argillic horizons, densipans, humus-pans,
  ortstein-pans, and lithic and paralithic contacts;
* the hydrothermal groups compare the soil temperature at 30 cm with the air
  temperature, in two new site fields.

The other readings are in `inst/rules/nzsc4/readings.yaml`, which now has 39
topics.

Two independent reviews checked the groups: one against the text of the book,
one for the logic, with 3,600 random profiles. Their findings are fixed, and
they changed the order key in three places:

* clay lamellae make an argillic horizon only when they add up to 15 cm
  within 90 cm;
* outside the New Zealand notation a Cr reads as a paralithic contact;
* site fields are read one value at a time.

Outside the NZSC, `ensure_horizon_schema()` now reads "yes" and "no" (and
y, n, 1, 0) in logical columns as TRUE and FALSE; before, they became NA. The
other systems do not change: the 44 canonical fixtures are identical.

# soilKey 0.9.297 (2026-10-28)

## The SiBCS family no longer repeats the class, and reads texture from mineral material

Two fixes to the family (5th level) of a SiBCS name, as Hugo decided:

* A class that already states the base saturation takes none in its family.
  The book uses the base saturation "except for the classes that already
  consider it at a higher level" (Cap 18, p 285), and the same rule holds
  for "álico" when the class is already Alumínica. The rule used to be set
  per order, so the Gleissolos Háplicos Ta Eutróficos repeated "eutrófico".
  It is now read on the class name itself. `classify_sibcs_familia()` gains
  `nome_superior` (the name of the higher levels), which `classify_sibcs()`
  passes.
* The textural group, the gravel class and the skeletal constitution are
  averaged over the mineral horizons only, in every order: an H or O horizon
  no longer enters them. The Organossolos give their texture only through the
  underlying material ("Organossolos Háplicos, material subjacente médio"),
  no longer through a textural group of the whole profile ("textura média").

Four family labels of the 44 canonical fixtures change (two Gleissolos, two
Organossolos); no class changes, and the fixtures, named without the family,
are identical.

# soilKey 0.9.296 (2026-10-28)

## The SiBCS family label in the words of the book

The family (5th categorical level) of a SiBCS name used to be written with
soilKey's internal codes, as in "Latossolos Vermelhos Eutróficos típicos,
muito_argilosa, moderado". It is now written as the SiBCS (2018, Cap 18)
writes it: "..., textura muito argilosa, A moderado". The textural group or
subgroup follows "textura" ("textura média-arenosa"), with the gravel class
and "esquelética" after it with no comma and the sand mineralogy in
parentheses ("textura argilosa pouco cascalhenta (micácea)"). The surface
horizon follows "A" ("A moderado", "A húmico"), and the histic one reads
"horizonte hístico". In the Organossolos, the underlying material reads
"material subjacente argiloso", and the woodiness reads "muito lenhoso".
Hugo decided three of these readings: "horizonte hístico", "material
subjacente", and writing the gravel class and "esquelética" together when
both apply. The names keep the plural of the key's classes. The codes in
`FamilyAttribute$value` do not change, and neither does any class: the 44
canonical fixtures, named without the family, are identical. Seen in the
name of an ISRIC profile sent by Jelle Janssen.

# soilKey 0.9.295 (2026-10-28)

## Many profiles from one horizon table

`classify_profiles()` classifies every profile of a table that has one row
per horizon and a column saying which profile each horizon belongs to, as a
survey or a database export usually comes. Site properties such as
`soil_moisture_regime` come from a second table (one row per profile) or a
list named by profile id. A `map` renames columns and converts their units on
the way in: `data.frame(from = "ORGC", to = "oc_pct", factor = 0.1)` reads an
organic carbon in g/kg as soilKey's %. The names are checked once for the
whole table, so a column that no key reads is named in one warning, not once
per profile. The result has one row per profile, in the order of the table,
with the class name in each system, the attributes the keys asked for and
did not find, and the error of a profile that could not be classified (the
others are classified as usual). The full `classify_all()` results and the
pedons ride along as attributes. Asked for by Jelle Janssen (ISRIC), who
classifies ISIS profiles this way. No classification changes: the 44
canonical fixtures are identical.

# soilKey 0.9.294 (2026-10-28)

## A warning for the attributes that no key reads

A horizon column that soilKey does not recognise, or a site property it does
not know, used to be kept in the `PedonRecord` and read by no key without a
word, so a misspelt column (`organic_carbon_gkg`) or a site property given as
a horizon column (`soil_moisture_regime`) went unnoticed. `PedonRecord$new()`
now warns about them when it is called from outside the package, naming each
one with the closest recognised name: "organic_carbon_gkg (did you mean
oc_pct?)", "soil_moisture_regime (a site property:
site$soil_moisture_regime)". `read_pedon_csv()` warns for the file it reads,
and now also accepts a spreadsheet whose columns are all aliases (`Top`,
`Bottom`, `Clay`, `SOC`), which it used to refuse. The loaders (BDsolos, FEBR,
WoSIS, NatSoil, NPDB, KSSL, LUCAS, AfSP, Redape) and the other constructions
inside soilKey keep extra columns on purpose and stay quiet. The warning has
the class `soilKey_unrecognised_attributes`;
`options(soilKey.warn_unrecognised = FALSE)` turns it off. The new
`unrecognised_attributes()` returns the same list as a table, for a pedon or
for a horizon table and a site list, so a batch can be checked before it is
built. Suggested by Jelle Janssen (ISRIC).

The site properties now have a reference of their own, as the horizon
attributes have had since 0.9.266: `inst/schemas/site-attributes.yaml`, read
by the new `site_attribute_spec()`, gives each recognised property its type,
unit, description, the values the keys compare it with and the keys that read
it, and `inst/ATTRIBUTES.md` lists them after the horizon attributes. A test
fails when the code reads a site property that the file does not list.

In soilKey Pro, opening a session file keeps the site properties that
soilKey recognises beyond the editor's own fields (`soil_moisture_regime`,
`drainage_class`, ...): they reach the pedon and the classification and are
saved again with the session. Until now they were dropped. A notice lists the
horizon columns and site properties that no key reads, on opening a session
and on uploading a CSV. No classification changes: the 44 canonical fixtures
are identical.

# soilKey 0.9.293 (2026-10-28)

## New Zealand Soil Classification: the 15 orders

A new, experimental and opt-in system: the New Zealand Soil
Classification, 4th edition (Manaaki Whenua - Landcare Research 2026),
read from its Quarto source of 7 October 2026. `classify_nzsc4()` keys a
pedon to the 15 orders of the Key to Soil Orders (Organic, Gley, Ultic,
Podzol, Allophanic, Pumice, Melanic, Semiarid, Oxidic, Granular, Pallic,
Brown, Anthropic, Recent and Raw Soils); `make_nzsc4_example()` gives one
example profile per order. Groups and subgroups follow; the Cryic Soils of
the appendix are not keyed. `classify_all(systems = "nzsc")` runs it.

The diagnostics read the New Zealand horizon notation (Bw, Bt, Bg, Bh,
Bs, Bo, Bx, Bw(g), the pans Bhm, Bsm, Blm, Bfm, CR, an e prefix for
anthropic soil material), moist colours, texture groups, pH, P retention,
bulk density, volcanic glass, coarse fragments and their origin,
structure and clay coatings, and the New Zealand field tests, as five new
horizon attributes: `naf_test` (reactive aluminium), `childs_test`,
`sensitivity_class`, `fluidity_class` and `stickiness`. From the site:
drainage class, parent material (Melanic Soils), human truncation and an
overthickened A horizon (the note to the key).

Three readings are Hugo's decisions: a reductimorphic horizon in a
designated profile is read from its notation (g, r, or peat), as pale B
horizons of Pallic Soils have low chroma without being reduced; a
measured P retention decides allophanic soil material, the field tests
standing in only when it is not measured; and a reductimorphic horizon
that reaches the base of the B horizon meets the depth condition of the
Gley Soils. The other readings are in
`inst/rules/nzsc4/readings.yaml`. WRB, SiBCS, USDA, Canadian and
Australian results do not change.

# soilKey 0.9.292 (2026-10-27)

## Australian classification: the families

`classify_asc3(include_family = TRUE)` now places a soil in the family
classes of its order and writes them after the name, as the book does:
"Bleached, Eutrophic, Red Chromosol; thick, non-gravelly, loamy/clayey,
deep". The new `classify_asc3_family()` returns each criterion as a
`FamilyAttribute` with its code (YY when the data cannot decide the class,
ZZ when no class of the order fits); the result keeps them in
`trace$family`, `trace$family_label` and `trace$family_code`.
`classify_all(include_family = TRUE)` passes the option on when the
Australian key is run. Without the option nothing changes.

The criteria are those of each order page: the thickness of the A1, or of
the whole A in the Kurosols, Sodosols and Chromosols and in the Kurosolic,
Sodosolic and Chromosolic Hydrosols (the book's general rule, where the
order pages disagree); gravel (2-60 mm) of the surface and A1, the larger
of the two estimates; the field texture of the A1 and the most clayey one
of the B; soil depth (to rock, a cemented pan or saprolite; otherwise to
the C under the B, or the depth described in a soil without a B); and
water repellence. The Vertosols use laboratory clay, the Calcarosols the
depth of the Bk, the Arenosols and Rudosols the surface soil, the
Organosols their organic materials. The Anthroposols and the tidal
Hydrosols have none. The classes and the criteria of each order are in
`inst/rules/asc3/families.yaml`, the readings in `readings.yaml`.

Two horizon attributes: `gravel_pct` (gravel-sized fragments) and
`water_repellence`. The NatSoil reader fills them, with the coarse
fragments of the ground surface and the surveyors' family codes, and
`benchmark_natsoil(family = TRUE)` compares each criterion. On the NatSoil
profiles whose order agrees, the decided classes agree with the surveyors
for 85 % of the thicknesses, 87 % of the A1 textures, 97 % of the B
textures, 83 % of the depths and 75 % of the gravel classes. The Vertosol
clay classes are mostly undecided: only 29 of 291 NatSoil Vertosols have
laboratory clay.

No order, suborder, great group or subgroup changes on NatSoil. WRB,
SiBCS and USDA results do not change.

# soilKey 0.9.291 (2026-10-27)

## Australian classification: subgroups of the Rudosolic and Tenosolic Hydrosols

The Hydrosol page points the Rudosolic and Tenosolic great groups of the
Redoxic and Oxyaquic Hydrosols to the subgroup lists of the Rudosols and
Tenosols. `classify_asc3()` now keys such a soil, apart from wetness,
through the Rudosol or Tenosol suborders, and takes the list of the
suborder it keys to: "Basic, Rudosolic, Oxyaquic Hydrosol". Those lists
are the same for every great group of a suborder, so the soil is keyed to
the suborder only. Apart from wetness, the Hypersalic Rudosol suborder is
read by its salinity alone (its other conditions, no water table and no
mottling, are wetness). A suborder with no list (Shelly, Carbic) gives no
subgroup. A suborder left undecided before the one assigned makes the
subgroup provisional only when it would lead to another list. The Salic
Rudosolic and Tenosolic Hydrosols keep the Salic list.

The rules name these great groups under `subgroups_from` in the Hydrosol
subgroup file, with site fields set for the walk only
(`.apart_from_wetness`); `load_rules()` reads such blocks, the rule
validator checks them, and the key walks the other order as the main key
would, only as deep as the choice of list needs. The walk shows in the
trace as a phase of its own ("suborders, as a Rudosol").

On 9,700 licensed NatSoil profiles only the five that key to these great
groups change: two get a subgroup, two stay undecided for lack of data,
and one fits none of the three Rudosol subgroups (exactly half of its soil
is strongly acid). Nothing else changes, at any level. WRB, SiBCS and USDA
results do not change.

# soilKey 0.9.290 (2026-10-26)

## Australian classification: carbonate segregations recorded as none

A horizon whose carbonate segregations are recorded as none (0 %), with no
effervescence or CaCO3 recorded, was left undecided by the test of
"calcareous", although the same horizon with nothing recorded is read by
its pH (a pHw below 7.0 is not calcareous, as free carbonate holds the pH
higher). A recorded 0 % of segregations says nothing of the fine-earth
carbonate, so the pH rule now applies to it too. Found in the review of
0.9.289.

On 9,700 licensed NatSoil profiles no order or suborder changes, and 32
more orders are decided (1,305, at 74.5 %). 98 great groups change, nearly
all to Eutrophic, which needs B and BC horizons that are not calcareous;
the great groups that agree with the survey go from 1,076 to 1,140 and the
subgroups from 499 to 555. WRB, SiBCS and USDA results do not change.

# soilKey 0.9.289 (2026-10-26)

## Australian classification: subgroups of the Hydrosols, Arenosols, Rudosols and Tenosols (experimental)

`classify_asc3()` keys the subgroups of the last four orders, so every
order that has great groups now keys its subgroups. WRB, SiBCS and USDA
results do not change, nor do the Australian orders, suborders and great
groups, nor the subgroups of the ten orders keyed before.

* Hydrosols: Fibric, Hemic and Sapric for the Histic-Hypersulfidic and
  Histic-Hyposulfidic Intertidal Hydrosols; Episulfidic-Petrocalcic,
  Episulfidic and Petrocalcic for the Salic ones; 40 subgroups for the
  Redoxic and Oxyaquic ones, without the acidic subgroups for the
  Kurosolic great groups and without the sodic and natric ones for the
  Sodosolic, as the page says. The page sends the Rudosolic and Tenosolic
  great groups to the Rudosol and Tenosol lists; that is not keyed yet, and
  they keep the great group's name.
* Arenosols: 13 subgroups for the Chernic, Bleached and coloured
  suborders, read over the upper 1.0 m of the profile; the carbonate great
  groups take the list without Humose-Calcareous, Melanic-Calcareous and
  Calcareous, as the page says.
* Rudosols: Acidic, Basic and Calcareous, read over the soil above any
  rock, pan or buried soil; for every suborder but the Shelly and Carbic.
  The Hypergypsic, Lutic and Stratic Rudosols have no great groups, and
  their subgroups follow the suborder ("Basic, Lutic Rudosol"): the
  hierarchical key skips a level only when the next one has a list named by
  the exact path.
* Tenosols: one list per suborder (4, 9, 6, 3, 12, 7 and 13 subgroups):
  chernic horizons with the acidity and carbonate of the B, the A2 or the
  solum; Subpeaty, Subhumose, Submelacic and Submelanic (every requirement
  but the thickness); Lithocalcic, Supracalcic and Hypercalcic for the
  Calcenic Tenosols, read on the Calcenic horizon.
* Readings decided with Hugo: the Salic Hydrosols take only the three
  subgroups listed; in the coloured Tenosols Basic is read as no part of
  the solum calcareous (the page's "the A2 horizon" read as a misprint);
  in the Chernic Tenosols "the B horizon (if present)" with no B fails.
* `benchmark_natsoil()` reads the survey's subgroup of a suborder with no
  great groups from its great-group column.
* The readings are in `inst/rules/asc3/readings.yaml` (57).

On 9,700 licensed NatSoil profiles, 195 of the 414 profiles of these four
orders that reach a subgroup list get a subgroup (75 decided). Only 20 can
be compared with the survey (5 agree): most survey labels of these orders
use older codes, and many profiles stop at a great group the survey does
not give.

# soilKey 0.9.288 (2026-10-26)

## Australian classification: subgroups of the Vertosols, Calcarosols, Podosols and Organosols (experimental)

`classify_asc3()` keys the subgroups of the Vertosols, Calcarosols,
Podosols and Organosols, one list per order as the order pages give them
(43, 12, 15 and 8 subgroups). WRB, SiBCS and USDA results do not change,
nor do the Australian orders, suborders and great groups, nor the
subgroups of the six orders keyed before.

* Vertosols: the acid sulfate subgroups as for the acid sulfate great
  groups; Salic as for the Salic Hydrosols; Duric and Petrocalcic; the
  upper 0.1 m of the solum sodic (Episodic, by its weighted average ESP);
  the upper 0.5 m of the solum and the solum below it strongly acid,
  calcareous, mottled or with an ESP of 15 or more (Epiacidic,
  Endocalcareous, Epihypersodic and their combinations); Magnesic over the
  B. The solum is read as described, pans left out, rows of unknown role
  and horizons of unknown depth tried every way they may go.
* Calcarosols: Melanic, Vertic, Subplastic and Gypsic; Epibasic and
  Hypervescent as for the Calsilic Arenosols; Supravescent; Epihypersodic
  and Endohypersodic; Ceteric for the others. The Carbonatic Calcarosols
  take the list without Supravescent and Hypervescent, as the page says
  they are not required there.
* Podosols: a strongly or weakly coherent B (Parapanic, Fragic), a
  densipan in the A2 (Densic), a thin ironpan in or under the B (Placic),
  a siliceous pan under the B, a ferric horizon in the B, with the peaty,
  humose, melacic and melanic combinations.
* Organosols: what the organic materials directly overlie (Lithic,
  Paralithic, Marly, Rudaceous, Modic), a thin ironpan below them, and
  layers of unconsolidated mineral material within or below them (Terric
  within 0.8 m, Regolithic below).
* The examples of the four orders key to a subgroup ("Endocalcareous,
  Self-mulching, Black Vertosol").
* The readings are in `inst/rules/asc3/readings.yaml` (56).

On 9,700 licensed NatSoil profiles, 424 of the 533 profiles keyed to these
four orders with a great group get a subgroup. Of the 347 whose order,
suborder and great group agree with the survey and whose survey gives a
subgroup of the key, 261 get one and 113 (43 %) the survey's (Vertosols 100
of 239). Nearly every Vertosol subgroup stays provisional: the Sulfuric
subgroup needs the pH of the upper 1.5 m, and the Sulfidic one a sulfide
test, rarely recorded (read as absent, a test not done would decide 11
more Vertosols and leave the agreement unchanged).

# soilKey 0.9.287 (2026-10-26)

## Australian classification: subgroups of the Dermosols, Kandosols and Ferrosols (experimental)

`classify_asc3()` keys the subgroups of the Dermosols, Kandosols and
Ferrosols, one list per order as the order pages give them (33, 29 and 14
subgroups). WRB, SiBCS and USDA results do not change.

* These pages read the B2 horizon where the Kurosols, Sodosols and
  Chromosols read the B2t: mottled, whole coloured, and strongly acid
  (Acidic, Humose-Acidic, Melanic-Acidic, Ferric-Acidic, Manganic-Acidic,
  Acidic-Sodic, Bleached-Acidic, Acidic-Mottled: the major part of the B2
  with a pHw below 5.5, or a pHCa below 4.6 without pHw).
* New in the Kandosols: an argic or a bauxitic horizon within the B
  (Argic, Bauxitic). New in the Ferrosols: Snuffy (an A horizon with a very
  fine granular structure, peds under 2 mm, and a weak or very weak dry
  strength), and Sodic read on the lower part of the B2, as printed.
* The Kandosol page says the acidic subgroups are not required for the
  calcareous great groups: the Hypocalcic, Lithocalcic, Supracalcic,
  Hypercalcic and Calcic Kandosols take the list without them, as the
  Natric Kurosols take theirs without the Sodic ones.
* The examples of the three orders record organic C, mottles and the CEC,
  and key to a subgroup ("Haplic, Eutrophic, Brown Dermosol").
* The dark horizon of the Chernic suborders (melacic or melanic) is now
  decided over every extent it may have, as the subgroups read it since
  0.9.286. Before, a horizon 0.2 m thick in its surely black rows was read
  on those rows only, though rows that may be black below could change its
  major part, and a gap in the record directly above the first row that may
  not be black was not counted as possibly black. Such a gap may likewise
  be organic material (the peaty horizon, where the mineral surface lies) or
  fusic material (Fusic Anthroposols); where the mineral surface may lie at
  more than one depth, a verdict holds only when it holds at each. Over a
  clear or abrupt textural B, a row of unknown role between two A horizons
  is now read as an A horizon (before, as not black).
* The readings are in `inst/rules/asc3/readings.yaml` (55).

On 9,700 licensed NatSoil profiles the orders and great groups do not
change. The Chernic test becomes undecided on five profiles, where a
suborder was already provisional, and decided on one: a Tenosol whose A1
(17.6 % organic C) may or may not be organic material gives a humose
horizon either way, and is now Chernic. Of the 799 profiles keyed to these three
orders with a great group, 549 get a subgroup. Of the 278 whose order,
suborder and great group agree with the survey and whose survey gives a
subgroup of the key, 213 get one and 113 (53 %) the survey's (Dermosols
32 of 65, Kandosols 80 of 137, Ferrosols 1 of 11). Most of the others stop
at Humose-Mottled, which needs organic C in the upper 0.2 m. The Ferrosol
disagreements come from one survey whose profiles have 5 to 9 % organic C
(combustion, a method the glossary names) at the surface, a humose horizon,
but were surveyed Haplic or Acidic.

# soilKey 0.9.286 (2026-10-26)

## Australian classification: subgroups of the Kurosols, Sodosols and Chromosols (experimental)

`classify_asc3()` now keys a fourth level, the subgroups, for the Kurosols,
Sodosols and Chromosols; the other orders follow. WRB, SiBCS and USDA
results do not change, nor do the Australian orders, suborders and great
groups, but their names now take the book's form.

* The names follow the general form the book gives, "Subgroup, Great Group,
  Suborder Order": "Bleached, Eutrophic, Red Chromosol". The great groups
  carry the same comma ("Eutrophic, Red Chromosol"; before, "Eutrophic Red
  Chromosol").
* One list per order, as the order pages give it, applied under every great
  group of the order: chernic horizons (peaty, humose, melacic, melanic),
  a conspicuously bleached A2, a B with at least 0.3 m of vertic
  properties, ferric and manganic horizons within the solum, the lower
  part of the B sodic, the B2t mottled or whole coloured, a reticulate
  horizon below the B2t, a gypsic horizon in the B or BC, a siliceous pan
  in the lower A or upper B, Magnesic, base status, the carbonate classes
  and Effervescent, combined as the lists combine them (Bleached-Sodic,
  Mottled-Sodic, Ferric-Sodic and so on).
* The hierarchical key finds a list for every class under a part of the
  path ("CH-*") and writes a name on the name of the class above
  ("Bleached, {parent}"); other systems do not change.
* `benchmark_natsoil()` reports the subgroups (`$subgroups`).
* The readings are in `inst/rules/asc3/readings.yaml` (54).

On 9,700 licensed NatSoil profiles the orders, suborders and great groups
do not change. Of the 560 profiles keyed to these three orders with a great
group, 294 get a subgroup. Of the 173 whose order, suborder and great group
agree with the survey and whose survey gives a subgroup of the key, 79 get
one and 37 (47 %) the survey's; most of the others stop at Humose, which
needs organic C in the upper 0.2 m, rarely measured. The disagreements
checked are survey practice: B horizons of heavy clay with many
slickensides and lenticular peds surveyed Haplic rather than Vertic.

# soilKey 0.9.285 (2026-10-24)

## Australian classification: great groups of the Hydrosols, Arenosols, Rudosols and Tenosols (experimental)

`classify_asc3()` now keys the great groups of every order that has them
(the Anthroposols have none in the text). WRB, SiBCS and USDA results do
not change; three Australian orders change on NatSoil, because an AC
horizon is now part of the solum (see below).

* Hydrosols: the acid sulfate groups as for the Organosols (with
  Histic-Hypersulfidic and Histic-Hyposulfidic), then by suborder: Histic,
  Faunic, Epicalcareous and the particle-size groups for the tidal and
  subaqueous soils; Gypsic, Halic, Epicalcareous, Mottled and Haplic for the
  Supratidal and Hypersalic; and for the Extratidal, Salic, Redoxic and
  Oxyaquic, Petroferric and the order the soil would key to apart from
  wetness (Calcarosolic to Rudosolic).
* Arenosols, Rudosols and Tenosols: what the soil directly overlies (hard
  rock, saprolite, marl, unconsolidated material, a red-brown hardpan, a
  calcrete pan, ferricrete, a siliceous pan, a reticulate horizon, coquina),
  "overlie" being read as "directly overlie"; ferric, bauxitic and manganic
  horizons; gypsic and argic horizons; Inceptic (a C/B), Tephric, Andic,
  Gritty, Gravic, Halic and Subhalic; the carbonate classes; Kandic, Tenic
  and Palic Arenosols; Epibasic and Hypervescent Calsilic Arenosols; Arenic
  Tenosols; Colluvic, Fluvic and Lithosolic Clastic Rudosols. In the
  Arenosols hard layers count only below 1.0 m.
* Nodules not recorded are read as absent (more than 20 % of nodules
  shows); this also applies to the Sesqui-Nodular Tenosols, which read them
  as unknown since 0.9.282. A gritty feel, heavy minerals, halite and
  gypsum not recorded are absent too.
* What the soil directly overlies, shared with the Calcarosols: a C or D
  horizon of unrecorded origin is no longer read as possibly hard rock, nor
  is sandy material read as possibly marl (Lithic and Marly Calcarosols
  included); a cemented layer that is no pan may still be rock, while one
  whose origin names saprolite is saprolite; a horizon recorded as a pan
  that makes up the layer (ferricrete, calcrete and the like) is a pan
  whatever its designation, and ferricrete or silcrete named by the layer
  or rock origin counts; an AC horizon is part of the solum, which moves
  three NatSoil profiles from Tenosols or Rudosols to Vertosols and
  Calcarosols, all three now as surveyed.
* New horizon attributes: `manganiferous_nodules_pct`,
  `gypsum_visible_pct`, `halite_visible_pct`, `gritty`,
  `heavy_minerals_pct`. The NatSoil reader fills the first three from the
  segregations, counting only those recorded as nodules or segregations of
  that kind (NaN where the recorded classes leave the amount on either side
  of the limit, which the key reads as undecided).
* The readings are in `inst/rules/asc3/readings.yaml` (52).

On 9,700 licensed NatSoil profiles, three orders change, all three now as
surveyed (1,273 decided, 74.8 % of them agreeing), and 51 fewer suborders
are provisional, from the Sesqui-Nodular reading. Of the 3,536 profiles
keyed to these four orders, 398 get a great group; most stop at the
suborder, which is itself undecided for many Tenosols, and the Hydrosols
wait on the sulfide test. Only 42 can be compared with the survey (order
and suborder agreeing, a great group of the key): 21 get the survey's
great group. Most Tenosol labels use the suborder codes of the editions
before 2021.

# soilKey 0.9.284 (2026-10-24)

## Australian classification: great groups of the Vertosols, Calcarosols, Podosols and Organosols (experimental)

`classify_asc3()` now keys the great groups of four more orders, ten in
all. WRB, SiBCS and USDA results do not change, nor do the Australian
orders, suborders and the great groups of 0.9.283.

* Vertosols, by the surface: Self-mulching (a moderately or strongly
  self-mulching surface in the site record, as the text asks that the
  group be used only when the condition is not in doubt), Epipedal (a pedal
  A, blocky or polyhedral), Crusty (a surface crusty horizon over pedal
  clay: a surface crust in the site record, or a surface horizon 30 mm
  thick or less that is massive or weakly structured), Massive (massive or
  weak blocky), Peaty. The site record is read item by item, negations
  included; a thin organic layer at the top is passed over.
* Calcarosols: Duric and Petrocalcic (a pan directly under the soil),
  Rendic (the A directly over a Bk of more than 80 % soft carbonate,
  Hypercalcic only), Argic (clay lamellae in the B), Pedal, and what the
  soil directly overlies (Lithic, Paralithic, Marly, Regolithic), read from
  the designation, the layer or rock origin and any pan; a C of unrecorded
  origin, a description that stops at the base of the solum, or a row of
  unknown role that may be part of it, leaves them undecided.
* Podosols, by the kinds of Podosol B in the designations (Bs, Bhs, Bh,
  and a Basi under a Bh), with each suborder's own list as printed, and
  Pipey where an A2 has a tongued boundary. The NatSoil reader now keeps
  the boundary shape (`boundary_topography`).
* Organosols: Folic (freely drained, organic materials as in Definition
  (ii)), Sulfuric, Sulfidic and the groups that replace them only with the
  evidence (Hypersulfidic, Hyposulfidic; no monosulfidic test is
  recorded), then Calcareous, Basic, Acidic. The upper 1.5 m includes
  buried horizons, and a part of it not described may hold acid sulfate
  material; sulfidic material not tested leaves the great group
  provisional.
* The readings are in `inst/rules/asc3/readings.yaml` (48).

On 9,700 licensed NatSoil profiles, of the 465 of these orders whose order
and suborder agree and whose survey gives a great group of the key, 75.5 %
get the survey's great group, and 76.4 % of the 301 decided (the
Vertosols: 324 of 418, and 220 of the 281 decided). Several surveys record
the crust of a Crusty Vertosol as a thin A11 over a pedal A12, which is
read as the text describes it. The Calcarosols and Organosols stay
provisional for want of data: the suborder or what lies under the solum
for the first, the sulfide test for the second.

# soilKey 0.9.283 (2026-10-23)

## Australian classification: great groups of the Kurosols, Sodosols, Chromosols, Ferrosols, Dermosols and Kandosols (experimental)

`classify_asc3()` now keys a third level, the great groups, for the six
orders with a B horizon of their own; the name is the great group's
("Eutrophic Red Chromosol"). The other orders follow. WRB, SiBCS and USDA
results do not change, nor do the Australian orders and suborders.

* Pans in or under the B: Duric (red-brown hardpan), Petroferric
  (ferricrete), Petrocalcic (a calcrete pan under a B that is not
  calcareous), Placic (thin ironpan), from a new attribute `pan_type`
  (NatSoil records it, "none" included) or the designation (km, sm, qm,
  cm). A pan the record does not show is absent, as the order reads hard
  layers; a cemented layer of unknown kind leaves it undecided.
* The B2t or B2: Pedaric, Subplastic, Magnesic (exchangeable Ca/Mg below
  0.1), Natric, base status (Dystrophic, Mesotrophic, Eutrophic, from the
  exchangeable bases and clay, the clay range of the field texture where
  no lab clay), the Sodosol ESP classes with their Mottled forms, and the
  carbonate classes as for the Calcarosols.
* Ferrosol Calcareous, Kandosol Leptic, Mellic and Andic.
* Hypocalcic now needs some carbonate in the solum (the class is of weakly
  calcareous soils); this does not change the Calcarosols.
* The hierarchical key finds the list under a class by the codes of the
  path to it ("CH-AA"), since the Australian suborder codes repeat from
  order to order; other systems do not change.
* `benchmark_natsoil()` reports the great groups (`$great_groups`).
* The readings are in `inst/rules/asc3/readings.yaml` (44).

On 9,700 licensed NatSoil profiles, of the 2,160 of these orders whose order
and suborder agree and whose survey gives a great group of the key, 21.6 %
get the survey's great group, and 80.6 % of the 98 decided. Most stay
provisional for want of laboratory data: exchangeable Ca and Mg (Magnesic,
before the base status classes), the bases and clay, mottles (the Mottled
Sodosols) and, for the Kandosols, the porosity the Mellic great group asks.

# soilKey 0.9.282 (2026-10-23)

## Australian classification: the suborders of the Anthroposols, Arenosols, Rudosols and Tenosols (experimental)

`classify_asc3()` now keys the suborders of all fifteen orders. WRB, SiBCS
and USDA results do not change, nor do the Australian orders.

* Peaty, humose, melacic and melanic horizons (Chernic suborders), as the
  glossary defines them: organic C against clay for the humose horizon;
  black moist colour, a dry value of 5 or less, pH and structure for the
  melacic and melanic ones. Without organic C or a dry colour they are
  undecided.
* The conspicuously bleached A2: an e (not a j) in the Australian notation,
  or the dry colour of the glossary (80 % bleached: mottles not recorded
  leave it undecided).
* Leptic suborders: rock, a hard layer or a calcrete pan, or a C horizon of
  saprolite, with its top within 0.5 m; a C of unrecorded origin, or a
  description that stops at 0.5 m or above, is undecided.
* Arenosols: Hypergypsic, Hypersalic (EC of a 1:5 extract above 2 dS/m, no
  water table, no mottles), Chernic, Carbonatic, Calsilic, Stratic,
  Bleached, then the colour of the upper 1.0 m of the profile.
* Rudosols: Hypergypsic, Hypersalic, Shelly, Carbic, Leptic, Arenic (loose
  or weakly coherent sands), Lutic, Stratic, Clastic (gravelly).
* Tenosols: Chernic-Leptic, Chernic, Sesqui-Nodular (a ferric or bauxitic
  horizon over more than half of the solum), Calcenic, Bleached-Leptic,
  Leptic, Bleached, then the colour of the upper 0.5 m of the profile.
* Anthroposols: the kind of human-made material, from the layer origin.
* A soil with no salinity record (no EC, no saline surface) is read as not
  highly saline; shells, carbic materials and fusic material the record
  does not mention are read as absent, as for the Hydrosol settings.
* New horizon attributes `ec_1to5_dS_m`, `strength_dry`, `strength_moist`,
  `ped_size_max_mm`, `ferric_nodules_pct`, `bauxitic_nodules_pct`.
* `load_natsoil_pedons()` reads them (EC method 3A1, strength classes, ped
  size, nodules from the segregations). Abundance classes are now added as
  ranges, each class strictly between its limits, so two records of 2-10 %
  and 10-20 % make 12-30 % (undecided at 20 %); before, their middles were
  added. This also applies to the soft and hard carbonate of 0.9.281.
* The readings are in `inst/rules/asc3/readings.yaml` (39).

On 9,700 licensed NatSoil profiles the orders are as before (43.9 %, 1,270
decided at 74.8 %). Of the 3,691 profiles of the fifteen orders whose order
agrees and whose survey gives a 3rd-edition suborder, 75.9 % get the
survey's suborder, and 95.7 % of the 679 decided. Rudosols: 20 of 61, and
the 10 decided agree; Arenosols 1 of 12 and Tenosols 15 of 222, none
decided. Most Tenosols stay provisional because the organic C of the upper
0.2 m, the dry colours, the nodules or the origin of a shallow C horizon are
not recorded; many surveyed Leptic Tenosols are described below 0.5 m with
no rock, and the text does not make them leptic.

# soilKey 0.9.281 (2026-10-22)

## Australian classification: suborders of the Organosols, Podosols, Hydrosols and Calcarosols (experimental)

`classify_asc3()` now keys the suborders of eleven of the fifteen orders.
The four added here are read from the order pages and the glossary; the
suborders of the Anthroposols, Arenosols, Rudosols and Tenosols follow.
WRB, SiBCS and USDA results do not change, nor do the Australian orders.

* Organosols: the kind of peat (fibric, hemic, sapric) with the greatest
  thickness; when kinds tie, the most decomposed (the Comment). The kind
  comes from the squeeze test of the glossary, then the rubbed fibre
  content, a peat field texture or an Oi, Oe or Oa designation.
* Podosols: the saturation of the B horizon, from its days of saturation or
  from the drainage class. Rapidly drained is Aeric (the suborders define
  free drainage so); moderately well and imperfectly drained are
  Semiaquic; very poorly drained is Aquic; a well drained site stays
  undecided between Aeric and Semiaquic. Sulfuric material (at least 15 cm,
  pH below 4 with jarosite or hypersulfidic material below) and
  hypersulfidic material (incubation test) as the glossary defines them;
  without the test they are undecided.
* Hydrosols: the setting of the soil from the site record (tidal zone,
  inland water, landform, a saline surface). A setting the record does not
  mention is read as absent, as the order key already reads tidal flooding.
  Then Redoxic (more than half of the profile mottled: more than 10 %
  distinct or prominent mottles) or Oxyaquic (more than half whole
  coloured).
* Calcarosols: Hypergypsic (gypsum over 50 % in the solum; read as absent
  when no gypsum is recorded), Carbonatic (more than 40 % fine-earth
  carbonate in the upper 0.1 m), Hypocalcic, then the carbonate classes of
  a calcareous horizon (Bk, BCk or 10 % or more carbonate segregations):
  Lithocalcic, Supracalcic, Hypercalcic, Calcic.
* New horizon attributes `carbonate_soft_pct` and `carbonate_hard_pct`
  (visual soft and hard pedogenic carbonate); `mottle_contrast` is now read
  by the ASC too.
* `load_natsoil_pedons()` reads the landform pattern, mottles (abundance
  and contrast) and the soft and hard carbonate of the segregations and
  calcrete fragments. Where a segregation of unknown form or a fragment of
  unknown lithology leaves the amount spanning two carbonate classes, it is
  unknown.
* The readings are in `inst/rules/asc3/readings.yaml` (30).

On 9,700 licensed NatSoil profiles the orders are as before (43.9 %, 1,270
decided at 74.8 %). Of the 3,396 profiles of the eleven orders whose order
agrees and whose survey gives a 3rd-edition suborder, 81.4 % get the
survey's suborder, and 95.7 % of the 669 decided. For the four orders added
here: Organosols 4 of 10 (the 4 decided agree), Podosols 27 of 48 (26 of
the 33 decided), Hydrosols 14 of 49 (6 of the 7 decided), Calcarosols 33 of
175 (the 2 decided agree). Most of these stay provisional for want of data
in NatSoil: the kind of peat, the contrast of the mottles, the form of the
carbonate segregations and laboratory carbonate.

# soilKey 0.9.280 (2026-10-22)

## Australian classification: the colour suborders (experimental)

`classify_asc3()` now keys the suborders of seven orders: the Kurosols,
Sodosols and Chromosols (the colour of the upper 0.2 m of the B2t), the
Ferrosols, Dermosols and Kandosols (the upper 0.5 m of the B2) and the
Vertosols (Aquic, then the colour of the upper 0.5 m of the solum). The
name is the suborder's ("Red Chromosol"); `rsg_or_order` keeps the order.
The other suborders follow. WRB, SiBCS and USDA results do not change.

* Colour classes as the ASC defines them (Red, Brown, Yellow, Grey,
  Black), from the moist matrix colour matched to the nearest chip; a colour
  half way between chips of different classes is of unknown class.
* The dominant class covers more than half of the window; when no class
  does, the one with the largest share, as the surveyors read it (a tie is
  undecided).
* Aquic Vertosols: saturation days of the upper 0.5 m, otherwise the
  drainage class as for the Hydrosols.
* An imperfectly drained site (some horizons wet for several weeks, in the
  Field Handbook) now counts as not saturated for 2-3 months, for the
  Hydrosols and the Aquic Vertosols; before, it was undecided. No order
  changes, but more are decided. Organic materials keep their own limit:
  "saturated for long periods" is 30 days, as in Soil Taxonomy, whose
  definition the glossary adopts, so poorly and imperfectly drained sites
  stay undecided there.
* `benchmark_natsoil()` also reports the suborders (`$suborders`).
* The readings are in `inst/rules/asc3/readings.yaml` (24).

On 9,700 licensed NatSoil profiles: the order agrees for 43.9 % as before,
and 1,270 are now decided (was 1,052), 74.8 % in agreement. Of the 3,114
profiles of these seven orders whose order agrees and whose survey gives a
suborder, 86.3 % get the survey's suborder, and 96.6 % of the 623 decided.

# soilKey 0.9.279 (2026-10-22)

## Australian classification: the field texture first in the textural B (experimental)

The clear or abrupt textural B of `classify_asc3()` (Kurosols, Sodosols,
Chromosols) now reads each horizon by its field texture grade, and by its
lab clay only when it has none. The glossary gives the limits in clay
"and/or" field texture and notes that where the two disagree the field
texture may be the better guide to how the soil behaves; before, lab clay
on both sides decided. The reading is in `inst/rules/asc3/readings.yaml`.

On the 9,700 licensed NatSoil profiles with an ASC order, 255 change order:
104 of them now agree with the surveyors, 56 did before. Agreement goes from
43.4 % to 43.9 % (45.7 % counting the Arenosols as Tenosols or Rudosols),
with 1,052 decided at 77.4 % (78.6 %). WRB, SiBCS and USDA results do not
change.

# soilKey 0.9.278 (2026-10-22)

## NatSoil (CSIRO) as a benchmark for the Australian key (experimental)

NatSoil, the national soil site database of CSIRO, can now be read through
its public API and used to check `classify_asc3()` against the orders the
surveyors gave. The data are licensed per site, most under CC BY 4.0, and
are not shipped: `fetch_natsoil()` saves them to a folder of your choice.
WRB, SiBCS and USDA results do not change.

* `fetch_natsoil()` downloads every public site (about 150 pages) and then
  the horizons, samples and laboratory results of each licensed site with
  an ASC order (one request per site, about 9,800), with a pause between
  requests. Files already on disk are kept, so a run can be resumed.
  Sites not licensed for external use are skipped.
* `load_natsoil_pedons()` turns the files into pedons: designations in the
  Australian notation, field texture grades, structure, moist and dry
  colours, slickensides, cracks, coarse fragments, calcareous segregations
  (records with only their nature are templates and are not read), pans,
  effervescence, plasticity type, the moisture when described, and
  laboratory pH, clay, exchangeable cations, CEC and effective CEC (whatever
  the extraction method, the "not recorded" methods included), ESP, organic
  C, free iron and carbonate. Samples count for the horizons they overlap,
  weighed by the overlap. A value below the detection limit ("<x") is read
  as half the limit, unless the limit lies above one the keys read. The
  field pH stands in only where the laboratory has no pH, and the loss on
  ignition gives organic C only in peat. The site keeps the surveyors'
  classification and confidence, the drainage class, the surface condition,
  coordinates, the licence and the citation.
* `benchmark_natsoil()` compares the orders: agreement for all pedons and
  for the decided ones, by the surveyors' confidence, recall by order, the
  confusion table and the first order left undecided. The surveys predate
  the 2021 Arenosols, so an Arenosol given to a surveyed Tenosol or Rudosol
  is also counted as agreement in the edition figures.
* New horizon attribute `esp_pct`: the exchangeable sodium percentage as
  the laboratory reports it. `classify_asc3()` reads it for the sodic
  condition when exchangeable Na or the CEC is not given.

On 9,700 licensed NatSoil profiles with an ASC order, the order agrees for
43.4 % (45.3 % counting the Arenosols as Tenosols or Rudosols); 1,045 are
decided, with 77.8 % agreement (79.1 %); of the 450 decided with the
surveyors' highest confidence, 80.2 % agree (81.8 %).

# soilKey 0.9.277 (2026-10-22)

## Australian Soil Classification: the 15 orders (experimental)

`classify_asc3()` keys a pedon to the orders of the Australian Soil
Classification, 3rd edition (Isbell and the National Committee on Soil and
Terrain 2021), following the online version with its errata. It is the
second national system on the registry (`soilkey_systems()`, alias `asc`)
and, like the Canadian one, runs in `classify_all()` only when asked.
Suborders, great groups, subgroups and families follow in later releases.
WRB, SiBCS and USDA results do not change.

* The key reads the Australian horizon notation of the Field Handbook (A1,
  A2e, B21t, B3, Bhs, 2B2b ...), and FAO or USDA designations by the role
  of each horizon (an E as an A2, a Bt as a B2t).
* New horizon attributes: `field_texture` (the Field Handbook's texture
  grades, such as SL, SCL or LMC, with + or -), `subplasticity`,
  `soil_water_status` (the moisture of the horizon when described) and
  `effervescence_class`. Where a limit is a texture grade the field texture
  decides and lab clay stands in; where it is in % clay, lab clay decides
  and the field texture stands in as the clay range of its grade.
* The clear or abrupt textural B (Kurosols, Sodosols, Chromosols) reads the
  glossary's clay figures, or its field-texture equivalents when lab clay is
  missing on either side of the boundary.
* From the site the key reads the drainage class and tidal flooding
  (Hydrosols), a cracking surface condition (Vertosols) and the notation of
  the designations.
* `make_asc3_example()` gives one small profile per order.
* The choices where the text is silent are listed in
  `inst/rules/asc3/readings.yaml` (21 readings). Two were settled on the
  survey data below: only a very poorly drained site counts as saturated
  for the Hydrosols (poorly drained is undecided), and the Vertosols' cracks
  are not required in a solum described moist (the key's "unless too
  moist").

Checked against NatSoil (CSIRO, NatSoil v13, CC BY 4.0; the data are not
shipped, and a reader will follow): of 9,698 profiles with an order from the
surveys, 42.9 % get the same order (44.8 % counting the 2021 Arenosols
against the Tenosols and Rudosols of earlier editions). 988 are decided,
with no earlier order left open, and 78.6 % of them agree (80.1 %; 82.9 %
of the 404 decided with the surveyors' highest confidence). Most profiles
stay provisional for missing data: the drainage class (Hydrosols), the
designations (Podosols), the cracks (Vertosols), free iron (Ferrosols),
carbonate records (Calcarosols) and exchangeable sodium (Sodosols).

# soilKey 0.9.276 (2026-10-21)

## Canadian classification: the colour of the Chernozemic great groups (experimental)

The key's chroma limits for the Chernozemic great groups say "usually".
`classify_cssc3()` now reads them at two more places, each written down in
`inst/rules/cssc3/readings.yaml`:

* a dry value of exactly 3.5, the limit of both the Black Chernozems
  ("darker than 3.5", Ch 5, p. 68) and the Dark Brown (3.5 to 4.5), is
  Black when the dry chroma is 1.5 or less, the chroma of the Black, unless
  an Ahe, Aej or Ae marks the eluviation of the Dark Gray, whose range
  holds 3.5;
* in a cultivated soil, an Ap whose own dry chroma is 1.5 or less counts as
  the sign of eluviation of the Dark Gray Chernozems, whose Ahe the plough
  has mixed in (the key gives that Ap its own value limit, and Ch 5, p. 69,
  admits a mixed Ap); an Ahe, Aej or Ae in the profile still counts, and a
  buried one no longer does.

The colour means are rounded to the millionth before they meet a limit. On
the NPDB scenario of 0.9.275, of the 348 Chernozems decided at the order
level, 116 (was 92) get the survey's great group, 84 of them (was 63) with
the great group decided.
WRB, SiBCS and USDA results do not change.

# soilKey 0.9.275 (2026-10-21)

## An NPDB scenario with the dry colour and the soil climate estimated (experimental)

The National Pedon Database of Canada records neither the dry colour of most
A horizons nor the soil climate, which the Canadian key reads for the
Chernozems (and the Gray Brown Luvisols and some subgroups). An optional
scenario now estimates them, each as a range, and `benchmark_npdb()`
classifies a pedon at the ends of its ranges, counting a class as decided
only where they all agree. `classify_cssc3()` itself does not change, and
WRB, SiBCS and USDA results do not change.

* `fetch_aafc_normals()` reads the climate normals of Agriculture and
  Agri-Food Canada (Derived Normal Climate Data, 5 km grid, Open Government
  Licence - Canada) at given points from the AAFC map service, without
  downloading files.
* `npdb_soil_climate()` turns them into the soil climate of the book's
  Tables 3 and 4: the mean annual soil temperature as the range from the
  air temperature to 4 degrees above it, and the climatic moisture index
  (growing-season precipitation over potential evapotranspiration, months
  of April to October above 5 degrees) with its moisture subclass. The
  potential evapotranspiration is computed by Thornthwaite, which matches
  the zones the book gives the Chernozem great groups on the NPDB, and by
  Hargreaves, the other end of the range.
* `npdb_dry_colour_model()` fits the dry value and chroma of A horizons on
  their moist colour where the NPDB records both (dry value = 1.70 + 0.92
  x moist value; error about one value unit), and `npdb_scenario()` adds
  the estimates to the pedons, never replacing recorded values.

On the NPDB with the 1961-1990 normals, 295 of the 899 Chernozems that
waited for the climate are decided, 292 of them in their own order:
agreement on the Chernozems goes from 0 to 24.1 % at the order level. The
error of the dry colour seldom decides the limits between the Black, Dark
Brown and Brown Chernozems (7.4 % at the great group level). The Gray
Brown Luvisols go from 0 to 228 of 265 in agreement (53 decided). On the
other orders, agreement goes from 90.6 to 90.1 % at the order level (56
soils of other orders are now decided as Chernozems) and from 71.9 to
74.6 % at the great group level.

# soilKey 0.9.274 (2026-10-21)

## Canadian classification: three readings raised by the NPDB benchmark (experimental)

`classify_cssc3()` changes three readings, each written down in
`inst/rules/cssc3/readings.yaml` (now 58 readings). WRB, SiBCS and USDA
results do not change.

* **Chroma 1 or less.** Clause a of the gleying test (Ch 3 key E, p. 34)
  counts only in materials that develop higher chromas when oxidised, which
  a colour cannot show: a pale calcareous C or a grey parent material is
  grey of its own. A horizon gleyed by clause a alone is now read with its
  designation: gleyed with a strong g, not gleyed when designated without
  one (a weak gj included; with mottles unknown, which could pass another
  clause, the designation decides as for unknown colours), and, with no
  designation, gleyed unless it holds 2 % CaCO3 or more (then undecided).
  The other clauses still read the colours alone. The Btg of the Luvic
  Gleysols reads its colours the same way.
* **Gleying and the Bt.** In a soil with a Bt that begins within 50 cm, the
  gley colours must be in the Bt (Ch 7, p. 83: a Luvic Gleysol when they are
  in the Btg within 50 cm, a Gleyed Luvisol when they are only in the Aeg or
  below 50 cm). Soils with a solonetzic B are left out, since Ch 7 places
  them in the Gleysolic order by their gleying. The 10 cm minimum of Ch 7
  (p. 81), which is not in the key, stays unapplied: on the NPDB it turned
  away 64 Gleysols the survey confirms, mostly because the depth recorded
  for the deepest horizon is the sampling depth.
* **Base saturation (neutral salt).** Criterion 6 of the chernozemic A is
  read as by an unbuffered salt, as the range each horizon's data allow:
  the bases over the effective CEC are the value; the bases over the bases
  plus the exchangeable Al are an upper bound (the H is left out); a base
  saturation at pH 7 is a lower bound (the unbuffered value is higher); a pH
  in CaCl2 of 5.5 or more (without it, 6.0 in water) places it above 80 %.
  Only a value known to be 80 % or less fails; the rest is undecided.
  Negative amounts, pH values outside 0-14 and contradictory bounds count as
  no data, and the KCl Al stands in for the exchangeable Al.

On the NPDB (Chernozems apart), agreement with the survey goes from 89.8 %
to 90.6 % at the order level (90.5 % to 91.5 % when decided), from 70.7 %
to 71.8 % at the great group level (82.1 % to 83.9 %) and from 56.3 % to
56.6 % at the subgroup level (71.3 % to 72.3 %). Decided disagreements in
which the key puts a surveyed soil in the Gleysolic order fall from 317 to
178 (Luvisols 141 to 72, Brunisols 51 to 29). In turn, 32 surveyed
Gleysols are now decided in the Luvisolic order, 30 of them by the Bt rule,
mostly Luvic Gleysols whose Btg or Btgj does not have gley colours. Base saturation no longer decides any
Chernozem outside its order (11 still fail criterion 6 because Ca is not
the dominant cation): 340 Chernozems (was 445) are decided outside the
order on other criteria, and 899 (was 794) wait for the soil climate.

# soilKey 0.9.273 (2026-10-21)

## The National Pedon Database of Canada as a benchmark (experimental)

`load_npdb_pedons()` reads the pre-packaged CSV files of the National Pedon
Database of Canada (NPDB, version 3, Agriculture and Agri-Food Canada,
CanSIS; Open Government Licence - Canada; not shipped with soilKey) into
pedon records, with the survey's classification under the Canadian System
of Soil Classification as the reference, and `benchmark_npdb()` compares
`classify_cssc3()` with it at the order, great group and subgroup levels.

The reader cleans what the files hold:

* it rebuilds the designations in the Canadian notation;
* it turns organic horizons recorded upward into depths above the mineral
  surface and repairs mineral horizons with reversed depths;
* it reads the coded matrix colours, telling the dry colour from the moist
  one pedon by pedon;
* it drops the no-data code and the copied values of total N and tells
  its units apart;
* it computes base saturation from the bases and the CEC where the recorded
  column says something else, and swaps back a dataset's pH columns;
* it turns the survey's words into the 1998 names where they name a class
  of the key (the Solodic subgroups as the Solonetzic ones they became).

The NPDB records no soil climate, which the key reads for the Chernozemic
order and the Gray Brown Luvisols: the 1,239 Chernozems are reported apart.
794 of them are provisional; the other 445 are decided outside the order,
mostly because a measured property of the A fails the chernozemic A, the
most frequent being base saturation (neutral salt) of 80 % or less. On the
other pedons with a reference:

| Level | Pedons | Agreement | Decided | Agreement when decided |
|---|---|---|---|---|
| Order | 7,298 | 89.8% | 6,524 | 90.5% |
| Great group | 7,272 | 70.7% | 5,327 | 82.1% |
| Subgroup | 4,041 | 56.3% | 2,587 | 71.3% |

Most disagreements come with a provisional result, where the records lack
what the key reads: the mean annual soil temperature of the Gray Brown
Luvisols (all 265 are undecided), the pH in CaCl2 of the Brunisols' B, the
organic C of the Humic Gleysols. Among the decided ones, the largest group
is the gleying of the Gleysolic order: Luvisols and Brunisols with a g
horizon within 50 cm, which the key places in the Gleysolic order and the
survey did not.

# soilKey 0.9.272 (2026-10-21)

## The Canadian orders and great groups: open bottoms, other notations, climate (experimental)

`classify_cssc3()` reads, at the order and great-group levels, what the
subgroups already read since 0.9.271:

* the deepest horizon with no bottom depth may go on: the Bt, the podzolic B,
  the chernozemic A, the Brunisolic B and the Btg of the Luvic Gleysols are
  undecided when their thickness could change the answer, so a profile such
  as LFH, Ae, Bt (20 cm, no bottom) is no longer a decided Orthic Regosol;
* the Bt is decided horizon by horizon, by its properties when they decide
  and otherwise by its designation: the clay increase is measured against
  the eluvial horizon above (Ch 2, p. 15), not against any lower-clay
  horizon; a horizon fails when its clay films are recorded as absent or
  when every clay needed shows no increase, and is undecided when the clay
  it should be compared with is missing; the 5 cm are then reached by
  adjacent Bt horizons added up, a Bt "within 50 cm" is a run that begins
  there, and a thin Btg on top of a Bt makes a Luvic Gleysol;
* the podzolic B is likewise decided layer by layer, so a Bf ruled out by
  its chemistry no longer counts by its designation;
* in the FAO and USDA notations a B with a t is a Bt and a Bs or Bh is
  podzolic B material by designation, a B with neither is not; an A, E, C or
  O is never podzolic B material nor a Bt; designations written in capitals
  ("BHS") are read; an EB or BE is the AB of a Solod; an E is the Ae of the Chernozemic and
  Solonetzic tests, so a Chernozem described as A, Bw, Bk, C reaches the
  Chernozemic order;
* the word "and" of a split horizon ("Ae and Bt") no longer switches the
  Canadian notation off;
* the soil moisture subclass is read whatever its spelling ("Sub-humid",
  "semi-arid").

In about 3,000 test profiles, 88 change order or great group: 82 described
in another notation with a Bt, now a provisional Luvisol or Luvic Gleysol
instead of a Regosol or Gleysol, and 6 whose designated Bt shows no clay
increase over the eluvial horizon above, now ruled out by that measured
clay. In Canadian-notation profiles the subgroups and great groups can also
change where clay films are recorded as absent, where clay was given on an
organic layer, or where the eluvial horizon has no clay recorded (the
designation then decides). WRB, SiBCS and USDA results do not
change.

# soilKey 0.9.271 (2026-10-21)

## The Canadian subgroups: Solonetzic, Chernozemic and Luvisolic (experimental)

`classify_cssc3()` now keys the subgroups of the last three orders of the
Canadian System of Soil Classification, 3rd edition (Key to Soil Subgroups,
Ch 3, pp. 42-49), 101 more, so all 231 subgroups are keyed and the name goes
down to the subgroup in every order ("Dark Gray Solod", "Calcareous Black
Chernozem", "Gleyed Brunisolic Gray Luvisol"). The subgroups read, as
before, measured properties first and the Canadian designations second:

* Solonetzic: the dry colour value of the A (Brown, Dark Brown, Black, and
  Gray and Dark Gray with a low chroma and a subhumid climate in the Solods
  and Solodized Solonetz), a strongly alkaline A (pH above 8.5), faint to
  distinct mottles within 50 cm;
* Chernozemic: a slickenside horizon, a weak solonetzic B (Bnj, Bnjtj,
  Btnj), eluvial horizons of 2 cm, a Bmk of 5 cm (0.5 % CaCO3 or more, or a
  k), no B of 5 cm, faint to distinct mottles within 50 cm;
* Luvisolic: a slickenside horizon, a fragipan, a podzolic B over a Bt within
  50 cm, a Bm of chroma 3 or a thin Bf in the upper solum, an Ah of 5 cm, a
  Btnj, distinct mottles within 50 cm or prominent ones at 50-100 cm.

The Solonetzic great groups have no residual subgroup in the key, so a
Solonetzic soil without an A stops at the great group; where an LFH lies
directly over an Ae, the Gray subgroups read the Ae, as Ch 12 words them.
The colour of the A is read over the whole A: a horizon with no value leaves
the class undecided when it could change it. The readings where the book is
silent are added to `inst/rules/cssc3/readings.yaml` (now 57). The Solonetzic
example of `make_cssc3_example()` gains a dry colour and a pH, so it keys to
a Dark Brown Solonetz.

Two changes reach the subgroups keyed since 0.9.269: a horizon with no bottom
depth (the deepest one, which may go on) no longer counts as 0 cm when
horizons are added up towards a thickness, so the Rego and Eluviated
subgroups are undecided instead of decided on it; and a buried Aeb is no
longer the Ae of the Eluviated Brunisols. Orders and great groups do not
change, and WRB, SiBCS and USDA results do not change.

# soilKey 0.9.270 (2026-10-21)

## The Canadian subgroups: Podzolic, Gleysolic, Brunisolic and Regosolic (experimental)

`classify_cssc3()` now keys the subgroups of four more orders of the
Canadian System of Soil Classification, 3rd edition (Key to Soil Subgroups,
Ch 3, pp. 40-51), 67 more, 130 of the 231 in all, so the name goes down to
the subgroup in seven orders ("Ortstein Humo-Ferric Podzol", "Fera Humic
Gleysol", "Gleyed Eluviated Dystric Brunisol", "Cumulic Regosol"); the
Solonetzic, Chernozemic and Luvisolic orders still stop at the great group.
The subgroups read, as before, measured properties first and the Canadian
designations second:

* Podzolic: an ortstein (no depth limit), placic and duric horizons and a
  fragipan within the control section (cementation class, or a c in the
  designation), a deep Bt, an Ah of 10 cm, distinct to prominent mottles
  within 1 m;
* Gleysolic: a slickenside horizon, a solonetzic B, a fragipan below the
  Btg, the Humic Gleysol A, a Bgf horizon (dithionite Fe over the parent
  material, or gf), no B of 10 cm;
* Brunisolic: a duric horizon, an Ae or Aej of 2 cm, gleying mottles within
  50 cm or at 50-100 cm;
* Regosolic: cumulic layering (colour value, organic C with depth, or a
  buried A) and faint to distinct mottles within 50 cm.

The readings where the book is silent are added to
`inst/rules/cssc3/readings.yaml` (now 54). The Vertic Solonetz great group
now reads its slickenside horizon horizon by horizon, as the Vertic Gleysol
subgroups do: a horizon with no designation and no slickenside record is
undecided instead of 'none', and a designation in the FAO or USDA notation
is read (an ss counts, a designation without ss fails). The surface A of
the great groups no longer takes in a buried A (b) under it, so an Ah over
an Ahb is measured by the Ah alone. WRB, SiBCS and USDA results do not
change.

# soilKey 0.9.269 (2026-10-21)

## The Canadian subgroups: Cryosolic, Organic and Vertisolic (experimental)

`classify_cssc3()` now keys the subgroups of three orders of the Canadian
System of Soil Classification, 3rd edition (Key to Soil Subgroups, Ch 3,
pp. 37-40), 63 of the 231, so the name goes down to the subgroup there
("Terric Humic Mesisol", "Brunisolic Dystric Turbic Cryosol", "Gleyed
Vertisol"); the other orders still stop at the great group. The subgroups
read, as the orders and great groups do, measured properties first and the
Canadian designations second:

* Cryosolic: ice layers, the mineral contact and the peat above it, the
  dominant peat below 40 cm, gleying, organic horizons over 15 cm, the pH of
  the B horizons, the thickness of the Bm, a Bt under an eluvial horizon;
* Organic: O horizons under folic ones, woody material, the dominant folic
  horizon, hydric, terric, limnic and cumulic layers, and the thickness of
  each kind of peat;
* Vertisolic: prominent, or faint to distinct, mottles within 50 cm.

The readings where the book is silent are added to
`inst/rules/cssc3/readings.yaml` (now 46). The Cryosolic example pedon gains
pH values. WRB, SiBCS and USDA results do not change.

Fixes to the orders and great groups of 0.9.267 and 0.9.268:

* the Organic great groups leave limnic and folic layers out of the peat
  tally (they are neither fibric, mesic nor humic); a limnic layer is not a
  terric contact, and a middle tier of limnic material only is read with the
  surface tier;
* gleying (Gleysolic order) is decided horizon by horizon: a horizon's
  measured colours outrank its own g, and a horizon with no colours and no
  designation is undecided instead of 'not gleyed';
* the rows are read in depth order whatever the order they are given in
  (on a copy; the layer numbers of the trace follow that order);
* the thick-A rule of the gleying test reads the whole surface A (adjacent Ah
  and Ap from the mineral surface; a buried A, b, never counts) and tests
  the mineral horizon right below it, whatever it is, a gap in the record
  allowed; in another notation the A masters are left out of the test and
  the E masters as the Ae are (a thick, deep E is tested);
* a horizon with no bottom depth ends where the next one down begins, and
  the deepest one may go on (both options of the Organic order, the organic
  column and the peat tally read it as possibly deep);
* limnic layers are read from whole words of their recorded origin, not on
  rock, and from the L master of the FAO and USDA notations;
* in designations of another notation, W and R are never mineral horizons,
  and an O or H without organic C is organic.

# soilKey 0.9.268 (2026-10-21)

## The Canadian great groups (experimental)

`classify_cssc3()` now keys the 31 great groups of the Canadian System of Soil
Classification, 3rd edition (Key to Soil Great Groups, Ch 3, pp. 35-36), under
the ten orders of 0.9.267, so the name is the great group ("Humo-Ferric
Podzol", "Dark Brown Chernozem") and the order is in `rsg_or_order`. The great
groups read, as the orders do, measured properties first and the Canadian
designations second:

* Cryosolic: organic material over permafrost, cryoturbation (y);
* Organic: the folic thickness of Ch 9 (L, F, H), the dominant material of
  the middle tier (fibric, mesic or humic, by rubbed fibre, then designation),
  widened to the surface tier only by a terric, lithic or hydric layer;
* Vertisolic: surface dry value or Ah thickness, by drainage class, and the
  Ch 13 rule for the Ap of a poorly drained cultivated soil;
* Podzolic: at least 10 cm of Bh or Bhf chemistry;
* Gleysolic: a gleyed Bt (Btg), a humic Ah or Ap with at least 2 % organic C;
* Solonetzic: a slickenside horizon, an Ae of at least 2 cm, an AB or BA;
* Chernozemic: the dry value and chroma of the chernozemic A and signs of
  eluviation;
* Luvisolic: a forest mull Ah and a mean annual soil temperature of 8 C;
* Brunisolic: a thick dark Ah or Ap and the pH (CaCl2) of the upper B;
* Regosolic: an Ah or Ap of 10 cm.

The last great group of each order is the key's "other soils". The readings
where the book is silent are added to `inst/rules/cssc3/readings.yaml` (now
33). The per-layer readings of podzolic B chemistry and of gley colours are
factored out of the order diagnostics without changing them. The example
pedons now reach a great group each. WRB, SiBCS and USDA results do not
change.

Fixes to the 0.9.267 order key, all in the Organic order:

* a folic layer (L, F, H) lying on peat is read as litter over peat, so
  F 0-5 cm over Om 5-160 cm is an Organic soil (it was keyed as Regosolic);
  the litter is not counted towards the peat limits;
* the kind of the surface organic layer is read from measured rubbed fibre
  before its designation (an Om with 60 % rubbed fibre is fibric);
* a layer of unknown organic status leaves the order undecided where it
  could change the answer, instead of failing it;
* in designations of another notation, the masters A, B, C and E are read as
  mineral, R as rock and W as water when organic C is missing.

# soilKey 0.9.267 (2026-10-20)

## The Canadian System of Soil Classification, to the order (experimental)

The first national system on the registry: `classify_cssc3()` keys a profile
through the Key to Soil Orders of the Canadian System of Soil Classification,
3rd edition (Soil Classification Working Group 1998, Ch 3, pp. 33-34), from
Cryosolic to Regosolic. Great groups and subgroups follow in later releases,
so the name is the order. The system is experimental and opt-in:
`classify_all()` and the Pro app keep running WRB, SiBCS and USDA by default;
`classify_all(systems = c("wrb2022", "sibcs5", "usda", "cssc3"))` runs it
beside them, and `classify_cssc3()` alone.

The Canadian keys are written largely in terms of horizon designations, which
Chapter 2 defines partly by measurable properties. Each diagnostic reads the
measured properties first (podzolic B chemistry, the Bt clay increase with
clay films, the solonetzic B Ca/Na ratio, the chernozemic A, the gley
colours) and the designations second, in the Canadian notation, where f is an
illuvial Fe-Al-organic accumulation, m a slightly altered B, v a vertic
horizon, ss slickensides, y cryoturbation, z permafrost and j a weak
expression of the suffix before it. Measured properties decide whenever they
can, pass or fail; designations decide only what they leave open, and only
when the profile's notation is Canadian (declared in
`site$designation_system`, a Canadian site, or no sign of the FAO/USDA
notation), because the same letters mean other things there. The trace
records which route decided, and a warning says when the order rests on
designations alone. The
chernozemic A includes a soil-climate criterion (mean annual soil temperature
at least 0 C, moisture subclass drier than humid), read from the new site
fields `mean_annual_soil_temp_C` and `soil_moisture_subclass` or approximated
from the USDA regimes; without it, the orders keyed after Chernozemic are
reported as provisional. The choices where the book is silent are listed in
`inst/rules/cssc3/readings.yaml`.

Three horizon attributes are added for the Canadian key and read by no other:
`fe_pyro_pct` and `al_pyro_pct` (sodium-pyrophosphate-extractable Fe and Al)
and `mottle_contrast` (faint, distinct, prominent). `make_cssc3_example()`
builds one example profile per order. WRB, SiBCS and USDA results do not
change: the 44 canonical fixtures are identical.

# soilKey 0.9.266 (2026-10-20)

## One file of metadata for the horizon attributes

The section, unit and description of each of the 112 canonical horizon
columns now live in `inst/schemas/horizon-attributes.yaml`, and the new
`horizon_attribute_spec()` returns them as a table with each column's type
and accepted aliases. The file also has room for what the national systems
will need: the keys that read an attribute, the values of a categorical one
and the range of a numeric one. The attribute reference `inst/ATTRIBUTES.md`
is generated from it (before, the generator parsed the comments of
`R/utils.R` and kept its own table of units), and a test fails when the file
and the reference fall out of step. The reference's tables are unchanged.
No classification changes.

# soilKey 0.9.265 (2026-10-20)

## A generic classifier for ordered keys

Most of the national systems on the roadmap (Canada, Australia, New Zealand,
Japan, Hungary, China) are ordered keys of the USDA kind: at each level the
first class whose tests pass is assigned. Their classifiers will be thin
wrappers around a new internal engine, `classify_hierarchical()`, that walks
the levels named in the system's registry entry. Two rules differ from the
three older keys, on purpose: a class is assigned only when its tests pass (a
`default: true` entry passes by definition), so a level where nothing passes
stays unassigned instead of falling back to the last class of its list (with
a warning saying where the name stopped); and a class that could not be
decided for lack of data, tested before the one assigned, makes the
assignment provisional. Run on the USDA rules, it gives
the same name, order and evidence grade as `classify_usda()` for all 44
canonical fixtures.

`ClassificationResult` gains three fields: `system_id` (the registry id; WRB,
SiBCS, USDA and SmartSolos results now carry it), `levels` (the class assigned at each
level) and `provisional_because`. Its `print()` counts the classes actually
tested (for SiBCS and USDA it counted the phases of the nested trace) and
calls them RSGs only for WRB. No classification changes.

# soilKey 0.9.264 (2026-10-20)

## The rule engine reads nested blocks and `none_of`

National keys say things such as "other soils that have A, and B or C, and
not D". The rule engine now reads that in YAML: an entry of an `all_of`,
`any_of` or `none_of` list may itself be one of these blocks, and `none_of`
(no test may pass) joins `all_of` and `any_of`, with the same three-valued
logic (an undecided test leaves the block undecided unless the outcome is
already settled). A nested block appears in the key trace as a test of its
own, with its sub-results as evidence.

The lower levels of a key are now read from the folders that the system's
registry entry names (`great_groups` from `great-groups/`), not from a fixed
list of six folder names. The rule validator also reports blocks that name
more than one combinator (the engine reads only one of them), combinators
written as a mapping instead of a list, class codes and names that are not
quoted strings (YAML reads an unquoted `N` as a logical and `11` as a number),
codes listed twice in one list or one level, and, for systems added after the
registry, predicates without the system's suffix, so that a Canadian
Chernozemic gate can never be WRB's `chernozem()`. The helpers that
every key shares (evidence grade, ambiguities, missing attributes) move from
`R/key-wrb2022.R` to `R/key-common.R`, unchanged.

No result changes: the 44 canonical fixtures give identical names, key traces,
evidence grades and warnings, and no rule of the three systems nests, mixes
combinators or uses `none_of`.

# soilKey 0.9.263 (2026-10-20)

## A registry of classification systems

Each classification system is now a folder `inst/rules/<id>/` holding its key
and a `system.yaml` entry: id, aliases, classifier, levels, display names,
edition, citation and status. `load_rules()`, `classify_all()` and the rule
validator read that registry instead of repeating the three ids, and the new
`soilkey_systems()` lists it. This is the first step towards the national
systems (Canada, Australia, New Zealand and others): a new system will be added
by adding its folder.

Nothing else changes. The 44 canonical fixtures give the same names, key
traces, evidence grades and warnings as in 0.9.262, and every id or alias
accepted so far still works, including the partial ones such as `"wrb"`.
`classify_all(systems = "sibcs5")` is now accepted as well as `"sibcs"`.
`load_rules()` now writes its default as `"wrb2022"`; it was the vector of the
three ids, of which the first was used.

# soilKey 0.9.262 (2026-10-15)

## The WRB 2022 reference points to the 2022 edition

The README and the Portuguese getting-started vignette gave, for WRB 2022
(IUSS Working Group WRB, 4th edition), an FAO OpenKnowledge PDF that is the
2014 edition: its own title reads "World reference base for soil resources
2014". Both now give the 2022 edition at ISRIC,
`https://files.isric.org/public/documents/WRB_fourth_edition_2022-12-18.pdf`.
The classification keys do not change.

# soilKey 0.9.261 (2026-10-14)

## Pro app: a visitor counter in the footer

The footer of the hosted app (soilkeypro.com) says, in one line, how many
people are on the site now, how many have visited and from how many countries;
a click on the countries opens a map of them above the footer. Asked for by the
maintainer.

* **A person is counted once.** On the first visit the browser keeps a mark in
  its own storage (no identifier) and tells the server it is a new visitor; a
  later visit from the same browser counts as a session only. Nothing that
  identifies the visitor is sent or kept.
* **The country** comes from the address of the request, looked up in DB-IP's
  *IP to Country Lite* table (CC BY 4.0), which the image downloads when it is
  built and the footer credits. The address is read for that and kept nowhere.
* **Not counted:** automated browsers (headless Chrome, crawlers, command-line
  clients, the capacity test, the Claude app's browser), and pages that never
  open a Shiny session.
* **The numbers** are kept in Firestore, in the app's own Google Cloud project:
  one document of totals (visitors, sessions, visitors by country, the day
  counting began) and one document per running instance with how many sessions
  it holds, written every 30 s. "Online now" adds up the instances that wrote in
  the last 90 s. Only soilkeypro.com writes to these; a tagged revision writes
  to separate test collections, and a local run to none.
* **Before the counter**, the request logs of Cloud Run (30 days) are read once
  by `deploy/cloudrun/visitors-backfill.R`: a session is a request to
  `/websocket/` answered with 101, a visitor a distinct address. The logs
  begin on 2026-10-07, when the site came back online.

* **The map waits for a click.** The footer is on screen in every tab, and a
  map in it (190 px in all) took the height the tabs' own tables need: with a
  790 px window the key trace of Classify had none left, and DT does not draw a
  table of height 0. The map now opens in a panel above the line's "from N
  countries" button (below it when there is no room above) and closes on a
  second click, a click elsewhere, Escape or, on a phone, a scroll of the page;
  the footer grows by one line only. On a phone the map keeps its 210 px (the
  app's narrow-screen rule sets every map's height to "auto").
* **Classify's key trace** is drawn at its own height, not the height left over
  in its card (`DTOutput(fill = FALSE)`). With a short window, or a footer one
  line taller, it was 36 px or less before.

New files: `R/utils_visitors.R` and `R/mod_visitors.R` in the app, its country
table `data/countries.csv` (built by `data-raw/visitor_countries.R`; see
`inst/DATA-PROVENANCE.md`), the image step in `deploy/fly/Dockerfile`, and eight
strings in each language. The classification keys do not change.

# soilKey 0.9.260 (2026-10-14)

## Pro app: the light base map is Esri's; CARTO's asks for a key

CARTO's base maps now answer a request without an API key with a tile that
says "API KEY REQUIRED". The app used CARTO's light map, "Positron", as the
base of the Batch and Grid maps and as the "Light" choice of the Map tab, so
those showed the notice in place of a map. The checks of each release counted
the tiles that loaded, not what they showed, and did not see it.

The light map is now Esri's light gray canvas (`Esri.WorldGrayCanvas`), with
its attribution, in the three places and in the list the app registers its
map scripts from. The Map tab's choice reads "Light (Esri)". Nothing else
changes.

# soilKey 0.9.259 (2026-10-14)

## Tests: the slow ones run off CRAN

The CRAN pre-test of 0.9.258 (2026-10-14) did not pass automatically on
r-devel-windows-x86_64: "Overall checktime 14 min > 10 min", mainly from the
tests, 461 s on Windows (158 s on Debian). The CRAN team asked to reduce the
test timings, among other ways by running less important tests only off CRAN.

Every test that took more than 0.03 s on CRAN's terms (`NOT_CRAN` unset, the
installed package) on a fast machine now starts with `skip_on_cran()`: 457
tests in 137 files. They run as before with `devtools::test()` and wherever
`NOT_CRAN=true`. The core of the three keys stays on CRAN, slow as it is:
the tests that the canonical profiles classify to their WRB Reference Soil
Group, to their SiBCS order, and to their USDA order.

| | before | now |
|---|---|---|
| tests run on CRAN | 1,821 | 1,354 |
| time of the tests on CRAN's terms, this machine | 87 s | 21 s |
| the same, the whole test process | 98 s | 28 s |

Windows ran the tests of 0.9.258 about five times slower than this machine;
by that ratio they take about 150 s there now.

## The links to Embrapa's AgroAPI are text

The check of this version found the three addresses of Embrapa's AgroAPI
(`www.agroapi.cnptia.embrapa.br`) out of reach: the server did not answer in
60 s, and the incoming check waited for it. The addresses stay, written as
text and no longer as links: in the NEWS entry of 0.9.237, in the README, and
in `inst/CITATION`, where the entry of the SmartSolos Expert article loses its
`url` and keeps its DOI. As for the data servers in 0.9.155.

No code changes; the classification of every profile is the same.
