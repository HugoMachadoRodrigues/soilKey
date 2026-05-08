# soilKey USDA Soil Taxonomy 13ed audit vs canonical (NCSS-tech)

**Date**: 2026-05-08  
**soilKey version**: 0.9.62  
**Canonical source**: NCSS-tech/SoilTaxonomy  (`ST_criteria_13th`, `ST_features`)
**Reference**: USDA-NRCS Soil Survey Staff (2022).  *Keys to Soil Taxonomy*, 13th edition.

## Coverage summary

| Element                       | Canonical | Implemented | Missing |
|-------------------------------|----------:|------------:|--------:|
| USDA Soil Orders              |        12 |          12 |       0 |
| Diagnostic features (canonical) |      84 |          13 |      71 |
| Distinct KST taxa             |       419 | n/a (~419 via YAML) | n/a |

## USDA Soil Orders

Implemented: 12 / 12

- `Gelisols` : implemented
- `Histosols` : implemented
- `Spodosols` : implemented
- `Andisols` : implemented
- `Oxisols` : implemented
- `Vertisols` : implemented
- `Aridisols` : implemented
- `Ultisols` : implemented
- `Mollisols` : implemented
- `Alfisols` : implemented
- `Inceptisols` : implemented
- `Entisols` : implemented

## Diagnostic features (84 canonical) by group


### Soil Materials (0 / 7 implemented)

- `Mineral Soil Material` : **not detected**
- `Organic Soil Material` : **not detected**
- `Distinction Between Mineral Soils and Organic` : **not detected**
- `Soil Surface` : **not detected**
- `Mineral Soil Surface` : **not detected**
- `Definition of Mineral Soils` : **not detected**
- `Definition of Organic Soils` : **not detected**

### Surface (3 / 8 implemented)

- `Anthropic Epipedon` : **not detected**
- `Folistic Epipedon` : OK
- `Histic Epipedon` : **not detected**
- `Melanic Epipedon` : OK
- `Mollic Epipedon` : OK
- `Ochric Epipedon` : **not detected**
- `Plaggen Epipedon` : **not detected**
- `Umbric Epipedon` : **not detected**

### Subsurface (4 / 20 implemented)

- `Agric Horizon` : **not detected**
- `Albic Horizon` : **not detected**
- `Anhydritic Horizon` : **not detected**
- `Argillic Horizon` : **not detected**
- `Calcic Horizon` : **not detected**
- `Cambic Horizon` : **not detected**
- `Duripan` : OK
- `Fragipan` : OK
- `Glossic Horizon` : **not detected**
- `Gypsic Horizon` : **not detected**
- `Kandic Horizon` : **not detected**
- `Natric Horizon` : OK
- `Ortstein` : OK
- `Oxic Horizon` : **not detected**
- `Petrocalcic Horizon` : **not detected**
- `Petrogypsic Horizon` : **not detected**
- `Placic Horizon` : **not detected**
- `Salic Horizon` : **not detected**
- `Sombric Horizon` : **not detected**
- `Spodic Horizon` : **not detected**

### Mineral (2 / 10 implemented)

- `Abrupt Textural Change` : **not detected**
- `Albic Materials` : **not detected**
- `Andic Soil Properties` : OK
- `Anhydrous Conditions` : **not detected**
- `Coefficient of Linear Extensibility (COLE)` : **not detected**
- `Fragic Soil Properties` : **not detected**
- `Free Carbonates` : **not detected**
- `Identifiable Secondary Carbonates` : **not detected**
- `Interfingering of Albic Materials` : **not detected**
- `Lamellae` : OK

### Organic (1 / 14 implemented)

- `Kinds of Organic Soil Materials` : **not detected**
- `Fibers` : **not detected**
- `Fibric Soil Materials` : **not detected**
- `Hemic Soil Materials` : **not detected**
- `Sapric Soil Materials` : **not detected**
- `Humilluvic Material` : **not detected**
- `Kinds of Limnic Materials` : **not detected**
- `Coprogenous Earth` : **not detected**
- `Diatomaceous Earth` : **not detected**
- `Marl` : OK
- `Thickness of Organic Soil Materials` : **not detected**
- `Surface Tier` : **not detected**
- `Subsurface Tier` : **not detected**
- `Bottom Tier` : **not detected**

### Mineral or Organic (3 / 15 implemented)

- `Aquic Conditions` : OK
- `Cryoturbation` : OK
- `Densic Contact` : **not detected**
- `Densic Materials` : **not detected**
- `Gelic Materials` : **not detected**
- `Glacic Layer` : **not detected**
- `Lithic Contact` : **not detected**
- `Paralithic Contact` : **not detected**
- `Paralithic Materials` : **not detected**
- `Permafrost` : OK
- `Soil Moisture Regimes` : **not detected**
- `Soil Moisture Control Section` : **not detected**
- `Classes of Soil Moisture Regimes` : **not detected**
- `Sulfidic Materials` : **not detected**
- `Sulfuric Horizon` : **not detected**

### Human (0 / 10 implemented)

- `Anthropogenic Landforms` : **not detected**
- `Constructional Anthropogenic Landforms` : **not detected**
- `Destructional Anthropogenic Landforms` : **not detected**
- `Anthropogenic Microfeatures` : **not detected**
- `Constructional Anthropogenic Microfeatures` : **not detected**
- `Destructional Anthropogenic Microfeatures` : **not detected**
- `Artifacts` : **not detected**
- `Human-Altered Material` : **not detected**
- `Human-Transported Material` : **not detected**
- `Manufactured Layer` : **not detected**

## Caveats

- Heuristic name-matching: a feature is 'implemented' if its
  name (lowercased + tokenised) appears anywhere in the USDA
  R sources or YAML rules. False positives possible (collision
  with other identifiers); false negatives if the feature was
  implemented under a different name.
- Subgroup coverage uses first-word matching as a proxy. The
  detailed Subgroup audit (matching all canonical subgroup
  names against the YAML rules) is a v0.9.63 task.
- USDA Order names are pluralised in canonical text and in
  soilKey output.
