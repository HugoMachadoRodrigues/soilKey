# soilKey USDA KST 13ed Subgroup audit (refined v0.9.63)

**Date**: 2026-05-08  
**soilKey version**: 0.9.63  
**Canonical source**: SoilKnowledgeBase 2022_KST_codes.json (vendored)

## Coverage at each KST level (refined matching)

| Level       | Canonical | Implemented | Missing |
|-------------|----------:|------------:|--------:|
| Order       |        12 |          12 |       0 |
| Suborder    |        68 |          68 |       0 |
| Great Group |       339 |         339 |       0 |
| Subgroup    |      2715 |        2369 |     346 |

## Subgroup coverage per Order

| Order | Subgroups (canonical) | Implemented | % |
|-------|---------------------:|------------:|--:|
| Gelisols (A) |                 129 |         129 | 100.0% |
| Histosols (B) |                  75 |          75 | 100.0% |
| Spodosols (C) |                 121 |         121 | 100.0% |
| Andisols (D) |                 218 |         216 | 99.1% |
| Oxisols (E) |                 213 |         192 | 90.1% |
| Vertisols (F) |                 158 |         118 | 74.7% |
| Aridisols (G) |                 270 |         205 | 75.9% |
| Ultisols (H) |                 215 |         192 | 89.3% |
| Mollisols (I) |                 376 |         304 | 80.9% |
| Alfisols (J) |                 352 |         302 | 85.8% |
| Inceptisols (K) |                 349 |         320 | 91.7% |
| Entisols (L) |                 239 |         195 | 81.6% |

## Top 30 missing Subgroups

- `Abruptic Argiaquolls`
- `Abruptic Argicryolls`
- `Abruptic Argiduridic Durixerolls`
- `Abruptic Argidurids`
- `Abruptic Argiudolls`
- `Abruptic Durixeralfs`
- `Abruptic Haplic Durixeralfs`
- `Abruptic Natrudolls`
- `Abruptic Palecryolls`
- `Abruptic Xeric Argidurids`
- `Aeric Chromic Vertic Epiaqualfs`
- `Andic Ombroaquic Kandihumults`
- `Anhydritic Aquisalids`
- `Anhydritic Haplosalids`
- `Anthraltic Sodic Xerorthents`
- `Anthraltic Torriorthents`
- `Anthraltic Xerorthents`
- `Anthraquic Eutrudepts`
- `Anthraquic Hapludalfs`
- `Anthraquic Hapludands`
- `Anthraquic Haplustepts`
- `Anthraquic Haplustolls`
- `Anthraquic Melanudands`
- `Anthraquic Paleudalfs`
- `Anthraquic Paleudults`
- `Anthraquic Ustifluvents`
- `Anthraquic Ustorthents`
- `Anthrodensic Dystrudepts`
- `Anthrodensic Haplustepts`
- `Anthrodensic Sodic Udorthents`

... and 316 more.

## Caveats

- Refined matcher: requires ALL space-separated tokens of a
  Subgroup name (e.g. "Typic" + "Hapludults") to appear in
  the YAML rules / R sources blob. Plural variants ("-s",
  "-es", "-ies") matched as alternates. Verbatim full-name
  match also accepted.
- This is much closer to truth than the v0.9.62 first-word
  heuristic but still has false positives (modifier words
  like "Typic" or "Aquic" are ubiquitous and may match
  unrelated rules) and false negatives (multi-word YAML
  names with non-canonical word order).
- Manual review of the missing-Subgroup list before opening
  v0.9.64 implementation tickets is recommended.
