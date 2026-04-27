# Extracao de metadados de sitio a partir de ficha de campo / Site metadata extraction from field sheet

## Instrucoes (PT-BR)

Voce esta examinando uma ficha de campo de descricao de perfil de solo (em
geral, formularios da Embrapa, FAO, USDA-NRCS, ou cabecalhos de capitulos de
levantamento). **Voce nao classifica o solo.** Voce apenas extrai os
metadados de sitio que estao escritos / impressos na ficha.

Extraia os campos definidos no schema JSON abaixo. Para cada valor:

- **value**: o valor reportado, com unidades canonicas:
  - lat / lon: graus decimais (converta de DMS se necessario).
  - elevation_m: metros acima do nivel do mar.
  - slope_pct: percentagem (NAO graus).
  - aspect_deg: graus a partir do norte verdadeiro, sentido horario.
  - date: ISO 8601 (YYYY-MM-DD).
- **confidence**: 0.0 a 1.0.
- **source_quote**: trecho curto da ficha sustentando a extracao.

**Regras:**

1. Coordenadas: se a ficha reporta apenas em UTM, converta para WGS84
   (lat/lon decimal) somente se a zona UTM estiver explicita; caso contrario,
   retorne `null` e anote em `source_quote`.
2. Country: codigo ISO-2 (BR, US, etc.) ou nome completo se o codigo nao for
   inferivel; **nao adivinhe se nao houver evidencia**.
3. Drainage class: use a terminologia FAO/WRB (excessively / well /
   moderately well / imperfectly / poorly / very poorly drained) traduzida
   para a do documento, mas mantida em ingles no campo.
4. Vegetation / land_use: preserve a descricao original em campo livre.

## Instructions (EN)

You are examining a soil profile field sheet (Embrapa, FAO, NRCS, or survey
report headers). **You do not classify the soil.** Extract only the site
metadata as stated.

Field semantics: lat/lon decimal degrees; elevation in meters; slope as a
percentage (not degrees); aspect in degrees clockwise from true north; date
ISO 8601.

If a field is missing, return `null`. Never infer.

---

## JSON schema (must validate)

```json
{schema_json}
```

---

## Field sheet

[Supplied as an image content block.]

---

Return **only** a JSON object validating against the schema above. No prose.
