# Extracao de metadados de sitio a partir de TEXTO / Site metadata extraction from TEXT

## Instrucoes (PT-BR)

Voce esta lendo o **texto** de uma ficha de campo de descricao de perfil de
solo (transcricao de formularios da Embrapa, FAO, USDA-NRCS, ou cabecalhos
de capitulos de levantamento). **Voce nao classifica o solo.** Voce apenas
extrai os metadados de sitio que estao escritos no texto.

Extraia os campos definidos no schema JSON abaixo. Para cada valor:

- **value**: o valor reportado, com unidades canonicas:
  - lat / lon: graus decimais (converta de DMS se necessario; sinal
    negativo para hemisferio sul / oeste).
  - elevation_m: metros acima do nivel do mar.
  - slope_pct: percentagem (NAO graus).
  - aspect_deg: graus a partir do norte verdadeiro, sentido horario.
  - date: ISO 8601 (YYYY-MM-DD).
- **confidence**: 0.0 a 1.0. Quando o valor esta literal no texto e sem
  ambiguidade, use 0.95+. Quando inferido (e.g. country = "BR" porque a
  cidade citada esta no Brasil), use 0.7-0.85.
- **source_quote**: trecho curto do texto sustentando a extracao
  (ate 200 caracteres).

**Regras:**

1. Se um campo aparece literalmente no texto (e.g. "Coord. -22.8654,
   -43.7811"), extraia-o com confidence alta. **Nao retorne null para um
   campo que esta visivel.**
2. country: extraia o codigo ISO-2 quando inferivel a partir do estado
   (RJ, MG, SP -> "BR"; CA, TX, NY -> "US"; etc.). Use confidence ~0.85
   para inferencias.
3. state: use a sigla canonica do pais (RJ, SP, MG para Brasil; CA, TX,
   NY para EUA, etc.).
4. drainage_class: use exatamente a terminologia do texto traduzida
   livremente -- "bem drenado", "imperfectly drained", etc.
5. vegetation / land_use / parent_material: preserve a descricao
   original em campo livre.
6. Se um campo de fato nao aparece no texto, retorne `null` para ele.

## Instructions (EN)

You are reading the **text** of a soil profile field sheet
(transcription of Embrapa, FAO, NRCS or survey-report headers). **You do
not classify the soil.** Extract only the site metadata as stated.

If a value is literally in the text, extract it with high confidence
(0.95+). If a value is inferred (e.g. country = "BR" from a Brazilian
state), use confidence 0.7-0.85.

Field semantics: lat/lon decimal degrees; elevation in metres; slope as
a percentage (not degrees); aspect in degrees clockwise from true
north; date ISO 8601.

**Do not return `null` for a field that is literally in the text.**

---

## JSON schema (must validate)

```json
{schema_json}
```

---

## Field-sheet text

```
{document_text}
```

---

Return **only** a JSON object validating against the schema above. No
markdown fences, no prose.
