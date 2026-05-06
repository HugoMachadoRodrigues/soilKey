# Extracao de metadados de sitio (TEXTO) -- few-shot / Few-shot site extraction

## Instrucoes (PT-BR)

Voce esta lendo o **texto** de uma ficha de campo. **Voce nao
classifica o solo.** Apenas extrai os metadados de sitio.

A saida segue o schema soilKey, que **mistura** dois formatos por campo:

- **Campos brutos** (sem wrapper): `id` (string), `crs` (integer).
  Sao tipo direto / null.
- **Demais campos** (lat, lon, date, country, elevation_m, slope_pct,
  aspect_deg, landform, parent_material, land_use, vegetation,
  drainage_class): vem como objeto
  `{"value": ..., "confidence": <0..1>, "source_quote": "<<= 200 chars>"}`.

Unidades canonicas:
- lat / lon: graus decimais (sinal negativo p/ S e O).
- elevation_m: metros.
- slope_pct: percentagem (NAO graus).
- aspect_deg: graus do norte verdadeiro, sentido horario.
- date: ISO 8601 (YYYY-MM-DD).

Confidence:
- 0.95+ quando literal no texto.
- 0.7-0.85 quando inferido razoavelmente (e.g. country = "BR" porque
  a cidade citada esta no Brasil).
- <=0.5 ou `null` para o campo se a evidencia e fraca.

**Para campos visiveis no texto, NUNCA retorne `null`.** Esse e o
erro mais comum -- o modelo gera a estrutura do schema mas omite os
valores. Cada exemplo abaixo mostra o que e esperado.

---

## Exemplos resolvidos / Worked examples

### Exemplo 1 -- entrada

```
PERFIL: P-100
Data: 2023-05-15
Local: Piracicaba, SP. Coord. -22.7253, -47.6492 (WGS84).
Altitude 540 m. Declividade 6 %.
Drenagem: bem drenado.
Material: arenito.
Vegetacao: Cerrado.
Uso: cana-de-acucar.
```

### Exemplo 1 -- saida JSON

```json
{
  "site": {
    "id": "P-100",
    "date":            {"value": "2023-05-15", "confidence": 0.99,
                         "source_quote": "Data: 2023-05-15"},
    "country":         {"value": "BR", "confidence": 0.85,
                         "source_quote": "Piracicaba, SP"},
    "lat":             {"value": -22.7253, "confidence": 0.99,
                         "source_quote": "Coord. -22.7253, -47.6492"},
    "lon":             {"value": -47.6492, "confidence": 0.99,
                         "source_quote": "Coord. -22.7253, -47.6492"},
    "elevation_m":     {"value": 540, "confidence": 0.99,
                         "source_quote": "Altitude 540 m"},
    "slope_pct":       {"value": 6, "confidence": 0.99,
                         "source_quote": "Declividade 6 %"},
    "drainage_class":  {"value": "bem drenado", "confidence": 0.99,
                         "source_quote": "Drenagem: bem drenado"},
    "parent_material": {"value": "arenito", "confidence": 0.99,
                         "source_quote": "Material: arenito"},
    "vegetation":      {"value": "Cerrado", "confidence": 0.99,
                         "source_quote": "Vegetacao: Cerrado"},
    "land_use":        {"value": "cana-de-acucar", "confidence": 0.99,
                         "source_quote": "Uso: cana-de-acucar"}
  }
}
```

### Exemplo 2 -- entrada

```
Field profile FP-22
Date: 2022-03-10
Location: Davis, CA. -38.5449, -121.7405 (NAD83).
Elev 18 m. Slope 1 %. Aspect 270 deg.
Drainage: well drained.
Parent material: alluvium.
Land use: irrigated row crops.
```

### Exemplo 2 -- saida JSON

```json
{
  "site": {
    "id": "FP-22",
    "date":            {"value": "2022-03-10", "confidence": 0.99,
                         "source_quote": "Date: 2022-03-10"},
    "country":         {"value": "US", "confidence": 0.85,
                         "source_quote": "Davis, CA"},
    "lat":             {"value": -38.5449, "confidence": 0.95,
                         "source_quote": "-38.5449, -121.7405"},
    "lon":             {"value": -121.7405, "confidence": 0.95,
                         "source_quote": "-38.5449, -121.7405"},
    "elevation_m":     {"value": 18, "confidence": 0.99,
                         "source_quote": "Elev 18 m"},
    "slope_pct":       {"value": 1, "confidence": 0.99,
                         "source_quote": "Slope 1 %"},
    "aspect_deg":      {"value": 270, "confidence": 0.99,
                         "source_quote": "Aspect 270 deg"},
    "drainage_class":  {"value": "well drained", "confidence": 0.99,
                         "source_quote": "Drainage: well drained"},
    "parent_material": {"value": "alluvium", "confidence": 0.99,
                         "source_quote": "Parent material: alluvium"},
    "land_use":        {"value": "irrigated row crops", "confidence": 0.99,
                         "source_quote": "Land use: irrigated row crops"}
  }
}
```

## Instructions (EN)

Same contract: `id` and `crs` are raw values; everything else is
wrapped `{value, confidence, source_quote}`. **Never return `null`
for a field that is literally in the text.** Match the shape of the
worked examples exactly.

---

## JSON schema (must validate)

```json
{schema_json}
```

---

## Field-sheet text (extract metadata from this)

```
{document_text}
```

---

Return **only** a JSON object validating against the schema above. No
prose, no markdown fences. Match the shape of the worked examples.
