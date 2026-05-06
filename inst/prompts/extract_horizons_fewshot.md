# Extracao de horizontes -- few-shot / Few-shot horizon extraction

## Instrucoes (PT-BR)

Voce e um pedologo experiente extraindo dados estruturados de uma descricao
de perfil. **Voce nao classifica o solo.** Voce apenas extrai os atributos
observados, exatamente como estao no documento.

A saida segue o schema soilKey, que **mistura** dois formatos por campo:

- **Campos brutos** (sem wrapper): `top_cm`, `bottom_cm`, `designation`,
  `boundary_distinctness`, `boundary_topography`. Sao numero / string /
  null direto.
- **Campos com wrapper**: numericos (clay_pct, silt_pct, sand_pct,
  ph_h2o, oc_pct, cec_cmol, etc.) e categoricos (structure_grade,
  consistence_moist, etc.) vem como objeto
  `{"value": ..., "confidence": <0..1>, "source_quote": "<<= 200 chars>"}`.
- **Munsell**: `munsell_moist` e `munsell_dry` sao um **unico** objeto
  com `{"hue", "value", "chroma", "confidence", "source_quote"}`.
  **Nao** separe em tres campos diferentes.

Profundidades em centimetros sempre. Se um campo nao aparece, omita-o
ou retorne `null`. **Nao invente valores.**

---

## Exemplos resolvidos / Worked examples

Estude estes 2 exemplos e siga **exatamente** a mesma forma de saida.

### Exemplo 1 -- entrada

```
# Perfil PV-099 (Cambissolo)
## A (0 a 10 cm)
Cor Munsell umida: 10YR 4/3. Argila 18 %, areia 60 %.
pH em agua: 5.5.

## Bw (10 a 60 cm)
Cor Munsell umida: 10YR 5/4. Argila 25 %, areia 50 %.
pH em agua: 5.0.
```

### Exemplo 1 -- saida JSON

```json
{
  "horizons": [
    {
      "designation": "A",
      "top_cm": 0,
      "bottom_cm": 10,
      "munsell_moist": {
        "hue": "10YR", "value": 4, "chroma": 3,
        "confidence": 0.99,
        "source_quote": "Cor Munsell umida: 10YR 4/3"
      },
      "clay_pct": {
        "value": 18, "confidence": 0.99,
        "source_quote": "Argila 18 %"
      },
      "sand_pct": {
        "value": 60, "confidence": 0.99,
        "source_quote": "areia 60 %"
      },
      "ph_h2o": {
        "value": 5.5, "confidence": 0.99,
        "source_quote": "pH em agua: 5.5"
      }
    },
    {
      "designation": "Bw",
      "top_cm": 10,
      "bottom_cm": 60,
      "munsell_moist": {
        "hue": "10YR", "value": 5, "chroma": 4,
        "confidence": 0.99,
        "source_quote": "Cor Munsell umida: 10YR 5/4"
      },
      "clay_pct": {
        "value": 25, "confidence": 0.99,
        "source_quote": "Argila 25 %"
      },
      "sand_pct": {
        "value": 50, "confidence": 0.99,
        "source_quote": "areia 50 %"
      },
      "ph_h2o": {
        "value": 5.0, "confidence": 0.99,
        "source_quote": "pH em agua: 5.0"
      }
    }
  ]
}
```

### Exemplo 2 -- entrada

```
## Horizonte Bt1 (35 a 80 cm)
Cor 5YR 4/6 umida. Argila 38 %, silte 25 %, areia 37 %.
pH em agua 4.9. Carbono organico 0.45 %.
```

### Exemplo 2 -- saida JSON

```json
{
  "horizons": [
    {
      "designation": "Bt1",
      "top_cm": 35,
      "bottom_cm": 80,
      "munsell_moist": {
        "hue": "5YR", "value": 4, "chroma": 6,
        "confidence": 0.99,
        "source_quote": "Cor 5YR 4/6 umida"
      },
      "clay_pct": {
        "value": 38, "confidence": 0.99,
        "source_quote": "Argila 38 %"
      },
      "silt_pct": {
        "value": 25, "confidence": 0.99,
        "source_quote": "silte 25 %"
      },
      "sand_pct": {
        "value": 37, "confidence": 0.99,
        "source_quote": "areia 37 %"
      },
      "ph_h2o": {
        "value": 4.9, "confidence": 0.99,
        "source_quote": "pH em agua 4.9"
      },
      "oc_pct": {
        "value": 0.45, "confidence": 0.99,
        "source_quote": "Carbono organico 0.45 %"
      }
    }
  ]
}
```

## Instructions (EN)

Same contract: `top_cm`, `bottom_cm`, `designation`, `boundary_*` are
raw values; `munsell_moist` / `munsell_dry` are a *single* wrapped
object holding hue / value / chroma; everything else is wrapped
`{value, confidence, source_quote}`.

---

## JSON schema (must validate)

```json
{schema_json}
```

---

## Source document (extract horizons from this)

```
{document_text}
```

---

Return **only** a JSON object validating against the schema above. No
prose, no markdown fences in the response. Match the shape of the
worked examples exactly.
