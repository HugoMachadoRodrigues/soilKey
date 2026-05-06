# Extracao Munsell de foto -- few-shot / Few-shot Munsell-from-photo

## Instrucoes (PT-BR)

Voce esta examinando uma foto de um perfil de solo. **Voce nao
classifica o solo.** Voce apenas estima a cor Munsell de cada
horizonte visivel quando houver um cartao Munsell ou padrao de cor
calibrado na imagem.

Cada horizonte deve ser retornado como um objeto com os campos:

- **designation**, **top_cm**, **bottom_cm**: valores brutos
  (string / number / null), sem wrapper.
- **munsell_moist**: um *unico* objeto wrapped com
  `{"hue", "value", "chroma", "confidence", "source_quote"}` --
  hue e string Munsell ("10YR", "5YR", "2.5YR", ...); value e chroma
  sao inteiros 1-8.

**Calibração da confianca:**

- Cartao Munsell visivel + iluminacao difusa: confianca ate 0.75.
- Sem cartao mas iluminacao consistente: 0.3-0.5.
- Sombras pesadas, foco ruim, ou sem cartao: 0.0-0.3 ou retorne
  `null` para o horizonte.

Em fotos nao ha texto literal -- o `source_quote` deve descrever a
regiao da imagem (ex.: "topo do perfil, ao lado do cartao Munsell em
luz solar difusa").

---

## Exemplo resolvido / Worked example

### Exemplo 1 -- contexto

Imagem hipotetica de um Latossolo Vermelho com cartao Munsell
visivel ao lado dos horizontes A (0-15 cm) e Bw (15-90 cm).

### Exemplo 1 -- saida JSON

```json
{
  "horizons": [
    {
      "designation": "A",
      "top_cm": 0,
      "bottom_cm": 15,
      "munsell_moist": {
        "hue": "5YR", "value": 3, "chroma": 4,
        "confidence": 0.7,
        "source_quote": "topo escuro 0-15 cm; comparado ao chip 5YR 3/4 do cartao"
      }
    },
    {
      "designation": "Bw",
      "top_cm": 15,
      "bottom_cm": 90,
      "munsell_moist": {
        "hue": "2.5YR", "value": 4, "chroma": 6,
        "confidence": 0.75,
        "source_quote": "horizonte vermelho homogeneo abaixo de A; comparado ao 2.5YR 4/6"
      }
    }
  ]
}
```

### Exemplo 2 -- contexto

Foto sem cartao Munsell, com sombra pesada na metade superior do
perfil. **Resposta esperada:** confidence baixa, `source_quote`
explicando.

### Exemplo 2 -- saida JSON

```json
{
  "horizons": [
    {
      "designation": "A",
      "top_cm": 0,
      "bottom_cm": 20,
      "munsell_moist": {
        "hue": "10YR", "value": 3, "chroma": 3,
        "confidence": 0.3,
        "source_quote": "no Munsell card visible; harsh shadow on upper face"
      }
    }
  ]
}
```

## Instructions (EN)

Same contract. `designation`, `top_cm`, `bottom_cm` are raw values;
`munsell_moist` is a single wrapped object holding hue / value /
chroma. Photos without a reference card cap at confidence ~0.5; with
a card in good light, up to ~0.75. Never extract clay %, CEC, pH, or
any non-color attribute from a photo.

---

## JSON schema (must validate)

```json
{schema_json}
```

---

## Photo

[The image is supplied as a separate content block. Examine it
directly and return one JSON object matching the worked-example
shape.]

---

Return **only** a JSON object validating against the schema above.
No prose, no markdown.
