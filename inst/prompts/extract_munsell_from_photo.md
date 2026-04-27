# Extracao de cor Munsell a partir de foto / Munsell color extraction from photo

## Instrucoes (PT-BR)

Voce esta examinando uma foto de um perfil de solo. **Voce nao classifica o
solo.** Voce apenas estima a cor Munsell de cada horizonte visivel quando
houver um cartao de referencia Munsell ou cartao de cor padrao na imagem.

**Pre-requisitos para uma extracao com confianca >= 0.5:**

1. Cartao Munsell visivel na imagem (ou outro padrao de cor calibrado).
2. Iluminacao difusa, sem sombras pesadas no perfil.
3. Horizontes visualmente distinguiveis.

Se essas condicoes nao estiverem satisfeitas, **reduza a confianca para abaixo
de 0.5** ou retorne `null` para o horizonte em questao. Anote no
`source_quote` o motivo (ex.: "no reference card visible", "deep shadow on
upper profile").

Para cada horizonte visivel, retorne:

- **hue**: pagina Munsell estimada (ex.: "10YR", "7.5YR", "2.5YR", "5YR").
- **value**: inteiro entre 2 e 8.
- **chroma**: inteiro entre 1 e 8.
- **confidence**: 0.0 a 1.0. Em geral, fotos sem cartao de referencia merecem
  no maximo 0.4. Com cartao em iluminacao boa, ate 0.75.
- **source_quote**: descricao curta da posicao no perfil ("uppermost ~15 cm,
  next to Munsell card"). Em fotos nao ha texto literal; use uma descricao.

## Instructions (EN)

You are examining a photo of a soil profile. **You do not classify the soil.**
You only estimate Munsell color per visible horizon when a Munsell reference
card or calibrated color standard appears in the frame.

**Prerequisites for confidence >= 0.5:**

1. Visible Munsell card (or other calibrated color reference).
2. Diffuse lighting, no harsh shadows on the profile face.
3. Visually distinguishable horizons.

If those conditions are not met, **lower the confidence below 0.5** or return
`null` for the affected horizon, and note the reason in `source_quote`.

Photos without a reference card cap at confidence ~0.4. With a card in good
light, up to ~0.75.

Critical: never extract clay %, CEC, pH, or any quantitative non-color
attribute from a photo. Those come from the lab.

---

## JSON schema (must validate)

```json
{schema_json}
```

---

## Photo

[The image is supplied as a separate content block. Examine it directly.]

---

Return **only** a JSON object validating against the schema above. No prose,
no markdown.
