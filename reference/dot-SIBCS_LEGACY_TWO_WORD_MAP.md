# Old soil names that take two words to tell apart

Consulted by
[`normalise_febr_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_sibcs.md)
before `.SIBCS_LEGACY_ORDER_MAP` when the first word of a label gives no
order. The key is the first two words of the name, upper case, without
accents, as written (a leading "Solo" or "Solos" is not counted, and a
hyphen separates words as a space does). One word would not do: "Bruno
Nao Calcico" is Luvissolos and "Solos Brunos Acidos" has no order in the
correlation of the 5th edition; "Terra" and "Humic" are not names.

## Usage

``` r
.SIBCS_LEGACY_TWO_WORD_MAP
```

## Details

- `Terra Roxa` and `Terra Bruna` (Estruturada, Estruturada Similar) -\>
  `Nitossolos`: "na maioria" (SiBCS 2018, p. 98). A "pequena parte",
  with the texture gradient of a B textural, went to Argissolos (p. 87);
  the name does not say which, and the label is read as the majority.

- `Bruno Nao Calcico` -\> `Luvissolos` (p. 95-96, Anexo I).

- `Humic Glei` and `Low-Humic Glei` (the English names of Glei Humico
  and Glei Pouco Humico) -\> `Gleissolos` (p. 93).

- `Solos Indiscriminados Concrecionarios` -\> `Plintossolos` (p. 104).
