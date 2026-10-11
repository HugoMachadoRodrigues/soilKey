# Pre-2018 SiBCS Order names -\> SiBCS 5a edicao plural Title Case map

Internal lookup applied by
[`normalise_febr_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_sibcs.md)
when `level = "order"`. BDsolos exports collected before the SiBCS 5a
edicao (2018) carry historical Order names that the modern classifier
does not emit.

## Usage

``` r
.SIBCS_LEGACY_ORDER_MAP
```

## Details

BDsolos exports collected before the SiBCS 5a edicao (2018) carry
historical Order names that the modern classifier does not emit. The
most common cases observed on RJ.csv (722 perfis):

- `Podzolicos` (54 perfis em RJ) -\> `Argissolos` (post-2018 a Order
  Argissolos absorveu o Podzolico Vermelho- Amarelo, Podzolico
  Vermelho-Escuro, etc.)

- `Gleis` (44 perfis em RJ) -\> `Gleissolos` (Gleis Humico, Gleis Pouco
  Humico colapsaram em Gleissolos)

- `Aluviais` (13 perfis em RJ) -\> `Neossolos` (Solos Aluviais foram
  reclassificados para Neossolos Fluvicos no SiBCS 5a edicao, mas a
  normalisacao aqui emite apenas a Ordem moderna `Neossolos` – a
  Subordem `Neossolos Fluvicos` nao eh recuperavel do label legado
  antigo `ALUVIAIS` (a granularidade de Subordem se perde). Para
  benchmark Order-level isso e suficiente; para Subordem o legado nao se
  mapeia.)

- `Solos` -\> `NA` ("Solos Halomorficos", "Solos Hidromorficos", e
  fragmentos de label do UI antigo do BDsolos onde a Ordem nao foi
  registrada). NA aqui significa "fora de scope para a comparacao".

Aplicado em `normalise_febr_sibcs(level = "order")` apos a pluralisacao
normal. Para subordem o legacy mapping ainda nao e aplicado (ver TODO no
v0.9.61: estender para Subordem com "Podzolico Vermelho-Amarelo" -\>
"Argissolos Vermelho-Amarelos").

Since v0.9.226 the map also holds the names of the classifications in
use before SiBCS that the 5th edition (Santos et al., 2018) gives to one
order, by the "Abrangencia" paragraph of each order in its chapter 3 (p.
87-106) and its Anexo I (p. 342): Latosol, Planosol,
Solonetz-Solodizado, Brunizem, Rendzina, Regossolo, Litosol, Solos
Litolicos, Solos Organicos, Vertisol, Cambisol, Solos Concrecionarios
and Rubrozem, with the misspellings of them and of the present orders
found in the national BDsolos export. Names that take two words are in
`.SIBCS_LEGACY_TWO_WORD_MAP`.

Left out on purpose, so that their labels stay `NA`: names the 5th
edition divides between orders by something the name does not say
(Hidromorfico Cinzento: Gleissolos without an abrupt textural change,
Planossolos with one; Solonchak: Gleissolos only with a glei horizon;
Alissolo), and names that are not in its correlation (Lateritico Bruno
Avermelhado, which is not Laterita Hidromorfica; Solos Brunos Acidos;
Solos Coluviais; Mediterraneo Vermelho-Amarelo).

The key is the first word of the label, pluralised and in Title Case, as
[`normalise_febr_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_sibcs.md)
forms it ("LATOSOL" is "Latosois", "BRUNIZEM" "Brunizens", "SOLONETZ"
"Solonetzs").
