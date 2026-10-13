# Horizonte B espodico (SiBCS Cap 2, p 62-65; v0.7)

Subsuperficial com acumulo iluvial de Al + Fe + materia organica;
espessura \\= 2.5 cm. Tipos: Bs, Bhs, Bh, ortstein. Reuso de
[`spodic`](https://hugomachadorodrigues.github.io/soilKey/reference/spodic.md)
(WRB) que ja codifica criterios essencialmente identicos.

## Usage

``` r
B_espodico(pedon, ...)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- ...:

  Reserved for future arguments.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Details

In the morphological mode the horizon is also read from its designation.
Since v0.9.247 that includes the "ir" of the older surveys (`Bir`,
`B2ir`, `B22irx`), the illuvial iron that the present notation writes
"s": the mode read "h" and "s" only.

Since v0.9.255 a horizon that the WRB test does not find is looked for
by item (a) of the book (p. 65), in both modes. The B espodico has an
"acumulacao iluvial de compostos organometalicos" and, by that item, "um
horizonte E (albico ou nao) sobrejacente e cores umidas de acordo com um
dos itens a seguir: 1) matiz 5YR ou mais vermelho; 2) matiz 7,5YR com
valor 5 ou menor e croma 4 ou menor; 3) matiz 10YR, com valor e croma 3
ou menor; 4) cores neutras com valor 3 ou menor". A B layer of 2.5 cm or
more passes when:

- an E lies above it in the profile;

- its moist colour is one of the four;

- its organic carbon is 0.5% or more and at least twice that of the
  nearest E above it; and

- no cerosidade is recorded in it.

The last two stand for the illuvial accumulation, which the sheets do
not measure where there is no oxalate extraction. The first takes the
form the book gives in item (b)(3) for aluminium and iron by oxalate,
"valor 0,50 ou maior, sendo este valor pelo menos o dobro do encontrado
no horizonte sobrejacente"; the second keeps out the B whose illuvial
material is clay. It is a reading by analogy, decided on 2026-10-13, and
not the letter of the book: the colours alone are those of the B
textural of many an Argissolo below an E. Items (b) and (c), the
cementation and the ortstein, are not tested.
