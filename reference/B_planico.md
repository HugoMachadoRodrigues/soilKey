# Horizonte B planico (SiBCS Cap 2, p 65-66; v0.7)

Tipo especial de B textural com mudanca textural abrupta +
permeabilidade lenta + cores neutras/escurecidas + cromas baixos.

## Usage

``` r
B_planico(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.

## Details

The structure (p. 65): "Apresenta estrutura prismatica, colunar ou em
blocos angulares e subangulares grandes ou medios e, as vezes, estrutura
macica". It is tested where the sheet records it (`structure_type`,
`structure_size`): a prismatic, columnar or massive structure passes;
blocks pass when medium or larger, or when their size is not recorded;
any other recorded structure does not pass.

The colours "devem atender a pelo menos um dos seguintes requisitos" (p.
65-66):

- \(a\) the matrix colour, with or without mottles: "Matiz 10YR ou mais
  amarelo, cromas \<= 3" or "Matizes 7,5YR ou 5YR, cromas \<= 2";

- \(b\) "Coloracao variegada com pelo menos uma cor apresentando matiz e
  croma conforme especificado no item (a)" (`variegated_colors`);

- \(c\) "matiz 10YR ou mais amarelo, cromas \>= 4, combinado com
  mosqueado de croma conforme especificado no item (a)": a mottle colour
  (`mottle_colors`) that satisfies item (a).
