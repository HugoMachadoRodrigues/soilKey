# Horizonte B planico (SiBCS Cap 2, p 65-66)

"E um tipo especial de horizonte B textural, com ou sem carater sodico,
subjacente a horizontes A ou E, apresentando mudanca textural abrupta ou
transicao abrupta associada a relacao textural com valor dentro do
especificado para o horizonte B textural (subitem h), porem calculado
entre o primeiro sub-horizonte B e o horizonte imediatamente acima (A ou
E)".

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

Since v0.9.244 the horizon is looked for where the book puts it, in the
first B sub-horizon, below an A or an E, by one of the two:

- a
  [`mudanca_textural_abrupta`](https://hugomachadorodrigues.github.io/soilKey/reference/mudanca_textural_abrupta.md)
  from the A or E to it; or

- a transition recorded as abrupt (`boundary_distinctness`) with a ratio
  of its clay to that of the horizon above over the limit of item (h) of
  the B textural (1.50, 1.70 or 1.80).

As a type of B textural, that sub-horizon has a texture francoarenosa or
finer (p. 54): one known to be coarser is no B planico, whatever its
clay over that of the E. The structure and the colours below are asked
of that first sub-horizon. Until v0.9.243 any layer with a jump of clay
over the layer above it could be the B planico, the second way was not
tested, and the structure and the colours were asked of the layer with
the jump. A profile with no horizon designation at all is still tested
that way.

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
