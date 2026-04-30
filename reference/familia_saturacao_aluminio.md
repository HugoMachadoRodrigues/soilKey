# Familia: saturacao por aluminio – "alico" (Cap 18, p 285)

Aplica o termo "alico" quando, em qualquer camada do horizonte B (ou C,
na ausencia de B):

- al_sat_pct \>= 50% (saturacao por Al = 100\*Al/(S+Al)),

- E al_cmol \> 0.5 cmolc/kg.

Quando aplicavel, o prefixo de profundidade (epi-/meso-/endo-) eh
determinado pelo topo da primeira camada que satisfaz, e concatenado ao
adjetivo: "epialico", "mesoalico", "endoalico".

## Usage

``` r
familia_saturacao_aluminio(pedon, min_al_sat = 50, min_al_cmol = 0.5)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_al_sat:

  Default 50.

- min_al_cmol:

  Default 0.5.

## Value

[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md)
com `value` igual a `"epialico"` / `"mesoalico"` / `"endoalico"` ou
NULL.

## Details

Aplicavel a classes cujo carater alumınico nao tenha sido considerado em
nivel categorico mais alto (p.ex. Argissolos Aluminicos ja o usam).

## References

Embrapa (2018), SiBCS 5a ed., Cap 18, p 285.
