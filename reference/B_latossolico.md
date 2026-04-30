# Horizonte B latossolico (SiBCS Cap 2, p 57-59; v0.7 strict)

Adicionalmente a
[`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md)
(WRB), o B latossolico SiBCS exige:

- Espessura minima de 50 cm;

- Textura francoarenosa ou mais fina;

- Estrutura granular muito pequena/pequena ou em blocos subangulares
  fraco/moderado;

- \< 5% volume mostrando estrutura da rocha original;

- Ki \\= 2.2 (geralmente \\= 2.0);

- Cerosidade no maximo pouca e fraca.

v0.7 enforce thickness, texture, e ausencia de estrutura primaria
herdada via designation e clay; Ki/Kr quantitativos sao v0.8 (precisa de
SiO2/Al2O3 lab-data nao no schema).

## Usage

``` r
B_latossolico(pedon, min_thickness = 50, max_cec_per_clay = 17, ...)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

- max_cec_per_clay:

  Numeric threshold or option (see Details).

- ...:

  Reserved for future arguments.
