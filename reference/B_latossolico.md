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
B_latossolico(
  pedon,
  min_thickness = 50,
  max_cec_per_clay = NULL,
  engine = NULL,
  ...
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

- max_cec_per_clay:

  CEC per kg clay the horizon must stay below. Defaults to `NULL`
  (engine-aware): 17 in soilkey engine (SiBCS Cap 2: "menor que 17 cmolc
  kg-1 de argila, sem correcao para carbono"; slightly more permissive
  than WRB ferralic's \< 16) or 20 in aqp engine (v0.9.68 regional
  tolerance for Embrapa lab methodology offset). Strict since v0.9.212:
  a value of exactly 17 is not latossolic, it is the B incipiente's "17
  ou maior".

- engine:

  One of `"soilkey"` (default) or `"aqp"`; `NULL` reads
  `getOption("soilKey.diagnostic_engine")`. Forwarded to
  [`ferralic`](https://hugomachadorodrigues.github.io/soilKey/reference/ferralic.md).

- ...:

  Reserved for future arguments.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
