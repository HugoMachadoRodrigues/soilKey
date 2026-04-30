# Familia: mineralogia da fracao argila para Latossolos (Cap 18, p 286-287)

Classifica via Ki = SiO2/(Al2O3) e Kr = SiO2/(Al2O3 + Fe2O3) molares
(helpers
[`compute_ki`](https://hugomachadorodrigues.github.io/soilKey/reference/compute_ki.md)
/
[`compute_kr`](https://hugomachadorodrigues.github.io/soilKey/reference/compute_kr.md)):

- `caulinitico`: Ki \> 0.75 e Kr \> 0.75

- `caulinitico-oxidico`: Ki \> 0.75 e Kr \<= 0.75

- `gibsitico-oxidico`: Ki \<= 0.75 e Kr \<= 0.75

- `oxidico`: Kr \<= 0.75 (predominio Fe2O3 + Al2O3)

## Usage

``` r
familia_mineralogia_argila_latossolo(pedon, max_depth_cm = 200)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Profundidade da secao de controle (default 200).

## Value

[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## Details

Aplicavel principalmente para Latossolos; tambem pode ser usado em
Argissolos, Cambissolos e Plintossolos quando ha informacao de
mineralogia da argila pelo menos semiquantitativa.

## References

Embrapa (2018), SiBCS 5a ed., Cap 18, p 286-287.
