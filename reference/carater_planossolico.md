# Carater planossolico (SiBCS Cap 5)

Caracter planico em posicao NAO diagnostica para Planossolos. Discrimina
os Subgrupos planossolicos de Argissolos (Cap 5: PA, PVA, PV).

## Usage

``` r
carater_planossolico(pedon, max_depth_cm = 150)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth_cm:

  Profundidade maxima (default 150).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementacao v0.7.4: aproxima como
[`B_planico`](https://hugomachadorodrigues.github.io/soilKey/reference/B_planico.md)
OR
([`mudanca_textural_abrupta`](https://hugomachadorodrigues.github.io/soilKey/reference/mudanca_textural_abrupta.md)
AND
[`carater_sodico`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_sodico.md)).
SiBCS Cap 1 estritamente define caracter planico via mudanca textural
abrupta + horizonte/caracter sodico em B + cores neutras.

## References

Embrapa (2018), SiBCS 5a ed., Cap 5; Cap 1, p 36; Cap 15 (Planossolos).
