# Ki/Kr para Latossolos (SiBCS Cap 10, p 173-176)

Diagnostico SiBCS estrito sobre o B latossolico: requer Ki \\\le\\
max_ki em todos os horizontes B avaliados, e Kr \\\le\\ max_kr quando
Fe2O3 estiver disponivel. Sub-classes acricas (Latossolos Acricos) e
acriferricas adicionalmente exigem
[`carater_acrico`](https://hugomachadorodrigues.github.io/soilKey/reference/carater_acrico.md).

## Usage

``` r
latossolo_ki_kr(pedon, max_ki = 2.2, max_kr = 1.7)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_ki:

  Ki limite superior (default 2.2 – limite kaolinitico SiBCS Cap 10).

- max_kr:

  Kr limite superior (default 1.7).

## Value

[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Quando os campos de ataque sulfurico (`sio2_sulfuric_pct`,
`al2o3_sulfuric_pct`, `fe2o3_sulfuric_pct`) estao todos NA, o
diagnostico retorna `passed = NA` com `missing` explicito.

## References

Embrapa (2018), SiBCS 5a ed., Cap 10 (Latossolos), pp 173-176.
