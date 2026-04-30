# Familia: prefixo de profundidade epi-/meso-/endo- (Cap 18, p 284-285)

Classifica a profundidade onde um diagnostico ocorre em um dos tres
prefixos:

- `epi-`: topo da primeira camada que satisfaz \< 50 cm

- `meso-`: topo da primeira camada em \[50, 100) cm

- `endo-`: topo da primeira camada em \>= 100 cm

## Usage

``` r
familia_prefixo_profundidade(diag, horizons)
```

## Arguments

- diag:

  Um
  [`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
  com `layers` (indices de horizontes que satisfazem o atributo).

- horizons:

  `data.table` de horizontes do pedon.

## Value

String "epi" / "meso" / "endo" ou NULL.

## Details

Wrapper generico para ser usado com qualquer
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
Retorna NULL se o diagnostico nao passou ou se nao ha camadas
identificadas.

## References

Embrapa (2018), SiBCS 5a ed., Cap 18, p 284-285.
