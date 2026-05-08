# Detecta fallback "cor a determinar" no nivel de subordem SiBCS

Quando a subordem atribuida e uma catch-all de cor (PVA, LVA, NX, TX) E
pelo menos um predicado anterior na trace falhou exatamente por ausencia
de `munsell_hue_moist`, considera-se que o fallback foi forçado pela
ausencia de matiz, nao pelo conteudo do perfil. Retorna NULL se a
situacao nao se aplica.

## Usage

``` r
.detect_color_undetermined_fallback(sub_result, subordem)
```
