# Test CEC (1M NH4OAc, pH 7) per kg clay \<= threshold

Default threshold is 16 cmol_c/kg clay (WRB 2022 ferralic horizon).

## Usage

``` r
test_cec_per_clay(h, max_cmol_per_kg_clay = 16, candidate_layers = NULL)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- max_cmol_per_kg_clay:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).

## v0.9.69 ECEC fallback (opt-in)

Brazilian / SOTERLAC / BDsolos profiles often record the exchange
complex as separate Ca, Mg, K, Na, Al cmol values without an explicit
"Valor T" CEC column, so `cec_cmol` is `NA` for the entire profile. With
`options(soilKey.ferralic_ecec_fallback = TRUE)` the test falls back to
the ECEC sum (`ca_cmol + mg_cmol + k_cmol + na_cmol + al_cmol`) on
layers where `cec_cmol` is missing but the components are present.
Default is `FALSE` (canonical WRB behaviour preserved).

Note: ECEC is typically smaller than CEC at acidic pH because it omits
H+; using ECEC against the same threshold is therefore conservative
(MORE permissive) – it should not produce false positives, only recover
Latossolos that lacked Valor T.
