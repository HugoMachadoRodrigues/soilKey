# Convert one PedonRecord to the SmartSolosExpert request payload

Builds the JSON-serialisable list expected by the `POST /classification`
(or `/verification`) endpoint. Missing soilKey horizon attributes are
sent as `NA` (the API tolerates partial data and replies with `NULL` for
taxonomic levels that cannot be resolved).

## Usage

``` r
.smartsolos_pedon_to_payload(pedon, drenagem = NULL, reference_sibcs = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- drenagem:

  Optional integer 1..8 (SiBCS drainage class) or a string in
  `c("excessivamente drenado", ..., "muito mal drenado")`.

- reference_sibcs:

  Optional named list with the user's reference SiBCS classification
  (`ordem, subordem, gde_grupo, subgrupo`) for use with the
  `/verification` endpoint.
