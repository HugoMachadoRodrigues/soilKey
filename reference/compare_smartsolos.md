# Cross-validate the local SiBCS classifier against the SmartSolosExpert API

Runs both
[`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md)
(local) and
[`classify_via_smartsolos_api`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_via_smartsolos_api.md)
(remote PROLOG via Embrapa AgroAPI) on the same
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
and tabulates agreement at each of the four SiBCS categorical levels.

## Usage

``` r
compare_smartsolos(pedon, ...)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- ...:

  Forwarded to
  [`classify_via_smartsolos_api`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_via_smartsolos_api.md).

## Value

A list with `local` and `remote` `ClassificationResult`s plus a one-row
`agreement` data.frame with columns
`ordem, subordem, gde_grupo, subgrupo, n_match`.

## Examples

``` r
if (FALSE) { # \dontrun{
Sys.setenv(AGROAPI_TOKEN = "<your token>")
cmp <- compare_smartsolos(make_argissolo_canonical())
cmp$agreement
} # }
```
