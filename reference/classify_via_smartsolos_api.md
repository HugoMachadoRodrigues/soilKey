# Classify a PedonRecord via Embrapa's SmartSolosExpert REST API

Sends a soilKey
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
to the SmartSolosExpert REST endpoint maintained by Embrapa (Glauber
Vaz's PROLOG-based implementation of the SiBCS classifier) and returns
the resulting four-level classification (Ordem / Subordem / Grande Grupo
/ Subgrupo) wrapped in a soilKey
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md).

## Usage

``` r
classify_via_smartsolos_api(
  pedon,
  api_key = Sys.getenv("AGROAPI_TOKEN"),
  endpoint = c("classification", "verification"),
  drenagem = NULL,
  reference_sibcs = NULL,
  base_url = "https://api.cnptia.embrapa.br/smartsolos/expert/v1",
  timeout_seconds = 30,
  post_fn = NULL,
  verbose = TRUE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- api_key:

  Bearer token. Defaults to `Sys.getenv("AGROAPI_TOKEN")`. Required
  unless `post_fn` is supplied (test injection).

- endpoint:

  One of `"classification"` (default; classify only) or `"verification"`
  (classify + compare against user-supplied `reference_sibcs`).

- drenagem:

  Optional drainage class. Integer 1..8 or Portuguese string
  (`"bem drenado"` etc.).

- reference_sibcs:

  Optional named list (`ordem, subordem, gde_grupo, subgrupo`) used by
  the `"verification"` endpoint as the user's reference.

- base_url:

  Override base URL. Default
  `"https://api.cnptia.embrapa.br/smartsolos/expert/v1"`.

- timeout_seconds:

  HTTP timeout (default 30).

- post_fn:

  Internal: function with signature `function(payload) -> response_list`
  for unit tests. When supplied, the network is bypassed.

- verbose:

  If `TRUE` (default), emits a one-line summary.

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system = "SiBCS 5a edicao (SmartSolosExpert API)"` and the four
taxonomic levels in `rsg_or_order` (Ordem) and `qualifiers` (Subordem /
GdeGrupo / Subgrupo). Verification-mode responses additionally carry
`trace$smartsolos_summary` (the per-level match counters `L0..L4`).

## Details

This is an \*\*external classifier\*\* – the package does not host or
replicate the PROLOG rules. The function exists so soilKey users can
cross-validate the local classifier against an authoritative
Embrapa-hosted reference. Use the `"verification"` endpoint to compare
against your own user-supplied reference classification (the API returns
a per-level match `summary` with counters `L0..L4`).

Authentication: register a free AgroAPI account at
<https://www.agroapi.cnptia.embrapa.br/portal/>, subscribe to the
SmartSolosExpert API and generate an access token. Pass it via the
`AGROAPI_TOKEN` environment variable or the `api_key` argument.

## See also

[`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md)
for the local PROLOG-free classifier;
[`compare_smartsolos`](https://hugomachadorodrigues.github.io/soilKey/reference/compare_smartsolos.md)
for a side-by-side comparison helper.

## Examples

``` r
if (FALSE) { # \dontrun{
Sys.setenv(AGROAPI_TOKEN = "<your token>")
res <- classify_via_smartsolos_api(make_argissolo_canonical())
res$rsg_or_order      # "ARGISSOLO"
res$qualifiers
#> $subordem  "VERMELHO"
#> $gde_grupo "Distrofico"
#> $subgrupo  "tipico"
} # }
```
