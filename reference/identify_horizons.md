# Identify the diagnostic horizons of a pedon

Reads every layer of a pedon as one of the diagnostic horizons of a
classification system, the reading its profile formulas are matched on
([`match_horizon_sequence`](https://hugomachadorodrigues.github.io/soilKey/reference/match_horizon_sequence.md)).
The system's own identification does the work; a system without one
stops with an error. In this release no system has one yet: the Russian
Field Guide of 2008 will be the first.

## Usage

``` r
identify_horizons(pedon, system, ...)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- system:

  Registry id or alias of the system
  ([`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md)).

- ...:

  Passed to the system's identification.

## Value

A data frame with one row per layer, in depth order: `horizon_idx` (the
row of the pedon's horizons), `top_cm`, `bottom_cm`, `designation`;
`code` (the diagnostic horizon, NA when undecided or none); `overlay`
(horizons laid over the main one in the same layer, a list column);
`features` (the features read on the layer) and `features_na` (the
features left undecided), list columns; `small` (a small horizon, a
feature rather than a diagnostic horizon); `transitional` (a transition
between horizons, not diagnostic); `buried` (0, or the number of the
buried profile the layer belongs to); `rockflag`, `mark` and
`composition` (rock flag, rock mark and turbated composition when
written); `route` (`"notation"`, `"data"` or `"both"`); `passed` (TRUE a
horizon read, FALSE no diagnostic horizon, NA undecided); `evidence`;
`missing` (the attributes that would decide it) and `candidates` (the
horizons still possible when undecided), list columns.

## See also

[`match_horizon_sequence`](https://hugomachadorodrigues.github.io/soilKey/reference/match_horizon_sequence.md).

## Examples

``` r
# no system identifies its horizons in this release; the error says so
tryCatch(identify_horizons(make_hscs2024_example("steppe"), "hscs2024"),
         error = function(e) conditionMessage(e))
#> [1] "The system 'hscs2024' has no horizon identification in soilKey."
```
